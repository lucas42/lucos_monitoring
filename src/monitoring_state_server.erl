-module(monitoring_state_server).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_cast/2, handle_call/3]).

start_link() -> gen_server:start_link(?MODULE, [], []).

init([]) ->
	{ok, {#{}, #{}}}.

handle_cast(Request, {SystemMap, SuppressionMap}) ->
	case Request of
		{updateSystem, Host, System, Source, SourceChecks, SystemMetrics} ->
			io:format("Received update for system ~p (Host ~p, Source ~p)~n", [System, Host, Source]),
			IsFirstSeen = not maps:is_key(Host, SystemMap),
			{_, OldSourceChecksMap, _} = maps:get(Host, SystemMap, {nil, #{}, nil}),
			OldMergedChecks = mergeSourceChecks(OldSourceChecksMap),
			NewSourceChecksMap = maps:put(Source, SourceChecks, OldSourceChecksMap),
			NewMergedChecks = mergeSourceChecks(NewSourceChecksMap),
			NormalisedChecks = normaliseChecks(OldMergedChecks, NewMergedChecks),
			NewSuppressionMap = case {IsFirstSeen, meaningfulChange(OldMergedChecks, NormalisedChecks)} of
				{true, _} ->
					io:format("Warm-up: skipping alert for ~p on first poll~n", [System]),
					SuppressionMap;
				{false, true} ->
					state_change(Host, System, NormalisedChecks, SystemMetrics, SuppressionMap);
				{false, false} ->
					SuppressionMap
			end,
			NewSystemMap = maps:put(Host, {System, NewSourceChecksMap, SystemMetrics}, SystemMap),
			{noreply, {NewSystemMap, NewSuppressionMap}}
	end.

handle_call(Request, _From, {SystemMap, SuppressionMap}) ->
	case Request of
		{fetch, all} ->
			% Callers expect {System, FlatChecks, Metrics} — merge sources before returning
			FlatSystemMap = maps:map(fun(_, {System, SourceChecksMap, Metrics}) ->
				{System, mergeSourceChecks(SourceChecksMap), Metrics}
			end, SystemMap),
			{reply, FlatSystemMap, {SystemMap, SuppressionMap}};
		{fetch, Host} ->
			{System, SourceChecksMap, Metrics} = maps:get(Host, SystemMap),
			{reply, {System, mergeSourceChecks(SourceChecksMap), Metrics}, {SystemMap, SuppressionMap}};
		{suppress, System} ->
			case systemExists(System, SystemMap) of
				true ->
					ExpiryTime = erlang:system_time(second) + 600,
					NewSuppressionMap = maps:put(System, ExpiryTime, SuppressionMap),
					io:format("Suppression window opened for ~p~n", [System]),
					{reply, ok, {SystemMap, NewSuppressionMap}};
				false ->
					{reply, {error, not_found}, {SystemMap, SuppressionMap}}
			end;
		{unsuppress, System} ->
			NewSuppressionMap = maps:remove(System, SuppressionMap),
			io:format("Suppression window closed for ~p~n", [System]),
			{reply, ok, {SystemMap, NewSuppressionMap}}
	end.

% Merges checks from all sources into a single flat map.
% Later sources in the fold override earlier ones for any shared keys,
% but in practice info and circleci write disjoint key sets.
mergeSourceChecks(SourceChecksMap) ->
	maps:fold(fun(_, Checks, Acc) ->
		maps:merge(Acc, Checks)
	end, #{}, SourceChecksMap).

systemExists(System, SystemMap) ->
	maps:fold(fun
		(_, {S, _, _}, _) when S =:= System -> true;
		(_, _, Acc) -> Acc
	end, false, SystemMap).

% Replaces any unknown "ok" with whichever the value was there previously, until 3 consecutive unknowns are recieved at which point ok is set to false
replaceUnknowns(OldChecks, NewChecks, Iterator) ->
	case maps:next(Iterator) of
		{Key, NewCheck, NextIterator} ->
			NormalisedCheck = case maps:get(<<"ok">>, NewCheck, unknown) of
				unknown ->
					OldCheck = maps:get(Key, OldChecks, #{<<"ok">> => unknown}),
					OldCount = maps:get(<<"unknown_count">>, OldCheck, 0),
					NewCount = OldCount + 1,
					IncrementedCheck = maps:put(<<"unknown_count">>, NewCount, NewCheck),
					case NewCount >= 3 of
						true ->
							 maps:put(<<"ok">>, false, IncrementedCheck);
						false ->
							maps:put(<<"ok">>, maps:get(<<"ok">>, OldCheck, unknown), IncrementedCheck)
					end;
				_ ->
					maps:put(<<"unknown_count">>, 0, NewCheck)
			end,
			maps:put(Key, NormalisedCheck, replaceUnknowns(OldChecks, NewChecks, NextIterator));
		none ->
			maps:new()
	end.

% If there's a problem with the 'fetch-info' check, merge the old and new checks to avoid flapiness
mergeMissingInfoChecks(OldChecks, NewChecks) ->
	case maps:get(<<"ok">>, maps:get(<<"fetch-info">>, NewChecks, #{<<"ok">> => unknown}), unknown) of
		true ->
			NewChecks;
		_ ->
			maps:merge(OldChecks, NewChecks)
	end.

% Reduce monitoring flapiness by using existing check data to bolster new checks which may be facing a temporary blip
normaliseChecks(OldChecks, NewChecks) ->
	MergedChecks = mergeMissingInfoChecks(OldChecks, NewChecks),
	replaceUnknowns(OldChecks, MergedChecks, maps:iterator(MergedChecks, reversed)).

% Decides whether the checks have changed in a meaningful way (ie ignore "unknown" states)
meaningfulChange(OldChecks, NewChecks) ->
	NewFailingChecks = failingChecks(NewChecks),
	OldFailingChecks = failingChecks(OldChecks),
	maps:keys(OldFailingChecks) /= maps:keys(NewFailingChecks).

failingChecks(Checks) ->
	maps:filter(fun(_, Check) ->
		maps:get(<<"ok">>, Check, unknown) == false
	end, Checks).

safe_notify(Host, System, FailingNow, Suppressed) ->
	try loganne:notify(Host, System, FailingNow, Suppressed)
	catch ExClass:ExReason ->
		io:format("Loganne notify failed for ~p on ~p: ~p ~p~n", [System, Host, ExClass, ExReason])
	end.

safe_email_notify(Host, System, FailingNow, SystemMetrics) ->
	try email:notify(Host, System, FailingNow, SystemMetrics)
	catch ExClass:ExReason ->
		io:format("Email notify failed for ~p on ~p: ~p ~p~n", [System, Host, ExClass, ExReason])
	end.

state_change(Host, System, SystemChecks, SystemMetrics, SuppressionMap) ->
	FailingNow = failingChecks(SystemChecks),
	case maps:get(System, SuppressionMap, undefined) of
		undefined ->
			io:format("Checks' state changed for ~p on ~p~n", [System, Host]),
			safe_notify(Host, System, FailingNow, false),
			safe_email_notify(Host, System, FailingNow, SystemMetrics),
			SuppressionMap;
		ExpiryTime ->
			Now = erlang:system_time(second),
			case Now < ExpiryTime of
				true ->
					io:format("Alert suppressed for ~p during deploy window~n", [System]),
					safe_notify(Host, System, FailingNow, true),
					SuppressionMap;
				false ->
					io:format("ERROR: Suppression window for ~p expired without being cleared - deploy may have taken longer than 10 minutes~n", [System]),
					io:format("Checks' state changed for ~p on ~p~n", [System, Host]),
					safe_notify(Host, System, FailingNow, false),
					safe_email_notify(Host, System, FailingNow, SystemMetrics),
					maps:remove(System, SuppressionMap)
			end
	end.


-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").
	nomaliseChecks_test() ->
		?assertEqual(#{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 1}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0}}, normaliseChecks(#{<<"ci">> => #{<<"ok">> => true}}, #{<<"ci">> => #{<<"ok">> => unknown}, <<"fetch-info">> => #{<<"ok">> => true}})),
		?assertEqual(#{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 2}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0}}, normaliseChecks(#{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 1}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 2}}, #{<<"ci">> => #{<<"ok">> => unknown}, <<"fetch-info">> => #{<<"ok">> => true}})),
		?assertEqual(#{<<"ci">> => #{<<"ok">> => false, <<"unknown_count">> => 3}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0}}, normaliseChecks(#{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 2}}, #{<<"ci">> => #{<<"ok">> => unknown}, <<"fetch-info">> => #{<<"ok">> => true}})),
		?assertEqual(#{<<"item-count">> => #{<<"ok">> => false, <<"unknown_count">> => 0}, <<"api-check">> => #{<<"ok">> => true, <<"unknown_count">> => 0}, <<"fetch-info">> => #{<<"ok">> => true,  <<"unknown_count">> => 1}}, normaliseChecks(#{<<"item-count">> => #{<<"ok">> => false}, <<"api-check">> => #{<<"ok">> => true}, <<"fetch-info">> => #{<<"ok">> => true}}, #{<<"fetch-info">> => #{<<"ok">> => unknown}})).

	meaningfulChange_test() ->
		?assertEqual(false, meaningfulChange(#{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 1}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0}}, #{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0}})),
		?assertEqual(true, meaningfulChange(#{<<"ci">> => #{<<"ok">> => false, <<"unknown_count">> => 3}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0}}, #{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 2}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0}})),
		?assertEqual(true, meaningfulChange(#{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 1}, <<"fetch-info">> => #{<<"ok">> => false, <<"unknown_count">> => 0}}, #{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0}})),
		?assertEqual(true, meaningfulChange(#{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0}}, #{<<"ci">> => #{<<"ok">> => false, <<"unknown_count">> => 0}, <<"fetch-info">> => #{<<"ok">> => false, <<"unknown_count">> => 4}})).

	systemExists_test() ->
		SystemMap = #{
			"host1.example.com" => {"lucos_foo", #{}, #{}},
			"host2.example.com" => {"lucos_bar", #{}, #{}}
		},
		?assertEqual(true, systemExists("lucos_foo", SystemMap)),
		?assertEqual(true, systemExists("lucos_bar", SystemMap)),
		?assertEqual(false, systemExists("lucos_missing", SystemMap)),
		?assertEqual(false, systemExists("lucos_foo", #{})).

	mergeSourceChecks_test() ->
		% Two disjoint sources produce a flat merged map
		SourceChecksMap = #{
			info => #{<<"fetch-info">> => #{<<"ok">> => true}, <<"tls-certificate">> => #{<<"ok">> => true}},
			circleci => #{<<"circleci">> => #{<<"ok">> => false}}
		},
		Result = mergeSourceChecks(SourceChecksMap),
		?assertEqual(#{
			<<"fetch-info">> => #{<<"ok">> => true},
			<<"tls-certificate">> => #{<<"ok">> => true},
			<<"circleci">> => #{<<"ok">> => false}
		}, Result).

	mergeSourceChecks_empty_test() ->
		?assertEqual(#{}, mergeSourceChecks(#{})).

	% First update for a host stores its state but doesn't alert (warm-up grace period).
	% safe_notify/safe_email_notify wrap loganne/email calls in try-catch, so even if
	% those modules are unavailable the cast still returns without crashing.
	warmup_first_update_stores_state_test() ->
		InitialState = {#{}, #{}},
		Checks = #{<<"fetch-info">> => #{<<"ok">> => false}},
		{noreply, {SystemMap, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", info, Checks, #{}},
			InitialState
		),
		% Host should now be in the SystemMap
		?assert(maps:is_key("host1.example.com", SystemMap)),
		{"lucos_foo", SourceChecksMap, _} = maps:get("host1.example.com", SystemMap),
		StoredChecks = mergeSourceChecks(SourceChecksMap),
		?assertEqual(false, maps:get(<<"ok">>, maps:get(<<"fetch-info">>, StoredChecks))).

	% Second update for a known host triggers normal alert logic (not warm-up).
	% Here both updates report the same healthy state, so no meaningful change — no alert.
	warmup_second_update_not_suppressed_test() ->
		Checks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		ExistingState = {
			#{"host1.example.com" => {"lucos_foo", #{info => Checks}, #{}}},
			#{}
		},
		{noreply, {SystemMap, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", info, Checks, #{}},
			ExistingState
		),
		% Host is still in the map after second update
		?assert(maps:is_key("host1.example.com", SystemMap)).

	% circleci update for a host doesn't clobber info checks, and vice versa.
	two_sources_dont_clobber_each_other_test() ->
		InfoChecks = #{<<"fetch-info">> => #{<<"ok">> => true}, <<"tls-certificate">> => #{<<"ok">> => true}},
		CIChecks = #{<<"circleci">> => #{<<"ok">> => false}},
		% Start with info checks already stored
		ExistingState = {
			#{"host1.example.com" => {"lucos_foo", #{info => InfoChecks}, #{}}},
			#{}
		},
		% circleci update arrives
		{noreply, {SystemMap, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", circleci, CIChecks, #{}},
			ExistingState
		),
		{"lucos_foo", SourceChecksMap, _} = maps:get("host1.example.com", SystemMap),
		Merged = mergeSourceChecks(SourceChecksMap),
		% All three checks must be present
		?assert(maps:is_key(<<"fetch-info">>, Merged)),
		?assert(maps:is_key(<<"tls-certificate">>, Merged)),
		?assert(maps:is_key(<<"circleci">>, Merged)),
		?assertEqual(false, maps:get(<<"ok">>, maps:get(<<"circleci">>, Merged))),
		?assertEqual(true, maps:get(<<"ok">>, maps:get(<<"fetch-info">>, Merged))).

	% When info source updates and drops a check key, it's removed from the merged view.
	dropped_info_check_is_removed_test() ->
		OldInfoChecks = #{<<"fetch-info">> => #{<<"ok">> => true}, <<"custom-check">> => #{<<"ok">> => true}},
		NewInfoChecks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		ExistingState = {
			#{"host1.example.com" => {"lucos_foo", #{info => OldInfoChecks}, #{}}},
			#{}
		},
		{noreply, {SystemMap, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", info, NewInfoChecks, #{}},
			ExistingState
		),
		{"lucos_foo", SourceChecksMap, _} = maps:get("host1.example.com", SystemMap),
		Merged = mergeSourceChecks(SourceChecksMap),
		?assertNot(maps:is_key(<<"custom-check">>, Merged)).

-endif.
