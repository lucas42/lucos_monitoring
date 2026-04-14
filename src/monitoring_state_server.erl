-module(monitoring_state_server).
-behaviour(gen_server).
-export([start_link/0, start_link/1, init/1, handle_cast/2, handle_call/3]).

start_link() ->
	start_link([fun loganne:notify/5, fun email:notify/5]).

start_link(Notifiers) ->
	gen_server:start_link(?MODULE, Notifiers, []).

init(Notifiers) ->
	{ok, {#{}, #{}, Notifiers}}.

handle_cast(Request, {SystemMap, SuppressionMap, Notifiers}) ->
	case Request of
		{updateSystem, Host, System, Source, SourceChecks, SystemMetrics} ->
			logger:info("Received update for system ~p (Host ~p, Source ~p)", [System, Host, Source]),
			IsFirstSeen = not maps:is_key(Host, SystemMap),
			{_, OldSourceChecksMap, OldNormalisedCache, OldMetrics} = maps:get(Host, SystemMap, {nil, #{}, #{}, #{}}),
			NewSourceChecksMap = maps:put(Source, SourceChecks, OldSourceChecksMap),
			NewMergedChecks = mergeSourceChecks(NewSourceChecksMap),
			% When a source explicitly reports no checks (e.g. circleci returns 404,
			% meaning no CI is configured for this repo), remove the keys it previously
			% contributed from the old normalised cache. This prevents mergeMissingInfoChecks
			% from resurrecting stale checks (e.g. circleci) when another source
			% (e.g. info) is temporarily unavailable.
			% We only prune on empty source updates: if a source reports any checks
			% (even unknown ones), we leave the old cache intact so that anti-flapiness
			% logic (mergeMissingInfoChecks) can still hold info checks steady during
			% a transient /_info blip.
			OldSourceChecks = maps:get(Source, OldSourceChecksMap, #{}),
			PrunedOldCache = case maps:size(SourceChecks) of
				0 -> maps:without(maps:keys(OldSourceChecks), OldNormalisedCache);
				_ -> OldNormalisedCache
			end,
			% Only count checks as "newly unknown" if they were included in the current
			% source update — not checks carried forward from other sources. This prevents
			% double-incrementing unknown_count when two sources (e.g. circleci + info)
			% update within the same monitoring cycle.
			CountableKeys = maps:fold(fun(Key, Check, Acc) ->
				case maps:get(<<"ok">>, Check, unknown) of
					unknown -> sets:add_element(Key, Acc);
					_ -> Acc
				end
			end, sets:new([{version, 2}]), SourceChecks),
			NormalisedChecks = normaliseChecks(PrunedOldCache, NewMergedChecks, CountableKeys),
			% Only overwrite metrics when the source provides them; sources
			% without metrics (e.g. circleci) pass #{} and should not wipe
			% metrics previously stored by the info fetcher.
			NewMetrics = case maps:size(SystemMetrics) of
				0 -> OldMetrics;
				_ -> SystemMetrics
			end,
			NewSuppressionMap = case IsFirstSeen of
				true ->
					logger:notice("Warm-up: skipping alert for ~p on first poll", [System]),
					SuppressionMap;
				false ->
					case maps:get(System, SuppressionMap, undefined) of
						{pending_verification, PendingSources} ->
							% Suppression was recently lifted. Defer the alert decision until
							% all sources have reported fresh post-deploy data.
							Remaining = sets:del_element(Source, PendingSources),
							case sets:size(Remaining) =:= 0 of
								true ->
									FailingNow = failingChecks(NormalisedChecks),
									case maps:size(FailingNow) > 0 of
										true ->
											logger:notice("Service ~p still unhealthy after deploy — alerting", [System]),
											notify_all(Host, System, FailingNow, false, NewMetrics, Notifiers);
										false ->
											logger:notice("Service ~p healthy after deploy", [System])
									end,
									maps:remove(System, SuppressionMap);
								false ->
									maps:put(System, {pending_verification, Remaining}, SuppressionMap)
							end;
						_ ->
							case meaningfulChange(OldNormalisedCache, NormalisedChecks) of
								true ->
									state_change(Host, System, NormalisedChecks, NewMetrics, SuppressionMap, Notifiers);
								false ->
									SuppressionMap
							end
					end
			end,
			NewSystemMap = maps:put(Host, {System, NewSourceChecksMap, NormalisedChecks, NewMetrics}, SystemMap),
			{noreply, {NewSystemMap, NewSuppressionMap, Notifiers}}
	end.

handle_call(Request, _From, {SystemMap, SuppressionMap, Notifiers}) ->
	case Request of
		{fetch, all} ->
			% Callers expect {System, FlatChecks, Metrics} — return the normalised cache directly
			FlatSystemMap = maps:map(fun(_, {System, _SourceChecksMap, NormalisedCache, Metrics}) ->
				{System, NormalisedCache, Metrics}
			end, SystemMap),
			{reply, FlatSystemMap, {SystemMap, SuppressionMap, Notifiers}};
		{fetch, suppression} ->
			{reply, SuppressionMap, {SystemMap, SuppressionMap, Notifiers}};
		{fetch, Host} ->
			{System, _SourceChecksMap, NormalisedCache, Metrics} = maps:get(Host, SystemMap),
			{reply, {System, NormalisedCache, Metrics}, {SystemMap, SuppressionMap, Notifiers}};
		{suppress, System} ->
			case systemExists(System, SystemMap) of
				true ->
					ExpiryTime = erlang:system_time(second) + 600,
					NewSuppressionMap = maps:put(System, ExpiryTime, SuppressionMap),
					logger:notice("Suppression window opened for ~p", [System]),
					{reply, ok, {SystemMap, NewSuppressionMap, Notifiers}};
				false ->
					{reply, {error, not_found}, {SystemMap, SuppressionMap, Notifiers}}
			end;
		{unsuppress, System} ->
			% Instead of alerting immediately on stale pre-unsuppress health data,
			% enter pending_verification so the alert decision is deferred until all
			% sources have reported fresh post-deploy results.
			case maps:is_key(System, SuppressionMap) of
				true ->
					Sources = collect_active_sources(System, SystemMap),
					NewSuppressionMap = maps:put(System, {pending_verification, Sources}, SuppressionMap),
					logger:notice("Suppression window closed for ~p — awaiting verification poll", [System]),
					% Cascade pending_verification to systems that have checks depending on this system.
					% Single-hop only: we do not follow dependsOn chains transitively.
					DependentSystems = find_dependent_systems(System, SystemMap),
					FinalSuppressionMap = lists:foldl(fun(DepSystem, SM) ->
						DepSources = collect_active_sources(DepSystem, SystemMap),
						logger:notice("Cascading pending_verification to ~p (has checks depending on ~p)", [DepSystem, System]),
						maps:put(DepSystem, {pending_verification, DepSources}, SM)
					end, NewSuppressionMap, DependentSystems),
					{reply, ok, {SystemMap, FinalSuppressionMap, Notifiers}};
				false ->
					logger:notice("Suppression window closed for ~p (was not suppressed)", [System]),
					{reply, ok, {SystemMap, SuppressionMap, Notifiers}}
			end
	end.

% Merges checks from all sources into a single flat map.
% The current sources (info, circleci) write disjoint check key sets,
% so fold ordering doesn't matter in practice. If a future source shares
% keys with an existing one, override order will be non-deterministic
% (map fold order over atom keys is unspecified in Erlang) — revisit then.
mergeSourceChecks(SourceChecksMap) ->
	maps:fold(fun(_, Checks, Acc) ->
		maps:merge(Acc, Checks)
	end, #{}, SourceChecksMap).

systemExists(System, SystemMap) ->
	maps:fold(fun
		(_, {S, _, _, _}, _) when S =:= System -> true;
		(_, _, Acc) -> Acc
	end, false, SystemMap).

% Returns the set of source keys that have reported for a given system,
% across all hosts. Used to build the PendingSources set on unsuppress.
collect_active_sources(System, SystemMap) ->
	maps:fold(fun
		(_, {S, SourceChecksMap, _, _}, Acc) when S =:= System ->
			sets:union(Acc, sets:from_list(maps:keys(SourceChecksMap)));
		(_, _, Acc) -> Acc
	end, sets:new([{version, 2}]), SystemMap).

% Replaces any unknown "ok" with whichever the value was there previously, until 3 consecutive unknowns are recieved at which point ok is set to false.
% CountableKeys is the set of check keys that should be considered for incrementing.
% Only keys in this set increment their unknown_count — this prevents double-counting when
% one source's unknown checks are carried forward in the merged view and processed again
% during another source's update.
replaceUnknowns(OldChecks, NewChecks, Iterator, CountableKeys) ->
	case maps:next(Iterator) of
		{Key, NewCheck, NextIterator} ->
			NormalisedCheck = case maps:get(<<"ok">>, NewCheck, unknown) of
				unknown ->
					OldCheck = maps:get(Key, OldChecks, #{<<"ok">> => unknown}),
					OldCount = maps:get(<<"unknown_count">>, OldCheck, 0),
					NewCount = case sets:is_element(Key, CountableKeys) of
						true -> OldCount + 1;  % This check was in the current source update
						false -> OldCount  % Carried over from a previous source, don't re-increment
					end,
					IncrementedCheck = maps:put(<<"unknown_count">>, NewCount, NewCheck),
					case NewCount >= 3 of
						true ->
							maps:put(<<"ok">>, false, IncrementedCheck);
						false ->
							logger:notice("Not sending alert for ~p as there has only been ~p recurring failures so far.", [Key, NewCount]),
							maps:put(<<"ok">>, maps:get(<<"ok">>, OldCheck, unknown), IncrementedCheck)
					end;
				_ ->
					maps:put(<<"unknown_count">>, 0, NewCheck)
			end,
			maps:put(Key, NormalisedCheck, replaceUnknowns(OldChecks, NewChecks, NextIterator, CountableKeys));
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

% Applies failThreshold logic: holds the previous ok state until N consecutive
% failures are seen. The fail_count counter resets to 0 on recovery.
% failThreshold defaults to 1 (alert immediately on first failure).
applyFailThreshold(OldChecks, NewChecks) ->
	maps:map(fun(Key, NewCheck) ->
		case maps:get(<<"ok">>, NewCheck, unknown) of
			false ->
				FailThreshold = maps:get(<<"failThreshold">>, NewCheck, 1),
				OldCheck = maps:get(Key, OldChecks, #{}),
				OldFailCount = maps:get(<<"fail_count">>, OldCheck, 0),
				NewFailCount = OldFailCount + 1,
				CheckWithCount = maps:put(<<"fail_count">>, NewFailCount, NewCheck),
				case NewFailCount < FailThreshold of
					true ->
						% Not yet at threshold — hold the previous ok value
						OldOk = maps:get(<<"ok">>, OldCheck, unknown),
						maps:put(<<"ok">>, OldOk, CheckWithCount);
					false ->
						CheckWithCount
				end;
			_ ->
				% Healthy or unknown — reset the failure counter
				maps:put(<<"fail_count">>, 0, NewCheck)
		end
	end, NewChecks).

% Reduce monitoring flapiness by using existing check data to bolster new checks which may be facing a temporary blip.
% CountableKeys is the set of check keys from the current source update that are reporting unknown.
% Only those keys can increment their unknown_count — this prevents double-counting when checks
% from one source are carried forward in the merged view during another source's update.
normaliseChecks(OldChecks, NewChecks, CountableKeys) ->
	MergedChecks = mergeMissingInfoChecks(OldChecks, NewChecks),
	AfterUnknowns = replaceUnknowns(OldChecks, MergedChecks, maps:iterator(MergedChecks, reversed), CountableKeys),
	applyFailThreshold(OldChecks, AfterUnknowns).

% Decides whether the checks have changed in a meaningful way (ie ignore "unknown" states)
meaningfulChange(OldChecks, NewChecks) ->
	NewFailingChecks = failingChecks(NewChecks),
	OldFailingChecks = failingChecks(OldChecks),
	maps:keys(OldFailingChecks) /= maps:keys(NewFailingChecks).

failingChecks(Checks) ->
	maps:filter(fun(_, Check) ->
		maps:get(<<"ok">>, Check, unknown) == false
	end, Checks).

% Calls every notifier in Notifiers, catching any errors so a single
% failing notifier cannot prevent the others from running.
notify_all(Host, System, FailingNow, Suppressed, Metrics, Notifiers) ->
	lists:foreach(fun(NotifyFn) ->
		try NotifyFn(Host, System, FailingNow, Suppressed, Metrics)
		catch ExClass:ExReason ->
			logger:error("Notify failed for ~p on ~p: ~p ~p", [System, Host, ExClass, ExReason])
		end
	end, Notifiers).

state_change(Host, System, SystemChecks, SystemMetrics, SuppressionMap, Notifiers) ->
	AllFailing = failingChecks(SystemChecks),
	% Filter out checks whose dependsOn system is currently under an active suppression window.
	% Single-hop only: we never follow dependsOn chains transitively.
	FailingNow = maps:filter(fun(_, Check) ->
		not is_dependency_suppressed(Check, System, SuppressionMap)
	end, AllFailing),
	case maps:size(AllFailing) > 0 andalso maps:size(FailingNow) =:= 0 of
		true ->
			% All failing checks are dependency-suppressed — notify as suppressed (no email alert).
			logger:notice("All failing checks on ~p suppressed via dependency: ~p", [System, maps:keys(AllFailing)]),
			notify_all(Host, System, AllFailing, true, SystemMetrics, Notifiers),
			SuppressionMap;
		false ->
			% FailingNow contains only non-dep-suppressed checks. Apply system-level suppression logic.
			case maps:get(System, SuppressionMap, undefined) of
				undefined ->
					logger:notice("Checks' state changed for ~p on ~p", [System, Host]),
					notify_all(Host, System, FailingNow, false, SystemMetrics, Notifiers),
					SuppressionMap;
				ExpiryTime ->
					Now = erlang:system_time(second),
					case Now < ExpiryTime of
						true ->
							logger:notice("Alert suppressed for ~p during deploy window", [System]),
							notify_all(Host, System, FailingNow, true, SystemMetrics, Notifiers),
							SuppressionMap;
						false ->
							logger:error("Suppression window for ~p expired without being cleared - deploy may have taken longer than 10 minutes", [System]),
							logger:notice("Checks' state changed for ~p on ~p", [System, Host]),
							notify_all(Host, System, FailingNow, false, SystemMetrics, Notifiers),
							maps:remove(System, SuppressionMap)
					end
			end
	end.

% Returns true if this check's dependsOn system is currently under an active suppression window.
% Guards against self-references (CurrentSystem == dependsOn) and treats pending_verification
% as "suppression lifted" (returns false). Does NOT follow transitive dependsOn chains.
is_dependency_suppressed(Check, CurrentSystem, SuppressionMap) ->
	case maps:get(<<"dependsOn">>, Check, undefined) of
		undefined -> false;
		DependsOn when is_binary(DependsOn) ->
			DependsOnStr = binary_to_list(DependsOn),
			% Guard: ignore self-references to prevent circular evaluation
			case DependsOnStr =:= CurrentSystem of
				true -> false;
				false ->
					case maps:get(DependsOnStr, SuppressionMap, undefined) of
						undefined -> false;
						{pending_verification, _} -> false;  % Suppression has been lifted
						ExpiryTime ->
							Now = erlang:system_time(second),
							Now < ExpiryTime
					end
			end;
		_ -> false  % Non-binary dependsOn value — ignore
	end.

% Returns a list of system IDs whose normalised checks include a dependsOn pointing at TargetSystem.
% Used to cascade pending_verification when TargetSystem unsuppresses.
% Excludes TargetSystem itself (self-reference guard).
find_dependent_systems(TargetSystem, SystemMap) ->
	TargetBin = list_to_binary(TargetSystem),
	lists:usort(maps:fold(fun(_Host, {System, SourceChecksMap, _, _}, Acc) ->
		case System =:= TargetSystem of
			true -> Acc;  % Guard: skip self
			false ->
				MergedChecks = mergeSourceChecks(SourceChecksMap),
				HasDependency = maps:fold(fun(_, Check, Found) ->
					Found orelse (maps:get(<<"dependsOn">>, Check, undefined) =:= TargetBin)
				end, false, MergedChecks),
				case HasDependency of
					true -> [System | Acc];
					false -> Acc
				end
		end
	end, [], SystemMap)).

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").
	nomaliseChecks_test() ->
		CiCountable = sets:from_list([<<"ci">>], [{version, 2}]),
		FetchInfoCountable = sets:from_list([<<"fetch-info">>], [{version, 2}]),
		% Single unknown check: ci goes from ok to unknown (count 0→1, ok held as true)
		?assertEqual(#{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 1, <<"fail_count">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 0}}, normaliseChecks(#{<<"ci">> => #{<<"ok">> => true}}, #{<<"ci">> => #{<<"ok">> => unknown}, <<"fetch-info">> => #{<<"ok">> => true}}, CiCountable)),
		% Second consecutive unknown for ci (count 1→2, ok still held as true)
		?assertEqual(#{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 2, <<"fail_count">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 0}}, normaliseChecks(#{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 1}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 2}}, #{<<"ci">> => #{<<"ok">> => unknown}, <<"fetch-info">> => #{<<"ok">> => true}}, CiCountable)),
		% Third consecutive unknown for ci (count 2→3, ok flips to false and alerts)
		?assertEqual(#{<<"ci">> => #{<<"ok">> => false, <<"unknown_count">> => 3, <<"fail_count">> => 1}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 0}}, normaliseChecks(#{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 2}}, #{<<"ci">> => #{<<"ok">> => unknown}, <<"fetch-info">> => #{<<"ok">> => true}}, CiCountable)),
		% fetch-info goes unknown while other checks are carried forward from old state
		?assertEqual(#{<<"item-count">> => #{<<"ok">> => false, <<"unknown_count">> => 0, <<"fail_count">> => 1}, <<"api-check">> => #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 0}, <<"fetch-info">> => #{<<"ok">> => true,  <<"unknown_count">> => 1, <<"fail_count">> => 0}}, normaliseChecks(#{<<"item-count">> => #{<<"ok">> => false}, <<"api-check">> => #{<<"ok">> => true}, <<"fetch-info">> => #{<<"ok">> => true}}, #{<<"fetch-info">> => #{<<"ok">> => unknown}}, FetchInfoCountable)).

	meaningfulChange_test() ->
		?assertEqual(false, meaningfulChange(#{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 1}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0}}, #{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0}})),
		?assertEqual(true, meaningfulChange(#{<<"ci">> => #{<<"ok">> => false, <<"unknown_count">> => 3}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0}}, #{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 2}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0}})),
		?assertEqual(true, meaningfulChange(#{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 1}, <<"fetch-info">> => #{<<"ok">> => false, <<"unknown_count">> => 0}}, #{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0}})),
		?assertEqual(true, meaningfulChange(#{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0}}, #{<<"ci">> => #{<<"ok">> => false, <<"unknown_count">> => 0}, <<"fetch-info">> => #{<<"ok">> => false, <<"unknown_count">> => 4}})).

	systemExists_test() ->
		SystemMap = #{
			"host1.example.com" => {"lucos_foo", #{}, #{}, #{}},
			"host2.example.com" => {"lucos_bar", #{}, #{}, #{}}
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
	warmup_first_update_stores_state_test() ->
		InitialState = {#{}, #{}, []},
		Checks = #{<<"fetch-info">> => #{<<"ok">> => false}},
		{noreply, {SystemMap, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", info, Checks, #{}},
			InitialState
		),
		% Host should now be in the SystemMap
		?assert(maps:is_key("host1.example.com", SystemMap)),
		{"lucos_foo", SourceChecksMap, _, _} = maps:get("host1.example.com", SystemMap),
		StoredChecks = mergeSourceChecks(SourceChecksMap),
		?assertEqual(false, maps:get(<<"ok">>, maps:get(<<"fetch-info">>, StoredChecks))).

	% Second update for a known host triggers normal alert logic (not warm-up).
	% Here both updates report the same healthy state, so no meaningful change — no alert.
	warmup_second_update_not_suppressed_test() ->
		Checks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		ExistingState = {
			#{"host1.example.com" => {"lucos_foo", #{info => Checks}, #{}, #{}}},
			#{},
			[]
		},
		{noreply, {SystemMap, _, _}} = handle_cast(
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
			#{"host1.example.com" => {"lucos_foo", #{info => InfoChecks}, #{}, #{}}},
			#{},
			[]
		},
		% circleci update arrives
		{noreply, {SystemMap, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", circleci, CIChecks, #{}},
			ExistingState
		),
		{"lucos_foo", SourceChecksMap, _, _} = maps:get("host1.example.com", SystemMap),
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
			#{"host1.example.com" => {"lucos_foo", #{info => OldInfoChecks}, #{}, #{}}},
			#{},
			[]
		},
		{noreply, {SystemMap, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", info, NewInfoChecks, #{}},
			ExistingState
		),
		{"lucos_foo", SourceChecksMap, _, _} = maps:get("host1.example.com", SystemMap),
		Merged = mergeSourceChecks(SourceChecksMap),
		?assertNot(maps:is_key(<<"custom-check">>, Merged)).

	% When a source with no metrics (e.g. circleci) updates a host that already
	% has metrics from the info fetcher, the existing metrics are preserved.
	empty_metrics_do_not_overwrite_test() ->
		InfoChecks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		Metrics = #{<<"agent-count">> => #{<<"value">> => 42, <<"techDetail">> => <<"count">>}},
		ExistingState = {
			#{"host1.example.com" => {"lucos_foo", #{info => InfoChecks}, #{}, Metrics}},
			#{},
			[]
		},
		CIChecks = #{<<"circleci">> => #{<<"ok">> => true}},
		{noreply, {SystemMap, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", circleci, CIChecks, #{}},
			ExistingState
		),
		{"lucos_foo", _, _, StoredMetrics} = maps:get("host1.example.com", SystemMap),
		?assertEqual(Metrics, StoredMetrics).

	% When a source provides non-empty metrics, they replace the existing ones.
	nonempty_metrics_do_overwrite_test() ->
		OldMetrics = #{<<"agent-count">> => #{<<"value">> => 42, <<"techDetail">> => <<"count">>}},
		NewMetrics = #{<<"agent-count">> => #{<<"value">> => 99, <<"techDetail">> => <<"count">>}},
		ExistingState = {
			#{"host1.example.com" => {"lucos_foo", #{info => #{<<"fetch-info">> => #{<<"ok">> => true}}}, #{}, OldMetrics}},
			#{},
			[]
		},
		{noreply, {SystemMap, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", info, #{<<"fetch-info">> => #{<<"ok">> => true}}, NewMetrics},
			ExistingState
		),
		{"lucos_foo", _, _, StoredMetrics} = maps:get("host1.example.com", SystemMap),
		?assertEqual(NewMetrics, StoredMetrics).

	% Helper to build a recording notifier and retrieve what it captured.
	% The notifier sends {notified, Args} to the calling test process.
	recording_notifier(TestPid) ->
		fun(Host, System, FailingNow, Suppressed, Metrics) ->
			TestPid ! {notified, Host, System, FailingNow, Suppressed, Metrics}
		end.

	% Drain all pending {notified, ...} messages from the mailbox.
	drain_notifications() ->
		receive
			{notified, _, _, _, _, _} -> drain_notifications()
		after 0 ->
			ok
		end.

	% Unsuppressing a healthy system enters pending_verification and does NOT fire an immediate alert.
	unsuppress_healthy_system_test() ->
		HealthyChecks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		SystemMap = #{"host1.example.com" => {"lucos_foo", #{info => HealthyChecks}, #{}, #{}}},
		Notifier = recording_notifier(self()),
		State = {SystemMap, #{"lucos_foo" => erlang:system_time(second) + 600}, [Notifier]},
		{reply, ok, {_, NewSuppressionMap, _}} = handle_call(
			{unsuppress, "lucos_foo"}, from, State
		),
		% System should be in pending_verification, not removed from the map
		?assertMatch({pending_verification, _}, maps:get("lucos_foo", NewSuppressionMap)),
		receive
			{notified, _, _, _, _, _} -> ?assert(false, "Unexpected alert fired for healthy system")
		after 100 ->
			ok  % No immediate notification — correct
		end.

	% Unsuppressing an unhealthy system enters pending_verification; alert is deferred, not immediate.
	unsuppress_unhealthy_system_test() ->
		FailingChecks = #{<<"fetch-info">> => #{<<"ok">> => false}},
		SystemMap = #{"host1.example.com" => {"lucos_foo", #{info => FailingChecks}, #{}, #{}}},
		Notifier = recording_notifier(self()),
		State = {SystemMap, #{"lucos_foo" => erlang:system_time(second) + 600}, [Notifier]},
		drain_notifications(),
		{reply, ok, {_, NewSuppressionMap, _}} = handle_call(
			{unsuppress, "lucos_foo"}, from, State
		),
		% System should be in pending_verification (not cleared)
		?assertMatch({pending_verification, _}, maps:get("lucos_foo", NewSuppressionMap)),
		receive
			{notified, _, _, _, _, _} -> ?assert(false, "Alert must not fire immediately on unsuppress")
		after 100 ->
			ok  % No immediate notification — correct
		end.

	% Unsuppressing a system that isn't in the map is a no-op (idempotent).
	unsuppress_unknown_system_is_noop_test() ->
		State = {#{}, #{}, []},
		{reply, ok, {_, NewSuppressionMap, _}} = handle_call(
			{unsuppress, "lucos_unknown"}, from, State
		),
		?assertEqual(#{}, NewSuppressionMap).

	% After unsuppress, a fresh poll reporting unhealthy fires an alert and clears pending state.
	pending_verification_fires_alert_when_unhealthy_test() ->
		FailingChecks = #{<<"fetch-info">> => #{<<"ok">> => false}},
		SystemMap = #{"host1.example.com" => {"lucos_foo", #{info => FailingChecks}, #{}, #{}}},
		PendingSources = sets:from_list([info], [{version, 2}]),
		Notifier = recording_notifier(self()),
		drain_notifications(),
		State = {SystemMap, #{"lucos_foo" => {pending_verification, PendingSources}}, [Notifier]},
		{noreply, {_, NewSuppressionMap, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", info, FailingChecks, #{}},
			State
		),
		% Suppression entry should be cleared
		?assertEqual(#{}, NewSuppressionMap),
		receive
			{notified, "host1.example.com", "lucos_foo", FailingNow, false, _Metrics} ->
				?assert(maps:is_key(<<"fetch-info">>, FailingNow))
		after 100 ->
			?assert(false, "Expected alert was not fired after verification poll")
		end.

	% After unsuppress, a fresh poll reporting healthy clears pending state without alerting.
	pending_verification_no_alert_when_healthy_test() ->
		HealthyChecks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		SystemMap = #{"host1.example.com" => {"lucos_foo", #{info => HealthyChecks}, #{}, #{}}},
		PendingSources = sets:from_list([info], [{version, 2}]),
		Notifier = recording_notifier(self()),
		drain_notifications(),
		State = {SystemMap, #{"lucos_foo" => {pending_verification, PendingSources}}, [Notifier]},
		{noreply, {_, NewSuppressionMap, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", info, HealthyChecks, #{}},
			State
		),
		% Suppression entry should be cleared
		?assertEqual(#{}, NewSuppressionMap),
		receive
			{notified, _, _, _, _, _} -> ?assert(false, "No alert expected for healthy service after verify")
		after 100 ->
			ok
		end.

	% With two sources pending, the first poll keeps pending state; the second evaluates.
	pending_verification_waits_for_all_sources_test() ->
		FailingChecks = #{<<"fetch-info">> => #{<<"ok">> => false}},
		CIChecks = #{<<"circleci">> => #{<<"ok">> => false}},
		SystemMap = #{
			"host1.example.com" => {"lucos_foo", #{info => FailingChecks, circleci => CIChecks}, #{}, #{}}
		},
		PendingSources = sets:from_list([info, circleci], [{version, 2}]),
		Notifier = recording_notifier(self()),
		drain_notifications(),
		State = {SystemMap, #{"lucos_foo" => {pending_verification, PendingSources}}, [Notifier]},
		% First source (info) reports — should still be pending
		{noreply, State2} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", info, FailingChecks, #{}},
			State
		),
		{_, SuppressionMap2, _} = State2,
		?assertMatch({pending_verification, _}, maps:get("lucos_foo", SuppressionMap2)),
		receive
			{notified, _, _, _, _, _} -> ?assert(false, "No alert expected after first source only")
		after 100 ->
			ok
		end,
		% Second source (circleci) reports — should evaluate and alert
		{noreply, {_, SuppressionMap3, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", circleci, CIChecks, #{}},
			State2
		),
		?assertEqual(#{}, SuppressionMap3),
		receive
			{notified, "host1.example.com", "lucos_foo", _, false, _} -> ok
		after 100 ->
			?assert(false, "Expected alert after all sources reported")
		end.

	% With default failThreshold (1), a single failure is reported immediately.
	failThreshold_default_alerts_immediately_test() ->
		OldChecks = #{<<"db-check">> => #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 0}},
		NewChecks = #{<<"db-check">> => #{<<"ok">> => false}},
		% db-check is false (not unknown), so CountableKeys is empty
		Result = normaliseChecks(OldChecks, NewChecks, sets:new([{version, 2}])),
		?assertEqual(false, maps:get(<<"ok">>, maps:get(<<"db-check">>, Result))),
		?assertEqual(1, maps:get(<<"fail_count">>, maps:get(<<"db-check">>, Result))).

	% With failThreshold 3, failures 1 and 2 hold the previous ok state.
	% On the third consecutive failure, ok flips to false.
	failThreshold_holds_until_threshold_test() ->
		NewChecks = #{<<"db-check">> => #{<<"ok">> => false, <<"failThreshold">> => 3}},
		% db-check is false (not unknown), so CountableKeys is empty
		EmptyCountable = sets:new([{version, 2}]),
		% First failure — fail_count goes to 1, ok stays true (held from old)
		OldChecks1 = #{<<"db-check">> => #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 0}},
		Result1 = normaliseChecks(OldChecks1, NewChecks, EmptyCountable),
		?assertEqual(true, maps:get(<<"ok">>, maps:get(<<"db-check">>, Result1))),
		?assertEqual(1, maps:get(<<"fail_count">>, maps:get(<<"db-check">>, Result1))),
		% Second failure — fail_count goes to 2, still held
		Result2 = normaliseChecks(Result1, NewChecks, EmptyCountable),
		?assertEqual(true, maps:get(<<"ok">>, maps:get(<<"db-check">>, Result2))),
		?assertEqual(2, maps:get(<<"fail_count">>, maps:get(<<"db-check">>, Result2))),
		% Third failure — fail_count goes to 3, now ok flips to false
		Result3 = normaliseChecks(Result2, NewChecks, EmptyCountable),
		?assertEqual(false, maps:get(<<"ok">>, maps:get(<<"db-check">>, Result3))),
		?assertEqual(3, maps:get(<<"fail_count">>, maps:get(<<"db-check">>, Result3))).

	% Recovery (ok: true) resets fail_count to 0.
	failThreshold_recovery_resets_count_test() ->
		OldChecks = #{<<"db-check">> => #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 2, <<"failThreshold">> => 3}},
		NewChecks = #{<<"db-check">> => #{<<"ok">> => true, <<"failThreshold">> => 3}},
		% db-check is true (not unknown), so CountableKeys is empty
		Result = normaliseChecks(OldChecks, NewChecks, sets:new([{version, 2}])),
		?assertEqual(true, maps:get(<<"ok">>, maps:get(<<"db-check">>, Result))),
		?assertEqual(0, maps:get(<<"fail_count">>, maps:get(<<"db-check">>, Result))).

	% Bug fix: when circleci returns 404 (no CI for this repo), the circleci check must be
	% removed from the normalised cache — even when info is already unavailable (fetch-info = unknown).
	% The critical ordering: info must go unknown FIRST, then circleci gets a 404.
	% Without the fix, mergeMissingInfoChecks would see fetch-info=unknown, do
	% maps:merge(OldNormalisedCache_with_circleci, NewMergedChecks), and resurrect circleci.
	% With the fix, the empty SourceChecks triggers removal of circleci from OldNormalisedCache
	% before mergeMissingInfoChecks runs, so it cannot be resurrected.
	circleci_404_wipes_check_even_when_info_unavailable_test() ->
		InfoChecks = #{<<"fetch-info">> => #{<<"ok">> => true}, <<"tls-certificate">> => #{<<"ok">> => true}},
		CIChecks = #{<<"circleci">> => #{<<"ok">> => true}},
		% Start with a system already known and healthy, with both sources reporting
		ExistingNormalisedCache = #{
			<<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 0},
			<<"tls-certificate">> => #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 0},
			<<"circleci">> => #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 0}
		},
		ExistingState = {
			#{"host1.example.com" => {"lucos_foo", #{info => InfoChecks, circleci => CIChecks}, ExistingNormalisedCache, #{}}},
			#{},
			[]
		},
		% Info becomes temporarily unavailable FIRST (fetch-info = unknown)
		{noreply, State2} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", info, #{<<"fetch-info">> => #{<<"ok">> => unknown}, <<"tls-certificate">> => #{<<"ok">> => true}}, #{}},
			ExistingState
		),
		% THEN circleci returns 404 — no CI configured, sends #{}
		{noreply, State3} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", circleci, #{}, #{}},
			State2
		),
		{SystemMap3, _, _} = State3,
		{"lucos_foo", _, NormalisedAfterCI404, _} = maps:get("host1.example.com", SystemMap3),
		% circleci check must be absent — 404 means "no CI", not "CI unknown"
		?assertNot(maps:is_key(<<"circleci">>, NormalisedAfterCI404),
			"circleci check must be wiped by 404, not resurrected by mergeMissingInfoChecks").

	% When /_info returns an unknown fetch-info (transient blip), other failing checks from
	% that same source must be preserved in the normalised cache. If they disappear and then
	% reappear when info recovers, a duplicate alert fires — we should only alert once.
	info_blip_preserves_failing_checks_test() ->
		InfoChecks = #{
			<<"fetch-info">> => #{<<"ok">> => true},
			<<"tls-certificate">> => #{<<"ok">> => false},
			<<"item-count">> => #{<<"ok">> => false}
		},
		% Start with a system with two failing checks already in the normalised cache
		ExistingNormalisedCache = #{
			<<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 0},
			<<"tls-certificate">> => #{<<"ok">> => false, <<"unknown_count">> => 0, <<"fail_count">> => 1},
			<<"item-count">> => #{<<"ok">> => false, <<"unknown_count">> => 0, <<"fail_count">> => 1}
		},
		ExistingState = {
			#{"host1.example.com" => {"lucos_foo", #{info => InfoChecks}, ExistingNormalisedCache, #{}}},
			#{},
			[]
		},
		% /_info has a transient blip: only fetch-info comes back (unknown), tls-certificate
		% and item-count are absent from the response
		{noreply, State2} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", info, #{<<"fetch-info">> => #{<<"ok">> => unknown}}, #{}},
			ExistingState
		),
		{SystemMap2, _, _} = State2,
		{"lucos_foo", _, NormalisedAfterBlip, _} = maps:get("host1.example.com", SystemMap2),
		% Both failing checks must still be present — they should NOT disappear during the blip
		?assert(maps:is_key(<<"tls-certificate">>, NormalisedAfterBlip),
			"tls-certificate must persist during transient /_info blip"),
		?assert(maps:is_key(<<"item-count">>, NormalisedAfterBlip),
			"item-count must persist during transient /_info blip").

	% Bug fix: multiple source updates in the same monitoring cycle must not double-increment unknown_count.
	% Scenario: circleci reports unknown, then info reports ok in the same cycle.
	% The circleci check is carried forward in the merged view during info's update.
	% With the fix, circleci's count should only increment when circleci itself reports — not when info reports.
	multiple_sources_same_cycle_no_double_increment_test() ->
		Notifiers = [],
		% Start with a system already known (non-first-seen)
		InfoChecks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		CIChecks = #{<<"circleci">> => #{<<"ok">> => true}},
		ExistingState = {
			#{"host1.example.com" => {"lucos_foo", #{info => InfoChecks, circleci => CIChecks}, #{}, #{}}},
			#{},
			Notifiers
		},
		% circleci reports unknown
		{noreply, State2} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", circleci, #{<<"circleci">> => #{<<"ok">> => unknown}}, #{}},
			ExistingState
		),
		{SystemMap2, _, _} = State2,
		{"lucos_foo", _, NormalisedAfterCI, _} = maps:get("host1.example.com", SystemMap2),
		?assertEqual(1, maps:get(<<"unknown_count">>, maps:get(<<"circleci">>, NormalisedAfterCI, #{}), -1)),
		% Now info reports (ok, no change) — circleci check is carried over in the merged view
		{noreply, State3} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", info, InfoChecks, #{}},
			State2
		),
		{SystemMap3, _, _} = State3,
		{"lucos_foo", _, NormalisedAfterInfo, _} = maps:get("host1.example.com", SystemMap3),
		% circleci count must still be 1, NOT 2 — info's update must not re-increment it
		?assertEqual(1, maps:get(<<"unknown_count">>, maps:get(<<"circleci">>, NormalisedAfterInfo, #{}), -1)).

	% is_dependency_suppressed: check with no dependsOn field → false
	is_dependency_suppressed_no_field_test() ->
		?assertEqual(false, is_dependency_suppressed(#{<<"ok">> => false}, "lucos_foo", #{})).

	% is_dependency_suppressed: dependsOn system is not in SuppressionMap → false
	is_dependency_suppressed_system_not_suppressed_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>},
		?assertEqual(false, is_dependency_suppressed(Check, "lucos_time", #{})).

	% is_dependency_suppressed: dependsOn system has an active suppression window → true
	is_dependency_suppressed_active_suppression_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>},
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_eolas" => FutureExpiry},
		?assertEqual(true, is_dependency_suppressed(Check, "lucos_time", SuppressionMap)).

	% is_dependency_suppressed: dependsOn system is in pending_verification (suppression lifted) → false
	is_dependency_suppressed_pending_verification_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>},
		PendingSources = sets:from_list([info], [{version, 2}]),
		SuppressionMap = #{"lucos_eolas" => {pending_verification, PendingSources}},
		?assertEqual(false, is_dependency_suppressed(Check, "lucos_time", SuppressionMap)).

	% is_dependency_suppressed: self-reference guard — dependsOn points to the same system → false
	is_dependency_suppressed_self_reference_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_foo">>},
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_foo" => FutureExpiry},
		?assertEqual(false, is_dependency_suppressed(Check, "lucos_foo", SuppressionMap)).

	% is_dependency_suppressed: suppression window has expired → false
	is_dependency_suppressed_expired_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>},
		PastExpiry = erlang:system_time(second) - 1,
		SuppressionMap = #{"lucos_eolas" => PastExpiry},
		?assertEqual(false, is_dependency_suppressed(Check, "lucos_time", SuppressionMap)).

	% find_dependent_systems: returns system IDs with checks declaring dependsOn TargetSystem
	find_dependent_systems_basic_test() ->
		SystemMap = #{
			"host1.example.com" => {"lucos_time", #{info => #{
				<<"eolas">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>}
			}}, #{}, #{}},
			"host2.example.com" => {"lucos_arachne", #{info => #{
				<<"triplestore">> => #{<<"ok">> => true}
			}}, #{}, #{}}
		},
		?assertEqual(["lucos_time"], find_dependent_systems("lucos_eolas", SystemMap)).

	% find_dependent_systems: no systems depend on target → empty list
	find_dependent_systems_none_test() ->
		SystemMap = #{
			"host1.example.com" => {"lucos_time", #{info => #{
				<<"eolas">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>}
			}}, #{}, #{}}
		},
		?assertEqual([], find_dependent_systems("some.other.system", SystemMap)).

	% find_dependent_systems: self-reference is excluded
	find_dependent_systems_excludes_self_test() ->
		SystemMap = #{
			"host1.example.com" => {"lucos_eolas", #{info => #{
				<<"db">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>}
			}}, #{}, #{}}
		},
		?assertEqual([], find_dependent_systems("lucos_eolas", SystemMap)).

	% find_dependent_systems: multiple systems can depend on the same target
	find_dependent_systems_multiple_test() ->
		SystemMap = #{
			"host1.example.com" => {"lucos_time", #{info => #{
				<<"eolas">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>}
			}}, #{}, #{}},
			"host2.example.com" => {"lucos_arachne", #{info => #{
				<<"eolas-data">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>}
			}}, #{}, #{}}
		},
		Deps = find_dependent_systems("lucos_eolas", SystemMap),
		?assertEqual(["lucos_arachne", "lucos_time"], lists:sort(Deps)).

	% When all failing checks have an active dependsOn suppression, no alert email is sent.
	state_change_all_dep_suppressed_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_eolas" => FutureExpiry},
		SystemChecks = #{
			<<"eolas">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>, <<"fail_count">> => 0, <<"unknown_count">> => 0}
		},
		Notifier = recording_notifier(self()),
		drain_notifications(),
		state_change("host1.example.com", "lucos_time", SystemChecks, #{}, SuppressionMap, [Notifier]),
		receive
			{notified, "host1.example.com", "lucos_time", _FailingChecks, true, _} ->
				ok  % Suppressed alert — correct
		after 100 ->
			?assert(false, "Expected suppressed notification")
		end.

	% When only some checks are dep-suppressed, the non-suppressed ones still alert.
	state_change_partial_dep_suppressed_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_eolas" => FutureExpiry},
		SystemChecks = #{
			<<"eolas">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>, <<"fail_count">> => 0, <<"unknown_count">> => 0},
			<<"db">> => #{<<"ok">> => false, <<"fail_count">> => 0, <<"unknown_count">> => 0}
		},
		Notifier = recording_notifier(self()),
		drain_notifications(),
		state_change("host1.example.com", "lucos_time", SystemChecks, #{}, SuppressionMap, [Notifier]),
		receive
			{notified, "host1.example.com", "lucos_time", FailingNow, false, _} ->
				% Only db should alert, not eolas
				?assert(maps:is_key(<<"db">>, FailingNow)),
				?assertNot(maps:is_key(<<"eolas">>, FailingNow))
		after 100 ->
			?assert(false, "Expected alert on non-dep-suppressed checks")
		end.

	% unsuppress cascades pending_verification to systems with checks depending on the unsuppressed system.
	unsuppress_cascades_pending_verification_test() ->
		% lucos_time has a check with dependsOn: lucos_eolas
		% lucos_eolas is being unsuppressed
		TimeChecks = #{<<"eolas">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>}},
		SystemMap = #{
			"lucos_eolas" => {"lucos_eolas", #{info => #{<<"fetch-info">> => #{<<"ok">> => true}}}, #{}, #{}},
			"schedule-tracker.l42.eu" => {"lucos_time", #{info => TimeChecks}, #{}, #{}}
		},
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_eolas" => FutureExpiry},
		State = {SystemMap, SuppressionMap, []},
		{reply, ok, {_, NewSuppressionMap, _}} = handle_call(
			{unsuppress, "lucos_eolas"}, from, State
		),
		% lucos_eolas itself should be in pending_verification
		?assertMatch({pending_verification, _}, maps:get("lucos_eolas", NewSuppressionMap)),
		% lucos_time (dependent) should also be in pending_verification
		?assertMatch({pending_verification, _}, maps:get("lucos_time", NewSuppressionMap)).

-endif.
