-module(monitoring_state_server).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_cast/2, handle_call/3]).

start_link() -> gen_server:start_link(?MODULE, [], []).

init([]) ->
	SystemMap = #{},
	{ok, SystemMap}.

handle_cast(Request, SystemMap) ->
	case Request of
		{updateSystem, Host, System, SystemChecks, SystemMetrics} ->
			io:format("Received update for system ~p (Host ~p)~n", [System, Host]),
			{_, OldSystemChecks, _} = maps:get(Host, SystemMap, {nil, maps:new(), nil}),
			NormalisedChecks = replaceUnknowns(OldSystemChecks, SystemChecks, maps:iterator(SystemChecks, reversed)),
			case meaningfulChange(OldSystemChecks, NormalisedChecks) of
				true ->
					state_change(Host, System, NormalisedChecks, SystemMetrics);
				false ->
					ok
			end,
			NewSystemMap = maps:put(Host, {System, NormalisedChecks, SystemMetrics}, SystemMap),
			{noreply, NewSystemMap}
	end.

handle_call(Request, _From, SystemMap) ->
	case Request of
		{fetch, all} ->
			{reply, SystemMap, SystemMap};
		{fetch, Host} ->
			{reply, maps:get(Host, SystemMap), SystemMap}
	end.

% Replaces any unknown "ok" with whichever the value was there previously.  Also keeps a tally of how many unknowns have been received in a row
replaceUnknowns(OldChecks, NewChecks, Iterator) ->
	case maps:next(Iterator) of
		{Key, NewCheck, NextIterator} ->
			NormalisedCheck = case maps:get(<<"ok">>, NewCheck, unknown) of
				unknown ->
					OldCheck = maps:get(Key, OldChecks, #{<<"ok">> => unknown}),
					OldOK = maps:get(<<"ok">>, OldCheck, unknown),
					OldCount = maps:get(<<"unknown_count">>, OldCheck, 0),
					NewOK = OldOK,
					NewCount = OldCount + 1,
					maps:put(<<"unknown_count">>, NewCount, maps:put(<<"ok">>, NewOK, NewCheck));
				_ ->
					maps:put(<<"unknown_count">>, 0, NewCheck)
			end,
			maps:put(Key, NormalisedCheck, replaceUnknowns(OldChecks, NewChecks, NextIterator));
		none ->
			maps:new()
	end.

% Decides whether the checks have changed in a meaningful way (ie ignore "unknown" states)
meaningfulChange(OldChecks, NewChecks) ->
	NewFailingChecks = failingChecks(NewChecks),
	OldFailingChecks = failingChecks(OldChecks),
	maps:keys(OldFailingChecks) /= maps:keys(NewFailingChecks).

failingChecks(Checks) ->
	maps:filter(fun(_, Check) ->
		maps:get(<<"ok">>, Check, unknown) == false
	end, Checks).

state_change(Host, System, SystemChecks, SystemMetrics) ->
	io:format("Checks' state changed for ~p on ~p~n", [System, Host]),
	notifier:notify(Host, System, failingChecks(SystemChecks), SystemMetrics).