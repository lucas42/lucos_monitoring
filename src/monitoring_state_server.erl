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