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
			{_, OldSystemChecks, _} = maps:get(Host, SystemMap, {nil, nil, nil}),
			case meaningfulChange(OldSystemChecks, SystemChecks) of
				true ->
					state_change(Host, System, SystemChecks, SystemMetrics);
				false ->
					ok
			end,
			NewSystemMap = maps:put(Host, {System, SystemChecks, SystemMetrics}, SystemMap),
			{noreply, NewSystemMap}
	end.

handle_call(Request, _From, SystemMap) ->
	case Request of
		{fetch, all} ->
			{reply, SystemMap, SystemMap};
		{fetch, Host} ->
			{reply, maps:get(Host, SystemMap), SystemMap}
	end.

% Decides whether the checks have changed in a meaningful way (ie ignore "unknown" states)
meaningfulChange(OldChecks, NewChecks) ->
	NewFailingChecks = failingChecks(NewChecks),
	OldFailingChecks = failingChecks(OldChecks),
	maps:keys(OldFailingChecks) /= maps:keys(NewFailingChecks).

failingChecks(Checks) ->
	case Checks of
		nil -> maps:new();
		_ ->
			maps:filter(fun(_, Check) ->
				maps:get(<<"ok">>, Check, unknown) == false
			end, Checks)
	end.

state_change(Host, System, SystemChecks, SystemMetrics) ->
	io:format("Checks' state changed for ~p on ~p~n", [System, Host]),
	notifier:notify(Host, System, failingChecks(SystemChecks), SystemMetrics).