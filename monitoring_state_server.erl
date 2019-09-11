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
			if
				% If no Checks were previously stored for this system, take no action
				OldSystemChecks == nil ->
					ok;
				% If checks are same as last time, take no action
				OldSystemChecks == SystemChecks ->
					ok;
				true ->
					state_change(Host, System, SystemChecks, SystemMetrics)
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

state_change(Host, System, SystemChecks, SystemMetrics) ->
	io:format("Checks' state changed for ~p on ~p~n", [System, Host]),
	notifier:notify(Host, System, SystemChecks, SystemMetrics).