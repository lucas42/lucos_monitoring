-module(monitoring_state_server).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_cast/2, handle_call/3]).

start_link() -> gen_server:start_link(?MODULE, [], []).

init([]) ->
	ChecksMap = #{},
	MetricsMap = #{},
	{ok, {ChecksMap, MetricsMap}}.

handle_cast(Request, {ChecksMap, MetricsMap}) ->
	case Request of
		{updateSystem, Host, System, SystemChecks, SystemMetrics} ->
			io:format("Received update for system ~p (Host ~p)~n", [System, Host]),
			NewChecksMap = maps:put(Host, {success, System, SystemChecks}, ChecksMap),
			NewMetricsMap = maps:put(Host, {success, System, SystemMetrics}, MetricsMap),
			{noreply, {NewChecksMap, NewMetricsMap}};
		{systemError, Host, Error} ->
			io:format("Hit error on host ~p~n", [Host]),
			NewChecksMap = maps:put(Host, {error, Error}, ChecksMap),
			NewMetricsMap = maps:put(Host, {error, Error}, MetricsMap),
			{noreply, {NewChecksMap, NewMetricsMap}}
	end.

handle_call(Request, _From, State) ->
	{ChecksMap, MetricsMap} = State,
	case Request of
		{fetch, checks, all} ->
			{reply, ChecksMap, State};
		{fetch, metrics, all} ->
			{reply, MetricsMap, State};
		{fetch, checks, Host} ->
			{reply, maps:get(Host, ChecksMap), State};
		{fetch, metrics, Host} ->
			{reply, maps:get(Host, MetricsMap), State}
	end.