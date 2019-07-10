-module(server).
-export([start/0, accept/2, handleRequest/1]).

start() ->
	{Port, _} = string:to_integer(os:getenv("PORT", "8080")),
	SchedulerCount = erlang:system_info(schedulers),
	Opts = [{active, false},
			binary,
			{packet, http_bin}],
	{ok, ListenSocket} = gen_tcp:listen(Port, Opts),
	Spawn = fun(SchedulerID) ->	
		spawn_opt(?MODULE, accept, [ListenSocket, SchedulerID], [link, {scheduler, SchedulerID}])
	end,
	lists:foreach(Spawn, lists:seq(1, SchedulerCount)),
	io:format("server listening on port ~b with ~b schedulers~n", [Port, SchedulerCount]),
	receive
		Any -> io:format("~p~n", [Any])
	end.

accept(ListenSocket, SchedulerID) ->
	case gen_tcp:accept(ListenSocket) of
		{ok, Socket} -> spawn_opt(?MODULE, handleRequest, [Socket], [{scheduler, SchedulerID}]);
		Error	-> erlang:error(Error)
	end,
	accept(ListenSocket, SchedulerID).

handleRequest(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, http_eoh} ->
			Response = <<"HTTP/1.1 200 OK\nContent-Length: 12\n\nhello world!\n">>,
			gen_tcp:send(Socket, Response),
			gen_tcp:close(Socket),
			ok;

		{ok, _Data} ->
			handleRequest(Socket);

		Error ->
			Error
	end.