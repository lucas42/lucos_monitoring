-module(server).
-export([start/0, accept/3, handleRequest/2]).

start() ->
	{Port, _} = string:to_integer(os:getenv("PORT", "8080")),
	{ok, StatePid} = monitoring_state_server:start_link(),
	SchedulerCount = erlang:system_info(schedulers),
	Opts = [{active, false},
			binary,
			{packet, http_bin}],
	{ok, ListenSocket} = gen_tcp:listen(Port, Opts),
	Spawn = fun(SchedulerID) ->	
		spawn_opt(?MODULE, accept, [ListenSocket, SchedulerID, StatePid], [link, {scheduler, SchedulerID}])
	end,
	lists:foreach(Spawn, lists:seq(1, SchedulerCount)),
	io:format("server listening on port ~b with ~b schedulers~n", [Port, SchedulerCount]),
	fetcher:start(StatePid),
	receive
		Any -> io:format("~p~n", [Any])
	end.

accept(ListenSocket, SchedulerID, StatePid) ->
	case gen_tcp:accept(ListenSocket) of
		{ok, Socket} -> spawn_opt(?MODULE, handleRequest, [Socket, StatePid], [{scheduler, SchedulerID}]);
		Error	-> erlang:error(Error)
	end,
	accept(ListenSocket, SchedulerID, StatePid).

handleRequest(Socket, StatePid) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, {http_request, Method, {abs_path, Path}, _Version}} ->
			handleRequest(Socket, Method, binary_to_list(Path), StatePid);
		Error ->
			Error
	end.
handleRequest(Socket, Method, Path, StatePid) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, http_eoh} ->
			DateTime = calendar:system_time_to_rfc3339(erlang:system_time(second)),
			ClientIP = getClientIP(Socket),
			{StatusCode, ContentType, Body} = controller(Method, Path, StatePid),
			Response = getHeaders(StatusCode, ContentType) ++ Body,
			gen_tcp:send(Socket, Response),
			gen_tcp:close(Socket),
			io:format("~p ~p ~p ~p ~p~n", [ClientIP, DateTime, Method, StatusCode, Path]),
			ok;
		{ok, _Data} ->
			handleRequest(Socket, Method, Path, StatePid);

		Error ->
			Error
	end.

getClientIP(Socket) ->
	{ok, {ClientIP, _}} = inet:peername(Socket),
	inet:ntoa(ClientIP).

getHeaders(StatusCode, ContentType) ->
	getStatusLine(StatusCode) ++
		"ContentType: " ++ ContentType ++ "\n" ++
		"\n".

getStatusLine(StatusCode) ->
	"HTTP/1.1 " ++ integer_to_list(StatusCode) ++ " " ++ getReasonPhrase(StatusCode) ++ "\n".

getReasonPhrase(StatusCode) ->
	case StatusCode of
		200 -> "OK";
		404 -> "Not Found"
	end.

stringifyError(Error) ->
	case Error of
		{http_error, {StatusCode, ReasonPhrase}} ->
			"Received HTTP response with status "++integer_to_list(StatusCode)++" "++ReasonPhrase;
		{failed_connect, [{to_address, {Host, _Port}}, {inet,[inet],nxdomain}]} ->
			"DNS failure when trying to resolve "++Host;
		{failed_connect, [{to_address, {Host, Port}}, {inet,[inet],econnrefused}]} ->
			"Failed to establish a TCP connection to host "++Host++" on port "++integer_to_list(Port);
		{ErrorType, _Details} ->
			"An unknown error of type "++atom_to_list(ErrorType)++" occured: "++lists:flatten(io_lib:format("~p",[Error]))
	end.

controller(_Method, Path, StatePid) ->
	case Path of
		"/" ->
			Checks = gen_server:call(StatePid, {fetch, checks, all}),
			io:format("Render checks: ~p~n", [Checks]),
			ChecksOutput = maps:fold(
				fun (Host, Data, Output) ->
					case Data of
						{success, System, SystemChecks} ->
							Output++"<h2>"++System++"</h2>\n";
						{error, Error} ->
							Output++"<h2 class=\"error\">"++Host++"</h2>\n<p>"++stringifyError(Error)++"</p>"
					end
				end, "", Checks),
			{200, "text/html", "<html><head><title>Lucos Monitoring</title></head><body><h1>Monitoring for lucos services</h1>" ++ ChecksOutput ++ "</body></html>"};
		_ ->
			{404, "text/plain", "Not Found"}
	end.