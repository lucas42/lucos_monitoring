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
		{ok, {http_request, Method, {abs_path, RequestUri}, _Version}} ->
			handleRequest(Socket, Method, binary_to_list(RequestUri), StatePid);
		Error ->
			Error
	end.
handleRequest(Socket, Method, RequestUri, StatePid) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, http_eoh} ->
			DateTime = calendar:system_time_to_rfc3339(erlang:system_time(second)),
			ClientIP = getClientIP(Socket),
			{StatusCode, ContentType, Body} = tryController(Method, RequestUri, StatePid),
			Response = getHeaders(StatusCode, ContentType) ++ Body,
			gen_tcp:send(Socket, Response),
			gen_tcp:close(Socket),
			io:format("~p ~p ~p ~p ~p~n", [ClientIP, DateTime, Method, StatusCode, RequestUri]),
			ok;
		{ok, _Data} ->
			handleRequest(Socket, Method, RequestUri, StatePid);

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
		404 -> "Not Found";
		500 -> "Internal Error"
	end.

renderSystemChecks(SystemChecks) ->
	{Html, Healthy, CheckCount} = maps:fold(
		fun (CheckId, CheckInfo, {Html, Healthy, CheckCount}) ->
			CheckHealthy = maps:get(<<"ok">>, CheckInfo, false),
			TechDetail = binary_to_list(maps:get(<<"techDetail">>, CheckInfo, <<"-">>)),
			Debug = binary_to_list(maps:get(<<"debug">>, CheckInfo, <<"">>)),
			CheckHtml = "
				<tr class=\""++getCssClass("check", CheckHealthy)++"\">
					<td class=\"checkid\">"++binary_to_list(CheckId)++"</td>
					<td class=\"status\">"++atom_to_list(CheckHealthy)++"</td>
					<td class=\"techDetail\">"++TechDetail++"</td>
					<td class=\"debug\">"++Debug++"</td>
				</tr>
			",
			{Html++CheckHtml, (Healthy and CheckHealthy), CheckCount+1}
		end, {"", true, 0}, SystemChecks),
	case CheckCount of
		0 ->
			{Healthy, "<span class=\"empty\">No checks found</span>"};
		_Count ->
			{Healthy, "
				<table>
					<thead><td>Check</td><td>Status</td><td>Technical Detail</td><td class=\"debug\">Debug</td></thead>
					"++Html++"
				</table>"}
	end.

renderSystemMetrics(SystemMetrics) ->
	Html = maps:fold(
		fun (MetricId, MetricInfo, Html) ->
			Value = maps:get(<<"value">>, MetricInfo, -1),
			TechDetail = binary_to_list(maps:get(<<"techDetail">>, MetricInfo, <<"">>)),
			MetricHtml = "
				<tr class=\"metric\" title=\""++TechDetail++"\">
					<td class=\"metricid\">"++binary_to_list(MetricId)++"</td>
					<td class=\"value\">"++integer_to_list(Value)++"</td>
				</tr>
			",
			Html++MetricHtml
		end, "", SystemMetrics),
	case Html of
		"" -> "";
		_ ->
			"
			<table class=\"metrics\">
				<thead><td>Metric</td><td>Value</td></thead>
				"++Html++"
			</table>"
	end.

getCssClass(Type, Healthy) ->
	case Healthy of
		true -> Type ++ " healthy";
		false -> Type ++ " erroring"
	end.

renderSystemHeader(System, Host) ->
	InfoURL = "https://" ++ Host ++ "/_info",
	case System of
		unknown ->
			"<h2>
				"++Host++"
				<a href=\""++InfoURL++"\" target=\"_blank\" class=\"rawInfoURL\">&#128279;</a>
			</h2>";
		System ->
			Name = re:replace(System, "_", " ", [global, {return,list}]),
			"<h2 class=\"system-name\">
				"++Name++"
				<a href=\""++InfoURL++"\" target=\"_blank\" class=\"rawInfoURL\">&#128279;</a>
			</h2>"
	end.

renderAll(Systems) ->
	maps:fold(
		fun (Host, {System, SystemChecks, SystemMetrics}, Output) ->
			{Healthy, SystemChecksHtml} = renderSystemChecks(SystemChecks),
			Output++"
			<div class=\""++getCssClass("system", Healthy)++"\">
				"++renderSystemHeader(System, Host)++"
				"++SystemChecksHtml++"
				"++renderSystemMetrics(SystemMetrics)++"
			</div>
			"
		end, "", Systems).

controller(_Method, RequestUri, StatePid) ->
	Path = re:replace(RequestUri, "\\?.*$", "", [{return,list}]),
	case Path of
		"/" ->
			Systems = gen_server:call(StatePid, {fetch, all}),
			ChecksOutput = renderAll(Systems),
			{200, "text/html", "<html><head><title>Lucos Monitoring</title><link rel=\"stylesheet\" type=\"text/css\" href=\"/style.css\" /></head><body><h1>Monitoring for Lucos Services</h1>" ++ ChecksOutput ++ "</body></html>"};
		"/style.css" ->
			{200, "text/css", "
			.system h2 { background-color: #666; color: #fff; padding: 0.1em 1em; border-radius: 0.2em; }
			h2.system-name { text-transform:capitalize; }
			h2 .rawInfoURL { float: right; text-decoration: none; }
			.empty { font-style: italic; }
			table { border-collapse: collapse; }
			td { border: none thin #ccc; padding: 0.2em 1em; }
			thead td { font-weight: bold; border-bottom-style: solid; }
			tr > td:not(:first-child) { border-left-style: solid; }
			tr:not(:last-child) > td { border-bottom-style: solid; }
			tr.check td.status { background-color: #666; color: #fff; }
			.system.healthy h2, tr.check.healthy td.status { background-color: #060; }
			.system.erroring h2, tr.check.erroring td.status { background-color: #900; }
			.system.healthy .debug { display: none; }
			.metrics { margin-top: 2em; }
			"};
		_ ->
			{404, "text/plain", "Not Found"}
	end.

tryController(Method, RequestUri, StatePid) ->
	try controller(Method, RequestUri, StatePid) of
		Response -> Response
	catch
		ExceptionClass:Term:StackTrace ->
			io:format("ExceptionClass: ~p Term: ~p StackTrace: ~p~n", [ExceptionClass, Term, StackTrace]),
			{500, "text/plain", "An Error occurred whilst generating this page."}
	end.