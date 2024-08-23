-module(server).
-export([start/2, accept/3, handleRequest/2]).

start(_StartType, _StartArgs) ->
	try
		{Port, _} = string:to_integer(os:getenv("PORT", "8080")),
		{ok, StatePid} = monitoring_state_server:start_link(),
		SchedulerCount = erlang:system_info(schedulers),
		Opts = [{active, false},
				binary,
				{packet, http_bin}],
		case gen_tcp:listen(Port, Opts) of
			{ok, ListenSocket} ->
				Spawn = fun(SchedulerID) ->
					spawn_opt(?MODULE, accept, [ListenSocket, SchedulerID, StatePid], [link, {scheduler, SchedulerID}])
				end,
				lists:foreach(Spawn, lists:seq(1, SchedulerCount)),
				io:format("server listening on port ~b with ~b schedulers~n", [Port, SchedulerCount]),
				fetcher:start(StatePid),
				receive
					Any -> io:format("~p~n", [Any])
				end;
			{error, Error} ->
				io:format("Can't listen on port ~p: ~p ~n",[Port, Error])
		end
	catch
		Exception:Reason -> io:format("Startup error occured: ~p ~p ~n",[Exception, Reason])
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
		"Content-Type: " ++ ContentType ++ "\n" ++
		"\n".

getStatusLine(StatusCode) ->
	"HTTP/1.1 " ++ integer_to_list(StatusCode) ++ " " ++ getReasonPhrase(StatusCode) ++ "\n".

getReasonPhrase(StatusCode) ->
	case StatusCode of
		200 -> "OK";
		404 -> "Not Found";
		500 -> "Internal Error"
	end.


renderCheckStatus(Health, Link) ->
	case Link of
		"" -> atom_to_list(Health);
		_ ->
			"<a href=\""++Link++"\" target=\"_blank\">"++atom_to_list(Health)++"</a>"
	end.

formatStringFromInfo(Key, CheckInfo) ->
	formatString(Key, maps:get(Key, CheckInfo, <<"">>)).

formatString(Key, BinaryValue) ->
	RawValue = binary_to_list(BinaryValue),
	ZWSValue = re:replace(RawValue, "_", "\\&ZeroWidthSpace;_",[global, {return, list}]),
	LinkedValue = re:replace(ZWSValue, "https?:\\S+", "<a href=\"&\" target=\"_blank\">&</a>",[global, {return, list}]),
	Value = re:replace(LinkedValue, "href=\"([^\"]*)&ZeroWidthSpace;([^\"]*)\"", "href=\"\\g1\\g2\"",[global, {return, list}]),
	"<td class=\"formattedString "++binary_to_list(Key)++"\">"++Value++"</td>\r\n".

renderSystemChecks(SystemChecks) ->
	"<div  class=\"system-checks\"><table>
		<thead><td>Check</td><td>Status</td><td>Technical Detail</td><td class=\"debug\">Debug</td></thead>
		" ++ maps:fold(
		fun (CheckId, CheckInfo, Html) ->
			CheckHealthy = maps:get(<<"ok">>, CheckInfo, false),
			Link = binary_to_list(maps:get(<<"link">>, CheckInfo, <<"">>)),
			CheckHtml = "
				<tr class=\""++getCssClass("check", CheckHealthy)++"\">
					"++formatString(<<"checkid">>, CheckId)++"
					<td class=\"status\">"++renderCheckStatus(CheckHealthy, Link)++"</td>
					"++formatStringFromInfo(<<"techDetail">>, CheckInfo)
					++formatStringFromInfo(<<"debug">>, CheckInfo)
				++"</tr>
			",
			Html++CheckHtml
		end, "", SystemChecks) ++ "
	</table></div>".

systemHealthy(SystemChecks) ->
	maps:fold(fun (_CheckId, CheckInfo, AccHealthy) ->
		CheckHealthy = maps:get(<<"ok">>, CheckInfo, false),
		case {AccHealthy, CheckHealthy} of
			{false, _} -> false;
			{true, _} -> CheckHealthy;
			{_, true} -> unknown;
			{_, _} -> CheckHealthy
		end
	end, true, SystemChecks).


renderSystemMetrics(SystemMetrics) ->
	Html = maps:fold(
		fun (MetricId, MetricInfo, Html) ->
			Value = maps:get(<<"value">>, MetricInfo, -1),
			TechDetail = binary_to_list(maps:get(<<"techDetail">>, MetricInfo, <<"">>)),
			MetricHtml = io_lib:format("
				<tr class=\"metric\" title=~p>
					<td class=\"metricid\">~s</td>
					<td class=\"value\">~p</td>
				</tr>
			", [TechDetail, binary_to_list(MetricId), Value]),
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
		false -> Type ++ " erroring";
		_ -> Type ++ " health-unknown"
	end.

renderSystemHeader(Name, Host, DupNameCount) ->
	InfoURL = "https://" ++ Host ++ "/_info",
	case {Name, DupNameCount} of
		{unknown, _} ->
			"<h2 id=\"host-"++Host++"\">
				<a href=\""++InfoURL++"\" target=\"_blank\" class=\"rawInfoURL\">&#128279;</a>
				"++Host++"
			</h2>";
		{_, 1} ->
			ReadableName = re:replace(Name, "_", " ", [global, {return,list}]),
			"<h2 id=\"host-"++Host++"\">
				<a href=\""++InfoURL++"\" target=\"_blank\" class=\"rawInfoURL\">&#128279;</a>
				<span class=\"system-name\">"++ReadableName++"</span>
			</h2>";
		{_, _} ->
			ReadableName = re:replace(Name, "_", " ", [global, {return,list}]),
			"<h2 id=\"host-"++Host++"\">
				<a href=\""++InfoURL++"\" target=\"_blank\" class=\"rawInfoURL\">&#128279;</a>
				<span class=\"system-name\">"++ReadableName++"</span> ("++Host++")
			</h2>"
	end.

% Used to sort systems by status
sortedHealthStatus(SystemChecks) ->
	case systemHealthy(SystemChecks) of
		true -> 2;
		false -> 0;
		% Ensure "unknown" systems come above healthy ones
		_ -> 1
	end.

countDupNames(SystemList, CompareSystemName) ->
	length(lists:filter(
		fun ({_, {SystemName, _, _}}) ->
			SystemName == CompareSystemName
		end, SystemList)).

renderAll(SystemMap) ->
	SystemList = maps:to_list(SystemMap),
	SortedSystems = lists:sort(
		fun ({HostA, {NameA, ChecksA, _}}, {HostB, {NameB, ChecksB, _}}) ->
			HealthyA = sortedHealthStatus(ChecksA),
			HealthyB = sortedHealthStatus(ChecksB),
			{HealthyA, NameA, HostA} < {HealthyB, NameB, HostB}
		end, SystemList),
	lists:foldl(
		fun ({Host, {SystemName, SystemChecks, SystemMetrics}}, Output) ->
			DupNameCount = countDupNames(SystemList, SystemName),
			Output++"
			<div class=\""++getCssClass("system", systemHealthy(SystemChecks))++"\">
				"++renderSystemHeader(SystemName, Host, DupNameCount)++"
				"++renderSystemChecks(SystemChecks)++"
				"++renderSystemMetrics(SystemMetrics)++"
			</div>
			"
		end, "", SortedSystems).

encodeInfo(Systems) ->
	jiffy:encode(#{
		system => <<"lucos_monitoring">>,
		checks => #{},
		metrics => #{
			<<"system-count">> => #{
				<<"value">> => maps:size(Systems),
				<<"techDetail">> => <<"The number of systems being monitored">>
			}
		},
		ci => #{
			circle => <<"gh/lucas42/lucos_monitoring">>
		},
		icon => <<"/icon">>,
		network_only => true,
		title => <<"Monitoring">>,
		show_on_homepage => true
	}).

controller(_Method, RequestUri, StatePid) ->
	Path = re:replace(RequestUri, "\\?.*$", "", [{return,list}]),
	case Path of
		"/" ->
			Systems = gen_server:call(StatePid, {fetch, all}),
			ChecksOutput = renderAll(Systems),
			{200, "text/html", "<html>
				<head>
					<title>Lucos Monitoring</title>
					<link rel=\"stylesheet\" type=\"text/css\" href=\"/style.css\" />
					<link rel=\"icon\" href=\"/icon\" />
					<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />
					<meta name=\"mobile-web-app-capable\" content=\"yes\">
				</head>
				<body>
					<lucos-navbar>Monitoring</lucos-navbar>
					<div id=\"checks\">
					" ++ ChecksOutput ++ "
					</div>
					<script src=\"/lucos_navbar.js\" type=\"text/javascript\"></script>
				</body>
			</html>"};
		"/style.css" ->
			{200, "text/css", "
			.system h2 { background-color: #666; color: #fff; padding: 0.1em 1em; border-radius: 0.2em; }
			h2 .system-name { text-transform:capitalize; }
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
			.system.health-unknown h2, tr.check.health-unknown td.status { background-color: #555; }
			.system.healthy .debug { display: none; }
			tbody .debug { white-space: pre-wrap; }
			.metrics { margin-top: 2em; }
			tr.metric[title] { cursor: help; }
			.status a { display: block; color:inherit; text-decoration: none; width: 100%; }
			.status a:hover { text-decoration: underline; }
			.formattedString a { word-break: break-word; }
			#checks { max-width: 720px; display: block; margin: 0 auto;}
			.system-checks { display: block; width: 100%; }
			.system-checks > table { width: 100%; }
			"};
		"/robots.txt" ->
			{200, "text/plain", "User-agent: *\nDisallow:\n"};
		"/_info" ->
			Systems = gen_server:call(StatePid, {fetch, all}),
			{200, "application/json", encodeInfo(Systems)};
		"/icon" ->
			{ok, IconFile} = file:read_file("icon.png"),
			{200, "image/png", IconFile};
		"/lucos_navbar.js" ->
			{ok, ScriptFile} = file:read_file("lucos_navbar.js"),
			{200, "text/javascript", ScriptFile};
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