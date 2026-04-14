-module(server).
-export([start/2, accept/3, handleRequest/2]).

start(_StartType, _StartArgs) ->
	configureLogLevel(),
	try
		{Port, _} = string:to_integer(os:getenv("PORT", "8080")),
		{ok, StatePid} = monitoring_state_server:start_link(),
		SchedulerCount = erlang:system_info(schedulers),
		Opts = [{active, false},
				binary,
				{packet, http_bin},
				{reuseaddr, true}],
		listen_with_retry(Port, Opts, StatePid, SchedulerCount, 30)
	catch
		Exception:Reason -> logger:emergency("Startup error occured: ~p ~p", [Exception, Reason])
	end.

configureLogLevel() ->
	LevelStr = os:getenv("LOG_LEVEL", "notice"),
	Level = list_to_atom(LevelStr),
	logger:set_primary_config(level, Level).

listen_with_retry(Port, _Opts, _StatePid, _SchedulerCount, 0) ->
	logger:emergency("Can't listen on port ~p: eaddrinuse (all retries exhausted)", [Port]),
	{error, {eaddrinuse, Port}};
listen_with_retry(Port, Opts, StatePid, SchedulerCount, RetriesLeft) ->
	case gen_tcp:listen(Port, Opts) of
		{ok, ListenSocket} ->
			Spawn = fun(SchedulerID) ->
				spawn_opt(?MODULE, accept, [ListenSocket, SchedulerID, StatePid], [link, {scheduler, SchedulerID}])
			end,
			lists:foreach(Spawn, lists:seq(1, SchedulerCount)),
			logger:notice("server listening on port ~b with ~b schedulers", [Port, SchedulerCount]),
			fetcher_info:start(StatePid),
			fetcher_circleci:start(StatePid),
			receive
				Any -> logger:notice("~p", [Any])
			end;
		{error, eaddrinuse} ->
			logger:warning("Can't listen on port ~p: eaddrinuse (~p retries left, retrying in 1s)", [Port, RetriesLeft]),
			timer:sleep(1000),
			listen_with_retry(Port, Opts, StatePid, SchedulerCount, RetriesLeft - 1);
		{error, Error} ->
			logger:emergency("Can't listen on port ~p: ~p", [Port, Error]),
			{error, Error}
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
			handleRequest(Socket, Method, binary_to_list(RequestUri), #{}, StatePid);
		Error ->
			Error
	end.
handleRequest(Socket, Method, RequestUri, Headers, StatePid) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, http_eoh} ->
			ContentLength = maps:get('Content-Length', Headers, 0),
			RequestBody = readBody(Socket, ContentLength),
			ClientIP = getClientIP(Socket),
			{StatusCode, ContentType, ResponseBody} = tryController(Method, RequestUri, RequestBody, Headers, StatePid),
			Response = getHeaders(StatusCode, ContentType) ++ ResponseBody,
			gen_tcp:send(Socket, Response),
			gen_tcp:close(Socket),
			AccessLogLevel = accessLogLevel(RequestUri),
			logger:log(AccessLogLevel, "~p ~p ~p ~p", [ClientIP, Method, StatusCode, RequestUri]),
			ok;
		{ok, {http_header, _, 'Content-Length', _, Value}} ->
			{Length, _} = string:to_integer(binary_to_list(Value)),
			handleRequest(Socket, Method, RequestUri, maps:put('Content-Length', Length, Headers), StatePid);
		{ok, {http_header, _, 'Authorization', _, Value}} ->
			handleRequest(Socket, Method, RequestUri, maps:put('Authorization', binary_to_list(Value), Headers), StatePid);
		{ok, _Data} ->
			handleRequest(Socket, Method, RequestUri, Headers, StatePid);

		Error ->
			Error
	end.

readBody(_Socket, 0) -> "";
readBody(Socket, Length) ->
	inet:setopts(Socket, [{packet, raw}]),
	case gen_tcp:recv(Socket, Length) of
		{ok, Data} -> binary_to_list(Data);
		_ -> ""
	end.

getClientIP(Socket) ->
	{ok, {ClientIP, _}} = inet:peername(Socket),
	inet:ntoa(ClientIP).

accessLogLevel(RequestUri) ->
	Path = re:replace(RequestUri, "\\?.*$", "", [{return, list}]),
	case Path of
		"/_info" -> info;
		_ -> notice
	end.

getHeaders(StatusCode, ContentType) ->
	getStatusLine(StatusCode) ++
		"Content-Type: " ++ ContentType ++ "; charset=utf-8\n" ++
		"\n".

getStatusLine(StatusCode) ->
	"HTTP/1.1 " ++ integer_to_list(StatusCode) ++ " " ++ getReasonPhrase(StatusCode) ++ "\n".

getReasonPhrase(StatusCode) ->
	case StatusCode of
		200 -> "OK";
		204 -> "No Content";
		400 -> "Bad Request";
		401 -> "Unauthorized";
		404 -> "Not Found";
		405 -> "Method Not Allowed";
		500 -> "Internal Error"
	end.


formatStringFromInfo(Key, CheckInfo) ->
	formatString(Key, maps:get(Key, CheckInfo, <<"">>)).

% Escapes HTML special characters to prevent XSS. Must be applied to all
% external data before inserting into HTML. & must be escaped first to avoid
% double-escaping the entities introduced by subsequent replacements.
htmlEscape(Value) ->
	lists:foldl(
		fun ({From, To}, Acc) ->
			re:replace(Acc, From, To, [global, {return, list}])
		end,
		Value,
		[{"&", "\\&amp;"}, {"<", "\\&lt;"}, {">", "\\&gt;"}, {"\"", "\\&quot;"}]
	).

formatString(Key, BinaryValue) ->
	RawValue = binary_to_list(BinaryValue),
	EscapedValue = htmlEscape(RawValue),
	ZWSValue = re:replace(EscapedValue, "_", "\\&ZeroWidthSpace;_",[global, {return, list}]),
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
			UnknownCount = maps:get(<<"unknown_count">>, CheckInfo, 0),
			FailCount = maps:get(<<"fail_count">>, CheckInfo, 0),
			FailThreshold = maps:get(<<"failThreshold">>, CheckInfo, 1),
			IsBuffering = CheckHealthy =/= false andalso
				((UnknownCount > 0) orelse (FailCount > 0 andalso FailThreshold > 1)),
			CheckClass = case IsBuffering of
				true -> "check buffering";
				false -> getCssClass("check", CheckHealthy)
			end,
			StatusText = case IsBuffering of
				true when UnknownCount > 0 ->
					"unknown (" ++ integer_to_list(UnknownCount) ++ ")";
				true ->
					"failing (" ++ integer_to_list(FailCount) ++ "/" ++ integer_to_list(FailThreshold) ++ ")";
				false ->
					atom_to_list(CheckHealthy)
			end,
			StatusHtml = case Link of
				"" -> StatusText;
				_ -> "<a href=\""++Link++"\" target=\"_blank\">"++StatusText++"</a>"
			end,
			CheckHtml = "
				<tr class=\""++CheckClass++"\">
					"++formatString(<<"checkid">>, CheckId)++"
					<td class=\"status\">"++StatusHtml++"</td>
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

getSuppressionStatus(unknown, _SuppressionMap) -> none;
getSuppressionStatus(SystemName, SuppressionMap) when is_list(SystemName) ->
	case maps:get(SystemName, SuppressionMap, undefined) of
		undefined -> none;
		{pending_verification, _} -> pending_verification;
		ExpiryTime when is_integer(ExpiryTime) ->
			Now = erlang:system_time(second),
			case Now < ExpiryTime of
				true -> active;
				false -> none
			end
	end.

renderAll(SystemMap, SuppressionMap) ->
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
			SuppressionStatus = getSuppressionStatus(SystemName, SuppressionMap),
			SuppressionClass = case SuppressionStatus of
				active -> " suppressed";
				pending_verification -> " pending-verification";
				none -> ""
			end,
			Output++"
			<div class=\""++getCssClass("system", systemHealthy(SystemChecks))++SuppressionClass++"\">
				"++renderSystemHeader(SystemName, Host, DupNameCount)++"
				"++renderSystemChecks(SystemChecks)++"
				"++renderSystemMetrics(SystemMetrics)++"
			</div>
			"
		end, "", SortedSystems).

systemNameBinary(unknown) -> <<"unknown">>;
systemNameBinary(Name) when is_list(Name) -> list_to_binary(Name).

encodeStatus(SystemMap) ->
	SystemList = maps:to_list(SystemMap),
	EncodedSystems = maps:from_list(lists:map(
		fun ({Host, {SystemName, SystemChecks, SystemMetrics}}) ->
			Healthy = systemHealthy(SystemChecks),
			EncodedChecks = maps:map(
				fun (_CheckId, CheckInfo) ->
					maps:without([<<"unknown_count">>, <<"link">>], CheckInfo)
				end, SystemChecks),
			SystemJson = #{
				<<"name">> => systemNameBinary(SystemName),
				<<"healthy">> => Healthy,
				<<"checks">> => EncodedChecks,
				<<"metrics">> => SystemMetrics
			},
			{list_to_binary(Host), SystemJson}
		end, SystemList)),
	{TotalSystems, HealthyCount, ErroringCount, UnknownCount} = lists:foldl(
		fun ({_Host, {_Name, SystemChecks, _Metrics}}, {Total, Healthy, Erroring, Unknown}) ->
			case systemHealthy(SystemChecks) of
				true  -> {Total + 1, Healthy + 1, Erroring, Unknown};
				false -> {Total + 1, Healthy, Erroring + 1, Unknown};
				_     -> {Total + 1, Healthy, Erroring, Unknown + 1}
			end
		end, {0, 0, 0, 0}, SystemList),
	Summary = #{
		<<"total_systems">> => TotalSystems,
		<<"healthy">> => HealthyCount,
		<<"erroring">> => ErroringCount,
		<<"unknown">> => UnknownCount
	},
	jiffy:encode(#{
		<<"systems">> => EncodedSystems,
		<<"summary">> => Summary
	}).

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



controller(Method, RequestUri, Body, Headers, StatePid) ->
	Path = re:replace(RequestUri, "\\?.*$", "", [{return,list}]),
	case Path of
		"/" ->
			Systems = gen_server:call(StatePid, {fetch, all}),
			SuppressionMap = gen_server:call(StatePid, {fetch, suppression}),
			ChecksOutput = renderAll(Systems, SuppressionMap),
			{200, "text/html", "<html>
				<head>
					<title>Lucos Monitoring</title>
					<link rel=\"stylesheet\" type=\"text/css\" href=\"/style.css\" />
					<link rel=\"icon\" href=\"/icon\" />
					<link rel=\"manifest\" href=\"/manifest.json\" />
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
			tr.check.buffering td.status { background-color: #880; }
			.system.suppressed h2, .system.pending-verification h2 { background-color: #880; }
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
		"/api/status" ->
			Systems = gen_server:call(StatePid, {fetch, all}),
			{200, "application/json", encodeStatus(Systems)};
		"/_info" ->
			Systems = gen_server:call(StatePid, {fetch, all}),
			{200, "application/json", encodeInfo(Systems)};
		"/icon" ->
			{ok, IconFile} = file:read_file("icon.png"),
			{200, "image/png", IconFile};
		"/maskable_icon.png" ->
			{ok, IconFile} = file:read_file("maskable_icon.png"),
			{200, "image/png", IconFile};
		"/manifest.json" ->
			{ok, ManifestFile} = file:read_file("manifest.json"),
			{200, "application/manifest.json", ManifestFile};
		"/lucos_navbar.js" ->
			{ok, ScriptFile} = file:read_file("lucos_navbar.js"),
			{200, "text/javascript", ScriptFile};
		_ ->
			case string:prefix(Path, "/suppress") of
				nomatch ->
					{404, "text/plain", "Not Found"};
				_ ->
					case suppression:checkAuth(Headers) of
						{error, unauthorized} ->
							{401, "text/plain", "Unauthorized"};
						ok ->
							case suppression:handle(Path, Method, Body, StatePid) of
								nomatch -> {404, "text/plain", "Not Found"};
								Response -> Response
							end
					end
			end
	end.

tryController(Method, RequestUri, Body, Headers, StatePid) ->
	try controller(Method, RequestUri, Body, Headers, StatePid) of
		Response -> Response
	catch
		ExceptionClass:Term:StackTrace ->
			logger:error("ExceptionClass: ~p Term: ~p StackTrace: ~p", [ExceptionClass, Term, StackTrace]),
			{500, "text/plain", "An Error occurred whilst generating this page."}
	end.

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").

	encodeStatus_empty_test() ->
		Result = jiffy:decode(encodeStatus(#{}), [return_maps]),
		?assertEqual(#{}, maps:get(<<"systems">>, Result)),
		Summary = maps:get(<<"summary">>, Result),
		?assertEqual(0, maps:get(<<"total_systems">>, Summary)),
		?assertEqual(0, maps:get(<<"healthy">>, Summary)),
		?assertEqual(0, maps:get(<<"erroring">>, Summary)),
		?assertEqual(0, maps:get(<<"unknown">>, Summary)).

	encodeStatus_healthy_system_test() ->
		SystemMap = #{
			"example.l42.eu" => {"lucos_example", #{
				<<"fetch-info">> => #{<<"ok">> => true, <<"techDetail">> => <<"Fetches /_info">>}
			}, #{}}
		},
		Result = jiffy:decode(encodeStatus(SystemMap), [return_maps]),
		Systems = maps:get(<<"systems">>, Result),
		System = maps:get(<<"example.l42.eu">>, Systems),
		?assertEqual(<<"lucos_example">>, maps:get(<<"name">>, System)),
		?assertEqual(true, maps:get(<<"healthy">>, System)),
		Summary = maps:get(<<"summary">>, Result),
		?assertEqual(1, maps:get(<<"total_systems">>, Summary)),
		?assertEqual(1, maps:get(<<"healthy">>, Summary)),
		?assertEqual(0, maps:get(<<"erroring">>, Summary)),
		?assertEqual(0, maps:get(<<"unknown">>, Summary)).

	encodeStatus_erroring_system_test() ->
		SystemMap = #{
			"broken.l42.eu" => {"lucos_broken", #{
				<<"fetch-info">> => #{<<"ok">> => false, <<"techDetail">> => <<"Fetches /_info">>, <<"debug">> => <<"Connection refused">>}
			}, #{}}
		},
		Result = jiffy:decode(encodeStatus(SystemMap), [return_maps]),
		Systems = maps:get(<<"systems">>, Result),
		System = maps:get(<<"broken.l42.eu">>, Systems),
		?assertEqual(false, maps:get(<<"healthy">>, System)),
		Checks = maps:get(<<"checks">>, System),
		FetchInfo = maps:get(<<"fetch-info">>, Checks),
		?assertEqual(false, maps:get(<<"ok">>, FetchInfo)),
		?assertEqual(<<"Connection refused">>, maps:get(<<"debug">>, FetchInfo)),
		Summary = maps:get(<<"summary">>, Result),
		?assertEqual(1, maps:get(<<"total_systems">>, Summary)),
		?assertEqual(0, maps:get(<<"healthy">>, Summary)),
		?assertEqual(1, maps:get(<<"erroring">>, Summary)),
		?assertEqual(0, maps:get(<<"unknown">>, Summary)).

	encodeStatus_unknown_system_name_test() ->
		SystemMap = #{
			"unreachable.l42.eu" => {unknown, #{
				<<"fetch-info">> => #{<<"ok">> => unknown, <<"techDetail">> => <<"Fetches /_info">>}
			}, #{}}
		},
		Result = jiffy:decode(encodeStatus(SystemMap), [return_maps]),
		Systems = maps:get(<<"systems">>, Result),
		System = maps:get(<<"unreachable.l42.eu">>, Systems),
		?assertEqual(<<"unknown">>, maps:get(<<"name">>, System)),
		Summary = maps:get(<<"summary">>, Result),
		?assertEqual(1, maps:get(<<"total_systems">>, Summary)),
		?assertEqual(0, maps:get(<<"healthy">>, Summary)),
		?assertEqual(0, maps:get(<<"erroring">>, Summary)),
		?assertEqual(1, maps:get(<<"unknown">>, Summary)).

	encodeStatus_strips_internal_fields_test() ->
		SystemMap = #{
			"example.l42.eu" => {"lucos_example", #{
				<<"fetch-info">> => #{<<"ok">> => true, <<"techDetail">> => <<"Fetches /_info">>, <<"unknown_count">> => 0, <<"link">> => <<"https://example.l42.eu/_info">>}
			}, #{}}
		},
		Result = jiffy:decode(encodeStatus(SystemMap), [return_maps]),
		Systems = maps:get(<<"systems">>, Result),
		System = maps:get(<<"example.l42.eu">>, Systems),
		Checks = maps:get(<<"checks">>, System),
		FetchInfo = maps:get(<<"fetch-info">>, Checks),
		?assertEqual(false, maps:is_key(<<"unknown_count">>, FetchInfo)),
		?assertEqual(false, maps:is_key(<<"link">>, FetchInfo)).

	encodeStatus_multiple_systems_summary_test() ->
		SystemMap = #{
			"healthy.l42.eu" => {"lucos_healthy", #{<<"fetch-info">> => #{<<"ok">> => true, <<"techDetail">> => <<"">>}}, #{}},
			"erroring.l42.eu" => {"lucos_erroring", #{<<"fetch-info">> => #{<<"ok">> => false, <<"techDetail">> => <<"">>}}, #{}},
			"unknown.l42.eu" => {unknown, #{<<"fetch-info">> => #{<<"ok">> => unknown, <<"techDetail">> => <<"">>}}, #{}}
		},
		Result = jiffy:decode(encodeStatus(SystemMap), [return_maps]),
		Summary = maps:get(<<"summary">>, Result),
		?assertEqual(3, maps:get(<<"total_systems">>, Summary)),
		?assertEqual(1, maps:get(<<"healthy">>, Summary)),
		?assertEqual(1, maps:get(<<"erroring">>, Summary)),
		?assertEqual(1, maps:get(<<"unknown">>, Summary)).

	htmlEscape_no_special_chars_test() ->
		?assertEqual("hello world", htmlEscape("hello world")).

	htmlEscape_ampersand_test() ->
		?assertEqual("foo &amp; bar", htmlEscape("foo & bar")).

	htmlEscape_angle_brackets_test() ->
		?assertEqual("&lt;script&gt;alert(1)&lt;/script&gt;", htmlEscape("<script>alert(1)</script>")).

	htmlEscape_double_quote_test() ->
		?assertEqual("say &quot;hello&quot;", htmlEscape("say \"hello\"")).

	htmlEscape_all_special_chars_test() ->
		?assertEqual("&lt;a href=&quot;x&quot;&gt;foo &amp; bar&lt;/a&gt;", htmlEscape("<a href=\"x\">foo & bar</a>")).

	htmlEscape_no_double_encoding_test() ->
		% Escaping once should not double-encode on a second pass
		Escaped = htmlEscape("a & b"),
		?assertEqual("a &amp; b", Escaped),
		?assertEqual("a &amp;amp; b", htmlEscape(Escaped)).

	formatString_escapes_html_test() ->
		% A techDetail containing a script tag should be escaped, not executed
		Result = formatString(<<"techDetail">>, <<"<script>alert(1)</script>">>),
		?assertEqual(false, string:str(Result, "<script>") > 0),
		?assert(string:str(Result, "&lt;script&gt;") > 0).

	formatString_url_still_linkified_test() ->
		% A plain URL in a value should still be wrapped in an anchor tag
		Result = formatString(<<"debug">>, <<"See https://example.com/path for details">>),
		?assert(string:str(Result, "<a href=") > 0),
		?assert(string:str(Result, "https://example.com/path") > 0).

	suppress_clear_requires_auth_test() ->
		% Phase 3: /suppress/clear requires a valid token
		os:putenv("CLIENT_KEYS", "lucos_deploy_orb=mysecrettoken"),
		{ok, StatePid} = monitoring_state_server:start_link(),
		Body = "{\"systemDeployed\":\"lucos_test\"}",
		{UnauthStatus, _, _} = tryController('POST', "/suppress/clear", Body, #{}, StatePid),
		{AuthStatus, _, _} = tryController('POST', "/suppress/clear", Body, #{'Authorization' => "Bearer mysecrettoken"}, StatePid),
		gen_server:stop(StatePid),
		os:unsetenv("CLIENT_KEYS"),
		?assertEqual(401, UnauthStatus),
		?assertEqual(204, AuthStatus).

	suppress_clear_invalid_token_rejected_test() ->
		% Phase 1: /suppress/clear must reject requests with an invalid token
		os:putenv("CLIENT_KEYS", "lucos_deploy_orb=mysecrettoken"),
		{ok, StatePid} = monitoring_state_server:start_link(),
		Body = "{\"systemDeployed\":\"lucos_test\"}",
		{StatusCode, _, _} = tryController('POST', "/suppress/clear", Body, #{'Authorization' => "Bearer wrongtoken"}, StatePid),
		gen_server:stop(StatePid),
		os:unsetenv("CLIENT_KEYS"),
		?assertEqual(401, StatusCode).

	suppress_other_routes_still_require_auth_test() ->
		% Other /suppress/* routes must still require auth
		os:putenv("CLIENT_KEYS", "lucos_deploy_orb=mysecrettoken"),
		{ok, StatePid} = monitoring_state_server:start_link(),
		{StatusCode, _, _} = tryController('PUT', "/suppress/lucos_test", "", #{}, StatePid),
		gen_server:stop(StatePid),
		os:unsetenv("CLIENT_KEYS"),
		?assertEqual(401, StatusCode).

	getSuppressionStatus_not_suppressed_test() ->
		% System not in suppression map — none
		?assertEqual(none, getSuppressionStatus("lucos_foo", #{})).

	getSuppressionStatus_pending_verification_test() ->
		% pending_verification tuple — returns pending_verification
		PendingSources = sets:from_list([info], [{version, 2}]),
		SuppressionMap = #{"lucos_foo" => {pending_verification, PendingSources}},
		?assertEqual(pending_verification, getSuppressionStatus("lucos_foo", SuppressionMap)).

	getSuppressionStatus_active_test() ->
		% Expiry time in the future — returns active
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_foo" => FutureExpiry},
		?assertEqual(active, getSuppressionStatus("lucos_foo", SuppressionMap)).

	getSuppressionStatus_expired_test() ->
		% Expiry time in the past — treated as none (window expired without being cleared)
		PastExpiry = erlang:system_time(second) - 1,
		SuppressionMap = #{"lucos_foo" => PastExpiry},
		?assertEqual(none, getSuppressionStatus("lucos_foo", SuppressionMap)).

	getSuppressionStatus_unknown_system_test() ->
		% System name is the atom 'unknown' — always none regardless of map contents
		?assertEqual(none, getSuppressionStatus(unknown, #{"unknown" => erlang:system_time(second) + 600})).

-endif.