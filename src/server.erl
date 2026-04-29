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
	Path = lists:flatten(re:replace(RequestUri, "\\?.*$", "", [{return, list}])),
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

% Maps a check status atom to a sort priority (lower = first in table).
checkStatusSortPriority(failing)   -> 0;
checkStatusSortPriority(unknown)   -> 1;
checkStatusSortPriority(buffering) -> 2;
checkStatusSortPriority(_)         -> 3. % healthy (and any future statuses)

% Maps a system status atom to a sort priority (lower = first in page).
systemStatusSortPriority(failing)              -> 0;
systemStatusSortPriority(unknown)              -> 1;
systemStatusSortPriority(buffering)            -> 2;
systemStatusSortPriority(suppressed)           -> 3;
systemStatusSortPriority(pending_verification) -> 3;
systemStatusSortPriority(_)                    -> 4. % healthy

% Maps a status atom to a CSS class string.
% pending_verification uses a hyphen to match existing CSS convention.
statusToCssClass(pending_verification) -> "pending-verification";
statusToCssClass(Status) -> atom_to_list(Status).

renderSystemChecks(SystemChecks) ->
	SortedChecks = lists:sort(
		fun (CheckA, CheckB) ->
			StatusA = maps:get(<<"status">>, CheckA, unknown),
			StatusB = maps:get(<<"status">>, CheckB, unknown),
			IdA = maps:get(<<"id">>, CheckA, <<>>),
			IdB = maps:get(<<"id">>, CheckB, <<>>),
			{checkStatusSortPriority(StatusA), IdA} =< {checkStatusSortPriority(StatusB), IdB}
		end, SystemChecks),
	"<div  class=\"system-checks\"><table>
		<thead><td>Check</td><td>Status</td><td>Technical Detail</td><td class=\"debug\">Debug</td></thead>
		" ++ lists:foldl(
		fun (Check, Html) ->
			CheckId = maps:get(<<"id">>, Check),
			Status = maps:get(<<"status">>, Check, unknown),
			StatusText = binary_to_list(maps:get(<<"statusText">>, Check, <<"unknown">>)),
			Link = binary_to_list(maps:get(<<"link">>, Check, <<"">>)),
			CheckClass = "check " ++ statusToCssClass(Status),
			StatusHtml = case Link of
				"" -> StatusText;
				_ -> "<a href=\""++Link++"\" target=\"_blank\">"++StatusText++"</a>"
			end,
			CheckHtml = "
				<tr class=\""++CheckClass++"\">
					"++formatString(<<"checkid">>, CheckId)++"
					<td class=\"status\">"++StatusHtml++"</td>
					"++formatStringFromInfo(<<"techDetail">>, Check)
					++formatStringFromInfo(<<"debug">>, Check)
				++"</tr>
			",
			Html++CheckHtml
		end, "", SortedChecks) ++ "
	</table></div>".


renderSystemMetrics(SystemMetrics) ->
	SortedMetrics = lists:sort(
		fun (MetricA, MetricB) ->
			maps:get(<<"id">>, MetricA) =< maps:get(<<"id">>, MetricB)
		end, SystemMetrics),
	Html = lists:foldl(
		fun (Metric, Html) ->
			MetricId = binary_to_list(maps:get(<<"id">>, Metric)),
			Value = maps:get(<<"value">>, Metric, -1),
			MetricHtml = lists:flatten(
				"<tr class=\"metric\">"
				++ "<td class=\"metricid\">" ++ MetricId ++ "</td>"
				++ "<td class=\"value\">" ++ lists:flatten(io_lib:format("~p", [Value])) ++ "</td>"
				++ formatStringFromInfo(<<"techDetail">>, Metric)
				++ "</tr>"
			),
			Html++MetricHtml
		end, "", SortedMetrics),
	case Html of
		"" -> "";
		_ ->
			"
			<table class=\"metrics\">
				<thead><td>Metric</td><td>Value</td><td>Technical Detail</td></thead>
				"++Html++"
			</table>"
	end.

renderSystemHeader(Name, Host, DupNameCount) ->
	InfoURL = "https://" ++ Host ++ "/_info",
	ReadableName = re:replace(binary_to_list(Name), "_", " ", [global, {return,list}]),
	case DupNameCount of
		1 ->
			"<h2 id=\"host-"++Host++"\">
				<a href=\""++InfoURL++"\" target=\"_blank\" class=\"rawInfoURL\">&#128279;</a>
				<span class=\"system-name\">"++ReadableName++"</span>
			</h2>";
		_ ->
			"<h2 id=\"host-"++Host++"\">
				<a href=\""++InfoURL++"\" target=\"_blank\" class=\"rawInfoURL\">&#128279;</a>
				<span class=\"system-name\">"++ReadableName++"</span> ("++Host++")
			</h2>"
	end.

% Renders all systems. Systems is the list returned by {fetch, all} — each
% element is a map with <<"host">>, <<"name">>, <<"status">>, <<"checks">>, <<"metrics">>.
% Status-based CSS class and suppression states are derived from <<"status">> directly;
% no separate suppression map fetch is needed.
renderAll(Systems) ->
	SortedSystems = lists:sort(
		fun (SysA, SysB) ->
			StatusA = maps:get(<<"status">>, SysA),
			StatusB = maps:get(<<"status">>, SysB),
			NameA = maps:get(<<"name">>, SysA, <<"">>),
			NameB = maps:get(<<"name">>, SysB, <<"">>),
			HostA = maps:get(<<"host">>, SysA, <<"">>),
			HostB = maps:get(<<"host">>, SysB, <<"">>),
			{systemStatusSortPriority(StatusA), NameA, HostA} =< {systemStatusSortPriority(StatusB), NameB, HostB}
		end, Systems),
	lists:foldl(
		fun (System, Output) ->
			Name = maps:get(<<"name">>, System),
			Host = binary_to_list(maps:get(<<"host">>, System)),
			Status = maps:get(<<"status">>, System),
			SystemChecks = maps:get(<<"checks">>, System, []),
			SystemMetrics = maps:get(<<"metrics">>, System, []),
			DupNameCount = length(lists:filter(
				fun(S) -> maps:get(<<"name">>, S, <<>>) =:= Name end,
				Systems)),
			CssClass = "system " ++ statusToCssClass(Status),
			Output++"
			<div class=\""++CssClass++"\">
				"++renderSystemHeader(Name, Host, DupNameCount)++"
				"++renderSystemChecks(SystemChecks)++"
				"++renderSystemMetrics(SystemMetrics)++"
			</div>
			"
		end, "", SortedSystems).

encodeStatus(Systems) ->
	EncodedSystems = maps:from_list(lists:map(
		fun (System) ->
			Host = maps:get(<<"host">>, System),
			Name = maps:get(<<"name">>, System),
			Status = maps:get(<<"status">>, System),
			% Rebuild checks as a map keyed by check id, omitting the id field from the value
			Checks = maps:from_list([
				{maps:get(<<"id">>, C), maps:without([<<"id">>], C)}
				|| C <- maps:get(<<"checks">>, System, [])]),
			% Rebuild metrics as a map keyed by metric id, omitting the id field from the value
			Metrics = maps:from_list([
				{maps:get(<<"id">>, M), maps:without([<<"id">>], M)}
				|| M <- maps:get(<<"metrics">>, System, [])]),
			SystemJson = #{
				<<"name">>    => Name,
				<<"status">>  => Status,
				<<"checks">>  => Checks,
				<<"metrics">> => Metrics
			},
			{Host, SystemJson}
		end, Systems)),
	% Summary counts: healthy, failing, anything else (unknown/buffering/suppressed/pending) → unknown
	{TotalSystems, HealthyCount, FailingCount, UnknownCount} = lists:foldl(
		fun (System, {Total, Healthy, Failing, Unknown}) ->
			case maps:get(<<"status">>, System) of
				healthy -> {Total + 1, Healthy + 1, Failing, Unknown};
				failing -> {Total + 1, Healthy, Failing + 1, Unknown};
				_       -> {Total + 1, Healthy, Failing, Unknown + 1}
			end
		end, {0, 0, 0, 0}, Systems),
	Summary = #{
		<<"total_systems">> => TotalSystems,
		<<"healthy">>       => HealthyCount,
		<<"failing">>       => FailingCount,
		<<"unknown">>       => UnknownCount
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
				<<"value">> => length(Systems),
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
			ChecksOutput = renderAll(Systems),
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
			.system.failing h2, tr.check.failing td.status { background-color: #900; }
			.system.unknown h2, tr.check.unknown td.status { background-color: #555; }
			.system.buffering h2, tr.check.buffering td.status { background-color: #da9000; }
			.system.suppressed h2, .system.pending-verification h2 { background-color: #da9000; }
			.system.suppressed h2:before { content: \""++binary_to_list(<<"🔇"/utf8>>)++"\"; }
			.system.pending-verification h2:before { content: \""++binary_to_list(<<"⏳"/utf8>>)++"\"; }
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
		Result = jiffy:decode(encodeStatus([]), [return_maps]),
		?assertEqual(#{}, maps:get(<<"systems">>, Result)),
		Summary = maps:get(<<"summary">>, Result),
		?assertEqual(0, maps:get(<<"total_systems">>, Summary)),
		?assertEqual(0, maps:get(<<"healthy">>, Summary)),
		?assertEqual(0, maps:get(<<"failing">>, Summary)),
		?assertEqual(0, maps:get(<<"unknown">>, Summary)).

	encodeStatus_healthy_system_test() ->
		Systems = [#{
			<<"host">>    => <<"example.l42.eu">>,
			<<"name">>    => <<"lucos_example">>,
			<<"status">>  => healthy,
			<<"checks">>  => [#{<<"id">> => <<"fetch-info">>, <<"status">> => healthy, <<"statusText">> => <<"healthy">>, <<"techDetail">> => <<"Fetches /_info">>}],
			<<"metrics">> => []
		}],
		Result = jiffy:decode(encodeStatus(Systems), [return_maps]),
		SystemsMap = maps:get(<<"systems">>, Result),
		System = maps:get(<<"example.l42.eu">>, SystemsMap),
		?assertEqual(<<"lucos_example">>, maps:get(<<"name">>, System)),
		?assertEqual(<<"healthy">>, maps:get(<<"status">>, System)),
		Summary = maps:get(<<"summary">>, Result),
		?assertEqual(1, maps:get(<<"total_systems">>, Summary)),
		?assertEqual(1, maps:get(<<"healthy">>, Summary)),
		?assertEqual(0, maps:get(<<"failing">>, Summary)),
		?assertEqual(0, maps:get(<<"unknown">>, Summary)).

	encodeStatus_failing_system_test() ->
		Systems = [#{
			<<"host">>    => <<"broken.l42.eu">>,
			<<"name">>    => <<"lucos_broken">>,
			<<"status">>  => failing,
			<<"checks">>  => [#{<<"id">> => <<"fetch-info">>, <<"status">> => failing, <<"statusText">> => <<"failing">>, <<"techDetail">> => <<"Fetches /_info">>, <<"debug">> => <<"Connection refused">>}],
			<<"metrics">> => []
		}],
		Result = jiffy:decode(encodeStatus(Systems), [return_maps]),
		SystemsMap = maps:get(<<"systems">>, Result),
		System = maps:get(<<"broken.l42.eu">>, SystemsMap),
		?assertEqual(<<"failing">>, maps:get(<<"status">>, System)),
		Checks = maps:get(<<"checks">>, System),
		FetchInfo = maps:get(<<"fetch-info">>, Checks),
		?assertEqual(<<"failing">>, maps:get(<<"status">>, FetchInfo)),
		?assertEqual(<<"Connection refused">>, maps:get(<<"debug">>, FetchInfo)),
		Summary = maps:get(<<"summary">>, Result),
		?assertEqual(1, maps:get(<<"total_systems">>, Summary)),
		?assertEqual(0, maps:get(<<"healthy">>, Summary)),
		?assertEqual(1, maps:get(<<"failing">>, Summary)),
		?assertEqual(0, maps:get(<<"unknown">>, Summary)).

	encodeStatus_unknown_system_test() ->
		Systems = [#{
			<<"host">>    => <<"unreachable.l42.eu">>,
			<<"name">>    => <<"lucos_unreachable">>,
			<<"status">>  => unknown,
			<<"checks">>  => [#{<<"id">> => <<"fetch-info">>, <<"status">> => unknown, <<"statusText">> => <<"unknown">>}],
			<<"metrics">> => []
		}],
		Result = jiffy:decode(encodeStatus(Systems), [return_maps]),
		SystemsMap = maps:get(<<"systems">>, Result),
		System = maps:get(<<"unreachable.l42.eu">>, SystemsMap),
		?assertEqual(<<"lucos_unreachable">>, maps:get(<<"name">>, System)),
		?assertEqual(<<"unknown">>, maps:get(<<"status">>, System)),
		Summary = maps:get(<<"summary">>, Result),
		?assertEqual(1, maps:get(<<"total_systems">>, Summary)),
		?assertEqual(0, maps:get(<<"healthy">>, Summary)),
		?assertEqual(0, maps:get(<<"failing">>, Summary)),
		?assertEqual(1, maps:get(<<"unknown">>, Summary)).

	encodeStatus_multiple_systems_summary_test() ->
		Systems = [
			#{<<"host">> => <<"healthy.l42.eu">>,   <<"name">> => <<"lucos_healthy">>,   <<"status">> => healthy,   <<"checks">> => [], <<"metrics">> => []},
			#{<<"host">> => <<"failing.l42.eu">>,   <<"name">> => <<"lucos_failing">>,   <<"status">> => failing,   <<"checks">> => [], <<"metrics">> => []},
			#{<<"host">> => <<"unknown.l42.eu">>,   <<"name">> => <<"lucos_unknown">>,   <<"status">> => unknown,   <<"checks">> => [], <<"metrics">> => []},
			#{<<"host">> => <<"buffering.l42.eu">>, <<"name">> => <<"lucos_buffering">>, <<"status">> => buffering, <<"checks">> => [], <<"metrics">> => []}
		],
		Result = jiffy:decode(encodeStatus(Systems), [return_maps]),
		Summary = maps:get(<<"summary">>, Result),
		?assertEqual(4, maps:get(<<"total_systems">>, Summary)),
		?assertEqual(1, maps:get(<<"healthy">>, Summary)),
		?assertEqual(1, maps:get(<<"failing">>, Summary)),
		% buffering counts towards unknown in summary (not definitively healthy or failing)
		?assertEqual(2, maps:get(<<"unknown">>, Summary)).

	checkStatusSortPriority_failing_test() ->
		?assertEqual(0, checkStatusSortPriority(failing)).

	checkStatusSortPriority_unknown_test() ->
		?assertEqual(1, checkStatusSortPriority(unknown)).

	checkStatusSortPriority_buffering_test() ->
		?assertEqual(2, checkStatusSortPriority(buffering)).

	checkStatusSortPriority_healthy_test() ->
		?assertEqual(3, checkStatusSortPriority(healthy)).

	systemStatusSortPriority_failing_test() ->
		?assertEqual(0, systemStatusSortPriority(failing)).

	systemStatusSortPriority_unknown_test() ->
		?assertEqual(1, systemStatusSortPriority(unknown)).

	systemStatusSortPriority_buffering_test() ->
		?assertEqual(2, systemStatusSortPriority(buffering)).

	systemStatusSortPriority_suppressed_test() ->
		?assertEqual(3, systemStatusSortPriority(suppressed)).

	systemStatusSortPriority_pending_verification_test() ->
		?assertEqual(3, systemStatusSortPriority(pending_verification)).

	systemStatusSortPriority_healthy_test() ->
		?assertEqual(4, systemStatusSortPriority(healthy)).

	statusToCssClass_healthy_test() ->
		?assertEqual("healthy", statusToCssClass(healthy)).

	statusToCssClass_failing_test() ->
		?assertEqual("failing", statusToCssClass(failing)).

	statusToCssClass_unknown_test() ->
		?assertEqual("unknown", statusToCssClass(unknown)).

	statusToCssClass_buffering_test() ->
		?assertEqual("buffering", statusToCssClass(buffering)).

	statusToCssClass_pending_verification_test() ->
		?assertEqual("pending-verification", statusToCssClass(pending_verification)).

	statusToCssClass_suppressed_test() ->
		?assertEqual("suppressed", statusToCssClass(suppressed)).

	renderSystemChecks_order_test() ->
		SystemChecks = [
			#{<<"id">> => <<"b-healthy">>,   <<"status">> => healthy,   <<"statusText">> => <<"healthy">>},
			#{<<"id">> => <<"a-failing">>,   <<"status">> => failing,   <<"statusText">> => <<"failing">>},
			#{<<"id">> => <<"c-unknown">>,   <<"status">> => unknown,   <<"statusText">> => <<"unknown">>},
			#{<<"id">> => <<"d-buffering">>, <<"status">> => buffering, <<"statusText">> => <<"unknown (1)">>},
			#{<<"id">> => <<"a-healthy">>,   <<"status">> => healthy,   <<"statusText">> => <<"healthy">>}
		],
		Html = renderSystemChecks(SystemChecks),
		PosFailing   = string:str(Html, "a-failing"),
		PosUnknown   = string:str(Html, "c-unknown"),
		PosBuffering = string:str(Html, "d-buffering"),
		PosAHealthy  = string:str(Html, "a-healthy"),
		PosBHealthy  = string:str(Html, "b-healthy"),
		?assert(PosFailing   < PosUnknown,   "failing must come before unknown"),
		?assert(PosUnknown   < PosBuffering, "unknown must come before buffering"),
		?assert(PosBuffering < PosAHealthy,  "buffering must come before healthy"),
		?assert(PosAHealthy  < PosBHealthy,  "healthy checks must be sorted alphabetically").

	renderSystemMetrics_order_test() ->
		SystemMetrics = [
			#{<<"id">> => <<"z-metric">>, <<"value">> => 1, <<"techDetail">> => <<"">>},
			#{<<"id">> => <<"a-metric">>, <<"value">> => 2, <<"techDetail">> => <<"">>},
			#{<<"id">> => <<"m-metric">>, <<"value">> => 3, <<"techDetail">> => <<"">>}
		],
		Html = renderSystemMetrics(SystemMetrics),
		PosA = string:str(Html, "a-metric"),
		PosM = string:str(Html, "m-metric"),
		PosZ = string:str(Html, "z-metric"),
		?assert(PosA < PosM, "a-metric must come before m-metric"),
		?assert(PosM < PosZ, "m-metric must come before z-metric").

	% Regression test: non-ASCII bytes in techDetail (e.g. em-dash U+2014, UTF-8 bytes 226,128,148)
	% must NOT appear as Erlang integer-list form ([226,128,148,...]) or binary form (<<226,...>>).
	renderSystemMetrics_nonascii_techdetail_test() ->
		SystemMetrics = [
			#{<<"id">> => <<"test-metric">>, <<"value">> => 42, <<"techDetail">> => <<"Count \xe2\x80\x94 current total">>}
		],
		Html = renderSystemMetrics(SystemMetrics),
		?assertEqual(0, string:str(Html, "226,128,148"), "techDetail must not appear as integer-list form"),
		?assertEqual(0, string:str(Html, "<<226"), "techDetail must not appear as binary term form"),
		?assert(string:str(Html, "test-metric") > 0, "metric ID must appear in HTML").

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

-endif.
