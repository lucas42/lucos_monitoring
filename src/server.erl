-module(server).
-export([start/2, accept/3, handleRequest/2]).
% View logic (HTML rendering, CSS, templates) lives in view.erl.

start(_StartType, _StartArgs) ->
	configureLogLevel(),
	try
		{Port, _} = string:to_integer(os:getenv("PORT", "8080")),
		stream_handler:start(),
		{ok, StatePid} = monitoring_state_server:start_link(
			[fun loganne:notify/1, fun email:notify/1],
			stream_handler:make_publish_fun()
		),
		SchedulerCount = erlang:system_info(schedulers),
		Opts = [{active, false},
				binary,
				{packet, http_bin},
				{reuseaddr, true},
				inet6,
				{ipv6_v6only, false}],
		listen_with_retry(Port, Opts, StatePid, SchedulerCount, 30)
	catch
		Exception:Reason -> logger:emergency("Startup error occured: ~p ~p", [Exception, Reason])
	end.

configureLogLevel() ->
	LevelStr = os:getenv("LOG_LEVEL", "notice"),
	Level = list_to_atom(LevelStr),
	logger:set_primary_config(level, Level),
	logger:update_handler_config(default, formatter, {logger_formatter, #{
		single_line => true,
		legacy_header => false,
		template => [time, " [", level, "] ", msg, "\n"]
	}}).

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
			fetcher_scheduled_jobs:start(StatePid),
			fetcher_ports:start(StatePid),
			loganne:notify_startup(),
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
			Path = re:replace(RequestUri, "\\?.*$", "", [{return, list}]),
			case {Method, Path} of
				{'GET', "/event-stream"} ->
					ClientIP = getClientIP(Socket),
					SseHeaders = "HTTP/1.1 200 OK\n"
						++ "Content-Type: text/event-stream; charset=utf-8\n"
						++ "Cache-Control: no-cache\n"
						++ "Connection: keep-alive\n"
						++ "X-Accel-Buffering: no\n"
						++ "\n",
					gen_tcp:send(Socket, SseHeaders),
					inet:setopts(Socket, [{packet, raw}]),
					logger:notice("~p GET 200 /event-stream", [ClientIP]),
					stream_handler:subscribe(Socket);
				_ ->
					ContentLength = maps:get('Content-Length', Headers, 0),
					RequestBody = readBody(Socket, ContentLength),
					ClientIP = getClientIP(Socket),
					{StatusCode, ContentType, ResponseBody} = tryController(Method, RequestUri, RequestBody, Headers, StatePid),
					Response = getHeaders(StatusCode, ContentType) ++ ResponseBody,
					gen_tcp:send(Socket, Response),
					gen_tcp:close(Socket),
					AccessLogLevel = accessLogLevel(RequestUri),
					logger:log(AccessLogLevel, "~p ~p ~p ~p", [ClientIP, Method, StatusCode, RequestUri]),
					ok
			end;
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


controller(Method, RequestUri, Body, Headers, StatePid) ->
	Path = re:replace(RequestUri, "\\?.*$", "", [{return,list}]),
	case Path of
		"/" ->
			Systems = gen_server:call(StatePid, {fetch, all}),
			{200, "text/html", view:render_page(Systems)};
		"/style.css" ->
			{ok, CssFile} = file:read_file("style.css"),
			{200, "text/css", binary_to_list(CssFile)};
		"/robots.txt" ->
			{200, "text/plain", "User-agent: *\nDisallow:\n"};
		"/api/status" ->
			Systems = gen_server:call(StatePid, {fetch, all}),
			{200, "application/json", view_json:encodeStatus(Systems)};
		"/_info" ->
			Systems = gen_server:call(StatePid, {fetch, all}),
			PollStats = gen_server:call(StatePid, {fetch, poll_stats}),
			{200, "application/json", view_json:encodeInfo(Systems, PollStats)};
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

	suppress_clear_requires_auth_test() ->
		% Phase 3: /suppress/clear requires a valid token
		os:putenv("CLIENT_KEYS", "lucos_deploy_orb=mysecrettoken"),
		{ok, StatePid} = monitoring_state_server:start_link([], fun(_) -> ok end),
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
		{ok, StatePid} = monitoring_state_server:start_link([], fun(_) -> ok end),
		Body = "{\"systemDeployed\":\"lucos_test\"}",
		{StatusCode, _, _} = tryController('POST', "/suppress/clear", Body, #{'Authorization' => "Bearer wrongtoken"}, StatePid),
		gen_server:stop(StatePid),
		os:unsetenv("CLIENT_KEYS"),
		?assertEqual(401, StatusCode).

	suppress_other_routes_still_require_auth_test() ->
		% Other /suppress/* routes must still require auth
		os:putenv("CLIENT_KEYS", "lucos_deploy_orb=mysecrettoken"),
		{ok, StatePid} = monitoring_state_server:start_link([], fun(_) -> ok end),
		{StatusCode, _, _} = tryController('PUT', "/suppress/lucos_test", "", #{}, StatePid),
		gen_server:stop(StatePid),
		os:unsetenv("CLIENT_KEYS"),
		?assertEqual(401, StatusCode).

-endif.
