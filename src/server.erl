-module(server).
-export([start/2, accept/3, handleRequest/2]).
% View logic (HTML rendering, CSS, templates) lives in view.erl.

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

encodeInfo(Systems, PollStats) ->
	PollMetrics = case maps:get(count, PollStats, 0) > 0 of
		true ->
			#{
				<<"poll-max-duration-ms">> => #{
					<<"value">> => maps:get(max_duration_ms, PollStats, 0),
					<<"techDetail">> => <<"Maximum check duration (ms) across all systems in the most recent poll cycle">>
				},
				<<"poll-mean-duration-ms">> => #{
					<<"value">> => maps:get(mean_duration_ms, PollStats, 0),
					<<"techDetail">> => <<"Mean check duration (ms) across all systems in the most recent poll cycle">>
				},
				<<"poll-failed-checks">> => #{
					<<"value">> => maps:get(failed_count, PollStats, 0),
					<<"techDetail">> => <<"Number of systems whose most recent poll had a fetch-info failure">>
				}
			};
		false ->
			#{}
	end,
	jiffy:encode(#{
		system => <<"lucos_monitoring">>,
		checks => #{},
		metrics => maps:merge(#{
			<<"system-count">> => #{
				<<"value">> => length(Systems),
				<<"techDetail">> => <<"The number of systems being monitored">>
			}
		}, PollMetrics),
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
			{200, "text/html", view:render_page(Systems)};
		"/style.css" ->
			{ok, CssFile} = file:read_file("style.css"),
			{200, "text/css", binary_to_list(CssFile)};
		"/robots.txt" ->
			{200, "text/plain", "User-agent: *\nDisallow:\n"};
		"/api/status" ->
			Systems = gen_server:call(StatePid, {fetch, all}),
			{200, "application/json", encodeStatus(Systems)};
		"/_info" ->
			Systems = gen_server:call(StatePid, {fetch, all}),
			PollStats = gen_server:call(StatePid, {fetch, poll_stats}),
			{200, "application/json", encodeInfo(Systems, PollStats)};
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

	% encodeInfo: no poll stats (count=0) → only system-count metric
	encodeInfo_no_poll_stats_test() ->
		EmptyStats = #{count => 0, max_duration_ms => 0, mean_duration_ms => 0, failed_count => 0},
		Result = jiffy:decode(encodeInfo([], EmptyStats), [return_maps]),
		Metrics = maps:get(<<"metrics">>, Result),
		?assert(maps:is_key(<<"system-count">>, Metrics)),
		?assertNot(maps:is_key(<<"poll-max-duration-ms">>, Metrics)),
		?assertNot(maps:is_key(<<"poll-mean-duration-ms">>, Metrics)),
		?assertNot(maps:is_key(<<"poll-failed-checks">>, Metrics)).

	% encodeInfo: with poll stats → all four metrics present
	encodeInfo_with_poll_stats_test() ->
		PollStats = #{count => 10, max_duration_ms => 900, mean_duration_ms => 350, failed_count => 2},
		Result = jiffy:decode(encodeInfo([], PollStats), [return_maps]),
		Metrics = maps:get(<<"metrics">>, Result),
		?assert(maps:is_key(<<"system-count">>, Metrics)),
		?assert(maps:is_key(<<"poll-max-duration-ms">>, Metrics)),
		?assert(maps:is_key(<<"poll-mean-duration-ms">>, Metrics)),
		?assert(maps:is_key(<<"poll-failed-checks">>, Metrics)),
		?assertEqual(900, maps:get(<<"value">>, maps:get(<<"poll-max-duration-ms">>, Metrics))),
		?assertEqual(350, maps:get(<<"value">>, maps:get(<<"poll-mean-duration-ms">>, Metrics))),
		?assertEqual(2, maps:get(<<"value">>, maps:get(<<"poll-failed-checks">>, Metrics))).

	% encodeInfo: system-count reflects number of systems
	encodeInfo_system_count_test() ->
		PollStats = #{count => 0},
		Systems = [#{<<"host">> => <<"a">>, <<"name">> => <<"s">>, <<"status">> => healthy, <<"checks">> => [], <<"metrics">> => []},
		           #{<<"host">> => <<"b">>, <<"name">> => <<"t">>, <<"status">> => healthy, <<"checks">> => [], <<"metrics">> => []}],
		Result = jiffy:decode(encodeInfo(Systems, PollStats), [return_maps]),
		?assertEqual(2, maps:get(<<"value">>, maps:get(<<"system-count">>, maps:get(<<"metrics">>, Result)))).

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
