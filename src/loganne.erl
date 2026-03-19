-module(loganne).
-export([notify/4]).

notify(Host, System, FailingChecks, Suppressed) ->
	{EventType, HumanReadable} = buildEvent(Host, System, FailingChecks, Suppressed),
	AppOrigin = os:getenv("APP_ORIGIN", ""),
	Url = AppOrigin ++ "/#host-" ++ Host,
	emit_event(EventType, HumanReadable, Url).

buildEvent(Host, System, FailingChecks, Suppressed) ->
	SystemStr = case System of
		unknown -> "unknown";
		_ -> System
	end,
	SystemTitle = lists:flatten(re:replace(SystemStr, "_", " ", [global, {return, list}])),
	case {maps:size(FailingChecks), Suppressed} of
		{0, _} ->
			{"monitoringRecovery", "All checks healthy on " ++ SystemTitle ++ " (" ++ Host ++ ")"};
		{FailCount, true} ->
			FailNames = string:join([binary_to_list(K) || K <- maps:keys(FailingChecks)], ", "),
			{"monitoringAlertSuppressed", integer_to_list(FailCount) ++ " failing check(s) on " ++ SystemTitle ++ " (" ++ Host ++ "): " ++ FailNames ++ " (suppressed during deploy window)"};
		{FailCount, false} ->
			FailNames = string:join([binary_to_list(K) || K <- maps:keys(FailingChecks)], ", "),
			{"monitoringAlert", integer_to_list(FailCount) ++ " failing check(s) on " ++ SystemTitle ++ " (" ++ Host ++ "): " ++ FailNames}
	end.

emit_event(EventType, HumanReadable, Url) ->
	Endpoint = os:getenv("LOGANNE_ENDPOINT"),
	case Endpoint of
		false ->
			io:format("LOGANNE_ENDPOINT not set, skipping event ~p~n", [EventType]);
		_ ->
			Body = jiffy:encode(#{
				<<"source">> => <<"lucos_monitoring">>,
				<<"type">> => list_to_binary(EventType),
				<<"humanReadable">> => list_to_binary(HumanReadable),
				<<"url">> => list_to_binary(Url)
			}),
			Request = {Endpoint, [], "application/json", Body},
			case httpc:request(post, Request, [], []) of
				{ok, {{_, StatusCode, _}, _, _}} when StatusCode >= 200, StatusCode < 300 ->
					io:format("Loganne event ~p sent successfully to ~p~n", [EventType, Endpoint]);
				{ok, {{_, StatusCode, _}, _, ResponseBody}} ->
					io:format("Loganne returned ~p posting ~p to ~p: ~p~n", [StatusCode, EventType, Endpoint, ResponseBody]),
					ok;
				{error, Reason} ->
					io:format("Failed to emit Loganne event ~p to ~p: ~p~n", [EventType, Endpoint, Reason]),
					ok
			end
	end,
	ok.


-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").

	buildEvent_test() ->
		%% Recovery: no failing checks
		?assertEqual(
			{"monitoringRecovery", "All checks healthy on lucos foo (foo.example.com)"},
			buildEvent("foo.example.com", "lucos_foo", #{}, false)
		),
		%% Recovery during suppression window still uses monitoringRecovery
		?assertEqual(
			{"monitoringRecovery", "All checks healthy on lucos foo (foo.example.com)"},
			buildEvent("foo.example.com", "lucos_foo", #{}, true)
		),
		%% Alert: one failing check, not suppressed
		?assertEqual(
			{"monitoringAlert", "1 failing check(s) on lucos foo (foo.example.com): ci"},
			buildEvent("foo.example.com", "lucos_foo", #{<<"ci">> => #{<<"ok">> => false}}, false)
		),
		%% Alert: one failing check, suppressed
		?assertEqual(
			{"monitoringAlertSuppressed", "1 failing check(s) on lucos foo (foo.example.com): ci (suppressed during deploy window)"},
			buildEvent("foo.example.com", "lucos_foo", #{<<"ci">> => #{<<"ok">> => false}}, true)
		).

	buildEvent_unknown_system_test() ->
		%% System = unknown (atom) should not crash — happens when /_info returns non-200
		?assertEqual(
			{"monitoringAlert", "1 failing check(s) on unknown (foo.example.com): fetch-info"},
			buildEvent("foo.example.com", unknown, #{<<"fetch-info">> => #{<<"ok">> => false}}, false)
		).

	emit_event_no_endpoint_test() ->
		%% When LOGANNE_ENDPOINT is unset, emit_event/3 should return without crashing
		os:unsetenv("LOGANNE_ENDPOINT"),
		?assertEqual(ok, emit_event("monitoringAlert", "Some alert", "https://monitoring.l42.eu/#host-foo.example.com")).

	notify_builds_url_test() ->
		%% notify/4 should derive the URL from APP_ORIGIN and Host.
		%% We can't easily intercept the HTTP call, so we test via emit_event behaviour:
		%% with no LOGANNE_ENDPOINT set, notify should return ok without crashing.
		os:unsetenv("LOGANNE_ENDPOINT"),
		os:putenv("APP_ORIGIN", "https://monitoring.l42.eu"),
		?assertEqual(ok, notify("foo.example.com", "lucos_foo", #{}, false)),
		os:unsetenv("APP_ORIGIN").

	notify_missing_app_origin_test() ->
		%% When APP_ORIGIN is unset, the url field should still be a valid (empty-prefixed) string.
		os:unsetenv("LOGANNE_ENDPOINT"),
		os:unsetenv("APP_ORIGIN"),
		?assertEqual(ok, notify("foo.example.com", "lucos_foo", #{}, false)).

-endif.
