-module(loganne).
-export([notify/5]).

notify(Host, System, FailingChecks, Suppressed, _Metrics) ->
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
			{"monitoringAlertSuppressed", integer_to_list(FailCount) ++ " failing " ++ plural_checks(FailCount) ++ " on " ++ SystemTitle ++ " (" ++ Host ++ "): " ++ FailNames ++ " (suppressed during deploy window)"};
		{FailCount, false} ->
			FailNames = string:join([binary_to_list(K) || K <- maps:keys(FailingChecks)], ", "),
			{"monitoringAlert", integer_to_list(FailCount) ++ " failing " ++ plural_checks(FailCount) ++ " on " ++ SystemTitle ++ " (" ++ Host ++ "): " ++ FailNames}
	end.

plural_checks(1) -> "check";
plural_checks(_) -> "checks".

emit_event(EventType, HumanReadable, Url) ->
	Endpoint = os:getenv("LOGANNE_ENDPOINT"),
	case Endpoint of
		false ->
			logger:warning("LOGANNE_ENDPOINT not set, skipping event ~p", [EventType]);
		_ ->
			Body = jiffy:encode(#{
				<<"source">> => <<"lucos_monitoring">>,
				<<"type">> => list_to_binary(EventType),
				<<"humanReadable">> => list_to_binary(HumanReadable),
				<<"url">> => list_to_binary(Url)
			}),
			Request = {Endpoint, [{"User-Agent", os:getenv("SYSTEM", "")}], "application/json", Body},
			case httpc:request(post, Request, [], []) of
				{ok, {{_, StatusCode, _}, _, _}} when StatusCode >= 200, StatusCode < 300 ->
					logger:info("Loganne event ~p sent successfully to ~p", [EventType, Endpoint]);
				{ok, {{_, StatusCode, _}, _, ResponseBody}} ->
					logger:error("Loganne returned ~p posting ~p to ~p: ~p", [StatusCode, EventType, Endpoint, ResponseBody]),
					ok;
				{error, Reason} ->
					logger:error("Failed to emit Loganne event ~p to ~p: ~p", [EventType, Endpoint, Reason]),
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
		%% Alert: one failing check, not suppressed (singular)
		?assertEqual(
			{"monitoringAlert", "1 failing check on lucos foo (foo.example.com): ci"},
			buildEvent("foo.example.com", "lucos_foo", #{<<"ci">> => #{<<"ok">> => false}}, false)
		),
		%% Alert: one failing check, suppressed (singular)
		?assertEqual(
			{"monitoringAlertSuppressed", "1 failing check on lucos foo (foo.example.com): ci (suppressed during deploy window)"},
			buildEvent("foo.example.com", "lucos_foo", #{<<"ci">> => #{<<"ok">> => false}}, true)
		).

	plural_checks_test() ->
		%% Singular for FailCount = 1
		?assertEqual("check", plural_checks(1)),
		%% Plural for FailCount = 2
		?assertEqual("checks", plural_checks(2)),
		%% Plural for larger counts
		?assertEqual("checks", plural_checks(10)).

	buildEvent_unknown_system_test() ->
		%% System = unknown (atom) should not crash — happens when /_info returns non-200
		?assertEqual(
			{"monitoringAlert", "1 failing check on unknown (foo.example.com): fetch-info"},
			buildEvent("foo.example.com", unknown, #{<<"fetch-info">> => #{<<"ok">> => false}}, false)
		).

	emit_event_no_endpoint_test() ->
		%% When LOGANNE_ENDPOINT is unset, emit_event/3 should return without crashing
		os:unsetenv("LOGANNE_ENDPOINT"),
		?assertEqual(ok, emit_event("monitoringAlert", "Some alert", "https://monitoring.l42.eu/#host-foo.example.com")).

	notify_builds_url_test() ->
		%% notify/5 should derive the URL from APP_ORIGIN and Host.
		%% We can't easily intercept the HTTP call, so we test via emit_event behaviour:
		%% with no LOGANNE_ENDPOINT set, notify should return ok without crashing.
		os:unsetenv("LOGANNE_ENDPOINT"),
		os:putenv("APP_ORIGIN", "https://monitoring.l42.eu"),
		?assertEqual(ok, notify("foo.example.com", "lucos_foo", #{}, false, #{})),
		os:unsetenv("APP_ORIGIN").

	notify_missing_app_origin_test() ->
		%% When APP_ORIGIN is unset, the url field should still be a valid (empty-prefixed) string.
		os:unsetenv("LOGANNE_ENDPOINT"),
		os:unsetenv("APP_ORIGIN"),
		?assertEqual(ok, notify("foo.example.com", "lucos_foo", #{}, false, #{})).

-endif.
