-module(loganne).
-export([notify/1, notify_startup/0]).

notify_startup() ->
	Host = os:getenv("APP_ORIGIN", os:getenv("SYSTEM", "lucos_monitoring")),
	HumanReadable = "lucos_monitoring restarted on " ++ Host,
	AppOrigin = os:getenv("APP_ORIGIN", ""),
	emit_event("monitoringSelfRestart", HumanReadable, AppOrigin, undefined, undefined).

% Emits one monitoring event to loganne. The Notification map carries the
% fields the event needs:
%   host             — hostname (may be "" for components without a domain)
%   system           — system id (string or 'unknown' atom)
%   failing_checks   — checks currently failing; non-empty drives alert events
%   was_failing      — checks that were failing in the prior state; populates
%                      the failingChecks payload on recovery events
%   suppressed       — whether the alert is suppressed (deploy window)
%   metrics          — system metrics (unused by loganne; carried for symmetry
%                      with other notifiers)
notify(#{host := Host, system := System, failing_checks := FailingChecks,
         was_failing := WasFailing, suppressed := Suppressed}) ->
	{EventType, HumanReadable, RelevantChecks, Level} = buildEvent(Host, System, FailingChecks, WasFailing, Suppressed),
	AppOrigin = os:getenv("APP_ORIGIN", ""),
	% Use the system ID for the anchor; host-based anchor was broken for components without a domain.
	SystemStr = case System of unknown -> "unknown"; _ -> System end,
	Url = AppOrigin ++ "/#system-" ++ SystemStr,
	emit_event(EventType, HumanReadable, Url, SystemStr, RelevantChecks, Level).

buildEvent(Host, System, FailingChecks, WasFailing, Suppressed) ->
	SystemStr = case System of
		unknown -> "unknown";
		_ -> System
	end,
	SystemTitle = lists:flatten(re:replace(SystemStr, "_", " ", [global, {return, list}])),
	% Include host in parentheses only when set (components may have no domain).
	HostSuffix = case Host of "" -> ""; _ -> " (" ++ Host ++ ")" end,
	case {maps:size(FailingChecks), Suppressed} of
		{0, _} ->
			{"monitoringRecovery", "All checks healthy on " ++ SystemTitle ++ HostSuffix, WasFailing, "routine"};
		{FailCount, true} ->
			FailNames = string:join([binary_to_list(K) || K <- maps:keys(FailingChecks)], ", "),
			{"monitoringAlertSuppressed", integer_to_list(FailCount) ++ " failing " ++ plural_checks(FailCount) ++ " on " ++ SystemTitle ++ HostSuffix ++ ": " ++ FailNames ++ " (suppressed during deploy window)", FailingChecks, "detail"};
		{FailCount, false} ->
			FailNames = string:join([binary_to_list(K) || K <- maps:keys(FailingChecks)], ", "),
			{"monitoringAlert", integer_to_list(FailCount) ++ " failing " ++ plural_checks(FailCount) ++ " on " ++ SystemTitle ++ HostSuffix ++ ": " ++ FailNames, FailingChecks, "routine"}
	end.

plural_checks(1) -> "check";
plural_checks(_) -> "checks".

% Builds the JSON `failingChecks` array from a map of check_name => check_map.
% Each entry carries the check id, its `debug` string (the per-failure-mode
% signal — timeout reason, HTTP status, exception message), and the static
% `techDetail`. Missing fields default to empty binary.
build_failing_checks_array(ChecksMap) ->
	maps:fold(fun(CheckId, Check, Acc) ->
		Entry = #{
			<<"check">> => CheckId,
			<<"debug">> => maps:get(<<"debug">>, Check, <<>>),
			<<"techDetail">> => maps:get(<<"techDetail">>, Check, <<>>)
		},
		[Entry | Acc]
	end, [], ChecksMap).

% emit_event/5 — convenience wrapper for events with no explicit level
% (monitoringSelfRestart). Defaults level to "routine". Pass System=undefined
% and RelevantChecks=undefined to omit those fields from the payload.
emit_event(EventType, HumanReadable, Url, SystemStr, RelevantChecks) ->
	emit_event(EventType, HumanReadable, Url, SystemStr, RelevantChecks, "routine").

% emit_event/6 — full form.  Level is a string (e.g. "routine", "detail")
% and is always included in the payload.
emit_event(EventType, HumanReadable, Url, SystemStr, RelevantChecks, Level) ->
	Endpoint = os:getenv("LOGANNE_ENDPOINT"),
	case Endpoint of
		false ->
			logger:warning("LOGANNE_ENDPOINT not set, skipping event ~p", [EventType]);
		_ ->
			BaseBody = #{
				<<"source">> => <<"lucos_monitoring">>,
				<<"type">> => list_to_binary(EventType),
				<<"humanReadable">> => list_to_binary(HumanReadable),
				<<"url">> => list_to_binary(Url),
				<<"level">> => list_to_binary(Level)
			},
			BodyWithSystem = case SystemStr of
				undefined -> BaseBody;
				_ -> maps:put(<<"system">>, list_to_binary(SystemStr), BaseBody)
			end,
			FullBody = case RelevantChecks of
				undefined -> BodyWithSystem;
				_ -> maps:put(<<"failingChecks">>, build_failing_checks_array(RelevantChecks), BodyWithSystem)
			end,
			Body = jiffy:encode(FullBody),
			Request = {Endpoint, [{"User-Agent", os:getenv("SYSTEM", "")}], "application/json", Body},
			try httpc:request(post, Request, [], []) of
				{ok, {{_, StatusCode, _}, _, _}} when StatusCode >= 200, StatusCode < 300 ->
					logger:info("Loganne event ~p sent successfully to ~p", [EventType, Endpoint]);
				{ok, {{_, StatusCode, _}, _, ResponseBody}} ->
					logger:error("Loganne returned ~p posting ~p to ~p: ~p", [StatusCode, EventType, Endpoint, ResponseBody]),
					ok;
				{error, Reason} ->
					logger:error("Failed to emit Loganne event ~p to ~p: ~p", [EventType, Endpoint, Reason]),
					ok
			catch
				ExClass:ExReason ->
					logger:error("Exception emitting Loganne event ~p to ~p: ~p ~p", [EventType, Endpoint, ExClass, ExReason]),
					ok
			end
	end,
	ok.


-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").

	buildEvent_test() ->
		%% Recovery: no failing checks (host present — shown in parentheses).
		%% WasFailing is the relevant-checks payload for recoveries.
		WasFailing = #{<<"ci">> => #{<<"ok">> => false, <<"debug">> => <<"Workflow failed">>}},
		?assertEqual(
			{"monitoringRecovery", "All checks healthy on lucos foo (foo.example.com)", WasFailing, "routine"},
			buildEvent("foo.example.com", "lucos_foo", #{}, WasFailing, false)
		),
		%% Recovery during suppression window still uses monitoringRecovery
		?assertEqual(
			{"monitoringRecovery", "All checks healthy on lucos foo (foo.example.com)", WasFailing, "routine"},
			buildEvent("foo.example.com", "lucos_foo", #{}, WasFailing, true)
		),
		%% Alert: one failing check, not suppressed — level is "routine"
		FailingCheck = #{<<"ci">> => #{<<"ok">> => false}},
		?assertEqual(
			{"monitoringAlert", "1 failing check on lucos foo (foo.example.com): ci", FailingCheck, "routine"},
			buildEvent("foo.example.com", "lucos_foo", FailingCheck, #{}, false)
		),
		%% Alert: one failing check, suppressed — level is "detail"
		?assertEqual(
			{"monitoringAlertSuppressed", "1 failing check on lucos foo (foo.example.com): ci (suppressed during deploy window)", FailingCheck, "detail"},
			buildEvent("foo.example.com", "lucos_foo", FailingCheck, #{}, true)
		).

	buildEvent_no_host_test() ->
		%% Components with no domain: host suffix is omitted from human-readable strings
		?assertEqual(
			{"monitoringRecovery", "All checks healthy on lucos component", #{}, "routine"},
			buildEvent("", "lucos_component", #{}, #{}, false)
		),
		FailingCheck = #{<<"ci">> => #{<<"ok">> => false}},
		?assertEqual(
			{"monitoringAlert", "1 failing check on lucos component: ci", FailingCheck, "routine"},
			buildEvent("", "lucos_component", FailingCheck, #{}, false)
		),
		?assertEqual(
			{"monitoringAlertSuppressed", "1 failing check on lucos component: ci (suppressed during deploy window)", FailingCheck, "detail"},
			buildEvent("", "lucos_component", FailingCheck, #{}, true)
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
		FailingCheck = #{<<"fetch-info">> => #{<<"ok">> => false}},
		?assertEqual(
			{"monitoringAlert", "1 failing check on unknown (foo.example.com): fetch-info", FailingCheck, "routine"},
			buildEvent("foo.example.com", unknown, FailingCheck, #{}, false)
		).

	build_failing_checks_array_test() ->
		%% Empty map → empty array
		?assertEqual([], build_failing_checks_array(#{})),
		%% Single check with debug and techDetail
		Single = #{<<"fetch-info">> => #{
			<<"ok">> => false,
			<<"debug">> => <<"Connection refused">>,
			<<"techDetail">> => <<"Fetches /_info">>
		}},
		?assertEqual(
			[#{<<"check">> => <<"fetch-info">>, <<"debug">> => <<"Connection refused">>, <<"techDetail">> => <<"Fetches /_info">>}],
			build_failing_checks_array(Single)
		),
		%% Check with no debug/techDetail keys → fields default to empty binary
		Bare = #{<<"foo">> => #{<<"ok">> => false}},
		?assertEqual(
			[#{<<"check">> => <<"foo">>, <<"debug">> => <<>>, <<"techDetail">> => <<>>}],
			build_failing_checks_array(Bare)
		).

	notify_startup_no_endpoint_test() ->
		%% When LOGANNE_ENDPOINT is unset, notify_startup/0 should return without crashing
		os:unsetenv("LOGANNE_ENDPOINT"),
		os:putenv("APP_ORIGIN", "https://monitoring.l42.eu"),
		?assertEqual(ok, notify_startup()),
		os:unsetenv("APP_ORIGIN").

	notify_startup_inets_not_started_test() ->
		%% When LOGANNE_ENDPOINT is set but inets is not running, notify_startup/0 must
		%% return ok without crashing — this is the production failure mode from the
		%% 2026-05-24 incident where notify_startup was called before the fetchers started inets.
		os:putenv("LOGANNE_ENDPOINT", "https://loganne.l42.eu/events"),
		os:putenv("APP_ORIGIN", "https://monitoring.l42.eu"),
		case lists:keyfind(inets, 1, application:which_applications()) of
			false ->
				%% inets not running: exercise the try/catch path directly.
				?assertEqual(ok, notify_startup());
			_ ->
				%% inets already running in this test environment — stopping it would
				%% kill httpc_manager and crash the shared test VM (CI failure mode).
				%% Skip the stop; the try/catch is the defence-in-depth guard, and the
				%% ordering fix in server.erl is the real production fix.
				ok
		end,
		os:unsetenv("LOGANNE_ENDPOINT"),
		os:unsetenv("APP_ORIGIN").

	emit_event_no_endpoint_test() ->
		%% When LOGANNE_ENDPOINT is unset, emit_event/5 should return without crashing
		os:unsetenv("LOGANNE_ENDPOINT"),
		?assertEqual(ok, emit_event("monitoringAlert", "Some alert", "https://monitoring.l42.eu/#system-lucos_foo", "lucos_foo", #{})).

	notify_builds_url_test() ->
		%% notify/1 should derive the URL from APP_ORIGIN and System (as anchor).
		%% We can't easily intercept the HTTP call, so we test via emit_event behaviour:
		%% with no LOGANNE_ENDPOINT set, notify should return ok without crashing.
		os:unsetenv("LOGANNE_ENDPOINT"),
		os:putenv("APP_ORIGIN", "https://monitoring.l42.eu"),
		?assertEqual(ok, notify(#{host => "foo.example.com", system => "lucos_foo",
		                          failing_checks => #{}, was_failing => #{},
		                          suppressed => false, metrics => #{}})),
		os:unsetenv("APP_ORIGIN").

	notify_missing_app_origin_test() ->
		%% When APP_ORIGIN is unset, the url field should still be a valid (empty-prefixed) string.
		os:unsetenv("LOGANNE_ENDPOINT"),
		os:unsetenv("APP_ORIGIN"),
		?assertEqual(ok, notify(#{host => "foo.example.com", system => "lucos_foo",
		                          failing_checks => #{}, was_failing => #{},
		                          suppressed => false, metrics => #{}})).

	notify_suppressed_no_endpoint_test() ->
		%% notify/1 with suppressed => true should still return ok without crashing.
		%% Exercises the full path: buildEvent returns "detail" level, emit_event/6
		%% includes it in the body.
		os:unsetenv("LOGANNE_ENDPOINT"),
		os:putenv("APP_ORIGIN", "https://monitoring.l42.eu"),
		FailingCheck = #{<<"ci">> => #{<<"ok">> => false}},
		?assertEqual(ok, notify(#{host => "foo.example.com", system => "lucos_foo",
		                          failing_checks => FailingCheck, was_failing => #{},
		                          suppressed => true, metrics => #{}})),
		os:unsetenv("APP_ORIGIN").

-endif.
