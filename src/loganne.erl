-module(loganne).
-export([notify/4]).

notify(Host, System, FailingChecks, Suppressed) ->
	{EventType, HumanReadable} = buildEvent(Host, System, FailingChecks, Suppressed),
	emit_event(EventType, HumanReadable).

buildEvent(Host, System, FailingChecks, Suppressed) ->
	SystemTitle = lists:flatten(re:replace(System, "_", " ", [global, {return, list}])),
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

emit_event(EventType, HumanReadable) ->
	Endpoint = os:getenv("LOGANNE_ENDPOINT"),
	case Endpoint of
		false ->
			io:format("LOGANNE_ENDPOINT not set, skipping event ~p~n", [EventType]);
		_ ->
			Body = jiffy:encode(#{
				<<"source">> => <<"lucos_monitoring">>,
				<<"type">> => list_to_binary(EventType),
				<<"humanReadable">> => list_to_binary(HumanReadable)
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

	emit_event_no_endpoint_test() ->
		%% When LOGANNE_ENDPOINT is unset, emit_event/2 should return without crashing
		os:unsetenv("LOGANNE_ENDPOINT"),
		?assertEqual(ok, emit_event("monitoringAlert", "Some alert")).

-endif.
