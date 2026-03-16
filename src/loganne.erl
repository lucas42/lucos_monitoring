-module(loganne).
-export([emit_event/2]).

emit_event(EventType, HumanReadable) ->
	Endpoint = os:getenv("LOGANNE_ENDPOINT"),
	case Endpoint of
		false ->
			io:format("LOGANNE_ENDPOINT not set, skipping event ~p~n", [EventType]);
		_ ->
			Url = Endpoint ++ "/events",
			Body = jiffy:encode(#{
				<<"source">> => <<"lucos_monitoring">>,
				<<"type">> => list_to_binary(EventType),
				<<"humanReadable">> => list_to_binary(HumanReadable)
			}),
			Request = {Url, [], "application/json", Body},
			case httpc:request(post, Request, [], []) of
				{ok, {{_, StatusCode, _}, _, _}} when StatusCode >= 200, StatusCode < 300 ->
					ok;
				{ok, {{_, StatusCode, _}, _, ResponseBody}} ->
					io:format("Loganne returned ~p for event ~p: ~p~n", [StatusCode, EventType, ResponseBody]),
					ok;
				{error, Reason} ->
					io:format("Failed to emit Loganne event ~p: ~p~n", [EventType, Reason]),
					ok
			end
	end,
	ok.


-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").

	emit_event_no_endpoint_test() ->
		%% When LOGANNE_ENDPOINT is unset, emit_event/2 should return without crashing
		os:unsetenv("LOGANNE_ENDPOINT"),
		?assertEqual(ok, emit_event("monitoringAlert", "Some alert")).

-endif.
