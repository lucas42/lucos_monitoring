-module(fetcher).
-export([start/0, fetch/1]).

start() ->
	ok = application:start(crypto),
	ok = application:start(asn1),
	ok = application:start(public_key),
	ok = application:start(ssl),
	ok = application:start(inets),
	{ok, Device} = file:open("./service-list", [read]),
	spawnFetcher(Device).

spawnFetcher(Device) ->
	case io:get_line(Device, "") of
		eof  -> ok;
		Line -> 
			Host = string:trim(Line),
			spawn(?MODULE, fetch, [Host]),
			spawnFetcher(Device)
	end.

fetch(Host) ->
	InfoURL = "https://" ++ Host ++ "/_info",
	io:format("Fetch: ~p~n", [InfoURL]),
	case httpc:request(InfoURL) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
			{System, Checks, Metrics} = parseInfo(Body),
			io:format("Success from ~p: ~n\t~p~n\t~p~n\t~p~n",[Host, System, Checks, Metrics]);
		{ok, {{_Version, StatusCode, ReasonPhrase}, _Headers, _Body}} ->
			io:format("Error for ~p: Recieved ~p ~p from ~p~n", [Host, StatusCode, ReasonPhrase, InfoURL]);
		{error, Reason} ->
			io:format("Error for ~p: ~p~n", [Host, Reason])
	end.

parseInfo(Body) ->
	Info = jiffy:decode(Body, [return_maps]),
	System = maps:get(<<"system">>, Info),
	Checks = maps:get(<<"checks">>, Info, #{}),
	Metrics = maps:get(<<"metrics">>, Info, #{}),
	{System, Checks, Metrics}.
