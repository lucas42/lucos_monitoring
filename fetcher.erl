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
			Info = jiffy:decode(Body),
			io:format("Success from ~p: ~p~n",[Host, Info]);
		{ok, {{_Version, StatusCode, ReasonPhrase}, _Headers, _Body}} ->
			io:format("Error for ~p: Recieved ~p ~p from ~p~n", [Host, StatusCode, ReasonPhrase, InfoURL]);
		{error, Reason} ->
			io:format("Error for ~p: ~p~n", [Host, Reason])
	end.
    