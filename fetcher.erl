-module(fetcher).
-export([start/0, fetch/1]).

start() ->
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
	io:format("Fetch: ~p~n", [InfoURL]).