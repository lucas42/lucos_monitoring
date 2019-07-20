-module(fetcher).
-export([start/1, fetch/2]).

start(StatePid) ->
	ok = application:start(crypto),
	ok = application:start(asn1),
	ok = application:start(public_key),
	ok = application:start(ssl),
	ok = application:start(inets),
	{ok, Device} = file:open("./service-list", [read]),
	spawnFetcher(StatePid, Device).

spawnFetcher(StatePid, Device) ->
	case io:get_line(Device, "") of
		eof  -> ok;
		Line -> 
			Host = string:trim(Line),
			spawn(?MODULE, fetch, [StatePid, Host]),
			spawnFetcher(StatePid, Device)
	end.

fetch(StatePid, Host) ->
	InfoURL = "https://" ++ Host ++ "/_info",
	case httpc:request(InfoURL) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
			{System, Checks, Metrics} = parseInfo(Body),
			ok = gen_server:cast(StatePid, {updateSystem, Host, System, Checks, Metrics});
		{ok, {{_Version, StatusCode, ReasonPhrase}, _Headers, _Body}} ->
			ok = gen_server:cast(StatePid, {systemError, Host, {http_error, {StatusCode, ReasonPhrase}}});
		{error, Error} ->
			ok = gen_server:cast(StatePid, {systemError, Host, Error})
	end,
	timer:sleep(timer:seconds(60)),
	fetch(StatePid, Host).

parseInfo(Body) ->
	Info = jiffy:decode(Body, [return_maps]),
	System = binary_to_list(maps:get(<<"system">>, Info)),
	Checks = maps:get(<<"checks">>, Info, #{}),
	Metrics = maps:get(<<"metrics">>, Info, #{}),
	{System, Checks, Metrics}.
