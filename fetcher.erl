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
			{TLSCheck} = checkTlsExpiry(Host),
			AllChecks = maps:put(<<"tls-certificate">>, TLSCheck, Checks),
			ok = gen_server:cast(StatePid, {updateSystem, Host, System, AllChecks, Metrics});
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

checkTlsExpiry(Host) ->
	TechDetail = <<"Checks whether the TLS Certificate is valid and not about to expire">>,
	Command = "echo | openssl s_client -connect "++Host++":443 -servername "++Host++" 2>/dev/null | openssl x509 -noout -enddate | sed 's/.*=//' | date +'%s' -f -",
	Output = os:cmd(Command),
	case string:to_integer(Output) of
		{error, _Reason} ->
			Check = #{
				<<"ok">> => false,
				<<"techDetail">> => TechDetail,
				<<"debug">> => <<"Can't get expiry time for TLS Cert">>
			},
			{Check};
		{Expiry, _Rest} ->
			Diff = Expiry - erlang:system_time(second),
			if
				% start failing when there's fewer than 20 days until expiry
				Diff < 1728000 ->
					Debug = list_to_binary("TLS Certificate due to expire in "++integer_to_list(Diff)++" seconds"),
					Check = #{
						<<"ok">> => false,
						<<"techDetail">> => TechDetail,
						<<"debug">> => Debug
					},
					{Check};
				true ->
					Check = #{
						<<"ok">> => true,
						<<"techDetail">> => TechDetail
					},
					{Check}
			end
	end.
