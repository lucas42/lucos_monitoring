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
	{TLSCheck} = checkTlsExpiry(Host),
	{InfoCheck, System, Checks, Metrics} = fetchInfo(Host),
	AllChecks = maps:merge(#{
		<<"fetch-info">> => InfoCheck,
		<<"tls-certificate">> => TLSCheck
	}, Checks),
	ok = gen_server:cast(StatePid, {updateSystem, Host, System, AllChecks, Metrics}),
	timer:sleep(timer:seconds(60)),
	fetch(StatePid, Host).

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

parseInfo(Body) ->
	Info = jiffy:decode(Body, [return_maps]),
	System = binary_to_list(maps:get(<<"system">>, Info)),
	Checks = maps:get(<<"checks">>, Info, #{}),
	Metrics = maps:get(<<"metrics">>, Info, #{}),
	{System, Checks, Metrics}.


parseError(Error) ->
	case Error of
		{http_error, {StatusCode, ReasonPhrase}} ->
			"Received HTTP response with status "++integer_to_list(StatusCode)++" "++ReasonPhrase;
		{failed_connect, [{to_address, {Host, _Port}}, {inet,[inet],nxdomain}]} ->
			"DNS failure when trying to resolve "++Host;
		{failed_connect, [{to_address, {Host, Port}}, {inet,[inet],econnrefused}]} ->
			"Failed to establish a TCP connection to host "++Host++" on port "++integer_to_list(Port);
		{failed_connect, [{to_address, {Host, Port}}, {inet,[inet],etimedout}]} ->
			"TCP connection timed out whilst connecting to "++Host++" on port "++integer_to_list(Port);
		{failed_connect, [{to_address, {Host, Port}}, {inet,[inet],timeout}]} ->
			"HTTP connection timed out whilst connecting to "++Host++" on port "++integer_to_list(Port);
		{ErrorType, _Details} ->
			"An unknown error of type "++atom_to_list(ErrorType)++" occured: "++lists:flatten(io_lib:format("~p",[Error]))
	end.

fetchInfo(Host) ->
	InfoURL = "https://" ++ Host ++ "/_info",
	TechDetail = list_to_binary("Makes HTTP request to "++InfoURL++""),
	case httpc:request(get, {InfoURL, []}, [{timeout, timer:seconds(1)}], []) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
			{System, Checks, Metrics} = parseInfo(Body),
			InfoCheck = #{
				<<"ok">> => true,
				<<"techDetail">> => TechDetail
			},
			{InfoCheck, System, Checks, Metrics};
		{ok, {{_Version, StatusCode, ReasonPhrase}, _Headers, _Body}} ->
			Error = {http_error, {StatusCode, ReasonPhrase}},
			InfoCheck = #{
				<<"ok">> => false,
				<<"techDetail">> => TechDetail,
				<<"debug">> => list_to_binary(parseError(Error))
			},
			{InfoCheck, unknown, #{}, #{}};
		{error, Error} ->
			InfoCheck = #{
				<<"ok">> => false,
				<<"techDetail">> => TechDetail,
				<<"debug">> => list_to_binary(parseError(Error))
			},
			{InfoCheck, unknown, #{}, #{}}
	end.
