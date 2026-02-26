-module(fetcher).
-export([start/1, tryRunChecks/2]).
-include_lib("eunit/include/eunit.hrl").

start(StatePid) ->
	ok = application:start(crypto),
	ok = application:start(asn1),
	ok = application:start(public_key),
	ok = application:start(ssl),
	ok = application:start(inets),
	{ok, Device} = file:open("./service-list", [read]),
	spawnFetcher(StatePid, Device),
	file:close(Device).

spawnFetcher(StatePid, Device) ->
	case io:get_line(Device, "") of
		eof  -> ok;
		Line -> 
			Host = string:trim(Line),
			spawn(?MODULE, tryRunChecks, [StatePid, Host]),
			spawnFetcher(StatePid, Device)
	end.

tryRunChecks(StatePid, Host) ->
	try runChecks(StatePid, Host) of
		_ -> ok
	catch
		ExceptionClass:Term:StackTrace ->
			io:format("ExceptionClass: ~p Term: ~p StackTrace: ~p~n", [ExceptionClass, Term, StackTrace])
	end,
	timer:sleep(timer:seconds(60)),
	tryRunChecks(StatePid, Host).

runChecks(StatePid, Host) ->
	{TLSCheck} = checkTlsExpiry(Host),
	{InfoCheck, System, Checks, Metrics, CircleCISlug} = fetchInfo(Host),
	CIChecks = checkCI(CircleCISlug),
	AllChecks = maps:merge(
		maps:merge(#{
			<<"fetch-info">> => InfoCheck,
			<<"tls-certificate">> => TLSCheck
		}, CIChecks)
	, Checks),
	ok = gen_server:cast(StatePid, {updateSystem, Host, System, AllChecks, Metrics}).

checkTlsExpiry(Host) ->
	TechDetail = <<"Checks whether the TLS Certificate is valid and not about to expire">>,
	Command = "echo | timeout 1s openssl s_client -connect "++Host++":443 -servername "++Host++" 2>/dev/null | openssl x509 -noout -enddate | sed 's/.*=//' | date +'%s' -f -",
	Output = os:cmd(Command),
	case string:to_integer(Output) of
		{error, _Reason} ->
			Check = #{
				<<"ok">> => unknown,
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
	Checks = maps:merge(#{}, maps:get(<<"checks">>, Info, #{})),
	Metrics = maps:merge(#{}, maps:get(<<"metrics">>, Info, #{})),
	CircleCISlug = maps:get(<<"circle">>, maps:get(<<"ci">>, Info, #{}), null),
	{System, Checks, Metrics, CircleCISlug}.


parseError(Error) ->
	case Error of
		{failed_connect, [{to_address, {Host, Port}}, {inet,[inet],Ipv4ErrorType}]} ->
			parseConnectionError(Host, Port, 4, Ipv4ErrorType);
		{failed_connect, [{to_address, {Host, Port}}, {inet6,[inet6],Ipv6ErrorType}]} ->
			parseConnectionError(Host, Port, 6, Ipv6ErrorType);
		{failed_connect, [{to_address, {Host, Port}}, {inet6,[inet6],Ipv6ErrorType}, {inet,[inet],Ipv4ErrorType}]} ->
			{Status4, Debug4} = parseConnectionError(Host, Port, 4, Ipv4ErrorType),
			{Status6, Debug6} = parseConnectionError(Host, Port, 6, Ipv6ErrorType),
			Debug = Debug4++"; "++Debug6,
			Status = case {Status4, Status6} of
				{unknown, unknown} ->
					unknown;
				{false, false} ->
					false;
				{false, unknown} ->
					false;
				{unknown, false} ->
					false
			end,
			{Status, Debug};
		socket_closed_remotely ->
			{false, "Socket closed remotely"};
		timeout ->
			{unknown, "HTTP Request timed out"};
		_ ->
			io:format("Unknown error handled: ~p~n",[Error]),
			{false, "An unknown error occured: "++lists:flatten(io_lib:format("~p",[Error]))}
	end.

parseConnectionError(Host, Port, IpVersion, ErrorType) ->
	case ErrorType of
		nxdomain ->
			{unknown, "DNS failure when trying to resolve ipv"++integer_to_list(IpVersion)++" address for "++Host};
		ehostunreach ->
			{unknown, "No route to host "++Host++" over ipv"++integer_to_list(IpVersion)};
		econnrefused ->
			{false, "Failed to establish a TCP connection to host "++Host++" on port "++integer_to_list(Port)++" over ipv"++integer_to_list(IpVersion)};
		closed ->
			{false, "TCP connection was closed connecting to host "++Host++" on port "++integer_to_list(Port)++" over ipv"++integer_to_list(IpVersion)};
		etimedout ->
			{unknown, "TCP connection timed out whilst connecting to "++Host++" on port "++integer_to_list(Port)++" over ipv"++integer_to_list(IpVersion)};
		timeout ->
			{unknown, "HTTP connection timed out whilst connecting to "++Host++" on port "++integer_to_list(Port)++" over ipv"++integer_to_list(IpVersion)};
		_ ->
			io:format("Unknown connection error handled: ~p (ipv~p connection)~n",[ErrorType, IpVersion]),
			{false, lists:flatten(io_lib:format("An unknown connection error occured: ~p (ipv~p connection)",[ErrorType, IpVersion]))}
	end.

fetchInfo(Host) ->
	InfoURL = "https://" ++ Host ++ "/_info",
	TechDetail = list_to_binary("Makes HTTP request to "++InfoURL++""),
	case httpc:request(get, {InfoURL, [{"User-Agent", "lucos_monitoring"}]}, [{timeout, timer:seconds(1)},{ssl,[{verify, verify_peer},{cacerts, public_key:cacerts_get()}]}], [{socket_opts, [{ipfamily, inet6fb4}]}]) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
			try
				{System, Checks, Metrics, CircleCISlug} = parseInfo(Body),
				InfoCheck = #{
					<<"ok">> => true,
					<<"techDetail">> => TechDetail
				},
				{InfoCheck, System, Checks, Metrics, CircleCISlug}
			catch
				Exception:Reason ->
					ErroringInfoCheck = #{
						<<"ok">> => false,
						<<"techDetail">> => TechDetail,
						<<"debug">> => list_to_binary(lists:flatten(io_lib:format("Couldn't parse response from endpoint.~nError: ~p ~p",[Exception, Reason])))
					},
					{ErroringInfoCheck, unknown, #{}, #{}, null}
			end;
		{ok, {{_Version, StatusCode, ReasonPhrase}, _Headers, _Body}} ->
			InfoCheck = #{
				<<"ok">> => false,
				<<"techDetail">> => TechDetail,
				<<"debug">> => list_to_binary("Received HTTP response with status "++integer_to_list(StatusCode)++" "++ReasonPhrase)
			},
			{InfoCheck, unknown, #{}, #{}, null};
		{error, Error} ->
			{Ok, Debug} = parseError(Error),
			InfoCheck = #{
				<<"ok">> => Ok,
				<<"techDetail">> => TechDetail,
				<<"debug">> => list_to_binary(Debug)
			},
			{InfoCheck, unknown, #{}, #{}, null}
	end.

checkCI(CircleCISlug) ->
	case CircleCISlug of
		null -> #{};
		_ ->
			ApiUrl = "https://circleci.com/api/v1.1/project/"++binary_to_list(CircleCISlug)++"/tree/main?circle-token="++os:getenv("CIRCLECI_API_TOKEN", "")++"&limit=1&filter=complete",
			case httpc:request(get, {ApiUrl, [{"Accept","application/json"}]}, [{timeout, timer:seconds(1)},{ssl,[{verify, verify_peer},{cacerts, public_key:cacerts_get()}]}], []) of
				{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
					Response = jiffy:decode(Body, [return_maps]),
					case Response of
						[] ->
							#{<<"circleci">> => #{
								<<"ok">> => true,
								<<"techDetail">> => <<"Checks status of most recent circleCI build">>,
								<<"debug">> => <<"No recent builds found">>
							}};
						_ ->
						Build = lists:nth(1, Response),
						Status = maps:get(<<"status">>, Build, <<"unknown">>),
						BuildUrl = maps:get(<<"build_url">>, Build, <<"">>),
						case Status of
							<<"success">> ->
								#{<<"circleci">> => #{
									<<"ok">> => true,
									<<"techDetail">> => <<"Checks status of most recent circleCI build">>,
									<<"link">> => BuildUrl
								}};
							<<"running">> ->
								#{<<"circleci">> => #{
									<<"ok">> => true,
									<<"techDetail">> => <<"Checks status of most recent circleCI build">>,
									<<"link">> => BuildUrl
								}};
							<<"pending">> ->
								% Treated as ok since we would just wait for it to finish anyway - no action to take
								#{<<"circleci">> => #{
									<<"ok">> => true,
									<<"techDetail">> => <<"Checks status of most recent circleCI build">>,
									<<"debug">> => <<"Most recent build is pending">>,
									<<"link">> => BuildUrl
								}};
							<<"queued">> ->
								% Treated as ok since we would just wait for it to finish anyway - no action to take
								#{<<"circleci">> => #{
									<<"ok">> => true,
									<<"techDetail">> => <<"Checks status of most recent circleCI build">>,
									<<"debug">> => <<"Most recent build is queued">>,
									<<"link">> => BuildUrl
								}};
							null ->
								#{<<"circleci">> => #{
									<<"ok">> => unknown,
									<<"techDetail">> => <<"Checks status of most recent circleCI build">>,
									<<"debug">> => <<"No status returned for most recent build">>,
									<<"link">> => BuildUrl
								}};
							_ ->
								#{<<"circleci">> => #{
									<<"ok">> => false,
									<<"techDetail">> => <<"Checks status of most recent circleCI build">>,
									<<"debug">> => <<"Most recent build's status was \"", Status/binary, "\"">>,
									<<"link">> => BuildUrl
								}}
						end
					end;
				{ok, {{_Version, StatusCode, ReasonPhrase}, _Headers, _Body}} when StatusCode >= 500 ->
					#{<<"circleci">> => #{
						<<"ok">> => unknown,
						<<"techDetail">> => <<"Checks status of most recent circleCI build">>,
						<<"debug">> => list_to_binary("Received HTTP response with status "++integer_to_list(StatusCode)++" "++ReasonPhrase)
					}};
				{ok, {{_Version, StatusCode, ReasonPhrase}, _Headers, _Body}} ->
					#{<<"circleci">> => #{
						<<"ok">> => false,
						<<"techDetail">> => <<"Checks status of most recent circleCI build">>,
						<<"debug">> => list_to_binary("Received HTTP response with status "++integer_to_list(StatusCode)++" "++ReasonPhrase)
					}};
				{error, Error} ->
					{Ok, Debug} = parseError(Error),
					#{<<"circleci">> => #{
						<<"ok">> => Ok,
						<<"techDetail">> => <<"Checks status of most recent circleCI build">>,
						<<"debug">> => list_to_binary(Debug)
					}}
			end
	end.

-ifdef(TEST).
	parseInfo_test() ->
		% Basic: system field only, no checks/metrics/ci
		?assertEqual({"lucos_test",#{},#{},null}, parseInfo("{\"system\":\"lucos_test\"}")),
		% Checks field is extracted when present
		?assertEqual({"lucos_test",#{<<"db">> => #{<<"ok">> => true}},#{},null}, parseInfo("{\"system\":\"lucos_test\",\"checks\":{\"db\":{\"ok\":true}}}")),
		% ci.circle field is extracted correctly
		?assertEqual({"lucos_test",#{},#{},<<"gh/lucas42/lucos_test">>}, parseInfo("{\"system\":\"lucos_test\",\"ci\":{\"circle\":\"gh/lucas42/lucos_test\"}}")),
		% ci present but no circle key returns null
		?assertEqual({"lucos_test",#{},#{},null}, parseInfo("{\"system\":\"lucos_test\",\"ci\":{}}")),
		% Invalid JSON raises an exception
		?assertException(error, {2,invalid_json}, parseInfo("{{{{}}}")),
		% Empty body raises an exception
		?assertException(error, _, parseInfo("")),
		% Missing required 'system' field raises an exception
		?assertException(error, {badkey,<<"system">>}, parseInfo("{}")),
		% system as integer raises an exception (binary_to_list fails on non-binary)
		?assertException(error, badarg, parseInfo("{\"system\":42}")),
		% system as JSON null raises an exception (null becomes atom null, not a binary)
		?assertException(error, badarg, parseInfo("{\"system\":null}")),
		% ci as a non-map raises an exception (maps:get fails on non-map)
		?assertException(error, {badmap,<<"not_a_map">>}, parseInfo("{\"system\":\"lucos_test\",\"ci\":\"not_a_map\"}")).

	% BUG: checks as a non-map string passes through parseInfo without error,
	% causing runChecks to crash with {badmap,_} when it calls maps:merge.
	% parseInfo should validate checks is a map so fetchInfo's try-catch handles it.
	parseInfo_checks_nonmap_bug_test() ->
		?assertException(error, _, parseInfo("{\"system\":\"lucos_test\",\"checks\":\"not_a_map\"}")).

	% BUG: checks as JSON null passes through parseInfo without error,
	% causing runChecks to crash with {badmap,null} when it calls maps:merge.
	parseInfo_checks_null_bug_test() ->
		?assertException(error, _, parseInfo("{\"system\":\"lucos_test\",\"checks\":null}")).

	% BUG: metrics as a non-map value passes through parseInfo without error,
	% causing runChecks to crash when the state server tries to handle it.
	parseInfo_metrics_nonmap_bug_test() ->
		?assertException(error, _, parseInfo("{\"system\":\"lucos_test\",\"metrics\":42}")).

	parseError_test() ->
		% IPv4+IPv6: IPv4 HTTP timeout (unknown) + IPv6 connection refused (false) → false
		?assertEqual({false, "HTTP connection timed out whilst connecting to example.l42.eu on port 443 over ipv4; Failed to establish a TCP connection to host example.l42.eu on port 443 over ipv6"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet6,[inet6],econnrefused},{inet,[inet],timeout}]})),
		% IPv4 only: TCP connection closed
		?assertEqual({false, "TCP connection was closed connecting to host example.l42.eu on port 1234 over ipv4"}, parseError({failed_connect,[{to_address,{"example.l42.eu",1234}}, {inet,[inet],closed}]})),
		% IPv6 only: DNS failure
		?assertEqual({unknown, "DNS failure when trying to resolve ipv6 address for example.l42.eu"}, parseError({failed_connect,[{to_address,{"example.l42.eu",1234}}, {inet6,[inet6],nxdomain}]})),
		% IPv6 only: unknown connection error type
		?assertEqual({false, "An unknown connection error occured: not_a_real_error (ipv6 connection)"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet6,[inet6],not_a_real_error}]})),
		% Completely unknown top-level error term
		?assertEqual({false, "An unknown error occured: {not_a_real_error}"}, parseError({not_a_real_error})).

	parseError_topLevel_test() ->
		% The remote end closed the socket unexpectedly
		?assertEqual({false, "Socket closed remotely"}, parseError(socket_closed_remotely)),
		% The overall HTTP request timed out at the httpc level (distinct from connection-level timeout)
		?assertEqual({unknown, "HTTP Request timed out"}, parseError(timeout)).

	parseError_ipv4_test() ->
		% DNS lookup failure over IPv4
		?assertEqual({unknown, "DNS failure when trying to resolve ipv4 address for example.l42.eu"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet,[inet],nxdomain}]})),
		% No route to host over IPv4
		?assertEqual({unknown, "No route to host example.l42.eu over ipv4"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet,[inet],ehostunreach}]})),
		% TCP connection refused over IPv4
		?assertEqual({false, "Failed to establish a TCP connection to host example.l42.eu on port 443 over ipv4"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet,[inet],econnrefused}]})),
		% TCP connection timed out over IPv4
		?assertEqual({unknown, "TCP connection timed out whilst connecting to example.l42.eu on port 443 over ipv4"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet,[inet],etimedout}]})),
		% HTTP-level connection timeout over IPv4
		?assertEqual({unknown, "HTTP connection timed out whilst connecting to example.l42.eu on port 443 over ipv4"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet,[inet],timeout}]})),
		% Unknown connection error type over IPv4
		?assertEqual({false, "An unknown connection error occured: not_a_real_error (ipv4 connection)"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet,[inet],not_a_real_error}]})),
		% TLS handshake/certificate errors fall through to the unknown connection error handler
		?assertEqual({false, "An unknown connection error occured: {tls_alert,certificate_expired} (ipv4 connection)"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet,[inet],{tls_alert,certificate_expired}}]})).

	parseError_ipv6_test() ->
		% No route to host over IPv6
		?assertEqual({unknown, "No route to host example.l42.eu over ipv6"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet6,[inet6],ehostunreach}]})),
		% TCP connection refused over IPv6
		?assertEqual({false, "Failed to establish a TCP connection to host example.l42.eu on port 443 over ipv6"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet6,[inet6],econnrefused}]})),
		% TCP connection timed out over IPv6
		?assertEqual({unknown, "TCP connection timed out whilst connecting to example.l42.eu on port 443 over ipv6"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet6,[inet6],etimedout}]})),
		% HTTP-level connection timeout over IPv6
		?assertEqual({unknown, "HTTP connection timed out whilst connecting to example.l42.eu on port 443 over ipv6"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet6,[inet6],timeout}]})),
		% TCP connection was closed over IPv6
		?assertEqual({false, "TCP connection was closed connecting to host example.l42.eu on port 443 over ipv6"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet6,[inet6],closed}]})).

	parseError_combined_test() ->
		% Both IPv4 and IPv6 fail with DNS errors (both unknown → overall unknown)
		?assertEqual({unknown, "DNS failure when trying to resolve ipv4 address for example.l42.eu; DNS failure when trying to resolve ipv6 address for example.l42.eu"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet6,[inet6],nxdomain},{inet,[inet],nxdomain}]})),
		% Both IPv4 and IPv6 connection refused (both false → overall false)
		?assertEqual({false, "Failed to establish a TCP connection to host example.l42.eu on port 443 over ipv4; Failed to establish a TCP connection to host example.l42.eu on port 443 over ipv6"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet6,[inet6],econnrefused},{inet,[inet],econnrefused}]})),
		% IPv4 connection refused (false) + IPv6 DNS failure (unknown) → overall false
		?assertEqual({false, "Failed to establish a TCP connection to host example.l42.eu on port 443 over ipv4; DNS failure when trying to resolve ipv6 address for example.l42.eu"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet6,[inet6],nxdomain},{inet,[inet],econnrefused}]})).
-endif.