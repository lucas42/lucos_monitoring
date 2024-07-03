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
	Command = "echo | openssl s_client -connect "++Host++":443 -servername "++Host++" 2>/dev/null | openssl x509 -noout -enddate | sed 's/.*=//' | date +'%s' -f -",
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
	Checks = maps:get(<<"checks">>, Info, #{}),
	Metrics = maps:get(<<"metrics">>, Info, #{}),
	CircleCISlug = maps:get(<<"circle">>, maps:get(<<"ci">>, Info, #{}), null),
	{System, Checks, Metrics, CircleCISlug}.


parseError(Error) ->
	case Error of
		{failed_connect, [{to_address, {Host, _Port}}, {_ipFamily,[_ipFamily],nxdomain}]} ->
			{unknown, "DNS failure when trying to resolve "++Host};
		{failed_connect, [{to_address, {Host, _Port}}, {_ipFamily,[_ipFamily],nxdomain}, {_ipFamily2,[_ipFamily2],timeout}]} ->
			{unknown, "DNS lookup timed out for host "++Host};
		{failed_connect, [{to_address, {Host, _Port}}, {_ipFamily,[_ipFamily],timeout}, {_ipFamily2,[_ipFamily2],nxdomain}]} ->
			{unknown, "DNS lookup timed out for host "++Host};
		{failed_connect, [{to_address, {Host, Port}}, {_ipFamily,[_ipFamily],econnrefused}]} ->
			{false, "Failed to establish a TCP connection to host "++Host++" on port "++integer_to_list(Port)};
		{failed_connect, [{to_address, {Host, Port}}, {_ipFamily,[_ipFamily],closed}]} ->
			{false, "TCP connection was closed connecting to host "++Host++" on port "++integer_to_list(Port)};
		{failed_connect, [{to_address, {Host, Port}}, {_ipFamily,[_ipFamily],etimedout}]} ->
			{unknown, "TCP connection timed out whilst connecting to "++Host++" on port "++integer_to_list(Port)};
		{failed_connect, [{to_address, {Host, Port}}, {_ipFamily,[_ipFamily],timeout}]} ->
			{unknown, "HTTP connection timed out whilst connecting to "++Host++" on port "++integer_to_list(Port)};
		socket_closed_remotely ->
			{false, "Socket closed remotely"};
		timeout ->
			{unknown, "HTTP Request timed out"};
		_ ->
			io:format("Unknown parse error handled: ~p~n",[Error]),
			{false, "An unknown error occured: "++lists:flatten(io_lib:format("~p",[Error]))}
	end.

fetchInfo(Host) ->
	InfoURL = "https://" ++ Host ++ "/_info",
	TechDetail = list_to_binary("Makes HTTP request to "++InfoURL++""),
	case httpc:request(get, {InfoURL, [{"User-Agent", "lucos_monitoring"}]}, [{timeout, timer:seconds(1)},{ssl,[{verify, verify_peer},{cacerts, public_key:cacerts_get()}]}], [{socket_opts, [{ipfamily, inet6fb4}]}]) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
			{System, Checks, Metrics, CircleCISlug} = parseInfo(Body),
			InfoCheck = #{
				<<"ok">> => true,
				<<"techDetail">> => TechDetail
			},
			{InfoCheck, System, Checks, Metrics, CircleCISlug};
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
						Outcome = binary_to_list(maps:get(<<"outcome">>, Build, <<"unknown">>)),
						BuildUrl = maps:get(<<"build_url">>, Build, <<"">>),
						case Outcome of
							"success" ->
								#{<<"circleci">> => #{
									<<"ok">> => true,
									<<"techDetail">> => <<"Checks status of most recent circleCI build">>,
									<<"link">> => BuildUrl
								}};
							_ ->
								#{<<"circleci">> => #{
									<<"ok">> => false,
									<<"techDetail">> => <<"Checks status of most recent circleCI build">>,
									<<"debug">> => list_to_binary("Most recent build's status was \""++Outcome++"\""),
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
		?assertEqual({"lucos_test",#{},#{},null}, parseInfo("{\"system\":\"lucos_test\"}")),
		?assertException(error, {2,invalid_json}, parseInfo("{{{{}}}")),
		?assertException(error, {badkey,<<"system">>}, parseInfo("{}")).
-endif.