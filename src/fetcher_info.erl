-module(fetcher_info).
-export([start/1, tryRunChecks/3]).
-include_lib("eunit/include/eunit.hrl").

start(StatePid) ->
	{ok, _} = application:ensure_all_started([ssl, inets]),
	{ok, SystemsBody} = file:read_file("./info-systems-list"),
	{ok, HostsBody} = file:read_file("./info-hosts-list"),
	Systems = parseInfoSystems(binary_to_list(SystemsBody)) ++
	          parseInfoSystems(binary_to_list(HostsBody)),
	lists:foreach(fun({Id, Host}) ->
		spawn(?MODULE, tryRunChecks, [StatePid, Id, Host])
	end, Systems).

parseInfoSystems(Body) ->
	Entries = jiffy:decode(Body, [return_maps]),
	[{binary_to_list(maps:get(<<"id">>, Entry)),
	  binary_to_list(maps:get(<<"domain">>, Entry))} || Entry <- Entries].

tryRunChecks(StatePid, Id, Host) ->
	try runChecks(StatePid, Id, Host) of
		_ -> ok
	catch
		ExceptionClass:Term:StackTrace ->
			io:format("ExceptionClass: ~p Term: ~p StackTrace: ~p~n", [ExceptionClass, Term, StackTrace])
	end,
	timer:sleep(timer:seconds(60)),
	tryRunChecks(StatePid, Id, Host).

runChecks(StatePid, Id, Host) ->
	{TLSCheck} = checkTlsExpiry(Host),
	{InfoCheck, _System, Checks, Metrics} = fetchInfo(Host),
	AllChecks = maps:merge(#{
		<<"fetch-info">> => InfoCheck,
		<<"tls-certificate">> => TLSCheck
	}, Checks),
	ok = gen_server:cast(StatePid, {updateSystem, Host, Id, info, AllChecks, Metrics}).

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
	RawChecks = maps:get(<<"checks">>, Info, #{}),
	RawMetrics = maps:get(<<"metrics">>, Info, #{}),
	true = is_map(RawChecks),
	true = is_map(RawMetrics),
	Checks = validateChecks(System, RawChecks),
	Metrics = validateMetrics(System, RawMetrics),
	{System, Checks, Metrics}.

% Checks expect an "ok" field — replace non-map entries with a warning check
% so the problem is visible in the UI.
validateChecks(System, Entries) ->
	maps:map(fun (Key, Value) ->
		case is_map(Value) of
			true -> Value;
			false ->
				io:format("WARNING: ~s has non-map check '~s': ~p~n", [System, Key, Value]),
				#{
					<<"ok">> => false,
					<<"techDetail">> => list_to_binary(io_lib:format("Invalid check format in /_info (expected a map, got ~p)", [Value]))
				}
		end
	end, Entries).

% Metrics assume numeric values — drop non-map entries entirely since they
% can't be meaningfully rendered, and log a warning.
validateMetrics(System, Entries) ->
	maps:filter(fun (Key, Value) ->
		case is_map(Value) of
			true -> true;
			false ->
				io:format("WARNING: ~s has non-map metric '~s': ~p (dropping)~n", [System, Key, Value]),
				false
		end
	end, Entries).

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
				{System, Checks, Metrics} = parseInfo(Body),
				InfoCheck = #{
					<<"ok">> => true,
					<<"techDetail">> => TechDetail
				},
				{InfoCheck, System, Checks, Metrics}
			catch
				Exception:Reason ->
					ErroringInfoCheck = #{
						<<"ok">> => false,
						<<"techDetail">> => TechDetail,
						<<"debug">> => list_to_binary(lists:flatten(io_lib:format("Couldn't parse response from endpoint.~nError: ~p ~p",[Exception, Reason])))
					},
					{ErroringInfoCheck, unknown, #{}, #{}}
			end;
		{ok, {{_Version, StatusCode, ReasonPhrase}, _Headers, _Body}} ->
			InfoCheck = #{
				<<"ok">> => false,
				<<"techDetail">> => TechDetail,
				<<"debug">> => list_to_binary("Received HTTP response with status "++integer_to_list(StatusCode)++" "++ReasonPhrase)
			},
			{InfoCheck, unknown, #{}, #{}};
		{error, Error} ->
			{Ok, Debug} = parseError(Error),
			InfoCheck = #{
				<<"ok">> => Ok,
				<<"techDetail">> => TechDetail,
				<<"debug">> => list_to_binary(Debug)
			},
			{InfoCheck, unknown, #{}, #{}}
	end.

-ifdef(TEST).
	parseInfoSystems_test() ->
		% Both id and domain are always present — id becomes the system identifier, domain the host.
		Body = "[{\"id\":\"lucos_foo\",\"domain\":\"foo.l42.eu\"},{\"id\":\"lucos_bar\",\"domain\":\"bar.l42.eu\"}]",
		?assertEqual([{"lucos_foo", "foo.l42.eu"}, {"lucos_bar", "bar.l42.eu"}], parseInfoSystems(Body)).

	parseInfoSystems_empty_test() ->
		?assertEqual([], parseInfoSystems("[]")).

	parseInfo_test() ->
		% Basic: system field only, no checks/metrics
		?assertEqual({"lucos_test",#{},#{}}, parseInfo("{\"system\":\"lucos_test\"}")),
		% Checks field is extracted when present
		?assertEqual({"lucos_test",#{<<"db">> => #{<<"ok">> => true}},#{}}, parseInfo("{\"system\":\"lucos_test\",\"checks\":{\"db\":{\"ok\":true}}}")),
		% ci field is ignored (CI is now sourced from configy, not /_info)
		?assertEqual({"lucos_test",#{},#{}}, parseInfo("{\"system\":\"lucos_test\",\"ci\":{\"circle\":\"gh/lucas42/lucos_test\"}}")),
		% Invalid JSON raises an exception
		?assertException(error, {2,invalid_json}, parseInfo("{{{{}}}")),
		% Empty body raises an exception
		?assertException(error, _, parseInfo("")),
		% Missing required 'system' field raises an exception
		?assertException(error, {badkey,<<"system">>}, parseInfo("{}")),
		% system as integer raises an exception (binary_to_list fails on non-binary)
		?assertException(error, badarg, parseInfo("{\"system\":42}")),
		% system as JSON null raises an exception (null becomes atom null, not a binary)
		?assertException(error, badarg, parseInfo("{\"system\":null}")).

	% checks as a non-map string is rejected by the is_map guard
	parseInfo_checks_nonmap_test() ->
		?assertException(error, _, parseInfo("{\"system\":\"lucos_test\",\"checks\":\"not_a_map\"}")).

	% checks as JSON null is rejected by the is_map guard
	parseInfo_checks_null_test() ->
		?assertException(error, _, parseInfo("{\"system\":\"lucos_test\",\"checks\":null}")).

	% metrics as a non-map value is rejected by the is_map guard
	parseInfo_metrics_nonmap_test() ->
		?assertException(error, _, parseInfo("{\"system\":\"lucos_test\",\"metrics\":42}")).

	% Individual check entry that is not a map gets replaced with a warning check
	parseInfo_individual_check_nonmap_test() ->
		{System, Checks, _} = parseInfo("{\"system\":\"lucos_test\",\"checks\":{\"good\":{\"ok\":true},\"bad\":\"not_a_map\"}}"),
		?assertEqual("lucos_test", System),
		% Good check is preserved as-is
		?assertEqual(#{<<"ok">> => true}, maps:get(<<"good">>, Checks)),
		% Bad check is replaced with a failing check that explains the problem
		BadCheck = maps:get(<<"bad">>, Checks),
		?assert(is_map(BadCheck)),
		?assertEqual(false, maps:get(<<"ok">>, BadCheck)),
		?assert(is_binary(maps:get(<<"techDetail">>, BadCheck))).

	% Check entry as null gets replaced with a warning check
	parseInfo_individual_check_null_test() ->
		{_, Checks, _} = parseInfo("{\"system\":\"lucos_test\",\"checks\":{\"bad\":null}}"),
		BadCheck = maps:get(<<"bad">>, Checks),
		?assert(is_map(BadCheck)),
		?assertEqual(false, maps:get(<<"ok">>, BadCheck)).

	% Individual metric entry that is not a map is dropped (not rendered)
	parseInfo_individual_metric_nonmap_test() ->
		{System, _, Metrics} = parseInfo("{\"system\":\"lucos_test\",\"metrics\":{\"good\":{\"value\":42,\"techDetail\":\"count\"},\"bad\":\"just_a_string\"}}"),
		?assertEqual("lucos_test", System),
		% Good metric is preserved as-is
		?assertEqual(#{<<"value">> => 42, <<"techDetail">> => <<"count">>}, maps:get(<<"good">>, Metrics)),
		% Bad metric is dropped entirely
		?assertNot(maps:is_key(<<"bad">>, Metrics)).

	% Metric entry as a number (not a map) is dropped
	parseInfo_individual_metric_number_test() ->
		{_, _, Metrics} = parseInfo("{\"system\":\"lucos_test\",\"metrics\":{\"bad\":123}}"),
		?assertNot(maps:is_key(<<"bad">>, Metrics)).

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
