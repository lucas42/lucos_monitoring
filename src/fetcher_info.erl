-module(fetcher_info).
-export([start/1, tryRunChecks/4]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

start(StatePid) ->
	{ok, _} = application:ensure_all_started([ssl, inets]),
	{ok, SystemsBody} = file:read_file("./config/info-systems-list.json"),
	{ok, HostsBody} = file:read_file("./config/info-hosts-list.json"),
	% info-systems-list.json entries are systems; info-hosts-list.json entries are hosts.
	% DefaultType is used when the JSON entry has no "type" field.
	Systems = parseInfoSystems(binary_to_list(SystemsBody), system) ++
	          parseInfoSystems(binary_to_list(HostsBody), host),
	lists:foreach(fun({Id, Type, Host}) ->
		spawn(?MODULE, tryRunChecks, [StatePid, Id, Type, Host])
	end, Systems).

% Parses a JSON array of system/host entries. DefaultType is the atom to use
% when an entry has no "type" field. Returns a list of {Id, Type, Host} triples.
parseInfoSystems(Body, DefaultType) ->
	Entries = jiffy:decode(Body, [return_maps]),
	[{binary_to_list(maps:get(<<"id">>, Entry)),
	  binary_to_atom(maps:get(<<"type">>, Entry, atom_to_binary(DefaultType)), utf8),
	  binary_to_list(maps:get(<<"domain">>, Entry))} || Entry <- Entries].

tryRunChecks(StatePid, Id, Type, Host) ->
	try runChecks(StatePid, Id, Type, Host) of
		_ -> ok
	catch
		ExceptionClass:Term:StackTrace ->
			logger:error("ExceptionClass: ~p Term: ~p StackTrace: ~p", [ExceptionClass, Term, StackTrace])
	end,
	timer:sleep(timer:seconds(60)),
	tryRunChecks(StatePid, Id, Type, Host).

runChecks(StatePid, Id, Type, Host) ->
	StartTime = erlang:monotonic_time(millisecond),
	{TLSCheck} = checkTlsExpiry(Host),
	{InfoCheck, _System, Checks, Metrics} = fetchInfo(Host),
	% Option B: require 2 consecutive failures before alerting on the fetcher's own synthetic
	% checks. These are the checks most likely to see transient blips during deploys or restarts,
	% so one extra poll of buffer meaningfully reduces false-positive alert volume.
	AllChecks = maps:merge(#{
		<<"fetch-info">> => maps:put(<<"failThreshold">>, 2, InfoCheck),
		<<"tls-certificate">> => maps:put(<<"failThreshold">>, 2, TLSCheck)
	}, Checks),
	ok = gen_server:cast(StatePid, {updateSystem, Host, Id, Type, info, AllChecks, Metrics}),
	DurationMs = erlang:monotonic_time(millisecond) - StartTime,
	InfoOk = maps:get(<<"ok">>, InfoCheck, unknown),
	TLSOk = maps:get(<<"ok">>, TLSCheck, unknown),
	% IsOk=true only when fetch-info succeeded; unknown/false counts as not-ok for burst detection.
	IsOk = (InfoOk =:= true),
	logger:info("Checked ~p: duration_ms=~p fetch_info=~p tls=~p", [Id, DurationMs, InfoOk, TLSOk]),
	ok = gen_server:cast(StatePid, {poll_timing, Id, DurationMs, IsOk}).

checkTlsExpiry(Host) ->
	TechDetail = <<"Checks whether the TLS Certificate is valid and not about to expire">>,
	SslOpts = [
		{verify, verify_peer},
		{cacerts, public_key:cacerts_get()},
		{server_name_indication, Host}
	],
	Unavailable = #{
		<<"ok">> => unknown,
		<<"techDetail">> => TechDetail,
		<<"debug">> => <<"Can't get expiry time for TLS Cert">>
	},
	case ssl:connect(Host, 443, SslOpts, timer:seconds(1)) of
		{ok, Socket} ->
			PeercertResult = ssl:peercert(Socket),
			ssl:close(Socket),
			case PeercertResult of
				{ok, DerCert} ->
					OtpCert = public_key:pkix_decode_cert(DerCert, otp),
					TBSCert = OtpCert#'OTPCertificate'.tbsCertificate,
					Validity = TBSCert#'OTPTBSCertificate'.validity,
					NotAfter = Validity#'Validity'.notAfter,
					Expiry = tlsNotAfterToUnix(NotAfter),
					Diff = Expiry - erlang:system_time(second),
					if
						% start failing when there's fewer than 20 days until expiry
						Diff < 1728000 ->
							Debug = list_to_binary("TLS Certificate due to expire in "++integer_to_list(Diff)++" seconds"),
							{#{<<"ok">> => false, <<"techDetail">> => TechDetail, <<"debug">> => Debug}};
						true ->
							{#{<<"ok">> => true, <<"techDetail">> => TechDetail}}
					end;
				{error, _} ->
					{Unavailable}
			end;
		{error, _} ->
			{Unavailable}
	end.

% Converts an ASN.1 certificate validity time to a Unix timestamp (seconds since epoch).
% utcTime format: "YYMMDDHHMMSSZ" — years 50-99 map to 1950-1999, 00-49 to 2000-2049.
% generalTime format: "YYYYMMDDHHMMSSZ".
tlsNotAfterToUnix({utcTime, [Y1,Y2,M1,M2,D1,D2,H1,H2,Mi1,Mi2,S1,S2|_]}) ->
	YY = list_to_integer([Y1,Y2]),
	Year = if YY >= 50 -> 1900 + YY; true -> 2000 + YY end,
	Datetime = {{Year, list_to_integer([M1,M2]), list_to_integer([D1,D2])},
	            {list_to_integer([H1,H2]), list_to_integer([Mi1,Mi2]), list_to_integer([S1,S2])}},
	datetimeToUnixSeconds(Datetime);
tlsNotAfterToUnix({generalTime, [Y1,Y2,Y3,Y4,M1,M2,D1,D2,H1,H2,Mi1,Mi2,S1,S2|_]}) ->
	Year = list_to_integer([Y1,Y2,Y3,Y4]),
	Datetime = {{Year, list_to_integer([M1,M2]), list_to_integer([D1,D2])},
	            {list_to_integer([H1,H2]), list_to_integer([Mi1,Mi2]), list_to_integer([S1,S2])}},
	datetimeToUnixSeconds(Datetime).

datetimeToUnixSeconds(DateTime) ->
	UnixEpoch = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
	calendar:datetime_to_gregorian_seconds(DateTime) - UnixEpoch.

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
				logger:warning("~s has non-map check '~s': ~p", [System, Key, Value]),
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
				logger:warning("~s has non-map metric '~s': ~p (dropping)", [System, Key, Value]),
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
			% Transient: remote closed the connection (e.g. mid-stream during a container restart).
			% Treated as unknown rather than false so flap-suppression can absorb brief blips.
			{unknown, "Socket closed remotely"};
		timeout ->
			{unknown, "HTTP Request timed out"};
		_ ->
			logger:warning("Unknown error handled: ~p", [Error]),
			{false, "An unknown error occured: "++lists:flatten(io_lib:format("~p",[Error]))}
	end.

parseConnectionError(Host, Port, IpVersion, ErrorType) ->
	case ErrorType of
		nxdomain ->
			{unknown, "DNS failure when trying to resolve ipv"++integer_to_list(IpVersion)++" address for "++Host};
		ehostunreach ->
			{unknown, "No route to host "++Host++" over ipv"++integer_to_list(IpVersion)};
		econnrefused ->
			% Transient: server briefly not listening (e.g. nginx reload, container restart).
			% Treated as unknown rather than false so flap-suppression can absorb brief blips.
			{unknown, "Failed to establish a TCP connection to host "++Host++" on port "++integer_to_list(Port)++" over ipv"++integer_to_list(IpVersion)};
		closed ->
			% Transient: connection dropped mid-stream during a restart.
			% Treated as unknown rather than false so flap-suppression can absorb brief blips.
			{unknown, "TCP connection was closed connecting to host "++Host++" on port "++integer_to_list(Port)++" over ipv"++integer_to_list(IpVersion)};
		etimedout ->
			{unknown, "TCP connection timed out whilst connecting to "++Host++" on port "++integer_to_list(Port)++" over ipv"++integer_to_list(IpVersion)};
		timeout ->
			{unknown, "HTTP connection timed out whilst connecting to "++Host++" on port "++integer_to_list(Port)++" over ipv"++integer_to_list(IpVersion)};
		_ ->
			logger:warning("Unknown connection error handled: ~p (ipv~p connection)", [ErrorType, IpVersion]),
			{false, lists:flatten(io_lib:format("An unknown connection error occured: ~p (ipv~p connection)",[ErrorType, IpVersion]))}
	end.

fetchInfo(Host) ->
	InfoURL = "https://" ++ Host ++ "/_info",
	TechDetail = list_to_binary("Makes HTTP request to "++InfoURL++""),
	case httpc:request(get, {InfoURL, [{"User-Agent", os:getenv("SYSTEM", "")}]}, [{timeout, timer:seconds(1)},{ssl,[{verify, verify_peer},{cacerts, public_key:cacerts_get()}]}], [{socket_opts, [{ipfamily, inet6fb4}]}]) of
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
		% DefaultType is used when no "type" field is present in the entry.
		Body = "[{\"id\":\"lucos_foo\",\"domain\":\"foo.l42.eu\"},{\"id\":\"lucos_bar\",\"domain\":\"bar.l42.eu\"}]",
		?assertEqual([{"lucos_foo", system, "foo.l42.eu"}, {"lucos_bar", system, "bar.l42.eu"}], parseInfoSystems(Body, system)).

	parseInfoSystems_default_type_host_test() ->
		% DefaultType host: entries from info-hosts-list.json use the host atom.
		Body = "[{\"id\":\"lucos_host\",\"domain\":\"host.l42.eu\"}]",
		?assertEqual([{"lucos_host", host, "host.l42.eu"}], parseInfoSystems(Body, host)).

	parseInfoSystems_explicit_type_overrides_default_test() ->
		% When an entry has an explicit "type" field, it overrides DefaultType.
		Body = "[{\"id\":\"lucos_comp\",\"domain\":\"comp.l42.eu\",\"type\":\"component\"}]",
		?assertEqual([{"lucos_comp", component, "comp.l42.eu"}], parseInfoSystems(Body, system)).

	parseInfoSystems_empty_test() ->
		?assertEqual([], parseInfoSystems("[]", system)).

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
		% IPv4+IPv6: IPv4 HTTP timeout (unknown) + IPv6 connection refused (now unknown) → unknown
		?assertEqual({unknown, "HTTP connection timed out whilst connecting to example.l42.eu on port 443 over ipv4; Failed to establish a TCP connection to host example.l42.eu on port 443 over ipv6"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet6,[inet6],econnrefused},{inet,[inet],timeout}]})),
		% IPv4 only: TCP connection closed — reclassified as unknown (transient)
		?assertEqual({unknown, "TCP connection was closed connecting to host example.l42.eu on port 1234 over ipv4"}, parseError({failed_connect,[{to_address,{"example.l42.eu",1234}}, {inet,[inet],closed}]})),
		% IPv6 only: DNS failure
		?assertEqual({unknown, "DNS failure when trying to resolve ipv6 address for example.l42.eu"}, parseError({failed_connect,[{to_address,{"example.l42.eu",1234}}, {inet6,[inet6],nxdomain}]})),
		% IPv6 only: unknown connection error type
		?assertEqual({false, "An unknown connection error occured: not_a_real_error (ipv6 connection)"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet6,[inet6],not_a_real_error}]})),
		% Completely unknown top-level error term
		?assertEqual({false, "An unknown error occured: {not_a_real_error}"}, parseError({not_a_real_error})).

	parseError_topLevel_test() ->
		% The remote end closed the socket unexpectedly — reclassified as unknown (transient)
		?assertEqual({unknown, "Socket closed remotely"}, parseError(socket_closed_remotely)),
		% The overall HTTP request timed out at the httpc level (distinct from connection-level timeout)
		?assertEqual({unknown, "HTTP Request timed out"}, parseError(timeout)).

	parseError_ipv4_test() ->
		% DNS lookup failure over IPv4
		?assertEqual({unknown, "DNS failure when trying to resolve ipv4 address for example.l42.eu"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet,[inet],nxdomain}]})),
		% No route to host over IPv4
		?assertEqual({unknown, "No route to host example.l42.eu over ipv4"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet,[inet],ehostunreach}]})),
		% TCP connection refused over IPv4 — reclassified as unknown (transient: server briefly not listening)
		?assertEqual({unknown, "Failed to establish a TCP connection to host example.l42.eu on port 443 over ipv4"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet,[inet],econnrefused}]})),
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
		% TCP connection refused over IPv6 — reclassified as unknown (transient: server briefly not listening)
		?assertEqual({unknown, "Failed to establish a TCP connection to host example.l42.eu on port 443 over ipv6"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet6,[inet6],econnrefused}]})),
		% TCP connection timed out over IPv6
		?assertEqual({unknown, "TCP connection timed out whilst connecting to example.l42.eu on port 443 over ipv6"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet6,[inet6],etimedout}]})),
		% HTTP-level connection timeout over IPv6
		?assertEqual({unknown, "HTTP connection timed out whilst connecting to example.l42.eu on port 443 over ipv6"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet6,[inet6],timeout}]})),
		% TCP connection was closed over IPv6 — reclassified as unknown (transient: mid-stream during restart)
		?assertEqual({unknown, "TCP connection was closed connecting to host example.l42.eu on port 443 over ipv6"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet6,[inet6],closed}]})).

	parseError_combined_test() ->
		% Both IPv4 and IPv6 fail with DNS errors (both unknown → overall unknown)
		?assertEqual({unknown, "DNS failure when trying to resolve ipv4 address for example.l42.eu; DNS failure when trying to resolve ipv6 address for example.l42.eu"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet6,[inet6],nxdomain},{inet,[inet],nxdomain}]})),
		% Both IPv4 and IPv6 connection refused — both now unknown → overall unknown
		?assertEqual({unknown, "Failed to establish a TCP connection to host example.l42.eu on port 443 over ipv4; Failed to establish a TCP connection to host example.l42.eu on port 443 over ipv6"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet6,[inet6],econnrefused},{inet,[inet],econnrefused}]})),
		% IPv4 connection refused (now unknown) + IPv6 DNS failure (unknown) → overall unknown
		?assertEqual({unknown, "Failed to establish a TCP connection to host example.l42.eu on port 443 over ipv4; DNS failure when trying to resolve ipv6 address for example.l42.eu"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet6,[inet6],nxdomain},{inet,[inet],econnrefused}]})),
		% IPv4 definitive failure (unknown error type → false) + IPv6 DNS failure (unknown) → overall false
		?assertEqual({false, "An unknown connection error occured: not_a_real_error (ipv4 connection); DNS failure when trying to resolve ipv6 address for example.l42.eu"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet6,[inet6],nxdomain},{inet,[inet],not_a_real_error}]})),
		% Both sides definitive failure (unknown error type → false on each) → overall false
		?assertEqual({false, "An unknown connection error occured: not_a_real_error (ipv4 connection); An unknown connection error occured: not_a_real_error (ipv6 connection)"}, parseError({failed_connect,[{to_address,{"example.l42.eu",443}}, {inet6,[inet6],not_a_real_error},{inet,[inet],not_a_real_error}]})).

	tlsNotAfterToUnix_test() ->
		% utcTime: YY >= 50 maps to 1900s. "700101000000Z" = 1970-01-01 00:00:00 UTC = Unix epoch = 0
		?assertEqual(0, tlsNotAfterToUnix({utcTime, "700101000000Z"})),
		% utcTime: YY < 50 maps to 2000s. "000101000000Z" = 2000-01-01 00:00:00 UTC = 946684800
		?assertEqual(946684800, tlsNotAfterToUnix({utcTime, "000101000000Z"})),
		% generalTime: "19700101000000Z" = 1970-01-01 00:00:00 UTC = 0
		?assertEqual(0, tlsNotAfterToUnix({generalTime, "19700101000000Z"})),
		% generalTime: "20000101000000Z" = 2000-01-01 00:00:00 UTC = 946684800
		?assertEqual(946684800, tlsNotAfterToUnix({generalTime, "20000101000000Z"})).

	datetimeToUnixSeconds_test() ->
		% Unix epoch maps to 0
		?assertEqual(0, datetimeToUnixSeconds({{1970, 1, 1}, {0, 0, 0}})),
		% 2000-01-01 00:00:00 UTC = 946684800
		?assertEqual(946684800, datetimeToUnixSeconds({{2000, 1, 1}, {0, 0, 0}})),
		% One second before epoch is negative
		?assertEqual(-1, datetimeToUnixSeconds({{1969, 12, 31}, {23, 59, 59}})).

-endif.
