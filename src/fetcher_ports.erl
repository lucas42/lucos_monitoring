-module(fetcher_ports).
-export([start/1, tryRunChecks/2]).
-include_lib("eunit/include/eunit.hrl").

% Probes raw TCP reachability of declared public ports.
%
% Source of truth is configy's `public_ports` declarations — the same list that
% drives lucos_firewall's inbound allow-list — so "declared public" and "monitored"
% are the same set by construction. The data is curled into config/info-ports-list.json
% at Docker build from the FULL configy.l42.eu/systems endpoint (not /systems/http),
% because the systems most worth probing here (dns, dns_secondary) have no http_port
% and so are absent from the HTTP-only list used by fetcher_info.
%
% For every system that declares a TCP public_port AND has a domain to connect to,
% start/1 spawns a 60s loop that opens a bare TCP connection to each such port and
% reports a `port-<N>-reachable` check under the `ports` source (which merges
% alongside fetch-info/tls/circleci in the state server rather than clobbering them).
%
% Two entries are skipped, by construction rather than by name:
%   - systems with no `domain` (e.g. lucos_router) — there is no hostname to connect to;
%   - non-TCP public_ports (e.g. dns's UDP:53) — they can't be TCP-probed.
% This is a liveness floor: a green check means the port accepts a TCP connection,
% not that the service behind it is functionally healthy (e.g. for DNS it does not
% verify resolution — the primary DNS path is UDP, which is not probed).

start(StatePid) ->
	{ok, Body} = file:read_file("./config/info-ports-list.json"),
	Targets = parsePortTargets(binary_to_list(Body)),
	logger:notice("fetcher_ports: probing ~p target system(s)", [length(Targets)]),
	lists:foreach(fun(Target) ->
		spawn(?MODULE, tryRunChecks, [StatePid, Target])
	end, Targets).

% Parses the configy /systems JSON into a list of probe targets.
% Each target is {Id, Type, Host, Ports} where Ports is a non-empty list of
% {Port, Purpose}. Systems with no domain or no TCP public_ports are dropped.
parsePortTargets(Body) ->
	Entries = jiffy:decode(Body, [return_maps]),
	lists:filtermap(fun(Entry) ->
		Id = binary_to_list(maps:get(<<"id">>, Entry)),
		Type = binary_to_atom(maps:get(<<"type">>, Entry, <<"system">>), utf8),
		Host = case maps:get(<<"domain">>, Entry, null) of
			null -> "";
			Domain -> binary_to_list(Domain)
		end,
		RawPorts = maps:get(<<"public_ports">>, Entry, []),
		TcpPorts = [{maps:get(<<"port">>, P), maps:get(<<"purpose">>, P, <<"">>)}
		            || P <- RawPorts, maps:get(<<"protocol">>, P, <<"">>) =:= <<"tcp">>],
		case {Host, TcpPorts} of
			% No domain (e.g. lucos_router) — nothing to connect to. Skip generically.
			{"", _} -> false;
			% No TCP public ports — nothing to probe.
			{_, []} -> false;
			{_, Ports} -> {true, {Id, Type, Host, Ports}}
		end
	end, Entries).

tryRunChecks(StatePid, Target) ->
	try runChecks(StatePid, Target) of
		_ -> ok
	catch
		ExceptionClass:Term:StackTrace ->
			logger:error("ExceptionClass: ~p Term: ~p StackTrace: ~p", [ExceptionClass, Term, StackTrace])
	end,
	timer:sleep(timer:seconds(60)),
	tryRunChecks(StatePid, Target).

runChecks(StatePid, {Id, Type, Host, Ports}) ->
	Checks = lists:foldl(fun({Port, Purpose}, Acc) ->
		Key = list_to_binary("port-" ++ integer_to_list(Port) ++ "-reachable"),
		Check = make_port_probe_check(checkPortReachable(Host, Port, Purpose)),
		maps:put(Key, Check, Acc)
	end, #{}, Ports),
	ok = gen_server:cast(StatePid, {updateSystem, Host, Id, Type, ports, Checks, #{}}).

% Opens a bare TCP connection (no payload, no TLS) to Host:Port with a 1s deadline.
% A successful connect → ok: true. Connection-level errors classify to `unknown` so
% the UnknownsGate absorbs a single-poll restart blip; a port that stays unreachable
% escalates to a real failure after ?CONSECUTIVE_UNKNOWNS_THRESHOLD polls. Only a
% genuinely unexpected error returns ok: false immediately.
checkPortReachable(Host, Port, Purpose) ->
	TechDetail = list_to_binary(
		"Confirms a TCP connection can be established to port " ++ integer_to_list(Port) ++
		" (" ++ binary_to_list(Purpose) ++ "). Liveness floor only: a green check means "
		"the port is accepting TCP connections, not that the service behind it is "
		"functionally healthy."),
	case gen_tcp:connect(Host, Port, [{active, false}], timer:seconds(1)) of
		{ok, Socket} ->
			gen_tcp:close(Socket),
			#{<<"ok">> => true, <<"techDetail">> => TechDetail};
		{error, Reason} ->
			{Ok, Debug} = classifyConnectError(Host, Port, Reason),
			#{<<"ok">> => Ok, <<"techDetail">> => TechDetail, <<"debug">> => list_to_binary(Debug)}
	end.

% Maps a gen_tcp:connect error to {ok-value, debug-string}. Connection-level errors
% are transient-or-sustained ambiguous on any single poll, so they report `unknown`
% and lean on the UnknownsGate to escalate a persistent one. An unrecognised error is
% surfaced loudly as `false`.
classifyConnectError(Host, Port, Reason) ->
	Target = Host ++ ":" ++ integer_to_list(Port),
	case Reason of
		econnrefused -> {unknown, "TCP connection to " ++ Target ++ " refused (listener may be down or restarting)"};
		etimedout    -> {unknown, "TCP connection to " ++ Target ++ " timed out"};
		timeout      -> {unknown, "TCP connection to " ++ Target ++ " timed out (1s connect deadline)"};
		ehostunreach -> {unknown, "No route to host " ++ Host};
		enetunreach  -> {unknown, "Network unreachable for host " ++ Host};
		nxdomain     -> {unknown, "DNS resolution failed for " ++ Host};
		closed       -> {unknown, "TCP connection to " ++ Target ++ " closed during handshake"};
		Other        -> {false, "Unexpected error connecting to " ++ Target ++ ": " ++ lists:flatten(io_lib:format("~p", [Other]))}
	end.

% Stamps the suppression policy for a port-reachability check: failThreshold 2
% (absorb a single-poll restart/deploy blip) + dependsOn [lucos_dns].
%
% This deliberately does NOT depend on lucos_router (unlike fetcher_info's
% make_direct_probe_check, used by the HTTP probes). A raw gen_tcp:connect to the
% target port bypasses nginx entirely, so a router deploy is not on the path —
% depending on it would suppress a genuine port outage that happened to coincide
% with a router deploy window, which is exactly the failure mode this check exists
% to catch. The probe DOES resolve the target's domain, so the lucos_dns dependency
% (suppress during a DNS deploy window) is retained.
make_port_probe_check(Check) ->
	maps:merge(Check, #{
		<<"failThreshold">> => 2,
		<<"dependsOn">> => [<<"lucos_dns">>]
	}).

-ifdef(TEST).

	% Only TCP public_ports with a domain produce a target; the {Port, Purpose} pairs are carried through.
	parsePortTargets_basic_test() ->
		Body = "[{\"id\":\"lucos_mail\",\"domain\":\"mail.l42.eu\",\"public_ports\":[{\"port\":25,\"protocol\":\"tcp\",\"purpose\":\"SMTP inbound\"}]}]",
		?assertEqual([{"lucos_mail", system, "mail.l42.eu", [{25, <<"SMTP inbound">>}]}], parsePortTargets(Body)).

	% A system with no domain (e.g. lucos_router) is dropped — there is no host to connect to.
	parsePortTargets_no_domain_dropped_test() ->
		Body = "[{\"id\":\"lucos_router\",\"domain\":null,\"public_ports\":[{\"port\":443,\"protocol\":\"tcp\",\"purpose\":\"HTTPS\"}]}]",
		?assertEqual([], parsePortTargets(Body)).

	% A system with an absent domain field is also dropped.
	parsePortTargets_absent_domain_dropped_test() ->
		Body = "[{\"id\":\"lucos_router\",\"public_ports\":[{\"port\":443,\"protocol\":\"tcp\",\"purpose\":\"HTTPS\"}]}]",
		?assertEqual([], parsePortTargets(Body)).

	% UDP ports are filtered out; a system left with no TCP ports is dropped.
	parsePortTargets_udp_filtered_test() ->
		Body = "[{\"id\":\"lucos_dns\",\"domain\":\"dns.l42.eu\",\"public_ports\":[{\"port\":53,\"protocol\":\"tcp\",\"purpose\":\"DNS (TCP fallback)\"},{\"port\":53,\"protocol\":\"udp\",\"purpose\":\"DNS (primary)\"}]}]",
		?assertEqual([{"lucos_dns", system, "dns.l42.eu", [{53, <<"DNS (TCP fallback)">>}]}], parsePortTargets(Body)).

	% A system that declares only a UDP port yields no target.
	parsePortTargets_udp_only_dropped_test() ->
		Body = "[{\"id\":\"lucos_dns\",\"domain\":\"dns.l42.eu\",\"public_ports\":[{\"port\":53,\"protocol\":\"udp\",\"purpose\":\"DNS (primary)\"}]}]",
		?assertEqual([], parsePortTargets(Body)).

	% Systems with no public_ports field at all are dropped.
	parsePortTargets_no_public_ports_dropped_test() ->
		Body = "[{\"id\":\"lucos_photos\",\"domain\":\"photos.l42.eu\"}]",
		?assertEqual([], parsePortTargets(Body)).

	% Multiple systems and multiple ports are all carried through.
	parsePortTargets_multi_test() ->
		Body = "[{\"id\":\"lucos_creds\",\"domain\":\"creds.l42.eu\",\"public_ports\":[{\"port\":2202,\"protocol\":\"tcp\",\"purpose\":\"SSH\"}]},"
		       "{\"id\":\"lucos_x\",\"domain\":\"x.l42.eu\",\"public_ports\":[{\"port\":1,\"protocol\":\"tcp\",\"purpose\":\"a\"},{\"port\":2,\"protocol\":\"tcp\",\"purpose\":\"b\"}]}]",
		?assertEqual([
			{"lucos_creds", system, "creds.l42.eu", [{2202, <<"SSH">>}]},
			{"lucos_x", system, "x.l42.eu", [{1, <<"a">>}, {2, <<"b">>}]}
		], parsePortTargets(Body)).

	parsePortTargets_empty_test() ->
		?assertEqual([], parsePortTargets("[]")).

	% Type defaults to `system` when absent (the current configy /systems shape has no type field).
	parsePortTargets_type_defaults_to_system_test() ->
		Body = "[{\"id\":\"lucos_mail\",\"domain\":\"mail.l42.eu\",\"public_ports\":[{\"port\":25,\"protocol\":\"tcp\",\"purpose\":\"SMTP inbound\"}]}]",
		[{_, Type, _, _}] = parsePortTargets(Body),
		?assertEqual(system, Type).

	% An explicit type field is honoured (future-proofs against a host/component declaring a public_port).
	parsePortTargets_explicit_type_test() ->
		Body = "[{\"id\":\"lucos_thing\",\"type\":\"component\",\"domain\":\"thing.l42.eu\",\"public_ports\":[{\"port\":9000,\"protocol\":\"tcp\",\"purpose\":\"x\"}]}]",
		?assertEqual([{"lucos_thing", component, "thing.l42.eu", [{9000, <<"x">>}]}], parsePortTargets(Body)).

	% Port checks depend on lucos_dns (resolution) but NOT lucos_router (raw TCP bypasses nginx).
	make_port_probe_check_test() ->
		Result = make_port_probe_check(#{<<"ok">> => true, <<"techDetail">> => <<"t">>}),
		?assertEqual(2, maps:get(<<"failThreshold">>, Result)),
		?assertEqual([<<"lucos_dns">>], maps:get(<<"dependsOn">>, Result)),
		% original fields preserved
		?assertEqual(true, maps:get(<<"ok">>, Result)).

	% Connection-level errors classify to unknown (UnknownsGate escalates a sustained one).
	classifyConnectError_econnrefused_test() ->
		{Ok, Debug} = classifyConnectError("mail.l42.eu", 25, econnrefused),
		?assertEqual(unknown, Ok),
		?assert(string:str(Debug, "refused") > 0).

	classifyConnectError_timeout_test() ->
		?assertMatch({unknown, _}, classifyConnectError("mail.l42.eu", 25, timeout)),
		?assertMatch({unknown, _}, classifyConnectError("mail.l42.eu", 25, etimedout)).

	classifyConnectError_nxdomain_test() ->
		{Ok, Debug} = classifyConnectError("nope.l42.eu", 25, nxdomain),
		?assertEqual(unknown, Ok),
		?assert(string:str(Debug, "DNS resolution failed") > 0).

	classifyConnectError_unreachable_test() ->
		?assertMatch({unknown, _}, classifyConnectError("h", 25, ehostunreach)),
		?assertMatch({unknown, _}, classifyConnectError("h", 25, enetunreach)),
		?assertMatch({unknown, _}, classifyConnectError("h", 25, closed)).

	% An unrecognised error is surfaced as a definitive failure.
	classifyConnectError_unexpected_is_false_test() ->
		{Ok, Debug} = classifyConnectError("mail.l42.eu", 25, some_weird_reason),
		?assertEqual(false, Ok),
		?assert(string:str(Debug, "Unexpected error") > 0).

	% techDetail embeds the port and purpose, and states the liveness-floor caveat.
	checkPortReachable_techdetail_test() ->
		% Connect to a port nothing is listening on locally; we only assert on the
		% techDetail, which is built before the connect result is known.
		Check = checkPortReachable("127.0.0.1", 9, <<"discard">>),
		TechDetail = binary_to_list(maps:get(<<"techDetail">>, Check)),
		?assert(string:str(TechDetail, "port 9") > 0),
		?assert(string:str(TechDetail, "discard") > 0),
		?assert(string:str(TechDetail, "Liveness floor") > 0).

-endif.
