-module(fetcher).
-export([start/1, tryRunChecks/2, parseConfigyRepos/1]).
-include_lib("eunit/include/eunit.hrl").

start(StatePid) ->
	{ok, _} = application:ensure_all_started([ssl, inets]),
	{ok, Device} = file:open("./service-list", [read]),
	spawnFetcher(StatePid, Device),
	file:close(Device),
	spawnCIFetcher(StatePid).

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

% Reads the CI repo list (written at build time from configy) and spawns a
% recurring CI check process for each repo.
spawnCIFetcher(StatePid) ->
	{ok, Body} = file:read_file("./ci-repo-list"),
	Repos = parseConfigyRepos(binary_to_list(Body)),
	lists:foreach(fun({RepoId, Host}) ->
		spawn(fun() -> ciRepoLoop(StatePid, RepoId, Host) end)
	end, Repos).

parseConfigyRepos(Body) ->
	Repos = jiffy:decode(Body, [return_maps]),
	[{binary_to_list(maps:get(<<"id">>, Repo)),
	  repoHost(Repo)} || Repo <- Repos].

repoHost(Repo) ->
	case maps:get(<<"domain">>, Repo, null) of
		null -> binary_to_list(maps:get(<<"id">>, Repo));
		Domain -> binary_to_list(Domain)
	end.

ciRepoLoop(StatePid, RepoId, Host) ->
	try
		Slug = list_to_binary("gh/lucas42/" ++ RepoId),
		CIChecks = checkCIForSlug(Slug, skip),
		case CIChecks of
			skip -> ok;
			_ -> ok = gen_server:cast(StatePid, {updateSystem, Host, RepoId, CIChecks, #{}})
		end
	catch
		ExceptionClass:Term:StackTrace ->
			io:format("ExceptionClass: ~p Term: ~p StackTrace: ~p~n", [ExceptionClass, Term, StackTrace])
	end,
	timer:sleep(timer:seconds(60)),
	ciRepoLoop(StatePid, RepoId, Host).

runChecks(StatePid, Host) ->
	{TLSCheck} = checkTlsExpiry(Host),
	{InfoCheck, System, Checks, Metrics} = fetchInfo(Host),
	AllChecks = maps:merge(#{
		<<"fetch-info">> => InfoCheck,
		<<"tls-certificate">> => TLSCheck
	}, Checks),
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
	{System, Checks, Metrics}.


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

% CircleCI pipeline check logic. OnNotFound is the value to return when the
% project returns 404 — pass `skip` to silently ignore repos with no CI.
checkCIForSlug(CircleCISlug, OnNotFound) ->
	TechDetail = <<"Checks status of recent circleCI pipelines">>,
	Token = os:getenv("CIRCLECI_API_TOKEN", ""),
	AuthHeader = {"Circle-Token", Token},
	% Fetch the last 5 pipelines so that a failed pipeline followed by a
	% push-to-fix (which creates a new pipeline) is still detected.
	PipelineUrl = "https://circleci.com/api/v2/project/"++binary_to_list(CircleCISlug)++"/pipeline?branch=main&limit=5",
	case httpc:request(get, {PipelineUrl, [{"Accept","application/json"}, AuthHeader]}, [{timeout, timer:seconds(5)},{ssl,[{verify, verify_peer},{cacerts, public_key:cacerts_get()}]}], []) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, PipelineBody}} ->
			PipelineResponse = jiffy:decode(PipelineBody, [return_maps]),
			case maps:get(<<"items">>, PipelineResponse, []) of
				[] ->
					#{<<"circleci">> => #{
						<<"ok">> => true,
						<<"techDetail">> => TechDetail,
						<<"debug">> => <<"No recent pipelines found">>
					}};
				[LatestPipeline | OtherPipelines] ->
					PipelineNumber = maps:get(<<"number">>, LatestPipeline),
					SlugStr = binary_to_list(CircleCISlug),
					WebSlug = re:replace(SlugStr, "^gh/", "github/", [{return, list}]),
					LatestPipelineUrl = "https://app.circleci.com/pipelines/"++WebSlug++"/"++integer_to_list(PipelineNumber),
					AllPipelines = [LatestPipeline | OtherPipelines],
					AllWorkflows = collectAllWorkflows(AllPipelines, AuthHeader),
					checkWorkflowStatuses(SlugStr, AllWorkflows, LatestPipelineUrl, TechDetail)
			end;
		{ok, {{_Version, 404, _ReasonPhrase}, _Headers, _Body}} ->
			OnNotFound;
		{ok, {{_Version, StatusCode, ReasonPhrase}, _Headers, _Body}} when StatusCode >= 500 ->
			#{<<"circleci">> => #{
				<<"ok">> => unknown,
				<<"techDetail">> => TechDetail,
				<<"debug">> => list_to_binary("Received HTTP response with status "++integer_to_list(StatusCode)++" "++ReasonPhrase++" from pipeline endpoint")
			}};
		{ok, {{_Version, StatusCode, ReasonPhrase}, _Headers, _Body}} ->
			#{<<"circleci">> => #{
				<<"ok">> => false,
				<<"techDetail">> => TechDetail,
				<<"debug">> => list_to_binary("Received HTTP response with status "++integer_to_list(StatusCode)++" "++ReasonPhrase++" from pipeline endpoint")
			}};
		{error, Error} ->
			{Ok, Debug} = parseError(Error),
			#{<<"circleci">> => #{
				<<"ok">> => Ok,
				<<"techDetail">> => TechDetail,
				<<"debug">> => list_to_binary(Debug)
			}}
	end.

% Fetches workflows for each pipeline in the list and concatenates them into a
% single flat list. Errors fetching a pipeline's workflows are silently skipped
% so that a transient API failure on one pipeline doesn't hide results from others.
collectAllWorkflows([], _AuthHeader) -> [];
collectAllWorkflows([Pipeline | Rest], AuthHeader) ->
	PipelineId = binary_to_list(maps:get(<<"id">>, Pipeline)),
	WorkflowUrl = "https://circleci.com/api/v2/pipeline/"++PipelineId++"/workflow",
	Workflows = case httpc:request(get, {WorkflowUrl, [{"Accept","application/json"}, AuthHeader]}, [{timeout, timer:seconds(5)},{ssl,[{verify, verify_peer},{cacerts, public_key:cacerts_get()}]}], []) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, WorkflowBody}} ->
			WorkflowResponse = jiffy:decode(WorkflowBody, [return_maps]),
			maps:get(<<"items">>, WorkflowResponse, []);
		_ ->
			[]
	end,
	Workflows ++ collectAllWorkflows(Rest, AuthHeader).

% For each workflow name, keep only the most recent workflow (by created_at).
% This ensures that a successful retry supersedes an earlier failure with the same name.
keepLatestWorkflowPerName(Workflows) ->
	LatestByName = lists:foldl(fun(W, Acc) ->
		Name = maps:get(<<"name">>, W, <<"">>),
		CreatedAt = maps:get(<<"created_at">>, W, <<"">>),
		case maps:find(Name, Acc) of
			{ok, Existing} ->
				ExistingCreatedAt = maps:get(<<"created_at">>, Existing, <<"">>),
				if CreatedAt > ExistingCreatedAt -> maps:put(Name, W, Acc);
				   true -> Acc
				end;
			error ->
				maps:put(Name, W, Acc)
		end
	end, #{}, Workflows),
	maps:values(LatestByName).

checkWorkflowStatuses(_Slug, [], PipelineUrl, TechDetail) ->
	#{<<"circleci">> => #{
		<<"ok">> => true,
		<<"techDetail">> => TechDetail,
		<<"debug">> => <<"No workflows found for most recent pipeline">>,
		<<"link">> => list_to_binary(PipelineUrl)
	}};
checkWorkflowStatuses(_Slug, Workflows, PipelineUrl, TechDetail) ->
	LatestWorkflows = keepLatestWorkflowPerName(Workflows),
	FailedWorkflows = [W || W <- LatestWorkflows, maps:get(<<"status">>, W, null) =:= <<"failed">>],
	RunningWorkflows = [W || W <- LatestWorkflows, maps:get(<<"status">>, W, null) =:= <<"running">>],
	case FailedWorkflows of
		[FailedWorkflow | _] ->
			WorkflowName = maps:get(<<"name">>, FailedWorkflow, <<"unknown">>),
			WorkflowId = binary_to_list(maps:get(<<"id">>, FailedWorkflow, <<"">>)),
			WorkflowLink = PipelineUrl++"/workflows/"++WorkflowId,
			#{<<"circleci">> => #{
				<<"ok">> => false,
				<<"techDetail">> => TechDetail,
				<<"debug">> => <<"Workflow \"", WorkflowName/binary, "\" failed">>,
				<<"link">> => list_to_binary(WorkflowLink)
			}};
		[] ->
			case RunningWorkflows of
				[_ | _] ->
					#{<<"circleci">> => #{
						<<"ok">> => true,
						<<"techDetail">> => TechDetail,
						<<"debug">> => <<"Pipeline is still running">>,
						<<"link">> => list_to_binary(PipelineUrl)
					}};
				[] ->
					#{<<"circleci">> => #{
						<<"ok">> => true,
						<<"techDetail">> => TechDetail,
						<<"link">> => list_to_binary(PipelineUrl)
					}}
			end
	end.

-ifdef(TEST).
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

	checkWorkflowStatuses_empty_test() ->
		% No workflows → ok with debug note
		Result = checkWorkflowStatuses("gh/lucas42/lucos_test", [], "https://app.circleci.com/pipelines/github/lucas42/lucos_test/42", <<"Checks status of most recent circleCI pipeline">>),
		?assertEqual(#{<<"circleci">> => #{
			<<"ok">> => true,
			<<"techDetail">> => <<"Checks status of most recent circleCI pipeline">>,
			<<"debug">> => <<"No workflows found for most recent pipeline">>,
			<<"link">> => <<"https://app.circleci.com/pipelines/github/lucas42/lucos_test/42">>
		}}, Result).

	checkWorkflowStatuses_success_test() ->
		% Single successful workflow → ok
		Workflows = [#{<<"id">> => <<"wf-1">>, <<"name">> => <<"build-deploy">>, <<"status">> => <<"success">>}],
		Result = checkWorkflowStatuses("gh/lucas42/lucos_test", Workflows, "https://app.circleci.com/pipelines/github/lucas42/lucos_test/42", <<"Checks status of most recent circleCI pipeline">>),
		?assertEqual(#{<<"circleci">> => #{
			<<"ok">> => true,
			<<"techDetail">> => <<"Checks status of most recent circleCI pipeline">>,
			<<"link">> => <<"https://app.circleci.com/pipelines/github/lucas42/lucos_test/42">>
		}}, Result).

	checkWorkflowStatuses_failed_test() ->
		% Single failed workflow → not ok, links to workflow (pipeline number included in URL)
		Workflows = [#{<<"id">> => <<"wf-2">>, <<"name">> => <<"build-deploy">>, <<"status">> => <<"failed">>}],
		Result = checkWorkflowStatuses("gh/lucas42/lucos_test", Workflows, "https://app.circleci.com/pipelines/github/lucas42/lucos_test/42", <<"Checks status of most recent circleCI pipeline">>),
		?assertEqual(#{<<"circleci">> => #{
			<<"ok">> => false,
			<<"techDetail">> => <<"Checks status of most recent circleCI pipeline">>,
			<<"debug">> => <<"Workflow \"build-deploy\" failed">>,
			<<"link">> => <<"https://app.circleci.com/pipelines/github/lucas42/lucos_test/42/workflows/wf-2">>
		}}, Result).

	checkWorkflowStatuses_failed_wins_over_success_test() ->
		% One failed, one success → not ok (failed takes priority, no race condition)
		Workflows = [
			#{<<"id">> => <<"wf-1">>, <<"name">> => <<"build-amd64">>, <<"status">> => <<"failed">>},
			#{<<"id">> => <<"wf-2">>, <<"name">> => <<"test-api">>, <<"status">> => <<"success">>}
		],
		Result = checkWorkflowStatuses("gh/lucas42/lucos_test", Workflows, "https://app.circleci.com/pipelines/github/lucas42/lucos_test/42", <<"Checks status of most recent circleCI pipeline">>),
		?assertMatch(#{<<"circleci">> := #{<<"ok">> := false}}, Result).

	checkWorkflowStatuses_running_test() ->
		% All running → ok with debug note
		Workflows = [#{<<"id">> => <<"wf-1">>, <<"name">> => <<"build-deploy">>, <<"status">> => <<"running">>}],
		Result = checkWorkflowStatuses("gh/lucas42/lucos_test", Workflows, "https://app.circleci.com/pipelines/github/lucas42/lucos_test/42", <<"Checks status of most recent circleCI pipeline">>),
		?assertEqual(#{<<"circleci">> => #{
			<<"ok">> => true,
			<<"techDetail">> => <<"Checks status of most recent circleCI pipeline">>,
			<<"debug">> => <<"Pipeline is still running">>,
			<<"link">> => <<"https://app.circleci.com/pipelines/github/lucas42/lucos_test/42">>
		}}, Result).

	checkWorkflowStatuses_on_hold_test() ->
		% on_hold (awaiting approval) → treated as ok (no action to take yet)
		Workflows = [#{<<"id">> => <<"wf-1">>, <<"name">> => <<"build-deploy">>, <<"status">> => <<"on_hold">>}],
		Result = checkWorkflowStatuses("gh/lucas42/lucos_test", Workflows, "https://app.circleci.com/pipelines/github/lucas42/lucos_test/42", <<"Checks status of most recent circleCI pipeline">>),
		?assertEqual(#{<<"circleci">> => #{
			<<"ok">> => true,
			<<"techDetail">> => <<"Checks status of most recent circleCI pipeline">>,
			<<"link">> => <<"https://app.circleci.com/pipelines/github/lucas42/lucos_test/42">>
		}}, Result).

	checkWorkflowStatuses_retry_success_supersedes_failure_test() ->
		% A successful re-run (same workflow name, later created_at) should supersede an earlier failure.
		% This is the bug described in #34: manual retries were being ignored.
		Workflows = [
			#{<<"id">> => <<"wf-1">>, <<"name">> => <<"build-deploy">>, <<"status">> => <<"failed">>,  <<"created_at">> => <<"2026-02-22T16:42:52.000Z">>},
			#{<<"id">> => <<"wf-2">>, <<"name">> => <<"build-deploy">>, <<"status">> => <<"success">>, <<"created_at">> => <<"2026-02-22T17:52:17.000Z">>}
		],
		Result = checkWorkflowStatuses("gh/lucas42/lucos_test", Workflows, "https://app.circleci.com/pipelines/github/lucas42/lucos_test/42", <<"Checks status of most recent circleCI pipeline">>),
		?assertMatch(#{<<"circleci">> := #{<<"ok">> := true}}, Result).

	keepLatestWorkflowPerName_test() ->
		% Single workflow → returned as-is
		W1 = #{<<"id">> => <<"wf-1">>, <<"name">> => <<"build-deploy">>, <<"created_at">> => <<"2026-02-22T16:00:00.000Z">>},
		?assertEqual([W1], keepLatestWorkflowPerName([W1])),

		% Two workflows with the same name → only the later one survives
		W2 = #{<<"id">> => <<"wf-2">>, <<"name">> => <<"build-deploy">>, <<"created_at">> => <<"2026-02-22T17:00:00.000Z">>},
		?assertEqual([W2], keepLatestWorkflowPerName([W1, W2])),

		% Two workflows with different names → both survive
		W3 = #{<<"id">> => <<"wf-3">>, <<"name">> => <<"test-api">>, <<"created_at">> => <<"2026-02-22T16:00:00.000Z">>},
		Result = keepLatestWorkflowPerName([W1, W3]),
		?assertEqual(2, length(Result)),
		?assert(lists:member(W1, Result)),
		?assert(lists:member(W3, Result)).

	% A failed workflow from an older pipeline is superseded by a success in a newer
	% pipeline when workflows from both pipelines are combined into a single list.
	% This is the cross-pipeline case that was previously invisible to monitoring:
	% failed pipeline N, then push-to-fix creates pipeline N+1 (success).
	checkWorkflowStatuses_cross_pipeline_success_supersedes_failure_test() ->
		Workflows = [
			% Older pipeline: failed
			#{<<"id">> => <<"wf-old">>, <<"name">> => <<"build-deploy">>, <<"status">> => <<"failed">>,  <<"created_at">> => <<"2026-03-13T17:11:00.000Z">>},
			% Newer pipeline: success (push-to-fix)
			#{<<"id">> => <<"wf-new">>, <<"name">> => <<"build-deploy">>, <<"status">> => <<"success">>, <<"created_at">> => <<"2026-03-13T17:15:00.000Z">>}
		],
		Result = checkWorkflowStatuses("gh/lucas42/lucos_test", Workflows, "https://app.circleci.com/pipelines/github/lucas42/lucos_test/43", <<"Checks status of recent circleCI pipelines">>),
		?assertMatch(#{<<"circleci">> := #{<<"ok">> := true}}, Result).

	% A failed workflow from a newer pipeline is NOT superseded by a success from an
	% older one: the failure is the most recent state and must be surfaced.
	checkWorkflowStatuses_cross_pipeline_failure_after_success_test() ->
		Workflows = [
			% Older pipeline: success
			#{<<"id">> => <<"wf-old">>, <<"name">> => <<"build-deploy">>, <<"status">> => <<"success">>, <<"created_at">> => <<"2026-03-13T17:11:00.000Z">>},
			% Newer pipeline: failed (regression)
			#{<<"id">> => <<"wf-new">>, <<"name">> => <<"build-deploy">>, <<"status">> => <<"failed">>,  <<"created_at">> => <<"2026-03-13T17:22:00.000Z">>}
		],
		Result = checkWorkflowStatuses("gh/lucas42/lucos_test", Workflows, "https://app.circleci.com/pipelines/github/lucas42/lucos_test/44", <<"Checks status of recent circleCI pipelines">>),
		?assertMatch(#{<<"circleci">> := #{<<"ok">> := false}}, Result).

	parseConfigyRepos_test() ->
		% System with a domain: host is the domain.
		% Component with no domain: host falls back to the repo id.
		Body = "[{\"id\":\"lucos_foo\",\"domain\":\"foo.l42.eu\"},{\"id\":\"lucos_bar\",\"domain\":null}]",
		?assertEqual([{"lucos_foo", "foo.l42.eu"}, {"lucos_bar", "lucos_bar"}], parseConfigyRepos(Body)).

	parseConfigyRepos_empty_test() ->
		?assertEqual([], parseConfigyRepos("[]")).

-endif.