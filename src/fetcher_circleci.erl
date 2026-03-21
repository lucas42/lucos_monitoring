-module(fetcher_circleci).
-export([start/1, parseConfigyRepos/1]).
-include_lib("eunit/include/eunit.hrl").

% Reads the CI repo lists (written at build time from configy) and spawns a
% recurring CI check process for each repo.
start(StatePid) ->
	{ok, _} = application:ensure_all_started([ssl, inets]),
	{ok, SystemsBody} = file:read_file("./ci-systems-list"),
	{ok, ComponentsBody} = file:read_file("./ci-components-list"),
	Repos = parseConfigyRepos(binary_to_list(SystemsBody)) ++
	        parseConfigyRepos(binary_to_list(ComponentsBody)),
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
		Slug = "github/lucas42/" ++ RepoId,
		case checkCIForSlug(Slug) of
			skip -> ok;
			CIChecks -> ok = gen_server:cast(StatePid, {updateSystem, Host, RepoId, CIChecks, #{}})
		end
	catch
		ExceptionClass:Term:StackTrace ->
			io:format("ExceptionClass: ~p Term: ~p StackTrace: ~p~n", [ExceptionClass, Term, StackTrace])
	end,
	timer:sleep(timer:seconds(60)),
	ciRepoLoop(StatePid, RepoId, Host).

% CircleCI pipeline check logic. Returns `skip` when the project returns 404
% (no CI configured), otherwise returns a checks map.
checkCIForSlug(Slug) ->
	TechDetail = <<"Checks status of recent circleCI pipelines">>,
	Token = os:getenv("CIRCLECI_API_TOKEN", ""),
	AuthHeader = {"Circle-Token", Token},
	% Fetch the last 5 pipelines so that a failed pipeline followed by a
	% push-to-fix (which creates a new pipeline) is still detected.
	PipelineUrl = "https://circleci.com/api/v2/project/"++Slug++"/pipeline?branch=main&limit=5",
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
					LatestPipelineUrl = "https://app.circleci.com/pipelines/"++Slug++"/"++integer_to_list(PipelineNumber),
					AllPipelines = [LatestPipeline | OtherPipelines],
					AllWorkflows = collectAllWorkflows(AllPipelines, AuthHeader),
					checkWorkflowStatuses(Slug, AllWorkflows, LatestPipelineUrl, TechDetail)
			end;
		{ok, {{_Version, 404, _ReasonPhrase}, _Headers, _Body}} ->
			skip;
		{ok, {{_Version, StatusCode, ReasonPhrase}, _Headers, _Body}} ->
			#{<<"circleci">> => #{
				<<"ok">> => unknown,
				<<"techDetail">> => TechDetail,
				<<"debug">> => list_to_binary("Received HTTP response with status "++integer_to_list(StatusCode)++" "++ReasonPhrase++" from pipeline endpoint")
			}};
		{error, _Error} ->
			#{<<"circleci">> => #{
				<<"ok">> => unknown,
				<<"techDetail">> => TechDetail,
				<<"debug">> => <<"Error making request to CircleCI API">>
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
	parseConfigyRepos_test() ->
		% System with a domain: host is the domain.
		% Component with no domain: host falls back to the repo id.
		Body = "[{\"id\":\"lucos_foo\",\"domain\":\"foo.l42.eu\"},{\"id\":\"lucos_bar\",\"domain\":null}]",
		?assertEqual([{"lucos_foo", "foo.l42.eu"}, {"lucos_bar", "lucos_bar"}], parseConfigyRepos(Body)).

	parseConfigyRepos_empty_test() ->
		?assertEqual([], parseConfigyRepos("[]")).

	checkWorkflowStatuses_empty_test() ->
		% No workflows → ok with debug note
		Result = checkWorkflowStatuses("github/lucas42/lucos_test", [], "https://app.circleci.com/pipelines/github/lucas42/lucos_test/42", <<"Checks status of most recent circleCI pipeline">>),
		?assertEqual(#{<<"circleci">> => #{
			<<"ok">> => true,
			<<"techDetail">> => <<"Checks status of most recent circleCI pipeline">>,
			<<"debug">> => <<"No workflows found for most recent pipeline">>,
			<<"link">> => <<"https://app.circleci.com/pipelines/github/lucas42/lucos_test/42">>
		}}, Result).

	checkWorkflowStatuses_success_test() ->
		% Single successful workflow → ok
		Workflows = [#{<<"id">> => <<"wf-1">>, <<"name">> => <<"build-deploy">>, <<"status">> => <<"success">>}],
		Result = checkWorkflowStatuses("github/lucas42/lucos_test", Workflows, "https://app.circleci.com/pipelines/github/lucas42/lucos_test/42", <<"Checks status of most recent circleCI pipeline">>),
		?assertEqual(#{<<"circleci">> => #{
			<<"ok">> => true,
			<<"techDetail">> => <<"Checks status of most recent circleCI pipeline">>,
			<<"link">> => <<"https://app.circleci.com/pipelines/github/lucas42/lucos_test/42">>
		}}, Result).

	checkWorkflowStatuses_failed_test() ->
		% Single failed workflow → not ok, links to workflow (pipeline number included in URL)
		Workflows = [#{<<"id">> => <<"wf-2">>, <<"name">> => <<"build-deploy">>, <<"status">> => <<"failed">>}],
		Result = checkWorkflowStatuses("github/lucas42/lucos_test", Workflows, "https://app.circleci.com/pipelines/github/lucas42/lucos_test/42", <<"Checks status of most recent circleCI pipeline">>),
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
		Result = checkWorkflowStatuses("github/lucas42/lucos_test", Workflows, "https://app.circleci.com/pipelines/github/lucas42/lucos_test/42", <<"Checks status of most recent circleCI pipeline">>),
		?assertMatch(#{<<"circleci">> := #{<<"ok">> := false}}, Result).

	checkWorkflowStatuses_running_test() ->
		% All running → ok with debug note
		Workflows = [#{<<"id">> => <<"wf-1">>, <<"name">> => <<"build-deploy">>, <<"status">> => <<"running">>}],
		Result = checkWorkflowStatuses("github/lucas42/lucos_test", Workflows, "https://app.circleci.com/pipelines/github/lucas42/lucos_test/42", <<"Checks status of most recent circleCI pipeline">>),
		?assertEqual(#{<<"circleci">> => #{
			<<"ok">> => true,
			<<"techDetail">> => <<"Checks status of most recent circleCI pipeline">>,
			<<"debug">> => <<"Pipeline is still running">>,
			<<"link">> => <<"https://app.circleci.com/pipelines/github/lucas42/lucos_test/42">>
		}}, Result).

	checkWorkflowStatuses_on_hold_test() ->
		% on_hold (awaiting approval) → treated as ok (no action to take yet)
		Workflows = [#{<<"id">> => <<"wf-1">>, <<"name">> => <<"build-deploy">>, <<"status">> => <<"on_hold">>}],
		Result = checkWorkflowStatuses("github/lucas42/lucos_test", Workflows, "https://app.circleci.com/pipelines/github/lucas42/lucos_test/42", <<"Checks status of most recent circleCI pipeline">>),
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
		Result = checkWorkflowStatuses("github/lucas42/lucos_test", Workflows, "https://app.circleci.com/pipelines/github/lucas42/lucos_test/42", <<"Checks status of most recent circleCI pipeline">>),
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
		Result = checkWorkflowStatuses("github/lucas42/lucos_test", Workflows, "https://app.circleci.com/pipelines/github/lucas42/lucos_test/43", <<"Checks status of recent circleCI pipelines">>),
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
		Result = checkWorkflowStatuses("github/lucas42/lucos_test", Workflows, "https://app.circleci.com/pipelines/github/lucas42/lucos_test/44", <<"Checks status of recent circleCI pipelines">>),
		?assertMatch(#{<<"circleci">> := #{<<"ok">> := false}}, Result).

-endif.
