-module(fetcher_scheduled_jobs).
-export([start/1, parseJobEntry/1, jobsToSystemChecks/1]).
-include_lib("eunit/include/eunit.hrl").

% Reads the systems list (written at build time from configy), then spawns a
% single recurring process that polls schedule_tracker /jobs every 60 seconds
% and attributes each scheduled-job check to its owning system.
start(StatePid) ->
	{ok, _} = application:ensure_all_started([ssl, inets]),
	{ok, SystemsBody} = file:read_file("./config/info-systems-list.json"),
	SystemsMap = buildSystemsMap(binary_to_list(SystemsBody)),
	spawn(fun() -> scheduledJobsLoop(StatePid, SystemsMap) end).

% Builds a map from system_id (string) → {Host, Type} from the configy systems
% JSON array.  Entries with null domain fall back to id as host.
buildSystemsMap(Body) ->
	Systems = jiffy:decode(Body, [return_maps]),
	lists:foldl(fun(System, Acc) ->
		Id   = binary_to_list(maps:get(<<"id">>, System)),
		Type = binary_to_atom(maps:get(<<"type">>, System, <<"system">>), utf8),
		Host = case maps:get(<<"domain">>, System, null) of
			null   -> "";
			Domain -> binary_to_list(Domain)
		end,
		maps:put(Id, {Host, Type}, Acc)
	end, #{}, Systems).

scheduledJobsLoop(StatePid, SystemsMap) ->
	try fetchAndUpdate(StatePid, SystemsMap)
	catch ExceptionClass:Term:StackTrace ->
		logger:error("fetcher_scheduled_jobs: ExceptionClass: ~p Term: ~p StackTrace: ~p",
		             [ExceptionClass, Term, StackTrace])
	end,
	timer:sleep(timer:seconds(60)),
	scheduledJobsLoop(StatePid, SystemsMap).

fetchAndUpdate(StatePid, SystemsMap) ->
	Url = os:getenv("SCHEDULE_TRACKER_ENDPOINT", ""),
	UAHeader = {"User-Agent", os:getenv("SYSTEM", "")},
	SslOpts = [{verify, verify_peer}, {cacerts, public_key:cacerts_get()}],
	Result = httpc:request(get, {Url, [{"Accept", "application/json"}, UAHeader]},
	                       [{timeout, timer:seconds(5)}, {ssl, SslOpts}], []),
	handleJobsResult(Result, StatePid, SystemsMap).

% Successful 200 response: parse all jobs, group checks by system, fire one
% updateSystem cast per system so each source update is atomic.
handleJobsResult({ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}}, StatePid, SystemsMap) ->
	Jobs = jiffy:decode(Body, [return_maps]),
	BySystem = jobsToSystemChecks(Jobs),
	maps:foreach(fun(SystemStr, Checks) ->
		{Host, Type} = maps:get(SystemStr, SystemsMap, {SystemStr, system}),
		ok = gen_server:cast(StatePid, {updateSystem, Host, SystemStr, Type,
		                                scheduled_jobs, Checks, #{}})
	end, BySystem);
handleJobsResult({ok, {{_Version, StatusCode, ReasonPhrase}, _Headers, _Body}},
                 _StatePid, _SystemsMap) ->
	logger:warning("fetcher_scheduled_jobs: HTTP ~p ~p from schedule_tracker /jobs",
	               [StatusCode, ReasonPhrase]);
handleJobsResult({error, Error}, _StatePid, _SystemsMap) ->
	logger:warning("fetcher_scheduled_jobs: transport error contacting schedule_tracker: ~p",
	               [Error]).

% Groups a list of /jobs entries into #{system_str => #{check_key => check_map}}.
% All jobs belonging to the same system are merged into one map entry so that a
% single updateSystem cast can carry all scheduled-job checks for that system.
jobsToSystemChecks(Jobs) ->
	lists:foldl(fun(Job, Acc) ->
		{SystemStr, CheckKey, Check} = parseJobEntry(Job),
		OldChecks = maps:get(SystemStr, Acc, #{}),
		maps:put(SystemStr, maps:put(CheckKey, Check, OldChecks), Acc)
	end, #{}, Jobs).

% Extracts the system string, check key, and check map from a single /jobs entry.
% Named jobs use the job_name as the check key; unnamed jobs use "scheduled-job".
parseJobEntry(Job) ->
	SystemStr = binary_to_list(maps:get(<<"system">>, Job)),
	JobName   = maps:get(<<"job_name">>, Job, <<"">>),
	Check     = maps:get(<<"check">>,    Job, #{}),
	CheckKey  = case JobName of
		<<"">> -> <<"scheduled-job">>;
		_      -> JobName
	end,
	{SystemStr, CheckKey, Check}.

-ifdef(TEST).

	% ── parseJobEntry ────────────────────────────────────────────────────────

	parseJobEntry_named_job_uses_job_name_as_key_test() ->
		Job = #{
			<<"system">>   => <<"lucos_arachne">>,
			<<"job_name">> => <<"ingestor_dbpedia">>,
			<<"check">>    => #{<<"ok">> => true, <<"techDetail">> => <<"Checks...">>}
		},
		{SystemStr, CheckKey, Check} = parseJobEntry(Job),
		?assertEqual("lucos_arachne", SystemStr),
		?assertEqual(<<"ingestor_dbpedia">>, CheckKey),
		?assert(maps:get(<<"ok">>, Check)).

	parseJobEntry_empty_job_name_uses_scheduled_job_key_test() ->
		Job = #{
			<<"system">>   => <<"lucos_cron">>,
			<<"job_name">> => <<"">>,
			<<"check">>    => #{<<"ok">> => false, <<"debug">> => <<"2 consecutive errors">>}
		},
		{SystemStr, CheckKey, _Check} = parseJobEntry(Job),
		?assertEqual("lucos_cron", SystemStr),
		?assertEqual(<<"scheduled-job">>, CheckKey).

	parseJobEntry_absent_job_name_uses_scheduled_job_key_test() ->
		% job_name field absent entirely (e.g. future schema version)
		Job = #{<<"system">> => <<"lucos_x">>, <<"check">> => #{}},
		{_SystemStr, CheckKey, _Check} = parseJobEntry(Job),
		?assertEqual(<<"scheduled-job">>, CheckKey).

	% ── jobsToSystemChecks ───────────────────────────────────────────────────

	jobsToSystemChecks_empty_list_returns_empty_map_test() ->
		?assertEqual(#{}, jobsToSystemChecks([])).

	jobsToSystemChecks_single_named_job_test() ->
		Jobs = [#{<<"system">> => <<"lucos_arachne">>,
		          <<"job_name">> => <<"ingestor_dbpedia">>,
		          <<"check">> => #{<<"ok">> => true}}],
		Result = jobsToSystemChecks(Jobs),
		?assert(maps:is_key("lucos_arachne", Result)),
		Checks = maps:get("lucos_arachne", Result),
		?assert(maps:is_key(<<"ingestor_dbpedia">>, Checks)).

	jobsToSystemChecks_two_jobs_same_system_merged_into_one_entry_test() ->
		% Two jobs for the same system must be merged so we send only one
		% updateSystem cast — a second cast would overwrite the first's checks.
		Jobs = [
			#{<<"system">> => <<"lucos_arachne">>, <<"job_name">> => <<"job_a">>,
			  <<"check">> => #{<<"ok">> => true}},
			#{<<"system">> => <<"lucos_arachne">>, <<"job_name">> => <<"job_b">>,
			  <<"check">> => #{<<"ok">> => false}}
		],
		Result = jobsToSystemChecks(Jobs),
		?assertEqual(1, maps:size(Result)),
		Checks = maps:get("lucos_arachne", Result),
		?assert(maps:is_key(<<"job_a">>, Checks)),
		?assert(maps:is_key(<<"job_b">>, Checks)).

	jobsToSystemChecks_two_jobs_different_systems_test() ->
		Jobs = [
			#{<<"system">> => <<"lucos_arachne">>, <<"job_name">> => <<"job_a">>,
			  <<"check">> => #{<<"ok">> => true}},
			#{<<"system">> => <<"lucos_photos">>,  <<"job_name">> => <<"job_b">>,
			  <<"check">> => #{<<"ok">> => false}}
		],
		Result = jobsToSystemChecks(Jobs),
		?assertEqual(2, maps:size(Result)),
		?assert(maps:is_key("lucos_arachne", Result)),
		?assert(maps:is_key("lucos_photos", Result)).

	% ── buildSystemsMap ──────────────────────────────────────────────────────

	buildSystemsMap_with_domain_test() ->
		Body = "[{\"id\":\"lucos_arachne\",\"domain\":\"arachne.l42.eu\"}]",
		Map = buildSystemsMap(Body),
		?assertEqual({"arachne.l42.eu", system}, maps:get("lucos_arachne", Map)).

	buildSystemsMap_null_domain_uses_empty_host_test() ->
		% Components with no domain get an empty string host — they have no URL to check.
		Body = "[{\"id\":\"lucos_internal\",\"domain\":null}]",
		Map = buildSystemsMap(Body),
		?assertEqual({"", system}, maps:get("lucos_internal", Map)).

	buildSystemsMap_explicit_type_test() ->
		Body = "[{\"id\":\"lucos_comp\",\"domain\":null,\"type\":\"component\"}]",
		Map = buildSystemsMap(Body),
		?assertEqual({"", component}, maps:get("lucos_comp", Map)).

	buildSystemsMap_empty_list_test() ->
		?assertEqual(#{}, buildSystemsMap("[]")).

-endif.
