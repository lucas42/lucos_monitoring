-module(monitoring_state_server).
-behaviour(gen_server).
-export([start_link/0, start_link/1, init/1, handle_cast/2, handle_call/3]).

start_link() ->
	start_link([fun loganne:notify/5, fun email:notify/5]).

start_link(Notifiers) ->
	gen_server:start_link(?MODULE, Notifiers, []).

init(Notifiers) ->
	{ok, {#{}, #{}, Notifiers}}.

handle_cast(Request, {SystemMap, SuppressionMap, Notifiers}) ->
	case Request of
		{updateSystem, Host, System, SystemType, Source, SourceChecks, SystemMetrics} ->
			logger:info("Received update for system ~p (Host ~p, Source ~p)", [System, Host, Source]),
			IsFirstSeen = not maps:is_key(Host, SystemMap),
			{_, _, OldSourceChecksMap, OldNormalisedCache, OldMetrics} = maps:get(Host, SystemMap, {nil, system, #{}, #{}, #{}}),
			NewSourceChecksMap = maps:put(Source, SourceChecks, OldSourceChecksMap),
			NewMergedChecks = mergeSourceChecks(NewSourceChecksMap),
			% When a source explicitly reports no checks (e.g. circleci returns 404,
			% meaning no CI is configured for this repo), remove the keys it previously
			% contributed from the old normalised cache. This prevents mergeMissingInfoChecks
			% from resurrecting stale checks (e.g. circleci) when another source
			% (e.g. info) is temporarily unavailable.
			% We only prune on empty source updates: if a source reports any checks
			% (even unknown ones), we leave the old cache intact so that anti-flapiness
			% logic (mergeMissingInfoChecks) can still hold info checks steady during
			% a transient /_info blip.
			OldSourceChecks = maps:get(Source, OldSourceChecksMap, #{}),
			PrunedOldCache = case maps:size(SourceChecks) of
				0 -> maps:without(maps:keys(OldSourceChecks), OldNormalisedCache);
				_ -> OldNormalisedCache
			end,
			% Only count checks as "newly unknown" if they were included in the current
			% source update — not checks carried forward from other sources. This prevents
			% double-incrementing unknown_count when two sources (e.g. circleci + info)
			% update within the same monitoring cycle.
			CountableKeys = maps:fold(fun(Key, Check, Acc) ->
				case maps:get(<<"ok">>, Check, unknown) of
					unknown -> sets:add_element(Key, Acc);
					_ -> Acc
				end
			end, sets:new([{version, 2}]), SourceChecks),
			NormalisedChecks = normaliseChecks(PrunedOldCache, NewMergedChecks, CountableKeys),
			% Only overwrite metrics when the source provides them; sources
			% without metrics (e.g. circleci) pass #{} and should not wipe
			% metrics previously stored by the info fetcher.
			NewMetrics = case maps:size(SystemMetrics) of
				0 -> OldMetrics;
				_ -> SystemMetrics
			end,
			NewSuppressionMap = case IsFirstSeen of
				true ->
					logger:notice("Warm-up: skipping alert for ~p on first poll", [System]),
					SuppressionMap;
				false ->
					case maps:get(System, SuppressionMap, undefined) of
						{pending_verification, PendingSources} ->
							% Suppression was recently lifted. Defer the alert decision until
							% all sources have reported fresh post-deploy data.
							Remaining = sets:del_element(Source, PendingSources),
							case sets:size(Remaining) =:= 0 of
								true ->
									FailingNow = failingChecks(NormalisedChecks),
									case maps:size(FailingNow) > 0 of
										true ->
											logger:notice("Service ~p still unhealthy after deploy — alerting", [System]),
											notify_all(Host, System, FailingNow, false, NewMetrics, Notifiers);
										false ->
											logger:notice("Service ~p healthy after deploy", [System])
									end,
									maps:remove(System, SuppressionMap);
								false ->
									maps:put(System, {pending_verification, Remaining}, SuppressionMap)
							end;
						_ ->
							case meaningfulChange(OldNormalisedCache, NormalisedChecks) of
								true ->
									state_change(Host, System, NormalisedChecks, NewMetrics, SuppressionMap, Notifiers);
								false ->
									SuppressionMap
							end
					end
			end,
			AnnotatedChecks = annotateCheckStatuses(NormalisedChecks),
			NewSystemMap = maps:put(Host, {System, SystemType, NewSourceChecksMap, AnnotatedChecks, NewMetrics}, SystemMap),
			{noreply, {NewSystemMap, NewSuppressionMap, Notifiers}}
	end.

handle_call(Request, _From, {SystemMap, SuppressionMap, Notifiers}) ->
	case Request of
		{fetch, all} ->
			% Return a list of system maps with pre-computed status atoms.
			% Each system map includes <<"host">> (not in the ADR spec but needed by the
			% view layer to build /_info URLs) alongside id, type, name, checks, metrics, status.
			SystemList = maps:fold(fun(Host, {SystemId, SystemType, _SourceChecksMap, NormalisedCache, Metrics}, Acc) ->
				[buildSystemOutput(Host, SystemId, SystemType, NormalisedCache, Metrics, SuppressionMap) | Acc]
			end, [], SystemMap),
			{reply, SystemList, {SystemMap, SuppressionMap, Notifiers}};
		{suppress, System} ->
			case systemExists(System, SystemMap) of
				true ->
					ExpiryTime = erlang:system_time(second) + 600,
					NewSuppressionMap = maps:put(System, ExpiryTime, SuppressionMap),
					logger:notice("Suppression window opened for ~p", [System]),
					{reply, ok, {SystemMap, NewSuppressionMap, Notifiers}};
				false ->
					{reply, {error, not_found}, {SystemMap, SuppressionMap, Notifiers}}
			end;
		{unsuppress, System} ->
			% Instead of alerting immediately on stale pre-unsuppress health data,
			% enter pending_verification so the alert decision is deferred until all
			% sources have reported fresh post-deploy results.
			case maps:is_key(System, SuppressionMap) of
				true ->
					Sources = collect_active_sources(System, SystemMap),
					NewSuppressionMap = maps:put(System, {pending_verification, Sources}, SuppressionMap),
					logger:notice("Suppression window closed for ~p — awaiting verification poll", [System]),
					% Cascade pending_verification to systems that have checks depending on this system.
					% Single-hop only: we do not follow dependsOn chains transitively.
					DependentSystems = find_dependent_systems(System, SystemMap),
					FinalSuppressionMap = lists:foldl(fun(DepSystem, SM) ->
						DepSources = collect_active_sources(DepSystem, SystemMap),
						logger:notice("Cascading pending_verification to ~p (has checks depending on ~p)", [DepSystem, System]),
						maps:put(DepSystem, {pending_verification, DepSources}, SM)
					end, NewSuppressionMap, DependentSystems),
					{reply, ok, {SystemMap, FinalSuppressionMap, Notifiers}};
				false ->
					logger:notice("Suppression window closed for ~p (was not suppressed)", [System]),
					{reply, ok, {SystemMap, SuppressionMap, Notifiers}}
			end
	end.

% Merges checks from all sources into a single flat map.
% The current sources (info, circleci) write disjoint check key sets,
% so fold ordering doesn't matter in practice. If a future source shares
% keys with an existing one, override order will be non-deterministic
% (map fold order over atom keys is unspecified in Erlang) — revisit then.
mergeSourceChecks(SourceChecksMap) ->
	maps:fold(fun(_, Checks, Acc) ->
		maps:merge(Acc, Checks)
	end, #{}, SourceChecksMap).

systemExists(System, SystemMap) ->
	maps:fold(fun
		(_, {S, _, _, _, _}, _) when S =:= System -> true;
		(_, _, Acc) -> Acc
	end, false, SystemMap).

% Returns the set of source keys that have reported for a given system,
% across all hosts. Used to build the PendingSources set on unsuppress.
collect_active_sources(System, SystemMap) ->
	maps:fold(fun
		(_, {S, _, SourceChecksMap, _, _}, Acc) when S =:= System ->
			sets:union(Acc, sets:from_list(maps:keys(SourceChecksMap)));
		(_, _, Acc) -> Acc
	end, sets:new([{version, 2}]), SystemMap).

% Replaces any unknown "ok" with whichever the value was there previously, until 3 consecutive unknowns are recieved at which point ok is set to false.
% CountableKeys is the set of check keys that should be considered for incrementing.
% Only keys in this set increment their unknown_count — this prevents double-counting when
% one source's unknown checks are carried forward in the merged view and processed again
% during another source's update.
replaceUnknowns(OldChecks, NewChecks, Iterator, CountableKeys) ->
	case maps:next(Iterator) of
		{Key, NewCheck, NextIterator} ->
			NormalisedCheck = case maps:get(<<"ok">>, NewCheck, unknown) of
				unknown ->
					OldCheck = maps:get(Key, OldChecks, #{<<"ok">> => unknown}),
					OldCount = maps:get(<<"unknown_count">>, OldCheck, 0),
					NewCount = case sets:is_element(Key, CountableKeys) of
						true -> OldCount + 1;  % This check was in the current source update
						false -> OldCount  % Carried over from a previous source, don't re-increment
					end,
					IncrementedCheck = maps:put(<<"unknown_count">>, NewCount, NewCheck),
					case NewCount >= 3 of
						true ->
							maps:put(<<"ok">>, false, IncrementedCheck);
						false ->
							logger:notice("Not sending alert for ~p as there has only been ~p recurring failures so far.", [Key, NewCount]),
							maps:put(<<"ok">>, maps:get(<<"ok">>, OldCheck, unknown), IncrementedCheck)
					end;
				_ ->
					maps:put(<<"unknown_count">>, 0, NewCheck)
			end,
			maps:put(Key, NormalisedCheck, replaceUnknowns(OldChecks, NewChecks, NextIterator, CountableKeys));
		none ->
			maps:new()
	end.

% If there's a problem with the 'fetch-info' check, merge the old and new checks to avoid flapiness
mergeMissingInfoChecks(OldChecks, NewChecks) ->
	case maps:get(<<"ok">>, maps:get(<<"fetch-info">>, NewChecks, #{<<"ok">> => unknown}), unknown) of
		true ->
			NewChecks;
		_ ->
			maps:merge(OldChecks, NewChecks)
	end.

% Applies failThreshold logic: holds the previous ok state until N consecutive
% failures are seen. The fail_count counter resets to 0 on recovery.
% failThreshold defaults to 1 (alert immediately on first failure).
applyFailThreshold(OldChecks, NewChecks) ->
	maps:map(fun(Key, NewCheck) ->
		case maps:get(<<"ok">>, NewCheck, unknown) of
			false ->
				FailThreshold = maps:get(<<"failThreshold">>, NewCheck, 1),
				OldCheck = maps:get(Key, OldChecks, #{}),
				OldFailCount = maps:get(<<"fail_count">>, OldCheck, 0),
				NewFailCount = OldFailCount + 1,
				CheckWithCount = maps:put(<<"fail_count">>, NewFailCount, NewCheck),
				case NewFailCount < FailThreshold of
					true ->
						% Not yet at threshold — hold the previous ok value
						OldOk = maps:get(<<"ok">>, OldCheck, unknown),
						maps:put(<<"ok">>, OldOk, CheckWithCount);
					false ->
						CheckWithCount
				end;
			_ ->
				% Healthy or unknown — reset the failure counter
				maps:put(<<"fail_count">>, 0, NewCheck)
		end
	end, NewChecks).

% Reduce monitoring flapiness by using existing check data to bolster new checks which may be facing a temporary blip.
% CountableKeys is the set of check keys from the current source update that are reporting unknown.
% Only those keys can increment their unknown_count — this prevents double-counting when checks
% from one source are carried forward in the merged view during another source's update.
normaliseChecks(OldChecks, NewChecks, CountableKeys) ->
	MergedChecks = mergeMissingInfoChecks(OldChecks, NewChecks),
	AfterUnknowns = replaceUnknowns(OldChecks, MergedChecks, maps:iterator(MergedChecks, reversed), CountableKeys),
	applyFailThreshold(OldChecks, AfterUnknowns).

% Decides whether the checks have changed in a meaningful way (ie ignore "unknown" states)
meaningfulChange(OldChecks, NewChecks) ->
	NewFailingChecks = failingChecks(NewChecks),
	OldFailingChecks = failingChecks(OldChecks),
	maps:keys(OldFailingChecks) /= maps:keys(NewFailingChecks).

failingChecks(Checks) ->
	maps:filter(fun(_, Check) ->
		maps:get(<<"ok">>, Check, unknown) == false
	end, Checks).

% Calls every notifier in Notifiers, catching any errors so a single
% failing notifier cannot prevent the others from running.
notify_all(Host, System, FailingNow, Suppressed, Metrics, Notifiers) ->
	lists:foreach(fun(NotifyFn) ->
		try NotifyFn(Host, System, FailingNow, Suppressed, Metrics)
		catch ExClass:ExReason ->
			logger:error("Notify failed for ~p on ~p: ~p ~p", [System, Host, ExClass, ExReason])
		end
	end, Notifiers).

state_change(Host, System, SystemChecks, SystemMetrics, SuppressionMap, Notifiers) ->
	AllFailing = failingChecks(SystemChecks),
	% Filter out checks whose dependsOn system is currently under an active suppression window.
	% Single-hop only: we never follow dependsOn chains transitively.
	FailingNow = maps:filter(fun(_, Check) ->
		not is_dependency_suppressed(Check, System, SuppressionMap)
	end, AllFailing),
	case maps:size(AllFailing) > 0 andalso maps:size(FailingNow) =:= 0 of
		true ->
			% All failing checks are dependency-suppressed — notify as suppressed (no email alert).
			logger:notice("All failing checks on ~p suppressed via dependency: ~p", [System, maps:keys(AllFailing)]),
			notify_all(Host, System, AllFailing, true, SystemMetrics, Notifiers),
			SuppressionMap;
		false ->
			% FailingNow contains only non-dep-suppressed checks. Apply system-level suppression logic.
			case maps:get(System, SuppressionMap, undefined) of
				undefined ->
					logger:notice("Checks' state changed for ~p on ~p", [System, Host]),
					notify_all(Host, System, FailingNow, false, SystemMetrics, Notifiers),
					SuppressionMap;
				ExpiryTime ->
					Now = erlang:system_time(second),
					case Now < ExpiryTime of
						true ->
							logger:notice("Alert suppressed for ~p during deploy window", [System]),
							notify_all(Host, System, FailingNow, true, SystemMetrics, Notifiers),
							SuppressionMap;
						false ->
							logger:error("Suppression window for ~p expired without being cleared - deploy may have taken longer than 10 minutes", [System]),
							logger:notice("Checks' state changed for ~p on ~p", [System, Host]),
							notify_all(Host, System, FailingNow, false, SystemMetrics, Notifiers),
							maps:remove(System, SuppressionMap)
					end
			end
	end.

% Returns true if this check's dependsOn system is currently under an active suppression window.
% Guards against self-references (CurrentSystem == dependsOn) and treats pending_verification
% as "suppression lifted" (returns false). Does NOT follow transitive dependsOn chains.
is_dependency_suppressed(Check, CurrentSystem, SuppressionMap) ->
	case maps:get(<<"dependsOn">>, Check, undefined) of
		undefined -> false;
		DependsOn when is_binary(DependsOn) ->
			DependsOnStr = binary_to_list(DependsOn),
			% Guard: ignore self-references to prevent circular evaluation
			case DependsOnStr =:= CurrentSystem of
				true -> false;
				false ->
					case maps:get(DependsOnStr, SuppressionMap, undefined) of
						undefined -> false;
						{pending_verification, _} -> false;  % Suppression has been lifted
						ExpiryTime ->
							Now = erlang:system_time(second),
							Now < ExpiryTime
					end
			end;
		_ -> false  % Non-binary dependsOn value — ignore
	end.

% Returns a list of system IDs whose normalised checks include a dependsOn pointing at TargetSystem.
% Used to cascade pending_verification when TargetSystem unsuppresses.
% Excludes TargetSystem itself (self-reference guard).
find_dependent_systems(TargetSystem, SystemMap) ->
	TargetBin = list_to_binary(TargetSystem),
	lists:usort(maps:fold(fun(_Host, {System, _SystemType, SourceChecksMap, _, _}, Acc) ->
		case System =:= TargetSystem of
			true -> Acc;  % Guard: skip self
			false ->
				MergedChecks = mergeSourceChecks(SourceChecksMap),
				HasDependency = maps:fold(fun(_, Check, Found) ->
					Found orelse (maps:get(<<"dependsOn">>, Check, undefined) =:= TargetBin)
				end, false, MergedChecks),
				case HasDependency of
					true -> [System | Acc];
					false -> Acc
				end
		end
	end, [], SystemMap)).

% Computes the status atom for a single normalised check.
% Mapping (per ADR-0001):
%   ok=false                                              → failing
%   ok=true|unknown with unknown_count>0 or fail buffering → buffering
%   ok=unknown, no counters                               → unknown
%   ok=true, no counters                                  → healthy
computeCheckStatus(Check) ->
	Ok = maps:get(<<"ok">>, Check, unknown),
	case Ok of
		false -> failing;
		_ ->
			UnknownCount = maps:get(<<"unknown_count">>, Check, 0),
			FailCount = maps:get(<<"fail_count">>, Check, 0),
			FailThreshold = maps:get(<<"failThreshold">>, Check, 1),
			IsBuffering = (UnknownCount > 0) orelse (FailCount > 0 andalso FailThreshold > 1),
			case IsBuffering of
				true -> buffering;
				false ->
					case Ok of
						true -> healthy;
						_ -> unknown
					end
			end
	end.

% Computes the human-readable status text string for a single check.
computeCheckStatusText(Status, Check) ->
	case Status of
		buffering ->
			UnknownCount = maps:get(<<"unknown_count">>, Check, 0),
			case UnknownCount > 0 of
				true ->
					list_to_binary("unknown (" ++ integer_to_list(UnknownCount) ++ ")");
				false ->
					FailCount = maps:get(<<"fail_count">>, Check, 0),
					FailThreshold = maps:get(<<"failThreshold">>, Check, 1),
					list_to_binary("failing (" ++ integer_to_list(FailCount) ++ "/" ++ integer_to_list(FailThreshold) ++ ")")
			end;
		Other ->
			atom_to_binary(Other, utf8)
	end.

% Adds <<"status">> and <<"statusText">> fields to every check in the normalised cache.
% Called at write-time (after normaliseChecks) so {fetch, all} reads pre-computed values.
annotateCheckStatuses(NormalisedCache) ->
	maps:map(fun(_CheckId, Check) ->
		Status = computeCheckStatus(Check),
		StatusText = computeCheckStatusText(Status, Check),
		maps:merge(Check, #{<<"status">> => Status, <<"statusText">> => StatusText})
	end, NormalisedCache).

% Computes the system-level status atom by walking the priority list from ADR-0001.
% Per-check status must already be annotated (<<"status">> present in each check map).
computeSystemStatus(SystemId, NormalisedCache, SuppressionMap) ->
	Now = erlang:system_time(second),
	case maps:get(SystemId, SuppressionMap, undefined) of
		{pending_verification, _} ->
			pending_verification;
		ExpiryTime when is_integer(ExpiryTime), ExpiryTime > Now ->
			suppressed;
		_ ->
			aggregateCheckStatuses(SystemId, NormalisedCache, SuppressionMap)
	end.

% Aggregates check statuses into a system status (priority list steps 3–7 from ADR-0001).
% Failing checks whose dependsOn system is actively suppressed are excluded from step 3.
% A system with no checks is unknown (absence of a signal is not a positive signal).
aggregateCheckStatuses(SystemId, NormalisedCache, SuppressionMap) ->
	case maps:size(NormalisedCache) of
		0 -> unknown;
		_ ->
			{HasFailing, HasUnknown, HasBuffering} = maps:fold(fun(_, Check, {F, U, B}) ->
				Status = maps:get(<<"status">>, Check, unknown),
				IsDepSuppressed = is_dependency_suppressed(Check, SystemId, SuppressionMap),
				case {Status, IsDepSuppressed} of
					{failing, true}  -> {F, U, B};   % dep-suppressed failing: exclude from system failing
					{failing, false} -> {true, U, B};
					{unknown, _}     -> {F, true, B};
					{buffering, _}   -> {F, U, true};
					_                -> {F, U, B}
				end
			end, {false, false, false}, NormalisedCache),
			if
				HasFailing   -> failing;
				HasUnknown   -> unknown;
				HasBuffering -> buffering;
				true         -> healthy
			end
	end.

% Builds the system output map returned by {fetch, all}.
% Includes <<"host">> for view-layer URL construction (not in ADR spec but required for rendering).
buildSystemOutput(Host, SystemId, SystemType, NormalisedCache, Metrics, SuppressionMap) ->
	Checks = [buildCheckOutput(CheckId, Check) || {CheckId, Check} <- maps:to_list(NormalisedCache)],
	MetricsList = [buildMetricOutput(MetricId, Metric) || {MetricId, Metric} <- maps:to_list(Metrics)],
	SystemStatus = computeSystemStatus(SystemId, NormalisedCache, SuppressionMap),
	#{
		<<"id">>      => list_to_binary(SystemId),
		<<"type">>    => SystemType,
		<<"name">>    => list_to_binary(SystemId),
		<<"host">>    => list_to_binary(Host),
		<<"checks">>  => Checks,
		<<"metrics">> => MetricsList,
		<<"status">>  => SystemStatus
	}.

% Builds the check output map. Includes optional string fields (techDetail, debug, link)
% only when present and non-empty; omits internal counter fields.
buildCheckOutput(CheckId, Check) ->
	Status = maps:get(<<"status">>, Check, unknown),
	StatusText = maps:get(<<"statusText">>, Check, <<"unknown">>),
	Base = #{
		<<"id">>         => CheckId,
		<<"status">>     => Status,
		<<"statusText">> => StatusText
	},
	lists:foldl(fun(Key, Acc) ->
		case maps:get(Key, Check, <<>>) of
			<<>> -> Acc;
			Val when is_binary(Val) -> maps:put(Key, Val, Acc);
			_ -> Acc
		end
	end, Base, [<<"techDetail">>, <<"debug">>, <<"link">>]).

% Builds the metric output map.
buildMetricOutput(MetricId, Metric) ->
	#{
		<<"id">>         => MetricId,
		<<"techDetail">> => maps:get(<<"techDetail">>, Metric, <<"">>),
		<<"value">>      => maps:get(<<"value">>, Metric, 0)
	}.

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").
	nomaliseChecks_test() ->
		CiCountable = sets:from_list([<<"ci">>], [{version, 2}]),
		FetchInfoCountable = sets:from_list([<<"fetch-info">>], [{version, 2}]),
		% Single unknown check: ci goes from ok to unknown (count 0→1, ok held as true)
		?assertEqual(#{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 1, <<"fail_count">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 0}}, normaliseChecks(#{<<"ci">> => #{<<"ok">> => true}}, #{<<"ci">> => #{<<"ok">> => unknown}, <<"fetch-info">> => #{<<"ok">> => true}}, CiCountable)),
		% Second consecutive unknown for ci (count 1→2, ok still held as true)
		?assertEqual(#{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 2, <<"fail_count">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 0}}, normaliseChecks(#{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 1}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 2}}, #{<<"ci">> => #{<<"ok">> => unknown}, <<"fetch-info">> => #{<<"ok">> => true}}, CiCountable)),
		% Third consecutive unknown for ci (count 2→3, ok flips to false and alerts)
		?assertEqual(#{<<"ci">> => #{<<"ok">> => false, <<"unknown_count">> => 3, <<"fail_count">> => 1}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 0}}, normaliseChecks(#{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 2}}, #{<<"ci">> => #{<<"ok">> => unknown}, <<"fetch-info">> => #{<<"ok">> => true}}, CiCountable)),
		% fetch-info goes unknown while other checks are carried forward from old state
		?assertEqual(#{<<"item-count">> => #{<<"ok">> => false, <<"unknown_count">> => 0, <<"fail_count">> => 1}, <<"api-check">> => #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 0}, <<"fetch-info">> => #{<<"ok">> => true,  <<"unknown_count">> => 1, <<"fail_count">> => 0}}, normaliseChecks(#{<<"item-count">> => #{<<"ok">> => false}, <<"api-check">> => #{<<"ok">> => true}, <<"fetch-info">> => #{<<"ok">> => true}}, #{<<"fetch-info">> => #{<<"ok">> => unknown}}, FetchInfoCountable)).

	meaningfulChange_test() ->
		?assertEqual(false, meaningfulChange(#{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 1}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0}}, #{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0}})),
		?assertEqual(true, meaningfulChange(#{<<"ci">> => #{<<"ok">> => false, <<"unknown_count">> => 3}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0}}, #{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 2}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0}})),
		?assertEqual(true, meaningfulChange(#{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 1}, <<"fetch-info">> => #{<<"ok">> => false, <<"unknown_count">> => 0}}, #{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0}})),
		?assertEqual(true, meaningfulChange(#{<<"ci">> => #{<<"ok">> => true, <<"unknown_count">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0}}, #{<<"ci">> => #{<<"ok">> => false, <<"unknown_count">> => 0}, <<"fetch-info">> => #{<<"ok">> => false, <<"unknown_count">> => 4}})).

	systemExists_test() ->
		SystemMap = #{
			"host1.example.com" => {"lucos_foo", system, #{}, #{}, #{}},
			"host2.example.com" => {"lucos_bar", host, #{}, #{}, #{}}
		},
		?assertEqual(true, systemExists("lucos_foo", SystemMap)),
		?assertEqual(true, systemExists("lucos_bar", SystemMap)),
		?assertEqual(false, systemExists("lucos_missing", SystemMap)),
		?assertEqual(false, systemExists("lucos_foo", #{})).

	mergeSourceChecks_test() ->
		% Two disjoint sources produce a flat merged map
		SourceChecksMap = #{
			info => #{<<"fetch-info">> => #{<<"ok">> => true}, <<"tls-certificate">> => #{<<"ok">> => true}},
			circleci => #{<<"circleci">> => #{<<"ok">> => false}}
		},
		Result = mergeSourceChecks(SourceChecksMap),
		?assertEqual(#{
			<<"fetch-info">> => #{<<"ok">> => true},
			<<"tls-certificate">> => #{<<"ok">> => true},
			<<"circleci">> => #{<<"ok">> => false}
		}, Result).

	mergeSourceChecks_empty_test() ->
		?assertEqual(#{}, mergeSourceChecks(#{})).

	% First update for a host stores its state but doesn't alert (warm-up grace period).
	warmup_first_update_stores_state_test() ->
		InitialState = {#{}, #{}, []},
		Checks = #{<<"fetch-info">> => #{<<"ok">> => false}},
		{noreply, {SystemMap, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, Checks, #{}},
			InitialState
		),
		% Host should now be in the SystemMap
		?assert(maps:is_key("host1.example.com", SystemMap)),
		{"lucos_foo", _Type, SourceChecksMap, _, _} = maps:get("host1.example.com", SystemMap),
		StoredChecks = mergeSourceChecks(SourceChecksMap),
		?assertEqual(false, maps:get(<<"ok">>, maps:get(<<"fetch-info">>, StoredChecks))).

	% Second update for a known host triggers normal alert logic (not warm-up).
	% Here both updates report the same healthy state, so no meaningful change — no alert.
	warmup_second_update_not_suppressed_test() ->
		Checks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		ExistingState = {
			#{"host1.example.com" => {"lucos_foo", system, #{info => Checks}, #{}, #{}}},
			#{},
			[]
		},
		{noreply, {SystemMap, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, Checks, #{}},
			ExistingState
		),
		% Host is still in the map after second update
		?assert(maps:is_key("host1.example.com", SystemMap)).

	% circleci update for a host doesn't clobber info checks, and vice versa.
	two_sources_dont_clobber_each_other_test() ->
		InfoChecks = #{<<"fetch-info">> => #{<<"ok">> => true}, <<"tls-certificate">> => #{<<"ok">> => true}},
		CIChecks = #{<<"circleci">> => #{<<"ok">> => false}},
		% Start with info checks already stored
		ExistingState = {
			#{"host1.example.com" => {"lucos_foo", system, #{info => InfoChecks}, #{}, #{}}},
			#{},
			[]
		},
		% circleci update arrives
		{noreply, {SystemMap, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, circleci, CIChecks, #{}},
			ExistingState
		),
		{"lucos_foo", _Type, SourceChecksMap, _, _} = maps:get("host1.example.com", SystemMap),
		Merged = mergeSourceChecks(SourceChecksMap),
		% All three checks must be present
		?assert(maps:is_key(<<"fetch-info">>, Merged)),
		?assert(maps:is_key(<<"tls-certificate">>, Merged)),
		?assert(maps:is_key(<<"circleci">>, Merged)),
		?assertEqual(false, maps:get(<<"ok">>, maps:get(<<"circleci">>, Merged))),
		?assertEqual(true, maps:get(<<"ok">>, maps:get(<<"fetch-info">>, Merged))).

	% When info source updates and drops a check key, it's removed from the merged view.
	dropped_info_check_is_removed_test() ->
		OldInfoChecks = #{<<"fetch-info">> => #{<<"ok">> => true}, <<"custom-check">> => #{<<"ok">> => true}},
		NewInfoChecks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		ExistingState = {
			#{"host1.example.com" => {"lucos_foo", system, #{info => OldInfoChecks}, #{}, #{}}},
			#{},
			[]
		},
		{noreply, {SystemMap, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, NewInfoChecks, #{}},
			ExistingState
		),
		{"lucos_foo", _Type, SourceChecksMap, _, _} = maps:get("host1.example.com", SystemMap),
		Merged = mergeSourceChecks(SourceChecksMap),
		?assertNot(maps:is_key(<<"custom-check">>, Merged)).

	% When a source with no metrics (e.g. circleci) updates a host that already
	% has metrics from the info fetcher, the existing metrics are preserved.
	empty_metrics_do_not_overwrite_test() ->
		InfoChecks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		Metrics = #{<<"agent-count">> => #{<<"value">> => 42, <<"techDetail">> => <<"count">>}},
		ExistingState = {
			#{"host1.example.com" => {"lucos_foo", system, #{info => InfoChecks}, #{}, Metrics}},
			#{},
			[]
		},
		CIChecks = #{<<"circleci">> => #{<<"ok">> => true}},
		{noreply, {SystemMap, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, circleci, CIChecks, #{}},
			ExistingState
		),
		{"lucos_foo", _, _, _, StoredMetrics} = maps:get("host1.example.com", SystemMap),
		?assertEqual(Metrics, StoredMetrics).

	% When a source provides non-empty metrics, they replace the existing ones.
	nonempty_metrics_do_overwrite_test() ->
		OldMetrics = #{<<"agent-count">> => #{<<"value">> => 42, <<"techDetail">> => <<"count">>}},
		NewMetrics = #{<<"agent-count">> => #{<<"value">> => 99, <<"techDetail">> => <<"count">>}},
		ExistingState = {
			#{"host1.example.com" => {"lucos_foo", system, #{info => #{<<"fetch-info">> => #{<<"ok">> => true}}}, #{}, OldMetrics}},
			#{},
			[]
		},
		{noreply, {SystemMap, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, #{<<"fetch-info">> => #{<<"ok">> => true}}, NewMetrics},
			ExistingState
		),
		{"lucos_foo", _, _, _, StoredMetrics} = maps:get("host1.example.com", SystemMap),
		?assertEqual(NewMetrics, StoredMetrics).

	% Helper to build a recording notifier and retrieve what it captured.
	% The notifier sends {notified, Args} to the calling test process.
	recording_notifier(TestPid) ->
		fun(Host, System, FailingNow, Suppressed, Metrics) ->
			TestPid ! {notified, Host, System, FailingNow, Suppressed, Metrics}
		end.

	% Drain all pending {notified, ...} messages from the mailbox.
	drain_notifications() ->
		receive
			{notified, _, _, _, _, _} -> drain_notifications()
		after 0 ->
			ok
		end.

	% Unsuppressing a healthy system enters pending_verification and does NOT fire an immediate alert.
	unsuppress_healthy_system_test() ->
		HealthyChecks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		SystemMap = #{"host1.example.com" => {"lucos_foo", system, #{info => HealthyChecks}, #{}, #{}}},
		Notifier = recording_notifier(self()),
		State = {SystemMap, #{"lucos_foo" => erlang:system_time(second) + 600}, [Notifier]},
		{reply, ok, {_, NewSuppressionMap, _}} = handle_call(
			{unsuppress, "lucos_foo"}, from, State
		),
		% System should be in pending_verification, not removed from the map
		?assertMatch({pending_verification, _}, maps:get("lucos_foo", NewSuppressionMap)),
		receive
			{notified, _, _, _, _, _} -> ?assert(false, "Unexpected alert fired for healthy system")
		after 100 ->
			ok  % No immediate notification — correct
		end.

	% Unsuppressing an unhealthy system enters pending_verification; alert is deferred, not immediate.
	unsuppress_unhealthy_system_test() ->
		FailingChecks = #{<<"fetch-info">> => #{<<"ok">> => false}},
		SystemMap = #{"host1.example.com" => {"lucos_foo", system, #{info => FailingChecks}, #{}, #{}}},
		Notifier = recording_notifier(self()),
		State = {SystemMap, #{"lucos_foo" => erlang:system_time(second) + 600}, [Notifier]},
		drain_notifications(),
		{reply, ok, {_, NewSuppressionMap, _}} = handle_call(
			{unsuppress, "lucos_foo"}, from, State
		),
		% System should be in pending_verification (not cleared)
		?assertMatch({pending_verification, _}, maps:get("lucos_foo", NewSuppressionMap)),
		receive
			{notified, _, _, _, _, _} -> ?assert(false, "Alert must not fire immediately on unsuppress")
		after 100 ->
			ok  % No immediate notification — correct
		end.

	% Unsuppressing a system that isn't in the map is a no-op (idempotent).
	unsuppress_unknown_system_is_noop_test() ->
		State = {#{}, #{}, []},
		{reply, ok, {_, NewSuppressionMap, _}} = handle_call(
			{unsuppress, "lucos_unknown"}, from, State
		),
		?assertEqual(#{}, NewSuppressionMap).

	% After unsuppress, a fresh poll reporting unhealthy fires an alert and clears pending state.
	pending_verification_fires_alert_when_unhealthy_test() ->
		FailingChecks = #{<<"fetch-info">> => #{<<"ok">> => false}},
		SystemMap = #{"host1.example.com" => {"lucos_foo", system, #{info => FailingChecks}, #{}, #{}}},
		PendingSources = sets:from_list([info], [{version, 2}]),
		Notifier = recording_notifier(self()),
		drain_notifications(),
		State = {SystemMap, #{"lucos_foo" => {pending_verification, PendingSources}}, [Notifier]},
		{noreply, {_, NewSuppressionMap, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, FailingChecks, #{}},
			State
		),
		% Suppression entry should be cleared
		?assertEqual(#{}, NewSuppressionMap),
		receive
			{notified, "host1.example.com", "lucos_foo", FailingNow, false, _Metrics} ->
				?assert(maps:is_key(<<"fetch-info">>, FailingNow))
		after 100 ->
			?assert(false, "Expected alert was not fired after verification poll")
		end.

	% After unsuppress, a fresh poll reporting healthy clears pending state without alerting.
	pending_verification_no_alert_when_healthy_test() ->
		HealthyChecks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		SystemMap = #{"host1.example.com" => {"lucos_foo", system, #{info => HealthyChecks}, #{}, #{}}},
		PendingSources = sets:from_list([info], [{version, 2}]),
		Notifier = recording_notifier(self()),
		drain_notifications(),
		State = {SystemMap, #{"lucos_foo" => {pending_verification, PendingSources}}, [Notifier]},
		{noreply, {_, NewSuppressionMap, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, HealthyChecks, #{}},
			State
		),
		% Suppression entry should be cleared
		?assertEqual(#{}, NewSuppressionMap),
		receive
			{notified, _, _, _, _, _} -> ?assert(false, "No alert expected for healthy service after verify")
		after 100 ->
			ok
		end.

	% With two sources pending, the first poll keeps pending state; the second evaluates.
	pending_verification_waits_for_all_sources_test() ->
		FailingChecks = #{<<"fetch-info">> => #{<<"ok">> => false}},
		CIChecks = #{<<"circleci">> => #{<<"ok">> => false}},
		SystemMap = #{
			"host1.example.com" => {"lucos_foo", system, #{info => FailingChecks, circleci => CIChecks}, #{}, #{}}
		},
		PendingSources = sets:from_list([info, circleci], [{version, 2}]),
		Notifier = recording_notifier(self()),
		drain_notifications(),
		State = {SystemMap, #{"lucos_foo" => {pending_verification, PendingSources}}, [Notifier]},
		% First source (info) reports — should still be pending
		{noreply, State2} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, FailingChecks, #{}},
			State
		),
		{_, SuppressionMap2, _} = State2,
		?assertMatch({pending_verification, _}, maps:get("lucos_foo", SuppressionMap2)),
		receive
			{notified, _, _, _, _, _} -> ?assert(false, "No alert expected after first source only")
		after 100 ->
			ok
		end,
		% Second source (circleci) reports — should evaluate and alert
		{noreply, {_, SuppressionMap3, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, circleci, CIChecks, #{}},
			State2
		),
		?assertEqual(#{}, SuppressionMap3),
		receive
			{notified, "host1.example.com", "lucos_foo", _, false, _} -> ok
		after 100 ->
			?assert(false, "Expected alert after all sources reported")
		end.

	% With default failThreshold (1), a single failure is reported immediately.
	failThreshold_default_alerts_immediately_test() ->
		OldChecks = #{<<"db-check">> => #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 0}},
		NewChecks = #{<<"db-check">> => #{<<"ok">> => false}},
		% db-check is false (not unknown), so CountableKeys is empty
		Result = normaliseChecks(OldChecks, NewChecks, sets:new([{version, 2}])),
		?assertEqual(false, maps:get(<<"ok">>, maps:get(<<"db-check">>, Result))),
		?assertEqual(1, maps:get(<<"fail_count">>, maps:get(<<"db-check">>, Result))).

	% With failThreshold 3, failures 1 and 2 hold the previous ok state.
	% On the third consecutive failure, ok flips to false.
	failThreshold_holds_until_threshold_test() ->
		NewChecks = #{<<"db-check">> => #{<<"ok">> => false, <<"failThreshold">> => 3}},
		% db-check is false (not unknown), so CountableKeys is empty
		EmptyCountable = sets:new([{version, 2}]),
		% First failure — fail_count goes to 1, ok stays true (held from old)
		OldChecks1 = #{<<"db-check">> => #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 0}},
		Result1 = normaliseChecks(OldChecks1, NewChecks, EmptyCountable),
		?assertEqual(true, maps:get(<<"ok">>, maps:get(<<"db-check">>, Result1))),
		?assertEqual(1, maps:get(<<"fail_count">>, maps:get(<<"db-check">>, Result1))),
		% Second failure — fail_count goes to 2, still held
		Result2 = normaliseChecks(Result1, NewChecks, EmptyCountable),
		?assertEqual(true, maps:get(<<"ok">>, maps:get(<<"db-check">>, Result2))),
		?assertEqual(2, maps:get(<<"fail_count">>, maps:get(<<"db-check">>, Result2))),
		% Third failure — fail_count goes to 3, now ok flips to false
		Result3 = normaliseChecks(Result2, NewChecks, EmptyCountable),
		?assertEqual(false, maps:get(<<"ok">>, maps:get(<<"db-check">>, Result3))),
		?assertEqual(3, maps:get(<<"fail_count">>, maps:get(<<"db-check">>, Result3))).

	% Recovery (ok: true) resets fail_count to 0.
	failThreshold_recovery_resets_count_test() ->
		OldChecks = #{<<"db-check">> => #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 2, <<"failThreshold">> => 3}},
		NewChecks = #{<<"db-check">> => #{<<"ok">> => true, <<"failThreshold">> => 3}},
		% db-check is true (not unknown), so CountableKeys is empty
		Result = normaliseChecks(OldChecks, NewChecks, sets:new([{version, 2}])),
		?assertEqual(true, maps:get(<<"ok">>, maps:get(<<"db-check">>, Result))),
		?assertEqual(0, maps:get(<<"fail_count">>, maps:get(<<"db-check">>, Result))).

	% Bug fix: when circleci returns 404 (no CI for this repo), the circleci check must be
	% removed from the normalised cache — even when info is already unavailable (fetch-info = unknown).
	%
	% The ordering — info blip first, THEN circleci 404 — is what makes this test non-trivial.
	% When fetch-info is unknown, mergeMissingInfoChecks merges old checks on top of the incoming
	% ones. Without the "prune-on-empty-source" logic, the 404's empty map would be treated as
	% "no update this cycle", and the merge would silently resurrect the stale circleci check
	% from the old normalised cache. This test regresses that specific scenario.
	circleci_404_wipes_check_even_when_info_unavailable_test() ->
		InfoChecks = #{<<"fetch-info">> => #{<<"ok">> => true}, <<"tls-certificate">> => #{<<"ok">> => true}},
		CIChecks = #{<<"circleci">> => #{<<"ok">> => true}},
		% Start with a system already known and healthy, with both sources reporting
		ExistingNormalisedCache = #{
			<<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 0},
			<<"tls-certificate">> => #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 0},
			<<"circleci">> => #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 0}
		},
		ExistingState = {
			#{"host1.example.com" => {"lucos_foo", system, #{info => InfoChecks, circleci => CIChecks}, ExistingNormalisedCache, #{}}},
			#{},
			[]
		},
		% Info becomes temporarily unavailable FIRST (fetch-info = unknown) — this activates
		% mergeMissingInfoChecks, which would resurrect old checks if the pruning logic is absent.
		{noreply, State2} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, #{<<"fetch-info">> => #{<<"ok">> => unknown}, <<"tls-certificate">> => #{<<"ok">> => true}}, #{}},
			ExistingState
		),
		% THEN circleci returns 404 — no CI configured, sends #{}.
		% The pruning step must remove the old circleci check from the cache BEFORE the merge
		% runs, so mergeMissingInfoChecks cannot put it back.
		{noreply, State3} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, circleci, #{}, #{}},
			State2
		),
		{SystemMap3, _, _} = State3,
		{"lucos_foo", _, _, NormalisedAfterCI404, _} = maps:get("host1.example.com", SystemMap3),
		% circleci check must be absent — 404 means "no CI", not "CI unknown"
		?assertNot(maps:is_key(<<"circleci">>, NormalisedAfterCI404),
			"circleci check must be wiped by 404, not resurrected by mergeMissingInfoChecks").

	% When /_info returns an unknown fetch-info (transient blip), other failing checks from
	% that same source must be preserved in the normalised cache.
	%
	% Without mergeMissingInfoChecks, a single /_info blip would silently clear all failing
	% checks from the normalised cache (because the blip's payload only contains fetch-info).
	% That would produce a false recovery alert on the blip poll, followed by a re-alert on
	% the next successful poll — doubling the alert noise for a single transient event.
	info_blip_preserves_failing_checks_test() ->
		InfoChecks = #{
			<<"fetch-info">> => #{<<"ok">> => true},
			<<"tls-certificate">> => #{<<"ok">> => false},
			<<"item-count">> => #{<<"ok">> => false}
		},
		% Start with a system with two failing checks already in the normalised cache.
		% The blip update will only carry fetch-info — without the merge logic, tls-certificate
		% and item-count would disappear from the cache.
		ExistingNormalisedCache = #{
			<<"fetch-info">> => #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 0},
			<<"tls-certificate">> => #{<<"ok">> => false, <<"unknown_count">> => 0, <<"fail_count">> => 1},
			<<"item-count">> => #{<<"ok">> => false, <<"unknown_count">> => 0, <<"fail_count">> => 1}
		},
		ExistingState = {
			#{"host1.example.com" => {"lucos_foo", system, #{info => InfoChecks}, ExistingNormalisedCache, #{}}},
			#{},
			[]
		},
		{noreply, State2} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, #{<<"fetch-info">> => #{<<"ok">> => unknown}}, #{}},
			ExistingState
		),
		{SystemMap2, _, _} = State2,
		{"lucos_foo", _, _, NormalisedAfterBlip, _} = maps:get("host1.example.com", SystemMap2),
		?assert(maps:is_key(<<"tls-certificate">>, NormalisedAfterBlip),
			"tls-certificate must persist during transient /_info blip"),
		?assert(maps:is_key(<<"item-count">>, NormalisedAfterBlip),
			"item-count must persist during transient /_info blip").

	% Bug fix: multiple source updates in the same monitoring cycle must not double-increment unknown_count.
	%
	% When circleci reports unknown, that check is merged into the shared view.  On the next
	% poll, when info reports (even with no change), the circleci check is still present in the
	% merged view as unknown.  Without the CountableKeys set, info's update would treat the
	% carried-forward circleci check as a "new" unknown and increment its counter a second time —
	% reaching count=2 in one cycle instead of two, and potentially triggering a premature alert.
	multiple_sources_same_cycle_no_double_increment_test() ->
		Notifiers = [],
		InfoChecks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		CIChecks = #{<<"circleci">> => #{<<"ok">> => true}},
		ExistingState = {
			#{"host1.example.com" => {"lucos_foo", system, #{info => InfoChecks, circleci => CIChecks}, #{}, #{}}},
			#{},
			Notifiers
		},
		% circleci reports unknown
		{noreply, State2} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, circleci, #{<<"circleci">> => #{<<"ok">> => unknown}}, #{}},
			ExistingState
		),
		{SystemMap2, _, _} = State2,
		{"lucos_foo", _, _, NormalisedAfterCI, _} = maps:get("host1.example.com", SystemMap2),
		?assertEqual(1, maps:get(<<"unknown_count">>, maps:get(<<"circleci">>, NormalisedAfterCI, #{}), -1)),
		% Now info reports (ok, no change) — circleci check is carried over in the merged view
		{noreply, State3} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, InfoChecks, #{}},
			State2
		),
		{SystemMap3, _, _} = State3,
		{"lucos_foo", _, _, NormalisedAfterInfo, _} = maps:get("host1.example.com", SystemMap3),
		% circleci count must still be 1, NOT 2 — info's update must not re-increment it
		?assertEqual(1, maps:get(<<"unknown_count">>, maps:get(<<"circleci">>, NormalisedAfterInfo, #{}), -1)).

	% is_dependency_suppressed: check with no dependsOn field → false
	is_dependency_suppressed_no_field_test() ->
		?assertEqual(false, is_dependency_suppressed(#{<<"ok">> => false}, "lucos_foo", #{})).

	% is_dependency_suppressed: dependsOn system is not in SuppressionMap → false
	is_dependency_suppressed_system_not_suppressed_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>},
		?assertEqual(false, is_dependency_suppressed(Check, "lucos_time", #{})).

	% is_dependency_suppressed: dependsOn system has an active suppression window → true
	is_dependency_suppressed_active_suppression_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>},
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_eolas" => FutureExpiry},
		?assertEqual(true, is_dependency_suppressed(Check, "lucos_time", SuppressionMap)).

	% is_dependency_suppressed: dependsOn system is in pending_verification (suppression lifted) → false
	is_dependency_suppressed_pending_verification_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>},
		PendingSources = sets:from_list([info], [{version, 2}]),
		SuppressionMap = #{"lucos_eolas" => {pending_verification, PendingSources}},
		?assertEqual(false, is_dependency_suppressed(Check, "lucos_time", SuppressionMap)).

	% is_dependency_suppressed: self-reference guard — dependsOn points to the same system → false
	is_dependency_suppressed_self_reference_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_foo">>},
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_foo" => FutureExpiry},
		?assertEqual(false, is_dependency_suppressed(Check, "lucos_foo", SuppressionMap)).

	% is_dependency_suppressed: suppression window has expired → false
	is_dependency_suppressed_expired_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>},
		PastExpiry = erlang:system_time(second) - 1,
		SuppressionMap = #{"lucos_eolas" => PastExpiry},
		?assertEqual(false, is_dependency_suppressed(Check, "lucos_time", SuppressionMap)).

	% find_dependent_systems: returns system IDs with checks declaring dependsOn TargetSystem
	find_dependent_systems_basic_test() ->
		SystemMap = #{
			"host1.example.com" => {"lucos_time", system, #{info => #{
				<<"eolas">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>}
			}}, #{}, #{}},
			"host2.example.com" => {"lucos_arachne", system, #{info => #{
				<<"triplestore">> => #{<<"ok">> => true}
			}}, #{}, #{}}
		},
		?assertEqual(["lucos_time"], find_dependent_systems("lucos_eolas", SystemMap)).

	% find_dependent_systems: no systems depend on target → empty list
	find_dependent_systems_none_test() ->
		SystemMap = #{
			"host1.example.com" => {"lucos_time", system, #{info => #{
				<<"eolas">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>}
			}}, #{}, #{}}
		},
		?assertEqual([], find_dependent_systems("some.other.system", SystemMap)).

	% find_dependent_systems: self-reference is excluded
	find_dependent_systems_excludes_self_test() ->
		SystemMap = #{
			"host1.example.com" => {"lucos_eolas", system, #{info => #{
				<<"db">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>}
			}}, #{}, #{}}
		},
		?assertEqual([], find_dependent_systems("lucos_eolas", SystemMap)).

	% find_dependent_systems: multiple systems can depend on the same target
	find_dependent_systems_multiple_test() ->
		SystemMap = #{
			"host1.example.com" => {"lucos_time", system, #{info => #{
				<<"eolas">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>}
			}}, #{}, #{}},
			"host2.example.com" => {"lucos_arachne", system, #{info => #{
				<<"eolas-data">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>}
			}}, #{}, #{}}
		},
		Deps = find_dependent_systems("lucos_eolas", SystemMap),
		?assertEqual(["lucos_arachne", "lucos_time"], lists:sort(Deps)).

	% When all failing checks have an active dependsOn suppression, no alert email is sent.
	state_change_all_dep_suppressed_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_eolas" => FutureExpiry},
		SystemChecks = #{
			<<"eolas">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>, <<"fail_count">> => 0, <<"unknown_count">> => 0}
		},
		Notifier = recording_notifier(self()),
		drain_notifications(),
		state_change("host1.example.com", "lucos_time", SystemChecks, #{}, SuppressionMap, [Notifier]),
		receive
			{notified, "host1.example.com", "lucos_time", _FailingChecks, true, _} ->
				ok  % Suppressed alert — correct
		after 100 ->
			?assert(false, "Expected suppressed notification")
		end.

	% When only some checks are dep-suppressed, the non-suppressed ones still alert.
	state_change_partial_dep_suppressed_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_eolas" => FutureExpiry},
		SystemChecks = #{
			<<"eolas">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>, <<"fail_count">> => 0, <<"unknown_count">> => 0},
			<<"db">> => #{<<"ok">> => false, <<"fail_count">> => 0, <<"unknown_count">> => 0}
		},
		Notifier = recording_notifier(self()),
		drain_notifications(),
		state_change("host1.example.com", "lucos_time", SystemChecks, #{}, SuppressionMap, [Notifier]),
		receive
			{notified, "host1.example.com", "lucos_time", FailingNow, false, _} ->
				% Only db should alert, not eolas
				?assert(maps:is_key(<<"db">>, FailingNow)),
				?assertNot(maps:is_key(<<"eolas">>, FailingNow))
		after 100 ->
			?assert(false, "Expected alert on non-dep-suppressed checks")
		end.

	% unsuppress cascades pending_verification to systems with checks depending on the unsuppressed system.
	unsuppress_cascades_pending_verification_test() ->
		TimeChecks = #{<<"eolas">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>}},
		SystemMap = #{
			"lucos_eolas" => {"lucos_eolas", system, #{info => #{<<"fetch-info">> => #{<<"ok">> => true}}}, #{}, #{}},
			"schedule-tracker.l42.eu" => {"lucos_time", system, #{info => TimeChecks}, #{}, #{}}
		},
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_eolas" => FutureExpiry},
		State = {SystemMap, SuppressionMap, []},
		{reply, ok, {_, NewSuppressionMap, _}} = handle_call(
			{unsuppress, "lucos_eolas"}, from, State
		),
		?assertMatch({pending_verification, _}, maps:get("lucos_eolas", NewSuppressionMap)),
		?assertMatch({pending_verification, _}, maps:get("lucos_time", NewSuppressionMap)).

	% computeCheckStatus: healthy check
	compute_check_status_healthy_test() ->
		Check = #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 0},
		?assertEqual(healthy, computeCheckStatus(Check)).

	% computeCheckStatus: failing check (ok=false)
	compute_check_status_failing_test() ->
		Check = #{<<"ok">> => false, <<"unknown_count">> => 0, <<"fail_count">> => 1},
		?assertEqual(failing, computeCheckStatus(Check)).

	% computeCheckStatus: unknown check (ok=unknown, no counters)
	compute_check_status_unknown_test() ->
		Check = #{<<"ok">> => unknown, <<"unknown_count">> => 0},
		?assertEqual(unknown, computeCheckStatus(Check)).

	% computeCheckStatus: buffering due to unknown_count > 0
	compute_check_status_buffering_unknown_count_test() ->
		Check = #{<<"ok">> => true, <<"unknown_count">> => 1, <<"fail_count">> => 0},
		?assertEqual(buffering, computeCheckStatus(Check)).

	% computeCheckStatus: buffering due to fail_count>0 with failThreshold>1
	compute_check_status_buffering_fail_count_test() ->
		Check = #{<<"ok">> => true, <<"unknown_count">> => 0, <<"fail_count">> => 1, <<"failThreshold">> => 3},
		?assertEqual(buffering, computeCheckStatus(Check)).

	% computeCheckStatus: fail_count=1 with failThreshold=1 is failing (already at threshold)
	compute_check_status_failing_at_threshold_test() ->
		Check = #{<<"ok">> => false, <<"fail_count">> => 1, <<"failThreshold">> => 1},
		?assertEqual(failing, computeCheckStatus(Check)).

	% computeCheckStatus: ok=unknown with unknown_count>0 → buffering.
	% This is the case where replaceUnknowns has held the previous ok value as unknown
	% (e.g. the check was never observed healthy) and the counter hasn't hit the threshold yet.
	% Without this case, a check stuck in unknown with an incrementing counter would appear
	% as plain "unknown" in the UI, hiding the fact that it is actively failing its poll.
	compute_check_status_buffering_unknown_ok_test() ->
		Check = #{<<"ok">> => unknown, <<"unknown_count">> => 1, <<"fail_count">> => 0},
		?assertEqual(buffering, computeCheckStatus(Check)).

	% computeCheckStatusText: healthy
	compute_check_status_text_healthy_test() ->
		?assertEqual(<<"healthy">>, computeCheckStatusText(healthy, #{})).

	% computeCheckStatusText: failing
	compute_check_status_text_failing_test() ->
		?assertEqual(<<"failing">>, computeCheckStatusText(failing, #{})).

	% computeCheckStatusText: unknown
	compute_check_status_text_unknown_test() ->
		?assertEqual(<<"unknown">>, computeCheckStatusText(unknown, #{})).

	% computeCheckStatusText: buffering via unknown_count
	compute_check_status_text_buffering_unknown_count_test() ->
		Check = #{<<"unknown_count">> => 2},
		?assertEqual(<<"unknown (2)">>, computeCheckStatusText(buffering, Check)).

	% computeCheckStatusText: buffering via fail_count
	compute_check_status_text_buffering_fail_count_test() ->
		Check = #{<<"unknown_count">> => 0, <<"fail_count">> => 1, <<"failThreshold">> => 3},
		?assertEqual(<<"failing (1/3)">>, computeCheckStatusText(buffering, Check)).

	% computeSystemStatus: no checks → unknown
	compute_system_status_no_checks_test() ->
		?assertEqual(unknown, computeSystemStatus("lucos_foo", #{}, #{})).

	% computeSystemStatus: all healthy checks → healthy
	compute_system_status_healthy_test() ->
		Cache = #{<<"a">> => #{<<"ok">> => true, <<"status">> => healthy, <<"statusText">> => <<"healthy">>}},
		?assertEqual(healthy, computeSystemStatus("lucos_foo", Cache, #{})).

	% computeSystemStatus: any failing check → failing
	compute_system_status_failing_test() ->
		Cache = #{<<"a">> => #{<<"ok">> => false, <<"status">> => failing, <<"statusText">> => <<"failing">>}},
		?assertEqual(failing, computeSystemStatus("lucos_foo", Cache, #{})).

	% computeSystemStatus: any unknown check → unknown
	compute_system_status_unknown_test() ->
		Cache = #{<<"a">> => #{<<"ok">> => unknown, <<"status">> => unknown, <<"statusText">> => <<"unknown">>}},
		?assertEqual(unknown, computeSystemStatus("lucos_foo", Cache, #{})).

	% computeSystemStatus: any buffering check → buffering
	compute_system_status_buffering_test() ->
		Cache = #{<<"a">> => #{<<"ok">> => true, <<"unknown_count">> => 1, <<"status">> => buffering, <<"statusText">> => <<"unknown (1)">>}},
		?assertEqual(buffering, computeSystemStatus("lucos_foo", Cache, #{})).

	% computeSystemStatus: active suppression → suppressed (even if checks are failing)
	compute_system_status_suppressed_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		Cache = #{<<"a">> => #{<<"ok">> => false, <<"status">> => failing, <<"statusText">> => <<"failing">>}},
		?assertEqual(suppressed, computeSystemStatus("lucos_foo", Cache, #{"lucos_foo" => FutureExpiry})).

	% computeSystemStatus: pending_verification → pending_verification
	compute_system_status_pending_verification_test() ->
		PendingSources = sets:from_list([info], [{version, 2}]),
		Cache = #{<<"a">> => #{<<"ok">> => false, <<"status">> => failing, <<"statusText">> => <<"failing">>}},
		?assertEqual(pending_verification, computeSystemStatus("lucos_foo", Cache, #{"lucos_foo" => {pending_verification, PendingSources}})).

	% computeSystemStatus: failing check inside suppressed system — system is suppressed, check is still failing
	suppressed_system_check_status_honest_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		Cache = #{<<"a">> => #{<<"ok">> => false, <<"status">> => failing, <<"statusText">> => <<"failing">>}},
		SystemStatus = computeSystemStatus("lucos_foo", Cache, #{"lucos_foo" => FutureExpiry}),
		CheckStatus = maps:get(<<"status">>, maps:get(<<"a">>, Cache)),
		?assertEqual(suppressed, SystemStatus),
		?assertEqual(failing, CheckStatus).

	% computeSystemStatus: dep-suppressed failing check excludes from system failing determination
	compute_system_status_dep_suppressed_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		Cache = #{<<"eolas">> => #{
			<<"ok">> => false,
			<<"dependsOn">> => <<"lucos_eolas">>,
			<<"status">> => failing,
			<<"statusText">> => <<"failing">>
		}},
		SuppressionMap = #{"lucos_eolas" => FutureExpiry},
		?assertEqual(healthy, computeSystemStatus("lucos_time", Cache, SuppressionMap)).

	% failing takes priority over unknown (step 3 before step 4)
	compute_system_status_priority_failing_over_unknown_test() ->
		Cache = #{
			<<"a">> => #{<<"ok">> => false, <<"status">> => failing, <<"statusText">> => <<"failing">>},
			<<"b">> => #{<<"ok">> => unknown, <<"status">> => unknown, <<"statusText">> => <<"unknown">>}
		},
		?assertEqual(failing, computeSystemStatus("lucos_foo", Cache, #{})).

	% unknown takes priority over buffering (step 4 before step 5)
	compute_system_status_priority_unknown_over_buffering_test() ->
		Cache = #{
			<<"a">> => #{<<"ok">> => unknown, <<"status">> => unknown, <<"statusText">> => <<"unknown">>},
			<<"b">> => #{<<"ok">> => true, <<"unknown_count">> => 1, <<"status">> => buffering, <<"statusText">> => <<"unknown (1)">>}
		},
		?assertEqual(unknown, computeSystemStatus("lucos_foo", Cache, #{})).

-endif.
