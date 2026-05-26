-module(monitoring_state_server).
-behaviour(gen_server).
-export([start_link/2, init/1, handle_cast/2, handle_call/3]).

% Alert-suppression chain: UnknownsGate → FailsGate.
%
% UnknownsGate (replaceUnknowns/4): tracks consecutiveUnknownsCount.  When a check
%   reports ok: unknown for ≥?CONSECUTIVE_UNKNOWNS_THRESHOLD consecutive polls it
%   flips to ok: false so the FailsGate can evaluate it.  Used by fetchers that probe
%   a third-party service (e.g. CircleCI) — third-party unreachability says nothing
%   about the monitored system's health, so "I couldn't tell" is the right signal.
%
% FailsGate (applyFailThreshold/2): tracks consecutiveFailsCount.  When ok: false
%   is seen for ≥failThreshold consecutive polls (default 1) the failure is surfaced.
%   Used by fetchers that directly probe the system itself (fetch-info, tls-certificate)
%   which stamp failThreshold: 2 to absorb single-poll transient blips.
%
% The two gates chain: an unknown that persists long enough becomes a false (UnknownsGate),
% which then goes through the FailsGate with the default threshold of 1.  A direct false
% (e.g. a workflow failure from circleci) skips the UnknownsGate entirely and is only
% subject to the FailsGate.
-define(CONSECUTIVE_UNKNOWNS_THRESHOLD, 3).

start_link(Notifiers, Publish) ->
	gen_server:start_link(?MODULE, {Notifiers, Publish}, []).

init({Notifiers, Publish}) ->
	{ok, {#{}, #{}, Notifiers, #{}, Publish}}.

handle_cast(Request, {SystemMap, SuppressionMap, Notifiers, PollTimings, Publish}) ->
	case Request of
		{updateSystem, Host, System, SystemType, Source, SourceChecks, SystemMetrics} ->
			logger:info("Received update for system ~p (Host ~p, Source ~p)", [System, Host, Source]),
			IsFirstSeen = not maps:is_key(System, SystemMap),
			{_, _, OldSourceChecksMap, OldNormalisedCache, OldMetrics, OldSourceTimestamps} = maps:get(System, SystemMap, {nil, system, #{}, #{}, #{}, #{}}),
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
			% double-incrementing consecutiveUnknownsCount when two sources (e.g. circleci + info)
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
									WasFailing = failingChecks(OldNormalisedCache),
									NotificationBase = #{
										host => Host,
										system => System,
										was_failing => WasFailing,
										metrics => NewMetrics
									},
									case maps:size(FailingNow) > 0 of
										true ->
											logger:notice("Service ~p still unhealthy after deploy — alerting", [System]),
											notify_all(NotificationBase#{failing_checks => FailingNow, suppressed => false}, Notifiers);
										false ->
											case map_size(WasFailing) > 0 of
												true ->
													logger:notice("Service ~p healthy after deploy — emitting recovery", [System]),
													notify_all(NotificationBase#{failing_checks => #{}, suppressed => false}, Notifiers);
												false ->
													logger:notice("Service ~p healthy after deploy", [System])
											end
									end,
									maps:remove(System, SuppressionMap);
								false ->
									maps:put(System, {pending_verification, Remaining}, SuppressionMap)
							end;
						_ ->
							case meaningfulChange(OldNormalisedCache, NormalisedChecks) of
								true ->
									SystemContext = #{
										host => Host,
										system => System,
										current_checks => NormalisedChecks,
										was_failing => failingChecks(OldNormalisedCache),
										metrics => NewMetrics
									},
									state_change(SystemContext, SuppressionMap, Notifiers);
								false ->
									SuppressionMap
							end
					end
			end,
			AnnotatedChecks = annotateCheckStatuses(NormalisedChecks),
			% Record the current time for this source. Used by the view to render
			% a per-section freshness indicator that reveals data-source-dark states.
			NewSourceTimestamps = maps:put(Source, erlang:system_time(second), OldSourceTimestamps),
			NewSystemMap = maps:put(System, {Host, SystemType, NewSourceChecksMap, AnnotatedChecks, NewMetrics, NewSourceTimestamps}, SystemMap),
			% Always publish an SSE event on every updateSystem cast.
			% The source timestamps are updated on every poll, and the client uses them
			% to render per-section freshness indicators. Suppressing the event when
			% check state is unchanged would cause client-side age counters to drift
			% until they falsely trigger the stale warning — even though the server
			% is polling correctly and the data is fresh.
			SystemList = build_system_list(NewSystemMap, NewSuppressionMap),
			Publish(SystemList),
			{noreply, {NewSystemMap, NewSuppressionMap, Notifiers, PollTimings, Publish}};
		{poll_timing, SystemId, DurationMs, IsOk} ->
			BurstThreshold = 3,
			BurstWindowMs = 30000,
			Now = erlang:monotonic_time(millisecond),
			% Count failures in the current PollTimings before this update (to detect threshold crossing)
			OldFailCount = maps:fold(fun(_, #{timestamp_ms := T, ok := Ok}, Acc) ->
				case (not Ok) andalso (Now - T =< BurstWindowMs) of
					true -> Acc + 1;
					false -> Acc
				end
			end, 0, PollTimings),
			NewEntry = #{duration_ms => DurationMs, timestamp_ms => Now, ok => IsOk},
			NewPollTimings = maps:put(SystemId, NewEntry, PollTimings),
			% Collect all recent failures (including new entry) to compute new count and build dump
			{NewFailCount, RecentFailures} = maps:fold(fun(Sys, #{timestamp_ms := T, ok := Ok, duration_ms := D}, {Count, Acc}) ->
				case (not Ok) andalso (Now - T =< BurstWindowMs) of
					true -> {Count + 1, [{Sys, D} | Acc]};
					false -> {Count, Acc}
				end
			end, {0, []}, NewPollTimings),
			% Log a burst warning only when the failure count first crosses the threshold
			case (NewFailCount >= BurstThreshold) andalso (OldFailCount < BurstThreshold) of
				true ->
					logger:warning("Poll burst: ~p checks failed within ~pms — timing dump: ~p", [NewFailCount, BurstWindowMs, lists:sort(RecentFailures)]);
				false ->
					ok
			end,
			{noreply, {SystemMap, SuppressionMap, Notifiers, NewPollTimings, Publish}}
	end.

handle_call(Request, _From, {SystemMap, SuppressionMap, Notifiers, PollTimings, Publish}) ->
	case Request of
		{fetch, all} ->
			% Return a list of system maps with pre-computed status atoms.
			% Each system map includes <<"host">> (not in the ADR spec but needed by the
			% view layer to build /_info URLs) alongside id, type, name, checks, metrics, status.
			SystemList = build_system_list(SystemMap, SuppressionMap),
			{reply, SystemList, {SystemMap, SuppressionMap, Notifiers, PollTimings, Publish}};
		{fetch, poll_stats} ->
			Stats = computePollStats(maps:values(PollTimings)),
			{reply, Stats, {SystemMap, SuppressionMap, Notifiers, PollTimings, Publish}};
		{suppress, System} ->
			case systemExists(System, SystemMap) of
				true ->
					ExpiryTime = erlang:system_time(second) + 600,
					% Snapshot per-Host failing-check keys at suppress-time. During the
					% suppression window, state_change/6 partitions the current FailingNow
					% set against this snapshot: checks that were already failing are
					% continuing problems (alert), checks that became failing during the
					% window are likely deploy churn (suppress).
					% Per-Host because the pre-existing snapshot is keyed by Host (domain) for
					% partitioning in state_change. A system could in principle span multiple
					% hosts (today it's 1:1; the data model supports more).
					PreExisting = maps:fold(fun
						(S, {Host, _, _, NormalisedCache, _, _}, Acc) when S =:= System ->
							FailingKeys = sets:from_list(maps:keys(failingChecks(NormalisedCache)), [{version, 2}]),
							maps:put(Host, FailingKeys, Acc);
						(_, _, Acc) -> Acc
					end, #{}, SystemMap),
					NewSuppressionMap = maps:put(System, {ExpiryTime, PreExisting}, SuppressionMap),
					logger:notice("Suppression window opened for ~p", [System]),
					SystemList = build_system_list(SystemMap, NewSuppressionMap),
					Publish(SystemList),
					{reply, ok, {SystemMap, NewSuppressionMap, Notifiers, PollTimings, Publish}};
				false ->
					{reply, {error, not_found}, {SystemMap, SuppressionMap, Notifiers, PollTimings, Publish}}
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
					SystemList = build_system_list(SystemMap, FinalSuppressionMap),
					Publish(SystemList),
					{reply, ok, {SystemMap, FinalSuppressionMap, Notifiers, PollTimings, Publish}};
				false ->
					logger:notice("Suppression window closed for ~p (was not suppressed)", [System]),
					{reply, ok, {SystemMap, SuppressionMap, Notifiers, PollTimings, Publish}}
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
	maps:is_key(System, SystemMap).

% Returns the set of source keys that have reported for a given system,
% across all hosts. Used to build the PendingSources set on unsuppress.
collect_active_sources(System, SystemMap) ->
	maps:fold(fun
		(S, {_, _, SourceChecksMap, _, _, _}, Acc) when S =:= System ->
			sets:union(Acc, sets:from_list(maps:keys(SourceChecksMap)));
		(_, _, Acc) -> Acc
	end, sets:new([{version, 2}]), SystemMap).

% If there's a problem with the 'fetch-info' check, merge the old and new checks to avoid flapiness
mergeMissingInfoChecks(OldChecks, NewChecks) ->
	case maps:get(<<"ok">>, maps:get(<<"fetch-info">>, NewChecks, #{<<"ok">> => unknown}), unknown) of
		true ->
			NewChecks;
		_ ->
			maps:merge(OldChecks, NewChecks)
	end.

% ── UnknownsGate ──────────────────────────────────────────────────────────────
% Replaces any ok: unknown with the previously-known value until
% ?CONSECUTIVE_UNKNOWNS_THRESHOLD consecutive unknowns are seen, at which point ok
% is flipped to false so the FailsGate (below) can evaluate it.
%
% CountableKeys is the set of check keys reported in the current source update.
% Only those keys increment their consecutiveUnknownsCount — this prevents
% double-counting when checks from one source are carried forward in the merged
% view during another source's update.
replaceUnknowns(OldChecks, NewChecks, Iterator, CountableKeys) ->
	case maps:next(Iterator) of
		{Key, NewCheck, NextIterator} ->
			NormalisedCheck = case maps:get(<<"ok">>, NewCheck, unknown) of
				unknown ->
					OldCheck = maps:get(Key, OldChecks, #{<<"ok">> => unknown}),
					OldCount = maps:get(<<"consecutiveUnknownsCount">>, OldCheck, 0),
					NewCount = case sets:is_element(Key, CountableKeys) of
						true -> OldCount + 1;  % This check was in the current source update
						false -> OldCount  % Carried over from a previous source, don't re-increment
					end,
					IncrementedCheck = maps:put(<<"consecutiveUnknownsCount">>, NewCount, NewCheck),
					case NewCount >= ?CONSECUTIVE_UNKNOWNS_THRESHOLD of
						true ->
							maps:put(<<"ok">>, false, IncrementedCheck);
						false ->
							logger:notice("Not sending alert for ~p as there has only been ~p recurring failures so far.", [Key, NewCount]),
							maps:put(<<"ok">>, maps:get(<<"ok">>, OldCheck, unknown), IncrementedCheck)
					end;
				_ ->
					maps:put(<<"consecutiveUnknownsCount">>, 0, NewCheck)
			end,
			maps:put(Key, NormalisedCheck, replaceUnknowns(OldChecks, NewChecks, NextIterator, CountableKeys));
		none ->
			maps:new()
	end.

% ── FailsGate ─────────────────────────────────────────────────────────────────
% Holds the previous ok state until N consecutive ok: false values are seen.
% failThreshold defaults to 1 (alert on first failure); direct-probe fetchers
% (fetch-info, tls-certificate) stamp failThreshold: 2 to absorb single-poll
% transient blips during deploys or container restarts.
% consecutiveFailsCount resets to 0 on recovery.
applyFailThreshold(OldChecks, NewChecks) ->
	maps:map(fun(Key, NewCheck) ->
		case maps:get(<<"ok">>, NewCheck, unknown) of
			false ->
				FailThreshold = maps:get(<<"failThreshold">>, NewCheck, 1),
				OldCheck = maps:get(Key, OldChecks, #{}),
				OldFailCount = maps:get(<<"consecutiveFailsCount">>, OldCheck, 0),
				NewFailCount = OldFailCount + 1,
				CheckWithCount = maps:put(<<"consecutiveFailsCount">>, NewFailCount, NewCheck),
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
				maps:put(<<"consecutiveFailsCount">>, 0, NewCheck)
		end
	end, NewChecks).

% Reduces monitoring flapiness by running both alert-suppression gates in sequence.
% CountableKeys is the set of check keys from the current source update that are
% reporting unknown — only those may increment consecutiveUnknownsCount, preventing
% double-counting when checks from one source are carried forward during another
% source's update.
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
%
% Notification is a map carrying everything a notifier needs to emit one event:
% host, system, failing_checks, was_failing, suppressed, metrics.
% See lucas42/lucos_monitoring#260.
notify_all(Notification, Notifiers) ->
	#{host := Host, system := System} = Notification,
	lists:foreach(fun(NotifyFn) ->
		try NotifyFn(Notification)
		catch ExClass:ExReason ->
			logger:error("Notify failed for ~p on ~p: ~p ~p", [System, Host, ExClass, ExReason])
		end
	end, Notifiers).

% SystemContext bundles all per-system fields state_change needs across its
% multiple internal notify_all dispatch sites:
%   host, system, current_checks, was_failing, metrics.
% SuppressionMap is kept as a separate arg because state_change can return a
% modified copy; bundling it would force unwrap-on-return at callers.
state_change(SystemContext, SuppressionMap, Notifiers) ->
	#{
		host := Host,
		system := System,
		current_checks := SystemChecks,
		was_failing := WasFailing,
		metrics := SystemMetrics
	} = SystemContext,
	AllFailing = failingChecks(SystemChecks),
	% Filter out checks whose dependsOn system is currently under an active suppression window.
	% Single-hop only: we never follow dependsOn chains transitively.
	FailingNow = maps:filter(fun(_, Check) ->
		not is_dependency_suppressed(Check, System, SuppressionMap)
	end, AllFailing),
	NotificationBase = #{
		host => Host,
		system => System,
		was_failing => WasFailing,
		metrics => SystemMetrics
	},
	case maps:size(AllFailing) > 0 andalso maps:size(FailingNow) =:= 0 of
		true ->
			% All failing checks are dependency-suppressed — notify as suppressed (no email alert).
			logger:notice("All failing checks on ~p suppressed via dependency: ~p", [System, maps:keys(AllFailing)]),
			notify_all(NotificationBase#{failing_checks => AllFailing, suppressed => true}, Notifiers),
			SuppressionMap;
		false ->
			% FailingNow contains only non-dep-suppressed checks. Apply system-level suppression logic.
			case maps:get(System, SuppressionMap, undefined) of
				undefined ->
					logger:notice("Checks' state changed for ~p on ~p", [System, Host]),
					notify_all(NotificationBase#{failing_checks => FailingNow, suppressed => false}, Notifiers),
					SuppressionMap;
				{ExpiryTime, PreExisting} ->
					Now = erlang:system_time(second),
					case Now < ExpiryTime of
						true ->
							% Partition FailingNow against the pre-existing snapshot for this Host:
							%   - PreExistingFailing: failing-already-at-suppress-time → continuing problem,
							%     alert as Suppressed=false
							%   - NewlyFailing: became unhealthy during the window → likely deploy churn,
							%     alert as Suppressed=true (the existing suppression rationale)
							% This restores the visual distinction between continuing failures and
							% post-deploy flap in the Loganne event stream.
							HostPreExisting = maps:get(Host, PreExisting, sets:new([{version, 2}])),
							{PreExistingFailing, NewlyFailing} = partitionByPreExisting(FailingNow, HostPreExisting),
							case maps:size(PreExistingFailing) > 0 of
								true ->
									logger:notice("Pre-existing failures continuing during deploy window for ~p: ~p", [System, maps:keys(PreExistingFailing)]),
									notify_all(NotificationBase#{failing_checks => PreExistingFailing, suppressed => false}, Notifiers);
								false -> ok
							end,
							case maps:size(NewlyFailing) > 0 of
								true ->
									logger:notice("Alert suppressed for ~p during deploy window", [System]),
									notify_all(NotificationBase#{failing_checks => NewlyFailing, suppressed => true}, Notifiers);
								false -> ok
							end,
							SuppressionMap;
						false ->
							logger:error("Suppression window for ~p expired without being cleared - deploy may have taken longer than 10 minutes", [System]),
							logger:notice("Checks' state changed for ~p on ~p", [System, Host]),
							notify_all(NotificationBase#{failing_checks => FailingNow, suppressed => false}, Notifiers),
							maps:remove(System, SuppressionMap)
					end
			end
	end.

% Partitions a map of failing checks into (PreExistingFailing, NewlyFailing) using
% a set of pre-existing-failing check keys captured at suppress-time.
partitionByPreExisting(FailingNow, HostPreExisting) ->
	maps:fold(fun(Key, Check, {PreAcc, NewAcc}) ->
		case sets:is_element(Key, HostPreExisting) of
			true  -> {maps:put(Key, Check, PreAcc), NewAcc};
			false -> {PreAcc, maps:put(Key, Check, NewAcc)}
		end
	end, {#{}, #{}}, FailingNow).

% Normalises a check's dependsOn field into a list of system ID strings.
% Accepts either:
%   - absent: []
%   - single binary (legacy single-dep shape): [binary_to_list(B)]
%   - list of binaries (aggregate/cross-cutting shape): [binary_to_list(E) || ...]
% Any unrecognised value (or non-binary element inside a list) is silently
% dropped. See ADR-0002.
normalise_depends_on(Check) ->
	case maps:get(<<"dependsOn">>, Check, undefined) of
		undefined -> [];
		B when is_binary(B) -> [binary_to_list(B)];
		L when is_list(L) ->
			[binary_to_list(E) || E <- L, is_binary(E)];
		_ -> []
	end.

% Returns true if any system in this check's normalised dependsOn list is
% currently under an active suppression window. OR semantics: a single
% suppressed element is sufficient. Guards against self-references
% (CurrentSystem appearing in the list) and treats pending_verification as
% "suppression lifted" per element. Does NOT follow transitive dependsOn
% chains — list elements are not themselves resolved against their own
% dependsOn declarations. See ADR-0002.
is_dependency_suppressed(Check, CurrentSystem, SuppressionMap) ->
	DependsOnList = normalise_depends_on(Check),
	lists:any(fun(DependsOnStr) ->
		% Guard: ignore self-references to prevent circular evaluation
		case DependsOnStr =:= CurrentSystem of
			true -> false;
			false ->
				case maps:get(DependsOnStr, SuppressionMap, undefined) of
					undefined -> false;
					{pending_verification, _} -> false;  % Suppression has been lifted
					{ExpiryTime, _PreExisting} ->
						Now = erlang:system_time(second),
						Now < ExpiryTime
				end
		end
	end, DependsOnList).

% Returns a list of system IDs whose normalised checks include TargetSystem
% in their dependsOn list (after polymorphic normalisation). Used to cascade
% pending_verification when TargetSystem unsuppresses. Excludes TargetSystem
% itself (self-reference guard).
find_dependent_systems(TargetSystem, SystemMap) ->
	lists:usort(maps:fold(fun(System, {_Host, _SystemType, SourceChecksMap, _, _, _}, Acc) ->
		case System =:= TargetSystem of
			true -> Acc;  % Guard: skip self
			false ->
				MergedChecks = mergeSourceChecks(SourceChecksMap),
				HasDependency = maps:fold(fun(_, Check, Found) ->
					Found orelse lists:member(TargetSystem, normalise_depends_on(Check))
				end, false, MergedChecks),
				case HasDependency of
					true -> [System | Acc];
					false -> Acc
				end
		end
	end, [], SystemMap)).

% Computes the status atom for a single normalised check.
% Mapping (per ADR-0001):
%   ok=false                                                         → failing
%   ok=true|unknown with consecutiveUnknownsCount>0 or fail buffering → buffering
%   ok=unknown, no counters                                          → unknown
%   ok=true, no counters                                             → healthy
computeCheckStatus(Check) ->
	Ok = maps:get(<<"ok">>, Check, unknown),
	case Ok of
		false -> failing;
		_ ->
			UnknownCount = maps:get(<<"consecutiveUnknownsCount">>, Check, 0),
			FailCount = maps:get(<<"consecutiveFailsCount">>, Check, 0),
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
			UnknownCount = maps:get(<<"consecutiveUnknownsCount">>, Check, 0),
			case UnknownCount > 0 of
				true ->
					list_to_binary("unknown (" ++ integer_to_list(UnknownCount) ++ ")");
				false ->
					FailCount = maps:get(<<"consecutiveFailsCount">>, Check, 0),
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
%
% During an active suppression window, the system collapses to `suppressed` UNLESS
% any pre-existing-failing check (snapshotted at suppress-time for this Host) is
% still in the current failing set — in which case the system has a continuing
% problem that must remain visible on the dashboard, and we fall through to
% aggregateCheckStatuses to compute the honest status. This matches the
% Loganne-side narrowing in state_change/6: the two surfaces stay consistent.
computeSystemStatus(Host, SystemId, NormalisedCache, SuppressionMap) ->
	Now = erlang:system_time(second),
	case maps:get(SystemId, SuppressionMap, undefined) of
		{pending_verification, _} ->
			pending_verification;
		{ExpiryTime, PreExisting} when ExpiryTime > Now ->
			HostPreExisting = maps:get(Host, PreExisting, sets:new([{version, 2}])),
			CurrentFailingKeys = sets:from_list(maps:keys(maps:filter(fun(_, Check) ->
				maps:get(<<"status">>, Check, unknown) =:= failing
			end, NormalisedCache)), [{version, 2}]),
			case sets:size(sets:intersection(HostPreExisting, CurrentFailingKeys)) > 0 of
				true -> aggregateCheckStatuses(SystemId, NormalisedCache, SuppressionMap);
				false -> suppressed
			end;
		_ ->
			aggregateCheckStatuses(SystemId, NormalisedCache, SuppressionMap)
	end.

% Aggregates check statuses into a system status (priority list steps 3–7 from ADR-0001).
% Failing checks whose dependsOn system is actively suppressed are excluded from step 3.
% A system with no checks (e.g. .github, vue-leaflet-antimeridian) is healthy — such
% systems will always have zero checks and should not appear as noise at the top of the
% monitoring page. The absence of checks is not a signal of failure.
aggregateCheckStatuses(SystemId, NormalisedCache, SuppressionMap) ->
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
	end.

% Builds the full system list as returned by {fetch, all}.
% Used by both {fetch, all} handler and the publish hook at each mutation site.
build_system_list(SystemMap, SuppressionMap) ->
	maps:fold(fun(SystemId, {Host, SystemType, _SourceChecksMap, NormalisedCache, Metrics, SourceTimestamps}, Acc) ->
		[buildSystemOutput(Host, SystemId, SystemType, NormalisedCache, Metrics, SuppressionMap, SourceTimestamps) | Acc]
	end, [], SystemMap).

% Builds the system output map returned by {fetch, all}.
% Includes <<"host">> for view-layer URL construction (not in ADR spec but required for rendering).
% SourceTimestamps is #{source_atom => unix_seconds} — used to compute freshness fields.
buildSystemOutput(Host, SystemId, SystemType, NormalisedCache, Metrics, SuppressionMap, SourceTimestamps) ->
	Checks = [buildCheckOutput(CheckId, Check) || {CheckId, Check} <- maps:to_list(NormalisedCache)],
	MetricsList = [buildMetricOutput(MetricId, Metric) || {MetricId, Metric} <- maps:to_list(Metrics)],
	SystemStatus = computeSystemStatus(Host, SystemId, NormalisedCache, SuppressionMap),
	{LastUpdated, OldestSourceTs} = computeFreshnessTimestamps(SourceTimestamps),
	#{
		<<"id">>               => list_to_binary(SystemId),
		<<"type">>             => SystemType,
		<<"name">>             => list_to_binary(SystemId),
		<<"host">>             => list_to_binary(Host),
		<<"checks">>           => Checks,
		<<"metrics">>          => MetricsList,
		<<"status">>           => SystemStatus,
		<<"last_updated">>     => LastUpdated,
		<<"oldest_source_ts">> => OldestSourceTs
	}.

% Returns {MostRecentTimestamp, OldestTimestamp} across all source entries.
% When no sources have reported yet, returns {0, 0} as a safe default.
computeFreshnessTimestamps(SourceTimestamps) ->
	case maps:size(SourceTimestamps) of
		0 -> {0, 0};
		_ ->
			Timestamps = maps:values(SourceTimestamps),
			{lists:max(Timestamps), lists:min(Timestamps)}
	end.

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

% Computes summary stats from a list of poll timing entries.
% Returns a map with count, max_duration_ms, mean_duration_ms, and failed_count.
% When there are no entries, all values are 0.
computePollStats([]) ->
	#{count => 0, max_duration_ms => 0, mean_duration_ms => 0, failed_count => 0};
computePollStats(Timings) ->
	Count = length(Timings),
	Durations = [maps:get(duration_ms, T, 0) || T <- Timings],
	FailedCount = length([T || T <- Timings, not maps:get(ok, T, true)]),
	MaxDuration = lists:max(Durations),
	MeanDuration = lists:sum(Durations) div Count,
	#{
		count => Count,
		max_duration_ms => MaxDuration,
		mean_duration_ms => MeanDuration,
		failed_count => FailedCount
	}.

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").
	nomaliseChecks_test() ->
		CiCountable = sets:from_list([<<"ci">>], [{version, 2}]),
		FetchInfoCountable = sets:from_list([<<"fetch-info">>], [{version, 2}]),
		% Single unknown check: ci goes from ok to unknown (count 0→1, ok held as true)
		?assertEqual(#{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 1, <<"consecutiveFailsCount">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 0}}, normaliseChecks(#{<<"ci">> => #{<<"ok">> => true}}, #{<<"ci">> => #{<<"ok">> => unknown}, <<"fetch-info">> => #{<<"ok">> => true}}, CiCountable)),
		% Second consecutive unknown for ci (count 1→2, ok still held as true)
		?assertEqual(#{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 2, <<"consecutiveFailsCount">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 0}}, normaliseChecks(#{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 1}, <<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 2}}, #{<<"ci">> => #{<<"ok">> => unknown}, <<"fetch-info">> => #{<<"ok">> => true}}, CiCountable)),
		% Third consecutive unknown for ci (count 2→3, ok flips to false and alerts)
		?assertEqual(#{<<"ci">> => #{<<"ok">> => false, <<"consecutiveUnknownsCount">> => 3, <<"consecutiveFailsCount">> => 1}, <<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 0}}, normaliseChecks(#{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 2}}, #{<<"ci">> => #{<<"ok">> => unknown}, <<"fetch-info">> => #{<<"ok">> => true}}, CiCountable)),
		% fetch-info goes unknown while other checks are carried forward from old state
		?assertEqual(#{<<"item-count">> => #{<<"ok">> => false, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 1}, <<"api-check">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 0}, <<"fetch-info">> => #{<<"ok">> => true,  <<"consecutiveUnknownsCount">> => 1, <<"consecutiveFailsCount">> => 0}}, normaliseChecks(#{<<"item-count">> => #{<<"ok">> => false}, <<"api-check">> => #{<<"ok">> => true}, <<"fetch-info">> => #{<<"ok">> => true}}, #{<<"fetch-info">> => #{<<"ok">> => unknown}}, FetchInfoCountable)).

	meaningfulChange_test() ->
		?assertEqual(false, meaningfulChange(#{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 1}, <<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0}}, #{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0}})),
		?assertEqual(true, meaningfulChange(#{<<"ci">> => #{<<"ok">> => false, <<"consecutiveUnknownsCount">> => 3}, <<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0}}, #{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 2}, <<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0}})),
		?assertEqual(true, meaningfulChange(#{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 1}, <<"fetch-info">> => #{<<"ok">> => false, <<"consecutiveUnknownsCount">> => 0}}, #{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0}})),
		?assertEqual(true, meaningfulChange(#{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0}}, #{<<"ci">> => #{<<"ok">> => false, <<"consecutiveUnknownsCount">> => 0}, <<"fetch-info">> => #{<<"ok">> => false, <<"consecutiveUnknownsCount">> => 4}})).

	systemExists_test() ->
		SystemMap = #{
			"lucos_foo" => {"host1.example.com", system, #{}, #{}, #{}, #{}},
			"lucos_bar" => {"host2.example.com", host, #{}, #{}, #{}, #{}}
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

	% First update for a system stores its state but doesn't alert (warm-up grace period).
	warmup_first_update_stores_state_test() ->
		InitialState = {#{}, #{}, [], #{}, fun(_) -> ok end},
		Checks = #{<<"fetch-info">> => #{<<"ok">> => false}},
		{noreply, {SystemMap, _, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, Checks, #{}},
			InitialState
		),
		% System ID should now be in the SystemMap
		?assert(maps:is_key("lucos_foo", SystemMap)),
		{"host1.example.com", _Type, SourceChecksMap, _, _, _} = maps:get("lucos_foo", SystemMap),
		StoredChecks = mergeSourceChecks(SourceChecksMap),
		?assertEqual(false, maps:get(<<"ok">>, maps:get(<<"fetch-info">>, StoredChecks))).

	% Second update for a known system triggers normal alert logic (not warm-up).
	% Here both updates report the same healthy state, so no meaningful change — no alert.
	warmup_second_update_not_suppressed_test() ->
		Checks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		ExistingState = {
			#{"lucos_foo" => {"host1.example.com", system, #{info => Checks}, #{}, #{}, #{}}},
			#{},
			[],
			#{}, fun(_) -> ok end
		},
		{noreply, {SystemMap, _, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, Checks, #{}},
			ExistingState
		),
		% System ID is still in the map after second update
		?assert(maps:is_key("lucos_foo", SystemMap)).

	% circleci update for a system doesn't clobber info checks, and vice versa.
	two_sources_dont_clobber_each_other_test() ->
		InfoChecks = #{<<"fetch-info">> => #{<<"ok">> => true}, <<"tls-certificate">> => #{<<"ok">> => true}},
		CIChecks = #{<<"circleci">> => #{<<"ok">> => false}},
		% Start with info checks already stored
		ExistingState = {
			#{"lucos_foo" => {"host1.example.com", system, #{info => InfoChecks}, #{}, #{}, #{}}},
			#{},
			[],
			#{}, fun(_) -> ok end
		},
		% circleci update arrives
		{noreply, {SystemMap, _, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, circleci, CIChecks, #{}},
			ExistingState
		),
		{"host1.example.com", _Type, SourceChecksMap, _, _, _} = maps:get("lucos_foo", SystemMap),
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
			#{"lucos_foo" => {"host1.example.com", system, #{info => OldInfoChecks}, #{}, #{}, #{}}},
			#{},
			[],
			#{}, fun(_) -> ok end
		},
		{noreply, {SystemMap, _, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, NewInfoChecks, #{}},
			ExistingState
		),
		{"host1.example.com", _Type, SourceChecksMap, _, _, _} = maps:get("lucos_foo", SystemMap),
		Merged = mergeSourceChecks(SourceChecksMap),
		?assertNot(maps:is_key(<<"custom-check">>, Merged)).

	% When a source with no metrics (e.g. circleci) updates a system that already
	% has metrics from the info fetcher, the existing metrics are preserved.
	empty_metrics_do_not_overwrite_test() ->
		InfoChecks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		Metrics = #{<<"agent-count">> => #{<<"value">> => 42, <<"techDetail">> => <<"count">>}},
		ExistingState = {
			#{"lucos_foo" => {"host1.example.com", system, #{info => InfoChecks}, #{}, Metrics, #{}}},
			#{},
			[],
			#{}, fun(_) -> ok end
		},
		CIChecks = #{<<"circleci">> => #{<<"ok">> => true}},
		{noreply, {SystemMap, _, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, circleci, CIChecks, #{}},
			ExistingState
		),
		{"host1.example.com", _, _, _, StoredMetrics, _} = maps:get("lucos_foo", SystemMap),
		?assertEqual(Metrics, StoredMetrics).

	% When a source provides non-empty metrics, they replace the existing ones.
	nonempty_metrics_do_overwrite_test() ->
		OldMetrics = #{<<"agent-count">> => #{<<"value">> => 42, <<"techDetail">> => <<"count">>}},
		NewMetrics = #{<<"agent-count">> => #{<<"value">> => 99, <<"techDetail">> => <<"count">>}},
		ExistingState = {
			#{"lucos_foo" => {"host1.example.com", system, #{info => #{<<"fetch-info">> => #{<<"ok">> => true}}}, #{}, OldMetrics, #{}}},
			#{},
			[],
			#{}, fun(_) -> ok end
		},
		{noreply, {SystemMap, _, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, #{<<"fetch-info">> => #{<<"ok">> => true}}, NewMetrics},
			ExistingState
		),
		{"host1.example.com", _, _, _, StoredMetrics, _} = maps:get("lucos_foo", SystemMap),
		?assertEqual(NewMetrics, StoredMetrics).

	% Helper to build a recording notifier and retrieve what it captured.
	% The notifier sends {notified, Notification} to the calling test process.
	recording_notifier(TestPid) ->
		fun(Notification) ->
			TestPid ! {notified, Notification}
		end.

	% Drain all pending {notified, ...} messages from the mailbox.
	drain_notifications() ->
		receive
			{notified, _} -> drain_notifications()
		after 0 ->
			ok
		end.

	% Unsuppressing a healthy system enters pending_verification and does NOT fire an immediate alert.
	unsuppress_healthy_system_test() ->
		HealthyChecks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		SystemMap = #{"lucos_foo" => {"host1.example.com", system, #{info => HealthyChecks}, #{}, #{}, #{}}},
		Notifier = recording_notifier(self()),
		State = {SystemMap, #{"lucos_foo" => {erlang:system_time(second) + 600, #{}}}, [Notifier], #{}, fun(_) -> ok end},
		{reply, ok, {_, NewSuppressionMap, _, _, _}} = handle_call(
			{unsuppress, "lucos_foo"}, from, State
		),
		% System should be in pending_verification, not removed from the map
		?assertMatch({pending_verification, _}, maps:get("lucos_foo", NewSuppressionMap)),
		receive
			{notified, _} -> ?assert(false, "Unexpected alert fired for healthy system")
		after 100 ->
			ok  % No immediate notification — correct
		end.

	% Unsuppressing an unhealthy system enters pending_verification; alert is deferred, not immediate.
	unsuppress_unhealthy_system_test() ->
		FailingChecks = #{<<"fetch-info">> => #{<<"ok">> => false}},
		SystemMap = #{"lucos_foo" => {"host1.example.com", system, #{info => FailingChecks}, #{}, #{}, #{}}},
		Notifier = recording_notifier(self()),
		State = {SystemMap, #{"lucos_foo" => {erlang:system_time(second) + 600, #{}}}, [Notifier], #{}, fun(_) -> ok end},
		drain_notifications(),
		{reply, ok, {_, NewSuppressionMap, _, _, _}} = handle_call(
			{unsuppress, "lucos_foo"}, from, State
		),
		% System should be in pending_verification (not cleared)
		?assertMatch({pending_verification, _}, maps:get("lucos_foo", NewSuppressionMap)),
		receive
			{notified, _} -> ?assert(false, "Alert must not fire immediately on unsuppress")
		after 100 ->
			ok  % No immediate notification — correct
		end.

	% Unsuppressing a system that isn't in the map is a no-op (idempotent).
	unsuppress_unknown_system_is_noop_test() ->
		State = {#{}, #{}, [], #{}, fun(_) -> ok end},
		{reply, ok, {_, NewSuppressionMap, _, _, _}} = handle_call(
			{unsuppress, "lucos_unknown"}, from, State
		),
		?assertEqual(#{}, NewSuppressionMap).

	% After unsuppress, a fresh poll reporting unhealthy fires an alert and clears pending state.
	pending_verification_fires_alert_when_unhealthy_test() ->
		FailingChecks = #{<<"fetch-info">> => #{<<"ok">> => false}},
		SystemMap = #{"lucos_foo" => {"host1.example.com", system, #{info => FailingChecks}, #{}, #{}, #{}}},
		PendingSources = sets:from_list([info], [{version, 2}]),
		Notifier = recording_notifier(self()),
		drain_notifications(),
		State = {SystemMap, #{"lucos_foo" => {pending_verification, PendingSources}}, [Notifier], #{}, fun(_) -> ok end},
		{noreply, {_, NewSuppressionMap, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, FailingChecks, #{}},
			State
		),
		% Suppression entry should be cleared
		?assertEqual(#{}, NewSuppressionMap),
		receive
			{notified, #{host := "host1.example.com", system := "lucos_foo", failing_checks := FailingNow, suppressed := false}} ->
				?assert(maps:is_key(<<"fetch-info">>, FailingNow))
		after 100 ->
			?assert(false, "Expected alert was not fired after verification poll")
		end.

	% After unsuppress, a fresh poll reporting healthy clears pending state without alerting —
	% when the prior normalised state was also healthy (no recovery to emit).
	pending_verification_no_alert_when_healthy_prior_also_healthy_test() ->
		HealthyChecks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		% NormalisedCache is #{} — no prior failures
		SystemMap = #{"lucos_foo" => {"host1.example.com", system, #{info => HealthyChecks}, #{}, #{}, #{}}},
		PendingSources = sets:from_list([info], [{version, 2}]),
		Notifier = recording_notifier(self()),
		drain_notifications(),
		State = {SystemMap, #{"lucos_foo" => {pending_verification, PendingSources}}, [Notifier], #{}, fun(_) -> ok end},
		{noreply, {_, NewSuppressionMap, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, HealthyChecks, #{}},
			State
		),
		% Suppression entry should be cleared
		?assertEqual(#{}, NewSuppressionMap),
		receive
			{notified, _} -> ?assert(false, "No alert expected when prior state was also healthy")
		after 100 ->
			ok
		end.

	% After unsuppress, a fresh poll reporting healthy emits a monitoringRecovery if the
	% prior normalised state had failing checks (Mechanism B bug fix).
	pending_verification_recovery_emitted_when_prior_state_was_failing_test() ->
		HealthyChecks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		FailingNormalisedCache = #{<<"fetch-info">> => #{<<"ok">> => false, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 1}},
		% NormalisedCache has a prior failure — a recovery must be emitted
		SystemMap = #{"lucos_foo" => {"host1.example.com", system, #{info => HealthyChecks}, FailingNormalisedCache, #{}, #{}}},
		PendingSources = sets:from_list([info], [{version, 2}]),
		Notifier = recording_notifier(self()),
		drain_notifications(),
		State = {SystemMap, #{"lucos_foo" => {pending_verification, PendingSources}}, [Notifier], #{}, fun(_) -> ok end},
		{noreply, {_, NewSuppressionMap, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, HealthyChecks, #{}},
			State
		),
		% Suppression entry should be cleared
		?assertEqual(#{}, NewSuppressionMap),
		receive
			{notified, #{host := "host1.example.com", system := "lucos_foo", failing_checks := FailingNow, suppressed := false}} ->
				?assertEqual(#{}, FailingNow, "Recovery must be emitted with empty failing checks")
		after 100 ->
			?assert(false, "Expected monitoringRecovery was not emitted after deploy when prior state was failing")
		end.

	% With two sources pending, the first poll keeps pending state; the second evaluates.
	pending_verification_waits_for_all_sources_test() ->
		FailingChecks = #{<<"fetch-info">> => #{<<"ok">> => false}},
		CIChecks = #{<<"circleci">> => #{<<"ok">> => false}},
		SystemMap = #{
			"lucos_foo" => {"host1.example.com", system, #{info => FailingChecks, circleci => CIChecks}, #{}, #{}, #{}}
		},
		PendingSources = sets:from_list([info, circleci], [{version, 2}]),
		Notifier = recording_notifier(self()),
		drain_notifications(),
		State = {SystemMap, #{"lucos_foo" => {pending_verification, PendingSources}}, [Notifier], #{}, fun(_) -> ok end},
		% First source (info) reports — should still be pending
		{noreply, State2} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, FailingChecks, #{}},
			State
		),
		{_, SuppressionMap2, _, _, _} = State2,
		?assertMatch({pending_verification, _}, maps:get("lucos_foo", SuppressionMap2)),
		receive
			{notified, _} -> ?assert(false, "No alert expected after first source only")
		after 100 ->
			ok
		end,
		% Second source (circleci) reports — should evaluate and alert
		{noreply, {_, SuppressionMap3, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, circleci, CIChecks, #{}},
			State2
		),
		?assertEqual(#{}, SuppressionMap3),
		receive
			{notified, #{host := "host1.example.com", system := "lucos_foo", suppressed := false}} -> ok
		after 100 ->
			?assert(false, "Expected alert after all sources reported")
		end.

	% With default failThreshold (1), a single failure is reported immediately.
	failThreshold_default_alerts_immediately_test() ->
		OldChecks = #{<<"db-check">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 0}},
		NewChecks = #{<<"db-check">> => #{<<"ok">> => false}},
		% db-check is false (not unknown), so CountableKeys is empty
		Result = normaliseChecks(OldChecks, NewChecks, sets:new([{version, 2}])),
		?assertEqual(false, maps:get(<<"ok">>, maps:get(<<"db-check">>, Result))),
		?assertEqual(1, maps:get(<<"consecutiveFailsCount">>, maps:get(<<"db-check">>, Result))).

	% With failThreshold 3, failures 1 and 2 hold the previous ok state.
	% On the third consecutive failure, ok flips to false.
	failThreshold_holds_until_threshold_test() ->
		NewChecks = #{<<"db-check">> => #{<<"ok">> => false, <<"failThreshold">> => 3}},
		% db-check is false (not unknown), so CountableKeys is empty
		EmptyCountable = sets:new([{version, 2}]),
		% First failure — consecutiveFailsCount goes to 1, ok stays true (held from old)
		OldChecks1 = #{<<"db-check">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 0}},
		Result1 = normaliseChecks(OldChecks1, NewChecks, EmptyCountable),
		?assertEqual(true, maps:get(<<"ok">>, maps:get(<<"db-check">>, Result1))),
		?assertEqual(1, maps:get(<<"consecutiveFailsCount">>, maps:get(<<"db-check">>, Result1))),
		% Second failure — consecutiveFailsCount goes to 2, still held
		Result2 = normaliseChecks(Result1, NewChecks, EmptyCountable),
		?assertEqual(true, maps:get(<<"ok">>, maps:get(<<"db-check">>, Result2))),
		?assertEqual(2, maps:get(<<"consecutiveFailsCount">>, maps:get(<<"db-check">>, Result2))),
		% Third failure — consecutiveFailsCount goes to 3, now ok flips to false
		Result3 = normaliseChecks(Result2, NewChecks, EmptyCountable),
		?assertEqual(false, maps:get(<<"ok">>, maps:get(<<"db-check">>, Result3))),
		?assertEqual(3, maps:get(<<"consecutiveFailsCount">>, maps:get(<<"db-check">>, Result3))).

	% Recovery (ok: true) resets consecutiveFailsCount to 0.
	failThreshold_recovery_resets_count_test() ->
		OldChecks = #{<<"db-check">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 2, <<"failThreshold">> => 3}},
		NewChecks = #{<<"db-check">> => #{<<"ok">> => true, <<"failThreshold">> => 3}},
		% db-check is true (not unknown), so CountableKeys is empty
		Result = normaliseChecks(OldChecks, NewChecks, sets:new([{version, 2}])),
		?assertEqual(true, maps:get(<<"ok">>, maps:get(<<"db-check">>, Result))),
		?assertEqual(0, maps:get(<<"consecutiveFailsCount">>, maps:get(<<"db-check">>, Result))).

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
			<<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 0},
			<<"tls-certificate">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 0},
			<<"circleci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 0}
		},
		ExistingState = {
			#{"lucos_foo" => {"host1.example.com", system, #{info => InfoChecks, circleci => CIChecks}, ExistingNormalisedCache, #{}, #{}}},
			#{},
			[],
			#{}, fun(_) -> ok end
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
		{SystemMap3, _, _, _, _} = State3,
		{"host1.example.com", _, _, NormalisedAfterCI404, _, _} = maps:get("lucos_foo", SystemMap3),
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
			<<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 0},
			<<"tls-certificate">> => #{<<"ok">> => false, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 1},
			<<"item-count">> => #{<<"ok">> => false, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 1}
		},
		ExistingState = {
			#{"lucos_foo" => {"host1.example.com", system, #{info => InfoChecks}, ExistingNormalisedCache, #{}, #{}}},
			#{},
			[],
			#{}, fun(_) -> ok end
		},
		{noreply, State2} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, #{<<"fetch-info">> => #{<<"ok">> => unknown}}, #{}},
			ExistingState
		),
		{SystemMap2, _, _, _, _} = State2,
		{"host1.example.com", _, _, NormalisedAfterBlip, _, _} = maps:get("lucos_foo", SystemMap2),
		?assert(maps:is_key(<<"tls-certificate">>, NormalisedAfterBlip),
			"tls-certificate must persist during transient /_info blip"),
		?assert(maps:is_key(<<"item-count">>, NormalisedAfterBlip),
			"item-count must persist during transient /_info blip").

	% Bug fix: multiple source updates in the same monitoring cycle must not double-increment consecutiveUnknownsCount.
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
			#{"lucos_foo" => {"host1.example.com", system, #{info => InfoChecks, circleci => CIChecks}, #{}, #{}, #{}}},
			#{},
			Notifiers,
			#{}, fun(_) -> ok end
		},
		% circleci reports unknown
		{noreply, State2} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, circleci, #{<<"circleci">> => #{<<"ok">> => unknown}}, #{}},
			ExistingState
		),
		{SystemMap2, _, _, _, _} = State2,
		{"host1.example.com", _, _, NormalisedAfterCI, _, _} = maps:get("lucos_foo", SystemMap2),
		?assertEqual(1, maps:get(<<"consecutiveUnknownsCount">>, maps:get(<<"circleci">>, NormalisedAfterCI, #{}), -1)),
		% Now info reports (ok, no change) — circleci check is carried over in the merged view
		{noreply, State3} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, InfoChecks, #{}},
			State2
		),
		{SystemMap3, _, _, _, _} = State3,
		{"host1.example.com", _, _, NormalisedAfterInfo, _, _} = maps:get("lucos_foo", SystemMap3),
		% circleci count must still be 1, NOT 2 — info's update must not re-increment it
		?assertEqual(1, maps:get(<<"consecutiveUnknownsCount">>, maps:get(<<"circleci">>, NormalisedAfterInfo, #{}), -1)).

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
		SuppressionMap = #{"lucos_eolas" => {FutureExpiry, #{}}},
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
		SuppressionMap = #{"lucos_foo" => {FutureExpiry, #{}}},
		?assertEqual(false, is_dependency_suppressed(Check, "lucos_foo", SuppressionMap)).

	% is_dependency_suppressed: suppression window has expired → false
	is_dependency_suppressed_expired_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>},
		PastExpiry = erlang:system_time(second) - 1,
		SuppressionMap = #{"lucos_eolas" => {PastExpiry, #{}}},
		?assertEqual(false, is_dependency_suppressed(Check, "lucos_time", SuppressionMap)).

	% find_dependent_systems: returns system IDs with checks declaring dependsOn TargetSystem
	find_dependent_systems_basic_test() ->
		SystemMap = #{
			"lucos_time" => {"host1.example.com", system, #{info => #{
				<<"eolas">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>}
			}}, #{}, #{}, #{}},
			"lucos_arachne" => {"host2.example.com", system, #{info => #{
				<<"triplestore">> => #{<<"ok">> => true}
			}}, #{}, #{}, #{}}
		},
		?assertEqual(["lucos_time"], find_dependent_systems("lucos_eolas", SystemMap)).

	% find_dependent_systems: no systems depend on target → empty list
	find_dependent_systems_none_test() ->
		SystemMap = #{
			"lucos_time" => {"host1.example.com", system, #{info => #{
				<<"eolas">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>}
			}}, #{}, #{}, #{}}
		},
		?assertEqual([], find_dependent_systems("some.other.system", SystemMap)).

	% find_dependent_systems: self-reference is excluded
	find_dependent_systems_excludes_self_test() ->
		SystemMap = #{
			"lucos_eolas" => {"host1.example.com", system, #{info => #{
				<<"db">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>}
			}}, #{}, #{}, #{}}
		},
		?assertEqual([], find_dependent_systems("lucos_eolas", SystemMap)).

	% find_dependent_systems: multiple systems can depend on the same target
	find_dependent_systems_multiple_test() ->
		SystemMap = #{
			"lucos_time" => {"host1.example.com", system, #{info => #{
				<<"eolas">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>}
			}}, #{}, #{}, #{}},
			"lucos_arachne" => {"host2.example.com", system, #{info => #{
				<<"eolas-data">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>}
			}}, #{}, #{}, #{}}
		},
		Deps = find_dependent_systems("lucos_eolas", SystemMap),
		?assertEqual(["lucos_arachne", "lucos_time"], lists:sort(Deps)).

	% --- Polymorphic dependsOn (ADR-0002): list-shape tests ---

	% is_dependency_suppressed: list shape, none of the listed systems suppressed → false
	is_dependency_suppressed_list_none_suppressed_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => [<<"lucos_a">>, <<"lucos_b">>]},
		?assertEqual(false, is_dependency_suppressed(Check, "lucos_loganne", #{})).

	% is_dependency_suppressed: list shape, one listed system actively suppressed → true (OR semantics)
	is_dependency_suppressed_list_one_suppressed_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => [<<"lucos_a">>, <<"lucos_b">>]},
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_b" => {FutureExpiry, #{}}},
		?assertEqual(true, is_dependency_suppressed(Check, "lucos_loganne", SuppressionMap)).

	% is_dependency_suppressed: list shape, one listed system in pending_verification → false (suppression lifted for that element)
	is_dependency_suppressed_list_pending_verification_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => [<<"lucos_a">>, <<"lucos_b">>]},
		PendingSources = sets:from_list([info]),
		SuppressionMap = #{"lucos_b" => {pending_verification, PendingSources}},
		?assertEqual(false, is_dependency_suppressed(Check, "lucos_loganne", SuppressionMap)).

	% is_dependency_suppressed: list shape containing the current system (self-reference) →
	% the self element is ignored, but other elements still get evaluated normally
	is_dependency_suppressed_list_self_reference_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => [<<"lucos_loganne">>, <<"lucos_b">>]},
		FutureExpiry = erlang:system_time(second) + 600,
		% Self is "suppressed" too, but that element is ignored; lucos_b is not suppressed → false
		SuppressionMap = #{"lucos_loganne" => {FutureExpiry, #{}}},
		?assertEqual(false, is_dependency_suppressed(Check, "lucos_loganne", SuppressionMap)),
		% Now suppress lucos_b — self is still ignored, but the other element triggers true
		SuppressionMap2 = #{
			"lucos_loganne" => {FutureExpiry, #{}},
			"lucos_b" => {FutureExpiry, #{}}
		},
		?assertEqual(true, is_dependency_suppressed(Check, "lucos_loganne", SuppressionMap2)).

	% is_dependency_suppressed: list shape, one element expired and one element active → true
	% (the active one wins under OR semantics)
	is_dependency_suppressed_list_mixed_expiry_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => [<<"lucos_a">>, <<"lucos_b">>]},
		PastExpiry = erlang:system_time(second) - 1,
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{
			"lucos_a" => {PastExpiry, #{}},
			"lucos_b" => {FutureExpiry, #{}}
		},
		?assertEqual(true, is_dependency_suppressed(Check, "lucos_loganne", SuppressionMap)).

	% is_dependency_suppressed: empty list shape → false (nothing to suppress against)
	is_dependency_suppressed_list_empty_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => []},
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_a" => {FutureExpiry, #{}}},
		?assertEqual(false, is_dependency_suppressed(Check, "lucos_loganne", SuppressionMap)).

	% find_dependent_systems: target appears as one of several elements in a list-shaped dependsOn
	find_dependent_systems_list_multi_element_test() ->
		SystemMap = #{
			"lucos_loganne" => {"host1.example.com", system, #{info => #{
				<<"webhook-error-rate">> =>
					#{<<"ok">> => false, <<"dependsOn">> => [<<"lucos_a">>, <<"lucos_eolas">>, <<"lucos_b">>]}
			}}, #{}, #{}, #{}}
		},
		?assertEqual(["lucos_loganne"], find_dependent_systems("lucos_eolas", SystemMap)).

	% find_dependent_systems: target is the only element in a singleton list
	find_dependent_systems_list_singleton_test() ->
		SystemMap = #{
			"lucos_loganne" => {"host1.example.com", system, #{info => #{
				<<"webhook-error-rate">> =>
					#{<<"ok">> => false, <<"dependsOn">> => [<<"lucos_eolas">>]}
			}}, #{}, #{}, #{}}
		},
		?assertEqual(["lucos_loganne"], find_dependent_systems("lucos_eolas", SystemMap)).

	% find_dependent_systems: self-reference inside a list is excluded
	find_dependent_systems_list_self_reference_test() ->
		SystemMap = #{
			"lucos_loganne" => {"host1.example.com", system, #{info => #{
				<<"webhook-error-rate">> =>
					#{<<"ok">> => false, <<"dependsOn">> => [<<"lucos_loganne">>, <<"lucos_a">>]}
			}}, #{}, #{}, #{}}
		},
		?assertEqual([], find_dependent_systems("lucos_loganne", SystemMap)).

	% When all failing checks have an active dependsOn suppression, no alert email is sent.
	state_change_all_dep_suppressed_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_eolas" => {FutureExpiry, #{}}},
		SystemChecks = #{
			<<"eolas">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>, <<"consecutiveFailsCount">> => 0, <<"consecutiveUnknownsCount">> => 0}
		},
		Notifier = recording_notifier(self()),
		drain_notifications(),
		state_change(#{host => "host1.example.com", system => "lucos_time",
		               current_checks => SystemChecks, was_failing => #{}, metrics => #{}},
		             SuppressionMap, [Notifier]),
		receive
			{notified, #{host := "host1.example.com", system := "lucos_time", suppressed := true}} ->
				ok  % Suppressed alert — correct
		after 100 ->
			?assert(false, "Expected suppressed notification")
		end.

	% When only some checks are dep-suppressed, the non-suppressed ones still alert.
	state_change_partial_dep_suppressed_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_eolas" => {FutureExpiry, #{}}},
		SystemChecks = #{
			<<"eolas">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>, <<"consecutiveFailsCount">> => 0, <<"consecutiveUnknownsCount">> => 0},
			<<"db">> => #{<<"ok">> => false, <<"consecutiveFailsCount">> => 0, <<"consecutiveUnknownsCount">> => 0}
		},
		Notifier = recording_notifier(self()),
		drain_notifications(),
		state_change(#{host => "host1.example.com", system => "lucos_time",
		               current_checks => SystemChecks, was_failing => #{}, metrics => #{}},
		             SuppressionMap, [Notifier]),
		receive
			{notified, #{host := "host1.example.com", system := "lucos_time", failing_checks := FailingNow, suppressed := false}} ->
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
			"lucos_eolas" => {"", system, #{info => #{<<"fetch-info">> => #{<<"ok">> => true}}}, #{}, #{}, #{}},
			"lucos_time" => {"schedule-tracker.l42.eu", system, #{info => TimeChecks}, #{}, #{}, #{}}
		},
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_eolas" => {FutureExpiry, #{}}},
		State = {SystemMap, SuppressionMap, [], #{}, fun(_) -> ok end},
		{reply, ok, {_, NewSuppressionMap, _, _, _}} = handle_call(
			{unsuppress, "lucos_eolas"}, from, State
		),
		?assertMatch({pending_verification, _}, maps:get("lucos_eolas", NewSuppressionMap)),
		?assertMatch({pending_verification, _}, maps:get("lucos_time", NewSuppressionMap)).

	% computeCheckStatus: healthy check
	compute_check_status_healthy_test() ->
		Check = #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 0},
		?assertEqual(healthy, computeCheckStatus(Check)).

	% computeCheckStatus: failing check (ok=false)
	compute_check_status_failing_test() ->
		Check = #{<<"ok">> => false, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 1},
		?assertEqual(failing, computeCheckStatus(Check)).

	% computeCheckStatus: unknown check (ok=unknown, no counters)
	compute_check_status_unknown_test() ->
		Check = #{<<"ok">> => unknown, <<"consecutiveUnknownsCount">> => 0},
		?assertEqual(unknown, computeCheckStatus(Check)).

	% computeCheckStatus: buffering due to consecutiveUnknownsCount > 0
	compute_check_status_buffering_unknown_count_test() ->
		Check = #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 1, <<"consecutiveFailsCount">> => 0},
		?assertEqual(buffering, computeCheckStatus(Check)).

	% computeCheckStatus: buffering due to consecutiveFailsCount>0 with failThreshold>1
	compute_check_status_buffering_fail_count_test() ->
		Check = #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 1, <<"failThreshold">> => 3},
		?assertEqual(buffering, computeCheckStatus(Check)).

	% computeCheckStatus: consecutiveFailsCount=1 with failThreshold=1 is failing (already at threshold)
	compute_check_status_failing_at_threshold_test() ->
		Check = #{<<"ok">> => false, <<"consecutiveFailsCount">> => 1, <<"failThreshold">> => 1},
		?assertEqual(failing, computeCheckStatus(Check)).

	% computeCheckStatus: ok=unknown with consecutiveUnknownsCount>0 → buffering.
	% This is the case where replaceUnknowns has held the previous ok value as unknown
	% (e.g. the check was never observed healthy) and the counter hasn't hit the threshold yet.
	% Without this case, a check stuck in unknown with an incrementing counter would appear
	% as plain "unknown" in the UI, hiding the fact that it is actively failing its poll.
	compute_check_status_buffering_unknown_ok_test() ->
		Check = #{<<"ok">> => unknown, <<"consecutiveUnknownsCount">> => 1, <<"consecutiveFailsCount">> => 0},
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

	% computeCheckStatusText: buffering via consecutiveUnknownsCount
	compute_check_status_text_buffering_unknown_count_test() ->
		Check = #{<<"consecutiveUnknownsCount">> => 2},
		?assertEqual(<<"unknown (2)">>, computeCheckStatusText(buffering, Check)).

	% computeCheckStatusText: buffering via consecutiveFailsCount
	compute_check_status_text_buffering_fail_count_test() ->
		Check = #{<<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 1, <<"failThreshold">> => 3},
		?assertEqual(<<"failing (1/3)">>, computeCheckStatusText(buffering, Check)).

	% computeSystemStatus: no checks → healthy.
	% Systems like .github or vue-leaflet-antimeridian have no checks and will always be in
	% this state. Treating them as unknown would pull them to the top of the monitoring page
	% as permanent noise. They should be healthy and sort to the bottom.
	compute_system_status_no_checks_test() ->
		?assertEqual(healthy, computeSystemStatus("host1", "lucos_foo", #{}, #{})).

	% computeSystemStatus: all healthy checks → healthy
	compute_system_status_healthy_test() ->
		Cache = #{<<"a">> => #{<<"ok">> => true, <<"status">> => healthy, <<"statusText">> => <<"healthy">>}},
		?assertEqual(healthy, computeSystemStatus("host1", "lucos_foo", Cache, #{})).

	% computeSystemStatus: any failing check → failing
	compute_system_status_failing_test() ->
		Cache = #{<<"a">> => #{<<"ok">> => false, <<"status">> => failing, <<"statusText">> => <<"failing">>}},
		?assertEqual(failing, computeSystemStatus("host1", "lucos_foo", Cache, #{})).

	% computeSystemStatus: any unknown check → unknown
	compute_system_status_unknown_test() ->
		Cache = #{<<"a">> => #{<<"ok">> => unknown, <<"status">> => unknown, <<"statusText">> => <<"unknown">>}},
		?assertEqual(unknown, computeSystemStatus("host1", "lucos_foo", Cache, #{})).

	% computeSystemStatus: any buffering check → buffering
	compute_system_status_buffering_test() ->
		Cache = #{<<"a">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 1, <<"status">> => buffering, <<"statusText">> => <<"unknown (1)">>}},
		?assertEqual(buffering, computeSystemStatus("host1", "lucos_foo", Cache, #{})).

	% computeSystemStatus: active suppression with no pre-existing failures → suppressed
	% (the failing check appeared during the deploy window, so it's appropriately hidden).
	compute_system_status_suppressed_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		Cache = #{<<"a">> => #{<<"ok">> => false, <<"status">> => failing, <<"statusText">> => <<"failing">>}},
		?assertEqual(suppressed, computeSystemStatus("host1", "lucos_foo", Cache, #{"lucos_foo" => {FutureExpiry, #{}}})).

	% computeSystemStatus: active suppression with a pre-existing failure still failing →
	% falls through to aggregateCheckStatuses so the system remains visibly failing on
	% the dashboard during the deploy window.
	compute_system_status_active_window_pre_existing_failure_visible_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		Cache = #{<<"a">> => #{<<"ok">> => false, <<"status">> => failing, <<"statusText">> => <<"failing">>}},
		PreExisting = #{"host1" => sets:from_list([<<"a">>], [{version, 2}])},
		?assertEqual(failing, computeSystemStatus("host1", "lucos_foo", Cache, #{"lucos_foo" => {FutureExpiry, PreExisting}})).

	% computeSystemStatus: active suppression where the only currently-failing check is NOT in
	% the pre-existing snapshot → still suppressed (this is exactly the "deploy churn" case
	% the suppression is designed for).
	compute_system_status_active_window_new_failure_only_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		Cache = #{<<"new-check">> => #{<<"ok">> => false, <<"status">> => failing, <<"statusText">> => <<"failing">>}},
		PreExisting = #{"host1" => sets:from_list([<<"some-other-check">>], [{version, 2}])},
		?assertEqual(suppressed, computeSystemStatus("host1", "lucos_foo", Cache, #{"lucos_foo" => {FutureExpiry, PreExisting}})).

	% computeSystemStatus: active suppression where pre-existing-failing has since recovered →
	% suppressed (no continuing problem to surface).
	compute_system_status_active_window_pre_existing_recovered_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		Cache = #{<<"a">> => #{<<"ok">> => true, <<"status">> => healthy, <<"statusText">> => <<"healthy">>}},
		PreExisting = #{"host1" => sets:from_list([<<"a">>], [{version, 2}])},
		?assertEqual(suppressed, computeSystemStatus("host1", "lucos_foo", Cache, #{"lucos_foo" => {FutureExpiry, PreExisting}})).

	% computeSystemStatus: pending_verification → pending_verification
	compute_system_status_pending_verification_test() ->
		PendingSources = sets:from_list([info], [{version, 2}]),
		Cache = #{<<"a">> => #{<<"ok">> => false, <<"status">> => failing, <<"statusText">> => <<"failing">>}},
		?assertEqual(pending_verification, computeSystemStatus("host1", "lucos_foo", Cache, #{"lucos_foo" => {pending_verification, PendingSources}})).

	% computeSystemStatus: failing check inside suppressed system (no pre-existing) — system
	% is suppressed, check is still failing.
	suppressed_system_check_status_honest_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		Cache = #{<<"a">> => #{<<"ok">> => false, <<"status">> => failing, <<"statusText">> => <<"failing">>}},
		SystemStatus = computeSystemStatus("host1", "lucos_foo", Cache, #{"lucos_foo" => {FutureExpiry, #{}}}),
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
		SuppressionMap = #{"lucos_eolas" => {FutureExpiry, #{}}},
		?assertEqual(healthy, computeSystemStatus("host1", "lucos_time", Cache, SuppressionMap)).

	% failing takes priority over unknown (step 3 before step 4)
	compute_system_status_priority_failing_over_unknown_test() ->
		Cache = #{
			<<"a">> => #{<<"ok">> => false, <<"status">> => failing, <<"statusText">> => <<"failing">>},
			<<"b">> => #{<<"ok">> => unknown, <<"status">> => unknown, <<"statusText">> => <<"unknown">>}
		},
		?assertEqual(failing, computeSystemStatus("host1", "lucos_foo", Cache, #{})).

	% unknown takes priority over buffering (step 4 before step 5)
	compute_system_status_priority_unknown_over_buffering_test() ->
		Cache = #{
			<<"a">> => #{<<"ok">> => unknown, <<"status">> => unknown, <<"statusText">> => <<"unknown">>},
			<<"b">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 1, <<"status">> => buffering, <<"statusText">> => <<"unknown (1)">>}
		},
		?assertEqual(unknown, computeSystemStatus("host1", "lucos_foo", Cache, #{})).

	% suppress: snapshot pre-existing failing checks at suppress-time. A subsequent
	% state_change with the same checks should split the alert into a Suppressed=false
	% notification (continuing problem) — NOT collapse to a single Suppressed=true.
	suppress_snapshots_pre_existing_failures_test() ->
		FailingChecks = #{<<"host-tracking-failures">> => #{<<"ok">> => false, <<"consecutiveFailsCount">> => 1, <<"consecutiveUnknownsCount">> => 0}},
		AnnotatedFailing = annotateCheckStatuses(FailingChecks),
		SystemMap = #{"lucos_foo" => {"host1", system, #{info => FailingChecks}, AnnotatedFailing, #{}, #{}}},
		Notifier = recording_notifier(self()),
		drain_notifications(),
		State = {SystemMap, #{}, [Notifier], #{}, fun(_) -> ok end},
		{reply, ok, {_SM, NewSuppressionMap, _, _, _}} = handle_call({suppress, "lucos_foo"}, from, State),
		% Snapshot must contain the host-tracking-failures key under "host1"
		{ExpiryTime, PreExisting} = maps:get("lucos_foo", NewSuppressionMap),
		?assert(is_integer(ExpiryTime)),
		?assertMatch(#{"host1" := _}, PreExisting),
		HostKeys = maps:get("host1", PreExisting),
		?assert(sets:is_element(<<"host-tracking-failures">>, HostKeys)).

	% state_change: pre-existing failure during active window → Suppressed=false (continuing problem).
	state_change_pre_existing_alerts_unsuppressed_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		PreExisting = #{"host1" => sets:from_list([<<"host-tracking-failures">>], [{version, 2}])},
		SuppressionMap = #{"lucos_backups" => {FutureExpiry, PreExisting}},
		SystemChecks = #{<<"host-tracking-failures">> => #{<<"ok">> => false, <<"consecutiveFailsCount">> => 1, <<"consecutiveUnknownsCount">> => 0}},
		Notifier = recording_notifier(self()),
		drain_notifications(),
		state_change(#{host => "host1", system => "lucos_backups",
		               current_checks => SystemChecks, was_failing => #{}, metrics => #{}},
		             SuppressionMap, [Notifier]),
		receive
			{notified, #{host := "host1", system := "lucos_backups", failing_checks := FailingNow, suppressed := false}} ->
				?assert(maps:is_key(<<"host-tracking-failures">>, FailingNow))
		after 100 ->
			?assert(false, "Expected unsuppressed alert for pre-existing failure")
		end.

	% state_change: only-newly-failing during active window → Suppressed=true (deploy churn).
	state_change_newly_failing_during_window_suppressed_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		PreExisting = #{"host1" => sets:new([{version, 2}])},
		SuppressionMap = #{"lucos_foo" => {FutureExpiry, PreExisting}},
		SystemChecks = #{<<"new-failure">> => #{<<"ok">> => false, <<"consecutiveFailsCount">> => 1, <<"consecutiveUnknownsCount">> => 0}},
		Notifier = recording_notifier(self()),
		drain_notifications(),
		state_change(#{host => "host1", system => "lucos_foo",
		               current_checks => SystemChecks, was_failing => #{}, metrics => #{}},
		             SuppressionMap, [Notifier]),
		receive
			{notified, #{host := "host1", system := "lucos_foo", failing_checks := FailingNow, suppressed := true}} ->
				?assert(maps:is_key(<<"new-failure">>, FailingNow))
		after 100 ->
			?assert(false, "Expected suppressed alert for newly-failing check")
		end.

	% state_change: mix of pre-existing and new failures during active window → two separate
	% notify_all calls, one per partition, with the right Suppressed flag for each.
	state_change_partitions_pre_existing_and_newly_failing_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		PreExisting = #{"host1" => sets:from_list([<<"old-failure">>], [{version, 2}])},
		SuppressionMap = #{"lucos_foo" => {FutureExpiry, PreExisting}},
		SystemChecks = #{
			<<"old-failure">> => #{<<"ok">> => false, <<"consecutiveFailsCount">> => 1, <<"consecutiveUnknownsCount">> => 0},
			<<"new-failure">> => #{<<"ok">> => false, <<"consecutiveFailsCount">> => 1, <<"consecutiveUnknownsCount">> => 0}
		},
		Notifier = recording_notifier(self()),
		drain_notifications(),
		state_change(#{host => "host1", system => "lucos_foo",
		               current_checks => SystemChecks, was_failing => #{}, metrics => #{}},
		             SuppressionMap, [Notifier]),
		% Collect both notifications — order is not guaranteed.
		Notifications = collect_notifications(2, []),
		Unsuppressed = [maps:get(failing_checks, N) || {notified, N} <- Notifications, maps:get(suppressed, N) =:= false],
		Suppressed   = [maps:get(failing_checks, N) || {notified, N} <- Notifications, maps:get(suppressed, N) =:= true],
		?assertEqual(1, length(Unsuppressed)),
		?assertEqual(1, length(Suppressed)),
		[UnsupKeys] = Unsuppressed,
		[SupKeys]   = Suppressed,
		?assert(maps:is_key(<<"old-failure">>, UnsupKeys)),
		?assertNot(maps:is_key(<<"new-failure">>, UnsupKeys)),
		?assert(maps:is_key(<<"new-failure">>, SupKeys)),
		?assertNot(maps:is_key(<<"old-failure">>, SupKeys)).

	% Helper: collect N notifications from the test process mailbox, in order received.
	collect_notifications(0, Acc) ->
		lists:reverse(Acc);
	collect_notifications(N, Acc) ->
		receive
			{notified, _} = Msg -> collect_notifications(N - 1, [Msg | Acc])
		after 100 ->
			lists:reverse(Acc)
		end.

	% poll_timing cast stores duration and ok status for a system.
	poll_timing_stores_entry_test() ->
		State = {#{}, #{}, [], #{}, fun(_) -> ok end},
		{noreply, {_, _, _, PollTimings, _}} = handle_cast(
			{poll_timing, "lucos_foo", 450, true},
			State
		),
		?assert(maps:is_key("lucos_foo", PollTimings)),
		Entry = maps:get("lucos_foo", PollTimings),
		?assertEqual(450, maps:get(duration_ms, Entry)),
		?assertEqual(true, maps:get(ok, Entry)).

	% poll_timing: failed entry is stored with ok=false.
	poll_timing_stores_failed_entry_test() ->
		State = {#{}, #{}, [], #{}, fun(_) -> ok end},
		{noreply, {_, _, _, PollTimings, _}} = handle_cast(
			{poll_timing, "lucos_bar", 1200, false},
			State
		),
		Entry = maps:get("lucos_bar", PollTimings),
		?assertEqual(false, maps:get(ok, Entry)).

	% poll_timing: burst warning fires when failure count first crosses BurstThreshold (3).
	% Three consecutive failed poll_timing casts should trigger the threshold crossing.
	poll_timing_burst_detection_fires_at_threshold_test() ->
		State = {#{}, #{}, [], #{}, fun(_) -> ok end},
		{noreply, State1} = handle_cast({poll_timing, "svc_a", 1000, false}, State),
		{noreply, State2} = handle_cast({poll_timing, "svc_b", 950, false}, State1),
		% At count=2, no burst yet. At count=3 (after svc_c), burst fires.
		% We just verify the cast succeeds and stores all three entries.
		{noreply, {_, _, _, PollTimings3, _}} = handle_cast({poll_timing, "svc_c", 800, false}, State2),
		?assert(maps:is_key("svc_a", PollTimings3)),
		?assert(maps:is_key("svc_b", PollTimings3)),
		?assert(maps:is_key("svc_c", PollTimings3)).

	% poll_timing: healthy polls do not accumulate towards burst threshold.
	poll_timing_healthy_polls_no_burst_test() ->
		State = {#{}, #{}, [], #{}, fun(_) -> ok end},
		{noreply, State1} = handle_cast({poll_timing, "svc_a", 200, true}, State),
		{noreply, State2} = handle_cast({poll_timing, "svc_b", 150, true}, State1),
		{noreply, {_, _, _, PollTimings, _}} = handle_cast({poll_timing, "svc_c", 180, true}, State2),
		% All three stored with ok=true
		?assertEqual(true, maps:get(ok, maps:get("svc_a", PollTimings))),
		?assertEqual(true, maps:get(ok, maps:get("svc_b", PollTimings))),
		?assertEqual(true, maps:get(ok, maps:get("svc_c", PollTimings))).

	% computePollStats: empty input → all zeros.
	compute_poll_stats_empty_test() ->
		?assertEqual(#{count => 0, max_duration_ms => 0, mean_duration_ms => 0, failed_count => 0},
			computePollStats([])).

	% computePollStats: single entry, healthy.
	compute_poll_stats_single_healthy_test() ->
		Timings = [#{duration_ms => 300, timestamp_ms => 0, ok => true}],
		Stats = computePollStats(Timings),
		?assertEqual(1, maps:get(count, Stats)),
		?assertEqual(300, maps:get(max_duration_ms, Stats)),
		?assertEqual(300, maps:get(mean_duration_ms, Stats)),
		?assertEqual(0, maps:get(failed_count, Stats)).

	% computePollStats: multiple entries with one failed.
	compute_poll_stats_mixed_test() ->
		Timings = [
			#{duration_ms => 100, timestamp_ms => 0, ok => true},
			#{duration_ms => 500, timestamp_ms => 0, ok => false},
			#{duration_ms => 300, timestamp_ms => 0, ok => true}
		],
		Stats = computePollStats(Timings),
		?assertEqual(3, maps:get(count, Stats)),
		?assertEqual(500, maps:get(max_duration_ms, Stats)),
		?assertEqual(300, maps:get(mean_duration_ms, Stats)),  % (100+500+300) div 3 = 300
		?assertEqual(1, maps:get(failed_count, Stats)).

	% {fetch, poll_stats} call returns stats from stored timings.
	fetch_poll_stats_empty_test() ->
		State = {#{}, #{}, [], #{}, fun(_) -> ok end},
		{reply, Stats, _} = handle_call({fetch, poll_stats}, from, State),
		?assertEqual(0, maps:get(count, Stats)).

	% {fetch, poll_stats} call returns correct stats after poll_timing updates.
	fetch_poll_stats_after_timings_test() ->
		State = {#{}, #{}, [], #{}, fun(_) -> ok end},
		{noreply, State1} = handle_cast({poll_timing, "svc_a", 400, true}, State),
		{noreply, State2} = handle_cast({poll_timing, "svc_b", 800, false}, State1),
		{reply, Stats, _} = handle_call({fetch, poll_stats}, from, State2),
		?assertEqual(2, maps:get(count, Stats)),
		?assertEqual(800, maps:get(max_duration_ms, Stats)),
		?assertEqual(600, maps:get(mean_duration_ms, Stats)),  % (400+800) div 2 = 600
		?assertEqual(1, maps:get(failed_count, Stats)).

	% Publish is called when a system's checks change.
	publish_called_when_checks_change_test() ->
		Self = self(),
		PublishFun = fun(SystemList) -> Self ! {published, SystemList} end,
		Checks1 = #{<<"fetch-info">> => #{<<"ok">> => true}},
		Checks2 = #{<<"fetch-info">> => #{<<"ok">> => false}},
		ExistingAnnotated = annotateCheckStatuses(normaliseChecks(#{}, Checks1, sets:new([{version, 2}]))),
		ExistingState = {
			#{"lucos_foo" => {"host1.example.com", system, #{info => Checks1}, ExistingAnnotated, #{}, #{}}},
			#{},
			[],
			#{}, PublishFun
		},
		handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, Checks2, #{}},
			ExistingState
		),
		receive
			{published, _} -> ok
		after 100 ->
			?assert(false, "Expected publish to be called when checks changed, but it was not")
		end.

	% Publish is called on every updateSystem cast, even when check state is unchanged.
	% Source timestamps are always updated and the client relies on them for freshness
	% indicators — suppressing the event on stable state would produce false stale warnings.
	publish_called_on_every_update_test() ->
		Self = self(),
		PublishFun = fun(SystemList) -> Self ! {published, SystemList} end,
		Checks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		ExistingAnnotated = annotateCheckStatuses(normaliseChecks(#{}, Checks, sets:new([{version, 2}]))),
		ExistingState = {
			#{"lucos_foo" => {"host1.example.com", system, #{info => Checks}, ExistingAnnotated, #{}, #{}}},
			#{},
			[],
			#{}, PublishFun
		},
		handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, Checks, #{}},
			ExistingState
		),
		receive
			{published, _} -> ok
		after 100 ->
			?assert(false, "Expected publish to be called on every update, but it was not")
		end.

	% Source timestamps are recorded in the system entry on each updateSystem cast.
	source_timestamp_recorded_on_first_update_test() ->
		InitialState = {#{}, #{}, [], #{}, fun(_) -> ok end},
		Before = erlang:system_time(second),
		{noreply, {SystemMap, _, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, #{}, #{}},
			InitialState
		),
		After = erlang:system_time(second),
		{"host1.example.com", _, _, _, _, SourceTimestamps} = maps:get("lucos_foo", SystemMap),
		?assert(maps:is_key(info, SourceTimestamps), "info source must have a timestamp entry"),
		Ts = maps:get(info, SourceTimestamps),
		?assert(Ts >= Before andalso Ts =< After, "timestamp must be within the test window").

	% A second update from the same source refreshes its timestamp.
	source_timestamp_updated_on_subsequent_cast_test() ->
		Checks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		ExistingTs = erlang:system_time(second) - 120,
		ExistingState = {
			#{"lucos_foo" => {"host1.example.com", system, #{info => Checks}, #{}, #{}, #{info => ExistingTs}}},
			#{}, [], #{}, fun(_) -> ok end
		},
		Before = erlang:system_time(second),
		{noreply, {SystemMap, _, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, Checks, #{}},
			ExistingState
		),
		After = erlang:system_time(second),
		{"host1.example.com", _, _, _, _, SourceTimestamps} = maps:get("lucos_foo", SystemMap),
		NewTs = maps:get(info, SourceTimestamps),
		?assert(NewTs >= Before andalso NewTs =< After, "timestamp must be refreshed to current time"),
		?assert(NewTs > ExistingTs, "new timestamp must be more recent than the old one").

	% A second source gets its own timestamp entry alongside the first.
	source_timestamp_per_source_independent_test() ->
		InfoChecks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		CIChecks = #{<<"circleci">> => #{<<"ok">> => true}},
		ExistingState = {
			#{"lucos_foo" => {"host1.example.com", system, #{info => InfoChecks}, #{}, #{}, #{info => 1000}}},
			#{}, [], #{}, fun(_) -> ok end
		},
		{noreply, {SystemMap, _, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, circleci, CIChecks, #{}},
			ExistingState
		),
		{"host1.example.com", _, _, _, _, SourceTimestamps} = maps:get("lucos_foo", SystemMap),
		?assert(maps:is_key(info, SourceTimestamps), "info timestamp must be preserved"),
		?assert(maps:is_key(circleci, SourceTimestamps), "circleci timestamp must be added"),
		?assertEqual(1000, maps:get(info, SourceTimestamps), "info timestamp must not be touched by circleci update").

	% computeFreshnessTimestamps: empty map returns {0, 0}.
	compute_freshness_timestamps_empty_test() ->
		?assertEqual({0, 0}, computeFreshnessTimestamps(#{})).

	% computeFreshnessTimestamps: single source returns that timestamp for both max and min.
	compute_freshness_timestamps_single_source_test() ->
		?assertEqual({1000, 1000}, computeFreshnessTimestamps(#{info => 1000})).

	% computeFreshnessTimestamps: multiple sources — max is most recent, min is oldest.
	compute_freshness_timestamps_multiple_sources_test() ->
		Ts = #{info => 1500, circleci => 900, scheduled_jobs => 200},
		{Max, Min} = computeFreshnessTimestamps(Ts),
		?assertEqual(1500, Max),
		?assertEqual(200, Min).

	% buildSystemOutput includes last_updated and oldest_source_ts fields.
	build_system_output_includes_freshness_fields_test() ->
		Now = erlang:system_time(second),
		SourceTimestamps = #{info => Now - 30, circleci => Now - 45},
		Output = buildSystemOutput("host1.example.com", "lucos_foo", system, #{}, #{}, #{}, SourceTimestamps),
		?assert(maps:is_key(<<"last_updated">>, Output), "last_updated must be present"),
		?assert(maps:is_key(<<"oldest_source_ts">>, Output), "oldest_source_ts must be present"),
		?assertEqual(Now - 30, maps:get(<<"last_updated">>, Output), "last_updated must be the most recent timestamp"),
		?assertEqual(Now - 45, maps:get(<<"oldest_source_ts">>, Output), "oldest_source_ts must be the oldest timestamp").

	% buildSystemOutput with no source timestamps produces {0, 0} freshness fields.
	build_system_output_no_timestamps_test() ->
		Output = buildSystemOutput("host1.example.com", "lucos_foo", system, #{}, #{}, #{}, #{}),
		?assertEqual(0, maps:get(<<"last_updated">>, Output)),
		?assertEqual(0, maps:get(<<"oldest_source_ts">>, Output)).

-endif.
