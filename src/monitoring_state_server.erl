-module(monitoring_state_server).
-behaviour(gen_server).
-export([start_link/2, init/1, handle_cast/2, handle_call/3]).
-include("monitoring_state.hrl").

start_link(Notifiers, Publish) ->
	gen_server:start_link(?MODULE, {Notifiers, Publish}, []).

init({Notifiers, Publish}) ->
	{ok, {#{}, #{}, Notifiers, #{}, Publish}}.

handle_cast(Request, {SystemMap, SuppressionMap, Notifiers, PollTimings, Publish}) ->
	case Request of
		{updateSystem, Host, System, SystemType, Source, SourceChecks, SystemMetrics} ->
			logger:info("Received update for system ~p (Host ~p, Source ~p)", [System, Host, Source]),
			IsFirstSeen = not maps:is_key(System, SystemMap),
			% The per-system tuple carries, as its final element, a sticky-per-episode
			% `Alerted` boolean: true once a non-suppressed alert email has actually gone
			% out for the current down-episode, reset to false on recovery. It is the only
			% reliable record of "did we tell the user about this?", which neither the current
			% nor previous check cache can answer (a suppressed failure is cached as ok=false
			% but was never emailed). See ADR-0003.
			#system_state{
				source_checks_map = OldSourceChecksMap,
				normalised_cache  = OldNormalisedCache,
				metrics           = OldMetrics,
				source_timestamps = OldSourceTimestamps,
				alerted           = OldAlerted
			} = maps:get(System, SystemMap, #system_state{}),
			NewSourceChecksMap = maps:put(Source, SourceChecks, OldSourceChecksMap),
			NewMergedChecks = check_normalisation:mergeSourceChecks(NewSourceChecksMap),
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
			NormalisedChecks = check_normalisation:normaliseChecks(PrunedOldCache, NewMergedChecks, CountableKeys),
			% Only overwrite metrics when the source provides them; sources
			% without metrics (e.g. circleci) pass #{} and should not wipe
			% metrics previously stored by the info fetcher.
			NewMetrics = case maps:size(SystemMetrics) of
				0 -> OldMetrics;
				_ -> SystemMetrics
			end,
			% Each branch yields {NewSuppressionMap, NewAlerted}: the suppression-window
			% bookkeeping plus the new value of the sticky per-episode Alerted flag.
			{NewSuppressionMap, NewAlerted} = case IsFirstSeen of
				true ->
					logger:notice("Warm-up: skipping alert for ~p on first poll", [System]),
					{SuppressionMap, false};
				false ->
					case maps:get(System, SuppressionMap, undefined) of
						#pending_verification{sources = PendingSources, pre_existing = PreExisting} ->
							% Suppression was recently lifted. Defer the alert decision until
							% all sources have reported fresh post-deploy data.
							Remaining = sets:del_element(Source, PendingSources),
							case sets:size(Remaining) =:= 0 of
								true ->
									FailingNow = check_normalisation:failingChecks(NormalisedChecks),
									WasFailing = check_normalisation:failingChecks(OldNormalisedCache),
									NotificationBase = #{
										host => Host,
										system => System,
										was_failing => WasFailing,
										metrics => NewMetrics
									},
									case maps:size(FailingNow) > 0 of
										true ->
											% Gate the re-alert: only fire if FailingNow contains any
											% check that wasn't already failing before the deploy
											% (i.e., a genuinely new failure). Checks present in
											% pre_existing are unchanged from before the deploy —
											% the user was already told about them; no new email needed.
											% A check born *inside* the deploy window is absent from
											% pre_existing, so it still alerts here (ADR-0003 guarantee
											% preserved). See #277.
											HostPreExisting = maps:get(Host, PreExisting, sets:new([{version, 2}])),
											{_PreExistingFailing, NewlyFailing} = alerting:partitionByPreExisting(FailingNow, HostPreExisting),
											case maps:size(NewlyFailing) > 0 of
												true ->
													logger:notice("Service ~p still unhealthy after deploy — alerting", [System]),
													alerting:notify_all(NotificationBase#{failing_checks => FailingNow, suppressed => false}, Notifiers),
													{maps:remove(System, SuppressionMap), true};
												false ->
													logger:notice("Service ~p still unhealthy after deploy but failing set unchanged — suppressing re-alert", [System]),
													{maps:remove(System, SuppressionMap), OldAlerted}
											end;
										false ->
											% Recovery after deploy: emit the all-clear iff we actually
											% sent an alert for this episode (OldAlerted). This supersedes
											% the prior `map_size(WasFailing) > 0` guard from #252, which
											% also fired for a suppressed-but-never-alerted failure (its
											% ok=false sits in OldNormalisedCache) and so produced an
											% orphaned all-clear (#264). See ADR-0003.
											alerting:maybe_emit_recovery(OldAlerted, NotificationBase, System, Host, Notifiers),
											{maps:remove(System, SuppressionMap), false}
									end;
								false ->
									% Pre_existing is threaded forward so the final verification
									% poll has the correct suppress-time baseline.
									{maps:put(System, #pending_verification{sources = Remaining, pre_existing = PreExisting}, SuppressionMap), OldAlerted}
							end;
						_ ->
							case check_normalisation:meaningfulChange(OldNormalisedCache, NormalisedChecks)
									orelse alerting:windowExpired(System, SuppressionMap) of
								true ->
									SystemContext = #{
										host => Host,
										system => System,
										current_checks => NormalisedChecks,
										was_failing => check_normalisation:failingChecks(OldNormalisedCache),
										metrics => NewMetrics,
										alerted => OldAlerted
									},
									alerting:state_change(SystemContext, SuppressionMap, Notifiers);
								false ->
									{SuppressionMap, OldAlerted}
							end
					end
			end,
			AnnotatedChecks = status:annotateCheckStatuses(NormalisedChecks),
			% Record the current time for this source. Used by the view to render
			% a per-section freshness indicator that reveals data-source-dark states.
			NewSourceTimestamps = maps:put(Source, erlang:system_time(second), OldSourceTimestamps),
			NewSystemMap = maps:put(System, #system_state{
				host              = Host,
				system_type       = SystemType,
				source_checks_map = NewSourceChecksMap,
				normalised_cache  = AnnotatedChecks,
				metrics           = NewMetrics,
				source_timestamps = NewSourceTimestamps,
				alerted           = NewAlerted
			}, SystemMap),
			% Always publish an SSE event on every updateSystem cast.
			% The source timestamps are updated on every poll, and the client uses them
			% to render per-section freshness indicators. Suppressing the event when
			% check state is unchanged would cause client-side age counters to drift
			% until they falsely trigger the stale warning — even though the server
			% is polling correctly and the data is fresh.
			SystemList = status:build_system_list(NewSystemMap, NewSuppressionMap),
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
			SystemList = status:build_system_list(SystemMap, SuppressionMap),
			{reply, SystemList, {SystemMap, SuppressionMap, Notifiers, PollTimings, Publish}};
		{fetch, poll_stats} ->
			Stats = status:computePollStats(maps:values(PollTimings)),
			{reply, Stats, {SystemMap, SuppressionMap, Notifiers, PollTimings, Publish}};
		{suppress, System} ->
			case alerting:systemExists(System, SystemMap) of
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
						(S, #system_state{host = Host, normalised_cache = NormalisedCache}, Acc) when S =:= System ->
							FailingKeys = sets:from_list(maps:keys(check_normalisation:failingChecks(NormalisedCache)), [{version, 2}]),
							maps:put(Host, FailingKeys, Acc);
						(_, _, Acc) -> Acc
					end, #{}, SystemMap),
					NewSuppressionMap = maps:put(System, #suppression_window{expiry_time = ExpiryTime, pre_existing = PreExisting}, SuppressionMap),
					logger:notice("Suppression window opened for ~p", [System]),
					SystemList = status:build_system_list(SystemMap, NewSuppressionMap),
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
					Sources = alerting:collect_active_sources(System, SystemMap),
					% Thread pre_existing from the closing suppression window so the
					% post-deploy alert gate can compare against the suppress-time
					% snapshot (checks already failing before the deploy are not
					% re-alerted if the set is unchanged). Falls back to #{} for any
					% non-window entry (e.g. a double-unsuppress), which treats all
					% failures as new — correct conservative default.
					WindowPreExisting = case maps:get(System, SuppressionMap) of
						#suppression_window{pre_existing = PE} -> PE;
						_ -> #{}
					end,
					NewSuppressionMap = maps:put(System, #pending_verification{sources = Sources, pre_existing = WindowPreExisting}, SuppressionMap),
					logger:notice("Suppression window closed for ~p — awaiting verification poll", [System]),
					% Cascade pending_verification to systems that have checks depending on this system.
					% Single-hop only: we do not follow dependsOn chains transitively.
					DependentSystems = depends_on:find_dependent_systems(System, SystemMap),
					FinalSuppressionMap = lists:foldl(fun(DepSystem, SM) ->
						DepSources = alerting:collect_active_sources(DepSystem, SystemMap),
						logger:notice("Cascading pending_verification to ~p (has checks depending on ~p)", [DepSystem, System]),
						maps:put(DepSystem, #pending_verification{sources = DepSources}, SM)
					end, NewSuppressionMap, DependentSystems),
					SystemList = status:build_system_list(SystemMap, FinalSuppressionMap),
					Publish(SystemList),
					{reply, ok, {SystemMap, FinalSuppressionMap, Notifiers, PollTimings, Publish}};
				false ->
					logger:notice("Suppression window closed for ~p (was not suppressed)", [System]),
					{reply, ok, {SystemMap, SuppressionMap, Notifiers, PollTimings, Publish}}
			end
	end.

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").

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
		#system_state{source_checks_map = SourceChecksMap} = maps:get("lucos_foo", SystemMap),
		StoredChecks = check_normalisation:mergeSourceChecks(SourceChecksMap),
		?assertEqual(false, maps:get(<<"ok">>, maps:get(<<"fetch-info">>, StoredChecks))).

	% Second update for a known system triggers normal alert logic (not warm-up).
	% Here both updates report the same healthy state, so no meaningful change — no alert.
	warmup_second_update_not_suppressed_test() ->
		Checks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		ExistingState = {
			#{"lucos_foo" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => Checks}}},
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
			#{"lucos_foo" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => InfoChecks}}},
			#{},
			[],
			#{}, fun(_) -> ok end
		},
		% circleci update arrives
		{noreply, {SystemMap, _, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, circleci, CIChecks, #{}},
			ExistingState
		),
		#system_state{source_checks_map = SourceChecksMap} = maps:get("lucos_foo", SystemMap),
		Merged = check_normalisation:mergeSourceChecks(SourceChecksMap),
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
			#{"lucos_foo" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => OldInfoChecks}}},
			#{},
			[],
			#{}, fun(_) -> ok end
		},
		{noreply, {SystemMap, _, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, NewInfoChecks, #{}},
			ExistingState
		),
		#system_state{source_checks_map = SourceChecksMap} = maps:get("lucos_foo", SystemMap),
		Merged = check_normalisation:mergeSourceChecks(SourceChecksMap),
		?assertNot(maps:is_key(<<"custom-check">>, Merged)).

	% When a source with no metrics (e.g. circleci) updates a system that already
	% has metrics from the info fetcher, the existing metrics are preserved.
	empty_metrics_do_not_overwrite_test() ->
		InfoChecks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		Metrics = #{<<"agent-count">> => #{<<"value">> => 42, <<"techDetail">> => <<"count">>}},
		ExistingState = {
			#{"lucos_foo" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => InfoChecks}, metrics=Metrics}},
			#{},
			[],
			#{}, fun(_) -> ok end
		},
		CIChecks = #{<<"circleci">> => #{<<"ok">> => true}},
		{noreply, {SystemMap, _, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, circleci, CIChecks, #{}},
			ExistingState
		),
		#system_state{metrics = StoredMetrics} = maps:get("lucos_foo", SystemMap),
		?assertEqual(Metrics, StoredMetrics).

	% When a source provides non-empty metrics, they replace the existing ones.
	nonempty_metrics_do_overwrite_test() ->
		OldMetrics = #{<<"agent-count">> => #{<<"value">> => 42, <<"techDetail">> => <<"count">>}},
		NewMetrics = #{<<"agent-count">> => #{<<"value">> => 99, <<"techDetail">> => <<"count">>}},
		ExistingState = {
			#{"lucos_foo" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => #{<<"fetch-info">> => #{<<"ok">> => true}}}, metrics=OldMetrics}},
			#{},
			[],
			#{}, fun(_) -> ok end
		},
		{noreply, {SystemMap, _, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, #{<<"fetch-info">> => #{<<"ok">> => true}}, NewMetrics},
			ExistingState
		),
		#system_state{metrics = StoredMetrics} = maps:get("lucos_foo", SystemMap),
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
		SystemMap = #{"lucos_foo" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => HealthyChecks}}},
		Notifier = recording_notifier(self()),
		State = {SystemMap, #{"lucos_foo" => #suppression_window{expiry_time = erlang:system_time(second) + 600, pre_existing = #{}}}, [Notifier], #{}, fun(_) -> ok end},
		{reply, ok, {_, NewSuppressionMap, _, _, _}} = handle_call(
			{unsuppress, "lucos_foo"}, from, State
		),
		% System should be in pending_verification, not removed from the map
		?assertMatch(#pending_verification{}, maps:get("lucos_foo", NewSuppressionMap)),
		receive
			{notified, _} -> ?assert(false, "Unexpected alert fired for healthy system")
		after 100 ->
			ok  % No immediate notification — correct
		end.

	% Unsuppressing an unhealthy system enters pending_verification; alert is deferred, not immediate.
	unsuppress_unhealthy_system_test() ->
		FailingChecks = #{<<"fetch-info">> => #{<<"ok">> => false}},
		SystemMap = #{"lucos_foo" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => FailingChecks}}},
		Notifier = recording_notifier(self()),
		State = {SystemMap, #{"lucos_foo" => #suppression_window{expiry_time = erlang:system_time(second) + 600, pre_existing = #{}}}, [Notifier], #{}, fun(_) -> ok end},
		drain_notifications(),
		{reply, ok, {_, NewSuppressionMap, _, _, _}} = handle_call(
			{unsuppress, "lucos_foo"}, from, State
		),
		% System should be in pending_verification (not cleared)
		?assertMatch(#pending_verification{}, maps:get("lucos_foo", NewSuppressionMap)),
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
		SystemMap = #{"lucos_foo" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => FailingChecks}}},
		PendingSources = sets:from_list([info], [{version, 2}]),
		Notifier = recording_notifier(self()),
		drain_notifications(),
		State = {SystemMap, #{"lucos_foo" => #pending_verification{sources = PendingSources}}, [Notifier], #{}, fun(_) -> ok end},
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
		SystemMap = #{"lucos_foo" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => HealthyChecks}}},
		PendingSources = sets:from_list([info], [{version, 2}]),
		Notifier = recording_notifier(self()),
		drain_notifications(),
		State = {SystemMap, #{"lucos_foo" => #pending_verification{sources = PendingSources}}, [Notifier], #{}, fun(_) -> ok end},
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

	% After unsuppress, a fresh poll reporting healthy emits a recovery if we had actually
	% alerted for this episode (Alerted=true in the per-system tuple). Recovery is now gated
	% on the sticky `alerted` flag, not on "was the prior cache failing" — see ADR-0003 / #264.
	pending_verification_recovery_emitted_when_episode_was_alerted_test() ->
		HealthyChecks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		FailingNormalisedCache = #{<<"fetch-info">> => #{<<"ok">> => false, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 1}},
		% Prior cache had a failure AND we alerted (7th tuple element = true) — recovery must fire.
		SystemMap = #{"lucos_foo" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => HealthyChecks}, normalised_cache=FailingNormalisedCache, alerted=true}},
		PendingSources = sets:from_list([info], [{version, 2}]),
		Notifier = recording_notifier(self()),
		drain_notifications(),
		State = {SystemMap, #{"lucos_foo" => #pending_verification{sources = PendingSources}}, [Notifier], #{}, fun(_) -> ok end},
		{noreply, {NewSystemMap, NewSuppressionMap, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, HealthyChecks, #{}},
			State
		),
		% Suppression entry should be cleared
		?assertEqual(#{}, NewSuppressionMap),
		% Alerted flag must reset to false once the episode recovers.
		?assertMatch(#system_state{alerted = false}, maps:get("lucos_foo", NewSystemMap)),
		receive
			{notified, #{host := "host1.example.com", system := "lucos_foo", failing_checks := FailingNow, suppressed := false}} ->
				?assertEqual(#{}, FailingNow, "Recovery must be emitted with empty failing checks")
		after 100 ->
			?assert(false, "Expected monitoringRecovery was not emitted after deploy when episode was alerted")
		end.

	% Orphan-recovery regression (#264): a prior failure that was suppressed-but-never-alerted
	% (Alerted=false) must NOT produce an all-clear when it recovers, even though the prior cache
	% holds it as ok=false. This is exactly the case #252's `prevFailing` guard got wrong.
	pending_verification_no_recovery_when_prior_failure_was_never_alerted_test() ->
		HealthyChecks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		FailingNormalisedCache = #{<<"fetch-info">> => #{<<"ok">> => false, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 1}},
		% Prior cache had a failure but we never alerted (7th tuple element = false, e.g. it was
		% dependency- or deploy-window-suppressed) — no orphaned all-clear may be sent.
		SystemMap = #{"lucos_foo" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => HealthyChecks}, normalised_cache=FailingNormalisedCache}},
		PendingSources = sets:from_list([info], [{version, 2}]),
		Notifier = recording_notifier(self()),
		drain_notifications(),
		State = {SystemMap, #{"lucos_foo" => #pending_verification{sources = PendingSources}}, [Notifier], #{}, fun(_) -> ok end},
		{noreply, {_, NewSuppressionMap, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, HealthyChecks, #{}},
			State
		),
		?assertEqual(#{}, NewSuppressionMap),
		receive
			{notified, _} -> ?assert(false, "Orphaned all-clear must not be sent for a suppressed-but-never-alerted failure")
		after 100 ->
			ok
		end.

	% End-to-end orphan regression (#264), mirroring the production trace for
	% lucos_media_weightings on 2026-05-30: a check whose dependsOn upstream is under an
	% active suppression window fails (dependency-suppressed → no alert email), then recovers
	% once the upstream deploy finishes. The recovery must send nothing, because we never
	% alerted. Before the fix, this produced an orphaned "Everything OK" email.
	dependency_suppressed_failure_recovers_without_orphan_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_media_metadata_api" => #suppression_window{expiry_time = FutureExpiry, pre_existing = #{}}},
		FailingCheck = #{<<"media-api-reachable">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_media_metadata_api">>}},
		HealthyCheck = #{<<"media-api-reachable">> => #{<<"ok">> => true, <<"dependsOn">> => <<"lucos_media_metadata_api">>}},
		Notifier = recording_notifier(self()),
		drain_notifications(),
		State0 = {#{}, SuppressionMap, [Notifier], #{}, fun(_) -> ok end},
		% Poll 0: first sighting, healthy → warm-up (no notification).
		{noreply, State1} = handle_cast(
			{updateSystem, "media-weighting.l42.eu", "lucos_media_weightings", system, info, HealthyCheck, #{}}, State0),
		% Poll 1: the dependent check fails while upstream is suppressed → suppressed notification, never alerted.
		{noreply, {SystemMap2, _, _, _, _} = State2} = handle_cast(
			{updateSystem, "media-weighting.l42.eu", "lucos_media_weightings", system, info, FailingCheck, #{}}, State1),
		?assertMatch(#system_state{alerted = false}, maps:get("lucos_media_weightings", SystemMap2)),
		receive
			{notified, #{suppressed := true}} -> ok
		after 100 ->
			?assert(false, "Expected a suppressed notification for the dependency-suppressed failure")
		end,
		% Poll 2: the check recovers → no orphaned all-clear (we never alerted).
		{noreply, {SystemMap3, _, _, _, _}} = handle_cast(
			{updateSystem, "media-weighting.l42.eu", "lucos_media_weightings", system, info, HealthyCheck, #{}}, State2),
		?assertMatch(#system_state{alerted = false}, maps:get("lucos_media_weightings", SystemMap3)),
		receive
			{notified, _} -> ?assert(false, "Orphaned all-clear must not be emitted for a dependency-suppressed recovery (#264)")
		after 100 ->
			ok
		end.

	% Guards the normal path: a genuine (non-suppressed) failure still alerts, sets the sticky
	% `alerted` flag, and its recovery still sends the all-clear and resets the flag.
	real_failure_then_recovery_alerts_then_all_clears_test() ->
		FailingCheck = #{<<"db">> => #{<<"ok">> => false}},
		HealthyCheck = #{<<"db">> => #{<<"ok">> => true}},
		Notifier = recording_notifier(self()),
		drain_notifications(),
		State0 = {#{}, #{}, [Notifier], #{}, fun(_) -> ok end},
		% Poll 0: first sighting, healthy → warm-up (no notification).
		{noreply, State1} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, HealthyCheck, #{}}, State0),
		% Poll 1: real failure → unsuppressed alert, alerted flag set true.
		{noreply, {SystemMap2, _, _, _, _} = State2} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, FailingCheck, #{}}, State1),
		?assertMatch(#system_state{alerted = true}, maps:get("lucos_foo", SystemMap2)),
		receive
			{notified, #{failing_checks := F1, suppressed := false}} -> ?assert(maps:is_key(<<"db">>, F1))
		after 100 ->
			?assert(false, "Expected an alert for the real failure")
		end,
		% Poll 2: recovery → all-clear emitted, alerted reset to false.
		{noreply, {SystemMap3, _, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, HealthyCheck, #{}}, State2),
		?assertMatch(#system_state{alerted = false}, maps:get("lucos_foo", SystemMap3)),
		receive
			{notified, #{failing_checks := F2, suppressed := false}} -> ?assertEqual(#{}, F2, "Recovery must carry empty failing checks")
		after 100 ->
			?assert(false, "Expected an all-clear after the real recovery")
		end.

	% With two sources pending, the first poll keeps pending state; the second evaluates.
	pending_verification_waits_for_all_sources_test() ->
		FailingChecks = #{<<"fetch-info">> => #{<<"ok">> => false}},
		CIChecks = #{<<"circleci">> => #{<<"ok">> => false}},
		SystemMap = #{
			"lucos_foo" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => FailingChecks, circleci => CIChecks}}
		},
		PendingSources = sets:from_list([info, circleci], [{version, 2}]),
		Notifier = recording_notifier(self()),
		drain_notifications(),
		State = {SystemMap, #{"lucos_foo" => #pending_verification{sources = PendingSources}}, [Notifier], #{}, fun(_) -> ok end},
		% First source (info) reports — should still be pending
		{noreply, State2} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, FailingChecks, #{}},
			State
		),
		{_, SuppressionMap2, _, _, _} = State2,
		?assertMatch(#pending_verification{}, maps:get("lucos_foo", SuppressionMap2)),
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

	% Regression (#277): when the post-deploy failing set is identical to the pre-deploy
	% snapshot, the re-alert is suppressed.  The user was already told about these failures
	% before the deploy; no new email is needed.  The episode stays open (alerted=true, no
	% recovery sent), so a genuine recovery still fires the all-clear.
	pending_verification_unchanged_failing_set_suppresses_re_alert_test() ->
		FailingChecks = #{<<"create-backups">> => #{<<"ok">> => false}},
		% pre_existing mirrors what was failing at suppress-time: same check key.
		PreExistingKeys = sets:from_list([<<"create-backups">>], [{version, 2}]),
		PreExisting = #{"host1.example.com" => PreExistingKeys},
		SystemMap = #{"lucos_backups" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => FailingChecks}, alerted=true}},
		PendingSources = sets:from_list([info], [{version, 2}]),
		Notifier = recording_notifier(self()),
		drain_notifications(),
		State = {SystemMap, #{"lucos_backups" => #pending_verification{sources = PendingSources, pre_existing = PreExisting}}, [Notifier], #{}, fun(_) -> ok end},
		{noreply, {NewSystemMap, NewSuppressionMap, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_backups", system, info, FailingChecks, #{}},
			State
		),
		% Suppression entry should be cleared
		?assertEqual(#{}, NewSuppressionMap),
		% alerted flag must remain true — the episode is still open, recovery not yet sent
		?assertMatch(#system_state{alerted = true}, maps:get("lucos_backups", NewSystemMap)),
		receive
			{notified, _} -> ?assert(false, "No re-alert expected when failing set is unchanged after deploy")
		after 100 ->
			ok
		end.

	% Regression (#277): a deploy that introduces a genuinely new failing check still
	% alerts, even when pre-existing failures are also present.
	pending_verification_new_failure_in_set_still_alerts_test() ->
		ExistingFailure = #{<<"create-backups">> => #{<<"ok">> => false}},
		NewFailure = #{<<"new-check">> => #{<<"ok">> => false}},
		FailingChecks = maps:merge(ExistingFailure, NewFailure),
		% pre_existing only covers the check that was failing before the deploy.
		PreExistingKeys = sets:from_list([<<"create-backups">>], [{version, 2}]),
		PreExisting = #{"host1.example.com" => PreExistingKeys},
		SystemMap = #{"lucos_backups" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => FailingChecks}, alerted=true}},
		PendingSources = sets:from_list([info], [{version, 2}]),
		Notifier = recording_notifier(self()),
		drain_notifications(),
		State = {SystemMap, #{"lucos_backups" => #pending_verification{sources = PendingSources, pre_existing = PreExisting}}, [Notifier], #{}, fun(_) -> ok end},
		{noreply, {_, NewSuppressionMap, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_backups", system, info, FailingChecks, #{}},
			State
		),
		% Suppression entry should be cleared
		?assertEqual(#{}, NewSuppressionMap),
		receive
			{notified, #{host := "host1.example.com", system := "lucos_backups", failing_checks := FailingNow, suppressed := false}} ->
				?assert(maps:is_key(<<"new-check">>, FailingNow), "Alert must include the new failure")
		after 100 ->
			?assert(false, "Expected alert for new failure introduced by deploy")
		end.

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
			#{"lucos_foo" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => InfoChecks, circleci => CIChecks}, normalised_cache=ExistingNormalisedCache}},
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
		#system_state{normalised_cache = NormalisedAfterCI404} = maps:get("lucos_foo", SystemMap3),
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
			#{"lucos_foo" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => InfoChecks}, normalised_cache=ExistingNormalisedCache}},
			#{},
			[],
			#{}, fun(_) -> ok end
		},
		{noreply, State2} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, #{<<"fetch-info">> => #{<<"ok">> => unknown}}, #{}},
			ExistingState
		),
		{SystemMap2, _, _, _, _} = State2,
		#system_state{normalised_cache = NormalisedAfterBlip} = maps:get("lucos_foo", SystemMap2),
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
			#{"lucos_foo" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => InfoChecks, circleci => CIChecks}}},
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
		#system_state{normalised_cache = NormalisedAfterCI} = maps:get("lucos_foo", SystemMap2),
		?assertEqual(1, maps:get(<<"consecutiveUnknownsCount">>, maps:get(<<"circleci">>, NormalisedAfterCI, #{}), -1)),
		% Now info reports (ok, no change) — circleci check is carried over in the merged view
		{noreply, State3} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, InfoChecks, #{}},
			State2
		),
		{SystemMap3, _, _, _, _} = State3,
		#system_state{normalised_cache = NormalisedAfterInfo} = maps:get("lucos_foo", SystemMap3),
		% circleci count must still be 1, NOT 2 — info's update must not re-increment it
		?assertEqual(1, maps:get(<<"consecutiveUnknownsCount">>, maps:get(<<"circleci">>, NormalisedAfterInfo, #{}), -1)).

	% unsuppress cascades pending_verification to systems with checks depending on the unsuppressed system.
	unsuppress_cascades_pending_verification_test() ->
		TimeChecks = #{<<"eolas">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>}},
		SystemMap = #{
			"lucos_eolas" => #system_state{host="", system_type=system, source_checks_map=#{info => #{<<"fetch-info">> => #{<<"ok">> => true}}}},
			"lucos_time" => #system_state{host="schedule-tracker.l42.eu", system_type=system, source_checks_map=#{info => TimeChecks}}
		},
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_eolas" => #suppression_window{expiry_time = FutureExpiry, pre_existing = #{}}},
		State = {SystemMap, SuppressionMap, [], #{}, fun(_) -> ok end},
		{reply, ok, {_, NewSuppressionMap, _, _, _}} = handle_call(
			{unsuppress, "lucos_eolas"}, from, State
		),
		?assertMatch(#pending_verification{}, maps:get("lucos_eolas", NewSuppressionMap)),
		?assertMatch(#pending_verification{}, maps:get("lucos_time", NewSuppressionMap)).

	% suppress: snapshot pre-existing failing checks at suppress-time. A subsequent
	% state_change with the same checks should split the alert into a Suppressed=false
	% notification (continuing problem) — NOT collapse to a single Suppressed=true.
	suppress_snapshots_pre_existing_failures_test() ->
		FailingChecks = #{<<"host-tracking-failures">> => #{<<"ok">> => false, <<"consecutiveFailsCount">> => 1, <<"consecutiveUnknownsCount">> => 0}},
		AnnotatedFailing = status:annotateCheckStatuses(FailingChecks),
		SystemMap = #{"lucos_foo" => #system_state{host="host1", system_type=system, source_checks_map=#{info => FailingChecks}, normalised_cache=AnnotatedFailing}},
		Notifier = recording_notifier(self()),
		drain_notifications(),
		State = {SystemMap, #{}, [Notifier], #{}, fun(_) -> ok end},
		{reply, ok, {_SM, NewSuppressionMap, _, _, _}} = handle_call({suppress, "lucos_foo"}, from, State),
		% Snapshot must contain the host-tracking-failures key under "host1"
		#suppression_window{expiry_time = ExpiryTime, pre_existing = PreExisting} = maps:get("lucos_foo", NewSuppressionMap),
		?assert(is_integer(ExpiryTime)),
		?assertMatch(#{"host1" := _}, PreExisting),
		HostKeys = maps:get("host1", PreExisting),
		?assert(sets:is_element(<<"host-tracking-failures">>, HostKeys)).

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
		ExistingAnnotated = status:annotateCheckStatuses(check_normalisation:normaliseChecks(#{}, Checks1, sets:new([{version, 2}]))),
		ExistingState = {
			#{"lucos_foo" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => Checks1}, normalised_cache=ExistingAnnotated}},
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
		ExistingAnnotated = status:annotateCheckStatuses(check_normalisation:normaliseChecks(#{}, Checks, sets:new([{version, 2}]))),
		ExistingState = {
			#{"lucos_foo" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => Checks}, normalised_cache=ExistingAnnotated}},
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
		#system_state{source_timestamps = SourceTimestamps} = maps:get("lucos_foo", SystemMap),
		?assert(maps:is_key(info, SourceTimestamps), "info source must have a timestamp entry"),
		Ts = maps:get(info, SourceTimestamps),
		?assert(Ts >= Before andalso Ts =< After, "timestamp must be within the test window").

	% A second update from the same source refreshes its timestamp.
	source_timestamp_updated_on_subsequent_cast_test() ->
		Checks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		ExistingTs = erlang:system_time(second) - 120,
		ExistingState = {
			#{"lucos_foo" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => Checks}, source_timestamps=#{info => ExistingTs}}},
			#{}, [], #{}, fun(_) -> ok end
		},
		Before = erlang:system_time(second),
		{noreply, {SystemMap, _, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, Checks, #{}},
			ExistingState
		),
		After = erlang:system_time(second),
		#system_state{source_timestamps = SourceTimestamps} = maps:get("lucos_foo", SystemMap),
		NewTs = maps:get(info, SourceTimestamps),
		?assert(NewTs >= Before andalso NewTs =< After, "timestamp must be refreshed to current time"),
		?assert(NewTs > ExistingTs, "new timestamp must be more recent than the old one").

	% A second source gets its own timestamp entry alongside the first.
	source_timestamp_per_source_independent_test() ->
		InfoChecks = #{<<"fetch-info">> => #{<<"ok">> => true}},
		CIChecks = #{<<"circleci">> => #{<<"ok">> => true}},
		ExistingState = {
			#{"lucos_foo" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => InfoChecks}, source_timestamps=#{info => 1000}}},
			#{}, [], #{}, fun(_) -> ok end
		},
		{noreply, {SystemMap, _, _, _, _}} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, circleci, CIChecks, #{}},
			ExistingState
		),
		#system_state{source_timestamps = SourceTimestamps} = maps:get("lucos_foo", SystemMap),
		?assert(maps:is_key(info, SourceTimestamps), "info timestamp must be preserved"),
		?assert(maps:is_key(circleci, SourceTimestamps), "circleci timestamp must be added"),
		?assertEqual(1000, maps:get(info, SourceTimestamps), "info timestamp must not be touched by circleci update").

	% Regression: a failure that begins inside a deploy window and persists unchanged past
	% the 10-minute timeout must alert on the first poll after window expiry (#266).
	%
	% Mechanism: windowExpired/2 extends the meaningfulChange gate so an expired-but-uncleared
	% window forces re-evaluation through state_change. state_change's own expired branch then
	% removes the window entry via maps:remove, making this fire exactly once.
	expired_deploy_window_fires_alert_on_next_poll_test() ->
		FailingChecks = #{<<"fetch-info">> => #{<<"ok">> => false}},
		% NormalisedCache simulates what was stored during the window (failing, but suppressed).
		FailingNormalisedCache = #{<<"fetch-info">> => #{<<"ok">> => false, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 1}},
		PastExpiry = erlang:system_time(second) - 1,
		% System is already known (second-poll semantics) with the failing check cached.
		SystemMap = #{"lucos_foo" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => FailingChecks}, normalised_cache=FailingNormalisedCache}},
		% Suppression window is expired (ExpiryTime in the past, no pre-existing failures).
		SuppressionMap = #{"lucos_foo" => #suppression_window{expiry_time = PastExpiry, pre_existing = #{}}},
		Notifier = recording_notifier(self()),
		drain_notifications(),
		State = {SystemMap, SuppressionMap, [Notifier], #{}, fun(_) -> ok end},
		% Poll 1: same failing check, unchanged set — meaningfulChange is false, but window is
		% expired → windowExpired forces re-evaluation. Alert must fire unsuppressed.
		{noreply, {SystemMap2, NewSuppressionMap, _, _, _} = State2} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, FailingChecks, #{}},
			State
		),
		% (a) alert fires unsuppressed
		receive
			{notified, #{host := "host1.example.com", system := "lucos_foo", failing_checks := FailingNow, suppressed := false}} ->
				?assert(maps:is_key(<<"fetch-info">>, FailingNow))
		after 100 ->
			?assert(false, "Expected unsuppressed alert on first poll after window expiry")
		end,
		% (b) alerted flag is now set
		?assertMatch(#system_state{alerted = true}, maps:get("lucos_foo", SystemMap2)),
		% (c) window entry removed — so the next poll cannot re-fire via windowExpired
		?assertEqual(#{}, NewSuppressionMap),
		% Poll 2: same failing check, window now absent — must NOT re-alert (self-limiting).
		{noreply, _} = handle_cast(
			{updateSystem, "host1.example.com", "lucos_foo", system, info, FailingChecks, #{}},
			State2
		),
		receive
			{notified, _} -> ?assert(false, "Second poll must not re-alert after window is removed")
		after 100 ->
			ok
		end.

	% --- ADR-0004: monitoring-authored dependsOn on synthetic probes ---
	% Closing lucos_router's deploy window fans pending_verification across every system
	% whose checks carry dependsOn: [lucos_router, lucos_dns] (the new list shape stamped
	% by make_direct_probe_check/1 in fetcher_info.erl).  This is the estate-wide cascade
	% described in ADR-0004 and the second read site of dependsOn (find_dependent_systems/2).
	unsuppress_router_cascades_estate_wide_pending_verification_test() ->
		RouterDnsDepChecks = #{
			<<"fetch-info">>      => #{<<"ok">> => true, <<"dependsOn">> => [<<"lucos_router">>, <<"lucos_dns">>]},
			<<"tls-certificate">> => #{<<"ok">> => true, <<"dependsOn">> => [<<"lucos_router">>, <<"lucos_dns">>]}
		},
		SystemMap = #{
			"lucos_router"  => #system_state{host="router.l42.eu", system_type=system,
			                                  source_checks_map=#{info => RouterDnsDepChecks}},
			"lucos_photos"  => #system_state{host="photos.l42.eu", system_type=system,
			                                  source_checks_map=#{info => RouterDnsDepChecks}},
			"lucos_arachne" => #system_state{host="arachne.l42.eu", system_type=system,
			                                  source_checks_map=#{info => RouterDnsDepChecks}}
		},
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_router" => #suppression_window{expiry_time = FutureExpiry, pre_existing = #{}}},
		State = {SystemMap, SuppressionMap, [], #{}, fun(_) -> ok end},
		{reply, ok, {_, NewSuppressionMap, _, _, _}} = handle_call(
			{unsuppress, "lucos_router"}, from, State
		),
		% lucos_router itself enters pending_verification
		?assertMatch(#pending_verification{}, maps:get("lucos_router", NewSuppressionMap)),
		% All dependent systems (fetch-info / tls-certificate carry dependsOn the router) also
		% enter pending_verification — estate-wide cascade.
		?assertMatch(#pending_verification{}, maps:get("lucos_photos",  NewSuppressionMap)),
		?assertMatch(#pending_verification{}, maps:get("lucos_arachne", NewSuppressionMap)).

	% After being swept into pending_verification by the estate-wide cascade (see above),
	% a healthy system's next poll must clear the pending state without alerting.
	% This is the "one poll interval" resolution: fresh data shows the system is fine.
	unsuppress_cascade_healthy_system_clears_without_alert_test() ->
		HealthyChecks = #{
			<<"fetch-info">>      => #{<<"ok">> => true, <<"dependsOn">> => [<<"lucos_router">>, <<"lucos_dns">>]},
			<<"tls-certificate">> => #{<<"ok">> => true, <<"dependsOn">> => [<<"lucos_router">>, <<"lucos_dns">>]}
		},
		SystemMap = #{"lucos_photos" => #system_state{host="photos.l42.eu", system_type=system, source_checks_map=#{info => HealthyChecks}}},
		PendingSources = sets:from_list([info], [{version, 2}]),
		Notifier = recording_notifier(self()),
		drain_notifications(),
		State = {SystemMap, #{"lucos_photos" => #pending_verification{sources = PendingSources}}, [Notifier], #{}, fun(_) -> ok end},
		{noreply, {_, NewSuppressionMap, _, _, _}} = handle_cast(
			{updateSystem, "photos.l42.eu", "lucos_photos", system, info, HealthyChecks, #{}},
			State
		),
		% pending_verification must be cleared after the healthy post-cascade poll
		?assertEqual(#{}, NewSuppressionMap),
		% No alert must fire — the system was healthy all along
		receive
			{notified, _} -> ?assert(false, "No alert expected for a healthy system clearing pending_verification after cascade")
		after 100 ->
			ok
		end.
-endif.
