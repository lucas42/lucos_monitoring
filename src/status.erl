-module(status).
-export([annotateCheckStatuses/1, build_system_list/2, computePollStats/1]).
-include("monitoring_state.hrl").

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
		#pending_verification{} ->
			pending_verification;
		#suppression_window{expiry_time = ExpiryTime, pre_existing = PreExisting} when ExpiryTime > Now ->
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
		IsDepSuppressed = depends_on:is_dependency_suppressed(Check, SystemId, SuppressionMap),
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
	maps:fold(fun(SystemId, #system_state{host = Host, system_type = SystemType, normalised_cache = NormalisedCache, metrics = Metrics, source_timestamps = SourceTimestamps}, Acc) ->
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
		?assertEqual(suppressed, computeSystemStatus("host1", "lucos_foo", Cache, #{"lucos_foo" => #suppression_window{expiry_time = FutureExpiry, pre_existing = #{}}})).

	% computeSystemStatus: active suppression with a pre-existing failure still failing →
	% falls through to aggregateCheckStatuses so the system remains visibly failing on
	% the dashboard during the deploy window.
	compute_system_status_active_window_pre_existing_failure_visible_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		Cache = #{<<"a">> => #{<<"ok">> => false, <<"status">> => failing, <<"statusText">> => <<"failing">>}},
		PreExisting = #{"host1" => sets:from_list([<<"a">>], [{version, 2}])},
		?assertEqual(failing, computeSystemStatus("host1", "lucos_foo", Cache, #{"lucos_foo" => #suppression_window{expiry_time = FutureExpiry, pre_existing = PreExisting}})).

	% computeSystemStatus: active suppression where the only currently-failing check is NOT in
	% the pre-existing snapshot → still suppressed (this is exactly the "deploy churn" case
	% the suppression is designed for).
	compute_system_status_active_window_new_failure_only_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		Cache = #{<<"new-check">> => #{<<"ok">> => false, <<"status">> => failing, <<"statusText">> => <<"failing">>}},
		PreExisting = #{"host1" => sets:from_list([<<"some-other-check">>], [{version, 2}])},
		?assertEqual(suppressed, computeSystemStatus("host1", "lucos_foo", Cache, #{"lucos_foo" => #suppression_window{expiry_time = FutureExpiry, pre_existing = PreExisting}})).

	% computeSystemStatus: active suppression where pre-existing-failing has since recovered →
	% suppressed (no continuing problem to surface).
	compute_system_status_active_window_pre_existing_recovered_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		Cache = #{<<"a">> => #{<<"ok">> => true, <<"status">> => healthy, <<"statusText">> => <<"healthy">>}},
		PreExisting = #{"host1" => sets:from_list([<<"a">>], [{version, 2}])},
		?assertEqual(suppressed, computeSystemStatus("host1", "lucos_foo", Cache, #{"lucos_foo" => #suppression_window{expiry_time = FutureExpiry, pre_existing = PreExisting}})).

	% computeSystemStatus: pending_verification → pending_verification
	compute_system_status_pending_verification_test() ->
		PendingSources = sets:from_list([info], [{version, 2}]),
		Cache = #{<<"a">> => #{<<"ok">> => false, <<"status">> => failing, <<"statusText">> => <<"failing">>}},
		?assertEqual(pending_verification, computeSystemStatus("host1", "lucos_foo", Cache, #{"lucos_foo" => #pending_verification{sources = PendingSources}})).

	% computeSystemStatus: failing check inside suppressed system (no pre-existing) — system
	% is suppressed, check is still failing.
	suppressed_system_check_status_honest_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		Cache = #{<<"a">> => #{<<"ok">> => false, <<"status">> => failing, <<"statusText">> => <<"failing">>}},
		SystemStatus = computeSystemStatus("host1", "lucos_foo", Cache, #{"lucos_foo" => #suppression_window{expiry_time = FutureExpiry, pre_existing = #{}}}),
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
		SuppressionMap = #{"lucos_eolas" => #suppression_window{expiry_time = FutureExpiry, pre_existing = #{}}},
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
