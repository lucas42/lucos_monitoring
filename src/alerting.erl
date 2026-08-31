-module(alerting).
-export([systemExists/2, collect_active_sources/2, windowExpired/2, state_change/3, maybe_emit_recovery/5, partitionByPreExisting/2, notify_all/2]).
-include("monitoring_state.hrl").

systemExists(System, SystemMap) ->
	maps:is_key(System, SystemMap).

% Returns the set of source keys that have reported for a given system,
% across all hosts. Used to build the PendingSources set on unsuppress.
collect_active_sources(System, SystemMap) ->
	maps:fold(fun
		(S, #system_state{source_checks_map = SourceChecksMap}, Acc) when S =:= System ->
			sets:union(Acc, sets:from_list(maps:keys(SourceChecksMap)));
		(_, _, Acc) -> Acc
	end, sets:new([{version, 2}]), SystemMap).

% Returns true when the system has an expired suppression window in SuppressionMap
% (ExpiryTime =< now). Returns false for undefined or pending_verification entries.
% Used to force re-evaluation on the poll immediately after a deploy window expires
% by timeout, even when the failing-check set has not changed (which would normally
% suppress the meaningfulChange gate). The existing expired-window branch inside
% state_change removes the entry via maps:remove, making this fire exactly once.
windowExpired(System, SuppressionMap) ->
	case maps:get(System, SuppressionMap, undefined) of
		#suppression_window{expiry_time = ExpiryTime} ->
			erlang:system_time(second) >= ExpiryTime;
		_ ->
			false
	end.

% Calls every notifier in Notifiers, catching any errors so a single
% failing notifier cannot prevent the others from running.
%
% Notification is a map carrying everything a notifier needs to emit one event:
% host, system, failing_checks, was_failing, suppressed, metrics.
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
%   host, system, current_checks, was_failing, metrics, alerted.
% `alerted` is the sticky per-episode "did we email an alert?" flag (see ADR-0003).
% state_change returns {NewSuppressionMap, NewAlerted}: the suppression bookkeeping
% plus the updated flag. SuppressionMap is a separate arg because state_change can
% return a modified copy; bundling it would force unwrap-on-return at callers.
state_change(SystemContext, SuppressionMap, Notifiers) ->
	#{
		host := Host,
		system := System,
		current_checks := SystemChecks,
		was_failing := WasFailing,
		metrics := SystemMetrics,
		alerted := OldAlerted
	} = SystemContext,
	AllFailing = check_normalisation:failingChecks(SystemChecks),
	% Filter out checks whose dependsOn system is currently under an active suppression window.
	% Single-hop only: we never follow dependsOn chains transitively.
	FailingNow = maps:filter(fun(_, Check) ->
		not depends_on:is_dependency_suppressed(Check, System, SuppressionMap)
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
			% A suppressed notification does NOT count as having alerted the user, so the flag
			% is carried forward unchanged (an earlier real alert in this episode still owes a
			% recovery; a suppressed-only episode stays un-alerted).
			logger:notice("All failing checks on ~p suppressed via dependency: ~p", [System, maps:keys(AllFailing)]),
			notify_all(NotificationBase#{failing_checks => AllFailing, suppressed => true}, Notifiers),
			{SuppressionMap, OldAlerted};
		false ->
			% FailingNow contains only non-dep-suppressed checks. Apply system-level suppression logic.
			case maps:get(System, SuppressionMap, undefined) of
				undefined ->
					case maps:size(FailingNow) > 0 of
						true ->
							% A non-suppressed alert email goes out → mark the episode alerted.
							logger:notice("Checks' state changed for ~p on ~p", [System, Host]),
							notify_all(NotificationBase#{failing_checks => FailingNow, suppressed => false}, Notifiers),
							{SuppressionMap, true};
						false ->
							% Fully healthy → recovery. Emit the all-clear iff we alerted this
							% episode (OldAlerted). Supersedes #252's prevFailing guard; see ADR-0003.
							maybe_emit_recovery(OldAlerted, NotificationBase, System, Host, Notifiers),
							{SuppressionMap, false}
					end;
				#suppression_window{pre_existing = PreExisting} ->
					case not windowExpired(System, SuppressionMap) of
						true ->
							% Partition FailingNow against the pre-existing snapshot for this Host:
							%   - PreExistingFailing: failing-already-at-suppress-time → continuing problem,
							%     alert as Suppressed=false
							%   - NewlyFailing: became unhealthy during the window → likely deploy churn,
							%     alert as Suppressed=true (the existing suppression rationale)
							% This restores the visual distinction between continuing failures and
							% post-deploy flap in the Loganne event stream. This branch only ever
							% *emits* alerts (never a recovery), so the episode becomes alerted iff a
							% non-suppressed (pre-existing) alert went out.
							HostPreExisting = maps:get(Host, PreExisting, sets:new([{version, 2}])),
							{PreExistingFailing, NewlyFailing} = partitionByPreExisting(FailingNow, HostPreExisting),
							PreExistingAlerted = maps:size(PreExistingFailing) > 0,
							case PreExistingAlerted of
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
							{SuppressionMap, OldAlerted orelse PreExistingAlerted};
						false ->
							logger:error("Suppression window for ~p expired without being cleared - deploy may have taken longer than 10 minutes", [System]),
							case maps:size(FailingNow) > 0 of
								true ->
									logger:notice("Checks' state changed for ~p on ~p", [System, Host]),
									notify_all(NotificationBase#{failing_checks => FailingNow, suppressed => false}, Notifiers),
									{maps:remove(System, SuppressionMap), true};
								false ->
									maybe_emit_recovery(OldAlerted, NotificationBase, System, Host, Notifiers),
									{maps:remove(System, SuppressionMap), false}
							end
					end
			end
	end.

% Emits an all-clear (recovery) notification iff a non-suppressed alert email was
% actually sent for the current down-episode (Alerted). This single gate replaces the
% three former per-site recovery conditions — the pending_verification/#252 branch, the
% dependency state_change branch, and the expired-window branch — each of which used
% "was something failing in the prior cache" as a proxy for "did we tell the user".
% That proxy is wrong: a suppressed failure sits in the cache as ok=false but was never
% emailed, so the proxy fired an orphaned all-clear (#264). `Alerted` is that fact
% directly. WasFailing is retained in NotificationBase only as the recovery event's
% Loganne payload (what had been failing), never as a decision input. See ADR-0003.
maybe_emit_recovery(true, NotificationBase, System, Host, Notifiers) ->
	logger:notice("Checks recovered for ~p on ~p — emitting all-clear", [System, Host]),
	notify_all(NotificationBase#{failing_checks => #{}, suppressed => false}, Notifiers);
maybe_emit_recovery(false, _NotificationBase, System, Host, _Notifiers) ->
	logger:notice("Checks recovered for ~p on ~p with no prior alert — no all-clear sent", [System, Host]),
	ok.

% Partitions a map of failing checks into (PreExistingFailing, NewlyFailing) using
% a set of pre-existing-failing check keys captured at suppress-time.
partitionByPreExisting(FailingNow, HostPreExisting) ->
	maps:fold(fun(Key, Check, {PreAcc, NewAcc}) ->
		case sets:is_element(Key, HostPreExisting) of
			true  -> {maps:put(Key, Check, PreAcc), NewAcc};
			false -> {PreAcc, maps:put(Key, Check, NewAcc)}
		end
	end, {#{}, #{}}, FailingNow).

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").

	systemExists_test() ->
		SystemMap = #{
			"lucos_foo" => #system_state{host="host1.example.com", system_type=system},
			"lucos_bar" => #system_state{host="host2.example.com", system_type=host}
		},
		?assertEqual(true, systemExists("lucos_foo", SystemMap)),
		?assertEqual(true, systemExists("lucos_bar", SystemMap)),
		?assertEqual(false, systemExists("lucos_missing", SystemMap)),
		?assertEqual(false, systemExists("lucos_foo", #{})).

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

	% When all failing checks have an active dependsOn suppression, no alert email is sent.
	state_change_all_dep_suppressed_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_eolas" => #suppression_window{expiry_time = FutureExpiry, pre_existing = #{}}},
		SystemChecks = #{
			<<"eolas">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>, <<"consecutiveFailsCount">> => 0, <<"consecutiveUnknownsCount">> => 0}
		},
		Notifier = recording_notifier(self()),
		drain_notifications(),
		state_change(#{host => "host1.example.com", system => "lucos_time",
		               current_checks => SystemChecks, was_failing => #{}, metrics => #{}, alerted => false},
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
		SuppressionMap = #{"lucos_eolas" => #suppression_window{expiry_time = FutureExpiry, pre_existing = #{}}},
		SystemChecks = #{
			<<"eolas">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>, <<"consecutiveFailsCount">> => 0, <<"consecutiveUnknownsCount">> => 0},
			<<"db">> => #{<<"ok">> => false, <<"consecutiveFailsCount">> => 0, <<"consecutiveUnknownsCount">> => 0}
		},
		Notifier = recording_notifier(self()),
		drain_notifications(),
		state_change(#{host => "host1.example.com", system => "lucos_time",
		               current_checks => SystemChecks, was_failing => #{}, metrics => #{}, alerted => false},
		             SuppressionMap, [Notifier]),
		receive
			{notified, #{host := "host1.example.com", system := "lucos_time", failing_checks := FailingNow, suppressed := false}} ->
				% Only db should alert, not eolas
				?assert(maps:is_key(<<"db">>, FailingNow)),
				?assertNot(maps:is_key(<<"eolas">>, FailingNow))
		after 100 ->
			?assert(false, "Expected alert on non-dep-suppressed checks")
		end.

	% state_change: pre-existing failure during active window → Suppressed=false (continuing problem).
	state_change_pre_existing_alerts_unsuppressed_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		PreExisting = #{"host1" => sets:from_list([<<"host-tracking-failures">>], [{version, 2}])},
		SuppressionMap = #{"lucos_backups" => #suppression_window{expiry_time = FutureExpiry, pre_existing = PreExisting}},
		SystemChecks = #{<<"host-tracking-failures">> => #{<<"ok">> => false, <<"consecutiveFailsCount">> => 1, <<"consecutiveUnknownsCount">> => 0}},
		Notifier = recording_notifier(self()),
		drain_notifications(),
		state_change(#{host => "host1", system => "lucos_backups",
		               current_checks => SystemChecks, was_failing => #{}, metrics => #{}, alerted => false},
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
		SuppressionMap = #{"lucos_foo" => #suppression_window{expiry_time = FutureExpiry, pre_existing = PreExisting}},
		SystemChecks = #{<<"new-failure">> => #{<<"ok">> => false, <<"consecutiveFailsCount">> => 1, <<"consecutiveUnknownsCount">> => 0}},
		Notifier = recording_notifier(self()),
		drain_notifications(),
		state_change(#{host => "host1", system => "lucos_foo",
		               current_checks => SystemChecks, was_failing => #{}, metrics => #{}, alerted => false},
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
		SuppressionMap = #{"lucos_foo" => #suppression_window{expiry_time = FutureExpiry, pre_existing = PreExisting}},
		SystemChecks = #{
			<<"old-failure">> => #{<<"ok">> => false, <<"consecutiveFailsCount">> => 1, <<"consecutiveUnknownsCount">> => 0},
			<<"new-failure">> => #{<<"ok">> => false, <<"consecutiveFailsCount">> => 1, <<"consecutiveUnknownsCount">> => 0}
		},
		Notifier = recording_notifier(self()),
		drain_notifications(),
		state_change(#{host => "host1", system => "lucos_foo",
		               current_checks => SystemChecks, was_failing => #{}, metrics => #{}, alerted => false},
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

	% windowExpired: active window (ExpiryTime in future) → false
	window_expired_active_test() ->
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_foo" => #suppression_window{expiry_time = FutureExpiry, pre_existing = #{}}},
		?assertEqual(false, windowExpired("lucos_foo", SuppressionMap)).

	% windowExpired: expired window (ExpiryTime in past) → true
	window_expired_past_test() ->
		PastExpiry = erlang:system_time(second) - 1,
		SuppressionMap = #{"lucos_foo" => #suppression_window{expiry_time = PastExpiry, pre_existing = #{}}},
		?assertEqual(true, windowExpired("lucos_foo", SuppressionMap)).

	% windowExpired: no entry in SuppressionMap → false
	window_expired_not_in_map_test() ->
		?assertEqual(false, windowExpired("lucos_foo", #{})).

	% windowExpired: pending_verification entry → false (suppression was lifted, not expired)
	window_expired_pending_verification_test() ->
		PendingSources = sets:from_list([info], [{version, 2}]),
		SuppressionMap = #{"lucos_foo" => #pending_verification{sources = PendingSources}},
		?assertEqual(false, windowExpired("lucos_foo", SuppressionMap)).
-endif.
