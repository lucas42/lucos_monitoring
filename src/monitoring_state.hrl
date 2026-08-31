% Shared gen_server state records for monitoring_state_server, alerting,
% depends_on and status. Split out of monitoring_state_server.erl so all four
% modules can share the same record definitions without a circular include.

% Per-system state stored as a value in SystemMap.  Named fields prevent
% positional-tuple confusion and make it safe to add future fields without
% touching every read/write site.
-record(system_state, {
	host,
	system_type,
	source_checks_map = #{},
	normalised_cache  = #{},
	metrics           = #{},
	source_timestamps = #{},
	alerted           = false
}).

% Active deploy-window entry in SuppressionMap.
-record(suppression_window, {
	expiry_time,
	pre_existing
}).

% Post-unsuppress entry in SuppressionMap: alert decision deferred until all
% sources have reported fresh data.
% pre_existing mirrors the field from #suppression_window: a per-Host map of
% check-key sets captured at suppress-time.  Used in the post-deploy alert gate
% to suppress re-alerts for failures that were already known before the deploy.
% Defaults to #{} (no pre-existing baseline) for cascaded dependent-system entries.
-record(pending_verification, {
	sources,
	pre_existing = #{}
}).
