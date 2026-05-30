# ADR-0003: Recovery notifications gated on a per-episode `alerted` flag

- **Status**: Proposed
- **Date**: 2026-05-30
- **Issue**: [#264](https://github.com/lucas42/lucos_monitoring/issues/264)
- **Supersedes**: the `prevFailing` recovery guard introduced for [#252](https://github.com/lucas42/lucos_monitoring/issues/252)

## Context

Alert suppression — both deploy-window suppression and `dependsOn` dependency suppression — silences the **failing-check** email but historically did **not** silence the subsequent **recovery** email. The result was an orphaned "Everything OK on …" email arriving with no preceding "Monitoring issue on …" email for the same system. On 2026-05-30 three such orphans went out (`lucos_media_metadata_manager`, `lucos_media_weightings`, `lucos_media_metadata_api`), all triggered by a single `lucos_media_metadata_api` deploy window.

The bug is an **asymmetry**: a failure is dropped at notify-time (`suppressed => true`), but the *check is still written to the cache as `ok=false`*. When it later recovers, the recovery path fires and the email goes out.

Every previous attempt to gate the recovery used some flavour of **"was something failing in the prior cache?"** as a proxy for **"did we actually tell the user there was a problem?"**:

- `state_change`'s dependency-recovery branch emitted a recovery whenever the failing-check *set* changed to empty.
- #252's fix gated the post-deploy recovery on `map_size(failingChecks(OldNormalisedCache)) > 0`.

That proxy is wrong in exactly the orphan case. A dependency-suppressed failure sits in `OldNormalisedCache` as `ok=false` (suppression is applied only when filtering `failingChecks` at notify-time — it never touches the stored cache). So `failingChecks(OldNormalisedCache)` — i.e. `wasFailing`/`prevFailing` — **includes the suppressed-but-never-alerted failure**, and the proxy says "emit a recovery" when no alert was ever sent.

The crux: **"did we alert the user for this episode?" is not a function of the current and previous check caches.** Both caches only know `ok`/`failing`; neither knows `alerted`/`suppressed`. That fact depends on the suppression state *at the failing poll*, which has already changed by the recovery poll (the upstream window has closed — that is *why* the dependent recovered). It genuinely cannot be reconstructed after the fact. One bit must be remembered.

## Decision

Add a **per-system, per-episode `alerted` boolean** to the in-memory state in `src/monitoring_state_server.erl`. It is the final element of the per-system state tuple already held in `SystemMap`:

```
{Host, SystemType, SourceChecksMap, NormalisedCache, Metrics, SourceTimestamps, Alerted}
```

Semantics:

- **Set `true`** the moment a *non-suppressed* alert (a real "Monitoring issue" email) is dispatched for the system. Sticky for the duration of the down-episode.
- **Read** as the sole decision input for whether to emit a recovery (all-clear): a recovery is dispatched **iff `Alerted` is true**.
- **Reset to `false`** when the system returns to fully healthy (the episode is over).
- **Carried forward unchanged** by suppressed notifications and by no-op polls — a suppressed failure does not alert, so it does not set the flag; an already-alerted episode keeps owing a recovery until it is genuinely healthy.

This single flag replaces the **three** former ad-hoc recovery conditions with one consistent gate (`maybe_emit_recovery/5`):

1. the `pending_verification` / post-deploy branch (#252's `prevFailing` guard),
2. the dependency `state_change` recovery branch (#264's dependency orphans),
3. the expired-deploy-window branch.

`failingChecks(OldNormalisedCache)` (`wasFailing`) is **retained only as the descriptive `was_failing` payload** on the recovery Loganne event ("here is what had been failing and is now healthy") — **never as a decision input**.

### Why `alerted`, and not `wasFailing`

`wasFailing` answers "was this check `ok=false` in the previous cache?" — which includes suppressed-but-never-alerted failures, so it cannot gate the orphan. `alerted` answers "did a non-suppressed alert email actually go out?" directly. It is the exact fact the orphan turns on.

### Why gate the dispatch, not the email module

Recovery is gated at the **`notify_all` dispatch site** in `monitoring_state_server`, so a gated-out recovery reaches *neither* notifier. Both `email:notify/1` and `loganne:notify/1` are driven from the same `notify_all` call; gating there keeps `alerted` as the single decision input in one module, rather than splitting the decision across `monitoring_state_server` (loganne) and `email.erl` (email). `loganne.erl` is left **untouched**. `email.erl` changes only its (previously inaccurate) header comment, which claimed recoveries were dropped — they are not; a recovery that reaches `email.erl` is a genuine all-clear closing a real alert, and is sent.

### This also fixes #252

#252 and #264 are the same question from opposite sides. #252's `prevFailing` guard was an *approximation* of "did a real problem precede this"; `alerted` is the real thing. The same bit gates #252's post-deploy recovery branch correctly:

| Prior episode | `prevFailing > 0` (old) | `alerted` (new) | Correct? |
|---|---|---|---|
| Real failure, emailed | emit ✓ | emit ✓ | both right |
| Healthy all along | silent ✓ | silent ✓ | both right |
| **Suppressed-only, never emailed** | **emit ✗ (orphan)** | **silent ✓** | only `alerted` right |

## Consequences

### Positive

- The orphaned-recovery bug is fixed for **both** suppression mechanisms (deploy-window and dependency) with a single rule.
- Net **reduction** in distinct decision logic: three different recovery conditions (one of them buggy) collapse to one gate. This directly answers the standing concern that the state server keeps accreting hard-to-reason-about special cases — the fix removes variety rather than adding it.
- The new state is **one named boolean** colocated with the per-system state already kept (its check cache, metrics, timestamps). No new map, timer, or structure; no change to the gen_server state tuple or any `handle_call` path.
- `alerted` makes an inherently-stateful fact ("have we emailed about this?") **explicit**, instead of being repeatedly — and incorrectly — re-derived from cache state at each recovery site.

### Negative / honest trade-offs

- **A purely-suppressed episode emits no `monitoringRecovery`.** Because the gate sits at dispatch, a dependency- or deploy-suppressed failure that recovers without ever alerting produces a `monitoringAlertSuppressed` event with no closing `monitoringRecovery` in the Loganne stream. This is internally consistent (a recovery event exists to close an *alert*, and there was none), but it is an observable change from the pre-fix behaviour where the recovery branch fired unconditionally. If a complete open/close bracket for suppressed intervals is later wanted in the audit log, that is a deliberate follow-up — it would mean dispatching the recovery to loganne while still gating only the email, i.e. splitting the decision across two modules.
- **State is in-memory and per-process.** A `monitoring` restart resets `alerted` to `false`. This is safe: a recovery only fires on an observed failing→healthy transition, and a freshly-restarted source starts `healthy` (no transition), so the orphan cannot recur across a restart. No persistence is introduced.
- **Failure born inside a deploy window and sustained past it.** `state_change` only runs when the failing-check *set* changes (`meaningfulChange/2`). A failure that begins suppressed during a deploy window and persists, unchanged, past the window's expiry never alerts for that episode — and so its eventual recovery stays gated too. This is a **pre-existing** limitation, not a regression introduced here, and is left for a separate follow-up about re-evaluating on window expiry.

## Alternatives considered

- **Gate on `wasFailing` as-is.** Provably cannot work: a dependency-suppressed failure is stored as `ok=false`, so `wasFailing` includes it on recovery. This is the route that produced the orphan.
- **Snapshot "failing at suppression start" (`PreExisting`).** Already captured for deploy windows, but dependency suppression has no "suppression period" to snapshot — it is evaluated per-poll and the context is gone by recovery time. Reaches only the deploy-window third of the problem.
- **Pass `alerted` to `email.erl` and let it drop the recovery (keep loganne emitting).** Keeps the Loganne audit log symmetric, but splits the recovery decision across two modules and keeps a second gate. Rejected in favour of one decision input in one place; can be revisited if audit-bracket symmetry is later judged worth the cost (see Negative, above).
- **An in-memory database (ETS / Mnesia / SQLite).** Relocates *where* state lives; it does not simplify the *decision logic*, which is where the complexity sits — the same suppression/recovery branches would query a table instead of a map. It adds operational weight to a service whose value is being a thin, reliable observer. The right call only if requirements changed to need queryable history, cross-restart persistence, or time-series — which they have not.
- **Naming the whole state (records for the positional tuples / polymorphic `SuppressionMap`).** A genuine readability improvement, but out of scope for a bugfix and tracked separately; this ADR adds exactly one named field without restructuring what is already there.
