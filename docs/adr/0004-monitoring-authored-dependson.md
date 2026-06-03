# ADR-0004: Monitoring-authored `dependsOn` on synthetic reachability probes

- **Status**: Accepted
- **Date**: 2026-06-03
- **Issue**: [#272](https://github.com/lucas42/lucos_monitoring/issues/272)
- **Builds on**: [ADR-0002](0002-polymorphic-dependson.md) (polymorphic `dependsOn`, OR semantics, single-hop) — which this ADR extends, not supersedes

## Context

`lucos_router` is the reverse proxy in front of every HTTP service in the estate, and `lucos_dns` resolves their hostnames. While either is mid-restart during a deploy, monitoring's two **synthetic reachability probes** fail transiently for *many systems at once*:

- `fetch-info` — an HTTP GET of each system's `/_info` (goes through the router; name resolved by dns).
- `tls-certificate` — the TLS handshake to each system (terminated at the router).

None of those systems is itself in a deploy window, so nothing suppresses the resulting failures. `failThreshold: 2` absorbs a single-poll flap but not a restart that spans two or more poll intervals. The 2026-06-03 `lucos_router` deploy (1.0.20) produced exactly this: a fan of estate-wide blips with no deploying system to attribute them to.

ADR-0002 established `dependsOn` as the mechanism for "this check's value is sensitive to that other system being mid-deploy," and made it polymorphic (string or list, OR semantics, single-hop). But ADR-0002 frames `dependsOn` exclusively as a **per-service, `/_info`-declared** relationship — "dozens of independent emitters (every lucos service)." That mental model has no answer for the router/dns case, because **the affected systems do not own the failing check**. The `fetch-info` and `tls-certificate` checks are not declared in any service's `/_info`; they are *synthesised by monitoring itself* in `fetcher_info.erl`. A service cannot declare a dependency on a check it does not emit.

## Decision

Monitoring stamps `dependsOn: ["lucos_router", "lucos_dns"]` onto the synthetic reachability probes it generates, in `make_direct_probe_check/1` (`src/fetcher_info.erl`) — the helper shared by both `fetch-info` and `tls-certificate`. The dependency is therefore **authored by monitoring on behalf of the probed system**, not declared by that system.

This establishes a deliberate second class of `dependsOn` author:

| Author | Source of the check | Example |
|---|---|---|
| A service | its own `/_info` | `webhook-error-rate` on `lucos_loganne` (ADR-0002) |
| **Monitoring itself** | synthetic probe generation | `fetch-info` / `tls-certificate` (this ADR) |

The suppression *engine* is untouched. Both read sites in `monitoring_state_server.erl` (`is_dependency_suppressed/3` and `find_dependent_systems/2`) consume `normalise_depends_on/1` and are agnostic to where the `dependsOn` came from. ADR-0002's invariants — polymorphic shape, OR semantics, single-hop, per-element self-reference guard — all hold unchanged. This ADR adds an *authorship* concept, not an engine change.

### Why monitoring-authored, not service-declared

The alternative would be every HTTP service declaring `dependsOn: ["lucos_router", "lucos_dns"]` on a reachability check in its own `/_info`. That is wrong on three counts: (1) the reachability check isn't the service's to declare — monitoring synthesises it; (2) it would duplicate the same cross-cutting infra truth across ~25 `/_info` documents, drifting the moment one is missed; (3) it conflates "am I, the service, healthy?" (the service's concern) with "can monitoring reach me through the shared ingress?" (monitoring's concern). The dependency is a property of *how monitoring observes*, so it belongs in monitoring's observation layer.

### Scope: the two direct-probe checks only

`make_direct_probe_check/1` is shared by `fetch-info` and `tls-certificate`, so the single stamp covers both — intended, since a router restart blips the TLS handshake as much as the HTTP GET. The `circleci` synthetic check hits CircleCI's API directly, not through our router or dns, so it carries no such dependency and is correctly unaffected.

## Consequences

**Positive**

- Router/dns deploy windows now suppress the estate-wide reachability blips they cause, instead of paging. The signal that's lost is one nobody can act on anyway (a system's `/_info` is genuinely unfetchable while the shared ingress bounces).
- The cross-cutting infra truth lives in exactly one place. New HTTP services inherit the dependency automatically — there is nothing to remember to add to their `/_info`.
- Establishes a clean, named pattern for any future synthetic probe whose value is contingent on shared infrastructure.

**Negative — the second read site widens the cascade blast radius**

This is the consequence most likely to surprise a future reader, so it is stated plainly. `dependsOn` is read at *two* sites (ADR-0002). The suppression site (`is_dependency_suppressed/3`) does exactly what we want. The **second** site — `find_dependent_systems/2`, which on window-close cascades `#pending_verification{}` to every system declaring the unsuppressing system as a dependency — now fans out to the **entire estate** on every `lucos_router` or `lucos_dns` deploy, because every system's `fetch-info`/`tls-certificate` names them.

- This is mostly correct: after a router restart, every system's reachability data *is* stale, and `pending_verification` is precisely the "defer the alert decision until a fresh post-deploy poll confirms" mechanism. Healthy systems clear on their next poll with no alert.
- The genuine cost: a router-*unrelated* failure anywhere in the estate (a service-internal `/_info` check, or a `circleci` check) has its alert **deferred by one poll cycle** on every router/dns deploy, because that system is swept into `pending_verification` alongside everyone else. One poll interval; fresh data resolves it. Acceptable, but real — and far larger in fan-out than the ADR-0002 motivating case (5 webhook targets cascading to 1 consumer).
- This behaviour must be covered by an acceptance criterion and an eunit test, not left implicit (tracked on #272).

**Negative — contract surface**

- "Where do `dependsOn` values come from?" now has two answers instead of one. Mitigated by this ADR and the table above; the engine stays single-sourced through `normalise_depends_on/1`.

## Alternatives considered

- **Service-declared reachability deps** — rejected above (wrong owner, estate-wide duplication, conflates concerns).
- **A bespoke "ingress-dependent" flag distinct from `dependsOn`** — rejected. It would duplicate suppression logic for no semantic gain; the existing single-hop OR mechanism expresses "any of router/dns mid-deploy makes this probe noisy" exactly.
- **Dedupe a genuine (non-deploy) router outage into a single alert** — explicitly *not* done. Deploy-window-only semantics are preserved: a real router death outside a deploy window still fires the full estate-wide cascade loudly, which is correct. Changing that is a separate decision, not this one.
