# ADR-0002: Polymorphic `dependsOn` with OR semantics

- **Status**: Accepted
- **Date**: 2026-05-12
- **Issue**: [#227](https://github.com/lucas42/lucos_monitoring/issues/227)
- **Companion**: [lucas42/lucos_loganne#456](https://github.com/lucas42/lucos_loganne/issues/456)

## Context

The `dependsOn` field on a `/_info` check is the monitoring-side contract for "this check's value is sensitive to that other system being mid-deploy." Today the contract is a single binary string — one check, one dep. The state server reads it at two sites in `src/monitoring_state_server.erl`:

- `is_dependency_suppressed/3` — if the failing check's `dependsOn` system is in an active suppression window, the failure is filtered out of `failingChecks/1` (alerts get suppressed).
- `find_dependent_systems/2` — when a system unsuppresses, cascade `pending_verification` to systems whose checks declare it as a dep.

This works well for checks bound to a single downstream system. It breaks down for **aggregate/cross-cutting checks** — checks whose value is a function of N downstream systems' health, where any one of them being mid-deploy is enough to make the aggregate noisy.

The motivating example is `webhook-error-rate` on `lucos_loganne` (see lucos_loganne#456). Loganne fans `deploySystem` events out to five webhook subscriber URLs and counts failures. If any one subscriber is mid-deploy and bouncing connections, the aggregate error rate spikes — but today's `dependsOn` can only name one of the five. The check ends up flapping on every estate deploy.

A single-valued `dependsOn` cannot express "this check is sensitive to any of N systems."

## Decision

`dependsOn` is extended to accept either a string (status quo) or a list of strings:

```json
"dependsOn": "lucos_eolas"               // legacy single-dep, unchanged
"dependsOn": ["lucos_a", "lucos_b", ...] // aggregate / cross-cutting
```

Both shapes are normalised on read at both call sites and **OR semantics** apply over the list:

- `is_dependency_suppressed` returns `true` if **any** element of the normalised list is a system currently under an active suppression window.
- `find_dependent_systems` returns the set of system IDs whose checks declare the target system **in their `dependsOn` list**.

The field stays **per-check and single-hop**. List elements are not themselves resolved against their own `dependsOn` declarations — the no-transitive-chains invariant is preserved unchanged.

### Why polymorphic, not "coerce everything to a list"

Coercing every existing check to a singleton list would be a breaking change to a `/_info` contract that has dozens of independent emitters (every lucos service). Polymorphic on the read side localises the change to one repo, with no estate-wide flip. The normalisation cost is trivial — a single pattern match per read.

### Why OR, not AND

A list-valued `dependsOn` describes "any of these going dark makes my value noisy." That's naturally a disjunction. For the motivating `webhook-error-rate` case: any one of the five subscribers being mid-deploy is sufficient to inflate the aggregate. AND semantics ("all five must be deploying before I'm noisy") describe no real check we know of and would silently bury most legitimate suppressions.

### Why single-hop

Transitive `dependsOn` resolution would let an emitter declare `dependsOn: "lucos_a"` and have monitoring follow `lucos_a`'s own `dependsOn` declarations. We deliberately don't do that, and adding lists doesn't change it. The single-hop rule keeps suppression evaluation O(N) per check rather than potentially cyclic, keeps the cascade target set knowable from local state, and matches the existing mental model: each check declares the systems it cares about, no further inference.

### Implementation shape

A single helper `normalise_depends_on/1` collapses the polymorphism into a `[string()]`:

```erlang
normalise_depends_on(Check) ->
    case maps:get(<<"dependsOn">>, Check, undefined) of
        undefined -> [];
        B when is_binary(B) -> [binary_to_list(B)];
        L when is_list(L) ->
            [binary_to_list(E) || E <- L, is_binary(E)];
        _ -> []
    end.
```

Both read sites consume the normalised list and apply the existing self-reference / `pending_verification` / expired-window rules per element.

## The deploy-loop self-reference with loganne

This is worth calling out explicitly because it's easy to miss when reading the code months later.

Loganne is **both**:

- the **source** of `deploySystem` Loganne events, which fan out via webhook to `https://monitoring.l42.eu/suppress/clear`, opening suppression windows in monitoring's state, **and**
- a **consumer** of those windows once its `webhook-error-rate` check declares its webhook targets via `dependsOn: [...]`.

There is no circularity bug here. Monitoring's suppression map is the single arbiter of suppression state; loganne is one of many emitters of the events that open windows in that map. A deploy of `lucos_a` causes:

1. CI publishes a `deploySystem` Loganne event mentioning `lucos_a`.
2. Loganne's webhook fan-out hits `monitoring.l42.eu/suppress/clear` on behalf of `lucos_a`.
3. Monitoring opens a suppression window for `lucos_a`.
4. Loganne's `webhook-error-rate` check polls. Its `dependsOn` includes `lucos_a`. Monitoring's `is_dependency_suppressed` returns `true`. Failure is filtered.

What this **isn't** is loganne suppressing its own checks — that would be the self-reference guard's job, and that guard fires per element so a `dependsOn` list containing both loganne and another system has its loganne element silently ignored while the other element is evaluated normally.

The thing to remember: the suppression *windows* loganne consumes are opened on behalf of *its webhook targets*, not loganne itself. The deploy-loop is therefore observation, not feedback.

## Consequences

**Positive**

- Aggregate/cross-cutting checks (`webhook-error-rate` and any future siblings) can honestly declare what they depend on, instead of choosing one downstream and hoping the others stay quiet.
- Backwards-compatible: no estate-wide flip needed. Every existing single-dep check is unaffected.
- Self-maintaining for emitters that compute the list dynamically: loganne can introspect its own webhook subscriber config and emit the corresponding `dependsOn` list automatically, so new webhook subscribers join the suppression set without code changes here.
- The `_ -> false` fallback at the old call site meant consumers could ship the list shape *before* this change landed without breakage — the field was silently ignored until monitoring caught up. This loosened sequencing across repos.

**Negative**

- Slight increase in the `dependsOn` contract surface area — two shapes instead of one. Mitigated by normalising on read in one helper.
- Risk of accidental misuse: someone might reach for `dependsOn: [...]` when what they actually want is transitive resolution through a chain of single-dep checks. This ADR is explicit that the list is single-hop OR, not chain-following — if a user really wants transitive semantics they need a different mechanism, and they should propose it as a separate issue rather than overload this one.
- A bug in `normalise_depends_on/1` is now load-bearing for both read sites instead of one. The eunit tests cover both legacy and list shapes at both sites; future changes to the helper need to keep both shape families passing.
