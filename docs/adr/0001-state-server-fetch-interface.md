# ADR-0001: Simplify the state server's fetch interface

- **Status**: Accepted
- **Date**: 2026-04-27
- **Issue**: [#196](https://github.com/lucas42/lucos_monitoring/issues/196)

## Context

`monitoring_state_server` exposes its current state to callers (chiefly `server.erl`) via three `gen_server:call` requests:

- `{fetch, all}` — returns a map of host → `{System, Checks, Metrics}`
- `{fetch, suppression}` — returns the suppression map
- `{fetch, Host}` — returns the entry for a single host

The shape `Checks` returned by `{fetch, all}` carries four interplaying fields that callers must interpret together to decide how a check should be rendered:

- `<<"ok">>` — `true` / `false` / `unknown`
- `<<"unknown_count">>` — consecutive unknown reports, gated against an internal threshold of 3
- `<<"fail_count">>` — consecutive `false` reports, gated against the per-check `<<"failThreshold">>`
- `<<"failThreshold">>` — number of failures required before the held value flips to `false`

The view layer (`server.erl:151–194`, `server.erl:211–220`) re-implements the interpretation each time: combining `ok` with the counters to derive a "buffering" state, and aggregating across checks to produce a system-level health value. The aggregation logic for systems lives parallel-but-different to the aggregation logic for individual checks. Suppression state lives in a separate map fetched separately and joined client-side.

This is fragile in three ways:

1. Every consumer (`/`, `/api/status`, `/_info`, future API consumers) re-derives the same interpretation. A bug fix or rule change has to be applied in N places.
2. The "buffering" visual state — a check whose displayed health is being held while the counters tick — is encoded implicitly across three fields. Anyone reading the code has to reconstruct the meaning rather than read it.
3. Suppression state is decoupled from system state at the boundary, so callers compose two independent fetches into one render. This forces the view to know about the internal shape of the suppression map (active vs `{pending_verification, _}`).

## Decision

The state server takes ownership of all aggregation. Callers receive pre-computed, ready-to-render values via a single fetch.

### `{fetch, all}` returns a list of systems

Each system is a map:

```erlang
#{
  <<"id">>      => Id,         % string, matches a lucos_configy identifier
  <<"type">>    => Type,       % atom, matches a lucos_configy type (system, host, component, ...)
  <<"name">>    => Name,       % human-readable string
  <<"checks">>  => Checks,     % list of check maps
  <<"metrics">> => Metrics,    % list of metric maps
  <<"status">>  => Status      % atom, see status enum below
}
```

Each check is a map:

```erlang
#{
  <<"id">>         => Id,           % unique string within the system
  <<"techDetail">> => TechDetail,   % string, per /_info spec
  <<"debug">>      => Debug,        % string, per /_info spec
  <<"link">>       => Link,         % string, optional clickable URL
  <<"status">>     => Status,       % atom, see status enum below
  <<"statusText">> => StatusText    % human-readable display string, e.g. "unknown (2/3)"
}
```

Each metric is a map:

```erlang
#{
  <<"id">>         => Id,           % unique string within the system
  <<"techDetail">> => TechDetail,   % string, per /_info spec
  <<"value">>      => Value         % number, per /_info spec
}
```

### Status enum

A single shared enum is used for both check and system `status`. The view layer can determine the CSS class for either by reading exactly this one field — no other variable is consulted:

| Atom | Meaning |
|---|---|
| `healthy` | Currently passing. |
| `failing` | Currently failing (past `failThreshold` for checks; one or more checks failing for systems, modulo suppression). |
| `unknown` | Cannot determine current state (data fetch failed, never reported, etc.). |
| `buffering` | Displayed status is being held while counters observe further data. Replaces the implicit "buffering" state previously derived from `unknown_count > 0` or `fail_count > 0 && failThreshold > 1`. |
| `suppressed` | A maintenance suppression window is active. Applies at system level. |
| `pending_verification` | A maintenance window has just closed; awaiting fresh post-deploy reports before deciding whether to alert. Applies at system level. |

In current usage `suppressed` and `pending_verification` only appear at system level. The shared enum nonetheless permits them on checks should per-check suppression ever be wanted.

### System-level aggregation priority

The state server computes system `status` from the constituent checks and the suppression map by walking the following priority list. The first match wins:

1. System has an active suppression window → `suppressed`
2. System is in pending-verification → `pending_verification`
3. Any check is `failing` (after dependency-suppression filtering) → `failing`
4. Any check is `unknown` → `unknown`
5. Any check is `buffering` → `buffering`
6. Otherwise → `healthy`

A system with no checks at all is `unknown` — the absence of a signal is not a positive signal.

When a system is `suppressed` or `pending_verification`, the status of individual checks is unaffected: a failing check inside a suppressed system still reports `<<"status">> => failing`. The system header reflects the suppression, the check rows reflect the underlying truth.

### Aggregation timing

Status is computed at write-time (when `updateSystem` mutates state), not at fetch-time. The state server already maintains a `NormalisedCache` per host alongside the source map; aggregated status fits the same pattern. This keeps fetches O(systems) rather than O(systems × checks) and ensures status is atomic with respect to mutations.

### Removal of `{fetch, suppression}` and `{fetch, Host}`

Suppression state is now reflected in the `<<"status">>` field of each system returned by `{fetch, all}`. The separate `{fetch, suppression}` request is removed.

`{fetch, Host}` has no callers in the codebase and is removed.

### Renaming: `erroring` → `failing`

The "currently failing" state is named `failing`, replacing the existing `erroring` used in CSS classes and `systemHealthy/1`. `failing` matches the existing user-visible string `"failing (1/3)"` rendered in `statusText`, the internal `fail_count` / `failThreshold` vocabulary, and reads as a normal English present participle. CSS class names update with it.

## Consequences

**Positive**

- View layer (`server.erl`) stops re-implementing health interpretation. Each renderer reads one field per check and one field per system to choose its CSS class. The HTML view, `/api/status`, and `/_info` all share the same authoritative state.
- The "buffering" state is now first-class and structurally explicit, where previously it was an emergent property of three counters.
- Suppression sub-states are visible to consumers without having to know the shape of the internal suppression map.
- Adding a new consumer of monitoring state (a future API client, or a different rendering path) requires no monitoring-internal knowledge beyond the published interface above.
- Aggregation correctness has one place to be wrong, not many.

**Negative**

- Callers must migrate. The current `{fetch, all}` shape (`#{Host => {System, Checks, Metrics}}` with raw counter fields) is replaced wholesale; this is a breaking change to the gen_server protocol. Acceptable because the only callers are inside this OTP application.
- The state server grows the responsibility of keeping aggregated status correct on every write path. Tests must cover each transition (healthy ↔ failing, failing ↔ buffering, suppression activation/expiry, pending_verification cycle). The `applyFailThreshold` and `replaceUnknowns` logic stays — it now feeds the aggregation rather than being interpreted downstream.
- `unknown_count`, `fail_count`, and `failThreshold` are no longer surfaced to callers as structured fields. The numeric progression appears only in `statusText` (e.g. `"unknown (2/3)"`). A future consumer that wants the raw counts would need a new field added; since the only current consumer is inside the same application this is fine.
- The CSS class rename (`erroring` → `failing`) requires touching `style.css`, all `getCssClass/2` call-sites, and any external CSS referencing `.erroring`. None known outside this repo.
