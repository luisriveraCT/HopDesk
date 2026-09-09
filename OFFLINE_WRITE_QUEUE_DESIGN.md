# Design: durable offline write queue

Status: **proposal — not yet approved, no implementation started.**
Companion to `HANDOFF_OFFLINE_WRITE_QUEUE.md`, which this supersedes once signed off.

## Scope of "offline"

This is server-side resilience: the R process cannot reach S3 (AWS blip, network partition
between the app host and the bucket), not the end user's browser losing connectivity — a
dropped browser just kills the Shiny session, which is out of scope here. Every existing
retry mechanism in the codebase (`s3_save_with_retry`, `.audit_queue`, `.save_moves_deferred`)
is already framed this way; this design keeps that framing.

## Chokepoint: wrap `.s3_write()`, not each of the ~50 `save_*()` callers

`.s3_write()` (`R/persistence.R:313-370`) is already the single funnel almost every save
path goes through: stage to a `tempfile()` via `saveRDS()`, upload via `aws.s3::s3saveRDS()`
(fallback `put_object()`), invalidate preload/missing caches, `unlink()` the tempfile on every
exit. Instrumenting this one function — instead of touching manual invoices, provisions,
bancos movimientos, pagar_hoy, etc. individually — gives every `save_*()` caller durable
queuing for free, with no per-call-site changes and no risk of missing one.

Behavior change to `.s3_write()`:

1. Stage to tempfile as today.
2. Attempt the S3 upload as today.
3. **On failure** (instead of just re-`stop()`-ing): copy the staged tempfile into the local
   queue directory as a new queue entry (see below), record it in the manifest as `pending`,
   and *then* re-`stop()` so the existing caller-side behavior (error notification, retry
   wrapper, `_fallo_guardado` audit log) is completely unchanged. The queue is purely
   additive — nothing about today's error-surfacing UX regresses.
4. **On success**, no change.

This means Phase 1 scope isn't "which business features" but "which S3 keys are eligible for
queuing" — see Phase 1 scope below.

## Local durable store: flat RDS files + manifest

New local directory, one file per queued entry, e.g.:

```
<queue_dir>/
  manifest.rds              # data.frame: seq, s3_key, client_id, table_name,
                             #   expected_sync_version, status, staged_at, synced_at
  entries/
    000000001234.rds         # the actual staged payload (same bytes .s3_write() would upload)
    000000001235.rds
```

- `seq` is a monotonic local integer (`.queue_next_seq()`), distinct from business UUIDs and
  from `bump_sync_version()`'s table-level counters — purely a local pending→synced tracker,
  per the handoff's requirement.
- The manifest is rewritten atomically (`saveRDS()` to a tempfile in the same directory, then
  `file.rename()` — rename is atomic on both POSIX and NTFS within the same filesystem) so a
  crash mid-write never leaves a half-written manifest.
- No new dependency (RSQLite stays out of the Dockerfile). This matches the existing idiom —
  `.s3_write()` already does tempfile-then-atomic-move-equivalent staging.

**Queue directory location**: needs a path that's writable and persists across an R *process*
restart (not just an R session) on the actual deployment target. On shinyapps.io each app
instance runs in its own container with local disk that survives process restarts within that
container's lifetime, but **not** a container recycle (idle eviction, redeploy). This is a
real, bounded gap: the queue protects against "S3 had a blip and the R process kept running or
got restarted," not "the container was also recycled while entries were still pending." That
combination (S3 outage + container recycle, both during the same window) is treated as
accepted residual risk rather than solved — solving it would mean replicating the queue
somewhere other than local disk, which is a different, bigger project. Flagging this explicitly
rather than silently accepting it. Path: reuse the existing precedent's shape but make it
platform-general — `tools::R_user_dir("hopdesk", "data")` (falls back sanely per-OS) rather than
the Windows-only `%LOCALAPPDATA%\HopDesk` the file-lock code uses today, since this needs to
work on the Linux/shinyapps.io deployment target too.

## Phase 1 scope: sync_bus-registered tables only

A queue entry can only be replayed safely if we can tell whether someone else already wrote
the same table while we were offline — that check is exactly what `bump_sync_version()` /
`sync_bus.R` already provides. So Phase 1 covers only tables that call `register_synced()` —
everything else keeps today's behavior (synchronous write, error surfaced, no queuing) until
it's explicitly onboarded. *(Needs a follow-up grep of `register_synced(` call sites to produce
the concrete Phase-1 table list before implementation — not done yet.)*

At queue-time, the entry additionally records `expected_sync_version`: the table's
`bump_sync_version()` counter as last observed by this session before the write was attempted.

## Replay policy: last-write-wins, but always logged

Per your call: on reconnect, replay does **not** block on a version mismatch — it re-attempts
the upload regardless of whether the table's version advanced while the entry was pending.

To keep this from being a silent clobber (this app's top priority is data integrity, and this
is the exact bug class `R/global.R:182-238`'s incident already burned the team on once):

- Before replaying, compare the entry's `expected_sync_version` to the table's current version.
- If they match: replay normally, bump the version, mark `synced`.
- If they **don't** match: still replay (per your choice), but write a `_conflict_replay`
  `log_action()` audit row noting the table, both version numbers, and the client — so there's
  a forensic trail if a later report needs to explain a discrepancy, even though the write
  itself isn't blocked. This costs nothing at write time and gives an escape hatch (an admin
  can grep the audit log for `_conflict_replay` if something looks wrong later) without
  changing the behavior you asked for.

## Background retry: process-level, not session-level

`.save_moves_deferred()` and `.audit_queue()` both use `later::later()`, but scoped to fire
once per triggering event within a live session's reactive context. A queue resume needs to
run even when no browser session is open (e.g. right after the process restarts and before any
user connects), so this needs a **process-level** self-rescheduling loop, started once from
`R/global.R` (which already runs exactly once per process, per `tests/test_stage7_global_always_resourced.R`):

```r
.queue_flush_tick <- function() {
  tryCatch(.queue_flush_pending(), error = function(e) NULL)  # never let a bad tick kill the chain
  later::later(.queue_flush_tick, delay = 15)                 # self-reschedule regardless of outcome
}
later::later(.queue_flush_tick, delay = 15)
```

`later::later()` fires on httpuv's event loop regardless of active sessions, so this is a
legitimate pattern here, not a new mechanism — same primitive `.audit_queue` already uses, just
triggered by a heartbeat instead of a buffered-row event. On boot, before the first tick, do one
eager scan of `manifest.rds` for anything left `pending` from a previous process (that's the
actual "resume on restart" requirement) rather than waiting 15s.

Per-entry retry backoff mirrors `.AUDIT_MAX_RETRIES`/`.AUDIT_FLUSH_RETRY_DELAY` in spirit, but
entries are **never dropped** — that's the whole point of durable local storage versus
`.audit_queue`'s in-memory buffer. A `pending` entry just stays `pending` and gets retried on
every subsequent tick until S3 is reachable again.

## Tenant safety

This is the part that matters most: every client's data lives under its own S3 prefix
(`.s3_key()` prefixes every object with `<client_id>/`), and clients must never see or be able
to affect another client's data. A background queue that replays writes with no active session
attached is a new place that guarantee could quietly break, so it gets extra scrutiny, not less.

- Every queue entry stores `client_id` from `shared$effective_client_id()` (the
  `jump_client_id() %||% home_client_id()` chain), captured at **stage time**, inside
  `.s3_write()` itself — never `Sys.getenv("CLIENT_ID")`, which is a same-process, wrong-tenant
  footgun given one worker process serves multiple clients' sessions concurrently
  (`R/global.R:649-668`). This was already true in the original proposal; the rest of this
  section is what's new in response to your flag.
- **The manifest is partitioned by `client_id`, not one flat list mixing every client's
  entries.** Concretely: `manifest.rds` is a data frame keyed by `client_id`, or — simpler and
  harder to get wrong — a separate `entries/<client_id>/` subdirectory and manifest per client.
  The flush loop iterates client_id partitions one at a time and, within a partition, only ever
  reads/writes/replays entries belonging to that same client_id. No code path ever needs to
  compare or merge across two clients' entries, so there's no join, no shared index, no place a
  mix-up could happen.
- **Defense in depth at replay time**: before uploading, assert
  `startsWith(entry$s3_key, paste0(entry$client_id, "/"))`. If that ever fails, refuse to
  replay, mark the entry `blocked` (not `synced`, not silently dropped), and write a
  `_queue_tenant_mismatch` audit row. This should be unreachable given the partitioning above,
  but it's a cheap last-line check against a future refactor accidentally cross-wiring entries,
  and it's the same "assume nothing, verify at the boundary" posture the `client_id` fallback
  bug already taught this codebase to take.
- **The flush loop itself has no session, so it has no "current client" to fall back to** —
  that's a feature, not a gap: every replay is driven entirely by the `client_id` recorded on
  the entry, never by ambient session/env state, which removes the entire class of bug where a
  background job accidentally acts as whichever client happened to be active most recently.
- Test coverage addition: two clients queue entries for the same table name concurrently (e.g.
  both offline at once) → after flush, assert client A's payload landed only under
  `A/<table>.rds` and client B's only under `B/<table>.rds`, and that neither manifest partition
  ever contained the other's entry at any point.

### Staff jump handling (HopDesk staff acting inside a client's environment)

`effective_client_id()` already resolves to `jump_client_id() %||% home_client_id()`, so a
write a staff member makes while jumped into a client is *already* attributed to that client's
own S3 prefix, not the staff member's — that's the entire point of jump, and the queue design
inherits it correctly as long as one thing is respected:

- **Capture `client_id` as a resolved value at the moment `.s3_write()` is called, not as a
  live reference to the `effective_client_id()` reactive.** A staff member can end their jump,
  or jump into a *different* client, in the same browser session, possibly within seconds of
  triggering an offline write. If the queue entry held a reactive/closure instead of a plain
  string, a later re-evaluation could resolve to the wrong client entirely. Storing a plain
  resolved string at stage time (which the design already calls for) is what prevents this —
  worth stating as a hard requirement, not just an implementation detail, since this is exactly
  the kind of thing that's invisible in a code review but wrong in production.
- **Record the acting identity separately from the target `client_id`, for audit purposes only
  — not for authorization.** Each entry should also carry `acting_user` and
  `acting_home_client_id` (the staff member's own tenant, distinct from the client they jumped
  into) alongside the target `client_id`. This means a `_conflict_replay` or
  `_queue_tenant_mismatch` audit row — or just the normal `synced` audit trail — can show
  "staff member X, jumped from Hopdesk into client A" rather than only "client A," which matters
  once you're running enough staff and clients that "who actually did this" isn't obvious from
  the client_id alone.
- **Replay never re-checks jump authorization**, because replay is a headless background retry
  finishing a write that was already legitimately attempted, not a new privileged action — there
  is no live session to check permissions against at flush time (same as `.save_moves_deferred()`
  today, which also doesn't re-authorize on its deferred retry). Authorization is a stage-time
  concern; the queue's job is only to durably finish what was already authorized.
- **Scaling note for "a lot of users at once":** with many staff members potentially jumped into
  many different clients concurrently, don't have the flush tick enumerate every client
  partition directory that has ever existed. Maintain a small top-level index (e.g. a
  `pending_client_ids.rds` set, updated whenever an entry is staged or fully drained) so each
  15s tick only visits partitions that actually have pending work, rather than scanning
  potentially hundreds of mostly-empty client directories every cycle.

## Testing approach (matches existing convention)

Extract the real `.s3_write()`, `.queue_flush_pending()`, `.queue_next_seq()` bodies via the
established `readLines()`/`parse()`/`eval()` pattern (`test_s3_read_retry_hardening.R`,
`test_confirm_agenda_resilience.R`) rather than reimplementing them under test. Mock
`aws.s3::s3saveRDS`/`s3readRDS` via `utils::assignInNamespace()` wrapped in
`tryCatch(..., finally = ...)` restoration — non-negotiable given the mock-S3-leak incident.
Point the queue directory at a `tempfile()`/`tempdir()` throwaway path per test, same as
`test_hopdesk_single_instance_lock.R` does for the filelock tests. Planned test cases:

- Write succeeds → no queue entry created.
- Write fails → entry staged, manifest shows `pending`, original error still surfaces to caller.
- Flush tick with S3 still down → entry stays `pending`, no crash, chain reschedules.
- Flush tick with S3 back up → entry uploads, manifest shows `synced`, version bumped.
- Version mismatch at replay → write still proceeds (per chosen policy), `_conflict_replay`
  audit row is written with both version numbers.
- Process restart with entries left `pending` → eager boot-time scan picks them up without
  waiting for the first 15s tick.
- Manifest write crash-safety → kill mid-rewrite (simulate by truncating the tempfile before
  rename), confirm the old manifest is still intact (rename never partially applied).

## Phase 1 table list (resolved)

`register_synced()` call sites, all in `app.R:1311-1395` (28 tables, one per line except the
first which wraps):

`bancos_confirmados_db`, `bancos_movimientos_db`, `ctas_cuentas`, `proveedores_db`,
`empresas_db`, `grupos_db`, `pasivos_provisions_db`, `pasivos_liabilities_db`, `papelera_rv`,
`manual_inv`, `notes_df`, `tags_db`, `moves_db`, `policy_catalog_db`,
`forecasting_series_observations_db`, `forecasting_metrics_db`,
`forecasting_subscriptions_db`, `forecasting_manual_curves_db`, `conciliacion_rv`,
`parte_alias_map_db`, `abonos_db`, `interco_v2`, `sap_ov_db`, `proveedores_inactivos_db`,
`partner_policies_db`, `policy_moves_db`, `holiday_overrides_db`, `hop_grants_db`.

This is a good Phase 1 set: it already covers the highest-value tables named in the original
handoff (bancos movimientos, pasivos provisions/liabilities, manual invoices) alongside smaller
config-like tables, without needing a separate onboarding step per table — the version-stamp
check Phase 1 depends on already exists for all 28.

**Side finding, worth a separate look but not blocking this design**: `tests/test_sync_bus_coverage.R`
hardcodes an expectation of exactly **8** registered keys (`test_sync_bus_coverage.R:14,32`),
but 28 exist in `app.R` today — either that test's list is stale (registrations were added over
time without updating it), or some of the 28 aren't the ones it's meant to track. Not an offline-
queue problem, but likely a genuine coverage gap in the sync_bus test suite that predates this
work — flagging it since I ran across it, not proposing to fix it as part of this queue design
unless you want it bundled in.

## Explicitly out of scope for this design

- Client-side/browser offline support (PWA-style local caching) — different problem, not what
  the handoff describes.
- Solving the "S3 outage + container recycle at the same time" double-fault — flagged as
  accepted residual risk above.
- Onboarding non-sync_bus tables (manual invoices, bancos movimientos, pasivos, etc.) — future
  phase, once Phase 1 is proven in production.
