# Handoff: durable offline write queue (does not exist yet)

Investigated 2026-09-06, confirmed via full-codebase search: this app has **no durable local write queue**. Every save path is synchronous-or-deferred-briefly, S3-or-bust. If a user loses connectivity for more than a few seconds, their change either fails outright (user sees an error, nothing saved) or is silently dropped after a bounded number of in-memory retries — never durably queued for automatic replay once connectivity returns.

## What already exists (do not duplicate)

1. **`save_pagar_hoy_with_retry()` / `s3_save_with_retry()`** (`R/persistence.R:1098-1128`) — synchronous, 3 attempts ~0.5s apart, blocks the caller. No durability past 3 failures.
2. **`log_action()`'s `.audit_queue()`** (`R/app_audit.R:184-233`) — in-memory buffer per client, flushed via `later::later()` after a 2s delay, up to 5 retries with 10s backoff (`.AUDIT_MAX_RETRIES`), then **drops the batch** (its own comment: "Deliberately just an in-memory buffer, not a local-disk queue"). Audit-log entries only, not application data.
3. **`.save_moves_deferred()`** (`R/ledger_module.R:3446-3484`) — defers a ledger move/restore/tag/delete save via `later::later()`, retries exactly once after 2s, then shows a persistent on-screen error and logs a `_fallo_guardado` entry. The actual data lives only in an R closure — gone if the process dies in that window.
4. **`.s3_write()`** (`R/persistence.R:313-353`) stages to a `tempfile()` before upload, but `on.exit(unlink(tf))` removes it unconditionally, success or failure — never kept around for replay.
5. **`bump_sync_version()`/`sync_bus.R`** — a whole-table version counter for detecting "your snapshot is stale, re-fetch." Read-side staleness detection only, zero involvement in buffering or retrying writes.
6. No per-entry sequence/index for tracking "which of my N pending offline writes actually made it to S3" exists anywhere — every `uuid::UUIDgenerate()` in the codebase is a business-entity id (invoice id, confirmacion_id, etc.), not a sync-state tracker.

## What's needed

- A local durable store for pending writes, written **before** attempting the S3 call — e.g. an append-only local RDS/JSON log, or SQLite via RSQLite. Must survive a full app/process restart, not just an R session.
- A per-entry serial/sequence number, distinct from and orthogonal to the existing business UUIDs and `bump_sync_version()`'s table-level counters — used purely to track pending → synced state.
- A background retry mechanism that, on app boot, reads whatever's still pending in the local store (not just an in-memory `later::later()` chain that dies with the process) and keeps retrying until connectivity returns.
- Only clear/mark an entry synced once the S3 write is *confirmed* — not optimistically.
- Needs a design decision on scope: which save paths get this (manual invoices? provisions? liabilities? bancos movimientos? pagar_hoy staging? all of the above?) and how conflicts are handled if the same row was also edited by another session/process while offline.

## Context for whoever picks this up

- This is a live financial/treasury app (HopDesk, formerly Antigüedad de Saldos) — data integrity is the top priority, more important than shipping fast.
- Existing retry/backoff conventions to match in style (not architecture) live in `R/persistence.R` (`save_pagar_hoy_with_retry`, `s3_save_with_retry`, and the just-added `.s3_read_raw_with_retry`), `R/app_audit.R` (`.audit_queue`), and `R/ledger_module.R` (`.save_moves_deferred`).
- This codebase's test convention: extract real function bodies via `readLines()`/`parse()`/`eval()` rather than hand-reimplementing logic under test (see e.g. `tests/test_s3_read_retry_hardening.R`, `tests/test_confirm_agenda_resilience.R` for the pattern), mock `aws.s3::s3readRDS`/`s3saveRDS` via `utils::assignInNamespace()` with a guaranteed `tryCatch(..., finally = ...)` restore (never leave a mock unrestored — there was a real incident from this, see git history / project memory around "mock S3 leak").
- **Do not start implementing straight away.** Given the stakes, propose a design first (storage format, scope of which save paths get covered, conflict-handling approach) and get explicit sign-off before writing code. Work in reviewable stages, write tests per stage, and do not push/merge without explicit authorization.
