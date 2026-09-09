# =============================================================================
# tests/test_offline_queue_stage2.R
#
# Stage 2 of the durable offline write queue (see OFFLINE_WRITE_QUEUE_DESIGN.md
# and R/offline_queue.R): the background flush loop and per-entry replay,
# built on top of Stage 1's durable capture (tests/test_offline_queue_stage1.R,
# tests/test_offline_queue_review_findings.R -- both already green and
# untouched by this file).
#
# Convention matches the rest of the suite: real function bodies extracted
# via readLines()/parse()/eval(), never reimplemented; aws.s3 mocks via
# utils::assignInNamespace() wrapped in tryCatch(..., finally = ...), never
# left unrestored.
#
# One deliberate departure: log_action() itself is STUBBED (a simple call
# recorder), not extracted real. log_action()'s own correctness (audit-row
# shape, dual-write, NA-handling) is that function's own concern and already
# covered elsewhere -- what THIS file needs to verify is which action name,
# client_id, and description offline_queue.R's replay code calls it with,
# which a recorder proves more directly than pulling in log_action()'s full
# tibble/uuid/jsonlite/audit-buffer dependency chain would.
# =============================================================================
cat("=== test_offline_queue_stage2 ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) {
    cat(" PASS:", label, "\n"); .pass <<- .pass + 1L
  } else {
    cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L
  }
}

.extract_fn <- function(file, fn_name) {
  exprs <- parse(file, keep.source = FALSE)
  for (e in exprs) {
    if (is.call(e) && length(e) >= 2 &&
        identical(e[[1]], as.name("<-")) &&
        identical(e[[2]], as.name(fn_name))) {
      assign(fn_name, eval(e[[3]], envir = globalenv()), envir = globalenv())
      return(invisible(TRUE))
    }
  }
  stop(sprintf("%s not found in %s", fn_name, file))
}

# The REAL %||% (R/global.R) -- confirmed by reading global.R directly that
# it substitutes on a length-1 NA as well as NULL (some comments elsewhere in
# the codebase, e.g. app_audit.R's log_action()/format_session_duration(),
# describe it as NULL-only; that's stale relative to the current definition
# extracted here, not a discrepancy this test needs to resolve).
.extract_fn("R/global.R", "%||%")

# ── Extract the real offline-queue functions (Stage 1 + Stage 2) ───────────
for (fn in c(".queue_root_dir", ".queue_slug", ".queue_valid_client_id",
             ".queue_client_dir", ".queue_entries_dir", ".queue_manifest_path",
             ".queue_pending_index_path", ".queue_manifest_schema",
             ".queue_read_manifest", ".queue_write_manifest", ".queue_next_seq",
             ".QUEUE_LOCK_STALE_SECS", ".queue_lock_dir", ".queue_with_client_lock",
             ".queue_read_pending_index", ".queue_write_pending_index",
             ".queue_mark_client_pending", ".queue_unmark_client_pending",
             ".queue_table_name_for_key", ".queue_stage_entry",
             ".QUEUE_FLUSH_INTERVAL_SECS", ".queue_start_flush_loop",
             ".queue_flush_tick", ".queue_flush_all_pending", ".queue_flush_client",
             ".queue_conflict_backups_dir", ".queue_conflict_backup_path",
             ".queue_replay_entry"))
  .extract_fn("R/offline_queue.R", fn)

# ── Extract the real sync_bus pieces (registry + version bump) ─────────────
assign(".SYNC_REGISTRY", list(), envir = globalenv())
assign(".sync_versions", list(), envir = globalenv())
.extract_fn("R/sync_bus.R", "register_synced")
.extract_fn("R/sync_bus.R", "bump_sync_version")

# ── Extract the real S3 layer bump_sync_version() and replay both depend on ─
.extract_fn("R/persistence.R", ".client_id")
.extract_fn("R/persistence.R", ".s3_key")
.extract_fn("R/persistence.R", ".s3_bucket")
.extract_fn("R/persistence.R", ".s3_write")
.extract_fn("R/persistence.R", ".s3_read_raw_with_retry")
.extract_fn("R/persistence.R", ".s3_read_with")
.extract_fn("R/persistence.R", ".s3_missing_cache_hit")
.extract_fn("R/persistence.R", ".s3_missing_cache_set")
assign(".s3_missing_cache", new.env(parent = emptyenv()), envir = globalenv())
assign(".S3_MISSING_CACHE_TTL", 60, envir = globalenv())
assign(".s3_preload_cache", new.env(parent = emptyenv()), envir = globalenv())

# The real S3_KEYS list -- only S3_KEYS$sync_versions is actually exercised,
# but extracting the real list (rather than a hand-built stand-in) means a
# future rename of that key would break this test loudly instead of silently
# testing against a stale literal.
.extract_fn("R/global.R", "S3_KEYS")

# log_action() stub -- see file header for why this is a recorder, not the
# real function. Assigned into globalenv() explicitly, matching .extract_fn():
# .queue_replay_entry() was ALSO extracted into globalenv(), so it calls
# log_action() by name via lexical scoping starting from globalenv() -- this
# suite's runner (_run_confirmed_logic.R's .run_module()) sources every test
# file into its own local env via source(file, local=e), so a plain
# `log_action <- function(...)` here would live in e, invisible to a
# globalenv-scoped caller. Confirmed the hard way: without this, the stub
# was silently never invoked in the combined suite -- whatever a DIFFERENT,
# earlier test file had left as the real log_action() in globalenv ran
# instead, and crashed on this file's minimal test fixtures.
.LOG_ACTION_CALLS <- list()
# A bare top-level `.LOG_ACTION_CALLS <<- list()` between Parts would NOT
# reset this: under this suite's runner (.run_module() sources each file via
# source(file, local=e)), superassignment used directly at top level starts
# searching from e's PARENT (globalenv()), skipping e itself -- so it would
# silently create/update an unrelated binding in globalenv() while this
# actual, e-scoped counter (the one the log_action() stub and every check
# below actually read/write) was never cleared. Wrapping the reset in its
# own function -- exactly how ok() already updates .pass/.fail correctly --
# makes the <<- inside it resolve from THIS function's closure (e) instead,
# which does correctly find and update this line's binding. Confirmed the
# hard way: a bare reset passed standalone (no e/globalenv split exists in a
# plain Rscript run) but silently failed to reset anything once run inside
# the combined suite.
.reset_log_calls <- function() { .LOG_ACTION_CALLS <<- list() }
assign("log_action", function(user, module, action, description, target_id = NA_character_,
                               s3_key = NA_character_, metadata = NULL, client_id = NULL,
                               viewer_home_client_id = NULL) {
  .LOG_ACTION_CALLS[[length(.LOG_ACTION_CALLS) + 1L]] <<- list(
    user = user, module = module, action = action, description = description,
    client_id = client_id, viewer_home_client_id = viewer_home_client_id
  )
  invisible(NULL)
}, envir = globalenv())

Sys.setenv(CLIENT_ID = "hd-admin", S3_BUCKET = "mock-bucket")

.TEST_QUEUE_ROOT <- file.path(tempdir(), paste0("hopdesk_queue_stage2_", as.integer(Sys.time())))
dir.create(.TEST_QUEUE_ROOT, recursive = TRUE, showWarnings = FALSE)
options(hopdesk.queue_dir = .TEST_QUEUE_ROOT)

register_synced("moves_db",  "invoice_moves.rds",  function(client_id = NULL) NULL)
register_synced("manual_inv", "manual_invoices.rds", function(client_id = NULL) NULL)

.make_payload <- function(marker) {
  tf <- tempfile(fileext = ".rds")
  saveRDS(list(marker = marker), tf, compress = TRUE)
  tf
}

# Mocks all three aws.s3 entry points this file's code paths can reach:
#   s3saveRDS/put_object -- bump_sync_version()'s own .s3_write() call
#   put_object            -- .queue_replay_entry()'s direct upload
#   s3readRDS             -- .s3_read_with()'s read of the version stamp
.with_mock_s3 <- function(fake_save, fake_put, fake_read, expr) {
  orig_save <- aws.s3::s3saveRDS
  orig_put  <- aws.s3::put_object
  orig_read <- aws.s3::s3readRDS
  utils::assignInNamespace("s3saveRDS", fake_save, "aws.s3")
  utils::assignInNamespace("put_object", fake_put, "aws.s3")
  utils::assignInNamespace("s3readRDS", fake_read, "aws.s3")
  tryCatch(expr, finally = {
    utils::assignInNamespace("s3saveRDS", orig_save, "aws.s3")
    utils::assignInNamespace("put_object", orig_put, "aws.s3")
    utils::assignInNamespace("s3readRDS", orig_read, "aws.s3")
  })
}
.orig_s3saveRDS <- aws.s3::s3saveRDS
.orig_put_object <- aws.s3::put_object
.orig_s3readRDS <- aws.s3::s3readRDS

# A version-stamp store: fake_read below serves whatever's in here per
# client_id, so each test block can set exactly the "current version" it
# wants replay to see.
.STAMP_STORE <- new.env(parent = emptyenv())
.set_stamp <- function(client_id, versions_list) {
  assign(.queue_slug(client_id), versions_list, envir = .STAMP_STORE)
}
.fake_read_stamps <- function(object, bucket, ...) {
  # object is the full, already-prefixed key, e.g. "acme/sync_versions.rds"
  cid <- sub("^([^/]+)/.*$", "\\1", object)
  v <- get0(cid, envir = .STAMP_STORE, ifnotfound = NULL)
  if (is.null(v)) stop("NoSuchKey") else v
}

# .queue_start_flush_loop() calls the REAL later::later(..., delay = 0) on its
# first invocation. In a plain Rscript this normally just sits dormant (there
# is no active httpuv/Shiny event loop pumping it) -- EXCEPT this suite's own
# runner (_run_confirmed_logic.R's .run_module()) sources every test file
# into the SAME process via source(file, local=e), so a real callback left
# pending by THIS file is not scoped to this file at all: it can fire later,
# asynchronously, mid-way through a LATER, unrelated test file in the same
# combined run. Confirmed empirically while writing this suite: leaving
# .queue_start_flush_loop()'s later::later() call for real triggered a
# genuine background flush attempt (a REAL, if harmlessly-failed, aws.s3
# network call) partway through an unrelated later Part of THIS file. later
# is mocked for this file's entire duration, from before Part T1, specifically
# to make that structurally impossible -- not just to keep Part T2's
# assertions clean.
.with_mock_later <- function(fake_later, expr) {
  orig <- later::later
  utils::assignInNamespace("later", fake_later, "later")
  tryCatch(expr, finally = utils::assignInNamespace("later", orig, "later"))
}
.orig_later <- later::later

.LATER_CALLS <- list()
# Same reasoning as .reset_log_calls() above -- see its comment.
.reset_later_calls <- function() { .LATER_CALLS <<- list() }
.fake_later_recorder <- function(func, delay = 0, ...) {
  .LATER_CALLS[[length(.LATER_CALLS) + 1L]] <<- list(delay = delay)
  invisible(NULL)
}
utils::assignInNamespace("later", .fake_later_recorder, "later")

# Everything from here through the end of Part T10 runs with later::later
# mocked (installed above). Wrapped in tryCatch/finally, not just relying on
# reaching the bottom of the script normally, so an unexpected error in any
# Part still restores the real later::later before this process's later-queue
# is shared with whatever test file _run_confirmed_logic.R's .run_module()
# sources next.
tryCatch({

# =============================================================================
# Part T1 -- .queue_start_flush_loop(): idempotency guard.
# =============================================================================
cat("\n-- Part T1: .queue_start_flush_loop() idempotency --\n")

{
  ok("T1a: the guard flag is not set before the first call",
     !isTRUE(.GlobalEnv$.queue_flush_loop_started))
  first  <- .queue_start_flush_loop()
  ok("T1b: the first call returns TRUE (it actually scheduled the loop)", isTRUE(first))
  ok("T1c: the guard flag is now TRUE", isTRUE(.GlobalEnv$.queue_flush_loop_started))
  ok("T1c2: the (mocked) later::later was actually invoked exactly once by that first call",
     length(.LATER_CALLS) == 1L && identical(.LATER_CALLS[[1]]$delay, 0))
  second <- .queue_start_flush_loop()
  ok("T1d: a second call in the same process returns FALSE (no-op, does not reschedule)",
     identical(second, FALSE))
  third <- .queue_start_flush_loop()
  ok("T1e: a third call is still a no-op (not just 'exactly twice')", identical(third, FALSE))
  ok("T1f: still exactly one later::later call total -- the no-op calls truly scheduled nothing further",
     length(.LATER_CALLS) == 1L)
}

# =============================================================================
# Part T2 -- .queue_flush_tick() always reschedules itself, regardless of
# whether the flush work succeeded, failed, or found nothing to do.
# =============================================================================
cat("\n-- Part T2: .queue_flush_tick() always reschedules --\n")

{
  .reset_later_calls()
  # Nothing pending anywhere -- .queue_flush_all_pending() should do nothing,
  # but the tick must still reschedule itself.
  .queue_write_pending_index(character(0))
  .queue_flush_tick()
  ok("T2a: exactly one reschedule call happened", length(.LATER_CALLS) == 1L)
  ok("T2b: the reschedule uses the real flush interval (15s), not some other value",
     identical(.LATER_CALLS[[1]]$delay, .QUEUE_FLUSH_INTERVAL_SECS))
}

ok("T3: aws.s3::s3saveRDS/put_object/s3readRDS are all still the real functions (no leaked mock) after Parts T1-T2",
   identical(aws.s3::s3saveRDS, .orig_s3saveRDS) && identical(aws.s3::put_object, .orig_put_object) &&
   identical(aws.s3::s3readRDS, .orig_s3readRDS))

# =============================================================================
# Part T4 -- .queue_flush_client(): the full happy path, including a genuine
# version-match vs version-mismatch pair within the SAME client.
# =============================================================================
cat("\n-- Part T4: full replay, match vs conflict, within one client --\n")

{
  cid <- "acme"
  .set_stamp(cid, list(moves_db = 5L, manual_inv = 9L))  # "current" versions in S3

  # Entry 1: expected matches current (5 == 5) -- no conflict.
  pf1 <- .make_payload("moves-write")
  .GlobalEnv$.sync_versions[["moves_db"]] <- 5L
  staged1 <- .queue_stage_entry(payload_path = pf1, s3_key = paste0(cid, "/invoice_moves.rds"),
                                 key = "invoice_moves.rds", client_id = cid)
  unlink(pf1)

  # Entry 2: expected (3) does NOT match current (9) -- a conflict.
  pf2 <- .make_payload("manual-write")
  .GlobalEnv$.sync_versions[["manual_inv"]] <- 3L
  staged2 <- .queue_stage_entry(payload_path = pf2, s3_key = paste0(cid, "/manual_invoices.rds"),
                                 key = "manual_invoices.rds", client_id = cid)
  unlink(pf2)

  ok("T4a: both entries staged successfully (setup sanity check)", isTRUE(staged1) && isTRUE(staged2))

  fake_save <- function(...) invisible(TRUE)   # bump_sync_version()'s own .s3_write() succeeds
  fake_put  <- function(...) invisible(TRUE)   # replay's direct upload succeeds
  .reset_log_calls()

  .with_mock_s3(fake_save, fake_put, .fake_read_stamps, .queue_flush_client(cid))

  mf <- .queue_read_manifest(cid)
  ok("T4b: both rows are now 'synced'", all(mf$status == "synced"))
  ok("T4c: both rows have a non-NA synced_at timestamp", all(!is.na(mf$synced_at)))
  ok("T4d: exactly ONE _conflict_replay audit call was made (only the mismatched entry)",
     sum(vapply(.LOG_ACTION_CALLS, function(c) identical(c$action, "_conflict_replay"), logical(1))) == 1L)
  conflict_call <- Filter(function(c) identical(c$action, "_conflict_replay"), .LOG_ACTION_CALLS)[[1]]
  ok("T4e: the conflict call is for the right client_id", identical(conflict_call$client_id, cid))
  ok("T4f: the conflict call's description mentions both the expected and actual version numbers",
     grepl("expected=3", conflict_call$description, fixed = TRUE) &&
     grepl("now=9", conflict_call$description, fixed = TRUE))
  ok("T4g: no _queue_tenant_mismatch call happened (neither entry was actually mismatched on tenant)",
     !any(vapply(.LOG_ACTION_CALLS, function(c) identical(c$action, "_queue_tenant_mismatch"), logical(1))))
  ok("T4h: the client is removed from pending_client_ids.rds once nothing is left pending",
     !(cid %in% .queue_read_pending_index()))
  ok("T4i: bump_sync_version() actually ran for both tables (in-memory counters advanced past what was staged)",
     .GlobalEnv$.sync_versions[["moves_db"]] > 5L && .GlobalEnv$.sync_versions[["manual_inv"]] > 3L)
}

# =============================================================================
# Part T5 -- upload still failing: the row stays pending, unchanged, and the
# client stays in the pending index for the next tick. Never dropped.
# =============================================================================
cat("\n-- Part T5: S3 still down -- entry stays pending, nothing lost --\n")

{
  cid <- "stilldown"
  pf <- .make_payload("retry-me")
  .queue_stage_entry(payload_path = pf, s3_key = paste0(cid, "/invoice_moves.rds"),
                      key = "invoice_moves.rds", client_id = cid)
  unlink(pf)
  before <- .queue_read_manifest(cid)

  fake_save <- function(...) stop("still down")
  fake_put  <- function(...) stop("still down")
  .with_mock_s3(fake_save, fake_put, .fake_read_stamps, .queue_flush_client(cid))

  after <- .queue_read_manifest(cid)
  ok("T5a: the row is still 'pending' (not synced, not blocked, not dropped)",
     identical(after$status[1], "pending"))
  ok("T5b: the row is otherwise byte-for-byte unchanged (same seq, s3_key, staged_at)",
     identical(before$seq, after$seq) && identical(before$s3_key, after$s3_key) &&
     identical(before$staged_at, after$staged_at))
  ok("T5c: the client is STILL in pending_client_ids.rds (will be retried next tick)",
     cid %in% .queue_read_pending_index())

  # Retrying again (simulating the next 15s tick) with S3 now back up must
  # still work -- confirms a failed attempt doesn't leave anything corrupted.
  fake_save2 <- function(...) invisible(TRUE)
  fake_put2  <- function(...) invisible(TRUE)
  .set_stamp(cid, list(moves_db = NA_integer_))
  .with_mock_s3(fake_save2, fake_put2, .fake_read_stamps, .queue_flush_client(cid))
  ok("T5d: once S3 is back, the SAME row (never re-staged, never duplicated) now syncs",
     identical(.queue_read_manifest(cid)$status[1], "synced"))
  ok("T5e: still exactly one row for this client (no duplicate got created across the two attempts)",
     nrow(.queue_read_manifest(cid)) == 1L)
}

# =============================================================================
# Part T6 -- NA expected_sync_version: replay proceeds, but silently (no
# _conflict_replay row) -- per the signed-off policy: no baseline captured
# means genuinely nothing to compare, not "assume the worst."
# =============================================================================
cat("\n-- Part T6: NA baseline replays silently, no false conflict --\n")

{
  cid <- "nabaseline"
  pf <- .make_payload("no-baseline")
  # Deliberately clear .sync_versions[["moves_db"]] first (whatever earlier
  # parts left it at), so .queue_stage_entry() captures expected_sync_version
  # = NA, as if this process had never seen a bump for this table before.
  .GlobalEnv$.sync_versions[["moves_db"]] <- NULL
  .queue_stage_entry(payload_path = pf, s3_key = paste0(cid, "/invoice_moves.rds"),
                      key = "invoice_moves.rds", client_id = cid)
  unlink(pf)
  ok("T6a: expected_sync_version was indeed captured as NA (setup sanity check)",
     is.na(.queue_read_manifest(cid)$expected_sync_version[1]))

  .set_stamp(cid, list(moves_db = 40L))  # some real "current" version exists in S3
  .reset_log_calls()
  .with_mock_s3(function(...) invisible(TRUE), function(...) invisible(TRUE), .fake_read_stamps,
                .queue_flush_client(cid))

  ok("T6b: the entry still synced despite the missing baseline", identical(.queue_read_manifest(cid)$status[1], "synced"))
  ok("T6c: no _conflict_replay row was logged (nothing to compare against)",
     !any(vapply(.LOG_ACTION_CALLS, function(c) identical(c$action, "_conflict_replay"), logical(1))))
}

# =============================================================================
# Part T7 -- tenant-prefix mismatch AT REPLAY TIME specifically (not stage
# time): a manifest row crafted directly (bypassing .queue_stage_entry()'s
# own identical check, which would refuse to ever create one) to confirm the
# replay-side defense-in-depth independently catches it too.
# =============================================================================
cat("\n-- Part T7: tenant-prefix mismatch caught at replay, not just at stage time --\n")

{
  cid <- "victimclient"
  bad_row <- data.frame(
    seq = 1L, s3_key = "attackerClient/invoice_moves.rds", table_name = "moves_db",
    client_id = cid, expected_sync_version = NA_integer_,
    acting_user = NA_character_, acting_home_client_id = NA_character_,
    status = "pending", staged_at = Sys.time(), synced_at = as.POSIXct(NA),
    stringsAsFactors = FALSE
  )

  put_calls <- 0L
  fake_put_counting <- function(...) { put_calls <<- put_calls + 1L; invisible(TRUE) }
  .reset_log_calls()

  result <- .with_mock_s3(function(...) invisible(TRUE), fake_put_counting, .fake_read_stamps,
                           .queue_replay_entry(bad_row, cid))

  ok("T7a: replay returns 'blocked' for a client_id/s3_key mismatch", identical(result, "blocked"))
  ok("T7b: put_object was NEVER called -- no upload attempt for a mismatched entry", put_calls == 0L)
  ok("T7c: a _queue_tenant_mismatch audit row was written",
     any(vapply(.LOG_ACTION_CALLS, function(c) identical(c$action, "_queue_tenant_mismatch"), logical(1))))
}

# =============================================================================
# Part T8 -- the staged payload file itself is missing (e.g. removed out of
# band): replay fails closed ("blocked"), not an error, not an infinite
# pending retry for something retrying could never fix.
# =============================================================================
cat("\n-- Part T8: missing staged payload file fails closed as 'blocked' --\n")

{
  cid <- "missingpayload"
  row <- data.frame(
    seq = 999L, s3_key = paste0(cid, "/invoice_moves.rds"), table_name = "moves_db",
    client_id = cid, expected_sync_version = NA_integer_,
    acting_user = NA_character_, acting_home_client_id = NA_character_,
    status = "pending", staged_at = Sys.time(), synced_at = as.POSIXct(NA),
    stringsAsFactors = FALSE
  )
  # Deliberately never create entries/000000000999.rds for this client.
  dir.create(.queue_client_dir(cid), recursive = TRUE, showWarnings = FALSE)
  result <- .queue_replay_entry(row, cid)
  ok("T8: replay returns 'blocked' when the staged file is gone, not an error", identical(result, "blocked"))
}

# =============================================================================
# Part T9 -- one entry throwing unexpectedly does not stop the rest of that
# same client's backlog from being processed in the same flush.
# =============================================================================
cat("\n-- Part T9: one bad entry does not stop the rest of the client's backlog --\n")

{
  cid <- "partialbad"
  pf1 <- .make_payload("first"); pf2 <- .make_payload("second")
  .queue_stage_entry(payload_path = pf1, s3_key = paste0(cid, "/invoice_moves.rds"),
                      key = "invoice_moves.rds", client_id = cid)
  .queue_stage_entry(payload_path = pf2, s3_key = paste0(cid, "/manual_invoices.rds"),
                      key = "manual_invoices.rds", client_id = cid)
  unlink(pf1); unlink(pf2)

  .set_stamp(cid, list(moves_db = NA_integer_, manual_inv = NA_integer_))
  call_n <- 0L
  # The FIRST put_object call (for seq=1) throws a non-standard condition;
  # the second (seq=2) succeeds -- proves .queue_flush_client()'s per-row
  # tryCatch isolates one bad entry from the rest of that same client's queue.
  flaky_put <- function(...) {
    call_n <<- call_n + 1L
    if (call_n == 1L) stop(structure(class = c("weirdError", "error", "condition"),
                                      list(message = "unexpected shape", call = NULL)))
    invisible(TRUE)
  }
  .with_mock_s3(function(...) invisible(TRUE), flaky_put, .fake_read_stamps, .queue_flush_client(cid))

  mf <- .queue_read_manifest(cid)
  ok("T9a: the first (failing) entry is still 'pending', not lost or corrupted",
     identical(mf$status[mf$seq == 1], "pending"))
  ok("T9b: the second entry for the SAME client still synced despite the first one throwing",
     identical(mf$status[mf$seq == 2], "synced"))
  ok("T9c: the client is still in the pending index (entry 1 still needs a retry)",
     cid %in% .queue_read_pending_index())
}

# =============================================================================
# Part T10 -- .queue_flush_all_pending() across several clients: replay
# order within a client follows seq, and no cross-client mixing happens
# during the flush any more than it did during staging (Stage 1).
# =============================================================================
cat("\n-- Part T10: multi-client flush -- seq order + no cross-contamination --\n")

{
  clients <- sprintf("multi%02d", 1:6)
  upload_order <- list()
  for (cid in clients) {
    .set_stamp(cid, list(moves_db = NA_integer_, manual_inv = NA_integer_))
    pf_a <- .make_payload(paste0(cid, "-a")); pf_b <- .make_payload(paste0(cid, "-b"))
    .queue_stage_entry(payload_path = pf_a, s3_key = paste0(cid, "/manual_invoices.rds"),
                        key = "manual_invoices.rds", client_id = cid)
    .queue_stage_entry(payload_path = pf_b, s3_key = paste0(cid, "/invoice_moves.rds"),
                        key = "invoice_moves.rds", client_id = cid)
    unlink(pf_a); unlink(pf_b)
  }

  recording_put <- function(file, object, bucket, ...) {
    upload_order[[length(upload_order) + 1L]] <<- object
    invisible(TRUE)
  }
  .with_mock_s3(function(...) invisible(TRUE), recording_put, .fake_read_stamps,
                .queue_flush_all_pending())

  all_clean <- TRUE
  for (cid in clients) {
    mf <- .queue_read_manifest(cid)
    if (!all(mf$status == "synced")) all_clean <- FALSE
    this_client_uploads <- upload_order[startsWith(vapply(upload_order, identity, ""), paste0(cid, "/"))]
    if (!identical(this_client_uploads,
                    list(paste0(cid, "/manual_invoices.rds"), paste0(cid, "/invoice_moves.rds"))))
      all_clean <- FALSE
  }
  ok("T10a: all 6 clients' entries synced", all_clean)
  ok("T10b: within each client, upload order followed seq order (1 then 2), not manifest-row or table-name order",
     all_clean)
  ok("T10c: pending_client_ids.rds is empty afterward -- every client fully drained",
     length(.queue_read_pending_index()) == 0L)
}

}, finally = utils::assignInNamespace("later", .orig_later, "later"))

ok("T11: aws.s3::s3saveRDS/put_object/s3readRDS and later::later are all still the real functions after every mocked block above (no leaked mock)",
   identical(aws.s3::s3saveRDS, .orig_s3saveRDS) && identical(aws.s3::put_object, .orig_put_object) &&
   identical(aws.s3::s3readRDS, .orig_s3readRDS) && identical(later::later, .orig_later))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
unlink(.TEST_QUEUE_ROOT, recursive = TRUE, force = TRUE)
if (.fail > 0) stop("Tests FAILED.")
invisible(NULL)
