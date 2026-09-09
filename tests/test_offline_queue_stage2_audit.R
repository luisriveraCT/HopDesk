# =============================================================================
# tests/test_offline_queue_stage2_audit.R
#
# Two things this file does NOT overlap with tests/test_offline_queue_stage2.R:
#
# Part U -- regression guards for the specific implementation bugs found
# while BUILDING Stage 2 (not bugs in the design, bugs in getting the code
# and its own test file right). Each one reproduces the exact precondition
# that exposed the bug, rather than just re-running the fixed code and
# hoping the bug would still show up by coincidence.
#
# Part V -- adversarial correctness questions a reviewer with a finance +
# CS background would ask about a durable write queue for a treasury app,
# specifically: can anything be applied twice, can a crash cause a silent
# clobber to go undetected, is the staged data's integrity ever checked,
# and what happens when a SEPARATE, secondary write (the version stamp)
# fails independently of the main write it's supposed to accompany. Some of
# these come back as "here's a real gap, tested and documented, needs your
# call" rather than a clean pass -- flagged explicitly, not silently patched.
# =============================================================================
cat("=== test_offline_queue_stage2_audit ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) {
    cat(" PASS:", label, "\n"); .pass <<- .pass + 1L
  } else {
    cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L
  }
}
note <- function(label) cat(" NOTE:", label, "\n")

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

# =============================================================================
# Part U1 -- regression guard for the dependency-break bug: refactoring
# .queue_mark_client_pending() (an existing Stage 1 function) to call the
# new Stage 2 helpers broke tests/test_offline_queue_stage1.R, which extracts
# ONLY the original Stage 1 function list and has no reason to also pull in
# functions Stage 1 never needed. Fixed by keeping .queue_mark_client_pending()
# fully self-contained. This checks the SOURCE TEXT directly (not behavior)
# because behavior alone wouldn't catch a future re-introduction of the same
# mistake if whoever makes it also happens to extract the right functions in
# THEIR test -- the actual invariant that matters is "this specific function
# stays self-contained," independent of any one caller's extraction list.
# =============================================================================
cat("\n-- Part U1: .queue_mark_client_pending() stays self-contained (source-level) --\n")

{
  src <- paste(readLines("R/offline_queue.R", warn = FALSE), collapse = "\n")
  fn_start <- regexpr("\\.queue_mark_client_pending <- function", src)
  ok("U1 setup: found .queue_mark_client_pending() in the source", fn_start > 0)
  # Grab from the function's start to the next top-level "^.queue_" or
  # "^# ──" boundary -- good enough to isolate just this one function body
  # without a full parser.
  body_and_after <- substr(src, fn_start, fn_start + 3000L)
  end_marker <- regexpr("\\n(\\.queue_|# ──)", substr(body_and_after, 50, nchar(body_and_after)))
  fn_text <- if (end_marker > 0) substr(body_and_after, 1, end_marker + 48L) else body_and_after

  ok("U1a: .queue_mark_client_pending() does NOT call the Stage 2 helper .queue_read_pending_index()",
     !grepl(".queue_read_pending_index(", fn_text, fixed = TRUE))
  ok("U1b: .queue_mark_client_pending() does NOT call the Stage 2 helper .queue_write_pending_index()",
     !grepl(".queue_write_pending_index(", fn_text, fixed = TRUE))
  ok("U1c: .queue_mark_client_pending() still does its own inline read of the pending-index file (self-contained)",
     grepl("readRDS(path)", fn_text, fixed = TRUE))
}

# =============================================================================
# Part U2 -- regression guard for BOTH the async-later-leak bug and the
# log_action-invisible-across-local-envs bug, reproduced under the EXACT
# mechanism that exposed them: tests/_run_confirmed_logic.R's .run_module()
# sources every file via source(file, local = e), e <- new.env(parent =
# globalenv()) -- not a plain top-level Rscript run. This block:
#   1. plants a DECOY log_action() in globalenv() first, simulating "some
#      earlier file in the combined suite already left one there" (the
#      exact precondition that made the real bug fire);
#   2. wraps the whole thing in an OUTER later::later() counting mock, so
#      if the target file's OWN internal mock ever failed to intercept
#      something (the original leak), THIS outer layer would catch it;
#   3. sources the real tests/test_offline_queue_stage2.R via local = e,
#      exactly like .run_module() does, and checks its OWN pass/fail count
#      inside that env.
# =============================================================================
cat("\n-- Part U2: Stage 2's test file survives a decoy log_action() + a real background scheduler, under the actual .run_module() mechanism --\n")

{
  decoy_called <- FALSE
  assign("log_action", function(...) { decoy_called <<- TRUE; stop("decoy log_action should never be reached") },
         envir = globalenv())

  outer_later_calls <- 0L
  orig_later_before_outer_mock <- later::later
  outer_fake_later <- function(func, delay = 0, ...) { outer_later_calls <<- outer_later_calls + 1L; invisible(NULL) }
  utils::assignInNamespace("later", outer_fake_later, "later")

  # .queue_flush_loop_started is a genuine, deliberately-persistent globalenv
  # flag (by design -- it must survive a real process's whole lifetime).
  # If tests/test_offline_queue_stage2.R already ran once earlier in this
  # SAME combined-suite process, this is already TRUE; that's a real
  # leftover from an EARLIER, unrelated test run, not something this block
  # means to introduce. Reset it so the ONLY variable this simulation
  # actually changes is the decoy log_action() above, not an incidental
  # side effect of running the same file twice in one process.
  .GlobalEnv$.queue_flush_loop_started <- NULL

  e <- new.env(parent = globalenv())
  e$.pass <- 0L; e$.fail <- 0L
  load_err <- tryCatch({ source("tests/test_offline_queue_stage2.R", local = e); NULL },
                        error = function(err) conditionMessage(err))

  utils::assignInNamespace("later", orig_later_before_outer_mock, "later")

  ok("U2a: test_offline_queue_stage2.R loaded without an uncaught error under source(local=e)",
     is.null(load_err))
  ok("U2b: it reported 0 failures despite a decoy log_action() already sitting in globalenv()",
     is.null(load_err) && identical(e$.fail, 0L))
  ok("U2c: it reported the full 38 passes (not silently short-circuited)",
     is.null(load_err) && identical(e$.pass, 38L))
  ok("U2d: the decoy log_action() was NEVER actually invoked -- the file's own real stub correctly shadowed it",
     !decoy_called)
  ok("U2e: this outer, independent later::later mock saw ZERO real calls -- nothing scheduled a genuine background task that could leak past the target file's own mock",
     outer_later_calls == 0L)
  ok("U2f: later::later is the genuine original again afterward (no mock left installed, at any nesting level)",
     identical(later::later, orig_later_before_outer_mock))

  rm(list = "log_action", envir = globalenv())
}

# =============================================================================
# Setup for Part V: fresh extraction of everything, matching
# tests/test_offline_queue_stage2.R's own setup exactly.
# =============================================================================
.extract_fn("R/global.R", "%||%")

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

assign(".SYNC_REGISTRY", list(), envir = globalenv())
assign(".sync_versions", list(), envir = globalenv())
.extract_fn("R/sync_bus.R", "register_synced")
.extract_fn("R/sync_bus.R", "bump_sync_version")

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
.extract_fn("R/global.R", "S3_KEYS")

.GlobalEnv$.queue_flush_loop_started <- NULL  # Part U2 may have left this TRUE

.LOG_ACTION_CALLS <- list()
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

.TEST_QUEUE_ROOT <- file.path(tempdir(), paste0("hopdesk_queue_audit_", as.integer(Sys.time())))
dir.create(.TEST_QUEUE_ROOT, recursive = TRUE, showWarnings = FALSE)
options(hopdesk.queue_dir = .TEST_QUEUE_ROOT)

register_synced("moves_db",  "invoice_moves.rds",  function(client_id = NULL) NULL)
register_synced("manual_inv", "manual_invoices.rds", function(client_id = NULL) NULL)

.make_payload <- function(marker) {
  tf <- tempfile(fileext = ".rds")
  saveRDS(list(marker = marker), tf, compress = TRUE)
  tf
}

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

.STAMP_STORE <- new.env(parent = emptyenv())
.set_stamp <- function(client_id, versions_list) assign(.queue_slug(client_id), versions_list, envir = .STAMP_STORE)
.fake_read_stamps <- function(object, bucket, ...) {
  cid <- sub("^([^/]+)/.*$", "\\1", object)
  v <- get0(cid, envir = .STAMP_STORE, ifnotfound = NULL)
  if (is.null(v)) stop("NoSuchKey") else v
}

# =============================================================================
# Part V1 -- can an already-resolved entry (synced OR blocked) ever be
# replayed a second time? For a treasury app this is the sharpest possible
# question: a full-table snapshot replayed twice is harmless IF it's the
# exact same snapshot and nothing else changed in between, but "never
# re-select a resolved row" should hold as an invariant on its own, not as
# an accident of timing. Verified across several flush cycles, not just one.
# =============================================================================
cat("\n-- Part V1: resolved (synced/blocked) entries are never re-selected for replay --\n")

{
  cid <- "auditsynced"
  .set_stamp(cid, list(moves_db = NA_integer_))
  pf <- .make_payload("once-only")
  .queue_stage_entry(payload_path = pf, s3_key = paste0(cid, "/invoice_moves.rds"),
                      key = "invoice_moves.rds", client_id = cid)
  unlink(pf)

  put_calls <- 0L
  counting_put <- function(...) { put_calls <<- put_calls + 1L; invisible(TRUE) }
  .with_mock_s3(function(...) invisible(TRUE), counting_put, .fake_read_stamps, .queue_flush_client(cid))
  ok("V1a: the entry synced on the first flush", identical(.queue_read_manifest(cid)$status[1], "synced"))
  ok("V1b: exactly one upload happened for it", put_calls == 1L)

  # Run 5 MORE flush cycles. A correct implementation only ever looks at
  # status == "pending" rows, so none of these should touch put_object again
  # -- but this is exactly the kind of invariant worth pinning down with a
  # repeated-cycle test rather than trusting it holds after only one call.
  for (i in 1:5) .with_mock_s3(function(...) invisible(TRUE), counting_put, .fake_read_stamps, .queue_flush_client(cid))
  ok("V1c: after 5 additional flush cycles, still exactly ONE upload total -- a synced entry is never re-uploaded",
     put_calls == 1L)
  ok("V1d: still exactly one row in the manifest (no duplicate got created by any of the extra cycles)",
     nrow(.queue_read_manifest(cid)) == 1L)

  # Same invariant for a BLOCKED (not synced) terminal state.
  cid2 <- "auditblocked"
  bad_row <- data.frame(seq = 1L, s3_key = "someoneelse/invoice_moves.rds", table_name = "moves_db",
                         client_id = cid2, expected_sync_version = NA_integer_,
                         acting_user = NA_character_, acting_home_client_id = NA_character_,
                         status = "pending", staged_at = Sys.time(), synced_at = as.POSIXct(NA),
                         stringsAsFactors = FALSE)
  .queue_write_manifest(cid2, bad_row)
  .queue_mark_client_pending(cid2)
  .reset_log_calls()
  put_calls2 <- 0L
  counting_put2 <- function(...) { put_calls2 <<- put_calls2 + 1L; invisible(TRUE) }
  for (i in 1:4) .with_mock_s3(function(...) invisible(TRUE), counting_put2, .fake_read_stamps, .queue_flush_client(cid2))
  ok("V1e: a blocked (tenant-mismatch) entry never triggers an upload attempt, across 4 flush cycles",
     put_calls2 == 0L)
  ok("V1f: exactly ONE _queue_tenant_mismatch audit row total across all 4 cycles -- blocked is logged once, not on every tick",
     length(.LOG_ACTION_CALLS) == 1L &&
     identical(.LOG_ACTION_CALLS[[1]]$action, "_queue_tenant_mismatch"))
  note("V1g (design question, not a failure): once blocked, this entry has no further signal beyond that one audit row -- nothing periodically re-surfaces it to a human. Worth deciding whether blocked entries need a standing dashboard/report rather than relying on someone reading the audit log.")
}

# =============================================================================
# Part V2 -- the sharpest financial-integrity question: if this process
# crashes AFTER a successful upload but BEFORE that success is durably
# recorded (the manifest write happens once, after the whole per-tick batch,
# per .queue_flush_client()'s own design), the entry is retried on the next
# tick even though its data already reached S3 once. That's "at least once,"
# not "exactly once," delivery. The question that actually matters: if
# something ELSE legitimately wrote to that same table in the gap between
# the lost acknowledgment and the retry, does the retry still correctly
# detect and log that as a conflict -- or does the crash-recovery path
# silently skip the safety net that a normal first-attempt replay gets?
# =============================================================================
cat("\n-- Part V2: a crash-recovery retry (row still 'pending' despite an earlier successful upload) still triggers conflict detection --\n")

{
  cid <- "auditcrash"
  .set_stamp(cid, list(moves_db = 5L))
  pf <- .make_payload("pre-crash-write")
  .GlobalEnv$.sync_versions[["moves_db"]] <- 5L
  .queue_stage_entry(payload_path = pf, s3_key = paste0(cid, "/invoice_moves.rds"),
                      key = "invoice_moves.rds", client_id = cid)
  unlink(pf)

  # Simulate: the upload for this row ALREADY succeeded once (a real write
  # reached S3), but the crash happened before the manifest could be
  # rewritten to say so -- the row is (as it would be on restart) still
  # "pending", completely indistinguishable from a first attempt.
  #
  # In the gap since that lost acknowledgment, someone else legitimately
  # wrote to the SAME table (their own bump_sync_version() call moved the
  # real version from 5 to 9).
  .set_stamp(cid, list(moves_db = 9L))
  .reset_log_calls()

  .with_mock_s3(function(...) invisible(TRUE), function(...) invisible(TRUE), .fake_read_stamps,
                .queue_flush_client(cid))

  ok("V2a: the retried row still resolves to 'synced' (per the signed-off last-write-wins policy, the retry still replays)",
     identical(.queue_read_manifest(cid)$status[1], "synced"))
  ok("V2b: the safety net still fires on a crash-recovery retry -- a _conflict_replay row was logged, not silently skipped",
     length(.LOG_ACTION_CALLS) == 1L && identical(.LOG_ACTION_CALLS[[1]]$action, "_conflict_replay"))
  ok("V2c: the conflict description correctly shows the ORIGINAL expected version (5), not something recomputed from the retry",
     grepl("expected=5", .LOG_ACTION_CALLS[[1]]$description, fixed = TRUE) &&
     grepl("now=9", .LOG_ACTION_CALLS[[1]]$description, fixed = TRUE))

  # FIXED (was V2d's flagged gap): the pre-overwrite content is now backed up
  # locally before the replay's own upload happens, and the audit row names
  # exactly where. Verify the path named in the log actually exists on disk
  # and holds real content, not just that the sentence mentions one.
  backup_mentioned <- grepl("backed up at", .LOG_ACTION_CALLS[[1]]$description, fixed = TRUE)
  ok("V2d: the conflict audit row states the victim content was backed up (not 'could NOT be backed up')",
     backup_mentioned)
  backup_path <- sub(".*backed up at '([^']+)'.*", "\\1", .LOG_ACTION_CALLS[[1]]$description)
  ok("V2e: the backup file named in the audit row actually exists on local disk",
     backup_mentioned && file.exists(backup_path))
  ok("V2f: the backup file holds real, readable content (not an empty placeholder)",
     backup_mentioned && !is.null(tryCatch(readRDS(backup_path), error = function(e) NULL)))
}

# =============================================================================
# Part V3 -- FIXED (was a flagged gap): payload integrity is now validated
# before a staged file is ever uploaded to a live financial table's S3 key.
# Local disks are not immune to bit rot or a truncated write, especially
# over however long an entry might sit pending during a real outage.
# =============================================================================
cat("\n-- Part V3: staged payload integrity IS now validated before upload --\n")

{
  cid <- "auditcorrupt"
  .set_stamp(cid, list(moves_db = NA_integer_))
  pf <- .make_payload("will-be-corrupted")
  .queue_stage_entry(payload_path = pf, s3_key = paste0(cid, "/invoice_moves.rds"),
                      key = "invoice_moves.rds", client_id = cid)
  unlink(pf)

  mf <- .queue_read_manifest(cid)
  dest <- file.path(.queue_entries_dir(cid), sprintf("%012d.rds", mf$seq[1]))
  # Simulate bit rot / a truncated write: corrupt the staged file so it's no
  # longer valid RDS at all.
  writeLines("this is not a valid RDS file", dest)
  ok("V3 setup: the staged file on disk is now genuinely unreadable as RDS",
     inherits(tryCatch(readRDS(dest), error = function(e) e), "error"))

  put_calls <- 0L
  counting_put <- function(...) { put_calls <<- put_calls + 1L; invisible(TRUE) }
  .reset_log_calls()
  .with_mock_s3(function(...) invisible(TRUE), counting_put, .fake_read_stamps, .queue_flush_client(cid))

  ok("V3a: FIXED -- the corrupted file is never uploaded at all (put_object was not called)",
     put_calls == 0L)
  ok("V3b: FIXED -- the entry is marked 'blocked', not 'synced' -- it will not be retried forever either",
     identical(.queue_read_manifest(cid)$status[1], "blocked"))
  ok("V3c: FIXED -- a _queue_corrupt_payload audit row was written, naming the table and the bad file",
     length(.LOG_ACTION_CALLS) == 1L &&
     identical(.LOG_ACTION_CALLS[[1]]$action, "_queue_corrupt_payload") &&
     grepl("moves_db", .LOG_ACTION_CALLS[[1]]$description, fixed = TRUE))
  ok("V3d: FIXED -- the client is removed from the pending index (a corrupt local file can't self-heal by retrying)",
     !(cid %in% .queue_read_pending_index()))
}

# =============================================================================
# Part V4 -- FIXED (bump_sync_version() itself now reports success/failure,
# and replay now logs when it fails): bump_sync_version() writes a SEPARATE
# object (the version stamp) from the main data write .queue_replay_entry()
# just performed. If THAT secondary write fails, independent of the main
# write's success, the entry must still correctly recognize the real data
# was saved (not end up "pending" forever for data that's actually already
# safely in S3) -- AND the previously-silent staleness-broadcast failure
# must now leave a findable record instead of vanishing into a message().
# =============================================================================
cat("\n-- Part V4: bump_sync_version()'s own write failing does not un-deliver an already-successful main write, and is now logged --\n")

{
  cid <- "auditstampfail"
  .set_stamp(cid, list(moves_db = NA_integer_))
  pf <- .make_payload("main-write-ok")
  .queue_stage_entry(payload_path = pf, s3_key = paste0(cid, "/invoice_moves.rds"),
                      key = "invoice_moves.rds", client_id = cid)
  unlink(pf)

  main_put_ok <- FALSE
  # put_object is called for BOTH the main upload (object = ".../invoice_moves.rds")
  # AND, via bump_sync_version()'s own .s3_write() fallback, for the stamp
  # file (object = ".../sync_versions.rds") if s3saveRDS itself fails first.
  # Fail s3saveRDS always (forcing the stamp write through its fallback),
  # and fail put_object ONLY for the stamp key, succeeding for the main key.
  selective_put <- function(file, object, bucket, ...) {
    if (grepl("sync_versions\\.rds$", object)) stop("stamp write failed independently")
    main_put_ok <<- TRUE
    invisible(TRUE)
  }
  .reset_log_calls()
  .with_mock_s3(function(...) stop("s3saveRDS always fails in this scenario"),
                selective_put, .fake_read_stamps, .queue_flush_client(cid))

  ok("V4a: the main data upload actually succeeded", main_put_ok)
  ok("V4b: despite the version-stamp write failing, the entry is still correctly marked 'synced' -- the real data delivery is what matters, not the secondary staleness broadcast",
     identical(.queue_read_manifest(cid)$status[1], "synced"))
  ok("V4c: FIXED -- a _queue_stale_broadcast audit row was written naming the affected table, instead of the failure staying silent",
     length(.LOG_ACTION_CALLS) == 1L &&
     identical(.LOG_ACTION_CALLS[[1]]$action, "_queue_stale_broadcast") &&
     grepl("moves_db", .LOG_ACTION_CALLS[[1]]$description, fixed = TRUE))
  note("V4d (still true, by nature of the underlying mechanism, not something this fix changes): other sessions' staleness polling will not learn about this change until some OTHER write to the same table happens to bump the counter successfully later. The data is safe; the cross-session 'go re-fetch' signal for it can go quiet for a while -- but it is now a findable, auditable event instead of a silent one.")
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
unlink(.TEST_QUEUE_ROOT, recursive = TRUE, force = TRUE)
if (.fail > 0) stop("Tests FAILED.")
invisible(NULL)
