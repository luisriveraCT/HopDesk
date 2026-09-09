# =============================================================================
# tests/test_offline_queue_review_findings.R
#
# Independent adversarial review of R/offline_queue.R (Stage 1 of the durable
# offline write queue, see OFFLINE_WRITE_QUEUE_DESIGN.md), on top of the
# author's own tests/test_offline_queue_stage1.R (312 assertions, already
# re-run and passing). This file exists to probe things that suite doesn't:
# a genuine race condition, a few edge cases in the eligibility/payload
# checks, and a sanity check of the real (non-test-overridden) default queue
# path. Convention matches the rest of the suite: real function bodies
# extracted via readLines()/parse()/eval(), never reimplemented.
# =============================================================================
cat("=== test_offline_queue_review_findings ===\n\n")

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
assign("%||%", function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x, envir = globalenv())

for (fn in c(".queue_root_dir", ".queue_slug", ".queue_valid_client_id",
             ".queue_client_dir", ".queue_entries_dir", ".queue_manifest_path",
             ".queue_pending_index_path", ".queue_manifest_schema",
             ".queue_read_manifest", ".queue_write_manifest", ".queue_next_seq",
             ".QUEUE_LOCK_STALE_SECS", ".queue_lock_dir", ".queue_with_client_lock",
             ".queue_mark_client_pending", ".queue_table_name_for_key",
             ".queue_stage_entry"))
  .extract_fn("R/offline_queue.R", fn)

assign(".SYNC_REGISTRY", list(), envir = globalenv())
.extract_fn("R/sync_bus.R", "register_synced")

.TEST_QUEUE_ROOT <- file.path(tempdir(), paste0("hopdesk_queue_review_", as.integer(Sys.time())))
dir.create(.TEST_QUEUE_ROOT, recursive = TRUE, showWarnings = FALSE)
options(hopdesk.queue_dir = .TEST_QUEUE_ROOT)

.make_payload <- function(marker) {
  tf <- tempfile(fileext = ".rds")
  saveRDS(list(marker = marker), tf, compress = TRUE)
  tf
}

register_synced("moves_db", "invoice_moves.rds", function(client_id = NULL) NULL)

# =============================================================================
# Part R1 -- FIXED 2026-09-09: .queue_next_seq() was read-then-increment
# with no lock held across the read and the eventual manifest write -- two
# overlapping calls to .queue_stage_entry() for the SAME client could
# compute the identical seq number. Fixed by wrapping seq-generation
# through the manifest write in .queue_with_client_lock(), an exclusive-
# directory-creation mutex (see its own header comment in R/offline_queue.R
# for why this needed no new package dependency). This part now proves,
# in order: (1) the raw underlying primitive .queue_next_seq() is STILL not
# atomic in isolation -- that's expected and fine, since real callers never
# invoke it outside the lock anymore; (2) the lock genuinely blocks a
# second acquirer for the same client while held; (3) it does NOT block a
# DIFFERENT client (no cross-client contention); (4) a stale lock (left
# behind by a process that died mid-critical-section) is reclaimed rather
# than wedging that client's queue forever; (5) the actual entry point,
# .queue_stage_entry(), now serializes two back-to-back calls into
# genuinely distinct, non-colliding seq numbers.
# =============================================================================
cat("\n-- Part R1: seq-collision race -- now fixed via .queue_with_client_lock() --\n")

{
  cid <- "race_client_r1"
  seq_first  <- .queue_next_seq(cid)
  seq_second <- .queue_next_seq(cid)  # no write happened between these two reads
  ok("R1a: the RAW .queue_next_seq() primitive is still not atomic in isolation (expected -- real callers never invoke it outside the lock anymore, see R1e below)",
     identical(seq_first, seq_second))
}

{
  # R1b: while a client's lock is held, a second acquire attempt for the
  # SAME client genuinely blocks (times out, rather than sailing through as
  # if no lock existed at all).
  cid <- "race_client_r1b"
  dir.create(.queue_client_dir(cid), recursive = TRUE, showWarnings = FALSE)
  held <- dir.create(.queue_lock_dir(cid), showWarnings = FALSE)
  ok("R1b setup: manually acquired the lock directory for this client", isTRUE(held))

  t0 <- Sys.time()
  err <- tryCatch({ .queue_with_client_lock(cid, function() "should never run", max_wait = 0.3, poll = 0.05); "no error" },
                  error = function(e) conditionMessage(e))
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  ok("R1b: a second acquire for the SAME (already-held, fresh) lock times out with a clear error, it does not silently proceed",
     grepl("could not acquire client lock", err, fixed = TRUE))
  ok("R1b: the timeout actually waited close to max_wait (genuinely blocked, not an instant no-op)",
     elapsed >= 0.25)
  unlink(.queue_lock_dir(cid), recursive = TRUE, force = TRUE)
}

{
  # R1c: a lock held for client A does not block client B at all (no
  # cross-client contention -- each client's mutex is independent).
  cid_a <- "race_client_r1c_a"; cid_b <- "race_client_r1c_b"
  dir.create(.queue_client_dir(cid_a), recursive = TRUE, showWarnings = FALSE)
  dir.create(.queue_lock_dir(cid_a), showWarnings = FALSE)
  ran <- tryCatch(.queue_with_client_lock(cid_b, function() "ran", max_wait = 0.3), error = function(e) "blocked")
  ok("R1c: client B's lock is unaffected by client A's held lock (no cross-client contention)",
     identical(ran, "ran"))
  unlink(.queue_lock_dir(cid_a), recursive = TRUE, force = TRUE)
}

{
  # R1d: a STALE lock (left by a process that died mid-critical-section --
  # simulated by backdating the lock directory's mtime past
  # .QUEUE_LOCK_STALE_SECS) is reclaimed, not waited out to the full
  # max_wait -- a crash can never permanently wedge one client's queue.
  cid <- "race_client_r1d"
  dir.create(.queue_client_dir(cid), recursive = TRUE, showWarnings = FALSE)
  dir.create(.queue_lock_dir(cid), showWarnings = FALSE)
  old_time <- Sys.time() - (.QUEUE_LOCK_STALE_SECS + 5)
  Sys.setFileTime(.queue_lock_dir(cid), old_time)

  t0 <- Sys.time()
  result <- .queue_with_client_lock(cid, function() "reclaimed-and-ran", max_wait = 5, poll = 0.05)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  ok("R1d: a stale (older than .QUEUE_LOCK_STALE_SECS) lock is reclaimed and fn() actually runs",
     identical(result, "reclaimed-and-ran"))
  ok("R1d: reclaiming a stale lock is fast, not a wait-out-the-full-timeout affair",
     elapsed < 2)
}

{
  # R1e: the actual entry point -- two back-to-back .queue_stage_entry()
  # calls for the same client now produce genuinely distinct seq numbers,
  # not a collision (this is the real-world guarantee that matters; R1a
  # above shows the raw primitive alone still wouldn't provide it).
  cid <- "race_client_r1e"
  pf1 <- .make_payload("racer-1"); pf2 <- .make_payload("racer-2")
  s3key <- paste0(cid, "/invoice_moves.rds")
  staged1 <- .queue_stage_entry(payload_path = pf1, s3_key = s3key, key = "invoice_moves.rds", client_id = cid)
  staged2 <- .queue_stage_entry(payload_path = pf2, s3_key = s3key, key = "invoice_moves.rds", client_id = cid)
  mf <- .queue_read_manifest(cid)
  ok("R1e: both staging calls succeeded", isTRUE(staged1) && isTRUE(staged2))
  ok("R1e: exactly 2 manifest rows exist, not 1 (no entry silently lost to a collision)", nrow(mf) == 2L)
  ok("R1e: the two entries have DISTINCT seq numbers (1 and 2, not both 1)",
     setequal(mf$seq, c(1L, 2L)))
  ok("R1e: both payload files independently exist on disk with their own distinct content",
     identical(readRDS(file.path(.queue_entries_dir(cid), sprintf("%012d.rds", 1L)))$marker, "racer-1") &&
     identical(readRDS(file.path(.queue_entries_dir(cid), sprintf("%012d.rds", 2L)))$marker, "racer-2"))
  unlink(pf1); unlink(pf2)
}

{
  # R1f: regression guard -- the lock directory itself is always cleaned up
  # after a successful .queue_stage_entry() call, never left behind to
  # (wrongly) block the NEXT legitimate call for that same client.
  cid <- "race_client_r1f"
  pf <- .make_payload("f1")
  .queue_stage_entry(payload_path = pf, s3_key = paste0(cid, "/invoice_moves.rds"),
                      key = "invoice_moves.rds", client_id = cid)
  ok("R1f: no lock directory is left behind after a successful staging call",
     !dir.exists(.queue_lock_dir(cid)))
  unlink(pf)
}

# =============================================================================
# Part R2 -- payload_path missing at stage time (e.g. some other process
# already cleaned it up, or a future caller passes a bad path) fails closed,
# same as every other ineligibility check -- no crash, no partial manifest
# row, no empty entry file.
# =============================================================================
cat("\n-- Part R2: missing payload_path fails closed --\n")

{
  cid <- "missing_payload_r2"
  staged <- .queue_stage_entry(payload_path = file.path(tempdir(), "definitely-does-not-exist-12345.rds"),
                                s3_key = paste0(cid, "/invoice_moves.rds"),
                                key = "invoice_moves.rds", client_id = cid)
  ok("R2: staging with a nonexistent payload_path returns FALSE, not an error", identical(staged, FALSE))
  ok("R2: no manifest row was written for the failed staging attempt",
     nrow(.queue_read_manifest(cid)) == 0L)
}

# =============================================================================
# Part R3 -- duplicate s3_key registered under two different register_synced()
# names: .queue_table_name_for_key() resolves to whichever was registered
# FIRST (a for-loop over names(.SYNC_REGISTRY), first match wins). Confirms
# this doesn't crash or pick randomly -- it's deterministic -- but flags
# that the table_name label on a staged entry could be ambiguous in this
# (already-known-to-exist-elsewhere, per _run_saas.R's own "Scenario 4:
# duplicate s3_key values across two registry names does not error" test)
# case. Low severity: the s3_key itself (what actually matters for replay)
# is always correct regardless of which name wins.
# =============================================================================
cat("\n-- Part R3: duplicate s3_key across two registered names (deterministic, not a crash) --\n")

{
  register_synced("dup_name_a", "shared_key.rds", function(client_id = NULL) NULL)
  register_synced("dup_name_b", "shared_key.rds", function(client_id = NULL) NULL)
  resolved <- .queue_table_name_for_key("shared_key.rds")
  ok("R3: resolves to one of the two registered names, not NULL, not an error",
     identical(resolved, "dup_name_a") || identical(resolved, "dup_name_b"))
  ok("R3: resolution is deterministic across repeated calls (always the same winner)",
     identical(.queue_table_name_for_key("shared_key.rds"), resolved))
}

# =============================================================================
# Part R4 -- the real (non-test-overridden) default queue root actually
# resolves to a sane, creatable path on this machine. Verified by
# temporarily clearing the test override, calling the REAL .queue_root_dir(),
# confirming it's creatable, then restoring the test override immediately
# (this must never leave stray directories under the real user-data path).
# =============================================================================
cat("\n-- Part R4: real (non-overridden) .queue_root_dir() default is sane --\n")

{
  saved_opt <- getOption("hopdesk.queue_dir")
  options(hopdesk.queue_dir = NULL)
  real_default <- tryCatch(.queue_root_dir(), error = function(e) NA_character_)
  options(hopdesk.queue_dir = saved_opt)  # restore test isolation immediately

  ok("R4: the real default queue root is a non-empty character path",
     is.character(real_default) && length(real_default) == 1 && !is.na(real_default) && nzchar(real_default))
  ok("R4: the real default is NOT the Windows-only %LOCALAPPDATA%\\HopDesk path the file-lock uses (must work on shinyapps.io/Linux too)",
     !grepl("HopDesk", real_default, fixed = TRUE) || grepl("hopdesk", real_default, fixed = TRUE))
  # Don't actually create anything under the real path -- just prove
  # dir.create() against it doesn't error (dry-run via a throwaway
  # subdirectory we immediately remove).
  probe <- file.path(real_default, "___review_probe___")
  created <- tryCatch({ dir.create(probe, recursive = TRUE, showWarnings = FALSE); dir.exists(probe) },
                       error = function(e) FALSE)
  if (created) unlink(probe, recursive = TRUE, force = TRUE)
  ok("R4: the real default path is actually writable/creatable on this machine (probe dir created then removed)",
     isTRUE(created))
  ok("R4: the test override is restored after the probe (test isolation not leaked)",
     identical(getOption("hopdesk.queue_dir"), .TEST_QUEUE_ROOT))
}

# =============================================================================
# Part R5 -- .s3_write() integration: confirm the tempfile is STILL deleted
# (on.exit still fires) even in the branch where staging to the queue
# succeeds -- i.e. queuing doesn't accidentally keep .s3_write()'s own
# tempfile alive/leaked on top of the copy already made into the queue.
# =============================================================================
cat("\n-- Part R5: .s3_write()'s own tempfile is still cleaned up when staging succeeds --\n")

{
  .extract_fn("R/persistence.R", ".client_id")
  .extract_fn("R/persistence.R", ".s3_key")
  .extract_fn("R/persistence.R", ".s3_bucket")
  .extract_fn("R/persistence.R", ".s3_write")
  assign(".s3_missing_cache", new.env(parent = emptyenv()), envir = globalenv())
  assign(".s3_preload_cache", new.env(parent = emptyenv()), envir = globalenv())
  assign(".sync_versions", list(), envir = globalenv())
  Sys.setenv(CLIENT_ID = "r5client", S3_BUCKET = "mock-bucket")

  orig_save <- aws.s3::s3saveRDS
  orig_put  <- aws.s3::put_object
  utils::assignInNamespace("s3saveRDS", function(...) stop("simulated outage"), "aws.s3")
  utils::assignInNamespace("put_object", function(...) stop("simulated outage (fallback)"), "aws.s3")

  seen_tempfile <- NULL
  # Wrap tempfile() just for this block to capture the path .s3_write() used,
  # without changing its behavior at all (still delegates to the real one).
  orig_tempfile <- tempfile
  assign("tempfile", function(...) { p <- orig_tempfile(...); if (grepl("\\.rds$", p)) seen_tempfile <<- p; p }, envir = globalenv())

  tryCatch(
    tryCatch(.s3_write(list(x = 1), "invoice_moves.rds", client_id = "r5client"), error = function(e) NULL),
    finally = {
      utils::assignInNamespace("s3saveRDS", orig_save, "aws.s3")
      utils::assignInNamespace("put_object", orig_put, "aws.s3")
      assign("tempfile", orig_tempfile, envir = globalenv())
    }
  )

  ok("R5: captured the tempfile path .s3_write() actually used", !is.null(seen_tempfile))
  if (!is.null(seen_tempfile))
    ok("R5: .s3_write()'s own tempfile no longer exists after it returns, even though staging copied its content into the queue first",
       !file.exists(seen_tempfile))
  ok("R5: the payload WAS durably staged into the queue (a real copy survives, independent of the deleted tempfile)",
     nrow(.queue_read_manifest("r5client")) == 1L)
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
unlink(.TEST_QUEUE_ROOT, recursive = TRUE, force = TRUE)
if (.fail > 0) stop("Tests FAILED.")
invisible(NULL)
