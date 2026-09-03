# =============================================================================
# tests/test_hopdesk_single_instance_lock.R
#
# CRITICAL fix (2026-09-02): clients reported activity "undone", as if it
# never happened. Root cause traced to two live app processes silently
# sharing the same production S3 bucket -- every save_*() in this app does
# "read the whole table -> mutate -> overwrite the whole table back to S3",
# with no locking or versioning, so two writers can clobber each other's
# saves with zero exception raised anywhere. This is made routine, not
# theoretical, by two confirmed facts: (1) this repo's single shared
# .Renviron means local testing and production hit the IDENTICAL S3 bucket,
# and (2) hopdesk_start.bat's `start "" /B "Rscript.exe" ...` relaunches
# through an internal cmd.exe wrapper into the real worker 2-3 layers deep --
# closing that script's console window does NOT stop the real worker
# underneath it, which survives as an invisible orphan indefinitely.
#
# Fix: .hopdesk_acquire_instance_lock() (R/global.R) takes a real OS-level
# advisory file lock (the `filelock` package) before the app does anything
# else. This test exercises the REAL function, extracted from the real
# source file (never hand-reimplemented -- see
# test_moves_save_failure_visibility.R for why this codebase insists on
# that), against a throwaway temp directory instead of the real
# %LOCALAPPDATA%\HopDesk, so it never touches anything real and is safe to
# run repeatedly. filelock is a real, cheap, side-effect-free-when-scoped
# package -- no mocking needed to test this for real.
# =============================================================================

cat("── Single-instance lock (critical, 2026-09-02) ─────────────────────────\n")

.pass <- 0L
.fail <- 0L
.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}
.chk_true  <- function(cond, label) .chk(isTRUE(cond), TRUE, label)

if (!requireNamespace("filelock", quietly = TRUE)) {
  cat("  SKIP  filelock package not installed -- cannot test the real lock mechanism.\n")
  cat("\n  Subtotal: 0 passed, 0 failed (skipped)\n")
} else {

global_src <- readLines("R/global.R", warn = FALSE)
fn_start   <- grep("^\\.hopdesk_acquire_instance_lock <- function", global_src)
fn_end     <- grep("^\\.hopdesk_lock <- \\.hopdesk_acquire_instance_lock\\(\\)", global_src)
.chk_true(length(fn_start) == 1 && length(fn_end) == 1 && fn_start < fn_end,
          "located exactly one .hopdesk_acquire_instance_lock() block in R/global.R")
fn_txt <- paste(global_src[fn_start:(fn_end - 1)], collapse = "\n")
eval(parse(text = fn_txt), envir = environment())
.chk_true(exists(".hopdesk_acquire_instance_lock", mode = "function"),
          ".hopdesk_acquire_instance_lock() extracted and evaluated successfully from the real source file")

test_dir <- tempfile("hopdesk_lock_test_")
dir.create(test_dir)
on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

# =============================================================================
# Part A -- a fresh acquire succeeds and records the real info
# =============================================================================
cat("\n-- Part A: fresh acquire succeeds, records pid/host/client/bucket --\n")

lock1 <- .hopdesk_acquire_instance_lock(lock_dir_base = test_dir,
                                        client_id = "test-client", s3_bucket = "test-bucket")
.chk_true(!is.null(lock1), "first acquire against an empty temp dir succeeds")

info_path <- file.path(test_dir, "HopDesk", "instance_test-client_test-bucket.lock.info")
.chk_true(file.exists(info_path), "the .info file was written")
info_txt <- readLines(info_path, warn = FALSE)
.chk_true(any(grepl(paste0("^pid: ", Sys.getpid(), "$"), info_txt)),
          "the .info file records this process's own real pid")
.chk_true(any(grepl("^client_id: test-client$", info_txt)), "the .info file records the client_id")
.chk_true(any(grepl("^s3_bucket: test-bucket$", info_txt)), "the .info file records the s3_bucket")

# =============================================================================
# Part B -- a second acquire attempt, same process, same key: must NOT hang
# or deadlock (this is exactly the "press Run App again in a kept-open R
# session" workflow app.R's own CRITICAL FIX note documents as required to
# keep working -- this is the one regression this fix could plausibly cause).
# =============================================================================
cat("\n-- Part B: re-acquiring the SAME key from the SAME process (repeated 'Run App') does not hang --\n")

lock2 <- .hopdesk_acquire_instance_lock(lock_dir_base = test_dir,
                                        client_id = "test-client", s3_bucket = "test-bucket")
.chk_true(!is.null(lock2), "re-acquiring the same lock from the same process succeeds (no self-deadlock)")

# =============================================================================
# Part C -- a DIFFERENT key (different client/bucket) is completely
# independent -- a future dev-bucket instance must never block a prod one.
# =============================================================================
cat("\n-- Part C: a different client_id/s3_bucket key is a completely independent lock --\n")

lock3 <- .hopdesk_acquire_instance_lock(lock_dir_base = test_dir,
                                        client_id = "other-client", s3_bucket = "other-bucket")
.chk_true(!is.null(lock3), "a different client_id/s3_bucket pair acquires independently, unaffected by lock1/lock2")
tryCatch(filelock::unlock(lock3), error = function(e) NULL)

# =============================================================================
# Part D -- a genuinely SEPARATE process holding the same key blocks a new
# acquire attempt, with a clear, actionable error naming the prior instance.
# This is the actual production bug this fix closes: two live processes on
# the same S3 bucket. Uses a real second Rscript process, not a mock, since
# a same-process re-acquire (Part B) and a cross-process conflict (this part)
# are governed by different OS semantics and must both be verified for real.
# =============================================================================
cat("\n-- Part D: a genuinely separate process holding the lock blocks a new acquire, with a clear error --\n")

# Uses callr::r_bg() -- the same real-separate-process mechanism this repo's
# own tests/helpers/live_app_harness.R already relies on (start_bg_app()) --
# rather than a thread or a mocked "process", since the property under test
# (a genuinely different OS process contending for the same file lock) only
# exists across real process boundaries.
holder_lock_path <- file.path(test_dir, "HopDesk", "instance_conflict-client_conflict-bucket.lock")
dir.create(dirname(holder_lock_path), recursive = TRUE, showWarnings = FALSE)
holder_ready_file <- tempfile()

holder <- callr::r_bg(
  function(lock_path, ready_file) {
    l <- filelock::lock(lock_path, timeout = 0)
    writeLines("pid: 0", paste0(lock_path, ".info"))  # placeholder, mirrors the real function's own info file
    writeLines("ready", ready_file)
    Sys.sleep(8)
  },
  args = list(lock_path = holder_lock_path, ready_file = holder_ready_file),
  stdout = tempfile(), stderr = tempfile()
)
on.exit(tryCatch({ if (holder$is_alive()) holder$kill() }, error = function(e) NULL), add = TRUE)

ready <- FALSE
for (i in 1:40) {
  Sys.sleep(0.25)
  if (file.exists(holder_ready_file)) { ready <- TRUE; break }
}

if (!ready) {
  cat("  SKIP  Part D: could not confirm the separate holder process started in time (environment-dependent, not a code defect)\n")
} else {
  result <- tryCatch({
    .hopdesk_acquire_instance_lock(lock_dir_base = test_dir,
                                   client_id = "conflict-client", s3_bucket = "conflict-bucket")
    "ACQUIRED"
  }, error = function(e) conditionMessage(e))

  .chk_true(!identical(result, "ACQUIRED"),
            "acquire attempt while a genuinely separate process holds the lock is REJECTED, not silently allowed")
  .chk_true(grepl("HOPDESK REFUSED TO START", result %||% ""),
            "rejection error clearly states HopDesk refused to start")
  .chk_true(grepl("conflict-bucket", result %||% "") && grepl("conflict-client", result %||% ""),
            "rejection error names the specific bucket/client in conflict")
  .chk_true(grepl("hopdesk_stop.bat", result %||% ""),
            "rejection error tells the user how to resolve it (hopdesk_stop.bat)")
}

}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
