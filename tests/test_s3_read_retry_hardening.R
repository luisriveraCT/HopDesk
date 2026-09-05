# =============================================================================
# tests/test_s3_read_retry_hardening.R
#
# Found 2026-09-06, live report: a test provision converted to a real item
# was invisible everywhere (Agenda de hoy AND Calendario) right after a
# full app relaunch, until converting a THIRD, unrelated item forced a
# fresh S3 read of the same file and the "missing" item reappeared
# alongside it. Traced to .s3_read()/.s3_read_with() (R/persistence.R):
# both called aws.s3::s3readRDS() exactly once and treated ANY error --
# a real "object doesn't exist," but equally a transient timeout/blip
# right after a cold process boot -- identically: marked "confirmed
# missing" in .s3_missing_cache for a full 60 seconds, no retry, no
# self-heal except an unrelated later read of the same key happening to
# fire after the TTL expired. The WRITE side already got exactly this
# kind of bounded-retry hardening (save_pagar_hoy_with_retry(),
# s3_save_with_retry(), 2026-08-05/2026-09-03) -- the read side never did.
#
# Fix: a shared .s3_read_raw_with_retry() helper, used by both .s3_read()
# and .s3_read_with(), that only marks a key "confirmed missing" after
# every retry attempt has failed, not on the first error.
#
# This is confirmed, not just theorized: staging never removes a manual
# item (separately audited), and the client-jump alternative theory was
# explicitly ruled out by the user -- this read-retry gap is the
# best-supported explanation, and closing it is a real hardening
# regardless.
# =============================================================================
cat("=== test_s3_read_retry_hardening ===\n\n")

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

# =============================================================================
# Part A -- .s3_read_raw_with_retry(): the real, extracted function,
# aws.s3::s3readRDS() stubbed via the same safe assignInNamespace +
# guaranteed-finally-restore pattern already used elsewhere in this suite
# (tests/test_minor_hardening.R) -- never leaves a leaked mock behind even
# if an assertion inside the test body throws.
# =============================================================================
cat("\n-- Part A: .s3_read_raw_with_retry() functional behavior --\n")

.extract_fn("R/persistence.R", ".s3_read_raw_with_retry")

.orig_s3readRDS <- aws.s3::s3readRDS

.with_mock_s3read <- function(fake_read, expr) {
  orig <- aws.s3::s3readRDS
  utils::assignInNamespace("s3readRDS", fake_read, "aws.s3")
  tryCatch(expr, finally = utils::assignInNamespace("s3readRDS", orig, "aws.s3"))
}

# A1: succeeds first try -- no retry needed, value passed through untouched.
{
  call_n <- 0L
  fake <- function(object, bucket, ...) { call_n <<- call_n + 1L; list(x = 1, key = object) }
  res <- .with_mock_s3read(fake, .s3_read_raw_with_retry("networks/foo.rds", "mock-bucket"))
  ok("A1: succeeds on the first attempt when there's no error", isTRUE(res$ok))
  ok("A1: attempts == 1", identical(res$attempts, 1L))
  ok("A1: returns the real object s3readRDS produced", identical(res$value$key, "networks/foo.rds"))
  ok("A1: only calls s3readRDS once (no unnecessary retry on success)", call_n == 1L)
}

# A2: recovers after 2 transient failures, succeeds on the 3rd attempt --
# THIS is the exact shape of the reported incident (a cold-boot blip that
# clears within a second or two).
{
  call_n <- 0L
  fake <- function(object, bucket, ...) {
    call_n <<- call_n + 1L
    if (call_n < 3L) stop("simulated transient network error")
    list(recovered = TRUE)
  }
  res <- .with_mock_s3read(fake,
    .s3_read_raw_with_retry("networks/bar.rds", "mock-bucket", max_attempts = 3, retry_delay = 0.05))
  ok("A2: recovers after 2 transient failures (3rd attempt succeeds)", isTRUE(res$ok))
  ok("A2: reports the real number of attempts taken (3)", identical(res$attempts, 3L))
  ok("A2: error is NULL once it eventually succeeds", is.null(res$error))
  ok("A2: value is the object from the successful attempt", isTRUE(res$value$recovered))
}

# A3: total failure -- exhausts max_attempts, reports ok=FALSE with the
# real underlying error, never silently swallowed.
{
  call_n <- 0L
  fake <- function(object, bucket, ...) { call_n <<- call_n + 1L; stop("persistent network error") }
  res <- .with_mock_s3read(fake,
    .s3_read_raw_with_retry("networks/baz.rds", "mock-bucket", max_attempts = 3, retry_delay = 0.05))
  ok("A3: total failure after exhausting max_attempts is reported as ok=FALSE", identical(res$ok, FALSE))
  ok("A3: total failure still reports the attempt count actually taken (3)", res$attempts == 3)
  ok("A3: total failure carries the real underlying error, not a generic one",
     !is.null(res$error) && grepl("persistent network error", conditionMessage(res$error), fixed = TRUE))
  ok("A3: every attempt was actually made (3 real calls, not fewer)", call_n == 3L)
  ok("A3: value is NULL on total failure", is.null(res$value))
}

# A4: the mock is fully restored afterward -- regression guard against the
# documented 2026-08-04 mock-S3-leak incident class (a monkey-patched
# aws.s3 function bleeding into a live session with no restore).
ok("A4: aws.s3::s3readRDS is restored to the exact real function after every mocked block above (no leaked mock)",
   identical(aws.s3::s3readRDS, .orig_s3readRDS))

# =============================================================================
# Part B -- .s3_read()/.s3_read_with() only mark a key "confirmed missing"
# AFTER every retry attempt fails, not on the first transient error (the
# actual fix -- this is what makes the reported incident impossible going
# forward). Extracts the real functions plus their real dependencies
# (.s3_key, .s3_bucket, .s3_missing_cache*, .s3_preload_cache) so this is a
# true integration test of the fixed call chain, not a reimplementation.
# =============================================================================
cat("\n-- Part B: .s3_read()/.s3_read_with() missing-cache is retry-gated --\n")

.extract_fn("R/persistence.R", ".client_id")
.extract_fn("R/persistence.R", ".s3_key")
.extract_fn("R/persistence.R", ".s3_bucket")
.extract_fn("R/persistence.R", ".s3_missing_cache_hit")
.extract_fn("R/persistence.R", ".s3_missing_cache_set")
assign(".s3_missing_cache", new.env(parent = emptyenv()), envir = globalenv())
assign(".S3_MISSING_CACHE_TTL", 60, envir = globalenv())
assign(".s3_preload_cache", new.env(parent = emptyenv()), envir = globalenv())
Sys.setenv(CLIENT_ID = "networks", S3_BUCKET = "mock-bucket")
.extract_fn("R/persistence.R", ".s3_read_with")
.extract_fn("R/persistence.R", ".s3_read")

# B1: .s3_read_with() -- a transient failure that clears within the retry
# window never marks the key missing at all, and returns the real value.
{
  rm(list = ls(envir = .s3_missing_cache), envir = .s3_missing_cache)
  call_n <- 0L
  fake <- function(object, bucket, ...) {
    call_n <<- call_n + 1L
    if (call_n < 2L) stop("simulated transient blip")
    list(ok_data = TRUE)
  }
  out <- .with_mock_s3read(fake, .s3_read_with("provfile.rds", client_id = "networks"))
  ok("B1: .s3_read_with() recovers from one transient failure and returns real data",
     isTRUE(out$ok_data))
  ok("B1: the key is NOT marked missing after a transient failure that the retry absorbed",
     !.s3_missing_cache_hit("networks/provfile.rds"))
}

# B2: .s3_read_with() -- a PERSISTENT failure (every attempt fails) still
# marks the key missing afterward (old behavior preserved for a genuine
# absence, just now only after real retries, not a single blip).
{
  rm(list = ls(envir = .s3_missing_cache), envir = .s3_missing_cache)
  fake <- function(object, bucket, ...) stop("NoSuchKey: really doesn't exist")
  out <- .with_mock_s3read(fake, .s3_read_with("gonefile.rds", client_id = "networks"))
  ok("B2: .s3_read_with() returns NULL on a genuinely persistent failure", is.null(out))
  ok("B2: the key IS marked missing after every retry attempt genuinely fails",
     .s3_missing_cache_hit("networks/gonefile.rds"))
}

# B3: .s3_read() (bare-key variant) -- same retry-before-missing behavior.
{
  rm(list = ls(envir = .s3_missing_cache), envir = .s3_missing_cache)
  call_n <- 0L
  fake <- function(object, bucket, ...) {
    call_n <<- call_n + 1L
    if (call_n < 2L) stop("simulated transient blip")
    list(ok_data = "bare-key path")
  }
  out <- .with_mock_s3read(fake, .s3_read("barefile.rds"))
  ok("B3: .s3_read() (bare-key variant) also recovers from a transient failure",
     identical(out$ok_data, "bare-key path"))
  ok("B3: .s3_read()'s key is NOT marked missing after an absorbed transient failure",
     !.s3_missing_cache_hit("networks/barefile.rds"))
}

# B4: control -- a genuinely missing key (persistent failure) via .s3_read()
# still ends up correctly returning NULL and cached as missing, exactly
# like before this fix, just via the shared retry helper now.
{
  rm(list = ls(envir = .s3_missing_cache), envir = .s3_missing_cache)
  fake <- function(object, bucket, ...) stop("NoSuchKey")
  out <- .with_mock_s3read(fake, .s3_read("reallygone.rds"))
  ok("B4: .s3_read() control -- a real, persistent 'not found' still returns NULL", is.null(out))
  ok("B4: .s3_read() control -- and still gets marked missing (regression guard, old behavior preserved for a genuine absence)",
     .s3_missing_cache_hit("networks/reallygone.rds"))
}

# B5: preload-cache and missing-cache short-circuits are untouched --
# neither .s3_read() nor .s3_read_with() should call s3readRDS at all when
# either cache already has an answer (regression guard: the fix didn't
# accidentally make every read hit the network).
{
  rm(list = ls(envir = .s3_preload_cache), envir = .s3_preload_cache)
  assign("networks/cached.rds", list(from = "preload"), envir = .s3_preload_cache)
  call_n <- 0L
  fake <- function(object, bucket, ...) { call_n <<- call_n + 1L; stop("should never be called") }
  out <- .with_mock_s3read(fake, .s3_read_with("cached.rds", client_id = "networks"))
  ok("B5: a preload-cache hit is served directly, never calling s3readRDS at all",
     identical(out$from, "preload") && call_n == 0L)
  rm(list = ls(envir = .s3_preload_cache), envir = .s3_preload_cache)

  rm(list = ls(envir = .s3_missing_cache), envir = .s3_missing_cache)
  .s3_missing_cache_set("networks/knownmissing.rds")
  call_n2 <- 0L
  fake2 <- function(object, bucket, ...) { call_n2 <<- call_n2 + 1L; stop("should never be called") }
  out2 <- .with_mock_s3read(fake2, .s3_read_with("knownmissing.rds", client_id = "networks"))
  ok("B5: a live (non-expired) missing-cache hit is served directly, never calling s3readRDS at all",
     is.null(out2) && call_n2 == 0L)
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0) stop("Tests FAILED.")
invisible(NULL)
