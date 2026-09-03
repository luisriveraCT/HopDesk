# =============================================================================
# tests/test_hopdesk_dev_bucket_override.R
#
# CRITICAL fix (2026-09-02): this repo has one single .Renviron pointed at the
# live PRODUCTION S3 bucket -- there was no way to run or test the app
# locally without touching real client data, which is also the underlying
# condition that makes the single-instance-lock fix (see
# test_hopdesk_single_instance_lock.R) necessary in the first place: a local
# test run and the real production instance (shinyapps.io, or
# hopdesk_start.bat) are, today, always the SAME S3 bucket.
#
# Fix: an optional, gitignored/rscignored .Renviron.local can override
# S3_BUCKET/AWS_*/CLIENT_ID for local work. This is extracted from R/global.R
# and evaluated against a throwaway temp working directory with controlled
# fake .Renviron/.Renviron.local files -- never the real ones -- so this test
# is safe to run repeatedly and never touches real credentials or S3.
#
# The property that actually matters here is the FAIL-SAFE DIRECTION: local
# work must default to the override the moment .Renviron.local exists, with
# NO separate opt-in step to remember -- an opt-IN dev flag would silently
# fall back to touching production the instant someone forgot to set it. Only
# hopdesk_start.bat (verified below to actually do this) may assert
# HOPDESK_PRODUCTION=1 to suppress the override.
# =============================================================================

cat("── Dev/staging S3 override (critical, 2026-09-02) ─────────────────────────\n")

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
.chk_true <- function(cond, label) .chk(isTRUE(cond), TRUE, label)

# This test mutates process-global env vars (S3_BUCKET, HOPDESK_PRODUCTION)
# and creates temp directories -- _run_saas.R runs many test files in one R
# process via source(file, local = e) (see .run_module() in _run_saas.R), and
# top-level on.exit() in a sourced-not-called script does NOT reliably defer
# to end-of-script under that harness (confirmed empirically: it fired
# immediately, deleting a just-created temp dir before the next line could
# use it). Everything stateful is wrapped in tryCatch(..., finally = ...)
# instead, which has no such ambiguity.
.orig_s3_bucket <- Sys.getenv("S3_BUCKET", unset = NA)
.orig_hopdesk_production <- Sys.getenv("HOPDESK_PRODUCTION", unset = NA)

global_src <- readLines("R/global.R", warn = FALSE)
start_line <- grep('^if \\(file\\.exists\\("\\.Renviron"\\)\\) readRenviron', global_src)
end_line   <- grep('^IS_ADMIN_DEPLOYMENT <-', global_src)
.chk_true(length(start_line) == 1 && length(end_line) == 1 && start_line < end_line,
          "located exactly one .Renviron/.Renviron.local override block in R/global.R")
override_txt <- paste(global_src[start_line:(end_line - 1)], collapse = "\n")

run_case <- function(fake_dir, hopdesk_production) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(fake_dir)
  if (is.null(hopdesk_production)) Sys.unsetenv("HOPDESK_PRODUCTION")
  else Sys.setenv(HOPDESK_PRODUCTION = hopdesk_production)
  Sys.unsetenv("S3_BUCKET")
  msgs <- capture.output(eval(parse(text = override_txt), envir = environment()), type = "message")
  list(bucket = Sys.getenv("S3_BUCKET"), messages = msgs)
}

test_dir <- tempfile("hopdesk_env_test_")
dir.create(test_dir)
no_override_dir <- tempfile("hopdesk_env_test_none_")
dir.create(no_override_dir)

tryCatch({
  writeLines("S3_BUCKET=fake-prod-bucket", file.path(test_dir, ".Renviron"))
  writeLines("S3_BUCKET=fake-dev-bucket",  file.path(test_dir, ".Renviron.local"))

  cat("\n-- A local run (no HOPDESK_PRODUCTION set) picks up the dev override automatically --\n")
  r1 <- run_case(test_dir, NULL)
  .chk("fake-dev-bucket", r1$bucket,
       "with .Renviron.local present and HOPDESK_PRODUCTION unset, S3_BUCKET is the DEV bucket")
  .chk_true(any(grepl("Local dev override active", r1$messages)),
            "a visible [ENV] message announces the dev override is active")

  cat("\n-- hopdesk_start.bat's HOPDESK_PRODUCTION=1 suppresses the override --\n")
  r2 <- run_case(test_dir, "1")
  .chk("fake-prod-bucket", r2$bucket,
       "with HOPDESK_PRODUCTION=1, S3_BUCKET stays the PRODUCTION bucket even though .Renviron.local exists")
  .chk_true(!any(grepl("Local dev override active", r2$messages)),
            "no dev-override message is printed when production is asserted")
  .chk_true(any(grepl("PRODUCTION MODE ACTIVE", r2$messages)),
            "a visible [ENV] message announces production mode is active")

  cat("\n-- No .Renviron.local at all: behaves exactly as before this fix, in both modes --\n")
  writeLines("S3_BUCKET=fake-prod-bucket", file.path(no_override_dir, ".Renviron"))
  r3 <- run_case(no_override_dir, NULL)
  .chk("fake-prod-bucket", r3$bucket,
       "with no .Renviron.local file, S3_BUCKET is production regardless of HOPDESK_PRODUCTION")
}, finally = {
  unlink(test_dir, recursive = TRUE)
  unlink(no_override_dir, recursive = TRUE)
  if (is.na(.orig_s3_bucket)) Sys.unsetenv("S3_BUCKET") else Sys.setenv(S3_BUCKET = .orig_s3_bucket)
  if (is.na(.orig_hopdesk_production)) Sys.unsetenv("HOPDESK_PRODUCTION") else Sys.setenv(HOPDESK_PRODUCTION = .orig_hopdesk_production)
})

cat("\n-- hopdesk_start.bat actually sets HOPDESK_PRODUCTION=1 before launching Rscript --\n")
bat_path <- "C:/Users/luisr/hopdesk_start.bat"
if (file.exists(bat_path)) {
  bat_txt <- paste(readLines(bat_path, warn = FALSE), collapse = "\n")
  set_line   <- grepl("set HOPDESK_PRODUCTION=1", bat_txt, fixed = TRUE)
  before_start <- regexpr("set HOPDESK_PRODUCTION=1", bat_txt, fixed = TRUE)
  start_pos    <- regexpr('start "" /B "C:\\Program Files\\R', bat_txt, fixed = TRUE)
  .chk_true(set_line, "hopdesk_start.bat sets HOPDESK_PRODUCTION=1 somewhere in the script")
  .chk_true(before_start > 0 && start_pos > 0 && before_start < start_pos,
            "HOPDESK_PRODUCTION=1 is set BEFORE the Rscript launch line, so the child process inherits it")
} else {
  cat("  SKIP  hopdesk_start.bat not found at expected path (not this repo's concern, but worth noting)\n")
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
