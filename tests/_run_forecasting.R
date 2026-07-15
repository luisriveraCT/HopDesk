# =============================================================================
# tests/_run_forecasting.R
# Runner for all Stage 6.1 Forecasting tests.
# Run from project root:  source("tests/_run_forecasting.R")
#
# Each test file is run as a subprocess to avoid cross-contamination from
# independent source() calls inside each file.
# =============================================================================

rscript_bin <- file.path(R.home("bin"), "Rscript")

.modules <- c(
  "tests/test_forecasting_schemas.R",
  "tests/test_forecasting_methods.R",
  "tests/test_forecasting_resolve.R",
  "tests/test_forecasting_sources.R"
)

.total_pass <- 0L
.total_fail <- 0L

cat("\n====================================================\n")
cat("  Forecasting Test Suite (Stage 6.1)\n")
cat("====================================================\n\n")

for (mod in .modules) {
  cat(sprintf("── %s\n", basename(mod)))
  out <- tryCatch(
    system2(rscript_bin, args = mod, stdout = TRUE, stderr = TRUE),
    error = function(e) { structure(character(0), status = 1L) }
  )
  exit_ok <- is.null(attr(out, "status")) || attr(out, "status") == 0L

  pass_line <- grep("^PASS:", out, value = TRUE)
  fail_line <- grep("^FAIL:", out, value = TRUE)
  pass_n <- if (length(pass_line)) {
    as.integer(regmatches(pass_line[1], regexpr("\\d+", pass_line[1])))
  } else 0L
  fail_n <- if (length(fail_line)) {
    as.integer(regmatches(fail_line[1], regexpr("\\d+", fail_line[1])))
  } else if (!exit_ok) 1L else 0L

  .total_pass <- .total_pass + pass_n
  .total_fail <- .total_fail + fail_n

  # Print each test line
  test_lines <- grep("^\\[PASS\\]|^\\[FAIL\\]|^\\[SKIP\\]", out, value = TRUE)
  for (ln in test_lines) cat("  ", ln, "\n")

  if (!exit_ok && !fail_n) {
    errs <- grep("Error|error", out, value = TRUE)
    if (length(errs)) cat(sprintf("  ERROR: %s\n", errs[1]))
  }
  cat("\n")
}

cat("====================================================\n")
cat(sprintf("  TOTAL: %d passed, %d failed\n", .total_pass, .total_fail))
cat("====================================================\n\n")

if (.total_fail > 0L) stop(sprintf("%d test(s) failed", .total_fail))
invisible(.total_pass)
