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

  # Each module ends with a single "PASS: %d   FAIL: %d" summary line — pull
  # both counts from it directly. (Previously this looked for a separate line
  # starting with "FAIL:", which never exists, so fail_n silently fell back to
  # a hardcoded 1 per broken module regardless of how many checks failed.)
  summary_line <- grep("^PASS:.*FAIL:", out, value = TRUE)
  if (length(summary_line)) {
    nums   <- regmatches(summary_line[1], gregexpr("\\d+", summary_line[1]))[[1]]
    pass_n <- as.integer(nums[1])
    fail_n <- as.integer(nums[2])
  } else {
    pass_n <- 0L
    fail_n <- if (!exit_ok) 1L else 0L
  }

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
