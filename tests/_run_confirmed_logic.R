# =============================================================================
# tests/_run_confirmed_logic.R
# Runner for the confirmed-invoice-logic unification test suite (see
# docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md and the staged implementation plan).
# Run from project root: Rscript tests/_run_confirmed_logic.R
# No live S3/AWS/SAP credentials needed — Stage A's tests are a static
# source scan plus a synthetic-data logic simulation, no I/O at all.
# =============================================================================

options(warn = 1)
suppressPackageStartupMessages(library(dplyr))

.pass <- 0L
.fail <- 0L

.run_module <- function(file) {
  e <- new.env(parent = globalenv())
  e$.pass <- 0L
  e$.fail <- 0L
  tryCatch(source(file, local = e), error = function(err) {
    cat(sprintf("  ERROR loading %s: %s\n", basename(file), err$message))
    e$.fail <- e$.fail + 1L
  })
  .pass <<- .pass + e$.pass
  .fail <<- .fail + e$.fail
}

cat("\n====================================================\n")
cat("  Confirmed-Invoice-Logic Test Suite\n")
cat("====================================================\n\n")

.run_module("tests/test_confirmed_logic_stage_a.R")

cat("\n====================================================\n")
cat(sprintf("  TOTAL: %d passed, %d failed\n", .pass, .fail))
cat("====================================================\n\n")

if (.fail > 0L) stop(sprintf("%d test(s) failed", .fail))
invisible(.pass)
