# =============================================================================
# tests/_run_confirmed_logic.R
# Runner for the confirmed-invoice-logic unification test suite (see
# docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md and the staged implementation plan).
# Run from project root: Rscript tests/_run_confirmed_logic.R
# No live S3/AWS/SAP credentials needed — Stage A's tests are a static
# source scan plus a synthetic-data logic simulation, no I/O at all.
# =============================================================================

options(warn = 1)
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(uuid)
})

# Extract a handful of small, dependency-free function definitions straight
# out of R/global.R by name, without sourcing the whole file (which would
# run its guarded S3 preload block and require live AWS credentials). Keeps
# tests exercising the REAL implementation rather than a hand-copied stand-in
# that could silently drift from it.
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
.extract_fn("R/global.R", "%||%")
.extract_fn("R/global.R", "is_erp_sourced")
.extract_fn("R/persistence.R", ".schema_papelera")
.extract_fn("R/persistence.R", ".normalize")
.extract_fn("R/persistence.R", "add_to_papelera")
.extract_fn("R/persistence.R", "restore_from_papelera")
.extract_fn("R/bancos_persistence.R", ".schema_bancos_confirmados")
.extract_fn("R/bancos_persistence.R", "recover_confirmacion")
.extract_fn("R/data_pipeline.R", "stage_manual_row_to_agenda")

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
.run_module("tests/test_is_erp_sourced.R")
.run_module("tests/test_archive_mechanism.R")
.run_module("tests/test_stage4_confirm_undo.R")
.run_module("tests/test_stage5_provision_confirm_undo.R")
.run_module("tests/test_stage6_agenda_derivation.R")
.run_module("tests/test_provision_no_direct_agenda.R")
.run_module("tests/test_vencidos_confirm_bar_fixed_position.R")
.run_module("tests/test_stage_source_propagation.R")
.run_module("tests/test_manual_inv_sync_registration.R")
.run_module("tests/test_stage7_ghost_isolation.R")
.run_module("tests/test_stage8_vincular_warning.R")
.run_module("tests/test_stage9_compute_confirmed_flags.R")
.run_module("tests/test_stage10_amount_match_guard.R")
.run_module("tests/test_stage11_cashflow_export_confirmed.R")
.run_module("tests/test_stage12_reporte_pulse_confirmed.R")

cat("\n====================================================\n")
cat(sprintf("  TOTAL: %d passed, %d failed\n", .pass, .fail))
cat("====================================================\n\n")

if (.fail > 0L) stop(sprintf("%d test(s) failed", .fail))
invisible(.pass)
