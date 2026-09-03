# =============================================================================
# scripts/diagnose_load_moves_readonly.R
#
# Read-only: calls the REAL load_moves() (R/persistence.R) exactly the way
# the running app does, to see whether the bug is in load_moves() itself
# (schema normalize/filter silently dropping every row) vs. somewhere
# downstream in the app's reactive wiring (moves_db() never actually getting
# populated in a live session, or df_with_moves()'s join). Raw S3 already
# confirmed 390 real rows exist in networks/invoice_moves.rds -- this checks
# what the app's own loader function turns that into.
#
# Never writes anything.
# =============================================================================

if (file.exists(".Renviron")) readRenviron(".Renviron")
suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(jsonlite)
})
source("R/persistence.R", local = FALSE)

# Minimal stand-ins for the two lists persistence.R's functions reference at
# CALL time (defined in R/global.R, which we deliberately do NOT fully
# source here -- it also runs the app's live SAP/startup bootstrap, which
# this read-only diagnostic has no business triggering). Values copied
# verbatim from R/global.R.
S3_KEYS_CRITICAL <- list(
  sap_snap_ar = "sap_snapshot_AR.rds",
  sap_snap_ap = "sap_snapshot_AP.rds",
  empresas    = "empresas.rds"
)
S3_KEYS <- list(
  moves = "invoice_moves.rds"
)

cat("== load_moves(client_id = 'networks') ==\n")
mv <- tryCatch(load_moves(client_id = "networks"), error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL })
if (is.null(mv)) {
  cat("load_moves() returned NULL / errored.\n")
} else {
  cat("rows:", nrow(mv), "\n")
  cat("classes:", paste(sapply(mv, class), collapse = ", "), "\n")
  if (nrow(mv)) {
    print(utils::head(mv[, intersect(c("ledger","Empresa","Moneda","Documento","FechaVenc_Proyectada","moved_by"), names(mv))], 10))
  }
}

cat("\n== load_sap_snapshot('AP', client_id = 'networks') -- current snapshot's key columns' actual R types ==\n")
snap <- tryCatch(load_sap_snapshot("AP", client_id = "networks"), error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL })
if (!is.null(snap) && !is.null(snap$data)) {
  d <- snap$data
  for (col in c("Empresa","Moneda","Documento")) {
    if (col %in% names(d)) cat(sprintf("  snapshot$%s class=%s sample=%s\n", col, class(d[[col]])[1], paste(utils::head(unique(d[[col]]), 3), collapse=" | ")))
  }
}

cat("\n== Raw S3 file's key columns' actual R types (bypassing load_moves' normalize) ==\n")
bucket <- Sys.getenv("S3_BUCKET")
raw_mv <- tryCatch(aws.s3::s3readRDS(object = "networks/invoice_moves.rds", bucket = bucket), error = function(e) NULL)
if (!is.null(raw_mv)) {
  cat("raw rows:", nrow(raw_mv), "\n")
  for (col in c("ledger","Empresa","Moneda","Documento","FechaVenc_Proyectada")) {
    if (col %in% names(raw_mv)) cat(sprintf("  raw$%s class=%s sample=%s\n", col, class(raw_mv[[col]])[1], paste(utils::head(unique(raw_mv[[col]]), 3), collapse=" | ")))
  }
}
