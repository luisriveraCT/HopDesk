# =============================================================================
# tests/test_stage12_reporte_pulse_confirmed.R
# Stage 12 of the ledger-integrity master plan (source
# docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md §5/§7, Stage F): Reporte's Cash Flow
# Pulse (compute_pulse(), R/reporte_module.R) previously applied ZERO
# confirmation exclusion, counting invoices Treasury has already closed out
# in its inflow/outflow projections -- the same class of bug fixed for Cash
# Flow Preview/Export in Stage 11.
#
# Design choice (flagged per the stage's own instruction to reason about
# this module's divergence rather than deciding silently): compute_pulse()'s
# normalize_sap() builds its own deliberately SAP-only frame (ar_all/ap_all
# -- no "source" column at all, since this module never includes manual
# entries or provisions), bypassing build_ledger_df()/df_combined() entirely.
# Routing it through the full build_ledger_df() pipeline would be a much
# bigger, riskier change just to reach the canonical function. Instead,
# compute_confirmed_flags() is called directly against ar_all/ap_all as they
# already are -- its manual/provision-specific masks are simply no-ops on
# data that has no "source" column, which is correct since every row here
# IS a SAP row by construction. Matched against "Saldo vencido" directly
# (compute_confirmed_flags()'s own fallback when Saldo_original is absent)
# since this module has no abono-netting step of its own.
#
# compute_pulse() itself is a reactive() defined inside a moduleServer
# closure, not directly unit-testable -- static scan for the wiring, plus a
# behavioral test replicating the exact drop-confirmed pattern against a
# synthetic ar_all/ap_all-shaped frame and the real compute_confirmed_flags().
# =============================================================================

cat("── Stage 12: Reporte's Cash Flow Pulse excludes confirmed invoices ─────\n")

suppressPackageStartupMessages(library(tibble))

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
.extract_fn("R/data_pipeline.R", "compute_confirmed_flags")
.strip_comments <- function(lines) sub("#.*$", "", lines)

# ── 1. Static scan: compute_pulse() wired to the canonical function ────────
{
  txt <- readLines("R/reporte_module.R", warn = FALSE)
  start <- grep("Exclude already-confirmed invoices \\(Stage 12\\)", txt)
  .chk(length(start) > 0, TRUE, "found the Stage 12 fix's comment to scan")
  if (length(start)) {
    block <- paste(.strip_comments(txt[start[1]:min(start[1] + 20, length(txt))]), collapse = "\n")
    .chk(grepl("compute_confirmed_flags\\(", block), TRUE,
         "compute_pulse() calls the canonical compute_confirmed_flags(), not a reimplementation")
    .chk(grepl('\\[\\["confirmed"\\]\\]', block), TRUE,
         "drops confirmed rows entirely after computing the flags")
    .chk(grepl("ar_all <- \\.drop_confirmed_pulse\\(ar_all", block) &&
         grepl("ap_all <- \\.drop_confirmed_pulse\\(ap_all", block), TRUE,
         "both ar_all and ap_all are filtered, not just one ledger")
  }
}

# ── 2. Behavioral: the exact drop-confirmed pattern against real logic ────
{
  .drop_confirmed_pulse <- function(df, ledger_type, conf_db, pap_db) {
    if (!nrow(df)) return(df)
    df <- compute_confirmed_flags(df, ledger_type, conf_db, pap_db)
    if ("confirmed" %in% names(df)) df <- df[is.na(df[["confirmed"]]) | !df[["confirmed"]], , drop = FALSE]
    df
  }

  # ar_all/ap_all-shaped synthetic frame, exactly matching normalize_sap()'s
  # real output columns -- deliberately no "source" column at all.
  ap_all <- tibble::tibble(
    FechaEff  = as.Date(c("2026-08-01", "2026-08-02")),
    Parte     = c("Vendor A", "Vendor B"),
    Moneda    = c("MXN", "MXN"),
    `Saldo vencido` = c(500, 300),
    Empresa   = c("ACME", "ACME"),
    Documento = c("F-CONF", "F-OPEN"),
    check.names = FALSE
  )
  conf_db <- tibble::tibble(
    empresa = "ACME", documento = "F-CONF", moneda = "MXN",
    tipo = "pago", eliminado = FALSE, importe = 500
  )

  out <- .drop_confirmed_pulse(ap_all, "AP", conf_db, NULL)
  .chk(nrow(out), 1L, "exactly 1 row survives out of 2 (the confirmed invoice is excluded from the Pulse projection)")
  .chk(out$Documento[1], "F-OPEN", "the surviving row is the open invoice")

  # An empty ledger (no data at all) must not error -- normalize_sap()
  # returns a 0-row tibble with these columns when SAP has nothing for a
  # ledger, and .drop_confirmed_pulse()'s own nrow guard should short-circuit.
  empty <- ap_all[0, ]
  out_empty <- .drop_confirmed_pulse(empty, "AP", conf_db, NULL)
  .chk(nrow(out_empty), 0L, "an empty ledger frame passes through unchanged, no error")

  # No confirmations at all -- both rows survive untouched.
  out_noconf <- .drop_confirmed_pulse(ap_all, "AP", NULL, NULL)
  .chk(nrow(out_noconf), 2L, "with no bancos_confirmados data at all, nothing is excluded")
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
