# =============================================================================
# tests/test_stage10_amount_match_guard.R
# Stage 10 of the ledger-integrity master plan (source
# docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md §5/§7, Stage D): the bancos_confirmados
# matching in compute_confirmed_flags() (Source 1 only -- Source 2, papelera
# SAP ghosts, is deliberately NOT guarded, see below) now also requires the
# amount to match, using the exact 2-decimal-rounded-string shape already
# proven in production at R/interco_module.R's .ckey(). Protects against SAP
# reusing a DocNum years later for an unrelated future invoice. Deliberately
# NO date-window check -- Mouse's explicit reasoning: a date guard would
# treat a normal month-end SAP delay as staleness and reopen a still-valid
# confirmation.
#
# Matched against Saldo_original (the balance BEFORE this render's abono
# netting), not Importe (doesn't exist at all for SAP rows) and not the live
# Saldo vencido (which keeps shrinking as abonos get applied after
# confirmation -- matching that would cause exactly the false-negative Mouse
# is worried about).
#
# Covers the two edge cases the stage names explicitly: a USD invoice
# re-snapshotted with a benign float/FX display difference, and an abono
# applied around confirmation time.
# =============================================================================

cat("── Stage 10: bancos_confirmados matching requires the amount too ───────\n")

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

# ── 1. The actual bug this guard prevents: DocNum reuse with a different amount ─
{
  df <- tibble::tibble(source = "sap", Empresa = "ACME", Moneda = "MXN",
                       Documento = "F-100", Saldo_original = 500)
  # Same Empresa/Documento/Moneda key, but the confirmation on record is for
  # a DIFFERENT amount -- a genuinely different, later invoice reusing the
  # same DocNum, not the same invoice re-appearing.
  bc <- tibble::tibble(empresa = "ACME", documento = "F-100", moneda = "MXN",
                       tipo = "pago", eliminado = FALSE, importe = 999)
  out <- compute_confirmed_flags(df, "AP", bc, NULL)
  .chk(out$confirmed[1], FALSE,
       "a DocNum-reuse case (same key, different amount) is correctly NOT treated as confirmed -- the guard's actual purpose")
}

# ── 2. The matching, correct case still works ───────────────────────────────
{
  df <- tibble::tibble(source = "sap", Empresa = "ACME", Moneda = "MXN",
                       Documento = "F-101", Saldo_original = 1234.56)
  bc <- tibble::tibble(empresa = "ACME", documento = "F-101", moneda = "MXN",
                       tipo = "pago", eliminado = FALSE, importe = 1234.56)
  out <- compute_confirmed_flags(df, "AP", bc, NULL)
  .chk(out$confirmed[1], TRUE, "same key AND same amount still confirms, as before")
}

# ── 3. USD re-snapshot / floating-point noise tolerance ────────────────────
{
  # A benign float-representation difference (not a real amount difference)
  # must not false-negative the match -- 2-decimal rounding absorbs it,
  # exactly like Intercompany's .ckey() already does in production.
  df <- tibble::tibble(source = "sap", Empresa = "ACME", Moneda = "USD",
                       Documento = "F-102", Saldo_original = 20631.930000001)
  bc <- tibble::tibble(empresa = "ACME", documento = "F-102", moneda = "USD",
                       tipo = "pago", eliminado = FALSE, importe = 20631.93)
  out <- compute_confirmed_flags(df, "AP", bc, NULL)
  .chk(out$confirmed[1], TRUE,
       "a benign floating-point/display difference (20631.930000001 vs 20631.93) still matches after 2-decimal rounding")

  # But a REAL difference beyond rounding tolerance still correctly fails.
  df2 <- tibble::tibble(source = "sap", Empresa = "ACME", Moneda = "USD",
                        Documento = "F-102", Saldo_original = 20631.93)
  bc2 <- tibble::tibble(empresa = "ACME", documento = "F-102", moneda = "USD",
                        tipo = "pago", eliminado = FALSE, importe = 20641.93)
  out2 <- compute_confirmed_flags(df2, "AP", bc2, NULL)
  .chk(out2$confirmed[1], FALSE,
       "a genuine amount difference ($10 off, not float noise) still correctly fails to match")
}

# ── 4. Abono applied after confirmation must not reopen it ─────────────────
{
  # Saldo_original (100, captured before this render's abono-netting) still
  # matches the confirmation-time amount (100) even though the live
  # Saldo vencido has since shrunk to 60 after a $40 abono was applied.
  # Matching against the live balance instead would silently reopen this
  # still-valid confirmation -- exactly what Mouse flagged as the risk to
  # avoid.
  df <- tibble::tibble(source = "sap", Empresa = "ACME", Moneda = "MXN",
                       Documento = "F-103", `Saldo vencido` = 60,
                       Saldo_original = 100, abono_total = 40, has_abono = TRUE,
                       check.names = FALSE)
  bc <- tibble::tibble(empresa = "ACME", documento = "F-103", moneda = "MXN",
                       tipo = "pago", eliminado = FALSE, importe = 100)
  out <- compute_confirmed_flags(df, "AP", bc, NULL)
  .chk(out$confirmed[1], TRUE,
       "an abono applied after confirmation does not reopen it -- matched against Saldo_original (pre-abono), not the live post-abono Saldo vencido")
}

# ── 5. Source 2 (papelera ghosts) deliberately has NO amount guard ─────────
{
  # Stage 10 scopes the guard to bancos_confirmados matching only -- papelera
  # SAP-ghost matching is a discrete, immediate, single-invoice action (the
  # user deleting THIS specific row via the calendar trash), not the
  # long-lived DocNum-reuse risk the guard exists for.
  df <- tibble::tibble(source = "sap", Empresa = "ACME", Moneda = "MXN",
                       Documento = "F-104", Saldo_original = 500)
  pap <- tibble::tibble(ledger = "AP", source = "sap", Empresa = "ACME",
                        Moneda = "MXN", Documento = "F-104")
  out <- compute_confirmed_flags(df, "AP", NULL, pap)
  .chk(out$confirmed[1], TRUE,
       "papelera SAP-ghost matching (Source 2) still matches on key alone, no amount check -- deliberately out of this stage's scope")
}

# ── 6. A confirmation row missing its own amount never accidentally matches ─
{
  df <- tibble::tibble(source = "sap", Empresa = "ACME", Moneda = "MXN",
                       Documento = "F-105", Saldo_original = 500)
  bc <- tibble::tibble(empresa = "ACME", documento = "F-105", moneda = "MXN",
                       tipo = "pago", eliminado = FALSE, importe = NA_real_)
  out <- compute_confirmed_flags(df, "AP", bc, NULL)
  .chk(out$confirmed[1], FALSE,
       "a bancos_confirmados row with no recorded amount is excluded from matching entirely, never a silent wildcard match")
}

# ── 7. Static scan: the guard is wired exactly where the design calls for ──
{
  txt <- readLines("R/data_pipeline.R", warn = FALSE)
  start <- grep("Amount-match guard \\(Stage 10\\)", txt)
  .chk(length(start) > 0, TRUE, "found the Stage 10 amount-match guard comment to scan")
  if (length(start)) {
    block <- paste(sub("#.*$", "", txt[start[1]:min(start[1] + 40, length(txt))]), collapse = "\n")
    .chk(grepl('sprintf\\("%\\.2f", round\\(as\\.numeric', block), TRUE,
         "the guard uses the same 2-decimal-rounded-string shape as interco_module.R's .ckey()")
    .chk(grepl("!is\\.na\\(conf_db\\[\\[\"importe\"\\]\\]\\)", block), TRUE,
         "confirmation rows with no recorded amount are filtered out before building the match key")
    .chk(grepl("Saldo_original", block), TRUE,
         "the guard matches against Saldo_original, not a live-recomputed balance")
    .chk(grepl("date", block, ignore.case = TRUE), FALSE,
         "no date-window check was added -- Mouse's explicit reasoning against one is respected")
  }
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
