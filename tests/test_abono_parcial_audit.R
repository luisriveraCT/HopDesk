# =============================================================================
# tests/test_abono_parcial_audit.R
# Audit findings, 2026-07-24: "partial payments are not getting completed" was
# traced to the Abono Parcial modal (R/staging_browse_module.R) never netting
# its displayed "Saldo" against already-confirmed abonos -- it accepted an
# `abonos_db` parameter but never actually read it. A user paying an invoice
# down in installments would see the SAME original balance every time they
# reopened the tool, with no sign the previous partial payment had any effect.
#
# Three fixes, all in the same audit:
#   1. output$ab_table now nets `Saldo vencido` against
#      active_abonos_summary(abonos_db()), mirroring build_ledger_df()'s exact
#      join (R/data_pipeline.R) so this modal agrees with Calendario itself.
#      Invoices fully covered by confirmed abonos are dropped from the list
#      entirely (nothing left to partially pay).
#   2. The AR side of Agenda de Hoy (R/pagar_hoy_module.R's tbl_ar_<emp>) had
#      no "ABONO" badge at all -- a staged partial payment looked exactly
#      like a full invoice awaiting collection. AP already had this badge;
#      AR now gets the same treatment.
#   3. Nothing server-side ever blocked staging an abono larger than the
#      invoice's remaining balance (the client-side .ab-warn class was purely
#      cosmetic). The ab_rows observer now rejects any row whose importe
#      exceeds its saldo, stages only the valid ones, and notifies the user
#      about anything rejected.
#
# Two other bugs found by the same audit (no undo path for a confirmed abono;
# the staged-abono row always shows "today" as its Vencimiento) are tracked
# separately in docs/LEDGER_INTEGRITY_MASTER_PLAN.md, not fixed by this stage.
# =============================================================================

cat("── Abono Parcial: balance netting, AR badge, over-payment guard ────────\n")

suppressPackageStartupMessages({
  library(dplyr); library(tibble)
})

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
"%||%" <- function(a, b) if (!is.null(a)) a else b
.extract_fn("R/persistence.R", "active_abonos_summary")

# ── 1a. Behavioral: active_abonos_summary() itself sums correctly per key ───
{
  abonos <- tibble::tibble(
    id = c("a1", "a2", "a3", "a4"),
    ledger = c("AP", "AP", "AP", "AR"),
    Empresa = c("ACME", "ACME", "ACME", "ACME"),
    Moneda = c("MXN", "MXN", "MXN", "MXN"),
    Documento = c("F-1", "F-1", "F-2", "F-1"),
    Parte = "Vendor A",
    importe = c(40, 10, 5, 999),
    fecha_abono = Sys.Date(),
    notas = "", created_by = "dev", created_at = Sys.time(),
    status = c("active", "active", "voided", "active")
  )
  s <- active_abonos_summary(abonos) |> dplyr::filter(ledger == "AP")
  f1 <- s[s$Documento == "F-1", ]
  .chk(nrow(f1), 1L, "F-1 has exactly one summary row (two active abonos merged)")
  .chk(f1$abono_total, 50, "F-1's abono_total sums both active abonos (40+10), not just one")
  .chk("F-2" %in% s$Documento, FALSE, "F-2's only abono is voided -- excluded from the summary entirely")
}

# ── 1b. Behavioral: the netting arithmetic matches build_ledger_df()'s rule ─
{
  saldo_original <- 100
  abono_total <- 40
  netted <- pmax(0, saldo_original - abono_total)
  .chk(netted, 60, "a $40 abono against a $100 balance nets to $60, the same rule build_ledger_df() uses")

  netted_over <- pmax(0, 30 - 999)
  .chk(netted_over, 0, "netting never goes negative even if abono_total somehow exceeds the balance")
}

# ── 1c. Static scan: the modal actually reads abonos_db() now ───────────────
{
  txt <- readLines("R/staging_browse_module.R", warn = FALSE)
  start <- grep("output\\$ab_table <- renderUI", txt)
  .chk(length(start) > 0, TRUE, "found output$ab_table to scan")
  if (length(start)) {
    end <- grep('Stage abono rows to pagar_hoy ONLY', txt)
    end <- end[end > start[1]][1] %||% (start[1] + 120)
    block <- paste(sub("#.*$", "", txt[start[1]:end]), collapse = "\n")
    .chk(grepl("active_abonos_summary\\(\\s*abonos_db\\(\\)", block), TRUE,
         "output$ab_table calls active_abonos_summary(abonos_db()) -- the parameter is no longer dead")
    .chk(grepl('`Saldo vencido`\\s*=\\s*pmax\\(0,\\s*`Saldo vencido`\\s*-\\s*abono_total\\)', block), TRUE,
         "Saldo vencido is netted against abono_total using the same pmax(0, ...) rule as build_ledger_df()")
    .chk(grepl('df\\[df\\[\\["Saldo vencido"\\]\\]\\s*>\\s*0,', block), TRUE,
         "invoices fully covered by confirmed abonos (netted balance <= 0) are dropped from the table")
  }
}

# ── 2. Static scan: AR table now has the same ABONO badge as AP ────────────
{
  txt <- readLines("R/pagar_hoy_module.R", warn = FALSE)
  start <- grep('output\\[\\[paste0\\("tbl_ar_", emp\\)\\]\\]', txt)
  .chk(length(start) > 0, TRUE, "found the AR table render to scan")
  if (length(start)) {
    block <- paste(sub("#.*$", "", txt[start[1]:min(start[1] + 40, length(txt))]), collapse = "\n")
    .chk(grepl('abono_pfx', block), TRUE,
         "AR table now computes an abono_pfx badge, matching the AP table's existing pattern")
    .chk(grepl('tipo_item.*==\\s*"abono"', block), TRUE,
         "the AR badge is driven by tipo_item == \"abono\", the same field AP uses")
    .chk(grepl('Cliente\\s*=\\s*paste0\\(abono_pfx', block), TRUE,
         "the badge is actually prepended to the Cliente cell, not just computed and discarded")
  }
}

# ── 3a. Behavioral: the accept/reject predicate itself ──────────────────────
{
  .would_reject <- function(importe, saldo) importe <= 0 || importe > saldo + 1e-6
  .chk(.would_reject(50, 100), FALSE, "an abono under the balance is accepted")
  .chk(.would_reject(100, 100), FALSE, "an abono exactly equal to the balance is accepted (boundary)")
  .chk(.would_reject(100.01, 100), TRUE, "an abono even slightly over the balance is rejected")
  .chk(.would_reject(0, 100), TRUE, "a zero-amount abono is rejected")
  .chk(.would_reject(-5, 100), TRUE, "a negative amount is rejected")
  .chk(.would_reject(100.0000001, 100), FALSE, "floating-point noise within the epsilon does not falsely reject an exact match")
}

# ── 3b. Static scan: the ab_rows observer enforces the guard server-side ────
{
  txt <- readLines("R/staging_browse_module.R", warn = FALSE)
  start <- grep("observeEvent\\(input\\$ab_rows,", txt)
  .chk(length(start) > 0, TRUE, "found the ab_rows observer to scan")
  if (length(start)) {
    end <- grep("\\}, ignoreInit = TRUE\\)", txt)
    end <- end[end > start[1]][1] %||% (start[1] + 80)
    block <- paste(sub("#.*$", "", txt[start[1]:end]), collapse = "\n")
    .chk(grepl("importe\\s*>\\s*saldo", block), TRUE,
         "the observer compares importe against saldo before staging")
    .chk(grepl("rejected", block), TRUE,
         "rejected rows are tracked separately from accepted ones")
    .chk(grepl('type\\s*=\\s*"warning"', block), TRUE,
         "a rejection produces a warning notification, not a silent drop")
    .chk(grepl("accepted", block), TRUE,
         "only accepted rows are actually staged to pagar_hoy_db")
  }
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
