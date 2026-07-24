# =============================================================================
# tests/test_stage11_cashflow_export_confirmed.R
# Stage 11 of the ledger-integrity master plan (source
# docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md §5/§7, Stage E): Cash Flow
# Preview/Export applied ZERO confirmation exclusion at all, overstating
# projections with invoices Treasury has already closed out (audit §4.2).
# build_export_combined_df() (R/cashflow_export_module.R) now calls
# compute_confirmed_flags() (the same canonical function the calendar uses)
# on each ledger's data and drops confirmed rows entirely -- including SAP
# ghosts, unlike the calendar's day-modal which keeps them visible, struck
# through, for detail review. An export only cares about the open total.
#
# cashflow_preview_module.R calls this exact same function
# (build_export_combined_df), so this one fix covers both the live preview
# panel and the Word/Excel export -- verified by grep, not assumed.
#
# Integration test: builds a mock `shared` (plain functions standing in for
# reactiveVals, the same calling convention build_export_combined_df()
# expects) with one confirmed SAP invoice and one open one, runs the real
# function, and asserts the confirmed one is excluded while the open one
# survives.
#
# Also fixes a second, real, pre-existing bug found live 2026-07-24 (not
# introduced by Stage 11 -- the papelera anti-join predates it): the
# anti-join matched by (Empresa, Moneda, Documento) only, so a brand-new
# manual entry reusing the same key as a previously-deleted, unrelated
# entry (e.g. a repeated "test" invoice) was silently hidden from the
# export/preview even though it was never itself deleted. Fixed to
# anti-join manual entries by UUID instead, matching the calendar's own
# df_combined() precedent exactly.
# =============================================================================

cat("── Stage 11: Cash Flow Preview/Export excludes confirmed invoices ──────\n")

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(uuid)
  library(tidyr); library(stringr); library(lubridate)
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
source("R/data_pipeline.R")
.extract_fn("R/persistence.R", "active_abonos_summary")
.extract_fn("R/pasivos_calendar_glue.R", "pasivos_provisions_as_ledger_rows")
.extract_fn("R/cashflow_export_module.R", "build_export_combined_df")

# ── Static scan: cashflow_preview_module.R shares the exact same function ──
{
  txt <- readLines("R/cashflow_preview_module.R", warn = FALSE)
  .chk(any(grepl("build_export_combined_df\\(", txt)), TRUE,
       "cashflow_preview_module.R calls build_export_combined_df() -- one fix covers both the live preview panel and the export")
}

# ── Integration: a confirmed SAP invoice is excluded, an open one survives ──
{
  raw_ap <- tibble::tibble(
    Documento = c("F-CONF", "F-OPEN"),
    Empresa   = c("ACME", "ACME"),
    Parte     = c("Vendor A", "Vendor B"),
    Moneda    = c("MXN", "MXN"),
    `Saldo vencido` = c(500, 300),
    `Fecha de vencimiento` = as.Date(c("2026-08-01", "2026-08-02")),
    check.names = FALSE
  )

  bancos_confirmados <- tibble::tibble(
    empresa = "ACME", documento = "F-CONF", moneda = "MXN",
    tipo = "pago", eliminado = FALSE, importe = 500
  )

  shared <- list(
    company_map            = function() list(),
    sap_data                = function() list(AR = NULL, AP = raw_ap),
    moves_db                 = function() NULL,
    manual_inv               = function() NULL,
    abonos_db                = function() NULL,
    policy_moves_db          = function() NULL,
    sap_ov_db                = function() NULL,
    pasivos_provisions_db    = function() NULL,
    pasivos_liabilities_db   = function() NULL,
    papelera_rv              = function() NULL,
    bancos_confirmados       = function() bancos_confirmados,
    interco_v2               = function() list(ar_prefix = "C", ap_prefix = "P", companies = list())
  )

  result <- build_export_combined_df(shared, ic_mode_val = "exclude")

  .chk(nrow(result), 1L, "exactly 1 row survives (the open invoice) out of 2 built")
  .chk(result$Documento[1], "F-OPEN", "the surviving row is the OPEN invoice, not the confirmed one")
  .chk("F-CONF" %in% result$Documento, FALSE,
       "the confirmed invoice (F-CONF, matched in bancos_confirmados) is excluded from the export/preview data entirely")
}

# ── Real bug found 2026-07-24: a brand-new manual entry reusing a key from
# a previously-deleted, unrelated papelera row was wrongly hidden ─────────
# The old papelera anti-join here matched by (Empresa, Moneda, Documento)
# only -- so a fresh manual invoice (a new UUID, a different amount/date)
# that happened to share a key with a past, already-deleted entry (e.g. a
# repeated "test" invoice) was silently excluded from the export/preview,
# even though it was never itself deleted. The calendar's own df_combined()
# was already fixed to anti-join manual papelera entries by UUID for
# exactly this reason -- this export never got that same fix until now.
{
  manual_new <- tibble::tibble(
    id = "new-uuid-123", ledger = "AP", Empresa = "ACME", Moneda = "MXN",
    Documento = "test", Factura = "", Parte = "TEST", Codigo = "",
    Importe = 1000, `Abono futuro` = NA_real_,
    `Fecha de vencimiento` = as.Date("2026-08-05"), Notas = "",
    created_by = "dev", created_at = Sys.time(), updated_at = Sys.time(),
    provision_id = NA_character_, liability_id = NA_character_, referencia = NA_character_,
    check.names = FALSE
  )
  # A DIFFERENT, OLD manual_inv row (a different UUID) with the SAME key was
  # deleted in the past -- this is what's in papelera today.
  papelera_stale <- tibble::tibble(
    id = "old-uuid-999", ledger = "AP", source = "manual",
    Empresa = "ACME", Moneda = "MXN", Documento = "test", Parte = "test",
    Importe = 1, FechaEff = as.Date("2026-07-01"),
    deleted_by = "dev", deleted_at = Sys.time() - 3600
  )

  shared2 <- list(
    company_map            = function() list(),
    sap_data                = function() list(AR = NULL, AP = NULL),
    moves_db                 = function() NULL,
    manual_inv               = function() manual_new,
    abonos_db                = function() NULL,
    policy_moves_db          = function() NULL,
    sap_ov_db                = function() NULL,
    pasivos_provisions_db    = function() NULL,
    pasivos_liabilities_db   = function() NULL,
    papelera_rv              = function() papelera_stale,
    bancos_confirmados       = function() NULL,
    interco_v2               = function() list(ar_prefix = "C", ap_prefix = "P", companies = list())
  )

  result2 <- build_export_combined_df(shared2, ic_mode_val = "exclude")
  .chk(is.null(result2), FALSE,
       "a brand-new manual invoice sharing a key with an old, deleted, DIFFERENT entry is NOT wrongly hidden")
  .chk(nrow(result2), 1L, "the new invoice survives")
  .chk(result2$id[1], "new-uuid-123",
       "specifically the NEW invoice (by its own UUID), not somehow a stale reference to the old one")

  txt <- readLines("R/cashflow_export_module.R", warn = FALSE)
  start <- grep("Filter soft-deleted MANUAL entries", txt)
  .chk(length(start) > 0, TRUE, "found the fixed papelera anti-join's comment to scan")
  if (length(start)) {
    block <- paste(sub("#.*$", "", txt[start[1]:min(start[1] + 25, length(txt))]), collapse = "\n")
    .chk(grepl('df\\[\\["id"\\]\\] %in% valid_ids', block), TRUE,
         "the anti-join now matches by id (UUID), not by Empresa/Moneda/Documento")
    .chk(grepl('source"\\]\\] == "manual"', block), TRUE,
         "the anti-join is still scoped to manual-sourced papelera entries only")
    .chk(grepl("anti_join\\(df, pap", block), FALSE,
         "the old dplyr::anti_join() by (Empresa, Moneda, Documento) is actually gone")
  }
}

# ── Static scan: the fix is wired where the design calls for ──────────────
{
  txt <- readLines("R/cashflow_export_module.R", warn = FALSE)
  start <- grep("Exclude already-confirmed invoices \\(Stage 11\\)", txt)
  .chk(length(start) > 0, TRUE, "found the Stage 11 fix's comment to scan")
  if (length(start)) {
    block <- paste(sub("#.*$", "", txt[start[1]:min(start[1] + 20, length(txt))]), collapse = "\n")
    .chk(grepl("compute_confirmed_flags\\(", block), TRUE,
         "uses the canonical compute_confirmed_flags(), not a reimplementation")
    .chk(grepl('df\\[\\["confirmed"\\]\\]', block), TRUE,
         "drops confirmed rows entirely after computing the flags (export cares about open totals, not visual ghosting)")
  }
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
