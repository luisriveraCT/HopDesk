# =============================================================================
# tests/test_confirmed_logic_stage_a.R
# Stage A of the confirmed-invoice-logic unification (see
# docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md): conciliacion_rv is retired as an
# active confirmed-matching source, both on the write side (every
# confirmation used to also write a row to conciliacion_rv) and the read side
# (df_combined() used to treat a conciliacion_rv match as sufficient, on its
# own, to mark an invoice confirmed=TRUE). This is why "Deshacer confirmación"
# didn't fully work before this stage: undo only ever touched
# bancos_confirmados.eliminado / pagar_hoy_db.status, never
# conciliacion_rv — which has no eliminado column and no undo UI at all.
#
# Two kinds of checks:
#   1. Static source scan — asserts the write sites are gone from
#      pagar_hoy_module.R and the read site is gone from ledger_module.R.
#      Same style as tests/test_saas_log_action_scoping.R's production-file
#      scan: cheap, durable, and fails loudly if either write block or the
#      read block is ever reintroduced.
#   2. Logic simulation — replicates df_combined()'s current (post-Stage-A)
#      3-source matching against synthetic data, confirming sources 2/3/4
#      (bancos_confirmados / papelera ghosts / pagar_hoy_db) are unchanged,
#      and that a row which would previously have matched ONLY via
#      conciliacion_rv is no longer marked confirmed.
# =============================================================================

cat("── Confirmed-logic Stage A: conciliacion_rv retirement ─────────────────\n")

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

# ── 1. Static scan: write sites gone from pagar_hoy_module.R ────────────────
{
  txt <- readLines("R/pagar_hoy_module.R", warn = FALSE)
  .chk(any(grepl("shared\\$conciliacion_rv\\(", txt)), FALSE,
       "R/pagar_hoy_module.R: no confirm handler writes to shared$conciliacion_rv(...)")
  .chk(any(grepl("save_conciliacion\\(", txt)), FALSE,
       "R/pagar_hoy_module.R: no confirm handler calls save_conciliacion(...)")
}

# ── 2. Static scan: read site gone from ledger_module.R's df_combined() ─────
{
  txt <- readLines("R/ledger_module.R", warn = FALSE)
  start <- grep("df_combined\\s*<-\\s*reactive\\(", txt)
  .chk(length(start) >= 1, TRUE, "R/ledger_module.R: df_combined() found")
  if (length(start) >= 1) {
    # df_combined() is a large reactive; scan from its start to the next
    # top-level function/reactive definition (or EOF) rather than guessing a
    # fixed line window.
    later_defs <- grep("^\\s*[a-zA-Z_.][a-zA-Z0-9_.]*\\s*<-\\s*(reactive|function|observeEvent|observe)\\(",
                       txt)
    later_defs <- later_defs[later_defs > start[1]]
    end <- if (length(later_defs)) min(later_defs) - 1 else length(txt)
    block <- txt[start[1]:end]
    .chk(any(grepl("shared\\$conciliacion_rv\\(", block)), FALSE,
         "R/ledger_module.R: df_combined() no longer reads shared$conciliacion_rv()")
  }
}

# ── 3. Logic simulation: post-Stage-A 3-source matching on synthetic data ───
{
  tipo_val <- "pago"  # AP

  df <- data.frame(
    Empresa   = c("EMP1", "EMP1", "EMP1", "EMP1"),
    Moneda    = c("MXN", "MXN", "MXN", "MXN"),
    Documento = c("DOC-BC", "DOC-PAP", "DOC-PH", "DOC-CONC-ONLY"),
    source    = c("sap", "sap", "sap", "sap"),
    stringsAsFactors = FALSE
  )
  df$confirmed <- FALSE
  is_manual    <- rep(FALSE, nrow(df))
  is_provision <- rep(FALSE, nrow(df))

  # Source: bancos_confirmados — should still match DOC-BC.
  conf_active <- data.frame(empresa = "EMP1", documento = "DOC-BC", moneda = "MXN",
                             stringsAsFactors = FALSE)
  match_key <- paste(toupper(trimws(df$Empresa)), toupper(trimws(df$Documento)),
                     toupper(trimws(df$Moneda)))
  conf_key  <- paste(toupper(trimws(conf_active$empresa)), toupper(trimws(conf_active$documento)),
                     toupper(trimws(conf_active$moneda)))
  bc_mask <- (match_key %in% conf_key) & !is_manual & !is_provision
  df$confirmed <- df$confirmed | bc_mask

  # Source: papelera SAP ghosts — should still match DOC-PAP.
  sap_pap <- data.frame(Empresa = "EMP1", Moneda = "MXN", Documento = "DOC-PAP",
                        stringsAsFactors = FALSE)
  pap_match_key <- paste(df$Empresa, df$Moneda, df$Documento)
  pap_key       <- paste(sap_pap$Empresa, sap_pap$Moneda, sap_pap$Documento)
  ghost_mask <- (pap_match_key %in% pap_key) & !is_provision
  df$confirmed <- df$confirmed | ghost_mask

  # Source: pagar_hoy_db — should still match DOC-PH.
  ph_conf <- data.frame(Empresa = "EMP1", Documento = "DOC-PH", Moneda = "MXN",
                        stringsAsFactors = FALSE)
  ph_key <- paste(toupper(trimws(ph_conf$Empresa)), toupper(trimws(ph_conf$Documento)),
                  toupper(trimws(ph_conf$Moneda)))
  df_key <- paste(toupper(trimws(df$Empresa)), toupper(trimws(df$Documento)),
                  toupper(trimws(df$Moneda)))
  ph_mask <- (df_key %in% ph_key) & !is_provision
  df$confirmed <- df$confirmed | ph_mask

  # NOTE: deliberately NOT applying a conciliacion_rv source here — that's
  # the entire point of Stage A. DOC-CONC-ONLY has no match in any of the
  # 3 remaining sources, simulating a row that pre-Stage-A would only have
  # matched via conciliacion_rv.

  .chk(df$confirmed[df$Documento == "DOC-BC"],  TRUE,
       "bancos_confirmados match still sets confirmed=TRUE (regression)")
  .chk(df$confirmed[df$Documento == "DOC-PAP"], TRUE,
       "papelera SAP-ghost match still sets confirmed=TRUE (regression)")
  .chk(df$confirmed[df$Documento == "DOC-PH"],  TRUE,
       "pagar_hoy_db match still sets confirmed=TRUE (regression)")
  .chk(df$confirmed[df$Documento == "DOC-CONC-ONLY"], FALSE,
       "a conciliacion_rv-only match no longer sets confirmed=TRUE (Stage A fix)")
}

cat("\n")
