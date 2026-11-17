# =============================================================================
# tests/test_stage_source_propagation.R
# Real, severe bug found live 2026-07-24: Calendario's day-modal "Agregar
# todo"/"Agregar selección" (R/ledger_module.R's stage_all/stage_sel) and the
# shared handle_invoice_action()'s stage_all/stage_selected branch (used by
# Search and Vencidos' Agregar todo/selección) all built the staged
# pagar_hoy row WITHOUT a source column at all.
#
# Consequence: load_pagar_hoy() normalizes a missing/NA source to "sap", and
# is_erp_sourced(NA) also treats it as ERP-sourced -- so every manual
# invoice staged through any of these three paths was silently
# misclassified as an ERP row. This was invisible before Stage 4 (nothing
# branched on source at confirm time), but Stage 4's manual-archiving logic
# filters candidates by !is_erp_sourced(source) -- so a misclassified
# manual row skipped archiving entirely: bancos_confirmados got written,
# the pagar_hoy row got unstaged (that part never checked source), but the
# manual_inv row was never removed. Since manual rows only disappear from
# the calendar via that removal (bancos_confirmados matching deliberately
# excludes manual rows), the invoice kept showing as open and unconfirmed
# even after being "confirmed" -- and re-staging it created a second,
# equally mislabeled pagar_hoy row. Confirmed against real production data:
# 12 manual invoices confirmed via Agenda on 2026-07-24 were left exactly
# in this state (bancos_confirmados written twice, manual_inv untouched,
# zero papelera archive rows, duplicate pagar_hoy rows under fresh ids).
#
# Fix: all three sites now build a source lookup from the row's own
# Empresa/Moneda/Documento key (already carrying source, since it comes
# from df_combined()/the client payload) and propagate it into the staged
# row, defaulting anything that isn't literally "manual" to "sap" -- never
# leaving it NA.
#
# Follow-up, found 2026-07-24 (same day, same bug class -- this staging
# site was missed by the fix above): R/ledger_module.R's cart_inv_click
# observer is the per-individual-invoice "+" toggle shown when a Parte
# group is expanded in the calendar day-modal cart (distinct from the
# already-fixed group-level cart_<i> button and from stage_all/stage_sel).
# It had `one[["source"]]` available on the row but hardcoded
# `source = "sap"` unconditionally when building the new pagar_hoy row,
# never consulting it. Reproduced live: a manual invoice staged via this
# single-invoice "+" and later confirmed via Agenda got bancos_confirmados
# written and unstaged correctly, but was never archived out of
# manual_inv (is_erp_sourced("sap") == TRUE skipped the manual-archive
# branch) -- identical symptom to the original 12-invoice incident.
#
# Also hardened two other pagar_hoy-row-construction sites that never set
# `source` at all (R/interco_module.R's .ic_send_rows, R/treasury_map_
# module.R's send_to_agenda) -- not implicated in any known incident (both
# are fed only by SAP/intercompany snapshot data today, so the NA->"sap"
# default they relied on was accidentally correct), but closed
# defensively so the whole bug class can't recur if a manual row ever
# flows through either path.
# =============================================================================

cat("── Staged pagar_hoy rows always carry an explicit source ───────────────\n")

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

# ── 1. Unit coverage of the exact normalization rule used at all 3 sites ───
{
  norm <- function(source) ifelse(is.na(source) | source != "manual", "sap", "manual")
  .chk(norm("manual"),   "manual", "source 'manual' stays 'manual'")
  .chk(norm("sap"),      "sap",    "source 'sap' stays 'sap'")
  .chk(norm(NA_character_), "sap", "NA source normalizes to 'sap' (never left NA)")
  .chk(norm("provision"), "sap",   "any other source (e.g. 'provision') normalizes to 'sap', not silently kept as-is")
  .chk(norm(c("manual","sap",NA,"provision")), c("manual","sap","sap","sap"),
       "vectorized: mixed batch normalizes element-wise")
}

# ── 2. Static scan: ledger_module.R's day-modal staging ────────────────────
# T05: stage_all, stage_sel and the two ＋ toggles are one handler,
# .dia_agregar (the day modal on the table engine).
{
  txt <- readLines("R/ledger_module.R", warn = FALSE)
  start <- grep("\\.dia_agregar <- function\\(keys, origen\\)", txt)
  .chk(length(start) > 0, TRUE, "found .dia_agregar (was stage_all/stage_sel/the ＋) to scan")
  if (length(start)) {
    end <- grep("Raw data diagnostic viewer", txt)
    end <- end[end > start[1]][1] %||% (start[1] + 120)
    block <- paste(txt[start[1]:end], collapse = "\n")
    .chk(grepl("src_lookup", block), TRUE, ".dia_agregar builds a src_lookup table")
    # cierra-facturas added fecha_factura after source in the same selection.
    .chk(grepl('"status","source"[,)]', block), TRUE, ".dia_agregar's final column selection includes source")
  }
}

# ── 3. Static scan: handle_invoice_action()'s stage_all/stage_selected ─────
{
  txt <- readLines("R/search_module.R", warn = FALSE)
  start <- grep('action %in% c\\("stage_all", "stage_selected"\\)', txt)
  .chk(length(start) > 0, TRUE, "found handle_invoice_action's stage_all/stage_selected branch to scan")
  if (length(start)) {
    block <- paste(txt[start[1]:min(start[1] + 40, length(txt))], collapse = "\n")
    .chk(grepl("source\\s*=\\s*ifelse\\(", block), TRUE,
         "handle_invoice_action's new_rows now assigns source from keys_df$source")
    .chk(grepl("keys_df\\$source", block), TRUE,
         "the assignment actually reads keys_df$source (the row's real source), not a hardcoded value")
  }
}

# ── 4. (T05) ledger_module.R's cart_inv_click is gone: a row's ＋ reaches
# .dia_agregar (section 2), which never hardcodes source. ──────────────────
{
  txt <- paste(sub("#.*$", "", readLines("R/ledger_module.R", warn = FALSE)), collapse = "\n")
  .chk(grepl("observeEvent\\(input\\$cart_inv_click,", txt), FALSE,
       "no separate cart_inv_click staging path left to drift")
}

# ── 5. interco_module.R's .ic_send_rows and treasury_map_module.R's
# send_to_agenda: new pagar_hoy rows always carry an explicit ERP source ────
# Since f806745 (Luis, 2026-11-04: confirming always closes the invoice) both
# build their rows with agenda_filas_factura() (R/persistence.R), which sets
# `source` from the invoice, or SAP_B1_ERP_TYPE when it has none. The old
# check looked for the literal source = "sap" in each body (T43): now each
# body must use the helper and build no new row of its own (a new staged row
# says status = "pending"; the empty-schema fallback does not), and the helper's
# rows must carry a non-NA ERP source.
{
  # Comments out, so a mention in a comment proves nothing.
  sin_coment <- function(x) sub("#.*$", "", x)
  txt_ic <- sin_coment(readLines("R/interco_module.R", warn = FALSE))
  start_ic <- grep("\\.ic_send_rows\\s*<-\\s*function", txt_ic)
  .chk(length(start_ic) > 0, TRUE, "found .ic_send_rows to scan")
  if (length(start_ic)) {
    block_ic <- paste(txt_ic[start_ic[1]:min(start_ic[1] + 60, length(txt_ic))], collapse = "\n")
    .chk(grepl("agenda_filas_factura\\(", block_ic) && !grepl('status\\s*=\\s*"pending"', block_ic), TRUE,
         ".ic_send_rows builds its pagar_hoy rows with agenda_filas_factura(), none by hand")
  }

  txt_tm <- sin_coment(readLines("R/treasury_map_module.R", warn = FALSE))
  start_tm <- grep("observeEvent\\(input\\$send_to_agenda,", txt_tm)
  .chk(length(start_tm) > 0, TRUE, "found send_to_agenda observer to scan")
  if (length(start_tm)) {
    block_tm <- paste(txt_tm[start_tm[1]:min(start_tm[1] + 70, length(txt_tm))], collapse = "\n")
    .chk(grepl("agenda_filas_factura\\(", block_tm) && !grepl('status\\s*=\\s*"pending"', block_tm), TRUE,
         "send_to_agenda builds its pagar_hoy rows with agenda_filas_factura(), none by hand")
  }

  # The helper itself (SFP_PERSISTENCE_R: another persistence.R, for the
  # contraprueba). Loaded into an environment of its own: this file is also a
  # module of tests/_run_confirmed_logic.R, and nothing here may replace the
  # functions the runner's later modules use.
  local({
  E <- new.env(parent = globalenv())
  E[["%||%"]] <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
  carga <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% nombres) eval(e, envir = E)
  carga("R/erp_connector_registry.R", "SAP_B1_ERP_TYPE")
  carga("R/global.R", "is_erp_sourced")
  carga(Sys.getenv("SFP_PERSISTENCE_R", "R/persistence.R"), c(".schema_pagar_hoy", "agenda_filas_factura"))
  agenda_filas_factura <- E$agenda_filas_factura; is_erp_sourced <- E$is_erp_sourced
  SAP_B1_ERP_TYPE <- E$SAP_B1_ERP_TYPE
  if (!requireNamespace("tibble", quietly = TRUE)) stop("tibble needed")
  E$tibble <- tibble::tibble
  fac <- data.frame(Empresa = "NTS", Moneda = "MXN", Documento = c("D1", "D2"), Parte = "Proveedor",
                    Codigo = "P1", `Saldo vencido` = c(100, 200), `Fecha de vencimiento` = as.Date("2026-12-01"),
                    `Fecha de contabilización` = as.Date("2026-11-01"), check.names = FALSE, stringsAsFactors = FALSE)
  sin <- agenda_filas_factura(fac, "tester", ledger = "AP")
  .chk(nrow(sin) == 2L && identical(unique(sin$source), SAP_B1_ERP_TYPE), TRUE,
       "agenda_filas_factura: invoices without a source column -> rows with source = SAP_B1_ERP_TYPE")
  otro <- agenda_filas_factura(transform(fac, source = "otro_erp"), "tester", ledger = "AP")
  .chk(nrow(otro) == 2L && identical(unique(otro$source), "otro_erp"), TRUE,
       "agenda_filas_factura: another ERP's source is kept as is")
  na <- agenda_filas_factura(transform(fac, source = NA_character_), "tester", ledger = "AP")
  todas <- c(sin$source, otro$source, na$source)
  .chk(nrow(na) == 2L && length(todas) == 6L && !anyNA(todas) && all(is_erp_sourced(todas)) &&
         identical(unique(na$source), SAP_B1_ERP_TYPE), TRUE,
       "agenda_filas_factura: an NA source becomes SAP_B1_ERP_TYPE; no row ever has an NA source, all are ERP")
  })
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
