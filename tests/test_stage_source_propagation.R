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

# ── 2. Static scan: ledger_module.R's stage_all and stage_sel ──────────────
{
  txt <- readLines("R/ledger_module.R", warn = FALSE)
  joined <- paste(txt, collapse = "\n")

  stage_all_start <- grep("observeEvent\\(input\\$stage_all,", txt)
  stage_sel_start <- grep("observeEvent\\(input\\$stage_sel,", txt)
  .chk(length(stage_all_start) > 0, TRUE, "found stage_all observer to scan")
  .chk(length(stage_sel_start) > 0, TRUE, "found stage_sel observer to scan")

  if (length(stage_all_start) && length(stage_sel_start)) {
    stage_all_block <- paste(txt[stage_all_start[1]:(stage_sel_start[1] - 1)], collapse = "\n")
    .chk(grepl("src_lookup", stage_all_block), TRUE,
         "stage_all builds a src_lookup table")
    .chk(grepl('"status","source"\\)', stage_all_block), TRUE,
         "stage_all's final column selection now includes source")

    # stage_sel's block runs to the next observer/section after it.
    next_start <- grep("Cart buttons", txt)
    next_start <- next_start[next_start > stage_sel_start[1]][1] %||% (stage_sel_start[1] + 60)
    stage_sel_block <- paste(txt[stage_sel_start[1]:next_start], collapse = "\n")
    .chk(grepl("src_lookup", stage_sel_block), TRUE,
         "stage_sel builds a src_lookup table")
    .chk(grepl('"status","source"\\)', stage_sel_block), TRUE,
         "stage_sel's final column selection now includes source")
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

# ── 4. Static scan: ledger_module.R's cart_inv_click ────────────────────────
{
  txt <- readLines("R/ledger_module.R", warn = FALSE)
  start <- grep("observeEvent\\(input\\$cart_inv_click,", txt)
  .chk(length(start) > 0, TRUE, "found cart_inv_click observer to scan")
  if (length(start)) {
    end <- grep("\\}, ignoreInit\\s*=\\s*TRUE, ignoreNULL\\s*=\\s*TRUE\\)", txt)
    end <- end[end > start[1]][1] %||% (start[1] + 70)
    block <- paste(sub("#.*$", "", txt[start[1]:end]), collapse = "\n")
    .chk(grepl('source\\s*=\\s*"sap"\\s*,', block), FALSE,
         "cart_inv_click no longer hardcodes a bare source = \"sap\" literal")
    .chk(grepl('one_source', block), TRUE,
         "cart_inv_click reads the row's real source (one_source) before building the new row")
    .chk(grepl('one_source\\s*!=\\s*"manual"', block), TRUE,
         "cart_inv_click normalizes anything that isn't literally \"manual\" to \"sap\", matching the other sites' rule")
  }
}

# ── 5. Static scan: interco_module.R's .ic_send_rows and
# treasury_map_module.R's send_to_agenda now set source explicitly ────────
{
  txt_ic <- readLines("R/interco_module.R", warn = FALSE)
  start_ic <- grep("\\.ic_send_rows\\s*<-\\s*function", txt_ic)
  .chk(length(start_ic) > 0, TRUE, "found .ic_send_rows to scan")
  if (length(start_ic)) {
    block_ic <- paste(txt_ic[start_ic[1]:min(start_ic[1] + 55, length(txt_ic))], collapse = "\n")
    .chk(grepl('source\\s*=\\s*"sap"', block_ic), TRUE,
         ".ic_send_rows now sets source = \"sap\" explicitly on new pagar_hoy rows")
  }

  txt_tm <- readLines("R/treasury_map_module.R", warn = FALSE)
  start_tm <- grep("observeEvent\\(input\\$send_to_agenda,", txt_tm)
  .chk(length(start_tm) > 0, TRUE, "found send_to_agenda observer to scan")
  if (length(start_tm)) {
    block_tm <- paste(txt_tm[start_tm[1]:min(start_tm[1] + 40, length(txt_tm))], collapse = "\n")
    .chk(grepl('source\\s*=\\s*"sap"', block_tm), TRUE,
         "send_to_agenda now sets source = \"sap\" explicitly on new pagar_hoy rows")
  }
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
