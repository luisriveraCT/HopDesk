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

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
