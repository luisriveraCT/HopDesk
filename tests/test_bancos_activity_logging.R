# =============================================================================
# tests/test_bancos_activity_logging.R
#
# Stage 2 of the Bancos/Pasivos activity-logging extension (2026-09-03):
# enriches the 9 existing log_action(module="bancos", ...) call sites in
# R/bancos_module.R with structured metadata, and teaches the shared
# Actividad detail-modal (R/audit_log_viewer_module.R, from the earlier
# Actividad audit overhaul) two new bespoke renderers for it:
#   - "bancos_movimientos"  : a row table, for the 4 bulk actions
#     (importar_movimientos, eliminar_movimientos, reasignar_sesion,
#     eliminar_sesion) via .bancos_audit_rows_summary() (R/bancos_persistence.R)
#   - "bancos_manual_edit"  : a before/after diff table, for
#     editar_movimiento_manual (agregar_movimiento_manual has no "before",
#     shown as a flat summary instead)
# The remaining single-row actions (deshacer_eliminar_movimiento_manual,
# deshacer_abono, eliminar_confirmacion, revertir_confirmacion) deliberately
# get NO bespoke renderer -- their metadata is a handful of scalar fields the
# existing generic key/value view already renders cleanly. This is the
# "otherwise don't" half of Mouse's instruction: not every action needs a
# popup just because one is now possible.
#
# Sourced by _run_saas.R.
# =============================================================================

cat("── Bancos structured activity logging (Stage 2) ─────────────────────────\n")

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
.chk_true  <- function(cond, label) .chk(isTRUE(cond), TRUE, label)
.chk_false <- function(cond, label) .chk(isTRUE(cond), FALSE, label)
.html <- function(tag) as.character(htmltools::doRenderTags(tag))

source("R/bancos_persistence.R",      local = FALSE)
source("R/audit_log_viewer_module.R", local = FALSE)

# =============================================================================
# Part A -- .bancos_audit_rows_summary() -- pure function
# =============================================================================
cat("\n-- Part A: .bancos_audit_rows_summary() --\n")

.chk(.bancos_audit_rows_summary(NULL), list(n_total = 0L, truncated = FALSE, rows = list()),
     "NULL input -> empty, non-truncated summary")
.chk(.bancos_audit_rows_summary(data.frame()), list(n_total = 0L, truncated = FALSE, rows = list()),
     "0-row input -> empty, non-truncated summary")

mk_movs <- function(n, cargo_val = 100) tibble::tibble(
  fecha    = as.Date("2026-08-01") + seq_len(n) - 1L,
  parte    = paste0("Parte ", seq_len(n)),
  concepto = paste0("Concepto ", seq_len(n)),
  tipo     = "spei_out",
  cargo    = rep(cargo_val, n),
  abono    = rep(0, n)
)

small <- mk_movs(3)
s3 <- .bancos_audit_rows_summary(small)
.chk(s3$n_total, 3L, "3-row input: n_total reflects the real row count")
.chk_false(s3$truncated, "3-row input (under the cap): truncated is FALSE")
.chk(length(s3$rows), 3L, "3-row input: rows list has exactly 3 entries")
.chk(s3$rows[[1]]$parte, "Parte 1", "row 1's parte is carried through untouched")
.chk(s3$rows[[1]]$cargo, 100, "row 1's cargo is carried through untouched")
.chk(s3$rows[[2]]$fecha, as.character(as.Date("2026-08-02")), "row 2's fecha is stringified (JSON-safe)")

big <- mk_movs(500)
s500 <- .bancos_audit_rows_summary(big)
.chk(s500$n_total, 500L, "500-row input: n_total is the TRUE total, not the capped count")
.chk_true(s500$truncated, "500-row input (over the 300 cap): truncated is TRUE")
.chk(length(s500$rows), 300L, "500-row input: rows list is capped at .BANCOS_AUDIT_ROW_CAP (300)")

exactly_cap <- mk_movs(.BANCOS_AUDIT_ROW_CAP)
s_cap <- .bancos_audit_rows_summary(exactly_cap)
.chk_false(s_cap$truncated, "input exactly AT the cap: truncated is FALSE (not > max_rows)")

na_row <- tibble::tibble(fecha = as.Date("2026-08-01"), parte = NA_character_,
                         concepto = NA_character_, tipo = "spei_out", cargo = NA_real_, abono = 50)
s_na <- .bancos_audit_rows_summary(na_row)
.chk(s_na$rows[[1]]$parte, "", "an NA parte normalizes to empty string, not the literal string 'NA'")

# =============================================================================
# Part B -- .audit_detail_kind() -- new bancos classifications
# =============================================================================
cat("\n-- Part B: .audit_detail_kind() bancos classifications --\n")

for (a in c("importar_movimientos", "eliminar_movimientos", "reasignar_sesion", "eliminar_sesion"))
  .chk(.audit_detail_kind("bancos", a), "bancos_movimientos",
       sprintf("bancos/%s -> bancos_movimientos (bulk row-table view)", a))

for (a in c("editar_movimiento_manual", "agregar_movimiento_manual"))
  .chk(.audit_detail_kind("bancos", a), "bancos_manual_edit",
       sprintf("bancos/%s -> bancos_manual_edit (before/after view)", a))

for (a in c("deshacer_eliminar_movimiento_manual", "deshacer_abono",
            "eliminar_confirmacion", "revertir_confirmacion"))
  .chk(.audit_detail_kind("bancos", a), "generic",
       sprintf("bancos/%s -> generic (single-row, scalar fields -- deliberately no bespoke view)", a))

# =============================================================================
# Part C -- .audit_detail_bancos_movimientos() rendering
# =============================================================================
cat("\n-- Part C: .audit_detail_bancos_movimientos() --\n")

meta_empty <- list(n_total = 0L, truncated = FALSE, rows = list())
html_empty <- .html(.audit_detail_bancos_movimientos(meta_empty))
.chk_true(grepl("Sin detalle de movimientos", html_empty, fixed = TRUE),
     "empty rows: shows the 'no detail' fallback message")

meta_full <- .bancos_audit_rows_summary(mk_movs(2, cargo_val = 1234.5))
html_full <- .html(.audit_detail_bancos_movimientos(meta_full))
.chk_true(grepl("Parte 1", html_full, fixed = TRUE), "non-truncated: row 1's parte rendered")
.chk_true(grepl("Parte 2", html_full, fixed = TRUE), "non-truncated: row 2's parte rendered")
.chk_true(grepl("1,234.50", html_full, fixed = TRUE), "non-truncated: cargo formatted with thousands separator + 2 decimals")
.chk_false(grepl("Mostrando los primeros", html_full, fixed = TRUE),
     "non-truncated: no truncation notice shown")

meta_trunc <- .bancos_audit_rows_summary(mk_movs(500))
html_trunc <- .html(.audit_detail_bancos_movimientos(meta_trunc))
.chk_true(grepl("Mostrando los primeros 300 de 500", html_trunc, fixed = TRUE),
     "truncated: shows an explicit 'first 300 of 500' notice, not a silent partial list")

# A row with abono only (no cargo) must not render "0.00" in the Cargo column
meta_abono <- .bancos_audit_rows_summary(tibble::tibble(
  fecha = as.Date("2026-08-01"), parte = "X", concepto = "Y", tipo = "spei_in", cargo = 0, abono = 500))
html_abono <- .html(.audit_detail_bancos_movimientos(meta_abono))
.chk_false(grepl(">0.00<", html_abono, fixed = TRUE),
     "a zero cargo on an abono-only row renders as blank, not '0.00' (matches app-wide fmt_money convention)")
.chk_true(grepl("500.00", html_abono, fixed = TRUE), "the actual abono value renders")

# =============================================================================
# Part D -- .audit_detail_bancos_manual_edit() rendering
# =============================================================================
cat("\n-- Part D: .audit_detail_bancos_manual_edit() --\n")

# "agregar" shape: no `after` wrapper, flat fields at the top level
meta_add <- list(cuenta_id = "CTA-01", tipo = "manual_in", cargo = 0, abono = 750)
html_add <- .html(.audit_detail_bancos_manual_edit(meta_add))
.chk_true(grepl("CTA-01", html_add, fixed = TRUE), "add: cuenta_id rendered")
.chk_true(grepl("750.00", html_add, fixed = TRUE), "add: abono rendered")

# "editar" shape: before/after, only CHANGED fields should render as rows
meta_edit <- list(
  before = list(cuenta_id = "CTA-01", fecha = "2026-08-01", tipo = "manual_in",
               parte = "Proveedor A", concepto = "Pago inicial", cargo = 0, abono = 100, notas = ""),
  after  = list(cuenta_id = "CTA-01", fecha = "2026-08-01", tipo = "manual_in",
               parte = "Proveedor A", concepto = "Pago corregido", cargo = 0, abono = 150, notas = "")
)
html_edit <- .html(.audit_detail_bancos_manual_edit(meta_edit))
.chk_true(grepl("Pago inicial", html_edit, fixed = TRUE), "edit: changed field's BEFORE value rendered")
.chk_true(grepl("Pago corregido", html_edit, fixed = TRUE), "edit: changed field's AFTER value rendered")
.chk_true(grepl("100", html_edit, fixed = TRUE), "edit: cargo/abono before value rendered")
.chk_true(grepl("150", html_edit, fixed = TRUE), "edit: cargo/abono after value rendered")
.chk_false(grepl("CTA-01</td>\\s*<td>CTA-01", gsub("\n", " ", html_edit)),
     "edit: an UNCHANGED field (cuenta_id, identical before/after) is not rendered as a diff row")

meta_nochange <- list(
  before = list(cuenta_id = "CTA-01", fecha = "2026-08-01", tipo = "manual_in",
               parte = "X", concepto = "Y", cargo = 0, abono = 100, notas = ""),
  after  = list(cuenta_id = "CTA-01", fecha = "2026-08-01", tipo = "manual_in",
               parte = "X", concepto = "Y", cargo = 0, abono = 100, notas = "")
)
html_nochange <- .html(.audit_detail_bancos_manual_edit(meta_nochange))
.chk_true(grepl("Sin cambios de valor", html_nochange, fixed = TRUE),
     "edit with an identical before/after: shows a 'no changes' message, not an empty table")

# =============================================================================
# Part E -- end-to-end via .audit_detail_modal_body() (full dispatch path)
# =============================================================================
cat("\n-- Part E: end-to-end dispatch (module/action -> real metadata -> real HTML) --\n")

.mk_row <- function(module, action, description, metadata_list, user = "tester", target_id = NA_character_) {
  tibble::tibble(
    id = "x", ts = Sys.time(), user = user, module = module, action = action,
    description = description, target_id = target_id, s3_key = NA_character_,
    client_id = "networks",
    metadata = as.character(jsonlite::toJSON(metadata_list, auto_unbox = TRUE, na = "null"))
  )
}

row_import <- .mk_row("bancos", "importar_movimientos", "2 movimiento(s) importado(s).",
  c(list(cuenta_id = "CTA-01", n_importados = 2L, n_auto_conciliado = 0L),
    .bancos_audit_rows_summary(mk_movs(2))))
html_import <- .html(.audit_detail_modal_body(row_import, "own_client"))
.chk_true(grepl("Parte 1", html_import, fixed = TRUE),
     "end-to-end importar_movimientos: real row detail reaches the popup (bespoke renderer engaged)")

row_del <- .mk_row("bancos", "eliminar_movimientos", "2 movimiento(s) enviado(s) a papelera",
  c(list(n = 2L), .bancos_audit_rows_summary(mk_movs(2))))
html_del <- .html(.audit_detail_modal_body(row_del, "own_client"))
.chk_true(grepl("Parte 1", html_del, fixed = TRUE),
     "end-to-end eliminar_movimientos: real row detail reaches the popup")

row_manual_edit <- .mk_row("bancos", "editar_movimiento_manual", "Movimiento manual editado",
  meta_edit)
html_manual_edit <- .html(.audit_detail_modal_body(row_manual_edit, "own_client"))
.chk_true(grepl("Pago corregido", html_manual_edit, fixed = TRUE),
     "end-to-end editar_movimiento_manual: before/after diff reaches the popup")

# The 4 single-row actions -- confirm they still resolve through generic (no
# crash, no missing renderer) now that their metadata is non-empty.
row_deshacer <- .mk_row("bancos", "deshacer_abono", "Abono parcial deshecho",
  list(empresa = "ACME", documento = "DOC-1", parte = "Proveedor A", importe = 500, fecha_abono = "2026-08-01"))
html_deshacer <- .html(.audit_detail_modal_body(row_deshacer, "own_client"))
.chk_true(grepl("Proveedor A", html_deshacer, fixed = TRUE),
     "end-to-end deshacer_abono: enriched scalar metadata renders via the generic key/value view")

row_revertir <- .mk_row("bancos", "revertir_confirmacion", "Confirmación revertida: Pago 01/08/2026",
  list(empresa = "ACME", parte = "Proveedor A", documento = "DOC-2", importe = 250, tipo = "pago",
       fecha_devuelta = "2026-08-01"))
html_revertir <- .html(.audit_detail_modal_body(row_revertir, "own_client"))
.chk_true(grepl("DOC-2", html_revertir, fixed = TRUE),
     "end-to-end revertir_confirmacion: enriched scalar metadata renders via the generic key/value view")

# =============================================================================
# Part F -- static scan: every one of the 9 bancos log_action() call sites
# passes a non-empty `metadata` argument (regression guard against a future
# edit accidentally dropping the enrichment this stage added)
# =============================================================================
cat("\n-- Part F: static scan of R/bancos_module.R log_action() call sites --\n")

bm_txt <- paste(readLines("R/bancos_module.R", warn = FALSE), collapse = "\n")
.chk_true({p <- tryCatch({parse("R/bancos_module.R"); TRUE}, error = function(e) FALSE); p},
          "R/bancos_module.R still parses without syntax errors")

call_blocks <- regmatches(bm_txt, gregexpr("log_action\\(\\s*\\n(?:.*?\\n)*?\\s*\\)", bm_txt, perl = TRUE))[[1]]
bancos_calls <- Filter(function(b) grepl('module\\s*=\\s*"bancos"', b), call_blocks)
.chk(length(bancos_calls), 9L, "found all 9 log_action(module=\"bancos\", ...) call sites")

n_with_metadata <- sum(vapply(bancos_calls, function(b) grepl("metadata\\s*=", b), logical(1)))
.chk(n_with_metadata, 9L, "every one of the 9 bancos log_action() calls now passes a metadata argument")

for (marker in c(".bancos_audit_rows_summary(del_rows)", ".bancos_audit_rows_summary(reasig_rows)",
                 ".bancos_audit_rows_summary(sesion_rows)", ".bancos_audit_rows_summary(nuevos)")) {
  .chk_true(grepl(marker, bm_txt, fixed = TRUE),
       sprintf("bulk-action row summary wired in: %s", marker))
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
