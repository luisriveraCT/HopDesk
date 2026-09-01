# =============================================================================
# tests/test_activity_stage3_pdf_export.R
#
# Stage 3 of the Actividad audit overhaul (2026-08-26): PDF export of the
# currently filtered log, segmented per Módulo, containing both the on-screen
# table AND (per entry) the same detail Stage 2's popup shows. This file
# unit-tests the pure/plain-function layer -- .audit_module_summary(),
# .audit_pdf_entry_card(), and generate_audit_log_html_doc() itself -- all of
# which take plain data and return tags/a file path with no reactive
# context, so none of this needs pagedown/Chrome to be installed (only the
# actual downloadHandler, untested here, needs those -- same scope boundary
# this codebase already draws around R/cashflow_export_module.R and
# R/pagos_audit_viewer_module.R's own PDF builders).
#
# Sourced by _run_saas.R (which already loads R/audit_log_viewer_module.R).
# =============================================================================

cat("── Activity audit Stage 3: PDF export ─────────────────────────────────────\n")

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

.mk_row <- function(module, action, description, metadata_list,
                    target_id = NA_character_, ts = "2026-08-26 12:00:00", user = "larivera") {
  data.frame(
    id = uuid::UUIDgenerate(), ts = as.POSIXct(ts, tz = "UTC"),
    user = user, module = module, action = action, description = description,
    target_id = target_id, s3_key = NA_character_, client_id = "networks",
    metadata = as.character(jsonlite::toJSON(metadata_list, auto_unbox = TRUE, na = "null")),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# Part A -- .audit_module_summary()
# =============================================================================
cat("\n-- Part A: .audit_module_summary() --\n")

.chk(nrow(.audit_module_summary(NULL)), 0L, "NULL df: zero-row summary")
.chk(nrow(.audit_module_summary(data.frame(module = character(0)))), 0L, "empty df: zero-row summary")

df_a <- rbind(
  .mk_row("pagar_hoy", "confirmar_pago", "x", list()),
  .mk_row("pagar_hoy", "confirmar_cobro", "y", list()),
  .mk_row("bancos", "importar_movimientos", "z", list())
)
summ_a <- .audit_module_summary(df_a)
.chk(nrow(summ_a), 2L, "3 rows across 2 modules: summary has 2 rows")
.chk(summ_a$module, c("bancos", "pagar_hoy"), "summary is alphabetically ordered by módulo")
.chk(summ_a$n[summ_a$module == "pagar_hoy"], 2L, "pagar_hoy count is 2")
.chk(summ_a$n[summ_a$module == "bancos"], 1L, "bancos count is 1")

df_na_mod <- .mk_row(NA_character_, "x", "y", list())
summ_na <- .audit_module_summary(df_na_mod)
.chk(summ_na$module[1], "(sin módulo)", "NA módulo is grouped under '(sin módulo)', not dropped")

# =============================================================================
# Part B -- .audit_pdf_entry_card()
# =============================================================================
cat("\n-- Part B: .audit_pdf_entry_card() --\n")

row_pago <- .mk_row("pagar_hoy", "confirmar_pago", "1 pago(s) confirmado(s)",
  list(empresa = "ACME", moneda = "MXN", cuenta_id = "CTA-01",
       pagos = list(list(parte = "PROVEEDOR X", documento = "FAC-7", importe = 300,
                         moneda = "MXN", cuenta_id = "CTA-01", fecha = "2026-08-26"))),
  target_id = "ACME")
card_html <- paste(as.character(.audit_pdf_entry_card(row_pago)), collapse = "\n")
.chk_true(grepl("larivera", card_html, fixed = TRUE), "entry card: user shown in header line")
.chk_true(grepl("confirmar_pago", card_html, fixed = TRUE), "entry card: action shown in header line")
.chk_true(grepl("1 pago\\(s\\) confirmado", card_html), "entry card: description text shown")
.chk_true(grepl("ACME", card_html, fixed = TRUE), "entry card: target_id shown")
.chk_true(grepl("PROVEEDOR X", card_html, fixed = TRUE),
     "entry card: Stage 2's payment detail renderer is reused (partner name present)")

row_generic <- .mk_row("bancos", "importar_movimientos", "12 movimientos", list(n = 12L))
card_generic <- paste(as.character(.audit_pdf_entry_card(row_generic)), collapse = "\n")
.chk_true(grepl("12", card_generic, fixed = TRUE), "entry card: generic fallback detail rendered for an unmodeled action")

row_no_target <- .mk_row("bancos", "x", "y", list(), target_id = NA_character_)
card_no_target <- paste(as.character(.audit_pdf_entry_card(row_no_target)), collapse = "\n")
.chk_false(grepl("ID destino", card_no_target, fixed = TRUE),
     "entry card: NA target_id omits the 'ID destino' line entirely")

# =============================================================================
# Part C -- generate_audit_log_html_doc()
# =============================================================================
cat("\n-- Part C: generate_audit_log_html_doc() --\n")

export_df <- rbind(
  .mk_row("pagar_hoy", "confirmar_pago", "1 pago(s) confirmado(s)",
    list(empresa = "ACME", moneda = "MXN", cuenta_id = "CTA-01",
         pagos = list(list(parte = "PROVEEDOR UNO", documento = "FAC-1", importe = 500,
                           moneda = "MXN", cuenta_id = "CTA-01", fecha = "2026-08-26"))),
    target_id = "ACME", ts = "2026-08-26 10:00:00"),
  .mk_row("settings_companies", "auto_aplicar_politicas", "Auto-aplicación",
    list(n_moves = 4L, partes = c("ACME", "BETA"), policy_names = c("Fin de mes")),
    ts = "2026-08-25 09:00:00", user = "system"),
  .mk_row("auth", "login", "Inicio de sesión: bunny",
    list(user = "bunny", tier = "hopdesk", is_staff = TRUE),
    ts = "2026-08-24 08:00:00", user = "bunny"),
  .mk_row("auth", "logout", "Cierre de sesión: bunny (2h 2min activo)",
    list(user = "bunny", duration_seconds = 7325,
         login_ts = "2026-08-24 08:00:00", logout_ts = "2026-08-24 10:02:05"),
    ts = "2026-08-24 10:02:05", user = "bunny")
)

meta_info <- list(
  generated_by = "Luis Rivera", date_from = "2026-08-20 00:00:00", date_to = "2026-08-27 00:00:00",
  modulo_filter = "Todos", accion_filter = "Todos", usuario_filter = "", client_label = "networks"
)

html_path <- generate_audit_log_html_doc(export_df, meta_info)
.chk_true(file.exists(html_path), "generate_audit_log_html_doc(): returns a path to a real file")
.chk_true(file.info(html_path)$size > 0, "generated file is non-empty")

html_txt <- paste(readLines(html_path, warn = FALSE), collapse = "\n")

.chk_true(grepl("HOPDESK", html_txt, fixed = TRUE), "doc: HOPDESK wordmark present (text header, per spec)")
.chk_true(grepl("Registro Oficial de Actividad", html_txt, fixed = TRUE), "doc: official-report title present")
.chk_true(grepl("registro permanente e inmutable", html_txt, fixed = TRUE),
     "doc: authenticity/official-document footer present")
.chk_true(grepl("Luis Rivera", html_txt, fixed = TRUE), "doc: generated_by shown in metadata table")
.chk_true(grepl("networks", html_txt, fixed = TRUE), "doc: client_label shown in metadata table")

# Every módulo present becomes its own section header
.chk_true(grepl("pagar_hoy", html_txt, fixed = TRUE), "doc: pagar_hoy section present")
.chk_true(grepl("settings_companies", html_txt, fixed = TRUE), "doc: settings_companies section present")
.chk_true(grepl("auth", html_txt, fixed = TRUE), "doc: auth section present")

# Quick table AND popup-equivalent detail both present for the payment entry
.chk_true(grepl("1 pago\\(s\\) confirmado", html_txt), "doc: payment entry's description in the quick table")
.chk_true(grepl("PROVEEDOR UNO", html_txt, fixed = TRUE),
     "doc: payment entry's detail card shows the paid partner (popup-equivalent content present)")
.chk_true(grepl("FAC-1", html_txt, fixed = TRUE), "doc: payment entry's document number present")

# Policy and login/logout bespoke detail also present, not just generic
.chk_true(grepl("Fin de mes", html_txt, fixed = TRUE), "doc: policy name present (bespoke policy renderer engaged)")
.chk_true(grepl("2h 2min", html_txt, fixed = TRUE), "doc: logout duration present (bespoke logout renderer engaged)")

# Page-break CSS actually present (multi-page segmentation)
.chk_true(grepl("page-break-before", html_txt, fixed = TRUE), "doc: page-break CSS rule present for section segmentation")

# Module summary counts on the cover
.chk_true(grepl(">4<", html_txt, fixed = TRUE) || grepl("Total de entradas", html_txt, fixed = TRUE),
     "doc: total entry count appears somewhere on the cover")

unlink(html_path)

# ── Zero-row export ──────────────────────────────────────────────────────────
empty_df <- export_df[0, ]
html_path_empty <- generate_audit_log_html_doc(empty_df, meta_info)
.chk_true(file.exists(html_path_empty), "zero-row export: still produces a real file, does not error")
html_txt_empty <- paste(readLines(html_path_empty, warn = FALSE), collapse = "\n")
.chk_true(grepl("Sin entradas", html_txt_empty, fixed = TRUE),
     "zero-row export: shows an explicit 'no entries' section instead of a blank/broken page")
unlink(html_path_empty)

# ── NULL export_df doesn't error ─────────────────────────────────────────────
ok_null <- tryCatch({ generate_audit_log_html_doc(NULL, meta_info); TRUE },
                    error = function(e) { message("  NULL export_df errored: ", e$message); FALSE })
.chk_true(ok_null, "NULL export_df does not error (defensive against a filtered() error-state slipping through)")

# ── Missing meta_info fields don't error ─────────────────────────────────────
ok_missing_meta <- tryCatch({ generate_audit_log_html_doc(export_df, list()); TRUE },
                            error = function(e) { message("  empty meta_info errored: ", e$message); FALSE })
.chk_true(ok_missing_meta, "an entirely empty meta_info list does not error (every field has a sane fallback)")

cat(sprintf("\n  Stage 3 subtotal: %d passed, %d failed\n", .pass, .fail))
