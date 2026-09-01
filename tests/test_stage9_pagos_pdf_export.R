# =============================================================================
# tests/test_stage9_pagos_pdf_export.R
#
# Follow-up (2026-08-27) to Actividad's PDF export: "Ver archivos de pago"
# gets the same "Exportar PDF" button, reusing R/audit_log_viewer_module.R's
# .AUDIT_PDF_CSS directly ("I like the formatting, fonts, colors"),
# segmented per Tipo, with FULL (unmasked) detail per entry -- consistent
# with this table's own existing single-row "Ver detalle" + receipt
# download, which already shows unmasked CLABE to a viewer already
# permissioned for this separately-gated audit trail.
#
# Sourced by _run_saas.R. Sources R/pagos_generados_persistence.R and
# R/pagos_audit_viewer_module.R itself (module-scope helpers only; the
# server body never executes), matching tests/test_payment_audit_stage.R's
# established pattern. R/audit_log_viewer_module.R (source of .AUDIT_PDF_CSS)
# is already in _run_saas.R's default source list.
# =============================================================================

cat("── \"Ver archivos de pago\" PDF export ─────────────────────────────────────\n")

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

source("R/pagos_generados_persistence.R", local = FALSE)
source("R/pagos_audit_viewer_module.R",   local = FALSE)

.mk_row <- function(tipo, empresa = "ACME", moneda = "MXN", cuenta_origen = "0123456789",
                    banco_origen = "BBVA", n_pagos = 1L, total = 100, archivo = "x.txt",
                    beneficiarios = NA_character_, detalle = NA_character_,
                    source_val = "individual", ts = "2026-08-27 10:00:00", user = "larivera") {
  tibble::tibble(
    id = uuid::UUIDgenerate(), ts = as.POSIXct(ts, tz = "UTC"), user = user,
    client_id = "stage9test", empresa = empresa, moneda = moneda,
    cuenta_origen = cuenta_origen, banco_origen = banco_origen,
    n_pagos = n_pagos, total = total, beneficiarios = beneficiarios,
    source = source_val, archivo = archivo, tipo = tipo, detalle = detalle
  )
}

ben_json <- as.character(jsonlite::toJSON(list(
  list(alias = "PROVEEDOR UNO", clabe = "012345678901234567", banco = "021",
      importe = 100, concepto = "FAC-1")
), auto_unbox = TRUE))

# =============================================================================
# Part A -- .pagos_tipo_summary()
# =============================================================================
cat("\n-- Part A: .pagos_tipo_summary() --\n")

.chk(nrow(.pagos_tipo_summary(NULL)), 0L, "NULL df: zero-row summary")

df_a <- rbind(
  .mk_row("pago_ppl", beneficiarios = ben_json),
  .mk_row("pago_ppl", beneficiarios = ben_json),
  .mk_row("reporte_excel")
)
summ_a <- .pagos_tipo_summary(df_a)
.chk(nrow(summ_a), 2L, "3 rows across 2 tipos: summary has 2 rows")
.chk_true(all(c("Pago", "Reporte Excel") %in% summ_a$tipo), "tipo labels resolved via .tipo_label()")
.chk(summ_a$n[summ_a$tipo == "Pago"], 2L, "Pago count is 2")

df_na_tipo <- .mk_row(NA_character_)
.chk(.pagos_tipo_summary(df_na_tipo)$tipo[1], "(sin tipo)", "NA tipo grouped under '(sin tipo)', not dropped")

# =============================================================================
# Part B -- .pagos_pdf_entry_block()
# =============================================================================
cat("\n-- Part B: .pagos_pdf_entry_block() --\n")

row_pago <- .mk_row("pago_ppl", empresa = "ACME", cuenta_origen = "0123456789",
                    beneficiarios = ben_json, total = 100, n_pagos = 1L, archivo = "pagos_ACME.txt")
html_pago <- paste(as.character(.pagos_pdf_entry_block(row_pago)), collapse = "\n")
.chk_true(grepl("ACME", html_pago, fixed = TRUE), "pago block: empresa shown")
.chk_true(grepl("0123456789", html_pago, fixed = TRUE), "pago block: cuenta_origen shown")
.chk_true(grepl("PROVEEDOR UNO", html_pago, fixed = TRUE), "pago block: beneficiary shown")
.chk_true(grepl("012345678901234567", html_pago, fixed = TRUE),
     "pago block: FULL (unmasked) CLABE shown -- consistent with this table's existing single-receipt policy")
.chk_true(grepl("pagos_ACME.txt", html_pago, fixed = TRUE), "pago block: archivo filename shown")

row_pago_empty <- .mk_row("pago_ppl", beneficiarios = NA_character_)
html_pago_empty <- paste(as.character(.pagos_pdf_entry_block(row_pago_empty)), collapse = "\n")
.chk_true(grepl("Sin beneficiarios", html_pago_empty, fixed = TRUE),
     "pago block: no beneficiarios shows the 'no data' message, not a broken table")

det_json <- as.character(jsonlite::toJSON(list(rango = "2026-08", empresas = c("ACME", "BETA")), auto_unbox = TRUE))
row_reporte <- .mk_row("reporte_excel", detalle = det_json, total = NA_real_)
html_reporte <- paste(as.character(.pagos_pdf_entry_block(row_reporte)), collapse = "\n")
.chk_true(grepl("rango", html_reporte, fixed = TRUE), "reporte block: generic detalle key shown")
.chk_true(grepl("ACME", html_reporte, fixed = TRUE) && grepl("BETA", html_reporte, fixed = TRUE),
     "reporte block: generic detalle vector value collapsed and shown")
.chk_false(grepl("Cuenta origen", html_reporte, fixed = TRUE),
     "reporte block: non-payment tipo does not show payment-only fields (Cuenta origen/Origen)")

# =============================================================================
# Part C -- generate_pagos_audit_html_doc()
# =============================================================================
cat("\n-- Part C: generate_pagos_audit_html_doc() --\n")

export_df <- rbind(
  .mk_row("pago_ppl", empresa = "ACME", cuenta_origen = "0123456789", banco_origen = "BBVA",
         beneficiarios = ben_json, total = 100, n_pagos = 1L, archivo = "pagos_ACME.txt",
         ts = "2026-08-27 10:00:00"),
  .mk_row("reporte_excel", empresa = "BETA", detalle = det_json, total = NA_real_,
         archivo = "reporte.xlsx", ts = "2026-08-26 09:00:00", user = "system")
)
meta_info <- list(generated_by = "Luis Rivera", date_from = "2026-08-20 00:00:00",
                  date_to = "2026-08-27 00:00:00", client_label = "stage9test",
                  empresa_filter = "Todas", usuario_filter = "")

html_path <- generate_pagos_audit_html_doc(export_df, meta_info)
.chk_true(file.exists(html_path), "returns a path to a real file")
.chk_true(file.info(html_path)$size > 0, "generated file is non-empty")

html_txt <- paste(readLines(html_path, warn = FALSE), collapse = "\n")

.chk_true(grepl("HOPDESK", html_txt, fixed = TRUE), "doc: HOPDESK wordmark present -- same header as Actividad's PDF")
.chk_true(grepl("Registro Oficial de Archivos de Pago", html_txt, fixed = TRUE), "doc: distinct title from Actividad's own PDF")
.chk_true(grepl("registro permanente e inmutable", html_txt, fixed = TRUE), "doc: authenticity footer present")
.chk_true(grepl("Luis Rivera", html_txt, fixed = TRUE), "doc: generated_by shown")
.chk_true(grepl("stage9test", html_txt, fixed = TRUE), "doc: client_label shown")

.chk_true(grepl("Pago", html_txt, fixed = TRUE) && grepl("Reporte Excel", html_txt, fixed = TRUE),
     "doc: both Tipo sections present")
.chk_true(grepl("PROVEEDOR UNO", html_txt, fixed = TRUE), "doc: payment entry's beneficiary detail present")
.chk_true(grepl("012345678901234567", html_txt, fixed = TRUE),
     "doc: full unmasked CLABE present in the export -- 'entirety of details' honored")
.chk_true(grepl("reporte.xlsx", html_txt, fixed = TRUE), "doc: non-payment entry's filename present")
.chk_true(grepl("page-break-before", html_txt, fixed = TRUE), "doc: page-break CSS present for multi-page segmentation")

# Reuses the exact same CSS constant as Actividad's PDF -- verifies the
# "formatting, fonts, colors" reuse is real, not merely visually similar.
.chk_true(grepl(".sec-hdr", html_txt, fixed = TRUE) && grepl(".cover", html_txt, fixed = TRUE),
     "doc: shares Actividad's .AUDIT_PDF_CSS class names (cover, sec-hdr, ...)")

unlink(html_path)

# ── Zero-row / NULL export ───────────────────────────────────────────────────
empty_df <- export_df[0, ]
html_path_empty <- generate_pagos_audit_html_doc(empty_df, meta_info)
.chk_true(file.exists(html_path_empty), "zero-row export: still produces a real file")
html_txt_empty <- paste(readLines(html_path_empty, warn = FALSE), collapse = "\n")
.chk_true(grepl("Sin entradas", html_txt_empty, fixed = TRUE), "zero-row export: shows an explicit 'no entries' section")
unlink(html_path_empty)

ok_null <- tryCatch({ generate_pagos_audit_html_doc(NULL, meta_info); TRUE },
                    error = function(e) { message("  NULL export_df errored: ", e$message); FALSE })
.chk_true(ok_null, "NULL export_df does not error")

ok_missing_meta <- tryCatch({ generate_pagos_audit_html_doc(export_df, list()); TRUE },
                            error = function(e) { message("  empty meta_info errored: ", e$message); FALSE })
.chk_true(ok_missing_meta, "an entirely empty meta_info list does not error")

# =============================================================================
# Part D -- static wiring + regression guard
# =============================================================================
cat("\n-- Part D: static wiring + regression guard --\n")

src_txt <- paste(readLines("R/pagos_audit_viewer_module.R", warn = FALSE), collapse = "\n")
.chk_true(grepl("downloadButton\\(ns\\(\"pagos_export_pdf\"\\)", src_txt),
     "the Exportar PDF button is wired into the UI")
.chk_true(grepl("output\\$pagos_export_pdf <- downloadHandler", src_txt),
     "the downloadHandler is defined")
.chk_true(grepl("\\.AUDIT_PDF_CSS", src_txt),
     "reuses R/audit_log_viewer_module.R's .AUDIT_PDF_CSS rather than a duplicate stylesheet")

src_lines <- readLines("R/pagos_audit_viewer_module.R", warn = FALSE)
zip_log_line <- grep('"exportar_pagos_pdf"', src_lines)
.chk_true(length(zip_log_line) >= 1L, "sanity: the new action string appears in the file")
window <- paste(src_lines[zip_log_line[1]:min(zip_log_line[1] + 10, length(src_lines))], collapse = "\n")
.chk_true(grepl("client_id\\s*=", window), "exportar_pagos_pdf call site passes client_id=")
.chk_true(grepl("viewer_home_client_id\\s*=", window), "exportar_pagos_pdf call site passes viewer_home_client_id=")

cat(sprintf("\n  Stage 9 subtotal: %d passed, %d failed\n", .pass, .fail))
