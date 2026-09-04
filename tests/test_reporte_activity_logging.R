# =============================================================================
# tests/test_reporte_activity_logging.R
#
# Reporte activity logging (2026-09-04). Two things this stage adds:
#   1. Filter/settings changes (btn_apply) now log module="reporte",
#      action="aplicar_filtros" with a before/after diff.
#   2. Both exports (HTML, PDF, and the two "actually delivered HTML
#      instead" fallback paths -- no pagedown installed, or chrome_print
#      errored) now log module="reporte", action="exportar_html"/
#      "exportar_pdf" via log_action() directly, replacing the previous
#      log_archivo_generado() calls that miscategorized these as payment
#      files (pagos_generados.rds, gated by can_view_payment_audit) instead
#      of general Actividad entries.
#
# Sourced by _run_saas.R (already has shiny loaded, log_action mocked, and
# R/audit_log_viewer_module.R sourced -- same harness Bancos/Pasivos use).
# =============================================================================

cat("── Reporte activity logging ─────────────────────────────────────────────\n")

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

source("R/audit_log_viewer_module.R", local = FALSE)

# =============================================================================
# Part A -- static scan of R/reporte_module.R
# =============================================================================
cat("\n-- Part A: static scan --\n")

rep_txt <- paste(readLines("R/reporte_module.R", warn = FALSE), collapse = "\n")
.chk_true({p <- tryCatch({parse("R/reporte_module.R"); TRUE}, error = function(e) FALSE); p},
          "R/reporte_module.R still parses without syntax errors")

.count_matches <- function(pattern, text, fixed = TRUE)
  length(regmatches(text, gregexpr(pattern, text, fixed = fixed))[[1]])

.chk(.count_matches("log_archivo_generado(", rep_txt), 0L,
     "log_archivo_generado() is no longer CALLED anywhere in reporte_module.R (the name may still appear in an explanatory comment)")
.chk_true(grepl("\\.log_reporte_export\\s*<-\\s*function", rep_txt),
          ".log_reporte_export() helper is defined")
.chk_true(grepl('action\\s*=\\s*"aplicar_filtros"', rep_txt),
          "btn_apply logs action=\"aplicar_filtros\"")
.chk_true(grepl('module\\s*=\\s*"reporte"', rep_txt),
          "the filter-apply log_action() call uses module=\"reporte\"")

# All 4 places a file can actually be delivered to the user must call the
# export logger -- html success, pdf success, pdf-fallback (no pagedown),
# pdf-fallback (chrome_print error). Missing any one of these means a real
# download happens with zero audit trail.
n_export_calls <- .count_matches(".log_reporte_export(", rep_txt)
.chk(n_export_calls, 4L,
     ".log_reporte_export() is called at all 4 file-delivery points (html, pdf, pdf->html no-pagedown fallback, pdf->html error fallback)")
# Extract each of the 4 individual .log_reporte_export(...) call blocks
# (a few lines each) rather than a whole-file regex -- a greedy match
# across the whole file could pair one call's formato= with a DIFFERENT
# call's nota= text and report a false pass.
rep_lines  <- readLines("R/reporte_module.R", warn = FALSE)
call_starts <- grep("\\.log_reporte_export\\(", rep_lines)
.chk(length(call_starts), 4L, "found all 4 .log_reporte_export() call sites to scan individually")
call_blocks <- lapply(call_starts, function(s) paste(rep_lines[s:(s + 4)], collapse = "\n"))

html_call     <- call_blocks[[1]]  # dl_html handler
no_pagedown   <- call_blocks[[2]]  # pdf handler, pagedown not installed
pdf_success   <- call_blocks[[3]]  # pdf handler, chrome_print succeeded
pdf_error     <- call_blocks[[4]]  # pdf handler, chrome_print errored

.chk_true(grepl('formato\\s*=\\s*"html"', html_call), "dl_html's own call logs formato=\"html\"")
.chk_true(grepl('formato\\s*=\\s*"html"', no_pagedown) && grepl("pagedown no instalado", no_pagedown),
          "the no-pagedown fallback logs formato=\"html\" WITH a note explaining why (not silently mislabeled as pdf)")
.chk_true(grepl('formato\\s*=\\s*"pdf"', pdf_success),
          "the chrome_print success path logs formato=\"pdf\"")
.chk_true(grepl('formato\\s*=\\s*"html"', pdf_error) && grepl("error al generar PDF", pdf_error),
          "the chrome_print-error fallback logs formato=\"html\" WITH a note explaining why")

# =============================================================================
# Part B -- .audit_detail_kind() classification
# =============================================================================
cat("\n-- Part B: .audit_detail_kind() --\n")

.chk(.audit_detail_kind("reporte", "aplicar_filtros"), "reporte_filtros",
     "reporte/aplicar_filtros -> reporte_filtros (reuses the generic diff renderer)")
.chk(.audit_detail_kind("reporte", "exportar_html"), "reporte_export",
     "reporte/exportar_html -> reporte_export")
.chk(.audit_detail_kind("reporte", "exportar_pdf"), "reporte_export",
     "reporte/exportar_pdf -> reporte_export")
.chk(.audit_detail_kind("reporte", "algo_no_modelado"), "generic",
     "an unrecognized reporte action still falls through to generic (no silent crash)")

# =============================================================================
# Part C -- .audit_detail_reporte_export() rendering
# =============================================================================
cat("\n-- Part C: .audit_detail_reporte_export() --\n")

meta_pdf <- list(formato = "pdf", desde = "2026-08-01", hasta = "2026-08-31",
                 empresas = c("NCS", "NTS"), n_cuentas = 5L, total_saldo = 123456.789,
                 nota = NA_character_)
html_pdf <- .html(.audit_detail_reporte_export(meta_pdf))
.chk_true(grepl("PDF", html_pdf, fixed = TRUE), "formato renders uppercased")
.chk_true(grepl("2026-08-01", html_pdf, fixed = TRUE) && grepl("2026-08-31", html_pdf, fixed = TRUE),
          "date range renders")
.chk_true(grepl("NCS, NTS", html_pdf, fixed = TRUE), "empresas list renders comma-joined")
.chk_true(grepl("123,456.79", html_pdf, fixed = TRUE),
          "total_saldo renders with thousands separator and 2 decimals")
.chk_false(grepl("pagedown", html_pdf, fixed = TRUE),
           "no fallback note rendered when nota is NA (clean success case, no clutter)")

meta_fallback <- list(formato = "html", desde = "2026-08-01", hasta = "2026-08-31",
                      empresas = "NCS", n_cuentas = 1L, total_saldo = 0,
                      nota = "pagedown no instalado; entregado como HTML")
html_fallback <- .html(.audit_detail_reporte_export(meta_fallback))
.chk_true(grepl("pagedown no instalado", html_fallback, fixed = TRUE),
          "the fallback note DOES render when present -- the audit trail must explain a pdf request that actually delivered html")

# =============================================================================
# Part D -- end-to-end via .audit_detail_modal_body() (real JSON round trip)
# =============================================================================
cat("\n-- Part D: end-to-end dispatch --\n")

.mk_row <- function(module, action, description, metadata_list) {
  tibble::tibble(
    id = "x", ts = Sys.time(), user = "tester", module = module, action = action,
    description = description, target_id = NA_character_, s3_key = NA_character_,
    client_id = "networks",
    metadata = as.character(jsonlite::toJSON(metadata_list, auto_unbox = TRUE, na = "null"))
  )
}

row_export <- .mk_row("reporte", "exportar_pdf", "Reporte financiero exportado (PDF)", meta_pdf)
html_e2e_export <- .html(.audit_detail_modal_body(row_export, "own_client"))
.chk_true(grepl("NCS, NTS", html_e2e_export, fixed = TRUE),
          "end-to-end export: real metadata reaches the popup via the bespoke renderer")

row_filtros <- .mk_row("reporte", "aplicar_filtros", "Filtros de reporte aplicados",
  list(before = list(desde = "2026-07-01", hasta = "2026-07-31", empresas = c("NCS")),
       after  = list(desde = "2026-08-01", hasta = "2026-08-31", empresas = c("NCS", "NTS"))))
html_e2e_filtros <- .html(.audit_detail_modal_body(row_filtros, "own_client"))
.chk_true(grepl("2026-07-01", html_e2e_filtros, fixed = TRUE) && grepl("2026-08-01", html_e2e_filtros, fixed = TRUE),
          "end-to-end filter change: before/after date values both reach the popup via the reused generic diff renderer")

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
