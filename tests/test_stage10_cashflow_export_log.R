# =============================================================================
# tests/test_stage10_cashflow_export_log.R
#
# Follow-up (2026-08-27): Calendario's cash flow export has three formats
# (PDF, Word, Excel). Only the PDF export logged to the general Actividad
# log (module="cashflow_export") -- Word and Excel only dual-wrote to the
# separate "Ver archivos de pago" trail via log_archivo_generado(), never to
# log_action(). Fixed by adding the missing log_action() calls to both,
# reordered (along with PDF's pre-existing one) to fire BEFORE the actual
# file write, matching the "log exists before the document reaches the
# user" convention established throughout this feature. Also added a
# bespoke "cashflow_export" popup/PDF rendering kind
# (R/audit_log_viewer_module.R), basic-detail level (no beneficiary/bank
# data exists for a cash flow report the way it does for a payment file).
#
# Sourced by _run_saas.R.
# =============================================================================

cat("── Cashflow export logging (PDF/Word/Excel) ───────────────────────────────\n")

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

# =============================================================================
# Part A -- .audit_detail_kind() classification
# =============================================================================
cat("\n-- Part A: classification --\n")

.chk(.audit_detail_kind("cashflow_export", "exportar_cashflow_pdf"),   "cashflow_export", "PDF action classifies correctly")
.chk(.audit_detail_kind("cashflow_export", "exportar_cashflow_word"),  "cashflow_export", "Word action classifies correctly")
.chk(.audit_detail_kind("cashflow_export", "exportar_cashflow_excel"), "cashflow_export", "Excel action classifies correctly")
.chk(.audit_detail_kind("cashflow_export", "algo_no_relacionado"), "generic",
     "an unrelated cashflow_export action still falls to generic, not misclassified")

# =============================================================================
# Part B -- .audit_detail_cashflow_export() rendering
# =============================================================================
cat("\n-- Part B: .audit_detail_cashflow_export() --\n")

meta_pdf <- list(date_from = "2026-08-01", date_to = "2026-08-27",
                 empresas = c("NG", "NTS"), ic_mode = "exclude",
                 currency_mode = "fused", formato = "pdf")
html_pdf <- paste(as.character(.audit_detail_cashflow_export(meta_pdf)), collapse = "\n")
.chk_true(grepl("2026-08-01", html_pdf, fixed = TRUE) && grepl("2026-08-27", html_pdf, fixed = TRUE),
     "date range shown")
.chk_true(grepl("NG", html_pdf, fixed = TRUE) && grepl("NTS", html_pdf, fixed = TRUE), "empresas shown")
.chk_true(grepl("Sin transacciones entre compa", html_pdf), "ic_mode label translated (exclude)")
.chk_true(grepl("fused", html_pdf, fixed = TRUE), "currency_mode shown")
.chk_true(grepl(">PDF<", html_pdf) || grepl("PDF", html_pdf, fixed = TRUE), "formato label translated (pdf -> PDF)")

meta_word <- list(date_from = "2026-08-01", date_to = "2026-08-27", empresas = "NG",
                  ic_mode = "include", formato = "word")
html_word <- paste(as.character(.audit_detail_cashflow_export(meta_word)), collapse = "\n")
.chk_true(grepl("Con transacciones entre compa", html_word), "ic_mode label translated (include)")
.chk_true(grepl("Word", html_word, fixed = TRUE), "formato label translated (word -> Word)")

meta_excel <- list(ic_mode = "only", formato = "excel")
html_excel <- paste(as.character(.audit_detail_cashflow_export(meta_excel)), collapse = "\n")
.chk_true(grepl("S.lo transacciones entre compa", html_excel), "ic_mode label translated (only)")
.chk_true(grepl("Excel", html_excel, fixed = TRUE), "formato label translated (excel -> Excel)")

meta_unknown <- list(ic_mode = "unrecognized_value", formato = "unrecognized_format")
html_unknown <- paste(as.character(.audit_detail_cashflow_export(meta_unknown)), collapse = "\n")
.chk_true(grepl("unrecognized_value", html_unknown, fixed = TRUE),
     "unknown ic_mode falls back to showing the raw value, not blank/NA")
.chk_true(grepl("unrecognized_format", html_unknown, fixed = TRUE),
     "unknown formato falls back to showing the raw value, not blank/NA")

meta_empty <- list()
ok_empty <- tryCatch({ .audit_detail_cashflow_export(meta_empty); TRUE }, error = function(e) FALSE)
.chk_true(ok_empty, "entirely empty metadata does not error")

# =============================================================================
# Part C -- end-to-end: log_action() metadata -> popup -> PDF
# =============================================================================
cat("\n-- Part C: end-to-end --\n")

Sys.setenv(CLIENT_ID = "stage10test")
mock_s3saveRDS(.APP_AUDIT_SCHEMA(), "stage10test/app_audit.rds", "mock-bucket")

for (fmt in list(list(action = "exportar_cashflow_pdf",   formato = "pdf"),
                list(action = "exportar_cashflow_word",  formato = "word"),
                list(action = "exportar_cashflow_excel", formato = "excel"))) {
  log_action(
    "larivera", "cashflow_export", fmt$action,
    sprintf("Reporte de flujo de efectivo exportado (%s)", fmt$formato),
    target_id = "NG",
    metadata = list(date_from = "2026-08-01", date_to = "2026-08-27",
                    empresas = c("NG", "NTS"), ic_mode = "exclude",
                    currency_mode = "base", formato = fmt$formato),
    client_id = "stage10test", viewer_home_client_id = "stage10test"
  )
}
.audit_flush_pending("stage10test")
log_df <- mock_s3readRDS("stage10test/app_audit.rds", "mock-bucket")
.chk(nrow(log_df), 3L, "all three formats (PDF/Word/Excel) each write one app_audit row")
.chk_true(all(c("exportar_cashflow_pdf", "exportar_cashflow_word", "exportar_cashflow_excel") %in% log_df$action),
     "all three distinct actions present")

row_word <- data.frame(
  id = "r1", ts = as.POSIXct("2026-08-27 09:00:00", tz = "UTC"), user = "larivera",
  module = "cashflow_export", action = "exportar_cashflow_word",
  description = "Reporte de flujo de efectivo exportado (word)",
  target_id = "NG", s3_key = NA_character_, client_id = "stage10test",
  metadata = as.character(jsonlite::toJSON(meta_word, auto_unbox = TRUE, na = "null")),
  stringsAsFactors = FALSE
)
html_modal <- paste(as.character(.audit_detail_modal_body(row_word, "own_client")), collapse = "\n")
.chk_true(grepl("Word", html_modal, fixed = TRUE), "end-to-end popup: bespoke cashflow_export renderer engaged")

html_path <- generate_audit_log_html_doc(row_word, list(generated_by = "Luis Rivera", client_label = "stage10test"))
pdf_txt <- paste(readLines(html_path, warn = FALSE), collapse = "\n")
.chk_true(grepl("Word", pdf_txt, fixed = TRUE), "end-to-end PDF: cashflow export detail present in the exported document")
unlink(html_path)

# =============================================================================
# Part D -- static regression guards
# =============================================================================
cat("\n-- Part D: static regression guards --\n")

cf_src <- readLines("R/cashflow_export_module.R", warn = FALSE)

# Both Word and Excel now have a log_action call site (PDF already did).
n_log_action_sites <- length(grep("log_action\\(", cf_src)) - length(grep("^\\s*#", cf_src[grep("log_action\\(", cf_src)]))
.chk_true(length(grep('"exportar_cashflow_word"', cf_src)) >= 1L, "exportar_cashflow_word action string present")
.chk_true(length(grep('"exportar_cashflow_excel"', cf_src)) >= 1L, "exportar_cashflow_excel action string present")
.chk_true(length(grep('"exportar_cashflow_pdf"', cf_src)) >= 1L, "exportar_cashflow_pdf action string still present")

# Ordering: for each format, the log_action call must appear BEFORE the
# actual file-write call in the source (print()/file.copy()/chrome_print()).
word_log_line   <- grep('"exportar_cashflow_word"', cf_src)[1]
word_write_line <- grep("print\\(doc, target = file\\)", cf_src)[1]
.chk_true(word_log_line < word_write_line,
     "Word: log_action call appears before print(doc, target=file) -- logged before the document is written")

excel_log_line   <- grep('"exportar_cashflow_excel"', cf_src)[1]
excel_write_line <- grep("file\\.copy\\(src, file, overwrite = TRUE\\)", cf_src)[1]
.chk_true(excel_log_line < excel_write_line,
     "Excel: log_action call appears before file.copy() -- logged before the document is written")

pdf_log_line   <- grep('"exportar_cashflow_pdf"', cf_src)[1]
pdf_write_line <- grep("pagedown::chrome_print\\(", cf_src)[1]
.chk_true(pdf_log_line < pdf_write_line,
     "PDF: log_action call appears before pagedown::chrome_print() -- reordered to log before the document is written")

# =============================================================================
# Part E -- follow-up (2026-08-27): description format consistency across
# PDF/Word/Excel, and Excel using company INITIALS (not full names)
# =============================================================================
cat("\n-- Part E: description format + initials consistency --\n")

.chk_true(any(grepl('"Reporte de flujo de efectivo exportado \\(PDF\\)', cf_src)),
     "PDF description explicitly states its format, matching Word's/Excel's own style")
.chk_true(any(grepl('"Reporte de flujo de efectivo exportado \\(Word\\)', cf_src)),
     "Word description explicitly states its format")
.chk_true(any(grepl('Reporte de flujo de efectivo exportado \\(Excel\\)', cf_src)),
     "Excel description explicitly states its format")

# Excel's log_action call must use params$initials, never params$empresa
# (full company names) -- the actual "slightly inconsistent information"
# bug between formats.
excel_block_start <- grep('action\\s*=\\s*"exportar_cashflow_excel"', cf_src)[1]
excel_block <- paste(cf_src[excel_block_start:min(excel_block_start + 20, length(cf_src))], collapse = "\n")
.chk_true(grepl("params\\$initials", excel_block),
     "Excel's log_action call references params$initials")
.chk_false(grepl("empresas\\s*=\\s*params\\$empresa\\b", excel_block),
     "Excel's log_action call no longer uses params$empresa (full names) for the empresas field")

# .xl_log_params() must actually store initials for the above to have real data.
.chk_true(any(grepl("\\.xl_log_params\\(list\\(empresa = paste\\(emp_sel, collapse = \", \"\\), initials = initials", cf_src)),
     ".xl_log_params() now stores `initials` alongside the full-name `empresa` string")

cat(sprintf("\n  Stage 10 subtotal: %d passed, %d failed\n", .pass, .fail))
