# =============================================================================
# tests/test_activity_stage6_zip_bulk_aggregate_log.R
#
# Follow-up (2026-08-27) to Stage 4's payment-file logging: the per-file
# .log_pago_generado() calls in the "Descargar todos" (.zip) download
# handler were already working (confirmed by Stage 4's own tests), but read
# from Actividad as N indistinguishable entries, no different from N
# separate single-company clicks -- nothing tied them together as one bulk
# action. Added .zip_pago_aggregate_metadata() (R/pagar_hoy_module.R) plus a
# new "generar_archivo_pago_zip" log_action() call logging ONE aggregate
# entry per bulk click, with full per-file beneficiary detail (same
# information density as the single-file case, per the explicit ask), and
# a matching "payment_file_zip" popup/PDF renderer
# (R/audit_log_viewer_module.R).
#
# Sourced by _run_saas.R.
# =============================================================================

cat("── Activity audit: ZIP/bulk aggregate payment-file log ────────────────────\n")

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

source("R/pagos_generados_persistence.R", local = FALSE)  # mask_clabe()

# =============================================================================
# Part A -- .zip_pago_aggregate_metadata()
# =============================================================================
cat("\n-- Part A: .zip_pago_aggregate_metadata() --\n")

empty_agg <- .zip_pago_aggregate_metadata(list())
.chk(empty_agg$n_archivos, 0L, "empty input: n_archivos = 0")
.chk(empty_agg$empresas, character(0), "empty input: empresas is an empty character vector")
.chk(empty_agg$total_general, 0, "empty input: total_general = 0")

one_file <- list(list(empresa = "ACME", moneda = "MXN", cuenta_origen = "0123456789",
                      banco_origen = "BBVA", n_pagos = 3L, total = 1500.75,
                      archivo = "x.txt", beneficiarios = list()))
agg1 <- .zip_pago_aggregate_metadata(one_file)
.chk(agg1$n_archivos, 1L, "single file: n_archivos = 1")
.chk(agg1$empresas, "ACME", "single file: empresas = 'ACME'")
.chk(agg1$n_pagos_total, 3L, "single file: n_pagos_total = 3")
.chk(agg1$total_general, 1500.75, "single file: total_general matches the one file's total")

multi <- list(
  list(empresa = "ACME", moneda = "MXN", n_pagos = 3L, total = 1000),
  list(empresa = "ACME", moneda = "USD", n_pagos = 2L, total = 500),
  list(empresa = "BETA", moneda = "MXN", n_pagos = 5L, total = 2500)
)
agg2 <- .zip_pago_aggregate_metadata(multi)
.chk(agg2$n_archivos, 3L, "3 files across 2 empresas x currencies: n_archivos = 3")
.chk(sort(agg2$empresas), c("ACME", "BETA"), "empresas de-duplicated: ACME appears once despite 2 files")
.chk(agg2$n_pagos_total, 10L, "n_pagos_total sums correctly across all files (3+2+5)")
.chk(agg2$total_general, 4000, "total_general sums correctly across all files (1000+500+2500)")
.chk(length(agg2$archivos), 3L, "archivos array preserves all 3 per-file summaries")

missing_fields <- list(list(empresa = "X"))  # no moneda/n_pagos/total at all
ok_missing <- tryCatch({ .zip_pago_aggregate_metadata(missing_fields); TRUE },
                       error = function(e) FALSE)
.chk_true(ok_missing, "a summary missing n_pagos/total fields does not error (falls back to 0)")

# =============================================================================
# Part B -- .audit_detail_kind() classification
# =============================================================================
cat("\n-- Part B: classification --\n")

.chk(.audit_detail_kind("pagar_hoy", "generar_archivo_pago_zip"), "payment_file_zip",
     "generar_archivo_pago_zip classifies as payment_file_zip")
.chk_false(identical(.audit_detail_kind("pagar_hoy", "generar_archivo_pago_zip"), "payment_file"),
     "payment_file_zip is distinct from the single-file payment_file kind")

# =============================================================================
# Part C -- .audit_detail_payment_file_zip() rendering
# =============================================================================
cat("\n-- Part C: .audit_detail_payment_file_zip() --\n")

meta_zip <- list(
  n_archivos = 2L, empresas = c("ACME", "BETA"), n_pagos_total = 5L, total_general = 3500.50,
  archivos = list(
    list(empresa = "ACME", moneda = "MXN", cuenta_origen = "0123456789", banco_origen = "BBVA",
        n_pagos = 2L, total = 1000.50, archivo = "pagos_ACME_MXN.txt",
        beneficiarios = list(list(alias = "PROVEEDOR UNO", clabe = "012345678901234567",
                                  banco = "021", importe = 500.25, concepto = "FAC-1"))),
    list(empresa = "BETA", moneda = "USD", cuenta_origen = "9988776655", banco_origen = "Santander",
        n_pagos = 3L, total = 2500.00, archivo = "pagos_BETA_USD.txt",
        beneficiarios = list(list(alias = "PROVEEDOR DOS", clabe = "098765432109876543",
                                  banco = "072", importe = 2500.00, concepto = "FAC-2")))
  )
)
html_zip <- paste(as.character(.audit_detail_payment_file_zip(meta_zip)), collapse = "\n")

.chk_true(grepl("ACME", html_zip, fixed = TRUE) && grepl("BETA", html_zip, fixed = TRUE),
     "zip detail: both empresas shown in the summary header")
.chk_true(grepl("3,500.50", html_zip, fixed = TRUE), "zip detail: total_general formatted with thousands separator")
.chk_true(grepl("PROVEEDOR UNO", html_zip, fixed = TRUE) && grepl("PROVEEDOR DOS", html_zip, fixed = TRUE),
     "zip detail: BOTH files' beneficiaries are shown (same detail density as single-file view)")
.chk_true(grepl("BBVA", html_zip, fixed = TRUE) && grepl("Santander", html_zip, fixed = TRUE),
     "zip detail: each file's own banco_origen is shown, not just one shared value")
.chk_true(grepl("pagos_ACME_MXN.txt", html_zip, fixed = TRUE) && grepl("pagos_BETA_USD.txt", html_zip, fixed = TRUE),
     "zip detail: each bundled file's own filename is shown")
.chk_false(grepl("012345678901234567", html_zip, fixed = TRUE),
     "zip detail: full unmasked CLABE never leaks for either file")
.chk_false(grepl("098765432109876543", html_zip, fixed = TRUE),
     "zip detail: full unmasked CLABE never leaks for the second file either")
.chk_true(grepl(mask_clabe("012345678901234567"), html_zip, fixed = TRUE),
     "zip detail: CLABE is masked using the same convention as the single-file view")

html_zip_empty <- paste(as.character(.audit_detail_payment_file_zip(
  list(n_archivos = 0L, empresas = character(0), n_pagos_total = 0L, total_general = 0, archivos = list())
)), collapse = "\n")
.chk_true(grepl("Sin archivos", html_zip_empty, fixed = TRUE),
     "zip detail: empty archivos list shows a 'no files' message, not a broken render")

# =============================================================================
# Part D -- end-to-end: log_action() metadata -> popup -> PDF
# =============================================================================
cat("\n-- Part D: end-to-end (log_action metadata round-trip -> popup -> PDF) --\n")

Sys.setenv(CLIENT_ID = "stage6test")
mock_s3saveRDS(.APP_AUDIT_SCHEMA(), "stage6test/app_audit.rds", "mock-bucket")

agg_real <- .zip_pago_aggregate_metadata(list(
  list(empresa = "ACME", moneda = "MXN", cuenta_origen = "0123456789", banco_origen = "BBVA",
      n_pagos = 1L, total = 750.00, archivo = "a.txt",
      beneficiarios = list(list(alias = "SOLO PROVEEDOR", clabe = "011122223333444455",
                                banco = "002", importe = 750.00, concepto = "FAC-9")))
))
log_action(
  "larivera", "pagar_hoy", "generar_archivo_pago_zip",
  "Descarga masiva de archivos de pago: 1 archivo(s), 1 empresa(s), 1 pago(s) en total",
  target_id = "ACME", metadata = agg_real,
  client_id = "stage6test", viewer_home_client_id = "stage6test"
)
.audit_flush_pending("stage6test")
log_df <- mock_s3readRDS("stage6test/app_audit.rds", "mock-bucket")
.chk(nrow(log_df), 1L, "end-to-end: aggregate zip entry written to app_audit")
.chk(log_df$action[1], "generar_archivo_pago_zip", "end-to-end: action recorded correctly")

row_zip <- data.frame(
  id = "r1", ts = as.POSIXct("2026-08-27 09:00:00", tz = "UTC"), user = "larivera",
  module = "pagar_hoy", action = "generar_archivo_pago_zip",
  description = log_df$description[1], target_id = "ACME", s3_key = NA_character_,
  client_id = "stage6test", metadata = log_df$metadata[1],
  stringsAsFactors = FALSE
)
html_modal <- paste(as.character(.audit_detail_modal_body(row_zip, "own_client")), collapse = "\n")
.chk_true(grepl("SOLO PROVEEDOR", html_modal, fixed = TRUE),
     "end-to-end popup: beneficiary detail reachable from a raw audit row read back from storage")
.chk_false(grepl("011122223333444455", html_modal, fixed = TRUE),
     "end-to-end popup: full CLABE still never leaks after a real JSON round-trip")

html_path <- generate_audit_log_html_doc(row_zip, list(generated_by = "Luis Rivera", client_label = "stage6test"))
pdf_txt <- paste(readLines(html_path, warn = FALSE), collapse = "\n")
.chk_true(grepl("SOLO PROVEEDOR", pdf_txt, fixed = TRUE),
     "end-to-end PDF: the aggregate zip entry's beneficiary detail appears in the exported document")
unlink(html_path)

# =============================================================================
# Part E -- static regression guard: the new log_action call site is scoped
# =============================================================================
cat("\n-- Part E: static regression guard --\n")

ph_src <- readLines("R/pagar_hoy_module.R", warn = FALSE)
zip_log_line <- grep('"generar_archivo_pago_zip"', ph_src)
.chk_true(length(zip_log_line) >= 1L, "sanity: the new action string appears in pagar_hoy_module.R")
window <- paste(ph_src[zip_log_line[1]:min(zip_log_line[1] + 10, length(ph_src))], collapse = "\n")
.chk_true(grepl("client_id\\s*=", window), "generar_archivo_pago_zip call site passes client_id=")
.chk_true(grepl("viewer_home_client_id\\s*=", window), "generar_archivo_pago_zip call site passes viewer_home_client_id=")

cat(sprintf("\n  Stage 6 subtotal: %d passed, %d failed\n", .pass, .fail))
