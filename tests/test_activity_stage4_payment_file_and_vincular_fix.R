# =============================================================================
# tests/test_activity_stage4_payment_file_and_vincular_fix.R
#
# Stage 4 (2026-08-27), triggered by live-testing feedback on Stages 1-3:
#
#   1. Generating a bank payment file (either the per-company "Archivo de
#      pagos" button or the bulk "Descargar todos (.zip)" button) wrote a
#      permanent record to the SEPARATE pagos_generados audit trail, but
#      never appeared in the general Actividad log the popup/PDF work was
#      built around -- from the user's point of view, "neither button logs
#      anything." Fixed by having .log_pago_generado() (R/pagar_hoy_module.R,
#      the single shared helper both download handlers already call) ALSO
#      call log_action(), dual-writing the same event into both trails, with
#      full beneficiary/bank detail -- and reordered to log BEFORE the PPL
#      bytes are written, per the explicit "even before the actual document
#      is downloaded" requirement.
#   2. "Vincular proveedor" (the magnifying-glass supplier-linking modal)
#      appeared to save but didn't reliably take effect elsewhere -- root
#      cause: save_parte_alias_map() was never followed by
#      bump_sync_version("parte_alias_map_db"), so other sessions never
#      picked up the change (R/sync_bus.R's own documented contract). Also
#      broadened the upsert's dedup filter from Parte+Empresa to Parte-only,
#      since a legacy/empresa-scoped row for the same Parte could otherwise
#      survive alongside a fresh link and silently keep winning.
#
# Sourced by _run_saas.R. Sources R/pagar_hoy_module.R,
# R/pagos_generados_persistence.R, and R/audit_log_viewer_module.R's helpers
# itself (module-scope functions only -- no server body ever executes),
# matching tests/test_payment_audit_stage.R's established pattern.
# =============================================================================

cat("── Activity audit Stage 4: payment-file logging + vincular fix ───────────\n")

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

source("R/pagos_generados_persistence.R", local = FALSE)  # mask_clabe(), add_pago_generado()

# =============================================================================
# Part A -- .log_pago_generado() dual-write: pagos_generados AND log_action
# =============================================================================
cat("\n-- Part A: .log_pago_generado() dual-write --\n")

Sys.setenv(CLIENT_ID = "stage4test")
mock_s3saveRDS(.schema_pagos_generados(), "stage4test/pagos_generados.rds", "mock-bucket")
mock_s3saveRDS(.APP_AUDIT_SCHEMA(),       "stage4test/app_audit.rds",       "mock-bucket")

.mk_shared <- function(user = "larivera", client_id = "stage4test", home_cid = "stage4test") {
  list(
    current_user        = function() user,
    effective_client_id = function() client_id,
    home_client_id       = function() home_cid
  )
}

ppl_rows <- data.frame(
  alias      = c("PROVEEDOR UNO", "PROVEEDOR DOS"),
  clabe_dest = c("012345678901234567", "098765432109876543"),
  banco_dest = c("021", "072"),
  medio_pago = c("SPI", "SPI"),
  importe    = c(1000.50, 2500.00),
  concepto   = c("FAC-1", "FAC-2"),
  stringsAsFactors = FALSE
)

.log_pago_generado(
  .mk_shared(), "ACME", "MXN", "0123456789", ppl_rows,
  fname = "pagos_ACME_26082026_001.txt", source_type = "individual",
  banco_origen = "BBVA"
)
.audit_flush_pending("stage4test")

pg <- mock_s3readRDS("stage4test/pagos_generados.rds", "mock-bucket")
.chk(nrow(pg), 1L, "pagos_generados: one row written (existing audit trail unaffected)")
.chk(pg$banco_origen[1], "BBVA", "pagos_generados: banco_origen now populated (was hardcoded NA)")
.chk(pg$empresa[1], "ACME", "pagos_generados: empresa correct")

log_df <- mock_s3readRDS("stage4test/app_audit.rds", "mock-bucket")
.chk(nrow(log_df), 1L, "app_audit (general Actividad log): ALSO got one row -- the actual fix")
.chk(log_df$module[1], "pagar_hoy", "app_audit: module='pagar_hoy'")
.chk(log_df$action[1], "generar_archivo_pago", "app_audit: action='generar_archivo_pago'")
.chk(log_df$user[1], "larivera", "app_audit: user recorded")
.chk(log_df$target_id[1], "ACME", "app_audit: target_id is the empresa")

meta <- jsonlite::fromJSON(log_df$metadata[1], simplifyVector = TRUE)
.chk(meta$empresa, "ACME", "app_audit metadata: empresa round-trips")
.chk(meta$moneda, "MXN", "app_audit metadata: moneda round-trips")
.chk(meta$cuenta_origen, "0123456789", "app_audit metadata: cuenta_origen round-trips")
.chk(meta$banco_origen, "BBVA", "app_audit metadata: banco_origen round-trips (the actual bank, not NA)")
.chk(as.numeric(meta$n_pagos), 2, "app_audit metadata: n_pagos round-trips")
.chk(as.numeric(meta$total), 3500.50, "app_audit metadata: total round-trips (1000.50 + 2500.00)")
.chk(meta$source, "individual", "app_audit metadata: source round-trips")
.chk(meta$archivo, "pagos_ACME_26082026_001.txt", "app_audit metadata: archivo (filename) round-trips")
.chk(nrow(meta$beneficiarios), 2L, "app_audit metadata: full beneficiarios array present (2 rows)")
.chk(meta$beneficiarios$alias, c("PROVEEDOR UNO", "PROVEEDOR DOS"),
     "app_audit metadata: beneficiary names present -- 'everything: supplier data' satisfied")
.chk_true(any(grepl("012345678901234567", as.character(meta$beneficiarios$clabe))),
     "app_audit metadata: FULL (unmasked) CLABE stored in metadata -- masking is a display-layer concern, not a storage one")

# ── Zero-row ppl_rows: never logs (nothing to log), never errors ────────────
ok_zero <- tryCatch({
  .log_pago_generado(.mk_shared(), "ACME", "MXN", "0123456789",
                     ppl_rows[0, ], fname = "x.txt", source_type = "individual")
  TRUE
}, error = function(e) FALSE)
.chk_true(ok_zero, "zero-row ppl_rows: does not error")
.audit_flush_pending("stage4test")
log_df_after_zero <- mock_s3readRDS("stage4test/app_audit.rds", "mock-bucket")
.chk(nrow(log_df_after_zero), 1L, "zero-row ppl_rows: no new app_audit row written (still just the 1 real one)")

# ── ZIP/bulk source_type ────────────────────────────────────────────────────
.log_pago_generado(
  .mk_shared(), "BETA", "USD", "9988776655", ppl_rows[1, , drop = FALSE],
  fname = "pagos_BETA_USD_001.txt", source_type = "zip_bulk", banco_origen = "Santander"
)
.audit_flush_pending("stage4test")
log_df_zip <- mock_s3readRDS("stage4test/app_audit.rds", "mock-bucket")
.chk(nrow(log_df_zip), 2L, "zip_bulk call also dual-writes into app_audit")
meta_zip <- jsonlite::fromJSON(log_df_zip$metadata[2], simplifyVector = TRUE)
.chk(meta_zip$source, "zip_bulk", "zip_bulk entry's metadata source is correctly 'zip_bulk'")

# ── banco_origen defaults to NA when omitted (backward compatible signature) ─
.log_pago_generado(.mk_shared(), "GAMMA", "MXN", "111", ppl_rows[1, , drop = FALSE],
                   fname = "y.txt", source_type = "individual")
.audit_flush_pending("stage4test")
log_df_noBank <- mock_s3readRDS("stage4test/app_audit.rds", "mock-bucket")
meta_noBank <- jsonlite::fromJSON(log_df_noBank$metadata[3], simplifyVector = TRUE)
.chk_true(is.na(meta_noBank$banco_origen) || is.null(meta_noBank$banco_origen),
     "banco_origen omitted by caller: stays NA/null, not a fabricated value")

# =============================================================================
# Part B -- Activity popup/PDF rendering for the new 'payment_file' kind
# =============================================================================
cat("\n-- Part B: 'payment_file' detail kind (popup + PDF reuse) --\n")

.chk(.audit_detail_kind("pagar_hoy", "generar_archivo_pago"), "payment_file",
     "generar_archivo_pago classifies as payment_file")
.chk_false(identical(.audit_detail_kind("pagar_hoy", "generar_archivo_pago"), "generic"),
     "payment_file is NOT the generic fallback -- bespoke renderer engaged")

meta_pf <- list(
  empresa = "ACME", moneda = "MXN", cuenta_origen = "0123456789", banco_origen = "BBVA",
  n_pagos = 2L, total = 3500.50, source = "individual", archivo = "pagos_ACME_001.txt",
  beneficiarios = list(
    list(alias = "PROVEEDOR UNO", clabe = "012345678901234567", banco = "021",
         importe = 1000.50, concepto = "FAC-1"),
    list(alias = "PROVEEDOR DOS", clabe = "098765432109876543", banco = "072",
         importe = 2500.00, concepto = "FAC-2")
  )
)
html_pf <- paste(as.character(.audit_detail_payment_file(meta_pf)), collapse = "\n")
.chk_true(grepl("ACME", html_pf, fixed = TRUE), "payment_file detail: empresa shown")
.chk_true(grepl("BBVA", html_pf, fixed = TRUE), "payment_file detail: banco_origen shown")
.chk_true(grepl("0123456789", html_pf, fixed = TRUE), "payment_file detail: cuenta_origen shown")
.chk_true(grepl("3,500.50", html_pf, fixed = TRUE), "payment_file detail: total formatted with thousands separator")
.chk_true(grepl("PROVEEDOR UNO", html_pf, fixed = TRUE), "payment_file detail: beneficiary name shown")
.chk_true(grepl("FAC-1", html_pf, fixed = TRUE), "payment_file detail: concepto shown")
.chk_true(grepl(mask_clabe("012345678901234567"), html_pf, fixed = TRUE),
     "payment_file detail: CLABE is MASKED (bullets + last 4 digits), matching the specialized viewer's convention")
.chk_false(grepl("012345678901234567", html_pf, fixed = TRUE),
     "payment_file detail: the FULL unmasked CLABE never appears in the popup/PDF markup")
.chk_true(grepl("Ver archivos de pago", html_pf, fixed = TRUE),
     "payment_file detail: points the user to the specialized viewer for the unmasked receipt")

meta_pf_empty <- list(empresa = "X", moneda = "MXN", beneficiarios = list())
html_pf_empty <- paste(as.character(.audit_detail_payment_file(meta_pf_empty)), collapse = "\n")
.chk_true(grepl("Sin beneficiarios", html_pf_empty, fixed = TRUE),
     "payment_file detail: empty beneficiarios shows the 'no data' message, not a broken table")

# End-to-end via .audit_detail_modal_body(), same as Stage 2's own Part I
row_pf <- data.frame(
  id = "r1", ts = as.POSIXct("2026-08-27 09:00:00", tz = "UTC"), user = "larivera",
  module = "pagar_hoy", action = "generar_archivo_pago",
  description = "Archivo de pago generado: ACME (2 pago(s), MXN)",
  target_id = "ACME", s3_key = NA_character_, client_id = "stage4test",
  metadata = as.character(jsonlite::toJSON(meta_pf, auto_unbox = TRUE, na = "null")),
  stringsAsFactors = FALSE
)
html_modal <- paste(as.character(.audit_detail_modal_body(row_pf, "own_client")), collapse = "\n")
.chk_true(grepl("PROVEEDOR DOS", html_modal, fixed = TRUE),
     "end-to-end popup: full beneficiary detail reachable from a raw audit row")
.chk_false(grepl("098765432109876543", html_modal, fixed = TRUE),
     "end-to-end popup: second beneficiary's full CLABE also never leaks")

# End-to-end via generate_audit_log_html_doc(), same as Stage 3's own Part C
export_df_pf <- row_pf
html_path_pf <- generate_audit_log_html_doc(export_df_pf, list(
  generated_by = "Luis Rivera", date_from = "2026-08-20 00:00:00",
  date_to = "2026-08-27 00:00:00", client_label = "stage4test"
))
pdf_txt <- paste(readLines(html_path_pf, warn = FALSE), collapse = "\n")
.chk_true(grepl("PROVEEDOR UNO", pdf_txt, fixed = TRUE),
     "end-to-end PDF: payment-file beneficiary detail present in the exported document")
.chk_false(grepl("012345678901234567", pdf_txt, fixed = TRUE),
     "end-to-end PDF: full CLABE never leaks into the exported document either")
unlink(html_path_pf)

# =============================================================================
# Part C -- vincular proveedor: sync propagation + dedup-filter regression guards
# =============================================================================
cat("\n-- Part C: vincular proveedor fix (static regression guards) --\n")

ph_src <- readLines("R/pagar_hoy_module.R", warn = FALSE)
save_lines <- grep("save_parte_alias_map\\(am,", ph_src)
.chk(length(save_lines), 2L, "sanity: exactly 2 save_parte_alias_map(am, ...) call sites found (link + unlink)")

for (ln in save_lines) {
  window <- paste(ph_src[ln:min(ln + 12, length(ph_src))], collapse = "\n")
  .chk_true(grepl('bump_sync_version\\("parte_alias_map_db"\\)', window),
       sprintf("pagar_hoy_module.R:%d -- bump_sync_version(\"parte_alias_map_db\") now follows the save (was missing)", ln))
}

# The old buggy pattern (Empresa-conditional upsert filter) must be gone --
# a literal regression guard, not just "the new pattern exists".
.chk_false(any(grepl('am\\$Empresa == emp \\|', ph_src)),
     "the old Empresa-conditional dedup filter (root cause of stale duplicate links) no longer appears anywhere")

link_count  <- length(grep('toupper\\(trimws\\(am\\$Parte\\)\\) != toupper\\(parte\\)', ph_src))
.chk(link_count, 2L, "both link and unlink handlers use the new Parte-only dedup filter")

# ── Demonstrate the filter FIX's actual behavior on synthetic data ──────────
# (independent of the module's reactive machinery -- proves the FORMULA is
# correct, not just that it's present in the source.)
am_legacy <- data.frame(
  Parte = c("ACME SA", "OTHER SA"), Empresa = c("Networks Trucking Services", ""),
  alias = c("OLD-ALIAS", "X"), linked_by = c("someone", "x"), linked_at = c("t1", "t2"),
  stringsAsFactors = FALSE
)
parte <- "ACME SA"; emp <- ""  # emp is always "" from this UI (confirmed in the investigation)

old_filtered <- am_legacy[!(toupper(trimws(am_legacy$Parte)) == toupper(parte) &
                            (am_legacy$Empresa == emp | (emp == "" & (is.na(am_legacy$Empresa) | am_legacy$Empresa == "")))), , drop = FALSE]
.chk_true(any(old_filtered$Parte == "ACME SA"),
     "OLD filter formula: a legacy empresa-scoped 'ACME SA' row would have survived a re-link (reproduces the bug)")

new_filtered <- am_legacy[toupper(trimws(am_legacy$Parte)) != toupper(parte), , drop = FALSE]
.chk_false(any(new_filtered$Parte == "ACME SA"),
     "NEW filter formula: the legacy empresa-scoped 'ACME SA' row is correctly removed before inserting the fresh link")
.chk_true(any(new_filtered$Parte == "OTHER SA"),
     "NEW filter formula: an unrelated Parte's row is left untouched")

cat(sprintf("\n  Stage 4 subtotal: %d passed, %d failed\n", .pass, .fail))
