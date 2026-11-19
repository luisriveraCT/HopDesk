# =============================================================================
# tests/test_t55c_sumas.R
# T55c, the two places the activity log showed one figure for amounts in
# different currencies:
#   1. a payments ZIP's "Total general": the stored total_general adds the
#      files' totals, MXN and USD alike. Now one total per currency from its
#      files; with more than one currency total_general is never shown; a
#      record whose files don't all say their total and currency shows "—"
#      (the integrator's conditions; pagar_hoy_module.R untouched).
#   2. a Reporte export's "Saldo total": the log stored the sum of every
#      account's balance. Now it stores saldo_por_moneda; an earlier record
#      says "no disponible por moneda (registro anterior)", never the sum.
# Fails on 2ba86ea.
#
# Run from project root:  Rscript --vanilla tests/test_t55c_sumas.R
# =============================================================================
cat("=== test_t55c_sumas ===\n\n")
suppressPackageStartupMessages(library(shiny))
`%||%` <- function(a, b) if (is.null(a)) b else a
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L } else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("R/monedas.R"); source("R/i18n.R")
.i18n_dir <- file.path(getwd(), "www", "i18n")
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres) eval(e, globalenv())
cargar("R/ui_components.R", "montos_por_moneda")
cargar("R/audit_log_viewer_module.R", c(".audit_zip_total_por_moneda", ".audit_detail_payment_file_zip", ".audit_normalize_beneficiarios",
                                         ".audit_beneficiarios_table", "mask_clabe", ".audit_saldo_por_moneda", ".audit_detail_reporte_export"))
cargar("R/reporte_module.R", c(".reporte_saldo_por_moneda", ".log_reporte_export"))
PUNTO <- intToUtf8(0xB7L); RAYA <- intToUtf8(0x2014L)
html <- function(x) paste(as.character(x), collapse = "\n")
archivo <- function(empresa, moneda, total) list(empresa = empresa, moneda = moneda, total = total, n_pagos = 1L,
                                                 cuenta_origen = "c", banco_origen = "b", archivo = "f.txt", beneficiarios = list())

cat("-- 1. a payments ZIP's \"Total general\" --\n")
zip <- list(n_archivos = 2L, empresas = c("NTS", "NRS"), n_pagos_total = 2L, total_general = 1050,
            archivos = list(archivo("NTS", "MXN", 1000), archivo("NRS", "USD", 50)))
h <- html(.audit_detail_payment_file_zip(zip))
ok("MXN + USD: one total per currency, \"1,000.00 MXN · 50.00 USD\"",
   grepl(paste("1,000.00 MXN", PUNTO, "50.00 USD"), h, fixed = TRUE))
ok("…and never the stored total_general, a sum of pesos and dollars (1,050)", !grepl("1,050", h, fixed = TRUE))
ok("one currency: its total, with its code",
   identical(.audit_zip_total_por_moneda(list(archivo("NTS", "MXN", 1000), archivo("NG", "MXN", 50))), "1,050.00 MXN"))
ok("an earlier record whose files don't all say their currency: \"—\", never a mixed sum",
   identical(.audit_zip_total_por_moneda(list(archivo("NTS", "MXN", 1000), archivo("NRS", NULL, 50))), RAYA) &&
     identical(.audit_zip_total_por_moneda(list(archivo("NTS", "MXN", NULL))), RAYA) &&
     identical(.audit_zip_total_por_moneda(list()), RAYA))
ok("…a record with no files shows \"—\" too, not total_general",
   { h0 <- html(.audit_detail_payment_file_zip(list(n_archivos = 0L, total_general = 1050, archivos = list())))
     grepl(paste0("Total general: </strong>\\s*", RAYA), h0) && !grepl("1,050", h0, fixed = TRUE) })

cat("\n-- 2. a Reporte export's \"Saldo total\" --\n")
bals <- data.frame(moneda = c("MXN", "USD", "mxn", "USD"), saldo = c(150000, 20000, 90000, -1500), stringsAsFactors = FALSE)
ok("the log's balances per currency: MXN 240,000, USD 18,500, never summed (258,500)",
   identical(.reporte_saldo_por_moneda(bals), list(MXN = 240000, USD = 18500)))
logged <- NULL
log_action <- function(...) { logged <<- list(...); invisible(NULL) }
.log_reporte_export(list(current_user = function() "u", effective_client_id = function() "c", home_client_id = function() "c"),
                    formato = "pdf", desde = as.Date("2026-11-01"), hasta = as.Date("2026-11-19"), empresas = c("NG", "NRS"),
                    n_cuentas = 4L, saldo_por_moneda = .reporte_saldo_por_moneda(bals))
ok("the log stores saldo_por_moneda and no total_saldo",
   identical(logged$metadata$saldo_por_moneda, list(MXN = 240000, USD = 18500)) && is.null(logged$metadata$total_saldo))
hn <- html(.audit_detail_reporte_export(logged$metadata))
ok("the activity log shows it per currency: \"240,000.00 MXN · 18,500.00 USD\"",
   grepl(paste("240,000.00 MXN", PUNTO, "18,500.00 USD"), hn, fixed = TRUE) && !grepl("258,500", hn, fixed = TRUE))
# A record read back from S3 can come as a list of length-1 vectors; it still works.
ok("…also as read back (a plain named list)",
   identical(.audit_saldo_por_moneda(list(saldo_por_moneda = list(USD = 18500, MXN = 240000))), paste("240,000.00 MXN", PUNTO, "18,500.00 USD")))
hv <- html(.audit_detail_reporte_export(list(formato = "pdf", desde = "2026-10-01", hasta = "2026-10-31", empresas = "NG",
                                             n_cuentas = 4L, total_saldo = 258500)))
ok("an earlier record (only total_saldo): \"no disponible por moneda (registro anterior)\", never the mixed sum",
   grepl(tr("comun.bitacora.saldo_no_disponible"), hv, fixed = TRUE) && !grepl("258,500", hv, fixed = TRUE) &&
     identical(tr("comun.bitacora.saldo_no_disponible", idioma = "en"), "not available per currency (earlier record)"))
ok("no caller of .log_reporte_export() still passes total_saldo",
   !grepl("total_saldo", paste(grep("^\\s*#", readLines("R/reporte_module.R", warn = FALSE, encoding = "UTF-8"), value = TRUE, invert = TRUE),
                                collapse = "\n"), fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
