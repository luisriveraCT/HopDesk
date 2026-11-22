# =============================================================================
# tests/test_t55c_montos.R
# T55c: the amounts that still didn't follow monto_moneda()'s format (no "$"
# there, but no code, the code before the number, an ASCII minus or no
# thousands separator). The two findings that summed currencies have their
# own test: tests/test_t55c_sumas.R.
#   3. the activity log and the payment files' viewer (screen, receipt, PDF).
#
# Run from project root:  Rscript --vanilla tests/test_t55c_montos.R
# =============================================================================
cat("=== test_t55c_montos ===\n\n")
suppressPackageStartupMessages(library(shiny))
`%||%` <- function(a, b) if (is.null(a)) b else a
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L } else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("R/monedas.R"); source("R/i18n.R")
.i18n_dir <- file.path(getwd(), "www", "i18n")
todas <- function(archivo) {   # every function a file defines, only the definitions
  for (e in parse(archivo, keep.source = FALSE, encoding = "UTF-8"))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && is.call(e[[3]]) &&
        identical(e[[3]][[1]], as.name("function"))) eval(e, globalenv())
}
constantes <- function(archivo) {   # its top-level constants too (.PAGOS_TIPO_LABELS)
  for (e in parse(archivo, keep.source = FALSE, encoding = "UTF-8"))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && !is.call(e[[3]]) ||
        (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("c"))))
      eval(e, globalenv())
}
todas("R/audit_log_viewer_module.R"); todas("R/pagos_audit_viewer_module.R"); constantes("R/pagos_audit_viewer_module.R")
todas("R/pagos_generados_persistence.R")   # mask_clabe(), parse_pago_beneficiarios()
for (e in parse("R/ui_components.R", keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("montos_por_moneda"))) eval(e, globalenv())
html <- function(x) paste(as.character(x), collapse = "\n")
MENOS <- intToUtf8(0x2212L)
leer <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
parsea <- function(f) !inherits(tryCatch(parse(f, keep.source = FALSE, encoding = "UTF-8"), error = function(e) e), "error")

cat("-- 3. the activity log and the payment files --\n")
ok("the files T55c touches parse", all(vapply(c("R/audit_log_viewer_module.R", "R/pagos_audit_viewer_module.R"), parsea, TRUE)))
ben <- list(list(alias = "PROV A", clabe = "012345678901234567", banco = "BBVA", importe = 1234.5, concepto = "F-1"),
            list(alias = "PROV B", clabe = "012345678901234568", banco = "BBVA", importe = 50, concepto = "F-2"))
meta <- list(empresa = "NTS", moneda = "USD", cuenta_origen = "c", banco_origen = "b", total = 1284.5, n_pagos = 2L,
             source = "individual", archivo = "f.txt", beneficiarios = ben)
h <- html(.audit_detail_payment_file(meta))
ok("the activity log, a payment file: its total with its code (\"1,284.50 USD\")", grepl("1,284.50 USD", h, fixed = TRUE))
ok("…each beneficiary the number alone (the file names its currency), no code repeated",
   grepl(">1,234.50<", h, fixed = TRUE) && !grepl("1,234.50 USD", h, fixed = TRUE))
meta_jpy <- meta; meta_jpy$moneda <- "JPY"; meta_jpy$total <- 1500.4
meta_jpy$beneficiarios <- list(list(alias = "X", clabe = "1", banco = "B", importe = 1500.4, concepto = "c"))
hj <- html(.audit_detail_payment_file(meta_jpy))
ok("…in its currency's decimals (JPY whole)", grepl("1,500 JPY", hj, fixed = TRUE) && grepl(">1,500<", hj, fixed = TRUE))
ok("…a record without a total says \"—\", not \"0.00\"",
   grepl(paste0("Total: </strong>\\s*", intToUtf8(0x2014L)), html(.audit_detail_payment_file(modifyList(meta, list(total = NULL))))))
hz <- html(.audit_detail_payment_file_zip(list(n_archivos = 2L, archivos = list(
  modifyList(meta, list(moneda = "MXN", total = 1000)), modifyList(meta, list(moneda = "USD", total = 50))))))
ok("a ZIP: each file's total with its own code", grepl("1,000.00 MXN", hz, fixed = TRUE) && grepl("50.00 USD", hz, fixed = TRUE))
hb <- html(.audit_detail_bancos_manual_edit(list(cuenta_id = "c1", tipo = "spei_out", cargo = -1500.25, abono = 0)))
ok("Bancos' movements (no currency in the record): the number alone, the true minus sign",
   grepl(paste0(MENOS, "1,500.25"), hb, fixed = TRUE) && !grepl("-1,500.25", hb, fixed = TRUE))
# A payment file's record as stored (pagos_generados): its beneficiaries as JSON.
fila <- data.frame(id = "p1", ts = as.POSIXct("2026-11-19 10:00:00", tz = "UTC"), user = "luis", tipo = "pago_ppl",
                   empresa = "NTS", moneda = "USD", total = 1284.5, n_pagos = 2L, archivo = "f.txt", source = "individual",
                   cuenta_origen = "c", banco_origen = "b", detalle = NA_character_,
                   beneficiarios = as.character(jsonlite::toJSON(ben, auto_unbox = TRUE)), stringsAsFactors = FALSE)
recibo <- tryCatch(paste(.build_pago_receipt_html(fila), collapse = "\n"), error = function(e) paste("ERROR", conditionMessage(e)))
ok("the payment file's receipt: total \"1,284.50 USD\", each beneficiary the number alone",
   grepl("Total: 1,284.50 USD", recibo, fixed = TRUE) && grepl(">1,234.50<", recibo, fixed = TRUE))
pdf <- tryCatch(html(.pagos_pdf_entry_block(fila)), error = function(e) paste("ERROR", conditionMessage(e)))
ok("…and its PDF block the same", grepl("1,284.50 USD", pdf, fixed = TRUE) && grepl(">1,234.50<", pdf, fixed = TRUE))
ok("no formatC() left formatting an amount in either viewer",
   !grepl("formatC(", leer("R/audit_log_viewer_module.R"), fixed = TRUE) && !grepl("formatC(", leer("R/pagos_audit_viewer_module.R"), fixed = TRUE))

cat("\n-- 4. the IC Map and Intercompañía --\n")
for (e in parse("R/treasury_map_module.R", keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(".tm_fmt"))) eval(e, globalenv())
ok("the IC Map's amounts: the code after the number, whole units, the arrow gives the direction (was \"MXN 1,234\")",
   identical(.tm_fmt(-1234.4, "MXN"), "1,234 MXN") && identical(.tm_fmt(2500000, "USD"), "2,500,000 USD") &&
     identical(.tm_fmt(NA, "MXN"), intToUtf8(0x2014L)))
tm <- leer("R/treasury_map_module.R"); ic <- leer("R/interco_module.R")
ok("the map's arrows: the engine's compact format (\"350K\", \"1.5M\"), never \"1,500K\"; a one-currency plan the number alone",
   grepl("lbl <- .hd_dinero_compacto(r$Total)", tm, fixed = TRUE) &&
     grepl("monto_moneda(x$Importe, x$Moneda, decimales = 0L, codigo = FALSE)", tm, fixed = TRUE) &&
     !grepl("big.mark", tm, fixed = TRUE))
ok("Intercompañía's totals per currency: \"1,234 MXN\", the code after",
   grepl("dplyr::mutate(lbl = monto_moneda(T, Moneda, decimales = 0L))", ic, fixed = TRUE) && !grepl("big.mark", ic, fixed = TRUE) &&
     identical(monto_moneda(c(1234.4, 99.6), c("MXN", "USD"), decimales = 0L), c("1,234 MXN", "100 USD")))
ok("both files parse", parsea("R/treasury_map_module.R") && parsea("R/interco_module.R"))

cat("\n-- 5. Requisiciones and the Pasivos wizard --\n")
ok("Requisiciones, the budget hint: \"1,234 de 5,000 MXN\" (es) / \"1,234 of 5,000 MXN\" (en), {moneda} no longer before",
   identical(tr("requisiciones.carpeta.presupuesto_de", usado = monto_moneda(1234.4, "MXN", decimales = 0L, codigo = FALSE),
                total = monto_moneda(5000, "MXN", decimales = 0L), idioma = "es"), "1,234 de 5,000 MXN") &&
     identical(tr("requisiciones.carpeta.presupuesto_de", usado = "1,234", total = "5,000 MXN", idioma = "en"), "1,234 of 5,000 MXN") &&
     grepl("total = monto_moneda(p, res$moneda[i] %||% NA_character_, decimales = 0L)", leer("R/requisiciones_module.R"), fixed = TRUE))
for (e in parse("R/pasivos_wizard_module.R", keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(".wiz_revision_summary_html"))) eval(e, globalenv())
wz <- leer("R/pasivos_wizard_module.R")
ok("the wizard's summary: \"Principal\" and \"Monto\" with separators and their currency (was \"1234.56\")",
   grepl('c("Principal",    monto_moneda(draft$principal_original %||% NA_real_, draft$moneda_pago %||% NA_character_))', wz, fixed = TRUE) &&
     grepl('c("Monto", monto_moneda(draft$amount_default %||% NA_real_, draft$moneda_pago %||% NA_character_))', wz, fixed = TRUE) &&
     !grepl('sprintf("%.2f"', wz, fixed = TRUE))
ok("…and its grace payment, in the chosen currency", lengths(regmatches(wz, gregexpr(
  "monto_moneda(", wz, fixed = TRUE))) >= 4L && !grepl("nsmall", wz, fixed = TRUE))
ok("both files parse", parsea("R/requisiciones_module.R") && parsea("R/pasivos_wizard_module.R"))

cat("\n-- 6. Bancos' TXT totals warning, and the guardian --\n")
ok("the TXT totals warning: the file's amounts with its currency (test_bancos_detect_cuenta checks the text)",
   grepl("f <- function(x) monto_moneda(x, if (length(moneda) && !is.na(moneda[1])) moneda[1] else NA_character_)",
         leer("R/bancos_detect.R"), fixed = TRUE) && parsea("R/bancos_detect.R"))
# Where an amount may still be written by hand with big.mark, and why:
PERMITIDOS <- c("R/monedas.R"         = "monto_moneda() itself",
                "R/hd_columnas.R"     = "the engine's compact and whole formats",
                "R/i18n.R"            = "tr() formats its numbers",
                "R/global.R"          = "fmt_money(), only for the activity log's text (T55b)",
                "R/bancos_module.R"   = "a movement's log text (log_action, T55c P1)")
llamadas <- unlist(lapply(c("app.R", list.files("R", "\\.R$", full.names = TRUE, recursive = TRUE)), function(f) {
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")
  k <- grep("big.mark", x, fixed = TRUE); k <- k[!grepl("^\\s*#", x[k])]
  if (length(k)) paste0(f, ":", k)
}))
fuera <- llamadas[!sub(":[0-9]+$", "", llamadas) %in% c(names(PERMITIDOS), "app.R")]
app_r <- llamadas[startsWith(llamadas, "app.R:")]
ok(sprintf("the guardian: big.mark only where allowed (%d lines; app.R only its log text)", length(llamadas)),
   { if (length(fuera)) message("  fuera de lo permitido: ", paste(fuera, collapse = ", "))
     !length(fuera) && length(app_r) <= 1L })

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
