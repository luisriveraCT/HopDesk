# =============================================================================
# tests/test_t92_cuenta_origen.R
# T92. "Archivos de pago" showed an empty Cuenta origen for 4 payment files
# of October 2026: get_cuenta_ppl() gave "" (the global company map, fixed
# 06e041c8) and both the PPL (zero-padded) and the log got it. A logged
# record is never edited and the account is never guessed, so:
#   1 pagos_cuenta_origen_texto(): masked in the table / Excel, whole in the
#     detail and the PDFs; a payment file without one says "No registrada";
#     a file that is not a payment has none ("");
#   2 the receipt PDF and the multi-record PDF block say it the same way;
#   3 in each of the 3 PPL paths, what .log_pago_generado() records is the
#     same expression generate_ppl() writes as the origin;
#   4 the lock: a payment file logged without an account leaves
#     generar_archivo_pago_sin_cuenta in the log (it was silent).
# Run from project root:  Rscript --vanilla tests/test_t92_cuenta_origen.R
# =============================================================================
cat("=== test_t92_cuenta_origen ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
suppressMessages({
  source("R/i18n.R")
  source("R/monedas.R")
  if (file.exists("R/tema_registro.R")) source("R/tema_registro.R")
  source("R/pagos_generados_persistence.R")
  source("R/pagos_audit_viewer_module.R")
})
NR <- tr("tablas.archivos_pago.cuenta_no_registrada")
CTA <- "012345678901"

# ── 1. The helper ────────────────────────────────────────────────────────
ok("1 the dictionary has the text in both languages",
   identical(NR, "No registrada") && identical(tr("tablas.archivos_pago.cuenta_no_registrada", idioma = "en"), "Not recorded"))
ok("1 table / Excel: a stored account is masked (•••• + last 4)",
   identical(pagos_cuenta_origen_texto(CTA, "pago_ppl", TRUE), "••••8901"))
ok("1 detail / PDF: whole, as their rule is", identical(pagos_cuenta_origen_texto(CTA, "pago_ppl"), CTA))
ok("1 a payment file without one (NA, \"\", spaces): No registrada, masked or not",
   all(pagos_cuenta_origen_texto(c(NA, "", "  "), "pago_ppl", TRUE) == NR) &&
     all(pagos_cuenta_origen_texto(c(NA, "", "  "), "pago_ppl") == NR))
ok("1 tipo NA reads as a payment file (as is_pago does)", identical(pagos_cuenta_origen_texto(NA, NA), NR))
ok("1 a file that is not a payment (a report) has none: \"\"",
   identical(pagos_cuenta_origen_texto(c(NA, ""), c("reporte_excel", "reporte_pdf"), TRUE), c("", "")))
ok("1 vectorized and zero rows", identical(pagos_cuenta_origen_texto(c(CTA, NA, NA), c("pago_ppl", "pago_ppl", "reporte_word"), TRUE),
                                           c("••••8901", NR, "")) &&
     identical(pagos_cuenta_origen_texto(character(0), character(0), TRUE), character(0)))

# ── 2. The PDFs ──────────────────────────────────────────────────────────
fila <- function(cuenta, tipo = "pago_ppl") tibble::tibble(
  id = "x", ts = as.POSIXct("2026-10-27 10:00:00", tz = "UTC"), user = "tesoreria", client_id = "networks",
  empresa = "NTS", moneda = "MXN", cuenta_origen = cuenta, banco_origen = NA_character_, n_pagos = 1L,
  total = 100, beneficiarios = "[]", source = "individual", archivo = "pagos_NTS_27102026_001.txt",
  tipo = tipo, detalle = NA_character_)
texto_de <- function(x) paste(as.character(x), collapse = "")
r_sin <- tryCatch(texto_de(.build_pago_receipt_html(fila(NA_character_))), error = function(e) paste("ERROR", conditionMessage(e)))
r_con <- tryCatch(texto_de(.build_pago_receipt_html(fila(CTA))), error = function(e) paste("ERROR", conditionMessage(e)))
ok("2 receipt PDF: a payment file without an account says No registrada", grepl(NR, r_sin, fixed = TRUE))
ok("2 receipt PDF: with one, the whole account (its rule)", grepl(CTA, r_con, fixed = TRUE) && !grepl(NR, r_con, fixed = TRUE))
b_sin <- texto_de(.pagos_pdf_entry_block(fila(NA_character_)))
b_con <- texto_de(.pagos_pdf_entry_block(fila(CTA)))
ok("2 multi-record PDF block: No registrada / the account", grepl(NR, b_sin, fixed = TRUE) && grepl(CTA, b_con, fixed = TRUE))

# ── 3. What is logged is what the file carries, in the 3 paths ───────────
ex <- parse("R/pagar_hoy_module.R", keep.source = TRUE, encoding = "UTF-8")
llamadas <- list()
andar <- function(e) {
  if (!is.call(e)) return(invisible())
  f <- e[[1]]
  if (is.name(f) && as.character(f) %in% c(".log_pago_generado", "generate_ppl")) {
    sr <- attr(e, "srcref"); llamadas[[length(llamadas) + 1L]] <<- list(f = as.character(f), e = e)
  }
  # T84 B2: the file is generated through the layouts' registry,
  # layout_llamar(lay, "generar", rows, origen, ...): the origin is its 4th
  # argument. Recorded as "generar" with the call shifted so that argument
  # sits where generate_ppl() had it (2nd).
  if (is.name(f) && identical(as.character(f), "layout_llamar") && length(e) >= 3L && identical(e[[3]], "generar")) {
    llamadas[[length(llamadas) + 1L]] <<- list(f = "generar", e = e[-c(2L, 3L)])
  }
  for (k in seq_along(e)[-1]) { a <- e[[k]]; if (!missing(a) && is.call(a)) andar(a) }
}
for (e in ex) andar(e)
# Source order is the order of the walk: each log call is followed by the
# generate_ppl() of its own file.
fs <- vapply(llamadas, `[[`, "", "f")
logs <- which(fs == ".log_pago_generado")
ok("3 three paths log a payment file (individual, ZIP, ZIP without the supplier's account)", length(logs) == 3L)
arg <- function(cl, pos, nombre) { a <- as.list(cl)[-1]; if (!is.null(names(a)) && nombre %in% names(a)) a[[nombre]] else a[[pos]] }
iguales <- vapply(logs, function(i) {
  sig <- which(fs %in% c("generate_ppl", "generar") & seq_along(fs) > i)[1]
  !is.na(sig) && identical(deparse(arg(llamadas[[i]]$e, 4L, "cuenta_origen")), deparse(arg(llamadas[[sig]]$e, 2L, "cuenta_origen")))
}, NA)
ok("3 in each, the account logged is the same expression generate_ppl() writes as the origin", all(iguales))
ok("3 and the files are generated through the layouts' registry (T84 B2), never generate_ppl() directly",
   sum(fs == "generar") == 3L && !any(fs == "generate_ppl"))

# ── 4. The lock ──────────────────────────────────────────────────────────
for (e in parse("R/pagar_hoy_module.R", keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), ".log_pago_generado"))
    eval(e, globalenv())
LOGS <- character(); REG <- list()
log_action <- function(user, module, action, ...) { LOGS <<- c(LOGS, action); invisible(NULL) }
add_pago_generado <- function(record, client_id = NULL, ...) { REG[[length(REG) + 1L]] <<- record; invisible(list(ok = TRUE)) }
shared <- list(effective_client_id = function() "networks", home_client_id = function() "networks", current_user = function() "tesoreria")
ppl <- tibble::tibble(alias = "A", clabe_dest = "012180001111111111", banco_dest = "012", importe = 100, concepto = "F1")
.log_pago_generado(shared, "NTS", "MXN", "", ppl, fname = "f.txt", source_type = "individual")
ok("4 logged without an account: generar_archivo_pago_sin_cuenta in the log (and the record as always)",
   "generar_archivo_pago_sin_cuenta" %in% LOGS && "generar_archivo_pago" %in% LOGS && length(REG) == 1L)
LOGS <- character(); REG <- list()
.log_pago_generado(shared, "NTS", "MXN", CTA, ppl, fname = "f.txt", source_type = "individual")
ok("4 with its account: no lock entry, the record carries it",
   !"generar_archivo_pago_sin_cuenta" %in% LOGS && identical(REG[[1]]$cuenta_origen, CTA))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
