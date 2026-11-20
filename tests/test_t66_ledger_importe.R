# =============================================================================
# tests/test_t66_ledger_importe.R
# T66: the SAP row editor's Importe (R/ledger_module.R, sap_edit_save) is a
# hd_numero_input; the browser no longer stops a negative, which the
# override's abs() used to turn positive silently. The handler checks it
# before anything is stored; empty (NA) keeps its meaning, back to SAP's
# value (tablas.dia.sap_edit.vacio_es_sap).
#
# There is no testServer harness for ledgerModuleServer, so this takes the
# handler's REAL check out of the parsed module (not its text) and runs it:
#   1. a negative is told and stops the handler; 0, a positive and NA go on;
#   2. the check sits after the date's and before .sap_ov_campos() and any
#      store (moves, overrides);
#   3. the modal's field is a hd_numero_input monto.
# Run from the repo root:  Rscript tests/test_t66_ledger_importe.R
# Contraprueba: on 5e102e3 (no check) sections 1 and 2 fail.
# =============================================================================
cat("=== test_t66_ledger_importe ===\n\n")
suppressPackageStartupMessages(library(shiny))
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L } else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
suppressMessages(source("R/i18n.R"))
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]])[1] %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
    eval(e, envir = globalenv())
cargar("R/ui_components.R", c("HD_NUMERO_FORMATOS", ".hd_numero_decimales", "hd_numero_texto", "hd_numero_deps",
                              "hd_numero_input", "hd_numero_update", "hd_numero_revisar"))

# The body of observeEvent(input$sap_edit_save, { ... }) in the parsed module.
buscar <- function(x) {
  if (!is.call(x)) return(NULL)
  if (identical(x[[1]], as.name("observeEvent")) && length(x) >= 3L &&
      identical(deparse(x[[2]]), "input$sap_edit_save")) return(x[[3]])
  args <- as.list(x)[-1]   # an empty argument (x[, 1]) can't be bound to a variable
  for (i in seq_along(args)) if (is.call(args[[i]])) { r <- buscar(args[[i]]); if (!is.null(r)) return(r) }
  NULL
}
cuerpo <- NULL
for (e in parse("R/ledger_module.R", keep.source = FALSE)) { cuerpo <- buscar(e); if (!is.null(cuerpo)) break }
ok("the sap_edit_save handler is found", is.call(cuerpo))
pasos <- if (is.call(cuerpo)) as.list(cuerpo)[-1] else list()
txt <- vapply(pasos, function(p) paste(deparse(p), collapse = " "), "")
i_chk  <- which(grepl("hd_numero_revisar(input$sap_edit_importe", txt, fixed = TRUE))
i_date <- which(grepl("tablas.dia.aviso.fecha_invalida", txt, fixed = TRUE))
i_ov   <- which(grepl(".sap_ov_campos(", txt, fixed = TRUE))
i_save <- which(grepl("save_moves_deferred|modificar_sap_overrides|save_sap_overrides|moves_db\\(", txt))

# 1. Run the check itself with a stand-in input.
# A function whose body is the handler's statement then "sigue": its
# return() leaves this function, as it leaves the handler.
correr <- function(valor) {
  AVISOS <- character(0)
  showNotification <- function(ui, ...) AVISOS <<- c(AVISOS, as.character(ui))
  f <- function(input) NULL
  body(f) <- call("{", pasos[[i_chk[1]]], "sigue")
  environment(f) <- environment()
  r <- f(list(sap_edit_importe = valor))
  list(sigue = identical(r, "sigue"), avisos = AVISOS)
}
ok("exactly one check of the Importe in the handler", length(i_chk) == 1L)
r <- correr(-1500)
ok("a negative Importe: 'Importe no puede ser negativo.' and the handler stops",
   !r$sigue && identical(r$avisos, "Importe no puede ser negativo."))
r <- correr(-0.01)
ok("…-0.01 too", !r$sigue && length(r$avisos) == 1L)
for (v in list(0, 32000.5, NA_real_, NULL)) {
  r <- correr(v)
  ok(sprintf("Importe %s: no aviso, the handler goes on (NA/NULL = back to SAP's value)",
             if (is.null(v)) "NULL" else as.character(v)),
     r$sigue && !length(r$avisos))
}

# 2. Where it sits.
ok("the check comes after the date's", length(i_date) >= 1L && length(i_chk) == 1L && i_chk > i_date[1])
ok("…and before .sap_ov_campos() and any store", length(i_ov) >= 1L && i_chk < i_ov[1] &&
     (!length(i_save) || i_chk < min(i_save)))

# 3. The field.
lm <- paste(readLines("R/ledger_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("the modal's Importe is hd_numero_input(..., formato = \"monto\"), no numericInput",
   grepl('hd_numero_input\\(ns\\("sap_edit_importe"\\), "Importe", formato = "monto"', lm) &&
     !grepl('numericInput\\(ns\\("sap_edit_importe"', lm))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
