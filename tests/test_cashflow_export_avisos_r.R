# =============================================================================
# tests/test_cashflow_export_avisos_r.R
# The notices of R/cashflow_export_module.R (the Flujo's Word, Excel, HTML and
# PDF exports: data errors, an empty period, timeout, cancel) come from the
# dictionary (settings-avisos): no fixed text, every tablas.flujo.aviso.* key
# in es and en, and the Spanish exactly as the old code built it -- including
# "Error desconocido al generar el archivo.", fixed text that reached the
# Excel notice through a variable.
# Run from project root:  Rscript tests/test_cashflow_export_avisos_r.R
# AVISOS_R=<file> checks another copy of the module (the contraprueba).
# =============================================================================
cat("=== test_cashflow_export_avisos_r ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
suppressMessages(source("R/i18n.R"))
source("tests/helpers/avisos_guardiana.R")
F_M <- Sys.getenv("AVISOS_R", "R/cashflow_export_module.R")

fijos <- avisos_fijos(F_M)
ok(sprintf("ningún aviso de cashflow_export_module.R con texto fijo (quedan %d)", length(fijos)),
   { if (length(fijos)) message("  fijos: ", paste(utils::head(fijos, 5), collapse = " | ")); !length(fijos) })
ok("el error del Excel sin texto ya no cae en un texto fijo",
   !grepl('err <- "Error desconocido al generar el archivo."', paste(readLines(F_M, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), fixed = TRUE))
claves <- avisos_claves_n(F_M, "tablas.flujo.aviso.")
ok(sprintf("las %d claves tablas.flujo.aviso.* existen en es y en", length(claves)),
   { falta <- avisos_claves_faltan(claves); if (length(falta)) message("  faltan: ", paste(falta, collapse = ", "))
     length(claves) >= 12L && !length(falta) })

es <- function(k, ...) tr(k, ..., idioma = "es")
d1 <- as.Date("2026-10-01"); d2 <- as.Date("2026-12-31")
iguales <- list(
  list(es("tablas.flujo.aviso.error_datos", error = "S3"), paste("Error en datos:", "S3")),
  list(es("tablas.flujo.aviso.sin_datos_periodo", desde = as.character(d1), hasta = as.character(d2)),
       paste0("Sin datos para el período seleccionado (", d1, " – ", d2, ").")),
  list(es("tablas.flujo.aviso.sin_datos_periodo", desde = as.character("2026-10-01"), hasta = as.character("2026-12-31")),
       paste0("Sin datos para el período seleccionado (", "2026-10-01", " – ", "2026-12-31", ").")),
  list(es("tablas.flujo.aviso.sin_datos_rango", desde = as.character(d1), hasta = as.character(d2)),
       paste0("Sin datos para el período (", d1, " – ", d2, ").")),
  list(es("tablas.flujo.aviso.error_agregar_datos", error = "x"), paste("Error al agregar datos:", "x")),
  list(es("tablas.flujo.aviso.error_agregar", error = "x"), paste("Error al agregar:", "x")),
  list(es("tablas.flujo.aviso.error_word", error = "x"), paste("Error generando Word:", "x")),
  list(es("tablas.flujo.aviso.error_excel", error = es("tablas.flujo.aviso.error_desconocido")),
       paste("Error al generar Excel:", "Error desconocido al generar el archivo.")),
  list(es("tablas.flujo.aviso.error_excel", error = "zip"), paste("Error al generar Excel:", "zip")),
  list(es("tablas.flujo.aviso.tiempo_agotado"),
       "El proceso superó 5 minutos y fue cancelado. Reduzca el rango de fechas e intente de nuevo."),
  list(es("tablas.flujo.aviso.cancelada"), "Exportación cancelada."),
  list(es("tablas.flujo.aviso.error_html", error = "x"), paste("Error generando HTML:", "x")),
  list(es("tablas.flujo.aviso.error_pdf", error = "x"), paste("Error al generar PDF:", "x")))
distintos <- avisos_distintos(iguales)
ok(sprintf("el español de los %d avisos dice exactamente lo mismo que antes", length(iguales)),
   { for (p in distintos) message("  ahora: ", p[[1]], "\n  antes: ", p[[2]]); !length(distintos) })

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
