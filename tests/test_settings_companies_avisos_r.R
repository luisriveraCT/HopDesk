# =============================================================================
# tests/test_settings_companies_avisos_r.R
# The notices of R/settings/settings_companies.R (partners' policies: revert,
# save, generate dates and why there are none) come from the dictionary
# (settings-avisos):
#   1. no showNotification() carries fixed text;
#   2. every tablas.ajustes.aviso.* key it uses exists in es and in en;
#   3. the Spanish reads EXACTLY as before, built as the old code built it.
# Run from project root:  Rscript tests/test_settings_companies_avisos_r.R
# AVISOS_R=<file> checks another copy of the module (the contraprueba).
# =============================================================================
cat("=== test_settings_companies_avisos_r ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
suppressMessages(source("R/i18n.R"))
source("tests/helpers/avisos_guardiana.R")
F_M <- Sys.getenv("AVISOS_R", "R/settings/settings_companies.R")

fijos <- avisos_fijos(F_M)
ok(sprintf("ningún aviso de settings_companies.R con texto fijo (quedan %d)", length(fijos)),
   { if (length(fijos)) message("  fijos: ", paste(utils::head(fijos, 5), collapse = " | ")); !length(fijos) })

claves <- avisos_claves_n(F_M, "tablas.ajustes.aviso.")
ok(sprintf("las %d claves tablas.ajustes.aviso.* existen en es y en", length(claves)),
   { falta <- avisos_claves_faltan(claves); if (length(falta)) message("  faltan: ", paste(falta, collapse = ", "))
     length(claves) >= 16L && !length(falta) })

es   <- function(k, ...) tr(k, ..., idioma = "es")
es_n <- function(k, n, ...) tr_n(k, as.character(n), ..., idioma = "es")
revertidas <- function(sel) sprintf("Fechas de política revertidas para %d socio%s.",
                                    length(sel), if (length(sel) == 1L) "" else "s")
# With no data loaded the count reads 0, not the word "NULL" (textos-codigo,
# a change Luis asked for in the visible-text review: not the old text).
columna_parte <- function(sap) paste0("Sin fechas: no se encontró la columna Parte en los datos SAP. ",
                                      "Revisa si ya cargaron (filas CxP: ",
                                      if (!is.null(sap$AP)) nrow(sap$AP) else 0L,
                                      ", filas CxC: ",
                                      if (!is.null(sap$AR)) nrow(sap$AR) else 0L, ").")
columna_parte_ahora <- function(sap) es("tablas.ajustes.aviso.sin_columna_parte",
                                        ap = if (!is.null(sap$AP)) as.character(nrow(sap$AP)) else "0",
                                        ar = if (!is.null(sap$AR)) as.character(nrow(sap$AR)) else "0")
ok("sin datos SAP el conteo dice 0, no \"NULL\" (en el código del módulo)",
   { cod <- paste(readLines("R/settings/settings_companies.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
     # (the "NULL" of the [POLICY_APPLY] server log line is not visible text)
     grepl('as.character(nrow(sap$AP)) else "0"', cod, fixed = TRUE) &&
       grepl('as.character(nrow(sap$AR)) else "0"', cod, fixed = TRUE) &&
       !grepl('as.character(nrow(sap$A[PR])) else "NULL"', cod) })
sap1 <- list(AP = data.frame(x = seq_len(1500)), AR = NULL)
sap_s <- paste0('"', paste(c("acme", "beta"), collapse = '","'), '"'); pp_s <- '"gamma"'
iguales <- list(
  list(es_n("tablas.ajustes.aviso.politicas_revertidas", length("a")), revertidas("a")),
  list(es_n("tablas.ajustes.aviso.politicas_revertidas", length(c("a", "b"))), revertidas(c("a", "b"))),
  list(es("tablas.ajustes.aviso.error_revertir", error = "S3"), paste("Error al revertir:", "S3")),
  list(es("tablas.ajustes.aviso.sin_socio"), "No hay socio seleccionado."),
  list(es("tablas.ajustes.aviso.politicas_guardadas", parte = "ACME SA"), sprintf('Políticas de "%s" guardadas.', "ACME SA")),
  list(es("tablas.ajustes.aviso.error_guardar", error = "S3"), paste("Error al guardar:", "S3")),
  list(es("tablas.ajustes.aviso.sin_asignadas"), "No hay políticas asignadas a ningún socio."),
  list(es("tablas.ajustes.aviso.catalogo_vacio"), "El catálogo de políticas está vacío."),
  list(es("tablas.ajustes.aviso.solo_intercompania"), "Solo hay socios intercompañía configurados."),
  list(es("tablas.ajustes.aviso.error_politicas_ap", error = "x"), paste("[Políticas CxP]", "x")),
  list(es("tablas.ajustes.aviso.error_politicas_ar", error = "x"), paste("[Políticas CxC]", "x")),
  list(columna_parte_ahora(sap1), columna_parte(sap1)),                     # 1500 rows: no thousands separator
  list(columna_parte_ahora(list()), columna_parte(list())),
  list(es("tablas.ajustes.aviso.sin_coincidencias", sap = sap_s, politicas = pp_s),
       paste0("Sin coincidencias de Socio: SAP tiene [", sap_s, "] pero políticas tienen [", pp_s,
              "]. Verifica el nombre del socio en Empresas y Socios.")),
  list(es("tablas.ajustes.aviso.politica_no_encontrada"),
       paste0("Política no encontrada en el catálogo (identificador inválido). ",
              "Edita el socio, elimina la política marcada con '?' y reasígnala.")),
  list(es("tablas.ajustes.aviso.sin_datos_ar"),
       paste0("Sin fechas: hay socios con CxC, pero no hay datos de CxC cargados. ",
              "Verifica que los datos de Cobrar estén disponibles.")),
  list(es("tablas.ajustes.aviso.sin_fechas"), "No se generaron fechas (sin documentos con socio asignado)."),
  # "Políticas aplicadas", built in a variable (textos-codigo): the old sprintf, scope by scope
  list(es("tablas.ajustes.aviso.politicas_aplicadas", alcance = es("tablas.ajustes.aviso.alcance_nuevos"),
          documentos = "1234", socios = "7"),
       sprintf("Políticas aplicadas (%s): %d documentos calculados (%d socios).", "nuevos", 1234L, 7L)),
  list(es("tablas.ajustes.aviso.alcance_sin_modificar"), "sin modificar"),
  list(paste0(es("tablas.ajustes.aviso.politicas_aplicadas", alcance = es("tablas.ajustes.aviso.alcance_todos"),
                 documentos = "0", socios = "0"),
              es("tablas.ajustes.aviso.politicas_sin_documentos", partes = "acme, beta…")),
       paste0(sprintf("Políticas aplicadas (%s): %d documentos calculados (%d socios).", "todos", 0L, 0L),
              sprintf(" Sin documentos: %s", "acme, beta…"))))
distintos <- avisos_distintos(iguales)
ok(sprintf("el español de los %d avisos dice exactamente lo mismo que antes", length(iguales)),
   { for (p in distintos) message("  ahora: ", p[[1]], "\n  antes: ", p[[2]]); !length(distintos) })

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
