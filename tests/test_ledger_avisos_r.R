# =============================================================================
# tests/test_ledger_avisos_r.R
# The notices of R/ledger_module.R (Calendario and the day window: move,
# restore, trash, tags, manual entry, take out of the Agenda, failed saves)
# come from the dictionary, not fixed text (PENDIENTES §2, seen in T27).
#   1. no showNotification() in ledger_module.R carries fixed text;
#   2. every tablas.dia.aviso.* key it uses exists in es and in en;
#   3. the Spanish reads EXACTLY as before, built the same way the old code
#      built it (paste/paste0 of the same pieces).
#
# Run from project root:  Rscript tests/test_ledger_avisos_r.R
# =============================================================================
cat("=== test_ledger_avisos_r ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
suppressMessages(source("R/i18n.R"))

# ── 1. No fixed text ─────────────────────────────────────────────────────────
# A showNotification() whose message (its first argument) is a string literal,
# or a paste/paste0/sprintf with a literal holding words, is fixed text.
ex <- parse("R/ledger_module.R", keep.source = FALSE)
fijos <- character(0)
tiene_palabras <- function(x) is.character(x) && length(x) == 1L && grepl("[[:alpha:]]{3,}", x)
recorrer <- function(x) {
  if (!is.call(x)) return(invisible())
  if (identical(x[[1]], as.name("showNotification")) && length(x) >= 2L) {
    m <- x[[2]]
    if (tiene_palabras(m) ||
        (is.call(m) && as.character(m[[1]])[1] %in% c("paste", "paste0", "sprintf") &&
         any(vapply(as.list(m)[-1], tiene_palabras, logical(1)))))
      fijos <<- c(fijos, paste(deparse(m, width.cutoff = 120L)[1]))
  }
  xs <- as.list(x); for (k in seq_along(xs)) if (is.call(xs[[k]])) recorrer(xs[[k]])
}
for (e in ex) recorrer(e)
ok(sprintf("ningún aviso de ledger_module.R con texto fijo (quedan %d)", length(fijos)),
   { if (length(fijos)) message("  fijos: ", paste(utils::head(fijos, 5), collapse = " | ")); !length(fijos) })

# ── 2. The keys exist in es and en ──────────────────────────────────────────
codigo <- paste(readLines("R/ledger_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
claves <- unique(regmatches(codigo, gregexpr("tablas\\.dia\\.aviso\\.[a-z_]+", codigo))[[1]])
ok(sprintf("las %d claves tablas.dia.aviso.* existen en es y en", length(claves)),
   length(claves) >= 20L &&
     all(vapply(claves, function(k) !is.null(i18n_dict("es")[[k]]) && !is.null(i18n_dict("en")[[k]]), logical(1))))

# ── 3. The Spanish reads exactly as before ──────────────────────────────────
es <- function(k, ...) tr(k, ..., idioma = "es")
iguales <- list(
  list(es("tablas.dia.aviso.filtro_empresa"),
       "El filtro de Empresa no coincide con los datos cargados. Verifica la lista de empresas."),
  list(es("tablas.dia.aviso.error_abrir", detalle = "boom"), paste("Error al abrir ventana:", "boom")),
  list(es("tablas.dia.aviso.no_quitado", n = as.character(3L)),
       paste0("No se pudo quitar de la Agenda tras ", 3L,
              " intento(s) (posible problema de conexión). Actualiza la página e intenta de nuevo.")),
  list(es("tablas.dia.aviso.quitado"), "Quitado de la Agenda del día."),
  list(es("tablas.dia.aviso.elige_fecha"), "Elige una fecha válida."),
  list(es("tablas.dia.aviso.solo_provisiones"), "No se encontraron facturas reales (sólo provisiones)."),
  list(es("tablas.dia.aviso.movidas", n = as.character(1234L)), paste0(1234L, " factura(s) movidas.")),
  list(es("tablas.dia.aviso.error_mover", detalle = "x"), paste("Error al mover:", "x")),
  list(es("tablas.dia.aviso.restaurada"), "Fecha original restaurada."),
  list(es("tablas.dia.aviso.error_restaurar", detalle = "x"), paste("Error al restaurar:", "x")),
  list(es("tablas.dia.aviso.sin_facturas"), "No se encontraron facturas."),
  list(es("tablas.dia.aviso.papelera_sap_error", detalle = "S3"),
       paste0("No se pudo guardar la papelera (\"", "S3",
              "\"). Las facturas SAP NO quedaron enviadas a papelera -- vuelve a intentarlo.")),
  list(es("tablas.dia.aviso.papelera_sap_ok", n = as.character(2L)),
       paste0(2L, " factura(s) SAP tachada(s) y enviada(s) a la papelera.")),
  list(es("tablas.dia.aviso.borrado_parcial", detalle = paste(c("a", "b"), collapse = "; ")),
       paste0("No se pudo guardar completamente el borrado (", paste(c("a", "b"), collapse = "; "),
              "). Puede que quede en un estado parcial -- vuelve a intentarlo en unos minutos y ",
              "no cierres la sesion mientras tanto.")),
  list(es("tablas.dia.aviso.papelera_ok", n = as.character(5L)), paste0(5L, " factura(s) enviadas a la papelera.")),
  list(es("tablas.dia.aviso.etiquetas_ok"), "Etiquetas actualizadas."),
  list(es("tablas.dia.aviso.etiquetas_error", detalle = "S3"),
       paste0("No se pudieron guardar las etiquetas (\"", "S3",
              "\"). El cambio se ve en pantalla pero NO quedo guardado -- vuelve a intentarlo.")),
  list(es("tablas.dia.aviso.manual_no_encontrada"), "Entrada manual no encontrada."),
  list(es("tablas.dia.aviso.fecha_invalida"), "Fecha inválida."),
  list(es("tablas.dia.aviso.cambios_guardados"), "Cambios guardados."),
  list(es("tablas.dia.aviso.mover_error_servidor", detalle = "S3"),
       paste0("No se pudo guardar el movimiento de fecha en el servidor (\"", "S3",
              "\"). El cambio se ve en pantalla pero NO quedo guardado -- vuelve a ",
              "intentarlo en unos minutos y NO cierres la sesion mientras tanto.")))
distintos <- Filter(function(p) !identical(p[[1]], p[[2]]), iguales)
ok(sprintf("el español de los %d avisos dice exactamente lo mismo que antes", length(iguales)),
   { for (p in distintos) message("  nuevo: ", p[[1]], "\n  viejo: ", p[[2]]); !length(distintos) })
ok("los números van sin separador de miles, como antes (1234, no 1,234)",
   identical(es("tablas.dia.aviso.movidas", n = as.character(1234L)), "1234 factura(s) movidas."))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
