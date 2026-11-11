# =============================================================================
# tests/test_agenda_avisos_r.R
# The notices of R/pagar_hoy_module.R (the Agenda: payment file, remove,
# confirm, partial payments, manual entries, supplier links, ZIP, balances)
# come from the dictionary, not fixed text (avisos-varios).
#   1. no showNotification() in pagar_hoy_module.R carries fixed text;
#   2. every tablas.agenda.aviso.* key it uses exists in es and in en;
#   3. the Spanish reads EXACTLY as before, built the way the old code built it.
#
# Run from project root:  Rscript tests/test_agenda_avisos_r.R
# =============================================================================
cat("=== test_agenda_avisos_r ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
suppressMessages(source("R/i18n.R"))
source("tests/helpers/avisos_guardiana.R")
F <- "R/pagar_hoy_module.R"; PRE <- "tablas.agenda.aviso."

fijos <- avisos_fijos(F)
ok(sprintf("ningún aviso de pagar_hoy_module.R con texto fijo (quedan %d)", length(fijos)),
   { if (length(fijos)) message("  fijos: ", paste(utils::head(fijos, 5), collapse = " | ")); !length(fijos) })

claves <- avisos_claves(F, PRE)
ok(sprintf("las %d claves %s* existen en es y en", length(claves), PRE),
   length(claves) == 23L && all(vapply(claves, clave_en_es_y_en, logical(1))))

es <- function(k, ...) tr(paste0(PRE, k), ..., idioma = "es")
cur_sel <- "EUR"; n <- 1234L; err <- "sin red"; parte <- "ACME SA"; ali <- "ACME"
iguales <- list(
  list(es("moneda_no_soportada", moneda = cur_sel),
       paste0("Solo MXN y USD están soportados para el archivo de pagos. Moneda: ", cur_sel, ".")),
  list(es("selecciona_facturas_primero"), "Selecciona facturas primero."),
  list(es("selecciona_facturas"), "Selecciona facturas."),
  list(es("selecciona_pago_quitar"), "Selecciona al menos una fila en la tabla de pagos para quitar."),
  list(es("selecciona_cobro_quitar"), "Selecciona al menos una fila en la tabla de cobros para quitar."),
  list(es("no_quitado", n = as.character(n)),
       paste0("No se pudo quitar de la Agenda tras ", n,
              " intento(s) (posible problema de conexión). Actualiza la página e intenta de nuevo.")),
  list(es("quitado_provision_error", error = err),
       paste0("Pago(s) quitado(s) correctamente, pero algo falló al revertir la provisión relacionada: ", err)),
  list(es("pagos_quitados", n = as.character(n)), paste0(n, " pago(s) quitado(s).")),
  list(es("cobros_quitados", n = as.character(n)), paste0(n, " cobro(s) quitado(s).")),
  list(es("confirmada_agenda_no", n = as.character(n)),
       paste0("La confirmación se registró, pero no se pudo actualizar la Agenda tras ",
              n, " intento(s) (posible problema de conexión). ",
              "Actualiza la página; si el ítem sigue en Agenda, vuelve a confirmarlo.")),
  list(es("error_abono", error = err), paste("Error al guardar abono:", err)),
  list(es("error_archivar_manual", error = err), paste("Error al archivar entrada manual:", err)),
  list(es("error_eliminar_manual", error = err), paste("Error al eliminar entrada manual:", err)),
  list(es("pagos_confirmados_error", error = err),
       paste0("Pago(s) confirmado(s) correctamente, pero algo falló al procesar ",
              "abonos/archivado relacionado: ", err)),
  list(es("cobros_confirmados_error", error = err),
       paste0("Cobro(s) confirmado(s) correctamente, pero algo falló al procesar ",
              "abonos/archivado relacionado: ", err)),
  list(es("vinculado", parte = parte, alias = ali), paste0("✓ Vinculado: \"", parte, "\" → ", ali)),
  list(es("vinculo_eliminado", parte = parte), paste0("Vínculo eliminado para: ", parte)),
  list(es("zip_sin_actividad", error = err),
       paste("Aviso: el ZIP se generó correctamente, pero no se pudo registrar el resumen en Actividad:", err)),
  list(es("vaciada"), "Agenda vaciada."),
  list(es("no_guardado"), "No se pudo guardar. Verifica la conexión e intenta de nuevo."),
  list(es("sin_cuentas"), "No hay cuentas registradas en Configuración > Cuentas."),
  list(es("sin_cuentas_activas"), "No hay cuentas activas en Configuración > Cuentas."),
  list(es("saldos_cargados", n = as.character(n)),
       paste0("Saldos cargados desde Bancos: ", n, " cuenta(s) actualizada(s).")))
dif <- Filter(function(p) !identical(p[[1]], p[[2]]), iguales)
ok(sprintf("el español de los %d avisos es idéntico al de antes", length(iguales)),
   { for (p in dif) message("  ahora: ", p[[1]], "\n  antes: ", p[[2]]); !length(dif) && length(iguales) == length(claves) })

vars <- function(s) sort(regmatches(s, gregexpr("\\{[a-z_]+\\}", s))[[1]])
ok("el inglés está traducido y lleva las mismas {variables}",
   all(vapply(claves, function(k) { a <- i18n_dict("es")[[k]]; b <- i18n_dict("en")[[k]]
     !identical(a, b) && identical(vars(a), vars(b)) }, logical(1))))
codigo <- paste(readLines(F, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("los números van como texto (sin separador de miles)",
   !grepl('tr\\("tablas\\.agenda\\.aviso\\.[a-z_]+", n = (?!as\\.character\\()', codigo, perl = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
