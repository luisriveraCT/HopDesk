# =============================================================================
# tests/test_bancos_avisos_r.R
# The notices of R/bancos_module.R (delete, edit, trash, TXT import, restore,
# undo an abono, the confirmation's trash and recovery, the ↩ back to the
# calendar) come from the dictionary, not fixed text (bancos-avisos).
#   1. no showNotification() in bancos_module.R carries fixed text;
#   2. every tablas.bancos.aviso.* key it uses exists in es and in en
#      (a tr_n() key as its .uno and .otros);
#   3. the Spanish reads EXACTLY as before, built the same way the old code
#      built it (sprintf/paste/paste0 of the same pieces).
#
# Run from project root:  Rscript tests/test_bancos_avisos_r.R
# BANCOS_R=<file> checks another copy of the module (the contraprueba).
# =============================================================================
cat("=== test_bancos_avisos_r ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
suppressMessages(source("R/i18n.R"))
F_BN <- Sys.getenv("BANCOS_R", "R/bancos_module.R")

# ── 1. No fixed text ─────────────────────────────────────────────────────────
# A showNotification() whose message (its first argument) is a string literal,
# or a paste/paste0/sprintf with a literal holding words, is fixed text.
# The common detector (tests/helpers/avisos_guardiana.R): it also sees
# shiny::showNotification(), which the copy that lived here did not.
source("tests/helpers/avisos_guardiana.R")
fijos <- avisos_fijos(F_BN)
ok(sprintf("ningún aviso de bancos_module.R con texto fijo (quedan %d)", length(fijos)),
   { if (length(fijos)) message("  fijos: ", paste(utils::head(fijos, 5), collapse = " | ")); !length(fijos) })

# ── 2. The keys exist in es and en ──────────────────────────────────────────
codigo <- paste(readLines(F_BN, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
usadas <- unique(regmatches(codigo, gregexpr("tablas\\.bancos\\.aviso\\.[a-z_.]+[a-z]", codigo))[[1]])
plurales <- unique(regmatches(codigo, gregexpr('tr_n\\("tablas\\.bancos\\.aviso\\.[a-z_]+"', codigo))[[1]])
plurales <- sub('^tr_n\\("', "", sub('"$', "", plurales))
claves <- c(setdiff(usadas, plurales), paste0(rep(plurales, each = 2L), c(".uno", ".otros")))
existe <- function(k) !is.null(i18n_dict("es")[[k]]) && !is.null(i18n_dict("en")[[k]])
ok(sprintf("las %d claves tablas.bancos.aviso.* existen en es y en", length(claves)),
   { falta <- claves[!vapply(claves, existe, logical(1))]
     if (length(falta)) message("  faltan: ", paste(falta, collapse = ", "))
     length(claves) >= 17L && !length(falta) })

# ── 3. The Spanish reads exactly as before ──────────────────────────────────
es   <- function(k, ...) tr(k, ..., idioma = "es")
es_n <- function(k, n, ...) tr_n(k, as.character(n), ..., idioma = "es")
antes_eliminados <- function(n) sprintf("%d movimiento%s eliminado%s.", n, if (n==1)"" else "s", if (n==1)"" else "s")
antes_papelera   <- function(actual_n) paste0(actual_n, " movimiento", if (actual_n == 1) "" else "s",
                                              " enviado", if (actual_n == 1) "" else "s", " a papelera.")
antes_devuelto   <- function(tipo, fecha_fmt) {
  cal_label <- if (isTRUE(tipo == "cobro")) "CxC" else "CxP"
  paste0("↩ ", if (isTRUE(tipo == "cobro")) "Cobro" else "Pago",
         " devuelto al calendario ", cal_label, " — para el ", fecha_fmt)
}
ahora_devuelto <- function(tipo, fecha_fmt)
  es(if (isTRUE(tipo == "cobro")) "tablas.bancos.aviso.devuelto.cobro" else "tablas.bancos.aviso.devuelto.pago",
     fecha = fecha_fmt)
fecha <- format(as.Date("2026-11-05"), "%d/%m/%Y")
iguales <- list(
  list(es_n("tablas.bancos.aviso.eliminados", 1L), antes_eliminados(1L)),
  list(es_n("tablas.bancos.aviso.eliminados", 3L), antes_eliminados(3L)),
  list(es_n("tablas.bancos.aviso.eliminados", 1234L), antes_eliminados(1234L)),   # no thousands separator
  list(es("tablas.bancos.aviso.solo_manuales"), "Solo se pueden editar movimientos manuales (no importados de TXT)."),
  list(es("tablas.bancos.aviso.ya_no_existe"), "El movimiento ya no existe (pudo haber sido eliminado en otra sesión)."),
  list(es_n("tablas.bancos.aviso.a_papelera", 1L), antes_papelera(1L)),
  list(es_n("tablas.bancos.aviso.a_papelera", 2L), antes_papelera(2L)),
  list(es_n("tablas.bancos.aviso.a_papelera", 0L), antes_papelera(0L)),
  list(es("tablas.bancos.aviso.txt_no_leido"), "No se pudo leer el archivo TXT."),
  list(es("tablas.bancos.aviso.error_parsear", error = "línea 3"), paste("Error al parsear:", "línea 3")),
  list(es("tablas.bancos.aviso.sin_nuevos"), "No hay movimientos nuevos."),
  list(es("tablas.bancos.aviso.restaurado"), "Movimiento restaurado."),
  list(es("tablas.bancos.aviso.abono_deshecho"), "Abono deshecho — el saldo del comprobante se restauró."),
  list(es("tablas.bancos.aviso.confirmacion_papelera"), "Confirmación movida a papelera."),
  list(es("tablas.bancos.aviso.error_guardar"), "Error al guardar. Intenta de nuevo."),
  list(es("tablas.bancos.aviso.ya_recuperada"), "Esta confirmación ya fue recuperada."),
  list(es("tablas.bancos.aviso.error_recuperar", error = "S3"), paste("Error al recuperar la entrada original:", "S3")),
  list(es("tablas.bancos.aviso.error_restaurar_manual", error = "S3"), paste("Error al restaurar entrada manual:", "S3")),
  list(ahora_devuelto("cobro", fecha), antes_devuelto("cobro", fecha)),
  list(ahora_devuelto("pago", fecha), antes_devuelto("pago", fecha)),
  list(ahora_devuelto(NA, fecha), antes_devuelto(NA, fecha)),
  # the if/else of the manual movement's save (avisos-integrador)
  list(es("tablas.bancos.aviso.movimiento_actualizado"), "Movimiento actualizado."),
  list(es("tablas.bancos.aviso.movimiento_guardado"), "Movimiento manual guardado."))
distintos <- Filter(function(p) !identical(p[[1]], p[[2]]), iguales)
ok(sprintf("el español de los %d avisos dice exactamente lo mismo que antes", length(iguales)),
   { for (p in distintos) message("  ahora: ", p[[1]], "\n  antes: ", p[[2]]); !length(distintos) })

# The ↩ notice picks its key as the old code picked "Cobro"/"Pago".
ok("el ↩ elige la clave de cobro o pago igual que antes",
   grepl('tr(if (isTRUE(row$tipo == "cobro")) "tablas.bancos.aviso.devuelto.cobro" else "tablas.bancos.aviso.devuelto.pago"',
         codigo, fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
