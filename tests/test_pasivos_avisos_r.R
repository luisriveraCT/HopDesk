# =============================================================================
# tests/test_pasivos_avisos_r.R
# The notices of R/pasivos_module.R (provisions: convert, delete, edit,
# manual creation, undo a conversion) come from the dictionary
# (pasivos-avisos), not fixed text:
#   1. no showNotification() carries fixed text -- this module calls it as
#      shiny::showNotification(), which the common detector sees;
#   2. every tablas.pasivos.aviso.* key it uses exists in es and in en
#      (plus tablas.pasivos.manual_creada, reused for "Provisión manual creada.");
#   3. the Spanish reads EXACTLY as before, built as the old code built it.
# Run from project root:  Rscript tests/test_pasivos_avisos_r.R
# AVISOS_R=<file> checks another copy of the module (the contraprueba).
# =============================================================================
cat("=== test_pasivos_avisos_r ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
suppressMessages(source("R/i18n.R"))
source("tests/helpers/avisos_guardiana.R")
F_M <- Sys.getenv("AVISOS_R", "R/pasivos_module.R")
codigo <- paste(readLines(F_M, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# ── 1. No fixed text ─────────────────────────────────────────────────────────
n_avisos <- lengths(regmatches(codigo, gregexpr("shiny::showNotification(", codigo, fixed = TRUE)))
ok(sprintf("el módulo llama shiny::showNotification() (%d veces): lo que este detector debe ver", n_avisos),
   n_avisos >= 37L)
fijos <- avisos_fijos(F_M)
ok(sprintf("ningún aviso de pasivos_module.R con texto fijo (quedan %d)", length(fijos)),
   { if (length(fijos)) message("  fijos: ", paste(utils::head(fijos, 5), collapse = " | ")); !length(fijos) })

# ── 2. The keys exist in es and en ──────────────────────────────────────────
claves <- c(avisos_claves(F_M, "tablas.pasivos.aviso."),
            if (grepl('tr("tablas.pasivos.manual_creada")', codigo, fixed = TRUE)) "tablas.pasivos.manual_creada")
ok(sprintf("las %d claves de los avisos existen en es y en", length(claves)),
   { falta <- claves[!vapply(claves, clave_en_es_y_en, logical(1))]
     if (length(falta)) message("  faltan: ", paste(falta, collapse = ", "))
     length(claves) >= 19L && !length(falta) })

# ── 3. The Spanish reads exactly as before ──────────────────────────────────
es <- function(k, ...) tr(k, ..., idioma = "es")
iguales <- list(
  list(es("tablas.pasivos.aviso.sin_permiso_convertir"), "No tienes permiso para convertir provisiones."),
  list(es("tablas.pasivos.aviso.error_cargar"), "Error cargando provisiones."),
  list(es("tablas.pasivos.aviso.no_encontrada"), "Provisión no encontrada."),
  list(es("tablas.pasivos.aviso.no_disponible_conversion"), "Esta provisión ya no está disponible para conversión."),
  list(es("tablas.pasivos.aviso.solo_provisional"), "Solo se pueden eliminar provisiones en estado 'provisional'."),
  list(es("tablas.pasivos.aviso.error", error = "S3"), paste0("Error: ", "S3")),
  list(es("tablas.pasivos.aviso.error", error = "S3"), paste("Error:", "S3")),
  list(es("tablas.pasivos.aviso.eliminada"), "Provisión eliminada."),
  list(es("tablas.pasivos.aviso.actualizada"), "Provisión actualizada."),
  list(es("tablas.pasivos.aviso.sin_permiso"), "Sin permiso."),
  list(es("tablas.pasivos.aviso.convertida"), "Provisión convertida a comprobante."),
  list(es("tablas.pasivos.aviso.convertida_agenda"), "Provisión convertida a comprobante e ingresada a Agenda de hoy."),
  list(es("tablas.pasivos.aviso.sin_permiso_crear"), "No tienes permiso para crear provisiones."),
  list(es("tablas.pasivos.aviso.error_guardar", error = "S3"), paste0("Error al guardar la provisión: ", "S3")),
  list(es("tablas.pasivos.manual_creada"), "Provisión manual creada."),
  list(es("tablas.pasivos.aviso.no_revertible"), "La provisión no puede ser revertida en este estado."),
  list(es("tablas.pasivos.aviso.error_manual_inv", error = "S3"), paste("Error al guardar la entrada manual:", "S3")),
  list(es("tablas.pasivos.aviso.error_agenda", error = "S3"), paste("Error al guardar agenda:", "S3")),
  list(es("tablas.pasivos.aviso.conversion_deshecha"), "Conversión deshecha. La provisión fue restaurada."),
  list(es("tablas.pasivos.aviso.eliminados_ambos"), "Provisión y comprobante eliminados."))
distintos <- Filter(function(p) !identical(p[[1]], p[[2]]), iguales)
ok(sprintf("el español de los %d avisos dice exactamente lo mismo que antes", length(iguales)),
   { for (p in distintos) message("  ahora: ", p[[1]], "\n  antes: ", p[[2]]); !length(distintos) })

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
