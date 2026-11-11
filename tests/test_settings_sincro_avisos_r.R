# =============================================================================
# tests/test_settings_sincro_avisos_r.R
# The notices of R/settings/settings_sincro.R (Agenda sync: users list, pick a
# user, on, off) come from the dictionary (settings-avisos): no fixed text,
# every tablas.sincro.aviso.* key in es and en, and the Spanish exactly as the
# old code built it.
# Run from project root:  Rscript tests/test_settings_sincro_avisos_r.R
# AVISOS_R=<file> checks another copy of the module (the contraprueba).
# =============================================================================
cat("=== test_settings_sincro_avisos_r ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
suppressMessages(source("R/i18n.R"))
source("tests/helpers/avisos_guardiana.R")
F_M <- Sys.getenv("AVISOS_R", "R/settings/settings_sincro.R")

fijos <- avisos_fijos(F_M)
ok(sprintf("ningún aviso de settings_sincro.R con texto fijo (quedan %d)", length(fijos)),
   { if (length(fijos)) message("  fijos: ", paste(utils::head(fijos, 5), collapse = " | ")); !length(fijos) })
claves <- avisos_claves_n(F_M, "tablas.sincro.aviso.")
ok(sprintf("las %d claves tablas.sincro.aviso.* existen en es y en", length(claves)),
   { falta <- avisos_claves_faltan(claves); if (length(falta)) message("  faltan: ", paste(falta, collapse = ", "))
     length(claves) >= 4L && !length(falta) })
es <- function(k, ...) tr(k, ..., idioma = "es")
u <- "maria.lopez"
iguales <- list(
  list(es("tablas.sincro.aviso.error_usuarios"), "No se pudo cargar la lista de usuarios."),
  list(es("tablas.sincro.aviso.elige_usuario"), "Selecciona un usuario primero."),
  list(es("tablas.sincro.aviso.activada", usuario = u), paste0("Sincronización activada con la agenda de «", u, "».")),
  list(es("tablas.sincro.aviso.desactivada"), "Sincronización desactivada. Cada cuenta mantiene su propia Agenda de hoy."))
distintos <- avisos_distintos(iguales)
ok(sprintf("el español de los %d avisos dice exactamente lo mismo que antes", length(iguales)),
   { for (p in distintos) message("  ahora: ", p[[1]], "\n  antes: ", p[[2]]); !length(distintos) })

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
