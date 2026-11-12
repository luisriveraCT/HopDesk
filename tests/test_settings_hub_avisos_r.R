# =============================================================================
# tests/test_settings_hub_avisos_r.R
# The notice of R/settings/settings_hub.R (a failed save to S3) comes from the
# dictionary (settings-avisos): no fixed text, its tablas.ajustes.aviso.* key
# in es and en, and the Spanish exactly as the old code built it.
# Run from project root:  Rscript tests/test_settings_hub_avisos_r.R
# AVISOS_R=<file> checks another copy of the module (the contraprueba).
# =============================================================================
cat("=== test_settings_hub_avisos_r ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
suppressMessages(source("R/i18n.R"))
source("tests/helpers/avisos_guardiana.R")
F_M <- Sys.getenv("AVISOS_R", "R/settings/settings_hub.R")

fijos <- avisos_fijos(F_M)
ok(sprintf("ningún aviso de settings_hub.R con texto fijo (quedan %d)", length(fijos)),
   { if (length(fijos)) message("  fijos: ", paste(utils::head(fijos, 5), collapse = " | ")); !length(fijos) })
claves <- avisos_claves_n(F_M, "tablas.ajustes.aviso.")
ok(sprintf("las %d claves tablas.ajustes.aviso.* existen en es y en", length(claves)),
   { falta <- avisos_claves_faltan(claves); if (length(falta)) message("  faltan: ", paste(falta, collapse = ", "))
     length(claves) >= 1L && !length(falta) })
iguales <- list(list(tr("tablas.ajustes.aviso.error_guardar_s3", error = "Forbidden", idioma = "es"),
                     paste("❌ Error al guardar en el servidor:", "Forbidden")),
                # the intercompany save, an if/else in a variable (textos-codigo); "S3" ->
                # "el servidor" is the wording Luis approved in textos-visibles
                list(tr("tablas.ajustes.aviso.ic_guardada_prefijos", idioma = "es"),
                     "✅ Guardado. Se eliminaron prefijos de letra automáticamente."),
                list(tr("tablas.ajustes.aviso.ic_guardada", idioma = "es"),
                     "✅ Configuración intercompany guardada en el servidor."))
distintos <- avisos_distintos(iguales)
ok(sprintf("el español de los %d avisos dice exactamente lo mismo que antes", length(iguales)),
   { for (p in distintos) message("  ahora: ", p[[1]], "\n  antes: ", p[[2]]); !length(distintos) })

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
