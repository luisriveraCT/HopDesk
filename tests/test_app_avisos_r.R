# =============================================================================
# tests/test_app_avisos_r.R
# The notices of app.R (password change, the ERP load, the S3 read, the
# manual entry of Agregar Item) come from the dictionary, not fixed text
# (avisos-integrador, with the integrator's permission: text only).
#   1. no showNotification() in app.R carries fixed text (if/else included);
#   2. every key it uses for them exists in es and in en;
#   3. the Spanish reads EXACTLY as before, built the way the old code built it.
#
# Run from project root:  Rscript tests/test_app_avisos_r.R
# =============================================================================
cat("=== test_app_avisos_r ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
suppressMessages(source("R/i18n.R"))
source("tests/helpers/avisos_guardiana.R")
F <- "app.R"

fijos <- avisos_fijos(F)
ok(sprintf("ningún aviso de app.R con texto fijo (quedan %d)", length(fijos)),
   { if (length(fijos)) message("  fijos: ", paste(utils::head(fijos, 5), collapse = " | ")); !length(fijos) })

claves <- c(avisos_claves(F, "tablas.datos_sap.aviso."), avisos_claves(F, "tablas.manual.aviso."),
            "niveles.usuarios.aviso.contrasena_actualizada", "comun.aviso.error_leer_s3")
ok(sprintf("las %d claves de estos avisos existen en es y en", length(claves)),
   length(claves) == 12L && all(vapply(claves, clave_en_es_y_en, logical(1))))
codigo <- paste(readLines(F, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("app.R usa las dos claves sueltas (contraseña y lectura de S3)",
   grepl('tr("niveles.usuarios.aviso.contrasena_actualizada")', codigo, fixed = TRUE) &&
   grepl('tr("comun.aviso.error_leer_s3"', codigo, fixed = TRUE))

es <- function(k, ...) tr(k, ..., idioma = "es")
ledger_name <- "AR"; err <- "timeout"; lbl <- "11 Nov 2026 09:30"
iguales <- list(
  list(es("niveles.usuarios.aviso.contrasena_actualizada"), "Contraseña actualizada. Bienvenido."),
  list(es("tablas.datos_sap.aviso.conectando"), "Conectando al ERP…"),
  list(es("tablas.datos_sap.aviso.error_ledger", ledger = ledger_name, error = err), paste("SAP", ledger_name, ":", err)),
  list(es("tablas.datos_sap.aviso.respaldo", ledger = ledger_name, fecha = lbl),
       paste0("SAP no disponible — ", ledger_name, " mostrando datos del ", lbl)),
  list(es("tablas.datos_sap.aviso.actualizados"), "Datos actualizados."),
  # "S3" -> "el servidor": the wording Luis approved in textos-visibles (d321e67), not the old text
  list(es("comun.aviso.error_leer_s3", error = err), paste0("Error al leer desde el servidor: ", err)),
  list(es("tablas.manual.aviso.documento_obligatorio"), "El campo Documento es obligatorio."),
  list(es("tablas.manual.aviso.no_guardada"), "No se pudieron guardar los cambios."),
  list(es("tablas.manual.aviso.actualizada"), "Entrada actualizada."),
  list(es("tablas.manual.aviso.error_agenda", error = err), paste("No se pudo guardar en Agenda:", err)),
  list(es("tablas.manual.aviso.agregada_agenda"), "Entrada agregada y enviada a Agenda de hoy."),
  list(es("tablas.manual.aviso.agregada"), "Entrada agregada."))
dif <- avisos_distintos(iguales)
ok(sprintf("el español de los %d textos es idéntico al de antes", length(iguales)),
   { for (p in dif) message("  ahora: ", p[[1]], "\n  antes: ", p[[2]]); !length(dif) && length(iguales) == length(claves) })

vars <- function(s) sort(regmatches(s, gregexpr("\\{[a-z_]+\\}", s))[[1]])
ok("el inglés está traducido y lleva las mismas {variables}",
   all(vapply(claves, function(k) { a <- i18n_dict("es")[[k]]; b <- i18n_dict("en")[[k]]
     !identical(a, b) && identical(vars(a), vars(b)) }, logical(1))))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
