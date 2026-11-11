# =============================================================================
# tests/test_empresas_avisos_r.R
# The notices of R/empresas_module.R (companies, ERP connections, group
# settings) come from the dictionary, not fixed text (avisos-varios).
#   1. no showNotification() in empresas_module.R carries fixed text;
#   2. every tablas.empresas.aviso.* key it uses exists in es and in en;
#   3. the Spanish reads EXACTLY as before, built the way the old code built
#      it (paste/paste0 of the same pieces).
#
# Run from project root:  Rscript tests/test_empresas_avisos_r.R
# =============================================================================
cat("=== test_empresas_avisos_r ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
suppressMessages(source("R/i18n.R"))
source("tests/helpers/avisos_guardiana.R")
F <- "R/empresas_module.R"; PRE <- "tablas.empresas.aviso."

# ── 1. No fixed text ─────────────────────────────────────────────────────────
fijos <- avisos_fijos(F)
ok(sprintf("ningún aviso de empresas_module.R con texto fijo (quedan %d)", length(fijos)),
   { if (length(fijos)) message("  fijos: ", paste(utils::head(fijos, 5), collapse = " | ")); !length(fijos) })

# ── 2. The keys exist in es and en ──────────────────────────────────────────
claves <- avisos_claves(F, PRE)
ok(sprintf("las %d claves %s* existen en es y en", length(claves), PRE),
   length(claves) == 24L && all(vapply(claves, clave_en_es_y_en, logical(1))))

# ── 3. The Spanish reads exactly as before ──────────────────────────────────
es <- function(k, ...) tr(paste0(PRE, k), ..., idioma = "es")
initials <- "NRS"; account_code <- "E0012"; cascade_err <- "boom"; label <- "SAP N&L"; err <- "sin red"
iguales <- list(
  list(es("iniciales_invalidas"), "Iniciales inválidas (solo letras mayúsculas y números)."),
  list(es("razon_social_obligatoria"), "La Razón Social es obligatoria."),
  list(es("iniciales_existe"), "Ya existe una empresa con esas iniciales."),
  list(es("no_guardada"), "No se pudo guardar la empresa. Verifica la conexión e intenta de nuevo."),
  list(es("creada", iniciales = initials, codigo = account_code),
       paste0("Empresa '", initials, "' (", account_code, ") creada.")),
  list(es("iniciales_otra"), "Ya existe otra empresa con esas iniciales."),
  list(es("error_cascada", error = cascade_err), paste0("Error actualizando tablas relacionadas: ", cascade_err)),
  list(es("cambios_no_guardados"), "No se pudieron guardar los cambios. Intenta de nuevo."),
  list(es("cambio_no_guardado"), "No se pudo guardar el cambio. Intenta de nuevo."),
  list(es("desactivada", iniciales = initials), paste0("Empresa '", initials, "' desactivada.")),
  list(es("erp_sin_permiso"), "No tienes permisos para administrar conexiones ERP."),
  list(es("erp_nombre_obligatorio"), "El nombre es obligatorio."),
  list(es("erp_credenciales_completas"), "Completa todos los campos de credenciales."),
  list(es("erp_credenciales_todas_o_ninguna"),
       "Completa todos los campos de credenciales, o déjalos todos en blanco para no modificarlas."),
  list(es("erp_guardada", nombre = label), paste0("Conexión '", label, "' guardada.")),
  list(es("erp_ya_no_existe"), "Esta conexión ya no existe (fue eliminada o modificada en otra sesión)."),
  list(es("erp_eliminada"), "Conexión eliminada."),
  list(es("erp_restaurada"), "Conexión restaurada. Verifica que los datos sigan siendo correctos antes de usarla."),
  list(es("grupo_nombre_vacio"), "El nombre del grupo no puede estar vacío."),
  list(es("grupo_nombre_largo"), "El nombre del grupo no puede superar 100 caracteres."),
  list(es("logo_formato"), "Formato inválido. Usa PNG o JPG."),
  list(es("logo_grande"), "El logo supera 2 MB. Usa una imagen más pequeña."),
  list(es("grupo_guardado"), "Configuración del grupo guardada correctamente."),
  list(es("error_guardar", error = err), paste("Error al guardar:", err)))
dif <- Filter(function(p) !identical(p[[1]], p[[2]]), iguales)
ok(sprintf("el español de los %d avisos es idéntico al de antes", length(iguales)),
   { for (p in dif) message("  ahora: ", p[[1]], "\n  antes: ", p[[2]]); !length(dif) && length(iguales) == length(claves) })

# The English is a translation, not the Spanish copied, and keeps the same {vars}.
vars <- function(s) sort(regmatches(s, gregexpr("\\{[a-z_]+\\}", s))[[1]])
ok("el inglés está traducido y lleva las mismas {variables}",
   all(vapply(claves, function(k) { a <- i18n_dict("es")[[k]]; b <- i18n_dict("en")[[k]]
     !identical(a, b) && identical(vars(a), vars(b)) }, logical(1))))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
