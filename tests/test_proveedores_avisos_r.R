# =============================================================================
# tests/test_proveedores_avisos_r.R
# The notices of R/proveedores_module.R (move to inactive, delete,
# reactivate, the Name check) come from the dictionary, not fixed text
# (avisos-varios).
#   1. no showNotification() in proveedores_module.R carries fixed text;
#   2. every tablas.proveedores.aviso.* key it uses exists in es and in en;
#   3. the Spanish reads EXACTLY as before, built the way the old code built it.
#
# Run from project root:  Rscript tests/test_proveedores_avisos_r.R
# =============================================================================
cat("=== test_proveedores_avisos_r ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
suppressMessages(source("R/i18n.R"))
source("tests/helpers/avisos_guardiana.R")
F <- "R/proveedores_module.R"; PRE <- "tablas.proveedores.aviso."

fijos <- avisos_fijos(F)
ok(sprintf("ningún aviso de proveedores_module.R con texto fijo (quedan %d)", length(fijos)),
   { if (length(fijos)) message("  fijos: ", paste(utils::head(fijos, 5), collapse = " | ")); !length(fijos) })

claves <- avisos_claves(F, PRE)
ok(sprintf("las %d claves %s* existen en es y en", length(claves), PRE),
   length(claves) == 7L && all(vapply(claves, clave_en_es_y_en, logical(1))))

es <- function(k, ...) tr(paste0(PRE, k), ..., idioma = "es")
n <- 1234L   # with 4 digits: no thousands separator, as paste0 never put one
iguales <- list(
  list(es("movido_inactivos"), "Proveedor movido a inactivos."),
  list(es("eliminado"), "Proveedor eliminado."),
  list(es("inactivar_error"), "No se pudieron inactivar los proveedores. Verifica la conexión e intenta de nuevo."),
  list(es("inactivados", n = as.character(n)), paste0(n, " proveedor(es) inactivados.")),
  list(es("nombre_obligatorio"), "El Nombre es obligatorio."),
  list(es("reactivado"), "Proveedor reactivado."),
  list(es("eliminado_permanente"), "Proveedor eliminado permanentemente."))
dif <- Filter(function(p) !identical(p[[1]], p[[2]]), iguales)
ok(sprintf("el español de los %d avisos es idéntico al de antes", length(iguales)),
   { for (p in dif) message("  ahora: ", p[[1]], "\n  antes: ", p[[2]]); !length(dif) && length(iguales) == length(claves) })

vars <- function(s) sort(regmatches(s, gregexpr("\\{[a-z_]+\\}", s))[[1]])
ok("el inglés está traducido y lleva las mismas {variables}",
   all(vapply(claves, function(k) { a <- i18n_dict("es")[[k]]; b <- i18n_dict("en")[[k]]
     !identical(a, b) && identical(vars(a), vars(b)) }, logical(1))))
ok("el número va como texto (sin separador de miles)",
   grepl('tr("tablas.proveedores.aviso.inactivados", n = as.character(', paste(readLines(F, warn = FALSE), collapse = "\n"), fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
