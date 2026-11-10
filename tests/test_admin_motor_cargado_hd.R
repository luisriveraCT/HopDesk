# =============================================================================
# tests/test_admin_motor_cargado_hd.R
# T28 (found in the step-0b screenshots): every table of the Administración
# tabs sits in a renderUI of a tab not shown yet, and each server sends its
# data when the session starts. If the engine's script only arrived with the
# first table drawn (Seguridad, say), the other tabs' first messages came
# before there was a handler, were lost, and those tables stayed
# "Cargando…" -- Clientes, Invitaciones and Salto for Principal. The page
# itself must load the engine: tiersUI() carries hd_table_deps().
#
# Run from project root:  Rscript tests/test_admin_motor_cargado_hd.R
# =============================================================================
cat("=== test_admin_motor_cargado_hd ===\n\n")
suppressPackageStartupMessages(library(shiny))
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R"))
  suppressMessages(source(f))
for (e in parse("R/tiers_module.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("tiersUI")))
    eval(e, envir = globalenv())

html <- htmltools::renderTags(tiersUI("tiers"))
ok("la página de Administración carga el motor de tablas desde el inicio (hdtable.js en el <head>)",
   grepl("hdtable.js", html$head, fixed = TRUE))
ok("… y su hoja de estilos", grepl("hdtable.css", html$head, fixed = TRUE))
ok("… sin ninguna tabla todavía: las tablas siguen en las pestañas, que se pintan después",
   !grepl("class=\"hdt", html$html, fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
