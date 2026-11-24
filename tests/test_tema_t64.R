# =============================================================================
# tests/test_tema_t64.R
# TEMA 6 (T6-4): the forecast chart (R/forecasting_module.R, a base-R plot,
# no CSS) takes its line colour from the registry for the session's theme.
#   1. no hand-written colour is left in the file;
#   2. the line and the points use the same colour, tema_color(
#      "leg-primario-0d6efd", tema_de_sesion()), inside the renderPlot (so the
#      chart redraws when the theme changes);
#   3. Clásico keeps today's #0D6EFD; Marca paints its primario.
# Run from project root:  Rscript tests/test_tema_t64.R
# =============================================================================
cat("=== test_tema_t64 ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
x <- readLines("R/forecasting_module.R", warn = FALSE, encoding = "UTF-8")
s <- paste(x, collapse = "\n")
tema <- new.env(); sys.source("R/tema_registro.R", envir = tema)

ok("1: no hex or rgb() left in R/forecasting_module.R",
   !any(grepl("(?<![&\\w])#[0-9A-Fa-f]{3,8}\\b", x, perl = TRUE)) && !any(grepl("\\brgba?\\(", x)))
plot_ini <- grep("output$series_plot <- shiny::renderPlot({", x, fixed = TRUE)
cuerpo <- paste(x[plot_ini:min(length(x), plot_ini + 40L)], collapse = "\n")
ok("2: the colour is read inside the renderPlot, for the session's theme",
   length(plot_ini) == 1L && grepl('linea <- tema_color("leg-primario-0d6efd", tema_de_sesion())', cuerpo, fixed = TRUE))
ok("2: the line and the points both use it",
   grepl('type = "l", col = linea', cuerpo, fixed = TRUE) && grepl("cex = 0.6, col = linea)", cuerpo, fixed = TRUE))
ok("3: Clásico keeps today's blue (#0D6EFD); Marca paints its primario",
   identical(toupper(tema$tema_color("leg-primario-0d6efd", "clasico")), "#0D6EFD") &&
     identical(toupper(tema$tema_color("leg-primario-0d6efd", "marca")), toupper(tema$tema_color("primario", "marca"))))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
