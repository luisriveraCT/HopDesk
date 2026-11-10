# =============================================================================
# tests/test_motor_carga_r.R
# T29, the guard without a browser (the behavior is checked in Chrome by
# tests/test_motor_carga_chromote.R): the engine's catch-up for lost first
# messages must keep watching for as long as the page lives -- it used to
# give up after the first container or 5 s, and any table drawn later in a
# renderUI stayed "Cargando…" -- and must ask at most once per container.
#
# Run from project root:  Rscript tests/test_motor_carga_r.R
# =============================================================================
cat("=== test_motor_carga_r ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
js <- paste(readLines("www/hdtable.js", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
bloque <- regmatches(js, regexpr("function pedirPerdidos\\(\\) \\{[\\s\\S]*?\\n  \\}\\n", js, perl = TRUE))

ok("pedirPerdidos() existe", length(bloque) == 1L)
ok("la vigilancia no se rinde: ni disconnect() ni un plazo de 5 s",
   !grepl("disconnect()", bloque, fixed = TRUE) && !grepl("5000", bloque, fixed = TRUE))
ok("el observador solo mira los nodos agregados (barato)", grepl("addedNodes", bloque, fixed = TRUE))
ok("pide una sola vez por contenedor (marca en el nodo) y solo si no tiene mensaje",
   grepl("el.hdtPedido", js, fixed = TRUE) && grepl("ULTIMO[el.id]", js, fixed = TRUE))
ok("en una carga normal espera al <body> antes de vigilar",
   grepl("DOMContentLoaded", bloque, fixed = TRUE))

deps <- paste(readLines("R/hd_table.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("hd_table_deps() pide el JS nuevo (?v=41 o más), no el que el navegador ya guardó",
   { v <- as.integer(sub(".*hdtable\\.js\\?v=([0-9]+).*", "\\1", deps)); isTRUE(v >= 41L) })

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
