# =============================================================================
# tests/helpers/aprox_fixture_app.R
# The table engine alone (hd_table, the real one) with cells that carry the
# accion_celda companions .marca and .aprox at once -- the case the real
# Pasivos table rarely builds (a mark is a receipt's, "≈" a provision's) --
# for tests/test_hd_aprox_chromote.R (banxico-fix). Rows:
#   a: mark pendiente + "≈" with its help        b: only "≈"
#   c: only the mark                              d: neither
#   e: "≈" whose help carries HTML (must stay text)
# =============================================================================
library(shiny)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
    eval(e, envir = globalenv())
cargar("R/global.R", c("strip_accents", "fmt_money", "CURRENCIES"))
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R")) source(f)
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())

AYUDA <- "Aproximado: convertido con el FIX del 12/11/2026 (18.0000)."
d <- data.frame(id = c("a", "b", "c", "d", "e"), Parte = c("Ambas", "Solo aprox", "Solo marca", "Ninguna", "HTML"),
                Moneda = "MXN", c1 = c(1000, 2000, 3000, 4000, 5000), stringsAsFactors = FALSE, check.names = FALSE)
d[["c1.id"]]    <- c("pa", "pb", "pc", "pd", "pe")
d[["c1.marca"]] <- c("pendiente", "", "pendiente", "", "")
d[["c1.aprox"]] <- c(AYUDA, AYUDA, "", "", "<b>negrita</b> <img src=x onerror=alert(1)>")
d[["c1.boton"]] <- ""
d[["c1.n"]]     <- 1L
d[["c1.mano"]]  <- FALSE

ui <- fluidPage(hd_table_deps(), hd_table_ui("t"))
server <- function(input, output, session) {
  hd_table_server("t", tabla = "pasivos_tabla", datos = reactive(d),
                  columnas = reactive(list(col_texto("Parte", "Parte"), col_dinero("c1", "Monto", accion = TRUE))),
                  piezas = c("accion_celda"), accion_celda = list(clic = "editar_celda"))
}
app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(getwd(), "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
