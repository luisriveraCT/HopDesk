# =============================================================================
# tests/helpers/flujo_piezas_fixture_app.R
# The engine pieces of T17c step 1 on a small table of class F (flujo_caja),
# shaped like the Flujo de caja but NOT its migration; never S3.
#   #t   sections by currency, groups Cobros / Pagos with their sums per
#        column (sumas_columnas, compact, empty at 0), the module's summary
#        rows at each currency's end (filas_resumen: neto, acumulada), and
#        Día 0 typed with arithmetic only on the position rows
#        (edicion_celda, expresion + filas)
#   #p   the same data, plain columns, none of the pieces' fields
#   #eco t$ediciones() as JSON
# =============================================================================
library(shiny)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R"))
  sys.source(f, envir = globalenv())
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())

d0 <- data.frame(
  id = c("a1", "a2", "p1", "neto_mxn", "acum_mxn", "u1", "neto_usd", "acum_usd"),
  Contraparte = c("CLIENTE ALFA", "CLIENTE BETA", "PROVEEDOR UNO", "FLUJO NETO MXN", "POS. ACUMULADA MXN",
                  "CLIENT USA", "FLUJO NETO USD", "POS. ACUMULADA USD"),
  Moneda = c("MXN", "MXN", "MXN", "MXN", "MXN", "USD", "USD", "USD"),
  Tipo = c("Cobros", "Cobros", "Pagos", "", "", "Cobros", "", ""),
  Dia0 = c(NA, NA, NA, NA, 0, NA, NA, 0),   # only the position rows have one
  c1 = c(4495962, 802000, 5000000, 297962, 297962, 10000, 10000, 10000),
  c2 = c(0, 2500, 0, 2500, 300462, 0, 0, 10000),
  .resumen = c("", "", "", "neto", "acumulada", "", "neto", "acumulada"),
  .dia0 = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE),
  stringsAsFactors = FALSE, check.names = FALSE)

cols <- function() list(
  col_texto("Contraparte", "Contraparte", fija = TRUE),
  col_dinero("Dia0", "Día 0", sumar = FALSE, editable = list(expresion = TRUE, filas = ".dia0", min = -1e15)),
  col_dinero("c1", "L 02", formato = "compacto", cero = "vacio", tono = "signo"),
  col_dinero("c2", "M 03", formato = "compacto", cero = "vacio", tono = "signo"))

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  hd_table_ui("t"),
  hd_table_ui("p"),
  verbatimTextOutput("eco")
)
server <- function(input, output, session) {
  t <- hd_table_server("t", tabla = "flujo_caja", datos = reactive(d0), columnas = cols(),
                       piezas = c("sumas_columnas", "filas_resumen", "edicion_celda", "dinero_compacto", "tono_celda", "expandir"),
                       secciones = list(list(campo = "Moneda", orden = c("MXN", "USD"))), grupo = "Tipo")
  hd_table_server("p", tabla = "flujo_caja", datos = reactive(d0[, c("id", "Contraparte", "Moneda", "c1", "c2")]),
                  columnas = list(col_texto("Contraparte"), col_dinero("c1", sumar = FALSE), col_dinero("c2", sumar = FALSE)))
  output$eco <- renderText(as.character(jsonlite::toJSON(t$ediciones(), auto_unbox = TRUE)))
}
app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(getwd(), "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
