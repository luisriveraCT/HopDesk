# =============================================================================
# tests/helpers/pasivos_piezas_fixture_app.R
# The three engine pieces of T17b step 2 on a small table of class F
# (pasivos_tabla), shaped like Pasivos · Tabla but NOT its migration: one row
# per liability, one money column per date; never S3.
#   #t        accion_celda (clic = "editar_celda"), semaforo (.semaforo),
#             sangria (.nivel: r2 is r1's fee); r5 is locked; a cell id with '
#   #ocultar  acciones_ocultas gets "comprobante" (no permission: ↩ goes)
#   #p        the same data, plain columns, none of the pieces' fields
#   #eco      t$accion() as JSON
# =============================================================================
library(shiny)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R"))
  sys.source(f, envir = globalenv())
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())

d0 <- data.frame(
  id = c("r1", "r2", "r3", "r4", "r5"),
  Nombre = c("Renta oficina", "Comisión por apertura", "Crédito Banorte", "Tarjeta Amex", "Luz pagada"),
  Moneda = "MXN",
  d1 = c(1000, 250, 700, 0, 90),   `d1.id` = c("p'1", "p2", "p3", "", "p5"),
  `d1.marca` = c("", "", "pendiente", "", "confirmada"), `d1.boton` = c("provision", "provision", "comprobante", "", "provision"),
  `d1.n` = c(1, 1, 3, 0, 1),
  d2 = c(1100, 0, 710, 300, 0),    `d2.id` = c("p1b", "", "p3b", "p4b", ""),
  `d2.marca` = c("", "", "vencida", "", ""), `d2.boton` = c("provision", "", "comprobante", "provision", ""),
  .semaforo = c("vencida", "", "muy_pronto", "pronto", ""), .nivel = c(0, 1, 0, 0, 0),
  .bloqueada = c(FALSE, FALSE, FALSE, FALSE, TRUE),
  check.names = FALSE, stringsAsFactors = FALSE)

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  actionButton("ocultar", "sin permiso de comprobante"),
  hd_table_ui("t"),
  hd_table_ui("p"),
  verbatimTextOutput("eco")
)
server <- function(input, output, session) {
  ocultas <- reactiveVal(character(0))
  observeEvent(input$ocultar, ocultas("comprobante"))
  t <- hd_table_server("t", tabla = "pasivos_tabla", datos = reactive(d0),
                       columnas = list(col_texto("Nombre", "Pasivo", fija = TRUE),
                                       col_dinero("d1", "1 dic", sumar = FALSE, accion = TRUE),
                                       col_dinero("d2", "2 dic", sumar = FALSE, accion = TRUE)),
                       piezas = c("accion_celda", "sangria"), accion_celda = list(clic = "editar_celda"),
                       acciones_ocultas = ocultas)
  hd_table_server("p", tabla = "flujo_caja", datos = reactive(d0[, c("id", "Nombre", "Moneda", "d1", "d2")]),
                  columnas = list(col_texto("Nombre"), col_dinero("d1", sumar = FALSE), col_dinero("d2", sumar = FALSE)))
  ultima <- reactiveVal(NULL)
  observe({ a <- t$accion(); if (!is.null(a)) ultima(a) })
  output$eco <- renderText(as.character(jsonlite::toJSON(ultima() %||% list(), auto_unbox = TRUE)))
}
app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(getwd(), "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
