# =============================================================================
# tests/helpers/edicion_celda_fixture_app.R
# The piece edicion_celda (T14) on a table shaped like Abono parcial
# (abono_parcial, class A): checkboxes, sections by currency with subtotals,
# groups by company + vendor, search, Excel; Saldo (not summed) and Importe
# (editable up to the row's Saldo, summed). Never touches S3.
#   #otra    another session changes the data: b's Saldo drops to 300, c is
#            gone, e gets locked (paid), and a new row n1 comes in;
#   #limpiar t$limpiar_ediciones() (what the module does after sending);
#   #modal   the same table inside hd_modal() (Esc must not close it while
#            typing);
#   #eco     t$ediciones() and t$seleccion() as JSON, for the test to read.
# EC_N = number of extra plain rows (window mode needs > 800).
# =============================================================================
library(shiny)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R"))
  sys.source(f, envir = globalenv())
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())

EXTRA <- as.integer(Sys.getenv("EC_N", "0"))
filas0 <- local({
  d <- data.frame(
    id         = c("a", "b", "c", "d", "e", "f", "g"),
    Empresa    = c("NTS", "NTS", "NTS", "BOD", "BOD", "NTS", "BOD"),
    Parte      = c("ACEROS", "ACEROS", "ACEROS", "PAPEL", "PAPEL", "LUZ", "GAS"),
    Referencia = c("F-101", "F-102", "F-103", "F-201", "F-202", "F-301", "F-401"),
    Moneda     = c("MXN", "MXN", "MXN", "MXN", "MXN", "USD", "USD"),
    Saldo      = c(1000, 500, 250, 800, 120, 75.5, 2000),
    stringsAsFactors = FALSE)
  d$.bloqueada <- d$id == "g"          # paid: can't be typed in
  if (EXTRA > 0L) d <- rbind(d, data.frame(
    id = sprintf("x%04d", seq_len(EXTRA)), Empresa = "NTS", Parte = sprintf("P%04d", seq_len(EXTRA)),
    Referencia = sprintf("X-%04d", seq_len(EXTRA)), Moneda = "MXN", Saldo = 100, .bloqueada = FALSE,
    stringsAsFactors = FALSE))
  d$Importe <- d$Saldo                  # where the edit starts: the balance, as Abono parcial does today
  d
})

columnas <- list(
  col_empresa("Empresa", "Empresa"),
  col_texto("Parte", "Proveedor"),
  col_texto("Referencia", "Referencia"),
  col_dinero("Saldo", "Saldo", sumar = FALSE),
  col_dinero("Importe", "Importe", editable = list(min = 0, max = "Saldo")))
tabla <- function(id, datos) hd_table_server(id, tabla = "abono_parcial", datos = datos, columnas = columnas,
  piezas = c("casillas", "edicion_celda"), secciones = "Moneda", grupo = c("Empresa", "Parte"))

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  actionButton("otra", "otra sesión"), actionButton("limpiar", "limpiar"), actionButton("modal", "modal"),
  hd_table_ui("t"),
  verbatimTextOutput("eco")
)
server <- function(input, output, session) {
  datos <- reactiveVal(filas0)
  t <- tabla("t", datos)
  m <- tabla("m", datos)
  observeEvent(input$otra, {
    d <- datos()
    d$Saldo[d$id == "b"] <- 300
    d$.bloqueada[d$id == "e"] <- TRUE
    d <- d[d$id != "c", ]
    if (!"n1" %in% d$id) d <- rbind(d, data.frame(id = "n1", Empresa = "NTS", Parte = "NUEVO", Referencia = "F-999", Moneda = "MXN",
                             Saldo = 40, .bloqueada = FALSE, Importe = 40, stringsAsFactors = FALSE))
    attr(d, "vez") <- input$otra   # new data every time (the test sends it twice)
    datos(d)
  })
  observeEvent(input$limpiar, t$limpiar_ediciones())
  observeEvent(input$modal, showModal(hd_modal(titulo = "Abono parcial", hd_table_ui("m"))))
  output$eco <- renderText(as.character(jsonlite::toJSON(list(
    ediciones = t$ediciones(), seleccion = t$seleccion(), m = m$ediciones()), auto_unbox = TRUE)))
}
app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(getwd(), "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
