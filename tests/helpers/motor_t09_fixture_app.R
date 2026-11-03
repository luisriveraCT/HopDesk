# =============================================================================
# tests/helpers/motor_t09_fixture_app.R
# A small app for two engine changes of T09:
#   - fichas$inicial: the table opens on the "b" chip;
#   - T09_MODO = "renderui": the page has no hd_table at start, so hdtable.js
#     first arrives inside a renderUI, with the session already running (the
#     bug found in T10: the first "hdt-datos" was lost, "Cargando…" forever).
#     #rehacer renders the renderUI again (the script is already there).
# #mas adds a row (a data change). Never touches S3.
# =============================================================================
library(shiny)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R"))
  sys.source(f, envir = globalenv())
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())

MODO <- Sys.getenv("T09_MODO", "directo")
filas0 <- data.frame(id = c("1", "2", "3", "4", "5"), Nombre = c("UNO", "DOS", "TRES", "CUATRO", "CINCO"),
                     Estado = c("a", "b", "a", "b", "c"), stringsAsFactors = FALSE)

tabla_ui <- function() hd_table_ui("t")
ui <- fluidPage(
  actionButton("mas", "más"), actionButton("rehacer", "rehacer"),
  if (identical(MODO, "renderui")) uiOutput("caja") else tabla_ui()
)
server <- function(input, output, session) {
  datos <- reactiveVal(filas0)
  observeEvent(input$mas, {
    d <- datos(); n <- nrow(d) + 1L
    datos(rbind(d, data.frame(id = as.character(n), Nombre = paste("NUEVA", n), Estado = "b", stringsAsFactors = FALSE)))
  })
  if (identical(MODO, "renderui")) output$caja <- renderUI({ input$rehacer; tabla_ui() })
  hd_table_server("t", tabla = "revision_excel_prov", datos = datos,
                  piezas = c("fichas"),
                  columnas = list(col_texto("Nombre", "Nombre"), col_texto("Estado", "Estado")),
                  fichas = list(campo = "Estado", orden = c("a", "b", "c"), inicial = "b"))
}
app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(getwd(), "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
