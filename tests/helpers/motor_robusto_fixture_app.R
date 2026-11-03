# =============================================================================
# tests/helpers/motor_robusto_fixture_app.R
# The engine never stops the session (incident 2026-11-03: a table's datos()
# failed at startup and the app hung on "Cargando… 14%"). Tables:
#   mala    datos() always fails
#   vuelve  datos() fails until #arreglar, then has rows
#   aviso   good rows, but its aviso() fails
#   espera  datos() is req()'d out (NULL): stays "Cargando…", as before
#   buena   good rows
# #mas bumps a counter (#contador) to show the session still answers.
# log_action is recorded in #registro. Never touches S3.
# =============================================================================
library(shiny)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R"))
  sys.source(f, envir = globalenv())
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())

registro <- reactiveVal(character(0))
assign("log_action", function(user, module, action, description = "", ...)
  registro(c(shiny::isolate(registro()), paste(module, action, description))), envir = globalenv())

filas <- data.frame(id = c("1", "2"), Nombre = c("UNO", "DOS"), stringsAsFactors = FALSE)
cols  <- list(col_texto("Nombre", "Nombre"))
caja  <- function(id) div(style = "margin: 8px 0;", tags$b(id), hd_table_ui(id))

ui <- fluidPage(
  actionButton("mas", "más"), actionButton("arreglar", "arreglar"),
  tags$span(id = "contador_caja", textOutput("contador", inline = TRUE)),
  caja("mala"), caja("vuelve"), caja("aviso"), caja("espera"), caja("buena"), caja("exporta"),
  tags$span(id = "exportado_caja", textOutput("exportado", inline = TRUE)),
  tags$pre(id = "registro", textOutput("registro_txt"))
)
server <- function(input, output, session) {
  n <- reactiveVal(0)
  observeEvent(input$mas, n(n() + 1))
  output$contador <- renderText(n())
  output$registro_txt <- renderText(paste(registro(), collapse = "\n"))
  arreglado <- reactiveVal(FALSE)
  observeEvent(input$arreglar, arreglado(TRUE))
  hd_table_server("mala", tabla = "revision_excel_prov", datos = reactive(rep("x", NULL)), columnas = cols)
  hd_table_server("vuelve", tabla = "revision_excel_prov", columnas = cols,
                  datos = reactive(if (arreglado()) filas else stop("todavía no")))
  hd_table_server("aviso", tabla = "revision_excel_prov", datos = reactive(filas), columnas = cols,
                  aviso = reactive(stop("el aviso truena")))
  hd_table_server("espera", tabla = "revision_excel_prov", datos = reactive(NULL), columnas = cols)
  hd_table_server("buena", tabla = "revision_excel_prov", datos = reactive(filas), columnas = cols)
  # t$exportado() (T12a's audit of the Excel download): #exportado says how
  # many downloads were written and the last one's ids.
  ex <- hd_table_server("exporta", tabla = "proveedores", datos = reactive(filas), columnas = cols)
  n_ex <- reactiveVal(0)
  observeEvent(ex$exportado(), n_ex(n_ex() + 1))
  output$exportado <- renderText({
    e <- ex$exportado()
    if (is.null(e)) "0" else paste(n_ex(), e$formato, paste(e$ids, collapse = ","))
  })
}
app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(getwd(), "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
