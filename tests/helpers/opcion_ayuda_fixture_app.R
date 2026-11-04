# =============================================================================
# tests/helpers/opcion_ayuda_fixture_app.R
# The two small engine pieces of T18 §6, never S3:
#   #c  a table shaped like the conflicts of a Pasivos edit (conflictos_pasivo,
#       class E): Antes, Nuevo, the choice (opcion_fila: Nuevo / Mantener /
#       Otro, the suggested amount in Nuevo's bubble) and Importe
#       (edicion_celda; typing picks "Otro"). p3 is locked.
#       #todos   t$poner_opcion("mantener") (what "Aplicar todos: Mantener" does)
#       #otra    new data: p1 is gone and p2's data now says "aplicar"
#   #f  a table with a filter whose options have help (filtros$ayuda)
#   #eco t$opciones() and t$ediciones() as JSON
# =============================================================================
library(shiny)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R"))
  sys.source(f, envir = globalenv())
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())

conf0 <- data.frame(id = c("p1", "p2", "p3", "p4"), fecha = as.Date("2026-12-01") + c(0, 31, 59, 90),
                    antes = c(100, 200, 300, 400), nuevo = c(150, 250, 350, 450), importe = 0,
                    decision = c("mantener", "mantener", "aplicar", "mantener"),
                    .bloqueada = c(FALSE, FALSE, TRUE, FALSE), stringsAsFactors = FALSE)
cols_conf <- list(
  col_fecha("fecha", "Fecha"),
  col_dinero("antes", "Antes", sumar = FALSE),
  col_dinero("nuevo", "Nuevo (sugerido)", sumar = FALSE),
  col_opcion("decision", "Decisión", opciones = c(aplicar = "Nuevo", mantener = "Mantener", personalizar = "Otro"),
             al_escribir = "personalizar", burbuja = c(aplicar = "nuevo")),
  col_dinero("importe", "Importe", editable = list(min = 0)))

lista <- data.frame(id = c("l1", "l2", "l3"), Nombre = c("Renta", "Crédito", "Amex"),
                    Categoria = c("regular", "financiero", "tarjeta"), stringsAsFactors = FALSE)

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  actionButton("todos", "todos: mantener"), actionButton("otra", "otra sesión"),
  hd_table_ui("c"),
  hd_table_ui("f"),
  verbatimTextOutput("eco")
)
server <- function(input, output, session) {
  datos <- reactiveVal(conf0)
  t <- hd_table_server("c", tabla = "conflictos_pasivo", datos = datos, columnas = cols_conf,
                       piezas = c("casillas", "opcion_fila", "edicion_celda"))
  hd_table_server("f", tabla = "lista_pasivos", datos = reactive(lista),
                  columnas = list(col_texto("Nombre"), col_texto("Categoria")),
                  filtros = list(list(campo = "Categoria", titulo = "Categoría",
                                      ayuda = c(regular = "tablas.vacio", financiero = "tablas.vacio_filtro"))))
  observeEvent(input$todos, t$poner_opcion("mantener"))
  observeEvent(input$otra, {
    d <- datos()[datos()$id != "p1", ]
    d$decision[d$id == "p2"] <- "aplicar"
    datos(d)
  })
  output$eco <- renderText(as.character(jsonlite::toJSON(list(opciones = t$opciones(), ediciones = t$ediciones()),
                                                         auto_unbox = TRUE)))
}
app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(getwd(), "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
