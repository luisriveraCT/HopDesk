# =============================================================================
# tests/helpers/pasivos_tabla_fixture_app.R
# Pasivos · Tabla (pasivos_table_module_ui/server, the real module) with a
# fake `shared` in memory and tests/helpers/pasivos_datos_prueba.R. Never S3:
# the manual-provision save goes to a stub defined only in this process.
# The global events the table sends (pasivos_cell_click,
# pasivos_convert_request, pasivos_edit_liability, pasivos_add_liability)
# are copied to #eco as JSON: list(evento, valor, n) of the last one.
# =============================================================================
library(shiny)
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
    eval(e, envir = globalenv())
cargar("R/global.R", c("strip_accents", "fmt_money", "CURRENCIES"))
cargar("R/persistence.R", "confirmaciones_vigentes")
cargar("R/pasivos_observers.R", "isTRUE_safe")
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/pasivos_schemas.R", "R/pasivos_table_pivot.R", "R/pasivos_table_styles.R",
            "R/pasivos_table_module.R", "R/pasivos_list_module.R")) source(f)
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())
source("tests/helpers/pasivos_datos_prueba.R")

has_capability <- function(user, cap) TRUE
save_pasivos_provisions <- function(df, client_id, base = NULL) invisible(TRUE)
bump_sync_version <- function(...) invisible(NULL)
pasivos_log_audit <- function(...) invisible(NULL)
find_proveedor_matches <- function(...) NULL

D <- pasivos_datos_prueba()
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  shinyjs::useShinyjs(),
  pasivos_table_module_ui("pt"),
  verbatimTextOutput("eco")
)
server <- function(input, output, session) {
  shared <- list(
    pasivos_provisions_db = reactiveVal(D$provisions), pasivos_liabilities_db = reactiveVal(D$liabilities),
    manual_inv = reactiveVal(D$manual_inv), bancos_confirmados = reactiveVal(D$bancos_confirmados),
    visible_initials = reactiveVal(D$visibles), empresa_sel = reactiveVal(D$visibles),
    proveedores_db = reactiveVal(NULL), current_user = reactiveVal("tester"), effective_client_id = function() "prueba",
    suppress_ledger_prov_refresh = reactiveVal(FALSE))
  pasivos_table_module_server("pt", shared)
  ultimo <- reactiveVal(list(evento = "", valor = "", n = 0L))
  for (ev in c("pasivos_cell_click", "pasivos_convert_request", "pasivos_edit_liability", "pasivos_add_liability")) local({
    e <- ev
    observeEvent(input[[e]], ultimo(list(evento = e, valor = as.character(input[[e]]), n = ultimo()$n + 1L)))
  })
  output$eco <- renderText(as.character(jsonlite::toJSON(ultimo(), auto_unbox = TRUE)))
}
app <- shinyApp(ui, server)
# The engine's assets (www/hdtable.*, the dictionary) from the repo.
local({
  repo_www <- shiny:::staticHandler(file.path(getwd(), "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
