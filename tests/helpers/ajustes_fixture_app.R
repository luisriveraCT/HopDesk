# =============================================================================
# tests/helpers/ajustes_fixture_app.R
# The real Ajustes modal (show_settings_modal) on the Business Partners panel,
# with Proveedores (catalog, Importar Excel, Inactivos) over the data of
# proveedores_fixture_app.R (no S3; saves land in #registro). #abrir opens it
# (T09: the table window and the Excel review in one scroll).
# =============================================================================
base_app <- source("tests/helpers/proveedores_fixture_app.R")$value   # globals: activos0, inactivos0, registro...
for (f in c("R/settings/settings_hub.R", "R/settings/settings_bp.R")) sys.source(f, envir = globalenv())

ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#0A58CA"),
  shinyjs::useShinyjs(), app_styles(),
  actionButton("abrir", "Ajustes"),
  tags$pre(id = "registro", style = "display:none")
)
server <- function(input, output, session) {
  shared <- list(
    proveedores_db = reactiveVal(activos0), proveedores_inactivos_db = reactiveVal(inactivos0),
    company_map = reactive(c(NTS = "Networks Trucking", NCS = "Networks Crossdocking")),
    effective_client_id = function() "fixture", home_client_id = function() "fixture",
    current_user = function() "luis", usuario = function() "luis")
  proveedoresServer("prov_mod", shared)
  observeEvent(input$abrir, {
    show_settings_modal(input, output, session, shared)
    output$settings_panel <- renderUI(.bp_panel_ui(shared, NULL))
  })
}
app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(getwd(), "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
