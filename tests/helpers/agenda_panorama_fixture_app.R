# =============================================================================
# tests/helpers/agenda_panorama_fixture_app.R
# T60: the Agenda de Hoy (pagarHoyServer) and Panorama (panoramaServer) side
# by side, over ONE in-memory Agenda, no S3: tests/helpers/agenda_fixture_app.R
# (its data, stubs and recorders) plus rows due today and ahead, from every
# origin -- ERP, manual, Pasivos, Requisiciones (Luis's REQ-2026-0001, due in
# two days) -- so each figure of Panorama can be held against its twin.
# Two tabs in a navbar: "PH" (the Agenda) and "PAN" (Panorama).
# Nothing reads or writes S3: avisos, preferences and the rest are stubs.
# =============================================================================
raiz <- normalizePath(".", winslash = "/")
invisible(source(file.path(raiz, "tests/helpers/agenda_fixture_app.R"), local = FALSE))
for (f in c("R/tier_registry.R", "R/permisos.R", "R/avisos.R", "R/avisos_alertas_module.R", "R/avisos_correo_vigia.R",
            "R/panorama_datos.R", "R/panorama_publicar_module.R", "R/panorama_module.R"))
  suppressMessages(sys.source(file.path(raiz, f), envir = globalenv()))
source(file.path(raiz, "tests/helpers/ic_filtro_piezas.R"))   # T89: panorama_filas_vigentes() needs the IC filter
# The fixture's stubs again (some files above define the real ones), and
# Panorama's own reads: no avisos, no last visit.
for (n in names(stubs)) assign(n, stubs[[n]], envir = globalenv())
assign("avisos_visibles", function(...) NULL, envir = globalenv())
assign("prefs_leer", function(...) NULL, envir = globalenv())
assign("prefs_guardar", function(...) invisible(NULL), envir = globalenv())
# Publicar's "Alertas del equipo" reads the client's users and its matrix from
# S3 (alertas_usuarios() -> auth_load_usuarios(); alertas_leer() ->
# leer_tabla_para_escribir()): here, no users and nobody marked
# (test_fixtures_completas).
assign("auth_load_usuarios", function(...) data.frame(username = character(), tier = character(),
  deleted = logical(), activo = logical(), stringsAsFactors = FALSE), envir = globalenv())
assign("alertas_leer", function(client_id) .schema_avisos_alertas(), envir = globalenv())

HOY_FIX <- as.Date(Sys.time(), tz = "America/Mexico_City")   # Panorama's own day (its tz)
mas <- dplyr::bind_rows(
  dplyr::mutate(pago("ERP-HOY", "ACEROS MONTERREY SA", "5001", 1000, HOY_FIX)),
  dplyr::mutate(pago("MAN-ANT", "BODEGAS DEL NORTE SA", "5002", 250, HOY_FIX + 5), source = "manual"),
  dplyr::mutate(pago("PAS-ANT", "ARRENDADORA SA", "5003", 640, HOY_FIX + 3), source = "provision",
                provision_id = "prov-pasivo"),
  dplyr::mutate(pago("REQ-ANT", "AMAZON", "REQ-2026-0001", 2000, HOY_FIX + 2), source = "provision",
                provision_id = "prov-req-2026-0001"),
  pago("USD-ANT", "ACEROS MONTERREY SA", "5005", 75, HOY_FIX + 9, moneda = "USD"),
  dplyr::mutate(pago("CONF", "ACEROS MONTERREY SA", "5006", 99999, HOY_FIX), status = "confirmed"))
AGENDA(dplyr::bind_rows(agenda0, mas))

usuario_fix <- list(user = "luis", tier = niveles_cliente()[1], permisos = permisos_resueltos(niveles_cliente()[1]))
ui <- bslib::page_navbar(
  id = "nav", title = "T60",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#0A58CA"),
  header = tagList(shinyjs::useShinyjs(), app_styles(), tags$head(tags$script(src = "i18n.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "hd_marca.css?v=1"),
    tags$link(rel = "stylesheet", type = "text/css", href = "panorama.css?v=1"))),
  bslib::nav_panel("Panorama", value = "PAN", panoramaUI("pan")),
  bslib::nav_panel("Agenda", value = "PH", pagarHoyUI("ph"), tags$pre(id = "registro")))

server <- function(input, output, session) {
  session$userData$anota <- function(...) invisible(NULL)
  shared <- list(
    company_map = reactiveVal(c(NTS = EMP, OTR = OTR)), proveedores_db = reactiveVal(catalogo),
    parte_alias_map_db = reactiveVal(NULL), pagar_hoy_db = AGENDA,
    sap_data = reactiveVal(NULL), sap_ov_db = reactiveVal(NULL), manual_inv = reactiveVal(NULL),
    bancos_confirmados = reactiveVal(tibble::tibble()), abonos_db = reactiveVal(NULL),
    effective_client_id = function() "dev", current_user = function() "luis", home_client_id = function() "dev",
    usuario = reactiveVal(usuario_fix), current_user_info = function() list(user = "luis", is_staff = FALSE),
    tier_efectivo = function() usuario_fix$tier, moneda_base_cliente = function() list(moneda = "MXN"),
    df_combined_AR = function() NULL, pasivos_provisions_db = reactiveVal(NULL),
    sap_snapshot_info = function() NULL, fx_fix = function() NULL)
  pagarHoyServer("ph", shared)
  panoramaServer("pan", shared, activa = reactive(identical(input$nav, "PAN")))
}

app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(raiz, "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
