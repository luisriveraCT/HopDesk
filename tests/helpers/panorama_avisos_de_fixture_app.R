# =============================================================================
# tests/helpers/panorama_avisos_de_fixture_app.R
# T68: the real Panorama with no S3 and a client of many people (T68_N, 340
# by default), for "De quién recibo avisos": tiers and job titles to group
# by, preferences in memory -- what the panel saves (prefs_reemplazar) is
# written to #registro as JSON. T68_NIVEL: the reader's tier (the client's
# top one by default; "compras" for a buyer). T68_STAFF = 1: HopDesk staff
# inside the client (sees, never saves). Based on panorama_req_fixture_app.R.
# =============================================================================
library(shiny)
raiz <- normalizePath(".", winslash = "/")
assign("%||%", function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x,
       envir = globalenv())
for (f in c("R/i18n.R", "R/monedas.R", "R/ui_components.R", "R/tier_registry.R", "R/permisos.R", "R/avisos.R",
            "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R", "R/avisos_alertas_module.R",
            "R/panorama_datos.R", "R/panorama_publicar_module.R", "R/panorama_module.R"))
  suppressMessages(sys.source(file.path(raiz, f), envir = globalenv()))
assign(".i18n_dir", file.path(raiz, "www", "i18n"), envir = globalenv())

TOP <- niveles_cliente()[1]
NIVEL <- Sys.getenv("T68_NIVEL", TOP)
N <- as.integer(Sys.getenv("T68_N", "340"))
STAFF <- identical(Sys.getenv("T68_STAFF"), "1")
ahora <- as.POSIXct(Sys.time(), tz = "UTC")
aviso <- function(id, tipo, titulo, clave = NA_character_, clase = NA_character_, min = 5)
  tibble::tibble(id = id, tipo = tipo, clase = clase, titulo = titulo, cuerpo = "NCS", severidad = "info",
                 niveles = '["x"]', usuarios = '["luis"]', permiso = NA_character_, clave = clave, contador = 1L,
                 enlace = "REQ", creado_por = "ana", creado = ahora - min * 60, actualizado = ahora - min * 60,
                 vence = as.POSIXct(NA, tz = "UTC"), retirado = FALSE, retirado_por = NA_character_,
                 retirado_en = as.POSIXct(NA, tz = "UTC"), hechos = "[]")
FEED <- dplyr::bind_rows(
  aviso("a1", "personal", "ana creó REQ-2026-0001 · Cable para la bodega", "requisiciones|h1|estatus|luis", min = 3),
  aviso("a2", "personal", "Comentario nuevo en REQ-2026-0001", "req:r1|luis", min = 8),
  aviso("a3", "automatico", "beto confirmó un pago", "act|confirmar_pago|beto", min = 12),
  aviso("a4", "publicado", "Cierre de mes el viernes", clase = "comunicado", min = 30))
USUARIOS <- data.frame(username = c("luis", "ana", "beto", sprintf("p%04d", seq_len(N))),
                       account_code = c("U0001", "U0002", "U0003", sprintf("P%04d", seq_len(N))),
                       display_name = c("Luis", "Ana Ruiz", "Beto Paz", sprintf("Persona %04d", seq_len(N))),
                       tier = c(NIVEL, TOP, TOP, rep(c("admin", "finance", "compras", "analysis"), length.out = N)),
                       puesto = c("Director", "Tesorera", "Contador", rep(c("Planta Norte", "planta  norte", "Oficina", NA), length.out = N)),
                       deleted = FALSE, activo = TRUE, stringsAsFactors = FALSE)
PREFS <- new.env()
assign("avisos_visibles", function(...) FEED, envir = globalenv())
assign("auth_load_usuarios", function(...) USUARIOS, envir = globalenv())
# Publicar's "Alertas del equipo" reads its matrix from S3 (alertas_leer() ->
# leer_tabla_para_escribir()): here, nobody marked (test_fixtures_completas).
assign("alertas_leer", function(client_id) .schema_avisos_alertas(), envir = globalenv())
assign("prefs_leer", function(cid, u) utils::modifyList(PREFS_DEFECTO, PREFS[[tolower(u)]] %||% list()), envir = globalenv())
assign("prefs_reemplazar", function(cid, u, clave, valor, sub = NULL) {
  d <- PREFS[[tolower(u)]] %||% list()
  if (is.null(sub)) d[clave] <- list(valor) else { bl <- d[[clave]] %||% list(); bl[sub] <- list(valor); d[clave] <- list(bl) }
  d$casa_donde <- cid   # which client's preferencias.rds it went to (T68: staff, their home)
  PREFS[[tolower(u)]] <- d
  shiny::getDefaultReactiveDomain()$sendCustomMessage("registro",
    as.character(jsonlite::toJSON(PREFS[[tolower(u)]], auto_unbox = TRUE)))
  invisible(TRUE) }, envir = globalenv())
assign("prefs_guardar", function(cid, u, cambios) {
  PREFS[[tolower(u)]] <- utils::modifyList(PREFS[[tolower(u)]] %||% list(), cambios)
  shiny::getDefaultReactiveDomain()$sendCustomMessage("registro",
    as.character(jsonlite::toJSON(PREFS[[tolower(u)]], auto_unbox = TRUE)))
  invisible(TRUE) }, envir = globalenv())

usuario_fix <- list(user = "luis", tier = NIVEL, permisos = permisos_resueltos(NIVEL))
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5),
  shinyjs::useShinyjs(),
  tags$head(tags$script(src = "i18n.js"), tags$script(src = "panorama.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "hd_marca.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "panorama.css")),
  # As app.R mounts it: the tab's own scrolling box, the window's height.
  div(class = "tab-content", div(class = "tab-pane active",
    # (no h-100 here: its height 100% !important would beat the 100vh)
    div(class = "overflow-auto", style = "height: 100vh;", panoramaUI("pan")))),
  tags$pre(id = "registro"),
  tags$script(HTML("Shiny.addCustomMessageHandler('registro', function (t) { document.getElementById('registro').textContent = t; });")))
server <- function(input, output, session) {
  shared <- list(effective_client_id = function() "dev", usuario = reactiveVal(usuario_fix),
                 current_user_info = function() list(user = "luis", is_staff = STAFF),
                 home_client_id = function() if (STAFF) "hopdesk" else "dev",
                 tier_efectivo = function() TOP, moneda_base_cliente = function() list(moneda = "MXN"),
                 pagar_hoy_db = reactiveVal(NULL), df_combined_AR = function() NULL,
                 pasivos_provisions_db = reactiveVal(NULL), sap_snapshot_info = function() NULL, fx_fix = function() NULL)
  panoramaServer("pan", shared)
}
app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(raiz, "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
