# =============================================================================
# tests/helpers/registros_fixture_app.R
# The two audit registers over fixed data, with a simulated `shared` -- no S3:
#   Actividad        auditLogViewerServer("act", mode = "own_client")
#   Bitácora Global  auditLogViewerServer("glo", mode = "multi_client")
#   Archivos de pago pagosAuditViewerServer("pag")
# read_audit_log() and read_pagos_generados() read in-memory tables (the real
# read_audit_log_scoped() still checks who may read what). Every
# log_action() call is listed in #registro as one line:
#   log <action> <target_id> <metadata as k=v;k=v>
# #otra_sesion plays "another session": it writes a new payment file, newest
# of all; #otra_sesion_log writes a new activity row (a-new-<n>). Both happen
# after the panels were drawn: the tables show them after Actualizar.
#
# REG_STAFF = 1: the viewer is Hopdesk staff (sees the jump bookkeeping rows).
# REG_MUCHAS = n: n more activity rows in the last day (window mode > 800).
# REG_RAIZ: the checkout whose R/ and www/ to use (default: this one).
# =============================================================================
library(shiny)
raiz <- normalizePath(Sys.getenv("REG_RAIZ", "."), winslash = "/")
assign("%||%", function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x,
       envir = globalenv())
cargar <- function(archivo, nombres) {
  ex <- parse(file.path(raiz, archivo), keep.source = FALSE)
  for (e in ex)
    if (is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
}
suppressPackageStartupMessages({ library(dplyr); library(shinyjs) })
cargar("R/ui_components.R", "app_styles")
cargar("R/app_audit.R", c(".APP_AUDIT_SCHEMA", "read_audit_log_scoped"))
cargar("R/pagos_generados_persistence.R", c(".schema_pagos_generados", "mask_clabe",
                                             "parse_pago_beneficiarios", "parse_pago_detalle"))
local({
  owd <- setwd(raiz); on.exit(setwd(owd))
  for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
              "R/audit_sticky_ui.R", "R/audit_log_viewer_module.R", "R/pagos_audit_viewer_module.R"))
    if (file.exists(f)) sys.source(f, envir = globalenv())
})
assign(".i18n_dir", file.path(raiz, "www", "i18n"), envir = globalenv())

# ── What the modules reach outside themselves, recorded ─────────────────────
registro <- reactiveVal(character(0))
anotar   <- function(x) registro(c(registro(), x))
staff    <- identical(Sys.getenv("REG_STAFF"), "1")
muchas   <- suppressWarnings(as.integer(Sys.getenv("REG_MUCHAS", "0")))
almacen  <- new.env()
ahora    <- Sys.time()
hace     <- function(dias, horas = 0) ahora - dias * 86400 - horas * 3600

fila_log <- function(id, ts, user, module, action, description, target_id = NA, metadata = NA, cid = "fixture")
  tibble::tibble(id = id, ts = ts, user = user, module = module, action = action, description = description,
                 target_id = target_id, s3_key = NA_character_, client_id = cid, metadata = metadata)
pago_meta <- jsonlite::toJSON(list(empresa = "NETWORKS TRANS", moneda = "MXN", total = 1500.5, n = 1L,
  pagos = list(list(parte = "PROVEEDOR UNO", documento = "F-1001", importe = 1500.5, moneda = "MXN",
                    cuenta_id = "c-1", fecha = "2026-11-01"))), auto_unbox = TRUE)
almacen$logs <- list(
  fixture = dplyr::bind_rows(
    fila_log("a-001", hace(0, 1), "luis",  "auth",      "login",          "Inicio de sesión: luis", "luis"),
    fila_log("a-002", hace(0, 3), "ana",   "pagar_hoy", "confirmar_pago", "Pago confirmado: PROVEEDOR UNO por $1,500.50", "F-1001", as.character(pago_meta)),
    fila_log("a-003", hace(1),    "luis",  "bancos",    "importar_movimientos",
             paste("Importación de movimientos BanBajío: 458 movimientos leídos del archivo, 455 nuevos, 3 duplicados",
                   "descartados y 0 con error; cuenta 030180900012345678, periodo del 1 al 31 de octubre de 2026;",
                   "los 3 duplicados ya estaban en la importación del 2 de noviembre y se dejaron como estaban, sin tocar",
                   "su fecha de registro ni su referencia, para que la conciliación de octubre no cambie"), "s-77"),
    fila_log(NA,      hace(2),    "ana",   "reporte",   "exportar_pdf",   "Reporte exportado a PDF (fila sin id)", NA),
    fila_log("a-005", hace(3),    "mouse", "clientes",  "client_access",  "Acceso de staff a fixture", "fixture"),
    fila_log("a-006", hace(4),    "ana",   "auth",      "logout",         "Cierre de sesión: ana (12min 3s activo)", "ana"),
    fila_log("a-007", hace(10),   "luis",  "auth",      "login",          "Inicio de sesión fuera del rango por defecto", "luis")),
  `hd-admin` = dplyr::bind_rows(
    fila_log("h-001", hace(0, 2), "mouse", "tiers", "guardar_tier_config", "Configuración de niveles guardada", "dev", cid = "hd-admin"),
    fila_log("h-002", hace(1, 5), "mouse", "pasivos", "provision.revived", "provision.revived", "p-1", cid = "fixture"))
)
if (!is.na(muchas) && muchas > 0)
  almacen$logs$fixture <- dplyr::bind_rows(almacen$logs$fixture, fila_log(
    sprintf("m-%04d", seq_len(muchas)), hace(0, 1) - seq_len(muchas) * 30, "luis", "ledger_AR", "etiquetar",
    sprintf("Factura %d etiquetada como urgente", seq_len(muchas)), as.character(seq_len(muchas))))

assign("read_audit_log", function(client_id = NULL, since = NULL, until = NULL) {
  r <- almacen$logs[[client_id]] %||% .APP_AUDIT_SCHEMA()
  if (!is.null(since)) r <- r[r$ts >= as.POSIXct(since), , drop = FALSE]
  if (!is.null(until)) r <- r[r$ts <= as.POSIXct(until), , drop = FALSE]
  r[order(r$ts, decreasing = TRUE), , drop = FALSE]
}, envir = globalenv())

ben <- function(alias, clabe, importe)
  as.character(jsonlite::toJSON(list(list(alias = alias, clabe = clabe, banco = "BBVA", importe = importe,
                                           concepto = "F-1001")), auto_unbox = TRUE))
pago <- function(id, ts, user, empresa, moneda, total, n, tipo = "pago_ppl", source = "individual",
                 clabe = "030180900012345678", beneficiario = "PROVEEDOR UNO SA DE CV")
  tibble::tibble(id = id, ts = ts, user = user, client_id = "fixture", empresa = empresa, moneda = moneda,
                 cuenta_origen = if (tipo == "pago_ppl") clabe else NA_character_,
                 banco_origen = if (tipo == "pago_ppl") "BANBAJIO" else NA_character_,
                 n_pagos = if (tipo == "pago_ppl") as.integer(n) else NA_integer_,
                 total = if (tipo == "pago_ppl") total else NA_real_,
                 beneficiarios = if (tipo == "pago_ppl") ben(beneficiario, "012180001234567891", total) else NA_character_,
                 source = if (tipo == "pago_ppl") source else "excel",
                 archivo = paste0("archivo_", id, if (tipo == "pago_ppl") ".txt" else ".xlsx"),
                 tipo = tipo, detalle = if (tipo == "pago_ppl") NA_character_ else '{"filas":42,"hoja":"datos"}')
almacen$pagos <- dplyr::bind_rows(
  pago("p-01", hace(0, 2), "luis", "NETWORKS TRANS", "USD", 189955.59, 4, source = "zip_bulk", beneficiario = "TRANSPORTES ALFA"),
  pago("p-02", hace(1),    "ana",  "NETWORKS CARGO", "MXN", 2324.49, 7, beneficiario = "CARGAS BETA"),
  pago("p-03", hace(2),    "ana",  "NETWORKS TRANS", "MXN", 9, 1, beneficiario = "GAMMA NUEVE"),
  pago("p-04", hace(3),    "luis", "NETWORKS CARGO", "USD", 10, 1, beneficiario = "DELTA DIEZ"),
  pago(NA,     hace(4),    "luis", "NETWORKS TRANS", "MXN", 0, 0, tipo = "reporte_excel"),
  pago("p-06", hace(5),    "mouse","NETWORKS TRANS", "MXN", 0, 0, tipo = "bancos_libro_excel"),
  pago("p-07", hace(40),   "luis", "NETWORKS TRANS", "MXN", 77, 1, beneficiario = "FUERA DE RANGO"))
assign("read_pagos_generados", function(client_id = NULL, since = NULL, until = NULL) {
  r <- almacen$pagos
  if (!is.null(since)) r <- r[r$ts >= as.POSIXct(since), , drop = FALSE]
  if (!is.null(until)) r <- r[r$ts <= as.POSIXct(until), , drop = FALSE]
  r[order(r$ts, decreasing = TRUE), , drop = FALSE]
}, envir = globalenv())

assign(".audit_viewer_resolve_perms", function(shared)
  list(can_view_client_audit_logs = TRUE, can_view_staff_audit_log = TRUE), envir = globalenv())
assign("puede", function(usuario, permiso, ...) TRUE, envir = globalenv())
assign("log_action", function(user, module, action, description = "", target_id = NA, metadata = NULL, ...) {
  m <- if (length(metadata)) paste(sprintf("%s=%s", names(metadata),
         vapply(metadata, function(v) paste(as.character(v %||% ""), collapse = ","), character(1))), collapse = ";") else ""
  anotar(sprintf("log %s %s %s", action, target_id %||% "", m))
}, envir = globalenv())

ui <- bslib::page_navbar(
  title = "registros", id = "nav",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#0A58CA"),
  # The engine in the page's head from the start, as in the app (Vencidos is
  # in app.R's static UI). Both modules paint their table inside a
  # renderUI: with the engine arriving only with it, the first data message
  # can come before hdtable.js has a handler, and Shiny drops it.
  header = tagList(shinyjs::useShinyjs(), app_styles(), hd_table_deps()),
  bslib::nav_panel("Actividad", value = "ACT",
    actionButton("otra_sesion_log", "Otra sesión: registra una actividad"),
    auditLogViewerUI("act")),
  bslib::nav_panel("Bitácora Global", value = "GLO", auditLogViewerUI("glo")),
  bslib::nav_panel("Archivos de pago", value = "PAG",
    actionButton("otra_sesion", "Otra sesión: genera un archivo de pago"),
    pagosAuditViewerUI("pag")),
  footer = tagList(
    tags$pre(id = "registro", style = "font-size:.7rem; max-height:160px; overflow:auto;"),
    tags$style("#registro:empty { display: none; }"),
    tags$script(HTML("Shiny.addCustomMessageHandler('registro', function (t) { document.getElementById('registro').textContent = t; });")))
)

server <- function(input, output, session) {
  info <- list(user = "luis", name = "Luis", tier = "principal", client_id = "fixture", is_staff = staff)
  shared <- list(
    current_user_info   = function() info,
    current_user        = function() "luis",
    usuario             = function() NULL,
    home_client_id      = function() "fixture",
    effective_client_id = function() "fixture",
    refresh_signal      = function() NULL
  )
  auditLogViewerServer("act", shared, mode = "own_client")
  auditLogViewerServer("glo", shared, mode = "multi_client",
                       allowed_client_ids = c("Fixture SA" = "fixture"), include_staff_log = TRUE)
  pagosAuditViewerServer("pag", shared)
  observeEvent(input$otra_sesion, {
    almacen$pagos <- dplyr::bind_rows(pago("p-new", Sys.time(), "otra", "NETWORKS CARGO", "MXN", 555, 2,
                                           beneficiario = "OTRA SESION"), almacen$pagos)
  })
  observeEvent(input$otra_sesion_log, {
    almacen$logs$fixture <- dplyr::bind_rows(fila_log(sprintf("a-new-%d", input$otra_sesion_log), Sys.time(), "otra",
      "bancos", "importar_movimientos", "Importación hecha por otra sesión", "s-99"), almacen$logs$fixture)
  })
  observe(session$sendCustomMessage("registro", paste(registro(), collapse = "\n")))
}

app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(raiz, "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
