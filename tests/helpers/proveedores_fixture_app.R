# =============================================================================
# tests/helpers/proveedores_fixture_app.R
# The Proveedores catalog (R/proveedores_module.R) over fixed providers and a
# simulated `shared` -- no S3. save_proveedores() / save_proveedores_inactivos()
# keep the lists in memory and list each save in #registro as one line:
#   activos <n>: <nombre>@<no_cuenta>, ...      inactivos <n>: ...
# plus every log_action() as "log <action> <target_id>".
#
# PROV_RAIZ: the checkout whose R/ and www/ to use (default: this one).
# =============================================================================
library(shiny)
raiz <- normalizePath(Sys.getenv("PROV_RAIZ", "."), winslash = "/")
assign("%||%", function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x,
       envir = globalenv())
cargar <- function(archivo, nombres) {
  ex <- parse(file.path(raiz, archivo), keep.source = FALSE)
  for (e in ex)
    if (is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
}
suppressPackageStartupMessages({ library(dplyr); library(shinyjs); library(shinyWidgets); library(readxl) })
cargar("R/global.R", c("strip_accents", "CURRENCIES"))
cargar("R/settings/settings_helpers.R", ".BANCOS_MX_CHOICES")
cargar("R/proveedores_match.R", "parse_activo_hasta")
cargar("R/persistence.R", c(".schema_proveedores", ".normalize"))
cargar("R/ui_components.R", "app_styles")
local({
  owd <- setwd(raiz); on.exit(setwd(owd))
  for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
              "R/proveedores_excel.R", "R/proveedores_module.R"))
    if (file.exists(f)) sys.source(f, envir = globalenv())
})
assign(".i18n_dir", file.path(raiz, "www", "i18n"), envir = globalenv())

registro <- reactiveVal(character(0))
anotar   <- function(x) registro(c(registro(), x))
resumen  <- function(df) paste(sprintf("%s@%s", df$nombre, df$no_cuenta), collapse = ", ")
assign("bump_sync_version", function(...) invisible(NULL), envir = globalenv())
assign("puede", function(usuario, permiso, ...) TRUE, envir = globalenv())
assign("puede_o_avisa", function(...) TRUE, envir = globalenv())
assign("save_proveedores", function(df, client_id = NULL)
  anotar(sprintf("activos %d: %s", nrow(df), resumen(df))), envir = globalenv())
assign("save_proveedores_inactivos", function(df, client_id = NULL)
  anotar(sprintf("inactivos %d: %s", nrow(df), resumen(df))), envir = globalenv())
assign("log_action", function(user, module, action, description = "", target_id = NA, ...)
  anotar(sprintf("log %s %s", action, target_id %||% "")), envir = globalenv())

prov <- function(id, nombre, alias, banco, nc, rfc = "", moneda = "MXN", empresas = "NTS", activo = TRUE,
                 status = "completo", activo_hasta = "indefinido")
  data.frame(id = id, Empresa = empresas, nombre = nombre, alias = alias, banco = banco, banco_destino = banco,
             no_cuenta = nc, clabe = nc, rfc = rfc, moneda = moneda, empresas = empresas, activo = activo,
             status = status, activo_hasta = activo_hasta, medio_pago = "SPI", fuente = "manual",
             created_at = "2026-01-01", updated_at = "2026-01-01", stringsAsFactors = FALSE)
# Accounts are valid CLABEs (check digit), as the import requires.
activos0 <- .normalize(rbind(
  prov("p-acme", "ACEROS MONTERREY SA", "ACEROS", "012", "012180001111111119", "AMO010101AA1"),
  prov("p-flet", "FLETES EL GÜERO", "FLETES", "072", "072180002222222224"),
  prov("p-papl", "PAPELERIA ÑANDU", "PAPELERIA", "", "", status = "incompleto"),
  prov("p-soft", "SOFTWARE CORP", "SOFTCORP", "002", "002180003333333330", moneda = "USD", activo_hasta = "2020-01-01")
), .schema_proveedores)
inactivos0 <- .normalize(rbind(
  prov("p-viej", "PROVEEDOR VIEJO SA", "VIEJO", "014", "014180004444444448", activo = FALSE)
), .schema_proveedores)

ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#0A58CA"),
  shinyjs::useShinyjs(), app_styles(),
  proveedoresUI("prov_mod"),
  tags$pre(id = "registro", style = "font-size:.7rem; max-height:200px; overflow:auto;"),
  tags$style("#registro:empty { display: none; }"),
  tags$script(HTML("Shiny.addCustomMessageHandler('registro', function (t) { document.getElementById('registro').textContent = t; });"))
)

server <- function(input, output, session) {
  shared <- list(
    proveedores_db           = reactiveVal(activos0),
    proveedores_inactivos_db = reactiveVal(inactivos0),
    company_map              = reactive(c(NTS = "Networks Trucking", NCS = "Networks Crossdocking")),
    effective_client_id      = function() "fixture",
    home_client_id           = function() "fixture",
    current_user             = function() "luis",
    usuario                  = function() "luis"
  )
  proveedoresServer("prov_mod", shared)
  observe(session$sendCustomMessage("registro", paste(registro(), collapse = "\n")))
}

app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(raiz, "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
