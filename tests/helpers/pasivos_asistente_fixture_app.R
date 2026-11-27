# =============================================================================
# tests/helpers/pasivos_asistente_fixture_app.R
# T63: the real Pasivos wizard (setup_pasivos_wizard(), every step, the
# Revisión previews) with a fake `shared` in memory: two companies, a user
# allowed to manage Pasivos, no liabilities yet. Never S3 and never FRED or
# Banxico: the reference rates are fixed (PRIME 7.50 / 7.25, SOFR 4.30 /
# 4.31, TIIE28 8.25 / 8.24). Saving is a stub that writes the liability it
# would save into #guardado (JSON), so a test can read it.
# A button "#abrir" fires pasivos_add_liability, as "+ Agregar pasivo" does.
# =============================================================================
library(shiny)
suppressPackageStartupMessages({ library(dplyr); library(tibble); library(lubridate); library(uuid) })
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
    eval(e, envir = globalenv())
cargar("R/global.R", c("strip_accents", "fmt_money"))
cargar("R/persistence.R", c("confirmaciones_vigentes", ".normalize"))
for (f in c("R/monedas.R", "R/zona.R", "R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/pasivos_schemas.R", "R/pasivos_capabilities.R", "R/forecasting_resolve.R", "R/policy_engine.R",
            "R/pasivos_engine.R", "R/pasivos_observers.R", "R/pasivos_rates.R",
            "R/pasivos_wizard_validate.R", "R/pasivos_wizard_steps.R", "R/pasivos_wizard_module.R", "R/adjuntos_s3.R", "R/adjuntos_registro.R", "R/adjuntos_almacen.R", "R/adjuntos_documentos.R", "R/adjuntos_cfdi.R", "R/adjuntos_pasivo_ui.R", "R/zona.R"))
  suppressMessages(source(f))
source("tests/helpers/hd_numero.R")   # T66: hd_numero_input()
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())

# Stubs at the boundaries: permissions, S3, audit, the rates' web APIs.
has_capability <- function(...) TRUE
forecasting_get_estimate <- function(...) NA_real_
forecasting_set_estimate <- function(...) invisible(TRUE)
bump_sync_version <- function(...) invisible(NULL)
pasivos_log_audit <- function(...) invisible(NULL)
avisos_actividad <- function(...) invisible(NULL)
log_action <- function(...) invisible(NULL)
find_proveedor_matches <- function(...) NULL
.tasas <- function(hoy, ayer) list(today = hoy, yesterday = ayer, date_today = Sys.Date() - 1, date_yesterday = Sys.Date() - 2)
.fetch_prime  <- function(...) .tasas(7.50, 7.25)
.fetch_sofr   <- function(...) .tasas(4.30, 4.31)
.fetch_tiie28 <- function(...) .tasas(8.25, 8.24)
GUARDADOS <- reactiveVal(NULL)
save_pasivos_liabilities <- function(df, ...) invisible(TRUE)
save_pasivos_provisions  <- function(df, ...) invisible(TRUE)
modificar_pasivos_liabilities <- function(client_id, cambio, ...) {
  nuevo <- cambio(.schema_pasivos_liability()); GUARDADOS(nuevo); invisible(nuevo) }
modificar_pasivos_provisions <- function(client_id, cambio, ...) invisible(cambio(.schema_pasivos_provision()))

EMPRESAS <- c("Networks Crossdocking" = "NCS", "Networks Trucking" = "NTS")
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  shinyjs::useShinyjs(),
  tags$button(id = "abrir", class = "btn btn-primary m-3",
              onclick = "Shiny.setInputValue('pasivos_add_liability', Math.random(), {priority: 'event'})", "+ Agregar pasivo"),
  tags$pre(id = "guardado"),
  tags$script(HTML("Shiny.addCustomMessageHandler('guardado', function (t) { document.getElementById('guardado').textContent = t; });")))
server <- function(input, output, session) {
  shared <- list(
    company_map = reactiveVal(EMPRESAS), current_user = reactiveVal("tester"),
    effective_client_id = function() "prueba", holiday_overrides_db = reactiveVal(NULL),
    moneda_base_cliente = reactive(list(moneda = "MXN")),
    pasivos_liabilities_db = reactiveVal(.schema_pasivos_liability()),
    pasivos_provisions_db = reactiveVal(.schema_pasivos_provision()),
    proveedores_db = reactiveVal(NULL), suppress_ledger_prov_refresh = reactiveVal(FALSE))
  setup_pasivos_wizard(input, output, session, shared)
  observeEvent(GUARDADOS(), session$sendCustomMessage("guardado",
    as.character(jsonlite::toJSON(as.list(GUARDADOS()[1, c("nombre", "tasa_tipo", "principal_original")]), auto_unbox = TRUE))))
}
app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(getwd(), "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
