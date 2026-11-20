# =============================================================================
# tests/helpers/flujo_fixture_app.R
# The Flujo de caja preview (R/cashflow_preview_module.R) inside the
# Calendario's slide wrapper, as in app.R, over fixed rows: only
# build_export_combined_df() (which needs the session's SAP data) is
# replaced; the aggregation (R/cashflow_export_module.R) and the painting
# are the real ones. No S3.
#
# This week (Monday to Sunday) has AR and AP rows in MXN for two companies.
# The panel starts closed and #abrir_flujo opens it, as the Calendario's
# arrow does (the state saved in localStorage is restored on opening).
# FLU_RAIZ: the checkout whose R/ and www/ to use (default: this one).
# =============================================================================
library(shiny)
raiz <- normalizePath(Sys.getenv("FLU_RAIZ", "."), winslash = "/")
assign("%||%", function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x,
       envir = globalenv())
cargar <- function(archivo, nombres) {
  ex <- parse(file.path(raiz, archivo), keep.source = FALSE)
  for (e in ex)
    if (is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
}
suppressPackageStartupMessages({ library(dplyr); library(shinyjs); library(tibble) })
cargar("R/ui_components.R", c("app_styles", "monedas_para_filtrar"))
# Tags by posting date since T52d: etiquetas_por_factura() and its day, hoy_en() (R/zona.R) (T53)
cargar("R/data_pipeline.R", c("etiquetas_por_factura", ".etiquetas_doc_unica", ".politica_doc", ".politica_doc_unica"))
if (file.exists(file.path(raiz, "R/zona.R"))) sys.source(file.path(raiz, "R/zona.R"), envir = globalenv())
cargar("R/persistence.R", "tag_label")
# The FX row's rates (monedas fase 2, R/fx_tipos.R) need these from
# fx_banxico.R, and nothing else: without fx_fix()/fx_divisas(),
# fx_fuentes_hoy() gives NULL and nothing reads S3 (T53).
if (file.exists(file.path(raiz, "R/fx_banxico.R")))
  cargar("R/fx_banxico.R", c(".schema_fx_divisas", ".fx_valido", "FX_FIX_TZ", "FX_FIX_MONEDA", "FX_BANXICO_EN"))
local({
  owd <- setwd(raiz); on.exit(setwd(owd))
  for (f in c("R/tema_registro.R", "R/i18n.R", "R/monedas.R", "R/fx_tipos.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
              "R/cashflow_export_module.R", "R/cashflow_preview_module.R"))
    if (file.exists(f)) sys.source(f, envir = globalenv())
})
assign(".i18n_dir", file.path(raiz, "www", "i18n"), envir = globalenv())

lunes <- lubridate::floor_date(Sys.Date(), "week", week_start = 1)
fila <- function(tipo, parte, dia, importe, empresa = "NETWORKS TRANS")
  tibble::tibble(Tipo = tipo, Parte = parte, Empresa = empresa, Moneda = "MXN", FechaEff = lunes + dia,
                 `Saldo vencido` = importe, source = "sap", Documento = paste0(tipo, "-", parte, "-", dia))
cf_df <- dplyr::bind_rows(
  fila("AR", "CLIENTE ALFA", 0, 150000), fila("AR", "CLIENTE ALFA", 2, 40000),
  fila("AR", "CLIENTE BETA", 1, 2500, "NETWORKS CARGO"),
  fila("AP", "PROVEEDOR UNO", 0, 90000), fila("AP", "PROVEEDOR DOS", 3, 12000))
# FLU_DATOS=prueba (T17c): tests/helpers/flujo_datos_prueba.R instead -- two
# currencies, tags, a provision -- and #eco shows what Exportar hands the
# export window. Without it, the fixture is as it was.
PRUEBA <- identical(Sys.getenv("FLU_DATOS"), "prueba")
if (PRUEBA) {
  source(file.path(raiz, "tests", "helpers", "flujo_datos_prueba.R"))
  DP <- flujo_datos_prueba(lunes)
  cf_df <- DP$df
}
assign("build_export_combined_df", function(shared, ic_mode_val) cf_df, envir = globalenv())

ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#0A58CA"),
  shinyjs::useShinyjs(), app_styles(),
  actionButton("abrir_flujo", "Flujo de caja"),
  actionButton("cambiar_cliente", "Cambiar cliente"),
  tags$link(rel = "stylesheet", type = "text/css", href = "cashflow_preview.css?v=7"),
  # The theme tokens the CSS uses (var(--hd-*)), as app.R loads them.
  if (exists("tema_head", mode = "function")) tema_head(),
  div(class = "h-100 cf-slide-wrapper", style = "height:600px;",
      div(id = "cf-panel-calendar", class = "cf-panel cf-panel-calendar h-100", "Calendario"),
      cashflowPreviewUI("cfp")),
  if (PRUEBA) verbatimTextOutput("eco")
)

server <- function(input, output, session) {
  shared <- list(
    cf_preview_open    = reactiveVal(FALSE),
    cf_preview_prefill = reactiveVal(NULL),
    currency_choices   = if (PRUEBA) list(AR = function() c("MXN", "USD"), AP = function() c("MXN", "USD"))
                         else list(AR = function() "MXN", AP = function() "MXN"),
    ic_mode            = list(AR = function() "exclude"),
    tags_db            = if (PRUEBA) function() DP$tags else function() NULL,
    empresa_sel        = function() c("NETWORKS TRANS", "NETWORKS CARGO"),
    # The session's client (Día 0 is kept per client); #cambiar_cliente
    # switches between two, as staff jumping between clients does.
    effective_client_id = reactiveVal("cliente_a")
  )
  cashflowPreviewServer("cfp", shared)
  observeEvent(input$abrir_flujo, shared$cf_preview_open(TRUE))
  observeEvent(input$cambiar_cliente, shared$effective_client_id(if (identical(shared$effective_client_id(), "cliente_a")) "cliente_b" else "cliente_a"))
  if (PRUEBA) output$eco <- renderText({
    p <- shared$cf_preview_prefill()
    as.character(jsonlite::toJSON(list(abierto = shared$cf_preview_open(),
                                       prefill = if (!is.null(p)) lapply(p, function(x) if (inherits(x, "Date")) as.character(x) else x)),
                                  auto_unbox = TRUE))
  })
}

app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(raiz, "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
