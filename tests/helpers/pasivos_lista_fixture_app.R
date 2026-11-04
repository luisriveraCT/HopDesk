# =============================================================================
# tests/helpers/pasivos_lista_fixture_app.R
# Pasivos' Lista and Papelera (the real pasivos_list_module_* and
# pasivos_papelera_* of R/pasivos_list_module.R) over fixed data, on the
# in-memory S3 of tests/helpers/s3_memoria.R: every save lands in that
# store, never in a real bucket. Four liabilities: two active (one quoted in
# USD), one paused, one deleted; each with its provisions.
#
# Every session shares one set of liabilities and provisions (as the sync bus
# makes it in the app). #registro lists the estado each liability has on the
# store after each save ("L1=active L2=paused ...").
#
# PASIVOS_SIN_PERMISO=1: the user lacks can_manage_pasivos.
# HD_ARRANQUE_NULL=1: the sources start NULL, as before S3 loads
# (tests/test_hd_table_arranque_null.R).
# =============================================================================
library(shiny)
raiz <- normalizePath(".", winslash = "/")
source(file.path(raiz, "tests/helpers/s3_memoria.R"))
cargar <- function(archivo, nombres) {
  ex <- parse(file.path(raiz, archivo), keep.source = FALSE)
  for (e in ex)
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
}
cargar("R/global.R", c("fmt_money", "CURRENCIES"))
cargar("R/ui_components.R", "app_styles")
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R",
            "R/pasivos_capabilities.R", "R/pasivos_audit.R", "R/forecasting_service.R",
            "R/policy_engine.R", "R/pasivos_engine.R", "R/pasivos_wizard_validate.R",
            "R/pasivos_wizard_module.R", "R/pasivos_list_module.R"))
  suppressMessages(sys.source(file.path(raiz, f), envir = globalenv()))
assign(".i18n_dir", file.path(raiz, "www", "i18n"), envir = globalenv())
assign("bump_sync_version", function(...) invisible(NULL), envir = globalenv())
assign("pasivos_log_audit", function(...) invisible(NULL), envir = globalenv())
SIN_PERMISO <- identical(Sys.getenv("PASIVOS_SIN_PERMISO"), "1")
assign("has_capability", function(user, cap) !SIN_PERMISO, envir = globalenv())
assign("permiso_nombre", function(p) p, envir = globalenv())

CID <- "acme"
pasivo <- function(id, nombre, empresa, estado = "active", categoria = "regular", moneda = "MXN",
                   cotizado = "", flavor = NULL, monto = 100) {
  l <- pasivos_wizard_draft_to_liability(list(
    categoria = categoria, flavor = flavor, nombre = nombre, empresa = empresa, parte = paste("Parte", id),
    moneda_pago = moneda, cotizado_en = cotizado, recurrence_type = "monthly_day",
    recurrence_params = list(day = 15L), fecha_inicio = Sys.Date() - 40, amount_default = monto),
    mode = "create", user = "ana")
  l$id <- id; l$estado <- estado; l
}
liabs0 <- dplyr::bind_rows(
  pasivo("L1", "Renta bodega",  "NTS"),
  pasivo("L2", "Licencias",     "NCS", cotizado = "USD", monto = 250),
  pasivo("L3", "Mantenimiento", "NTS", estado = "paused"),
  pasivo("L4", "Seguro viejo",  "NCS", estado = "deleted")
)
provs0 <- dplyr::bind_rows(lapply(seq_len(nrow(liabs0)), function(i) {
  l <- liabs0[i, ]; l$estado <- "active"
  pasivos_generate_provisions(l, window_start = Sys.Date() - 40)
}))
# Paused: pausing removed its future provisions.
provs0 <- provs0[!(provs0$liability_id == "L3" & provs0$fecha_efectiva >= Sys.Date()), ]
if (identical(Sys.getenv("HD_ARRANQUE_NULL"), "1")) { liabs0 <- NULL; provs0 <- NULL }
s3_memoria_reset()
if (!is.null(liabs0)) {
  save_pasivos_liabilities(liabs0, client_id = CID)
  .s3_write(provs0, S3_KEYS$pasivos_provisions, client_id = CID)
}

# ── App ──────────────────────────────────────────────────────────────────────
LIABS <- reactiveVal(liabs0)                                 # one set for every session
PROVS <- reactiveVal(provs0)
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#0A58CA"),
  shinyjs::useShinyjs(), app_styles(), tags$head(tags$script(src = "i18n.js")),
  tabsetPanel(id = "pestana",
    tabPanel("Lista de pasivos", value = "lista", pasivos_list_module_ui("list")),
    tabPanel("Papelera", value = "papelera", pasivos_papelera_ui("pap"))),
  tags$pre(id = "registro", style = "font-size:.7rem;"),
  tags$script(HTML("Shiny.addCustomMessageHandler('registro', function (t) { document.getElementById('registro').textContent = t; });"))
)
server <- function(input, output, session) {
  shared <- list(
    pasivos_liabilities_db = LIABS, pasivos_provisions_db = PROVS,
    visible_initials = reactiveVal(NULL),
    effective_client_id = function() CID, current_user = function() "luis",
    suppress_ledger_prov_refresh = function(...) invisible(NULL))
  observe({
    LIABS()
    g <- get0(paste0(CID, "/", S3_KEYS$pasivos_liabilities), envir = .mem)
    txt <- if (is.null(g)) "" else paste(sprintf("%s=%s", g$id, g$estado), collapse = " ")
    session$sendCustomMessage("registro", txt)
  })
  # ✎ in the fixture: what the wizard would receive.
  observeEvent(input$pasivos_edit_liability, session$sendCustomMessage("registro",
    paste("editar:", input$pasivos_edit_liability)))
  pasivos_list_module_server("list", shared)
  pasivos_papelera_server("pap", shared)
}

app <- shinyApp(ui, server)
