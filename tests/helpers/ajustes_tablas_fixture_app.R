# =============================================================================
# tests/helpers/ajustes_tablas_fixture_app.R
# Ajustes (R/settings/: Cuentas de Empresa, Intercompany, Business Partners
# and the IC scanner) with the real observers, over the in-memory S3 of
# tests/helpers/s3_memoria.R (only in this app's own R process; it restores
# aws.s3 on exit). The page holds the three panel buttons of the gear and its
# #settings_panel, so the panels open as in the app.
#
# Companies NTS and NCS. Banks b1 (BanBajío), b2 (Santander); accounts c1
# (NTS, b1, PPL source) and c2 (NCS, b2). IC registry: NTS CxC 1027. SAP
# snapshot (AR): C1027 (already IC), C2001; AP: P3003, which the supplier
# catalog knows by its code.
# #otra_sesion_cuenta plays another session adding account c0 ahead of the
# others (it reaches this session through shared$ctas_cuentas, as the sync
# bus does). Every write is listed in #registro:
#   escribe <key> <rows or "NTS ar=.. ap=..;NCS ar=.. ap=..">
# AJ_SIN_PERMISO = 1: the user is Finanzas (no can_manage_interco).
# AJ_RAIZ: the checkout whose R/ and www/ to use (default: this one).
# =============================================================================
library(shiny)
raiz <- normalizePath(Sys.getenv("AJ_RAIZ", "."), winslash = "/")
setwd(raiz)
suppressPackageStartupMessages({ library(dplyr); library(shinyjs); library(tibble) })
source("tests/helpers/s3_memoria.R")
cargar <- function(archivo, nombres) {
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
}
cargar("R/settings/settings_helpers.R", c(".schema_ctas_bancos", ".schema_ctas_cuentas", "load_ctas_bancos",
                                          "load_ctas_cuentas", "modificar_ctas_bancos", "modificar_ctas_cuentas",
                                          ".BANCOS_MX_CHOICES", "borrar_banco_si_libre",
                                          ".banco_en_uso_filas", ".banco_en_uso_texto", ".cuentas_form_empty"))
cargar("R/adjuntos_s3.R", c(".NUMERO_PURO", ".CELDA_PELIGRO", "neutralizar_celdas", "tabla_a_texto_copiable"))
cargar("R/data_pipeline.R", "scan_ic_candidates")
cargar("R/ui_components.R", "app_styles")
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R",
            "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/settings/settings_cuentas.R", "R/settings/settings_bp.R", "R/settings/settings_hub.R"))
  suppressMessages(source(f))
.i18n_dir <- file.path(raiz, "www", "i18n")

registro <- reactiveVal(character(0))
anotar   <- function(x) registro(c(registro(), x))
# Every write that reaches the store, in #registro.
local({
  guardar <- get("s3saveRDS", asNamespace("aws.s3"))
  utils::assignInNamespace("s3saveRDS", function(x, object, bucket, ...) {
    guardar(x, object, bucket, ...)
    clave <- sub("^[^/]+/", "", object)
    if (is.data.frame(x)) anotar(sprintf("escribe %s %d", clave, nrow(x)))
    else if (is.list(x) && !is.null(x$companies))
      anotar(sprintf("escribe %s %s", clave, paste(sprintf("%s ar=%s ap=%s", names(x$companies),
        vapply(x$companies, function(co) paste(co$ar, collapse = ","), ""),
        vapply(x$companies, function(co) paste(co$ap, collapse = ","), "")), collapse = ";")))
    invisible(TRUE)
  }, "aws.s3")
})
log_action <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
show_settings_modal <- function(...) invisible(NULL)
proveedoresServer <- function(...) invisible(NULL)
proveedoresUI <- function(id) div()

CID  <- "acme"
CMAP <- c(NTS = "Networks Trucking Services", NCS = "Networks Crossdocking Services")
BANCOS0 <- tibble::tibble(id = c("b1", "b2"), nombre = c("BanBajío", "Santander"), clave = c("030", "014"))
cta <- function(id, empresa, banco, cuenta, ppl = FALSE)
  tibble::tibble(id = id, banco_id = banco, Empresa = empresa, razon_social = paste(empresa, "SA de CV"), rfc = "",
                 Moneda = "MXN", alias = paste0(empresa, "-MXN"), cuenta = cuenta, clabe_interbancaria = "",
                 is_ppl_default = ppl, saldo_inicial = 0, activa = TRUE)
CTAS0 <- dplyr::bind_rows(cta("c1", "NTS", "b1", "0100", ppl = TRUE), cta("c2", "NCS", "b2", "0200"))
REG0  <- list(ar_prefix = "C", ap_prefix = "P", rfcs = character(),
              companies = list(NTS = list(ar = "1027", ap = character()), NCS = list(ar = character(), ap = character())))
SNAP_AR <- data.frame(Empresa = "Networks Trucking Services", `Código de cliente` = c("C1027", "C2001", "C2001"),
                      Parte = c("NETWORKS CROSSDOCKING", "NETWORKS REALTORS", "NETWORKS REALTORS"),
                      RFC = c("NCS010101AA1", "NRS010101AA1", "NRS010101AA1"), check.names = FALSE, stringsAsFactors = FALSE)
SNAP_AP <- data.frame(Empresa = "Networks Crossdocking Services", `Código de proveedor` = "P3003",
                      Parte = "NETWORKS TRUCKING SERVICES", RFC = "NTS010101AA1", check.names = FALSE, stringsAsFactors = FALSE)
load_sap_snapshot <- function(ledger, client_id = NULL) list(data = if (ledger == "AR") SNAP_AR else SNAP_AP)
assign(paste0(CID, "/", S3_KEYS$ctas_bancos),  BANCOS0, envir = .mem)
assign(paste0(CID, "/", S3_KEYS$ctas_cuentas), CTAS0,   envir = .mem)
assign(paste0(CID, "/", S3_KEYS$interco_v2),   REG0,    envir = .mem)

sin_permiso <- identical(Sys.getenv("AJ_SIN_PERMISO"), "1")
USR <- local({ t <- if (sin_permiso) "finance" else "admin"; list(user = t, tier = t, permisos = permisos_resueltos(t)) })

ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#0A58CA"),
  shinyjs::useShinyjs(), app_styles(), hd_table_deps(),
  div(class = "d-flex gap-2 my-2",
    actionButton("stg_btn_cuentas", "Cuentas de Empresa"),
    actionButton("stg_btn_interco", "Intercompany"),
    actionButton("stg_btn_catalogo", "Business Partners"),
    actionButton("otra_sesion_cuenta", "Otra sesión: agrega c0")),
  uiOutput("settings_panel"),
  tags$pre(id = "registro", style = "font-size:.7rem; max-height:160px; overflow:auto;"),
  tags$style("#registro:empty { display: none; }")
)

server <- function(input, output, session) {
  shared <- list(
    usuario = reactiveVal(USR), current_user = function() USR$user,
    effective_client_id = function() CID, home_client_id = function() CID,
    company_map = reactive(CMAP), ctas_cuentas = reactiveVal(CTAS0),
    interco_v2 = reactiveVal(REG0),
    proveedores_db = reactiveVal(data.frame(nombre = "NTS", rfc = "", codigo = "P3003", stringsAsFactors = FALSE)),
    sap_data = function() list())
  settings_observers(input, output, session, shared)
  settings_cuentas_observer(input, output, session, shared)
  observeEvent(input$otra_sesion_cuenta, {
    otra <- dplyr::bind_rows(cta("c0", "NCS", "b1", "0999"), shared$ctas_cuentas())
    assign(paste0(CID, "/", S3_KEYS$ctas_cuentas), otra, envir = .mem)
    shared$ctas_cuentas(otra)
  })
  observe(session$sendCustomMessage("registro", paste(registro(), collapse = "\n")))
}
ui <- tagList(ui, tags$script(HTML(
  "Shiny.addCustomMessageHandler('registro', function (t) { document.getElementById('registro').textContent = t; });")))
app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(raiz, "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
