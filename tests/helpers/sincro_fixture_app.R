# =============================================================================
# tests/helpers/sincro_fixture_app.R
# Ajustes › Sincronizar Agenda (R/settings/settings_sincro.R) over the REAL
# agenda persistence of R/persistence.R (save/load per user, the shared sync
# agenda, the sync config), on an in-memory S3 local to this app: only
# .s3_read_with / .s3_write / .s3_leer_para_escribir are redefined, here;
# aws.s3 is never touched.
#
# SIN_FALLA: a regex of S3 keys whose writes fail ("pagar_hoy_sync",
# "agenda_sync_config", "pagar_hoy_user_ana", ...), to see a failed save;
# SIN_FALLA_LEE: the same for reads. SIN_SYNC = 1 starts with the sync ON
# (the shared agenda holds beto's 3 items).
# Sync starts OFF. Users: ana (65 pending items, the preview's cut used to
# be 50), beto (3), caro (none). Every write and log_action() is listed in
# #registro:  escribe <key> <rows>   falla <key>   log <action> <target_id>
# SIN_HUB = 1: the panel lives inside an Ajustes modal, as in the app
# (show_settings_modal(), R/settings/settings_hub.R): #abrir_ajustes opens it,
# and the activation / deactivation dialogs replace it, as showModal() does.
# SIN_RAIZ: the checkout whose R/ and www/ to use (default: this one).
# =============================================================================
library(shiny)
raiz <- normalizePath(Sys.getenv("SIN_RAIZ", "."), winslash = "/")
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
cargar("R/global.R", "S3_KEYS")
cargar("R/erp_connector_registry.R", "SAP_B1_ERP_TYPE")
cargar("R/ui_components.R", "app_styles")
cargar("R/persistence.R", c(".normalize", ".schema_pagar_hoy", ".schema_agenda_sync_config",
                            "s3_key_pagar_hoy_user", "load_pagar_hoy_user", "save_pagar_hoy_user",
                            "load_pagar_hoy_sync", "save_pagar_hoy_sync", ".pagar_hoy_vivo",
                            "load_agenda_sync_config", "save_agenda_sync_config"))
local({
  owd <- setwd(raiz); on.exit(setwd(owd))
  for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
              "R/settings/settings_sincro.R"))
    if (file.exists(f)) sys.source(f, envir = globalenv())
})
assign(".i18n_dir", file.path(raiz, "www", "i18n"), envir = globalenv())

# ── In-memory S3, local to this app ─────────────────────────────────────────
registro <- reactiveVal(character(0))
anotar   <- function(x) registro(c(registro(), x))
falla    <- Sys.getenv("SIN_FALLA")
falla_lee <- Sys.getenv("SIN_FALLA_LEE")
leer <- function(key, client_id) {
  if (nzchar(falla_lee) && grepl(falla_lee, key)) stop("S3 no disponible al leer (fixture): ", key)
  mem[[paste0(client_id, "/", key)]]
}
mem      <- new.env()
CID      <- "fixture"
assign(".s3_read_with", function(key, client_id = NULL) leer(key, client_id), envir = globalenv())
assign(".s3_leer_para_escribir", function(key, client_id = NULL) leer(key, client_id), envir = globalenv())
assign(".s3_write", function(obj, key, client_id = NULL) {
  if (nzchar(falla) && grepl(falla, key)) { anotar(paste("falla", key)); stop("S3 no disponible (fixture): ", key) }
  assign(paste0(client_id, "/", key), obj, envir = mem)
  anotar(sprintf("escribe %s %d", key, if (is.data.frame(obj)) nrow(obj) else length(obj)))
  invisible(TRUE)
}, envir = globalenv())
assign("bump_sync_version", function(...) anotar("bump"), envir = globalenv())
assign("puede_o_avisa", function(...) TRUE, envir = globalenv())
hub <- identical(Sys.getenv("SIN_HUB"), "1")
ajustes <- function() showModal(modalDialog(title = "Configuración", size = "xl", easyClose = FALSE,
  footer = modalButton("Cerrar"),
  actionButton("stg_btn_sincro", "Sincronización"), uiOutput("settings_panel")))
assign("show_settings_modal", function(...) if (hub) ajustes() else invisible(NULL), envir = globalenv())
assign("log_action", function(user, module, action, description = "", target_id = NA, ...)
  anotar(sprintf("log %s %s", action, target_id %||% "")), envir = globalenv())
assign("auth_load_usuarios", function(client_id = NULL) data.frame(
  username = c("ana", "beto", "caro"), display_name = c("Ana López", "Beto Ruiz", "Caro Díaz"),
  deleted = FALSE, activo = TRUE, stringsAsFactors = FALSE), envir = globalenv())

agenda <- function(prefijo, n) {
  if (!n) return(.schema_pagar_hoy())
  tibble::tibble(id = sprintf("%s-%02d", prefijo, seq_len(n)), ledger = "AP",
                 Empresa = rep(c("NETWORKS TRANS", "NETWORKS CARGO"), length.out = n),
                 Moneda = rep(c("MXN", "USD"), length.out = n),
                 Documento = sprintf("F-%04d", seq_len(n)),
                 Parte = sprintf("PROVEEDOR %02d SA DE CV", seq_len(n)),
                 Importe = seq_len(n) * 1000 + 0.5, status = "pending")
}
mem[[paste0(CID, "/", s3_key_pagar_hoy_user("ana"))]]  <- .normalize(agenda("ana", 65L), .schema_pagar_hoy)
mem[[paste0(CID, "/", s3_key_pagar_hoy_user("beto"))]] <- .normalize(agenda("beto", 3L), .schema_pagar_hoy)
if (identical(Sys.getenv("SIN_SYNC"), "1")) {
  mem[[paste0(CID, "/", S3_KEYS$pagar_hoy_sync)]] <- .normalize(agenda("beto", 3L), .schema_pagar_hoy)
  mem[[paste0(CID, "/", S3_KEYS$agenda_sync_config)]] <- tibble::tibble(is_enabled = TRUE, enabled_by = "luis",
                                                                       enabled_at = Sys.time())
}

ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#0A58CA"),
  # The engine in the page's head from the start, as in the app (Vencidos).
  shinyjs::useShinyjs(), app_styles(), hd_table_deps(),
  if (!hub) tagList(actionButton("stg_btn_sincro", "Sincronizar Agenda"), uiOutput("settings_panel"))
  else actionButton("abrir_ajustes", "Ajustes"),
  tags$pre(id = "registro", style = "font-size:.7rem; max-height:200px; overflow:auto;"),
  tags$style("#registro:empty { display: none; }"),
  tags$script(HTML("Shiny.addCustomMessageHandler('registro', function (t) { document.getElementById('registro').textContent = t; });"))
)

server <- function(input, output, session) {
  pagar_hoy_db <- reactiveVal(NULL)
  shared <- list(
    effective_client_id = function() CID,
    home_client_id      = function() CID,
    current_user        = function() "luis"
  )
  settings_sincro_observer(input, output, session, shared, pagar_hoy_db)
  observeEvent(input$abrir_ajustes, ajustes())
  observe(session$sendCustomMessage("registro", paste(registro(), collapse = "\n")))
}

app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(raiz, "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
