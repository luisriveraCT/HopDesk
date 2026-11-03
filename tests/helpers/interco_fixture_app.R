# =============================================================================
# tests/helpers/interco_fixture_app.R
# The Intercompany tab (R/interco_module.R, with its Mapa IC sub-module) over
# fixed SAP snapshots and an IC registry, with the REAL agenda persistence of
# R/persistence.R on an in-memory S3 local to this app: only .s3_read_with /
# .s3_write / .s3_leer_para_escribir are redefined, here; aws.s3 is never
# touched.
#
# Companies NTS and NCS. NCS owes NTS five AP invoices (P-1..P-5, due one day
# apart) and NTS bills NCS two AR ones; non-IC rows are left out by the
# registry. #otra_sesion_confirma plays another session confirming P-1 in
# Agenda de Hoy (it lands in bancos_confirmados): the pair's list loses its
# first row and every position shifts by one. #otra_sesion_cobra does the
# same with A-1 as a cobro: the Mapa IC's edge loses its first invoice.
#
# IC_FALLA: a regex of S3 keys whose writes fail ("pagar_hoy"), to see a
# failed save. Every write and log_action() is listed in #registro:
#   escribe <key> <rows> <Documentos, sorted>   ids <key> <row ids>
#   falla <key>   log <action> <target_id>
# IC_SIN_PERMISO = 1: the user lacks can_register_payments (puede_o_avisa()
# says so with its notice and returns FALSE; logged as "permiso <key> no").
# IC_VACIO = 1: no SAP snapshot yet (shared$sap_data() is NULL), the cold start.
# IC_RAIZ: the checkout whose R/ and www/ to use (default: this one).
# =============================================================================
library(shiny)
raiz <- normalizePath(Sys.getenv("IC_RAIZ", "."), winslash = "/")
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
cargar("R/global.R", c("S3_KEYS", "S3_KEYS_CRITICAL", "CURRENCIES"))
cargar("R/erp_connector_registry.R", "SAP_B1_ERP_TYPE")
cargar("R/ui_components.R", "app_styles")
cargar("R/vencidos_module.R", "facturas_ids")
local({
  owd <- setwd(raiz); on.exit(setwd(owd))
  for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
              "R/persistence.R", "R/data_pipeline.R", "R/ic_mapa_llave.R", "R/treasury_map_module.R", "R/interco_module.R"))
    if (file.exists(f)) sys.source(f, envir = globalenv())
})
assign(".i18n_dir", file.path(raiz, "www", "i18n"), envir = globalenv())

# ── In-memory S3, local to this app ─────────────────────────────────────────
registro <- reactiveVal(character(0))
anotar   <- function(x) registro(c(registro(), x))
falla    <- Sys.getenv("IC_FALLA")
mem      <- new.env()
CID      <- "fixture"
leer <- function(key, client_id = NULL) mem[[paste0(client_id %||% CID, "/", key)]]
assign(".s3_read_with", function(key, client_id = NULL) leer(key, client_id), envir = globalenv())
assign(".s3_read", function(key) leer(key, CID), envir = globalenv())
assign(".s3_leer_para_escribir", function(key, client_id = NULL) leer(key, client_id), envir = globalenv())
assign(".s3_write", function(obj, key, client_id = NULL) {
  if (nzchar(falla) && grepl(falla, key)) { anotar(paste("falla", key)); stop("S3 no disponible (fixture): ", key) }
  assign(paste0(client_id %||% CID, "/", key), obj, envir = mem)
  anotar(sprintf("escribe %s %d %s", key, if (is.data.frame(obj)) nrow(obj) else length(obj),
                 if (is.data.frame(obj) && "Documento" %in% names(obj)) paste(sort(obj$Documento), collapse = ",") else ""))
  if (is.data.frame(obj) && "id" %in% names(obj)) anotar(paste("ids", key, paste(obj$id, collapse = ",")))
  invisible(TRUE)
}, envir = globalenv())
assign("bump_sync_version", function(...) invisible(NULL), envir = globalenv())
sin_permiso <- identical(Sys.getenv("IC_SIN_PERMISO"), "1")
assign("puede_o_avisa", function(shared, permiso, session = shiny::getDefaultReactiveDomain()) {
  if (!sin_permiso) return(TRUE)
  anotar(paste("permiso", permiso, "no"))
  showNotification(tr("permisos.denegado", permiso = permiso), type = "error")
  FALSE
}, envir = globalenv())
# What the Mapa IC shows (Agregar al plan, Enviar a Agenda) asks puede().
assign("puede", function(usuario, permiso) !sin_permiso, envir = globalenv())
assign("log_action", function(user, module, action, description = "", target_id = NA, ...)
  anotar(sprintf("log %s %s", action, target_id %||% "")), envir = globalenv())

# ── Fixed SAP snapshots ─────────────────────────────────────────────────────
CMAP <- c(NTS = "Networks Trucking Services", NCS = "Networks Crossdocking Services")
hoy  <- Sys.Date()
fila <- function(empresa, parte, doc, ref, venc, saldo, cliente = NA, proveedor = NA, moneda = "MXN")
  tibble::tibble(Documento = doc, Factura = ref, `Código de cliente` = cliente, `Código de proveedor` = proveedor,
                 Parte = parte, `Fecha de contabilización` = hoy - 40, `Fecha de vencimiento` = venc,
                 Moneda = moneda, `Saldo vencido` = saldo, Empresa = empresa)
snap_ap <- dplyr::bind_rows(
  lapply(1:5, function(k) fila("Networks Crossdocking Services", "Networks Trucking Services SA de CV",
                               paste0("P-", k), paste0("T17050", k), hoy - 50 + k * 10, 1000 * k + 0.5, proveedor = "P1039")),
  fila("Networks Crossdocking Services", "PROVEEDOR EXTERNO SA", "P-90", "X1", hoy, 999, proveedor = "P9999"))
# A-1 carries P-1's folio (T170501): the Mapa IC pays the debtor's AP invoice
# found by folio (R/ic_mapa_llave.R), so A-1 -> P-1; A-2 has no AP and stays.
snap_ar <- dplyr::bind_rows(
  fila("Networks Trucking Services", "NETWORKS CROSSDOCKING SERVICES", "A-1", "T170501", hoy - 5, 2500, cliente = "C1003"),
  fila("Networks Trucking Services", "NETWORKS CROSSDOCKING SERVICES", "A-2", "F-2", hoy + 5, 700, cliente = "C1003"))
registro_ic <- list(ar_prefix = "C", ap_prefix = "P",
                    companies = list(NTS = list(ar = "1003", ap = character()), NCS = list(ar = character(), ap = "1039")))

ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#0A58CA"),
  # The engine in the head from the start, as in the app (Vencidos).
  shinyjs::useShinyjs(), app_styles(), hd_table_deps(),
  actionButton("otra_sesion_confirma", "Otra sesión: confirma P-1"),
  actionButton("otra_sesion_cobra", "Otra sesión: cobra A-1"),
  intercoUI("ic"),
  tags$pre(id = "registro", style = "font-size:.7rem; max-height:200px; overflow:auto;"),
  tags$style("#registro:empty { display: none; }"),
  tags$script(HTML("Shiny.addCustomMessageHandler('registro', function (t) { document.getElementById('registro').textContent = t; });"))
)

server <- function(input, output, session) {
  confirmados <- reactiveVal(tibble::tibble())
  shared <- list(
    company_map         = reactive(CMAP),
    interco_v2          = reactiveVal(registro_ic),
    # IC_VACIO = 1: the cold start, before any snapshot has loaded.
    sap_data            = reactiveVal(if (identical(Sys.getenv("IC_VACIO"), "1")) NULL else list(AR = snap_ar, AP = snap_ap)),
    empresa_sel         = reactiveVal(character()),
    bancos_confirmados  = confirmados,
    pagar_hoy_db        = reactiveVal(.schema_pagar_hoy()),
    abonos_db           = reactiveVal(tibble::tibble()),
    papelera_rv         = reactiveVal(tibble::tibble()),
    current_user        = function() "luis",
    usuario             = reactive(list(user = "luis")),
    effective_client_id = function() CID,
    home_client_id      = function() CID,
    ic_map_target       = reactiveVal(NULL),
    ctas_cuentas        = NULL
  )
  intercoServer("ic", shared = shared)
  observeEvent(input$otra_sesion_confirma, {
    p1 <- snap_ap[snap_ap$Documento == "P-1", ]
    confirmados(tibble::tibble(tipo = "pago", empresa = p1$Empresa, documento = p1$Documento,
                               moneda = p1$Moneda, importe = p1$`Saldo vencido`))
  })
  observeEvent(input$otra_sesion_cobra, {
    a1 <- snap_ar[snap_ar$Documento == "A-1", ]
    confirmados(dplyr::bind_rows(confirmados(), tibble::tibble(
      tipo = "cobro", empresa = a1$Empresa, documento = a1$Documento, moneda = a1$Moneda, importe = a1$`Saldo vencido`)))
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
