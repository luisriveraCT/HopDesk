# =============================================================================
# tests/helpers/bancos_fixture_app.R
# Bancos (the real bancosUI/bancosServer of R/bancos_module.R) over fixed
# data, on the in-memory S3 of tests/helpers/s3_memoria.R: every save the
# module makes lands in that store, never in a real bucket. Three accounts
# (two companies), 60 movements by default -- commissions, a reconciled one,
# manual entries -- and a few confirmations.
#
# Every session shares one set of movements and confirmations (as the sync
# bus makes it in the app): what one session deletes, the others see go.
# #registro lists, per session, the movements the store holds as deleted
# ("eliminados: MOV-0003,MOV-0007") after each save, and the last Excel
# export's log ("bancos_libro_excel: 59 filas").
#
# BANCOS_SIN_PERMISO=1: the user lacks can_manage_bancos.
# BANCOS_N: that many more movements (default 0). BANCOS_N=900 puts the
# Libro in window mode; the oldest is "PROVEEDOR ULTIMO SA", receipt 999999.
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
cargar("R/erp_connector_registry.R", "SAP_B1_ERP_TYPE")
cargar("R/global.R", c("fmt_money", "is_erp_sourced", "CURRENCIES"))
cargar("R/ui_components.R", "app_styles")
cargar("R/settings/settings_helpers.R", c(".schema_ctas_bancos", "load_ctas_bancos", ".schema_ctas_cuentas", "load_ctas_cuentas"))
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/bancos_parser.R", "R/bancos_detect.R", "R/bancos_persistence.R", "R/bancos_module.R"))
  sys.source(file.path(raiz, f), envir = globalenv())
assign(".i18n_dir", file.path(raiz, "www", "i18n"), envir = globalenv())
assign("log_action", function(...) invisible(NULL), envir = globalenv())
assign("bump_sync_version", function(...) invisible(NULL), envir = globalenv())
# BANCOS_SIN_PERMISO=1: the user lacks can_manage_bancos (the buttons that
# write must not show, and the server must refuse).
SIN_PERMISO <- identical(Sys.getenv("BANCOS_SIN_PERMISO"), "1")
assign("puede", function(usuario, clave, ...) !(SIN_PERMISO && identical(clave, "can_manage_bancos")), envir = globalenv())
assign("puede_o_avisa", function(shared, clave, ...) {
  ok <- puede("luis", clave)
  if (!ok) showNotification("Sin permiso", type = "warning")
  ok
}, envir = globalenv())
# The Libro's Excel is logged as a generated file: record it in #registro.
EXCEL <- NULL
assign("log_archivo_generado", function(shared, tipo, empresa, moneda, detalle_list, total = NA, n_items = NA, fname = NA)
  EXCEL(sprintf("%s: %s filas", tipo, n_items)), envir = globalenv())
suppressPackageStartupMessages({ library(dplyr); library(shinyjs) })

# ── Data ─────────────────────────────────────────────────────────────────────
CID <- "acme"
ctas <- tibble::tibble(id = c("cta_nts_mxn", "cta_nts_usd", "cta_ncs_mxn"), Empresa = c("NTS", "NTS", "NCS"),
                       banco_id = "bb", Moneda = c("MXN", "USD", "MXN"), alias = c("NTSBB", "NTSUSD", "NCSBB"),
                       cuenta = c("0111", "0222", "0333"), clabe_interbancaria = c("030111", "030222", "030333"),
                       saldo_inicial = 0, activa = TRUE)   # the ctas_cuentas shape (Configuración)
mov <- function(i, cuenta, fecha, cargo = 0, abono = 0, tipo = "spei_out", fuente = "txt", parte = "PROVEEDOR SA",
                conciliado = FALSE)
  tibble::tibble(id = sprintf("MOV-%04d", i), cuenta_id = cuenta, fecha = as.Date(fecha),
                 hora = sprintf("%02d:00:00", 8 + i %% 10), recibo = as.character(100000 + i),
                 descripcion_raw = sprintf("SPEI | %s | Recibo # %d", parte, i), tipo = tipo, parte = parte,
                 rfc = NA_character_, referencia = NA_character_, clave_rastreo = NA_character_,
                 concepto = paste("Concepto", i), cargo = cargo, abono = abono, saldo_banco = 1e6 - i * 100,
                 conciliado = conciliado, doc_vinculado = NA_character_, agenda_id = NA_character_,
                 importado_at = as.POSIXct("2026-10-01 10:00:00"), fuente = fuente, eliminado = FALSE,
                 eliminado_por = NA_character_, eliminado_at = as.POSIXct(NA), notas = NA_character_)
hoy <- Sys.Date()
movs0 <- dplyr::bind_rows(
  mov(1, "cta_nts_mxn", hoy - 1, cargo = 15000, parte = "ACEROS MONTERREY SA"),
  mov(2, "cta_nts_mxn", hoy - 1, abono = 50000, tipo = "spei_in", parte = "CLIENTE UNO SA"),
  mov(3, "cta_nts_mxn", hoy - 2, cargo = 12.5, tipo = "comision", parte = "BANBAJIO"),
  mov(4, "cta_nts_mxn", hoy - 2, cargo = 8200, parte = "BODEGAS DEL NORTE SA", conciliado = TRUE),
  mov(5, "cta_nts_mxn", hoy - 3, abono = 900, tipo = "manual_in", fuente = "manual", parte = "AJUSTE MANUAL"),
  mov(6, "cta_nts_usd", hoy - 3, cargo = 1200, parte = "PROVEEDOR USD INC"),
  mov(7, "cta_ncs_mxn", hoy - 4, cargo = 3100, parte = "TRANSPORTES DEL BAJIO"),
  mov(8, "cta_ncs_mxn", hoy - 5, abono = 7000, tipo = "spei_in", parte = "CLIENTE DOS SA"),
  lapply(9:60, function(i) mov(i, c("cta_nts_mxn", "cta_ncs_mxn")[1 + i %% 2], hoy - 5 - i %/% 4,
                               cargo = 100 * i, parte = sprintf("PROVEEDOR %02d SA", i))))
# A second manual entry, outside the first rows (the test deletes a range there).
m30 <- movs0$id == "MOV-0030"
movs0$fuente[m30] <- "manual"; movs0$tipo[m30] <- "manual_in"; movs0$cargo[m30] <- 0; movs0$abono[m30] <- 4500
n_extra <- as.integer(Sys.getenv("BANCOS_N", "0"))
if (n_extra > 0L) {
  extra <- dplyr::bind_rows(lapply(seq_len(n_extra), function(k)
    mov(1000 + k, "cta_nts_mxn", hoy - 20 - k %/% 10, cargo = 10 + k, parte = sprintf("PROVEEDOR %04d SA", k))))
  extra$parte[n_extra] <- "PROVEEDOR ULTIMO SA"; extra$recibo[n_extra] <- "999999"
  extra$descripcion_raw[n_extra] <- "SPEI | PROVEEDOR ULTIMO SA | Recibo # 999999"
  extra$fecha[n_extra] <- hoy - 400
  movs0 <- dplyr::bind_rows(movs0, extra)
}
movs0 <- .normalize(movs0, .schema_bancos_movimientos)
assign(paste0(CID, "/", S3_KEYS$bancos_movimientos), movs0, envir = .mem)

# ── App ──────────────────────────────────────────────────────────────────────
MOVS <- reactiveVal(movs0)                                   # one set for every session
EXCEL <- reactiveVal("")
CONF <- reactiveVal(.schema_bancos_confirmados())
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#0A58CA"),
  shinyjs::useShinyjs(), app_styles(), tags$head(tags$script(src = "i18n.js")),
  bancosUI("bnc"),
  tags$pre(id = "registro", style = "font-size:.7rem;"),
  tags$script(HTML("Shiny.addCustomMessageHandler('registro', function (t) { document.getElementById('registro').textContent = t; });"))
)
server <- function(input, output, session) {
  shared <- list(
    effective_client_id = function() CID, home_client_id = function() CID, current_user = function() "luis",
    usuario = reactive("luis"),
    company_map = reactiveVal(c(NTS = "Networks Trucking", NCS = "Networks Crossdocking")),
    visible_initials = reactiveVal(NULL), bancos_movimientos = MOVS, bancos_confirmados = CONF,
    conciliacion_rv = reactiveVal(tibble::tibble()), ctas_cuentas = reactiveVal(ctas),
    papelera_rv = reactiveVal(NULL), abonos_db = reactiveVal(tibble::tibble()), manual_inv = reactiveVal(NULL),
    pasivos_provisions_db = reactiveVal(NULL), sap_data = reactiveVal(list()), empresas_db = reactiveVal(NULL),
    suppress_ledger_prov_refresh = reactiveVal(FALSE))
  # What the store holds as deleted, after each save.
  observe({
    MOVS()
    guardado <- get0(paste0(CID, "/", S3_KEYS$bancos_movimientos), envir = .mem)
    elim <- if (is.null(guardado)) character(0) else sort(guardado$id[guardado$eliminado %in% TRUE])
    session$sendCustomMessage("registro", paste(sprintf("eliminados: %s", paste(elim, collapse = ",")), EXCEL(), sep = "
"))
  })
  bancosServer("bnc", shared)
}

app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(raiz, "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
