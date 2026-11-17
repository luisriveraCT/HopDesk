# =============================================================================
# tests/helpers/reporte_fixture.R  (T35)
# Runs the real reporteServer() under shiny::testServer() against an
# in-memory client and returns what the user gets: the report HTML (4 pages)
# and the settings panel's summary. No S3, no SAP, no network.
#
#   reporte_correr(cuentas, movs, sap, base = NULL) ->
#     list(html = <report HTML>, resumen = <preview summary HTML>)
#
# `base` is what shared$moneda_base_cliente() returns (NULL = the function is
# absent, as in a5c696a's shared). The generation time is masked, so two runs
# give the same bytes (so are the gradient ids); the day is fixed (REPORTE_HOY). reporte_ng() is Networks Group's shape: accounts in
# MXN and USD, documents in MXN, USD and EUR.
# Run from the project root.
# =============================================================================
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
# The module reads Sys.Date() (the period, the pulse's weeks). A fixed day
# keeps the stored reference HTML valid any day; the module is sourced into
# the global env, so this one is found before base's.
REPORTE_HOY <- as.Date("2026-11-17")
Sys.Date <- function() REPORTE_HOY
`%||%` <- function(a, b) if (is.null(a)) b else a
for (f in c("R/i18n.R", "R/monedas.R", "R/empresas_identidad.R", "R/data_pipeline.R", "R/reporte_module.R"))
  if (file.exists(f)) source(f, local = FALSE)
if (!exists("fmt_money"))
  fmt_money <- function(x) paste0("$", formatC(x, format = "f", digits = 2, big.mark = ","))
load_ctas_bancos <- function(client_id = NULL)
  data.frame(id = c("b1", "b2"), nombre = c("BanBajío", "BBVA"), stringsAsFactors = FALSE)
log_action <- function(...) invisible(NULL)

reporte_ng <- function(hoy = REPORTE_HOY) {
  cuentas <- data.frame(
    id = c("c1", "c2", "c3", "c4"),
    Empresa = c("NG", "NG", "NRS", "NRS"),
    Moneda = c("MXN", "USD", "MXN", "USD"),
    alias = c("NG Operativa", "NG Dólares", "NRS Pesos", "NRS USD"),
    banco_id = c("b1", "b1", "b2", "b2"),
    saldo_inicial = c(150000, 20000, 90000, -1500),
    stringsAsFactors = FALSE)
  d0 <- lubridate::floor_date(hoy, "month")
  movs <- data.frame(
    cuenta_id = c("c1", "c1", "c2", "c3", "c3", "c4", "c2", "c1"),
    cargo = c(12000, 0, 3000, 0, 4500, 800, 0, 2500),
    abono = c(0, 54000, 0, 7000, 0, 0, 1200, 0),
    fecha = d0 + c(0, 1, 1, 2, 2, 3, 3, 0),
    eliminado = FALSE, fuente = "txt",
    tipo = c("spei_out", "spei_in", "spei_out", "traspaso", "nomina", "pos", "spei_in", "comision"),
    parte = c("Proveedor Uno", "Cliente Grande SA de CV", "Vendor USA", "", "Nómina", "Tienda", "US Client", "Banco"),
    concepto = c("Pago", "Cobro", "Pay", "Traspaso entre cuentas", "Quincena", "Compra", "Payment", "Comisión"),
    stringsAsFactors = FALSE)
  movs <- movs[movs$fecha <= hoy, , drop = FALSE]
  semana <- lubridate::floor_date(hoy, "week", week_start = 1)
  ar <- data.frame(
    `Fecha de vencimiento` = format(semana + c(0, 2, 4, 8, 10, 3)),
    `Saldo vencido` = c(25000, 8000, 1300, 40000, 2200, 900),
    Moneda = c("MXN", "USD", "MXN", "MXN", "USD", "EUR"),
    `Nombre del cliente` = c("Cliente A", "Client B", "Cliente C", "Cliente D", "Client E", "Kunde F"),
    Empresa = c("NG", "NG", "NRS", "NG", "NRS", "NG"),
    Documento = c("A1", "A2", "A3", "A4", "A5", "A6"),
    check.names = FALSE, stringsAsFactors = FALSE)
  ap <- data.frame(
    `Fecha de vencimiento` = format(semana + c(1, 3, 9, 11, 5)),
    `Saldo vencido` = c(18000, 4100, 9000, 3300, 700),
    Moneda = c("MXN", "USD", "MXN", "USD", "EUR"),
    `Nombre de acreedor` = c("Proveedor A", "Vendor B", "Proveedor C", "Vendor D", "Lieferant E"),
    Empresa = c("NG", "NRS", "NG", "NG", "NRS"),
    Documento = c("P1", "P2", "P3", "P4", "P5"),
    check.names = FALSE, stringsAsFactors = FALSE)
  list(cuentas = cuentas, movs = movs, sap = list(AR = ar, AP = ap))
}

reporte_correr <- function(cuentas, movs, sap, base = NULL) {
  shared <- list(
    ctas_cuentas = reactive(cuentas),
    bancos_movimientos = reactive(movs),
    sap_data = reactive(sap),
    tags_db = reactive(NULL),
    bancos_confirmados = reactive(NULL),
    papelera_rv = reactive(NULL),
    company_map = reactive(c(NG = "Networks Group", NRS = "Networks Realtors")),
    group_name = reactive("Networks Group"),
    visible_initials = reactive(NULL),
    effective_client_id = reactive("cliente_prueba"),
    current_user = reactive("prueba"))
  if (!is.null(base)) shared$moneda_base_cliente <- reactive(list(moneda = base, origen = "ajuste"))
  out <- NULL
  testServer(reporteServer, args = list(shared = shared), {
    session$setInputs(chk_emp_NG = TRUE, chk_emp_NRS = TRUE)
    html <- report_html()
    resumen <- as.character(output$preview_summary$html)
    out <<- list(html = html, resumen = resumen)
  })
  # The generation time and the sparklines' gradient ids (sample.int()) differ
  # on every run; both are masked.
  mask <- function(x) {
    x <- gsub("[0-9]{2}/[0-9]{2}/[0-9]{4} [0-9]{2}:[0-9]{2}", "<GEN>", x)
    gsub("(id=\"|url\\(#)g[0-9]{1,4}", "\\1g<ID>", x, perl = TRUE)
  }
  list(html = mask(out$html), resumen = mask(out$resumen))
}
