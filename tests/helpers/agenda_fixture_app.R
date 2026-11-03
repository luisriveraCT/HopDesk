# =============================================================================
# tests/helpers/agenda_fixture_app.R
# Agenda de Hoy (the real pagarUI/pagarHoyServer of R/pagar_hoy_module.R) over
# fixed data, without S3. One company (Networks Trucking Services, "NTS"),
# Pagos in MXN and USD and Cobros in MXN, against a small supplier catalog:
# an included supplier, one without CLABE and one not in the catalog, plus
# an abono.
#
# Everything that would write or reach S3 is a recorder: confirming writes
# the confirmed ids to #registro ("confirmados AP: id,id") and takes them out
# of the in-memory Agenda; Quitar does the same ("quitados AP: ..."); the
# payment file only records what it would pay ("ppl: 15000 F-4001,..." --
# amount and concepto per line).
#
# Raw SAP rows (shared$sap_data) for documents 4001 and 4002 (and one that
# isn't in the Agenda), plus document 4001 of ANOTHER company (OTR), for
# "Ver datos de origen"; their Factura is the
# Pagos table's Referencia (F-4001, F-4002).
# Every session shares one Agenda (two sessions: one confirms what the other
# had marked); each has its own #registro.
# AGENDA_N: that many more MXN payments (default 0): AGENDA_N=850 puts the
# table in window mode. The last one is "PROVEEDOR ULTIMO", Referencia
# "REF-ULTIMA", due last.
# =============================================================================
library(shiny)
raiz <- normalizePath(".", winslash = "/")
assign("%||%", function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x,
       envir = globalenv())
cargar <- function(archivo, nombres) {
  ex <- parse(file.path(raiz, archivo), keep.source = FALSE)
  for (e in ex)
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
}
cargar("R/erp_connector_registry.R", "SAP_B1_ERP_TYPE")
cargar("R/global.R", c("fmt_money", "is_erp_sourced", "CURRENCIES"))
cargar("R/persistence.R", "unstage_pagar_hoy")
cargar("R/ui_components.R", "app_styles")
cargar("R/search_module.R", "mostrar_datos_sap")
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/ppl_generator.R", "R/pagar_hoy_module.R"))
  sys.source(file.path(raiz, f), envir = globalenv())
assign(".i18n_dir", file.path(raiz, "www", "i18n"), envir = globalenv())
suppressPackageStartupMessages({ library(dplyr) })

# ── Recorders and stubs (after the module, so they win) ─────────────────────
# Each session's own #registro (a write by session B shows in B's, not A's).
.anota <- function(...) shiny::getDefaultReactiveDomain()$userData$anota(...)
stubs <- list(
  puede_o_avisa = function(shared, permiso, ...) TRUE,
  log_action = function(...) invisible(NULL),
  bump_sync_version = function(...) invisible(NULL),
  safe_load_pagar_hoy = function(...) NULL,          # falls back to shared$pagar_hoy_db()
  save_pagar_hoy_with_retry = function(...) list(ok = TRUE, attempts = 1L),
  save_pagar_hoy = function(...) invisible(TRUE),
  modificar_bancos_confirmados = function(cid, f) {
    nuevas <- f(tibble::tibble())
    .anota(sprintf("confirmados AP: %s", paste(nuevas$agenda_item_id, collapse = ",")))
    nuevas
  },
  load_ctas_cuentas = function(...) NULL,
  get_cuentas_ppl_data = function(emp, cur, shared)
    data.frame(cuenta = "012180001111111111", banco = "012", alias = "Operativa", is_ppl_default = TRUE,
               stringsAsFactors = FALSE),
  get_cuenta_ppl = function(...) "012180001111111111",
  .log_pago_generado = function(shared, emp, moneda, cuenta_origen, ppl_rows, ...)
    .anota(sprintf("ppl: %s", paste(ppl_rows$importe, ppl_rows$concepto, collapse = ","))),
  # Confirming an abono: the abonos store
  load_abonos = function(...) tibble::tibble(),
  upsert_abono = function(db, nuevo) {
    .anota(sprintf("abonos: %s", paste(nuevo$Documento, collapse = ",")))
    dplyr::bind_rows(db, nuevo)
  },
  save_abonos = function(...) invisible(TRUE)
)
for (n in names(stubs)) assign(n, stubs[[n]], envir = globalenv())

# ── Data ─────────────────────────────────────────────────────────────────────
EMP <- "Networks Trucking Services"
OTR <- "Otra Empresa SA"   # a second company, with its own document 4001
catalogo <- tibble::tibble(
  nombre = c("ACEROS MONTERREY SA", "BODEGAS DEL NORTE SA", "PROVEEDOR ULTIMO SA"),
  alias  = c("ACEROS", "BODEGAS", "ULTIMO"),
  clabe  = c("012345678901234567", NA, "014180002222222222"),
  medio_pago = "SPEI", banco = c("012", "014", "014"), Empresa = NA_character_)
pago <- function(id, parte, doc, imp, fecha, moneda = "MXN", tipo = NA_character_, ledger = "AP")
  tibble::tibble(id = id, ledger = ledger, Empresa = EMP, Moneda = moneda, Documento = doc, Parte = parte,
                 Codigo = NA_character_, Importe = imp, FechaVenc = as.Date(fecha), status = "pending",
                 tipo_item = tipo, source = SAP_B1_ERP_TYPE, provision_id = NA_character_,
                 staged_by = "luis", staged_at = Sys.time(), confirmed_at = as.POSIXct(NA))
agenda0 <- dplyr::bind_rows(
  pago("P1", "ACEROS MONTERREY SA",  "4001", 15000, "2026-11-03"),
  pago("P2", "BODEGAS DEL NORTE SA", "4002",  8200, "2026-11-01"),
  pago("P3", "TRANSPORTES DESCONOCIDOS", "4003", 3100, "2026-11-02"),
  pago("P4", "ACEROS MONTERREY SA",  "4004",   400, "2026-11-04", tipo = "abono"),
  pago("P5", "ACEROS MONTERREY SA",  "4005",  1200, "2026-11-02", moneda = "USD"),
  pago("P6", "BODEGAS DEL NORTE SA", "4006",   300, "2026-11-03", moneda = "USD"),
  dplyr::mutate(pago("Q1", "ACEROS MONTERREY SA", "4001", 700, "2026-11-05"), Empresa = OTR),
  pago("C1", "CLIENTE UNO SA",       "9001", 50000, "2026-11-02", ledger = "AR"),
  pago("C2", "CLIENTE DOS SA",       "9002", 12000, "2026-11-01", ledger = "AR"))
n_extra <- as.integer(Sys.getenv("AGENDA_N", "0"))
if (n_extra > 0L) {
  extra <- pago(sprintf("X%04d", seq_len(n_extra)), sprintf("PROVEEDOR %04d SA", seq_len(n_extra)),
                sprintf("7%04d", seq_len(n_extra)), 100 + seq_len(n_extra), "2026-11-10")
  extra$Parte[n_extra] <- "PROVEEDOR ULTIMO SA"; extra$Documento[n_extra] <- "REF-ULTIMA"
  extra$FechaVenc[n_extra] <- as.Date("2026-12-31")
  agenda0 <- dplyr::bind_rows(agenda0, extra)
}

# The Agenda every session sees: what one session confirms or removes, the
# others see go (the sync bus, in the app).
# HD_ARRANQUE_NULL = 1: the sources start NULL, as before S3 loads at
# startup (tests/test_hd_table_arranque_null.R; incident 2026-11-03).
if (identical(Sys.getenv("HD_ARRANQUE_NULL"), "1")) { agenda0 <- NULL; catalogo <- NULL }

AGENDA <- reactiveVal(agenda0)

# ── App ──────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#0A58CA"),
  app_styles(), tags$head(tags$script(src = "i18n.js")),
  pagarHoyUI("ph"),
  tags$pre(id = "registro", style = "font-size:.7rem;"),
  tags$script(HTML("Shiny.addCustomMessageHandler('registro', function (t) { document.getElementById('registro').textContent = t; });"))
)

server <- function(input, output, session) {
  registro <- reactiveVal(character(0))
  session$userData$anota <- function(...) {
    registro(c(registro(), paste0(...)))
    session$sendCustomMessage("registro", paste(registro(), collapse = "\n"))
  }
  ph <- AGENDA   # one Agenda for every session, as the sync bus makes it
  shared <- list(
    company_map = reactiveVal(c(NTS = EMP, OTR = OTR)), proveedores_db = reactiveVal(catalogo),
    parte_alias_map_db = reactiveVal(NULL), pagar_hoy_db = ph,
    sap_data = reactiveVal(list(AP = data.frame(Empresa = c("NTS", "NTS", "NTS", "OTR"),
                                                Documento = c("4001", "4002", "4999", "4001"),
                                                Factura = c("F-4001", "F-4002", "F-4999", "F-OTRA"),
                                                Parte = c("ACEROS MONTERREY SA", "BODEGAS DEL NORTE SA", "OTRO", "OTRA EMPRESA"),
                                                `Saldo vencido` = c(15000, 8200, 1, 7), check.names = FALSE))), sap_ov_db = reactiveVal(NULL), manual_inv = reactiveVal(NULL),
    bancos_confirmados = reactiveVal(tibble::tibble()), abonos_db = reactiveVal(NULL),
    effective_client_id = function() "dev", current_user = function() "luis",
    home_client_id = function() "dev")
  # Quitar and the PPL leave a trace too: watch the in-memory Agenda.
  observeEvent(ph(), {
    antes <- isolate(session$userData$ids_antes) %||% agenda0$id
    fuera <- setdiff(antes, ph()$id)
    session$userData$ids_antes <- ph()$id
    if (length(fuera)) .anota(sprintf("fuera de la Agenda: %s", paste(sort(fuera), collapse = ",")))
  }, ignoreInit = TRUE)
  pagarHoyServer("ph", shared)
}

app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(raiz, "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
