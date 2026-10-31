# =============================================================================
# tests/helpers/vencidos_fixture_app.R
# A Shiny app with the real Vencidos module over fixed data (no S3, no
# global.R): a simulated `shared` with 2 companies, 2 currencies, AR and AP,
# groups, tags, paid ("ghost") rows, a provision, a manual invoice, an
# intercompany row (by name), a row with no company and one already in the
# Agenda. Actions never reach S3: a recorder applies tags, staging and
# deletes to the in-memory data and lists every payload in #registro.
#
# The same data feeds the lupa (search_module.R): input btn_search opens it
# on input month_sel ("AAAA-MM-DD"; this month if never set). Two invoices
# fall in the month after next, so another month has its own rows.
#
# VEN_RAIZ: the repo whose R/ and www/ to load (default: this one). Point it
# at an older checkout to see the old Vencidos with the same data.
#
#   start_bg_app(sprintf('Sys.setenv(VEN_RAIZ = "%s"); setwd("%s"); source("tests/helpers/vencidos_fixture_app.R")$value',
#                        raiz, repo), port)
# =============================================================================
library(shiny)
raiz <- normalizePath(Sys.getenv("VEN_RAIZ", "."), winslash = "/")
assign("%||%", function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x,
       envir = globalenv())

# Only these definitions from each file (the files' other top-level code is
# app setup that needs S3 and secrets).
cargar <- function(archivo, nombres) {
  ex <- parse(file.path(raiz, archivo), keep.source = FALSE)
  for (e in ex) {
    if (is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
  }
}
cargar("R/erp_connector_registry.R", "SAP_B1_ERP_TYPE")
cargar("R/data_pipeline.R", c("apply_ic_filter", "build_ic_fullcodes"))
cargar("R/pasivos_capabilities.R", c("PASIVOS_CAPABILITIES", "has_capability"))
# has_capability() asks the session's user through the permissions registry
# (R/permisos.R, which needs a logged-in session): here, a user who may do
# everything.
if (!exists(".usuario_de_sesion")) assign(".usuario_de_sesion", function() "luis", envir = globalenv())
if (!exists("puede")) assign("puede", function(usuario, permiso, ...) TRUE, envir = globalenv())
cargar("R/global.R", "fmt_money")
cargar("R/persistence.R", c("tag_label", "add_tag_labels", "normalize_code"))
cargar("R/ui_components.R", "app_styles")
local({
  owd <- setwd(raiz); on.exit(setwd(owd))
  for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
              "R/search_module.R", "R/vencidos_module.R"))
    if (file.exists(f)) sys.source(f, envir = globalenv())
})
assign(".i18n_dir", file.path(raiz, "www", "i18n"), envir = globalenv())
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

# ── Fixed data ───────────────────────────────────────────────────────────────
CMAP <- c(NTS = "NETWORKS TRANS", NCS = "NETWORKS CARGO")
# Only the old module (an older checkout) reads the global map; today's
# code must take the session's, shared$company_map().
if (!exists("lupaServer")) assign("COMPANY_MAP", CMAP, envir = globalenv())
hoy <- Sys.Date()
MES_LEJOS <- seq(as.Date(format(hoy, "%Y-%m-01")), by = "month", length.out = 3)[3]   # 1st of the month after next
fila <- function(ledger, emp, mon, doc, parte, dias, saldo, ref = "", source = SAP_B1_ERP_TYPE,
                 confirmed = FALSE, id = "", prov = "", orig = NA, fecha = hoy - dias)
  data.frame(Ledger = ledger, Empresa = emp, Moneda = mon, Documento = doc, Parte = parte, Factura = ref,
             Codigo = paste0(substr(ledger, 2, 2), doc), FechaEff = fecha,
             FechaVenc_Original = if (is.na(orig)) fecha else hoy - orig,
             `Saldo vencido` = saldo, source = source, confirmed = confirmed, id = id, provision_id = prov,
             check.names = FALSE, stringsAsFactors = FALSE)
facturas0 <- rbind(
  # AR MXN: a group of three (one paid), loose rows, one with no company
  fila("AR", "NTS", "MXN", "1001", "ACME LOGISTICA", 5, 12000, "F-1001"),
  fila("AR", "NTS", "MXN", "1002", "ACME LOGISTICA", 5, 8000, "F-1002"),
  fila("AR", "NTS", "MXN", "1003", "ACME LOGISTICA", 5, 3000, "F-1003", confirmed = TRUE),
  fila("AR", "NCS", "MXN", "1004", "BODEGAS DEL NORTE", 20, 45000.5, "F-1004", orig = 40),
  fila("AR", "NTS", "MXN", "1005", "CLIENTE MUY LARGO DE SERVICIOS INTEGRALES DE TRANSPORTE, S.A. DE C.V.", 1, 950),
  fila("AR", "",    "MXN", "1001", "ACME LOGISTICA", 2, 400, "F-1001-B"),
  # AR USD
  fila("AR", "NTS", "USD", "2001", "GLOBAL FREIGHT INC", 9, 15000, "INV-2001"),
  fila("AR", "NCS", "USD", "2002", "GLOBAL FREIGHT INC", 0, 2300, "INV-2002"),
  # AP MXN: a group of two, a provision, a manual invoice, an intercompany row
  fila("AP", "NTS", "MXN", "3001", "ACEROS MONTERREY", 3, 22000, "P-3001"),
  fila("AP", "NTS", "MXN", "3002", "ACEROS MONTERREY", 3, 11000, "P-3002"),
  fila("AP", "NCS", "MXN", "",     "RENTA DE BODEGA", 7, 18000, source = "provision", prov = "PRV-1"),
  fila("AP", "NCS", "MXN", "3003", "PAPELERIA ÑANDU", 12, 640, source = "manual", id = "MAN-1"),
  fila("AP", "NTS", "MXN", "3004", "NETWORKS CARGO", 4, 99999),
  fila("AP", "NTS", "MXN", "3005", "FLETES EL GUERO", 30, 5400.75, "=HIPERVINCULO(\"x\")"),
  # AP USD
  fila("AP", "NCS", "USD", "4001", "SOFTWARE CORP", 6, 1200, "S-4001"),
  # Not overdue: never shows
  fila("AP", "NTS", "MXN", "3999", "FUTURO SA", -10, 7000),
  # The month after next (the lupa's other month)
  fila("AR", "NTS", "MXN", "5001", "MAS ADELANTE SA", 0, 3100, "F-5001", fecha = MES_LEJOS + 14),
  fila("AP", "NCS", "USD", "5002", "MAS ADELANTE SA", 0, 870, "P-5002", fecha = MES_LEJOS + 19)
)
# VEN_VACIO=1: no overdue invoices at all (the empty message).
if (identical(Sys.getenv("VEN_VACIO"), "1")) facturas0 <- facturas0[0, , drop = FALSE]
tags0 <- data.frame(ledger = c("AR", "AR", "AR", "AP", "AP"), Empresa = c("NCS", "NTS", "NTS", "NTS", "NTS"),
                    Moneda = c("MXN", "USD", "USD", "MXN", "MXN"), Documento = c("1004", "2001", "2001", "3005", "3001"),
                    tag = c("urgent", "urgent", "important", "important", "urgent"), stringsAsFactors = FALSE)
agenda0 <- data.frame(id = "AG-1", ledger = "AP", Empresa = "NTS", Moneda = "MXN", Documento = "3002",
                      status = "pending", stringsAsFactors = FALSE)
crudos0 <- list(
  AR = data.frame(Empresa = c("NTS", "NCS"), `Número de documento` = c("1001", "1004"),
                  `Fecha de vencimiento` = hoy - c(5, 20), Total = c(12000, 45000.5), check.names = FALSE),
  AP = data.frame(Empresa = "NTS", `Número de documento` = "3001", `Fecha de vencimiento` = hoy - 3,
                  Total = 22000, check.names = FALSE))

# ── App ────────────────────────────────────────────────────────────────────────
ui <- bslib::page_navbar(
  title = "HopDesk", id = "nav", fillable = TRUE,
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#0A58CA"),
  header = tagList(app_styles(), tags$head(tags$script(src = "i18n.js"))),
  bslib::nav_panel(
    title = tagList(icon("clock-rotate-left"), " Vencidos ",
                    tags$span(id = "ven_tab_badge", class = "badge rounded-pill bg-danger",
                              style = "font-size:.6rem; vertical-align:middle; display:none;", "")),
    value = "VEN",
    # The lupa's button lives in the Calendario; here the tests press it
    # (Shiny.setInputValue('btn_search', ...)).
    div(class = "h-100 overflow-auto", vencidosUI("ven"),
        tags$pre(id = "registro", style = "font-size:.7rem; max-height:160px; overflow:auto;"))
  )
)

server <- function(input, output, session) {
  facturas <- reactiveVal(facturas0)
  tags_db  <- reactiveVal(tags0)
  agenda   <- reactiveVal(agenda0)
  registro <- reactiveVal(character(0))
  de <- function(l) reactive({ d <- facturas(); d[d$Ledger == l, setdiff(names(d), "Ledger"), drop = FALSE] })
  shared <- list(
    interco_v2 = reactive(list()), tags_db = tags_db, pagar_hoy_db = agenda,
    ic_mode = list(AR = reactive("exclude"), AP = reactive("exclude")),
    df_combined_AR = de("AR"), df_combined_AP = de("AP"),
    empresa_sel = reactive(c("NTS", "NCS")), company_map = reactive(CMAP),
    current_user = reactive("luis"), sap_data = reactive(crudos0), amount_col = reactive("Saldo vencido")
  )
  # What handle_invoice_action() would do, in memory, and a line per call.
  accion <- function(payload, shared, session = NULL) {
    rows <- payload$rows
    docs <- vapply(rows, function(r) paste(r$ledger, r$empresa, r$moneda, r$documento), "")
    registro(c(registro(), sprintf("%s %s [%d] %s%s", format(Sys.time(), "%H:%M:%OS2"), payload$action, length(rows),
                                   paste(docs, collapse = "; "), if (!is.null(payload$move_to)) paste0(" -> ", payload$move_to) else "")))
    tg <- switch(payload$action, tag_urgent = "urgent", tag_important = "important",
                 tag_both = c("urgent", "important"), tag_clear = character(0), NULL)
    if (!is.null(tg) || identical(payload$action, "tag_clear")) {
      t <- tags_db()
      for (r in rows) {
        t <- t[!(t$ledger == r$ledger & t$Empresa == r$empresa & t$Moneda == r$moneda & t$Documento == r$documento), ]
        for (x in tg) t <- rbind(t, data.frame(ledger = r$ledger, Empresa = r$empresa, Moneda = r$moneda,
                                               Documento = r$documento, tag = x, stringsAsFactors = FALSE))
      }
      tags_db(t)
    }
    if (identical(payload$action, "stage_all")) {
      nuevas <- do.call(rbind, lapply(rows, function(r) data.frame(id = paste0("AG-", r$documento), ledger = r$ledger,
        Empresa = r$empresa, Moneda = r$moneda, Documento = r$documento, status = "pending", stringsAsFactors = FALSE)))
      agenda(rbind(agenda(), nuevas))
    }
    if (identical(payload$action, "delete")) {
      d <- facturas()
      fuera <- paste(d$Ledger, d$Empresa, d$Moneda, d$Documento) %in% docs
      facturas(d[!fuera, , drop = FALSE])
    }
  }
  # The old module sends its actions to the app's input$vencidos_action.
  observeEvent(input$vencidos_action, accion(input$vencidos_action, shared))
  srv <- get("vencidosServer")
  if ("accion_factura" %in% names(formals(srv))) srv("ven", shared = shared, accion_factura = accion)
  else srv("ven", shared = shared)
  # The ⚡ of a provision asks the pasivos module to convert it.
  observeEvent(input$pasivos_convert_request,
               registro(c(registro(), paste("convertir", input$pasivos_convert_request))))
  # The lupa: the new module, or (an older checkout) the old modal.
  if (exists("lupaServer")) {
    lupaServer("lupa", shared, abrir = reactive(input$btn_search), mes = reactive(input$month_sel),
               accion_factura = accion)
  } else {
    observeEvent(input$btn_search, show_search_modal(input, output, session, shared, reactiveVal(NULL)))
    observeEvent(input$search_action, accion(input$search_action, shared))
  }
  observe(session$sendCustomMessage("registro", paste(registro(), collapse = "\n")))
}
# The log takes no room (nor shows as a grey strip) until something is in it.
ui <- tagList(ui, tags$style("#registro:empty { display: none; }"), tags$script(HTML("Shiny.addCustomMessageHandler('registro', function (t) { document.getElementById('registro').textContent = t; });")))

app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(raiz, "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
