# =============================================================================
# tests/helpers/dia_fixture_app.R
# The Calendario's day modal over fixed data, without S3 or the rest of the
# Calendario: the pieces of ledgerModuleServer that make the modal --
# modal_ctx, the render gate, .render_day_modal and the outputs it uses,
# and (today) dia_server() -- are taken from R/ledger_module.R as they are
# and evaluated in a small module. One AP day, MXN: groups, tags, one invoice
# already in the Agenda, a paid (ghost) one, a provision, a manual entry, a
# moved one and one with a SAP override.
#
# The handlers that would write (Agenda, tags, moves, papelera, edit, raw
# data) are recorders: they apply what they can in memory (tags, the
# Agenda, deletes) and list every call in #registro.
#
# DIA_RAIZ: the checkout whose R/ and www/ to use (default: this one). An
# older checkout shows the day modal as it was (cal_cart.js + DT).
#   input "abrir" opens the day; "abrir_detalle" opens it in Detalle.
# DIA_EXTRA: more invoices for the day (see below).
# =============================================================================
library(shiny)
raiz <- normalizePath(Sys.getenv("DIA_RAIZ", "."), winslash = "/")
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
cargar("R/persistence.R", c("tag_label", "set_invoice_tags"))
cargar("R/pasivos_capabilities.R", c("PASIVOS_CAPABILITIES", "has_capability"))
cargar("R/ui_components.R", "app_styles")
sys.source(file.path(raiz, "R/policy_engine.R"), envir = globalenv())   # only definitions
if (!exists(".usuario_de_sesion")) assign(".usuario_de_sesion", function() "luis", envir = globalenv())
# HD_SIN_AGENDA = 1: a user without can_register_payments (no Agregar / ＋).
if (!exists("puede")) assign("puede", function(usuario, permiso, ...)
  !(identical(Sys.getenv("HD_SIN_AGENDA"), "1") && identical(permiso, "can_register_payments")), envir = globalenv())
local({
  owd <- setwd(raiz); on.exit(setwd(owd))
  for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
              "R/vencidos_module.R", "R/dia_module.R"))
    if (file.exists(f)) sys.source(f, envir = globalenv())
})
assign(".i18n_dir", file.path(raiz, "www", "i18n"), envir = globalenv())
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

# ── The modal's pieces, from ledgerModuleServer ─────────────────────────────
# Statements of its moduleServer body: these assignments and observers.
piezas_modal <- function() {
  ex <- parse(file.path(raiz, "R/ledger_module.R"), keep.source = FALSE)
  srv <- Find(function(e) is.call(e) && identical(as.character(e[[2]]), "ledgerModuleServer"), ex)
  cuerpo <- srv[[3]][[3]]                              # function(id, config, shared) { moduleServer(...) }
  ms <- Find(function(e) is.call(e) && identical(as.character(e[[1]]), "moduleServer"), as.list(cuerpo)[-1])
  stmts <- as.list(ms[[3]][[3]])[-1]
  # (cart_expanded, modal_tbl, sel_total_ui, move_to_hint_ui: an older
  # checkout's day modal, DIA_RAIZ.)
  nombres <- c("modal_ctx", "modal_open", "cart_expanded", ".render_day_modal", "ns", "ledger")
  salidas <- c("modal_tbl", "sel_total_ui", "move_to_hint_ui", "dia_agenda")
  Filter(function(e) {
    if (!is.call(e)) return(FALSE)
    f <- as.character(e[[1]])[1]
    if (f == "<-" && is.name(e[[2]])) return(as.character(e[[2]]) %in% nombres)
    if (f == "<-" && is.call(e[[2]]) && identical(as.character(e[[2]][[1]]), "$"))
      return(identical(as.character(e[[2]][[2]]), "output") && as.character(e[[2]][[3]]) %in% salidas)
    if (f == "observeEvent") {
      que <- paste(deparse(e[[2]]), collapse = "")
      return(que %in% c("modal_ctx()", "input$dia_vista"))
    }
    f == "dia_server"
  }, stmts)
}

# ── Fixed data: one AP day ───────────────────────────────────────────────────
DIA <- as.Date("2026-11-03")
fila <- function(emp, doc, parte, imp, ref = "", source = SAP_B1_ERP_TYPE, confirmed = FALSE, prov = NA, id = NA,
                 movida = "Falso", override = FALSE, orig = DIA)
  data.frame(Empresa = emp, Moneda = "MXN", Documento = doc, Factura = ref, Parte = parte, Importe = imp,
             FechaEff = DIA, FechaVenc_Original = orig, FechaVenc_Proyectada = if (movida == "Manual") DIA else as.Date(NA),
             Movida = movida, Etiqueta = "", source = source, Tipo = "Pago", Codigo = paste0("P", doc), notas = "",
             confirmed = confirmed, has_sap_override = override, provision_id = prov, id = id,
             is_ghost = FALSE, is_paid_ghost = confirmed, stringsAsFactors = FALSE)
detalle0 <- rbind(
  fila("NTS", "3001", "ACEROS MONTERREY", 22000, "P-3001"),
  fila("NTS", "3002", "ACEROS MONTERREY", 11000, "P-3002"),
  fila("NTS", "3006", "ACEROS MONTERREY", 4000, "P-3006", confirmed = TRUE),
  fila("NCS", "3010", "BODEGAS DEL NORTE", 9500, "B-77", movida = "Manual", orig = DIA - 4),
  fila("NCS", "PROV_1", "RENTA DE BODEGA", 18000, source = "provision", prov = "PRV-1"),
  fila("NCS", "3003", "PAPELERIA ÑANDU", 640, source = "manual", id = "MAN-1"),
  fila("NTS", "3005", "FLETES EL GUERO", 5400.75, "=HIPERVINCULO(\"x\")", override = TRUE),
  fila("NTS", "3007", "TRANSPORTES DEL BAJIO", 12500, "T-12"),
  fila("NTS", "3008", "TRANSPORTES DEL BAJIO", 7300, "T-13")
)
# DIA_EXTRA = n: n more invoices, each from its own supplier (rows, not
# groups), for the height test.
local({
  n <- suppressWarnings(as.integer(Sys.getenv("DIA_EXTRA", "0")))
  if (!is.na(n) && n > 0L)
    detalle0 <<- rbind(detalle0, do.call(rbind, lapply(seq_len(n), function(i)
      fila("NTS", as.character(5000L + i), sprintf("PROVEEDOR %03d", i), 1000 + i, sprintf("X-%d", i)))))
})
tags0   <- data.frame(ledger = "AP", Empresa = "NTS", Moneda = "MXN", Documento = c("3001", "3005"),
                      tag = c("urgent", "important"), stringsAsFactors = FALSE)
agenda0 <- data.frame(id = "AG-1", ledger = "AP", Empresa = "NTS", Moneda = "MXN", Documento = "3002", Importe = 11000,
                      Parte = "ACEROS MONTERREY", status = "pending", stringsAsFactors = FALSE)
# DIA_NOMBRES = 1: the rows carry company NAMES, as the real day does (T09:
# "Networks Crossdocking Services" in Luis's capture), with the real map.
CMAP <- c(NTS = "N T", NCS = "N C")
if (identical(Sys.getenv("DIA_NOMBRES"), "1")) {
  CMAP <- c(NTS = "Networks Trucking Services", NCS = "Networks Crossdocking Services")
  a_nombre <- function(x) ifelse(x %in% names(CMAP), unname(CMAP[x]), x)
  detalle0$Empresa <- a_nombre(detalle0$Empresa)
  tags0$Empresa    <- a_nombre(tags0$Empresa)
  agenda0$Empresa  <- a_nombre(agenda0$Empresa)
}
politicas <- list(
  pp  = data.frame(parte = "BODEGAS DEL NORTE", is_interco = FALSE, policy_order = 1L, policy_id = "POL-1",
                   stringsAsFactors = FALSE),
  cat = { d <- data.frame(id = "POL-1", type = "weekdays", roll_direction = "forward", stringsAsFactors = FALSE)
          d$params <- list(list(days = 5L)); d })   # fridays only

# ── App ──────────────────────────────────────────────────────────────────────
# HD_ARRANQUE_NULL = 1: the sources start NULL, as before S3 loads at
# startup (tests/test_hd_table_arranque_null.R; incident 2026-11-03).
if (identical(Sys.getenv("HD_ARRANQUE_NULL"), "1")) { detalle0 <- NULL; tags0 <- NULL; agenda0 <- NULL }

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#0A58CA"),
  app_styles(), tags$head(tags$script(src = "i18n.js"),
                             if (file.exists(file.path(raiz, "www", "cal_cart.js"))) tags$script(src = "cal_cart.js?v=4")),
  tags$script(HTML("Shiny.addCustomMessageHandler('restoreModalScroll', function(m){});")),
  h4("Calendario (fixture)"),
  tags$pre(id = "registro", style = "font-size:.7rem;")
)
ui <- tagList(ui, tags$style("#registro:empty { display: none; }"),
              tags$script(HTML("Shiny.addCustomMessageHandler('registro', function (t) { document.getElementById('registro').textContent = t; });")))

server <- function(input, output, session) {
  registro <- reactiveVal(character(0))
  anota <- function(...) registro(c(registro(), paste0(...)))
  detalle_rv <- reactiveVal(detalle0)
  shared <- list(
    tags_db = reactiveVal(tags0), pagar_hoy_db = reactiveVal(agenda0), current_user = reactive("luis"),
    effective_client_id = reactive("dev"), home_client_id = reactive("dev"), company_map = reactive(CMAP),
    audit_mode = reactiveVal(FALSE), amount_col = reactive("Saldo vencido"),
    partner_policies_db = reactive(politicas$pp), policy_catalog_db = reactive(politicas$cat))
  observeEvent(input$pasivos_convert_request, anota("convertir ", input$pasivos_convert_request))
  moduleServer("ap", function(input, output, session) {
    ns <- session$ns; ledger <- "AP"; config <- list(ledger = "AP")
    docs <- function(keys) paste(sort(unique(keys$Documento)), collapse = ",")
    # The handlers, recorded (and applied in memory where it's simple).
    .dia_agregar <- function(keys, origen) {
      anota("agregar ", origen, " [", nrow(keys), "] ", docs(keys))
      ph <- shared$pagar_hoy_db()
      nuevos <- data.frame(id = paste0("AG-", keys$Documento), ledger = "AP", Empresa = keys$Empresa, Moneda = keys$Moneda,
                           Documento = keys$Documento, Importe = keys$Importe, Parte = keys$Parte, status = "pending")
      shared$pagar_hoy_db(rbind(ph, nuevos[keys$Documento != "" & !startsWith(keys$Documento, "PROV_"), , drop = FALSE]))
    }
    .dia_quitar <- function(keys) {
      anota("quitar [", nrow(keys), "] ", docs(keys))
      ph <- shared$pagar_hoy_db()
      shared$pagar_hoy_db(ph[!paste(ph$Empresa, ph$Documento) %in% paste(keys$Empresa, keys$Documento), , drop = FALSE])
    }
    .dia_etiquetar <- function(rows, new_tags) {
      anota("etiquetar ", paste(new_tags, collapse = "+"), " [", nrow(rows), "] ", docs(rows))
      t <- shared$tags_db()
      t <- t[!paste(t$Empresa, t$Documento) %in% paste(rows$Empresa, rows$Documento), , drop = FALSE]
      for (k in seq_len(nrow(rows))) for (g in new_tags)
        t <- rbind(t, data.frame(ledger = "AP", Empresa = rows$Empresa[k], Moneda = rows$Moneda[k], Documento = rows$Documento[k], tag = g))
      shared$tags_db(t)
    }
    .dia_mover     <- function(keys, new_date) anota("mover [", nrow(keys), "] ", docs(keys), " -> ", format(new_date))
    .dia_restaurar <- function(keys) anota("restaurar [", nrow(keys), "] ", docs(keys))
    .dia_eliminar  <- function(keys) {
      anota("eliminar [", nrow(keys), "] ", docs(keys))
      d <- detalle_rv(); d <- d[!paste(d$Empresa, d$Documento) %in% paste(keys$Empresa, keys$Documento), , drop = FALSE]
      detalle_rv(d)
      c <- modal_ctx(); c$detail <- d; c$prov_refresh <- TRUE; c$nonce <- runif(1)   # (modifyList would walk into detail)
      modal_ctx(c)
    }
    .dia_editar <- function(row) anota("editar ", row$Documento, " (", row$source, ", notas=", row$notas %||% "", ")")
    .dia_crudos <- function() anota("crudos")
    for (e in piezas_modal()) eval(e)
    abrir <- function(audit) modal_ctx(list(detail = detalle_rv(), sel_date = DIA, cur = "MXN", amt = "Saldo vencido",
                                            audit = audit, nonce = runif(1)))
    observeEvent(session$rootScope()$input$abrir, abrir(isTRUE(shared$audit_mode())))
    observeEvent(session$rootScope()$input$abrir_detalle, abrir(TRUE))
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
