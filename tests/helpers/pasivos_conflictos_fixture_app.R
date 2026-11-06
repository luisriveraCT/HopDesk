# =============================================================================
# tests/helpers/pasivos_conflictos_fixture_app.R
# The edit-confirm panel of a Pasivos edit ("Conflictos al editar un pasivo",
# the real R/pasivos_edit_confirm_module.R, hd_table "conflictos_pasivo") on
# the in-memory S3 of tests/helpers/s3_memoria.R: every save lands in that
# store, never in a real bucket.
#
# L1 (monthly, 100) is edited to 120 with the real wizard save. Two of its
# future provisions were edited by hand: P1's amount (150) and P2's date
# (3 days later). The save keeps both and hands them to the panel.
#   #abrir      opens the panel with those two conflicts (as the wizard does)
#   #registro   P1 and P2 as the store has them after each save (P2 with its
#               date override, NA once Nuevo applies the new date):
#               "P1=<amount>/<override> P2=<amount>/<override>/<date override>"
#   #ids        P1's and P2's provision ids ("P1=<id> P2=<id>")
# =============================================================================
library(shiny)
suppressPackageStartupMessages({ library(dplyr); library(tibble); library(lubridate); library(uuid) })
raiz <- normalizePath(".", winslash = "/")
source(file.path(raiz, "tests/helpers/s3_memoria.R"))
cargar <- function(archivo, nombres) {
  ex <- parse(file.path(raiz, archivo), keep.source = FALSE)
  for (e in ex)
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
}
cargar("R/global.R", c("fmt_money", "CURRENCIES", "%||%"))
cargar("R/ui_components.R", "app_styles")
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R",
            "R/pasivos_capabilities.R", "R/pasivos_audit.R", "R/forecasting_service.R",
            "R/policy_engine.R", "R/pasivos_engine.R", "R/pasivos_wizard_validate.R",
            "R/pasivos_wizard_module.R", "R/pasivos_edit_confirm_module.R"))
  suppressMessages(sys.source(file.path(raiz, f), envir = globalenv()))
assign(".i18n_dir", file.path(raiz, "www", "i18n"), envir = globalenv())
assign("bump_sync_version", function(...) invisible(NULL), envir = globalenv())
assign("pasivos_log_audit", function(...) invisible(NULL), envir = globalenv())
assign("has_capability", function(user, cap) TRUE, envir = globalenv())
assign("pasivos_autoriza", function(...) TRUE, envir = globalenv())

CID <- "acme"
liab0 <- pasivos_wizard_draft_to_liability(list(
  categoria = "regular", nombre = "Renta bodega", empresa = "NG", parte = "Inmobiliaria",
  moneda_pago = "MXN", recurrence_type = "monthly_day", recurrence_params = list(day = 15L),
  fecha_inicio = Sys.Date(), amount_default = 100), mode = "create", user = "ana")
liab0$id <- "L1"
provs0 <- pasivos_generate_provisions(liab0, window_start = Sys.Date())
P1 <- provs0$id[3]; P2 <- provs0$id[5]
provs0$amount_pago_override[provs0$id == P1] <- 150
f2 <- provs0$fecha_efectiva[provs0$id == P2] + 3L
provs0$fecha_efectiva[provs0$id == P2] <- f2
provs0$fecha_efectiva_override[provs0$id == P2] <- f2
s3_memoria_reset()
save_pasivos_liabilities(liab0, client_id = CID)
.s3_write(provs0, S3_KEYS$pasivos_provisions, client_id = CID)

nuevo_shared <- function() {
  L <- reactiveVal(liab0); P <- reactiveVal(load_pasivos_provisions(client_id = CID))
  list(pasivos_liabilities_db = L, pasivos_provisions_db = P,
       company_map = function() list(NG = "Nuevo Grupo"),
       holiday_overrides_db = function() list(),
       effective_client_id = function() CID, current_user = function() "ana",
       suppress_ledger_prov_refresh = function(...) invisible(NULL))
}
# The wizard's real save (edit L1 to 120), once: what it hands the panel.
abierto <- NULL
local({
  abrir_real <- pasivos_edit_confirm_open
  pasivos_edit_confirm_open <<- function(session, conflicts, replacement, shared, liability_id)
    abierto <<- list(conflicts = conflicts, replacement = replacement)
  draft <- pasivos_liability_to_draft(liab0); draft$amount_default <- 120
  wiz <- list(mode = "edit", edit_target_id = "L1", draft = draft)
  res <- NULL
  testServer(function(input, output, session) res <<- .pasivos_wizard_save(wiz, nuevo_shared(), session, "ana"), {})
  pasivos_edit_confirm_open <<- abrir_real
  if (!setequal(abierto$conflicts$id, c(P1, P2)))
    stop("the wizard save didn't hand the panel P1 and P2: ", paste(utils::capture.output(str(res)), collapse = " "))
})

registro <- function() {
  p <- load_pasivos_provisions(client_id = CID)
  f <- function(id) sprintf("%s/%s", p$amount_pago[p$id == id], p$amount_pago_override[p$id == id])
  sprintf("P1=%s P2=%s/%s", f(P1), f(P2), format(p$fecha_efectiva_override[p$id == P2]))
}

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  app_styles(),
  actionButton("abrir", "abrir"),
  tags$pre(id = "ids", sprintf("P1=%s P2=%s", P1, P2)),
  verbatimTextOutput("registro")
)
server <- function(input, output, session) {
  sh <- nuevo_shared()
  setup_pasivos_edit_confirm(input, output, session, sh)
  observeEvent(input$abrir, pasivos_edit_confirm_open(session, abierto$conflicts, abierto$replacement, sh, "L1"))
  output$registro <- renderText({ input$pec_save; input$abrir; invalidateLater(500); registro() })
}
app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(raiz, "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
