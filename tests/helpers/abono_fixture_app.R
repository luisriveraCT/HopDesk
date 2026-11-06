# =============================================================================
# tests/helpers/abono_fixture_app.R
# Abono parcial (R/staging_browse_module.R, the real show_abono_modal() and
# setup_abono_browse(); hd_table "abono_parcial" since T25) as app.R wires
# them: a "pick_abono" button opens the modal in the top-level session.
# Data: tests/helpers/abono_datos_prueba.R. Never S3: the Agenda read/save and log_action are stubs in this process.
# #eco shows, as JSON, what was saved (Documento, Importe per new row), the
# notifications and how many times the modal closed by itself.
# =============================================================================
library(shiny)
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
    eval(e, envir = globalenv())
cargar("R/global.R", "CURRENCIES")
cargar("R/persistence.R", c("active_abonos_summary", "upsert_pagar_hoy"))
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/staging_browse_module.R")) source(f)
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())
source("tests/helpers/abono_datos_prueba.R")

D <- abono_datos_prueba(Sys.Date())
ALMACEN <- new.env()
ALMACEN$ph <- D$pagar_hoy
safe_load_pagar_hoy <- function(username = NULL, client_id = NULL) ALMACEN$ph
save_pagar_hoy <- function(df, username = NULL, client_id = NULL, base = NULL) { ALMACEN$ph <- df; invisible(TRUE) }
log_action <- function(...) invisible(NULL)

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  hd_table_deps(),
  actionButton("pick_abono", "Abono parcial", class = "btn btn-warning"),
  verbatimTextOutput("eco")
)
server <- function(input, output, session) {
  ALMACEN$ph <- D$pagar_hoy   # each session (each screen size) starts from the same Agenda
  eco <- reactiveVal(list(guardado = list(), avisos = list(), cierres = 0L))
  anota <- function(campo, valor) { e <- eco(); e[[campo]] <- valor; eco(e) }
  # The module's notifications and removeModal(), also recorded.
  showNotification <- function(ui, ..., type = "default") {
    anota("avisos", c(eco()$avisos, list(list(texto = paste(ui, collapse = ""), tipo = type))))
    shiny::showNotification(ui, ..., type = type)
  }
  removeModal <- function(...) { anota("cierres", eco()$cierres + 1L); shiny::removeModal(...) }
  environment(setup_abono_browse) <- environment()
  ph <- reactiveVal(D$pagar_hoy)
  observe({
    df <- ph(); n <- df[df$id != "ph1", ]
    anota("guardado", lapply(seq_len(nrow(n)), function(k) list(Documento = n$Documento[k], Importe = n$Importe[k])))
  })
  setup_abono_browse(input, output, session,
    sap_data = reactive(D$sap), abonos_db = reactiveVal(D$abonos), pagar_hoy_db = ph,
    current_user = function() "tester", client_id = function() "prueba",
    autorizado = function() TRUE)
  observeEvent(input$pick_abono, show_abono_modal(reactive(D$sap), session, empresa_vals = c("NCS", "NTS")))
  output$eco <- renderText(as.character(jsonlite::toJSON(eco(), auto_unbox = TRUE)))
}
app <- shinyApp(ui, server)
# The engine's assets (www/hdtable.*, the dictionary) from the repo.
local({
  repo_www <- shiny:::staticHandler(file.path(getwd(), "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
