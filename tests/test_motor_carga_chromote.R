# =============================================================================
# tests/test_motor_carga_chromote.R
# T29: no table stays "Cargando…" because the engine's script arrived late.
# A page with NO table outside a renderUI: tab A holds t1, tab B holds t2
# (rows) and t3 (datos() gives NULL, as without permission). Every server
# sends its data when the session starts; hdtable.js only arrives with t1,
# the first table drawn. Then tab B:
#   M1  t1 loads (it was there when the script arrived);
#   M2  t2, drawn later, loads its rows (it stayed "Cargando…" before T29);
#   M3  t3 asks for its data exactly once, and not again however much the
#       page changes afterwards (no loop on a table that gives nothing);
#   M4  t1 and t2, which got their data, didn't ask more than once either.
# MOTOR_RAIZ: the checkout whose engine (R/hd_*.R, www/) to use -- master's
# for the counter-test.
#
# Run from project root:  Rscript tests/test_motor_carga_chromote.R
# =============================================================================
cat("=== test_motor_carga_chromote ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
source("tests/helpers/live_app_harness.R")
raiz <- normalizePath(Sys.getenv("MOTOR_RAIZ", "."), winslash = "/")
PORT <- 39095L

codigo <- sprintf('
library(shiny)
raiz <- "%s"
assign("%%||%%", function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x, envir = globalenv())
local({ owd <- setwd(raiz); on.exit(setwd(owd))
  for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R")) sys.source(f, envir = globalenv()) })
assign(".i18n_dir", file.path(raiz, "www", "i18n"), envir = globalenv())
cols <- list(col_texto("nombre", "Nombre"))
ui <- bslib::page_navbar(id = "nav", selected = "A",
  bslib::nav_panel("A", value = "A", uiOutput("ua")),
  bslib::nav_panel("B", value = "B", uiOutput("ub")),
  footer = tags$pre(id = "cuenta", textOutput("cuenta", inline = TRUE)))
server <- function(input, output, session) {
  pedidos <- reactiveValues(t1 = 0L, t2 = 0L, t3 = 0L)
  for (k in c("t1", "t2", "t3")) local({ k <- k
    observeEvent(input[[paste0(k, "-hdt_pedir")]], pedidos[[k]] <- pedidos[[k]] + 1L) })
  output$cuenta <- renderText(sprintf("t1=%%d t2=%%d t3=%%d", pedidos$t1, pedidos$t2, pedidos$t3))
  outputOptions(output, "cuenta", suspendWhenHidden = FALSE)
  output$ua <- renderUI(hd_table_ui("t1"))
  output$ub <- renderUI(tagList(hd_table_ui("t2"), hd_table_ui("t3")))
  hd_table_server("t1", tabla = "proveedores", datos = reactive(data.frame(id = c("a", "b"), nombre = c("Uno", "Dos"))), columnas = cols)
  hd_table_server("t2", tabla = "proveedores", datos = reactive(data.frame(id = c("c", "d", "e"), nombre = c("Tres", "Cuatro", "Cinco"))), columnas = cols)
  hd_table_server("t3", tabla = "proveedores", datos = reactive(NULL), columnas = cols)
}
app <- shinyApp(ui, server)
local({ repo_www <- shiny:::staticHandler(file.path(raiz, "www")); propio <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %%||%% propio(req) })
app', raiz)

app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
app <- tryCatch(start_bg_app(codigo, PORT, timeout_s = 90), error = function(e) { message(conditionMessage(e)); NULL })
ok("la app mínima arranca", !is.null(app))
if (is.null(app)) { cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail)); quit(status = 1L) }

b <- new_chromote_session()
b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE)
ev <- function(js) b$Runtime$evaluate(js, returnByValue = TRUE)$result$value
espera <- function(f, s = 15) poll_until(function() isTRUE(tryCatch(f(), error = function(e) FALSE)), timeout_s = s)
vis <- function(t) unlist(ev(sprintf("(window.HDT && HDT.estado('%s-hdt') || {}).visibles", t)))
cuenta <- function() {
  x <- ev("(document.getElementById('cuenta') || {}).textContent || ''")
  m <- regmatches(x, gregexpr("t[0-9]=[0-9]+", x))[[1]]
  as.list(stats::setNames(as.integer(sub(".*=", "", m)), sub("=.*", "", m)))
}
inicial <- rawToChar(curl::curl_fetch_memory(sprintf("http://127.0.0.1:%d/", PORT))$content)
ok("la página no trae el motor al cargar (ninguna tabla fuera de un renderUI)",
   !grepl("hdtable.js", inicial, fixed = TRUE))
b$Page$navigate(sprintf("http://127.0.0.1:%d/", PORT)); b$Page$loadEventFired(wait_ = TRUE)

ok("M1 t1 (la primera tabla, en la pestaña abierta) carga sus filas", espera(function() setequal(vis("t1"), c("a", "b"))))
ev("document.querySelector('a[data-value=\"B\"]').click()")
ok("M2 t2, que aparece después en otra pestaña, carga sus filas (antes: 'Cargando…')",
   espera(function() setequal(vis("t2"), c("c", "d", "e")), 10))
Sys.sleep(1.5)
# Make the page change a lot: containers come and go (not tables).
for (i in 1:20) ev("(function(){ var d = document.createElement('div'); d.className = 'ruido'; document.body.appendChild(d); setTimeout(function(){ d.remove(); }, 10); })()")
ev("document.querySelector('a[data-value=\"A\"]').click()"); Sys.sleep(0.5)
ev("document.querySelector('a[data-value=\"B\"]').click()"); Sys.sleep(2)
k <- cuenta()
cat("  hdt_pedir recibidos:", paste(names(k), unlist(k), sep = "=", collapse = " "), "\n")
ok("M3 t3 (datos() NULL) pidió sus datos exactamente una vez, sin bucle", identical(k$t3, 1L))
ok("M4 t1 y t2 no pidieron más de una vez", isTRUE(k$t1 <= 1L) && isTRUE(k$t2 <= 1L))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
