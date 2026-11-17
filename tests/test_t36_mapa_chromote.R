# =============================================================================
# tests/test_t36_mapa_chromote.R
# T36 in the browser (T15), on Luis's window (1293x632): the Mapa IC's
# currency selector is the client's base plus the data's currencies, and the
# payment plan totals each currency apart.
# Fixture: tests/helpers/interco_fixture_app.R (IC_USD, IC_BASE).
# Run from project root:  Rscript tests/test_t36_mapa_chromote.R
# =============================================================================
suppressPackageStartupMessages({ library(chromote) })
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 39036L
CAPT <- "C:/Temp/ej3_t36_capturas"; dir.create(CAPT, showWarnings = FALSE)
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
arrancar <- function(usd = FALSE, base_cfg = "") {
  if (!is.null(app)) stop_bg_app(app)
  app <<- tryCatch(start_bg_app(sprintf('Sys.setenv(IC_USD = "%s", IC_BASE = "%s", IC_RAIZ = "%s"); setwd("%s"); source("tests/helpers/interco_fixture_app.R")$value',
                                        if (usd) "1" else "0", base_cfg, repo, repo), PORT, timeout_s = 120),
                   error = function(e) { message(conditionMessage(e)); NULL })
  !is.null(app)
}
b <- new_chromote_session()
invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
base_url <- sprintf("http://127.0.0.1:%d", PORT)
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
abrir_mapa <- function() {
  invisible(b$Page$navigate(base_url, wait_ = TRUE))
  poll_until(function() isTRUE(ev("!!window.Shiny && !!document.getElementById('ic-view_map_btn')")), timeout_s = 60)
  ev("document.getElementById('ic-view_map_btn').click(); 0")
  poll_until(function() length(opciones()) > 0L, timeout_s = 30)
}
# The selectize's options in the order it shows them, and its value.
opciones <- function() unlist(ev("(function(){ var e = document.getElementById('ic-icmap-map_cur'); if (!e || !e.selectize) return [];
  return Object.values(e.selectize.options).sort(function(a, b){ return a.$order - b.$order; }).map(function(o){ return o.value; }); })()"))
valor <- function() ev("(function(){ var e = document.getElementById('ic-icmap-map_cur'); return e && e.selectize ? e.selectize.getValue() : null; })()")
elegir <- function(m) { ev(sprintf("document.getElementById('ic-icmap-map_cur').selectize.setValue('%s'); 0", m)); Sys.sleep(1.5) }
texto <- function(id) ev(sprintf("(document.getElementById('%s')||{}).textContent || ''", id))
errores_app <- function() { l <- c(readLines(app$proc$get_output_file(), warn = FALSE), readLines(app$proc$get_error_file(), warn = FALSE))
                            l[grepl("Warning: Error in|Execution halted", l)] }
# Opens the first edge and adds every invoice it lists to the plan.
agregar_arista <- function() {
  ev("Shiny.setInputValue('ic-icmap-net_click', {nodes: [], edges: [1], nonce: Math.random()}, {priority: 'event'}); 0")
  M <- "ic-icmap-arista_t-hdt"
  if (!poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s')", M))), timeout_s = 20)) return(FALSE)
  Sys.sleep(1)
  ids <- unlist(ev(sprintf("HDT.estado('%s').visibles", M)))
  ev(sprintf("Shiny.setInputValue('ic-icmap-arista_t-sel', %s, {priority: 'event'}); 0", jsonlite::toJSON(ids)))
  Sys.sleep(0.5)
  ev("document.getElementById('ic-icmap-add_to_plan').click(); 0"); Sys.sleep(1.5)
  TRUE
}

# ── 1. MXN client, MXN invoices only ─────────────────────────────────────────
ok("1: fixture (MXN client, MXN invoices) started", arrancar())
ok("1: the Mapa opens", abrir_mapa())
ok("1: the selector offers only MXN (before: MXN and USD, with USD always empty)", identical(opciones(), "MXN"))
ok("1: and shows MXN", identical(valor(), "MXN"))
ok("1: the routes are in MXN", poll_until(function() grepl("MXN", texto("ic-icmap-bfs_routes_ui")), timeout_s = 15))

# ── 2. MXN client with a USD invoice: as before ──────────────────────────────
ok("2: fixture with a USD invoice (A-9) started", arrancar(usd = TRUE))
ok("2: the Mapa opens", abrir_mapa())
ok("2: the selector offers MXN, USD (as before)", identical(opciones(), c("MXN", "USD")) && identical(valor(), "MXN"))
ok("2: MXN edge to the plan", agregar_arista())
elegir("USD")
ok("2: USD selected, the route is in USD", identical(valor(), "USD") && poll_until(function() grepl("USD", texto("ic-icmap-bfs_routes_ui")), timeout_s = 15))
ok("2: USD edge to the plan", agregar_arista())
plan <- texto("ic-icmap-payment_plan_ui")
cat("  plan:", gsub("\\s+", " ", plan), "\n")
b$screenshot(file.path(CAPT, "t36_plan_mixto.png"), show = FALSE)
ok("2: the plan totals each currency apart: MXN 3,200 and USD 300",
   grepl("Total\\s*MXN\u00a03,200", plan) && grepl("Total\\s*USD\u00a0300", plan))
ok("2: never one total of 3,500", !grepl("3,500", plan))

# ── 3. A USD client ──────────────────────────────────────────────────────────
ok("3: fixture with base USD started", arrancar(usd = TRUE, base_cfg = "USD"))
ok("3: the Mapa opens", abrir_mapa())
ok("3: USD first and selected", identical(opciones(), c("USD", "MXN")) && identical(valor(), "USD"))
b$screenshot(file.path(CAPT, "t36_mapa_base_usd.png"), show = FALSE)
ok("3: the routes are in USD", poll_until(function() grepl("USD", texto("ic-icmap-bfs_routes_ui")), timeout_s = 15))
ok("no errors in the app's log", !length(errores_app()))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
