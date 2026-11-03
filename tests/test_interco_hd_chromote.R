# =============================================================================
# tests/test_interco_hd_chromote.R
# Intercompany on the hd_table engine (T13), in a real headless Chrome,
# against tests/helpers/interco_fixture_app.R: the real module and the real
# agenda persistence over an in-memory S3 (no aws.s3). Every click is a real
# input event. Blocks follow the T13 inventory
# (hopdesk_coord/tablas/reportes/T13_inventario.md): S balances (§1).
#
# Run from project root:  Rscript tests/test_interco_hd_chromote.R
# IC_RAIZ=<older checkout> runs the same test against that checkout's code.
# =============================================================================
cat("=== test_interco_hd_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 39001L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
arrancar <- function(falla = "") {
  if (!is.null(app)) stop_bg_app(app)
  app <<- tryCatch(start_bg_app(sprintf('Sys.setenv(IC_FALLA = "%s", IC_RAIZ = "%s"); setwd("%s"); source("tests/helpers/interco_fixture_app.R")$value',
                                        falla, Sys.getenv("IC_RAIZ", repo), repo), PORT, timeout_s = 120),
                   error = function(e) { message(conditionMessage(e)); NULL })
  !is.null(app)
}
ok("fixture app started", arrancar())
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
base <- sprintf("http://127.0.0.1:%d", PORT)
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
centro <- function(js_el) ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', behavior:'instant'});
  var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
raton <- function(tipo, p, botones = 0L, n = 1L)
  b$Input$dispatchMouseEvent(type = tipo, x = p[[1]], y = p[[2]], button = if (tipo == "mouseMoved") "none" else "left",
                             buttons = botones, clickCount = n)
clic <- function(js_el) {
  p <- centro(js_el)
  raton("mouseMoved", p); raton("mousePressed", p, 1L); raton("mouseReleased", p)
  Sys.sleep(0.6)
}
doble <- function(js_el) {
  p <- centro(js_el)
  raton("mouseMoved", p)
  raton("mousePressed", p, 1L, 1L); raton("mouseReleased", p, 0L, 1L)
  raton("mousePressed", p, 1L, 2L); raton("mouseReleased", p, 0L, 2L)
  Sys.sleep(0.8)
}
escribir <- function(js_input, texto) {
  p <- centro(js_input); raton("mousePressed", p, 1L); raton("mouseReleased", p)
  ev(sprintf("var i=%s; i.value=''; i.dispatchEvent(new Event('input'))", js_input))
  if (nzchar(texto)) b$Input$insertText(text = texto)
  Sys.sleep(0.5)
}
listo <- function(t) poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s') && !!document.getElementById('%s')", t, t))), timeout_s = 30)
estado <- function(t) ev(sprintf("HDT.estado('%s')", t))
vis <- function(t) unlist(estado(t)$visibles)
fila <- function(t, id, sel = "") sprintf("HDT.fila('%s', '%s')%s", t, id, sel)
boton <- function(t, id, accion) fila(t, id, sprintf(".querySelector('td.hdt-acc button[data-accion=%s]')", accion))
titulos <- function(t) unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.dataset.col != null; }).map(function(th){ return th.firstChild.textContent; })", t)))
texto_fila <- function(t, id) ev(sprintf("%s.textContent", fila(t, id)))
registro <- function() strsplit(ev("document.getElementById('registro').textContent") %||% "", "\n")[[1]]
aviso <- function() ev("Array.from(document.querySelectorAll('.shiny-notification')).map(function(n){ return n.textContent; }).join(' / ')")
detalle_abierto <- function() ev("(document.getElementById('ic-ic_detail')||{}).textContent || ''")
abrir <- function() {
  invisible(b$Page$navigate(base, wait_ = TRUE))
  poll_until(function() isTRUE(ev("!!window.Shiny && !!document.getElementById('ic-ic_body')")), timeout_s = 60)
}

ok("the Intercompany tab loads", abrir())

# ── S · Saldos por empresa y contraparte (grouped summary B) ────────────────
S <- "ic-saldos_t-hdt"
PAR_NCS <- "NCS|AP|Networks Trucking Services SA de CV|MXN"
PAR_NTS <- "NTS|AR|NETWORKS CROSSDOCKING SERVICES|MXN"
ok("S: the balances are on the engine", listo(S))
ok("S: one row per pair, keyed by company|ledger|counterpart|currency, in the factory order",
   identical(vis(S), c(PAR_NCS, PAR_NTS)))
ok("S: columns Empresa / Tipo / Contraparte / Moneda / Total / Facturas",
   identical(titulos(S), c("Empresa", "Tipo", "Contraparte", "Moneda", "Total", "Facturas")))
ok("S4/S5: Tipo is a badge; Total is money; Facturas a number",
   grepl("CxP — Pago", texto_fila(S, PAR_NCS)) && grepl("CxC — Cobro", texto_fila(S, PAR_NTS)) &&
   grepl("$15,002.50", texto_fila(S, PAR_NCS), fixed = TRUE) && grepl("5", texto_fila(S, PAR_NCS)))
clic(fila(S, PAR_NCS, ".children[2]")); Sys.sleep(0.6)
ok("S1: a click on a row no longer opens the detail", !nzchar(detalle_abierto()))
clic(boton(S, PAR_NCS, "detalle")); Sys.sleep(1)
ok("S1/S2: ℹ opens that pair's detail below, by its id",
   grepl("Networks Crossdocking Services \\(NCS\\)", detalle_abierto()) &&
   grepl("Networks Trucking Services SA de CV", detalle_abierto()))
doble(fila(S, PAR_NTS, ".children[2]"))
ok("S1: a double click opens the other pair",
   grepl("Networks Trucking Services \\(NTS\\)", detalle_abierto()) && grepl("CxC", detalle_abierto()))
escribir(sprintf("document.querySelector('#%s .hdt-buscar')", S), "(nts)")
ok("S3: the engine's search finds a pair", identical(vis(S), PAR_NTS))
busqueda_antes <- estado(S)$busqueda
clic("document.getElementById('otra_sesion_confirma')"); Sys.sleep(1.5)
ok("S7: another session confirms P-1 -- the table repaints and keeps its search",
   identical(estado(S)$busqueda, busqueda_antes) && identical(vis(S), PAR_NTS))
escribir(sprintf("document.querySelector('#%s .hdt-buscar')", S), "")
ok("S7: and the NCS pair no longer counts P-1 (4 invoices, $14,002.00)",
   grepl("$14,002.004", texto_fila(S, PAR_NCS), fixed = TRUE))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
