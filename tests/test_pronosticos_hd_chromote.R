# =============================================================================
# tests/test_pronosticos_hd_chromote.R
# The three Pronósticos tables on the hd_table engine (T10), in a real
# headless Chrome, against tests/helpers/pronosticos_fixture_app.R: the real
# module over fixed data and a simulated `shared` (no S3). Every click is a
# real input event. Blocks follow the T10 inventory
# (hopdesk_coord/tablas/reportes/T10_inventario.md): M methods (§4).
#
# Run from project root:  Rscript tests/test_pronosticos_hd_chromote.R
# =============================================================================
cat("=== test_pronosticos_hd_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38993L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
arrancar <- function(falla = FALSE) {
  if (!is.null(app)) stop_bg_app(app)
  app <<- tryCatch(start_bg_app(sprintf('Sys.setenv(PRO_FALLA = "%s"); setwd("%s"); source("tests/helpers/pronosticos_fixture_app.R")$value',
                                        if (falla) "1" else "0", repo), PORT, timeout_s = 90),
                   error = function(e) { message(conditionMessage(e)); NULL })
  !is.null(app)
}
ok("fixture app started", arrancar())
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
invisible(b$Emulation$setDeviceMetricsOverride(width = 1772L, height = 866L, deviceScaleFactor = 1, mobile = FALSE))
base <- sprintf("http://127.0.0.1:%d", PORT)
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
M <- "fc-metodos_t-hdt"
centro <- function(js_el) ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', behavior:'instant'});
  var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
raton <- function(tipo, p, botones = 0L, n = 1L)
  b$Input$dispatchMouseEvent(type = tipo, x = p[[1]], y = p[[2]], button = if (tipo == "mouseMoved") "none" else "left",
                             buttons = botones, clickCount = n)
clic <- function(js_el) {
  p <- centro(js_el)
  raton("mouseMoved", p); raton("mousePressed", p, 1L); raton("mouseReleased", p)
  Sys.sleep(0.5)
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
pestana <- function(texto) clic(sprintf("Array.from(document.querySelectorAll('#fc-fc_tabs a')).filter(function(a){ return a.textContent.trim() === '%s'; })[0]", texto))
listo <- function(t) poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s') && !!document.getElementById('%s').offsetParent", t, t))), timeout_s = 30)
estado <- function(t) ev(sprintf("HDT.estado('%s')", t))
vis <- function(t) unlist(estado(t)$visibles)
fila <- function(t, id, sel = "") sprintf("HDT.fila('%s', '%s')%s", t, id, sel)
titulos <- function(t) unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.dataset.col != null; }).map(function(th){ return th.firstChild.textContent; })", t)))
texto_fila <- function(t, id) ev(sprintf("%s.textContent", fila(t, id)))
modal_titulo <- function() ev("(document.querySelector('.modal.show .modal-title')||{}).textContent || ''")
cerrar_modal <- function() { ev("$('.modal').modal('hide'); 0"); Sys.sleep(0.8) }
registro <- function() strsplit(ev("document.getElementById('registro').textContent") %||% "", "\n")[[1]]
aviso <- function() ev("Array.from(document.querySelectorAll('.shiny-notification')).map(function(n){ return n.textContent; }).join(' / ')")
abrir <- function() {
  invisible(b$Page$navigate(base, wait_ = TRUE))
  poll_until(function() isTRUE(ev("!!window.Shiny && !!document.getElementById('fc-fc_tabs')")), timeout_s = 60)
  Sys.sleep(1)
  TRUE
}

ok("the module loads", abrir())

# ── M · Métodos (catalog D) ─────────────────────────────────────────────────
pestana("Métodos y suscripciones")
ok("M: Métodos is on the engine", listo(M))
ok("M: the 3 methods, by method_id, in stored order",
   identical(vis(M), c("yesterday_spot", "spot", "manual_curve")))
ok("M: columns ID / Nombre / Tipo / Descripción (from the dictionary)",
   identical(titulos(M), c("ID", "Nombre", "Tipo", "Descripción")))
ok("M: a long description is cut, not wrapped (recorte)",
   isTRUE(ev(sprintf("(function(){ var td = %s; return td.scrollWidth > td.clientWidth; })()",
                     fila(M, "manual_curve", ".children[3]")))))
ok("M: no row buttons and no selection (read-only)",
   { clic(fila(M, "spot", ".children[1]"))
     length(unlist(estado(M)$seleccion)) == 0L && isTRUE(ev(sprintf("!%s", fila(M, "spot", ".querySelector('td.hdt-acc button')")))) })
ok("M: the engine's search finds by description",
   { escribir(sprintf("document.querySelector('#%s .hdt-buscar')", M), "publicado"); identical(vis(M), "yesterday_spot") })
escribir(sprintf("document.querySelector('#%s .hdt-buscar')", M), "")
ok("M: sorting by Nombre",
   { clic(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.firstChild && th.firstChild.textContent === 'Nombre'; })[0]", M))
     identical(vis(M), c("manual_curve", "spot", "yesterday_spot")) })

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
