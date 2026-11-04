# =============================================================================
# tests/test_pasivos_tabla_caracterizacion_chromote.R
# Pasivos · Tabla as it is today, in a real headless Chrome (T17b step 1):
# what only real clicks show. The fixture is tests/helpers/
# pasivos_tabla_fixture_app.R (the real module, fake shared, no S3); the grid
# is read with the same adapter as the R layer (tests/helpers/
# pasivos_tabla_adaptador.R), from the HTML the browser painted. Case
# numbers are those of reportes/T17b_paso0.md. Sizes: Luis's window
# (1293 x 632) and 1772 x 866; screenshots to PT_CAPTURAS if set.
#
# Run from project root:  Rscript tests/test_pasivos_tabla_caracterizacion_chromote.R
# =============================================================================
cat("=== test_pasivos_tabla_caracterizacion_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
source("tests/helpers/pasivos_tabla_adaptador.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38981L
CAPTURAS <- Sys.getenv("PT_CAPTURAS", "")
HOY <- Sys.Date()
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)

app <- tryCatch(start_bg_app(sprintf('setwd("%s"); source("tests/helpers/pasivos_tabla_fixture_app.R")$value', repo),
                             PORT, timeout_s = 90), error = function(e) { message(conditionMessage(e)); NULL })
ok("fixture app started", !is.null(app))
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
pantalla <- function(w, h) invisible(b$Emulation$setDeviceMetricsOverride(width = w, height = h, deviceScaleFactor = 1, mobile = FALSE))
base <- sprintf("http://127.0.0.1:%d", PORT)
ev <- function(js, await = FALSE) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE, awaitPromise = await)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
abrir <- function() {
  b$Page$navigate(base, wait_ = TRUE)
  ok <- poll_until(function() isTRUE(ev("!!document.querySelector('.pasivos-table tbody tr')")), timeout_s = 60)
  Sys.sleep(0.5)
  ok
}
rejilla <- function() pt_rejilla(ev("document.querySelector('.pasivos-table').outerHTML"), HOY)
eco <- function() jsonlite::fromJSON(ev("document.getElementById('eco').textContent") %||% "{}")
# The cell (row label, period key) as a JS expression.
td_js <- function(etiqueta, clave) sprintf("(function(){
  var ths = document.querySelectorAll('.pasivos-table thead th'), k = -1;
  for (var i = 1; i < ths.length; i++) if (ths[i].textContent === %s) k = i;
  var trs = document.querySelectorAll('.pasivos-table tbody tr.pasivos-data-row');
  for (var j = 0; j < trs.length; j++) {
    var s = trs[j].querySelector('td span.fw-semibold, td span.text-truncate');
    if (s && s.textContent === %s) return trs[j].children[k];
  }
  return null; })()", jsonlite::toJSON(as.character(HOY + as.integer(sub("^d", "", clave))), auto_unbox = TRUE),
  jsonlite::toJSON(etiqueta, auto_unbox = TRUE))
# A real click on an element (a JS expression), in view first.
clic <- function(js_el) {
  p <- ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', inline:'center', behavior:'instant'});
    var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
  for (t in c("mouseMoved", "mousePressed", "mouseReleased"))
    b$Input$dispatchMouseEvent(type = t, x = p[[1]], y = p[[2]], button = if (t == "mouseMoved") "none" else "left",
                               buttons = if (t == "mousePressed") 1L else 0L, clickCount = 1L)
  Sys.sleep(0.5)
}
espera_eco <- function(evento, valor, n0) poll_until(function() {
  e <- eco(); identical(e$evento, evento) && (is.null(valor) || identical(e$valor, valor)) && (e$n %||% 0L) > n0
}, timeout_s = 5)

ronda <- function(w, h) {
  et <- sprintf("[%dx%d]", w, h)
  pantalla(w, h)
  ok(paste(et, "the page loads with the table"), abrir())
  r <- rejilla()
  ok(paste(et, "B1: the browser paints the same rows as the R layer (C1)"),
     identical(pt_etiquetas(r), c("Papelería Centro", "Arrendamiento archivado", "Renta oficina", "Crédito Banorte",
                                  "Comisión por apertura", "Comisión por disposición", "Tarjeta Amex",
                                  "Licencias <b>Ñandú</b> & 'Co'")))
  ok(paste(et, "B2: a name with <b> is text: no <b> or <i> element inside the table (C25)"),
     isTRUE(ev("!document.querySelector('.pasivos-table b, .pasivos-table i')")))
  n <- eco()$n %||% 0L
  clic(paste0(td_js("Renta oficina", "d+5"), ".querySelector('button.pasivos-convert-btn')"))
  ok(paste(et, "B3: ⚡ sends pasivos_convert_request with the provision's id (C7)"), espera_eco("pasivos_convert_request", "p1d", n))
  ok(paste(et, "B3b: ...and only that: the cell's own click doesn't fire too"), identical(eco()$n, n + 1L))
  n <- eco()$n
  clic(paste0(td_js("Renta oficina", "d+1"), ".querySelector('div[onclick]')"))
  ok(paste(et, "B4: a click on a cell sends pasivos_cell_click with its provision (C11)"), espera_eco("pasivos_cell_click", "p1c", n))
  n <- eco()$n
  clic(paste0(td_js("Crédito Banorte", "d+3"), ".querySelector('button.pasivos-convert-btn')"))
  ok(paste(et, "B5: ↩ on a pending item sends pasivos_convert_request with its provision (C9)"),
     espera_eco("pasivos_convert_request", "p2p", n))
  n <- eco()$n
  clic("(function(){ var s = Array.prototype.find.call(document.querySelectorAll('.pasivos-table span.fw-semibold'), function (x) { return x.textContent === 'Renta oficina'; }); return s.parentNode.querySelector('button.pasivos-row-edit-btn'); })()")
  ok(paste(et, "B6: ✎ sends pasivos_edit_liability with the liability's id (C16)"), espera_eco("pasivos_edit_liability", "lib1", n))
  n <- eco()$n
  clic("(function(){ var s = Array.prototype.find.call(document.querySelectorAll('.pasivos-table span.fw-semibold'), function (x) { return x.textContent.indexOf('Licencias') === 0; }); return s.parentNode.querySelector('button.pasivos-row-edit-btn'); })()")
  Sys.sleep(0.6)
  ok(paste(et, "B7 [falla conocida, C26]: ✎ on a liability whose id has a ' sends nothing"), identical(eco()$n, n))
  n <- eco()$n
  clic("document.querySelector('.pasivos-table-module button.btn-primary')")
  ok(paste(et, "B8: + Agregar pasivo sends pasivos_add_liability (C22)"), espera_eco("pasivos_add_liability", NULL, n))
  ok(paste(et, "B9: Empresa offers only the visible companies, never OCU (C27)"), {
    ops <- ev("(function(){ var s = document.getElementById('pt-empresa_sel'); return s && s.selectize ? Object.keys(s.selectize.options).sort() : null; })()")
    identical(unlist(ops), c("BOD", "NRS", "NTS"))
  })
  ok(paste(et, "B10: today's column is marked in the header (C5)"),
     identical(ev("document.querySelector('.pasivos-table thead th.pasivos-table-today-col').textContent"), as.character(HOY)))
  ok(paste(et, "B11 [CAMBIA, P5]: the mirror scrollbar moves the table sideways"), {
    ev("(function(){ var b = document.getElementById('pt-pasivos_scroll_bar'); b.scrollLeft = 400; b.dispatchEvent(new Event('scroll')); })()")
    Sys.sleep(0.3)
    ev("document.getElementById('pt-pasivos_tbl_wrap').scrollLeft") > 300
  })
  clic("document.getElementById('pt-extend_right')")
  ok(paste(et, "B12: 60 días → shows the provision at today + 54 (C6)"),
     poll_until(function() identical(pt_buscar_fila(rejilla(), "Renta oficina")$celdas[["d+54"]]$texto, "$ 950.00"), timeout_s = 10))
  if (nzchar(CAPTURAS)) { dir.create(CAPTURAS, showWarnings = FALSE, recursive = TRUE)
    b$screenshot(file.path(CAPTURAS, sprintf("pasivos_tabla_%dx%d.png", w, h)), show = FALSE) }
}

ronda(1293, 632)
ronda(1772, 866)

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
