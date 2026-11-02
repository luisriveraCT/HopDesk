# =============================================================================
# tests/test_proveedores_sin_permiso_chromote.R
# A user WITHOUT can_manage_proveedores sees the catalog and Inactivos but no
# button that writes: no ✎ 🚫 ✕ in the catalog's rows, no 🔄 ✕ in Inactivos,
# no "Inactivar selección" bar and no "＋ Nuevo proveedor". Reading stays:
# search, filters, selection, the Excel export. And the server lock is still
# there behind the hidden buttons: a double click (= Editar) is refused and
# nothing is saved. Real headless Chrome against
# tests/helpers/proveedores_fixture_app.R with PROV_SIN_PERMISO = 1.
# (With the permission, every button is there: test_proveedores_hd_chromote.R.)
#
# Run from project root:  Rscript tests/test_proveedores_sin_permiso_chromote.R
# =============================================================================
cat("=== test_proveedores_sin_permiso_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38987L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
app <- tryCatch(start_bg_app(sprintf('Sys.setenv(PROV_SIN_PERMISO = "1"); setwd("%s"); source("tests/helpers/proveedores_fixture_app.R")$value',
                                     repo), PORT, timeout_s = 90),
                error = function(e) { message(conditionMessage(e)); NULL })
ok("fixture app started (user without can_manage_proveedores)", !is.null(app))
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
invisible(b$Emulation$setDeviceMetricsOverride(width = 1400L, height = 1000L, deviceScaleFactor = 1, mobile = FALSE))
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
CAT <- "prov_mod-catalogo-hdt"; INA <- "prov_mod-inactivos-hdt"
vis <- function(t = CAT) unlist(ev(sprintf("HDT.estado('%s')", t))$visibles)
sel <- function(t = CAT) unlist(ev(sprintf("HDT.estado('%s')", t))$seleccion)
centro <- function(js_el) ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', behavior:'instant'});
  var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
raton <- function(tipo, p, botones = 0L, n = 1L)
  b$Input$dispatchMouseEvent(type = tipo, x = p[[1]], y = p[[2]], button = if (tipo == "mouseMoved") "none" else "left",
                             buttons = botones, clickCount = n)
clic <- function(js_el) { p <- centro(js_el); raton("mouseMoved", p); raton("mousePressed", p, 1L); raton("mouseReleased", p); Sys.sleep(0.6) }
doble <- function(js_el) {
  p <- centro(js_el); raton("mouseMoved", p)
  raton("mousePressed", p, 1L, 1L); raton("mouseReleased", p, 0L, 1L)
  raton("mousePressed", p, 1L, 2L); raton("mouseReleased", p, 0L, 2L)
  Sys.sleep(0.8)
}
escribir <- function(js_input, texto) {
  p <- centro(js_input); raton("mousePressed", p, 1L); raton("mouseReleased", p)
  ev(sprintf("var i=%s; i.value=''; i.dispatchEvent(new Event('input')); 0", js_input))
  if (nzchar(texto)) b$Input$insertText(text = texto)
  Sys.sleep(0.5)
}
fila <- function(id, s = "", t = CAT) sprintf("HDT.fila('%s', '%s')%s", t, id, s)
botones <- function(t) ev(sprintf("document.querySelectorAll('#%s tbody td.hdt-acc button').length", t))
visible <- function(js_el) isTRUE(ev(sprintf("(function(){ var e = %s; return !!e && e.offsetParent !== null && getComputedStyle(e).display !== 'none'; })()", js_el)))
registro <- function() strsplit(ev("document.getElementById('registro').textContent") %||% "", "\n")[[1]]
modal <- function() ev("(document.querySelector('.modal.show') || {}).innerText || ''")

invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE))
ok("the catalog loads, with its 4 providers (reading is allowed)",
   poll_until(function() isTRUE(tryCatch(length(vis()) == 4L, error = function(e) FALSE)), timeout_s = 60))
Sys.sleep(0.5)

ok("no row of the catalog shows ✎ 🚫 ✕", botones(CAT) == 0L)
ok("＋ Nuevo proveedor is hidden", !visible("document.getElementById('prov_mod-cat_new')"))
ok("the Importar Excel tab (and its template) is hidden; Catálogo activo and Inactivos stay",
   !visible("document.querySelector(\"a[data-value='importar']\")") &&
   !visible("document.getElementById('prov_mod-descargar_plantilla')") &&
   visible("document.querySelector(\"a[data-value='catalogo']\")") &&
   visible("document.querySelector(\"a[data-value='inactivos']\")"))
clic(fila("p-acme", ".children[0]"))
ok("selecting still works, but no 'Inactivar selección' bar shows",
   identical(sel(), "p-acme") &&
   !visible(sprintf("document.querySelector('#%s .hdt-acciones')", CAT)) &&
   !visible(sprintf("document.querySelector('#%s button[data-accion=inactivar_sel]')", CAT)))
clic(fila("p-acme", ".children[0]"))
escribir(sprintf("document.querySelector('#%s .hdt-buscar')", CAT), "nandu")
ok("search still works", identical(vis(), "p-papl"))
escribir(sprintf("document.querySelector('#%s .hdt-buscar')", CAT), "")
ok("the Excel export (reading) stays", visible(sprintf("document.querySelector('#%s .hdt-excel')", CAT)))

# Behind the hidden buttons, the server's lock: the double click (= Editar)
# reaches .editar(), which refuses -- no edit modal, nothing saved.
n0 <- length(registro())
doble(fila("p-flet", ".children[2]"))
ok("the server still refuses: a double click doesn't open the editor, and is logged as refused",
   !grepl("Editar proveedor", modal()) && any(grepl("^sin_permiso can_manage_proveedores", registro())) &&
   !any(grepl("^(activos|inactivos) ", registro())))

clic("document.querySelector(\"a[data-value='inactivos']\")")
ok("Inactivos shows its provider, with no 🔄 ✕",
   poll_until(function() isTRUE(tryCatch(length(vis(INA)) == 1L, error = function(e) FALSE)), timeout_s = 20) &&
   botones(INA) == 0L)

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
