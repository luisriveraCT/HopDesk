# =============================================================================
# tests/test_proveedores_hd_chromote.R
# Proveedores' active catalog and Inactivos on the hd_table engine (T07), in a
# real headless Chrome against tests/helpers/proveedores_fixture_app.R (no
# S3; every save lands in #registro). Every click is a real input event.
# Blocks follow the T07 inventory (hopdesk_coord/tablas/reportes/
# T07_inventario.md): P1-P20 (catalog) and I1-I9 (inactive). The fixture:
# ACEROS, FLETES, PAPELERIA (incomplete) and SOFTWARE (USD, expired) active;
# PROVEEDOR VIEJO inactive.
#
# Run from project root:  Rscript tests/test_proveedores_hd_chromote.R
# =============================================================================
cat("=== test_proveedores_hd_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38984L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
app <- tryCatch(start_bg_app(sprintf('setwd("%s"); source("tests/helpers/proveedores_fixture_app.R")$value', repo),
                             PORT, timeout_s = 90),
                error = function(e) { message(conditionMessage(e)); NULL })
ok("fixture app started", !is.null(app))
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
invisible(b$Emulation$setDeviceMetricsOverride(width = 1400L, height = 1000L, deviceScaleFactor = 1, mobile = FALSE))
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
CAT <- "prov_mod-catalogo-hdt"; INA <- "prov_mod-inactivos-hdt"
estado <- function(t) ev(sprintf("HDT.estado('%s')", t))
vis <- function(t = CAT) unlist(estado(t)$visibles)
sel <- function(t = CAT) unlist(estado(t)$seleccion)
centro <- function(js_el, ver = TRUE) ev(sprintf("(function(){ var e = %s; %s
  var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el,
  if (ver) "e.scrollIntoView({block:'center', behavior:'instant'});" else ""))
raton <- function(tipo, p, botones = 0L, n = 1L, shift = FALSE)
  b$Input$dispatchMouseEvent(type = tipo, x = p[[1]], y = p[[2]], button = if (tipo == "mouseMoved") "none" else "left",
                             buttons = botones, clickCount = n, modifiers = if (shift) 8L else 0L)
# A modal still fading in (or out) moves under the pointer: click only once
# the element sits still (two equal reads), as a person would.
quieto <- function(js_el, ver = TRUE) {
  p <- centro(js_el, ver)
  for (k in 1:20) {
    Sys.sleep(0.1)
    q <- centro(js_el, FALSE)
    if (identical(p, q)) return(q)
    p <- q
  }
  p
}
clic <- function(js_el, shift = FALSE, ver = TRUE) {
  p <- quieto(js_el, ver)
  raton("mouseMoved", p); raton("mousePressed", p, 1L, shift = shift); raton("mouseReleased", p, shift = shift)
  Sys.sleep(0.15)
}
doble <- function(js_el) {
  p <- quieto(js_el)
  raton("mouseMoved", p)
  raton("mousePressed", p, 1L, 1L); raton("mouseReleased", p, 0L, 1L)
  raton("mousePressed", p, 1L, 2L); raton("mouseReleased", p, 0L, 2L)
  Sys.sleep(0.15)
}
escribir <- function(js_input, texto) {
  p <- centro(js_input); raton("mousePressed", p, 1L); raton("mouseReleased", p)
  ev(sprintf("var i=%s; i.value=''; i.dispatchEvent(new Event('input')); 0", js_input))
  if (nzchar(texto)) b$Input$insertText(text = texto)
  Sys.sleep(0.15)
}
fila <- function(id, sel = "", t = CAT) sprintf("HDT.fila('%s', '%s')%s", t, id, sel)
boton <- function(id, accion, t = CAT) fila(id, sprintf(".querySelector('td.hdt-acc button[data-accion=%s]')", accion), t)
celda <- function(id, titulo, t = CAT) ev(sprintf("(function(){
  var ths = Array.from(document.querySelectorAll('#%s thead th'));
  var j = ths.findIndex(function(th){ return th.firstChild && th.firstChild.textContent === '%s'; });
  return %s.children[j].textContent; })()", t, titulo, fila(id, "", t)))
insignia <- function(id, t = CAT) unlist(ev(sprintf("Array.from(%s.querySelectorAll('.hdt-ins')).map(function(s){ return s.className.match(/hdt-tono-[a-z]+/)[0] + ' ' + s.textContent; })", fila(id, "", t))))
registro <- function() strsplit(ev("document.getElementById('registro').textContent") %||% "", "\n")[[1]]
nuevas <- function(n0) { r <- registro(); if (length(r) > n0) r[(n0 + 1):length(r)] else character(0) }
modal <- function() ev("(document.querySelector('.modal.show') || {}).innerText || ''")
# Bootstrap fades a modal and its backdrop out: until both are gone they sit
# over the page and take the next click. Wait for that, as a person would.
sin_modal <- function() poll_until(function() isTRUE(ev("!document.querySelector('.modal, .modal-backdrop')")), timeout_s = 10)
CANCELAR <- "Array.from(document.querySelectorAll('.modal.show .modal-footer button')).filter(function(b){ return /Cancelar/.test(b.textContent); })[0]"
cerrar_modal <- function() {
  espera(function() isTRUE(ev(sprintf("!!(%s)", CANCELAR))))
  clic(CANCELAR)
  sin_modal()
}
conf_si <- function(t = CAT) clic(sprintf("document.querySelector('#%s .hdt-confirmar-si')", t))
pestana <- function(v) clic(sprintf("document.querySelector(\"a[data-value='%s']\")", v))
menu_op <- function(valor) sprintf("document.querySelector('.hdt-menu input[data-valor=\"%s\"]')", valor)
# Waits for what a check looks at instead of a fixed time: with the machine
# loaded, fixed pauses made a different block fail each run (P10, P18; T09).
espera <- function(cond, timeout_s = 15)
  poll_until(function() isTRUE(tryCatch(cond(), error = function(e) FALSE)), timeout_s = timeout_s, interval_s = 0.15)

invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE))
ok("the catalog loads on the engine with the 4 active providers",
   espera(function() length(vis()) == 4L, timeout_s = 60))

# ── P1/P6/P7 · columns and badges ───────────────────────────────────────────
titulos <- unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.dataset.col != null; }).map(function(th){ return th.firstChild.textContent; })", CAT)))
ok("P1/T09b: columns Nombre / Alias / Banco / No. Cuenta / Moneda / Empresas / Activo hasta / Vigencia / Estado (RFC hidden)",
   identical(titulos, c("Nombre", "Alias", "Banco", "No. Cuenta", "Moneda", "Empresas", "Activo hasta", "Vigencia", "Estado")))
# T09b: RFC and Vigencia are optional columns; RFC starts hidden (no sideways
# scroll at Luis's 1293x632), Vigencia in sight (it carries "Expirado").
clic(sprintf("document.querySelector('#%s .hdt-columnas')", CAT))
ok("T09b: Columnas offers RFC (off) and Vigencia (on)",
   espera(function() identical(ev("[].map.call(document.querySelectorAll('.hdt-menu .hdt-menu-op input[data-campo]'), function (c) { return c.dataset.campo + ':' + (c.checked ? 1 : 0); }).join(' ')"),
                               "rfc:0 vigencia:1")))
invisible(ev("document.body.click(); 0"))
ok("P6: Estado is Completo (green) or Incompleto (aviso)",
   identical(insignia("p-acme"), "hdt-tono-exito Completo") && identical(insignia("p-papl"), "hdt-tono-aviso Incompleto"))
ok("P7: only the expired date gets the red Expirado badge, next to its date; the rest say Indefinido",
   identical(insignia("p-soft"), c("hdt-tono-rojo Expirado", "hdt-tono-exito Completo")) &&
   identical(celda("p-soft", "Activo hasta"), "2020-01-01") && identical(celda("p-acme", "Activo hasta"), "Indefinido") &&
   identical(celda("p-acme", "Vigencia"), ""))

# ── P3/P19 · search and filters (the table's own now) ──────────────────────
BUSCAR <- sprintf("document.querySelector('#%s .hdt-buscar')", CAT)
escribir(BUSCAR, "nandu")
ok("P3: search without accents finds PAPELERIA ÑANDU", espera(function() identical(vis(), "p-papl")))
escribir(BUSCAR, "AMO0101")
ok("P3: by RFC", espera(function() identical(vis(), "p-acme")))
escribir(BUSCAR, "072")
ok("P3: wider than before -- the bank column too (072 = FLETES)", espera(function() identical(vis(), "p-flet")))
escribir(BUSCAR, "")
invisible(espera(function() length(vis()) == 4L))
ok("P19: filters Estado, Moneda and Empresas in the bar",
   identical(unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s .hdt-filtro')).map(function(b){ return b.textContent; })", CAT))),
             c("Estado", "Moneda", "Empresas")))
clic(sprintf("document.querySelectorAll('#%s .hdt-filtro')[1]", CAT))
invisible(espera(function() isTRUE(ev(sprintf("!!(%s)", menu_op("USD"))))))
clic(menu_op("USD"), ver = FALSE)
ok("P19: Moneda = USD leaves SOFTWARE CORP", espera(function() identical(vis(), "p-soft")))
clic(sprintf("document.querySelector('#%s .hdt-limpiar')", CAT))
invisible(ev("document.body.click(); 0"))
ok("P19: Limpiar filtros brings the 4 back", espera(function() length(vis()) == 4L))

# ── P9/P18 · Editar: button and double click ────────────────────────────────
clic(boton("p-flet", "editar"))
ok("P9: ✎ on FLETES opens the edit modal with its data",
   espera(function() grepl("Editar proveedor", modal()) && identical(ev("document.getElementById('prov_mod-prov_nombre').value"), "FLETES EL GÜERO")))
cerrar_modal()
# Nothing marked: the bar's place is kept (T09), so the first click of the
# double click doesn't move the table and the second lands on the same row.
# (Before T09 it landed on the header; T07's test marked a row first.)
top0 <- ev(sprintf("Math.round(document.querySelector('#%s thead').getBoundingClientRect().top)", CAT))
doble(fila("p-acme", ".children[0]"))
ok("P18: with nothing marked, a double click on ACEROS opens its edit modal",
   espera(function() grepl("Editar proveedor", modal()) && identical(ev("document.getElementById('prov_mod-prov_nombre').value"), "ACEROS MONTERREY SA")))
ok("P18/T09: the first click didn't move the table (the bar's place is reserved)",
   identical(ev(sprintf("Math.round(document.querySelector('#%s thead').getBoundingClientRect().top)", CAT)), top0))
cerrar_modal()
if ("p-acme" %in% sel()) clic(fila("p-acme", ".children[0]"))   # in case the double click left it marked
invisible(espera(function() !length(sel())))
# With another row already marked, the selection stays as it was.
clic(fila("p-papl", ".children[0]"))
invisible(espera(function() identical(sel(), "p-papl")))
doble(fila("p-acme", ".children[0]"))
ok("P18: a double click on ACEROS opens its edit modal, and leaves the selection as it was",
   espera(function() grepl("Editar proveedor", modal()) && identical(ev("document.getElementById('prov_mod-prov_nombre').value"), "ACEROS MONTERREY SA") &&
   identical(sel(), "p-papl")))
cerrar_modal()
clic(fila("p-papl", ".children[0]"))
invisible(espera(function() !length(sel())))
clic("document.getElementById('prov_mod-cat_new')")
ok("P12: Nuevo proveedor still opens an empty modal", espera(function() grepl("Nuevo proveedor", modal()) &&
   identical(ev("document.getElementById('prov_mod-prov_nombre').value"), "")))
cerrar_modal()

# ── P10 · Inactivar one row (modal, receiving list first) ───────────────────
n0 <- length(registro())
clic(boton("p-flet", "inactivar"))
ok("P10: 🚫 asks in today's modal", espera(function() grepl("¿Mover a inactivos?", modal())))
clic("document.getElementById('prov_mod-prov_confirm_inactivar')")
sin_modal()
invisible(espera(function() length(nuevas(n0)) >= 2L))
r <- nuevas(n0)
ok("P10/VP2: Inactivos is written first (with FLETES), then Activos (without it)",
   length(r) == 2L && startsWith(r[1], "inactivos 2: ") && grepl("FLETES", r[1]) &&
   startsWith(r[2], "activos 3: ") && !grepl("FLETES", r[2]))
ok("P15: the table follows by itself, no re-render", espera(function() !"p-flet" %in% vis() && length(vis()) == 3L))

# ── P8/P13 · selection and Inactivar selección (modal with the names) ───────
clic(fila("p-acme", ".children[0]"))
clic(fila("p-papl", ".children[0]"))
ok("P8: a click marks rows, by id", espera(function() setequal(sel(), c("p-acme", "p-papl"))))
clic(sprintf("document.querySelector('#%s .hdt-acciones button[data-accion=inactivar_sel]')", CAT))
ok("P13: the bar's Inactivar selección opens today's modal, listing the names (by id, not position)",
   espera(function() grepl("Mover 2 proveedor", modal()) && grepl("ACEROS MONTERREY SA", modal()) && grepl("PAPELERIA ÑANDU", modal()) &&
   !grepl("SOFTWARE", modal())))
n0 <- length(registro())
clic("document.getElementById('prov_mod-bulk_inactivar_confirm')")
sin_modal()
invisible(espera(function() length(nuevas(n0)) >= 2L))
r <- nuevas(n0)
ok("P13: both move, Inactivos first", length(r) == 2L && startsWith(r[1], "inactivos 4: ") && startsWith(r[2], "activos 1: "))
ok("P13: only SOFTWARE is left, and the selection of the gone rows is cleared",
   espera(function() identical(vis(), "p-soft") && !length(sel())))

# ── P11 · Eliminar (the table's confirmation instead of confirm()) ──────────
clic(boton("p-soft", "eliminar"))
ok("P11: ✕ asks in the table's bar first; nothing is saved yet",
   espera(function() identical(ev(sprintf("document.querySelector('#%s .hdt-confirmar-texto').textContent", CAT)), "¿Eliminar 1 fila?")))
n0 <- length(registro())
conf_si()
ok("P11: Confirmar deletes it", espera(function() isTRUE(startsWith(nuevas(n0)[1], "activos 0:"))))
ok("I9-like: an empty catalog says so", espera(function() length(vis()) == 0L &&
   grepl("Sin proveedores activos", ev(sprintf("document.getElementById('%s').innerText", CAT)))))

# ── I1-I9 · Inactivos ───────────────────────────────────────────────────────
pestana("inactivos")
ok("I1/I2: Inactivos shows the 4 inactive providers (VIEJO + the 3 moved)",
   espera(function() setequal(vis(INA), c("p-viej", "p-flet", "p-acme", "p-papl"))))
ok("I1: columns Nombre / Alias / Banco / No. Cuenta / RFC / Moneda",
   identical(unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.dataset.col != null; }).map(function(th){ return th.firstChild.textContent; })", INA))),
             c("Nombre", "Alias", "Banco", "No. Cuenta", "RFC", "Moneda")))
clic(fila("p-viej", ".children[0]", INA))
ok("I5: a click selects nothing (nothing used that selection)", !length(sel(INA)))
clic(boton("p-viej", "reactivar", INA))
ok("I6: 🔄 opens today's 'Activo por' modal", espera(function() grepl("Reactivar proveedor", modal()) && grepl("Activo por", modal())))
n0 <- length(registro())
clic("document.getElementById('prov_mod-prov_confirm_reactivar')")
sin_modal()
invisible(espera(function() length(nuevas(n0)) >= 2L))
r <- nuevas(n0)
ok("I6/VP2: Activos is written first (with VIEJO), then Inactivos (without it)",
   length(r) == 2L && startsWith(r[1], "activos 1: ") && grepl("PROVEEDOR VIEJO", r[1]) && startsWith(r[2], "inactivos 3: "))
clic(boton("p-papl", "eliminar", INA))
invisible(espera(function() isTRUE(ev(sprintf("!document.querySelector('#%s .hdt-confirmar').hidden", INA)))))
n0 <- length(registro())
conf_si(INA)
ok("I7: ✕ on an inactive one confirms in the bar, then deletes it",
   espera(function() isTRUE(startsWith(nuevas(n0)[1], "inactivos 2: ")) && !grepl("PAPELERIA", nuevas(n0)[1]) && !"p-papl" %in% vis(INA)))
pestana("catalogo")
ok("the reactivated one is back in the catalog without a reload", espera(function() identical(vis(), "p-viej")))
ok("P17: the catalog has the class's Excel export", isTRUE(ev(sprintf("!!document.querySelector('#%s .hdt-excel')", CAT))))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
