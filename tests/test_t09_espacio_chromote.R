# =============================================================================
# tests/test_t09_espacio_chromote.R
# T09, Luis: "se pierde mucho espacio en blanco ... se necesita espacio para
# ver más cosas a la vez". Measured in a real headless Chrome at two windows:
# 1772x866 and Luis's real one, 1293x632 CSS px (his screen at ~137%).
#   1. The day modal, Detalle, 101 invoices: >= 16 rows in sight, no
#      horizontal scroll, one vertical scroll (the table's). Also with the
#      companies' full names in the data (Luis's capture: 8 rows, two
#      scrolls and a horizontal one).
#   2. The lupa and Vencidos lose no rows (before: 23 and 28 at 1772x866).
#   3. Ajustes -> Importar Excel, BanBajío's export (451 accounts): the
#      review shows >= 12 rows with one vertical scroll and no horizontal
#      one; back on the catalog, what stepped aside is back.
#
# Run from project root:  Rscript tests/test_t09_espacio_chromote.R
# =============================================================================
cat("=== test_t09_espacio_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
VENTANAS <- list(c(1772L, 866L), c(1293L, 632L))
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
espera <- function(cond, timeout_s = 20)
  poll_until(function() isTRUE(tryCatch(cond(), error = function(e) FALSE)), timeout_s = timeout_s, interval_s = 0.2)
ventana <- function(v) invisible(b$Emulation$setDeviceMetricsOverride(width = v[1], height = v[2], deviceScaleFactor = 1, mobile = FALSE))
arrancar <- function(codigo, puerto) {
  try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app)
  app <<- tryCatch(start_bg_app(sprintf('setwd("%s"); %s', repo, codigo), puerto, timeout_s = 120),
                   error = function(e) { message(conditionMessage(e)); NULL })
  if (is.null(app)) return(FALSE)
  b <<- new_chromote_session()
  ventana(VENTANAS[[1]])
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", puerto), wait_ = TRUE))
  # (window.HDT isn't there yet where the engine comes with the first modal.)
  espera(function() isTRUE(ev("!!window.Shiny && !!Shiny.setInputValue")), timeout_s = 90) && { Sys.sleep(1); TRUE }
}
cerrar <- function() {
  invisible(ev("$('.modal').modal('hide'); 0"))
  espera(function() isTRUE(ev("!document.querySelector('.modal, .modal-backdrop')")), timeout_s = 10)
}
clic <- function(js_el) {
  p <- ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center'}); var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
  b$Input$dispatchMouseEvent(type = "mouseMoved", x = p[[1]], y = p[[2]], button = "none")
  b$Input$dispatchMouseEvent(type = "mousePressed", x = p[[1]], y = p[[2]], button = "left", buttons = 1L, clickCount = 1L)
  b$Input$dispatchMouseEvent(type = "mouseReleased", x = p[[1]], y = p[[2]], button = "left", clickCount = 1L)
}
# Rows wholly in sight, whether the table scrolls sideways, and every box
# that really scrolls vertically, from the table up ("tabla" = its own).
medida <- function(id) ev(sprintf("(function(){ var R = document.getElementById('%s'), s = R.querySelector('.hdt-scroll'), sr = s.getBoundingClientRect();
  var filas = [].filter.call(s.querySelectorAll('tbody tr:not(.hdt-esp)'), function (tr) {
    var r = tr.getBoundingClientRect(); return r.height > 0 && r.top >= sr.top - 1 && r.bottom <= sr.bottom + 1 && r.bottom <= innerHeight; }).length;
  var scrolls = [], e = s;
  while (e && e !== document.documentElement) {
    var cs = getComputedStyle(e);
    if ((cs.overflowY === 'auto' || cs.overflowY === 'scroll') && e.scrollHeight > e.clientHeight + 1)
      scrolls.push(e === s ? 'tabla' : (e.className || e.tagName).toString().slice(0, 30));
    e = e.parentElement;
  }
  if (document.documentElement.scrollHeight > innerHeight + 1 && !document.body.classList.contains('modal-open')) scrolls.push('pagina');
  return { filas: filas, horizontal: s.scrollWidth > s.clientWidth + 1, scrolls: scrolls }; })()", id))
contar <- function(m, que) cat(sprintf("  %s: %d rows, horizontal %s, vertical scrolls: %s\n", que, m$filas, m$horizontal,
                                       paste(unlist(m$scrolls), collapse = " > ")))
un_scroll <- function(m) identical(unlist(m$scrolls), "tabla")
# The rows settle once the modal's height is known: wait until two reads agree.
estable <- function(id) {
  m0 <- NULL
  for (k in 1:30) { Sys.sleep(0.3); m <- tryCatch(medida(id), error = function(e) NULL)
    if (!is.null(m) && !is.null(m0) && identical(m, m0) && m$filas > 0) return(m); m0 <- m }
  m0
}

# ── 1. The day modal, Detalle, 101 invoices ────────────────────────────────
D <- "ap-detalle-hdt"
for (nombres in c("", "1")) {
  que <- if (nzchar(nombres)) "with full company names" else "with initials"
  ok(sprintf("day fixture started (101 invoices, %s)", que),
     arrancar(sprintf('Sys.setenv(DIA_EXTRA = "92", DIA_NOMBRES = "%s"); source("tests/helpers/dia_fixture_app.R")$value', nombres),
              if (nzchar(nombres)) 38973L else 38972L))
  for (v in VENTANAS) {
    ventana(v)
    invisible(ev("Shiny.setInputValue('abrir_detalle', Math.random(), {priority: 'event'}); 0"))
    espera(function() isTRUE(ev(sprintf("!!document.querySelector('.modal.show') && !!HDT.estado('%s') && HDT.estado('%s').visibles.length > 0", D, D))))
    m <- estable(D)
    contar(m, sprintf("day Detalle %dx%d, %s", v[1], v[2], que))
    ok(sprintf("day Detalle at %dx%d (%s): >= 16 rows in sight", v[1], v[2], que), m$filas >= 16L)
    ok(sprintf("day Detalle at %dx%d (%s): no horizontal scroll, one vertical scroll", v[1], v[2], que),
       !isTRUE(m$horizontal) && un_scroll(m))
    if (nzchar(nombres) && identical(v, VENTANAS[[2]]))
      ok("full names in the data show as their initials (the name whole in the bubble)",
         local({ t <- ev(sprintf("document.querySelector('#%s tbody').innerText", D))
                 grepl("NTS", t, fixed = TRUE) && !grepl("Networks Trucking Services", t, fixed = TRUE) }))
    if (nzchar(nombres) && identical(v, VENTANAS[[2]])) {
      # The engine's bubble (after 350 ms on the cell) shows the name whole.
      p <- ev(sprintf("(function(){ var ths = [].slice.call(document.querySelectorAll('#%s thead th'));
        var j = ths.findIndex(function (th) { return th.firstChild && th.firstChild.textContent === 'Empresa'; });
        var td = [].filter.call(document.querySelectorAll('#%s tbody tr:not(.hdt-esp)'), function (tr) {
          return tr.children[j] && tr.children[j].textContent === 'NTS'; })[0].children[j];
        var r = td.getBoundingClientRect(); return [r.left + r.width / 2, r.top + r.height / 2]; })()", D, D))
      b$Input$dispatchMouseEvent(type = "mouseMoved", x = p[[1]] - 3, y = p[[2]], button = "none")
      b$Input$dispatchMouseEvent(type = "mouseMoved", x = p[[1]], y = p[[2]], button = "none")
      ok("the initials' bubble shows the company's full name",
         espera(function() identical(ev("(document.querySelector('.hdt-tip:not([hidden]) .hdt-tip-texto')||{}).textContent || ''"),
                                     "Networks Trucking Services"), timeout_s = 5))
      invisible(ev("document.body.dispatchEvent(new MouseEvent('mousemove')); 0"))
    }
    cerrar()
  }
}

# ── 2. The lupa and Vencidos lose no rows ───────────────────────────────────
V <- "ven-tabla-t-hdt"; L <- "lupa-tabla-t-hdt"
ok("Vencidos fixture started (400+ invoices)",
   arrancar('Sys.setenv(VEN_EXTRA = "400"); source("tests/helpers/vencidos_fixture_app.R")$value', 38974L))
invisible(espera(function() isTRUE(ev(sprintf("!!HDT.estado('%s') && HDT.estado('%s').visibles.length > 0", V, V))), timeout_s = 60))
m <- estable(V); contar(m, "Vencidos 1772x866")
ok("Vencidos at 1772x866: no fewer rows than before T09 (28)", m$filas >= 28L && !isTRUE(m$horizontal))
for (v in VENTANAS) {
  ventana(v)
  invisible(ev("Shiny.setInputValue('btn_search', Math.random(), {priority: 'event'}); 0"))
  espera(function() isTRUE(ev(sprintf("!!document.querySelector('.modal.show') && !!HDT.estado('%s') && HDT.estado('%s').visibles.length > 0", L, L))))
  # October (the fixture's extra invoices: a busy month), with the bar's ‹.
  if (!identical(ev(sprintf("HDT.estado('%s').periodo", L)), "2026-10")) {
    Sys.sleep(0.5); clic(sprintf("document.querySelector('#%s .hdt-mes-flecha')", L))
  }
  espera(function() identical(ev(sprintf("HDT.estado('%s').periodo", L)), "2026-10") &&
                    isTRUE(ev(sprintf("HDT.estado('%s').visibles.length > 50", L))))
  m <- estable(L); contar(m, sprintf("lupa %dx%d", v[1], v[2]))
  ok(sprintf("lupa at %dx%d: one vertical scroll, no horizontal one", v[1], v[2]), un_scroll(m) && !isTRUE(m$horizontal))
  if (identical(v, VENTANAS[[1]])) ok("lupa at 1772x866: no fewer rows than before T09 (23)", m$filas >= 23L)
  else ok("lupa at 1293x632: >= 16 rows in sight", m$filas >= 16L)
  cerrar()
}

# ── 3. Ajustes -> Importar Excel ────────────────────────────────────────────
A <- "prov_mod-revision-hdt"
# A fresh app per window: the module keeps its upload across a reopen and
# the same file uploaded again changes nothing (reported apart, T09).
for (k in seq_along(VENTANAS)) {
  v <- VENTANAS[[k]]
  ok(sprintf("Ajustes fixture started (%dx%d)", v[1], v[2]),
     arrancar('source("tests/helpers/ajustes_fixture_app.R")$value', 38975L + k))
  ventana(v)
  clic("document.getElementById('abrir')")
  espera(function() isTRUE(ev("!!document.querySelector('.modal.show .settings-hub') && !!document.querySelector(\"a[data-value='importar']\")")))
  Sys.sleep(0.5)
  clic("document.querySelector(\"a[data-value='importar']\")")
  espera(function() isTRUE(ev("!!document.getElementById('prov_mod-excel_file')")))
  Sys.sleep(0.5)
  nodo <- b$DOM$querySelector(b$DOM$getDocument()$root$nodeId, "#prov_mod-excel_file")$nodeId
  invisible(b$DOM$setFileInputFiles(files = list(normalizePath("tests/fixtures/proveedores_excel/10_bajio_export.xlsx", winslash = "\\")),
                                    nodeId = nodo))
  espera(function() isTRUE(ev(sprintf("!!HDT.estado('%s') && HDT.estado('%s').visibles.length > 100", A, A))), timeout_s = 40)
  m <- estable(A); contar(m, sprintf("Excel review %dx%d", v[1], v[2]))
  ok(sprintf("Excel review at %dx%d: >= 12 rows in sight", v[1], v[2]), m$filas >= 12L)
  ok(sprintf("Excel review at %dx%d: one vertical scroll (not the panel's nor the window's), no horizontal one", v[1], v[2]),
     un_scroll(m) && !isTRUE(m$horizontal))
  ok(sprintf("Excel review at %dx%d: Importar is in sight", v[1], v[2]),
     isTRUE(ev("(function(){ var e = document.getElementById('prov_mod-confirmar_upload'); if (!e) return false; var r = e.getBoundingClientRect(); return r.height > 0 && r.bottom <= innerHeight; })()")))
  clic("document.querySelector(\"a[data-value='catalogo']\")")
  ok(sprintf("back on the catalog (%dx%d), Business Partners shows again", v[1], v[2]),
     espera(function() isTRUE(ev("document.querySelector('.bp-sap-seccion').offsetHeight > 0")), timeout_s = 5))
  invisible(ev("$('.modal').modal('hide'); 0"))   # ✕ asks for stg_close_btn, not in this fixture
  espera(function() isTRUE(ev("!document.querySelector('.modal, .modal-backdrop')")), timeout_s = 10)
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
