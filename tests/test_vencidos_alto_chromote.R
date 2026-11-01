# =============================================================================
# tests/test_vencidos_alto_chromote.R
# T04b, in a real headless Chrome, against tests/helpers/vencidos_fixture_app.R
# with a real company's size (2,600 more invoices) and the app opening on
# another tab, as the real one does:
#   1. Vencidos painted while its tab is hidden fills the window once shown
#      (in production it stayed at the 8-row minimum, ~280 px, with every
#      column the same width: both were measured while hidden, all 0 px).
#   2. Documento takes its own width, not a share of the spare width.
#   3. "Vista completa" in the Columnas menu shows every optional column and,
#      turned off, goes back to exactly the user's choice; it is remembered.
#   4. The lupa fills its modal without the modal leaving the window.
#
# Run from project root:  Rscript tests/test_vencidos_alto_chromote.R
# =============================================================================
cat("=== test_vencidos_alto_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

# ── R: col_texto(ancho) ─────────────────────────────────────────────────────
local({
  for (f in c("R/i18n.R", "R/hd_columnas.R")) source(f, local = TRUE)
  m <- hd_columnas_meta(list(col_texto("Documento", ancho = 80), col_texto("Parte")))
  ok("R: col_texto(ancho = 80) sends ancho = 80 to the browser", identical(m[[1]]$ancho, 80))
  ok("R: a column without ancho sends NA (null)", is.na(m[[2]]$ancho))
  ok("R: ancho must be one positive number",
     inherits(tryCatch(col_texto("X", ancho = -1), error = identity), "error") &&
     inherits(tryCatch(col_texto("X", ancho = "80px"), error = identity), "error"))
})

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38971L
app <- tryCatch(start_bg_app(sprintf('Sys.setenv(VEN_OTRA = "1", VEN_EXTRA = "2600"); setwd("%s"); source("tests/helpers/vencidos_fixture_app.R")$value',
                                     repo), PORT, timeout_s = 90),
                error = function(e) { message(conditionMessage(e)); NULL })
b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
ok("fixture app started (opens on another tab, 2,600 more invoices)", !is.null(app))
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
base <- sprintf("http://127.0.0.1:%d", PORT)
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
A <- "ven-tabla-t-hdt"
tamano <- function(alto) invisible(b$Emulation$setDeviceMetricsOverride(width = 1400L, height = alto, deviceScaleFactor = 1, mobile = FALSE))
# A fresh page: Vencidos gets its data while hidden.
cargar <- function() {
  invisible(b$Page$navigate(base, wait_ = TRUE))
  poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s') && HDT.estado('%s').visibles.length > 2600", A, A))), timeout_s = 60) &&
    { Sys.sleep(0.8); TRUE }
}
mostrar <- function() { invisible(ev("document.querySelector('a[data-value=\"VEN\"]').click()")); Sys.sleep(1.2) }
medir <- function() ev(sprintf("(function(){ var r = document.getElementById('%s'), s = r.querySelector('.hdt-scroll');
  var w = [].map.call(r.querySelectorAll('thead th'), function (t) { return t.getBoundingClientRect().width; });
  var cab = [].map.call(r.querySelectorAll('thead th'), function (t) { return t.textContent.trim(); });
  return { ventana: innerHeight, scroll: s.getBoundingClientRect().height, abajo: r.getBoundingClientRect().bottom,
           anchos: w, cabeceras: cab }; })()", A))

# ── 1. Painted hidden, then shown ───────────────────────────────────────────
for (alto in c(900L, 1080L)) {
  tamano(alto)
  ok(sprintf("1400x%d: page loaded on the other tab, Vencidos has its rows", alto), cargar())
  oculta <- medir()
  ok(sprintf("1400x%d: while hidden the table measures nothing (0 px)", alto), oculta$scroll == 0)
  mostrar()
  m <- medir()
  w <- unlist(m$anchos)
  cat(sprintf("  1400x%d: rows area %.0f px, table ends at %.0f (window %d); columns %s\n",
              alto, m$scroll, m$abajo, alto, paste(round(w), collapse = " ")))
  ok(sprintf("1400x%d: shown, it fills down to the window's bottom less the margin (56 px)", alto),
     abs(m$abajo - (alto - 56)) <= 2)
  ok(sprintf("1400x%d: far above the 8-row minimum it stayed at (212 px)", alto), m$scroll > alto - 260)
  ok(sprintf("1400x%d: the columns have their own widths, not one equal share each", alto),
     max(w) - min(w) > 50)
}

# ── 2. Documento's width ────────────────────────────────────────────────────
tamano(900L)
invisible(b$Page$navigate(base, wait_ = TRUE))
invisible(ev("localStorage.setItem('hdt.columnas.vencidos', JSON.stringify({ Documento: true })); localStorage.removeItem('hdt.columnas_completa.vencidos'); true"))
ok("Documento turned on (Columnas, remembered)", cargar())
mostrar()
m <- medir()
cab <- unlist(m$cabeceras); w <- unlist(m$anchos)
doc_w <- w[grepl("^Documento", cab)]
parejo <- sum(w) / length(w)   # what each column got when they were all measured hidden
cat(sprintf("  Documento %.0f px; an equal share would be %.0f px\n", doc_w, parejo))
ok("Documento is at most 90 px", length(doc_w) == 1L && doc_w <= 90)
ok("... well under the equal share it had in production (<= 60%)", length(doc_w) == 1L && doc_w <= 0.6 * parejo)
td <- ev(sprintf("(function(){ var j = [].map.call(document.querySelectorAll('#%s thead th'), function (t) { return t.textContent.trim(); })
  .findIndex(function (x) { return x.indexOf('Documento') === 0; });
  var td = document.querySelector('#%s tbody tr:not(.hdt-esp):not(.hdt-banda) td:nth-child(' + (j + 1) + ')');
  return td ? td.className + '|' + td.style.maxWidth : ''; })()", A, A))
ok("its cells cut the rest of a long value (hdt-recorte, max 80px; the bubble shows it whole)",
   grepl("hdt-recorte", td) && grepl("\\|80px$", td))

# ── 3. Vista completa ───────────────────────────────────────────────────────
menu <- function() { invisible(ev(sprintf("document.querySelector('#%s .hdt-columnas').click()", A))); Sys.sleep(0.3) }
cajas <- function() ev("[].map.call(document.querySelectorAll('.hdt-menu .hdt-menu-op input'), function (c) {
  return (c.dataset.campo || 'COMPLETA') + ':' + (c.checked ? 1 : 0) + (c.disabled ? 'd' : ''); }).join(' ')")
antes <- unlist(medir()$cabeceras)
menu()
ok("the menu has 'Vista completa' first, apart from the list",
   identical(ev("document.querySelector('.hdt-menu .hdt-menu-op').textContent"), "Vista completa") &&
   isTRUE(ev("!!document.querySelector('.hdt-menu .hdt-menu-completa + .hdt-menu-sep')")))
invisible(invisible(ev("document.querySelector('.hdt-menu .hdt-menu-completa input').click()")); Sys.sleep(0.8))
completa <- unlist(medir()$cabeceras)
cat("  boxes with Vista completa on:", cajas(), "\n")
ok("on: every optional column shows (Origen, Venc. original, Moneda too)",
   all(c("Origen", "Venc. original", "Moneda") %in% sub("[▲▼]$", "", completa)) && length(completa) == length(antes) + 3L)
ok("on: each column's box shows checked and can't change",
   identical(cajas(), "COMPLETA:1 Documento:1d Origen:1d FechaOriginal:1d Moneda:1d"))
ok("the table still has all its rows", ev(sprintf("HDT.estado('%s').visibles.length", A)) > 2600)
invisible(cargar()); mostrar()
ok("remembered: after a reload it is still the full view", identical(unlist(medir()$cabeceras), completa))
menu()
ok("remembered: the box is still on", grepl("^COMPLETA:1 ", cajas()))
invisible(invisible(ev("document.querySelector('.hdt-menu .hdt-menu-completa input').click()")); Sys.sleep(0.8))
ok("off: back to exactly the user's choice (Documento shown, the rest hidden)", identical(unlist(medir()$cabeceras), antes))
cat("  boxes with Vista completa off:", cajas(), "\n")
ok("off: the boxes can change again and show the user's choice",
   identical(cajas(), "COMPLETA:0 Documento:1 Origen:0 FechaOriginal:0 Moneda:0"))
invisible(ev("document.body.click()"))

# ── 4. The lupa fills its modal, inside the window ─────────────────────────
invisible(ev("Shiny.setInputValue('btn_search', 1, {priority: 'event'})"))
ok("the lupa opens", poll_until(function() isTRUE(ev("!!document.querySelector('.modal.show .hdt-tabla tbody tr')")), timeout_s = 30))
Sys.sleep(1)
invisible(ev("document.querySelector('.modal.show .hdt-mes-flecha').click()"))   # last month: hundreds of invoices
Sys.sleep(1.5)
l <- ev("(function(){ var s = document.querySelector('.modal.show .hdt-scroll'), c = document.querySelector('.modal.show .modal-content');
  return { ventana: innerHeight, scroll: s.getBoundingClientRect().height, abajo: c.getBoundingClientRect().bottom,
           filas: HDT.estado(s.closest('.hdt').id).visibles.length }; })()")
cat(sprintf("  lupa: %d rows, rows area %.0f px, modal ends at %.0f (window %d)\n", l$filas, l$scroll, l$abajo, l$ventana))
ok("lupa: a month with hundreds of rows", l$filas > 300)
ok("lupa: the modal ends inside the window (its Cerrar shows)", l$abajo <= l$ventana)
ok("lupa: and the table takes the rest of it (window's bottom less the modal margin, +-2 px)",
   abs(l$abajo - (l$ventana - 28)) <= 2)

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
