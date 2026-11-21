# =============================================================================
# tests/test_t78_deuda_chromote.R
# T78 in the browser: Pasivos · "Costo de la deuda" on the real Pasivos module
# (tests/helpers/pasivos_tabla_fixture_app.R with PASIVOS_DEUDA=1: a French
# loan in MXN at 12 % with an opening fee, a lease in USD at 8 %). At Luis's
# window, 1293x632, and at 1772x866:
#   - the sub-tab opens, with one card per currency (never added);
#   - the table: EAR 12.00 for the MXN loan and a cost with its fee above it;
#     sections by currency;
#   - the 12 months' service: Capital and Interés per currency;
#   - no horizontal scroll on the page nor on either table.
# Screenshots (T78_FOTOS) for the report.
# Run from the repo root:  Rscript tests/test_t78_deuda_chromote.R
# =============================================================================
cat("=== test_t78_deuda_chromote ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("tests/helpers/live_app_harness.R")
raiz <- normalizePath(".", winslash = "/")
FOTOS <- Sys.getenv("T78_FOTOS", "")
app <- NULL; b <- NULL
limpiar <- function() { try(if (!is.null(b)) b$close(), silent = TRUE); try(if (!is.null(app)) app$proc$kill(), silent = TRUE) }
ev <- function(js) { r <- b$Runtime$evaluate(js, returnByValue = TRUE); r$result$value }
espera <- function(js, s = 30) isTRUE(tryCatch(poll_until(function() isTRUE(tryCatch(ev(js), error = function(e) FALSE)),
  timeout_s = s, interval_s = 0.3), error = function(e) FALSE))
foto <- function(n) if (nzchar(FOTOS)) {
  ok(sprintf("%s: no login form on screen", n), !isTRUE(ev("!!document.querySelector('input[type=password]')")))
  dir.create(FOTOS, showWarnings = FALSE, recursive = TRUE)
  writeBin(jsonlite::base64_dec(b$Page$captureScreenshot(format = "png")$data), file.path(FOTOS, paste0(n, ".png"))) }
# The text of a column's cell in the first row of a table whose text has `fila`.
celda <- function(tabla, fila, columna) ev(sprintf("(function(){ var t = document.querySelector('%s'); if (!t) return 'NO';
  var ths = [].map.call(t.querySelectorAll('thead th'), function(x){ return x.innerText.trim(); });
  var j = ths.indexOf('%s'); var r = [].filter.call(t.querySelectorAll('tbody tr'), function(tr){ return tr.innerText.indexOf('%s') >= 0; })[0];
  return r && j >= 0 ? r.children[j].innerText.trim() : 'NO'; })()", tabla, columna, fila))
sin_scroll <- function(sel) isTRUE(ev(sprintf("(function(){ var s = document.querySelector('%s .hdt-scroll'); return !!s && s.scrollWidth <= s.clientWidth + 1; })()", sel)))

Sys.setenv(PASIVOS_DEUDA = "1")
for (tam in list(c(1293L, 632L), c(1772L, 866L))) {
  et <- paste0(tam[1], "x", tam[2])
  tryCatch({
    PORT <- httpuv::randomPort()
    app <- start_bg_app(sprintf('Sys.setenv(PASIVOS_DEUDA = "1"); setwd("%s"); source("tests/helpers/pasivos_tabla_fixture_app.R")$value', raiz),
                        PORT, timeout_s = 120)
    b <- new_chromote_session()
    invisible(b$Emulation$setDeviceMetricsOverride(width = tam[1], height = tam[2], deviceScaleFactor = 1, mobile = FALSE))
    invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = FALSE))
    ok(sprintf("%s: the sub-tab 'Costo de la deuda' is there", et),
       espera("(function(){ var a = document.querySelector('a[data-value=\"deuda\"]'); return !!a && a.innerText.trim() === 'Costo de la deuda'; })()", 90))
    ev("document.querySelector('a[data-value=\"deuda\"]').click()")
    ok(sprintf("%s: the table is up (the two loans)", et),
       espera("[].filter.call(document.querySelectorAll('#pt-deuda-tabla-hdt tbody tr'), function(tr){ return /BBVA|Volvo/.test(tr.innerText); }).length >= 2", 60))
    tarjetas <- jsonlite::fromJSON(ev("JSON.stringify([].map.call(document.querySelectorAll('.pasivos-deuda-tarjeta > div:first-child'), function(x){ return x.innerText.trim(); }))"))
    ok(sprintf("%s: one card per currency, never added (%s)", et, paste(tarjetas, collapse = ",")),
       all(c("MXN", "USD") %in% tarjetas) && !anyDuplicated(tarjetas))
    cabs <- jsonlite::fromJSON(ev("JSON.stringify([].map.call(document.querySelectorAll('#pt-deuda-tabla-hdt thead th'), function(t){ return t.innerText.trim(); }))"))
    ok(sprintf("%s: the columns say which rate is which", et), all(c("EAR %", "Costo efectivo %", "Nominal % (m pagos/año)") %in% cabs))
    ear <- celda("#pt-deuda-tabla-hdt", "BBVA", "EAR %"); efe <- celda("#pt-deuda-tabla-hdt", "BBVA", "Costo efectivo %")
    ok(sprintf("%s: the MXN loan: EAR 12.00 (%s), its fee puts the effective cost above it (%s)", et, ear, efe),
       identical(ear, "12.00") && isTRUE(suppressWarnings(as.numeric(efe)) > 12))
    ok(sprintf("%s: the USD lease: EAR 8.00", et), identical(celda("#pt-deuda-tabla-hdt", "Volvo", "EAR %"), "8.00"))
    ok(sprintf("%s: the service: Capital and Interés", et),
       espera("(function(){ var t = document.querySelector('#pt-deuda-servicio-hdt'); return !!t && /Capital/.test(t.innerText) && /Inter/.test(t.innerText); })()", 30))
    ok(sprintf("%s: the footer says which rate was captured", et),
       isTRUE(ev("/efectiva anual \\(EAR\\)/.test(document.querySelector('.pasivos-deuda').innerText)")))
    ok(sprintf("%s: nothing scrolls sideways (page, table, service)", et),
       sin_scroll("#pt-deuda-tabla-hdt") && sin_scroll("#pt-deuda-servicio-hdt") &&
         isTRUE(ev("document.documentElement.scrollWidth <= window.innerWidth + 1")))
    foto(paste0("deuda_", et))
  }, error = function(e) ok(paste(et, ": no error:", conditionMessage(e)), FALSE))
  limpiar()
}

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
