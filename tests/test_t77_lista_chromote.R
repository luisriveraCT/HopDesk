# =============================================================================
# tests/test_t77_lista_chromote.R
# T77 in the browser: Pasivos' Lista gains ONE column, "Costo por año"
# (pasivos_costeo()), and the table still fits Luis's window, 1293x632, with
# no horizontal scroll (and at 1772x866). Over the real Lista module on the
# in-memory S3 (tests/helpers/pasivos_lista_fixture_app.R): "Renta bodega"
# pays 100 MXN a month -> 1,200.00 a year.
# Screenshots (T77_FOTOS) for the report.
# Run from the repo root:  Rscript tests/test_t77_lista_chromote.R
# =============================================================================
cat("=== test_t77_lista_chromote ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("tests/helpers/live_app_harness.R")
raiz <- normalizePath(".", winslash = "/")
FOTOS <- Sys.getenv("T77_FOTOS", "")
app <- NULL; b <- NULL
limpiar <- function() { try(if (!is.null(b)) b$close(), silent = TRUE); try(if (!is.null(app)) app$proc$kill(), silent = TRUE) }
ev <- function(js) { r <- b$Runtime$evaluate(js, returnByValue = TRUE); r$result$value }
espera <- function(js, s = 30) isTRUE(tryCatch(poll_until(function() isTRUE(tryCatch(ev(js), error = function(e) FALSE)),
  timeout_s = s, interval_s = 0.3), error = function(e) FALSE))
foto <- function(n) if (nzchar(FOTOS)) {
  ok(sprintf("%s: no login form on screen", n), !isTRUE(ev("!!document.querySelector('input[type=password]')")))
  dir.create(FOTOS, showWarnings = FALSE, recursive = TRUE)
  writeBin(jsonlite::base64_dec(b$Page$captureScreenshot(format = "png")$data), file.path(FOTOS, paste0(n, ".png"))) }

for (tam in list(c(1293L, 632L), c(1772L, 866L))) {
  et <- paste0(tam[1], "x", tam[2])
  tryCatch({
    PORT <- httpuv::randomPort()
    app <- start_bg_app(sprintf('setwd("%s"); source("tests/helpers/pasivos_lista_fixture_app.R")$value', raiz), PORT, timeout_s = 120)
    b <- new_chromote_session()
    invisible(b$Emulation$setDeviceMetricsOverride(width = tam[1], height = tam[2], deviceScaleFactor = 1, mobile = FALSE))
    invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = FALSE))
    ok(sprintf("%s: the Lista is up", et), espera("document.querySelectorAll('#list-lista-hdt tbody tr').length >= 3", 90))
    cabs <- jsonlite::fromJSON(ev("JSON.stringify([].map.call(document.querySelectorAll('#list-lista-hdt thead th'), function(t){ return t.innerText.trim(); }))"))
    ok(sprintf("%s: one new column, 'Costo por año', at the end", et), "Costo por año" %in% cabs && sum(cabs == "Costo por año") == 1L)
    fila <- ev("(function(){ var t = document.querySelector('#list-lista-hdt'); var ths = [].map.call(t.querySelectorAll('thead th'), function(x){ return x.innerText.trim(); });
      var j = ths.indexOf('Costo por año'); var r = [].filter.call(t.querySelectorAll('tbody tr'), function(tr){ return /Renta bodega/.test(tr.innerText); })[0];
      return r && j >= 0 ? r.children[j].innerText.trim() : ''; })()")
    ok(sprintf("%s: 'Renta bodega' (100 a month) costs 1,200.00 a year (%s)", et, fila), grepl("1,200.00", fila, fixed = TRUE))
    celda <- function(nombre) ev(sprintf("(function(){ var t = document.querySelector('#list-lista-hdt'); var ths = [].map.call(t.querySelectorAll('thead th'), function(x){ return x.innerText.trim(); });
      var j = ths.indexOf('Costo por año'); var r = [].filter.call(t.querySelectorAll('tbody tr'), function(tr){ return tr.innerText.indexOf('%s') >= 0; })[0];
      return r && j >= 0 ? r.children[j].innerText.trim() : 'NO'; })()", nombre))
    ok(sprintf("%s: a paused liability has no cost per year (empty)", et), identical(celda("Mantenimiento"), ""))
    ok(sprintf("%s: one quoted in USD with no exchange rate: unknown, never 0.00 (%s)", et, celda("Licencias")),
       !grepl("^0\\.00", celda("Licencias")))
    ok(sprintf("%s: the table fits: no horizontal scroll", et),
       isTRUE(ev("(function(){ var s = document.querySelector('#list-lista-hdt .hdt-scroll'); return s.scrollWidth <= s.clientWidth + 1; })()")) &&
         isTRUE(ev("document.documentElement.scrollWidth <= window.innerWidth + 1")))
    foto(paste0("lista_", et))
  }, error = function(e) ok(paste(et, ": no error:", conditionMessage(e)), FALSE))
  limpiar()
}

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
