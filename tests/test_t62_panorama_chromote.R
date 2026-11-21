# =============================================================================
# tests/test_t62_panorama_chromote.R
# T62 in the browser, over the real Panorama with no S3
# (tests/helpers/panorama_req_fixture_app.R), at 1293x632:
#   - the feed's "Requisiciones" chip counts the two requisition avisos and,
#     pressed, shows only them;
#   - the ⚙ opens "De quién recibo avisos" (T68; its own test is
#     tests/test_t68_panel_chromote.R).
# Run from the repo root:  Rscript tests/test_t62_panorama_chromote.R
# =============================================================================
cat("=== test_t62_panorama_chromote ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("tests/helpers/live_app_harness.R")
PORT <- httpuv::randomPort()   # never a fixed port
raiz <- normalizePath(".", winslash = "/")
app <- NULL; b <- NULL
limpiar <- function() { try(if (!is.null(b)) b$close(), silent = TRUE); try(if (!is.null(app)) app$proc$kill(), silent = TRUE) }
ev <- function(js) { r <- b$Runtime$evaluate(js, returnByValue = TRUE); r$result$value }
espera <- function(js, s = 60) isTRUE(tryCatch({ poll_until(function() isTRUE(tryCatch(ev(js), error = function(e) FALSE)),
  timeout_s = s, interval_s = 0.5); TRUE }, error = function(e) FALSE))
clic <- function(sel) {   # a real mouse click at the element's centre
  p <- jsonlite::fromJSON(ev(sprintf("(function(){ var e = document.querySelector(%s); if (!e) return 'null';
    e.scrollIntoView({block: 'center'}); var r = e.getBoundingClientRect(); return JSON.stringify([r.left + r.width / 2, r.top + r.height / 2]); })()",
    jsonlite::toJSON(sel, auto_unbox = TRUE))))
  if (is.null(p)) return(FALSE)
  for (t in c("mousePressed", "mouseReleased")) b$Input$dispatchMouseEvent(type = t, x = p[1], y = p[2], button = "left", clickCount = 1)
  TRUE
}
foto <- function(n) if (nzchar(Sys.getenv("T62_FOTOS"))) {
  dir.create(Sys.getenv("T62_FOTOS"), showWarnings = FALSE, recursive = TRUE)
  writeBin(jsonlite::base64_dec(b$Page$captureScreenshot(format = "png")$data), file.path(Sys.getenv("T62_FOTOS"), paste0(n, ".png"))) }

tryCatch({
  app <- start_bg_app(sprintf('setwd("%s"); source("tests/helpers/panorama_req_fixture_app.R")$value', raiz), PORT, timeout_s = 180)
  b <- new_chromote_session()
  invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = FALSE))
  ok("the feed shows", espera("document.querySelectorAll('.pan-feed .pan-item').length === 4", 120))
  n <- ev("(function(){ var c = document.querySelector('.pan-chip[data-tipo=\"req\"]'); return c ? c.textContent.replace(/\\s+/g, ' ').trim() : ''; })()")
  ok(sprintf("chip 'Requisiciones' with its count of 2 (%s)", n), grepl("^Requisiciones ?2$", n))
  fuera <- ev("(function(){ var f = document.querySelector('.pan-feed').getBoundingClientRect();
    return [].filter.call(document.querySelectorAll('.pan-feed .pan-chip'), function(c){ var r = c.getBoundingClientRect();
      return r.right > f.right + 0.5 || r.left < f.left - 0.5; }).map(function(c){ return c.getAttribute('data-tipo'); }).join(','); })()")
  ok(sprintf("at 1293 every chip shows whole inside the feed (cut: %s)", if (nzchar(fuera)) fuera else "none"), !nzchar(fuera))
  clic('.pan-chip[data-tipo="req"]')
  espera("document.querySelectorAll('.pan-feed .pan-item[hidden]').length > 0", 10)
  vis <- ev("JSON.stringify([].filter.call(document.querySelectorAll('.pan-feed .pan-item'), function(i){ return !i.hidden; }).map(function(i){ return i.getAttribute('data-tipo'); }))")
  ok(sprintf("pressed, it shows only the two requisition items (%s)", vis), identical(jsonlite::fromJSON(vis), c("req", "req")))
  ok("…with the requisition icon", isTRUE(ev("!!document.querySelector('.pan-item[data-tipo=\"req\"] .pan-icono-req')")))
  foto("t62_feed_req")
  # T68: "Mis avisos" became "De quién recibo avisos" (its own test:
  # tests/test_t68_panel_chromote.R); here, only that it opens from the feed.
  ok("the ⚙ is in the feed's header", isTRUE(ev("!!document.querySelector('.pan-feed-cabeza .pan-feed-ajustes')")))
  clic(".pan-feed-cabeza .pan-feed-ajustes")
  ok("it opens 'De quién recibo avisos' with its grid",
     espera("!!document.querySelector('.modal #pan-avisos_de-hdt') && /De quién recibo avisos/.test(document.querySelector('.modal .modal-title').textContent)"))
  foto("t62_mis_avisos")
}, error = function(e) ok(paste("no error running the checks:", conditionMessage(e)), FALSE))
limpiar()

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
