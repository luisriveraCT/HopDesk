# =============================================================================
# tests/test_t68_panel_chromote.R
# T68 in the browser: "De quién recibo avisos" over the real Panorama with
# 340 people in memory (tests/helpers/panorama_avisos_de_fixture_app.R).
# No S3, no login. Three readers:
#   A  the client's top tier at 1293x632 and 1772x866: the modal never
#      passes the window, the grid scrolls inside, no horizontal scroll, five
#      columns with their counts; search + the column box tick only those
#      found; Guardar writes avisos_de (base + exceptions, cambia an array);
#   B  a buyer (compras): only Requisiciones and Altas, never the treasury
#      columns; what it saves has no treasury channel;
#   C  HopDesk staff inside the client: no Requisiciones, no "Lo mio", a line
#      saying why; Guardar writes in THEIR home (hopdesk), under
#      avisos_de_clientes$dev, never in the client's preferences.
# Screenshots (T68_FOTOS) for the report.
# Run from the repo root:  Rscript tests/test_t68_panel_chromote.R
# =============================================================================
cat("=== test_t68_panel_chromote ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("tests/helpers/live_app_harness.R")
raiz <- normalizePath(".", winslash = "/")
FOTOS <- Sys.getenv("T68_FOTOS", "")
app <- NULL; b <- NULL
limpiar <- function() { try(if (!is.null(b)) b$close(), silent = TRUE); try(if (!is.null(app)) app$proc$kill(), silent = TRUE) }
ev <- function(js) { r <- b$Runtime$evaluate(js, returnByValue = TRUE); r$result$value }
espera <- function(js, s = 30) isTRUE(tryCatch(poll_until(function() isTRUE(tryCatch(ev(js), error = function(e) FALSE)),
  timeout_s = s, interval_s = 0.3), error = function(e) FALSE))
clic <- function(sel) {
  p <- jsonlite::fromJSON(ev(sprintf("(function(){ var e = document.querySelector(%s); if (!e) return 'null';
    e.scrollIntoView({block: 'center'}); var r = e.getBoundingClientRect(); return JSON.stringify([r.left + r.width / 2, r.top + r.height / 2]); })()",
    jsonlite::toJSON(sel, auto_unbox = TRUE))))
  if (is.null(p)) return(FALSE)
  for (t in c("mousePressed", "mouseReleased")) b$Input$dispatchMouseEvent(type = t, x = p[1], y = p[2], button = "left", clickCount = 1)
  Sys.sleep(0.4); TRUE
}
foto <- function(n) if (nzchar(FOTOS)) {
  ok(sprintf("%s: no login form on screen", n), !isTRUE(ev("!!document.querySelector('input[type=password]')")))
  dir.create(FOTOS, showWarnings = FALSE, recursive = TRUE)
  writeBin(jsonlite::base64_dec(b$Page$captureScreenshot(format = "png")$data), file.path(FOTOS, paste0(n, ".png"))) }
arrancar <- function(ancho, alto, nivel = "", staff = "") {
  PORT <- httpuv::randomPort()
  app <<- start_bg_app(sprintf('Sys.setenv(T68_NIVEL = "%s", T68_STAFF = "%s"); setwd("%s"); %s source("tests/helpers/panorama_avisos_de_fixture_app.R")$value',
                               nivel, staff, raiz, if (nzchar(nivel)) "" else 'Sys.unsetenv("T68_NIVEL");'), PORT, timeout_s = 120)
  b <<- new_chromote_session()
  invisible(b$Emulation$setDeviceMetricsOverride(width = ancho, height = alto, deviceScaleFactor = 1, mobile = FALSE))
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = FALSE))
  espera("!!document.querySelector('.pan-feed-ajustes')", 90)
}
abrir_panel <- function() {
  clic(".pan-feed-ajustes")
  espera("document.querySelectorAll('.modal #pan-avisos_de-hdt thead th.hdt-cas-th').length > 0 && document.querySelectorAll('.modal #pan-avisos_de-hdt tr.hdt-grupo').length > 0", 30)
}
titulos <- function() jsonlite::fromJSON(ev("JSON.stringify([].map.call(document.querySelectorAll('.modal #pan-avisos_de-hdt thead th.hdt-cas-th'), function(th){ return th.querySelector('span:not(.hdt-cas-n)').textContent; }))"))
registro <- function() { t <- ev("document.getElementById('registro').textContent"); if (nzchar(t)) jsonlite::fromJSON(t, simplifyVector = FALSE) else NULL }
cabe <- function() isTRUE(ev("(function(){ var d = document.querySelector('.modal .modal-dialog'); var r = d.getBoundingClientRect();
  var s = document.querySelector('.modal #pan-avisos_de-hdt .hdt-scroll');
  return r.bottom <= window.innerHeight + 1 && r.right <= window.innerWidth + 1 && s.scrollWidth <= s.clientWidth + 1; })()"))

# ── A: the top tier, two window sizes ───────────────────────────────────────
for (tam in list(c(1293L, 632L), c(1772L, 866L))) {
  et <- paste0(tam[1], "x", tam[2])
  tryCatch({
    ok(sprintf("A %s: Panorama is up with the ⚙", et), arrancar(tam[1], tam[2]))
    ok(sprintf("A %s: the panel opens with its grid", et), abrir_panel())
    ok(sprintf("A %s: five kinds of notice", et),
       identical(titulos(), c("Requisiciones", "Altas", "Archivo de pago", "Pago confirmado", "Bancos")))
    ok(sprintf("A %s: the modal never passes the window, no horizontal scroll", et), cabe())
    ok(sprintf("A %s: groups by tier, with their names", et),
       isTRUE(ev("[].some.call(document.querySelectorAll('.modal tr.hdt-grupo'), function(r){ return /Compras/.test(r.innerText); })")))
    foto(paste0("A1_panel_", et))
    ev("(function(){ var q = document.querySelector('.modal #pan-avisos_de-hdt input.hdt-buscar'); q.value = 'planta norte'; q.dispatchEvent(new Event('input', {bubbles: true})); return true; })()")
    Sys.sleep(0.6)
    clic(".modal #pan-avisos_de-hdt thead input.hdt-cas-todos")   # Requisiciones, the first one
    n_req <- ev("document.querySelector('.modal #pan-avisos_de-hdt thead th.hdt-cas-th .hdt-cas-n').textContent")
    ok(sprintf("A %s: with 'planta norte', the column box ticks those found (%s)", et, n_req), identical(n_req, "Todos"))
    ev("(function(){ var q = document.querySelector('.modal #pan-avisos_de-hdt input.hdt-buscar'); q.value = ''; q.dispatchEvent(new Event('input', {bubbles: true})); return true; })()")
    Sys.sleep(0.6)
    n_todo <- ev("document.querySelector('.modal #pan-avisos_de-hdt thead th.hdt-cas-th .hdt-cas-n').textContent")
    ok(sprintf("A %s: without the search, only those (%s)", et, n_todo), grepl("^[0-9,]+ de [0-9,]+$", n_todo))
    foto(paste0("A2_busqueda_", et))
    clic(".modal #pan-ad_guardar")
    ok(sprintf("A %s: Guardar closes the panel", et), espera("!document.querySelector('.modal.show')", 15))
    r <- registro()
    req <- r$avisos_de$canales$req
    ok(sprintf("A %s: avisos_de saved: req nadie + the Planta Norte people (%d)", et, length(req$cambia)),
       identical(req$base, "nadie") && length(req$cambia) > 50L && all(grepl("^P", unlist(req$cambia))))
    ok(sprintf("A %s: 'Lo mío' on by default", et), isTRUE(r$avisos_de$mio))
  }, error = function(e) ok(paste("A", et, ": no error:", conditionMessage(e)), FALSE))
  limpiar()
}

# ── B: a buyer ──────────────────────────────────────────────────────────────
tryCatch({
  arrancar(1293L, 632L, nivel = "compras")
  ok("B: the panel opens for a buyer", abrir_panel())
  ok("B: only Requisiciones and Altas, never the treasury columns", identical(titulos(), c("Requisiciones", "Altas")))
  foto("B_compras")
  clic(".modal #pan-ad_todos"); Sys.sleep(0.8)
  clic(".modal #pan-ad_guardar")
  espera("!document.querySelector('.modal.show')", 15)
  r <- registro()
  ok("B: what it saves has no treasury channel", identical(r$avisos_de$canales$req$base, "todos") &&
       identical(r$avisos_de$canales$bancos$base, "nadie") && identical(r$avisos_de$canales$confirmar_pago$base, "nadie") &&
       identical(r$avisos_de$canales$archivo_pago$base, "nadie"))
}, error = function(e) ok(paste("B: no error:", conditionMessage(e)), FALSE))
limpiar()

# ── C: staff ────────────────────────────────────────────────────────────────
tryCatch({
  arrancar(1293L, 632L, staff = "1")
  ok("C: staff see the panel", abrir_panel())
  ok("C: no Requisiciones column", !("Requisiciones" %in% titulos()) && length(titulos()) == 4L)
  ok("C: no 'Lo mio', and the line saying why", !isTRUE(ev("!!document.getElementById('pan-ad_mio')")) &&
       isTRUE(ev("/personal de HopDesk/.test(document.querySelector('.modal .pan-avisos-de').innerText)")))
  foto("C_staff")
  clic(".modal #pan-ad_ninguno"); Sys.sleep(0.8)
  clic(".modal #pan-ad_guardar")
  espera("!document.querySelector('.modal.show')", 15)
  r <- registro()
  ok("C: Guardar writes in the staff's home (hopdesk), under avisos_de_clientes$dev",
     identical(r$casa_donde, "hopdesk") && is.list(r$avisos_de_clientes$dev) && is.null(r$avisos_de) &&
       identical(r$avisos_de_clientes$dev$canales$bancos$base, "nadie"))
}, error = function(e) ok(paste("C: no error:", conditionMessage(e)), FALSE))
limpiar()

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
