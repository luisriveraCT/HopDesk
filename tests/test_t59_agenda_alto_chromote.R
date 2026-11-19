# =============================================================================
# tests/test_t59_agenda_alto_chromote.R
# T59 (3): the Agenda's company panel (.ph-company-panel) fills what is left
# under the summary bar and the company tabs, whatever their height, instead
# of max-height: calc(100vh - 180px). In the browser, at 1293x632 (Luis's
# window) and 1772x866, as staff and as a client, on every company tab:
#   - the panel ends inside the window and fills it (no dead band below);
#   - scrolled to its end, its content ends inside the panel and the window;
#   - and the same with the summary bar 70 px taller (the net position wrapped
#     into two lines), where the old 180 ran the panel past the window.
# The real app against dev (credentials in .Renviron.local; never printed).
# Chrome takes its turn (rule 28).
#
# Run from project root:  Rscript tests/test_t59_agenda_alto_chromote.R
# =============================================================================
cat("=== test_t59_agenda_alto_chromote ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
renv <- Sys.getenv("T59_RENVIRON", ".Renviron.local"); if (file.exists(renv)) readRenviron(renv)
if (!nzchar(Sys.getenv("HOPDESK_TEST_USER")) || !nzchar(Sys.getenv("HOPDESK_TEST_CLIENTE_USER"))) {
  cat("SKIP: no test credentials in .Renviron.local\n"); quit(status = 0L)
}
source("tests/helpers/live_app_harness.R")
PORT <- 39063L
FOTOS <- Sys.getenv("T59_FOTOS", "")
repo <- normalizePath(".", winslash = "/")
f <- file.path(repo, ".t59_app.R")
writeLines(sprintf('Sys.setenv(ARR_MODO = "normal", ARR_RAIZ = "%s"); setwd("%s"); source("tests/helpers/arranque_app.R")$value', repo, repo), f)
proc <- callr::r_bg(function(app_file, port) shiny::runApp(app_file, port = port, host = "127.0.0.1", launch.browser = FALSE),
                    args = list(app_file = f, port = PORT), stdout = tempfile(), stderr = tempfile(),
                    env = c(callr::rcmd_safe_env(), Sys.getenv()))
b <- NULL
# Rscript never runs a top-level on.exit(): the cleanup is called by hand.
limpiar <- function() { try(if (!is.null(b)) b$close(), silent = TRUE); try(proc$kill(), silent = TRUE); unlink(f) }
ev <- function(js) { r <- b$Runtime$evaluate(js, returnByValue = TRUE); r$result$value }
espera <- function(js, s = 60) isTRUE(tryCatch({ poll_until(function() isTRUE(tryCatch(ev(js), error = function(e) FALSE)),
  timeout_s = s, interval_s = 0.5); TRUE }, error = function(e) FALSE))
foto <- function(nombre) if (nzchar(FOTOS)) { dir.create(FOTOS, showWarnings = FALSE, recursive = TRUE)
  writeBin(jsonlite::base64_dec(b$Page$captureScreenshot(format = "png")$data), file.path(FOTOS, paste0(nombre, ".png"))) }

# The active company panel, scrolled to its end: its box against the window,
# and its content's lowest edge against the box.
JS_PANEL <- "(function(){
  var p = document.querySelector('.tab-pane.active .ph-container .tab-pane.active .ph-company-panel');
  if (!p) return JSON.stringify({sin_panel: true});
  p.scrollTop = p.scrollHeight;
  var r = p.getBoundingClientRect(), fondo = -Infinity;
  for (var c = p.firstElementChild; c; c = c.nextElementSibling) {
    var cr = c.getBoundingClientRect(); if (cr.height > 0) fondo = Math.max(fondo, cr.bottom); }
  var res = { arriba: Math.round(r.top), abajo: Math.round(r.bottom), fondo: Math.round(fondo),
              ventana: window.innerHeight, desborda: p.scrollHeight > p.clientHeight + 2 };
  p.scrollTop = 0;
  return JSON.stringify(res); })()"
# A summary bar 70 px taller, as when the net position wraps (removed after).
JS_RESUMEN_ALTO <- "(function(){ var s = document.querySelector('.tab-pane.active .ph-summary-bar'); if (!s) return false;
  var d = document.createElement('div'); d.id = 't59-relleno'; d.style.cssText = 'height:70px;width:1px;flex:none';
  s.appendChild(d); return true; })()"
JS_RESUMEN_NORMAL <- "(function(){ var d = document.getElementById('t59-relleno'); if (d) d.remove(); return true; })()"

revisar <- function(lugar) {
  Sys.sleep(1)
  m <- jsonlite::fromJSON(ev(JS_PANEL))
  if (isTRUE(m$sin_panel)) { ok(sprintf("%s: the company panel is there", lugar), FALSE); return(invisible()) }
  det <- sprintf("(panel %d–%d, contenido hasta %d, ventana %d)", m$arriba, m$abajo, m$fondo, m$ventana)
  ok(sprintf("%s: the panel ends inside the window %s", lugar, det), m$abajo <= m$ventana + 0.5)
  ok(sprintf("%s: …and fills it, no dead band under it %s", lugar, det), m$abajo >= m$ventana - 24)
  ok(sprintf("%s: scrolled to its end, its content ends inside the panel and the window %s", lugar, det),
     m$fondo <= m$abajo + 0.5 && m$fondo <= m$ventana + 0.5)
}

tryCatch({
  poll_until(function() !inherits(tryCatch(curl::curl_fetch_memory(sprintf("http://127.0.0.1:%d/", PORT),
    handle = curl::new_handle(TIMEOUT = 2)), error = function(e) e), "error"), timeout_s = 300, interval_s = 1)
  for (quien in c("staff", "cliente")) {
    U <- if (quien == "staff") "HOPDESK_TEST_USER" else "HOPDESK_TEST_CLIENTE_USER"
    P <- if (quien == "staff") "HOPDESK_TEST_PASS" else "HOPDESK_TEST_CLIENTE_PASS"
    b <- new_chromote_session()
    invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
    invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE))
    poll_until(function() isTRUE(tryCatch(ev("!!document.getElementById('auth-user_id')"), error = function(e) FALSE)), timeout_s = 60)
    ev(sprintf("(function(){ var u = document.getElementById('auth-user_id'), p = document.getElementById('auth-user_pwd');
       u.value = %s; p.value = %s; $(u).trigger('change'); $(p).trigger('change'); return true; })()",
       jsonlite::toJSON(Sys.getenv(U), auto_unbox = TRUE), jsonlite::toJSON(Sys.getenv(P), auto_unbox = TRUE)))
    Sys.sleep(0.5); ev("document.getElementById('auth-go_auth').click(); true")
    poll_until(function() isTRUE(tryCatch(ev("(function(){ var u = document.getElementById('navbar_user_badge');
      return !!u && u.textContent.trim().length > 0; })()"), error = function(e) FALSE)), timeout_s = 300, interval_s = 2)
    Sys.sleep(5)
    ev("(function(){ var a = document.querySelector(\"nav.navbar a[data-value='PH']\"); if (window.jQuery) jQuery(a).tab('show'); a.click(); return true; })()")
    ok(sprintf("%s: the Agenda shows its company tabs", quien),
       espera("document.querySelectorAll('.tab-pane.active .ph-container .nav-tabs .nav-link[data-value]').length > 0", 120))
    emps <- jsonlite::fromJSON(ev("JSON.stringify([].map.call(document.querySelectorAll('.tab-pane.active .ph-container .nav-tabs .nav-link[data-value]'),
      function(a){ return a.getAttribute('data-value'); }))"))
    cat(sprintf("\n-- %s: %d company tab(s) --\n", quien, length(emps)))
    for (tam in list(c(1293L, 632L), c(1772L, 866L))) {
      invisible(b$Emulation$setDeviceMetricsOverride(width = tam[1], height = tam[2], deviceScaleFactor = 1, mobile = FALSE))
      Sys.sleep(2)
      for (k in seq_along(emps)) {
        ev(sprintf("(function(){ var a = document.querySelectorAll('.tab-pane.active .ph-container .nav-tabs .nav-link[data-value]')[%d];
          if (window.jQuery) jQuery(a).tab('show'); a.click(); return true; })()", k - 1L))
        espera("!!document.querySelector('.tab-pane.active .ph-container .tab-pane.active .ph-company-panel')", 30)
        lugar <- sprintf("%s · %dx%d · %s", quien, tam[1], tam[2], emps[k])
        revisar(lugar)
        if (k == 1L) foto(sprintf("t59_%s_%dx%d", quien, tam[1], tam[2]))
        ev(JS_RESUMEN_ALTO); revisar(paste(lugar, "· summary 70 px taller"))
        if (k == 1L) foto(sprintf("t59_%s_%dx%d_resumen_alto", quien, tam[1], tam[2]))
        ev(JS_RESUMEN_NORMAL)
      }
    }
    ev("(function(){ var x = document.getElementById('btn_logout'); if (x) x.click(); return !!x; })()")
    poll_until(function() isTRUE(tryCatch(ev("!!document.getElementById('auth-user_id')"), error = function(e) FALSE)), timeout_s = 60)
    b$close(); b <- NULL
  }
}, error = function(e) ok(paste("no error running the checks:", conditionMessage(e)), FALSE))
limpiar()

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
