# =============================================================================
# tests/test_t67_overscroll_chromote.R
# T67: no elastic bounce past the end of a tab (R/ui_components.R,
# overscroll-behavior-y: none). Headless Chrome cannot play a touchpad's
# bounce, so the test reads what decides it, in the real app (dev bucket, read
# only), for every user (staff and client) and every tab they see, at
# 1293 x 632 and 1772 x 866:
#   1. under T67_RAMA: html, body and every scrolling box of the active tab
#      compute overscroll-behavior-y = none;
#   2. under T67_BASE (master before T67) the same read says auto, so the
#      check can tell the two apart;
#   3. the room at the end is unchanged: scrolled to the end, the gap between
#      the last visible content and the window's bottom is the same in base
#      and branch (within 1 px), and so is each box's scroll height.
#
# Needs the test users (.Renviron.local of the checkout that runs it) and
# Chrome's turn (rule 28). The roots may not have their own .Renviron.
#   T67_BASE=C:/temp/<base> T67_RAMA=C:/temp/<rama> [T67_SALIDA=<folder>] \
#   Rscript --vanilla tests/test_t67_overscroll_chromote.R
# T67_USUARIOS narrows it ("prueba", "cliente").
# =============================================================================
cat("=== test_t67_overscroll_chromote ===\n\n")
if (file.exists(".Renviron.local")) readRenviron(".Renviron.local")
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

usuarios <- list(
  prueba  = c(user = Sys.getenv("HOPDESK_TEST_USER"),         pass = Sys.getenv("HOPDESK_TEST_PASS")),
  cliente = c(user = Sys.getenv("HOPDESK_TEST_CLIENTE_USER"), pass = Sys.getenv("HOPDESK_TEST_CLIENTE_PASS")))
quienes <- strsplit(Sys.getenv("T67_USUARIOS", "prueba,cliente"), ",")[[1]]
faltan <- quienes[!vapply(usuarios[quienes], function(u) all(nzchar(u)), logical(1))]
if (length(faltan)) {
  cat("SKIP: faltan las credenciales de prueba de:", paste(faltan, collapse = ", "),
      "en .Renviron.local.\n     Esta prueba NO paso: no se corrio.\n")
  quit(status = 2L)
}
BASE <- Sys.getenv("T67_BASE"); RAMA <- Sys.getenv("T67_RAMA")
if (!nzchar(BASE) || !nzchar(RAMA)) { cat("SKIP: faltan T67_BASE y T67_RAMA.\n     Esta prueba NO paso: no se corrio.\n"); quit(status = 2L) }
BASE <- normalizePath(BASE, winslash = "/"); RAMA <- normalizePath(RAMA, winslash = "/")
SALIDA <- Sys.getenv("T67_SALIDA", file.path(tempdir(), "t67"))
dir.create(SALIDA, showWarnings = FALSE, recursive = TRUE)
LIMITE <- as.numeric(Sys.getenv("ARR_LIMITE", "300"))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
  invisible(isTRUE(result))
}
source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 39033L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)

ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
dentro <- function() isTRUE(tryCatch(ev(
  "(function(){ var f = document.getElementById('auth-user_id');
     return (!f || f.offsetParent === null) && !!document.querySelector('.nav-link[data-value]'); })()"),
  error = function(e) FALSE))
carga_terminada <- function() dentro() && isTRUE(tryCatch(ev(
  "(function(){ var o = document.getElementById('hopdesk-loading');
     return !o || o.style.display === 'none' || o.classList.contains('hld-fadeout'); })()"), error = function(e) FALSE))
tamano <- function(w, h) invisible(b$Emulation$setDeviceMetricsOverride(width = w, height = h, deviceScaleFactor = 1, mobile = FALSE))
captura <- function(nombre) tryCatch({
  s <- b$Page$captureScreenshot(format = "png")
  writeBin(jsonlite::base64_dec(s$data), file.path(SALIDA, paste0(nombre, ".png")))
}, error = function(e) message("  (captura ", nombre, ": ", conditionMessage(e), ")"))
# At 1293 px the navbar collapses behind its ☰: open it to find the tabs.
menu <- function(abrir) tryCatch(ev(sprintf("(function(){
    var t = document.querySelector('.navbar-toggler'); if (!t || t.offsetParent === null) return false;
    var c = document.querySelector(t.getAttribute('data-bs-target') || '.navbar-collapse');
    var abierto = !!c && c.classList.contains('show');
    if (abierto !== %s) t.click(); return true; })()", if (abrir) "true" else "false")), error = function(x) FALSE)

PESTANAS <- c("PAN", "CAL", "VEN", "PH", "BNC", "IC", "PSV", "REQ", "FC", "RPT", "GRUPO")

# The active tab (found through its nav link), every box in it that scrolls,
# all scrolled to the end; then html/body's and each box's
# overscroll-behavior-y, and the gap under the lowest visible leaf (fixed and
# sticky bars aside).
MEDIR_JS <- "(function(v){
  var a0 = document.querySelector(\".nav-link[data-value='\" + v + \"']\");
  var id = a0 ? (a0.getAttribute('data-bs-target') || a0.getAttribute('href') || '').replace(/^#/, '') : '';
  if (!id) return JSON.stringify({saltar: 'no es una pestaña con panel'});   // e.g. GRUPO: a link, not a tab
  var pane = document.getElementById(id);
  if (!pane || !pane.classList.contains('active')) return JSON.stringify({error: 'tab-pane no activo (' + id + ')'});
  var todos = [pane].concat(Array.from(pane.querySelectorAll('*')));
  var cajas = todos.filter(function(e){ var s = getComputedStyle(e);
    return /(auto|scroll)/.test(s.overflowY) && e.scrollHeight > e.clientHeight + 2 && e.offsetParent !== null; });
  cajas.forEach(function(e){ e.scrollTop = e.scrollHeight; });
  document.scrollingElement.scrollTop = document.scrollingElement.scrollHeight;
  var vh = window.innerHeight;
  function fija(e){ for (var x = e; x && x !== document.body; x = x.parentElement) {
      var p = getComputedStyle(x).position; if (p === 'fixed' || p === 'sticky') return true; } return false; }
  var fondo = -1;
  todos.forEach(function(e){
    if (e.childElementCount) return;
    var r = e.getBoundingClientRect(); if (r.height < 1 || r.width < 1 || r.top >= vh) return;
    var s = getComputedStyle(e); if (s.visibility === 'hidden' || +s.opacity === 0 || e.offsetParent === null) return;
    if (!fija(e) && r.bottom > fondo) fondo = r.bottom;
  });
  function nombre(e){ return e.tagName.toLowerCase() + (e.id ? '#' + e.id : '') +
      (typeof e.className === 'string' && e.className.trim() ? '.' + e.className.trim().split(/\\s+/).slice(0, 2).join('.') : ''); }
  return JSON.stringify({
    html: getComputedStyle(document.documentElement).overscrollBehaviorY,
    body: getComputedStyle(document.body).overscrollBehaviorY,
    cajas: cajas.map(function(e){ return {el: nombre(e), osb: getComputedStyle(e).overscrollBehaviorY, alto: e.scrollHeight}; }),
    hueco: fondo < 0 ? null : Math.round(vh - fondo)});
})"

correr <- function(raiz, cual) {
  filas <- list()
  for (quien in quienes) {
    cat(sprintf("\n── %s · %s ──\n", cual, quien))
    if (!is.null(app)) { try(b$close(), silent = TRUE); b <<- NULL; stop_bg_app(app); app <<- NULL }
    app <<- tryCatch(start_bg_app(sprintf('Sys.setenv(ARR_MODO = "normal", ARR_RAIZ = "%s"); setwd("%s"); source("tests/helpers/arranque_app.R")$value',
                                          raiz, repo), PORT, timeout_s = 300, www = file.path(raiz, "www")),
                     error = function(e) { message(conditionMessage(e)); NULL })
    if (!ok(sprintf("[%s · %s] the real app starts on dev", cual, quien), !is.null(app))) next
    b <<- new_chromote_session()
    tamano(1293L, 632L)
    invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE))
    if (!ok(sprintf("[%s · %s] the sign-in form shows", cual, quien),
            poll_until(function() isTRUE(tryCatch(ev("!!document.getElementById('auth-user_id')"), error = function(e) FALSE)), timeout_s = 60)))
      next
    Sys.sleep(2)
    u <- usuarios[[quien]]
    ev(sprintf("(function(){ var u = document.getElementById('auth-user_id'), p = document.getElementById('auth-user_pwd');
       u.value = %s; p.value = %s; $(u).trigger('change'); $(p).trigger('change'); return true; })()",
       jsonlite::toJSON(u[["user"]], auto_unbox = TRUE), jsonlite::toJSON(u[["pass"]], auto_unbox = TRUE)))
    Sys.sleep(0.5)
    ev("document.getElementById('auth-go_auth').click(); true")
    poll_until(carga_terminada, timeout_s = LIMITE, interval_s = 1)
    if (!ok(sprintf("[%s · %s] signed in and loaded", cual, quien), carga_terminada())) next
    # The data takes a while to paint: wait for the tabs, then Panorama's Clima.
    vistas <- function() { menu(TRUE); tryCatch(unlist(jsonlite::fromJSON(ev(sprintf(
      "JSON.stringify([%s].filter(function(v){ var a = document.querySelector(\".nav-link[data-value='\" + v + \"']\");
         return !!a && a.offsetParent !== null; }))", paste0("'", PESTANAS, "'", collapse = ","))))), error = function(x) character()) }
    poll_until(function() length(vistas()) >= 1L, timeout_s = LIMITE, interval_s = 2)
    Sys.sleep(4)
    ves <- vistas()
    if ("PAN" %in% ves)
      poll_until(function() isTRUE(tryCatch(ev("(function(){ var c = document.querySelector('.hd-panorama .pan-clima');
        return !!c && c.getBoundingClientRect().height > 0; })()"), error = function(e) FALSE)), timeout_s = 150, interval_s = 2)
    cat("  pestañas:", paste(ves, collapse = ","), "\n")
    ok(sprintf("[%s · %s] at least one tab shows", cual, quien), length(ves) > 0L)
    for (v in ves) {
      menu(TRUE); Sys.sleep(1)
      ev(sprintf("document.querySelector(\".nav-link[data-value='%s']\").click(); true", v))
      Sys.sleep(1); menu(FALSE); Sys.sleep(4)
      poll_until(function() isTRUE(tryCatch(ev("!document.querySelector('.tab-pane.active .hdt-cargando')"), error = function(x) FALSE)),
                 timeout_s = 60, interval_s = 2)
      Sys.sleep(3)
      login <- isTRUE(tryCatch(ev("(function(){ var f = document.getElementById('auth-user_id'); return !!f && f.offsetParent !== null; })()"),
                               error = function(x) FALSE))
      if (!ok(sprintf("[%s · %s · %s] no sign-in form on the tab", cual, quien, v), !login)) next
      for (tm in list(c(1293L, 632L), c(1772L, 866L))) {
        tamano(tm[1], tm[2]); Sys.sleep(1.5)
        m <- tryCatch(jsonlite::fromJSON(ev(sprintf("%s('%s')", MEDIR_JS, v)), simplifyVector = FALSE),
                      error = function(e) list(error = conditionMessage(e)))
        if (!is.null(m$saltar)) { cat(sprintf("  %-5s %4d  saltada: %s\n", v, tm[1], m$saltar)); next }
        if (!is.null(m$error)) { ok(sprintf("[%s · %s · %s · %d] measured (%s)", cual, quien, v, tm[1], m$error), FALSE); next }
        if (identical(cual, "rama")) captura(sprintf("t67_%s_%s_%d", quien, v, tm[1]))
        cajas <- vapply(m$cajas, function(k) paste0(k$el, "=", k$osb), character(1))
        filas[[length(filas) + 1L]] <- list(usuario = quien, pestana = v, ancho = tm[1], html = m$html, body = m$body,
          osb = vapply(m$cajas, function(k) k$osb, character(1)), altos = vapply(m$cajas, function(k) k$alto, numeric(1)),
          hueco = m$hueco %||% NA_real_, cajas = cajas)
        cat(sprintf("  %-5s %4d  html=%s body=%s hueco=%s  %s\n", v, tm[1], m$html, m$body, m$hueco %||% "NA", paste(cajas, collapse = " ")))
        ev("document.scrollingElement.scrollTop = 0; true")
      }
      tamano(1293L, 632L); Sys.sleep(1)
    }
  }
  filas
}

fb <- correr(BASE, "base")
fr <- correr(RAMA, "rama")
clave <- function(f) paste(f$usuario, f$pestana, f$ancho)
names(fb) <- vapply(fb, clave, ""); names(fr) <- vapply(fr, clave, "")

cat("\n── checks ──\n")
ok(sprintf("the branch measured something (%d tab views)", length(fr)), length(fr) > 0L)
for (k in names(fr)) {
  f <- fr[[k]]
  ok(sprintf("[rama · %s] html, body and %d scrolling box(es): overscroll-behavior-y none", k, length(f$osb)),
     identical(f$html, "none") && identical(f$body, "none") && all(f$osb == "none"))
}
# The base had none of it: the check can tell them apart.
ok("[base] html and body were auto before T67 (the check sees the change)",
   length(fb) > 0L && all(vapply(fb, function(f) identical(f$html, "auto") && identical(f$body, "auto"), logical(1))))
comunes <- intersect(names(fb), names(fr))
ok(sprintf("base and branch measured the same tab views (%d)", length(comunes)), length(comunes) > 0L && length(comunes) == length(fr))
for (k in comunes) {
  a <- fb[[k]]; r <- fr[[k]]
  ok(sprintf("[%s] the room at the end is unchanged (hueco %s -> %s, scroll heights %s -> %s)", k,
             a$hueco, r$hueco, paste(a$altos, collapse = "/"), paste(r$altos, collapse = "/")),
     identical(is.na(a$hueco), is.na(r$hueco)) && (is.na(a$hueco) || abs(a$hueco - r$hueco) <= 1) &&
       length(a$altos) == length(r$altos) && all(abs(a$altos - r$altos) <= 1))
}

cat(sprintf("\nCapturas: %s\n", SALIDA))
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
