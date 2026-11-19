# =============================================================================
# tests/test_t56_notas_chromote.R
# T56, in the browser: at Luis's window (1293x632), on every tab the user can
# open (and every pill of Usuarios), with every scrolling box scrolled to its
# end, nothing readable or clickable lies under the floating "Notas del mes"
# button. Usuarios and Empresas keep their own scroll (T51) and never show two
# vertical scrollbars at once. Static half: tests/test_t56_notas_reserva.R.
#
# The real app against dev, logged in as staff and as a client (credentials
# in .Renviron.local: HOPDESK_TEST_USER/_PASS, HOPDESK_TEST_CLIENTE_USER/_PASS;
# never printed). Chrome takes its turn (rule 28).
#
# Run from project root:  Rscript tests/test_t56_notas_chromote.R
# =============================================================================
cat("=== test_t56_notas_chromote ===\n\n")
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || is.na(x)) y else x
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
renv <- Sys.getenv("T56_RENVIRON", ".Renviron.local"); if (file.exists(renv)) readRenviron(renv)
if (!nzchar(Sys.getenv("HOPDESK_TEST_USER")) || !nzchar(Sys.getenv("HOPDESK_TEST_CLIENTE_USER"))) {
  cat("SKIP: no test credentials in .Renviron.local\n"); quit(status = 0L)
}
source("tests/helpers/live_app_harness.R")
ANCHO <- 1293L; ALTO <- 632L; PORT <- 39062L
FOTOS <- Sys.getenv("T56_FOTOS", "")      # a folder: screenshots of every tab scrolled to its end
repo <- normalizePath(".", winslash = "/")
f <- file.path(repo, ".t56_app.R")
writeLines(sprintf('Sys.setenv(ARR_MODO = "normal", ARR_RAIZ = "%s"); setwd("%s"); source("tests/helpers/arranque_app.R")$value', repo, repo), f)
proc <- callr::r_bg(function(app_file, port) shiny::runApp(app_file, port = port, host = "127.0.0.1", launch.browser = FALSE),
                    args = list(app_file = f, port = PORT), stdout = tempfile(), stderr = tempfile(),
                    env = c(callr::rcmd_safe_env(), Sys.getenv()))
b <- NULL
# Rscript never runs a top-level on.exit(): the cleanup is called by hand.
limpiar <- function() { try(if (!is.null(b)) b$close(), silent = TRUE); try(proc$kill(), silent = TRUE); unlink(f) }
poll_until(function() !inherits(tryCatch(curl::curl_fetch_memory(sprintf("http://127.0.0.1:%d/", PORT),
  handle = curl::new_handle(TIMEOUT = 2)), error = function(e) e), "error"), timeout_s = 300, interval_s = 1)
ev <- function(js) { r <- b$Runtime$evaluate(js, returnByValue = TRUE); r$result$value }

# What lies under the button: its box plus 4 px of air, sampled every 3 px
# with the button hidden. A point counts when the text of an element's own
# text node, or an interactive element, is there.
JS_BAJO <- "(function(){
  var bar = document.getElementById('notes_bar'); if (!bar) return JSON.stringify({sin_boton: true});
  if (getComputedStyle(bar).display === 'none') return JSON.stringify({oculto: true, puntos: 0});
  var r = bar.getBoundingClientRect();
  bar.style.visibility = 'hidden';
  var hits = {}, n = 0;
  function desc(el){ var s = el.tagName.toLowerCase(); if (el.id) s += '#' + el.id;
    if (el.className && typeof el.className === 'string') s += '.' + el.className.trim().split(/\\s+/).slice(0,3).join('.'); return s; }
  for (var x = r.left - 4; x <= r.right + 4; x += 3) for (var y = r.top - 4; y <= r.bottom + 4; y += 3) {
    var el = document.elementFromPoint(x, y); if (!el) continue;
    var cosa = null;
    var inter = el.closest('button, a, input, select, textarea, label, .btn, [role=button]');
    if (inter) cosa = 'interactivo ' + desc(inter) + ' «' + (inter.innerText || inter.value || '').trim().slice(0, 40) + '»';
    else for (var c = el.firstChild; c && !cosa; c = c.nextSibling) if (c.nodeType === 3 && c.textContent.trim()) {
      var rg = document.createRange(); rg.selectNodeContents(c); var rs = rg.getClientRects();
      for (var i = 0; i < rs.length; i++) if (x >= rs[i].left && x <= rs[i].right && y >= rs[i].top && y <= rs[i].bottom) {
        cosa = 'texto ' + desc(el) + ' «' + c.textContent.trim().slice(0, 40) + '»'; break; } }
    if (cosa) { hits[cosa] = (hits[cosa] || 0) + 1; n++; }
  }
  bar.style.visibility = '';
  return JSON.stringify({ boton: [Math.round(r.left), Math.round(r.top), Math.round(r.right), Math.round(r.bottom)], puntos: n, bajo: hits });
})()"
# Every scrolling box of the active panes (and the page) to its end.
JS_AL_FINAL <- "(function(){
  var todos = [document.scrollingElement];
  document.querySelectorAll('.tab-pane.active').forEach(function(p){ todos.push(p); p.querySelectorAll('*').forEach(function(e){ todos.push(e); }); });
  var n = 0;
  todos.forEach(function(e){ var cs = getComputedStyle(e);
    if ((e === document.scrollingElement || /(auto|scroll)/.test(cs.overflowY)) && e.scrollHeight > e.clientHeight + 2) { e.scrollTop = e.scrollHeight; n++; } });
  return n; })()"
# Geometry, data or not: every scrolling box of the active panes that reaches
# below the button's top and spans its columns, scrolled to its end, must end
# its content (its children's lowest edge) above the button.
JS_FIN_CONTENIDO <- "(function(){
  var bar = document.getElementById('notes_bar'); if (!bar || getComputedStyle(bar).display === 'none') return '[]';
  var r = bar.getBoundingClientRect(); var malas = [];
  document.querySelectorAll('.tab-pane.active, .tab-pane.active *').forEach(function(e){
    var cs = getComputedStyle(e); if (!/(auto|scroll)/.test(cs.overflowY)) return;
    var b = e.getBoundingClientRect(); if (b.width < 1 || b.height < 1) return;
    if (b.bottom <= r.top || b.left >= r.right || b.right <= r.left) return;
    if (e.querySelector('.pan-cinta')) return;   // T49: the Cinta lifts the button; the Cinta itself sits below it
    var fondo = -Infinity;
    for (var c = e.firstElementChild; c; c = c.nextElementSibling) {
      var cr = c.getBoundingClientRect(); if (cr.height > 0 && getComputedStyle(c).display !== 'none') fondo = Math.max(fondo, cr.bottom); }
    if (fondo > r.top + 0.5) malas.push((e.className && typeof e.className === 'string' ? e.className.split(/\\s+/).slice(0,2).join('.') : e.tagName) +
      ' termina en ' + Math.round(fondo) + ' (botón en ' + Math.round(r.top) + '; la caja acaba en ' + Math.round(b.bottom) + ', ventana ' + window.innerHeight + ')');
  });
  return JSON.stringify(malas); })()"
JS_AL_INICIO <-"(function(){ document.querySelectorAll('*').forEach(function(e){ if (e.scrollTop) e.scrollTop = 0; }); return true; })()"
# The vertical scrollbars showing at once: the page plus every box of the
# active panes whose content overflows a scrolling overflow-y.
JS_BARRAS <- "(function(){
  var n = 0, de = [];
  var se = document.scrollingElement; if (se.scrollHeight > se.clientHeight + 2) { n++; de.push('página'); }
  var vistos = new Set();
  document.querySelectorAll('.tab-pane.active, .tab-pane.active *').forEach(function(e){
    if (vistos.has(e)) return; vistos.add(e);
    var cs = getComputedStyle(e); if (!/(auto|scroll)/.test(cs.overflowY)) return;
    var r = e.getBoundingClientRect(); if (r.width < 1 || r.height < 1) return;
    if (e.scrollHeight > e.clientHeight + 2) { n++; de.push(e.className && typeof e.className === 'string' ? e.className.split(/\\s+/).slice(0,2).join('.') : e.tagName); } });
  var caja = document.querySelector('.tab-pane.active .hd-scroll-pestana');
  var cs = caja ? getComputedStyle(caja) : null;
  return JSON.stringify({ barras: n, de: de, caja: !!caja, overflow: cs ? cs.overflowY : null,
    max_alto: cs ? cs.maxHeight : null, hueco: caja ? getComputedStyle(caja, '::after').height : null }); })()"
mostrar <- function(valor) ev(sprintf("(function(){ var a = document.querySelector(\"nav.navbar a[data-value='%1$s']\") ||
  document.querySelector(\".nav-link[data-value='%1$s']\"); if (!a) return false;
  if (window.jQuery) jQuery(a).tab('show'); a.click(); return true; })()", valor))
foto <- function(nombre) if (nzchar(FOTOS)) { dir.create(FOTOS, showWarnings = FALSE, recursive = TRUE)
  writeBin(jsonlite::base64_dec(b$Page$captureScreenshot(format = "png")$data), file.path(FOTOS, paste0(nombre, ".png"))) }

FLOTA <- "(function(){ var b = document.getElementById('notes_bar'); return !!b && getComputedStyle(b).display !== 'none'; })()"
PANEL <- "!!document.querySelector('#notes_panel .notes-panel-wrapper')"
clic <- function(sel) {   # a real mouse click at the element's centre (Input.dispatchMouseEvent)
  p <- jsonlite::fromJSON(ev(sprintf("(function(){ var e = document.querySelector(%s); if (!e) return 'null';
    var r = e.getBoundingClientRect(); return JSON.stringify([r.left + r.width / 2, r.top + r.height / 2]); })()",
    jsonlite::toJSON(sel, auto_unbox = TRUE))))
  if (is.null(p)) return(FALSE)
  for (t in c("mousePressed", "mouseReleased"))
    b$Input$dispatchMouseEvent(type = t, x = p[1], y = p[2], button = "left", clickCount = 1)
  TRUE
}
espera <- function(js, s = 30) isTRUE(tryCatch({ poll_until(function() isTRUE(tryCatch(ev(js), error = function(e) FALSE)),
  timeout_s = s, interval_s = 0.5); TRUE }, error = function(e) FALSE))
# The Calendar: the header's "Notas del mes" opens and closes the very panel
# the floating button opens (#notes_panel, drawn by the server), and with the
# Flujo preview slid in the floating button comes back.
calendario <- function(quien) {
  mostrar("CAL")
  ok(sprintf("%s · CAL: the header has \"Notas del mes\", from the dictionary", quien),
     espera("(function(){ var e = document.querySelector('.tab-pane.active .cal-title-bar .cal-notes-btn');
       return !!e && e.getBoundingClientRect().width > 0 && e.textContent.trim() === 'Notas del mes'; })()"))
  ok(sprintf("%s · CAL: the floating button is hidden", quien), !isTRUE(ev(FLOTA)))
  ok(sprintf("%s · CAL: the panel starts closed", quien), !isTRUE(ev(PANEL)))
  clic(".tab-pane.active .cal-title-bar .cal-notes-btn")
  ok(sprintf("%s · CAL: the header's button opens the same notes panel (#notes_panel, from the server)", quien), espera(PANEL))
  foto(sprintf("t56_%s_CAL_notas_abiertas", quien))
  clic(".tab-pane.active .cal-title-bar .cal-notes-btn")
  ok(sprintf("%s · CAL: …and closes it", quien), espera(sprintf("!%s", PANEL)))
  clic(".tab-pane.active .cal-title-bar .cf-preview-toggle-btn")
  ok(sprintf("%s · CAL with the Flujo preview: the floating button comes back", quien),
     espera("document.querySelector('.cf-slide-wrapper.cf-preview-active') !== null") && espera(FLOTA))
  foto(sprintf("t56_%s_CAL_flujo", quien))
  clic(".cf-back-arrow")
  ok(sprintf("%s · CAL back from the Flujo: hidden again", quien),
     espera("document.querySelector('.cf-slide-wrapper.cf-preview-active') === null") && espera(sprintf("!%s", FLOTA)))
}

revisar <- function(quien, tab) {
  Sys.sleep(6); ev(JS_AL_INICIO); invisible(ev(JS_AL_FINAL)); Sys.sleep(1.5)
  m <- jsonlite::fromJSON(ev(JS_BAJO))
  # The floating button: hidden only on the Calendar (its header has one), shown everywhere else.
  ok(sprintf("%s · %s: the floating button is %s", quien, tab, if (tab == "CAL") "hidden (the header has it)" else "there"),
     if (tab == "CAL") isTRUE(m$oculto) else !isTRUE(m$oculto) && !isTRUE(m$sin_boton))
  foto(sprintf("t56_%s_%s_final", quien, gsub("[^A-Za-z0-9_]", "_", tab)))
  bajo <- if (length(m$bajo)) paste(sprintf("%s ×%s", names(m$bajo), unlist(m$bajo)), collapse = "; ") else ""
  ok(sprintf("%s · %s: scrolled to the end, nothing under the button%s", quien, tab,
             if (nzchar(bajo)) paste0(" (", bajo, ")") else ""), isTRUE(m$sin_boton) || identical(as.integer(m$puntos), 0L))
  malas <- jsonlite::fromJSON(ev(JS_FIN_CONTENIDO))
  ok(sprintf("%s · %s: scrolled to the end, every box's content ends above the button%s", quien, tab,
             if (length(malas)) paste0(" (", paste(malas, collapse = "; "), ")") else ""), length(malas) == 0L)
  ev(JS_AL_INICIO)
}

tryCatch(for (quien in c("staff", "cliente")) {
  U <- if (quien == "staff") "HOPDESK_TEST_USER" else "HOPDESK_TEST_CLIENTE_USER"
  P <- if (quien == "staff") "HOPDESK_TEST_PASS" else "HOPDESK_TEST_CLIENTE_PASS"
  b <- new_chromote_session()
  invisible(b$Emulation$setDeviceMetricsOverride(width = ANCHO, height = ALTO, deviceScaleFactor = 1, mobile = FALSE))
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE))
  poll_until(function() isTRUE(tryCatch(ev("!!document.getElementById('auth-user_id')"), error = function(e) FALSE)), timeout_s = 60)
  ev(sprintf("(function(){ var u = document.getElementById('auth-user_id'), p = document.getElementById('auth-user_pwd');
     u.value = %s; p.value = %s; $(u).trigger('change'); $(p).trigger('change'); return true; })()",
     jsonlite::toJSON(Sys.getenv(U), auto_unbox = TRUE), jsonlite::toJSON(Sys.getenv(P), auto_unbox = TRUE)))
  Sys.sleep(0.5); ev("document.getElementById('auth-go_auth').click(); true")
  poll_until(function() isTRUE(tryCatch(ev("(function(){ var u = document.getElementById('navbar_user_badge');
    return !!u && u.textContent.trim().length > 0; })()"), error = function(e) FALSE)), timeout_s = 300, interval_s = 2)
  Sys.sleep(5)
  tabs <- unique(jsonlite::fromJSON(ev("JSON.stringify([].map.call(document.querySelectorAll('nav.navbar a[data-value]'),
    function(a){ return a.getAttribute('data-value'); }))")))
  tabs <- setdiff(tabs, "GRUPO")            # a menu, not a tab: its items (TIERS, EMP) are in the list
  cat(sprintf("\n-- %s: %s --\n", quien, paste(tabs, collapse = " ")))
  ok(sprintf("%s: the Calendar is among the tabs", quien), "CAL" %in% tabs)
  for (tb in tabs) {
    if (!isTRUE(mostrar(tb))) { ok(sprintf("%s · %s: its link opens", quien, tb), FALSE); next }
    revisar(quien, tb)
    if (tb %in% c("TIERS", "EMP")) {
      pills <- if (tb == "TIERS") unique(jsonlite::fromJSON(ev("JSON.stringify([].map.call(
        document.querySelectorAll('.tab-pane.active .nav-pills .nav-link[data-value]'), function(a){ return a.getAttribute('data-value'); }))"))) else ""
      for (pl in pills) {
        if (nzchar(pl)) { mostrar(pl); revisar(quien, paste0(tb, "/", pl)) }
        Sys.sleep(1); ev(JS_AL_INICIO)
        s <- jsonlite::fromJSON(ev(JS_BARRAS))
        lugar <- sprintf("%s · %s%s", quien, tb, if (nzchar(pl)) paste0("/", pl) else "")
        ok(sprintf("%s: its box keeps its own scroll (overflow-y auto, a max-height; T51) and the room (an ::after of 56px)", lugar),
           isTRUE(s$caja) && identical(s$overflow, "auto") && !identical(s$max_alto, "none") && identical(s$hueco, "56px"))
        ok(sprintf("%s: at most one vertical scrollbar (%s)", lugar, paste(s$de, collapse = ", ")), s$barras <= 1L)
      }
    }
  }
  calendario(quien)
  otra <- setdiff(tabs, "CAL")[1]
  mostrar(otra)
  ok(sprintf("%s · back on %s: the floating button is there again", quien, otra), espera(FLOTA))
  # Log out, so the next user's session starts at the login screen.
  ev("(function(){ var x = document.getElementById('btn_logout'); if (x) x.click(); return !!x; })()")
  poll_until(function() isTRUE(tryCatch(ev("!!document.getElementById('auth-user_id')"), error = function(e) FALSE)), timeout_s = 60)
  b$close(); b <- NULL
}, error = function(e) ok(paste("no error running the checks:", conditionMessage(e)), FALSE))
limpiar()

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
