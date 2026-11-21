# =============================================================================
# tests/test_tema_marca_puente_chromote.R
# TEMA stage 2: the Bootstrap bridge (www/hd_tema_bootstrap.css) in the real
# app (tests/helpers/arranque_app.R, dev bucket, no writes), started from
# TEMA_RAMA. For every user (staff and client):
#   1. the page asks for hd_tema_bootstrap.css and gets 200;
#   2. a fixed sample of Bootstrap components, added to the page by the test
#      (button, outline button, warning button, link, focused input, checked
#      box, active tab, badge, heading, warning alert; TEMA 2b: an
#      actionButton's btn-default, btn-secondary and btn-outline-secondary,
#      at rest and under a forced :hover, and the top bar, its ☰ and the
#      active tab's name), is read with
#      <html data-hd-tema="clasico"> and then "marca":
#        - under Clásico it keeps flatly's colours (the bridge does not reach
#          it; tests/test_tema_clasico_igual_chromote.R checks the whole app);
#        - under Marca every value is the role's, taken from the registry
#          (R/tema_registro.R), never written here;
#   3. a screenshot of every tab under Marca at 1293 x 632 and 1772 x 866 to
#      TEMA_SALIDA, to look at (they are not the test: inline styles and
#      !important in app.R still win until TEMA stage 4).
#
# Needs the test users (.Renviron.local of the checkout that runs it) and
# Chrome's turn (rule 28). The root may not have its own .Renviron.
#   TEMA_RAMA=C:/temp/tema_<hash> TEMA_SALIDA=<folder> \
#   Rscript --vanilla tests/test_tema_marca_puente_chromote.R
# TEMA_USUARIOS narrows it ("prueba", "cliente").
# =============================================================================
cat("=== test_tema_marca_puente_chromote ===\n\n")
if (file.exists(".Renviron.local")) readRenviron(".Renviron.local")
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

usuarios <- list(
  prueba  = c(user = Sys.getenv("HOPDESK_TEST_USER"),         pass = Sys.getenv("HOPDESK_TEST_PASS")),
  cliente = c(user = Sys.getenv("HOPDESK_TEST_CLIENTE_USER"), pass = Sys.getenv("HOPDESK_TEST_CLIENTE_PASS")))
quienes <- strsplit(Sys.getenv("TEMA_USUARIOS", "prueba,cliente"), ",")[[1]]
faltan <- quienes[!vapply(usuarios[quienes], function(u) all(nzchar(u)), logical(1))]
if (length(faltan)) {
  cat("SKIP: faltan las credenciales de prueba de:", paste(faltan, collapse = ", "),
      "en .Renviron.local.\n     Esta prueba NO paso: no se corrio.\n")
  quit(status = 2L)
}
RAMA <- normalizePath(Sys.getenv("TEMA_RAMA", "."), winslash = "/")
SALIDA <- Sys.getenv("TEMA_SALIDA", file.path(tempdir(), "tema_marca"))
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
PORT <- 39031L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)

# What the sample must show, from the registry of the branch under test.
reg <- new.env(); sys.source(file.path(RAMA, "R/tema_registro.R"), envir = reg)
rgb_de <- function(hex) { v <- strtoi(substring(hex, c(2, 4, 6), c(3, 5, 7)), 16L); sprintf("rgb(%d, %d, %d)", v[1], v[2], v[3]) }
M <- reg$tema_tokens("marca")
mc <- function(rol) rgb_de(M[[rol]])
ESPERADO_MARCA <- list(
  btn_bg = mc("primario"), btn_color = mc("primario-texto"),
  outline_color = mc("primario"), outline_border = mc("primario"),
  warn_bg = mc("aviso"), warn_color = mc("texto"),
  link = mc("enlace"),
  focus_border = mc("foco-color"),
  check_bg = mc("primario"),
  tab_color = mc("primario"),
  badge_bg = mc("primario"),
  alert_bg = mc("aviso-suave"), alert_color = mc("aviso-texto"),
  bs_primary = toupper(M[["primario"]]),
  # TEMA 2b: the quiet button (an actionButton() without a class is btn-default)
  def_bg = mc("superficie"), def_color = mc("texto"), def_border = mc("borde-fuerte"),
  sec_bg = mc("superficie"), sec_color = mc("texto"), sec_border = mc("borde-fuerte"),
  osec_color = mc("texto-2"), osec_border = mc("borde-fuerte"),
  sec_hover_bg = mc("hover"), sec_hover_color = mc("texto"),
  osec_hover_bg = mc("hover"), osec_hover_color = mc("texto"),
  # TEMA 2b: the top bar
  nav_bg = mc("barra-fondo"), nav_brand = mc("barra-texto"), menu_btn = mc("barra-texto"), activa = mc("barra-texto"))
# Under Clásico the bridge must not reach the sample: flatly as compiled
# (hd_bs_theme(): primary #0A58CA, warning #f39c12).
ESPERADO_CLASICO <- list(btn_bg = "rgb(10, 88, 202)", warn_bg = "rgb(243, 156, 18)", check_bg = "rgb(10, 88, 202)",
                         badge_bg = "rgb(10, 88, 202)",
                         def_bg = "rgb(149, 165, 166)", sec_bg = "rgb(149, 165, 166)",   # flatly's grey #95a5a6
                         nav_bg = "rgb(10, 88, 202)", menu_btn = "rgb(255, 255, 255)", activa = "rgb(255, 255, 255)",
                         focus_border = "rgb(133, 172, 229)")   # flatly's #85ace5: proves the input is focused

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

PESTANAS <- c(PAN = "Panorama", CAL = "Calendario", VEN = "Vencidos", PH = "Agenda de Hoy", BNC = "Bancos",
              IC = "Intercompany", PSV = "Pasivos", FC = "Pronósticos", RPT = "Reporte", GRUPO = "Grupo")

AMPLIA_BUFER_JS <- "performance.setResourceTimingBufferSize(10000);"
ESTADO_RECURSOS_JS <- "(function(){
  var urls = Array.from(document.querySelectorAll('link[rel=stylesheet][href], script[src]'))
    .map(function(e){ return e.href || e.src; }).filter(function(u){ return u.indexOf(location.origin) === 0; });
  var ent = performance.getEntriesByType('resource');
  return JSON.stringify(urls.map(function(u){
    var e = ent.filter(function(x){ return x.name === u; }).pop();
    var s = !e ? 'sin-registro' : (typeof e.responseStatus === 'number' && e.responseStatus > 0 ? e.responseStatus : 'sin-estado');
    return s + ' ' + u.replace(location.origin, '');
  }));
})()"

# The sample: plain Bootstrap markup, at the top of <body>, above everything.
MUESTRA_JS <- "(function(){
  var v = document.getElementById('hd-muestra-puente'); if (v) v.remove();
  var d = document.createElement('div'); d.id = 'hd-muestra-puente';
  d.style.cssText = 'position:fixed;top:0;left:0;z-index:99999;padding:8px;width:600px;';
  d.innerHTML =
    '<button id=\"m-btn\" class=\"btn btn-primary\">P</button>' +
    '<button id=\"m-out\" class=\"btn btn-outline-primary\">O</button>' +
    '<button id=\"m-warn\" class=\"btn btn-warning\">W</button>' +
    '<a id=\"m-link\" href=\"#x\">enlace</a>' +
    '<input id=\"m-input\" class=\"form-control\" value=\"x\">' +
    '<input id=\"m-check\" class=\"form-check-input\" type=\"checkbox\" checked>' +
    '<ul class=\"nav nav-tabs\"><li class=\"nav-item\"><a id=\"m-tab\" class=\"nav-link active\" href=\"#y\">T</a></li></ul>' +
    '<span id=\"m-badge\" class=\"badge bg-primary\">B</span>' +
    '<h4 id=\"m-h4\">Titulo</h4>' +
    '<div id=\"m-alert\" class=\"alert alert-warning\">A</div>' +
    '<button id=\"m-def\" class=\"btn btn-default action-button\">D</button>' +
    '<button id=\"m-sec\" class=\"btn btn-secondary\">S</button>' +
    '<button id=\"m-osec\" class=\"btn btn-outline-secondary\">OS</button>';
  document.body.insertBefore(d, document.body.firstChild);
  return true;
})()"
LEER_MUESTRA_JS <- "(function(){
  function cs(id){ return getComputedStyle(document.getElementById(id)); }
  var i = document.getElementById('m-input');   // focused by ENFOCAR_JS, a beat before
  var r = {
    tema: document.documentElement.getAttribute('data-hd-tema'),
    bs_primary: getComputedStyle(document.documentElement).getPropertyValue('--bs-primary').trim().toUpperCase(),
    btn_bg: cs('m-btn').backgroundColor, btn_color: cs('m-btn').color,
    outline_color: cs('m-out').color, outline_border: cs('m-out').borderTopColor,
    warn_bg: cs('m-warn').backgroundColor, warn_color: cs('m-warn').color,
    link: cs('m-link').color,
    focus_border: cs('m-input').borderTopColor, focus_shadow: cs('m-input').boxShadow,
    focused: document.activeElement === i,
    check_bg: cs('m-check').backgroundColor,
    tab_color: cs('m-tab').color,
    badge_bg: cs('m-badge').backgroundColor,
    h4_font: cs('m-h4').fontFamily,
    alert_bg: cs('m-alert').backgroundColor, alert_color: cs('m-alert').color,
    def_bg: cs('m-def').backgroundColor, def_color: cs('m-def').color, def_border: cs('m-def').borderTopColor,
    sec_bg: cs('m-sec').backgroundColor, sec_color: cs('m-sec').color, sec_border: cs('m-sec').borderTopColor,
    osec_color: cs('m-osec').color, osec_border: cs('m-osec').borderTopColor };
  var nav = document.querySelector('nav.navbar'), br = document.querySelector('nav.navbar .navbar-brand'),
      mb = document.querySelector('.hd-menu-btn'), ac = document.getElementById('hd-barra-activa');
  r.nav_bg = nav ? getComputedStyle(nav).backgroundColor : null;
  r.nav_brand = br ? getComputedStyle(br).color : null;
  r.menu_btn = mb ? getComputedStyle(mb).color : null;
  r.activa = ac ? getComputedStyle(ac).color : null;
  return JSON.stringify(r);
})()"
# .form-control animates border and shadow (0.15 s): read a focused input only
# after its transition, never in the same call that focuses it (2026-11-20:
# read at once, it still showed the unfocused border and no shadow).
ENFOCAR_JS <- "document.getElementById('m-input').focus(); true"
# TEMA 2b: the quiet buttons under :hover, forced through CDP (no mouse in a
# headless window), read after Bootstrap's 0.15 s button transition.
leer_hover <- function() {
  invisible(b$DOM$enable()); invisible(b$CSS$enable())
  raiz <- b$DOM$getDocument(depth = 0L)$root$nodeId
  out <- list()
  for (k in c("sec", "osec")) {
    sel <- paste0("#m-", k)
    n <- b$DOM$querySelector(nodeId = raiz, selector = sel)$nodeId
    invisible(b$CSS$forcePseudoState(nodeId = n, forcedPseudoClasses = list("hover")))
    Sys.sleep(0.6)
    v <- jsonlite::fromJSON(ev(sprintf("(function(){ var s = getComputedStyle(document.querySelector('%s'));
      return JSON.stringify({bg: s.backgroundColor, c: s.color}); })()", sel)))
    invisible(b$CSS$forcePseudoState(nodeId = n, forcedPseudoClasses = list()))
    out[[paste0(k, "_hover_bg")]] <- v$bg; out[[paste0(k, "_hover_color")]] <- v$c
  }
  out
}
leer_muestra <- function() {
  ev(ENFOCAR_JS); Sys.sleep(0.6)
  r <- jsonlite::fromJSON(ev(LEER_MUESTRA_JS))
  ev("document.getElementById('m-input').blur(); true")
  c(r, leer_hover())
}
pon_tema <- function(t) ev(sprintf("document.documentElement.setAttribute('data-hd-tema', '%s'); true", t))

captura <- function(nombre) tryCatch({
  s <- b$Page$captureScreenshot(format = "png")
  writeBin(jsonlite::base64_dec(s$data), file.path(SALIDA, paste0(nombre, ".png")))
}, error = function(e) message("  (captura ", nombre, ": ", conditionMessage(e), ")"))
tamano <- function(w, h) invisible(b$Emulation$setDeviceMetricsOverride(width = w, height = h, deviceScaleFactor = 1, mobile = FALSE))

for (quien in quienes) {
  cat(sprintf("\n── %s ──\n", quien))
  if (!is.null(app)) { try(b$close(), silent = TRUE); b <- NULL; stop_bg_app(app); app <- NULL }
  app <- tryCatch(start_bg_app(sprintf('Sys.setenv(ARR_MODO = "normal", ARR_RAIZ = "%s"); setwd("%s"); source("tests/helpers/arranque_app.R")$value',
                                       RAMA, repo), PORT, timeout_s = 300, www = file.path(RAMA, "www")),
                  error = function(e) { message(conditionMessage(e)); NULL })
  if (!ok(sprintf("[%s] the real app starts on dev", quien), !is.null(app))) next
  b <- new_chromote_session()
  tamano(1293L, 632L)
  invisible(b$Emulation$setFocusEmulationEnabled(enabled = TRUE))   # :focus without a window in front
  invisible(b$Page$addScriptToEvaluateOnNewDocument(source = AMPLIA_BUFER_JS))
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE))
  if (!ok(sprintf("[%s] the sign-in form shows", quien),
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
  if (!ok(sprintf("[%s] signed in and loaded", quien), carga_terminada())) next
  Sys.sleep(6)

  recursos <- tryCatch(unlist(jsonlite::fromJSON(ev(ESTADO_RECURSOS_JS))),
                       error = function(x) { message("  ERROR reading resources: ", conditionMessage(x)); character() })
  malos <- recursos[!grepl("^(200|304) ", recursos)]
  if (!ok(sprintf("[%s] every local stylesheet and script answers 200 (%d)", quien, length(recursos)),
          length(recursos) > 5L && !length(malos))) cat(paste0("      | ", malos), sep = "\n")
  ok(sprintf("[%s] the page asks for hd_tema_bootstrap.css, and it answers 200", quien),
     any(grepl("^(200|304) /hd_tema_bootstrap\\.css", recursos)))

  # The sample under Clásico, then under Marca.
  ev(MUESTRA_JS)
  pon_tema("clasico"); Sys.sleep(0.5)
  cl <- leer_muestra()
  for (k in names(ESPERADO_CLASICO))
    ok(sprintf("[%s · clasico] %s = %s (flatly; the bridge does not reach it)", quien, k, cl[[k]]),
       identical(cl[[k]], ESPERADO_CLASICO[[k]]))
  ok(sprintf("[%s · clasico] h4 keeps Lato (%s)", quien, substr(cl$h4_font, 1, 30)), grepl("^\"?Lato", cl$h4_font))
  pon_tema("marca"); Sys.sleep(0.5)
  ma <- leer_muestra()
  ok(sprintf("[%s · marca] the attribute took", quien), identical(ma$tema, "marca"))
  for (k in names(ESPERADO_MARCA))
    ok(sprintf("[%s · marca] %s = %s (expected %s)", quien, k, ma[[k]], ESPERADO_MARCA[[k]]),
       identical(ma[[k]], ESPERADO_MARCA[[k]]))
  ok(sprintf("[%s · marca] focused input: the foco shadow (%s)", quien, ma$focus_shadow),
     isTRUE(ma$focused) && grepl(regmatches(M[["foco"]], regexpr("rgba?\\([^)]*\\)", M[["foco"]])), ma$focus_shadow, fixed = TRUE))
  ok(sprintf("[%s · marca] h4 in Fraunces (%s)", quien, substr(ma$h4_font, 1, 30)), grepl("Fraunces", ma$h4_font))
  ev("document.getElementById('hd-muestra-puente').remove(); true")

  # Screenshots of every tab under Marca, at Luis' window and a wide one.
  # At 1293 px the navbar collapses behind its ☰ (the Diseñador 2's fix to
  # the Clásico test, e95b26b/bef277e): open it to find and click each tab,
  # close it before the screenshot, and wait for the user's tabs to show.
  menu <- function(abrir) tryCatch(ev(sprintf("(function(){
      var t = document.querySelector('.navbar-toggler'); if (!t || t.offsetParent === null) return false;
      var c = document.querySelector(t.getAttribute('data-bs-target') || '.navbar-collapse');
      var abierto = !!c && c.classList.contains('show');
      if (abierto !== %s) t.click(); return true; })()", if (abrir) "true" else "false")), error = function(x) FALSE)
  hay_pestana <- function() {
    menu(TRUE)
    isTRUE(tryCatch(ev(sprintf("[%s].some(function(v){ var a = document.querySelector(\".nav-link[data-value='\" + v + \"']\");
      return !!a && a.offsetParent !== null; })", paste0("'", names(PESTANAS), "'", collapse = ","))), error = function(x) FALSE))
  }
  poll_until(hay_pestana, timeout_s = LIMITE, interval_s = 2)
  Sys.sleep(4)
  vistas <- character()
  for (v in names(PESTANAS)) {
    menu(TRUE); Sys.sleep(1)
    visible <- isTRUE(tryCatch(ev(sprintf("(function(){ var a = document.querySelector(\".nav-link[data-value='%s']\");
      return !!a && a.offsetParent !== null; })()", v)), error = function(x) FALSE))
    if (!visible) next
    ev(sprintf("document.querySelector(\".nav-link[data-value='%s']\").click(); true", v))
    Sys.sleep(1); menu(FALSE)
    Sys.sleep(7)
    ev("window.scrollTo(0, 0); document.activeElement && document.activeElement.blur && document.activeElement.blur(); true")
    pon_tema("marca"); Sys.sleep(1)
    login <- isTRUE(tryCatch(ev("(function(){ var f = document.getElementById('auth-user_id'); return !!f && f.offsetParent !== null; })()"),
                             error = function(x) FALSE))
    if (!ok(sprintf("[%s · %s] no sign-in form on the tab", quien, v), !login)) next
    captura(sprintf("marca_%s_%s_1293", quien, v))
    tamano(1772L, 866L); Sys.sleep(1.5)
    captura(sprintf("marca_%s_%s_1772", quien, v))
    tamano(1293L, 632L); Sys.sleep(1)
    vistas <- c(vistas, v)
  }
  ok(sprintf("[%s] at least one tab shot under Marca (%s)", quien, paste(vistas, collapse = ",")), length(vistas) > 0L)
  ok(sprintf("[%s] still Marca after visiting the tabs (nothing resets the attribute)", quien),
     identical(ev("document.documentElement.getAttribute('data-hd-tema')"), "marca"))
}

cat(sprintf("\nCapturas: %s\n", SALIDA))
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
