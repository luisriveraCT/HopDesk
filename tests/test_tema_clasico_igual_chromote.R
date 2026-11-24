# =============================================================================
# tests/test_tema_clasico_igual_chromote.R
# TEMA: under Clásico, a branch must look like master (colours within ΔE
# TEMA_DE_MAX, 2 by default; everything else exactly). For every user
# (staff and client) and every main tab, at 1293 x 632 (Luis' window), it
# reads the COMPUTED style of every visible element (colours, borders, fonts,
# radius, shadow) in the real app (tests/helpers/arranque_app.R, dev bucket,
# no writes) started from TEMA_BASE (a worktree of master) and from TEMA_RAMA
# (a worktree of the branch's commit), and compares them element by element.
# Text and numbers that change between two runs do not matter: only styles.
# A screenshot of each tab and run goes to TEMA_SALIDA.
#
# It also checks the branch puts data-hd-tema="clasico" on <html> and that
# its tokens load (--hd-primario resolves), and the sign-in page.
#
# Needs the test users (.Renviron.local of the checkout that runs it:
# HOPDESK_TEST_USER/_PASS, HOPDESK_TEST_CLIENTE_USER/_PASS) and Chrome's turn
# (rule 28). Neither root may have its own .Renviron (arranque_app.R stops).
#
#   TEMA_BASE=C:/temp/tema_base_<hash> TEMA_RAMA=C:/temp/tema_<hash> \
#   TEMA_SALIDA=<folder> Rscript --vanilla tests/test_tema_clasico_igual_chromote.R
# TEMA_USUARIOS narrows it ("prueba", "cliente").
# =============================================================================
cat("=== test_tema_clasico_igual_chromote ===\n\n")
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
BASE <- Sys.getenv("TEMA_BASE"); RAMA <- Sys.getenv("TEMA_RAMA", ".")
if (!nzchar(BASE) || !dir.exists(BASE)) { cat("SKIP: TEMA_BASE (worktree de master) no existe.\n"); quit(status = 2L) }
BASE <- normalizePath(BASE, winslash = "/"); RAMA <- normalizePath(RAMA, winslash = "/")
SALIDA <- Sys.getenv("TEMA_SALIDA", file.path(tempdir(), "tema_clasico"))
dir.create(SALIDA, showWarnings = FALSE, recursive = TRUE)
LIMITE <- as.numeric(Sys.getenv("ARR_LIMITE", "300"))
# Each tab is measured only once no visible output is still recalculating
# (2026-11-20: the dev took 30-60 s to draw Agenda de Hoy, master and
# branch alike, and a 7 s wait compared a half-drawn tab). Generous cap.
ESPERA_DATOS <- as.numeric(Sys.getenv("TEMA_ESPERA_DATOS", "180"))
RECALCULANDO_JS <- "Array.prototype.filter.call(document.querySelectorAll('.recalculating'),
  function(e){ return e.offsetParent !== null; }).length"
# Colours (tests/helpers/tema_comparar_estilos.R): the same within ΔE76
# TEMA_DE_MAX (2 by default, Coordinador 2026-11-20; 0 = strict) and alpha
# 0.01. Each colour that moved but passes is a warning, listed at the end.
source("tests/helpers/tema_comparar_estilos.R")
DE_MAX <- as.numeric(Sys.getenv("TEMA_DE_MAX", "2"))
if (is.na(DE_MAX) || DE_MAX < 0) stop("TEMA_DE_MAX must be a number >= 0")
AVISOS <- list()
TIEMPOS <- list()   # sign-in page timings per run (TEMA stage 4, app.R)
# A rise of the sign-in page's median paint over this many ms is a FAIL.
LOGIN_MS_MAX <- as.numeric(Sys.getenv("TEMA_LOGIN_MS_MAX", "100"))
# Views that had nothing to open on (dev data, the user's tabs): not a pass,
# not a fail -- listed at the end so they never go unnoticed (Coordinador).
NO_CUBIERTAS <- character()
no_cubierta <- function(etiqueta, quien, vista, por_que) {
  linea <- sprintf("[%s · %s] %s: %s", etiqueta, quien, vista, por_que)
  cat("      | NOT COVERED", linea, "\n")
  NO_CUBIERTAS <<- c(NO_CUBIERTAS, linea)
}

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
  invisible(isTRUE(result))
}
source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 39030L
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

PESTANAS <- c(PAN = "Panorama", CAL = "Calendario", VEN = "Vencidos", PH = "Agenda de Hoy", BNC = "Bancos",
              IC = "Intercompany", PSV = "Pasivos", FC = "Pronósticos", RPT = "Reporte", GRUPO = "Grupo",
              REQ = "Requisiciones")

# Every visible element: a stable key (its path of tag#id.class:nth-of-type)
# and the style properties a theme can touch.
ESTILOS_JS <- "(function(){
  var props = ['color','background-color','background-image','border-top-color','border-right-color',
    'border-bottom-color','border-left-color','border-top-width','border-top-style','font-family','font-size',
    'font-weight','font-style','border-top-left-radius','border-bottom-right-radius','box-shadow',
    'outline-color','opacity','text-decoration-color','fill','stroke'];
  function paso(e){
    var s = e.tagName.toLowerCase();
    if (e.id && !/[0-9]{4,}/.test(e.id)) s += '#' + e.id;
    var c = (typeof e.className === 'string' ? e.className : (e.getAttribute('class')||'')).trim().split(/\\s+/)
      .filter(function(x){ return x && !/active|show|open|hover|focus|selected|shiny-bound|recalculating|[0-9]{4,}/.test(x); }).sort();
    if (c.length) s += '.' + c.join('.');
    var i = 1, h = e; while ((h = h.previousElementSibling)) if (h.tagName === e.tagName) i++;
    return s + ':' + i;
  }
  function clave(e){ var p = [], n = e; for (var k = 0; n && n.nodeType === 1 && k < 6; k++, n = n.parentElement) p.unshift(paso(n)); return p.join('>'); }
  var out = {};
  document.querySelectorAll('body *').forEach(function(e){
    if (!(e.offsetParent !== null || getComputedStyle(e).position === 'fixed')) return;
    var r = e.getBoundingClientRect(); if (r.width === 0 || r.height === 0) return;
    if (r.bottom < 0 || r.top > window.innerHeight) return;
    var s = getComputedStyle(e), v = {};
    props.forEach(function(p){ v[p] = s.getPropertyValue(p); });
    out[clave(e)] = v;
  });
  var hs = getComputedStyle(document.documentElement);
  out['__html__'] = { tema: document.documentElement.getAttribute('data-hd-tema') || '',
                      primario: hs.getPropertyValue('--hd-primario').trim(), body_bg: getComputedStyle(document.body).backgroundColor };
  return JSON.stringify(out);
})()"

# Every same-origin stylesheet and script the page asks for, with the HTTP
# status the browser got for it ("sin-registro": no Resource Timing entry).
# The buffer holds 250 entries by default: widened before every document.
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

captura <- function(nombre) tryCatch({
  s <- b$Page$captureScreenshot(format = "png")
  writeBin(jsonlite::base64_dec(s$data), file.path(SALIDA, paste0(nombre, ".png")))
}, error = function(e) message("  (captura ", nombre, ": ", conditionMessage(e), ")"))
# The same view under Marca, for a person to look at (not compared): the
# branch only, then back to Clásico before anything else is measured.
captura_marca <- function(etiqueta, nombre) if (identical(etiqueta, "rama")) {
  ev("document.documentElement.setAttribute('data-hd-tema', 'marca'); true"); Sys.sleep(1)
  captura(paste0(nombre, "_marca"))
  ev("document.documentElement.setAttribute('data-hd-tema', 'clasico'); true"); Sys.sleep(1)
}

# One run: start the app from `raiz`, sign in as `quien`, read every tab.
recorrer <- function(raiz, etiqueta, quien) {
  res <- list()
  if (!is.null(app)) { try(b$close(), silent = TRUE); b <<- NULL; stop_bg_app(app); app <<- NULL }
  app <<- tryCatch(start_bg_app(sprintf('Sys.setenv(ARR_MODO = "normal", ARR_RAIZ = "%s"); setwd("%s"); source("tests/helpers/arranque_app.R")$value',
                                        raiz, repo), PORT, timeout_s = 300, www = file.path(raiz, "www")),
                   error = function(e) { message(conditionMessage(e)); NULL })
  if (!ok(sprintf("[%s · %s] the real app starts on dev", etiqueta, quien), !is.null(app))) return(res)
  b <<- new_chromote_session()
  invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
  invisible(b$Page$addScriptToEvaluateOnNewDocument(source = AMPLIA_BUFER_JS))   # before the page loads
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE))
  if (!ok(sprintf("[%s · %s] the sign-in form shows", etiqueta, quien),
          poll_until(function() isTRUE(tryCatch(ev("!!document.getElementById('auth-user_id')"), error = function(e) FALSE)), timeout_s = 60)))
    return(res)
  Sys.sleep(2)
  res[["ENTRAR"]] <- jsonlite::fromJSON(ev(ESTILOS_JS), simplifyVector = FALSE)
  captura(sprintf("%s_%s_ENTRAR", etiqueta, quien))
  # TEMA stage 4 (app.R): the sign-in page must not get slower (Coordinador:
  # report a rise over ~100 ms). Five reloads, the median of first contentful
  # paint and of the moment the user field exists, per run; compared at the end.
  t_fcp <- t_campo <- numeric()
  for (k in 1:5) {
    invisible(b$Page$reload(ignoreCache = FALSE, wait_ = TRUE))
    poll_until(function() isTRUE(tryCatch(ev("!!document.getElementById('auth-user_id')"), error = function(e) FALSE)), timeout_s = 60, interval_s = 0.1)
    m <- tryCatch(ev("(function(){ var p = performance.getEntriesByName('first-contentful-paint')[0];
      var n = performance.getEntriesByType('navigation')[0];
      return [p ? p.startTime : -1, n ? n.domContentLoadedEventEnd : -1]; })()"), error = function(e) list(-1, -1))
    t_fcp <- c(t_fcp, as.numeric(m[[1]])); t_campo <- c(t_campo, as.numeric(m[[2]]))
  }
  TIEMPOS[[length(TIEMPOS) + 1L]] <<- data.frame(corrida = etiqueta, usuario = quien,
                                                fcp = stats::median(t_fcp), dom = stats::median(t_campo))
  cat(sprintf("      | [%s · %s] sign-in page: first contentful paint %.0f ms, DOM ready %.0f ms (median of 5)\n",
              etiqueta, quien, stats::median(t_fcp), stats::median(t_campo)))
  # TEMA stage 4 (app.R): the activation and forgot-password pages live above
  # secure_app; opened with a fake token and the blank form, nothing submitted.
  for (vp in list(c("ACT_INVALIDA", "?invite=tema-token-falso-0000"), c("OLVIDE", "?olvide=1"))) {
    invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d/%s", PORT, vp[[2]]), wait_ = TRUE))
    abre <- poll_until(function() isTRUE(tryCatch(ev("(function(){ var c = document.querySelector('.hd-activate-card');
      return !!c && c.getClientRects().length > 0 && !!c.querySelector('h5'); })()"), error = function(e) FALSE)), timeout_s = 60)
    if (ok(sprintf("[%s · %s] %s: the page opens", etiqueta, quien, vp[[1]]), abre)) {
      Sys.sleep(2)
      res[[vp[[1]]]] <- jsonlite::fromJSON(ev(ESTILOS_JS), simplifyVector = FALSE)
      captura(sprintf("%s_%s_%s", etiqueta, quien, vp[[1]]))
      captura_marca(etiqueta, sprintf("%s_%s_%s", etiqueta, quien, vp[[1]]))
    }
  }
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE))
  poll_until(function() isTRUE(tryCatch(ev("!!document.getElementById('auth-user_id')"), error = function(e) FALSE)), timeout_s = 60)
  Sys.sleep(1)
  captura_marca(etiqueta, sprintf("%s_%s_ENTRAR", etiqueta, quien))
  # TEMA 5b: the loading overlay (www/overlay_init.js) is built hidden on the
  # sign-in page and shown on the click. Shown here as it is (no message yet),
  # its pulse and fades frozen (the icon's opacity would differ by the
  # instant), measured, hidden again before signing in.
  if (isTRUE(tryCatch(ev("(function(){ var o = document.getElementById('hopdesk-loading'); if (!o) return false;
        var c = document.createElement('style'); c.id = 'tema-carga-quieta';
        c.textContent = '#hopdesk-loading, #hopdesk-loading * { animation: none !important; transition: none !important; }';
        document.head.appendChild(c); o.style.display = 'flex'; return true; })()"), error = function(e) FALSE))) {
    Sys.sleep(1.5)
    res[["CARGA"]] <- jsonlite::fromJSON(ev(ESTILOS_JS), simplifyVector = FALSE)
    captura(sprintf("%s_%s_CARGA", etiqueta, quien))
    captura_marca(etiqueta, sprintf("%s_%s_CARGA", etiqueta, quien))
    ev("(function(){ document.getElementById('hopdesk-loading').style.display = 'none';
        var c = document.getElementById('tema-carga-quieta'); if (c) c.remove(); return true; })()")
  } else no_cubierta(etiqueta, quien, "Pantalla de carga", "no #hopdesk-loading on the sign-in page")
  u <- usuarios[[quien]]
  ev(sprintf("(function(){ var u = document.getElementById('auth-user_id'), p = document.getElementById('auth-user_pwd');
     u.value = %s; p.value = %s; $(u).trigger('change'); $(p).trigger('change'); return true; })()",
     jsonlite::toJSON(u[["user"]], auto_unbox = TRUE), jsonlite::toJSON(u[["pass"]], auto_unbox = TRUE)))
  Sys.sleep(0.5)
  ev("document.getElementById('auth-go_auth').click(); true")
  poll_until(carga_terminada, timeout_s = LIMITE, interval_s = 1)
  if (!ok(sprintf("[%s · %s] signed in and loaded", etiqueta, quien), carga_terminada())) return(res)
  Sys.sleep(6)
  # The status the browser itself got (Resource Timing's responseStatus), read
  # synchronously: asking the app again from R timed out while it was busy.
  # A failure to read is a FAIL with its message, never "0 resources".
  recursos <- tryCatch(unlist(jsonlite::fromJSON(ev(ESTADO_RECURSOS_JS))),
                       error = function(x) { message("  ERROR reading resources: ", conditionMessage(x)); character() })
  malos <- recursos[!grepl("^(200|304) ", recursos)]
  if (!ok(sprintf("[%s · %s] every local stylesheet and script answers 200 (%d)", etiqueta, quien, length(recursos)),
          length(recursos) > 5L && !length(malos))) cat(paste0("      | ", malos), sep = "\n")
  if (identical(etiqueta, "rama"))
    ok(sprintf("[rama · %s] the page asks for hd_temas.css and hd_clasico_legado.css", quien),
       any(grepl("hd_temas\\.css", recursos)) && any(grepl("hd_clasico_legado\\.css", recursos)))
  captura(sprintf("%s_%s_DENTRO", etiqueta, quien))
  # At 1293 px the navbar collapses behind its ☰ button (2026-11-20): open
  # it to find and click a tab, close it again before measuring.
  menu <- function(abrir) tryCatch(ev(sprintf("(function(){
      var t = document.querySelector('.navbar-toggler'); if (!t || t.offsetParent === null) return false;
      var c = document.querySelector(t.getAttribute('data-bs-target') || '.navbar-collapse');
      var abierto = !!c && c.classList.contains('show');
      if (abierto !== %s) t.click(); return true; })()", if (abrir) "true" else "false")), error = function(x) FALSE)
  # The server reveals each user's tabs once their data loads (~35 s on a
  # busy machine): wait until one of them shows, menu open.
  hay_pestana <- function() {
    menu(TRUE)
    isTRUE(tryCatch(ev(sprintf("[%s].some(function(v){ var a = document.querySelector(\".nav-link[data-value='\" + v + \"']\");
      return !!a && a.offsetParent !== null; })", paste0("'", names(PESTANAS), "'", collapse = ","))), error = function(x) FALSE))
  }
  poll_until(hay_pestana, timeout_s = LIMITE, interval_s = 2)
  Sys.sleep(4)
  for (v in names(PESTANAS)) {
    menu(TRUE); Sys.sleep(1)   # the collapse animates; offsetParent is null until it opens
    visible <- isTRUE(tryCatch(ev(sprintf("(function(){ var a = document.querySelector(\".nav-link[data-value='%s']\");
      return !!a && a.offsetParent !== null; })()", v)), error = function(x) FALSE))
    if (!visible) next
    ev(sprintf("document.querySelector(\".nav-link[data-value='%s']\").click(); true", v))
    Sys.sleep(1); menu(FALSE)
    Sys.sleep(7)   # data drawn (R takes its time on a busy machine)
    quieta <- poll_until(function() identical(tryCatch(as.integer(ev(RECALCULANDO_JS)), error = function(x) NA_integer_), 0L),
                         timeout_s = ESPERA_DATOS, interval_s = 1)
    if (!ok(sprintf("[%s · %s] %s: nothing still recalculating before measuring (cap %d s)", etiqueta, quien,
                    PESTANAS[[v]], as.integer(ESPERA_DATOS)), quieta)) next
    Sys.sleep(2)   # what the last output drew settles
    ev("window.scrollTo(0, 0); document.activeElement && document.activeElement.blur && document.activeElement.blur(); true")
    Sys.sleep(1)
    res[[v]] <- jsonlite::fromJSON(ev(ESTILOS_JS), simplifyVector = FALSE)
    captura(sprintf("%s_%s_%s", etiqueta, quien, v))
    if (v %in% c("CAL", "PH", "PSV", "VEN")) captura_marca(etiqueta, sprintf("%s_%s_%s", etiqueta, quien, v))
  }
  # Signed in but no tab measured would make "the same" out of nothing.
  if (!ok(sprintf("[%s · %s] at least one tab measured (%s)", etiqueta, quien,
                  paste(setdiff(names(res), "ENTRAR"), collapse = ",")), length(setdiff(names(res), "ENTRAR")) > 0L)) {
    menu(TRUE); Sys.sleep(1)
    cat("      | pestañas:", tryCatch(ev("Array.from(document.querySelectorAll('.nav-link[data-value]')).map(function(a){
      return a.getAttribute('data-value') + (a.offsetParent === null ? '(oculta)' : ''); }).join(',')"), error = function(x) "?"), "\n")
  }
  # The cash-flow preview (www/cashflow_preview.css) is not a tab: it opens
  # from the Calendar's arrow.
  if ("CAL" %in% names(res)) {
    menu(TRUE); Sys.sleep(0.5)
    ev("document.querySelector(\".nav-link[data-value='CAL']\").click(); true")
    Sys.sleep(1); menu(FALSE)
    Sys.sleep(3)
    abierto <- isTRUE(tryCatch(ev("(function(){ var a = document.querySelector('.cf-preview-toggle-btn');
      if (!a || a.offsetParent === null) return false; a.click(); return true; })()"), error = function(x) FALSE))
    if (abierto) {
      Sys.sleep(10)
      ev("window.scrollTo(0, 0); document.activeElement && document.activeElement.blur && document.activeElement.blur(); true")
      Sys.sleep(1)
      res[["FLUJO"]] <- jsonlite::fromJSON(ev(ESTILOS_JS), simplifyVector = FALSE)
      captura(sprintf("%s_%s_FLUJO", etiqueta, quien))
      captura_marca(etiqueta, sprintf("%s_%s_FLUJO", etiqueta, quien))   # the table engine's summary band
    }
  }
  # TEMA stage 4 (ui_components.R): the Calendario's Pagos mode and the open
  # notes panel are styles no tab shows by default. Both must open: a view
  # that silently fails to open would compare nothing. The notes panel and its
  # floating button are position: fixed, whose offsetParent is always null:
  # visibility is getClientRects() there.
  if ("CAL" %in% names(res)) {
    menu(TRUE); Sys.sleep(0.5)
    ev("document.querySelector(\".nav-link[data-value='CAL']\").click(); true")
    Sys.sleep(1); menu(FALSE)
    ev("Shiny.setInputValue('cal_ledger_toggle', 'AP', {priority: 'event'}); true")
    en_pagos <- poll_until(function() isTRUE(tryCatch(ev("(function(){ var c = document.querySelector('.cal-theme-ap');
      return !!c && c.offsetParent !== null && document.querySelectorAll('.cal-theme-ap .cal-tile').length > 0; })()"),
      error = function(x) FALSE)), timeout_s = ESPERA_DATOS, interval_s = 1)
    quieta <- en_pagos && poll_until(function() identical(tryCatch(as.integer(ev(RECALCULANDO_JS)), error = function(x) NA_integer_), 0L),
                                     timeout_s = ESPERA_DATOS, interval_s = 1)
    if (ok(sprintf("[%s · %s] Calendario · Pagos: the AP mode opens and settles", etiqueta, quien), quieta)) {
      Sys.sleep(2)
      ev("window.scrollTo(0, 0); document.activeElement && document.activeElement.blur && document.activeElement.blur(); true")
      Sys.sleep(1)
      res[["CAL_AP"]] <- jsonlite::fromJSON(ev(ESTILOS_JS), simplifyVector = FALSE)
      captura(sprintf("%s_%s_CAL_AP", etiqueta, quien))
      captura_marca(etiqueta, sprintf("%s_%s_CAL_AP", etiqueta, quien))
    }
    abre <- isTRUE(tryCatch(ev("(function(){ var b = Array.from(document.querySelectorAll('.notes-bar-btn'))
      .filter(function(x){ return x.getClientRects().length > 0; })[0]; if (!b) return false; b.click(); return true; })()"),
      error = function(x) FALSE))
    notas <- abre && poll_until(function() isTRUE(tryCatch(ev("(function(){ var w = document.querySelector('#notes_panel .notes-panel-wrapper');
      return !!w && w.getClientRects().length > 0; })()"), error = function(x) FALSE)), timeout_s = 60, interval_s = 1)
    if (ok(sprintf("[%s · %s] Notas: the notes panel opens", etiqueta, quien), notas)) {
      Sys.sleep(2)
      ev("document.activeElement && document.activeElement.blur && document.activeElement.blur(); true")
      Sys.sleep(1)
      res[["NOTAS"]] <- jsonlite::fromJSON(ev(ESTILOS_JS), simplifyVector = FALSE)
      captura(sprintf("%s_%s_NOTAS", etiqueta, quien))
      captura_marca(etiqueta, sprintf("%s_%s_NOTAS", etiqueta, quien))
    }
  }
  # TEMA stage 4 (hdtable.css): the value bubble (.hdt-tip) hangs from body,
  # outside every .hdt, and shows only after a pointer rests on a cell; no tab
  # shows it by default. The first tab with a table cell that has text: a
  # mousemove there, and the bubble must open (hdtable.js waits 350 ms).
  # No candidate cell at all (no such tab, or no row with text: the dev data)
  # is NOT COVERED, listed at the end; a cell with no bubble is a FAIL.
  candidatas <- intersect(c("VEN", "PSV", "PH", "BNC"), names(res))
  burbuja <- FALSE; hubo_celda <- FALSE
  for (v in candidatas) {
    menu(TRUE); Sys.sleep(0.5)
    ev(sprintf("document.querySelector(\".nav-link[data-value='%s']\").click(); true", v))
    Sys.sleep(1); menu(FALSE)
    Sys.sleep(3)
    ev("window.scrollTo(0, 0); document.activeElement && document.activeElement.blur && document.activeElement.blur(); true")
    # A real pointer (CDP), as test_hd_matriz_chromote does: a synthetic
    # mousemove on the cell never opened the bubble (2026-11-21, master too).
    # TEMA-P6 (E2): a DATA cell. hdtable.js opens the bubble only where td._c
    # is set; the currency band row (tr.hdt-grupo) has none, by design.
    p <- tryCatch(ev("(function(){ var td = Array.from(document.querySelectorAll('.hdt-tabla tbody td'))
      .filter(function(x){ return x.getClientRects().length > 0 && x.textContent.trim() !== '' && !x.querySelector('button, input') &&
        x._c != null && !x.closest('tr.hdt-grupo'); })[0];
      if (!td) return null; td.scrollIntoView({block: 'center', inline: 'nearest'});
      var r = td.getBoundingClientRect(); return [r.left + Math.min(12, r.width / 2), r.top + r.height / 2]; })()"),
      error = function(x) NULL)
    if (is.null(p) || length(p) < 2L) next
    hubo_celda <- TRUE
    Sys.sleep(0.5)
    invisible(b$Input$dispatchMouseEvent(type = "mouseMoved", x = p[[1]] + 30, y = p[[2]], button = "none", buttons = 0L))
    invisible(b$Input$dispatchMouseEvent(type = "mouseMoved", x = p[[1]], y = p[[2]], button = "none", buttons = 0L))
    burbuja <- poll_until(function() isTRUE(tryCatch(ev("(function(){ var t = document.querySelector('.hdt-tip');
      return !!t && !t.hidden && t.getClientRects().length > 0; })()"), error = function(x) FALSE)), timeout_s = 10, interval_s = 0.5)
    if (burbuja) break
  }
  if (!hubo_celda) no_cubierta(etiqueta, quien, "Burbuja de valor",
                               sprintf("no table cell with text in %s", if (length(candidatas)) paste(candidatas, collapse = ",") else "any tab (none of VEN, PSV, PH, BNC)"))
  if (hubo_celda && ok(sprintf("[%s · %s] Burbuja de valor: it opens over a table cell (%s)", etiqueta, quien,
                               paste(candidatas, collapse = ",")), burbuja)) {
    res[["BURBUJA"]] <- jsonlite::fromJSON(ev(ESTILOS_JS), simplifyVector = FALSE)
    captura(sprintf("%s_%s_BURBUJA", etiqueta, quien))
    captura_marca(etiqueta, sprintf("%s_%s_BURBUJA", etiqueta, quien))
  }
  # TEMA stage 4 (requisiciones.css): the evidence zone lives in the new-request
  # form, and the folio, thumbnails, type badges and comments in a request's
  # detail; no tab shows them by default. "Requisición abierta" prefers the
  # dev request "PRUEBA TEMA…" (integrador, 2026-11-21) and says which row it
  # opened; with no rows it reports itself not covered, never "the same".
  if ("REQ" %in% names(res)) {
    cerrar_modal <- function() {
      ev("(function(){ var b = document.querySelector('.modal.show .hd-modal-cerrar'); if (b) b.click(); return true; })()")
      poll_until(function() identical(tryCatch(as.integer(ev("document.querySelectorAll('.modal.show').length")),
                                               error = function(x) NA_integer_), 0L), timeout_s = 15, interval_s = 0.5)
      Sys.sleep(1)
    }
    medir_req <- function(clave) {
      Sys.sleep(2)
      ev("document.activeElement && document.activeElement.blur && document.activeElement.blur(); true")
      Sys.sleep(1)
      res[[clave]] <<- jsonlite::fromJSON(ev(ESTILOS_JS), simplifyVector = FALSE)
      captura(sprintf("%s_%s_%s", etiqueta, quien, clave))
      captura_marca(etiqueta, sprintf("%s_%s_%s", etiqueta, quien, clave))
    }
    menu(TRUE); Sys.sleep(0.5)
    ev("document.querySelector(\".nav-link[data-value='REQ']\").click(); true")
    Sys.sleep(1); menu(FALSE)
    Sys.sleep(5)
    nueva <- isTRUE(tryCatch(ev("(function(){ var b = document.getElementById('req-nueva');
      if (!b || b.getClientRects().length === 0) return false; b.click(); return true; })()"), error = function(x) FALSE)) &&
      poll_until(function() isTRUE(tryCatch(ev("(function(){ var z = document.querySelector('.modal.show .hdreq-zona');
        return !!z && z.getClientRects().length > 0; })()"), error = function(x) FALSE)), timeout_s = 30, interval_s = 1)
    if (ok(sprintf("[%s · %s] Nueva requisición: the form opens with its evidence zone", etiqueta, quien), nueva)) {
      medir_req("REQ_NUEVA")
      cerrar_modal()
    }
    fila <- tryCatch(ev("(function(){
      var trs = Array.from(document.querySelectorAll('.hdreq .hdt-tabla tbody tr'))
        .filter(function(r){ return r.getClientRects().length > 0 && r.querySelector('button[data-accion]'); });
      var r = trs.filter(function(x){ return x.textContent.indexOf('PRUEBA TEMA') >= 0; })[0] || trs[0];
      if (!r) return '';
      (r.querySelector('button[data-accion=\"adjuntos\"]') || r.querySelector('button[data-accion=\"detalle\"]')).click();
      return r.textContent.replace(/\\s+/g, ' ').trim().slice(0, 80); })()"), error = function(x) "")
    abierta <- nzchar(fila) && poll_until(function() isTRUE(tryCatch(ev("(function(){ var f = document.querySelector('.modal.show .hdreq-folio');
      return !!f && f.getClientRects().length > 0; })()"), error = function(x) FALSE)), timeout_s = 30, interval_s = 1)
    if (!nzchar(fila)) no_cubierta(etiqueta, quien, "Requisición abierta", "no request rows in dev")
    if (nzchar(fila) && ok(sprintf("[%s · %s] Requisición abierta: the detail opens (%s)", etiqueta, quien, fila), abierta)) {
      vistas_det <- c(evidencia = "REQ_EVID", comentarios = "REQ_COM")
      for (pest in names(vistas_det)) {
        ev(sprintf("(function(){ var a = document.querySelector(\".modal.show .nav-link[data-value='%s']\"); if (a) a.click(); return true; })()", pest))
        Sys.sleep(3)
        medir_req(vistas_det[[pest]])
      }
      cerrar_modal()
    }
  }
  # TEMA stage 4 (settings_policies.R): the payment policies' calendar
  # simulator lives in Configuración (the gear, btn_settings) → Políticas de
  # Pago → "Nueva política"; nothing is written until Guardar, which is never
  # pressed. A click on a day shows the selected and projected glows. A user
  # with no gear reports itself not covered, never "the same".
  if ("CAL" %in% names(res)) {
    menu(TRUE); Sys.sleep(0.5)
    ev("document.querySelector(\".nav-link[data-value='CAL']\").click(); true")
    Sys.sleep(1); menu(FALSE); Sys.sleep(2)
    clic <- function(js) poll_until(function() isTRUE(tryCatch(ev(js), error = function(x) FALSE)), timeout_s = 60, interval_s = 1)
    engrane <- isTRUE(tryCatch(ev("(function(){ var b = document.getElementById('btn_settings');
      return !!b && b.getClientRects().length > 0; })()"), error = function(x) FALSE))
    if (!engrane) no_cubierta(etiqueta, quien, "Ajustes · Políticas", "no settings button (btn_settings) for this user")
    if (engrane) {
      pol <- clic("(function(){ var b = document.getElementById('btn_settings'); b.click(); return true; })()") &&
        clic("(function(){ var b = document.getElementById('stg_btn_policies');
          if (!b || b.getClientRects().length === 0) return false; b.click(); return true; })()") &&
        clic("(function(){ var b = document.getElementById('pol_btn_new');
          if (!b || b.getClientRects().length === 0) return false; b.click(); return true; })()") &&
        clic("(function(){ var d = Array.from(document.querySelectorAll('#pol_preview_ui span[onclick*=\"pol_cal_click\"]'))
          .filter(function(x){ return x.getClientRects().length > 0; })[3]; if (!d) return false; d.click(); return true; })()") &&
        poll_until(function() identical(tryCatch(as.integer(ev(RECALCULANDO_JS)), error = function(x) NA_integer_), 0L),
                   timeout_s = ESPERA_DATOS, interval_s = 1)
      if (ok(sprintf("[%s · %s] Ajustes · Políticas: the simulator opens and shows a selected day", etiqueta, quien), pol)) {
        Sys.sleep(3)
        ev("document.activeElement && document.activeElement.blur && document.activeElement.blur(); true")
        Sys.sleep(1)
        res[["POL"]] <- jsonlite::fromJSON(ev(ESTILOS_JS), simplifyVector = FALSE)
        captura(sprintf("%s_%s_POL", etiqueta, quien))
        captura_marca(etiqueta, sprintf("%s_%s_POL", etiqueta, quien))
      }
      # TEMA 5b (settings_hub.R): the IC partners' labels (ic_lbl_*) in the
      # same Configuración modal → Intercompany. They live in an accordion
      # that opens closed (one panel per company): the first panel is opened.
      # No company in the accordion is NOT COVERED. Nothing is saved.
      panel <- clic("(function(){ var b = document.getElementById('stg_btn_interco');
          if (!b || b.getClientRects().length === 0) return false; b.click(); return true; })()") &&
        poll_until(function() isTRUE(tryCatch(ev("(function(){ var a = document.getElementById('ic_empresa_accordion');
          return !!a && a.getClientRects().length > 0; })()"), error = function(x) FALSE)), timeout_s = 60, interval_s = 1)
      hay_empresa <- panel && isTRUE(tryCatch(ev("(function(){ var b = document.querySelector('#ic_empresa_accordion .accordion-button');
          if (!b) return false; if (b.classList.contains('collapsed')) b.click(); return true; })()"), error = function(x) FALSE))
      if (panel && !hay_empresa) no_cubierta(etiqueta, quien, "Ajustes · Intercompany", "no company in the IC accordion")
      ic <- hay_empresa &&
        poll_until(function() isTRUE(tryCatch(ev("(function(){ return Array.from(document.querySelectorAll('[id^=\"ic_lbl_\"]'))
          .some(function(x){ return x.getClientRects().length > 0; }); })()"), error = function(x) FALSE)), timeout_s = 30, interval_s = 1) &&
        poll_until(function() identical(tryCatch(as.integer(ev(RECALCULANDO_JS)), error = function(x) NA_integer_), 0L),
                   timeout_s = ESPERA_DATOS, interval_s = 1)
      if ((!panel || hay_empresa) &&
          ok(sprintf("[%s · %s] Ajustes · Intercompany: the first company opens and its partners' labels show", etiqueta, quien), ic)) {
        Sys.sleep(2)
        ev("document.activeElement && document.activeElement.blur && document.activeElement.blur(); true")
        Sys.sleep(1)
        res[["AJ_IC"]] <- jsonlite::fromJSON(ev(ESTILOS_JS), simplifyVector = FALSE)
        captura(sprintf("%s_%s_AJ_IC", etiqueta, quien))
        captura_marca(etiqueta, sprintf("%s_%s_AJ_IC", etiqueta, quien))
      }
      ev("(function(){ var b = document.querySelector('.modal.show .hd-modal-cerrar'); if (b) b.click(); return true; })()")
      poll_until(function() identical(tryCatch(as.integer(ev("document.querySelectorAll('.modal.show').length")),
                                               error = function(x) NA_integer_), 0L), timeout_s = 15, interval_s = 0.5)
    }
  }
  # TEMA 5b (empresas_module.R): Grupo → Empresas is an item of the Grupo
  # dropdown, hidden until the menu opens, so the tab loop never sees it.
  # A user with no Grupo menu reports it NOT COVERED.
  grupo <- isTRUE(tryCatch(ev("(function(){ return !!document.querySelector(\".nav-link[data-value='GRUPO'], .dropdown-toggle[data-value='GRUPO']\"); })()"),
                           error = function(x) FALSE))
  if (!grupo) no_cubierta(etiqueta, quien, "Grupo · Empresas", "no Grupo menu for this user")
  if (grupo) {
    menu(TRUE); Sys.sleep(0.5)
    abre <- isTRUE(tryCatch(ev("(function(){
        var t = document.querySelector(\".nav-link[data-value='GRUPO'], .dropdown-toggle[data-value='GRUPO']\");
        if (t && !(t.getAttribute('aria-expanded') === 'true')) t.click();
        return true; })()"), error = function(x) FALSE)) &&
      poll_until(function() isTRUE(tryCatch(ev("(function(){ var a = document.querySelector(\"[data-value='EMP']\");
        if (!a || a.getClientRects().length === 0) return false; a.click(); return true; })()"), error = function(x) FALSE)),
        timeout_s = 20, interval_s = 0.5) &&
      poll_until(function() identical(tryCatch(as.integer(ev(RECALCULANDO_JS)), error = function(x) NA_integer_), 0L),
                 timeout_s = ESPERA_DATOS, interval_s = 1)
    menu(FALSE)
    if (ok(sprintf("[%s · %s] Grupo · Empresas: the page opens from the Grupo menu", etiqueta, quien), abre)) {
      Sys.sleep(3)
      ev("window.scrollTo(0, 0); document.activeElement && document.activeElement.blur && document.activeElement.blur(); true")
      Sys.sleep(1)
      res[["EMP"]] <- jsonlite::fromJSON(ev(ESTILOS_JS), simplifyVector = FALSE)
      captura(sprintf("%s_%s_EMP", etiqueta, quien))
      captura_marca(etiqueta, sprintf("%s_%s_EMP", etiqueta, quien))
    }
  }
  res
}

for (quien in quienes) {
  cat(sprintf("\n── %s ──\n", quien))
  base <- recorrer(BASE, "master", quien)
  rama <- recorrer(RAMA, "rama", quien)
  saveRDS(list(base = base, rama = rama), file.path(SALIDA, sprintf("estilos_%s.rds", quien)))
  if ("PAN" %in% names(rama) || length(rama) > 1L) {
    # The first view INSIDE the app: the access pages (sign-in, activation,
    # forgot) carry only the acceso-* sheet, no theme attribute (app.R).
    h <- rama[[setdiff(names(rama), c("ENTRAR", "CARGA", "ACT_INVALIDA", "OLVIDE"))[1]]][["__html__"]]
    ok(sprintf("[rama · %s] <html data-hd-tema=\"clasico\"> and the tokens load (--hd-primario = %s)", quien, h$primario %||% ""),
       identical(h$tema, "clasico") && toupper(h$primario %||% "") == "#0A58CA")
  }
  for (v in intersect(names(base), names(rama))) {
    eb <- base[[v]]; er <- rama[[v]]
    eb[["__html__"]] <- NULL; er[["__html__"]] <- NULL
    # Shiny puts .shiny-busy on <html> while it works: one side measured busy
    # and the other not would share no key at all (the overlay was skipped so).
    sin_busy <- function(x) { names(x) <- gsub("\\.shiny-busy", "", names(x)); x }
    eb <- sin_busy(eb); er <- sin_busy(er)
    comunes <- intersect(names(eb), names(er))
    difs <- character()
    nombre <- switch(v, ENTRAR = "sign-in page", CARGA = "Pantalla de carga", FLUJO = "Flujo (vista previa)",
                     ACT_INVALIDA = "Activación (enlace inválido)", OLVIDE = "Olvidé mi contraseña",
                     CAL_AP = "Calendario · Pagos", NOTAS = "Notas abiertas", BURBUJA = "Burbuja de valor",
                     REQ_NUEVA = "Nueva requisición", REQ_EVID = "Requisición abierta · Evidencia",
                     REQ_COM = "Requisición abierta · Comentarios", POL = "Ajustes · Políticas (simulador)", AJ_IC = "Ajustes · Intercompany", EMP = "Grupo · Empresas",
                     PESTANAS[[v]])
    for (k in comunes) for (p in names(eb[[k]])) {
      r <- tema_comparar_valor(p, eb[[k]][[p]], er[[k]][[p]], de_max = DE_MAX, alfa_max = 0.01)
      if (r$estado == "falla")
        difs <- c(difs, sprintf("%s { %s: %s -> %s }", k, p, eb[[k]][[p]], er[[k]][[p]]))
      else if (r$estado == "aviso")
        AVISOS[[length(AVISOS) + 1L]] <- data.frame(pestana = nombre, usuario = quien, elemento = k, propiedad = p,
                                                     master = eb[[k]][[p]], rama = er[[k]][[p]], dE = round(r$de, 2))
    }
    # Never "the same" out of two empty or signed-out screens (Coordinador,
    # 2026-11-20): past the sign-in page, a visible login form fails, and
    # every comparison needs more than 20 elements measured.
    login <- function(e) any(grepl("#auth-user_id", names(e), fixed = TRUE))
    # The two access pages above secure_app are small by design (the
    # invalid-link card has 20 elements) and were only measured once their
    # card and title showed: more than 10 there; more than 20 everywhere else.
    min_elementos <- if (v %in% c("ACT_INVALIDA", "OLVIDE")) 10L else 20L
    # The loading overlay is measured over the sign-in page, form included.
    if (!v %in% c("ENTRAR", "CARGA"))
      ok(sprintf("[%s] %s: no sign-in form in master or branch", quien, nombre), !login(eb) && !login(er))
    if (!ok(sprintf("[%s] %s: %d elements compared, same computed style within ΔE %g (only in master %d, only in branch %d)",
                    quien, nombre, length(comunes), DE_MAX, length(setdiff(names(eb), comunes)), length(setdiff(names(er), comunes))),
            length(difs) == 0L && length(comunes) > min_elementos))
      cat(paste0("      | ", utils::head(difs, 12)), sep = "\n")
  }
  faltan_v <- setdiff(names(base), names(rama))
  ok(sprintf("[%s] the branch shows every tab master shows (%s)", quien, paste(names(base), collapse = ",")), !length(faltan_v))
}

if (length(AVISOS)) {
  av <- do.call(rbind, AVISOS)
  utils::write.csv(av, file.path(SALIDA, "avisos_color.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  cat(sprintf("\nAvisos de color (Clásico se movió, dentro de ΔE %g):\n", DE_MAX))
  for (i in seq_len(nrow(av)))
    cat(sprintf("  AVISO [%s · %s] %s { %s: %s -> %s } ΔE %.2f\n", av$pestana[i], av$usuario[i], av$elemento[i],
                av$propiedad[i], av$master[i], av$rama[i], av$dE[i]))
}
cat(sprintf("\nAvisos de color: %d (lista en %s)\n", length(AVISOS), file.path(SALIDA, "avisos_color.csv")))
cat(sprintf("\nCapturas y estilos: %s\n", SALIDA))
if (length(TIEMPOS)) {
  tt <- do.call(rbind, TIEMPOS)
  for (u in unique(tt$usuario)) {
    m <- tt[tt$usuario == u & tt$corrida == "master", ]; r <- tt[tt$usuario == u & tt$corrida == "rama", ]
    if (nrow(m) && nrow(r) && m$fcp > 0 && r$fcp > 0)
      ok(sprintf("[%s] sign-in page paint: master %.0f ms, branch %.0f ms (rise %+.0f ms, cap %g)", u, m$fcp, r$fcp,
                 r$fcp - m$fcp, LOGIN_MS_MAX), r$fcp - m$fcp <= LOGIN_MS_MAX)
  }
}
cat(sprintf("\nVistas NO CUBIERTAS: %d%s\n", length(NO_CUBIERTAS),
            if (length(NO_CUBIERTAS)) " (nothing measured there: say so in the report)" else ""))
for (x in NO_CUBIERTAS) cat("  NOT COVERED", x, "\n")
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
