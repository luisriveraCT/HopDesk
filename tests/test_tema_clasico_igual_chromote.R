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
              IC = "Intercompany", PSV = "Pasivos", FC = "Pronósticos", RPT = "Reporte", GRUPO = "Grupo")

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
    if (v %in% c("CAL", "PH")) captura_marca(etiqueta, sprintf("%s_%s_%s", etiqueta, quien, v))
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
  res
}

for (quien in quienes) {
  cat(sprintf("\n── %s ──\n", quien))
  base <- recorrer(BASE, "master", quien)
  rama <- recorrer(RAMA, "rama", quien)
  saveRDS(list(base = base, rama = rama), file.path(SALIDA, sprintf("estilos_%s.rds", quien)))
  if ("PAN" %in% names(rama) || length(rama) > 1L) {
    h <- rama[[setdiff(names(rama), "ENTRAR")[1]]][["__html__"]]
    ok(sprintf("[rama · %s] <html data-hd-tema=\"clasico\"> and the tokens load (--hd-primario = %s)", quien, h$primario %||% ""),
       identical(h$tema, "clasico") && toupper(h$primario %||% "") == "#0A58CA")
  }
  for (v in intersect(names(base), names(rama))) {
    eb <- base[[v]]; er <- rama[[v]]
    eb[["__html__"]] <- NULL; er[["__html__"]] <- NULL
    comunes <- intersect(names(eb), names(er))
    difs <- character()
    nombre <- switch(v, ENTRAR = "sign-in page", FLUJO = "Flujo (vista previa)",
                     CAL_AP = "Calendario · Pagos", NOTAS = "Notas abiertas", PESTANAS[[v]])
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
    if (v != "ENTRAR")
      ok(sprintf("[%s] %s: no sign-in form in master or branch", quien, nombre), !login(eb) && !login(er))
    if (!ok(sprintf("[%s] %s: %d elements compared, same computed style within ΔE %g (only in master %d, only in branch %d)",
                    quien, nombre, length(comunes), DE_MAX, length(setdiff(names(eb), comunes)), length(setdiff(names(er), comunes))),
            length(difs) == 0L && length(comunes) > 20L))
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
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
