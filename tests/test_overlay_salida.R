# =============================================================================
# tests/test_overlay_salida.R
# T88-1 / T88-1b: the loading overlay covers from the click on "Entrar" until
# the app is really done, and never stays stuck.
#   1. window.hldSalida.decidir() (www/overlay_init.js §0) is pure: state + a
#      fake clock in, 'esperar' | 'quitar' | 'tope' out.
#   2. The sign-in mark and the first-frame cover (§0b, §0c), each scenario
#      in a fresh node context with a fake document, sessionStorage (also one
#      that throws, and none) and setTimeout.
#   3. The wiring, on the text.
# Run in node when node is installed. Chrome: the real moment and "no frame
# shows the app half-built" are measured with the real app (T88-1b report).
# Run from project root: Rscript --vanilla tests/test_overlay_salida.R
# =============================================================================
cat("=== test_overlay_salida ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
  invisible(isTRUE(result))
}
ruta_js <- normalizePath("www/overlay_init.js", winslash = "/")
js_txt <- paste(readLines(ruta_js, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
node <- Sys.which("node")
correr_node <- function(lineas) {
  js <- tempfile(fileext = ".js")
  writeLines(c(
    "var vm = require('vm'), fs = require('fs');",
    sprintf("var CODIGO = fs.readFileSync(%s, 'utf8');", jsonlite::toJSON(ruta_js, auto_unbox = TRUE)),
    # A fresh page: a fake document (still loading, so the DOM-ready part
    # never runs here), a sessionStorage (a Map, one that throws, or none),
    # a clock and a setTimeout that only records.
    "function pagina(o) {",
    "  var clases = {}, head = [], tiempos = [], ss;",
    "  if (o.ss === 'falla') ss = { getItem: function () { throw new Error('bloqueado'); },",
    "    setItem: function () { throw new Error('bloqueado'); }, removeItem: function () { throw new Error('bloqueado'); } };",
    "  else if (o.ss === 'ninguno') ss = undefined;",
    "  else { var m = new Map(Object.entries(o.ss || {})); ss = { getItem: function (k) { return m.has(k) ? m.get(k) : null; },",
    "    setItem: function (k, v) { m.set(k, String(v)); }, removeItem: function (k) { m.delete(k); }, _m: m }; }",
    "  var doc = { readyState: 'loading', addEventListener: function () {},",
    "    currentScript: { getAttribute: function (a) { return a === 'data-pagina' ? (o.app ? 'app' : null) : null; } },",
    "    documentElement: { classList: { add: function (c) { clases[c] = 1; }, remove: function (c) { delete clases[c]; } } },",
    "    head: { appendChild: function (e) { head.push(e); } },",
    "    createElement: function () { return {}; }, getElementById: function () { return null; } };",
    "  var win = { sessionStorage: ss };",
    "  var sb = { window: win, document: doc, JSON: JSON, Math: Math, Number: Number, console: { error: function () {} },",
    "    Date: { now: function () { return o.ahora; } },",
    "    setTimeout: function (f, ms) { tiempos.push(ms); }, clearInterval: function () {} };",
    "  vm.runInNewContext(CODIGO, sb);",
    "  for (var i = 1; i < (o.veces || 1); i++) vm.runInNewContext(CODIGO, sb);",
    "  return { win: win, clases: clases, head: head, tiempos: tiempos, ss: ss };",
    "}",
    "function marca(t, pct) { return { 'hld-entrando': JSON.stringify({ t: t, pct: pct }) }; }",
    lineas), js)
  jsonlite::fromJSON(paste(system2(node, js, stdout = TRUE), collapse = ""), simplifyVector = FALSE)
}

if (!nzchar(node)) {
  cat("SKIP (1, 2): node no está instalado\n")
} else {
  # ── 1. The decision, with a fake clock ─────────────────────────────────────
  casos <- list(
    sin_100        = list(s = list(temaAt = 1000, busy = FALSE, idleAt = 1000, pestanaLista = TRUE), t = 99999),
    sin_tema       = list(s = list(pct100At = 10000, busy = FALSE, idleAt = 10000, pestanaLista = TRUE), t = 20000),
    sin_tema_tope  = list(s = list(pct100At = 10000, busy = FALSE, idleAt = 10000, pestanaLista = TRUE), t = 40000),
    ocupada        = list(s = list(pct100At = 10000, temaAt = 8000, busy = TRUE, idleAt = 9000, pestanaLista = TRUE), t = 20000),
    pestana_vacia  = list(s = list(pct100At = 10000, temaAt = 8000, busy = FALSE, idleAt = 10000, pestanaLista = FALSE), t = 20000),
    silencio_corto = list(s = list(pct100At = 10000, temaAt = 8000, busy = FALSE, idleAt = 12000, pestanaLista = TRUE), t = 13499),
    silencio_ok    = list(s = list(pct100At = 10000, temaAt = 8000, busy = FALSE, idleAt = 12000, pestanaLista = TRUE), t = 13500),
    idle_antes     = list(s = list(pct100At = 10000, temaAt = 8000, busy = FALSE, idleAt = 5000, pestanaLista = TRUE), t = 11000),
    idle_antes_ok  = list(s = list(pct100At = 10000, temaAt = 8000, busy = FALSE, idleAt = 5000, pestanaLista = TRUE), t = 11500),
    ocupada_tope   = list(s = list(pct100At = 10000, temaAt = 8000, busy = TRUE, idleAt = 9000, pestanaLista = FALSE), t = 40000),
    sin_100_carga  = list(s = list(cargaAt = 1000, busy = TRUE), t = 120999),
    sin_100_tope   = list(s = list(cargaAt = 1000, busy = TRUE), t = 121000),
    nada           = list(s = NULL, t = 5000))
  r <- correr_node(c(
    sprintf("var casos = %s;", jsonlite::toJSON(casos, auto_unbox = TRUE, null = "null")),
    "var w = pagina({ app: false, ahora: 0 }).win, r = {};",
    "Object.keys(casos).forEach(function (k) { r[k] = w.hldSalida.decidir(casos[k].s, casos[k].t); });",
    # Production's 16:39 sign-in (T88 paso 0), time from Phase 1: 100 % at
    # 15 s, the theme at 8 s, busy until 28 s, the open tab drawn at 28 s.
    "var s = { cargaAt: 1, pct100At: 15000, temaAt: 8000, busy: true, idleAt: 0, pestanaLista: false }, sale = null;",
    "for (var t = 15000; t <= 60000 && !sale; t += 250) {",
    "  if (t >= 28000 && s.busy) { s.busy = false; s.idleAt = 28000; s.pestanaLista = true; }",
    "  var v = w.hldSalida.decidir(s, t); if (v !== 'esperar') sale = { t: t, v: v };",
    "}",
    "r.prod = sale; r.silencio = w.hldSalida.SILENCIO; r.tope = w.hldSalida.TOPE; r.tope_carga = w.hldSalida.TOPE_CARGA;",
    "console.log(JSON.stringify(r));"))
  ok("1a  before the 100 %, it waits (whatever else is ready)", identical(r$sin_100, "esperar"))
  ok("1b  without the theme, it waits", identical(r$sin_tema, "esperar"))
  ok("1c  ... until the cap: 30 s after the 100 %, it goes anyway", identical(r$sin_tema_tope, "tope"))
  ok("1d  while Shiny is busy, it waits", identical(r$ocupada, "esperar"))
  ok("1e  with the open tab empty or recalculating, it waits", identical(r$pestana_vacia, "esperar"))
  ok("1f  idle for 1499 ms: still waits", identical(r$silencio_corto, "esperar"))
  ok("1g  idle for 1500 ms, theme in, tab drawn: it goes", identical(r$silencio_ok, "quitar"))
  ok("1h  idle since before the 100 %: the silence counts from the 100 %", identical(r$idle_antes, "esperar") &&
       identical(r$idle_antes_ok, "quitar"))
  ok("1i  busy and empty at the cap: it goes anyway", identical(r$ocupada_tope, "tope"))
  ok("1j  no 100 % ever (a session that breaks): it goes 120 s after the page loaded, not before",
     identical(r$sin_100_carga, "esperar") && identical(r$sin_100_tope, "tope"))
  ok("1k  no state at all: it waits", identical(r$nada, "esperar"))
  ok("1l  silence 1500 ms, cap 30000 ms after the 100 %, 120000 ms after the load",
     identical(r$silencio, 1500L) && identical(r$tope, 30000L) && identical(r$tope_carga, 120000L))
  ok(sprintf("1m  production's 16:39 sign-in: it goes at %.2f s from Phase 1, never before the tab is drawn (28 s)",
             r$prod$t / 1000), identical(r$prod$v, "quitar") && r$prod$t == 29500)

  # ── 2. The mark and the first-frame cover ──────────────────────────────────
  r <- correr_node(c(
    "var A = 1000000, r = {};",
    # the app page a sign-in brought (mark 6 s old)
    "var p = pagina({ app: true, ahora: A, ss: marca(A - 6000, 14) });",
    "r.cubre = !!p.clases['hld-entrando']; r.css = p.head.length === 1 && /visibility:hidden/.test(p.head[0].textContent) &&",
    "  /#hopdesk-loading/.test(p.head[0].textContent) && !/#[0-9a-fA-F]{3,6}\\b/.test(p.head[0].textContent);",
    "r.tope_armado = p.tiempos.indexOf(120000) >= 0; r.marca_sigue = p.ss._m.has('hld-entrando');",
    "p.win._hldSoltar('x'); r.soltar_quita = !p.clases['hld-entrando'] && !p.ss._m.has('hld-entrando');",
    "var dos = true; try { p.win._hldSoltar('x'); } catch (e) { dos = false; } r.soltar_dos = dos;",
    # a cold start: the app page 57 s after the click still counts; 3 min does not
    "r.frio = !!pagina({ app: true, ahora: A, ss: marca(A - 57000, 9) }).clases['hld-entrando'];",
    "var v = pagina({ app: true, ahora: A, ss: marca(A - 181000, 9) }); r.vieja = !v.clases['hld-entrando'] && !v.ss._m.has('hld-entrando');",
    "var f = pagina({ app: true, ahora: A, ss: marca(A + 5000, 9) }); r.futura = !f.clases['hld-entrando'];",
    "var b = pagina({ app: true, ahora: A, ss: { 'hld-entrando': '{roto' } }); r.rota = !b.clases['hld-entrando'] && !b.ss._m.has('hld-entrando');",
    # the sign-in page (and the login again after signing out): drops any mark, never covers
    "var l = pagina({ app: false, ahora: A, ss: marca(A - 2000, 9) }); r.login = !l.clases['hld-entrando'] && !l.ss._m.has('hld-entrando') && l.tiempos.indexOf(120000) < 0;",
    # without a mark the app page is not covered at first (showOverlay covers), but the cap is armed
    "var n = pagina({ app: true, ahora: A, ss: {} }); r.sin_marca = !n.clases['hld-entrando'] && n.tiempos.indexOf(120000) >= 0;",
    # sessionStorage that throws, or none at all: nothing breaks, nothing covers
    "var ok1 = true, ok2 = true, e1, e2; try { e1 = pagina({ app: true, ahora: A, ss: 'falla' }); e1.win.hldMarca.poner(9, A); e1.win._hldSoltar('x'); } catch (e) { ok1 = false; }",
    "try { e2 = pagina({ app: true, ahora: A, ss: 'ninguno' }); e2.win.hldMarca.poner(9, A); e2.win._hldSoltar('x'); } catch (e) { ok2 = false; }",
    "r.ss_falla = ok1 && !e1.clases['hld-entrando']; r.ss_ninguno = ok2 && !e2.clases['hld-entrando'];",
    # loaded twice on one page: the second load does nothing (handlers once)
    "var d = pagina({ app: true, ahora: A, ss: marca(A - 1000, 9), veces: 2 }); r.una_vez = d.head.length === 1 && d.tiempos.filter(function (x) { return x === 120000; }).length === 1;",
    "console.log(JSON.stringify(r));"))
  ok("2a  the app page a sign-in brought: covered from the first frame (html.hld-entrando)", isTRUE(r$cubre))
  ok("2b  the cover hides everything but the overlay, painted with acceso-* tokens only (no hex)", isTRUE(r$css))
  ok("2c  the app page arms the 120 s safety cap, whatever else happens", isTRUE(r$tope_armado))
  ok("2d  the mark stays while covering, and _hldSoltar() uncovers and drops it", isTRUE(r$marca_sigue) && isTRUE(r$soltar_quita))
  ok("2e  _hldSoltar() twice is harmless", isTRUE(r$soltar_dos))
  ok("2f  a cold start (57 s from click to app page) still covers", isTRUE(r$frio))
  ok("2g  a mark older than 3 min, from the future, or unreadable: no cover, and it is dropped",
     isTRUE(r$vieja) && isTRUE(r$futura) && isTRUE(r$rota))
  ok("2h  the sign-in page (also the login after signing out) drops the mark and never covers", isTRUE(r$login))
  ok("2i  the app page without a mark: not covered at first (showOverlay does it), cap armed", isTRUE(r$sin_marca))
  ok("2j  sessionStorage that throws: nothing breaks, nothing is covered", isTRUE(r$ss_falla))
  ok("2k  no sessionStorage at all: nothing breaks, nothing is covered", isTRUE(r$ss_ninguno))
  ok("2l  loaded twice on one page: the second load does nothing (no second cover, no second cap)", isTRUE(r$una_vez))
}

# ── 3. The wiring (text) ───────────────────────────────────────────────────────
ok("3a  no fixed 30 s timer is left: 30000 appears only as the cap",
   lengths(regmatches(js_txt, gregexpr("\\b30000\\b", js_txt))) == 1L && grepl("TOPE: 30000", js_txt, fixed = TRUE))
ok("3b  the 100 % starts the watcher (and stamps the time once)",
   grepl("if \\(!H\\.salida\\.pct100At\\) H\\.salida\\.pct100At = Date\\.now\\(\\);\\s*_vigilarSalida\\(\\);", js_txt))
ok("3c  the theme is heard on shiny:message, never with a second hdTema handler (it would replace the theme's own)",
   grepl("on('shiny:message'", js_txt, fixed = TRUE) && grepl("c.hdTema", js_txt, fixed = TRUE) &&
     !grepl("addCustomMessageHandler\\(\\s*['\"]hdTema", js_txt))
ok("3d  busy and idle come from Shiny's own events",
   grepl("on('shiny:busy'", js_txt, fixed = TRUE) && grepl("on('shiny:idle'", js_txt, fixed = TRUE))
ok("3e  the open tab: the outermost active pane, nothing recalculating, something drawn",
   grepl("closest('.tab-pane')", js_txt, fixed = TRUE) && grepl(".shiny-output-recalculating", js_txt, fixed = TRUE) &&
     grepl("querySelectorAll('.shiny-bound-output')", js_txt, fixed = TRUE))
# Chrome (T88-1b): Panorama's hidden "Publicar" outputs keep .recalculating
# forever; counted, the overlay could only ever leave by its cap.
# ... and not by offsetParent: Panorama's outputs are display:contents and have
# none while showing (the overlay then never saw a drawn output).
ok("3e2 a hidden output (display:none up to the tab) does not count, either way; display:contents does",
   grepl("for (var r = 0; r < rec.length; r++) if (!_oculto(rec[r], pane)) return false;", js_txt, fixed = TRUE) &&
     grepl("if (!_oculto(outs[j], pane) && (outs[j].children.length", js_txt, fixed = TRUE) &&
     grepl("if (getComputedStyle(h).display === 'none') return true;", js_txt, fixed = TRUE) &&
     !grepl(".offsetParent", js_txt, fixed = TRUE))
ok("3f  every sessionStorage touch is inside a try (3 of them)",
   lengths(regmatches(js_txt, gregexpr("try \\{ (v = JSON\\.parse\\()?window\\.sessionStorage\\.", js_txt))) == 3L &&
     lengths(regmatches(js_txt, gregexpr("sessionStorage\\.", js_txt))) == 3L)
ok("3g  whatever throws in the DOM-ready part or in the watcher uncovers the page",
   grepl("_iniciar\\(\\);\\s*\\} catch \\(e\\) \\{[^}]*_hldSoltar\\('error'\\)", js_txt) &&
     grepl("_quitar\\(v\\);\\s*\\} catch \\(e\\) \\{[^}]*_hldSoltar\\('error'\\)", js_txt))
ok("3h  the click sets the mark; a failed sign-in drops it",
   grepl("H.ov.style.display = 'flex';\\s*window\\.hldMarca\\.poner\\(8, Date\\.now\\(\\)\\);", js_txt) &&
     grepl("_hideForRetry() {", js_txt, fixed = TRUE) && grepl("window.hldMarca.borrar();   /* a failed sign-in", js_txt, fixed = TRUE))
ok("3i  once gone, showOverlay and loadingProgress never bring the overlay back on that page",
   lengths(regmatches(js_txt, gregexpr("if \\(H\\.salida\\.motivo\\) return;", js_txt))) == 2L)
ok("3j  the app page watches from the start (the 120 s cap even without the 100 %)",
   grepl("if (_esApp) _vigilarSalida();", js_txt, fixed = TRUE))
# Shiny 1.11 throws "handler must be a function that takes one argument" at
# registration: on the app page that uncovered it at once (Chrome, T88-1b),
# and on the sign-in page it had left loadingProgress and calFadeNow unset.
manejadores <- regmatches(js_txt, gregexpr("addCustomMessageHandler\\(\\s*'[^']+'\\s*,\\s*function\\s*\\([^)]*\\)", js_txt))[[1]]
ok(sprintf("3k  every Shiny message handler takes exactly one argument (%d handlers)", length(manejadores)),
   length(manejadores) == 3L && all(grepl("function\\s*\\(\\s*[A-Za-z_$][A-Za-z0-9_$]*\\s*\\)$", manejadores)))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
