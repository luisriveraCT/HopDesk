# =============================================================================
# tests/test_overlay_salida.R
# T88 #1: the loading overlay goes once the app is really done, not a fixed
# 30 s after the 100 %. window.hldSalida.decidir() (www/overlay_init.js §0)
# is pure: state + a fake clock in, 'esperar' | 'quitar' | 'tope' out. Run in
# node when node is installed; the wiring (§1a, §5b, §8) is checked on the
# text. Chrome: tests/test_tema_cliente_chromote.R measures the real moment.
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
js_txt <- paste(readLines("www/overlay_init.js", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# ── 1. The decision, with a fake clock (node) ───────────────────────────────
node <- Sys.which("node")
if (!nzchar(node)) {
  cat("SKIP (1): node no está instalado\n")
} else {
  # Each case: a state and the clock; the overlay file loaded with a $ that
  # never runs its body (no DOM here), so only section 0 exists.
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
    nada           = list(s = NULL, t = 5000))
  # Production's 16:39 sign-in (T88 paso 0), time from Phase 1: 100 % at 15 s,
  # the theme at 8 s, busy 15-28 s (the Pasivos rebuild, then the IC filter),
  # the open tab drawn at 28 s. Stepped every 250 ms as the browser does.
  js <- tempfile(fileext = ".js")
  writeLines(c(
    "global.window = {}; global.$ = function () {};",
    sprintf("eval(require('fs').readFileSync(%s, 'utf8'));",
            jsonlite::toJSON(normalizePath("www/overlay_init.js", winslash = "/"), auto_unbox = TRUE)),
    sprintf("var casos = %s;", jsonlite::toJSON(casos, auto_unbox = TRUE, null = "null")),
    "var r = {}; Object.keys(casos).forEach(function (k) { r[k] = window.hldSalida.decidir(casos[k].s, casos[k].t); });",
    "var s = { pct100At: 15000, temaAt: 8000, busy: true, idleAt: 0, pestanaLista: false }, sale = null;",
    "for (var t = 15000; t <= 60000 && !sale; t += 250) {",
    "  if (t >= 28000 && s.busy) { s.busy = false; s.idleAt = 28000; s.pestanaLista = true; }",
    "  var v = window.hldSalida.decidir(s, t); if (v !== 'esperar') sale = { t: t, v: v };",
    "}",
    "r.prod = sale; r.silencio = window.hldSalida.SILENCIO; r.tope = window.hldSalida.TOPE;",
    "console.log(JSON.stringify(r));"), js)
  r <- jsonlite::fromJSON(paste(system2(node, js, stdout = TRUE), collapse = ""), simplifyVector = FALSE)
  ok("1a  before the 100 %, it waits (whatever else is ready)", identical(r$sin_100, "esperar"))
  ok("1b  without the theme, it waits", identical(r$sin_tema, "esperar"))
  ok("1c  ... until the cap: 30 s after the 100 %, it goes anyway (today's behaviour)", identical(r$sin_tema_tope, "tope"))
  ok("1d  while Shiny is busy, it waits", identical(r$ocupada, "esperar"))
  ok("1e  with the open tab empty or recalculating, it waits", identical(r$pestana_vacia, "esperar"))
  ok("1f  idle for 1499 ms: still waits", identical(r$silencio_corto, "esperar"))
  ok("1g  idle for 1500 ms, theme in, tab drawn: it goes", identical(r$silencio_ok, "quitar"))
  ok("1h  idle since before the 100 %: the silence counts from the 100 %", identical(r$idle_antes, "esperar") &&
       identical(r$idle_antes_ok, "quitar"))
  ok("1i  busy and empty at the cap: it goes anyway", identical(r$ocupada_tope, "tope"))
  ok("1j  no state at all: it waits", identical(r$nada, "esperar"))
  ok("1k  silence 1500 ms, cap 30000 ms (the Coordinador's numbers)", identical(r$silencio, 1500L) && identical(r$tope, 30000L))
  ok(sprintf("1l  production's 16:39 sign-in: it goes at %.2f s from Phase 1 (was 45.4 s), never before the tab is drawn (28 s)",
             r$prod$t / 1000), identical(r$prod$v, "quitar") && r$prod$t == 29500)
}

# ── 2. The wiring (text) ─────────────────────────────────────────────────────
ok("2a  no fixed 30 s timer is left: 30000 appears only as the cap",
   lengths(regmatches(js_txt, gregexpr("30000", js_txt, fixed = TRUE))) == 1L && grepl("TOPE: 30000", js_txt, fixed = TRUE))
ok("2b  the 100 % starts the watcher (and stamps the time once)",
   grepl("if \\(!H\\.salida\\.pct100At\\) H\\.salida\\.pct100At = Date\\.now\\(\\);\\s*_vigilarSalida\\(\\);", js_txt))
ok("2c  the theme is heard on shiny:message, never with a second hdTema handler (it would replace the theme's own)",
   grepl("on('shiny:message'", js_txt, fixed = TRUE) && grepl("c.hdTema", js_txt, fixed = TRUE) &&
     !grepl("addCustomMessageHandler\\(\\s*['\"]hdTema", js_txt))
ok("2d  busy and idle come from Shiny's own events",
   grepl("on('shiny:busy'", js_txt, fixed = TRUE) && grepl("on('shiny:idle'", js_txt, fixed = TRUE))
ok("2e  the open tab: the outermost active pane, nothing recalculating, something drawn",
   grepl("closest('.tab-pane')", js_txt, fixed = TRUE) && grepl(".shiny-output-recalculating", js_txt, fixed = TRUE) &&
     grepl("querySelectorAll('.shiny-bound-output')", js_txt, fixed = TRUE))
ok("2f  a new session (showOverlay) starts the clock again",
   grepl("H.salida.pct100At = 0;", js_txt, fixed = TRUE))
ok("2g  the watcher asks the pure decision, every 250 ms", grepl("window.hldSalida.decidir(H.salida, Date.now())", js_txt, fixed = TRUE) &&
     grepl("}, 250);", js_txt, fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
