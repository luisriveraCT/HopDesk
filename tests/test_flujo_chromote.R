# =============================================================================
# tests/test_flujo_chromote.R
# The Flujo de caja preview (T11), in a real headless Chrome, against
# tests/helpers/flujo_fixture_app.R: the real module and aggregation over
# fixed rows (no S3). Every click and key is a real input event. Blocks follow
# the T11 inventory (hopdesk_coord/tablas/reportes/T11_inventario.md, seen in
# passing): D Día 0 (5). Since T17c the table is the engine's: Día 0 is typed in
# the position row's cell (edicion_celda, arithmetic only), no dialog.
#
# Run from project root:  Rscript tests/test_flujo_chromote.R
# FLU_RAIZ=<older checkout> runs the same test against that checkout's code.
# =============================================================================
cat("=== test_flujo_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38999L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
app <- tryCatch(start_bg_app(sprintf('Sys.setenv(FLU_RAIZ = "%s"); setwd("%s"); source("tests/helpers/flujo_fixture_app.R")$value',
                                     Sys.getenv("FLU_RAIZ", repo), repo), PORT, timeout_s = 90),
                error = function(e) { message(conditionMessage(e)); NULL })
ok("fixture app started", !is.null(app))
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
centro <- function(js_el) ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', behavior:'instant'});
  var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
raton <- function(tipo, p, botones = 0L)
  b$Input$dispatchMouseEvent(type = tipo, x = p[[1]], y = p[[2]], button = if (tipo == "mouseMoved") "none" else "left",
                             buttons = botones, clickCount = 1L)
clic <- function(js_el) {
  p <- centro(js_el)
  raton("mouseMoved", p); raton("mousePressed", p, 1L); raton("mouseReleased", p)
  Sys.sleep(0.5)
}
T <- "cfp-tabla-hdt"
celda <- sprintf("HDT.fila('%s', 'acum|MXN').children[2]", T)   # 0 Contraparte, 1 Etiqueta, 2 Día 0
editor <- function() isTRUE(ev(sprintf("!!%s.querySelector('input')", celda)))
texto_celda <- function() trimws(ev(sprintf("%s.textContent", celda)))
valor_celda <- function() ev(sprintf("HDT.estado('%s').ediciones['acum|MXN'] ? 'editando' : %s.title", T, celda))
pos <- function() trimws(ev(sprintf("HDT.fila('%s', 'acum|MXN').children[3].textContent", T)))
# Type an expression in Día 0 with real keys and Enter.
dia0 <- function(expr) {
  clic(celda); Sys.sleep(0.3)
  ev("document.activeElement.select(); 0")
  b$Input$insertText(text = expr); Sys.sleep(0.2)
  b$Input$dispatchKeyEvent(type = "keyDown", key = "Enter", code = "Enter", windowsVirtualKeyCode = 13L, text = "\r")
  b$Input$dispatchKeyEvent(type = "keyUp", key = "Enter", code = "Enter", windowsVirtualKeyCode = 13L)
  Sys.sleep(1.5)
}

abrir <- function() {
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE))
  poll_until(function() isTRUE(ev("!!window.Shiny && !!document.getElementById('abrir_flujo')")), timeout_s = 60)
  Sys.sleep(1)
  invisible(ev("try { localStorage.removeItem('hopdesk_cf_dia0'); } catch (e) {}; 0"))
  clic("document.getElementById('abrir_flujo')")
}
abrir()
ok("the preview paints this week's grid",
   poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.fila('%s', 'acum|MXN')", T))), timeout_s = 60))
invisible(ev("window.__alertas = 0; window.alert = function(){ window.__alertas++; }; 0"))

# ── D · Día 0 ───────────────────────────────────────────────────────────────
dia0("1500*2-300")
ok("D: 1500*2-300 is applied (2,700, shown compact; the full amount in its bubble)",
   poll_until(function() identical(texto_celda(), "$3K"), timeout_s = 10) && grepl("2,700.00", valor_celda(), fixed = TRUE))
clic(celda); Sys.sleep(0.3)
ok("D: reopening starts from the stored value (it read '$3K' and marked an error)",
   editor() && identical(as.numeric(ev("document.activeElement.value")), 2700))
b$Input$dispatchKeyEvent(type = "rawKeyDown", key = "Escape", code = "Escape", windowsVirtualKeyCode = 27L)
b$Input$dispatchKeyEvent(type = "keyUp", key = "Escape", code = "Escape", windowsVirtualKeyCode = 27L); Sys.sleep(0.4)
dia0("-(500 + 250)")
ok("D: a negative keeps its sign", poll_until(function() identical(texto_celda(), "-$750"), timeout_s = 10))
clic(celda); Sys.sleep(0.3)
ok("D: and reopens as -750 (stripping the minus made it 750)", identical(as.numeric(ev("document.activeElement.value")), -750))
invisible(ev("document.activeElement.select(); 0")); b$Input$insertText(text = "alert(1)"); Sys.sleep(0.2)
b$Input$dispatchKeyEvent(type = "keyDown", key = "Enter", code = "Enter", windowsVirtualKeyCode = 13L, text = "\r")
b$Input$dispatchKeyEvent(type = "keyUp", key = "Enter", code = "Enter", windowsVirtualKeyCode = 13L); Sys.sleep(0.6)
ok("D: alert(1) is refused -- nothing runs, the cell turns red and says how",
   ev("window.__alertas") == 0 && grepl("hdt-ed-mal", ev(sprintf("%s.className", celda))))
b$Input$dispatchKeyEvent(type = "rawKeyDown", key = "Escape", code = "Escape", windowsVirtualKeyCode = 27L)
b$Input$dispatchKeyEvent(type = "keyUp", key = "Escape", code = "Escape", windowsVirtualKeyCode = 27L); Sys.sleep(0.4)
ok("D: and the stored value doesn't change", identical(texto_celda(), "-$750"))
# The evaluator itself (\$ and commas, refusals, precedence) is the engine's: test_hd_flujo_piezas_chromote (S7, S8).

# ── H · day letters in the headers (seen in passing 7) ──────────────────────
letras <- unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s thead th')).slice(3, 10).map(function(t){ return t.textContent.trim().charAt(0); })", T)))
ok("H: this week's headers read L M X J V S D (Wednesday was a second M)",
   identical(letras, c("L", "M", "X", "J", "V", "S", "D")))

# ── E · the range button's text: one place, one format ──────────────────────
# It used to read "02–08 Nov" at first and the picker's own "26 Oct - 01 Nov"
# after a change. Now the server writes it from the dictionary every time.
mes <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
etiqueta <- function(d1, d2) {
  dd <- function(d) format(d, "%d"); m <- function(d) mes[as.integer(format(d, "%m"))]
  if (format(d1, "%Y-%m") == format(d2, "%Y-%m")) sprintf("%s–%s %s", dd(d1), dd(d2), m(d2))
  else sprintf("%s %s – %s %s", dd(d1), m(d1), dd(d2), m(d2))
}
boton_rango <- function() trimws(ev("document.getElementById('cfp-btn_date_range').textContent"))
lunes <- as.Date(cut(Sys.Date(), "week"))
ok("E: the first text is this week's, from the dictionary",
   identical(boton_rango(), etiqueta(lunes, lunes + 6)))
d1 <- lunes - 9; d2 <- lunes + 3
invisible(ev(sprintf("(function(){ var bs = Shiny.inputBindings.getBindings();
  for (var i = 0; i < bs.length; i++) { var st = bs[i].binding.store;
    if (st && st['cfp-date_range']) { st['cfp-date_range'].selectDate([new Date(%d, %d, %d), new Date(%d, %d, %d)]); return true; } }
  return false; })()",
  as.integer(format(d1, "%Y")), as.integer(format(d1, "%m")) - 1L, as.integer(format(d1, "%d")),
  as.integer(format(d2, "%Y")), as.integer(format(d2, "%m")) - 1L, as.integer(format(d2, "%d")))))
ok("E: a range picked in the picker gets the same format (two months here)",
   poll_until(function() identical(boton_rango(), etiqueta(d1, d2)), timeout_s = 10))
fuente <- paste(readLines(file.path(Sys.getenv("FLU_RAIZ", repo), "R/cashflow_preview_module.R"), warn = FALSE), collapse = "\n")
ok("E: no updateActionButton() on the plain range <button> is left",
   !grepl('updateActionButton\\(session, "btn_date_range"', fuente))

# ── R · the saved state comes back after 18:00 in Mexico ────────────────────
# The browser in Mexico City with its clock at 19:39 today -- already
# tomorrow in UTC. The state is saved for the day; it was written with the
# server's date and read against toISOString() (UTC), so from 18:00 it was
# never restored.
hoy <- format(Sys.Date(), "%Y-%m-%d")
fijo <- as.numeric(as.POSIXct(paste(hoy, "19:39:00"), tz = "America/Mexico_City")) * 1000
invisible(b$Emulation$setTimezoneOverride(timezoneId = "America/Mexico_City"))
invisible(b$Page$addScriptToEvaluateOnNewDocument(source = sprintf(paste0(
  "(function(){ var RD = Date, t0 = RD.now(), FIJO = %.0f;",
  "  function ahora(){ return FIJO + (RD.now() - t0); }",
  "  function FD(){ if (!(this instanceof FD)) return new RD(ahora()).toString();",
  "    if (!arguments.length) return new RD(ahora());",
  "    return new (Function.prototype.bind.apply(RD, [null].concat([].slice.call(arguments))))(); }",
  "  FD.prototype = RD.prototype; FD.now = ahora; FD.UTC = RD.UTC; FD.parse = RD.parse; window.Date = FD; })();"), fijo)))
abrir()
invisible(poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.fila('%s', 'acum|MXN')", T))), timeout_s = 60))
ok("R: the browser thinks it is 19:39 here and already the next day in UTC",
   identical(ev("new Date().getHours()"), 19L) && ev("new Date().toISOString().slice(0,10)") > hoy)
invisible(ev("localStorage.removeItem('hopdesk_cf_preview_state'); 0"))
clic("document.getElementById('cfp-btn_grouping')"); Sys.sleep(2)
ok("R: changing the grouping (Auto -> Diario) saves the state under today's LOCAL date",
   identical(ev("JSON.parse(localStorage.getItem('hopdesk_cf_preview_state') || '{}').saved_date"), hoy) &&
   identical(ev("JSON.parse(localStorage.getItem('hopdesk_cf_preview_state') || '{}').grouping"), "daily"))
abrir()
ok("R: after a reload, opening the preview restores it (the grouping says Diario)",
   poll_until(function() identical(trimws(ev("document.getElementById('cfp-btn_grouping').textContent")), "Diario"),
              timeout_s = 20))
ok("E: a restored range is written by the server in the same format",
   identical(boton_rango(), etiqueta(lunes, lunes + 6)))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
