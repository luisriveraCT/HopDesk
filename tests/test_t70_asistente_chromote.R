# =============================================================================
# tests/test_t70_asistente_chromote.R
# T70/T71 in the browser, over the real Pasivos wizard with no S3
# (tests/helpers/pasivos_asistente_fixture_app.R), at 1293x632:
#   - Detalles shows "Nota (opcional)" with its help; the note is typed;
#   - Recurrencia: the end date starts blank and says "Sin fecha de fin: se
#     repite hasta que lo canceles."; with a date it says the last provision;
#   - Revisión: "Fecha de fin" and "Nota" in the summary, and the preview has
#     no provision after the end date.
# Screenshots (T70_FOTOS) for the report.
# Run from the repo root:  Rscript tests/test_t70_asistente_chromote.R
# =============================================================================
cat("=== test_t70_asistente_chromote ===

")
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("tests/helpers/live_app_harness.R")
PORT <- httpuv::randomPort()   # never a fixed port
raiz <- normalizePath(".", winslash = "/")
FOTOS <- Sys.getenv("T70_FOTOS", "")
app <- NULL; b <- NULL
limpiar <- function() { try(if (!is.null(b)) b$close(), silent = TRUE); try(if (!is.null(app)) app$proc$kill(), silent = TRUE) }
ev <- function(js) { r <- b$Runtime$evaluate(js, returnByValue = TRUE); r$result$value }
espera <- function(js, s = 30) isTRUE(tryCatch({ poll_until(function() isTRUE(tryCatch(ev(js), error = function(e) FALSE)),
  timeout_s = s, interval_s = 0.4); TRUE }, error = function(e) FALSE))
clic <- function(sel) {
  p <- jsonlite::fromJSON(ev(sprintf("(function(){ var e = document.querySelector(%s); if (!e) return 'null';
    e.scrollIntoView({block: 'center'}); var r = e.getBoundingClientRect(); return JSON.stringify([r.left + r.width / 2, r.top + r.height / 2]); })()",
    jsonlite::toJSON(sel, auto_unbox = TRUE))))
  if (is.null(p)) return(FALSE)
  for (t in c("mousePressed", "mouseReleased")) b$Input$dispatchMouseEvent(type = t, x = p[1], y = p[2], button = "left", clickCount = 1)
  Sys.sleep(0.3); TRUE
}
teclea <- function(sel, texto) {   # a person typing: focus, select all, insert, leave
  ev(sprintf("(function(){ var e = document.querySelector(%s); e.focus(); if (e.select) e.select(); return true; })()", jsonlite::toJSON(sel, auto_unbox = TRUE)))
  b$Input$insertText(text = texto); Sys.sleep(0.4)
  ev(sprintf("(function(){ var e = document.querySelector(%s); e.blur(); return e.value; })()", jsonlite::toJSON(sel, auto_unbox = TRUE)))
}
valor_js <- function(id, v) ev(sprintf("(function(){ var e = document.getElementById('%s'); e.value = %s; $(e).trigger('change'); return true; })()",
                                       id, jsonlite::toJSON(v, auto_unbox = TRUE)))
# A selectInput is selectize: set it through its API, as a click would (a jQuery .val() never reaches Shiny).
elige <- function(id, v) ev(sprintf("(function(){ var s = document.getElementById('%s').selectize; s.setValue('%s'); return s.getValue(); })()", id, v))
foto <- function(n) if (nzchar(FOTOS)) {
  sin_login(n); dir.create(FOTOS, showWarnings = FALSE, recursive = TRUE)
  writeBin(jsonlite::base64_dec(b$Page$captureScreenshot(format = "png")$data), file.path(FOTOS, paste0(n, ".png"))) }
lineas <- function() jsonlite::fromJSON(ev("JSON.stringify([].filter.call(document.querySelectorAll('.modal .pwiz-errores li'), function(l){ return l.offsetParent !== null; }).map(function(l){ return l.innerText.trim(); }))"))
caja_visible <- function() isTRUE(ev("(function(){ var c = document.querySelector('.modal .pwiz-errores'); return !!c && c.offsetParent !== null; })()"))

# The fixture has no login: a login form means the wrong app (an exploratory
# run against dev once typed into one). Checked before every screenshot.
sin_login <- function(donde) ok(sprintf("%s: no login form on screen", donde),
  !isTRUE(ev("!!document.getElementById('auth-user_id') || !!document.querySelector('input[type=password]')")))
fecha_js <- function(id, f) ev(sprintf("(function(){ var i = $('#%s input'); var dp = i.bsDatepicker ? 'bsDatepicker' : 'datepicker';
  i[dp]('update', '%s'); i.trigger('change'); return i.val(); })()", id, f))
texto <- function(sel) ev(sprintf("(function(){ var e = document.querySelector(%s); return e ? e.innerText : ''; })()", jsonlite::toJSON(sel, auto_unbox = TRUE)))

tryCatch({
  app <- start_bg_app(sprintf('setwd("%s"); source("tests/helpers/pasivos_asistente_fixture_app.R")$value', raiz), PORT, timeout_s = 180)
  b <- new_chromote_session()
  invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = FALSE))
  ok("the fixture opens", espera("!!document.getElementById('abrir')", 120))
  clic("#abrir"); espera("!!document.getElementById('pwiz_categoria_regular')")
  clic("#pwiz_categoria_regular"); Sys.sleep(0.6)
  clic("#pwiz_next")
  ok("Detalles: the note field is there", espera("!!document.getElementById('pwiz_notas')"))
  ok("Detalles: with its help line", grepl("La verán todas sus provisiones y sus pagos.", texto(".modal-body"), fixed = TRUE))
  teclea("#pwiz_nombre", "Renta T70"); teclea("#pwiz_parte", "Inmobiliaria Prueba")
  teclea("#pwiz_notas", "Pagar por transferencia a la cuenta 1234")
  foto("T70_A_detalles_nota")
  clic("#pwiz_next")
  ok("Recurrencia: the end date is there, blank", espera("!!document.getElementById('pwiz_fecha_fin')") &&
       identical(ev("$('#pwiz_fecha_fin input').val()"), ""))
  ok("Recurrencia: blank says 'Sin fecha de fin: se repite hasta que lo canceles.'",
     espera("(document.getElementById('pwiz_fecha_fin_ayuda') || {}).innerText === 'Sin fecha de fin: se repite hasta que lo canceles.'", 10))
  foto("T70_B_sin_fin")
  fin <- format(Sys.Date() + 120, "%Y-%m-%d"); fin_txt <- format(Sys.Date() + 120, "%d/%m/%Y")
  fecha_js("pwiz_fecha_fin", fin)
  ok(sprintf("Recurrencia: with %s it says the last provision", fin_txt),
     espera(sprintf("(document.getElementById('pwiz_fecha_fin_ayuda') || {}).innerText.indexOf('%s') >= 0", fin_txt), 10))
  teclea("#pwiz_amount_default", "1,500")
  foto("T70_C_con_fin")
  clic("#pwiz_next")
  ok("Revisión opens", espera("!!document.getElementById('pwiz_revision_summary') && document.getElementById('pwiz_revision_summary').innerText.length > 20"))
  s <- texto("#pwiz_revision_summary")
  ok("Revisión: 'Fecha de fin' with the date", grepl("Fecha de fin", s, fixed = TRUE) && grepl(fin_txt, s, fixed = TRUE))
  ok("Revisión: 'Nota' with the note", grepl("Nota", s, fixed = TRUE) && grepl("Pagar por transferencia a la cuenta 1234", s, fixed = TRUE))
  Sys.sleep(1.5)
  fechas <- jsonlite::fromJSON(ev("JSON.stringify([].map.call(document.querySelectorAll('.modal [id$=-hdt] tbody tr'), function(r){ return r.innerText; }))"))
  # the engine's date format: "1 dic 2026"
  meses <- c(ene = 1, feb = 2, mar = 3, abr = 4, may = 5, jun = 6, jul = 7, ago = 8, sep = 9, oct = 10, nov = 11, dic = 12)
  m <- regmatches(fechas, regexec("^[[:space:]]*([0-9]{1,2}) ([a-z]{3})[a-z.]* ([0-9]{4})", fechas))
  ff <- as.Date(vapply(m, function(x) if (length(x) == 4L) sprintf("%s-%02d-%02d", x[4], meses[[x[3]]], as.integer(x[2])) else NA_character_, character(1)))
  ok(sprintf("Revisión: the preview has rows (%d) and none after the end", length(ff)), length(ff) > 0L && !anyNA(ff) && all(ff <= as.Date(fin)))
  foto("T70_D_revision")
}, error = function(e) ok(paste("no error running the checks:", conditionMessage(e)), FALSE))
limpiar()

cat(sprintf("
=== results: %d passed, %d failed ===
", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
