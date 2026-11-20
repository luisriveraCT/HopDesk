# =============================================================================
# tests/test_t63_asistente_chromote.R
# T63 in the browser, over the real Pasivos wizard with no S3
# (tests/helpers/pasivos_asistente_fixture_app.R), at 1293x632. Saves nothing.
#   A  Luis's path: Nuevo -> Financiero -> Términos, Siguiente with principal
#      and rate empty -> both errors; typing the principal hides its line at
#      once, typing the rate hides the box; Siguiente goes on to Cargos.
#      An empty rate says "Captura la tasa anual", never "negativa".
#   B  the principal typed "608,763.35" (with a comma): what the field keeps.
#   C  Variable PRIME (fixed rates in the fixture) + IVA 16 % on interest + a
#      fixed tax of 50: the Revisión shows "Impuestos" and "Total a pagar",
#      and the first payment's tax is 16 % of its interest + 50.
# Screenshots (T63_FOTOS) for the report.
# Run from the repo root:  Rscript tests/test_t63_asistente_chromote.R
# =============================================================================
cat("=== test_t63_asistente_chromote ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("tests/helpers/live_app_harness.R")
PORT <- httpuv::randomPort()   # never a fixed port
raiz <- normalizePath(".", winslash = "/")
FOTOS <- Sys.getenv("T63_FOTOS", "")
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
foto <- function(n) if (nzchar(FOTOS)) { dir.create(FOTOS, showWarnings = FALSE, recursive = TRUE)
  writeBin(jsonlite::base64_dec(b$Page$captureScreenshot(format = "png")$data), file.path(FOTOS, paste0(n, ".png"))) }
lineas <- function() jsonlite::fromJSON(ev("JSON.stringify([].filter.call(document.querySelectorAll('.modal .pwiz-errores li'), function(l){ return l.offsetParent !== null; }).map(function(l){ return l.innerText.trim(); }))"))
caja_visible <- function() isTRUE(ev("(function(){ var c = document.querySelector('.modal .pwiz-errores'); return !!c && c.offsetParent !== null; })()"))

hasta_terminos <- function() {
  ev("$('.modal').modal('hide'); true"); Sys.sleep(0.8)
  clic("#abrir"); espera("!!document.getElementById('pwiz_categoria_financiero')")
  clic("#pwiz_categoria_financiero"); Sys.sleep(0.6)
  clic("#pwiz_next"); espera("!!document.getElementById('pwiz_nombre')")
  teclea("#pwiz_nombre", "Crédito T63"); teclea("#pwiz_parte", "Banco Prueba")
  clic("#pwiz_next"); espera("!!document.getElementById('pwiz_principal_original')")
}

tryCatch({
  app <- start_bg_app(sprintf('setwd("%s"); source("tests/helpers/pasivos_asistente_fixture_app.R")$value', raiz), PORT, timeout_s = 180)
  b <- new_chromote_session()
  invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = FALSE))
  ok("the fixture opens", espera("!!document.getElementById('abrir')", 120))

  # ── A ──
  ok("A: Nuevo -> Financiero -> Detalles -> Términos", hasta_terminos())
  clic("#pwiz_next"); espera("!!document.querySelector('.modal .pwiz-errores')")
  l0 <- lineas(); cat("   errores con todo vacío:", paste(l0, collapse = " | "), "\n"); foto("A1_todo_vacio")
  ok("A: Siguiente with both empty: principal and rate errors", any(grepl("principal", l0)) && any(grepl("tasa", l0, ignore.case = TRUE)))
  ok("A: an empty rate says it is missing, never 'negativa'", any(grepl("Captura la tasa anual", l0, fixed = TRUE)) && !any(grepl("negativa", l0)))
  teclea("#pwiz_principal_original", "608763.35")
  l1 <- lineas(); cat("   tras teclear el principal:", paste(l1, collapse = " | "), "\n"); foto("A2_principal_tecleado")
  ok("A: typing the principal hides its line at once (no Siguiente)", !any(grepl("principal", l1)) && any(grepl("tasa", l1, ignore.case = TRUE)))
  ok("A: …and the typed value stays in the field", identical(ev("document.getElementById('pwiz_principal_original').value"), "608763.35"))
  teclea("#pwiz_tasa_anual", "12")
  ok("A: typing the rate hides the box", !caja_visible())
  valor_js("pwiz_plazo_meses", 45); valor_js("pwiz_dia_pago", 5)
  clic("#pwiz_next")
  ok("A: Siguiente goes on to Cargos", espera("!!document.getElementById('pwiz_add_impuesto')"))

  # ── B ──
  hasta_terminos()
  v <- teclea("#pwiz_principal_original", "608,763.35")
  cat("   'B' el campo guarda:", sprintf("'%s'", v), "\n"); foto("B_principal_con_coma")
  ok("B: a comma typed in the principal never becomes a different number", v %in% c("", "608763.35", "608,763.35"))

  # ── C ──
  hasta_terminos()
  teclea("#pwiz_principal_original", "608763.35")
  clic('input[name="pwiz_tasa_tipo"][value="variable_prime"]')
  ok("C: Variable PRIME shows the reference rate", espera("(function(){ var e = document.getElementById('pwiz_variable_rate_preview'); return !!e && /PRIME/.test(e.innerText) && /DPRIME/.test(e.innerText); })()"))
  valor_js("pwiz_tasa_spread", 200); valor_js("pwiz_plazo_meses", 45); valor_js("pwiz_dia_pago", 5)
  Sys.sleep(1); foto("C1_terminos_prime")
  clic("#pwiz_next"); espera("!!document.getElementById('pwiz_add_impuesto')")
  clic("#pwiz_add_impuesto"); espera("!!document.getElementById('pwiz_imp_nombre_1')")
  ok("C: the tax row's name shows 'IVA' as placeholder, percentage by default",
     identical(ev("document.getElementById('pwiz_imp_nombre_1').placeholder"), "IVA") &&
       identical(ev("document.getElementById('pwiz_imp_tipo_1').value"), "porcentual"))
  teclea("#pwiz_imp_nombre_1", "IVA"); valor_js("pwiz_imp_valor_1", 16)
  elige('pwiz_imp_base_1', 'intereses')
  clic("#pwiz_add_impuesto"); espera("!!document.getElementById('pwiz_imp_nombre_2')")
  teclea("#pwiz_imp_nombre_2", "Derecho"); elige('pwiz_imp_tipo_2', 'fijo'); valor_js("pwiz_imp_valor_2", 50)
  Sys.sleep(0.6); foto("C2_cargos_impuestos")
  clic("#pwiz_next")
  ok("C: the Revisión opens", espera("(function(){ return document.querySelectorAll('#pwiz_amort-hdt tbody tr').length > 1; })()", 40))
  Sys.sleep(1.5)
  resumen <- ev("(function(){ var e = document.querySelector('.modal dl'); return e ? e.innerText.replace(/\\s+/g,' ') : ''; })()")
  cat("   resumen:", resumen, "\n")
  ok("C: the summary names the rate PRIME and both taxes", grepl("PRIME", resumen) && grepl("IVA 16 %", resumen, fixed = TRUE) && grepl("Derecho", resumen))
  cab <- ev("[].map.call(document.querySelectorAll('#pwiz_amort-hdt th'), function(t){ return t.innerText.trim(); }).join(' | ')")
  cat("   columnas:", cab, "\n")
  ok("C: the schedule shows 'Impuestos' and 'Total a pagar'", grepl("Impuestos", cab) && grepl("Total a pagar", cab))
  fila <- jsonlite::fromJSON(ev("(function(){ var r = document.querySelector('#pwiz_amort-hdt tbody tr:not(.hdt-esp)');
    return JSON.stringify([].map.call(r.querySelectorAll('td'), function(td){ return td.innerText.trim(); })); })()"))
  cat("   primera fila:", paste(fila, collapse = " | "), "\n")
  num <- function(x) as.numeric(gsub("[^0-9.-]", "", x))
  cols <- strsplit(cab, " | ", fixed = TRUE)[[1]]
  int <- num(fila[match(TRUE, grepl("Inter", cols))]); imp <- num(fila[match(TRUE, grepl("Impuestos", cols))])
  ok(sprintf("C: first payment's tax = 16 %% of its interest + 50 (%s vs %s)", imp, round(int * 0.16 + 50, 2)),
     isTRUE(abs(imp - round(int * 0.16 + 50, 2)) <= 0.011))
  ok("C: amounts carry their currency, no '$'", !grepl("$", paste(fila, collapse = ""), fixed = TRUE))
  foto("C3_revision")
}, error = function(e) ok(paste("no error running the checks:", conditionMessage(e)), FALSE))
limpiar()

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
