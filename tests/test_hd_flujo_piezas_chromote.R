# =============================================================================
# tests/test_hd_flujo_piezas_chromote.R
# T17c step 1 in a real headless Chrome: sumas_columnas, filas_resumen and
# edicion_celda's expresion / filas on tests/helpers/flujo_piezas_fixture_app.R
# (class F, no S3). Clicks and keys are real input events; what the server got
# is read from #eco (t$ediciones()). Sizes: 1293 x 632 (Luis) and 1772 x 866.
#
# Run from project root:  Rscript tests/test_hd_flujo_piezas_chromote.R
# =============================================================================
cat("=== test_hd_flujo_piezas_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38989L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
app <- tryCatch(start_bg_app(sprintf('setwd("%s"); source("tests/helpers/flujo_piezas_fixture_app.R")$value', repo),
                             PORT, timeout_s = 90), error = function(e) { message(conditionMessage(e)); NULL })
ok("fixture app started", !is.null(app))
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
pantalla <- function(w, h) invisible(b$Emulation$setDeviceMetricsOverride(width = w, height = h, deviceScaleFactor = 1, mobile = FALSE))
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
T <- "t-hdt"; P <- "p-hdt"
abrir <- function() {
  b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE)
  ok <- poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s') && !!HDT.estado('%s')", T, P))), timeout_s = 60)
  Sys.sleep(0.6)
  ok
}
clic <- function(js_el) {
  p <- ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', behavior:'instant'});
    var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
  for (t in c("mouseMoved", "mousePressed", "mouseReleased"))
    b$Input$dispatchMouseEvent(type = t, x = p[[1]], y = p[[2]], button = if (t == "mouseMoved") "none" else "left",
                               buttons = if (t == "mousePressed") 1L else 0L, clickCount = 1L)
  Sys.sleep(0.4)
}
tecla <- function(k, code, vk, texto = NULL) {
  if (is.null(texto)) b$Input$dispatchKeyEvent(type = "rawKeyDown", key = k, code = code, windowsVirtualKeyCode = vk)
  else b$Input$dispatchKeyEvent(type = "keyDown", key = k, code = code, windowsVirtualKeyCode = vk, text = texto)
  b$Input$dispatchKeyEvent(type = "keyUp", key = k, code = code, windowsVirtualKeyCode = vk)
  Sys.sleep(0.35)
}
espera <- function(f, s = 6) poll_until(function() isTRUE(tryCatch(f(), error = function(e) FALSE)), timeout_s = s)
eco <- function() jsonlite::fromJSON(ev("document.getElementById('eco').textContent") %||% "[]")
grupo <- function(primera) sprintf("document.querySelector('#%s tr.hdt-grupo[data-primera=\"%s\"]')", T, primera)
textos_grupo <- function(primera) unlist(ev(sprintf("Array.prototype.map.call(%s.children, function (td) { return td.textContent; })", grupo(primera))))
td <- function(id, k) sprintf("HDT.fila('%s', '%s').children[%d]", T, id, k)   # 0 Contraparte, 1 Día 0, 2 L 02, 3 M 03

ronda <- function(w, h) {
  et <- sprintf("[%dx%d]", w, h)
  pantalla(w, h)
  ok(paste(et, "the page loads with both tables"), abrir())
  # ── sumas_columnas ──
  ok(paste(et, "S1: the Cobros group adds up EACH column, compact, empty at 0: $5.3M, $3K (2,500 rounds up)"),
     identical(textos_grupo("a1")[3:4], c("$5.3M", "$3K")))
  ok(paste(et, "S1b: Pagos the same: $5.0M and nothing for a 0; the Día 0 column doesn't sum"),
     identical(textos_grupo("p1")[2:4], c("", "$5.0M", "")))
  ok(paste(et, "S2: groups start closed: the counterparties hidden, the summary rows shown"),
     identical(unlist(ev(sprintf("HDT.estado('%s').lista.filter(function (x) { return x.indexOf('banda') !== 0 && x.indexOf('grupo') !== 0; })", T))),
               c("neto_mxn", "acum_mxn", "neto_usd", "acum_usd")))
  # ── filas_resumen ──
  ok(paste(et, "S3: each currency's summary rows go at its end, after its groups (a group of one too: its sums), in order"), {
    l <- unlist(ev(sprintf("HDT.estado('%s').lista", T)))
    identical(l, c("banda:1:MXN", "grupo:cerrado:a1,a2", "grupo:cerrado:p1", "neto_mxn", "acum_mxn",
                   "banda:1:USD", "grupo:cerrado:u1", "neto_usd", "acum_usd"))
  })
  ok(paste(et, "S4: summary rows are a dark band, outside the count and the visible rows"),
     identical(ev(sprintf("getComputedStyle(%s).backgroundColor", td("neto_mxn", 2))), "rgb(30, 58, 95)") &&
       !any(c("neto_mxn", "acum_mxn") %in% unlist(ev(sprintf("HDT.estado('%s').visibles", T)))))
  ev(sprintf("(function(){ var i = document.querySelector('#%s .hdt-buscar'); i.value = 'alfa'; i.dispatchEvent(new Event('input', {bubbles: true})); })()", T))
  ok(paste(et, "S5: a search keeps a section's summary rows while the section shows; one with nothing left loses them"),
     espera(function() identical(unlist(ev(sprintf("HDT.estado('%s').lista", T))),
                                 c("banda:1:MXN", "grupo:cerrado:a1", "neto_mxn", "acum_mxn"))))
  ev(sprintf("(function(){ var i = document.querySelector('#%s .hdt-buscar'); i.value = ''; i.dispatchEvent(new Event('input', {bubbles: true})); })()", T))
  Sys.sleep(0.6)
  # ── Día 0: expresion + filas ──
  ok(paste(et, "S6: Día 0 can be typed only on the position rows"),
     isTRUE(ev(sprintf("%s.classList.contains('hdt-editable') && !%s.classList.contains('hdt-editable')", td("acum_mxn", 1), td("neto_mxn", 1)))))
  clic(td("acum_mxn", 1))
  ev("document.activeElement.select()"); b$Input$insertText(text = "1000+500"); tecla("Enter", "Enter", 13L)
  ok(paste(et, "S7: 1000+500 and Enter: the cell reads $1,500.00 and the server has 1500, valid"),
     espera(function() { e <- eco(); identical(e$id, "acum_mxn") && isTRUE(as.numeric(e$valor) == 1500) }) &&
       grepl("1,500.00", ev(paste0(td("acum_mxn", 1), ".textContent")), fixed = TRUE) && isTRUE(eco()$valido))
  clic(td("acum_usd", 1))
  ev("document.activeElement.select()"); b$Input$insertText(text = "alert(1)"); tecla("Enter", "Enter", 13L)
  ok(paste(et, "S8: anything that isn't arithmetic is refused: the cell red, the message says how, nothing sent"),
     grepl("hdt-ed-mal", ev(paste0(td("acum_usd", 1), ".className"))) &&
       grepl("5000 + 1200", ev("(document.querySelector('.hdt-tip:not([hidden])') || {}).textContent || ''"), fixed = TRUE) &&
       identical(length(eco()$id), 1L))
  tecla("Escape", "Escape", 27L)
  ok(paste(et, "S9: Esc leaves it as it was"), !isTRUE(ev(sprintf("!!%s.querySelector('input')", td("acum_usd", 1)))))
  ok(paste(et, "S10: the plain table has none of it"),
     isTRUE(ev(sprintf("!document.querySelector('#%s .hdt-resumen, #%s .hdt-editable, #%s tr.hdt-grupo')", P, P, P))))
}

ronda(1293, 632)
ronda(1772, 866)

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
