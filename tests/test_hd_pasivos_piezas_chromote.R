# =============================================================================
# tests/test_hd_pasivos_piezas_chromote.R
# T17b step 2 in a real headless Chrome: accion_celda, semaforo and sangria
# on tests/helpers/pasivos_piezas_fixture_app.R (class F, no S3). Clicks are
# real input events; what the module got is read from #eco (t$accion()).
# Sizes: 1293 x 632 (Luis) and 1772 x 866.
#
# Run from project root:  Rscript tests/test_hd_pasivos_piezas_chromote.R
# =============================================================================
cat("=== test_hd_pasivos_piezas_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38985L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
app <- tryCatch(start_bg_app(sprintf('setwd("%s"); source("tests/helpers/pasivos_piezas_fixture_app.R")$value', repo),
                             PORT, timeout_s = 90), error = function(e) { message(conditionMessage(e)); NULL })
ok("fixture app started", !is.null(app))
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
pantalla <- function(w, h) invisible(b$Emulation$setDeviceMetricsOverride(width = w, height = h, deviceScaleFactor = 1, mobile = FALSE))
base <- sprintf("http://127.0.0.1:%d", PORT)
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
T <- "t-hdt"; P <- "p-hdt"
abrir <- function() {
  b$Page$navigate(base, wait_ = TRUE)
  ok <- poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s') && !!HDT.estado('%s')", T, P))), timeout_s = 60)
  Sys.sleep(0.6)
  ok
}
eco <- function() jsonlite::fromJSON(ev("document.getElementById('eco').textContent") %||% "{}")
td <- function(fila, k) sprintf("HDT.fila('%s', '%s').children[%d]", T, fila, k)   # 0 = Pasivo, 1 = d1, 2 = d2
punto <- function(js_el) ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', behavior:'instant'});
  var r = e.getBoundingClientRect(); return [r.left + Math.min(r.width / 2, 12), r.top + r.height/2]; })()", js_el))
clic <- function(js_el, veces = 1L) {
  p <- punto(js_el)
  b$Input$dispatchMouseEvent(type = "mouseMoved", x = p[[1]], y = p[[2]], button = "none")
  for (k in seq_len(veces)) {
    b$Input$dispatchMouseEvent(type = "mousePressed", x = p[[1]], y = p[[2]], button = "left", buttons = 1L, clickCount = k)
    b$Input$dispatchMouseEvent(type = "mouseReleased", x = p[[1]], y = p[[2]], button = "left", clickCount = k)
  }
  Sys.sleep(0.4)
}
espera <- function(f) poll_until(function() isTRUE(tryCatch(f(), error = function(e) FALSE)), timeout_s = 5)
fondo <- function(js_el) ev(sprintf("getComputedStyle(%s).backgroundColor", js_el))

ronda <- function(w, h) {
  et <- sprintf("[%dx%d]", w, h)
  pantalla(w, h)
  ok(paste(et, "the page loads with both tables"), abrir())
  # ── accion_celda ──
  ok(paste(et, "P1: marks before the amount, with their meaning on hover (◷ pending, ! overdue, ✓ paid)"),
     identical(ev(paste0(td("r3", 1), ".querySelector('.hdt-celda-marca').textContent")), "◷") &&
       identical(ev(paste0(td("r3", 1), ".querySelector('.hdt-celda-marca').title")), "Pendiente de pago") &&
       identical(ev(paste0(td("r3", 2), ".querySelector('.hdt-celda-marca').textContent")), "!") &&
       identical(ev(paste0(td("r5", 1), ".querySelector('.hdt-celda-marca').title")), "Pagado") &&
       isTRUE(ev(paste0("!", td("r1", 1), ".querySelector('.hdt-celda-marca')"))))
  ok(paste(et, "P2: a cell that adds up 3 says so, with 'Suma de 3 partidas' on hover"),
     identical(ev(paste0(td("r3", 1), ".querySelector('.hdt-celda-n').textContent")), "3") &&
       identical(ev(paste0(td("r3", 1), ".querySelector('.hdt-celda-n').title")), "Suma de 3 partidas") &&
       isTRUE(ev(paste0("!", td("r1", 1), ".querySelector('.hdt-celda-n')"))))
  clic(paste0(td("r1", 1), ".querySelector('button.hdt-celda-btn')"))
  ok(paste(et, "P3: ⚡ sends provision with the row, the cell's id (with its ') and the column"),
     espera(function() { e <- eco(); identical(e[c("accion", "ids", "de", "celda", "columna")],
                                               list(accion = "provision", ids = "r1", de = "celda", celda = "p'1", columna = "d1")) }))
  n0 <- eco()$nonce
  clic(td("r3", 2))
  ok(paste(et, "P4: a click on the cell itself sends editar_celda with its id"),
     espera(function() { e <- eco(); identical(e$accion, "editar_celda") && identical(e$celda, "p3b") && !identical(e$nonce, n0) }))
  n0 <- eco()$nonce
  clic(td("r4", 1))
  ok(paste(et, "P5: a cell without id isn't clickable and sends nothing"),
     isTRUE(ev(paste0("!", td("r4", 1), ".classList.contains('hdt-celda-clic')"))) && { Sys.sleep(0.4); identical(eco()$nonce, n0) })
  clic(td("r3", 2), veces = 2L)
  ok(paste(et, "P6: a double click on a cell doesn't open the row's detail (only editar_celda)"),
     espera(function() identical(eco()$accion, "editar_celda")) && { Sys.sleep(0.4); !identical(eco()$accion, "detalle") })
  n0 <- eco()$nonce
  clic(td("r5", 1))
  ok(paste(et, "P7: a locked row's cells have no button and send nothing"),
     isTRUE(ev(paste0("!", td("r5", 1), ".querySelector('button.hdt-celda-btn')"))) && { Sys.sleep(0.4); identical(eco()$nonce, n0) })
  ev("document.getElementById('ocultar').click()")
  ok(paste(et, "P8: without permission (acciones_ocultas), ↩ disappears; ⚡ stays"),
     espera(function() isTRUE(ev(paste0("!", td("r3", 1), ".querySelector('button.hdt-celda-btn')")))) &&
       isTRUE(ev(paste0("!!", td("r1", 1), ".querySelector('button.hdt-celda-btn')"))))
  # ── semaforo ──
  ok(paste(et, "P9: semaforo: overdue red, within 2 days amber, within 7 light, with a bar; none plain"),
     identical(fondo(td("r1", 1)), "rgb(255, 236, 236)") && identical(fondo(td("r3", 1)), "rgb(255, 244, 220)") &&
       identical(fondo(td("r4", 1)), "rgb(255, 250, 240)") &&
       grepl("inset", ev(sprintf("getComputedStyle(%s).boxShadow", td("r1", 0)))) &&
       !grepl("hdt-sem-", ev(sprintf("HDT.fila('%s', 'r2').className", T))))
  # ── sangria ──
  ok(paste(et, "P10: sangria: the fee hangs from its liability with ↳, indented"),
     identical(ev(paste0(td("r2", 0), ".querySelector('.hdt-sangria-flecha').textContent")), "↳") &&
       as.numeric(sub("px", "", ev(sprintf("getComputedStyle(%s).paddingLeft", td("r2", 0))))) > 15 &&
       isTRUE(ev(paste0("!", td("r1", 0), ".querySelector('.hdt-sangria-flecha')"))))
  # ── nothing for a table without them ──
  ok(paste(et, "P11: the plain table has none of it"),
     isTRUE(ev(sprintf("!document.querySelector('#%s [class*=\"hdt-sem-\"], #%s .hdt-celda-marca, #%s .hdt-celda-btn, #%s .hdt-celda-clic, #%s .hdt-sangria')",
                       P, P, P, P, P))))
}

ronda(1293, 632)
ronda(1772, 866)

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
