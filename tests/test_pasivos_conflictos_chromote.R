# =============================================================================
# tests/test_pasivos_conflictos_chromote.R
# T20: the edit-confirm panel of a Pasivos edit on hd_table
# ("conflictos_pasivo") in a real headless Chrome, against
# tests/helpers/pasivos_conflictos_fixture_app.R (in-memory S3). Clicks and
# keys are real input events; what was saved is read from #registro.
# Sizes: 1293 x 632 (Luis) and 1772 x 866.
#
# Run from project root:  Rscript tests/test_pasivos_conflictos_chromote.R
# =============================================================================
cat("=== test_pasivos_conflictos_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38991L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)

b <- new_chromote_session()
pantalla <- function(w, h) invisible(b$Emulation$setDeviceMetricsOverride(width = w, height = h, deviceScaleFactor = 1, mobile = FALSE))
base <- sprintf("http://127.0.0.1:%d", PORT)
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
C <- "pec_tabla-hdt"
espera <- function(f, s = 6) poll_until(function() isTRUE(tryCatch(f(), error = function(e) FALSE)), timeout_s = s)
registro <- function() ev("document.getElementById('registro').textContent")
ids <- function() { x <- strsplit(ev("document.getElementById('ids').textContent"), " ")[[1]]; setNames(sub("^P.=", "", x), c("P1", "P2")) }
modal_abierto <- function() isTRUE(ev(sprintf("!!document.querySelector('.modal.show #%s')", C)))
# The table's row id for a provision ("<opening>:<provision id>").
fid <- function(pid) ev(sprintf("(HDT.estado('%s').visibles || []).find(function (x) { return x.endsWith(':%s'); }) || null", C, pid))
FILA <- function(pid) sprintf("HDT.fila('%s', (HDT.estado('%s').visibles || []).find(function (x) { return x.endsWith(':%s'); }))", C, C, pid)
boton <- function(pid, texto) sprintf("Array.prototype.find.call(%s.querySelectorAll('button.hdt-op-b'), function (b) { return b.textContent === '%s'; })", FILA(pid), texto)
elegida <- function(pid) ev(sprintf("(function(){ var b = %s.querySelector('button.hdt-op-si'); return b ? b.textContent : null; })()", FILA(pid)))
celda <- function(pid, n) ev(sprintf("%s.children[%d].textContent", FILA(pid), n))
td_otro <- function(pid) sprintf("(function(){ var f = %s; return f.children[f.children.length - 1]; })()", FILA(pid))
punto <- function(js_el) ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', behavior:'instant'});
  var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
clic <- function(js_el) {
  p <- punto(js_el)
  for (t in c("mouseMoved", "mousePressed", "mouseReleased"))
    b$Input$dispatchMouseEvent(type = t, x = p[[1]], y = p[[2]], button = if (t == "mouseMoved") "none" else "left",
                               buttons = if (t == "mousePressed") 1L else 0L, clickCount = 1L)
  Sys.sleep(0.35)
}
clic_id <- function(id) clic(sprintf("document.getElementById('%s')", id))
tecla <- function(k, code, vk) {
  b$Input$dispatchKeyEvent(type = "rawKeyDown", key = k, code = code, windowsVirtualKeyCode = vk)
  b$Input$dispatchKeyEvent(type = "keyUp", key = k, code = code, windowsVirtualKeyCode = vk)
  Sys.sleep(0.3)
}
abrir_panel <- function() {
  # The last panel's fade-out leaves its backdrop for a moment: a click then
  # lands on the backdrop, not on the button.
  espera(function() isTRUE(ev("!document.querySelector('.modal, .modal-backdrop')")))
  clic_id("abrir")
  espera(function() modal_abierto() && length(ev(sprintf("HDT.estado('%s').visibles", C))) == 2L, 15)
}
aviso <- function(tipo) isTRUE(ev(sprintf("!!document.querySelector('#shiny-notification-panel .shiny-notification-%s')", tipo)))

ronda <- function(w, h) {
  et <- sprintf("[%dx%d]", w, h)
  # A fresh app per round: the store starts as the fixture seeds it.
  app <<- tryCatch(start_bg_app(sprintf('setwd("%s"); source("tests/helpers/pasivos_conflictos_fixture_app.R")$value', repo),
                                PORT, timeout_s = 120), error = function(e) { message(conditionMessage(e)); NULL })
  ok(paste(et, "fixture app started"), !is.null(app))
  if (is.null(app)) return(invisible())
  on.exit({ stop_bg_app(app); app <<- NULL }, add = TRUE)
  pantalla(w, h)
  b$Page$navigate(base, wait_ = TRUE)
  poll_until(function() isTRUE(ev("!!window.HDT && !!document.getElementById('abrir')")), timeout_s = 60)
  Sys.sleep(0.6)
  P <- ids(); r0 <- registro()

  ok(paste(et, "K1: the panel opens with one row per conflict, both on Mantener"),
     abrir_panel() && identical(elegida(P[["P1"]]), "Mantener") && identical(elegida(P[["P2"]]), "Mantener"))
  ok(paste(et, "K2: headers Fecha, Antes, Nuevo (sugerido), Decisión, Otro monto"),
     identical(unlist(ev(sprintf("Array.prototype.map.call(document.querySelectorAll('#%s thead th'), function (t) { return t.textContent.trim(); }).filter(function (t) { return t; })", C))),
               c("Fecha", "Antes", "Nuevo (sugerido)", "Decisión", "Otro monto")))
  ok(paste(et, "K3: Antes and Nuevo are 'fecha · monto' (P1 150 -> 120; P2 100 -> 120, two dates)"), {
    a1 <- celda(P[["P1"]], 1); n1 <- celda(P[["P1"]], 2); a2 <- celda(P[["P2"]], 1); n2 <- celda(P[["P2"]], 2)
    grepl(" · \\$150\\.00$", a1) && grepl(" · \\$120\\.00$", n1) && grepl(" · \\$100\\.00$", a2) &&
      grepl(" · \\$120\\.00$", n2) && !identical(sub(" · .*", "", a2), sub(" · .*", "", n2))
  })
  ok(paste(et, "K4: Nuevo's bubble is the suggested value"), grepl("\\$120\\.00$", ev(paste0(boton(P[["P1"]], "Nuevo"), ".title"))))
  ok(paste(et, "K5: the table fits the panel: no sideways scroll"),
     isTRUE(ev(sprintf("(function(){ var t = document.getElementById('%s'); var s = t.querySelector('.hdt-scroll') || t; return s.scrollWidth <= s.clientWidth + 1; })()", C))))

  # Aplicar todos: Nuevo, then P1 to Otro by typing its amount.
  clic_id("pec_all_nuevo")
  ok(paste(et, "K6: Aplicar todos: Nuevo puts Nuevo on every row"),
     espera(function() identical(elegida(P[["P1"]]), "Nuevo") && identical(elegida(P[["P2"]]), "Nuevo")))
  clic(td_otro(P[["P1"]]))
  ev("document.activeElement.select()"); b$Input$insertText(text = "13")
  ok(paste(et, "K7: typing keeps the focus in the cell (the panel is not redrawn per key)"),
     isTRUE(ev(sprintf("%s.contains(document.activeElement)", td_otro(P[["P1"]])))))
  b$Input$insertText(text = "5"); tecla("Enter", "Enter", 13L)
  ok(paste(et, "K8: typing an amount picks Otro"), espera(function() identical(elegida(P[["P1"]]), "Otro")))

  # Cancelar -> Continuar revisando: the choices come back.
  clic_id("pec_cancel")
  espera(function() isTRUE(ev("!!document.getElementById('pec_cancel_no')")))
  clic_id("pec_cancel_no")
  ok(paste(et, "K9: Continuar revisando gives the panel back with Otro 135 on P1 and Nuevo on P2"),
     espera(function() modal_abierto() && identical(elegida(P[["P1"]]), "Otro") && identical(elegida(P[["P2"]]), "Nuevo") &&
              grepl("135", ev(paste0(td_otro(P[["P1"]]), ".textContent"))), 10))

  clic_id("pec_save")
  ok(paste(et, "K10: Guardar saves Otro 135 on P1 and Nuevo on P2 (120, its date override cleared), and closes the panel"),
     espera(function() !modal_abierto() && identical(registro(), "P1=100/135 P2=120/NA/NA"), 10))

  # Opening again starts on Mantener; Otro without an amount saves nothing.
  ok(paste(et, "K11: opening again starts every row on Mantener"),
     abrir_panel() && identical(elegida(P[["P1"]]), "Mantener") && identical(elegida(P[["P2"]]), "Mantener"))
  clic(boton(P[["P2"]], "Otro")); r1 <- registro()
  clic_id("pec_save")
  ok(paste(et, "K12: Otro without an amount: a warning, nothing saved, the panel stays"),
     espera(function() aviso("warning")) && modal_abierto() && identical(registro(), r1))

  # Cancelar -> Sí, mantener todo: nothing saved.
  clic_id("pec_cancel")
  espera(function() isTRUE(ev("!!document.getElementById('pec_cancel_yes')")))
  clic_id("pec_cancel_yes")
  ok(paste(et, "K13: Sí, mantener todo closes and saves nothing"),
     espera(function() !modal_abierto()) && identical(registro(), r1))
  ok(paste(et, "K14: the store changed only with K10's save"), !identical(r0, r1))
}

ronda(1293, 632)
ronda(1772, 866)

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
