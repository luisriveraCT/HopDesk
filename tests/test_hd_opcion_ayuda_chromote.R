# =============================================================================
# tests/test_hd_opcion_ayuda_chromote.R
# opcion_fila and filtros$ayuda (T18 §6) in a real headless Chrome, against
# tests/helpers/opcion_ayuda_fixture_app.R (no S3). Clicks and keys are real
# input events; what the server got is read from #eco (t$opciones(),
# t$ediciones()). Sizes: 1293 x 632 (Luis) and 1772 x 866.
#
# Run from project root:  Rscript tests/test_hd_opcion_ayuda_chromote.R
# =============================================================================
cat("=== test_hd_opcion_ayuda_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38983L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
app <- tryCatch(start_bg_app(sprintf('setwd("%s"); source("tests/helpers/opcion_ayuda_fixture_app.R")$value', repo),
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
C <- "c-hdt"; F <- "f-hdt"
abrir <- function() {
  b$Page$navigate(base, wait_ = TRUE)
  ok <- poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s') && !!HDT.estado('%s')", C, F))), timeout_s = 60)
  Sys.sleep(0.6)
  ok
}
eco <- function() jsonlite::fromJSON(ev("document.getElementById('eco').textContent") %||% "{}")
opcion_eco <- function(id) { o <- eco()$opciones; o$opcion[match(id, o$id)] }
valido_eco <- function(id) { e <- eco()$ediciones; if (!length(e)) NA else e$valido[match(id, e$id)] }
boton <- function(fila, texto) sprintf("Array.prototype.find.call(HDT.fila('%s', '%s').querySelectorAll('button.hdt-op-b'), function (b) { return b.textContent === '%s'; })", C, fila, texto)
elegida <- function(fila) ev(sprintf("(function(){ var b = HDT.fila('%s', '%s').querySelector('button.hdt-op-si'); return b ? b.textContent : null; })()", C, fila))
td_importe <- function(fila) sprintf("HDT.fila('%s', '%s').children[HDT.fila('%s', '%s').children.length - 1]", C, fila, C, fila)
punto <- function(js_el) ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', behavior:'instant'});
  var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
clic <- function(js_el) {
  p <- punto(js_el)
  for (t in c("mouseMoved", "mousePressed", "mouseReleased"))
    b$Input$dispatchMouseEvent(type = t, x = p[[1]], y = p[[2]], button = if (t == "mouseMoved") "none" else "left",
                               buttons = if (t == "mousePressed") 1L else 0L, clickCount = 1L)
  Sys.sleep(0.35)
}
tecla <- function(k, code, vk) {
  b$Input$dispatchKeyEvent(type = "rawKeyDown", key = k, code = code, windowsVirtualKeyCode = vk)
  b$Input$dispatchKeyEvent(type = "keyUp", key = k, code = code, windowsVirtualKeyCode = vk)
  Sys.sleep(0.3)
}
espera <- function(f) poll_until(function() isTRUE(tryCatch(f(), error = function(e) FALSE)), timeout_s = 5)

ronda <- function(w, h) {
  et <- sprintf("[%dx%d]", w, h)
  pantalla(w, h)
  ok(paste(et, "the page loads with both tables"), abrir())
  ok(paste(et, "O1: each row shows Nuevo | Mantener | Otro on one line, starting at the data's choice"),
     identical(ev(sprintf("Array.prototype.map.call(HDT.fila('%s', 'p1').querySelectorAll('button.hdt-op-b'), function (b) { return b.textContent; })", C)),
               list("Nuevo", "Mantener", "Otro")) &&
       identical(elegida("p1"), "Mantener") && identical(elegida("p3"), "Nuevo") &&
       ev(sprintf("HDT.fila('%s', 'p1').getBoundingClientRect().height", C)) < 32)
  ok(paste(et, "O2: Nuevo's bubble is the suggested amount"), identical(ev(paste0(boton("p1", "Nuevo"), ".title")), "$150.00"))
  clic(boton("p1", "Nuevo"))
  ok(paste(et, "O3: a click chooses Nuevo, and the server has it"),
     identical(elegida("p1"), "Nuevo") && espera(function() identical(opcion_eco("p1"), "aplicar")))
  ok(paste(et, "O4: choosing never marks the row"), length(ev(sprintf("HDT.estado('%s').seleccion", C))) == 0L)
  clic(td_importe("p2"))
  ev("document.activeElement.select()"); b$Input$insertText(text = "120"); tecla("Enter", "Enter", 13L)
  ok(paste(et, "O5: typing an amount picks Otro; it counts (valido) on the server"),
     identical(elegida("p2"), "Otro") && espera(function() identical(opcion_eco("p2"), "personalizar") && isTRUE(valido_eco("p2"))))
  clic(boton("p2", "Mantener"))
  ok(paste(et, "O6: back to Mantener, the amount is greyed and doesn't count"),
     grepl("hdt-ed-fuera", ev(paste0(td_importe("p2"), ".className"))) && espera(function() isFALSE(valido_eco("p2"))))
  clic(boton("p2", "Otro"))
  ok(paste(et, "O6b: Otro again, it counts again"), espera(function() isTRUE(valido_eco("p2"))) &&
       !grepl("hdt-ed-fuera", ev(paste0(td_importe("p2"), ".className"))))
  ev(paste0(boton("p4", "Mantener"), ".focus()"))
  tecla("ArrowRight", "ArrowRight", 39L)
  ok(paste(et, "O7: → moves the choice to Otro, and the focus with it"),
     identical(elegida("p4"), "Otro") && identical(ev("document.activeElement.textContent"), "Otro"))
  tecla("ArrowLeft", "ArrowLeft", 37L); tecla("ArrowLeft", "ArrowLeft", 37L)
  ok(paste(et, "O7b: ← ← goes to Nuevo"), identical(elegida("p4"), "Nuevo") && espera(function() identical(opcion_eco("p4"), "aplicar")))
  ev(paste0(boton("p4", "Mantener"), ".focus()"))
  # keyDown with its text, as a real keyboard: rawKeyDown alone never activates a button.
  b$Input$dispatchKeyEvent(type = "keyDown", key = "Enter", code = "Enter", windowsVirtualKeyCode = 13L, text = "\r")
  b$Input$dispatchKeyEvent(type = "keyUp", key = "Enter", code = "Enter", windowsVirtualKeyCode = 13L)
  Sys.sleep(0.3)
  ok(paste(et, "O7c: Enter on a focused option chooses it (the keyboard's click)"),
     identical(elegida("p4"), "Mantener") && espera(function() identical(opcion_eco("p4"), "mantener")))
  ok(paste(et, "O8: a locked row's buttons are disabled; a click does nothing"), {
    clic(boton("p3", "Mantener"))
    isTRUE(ev(paste0(boton("p3", "Mantener"), ".disabled"))) && identical(elegida("p3"), "Nuevo")
  })
  ev("document.getElementById('todos').click()")
  ok(paste(et, "O9: t$poner_opcion('mantener') sets every free row; the locked one stays"),
     espera(function() identical(eco()$opciones$opcion, c("mantener", "mantener", "aplicar", "mantener"))) &&
       identical(elegida("p1"), "Mantener") && identical(elegida("p3"), "Nuevo"))
  clic(boton("p1", "Otro")); clic(boton("p2", "Nuevo"))
  ev("document.getElementById('otra').click()")
  ok(paste(et, "O10: new data: p1 is gone (and its choice); p2's data now says Nuevo, still chosen"),
     espera(function() identical(eco()$opciones$id, c("p2", "p3", "p4"))) && identical(elegida("p2"), "Nuevo") &&
       identical(names(ev(sprintf("HDT.estado('%s').opciones", C))) %||% character(0), character(0)))
  ok(paste(et, "O11: the general bubble never opens over the choice cell"), {
    p <- punto(boton("p4", "Mantener"))
    b$Input$dispatchMouseEvent(type = "mouseMoved", x = p[[1]] - 2, y = p[[2]], button = "none")
    b$Input$dispatchMouseEvent(type = "mouseMoved", x = p[[1]], y = p[[2]], button = "none")
    Sys.sleep(0.6)
    isTRUE(ev("(function(){ var t = document.querySelector('.hdt-tip'); return !t || t.hidden; })()"))
  })

  # ── filtros$ayuda ──
  clic(sprintf("document.querySelector('#%s button.hdt-filtro')", F))
  ok(paste(et, "A1: in the filter's menu, an option with help says it on hover (title), one without doesn't"),
     espera(function() {
       t <- ev("(function(){ var o = {}; document.querySelectorAll('.hdt-menu .hdt-menu-op').forEach(function (l) { o[l.querySelector('.hdt-menu-texto').textContent] = l.title; }); return o; })()")
       identical(t$regular, ev("HD_I18N['tablas.vacio']")) && identical(t$financiero, ev("HD_I18N['tablas.vacio_filtro']")) &&
         identical(t$tarjeta, "")
     }))
  ok(paste(et, "A2: the help takes no space: no extra text in the menu"),
     isTRUE(ev("Array.prototype.every.call(document.querySelectorAll('.hdt-menu .hdt-menu-op'), function (l) { return l.children.length === 3; })")))
  tecla("Escape", "Escape", 27L)
}

ronda(1293, 632)
ronda(1772, 866)

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
