# =============================================================================
# tests/test_pasivos_tabla_caracterizacion_chromote.R
# Pasivos · Tabla in a real headless Chrome -- what only real clicks show --
# now on the table engine (T17b step 3). The fixture is
# tests/helpers/pasivos_tabla_fixture_app.R (the real module, fake shared, no
# S3); what reaches the rest of Pasivos is read from #eco (the same global
# inputs as before the engine). Case numbers are those of
# reportes/T17b_paso0.md; [CAMBIA] marks what Luis decided to change (before
# the engine, ba85ead, this test asserted today's: 29/0).
# Sizes: Luis's window (1293 x 632) and 1772 x 866; screenshots to
# PT_CAPTURAS if set.
#
# Run from project root:  Rscript tests/test_pasivos_tabla_caracterizacion_chromote.R
# =============================================================================
cat("=== test_pasivos_tabla_caracterizacion_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38981L
CAPTURAS <- Sys.getenv("PT_CAPTURAS", "")
HOY <- Sys.Date()
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)

app <- tryCatch(start_bg_app(sprintf('setwd("%s"); source("tests/helpers/pasivos_tabla_fixture_app.R")$value', repo),
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
T <- "pt-tabla-hdt"
abrir <- function() {
  b$Page$navigate(base, wait_ = TRUE)
  ok <- poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s') && HDT.estado('%s').visibles.length > 0", T, T))),
                   timeout_s = 60)
  Sys.sleep(0.6)
  ok
}
estado <- function() ev(sprintf("HDT.estado('%s')", T))
eco <- function() jsonlite::fromJSON(ev("document.getElementById('eco').textContent") %||% "{}")
js <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE)
# Row `id`'s cell under the column titled `dias` from today (Día).
td <- function(id, dias) sprintf("(function(){ var ths = document.querySelectorAll('#%s thead th'), k = -1;
  for (var i = 0; i < ths.length; i++) if (ths[i].textContent === %s) k = i;
  var tr = HDT.fila('%s', %s); return tr && k >= 0 ? tr.children[k] : null; })()",
  T, js(as.character(HOY + dias)), T, js(id))
clic <- function(js_el) {
  p <- ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', inline:'center', behavior:'instant'});
    var r = e.getBoundingClientRect(); return [r.left + Math.min(r.width / 2, 14), r.top + r.height/2]; })()", js_el))
  for (t in c("mouseMoved", "mousePressed", "mouseReleased"))
    b$Input$dispatchMouseEvent(type = t, x = p[[1]], y = p[[2]], button = if (t == "mouseMoved") "none" else "left",
                               buttons = if (t == "mousePressed") 1L else 0L, clickCount = 1L)
  Sys.sleep(0.5)
}
espera_eco <- function(evento, valor, n0) poll_until(function() {
  e <- eco(); identical(e$evento, evento) && (is.null(valor) || identical(e$valor, valor)) && (e$n %||% 0L) > n0
}, timeout_s = 6)
espera <- function(f) poll_until(function() isTRUE(tryCatch(f(), error = function(e) FALSE)), timeout_s = 6)

ronda <- function(w, h) {
  et <- sprintf("[%dx%d]", w, h)
  pantalla(w, h)
  ok(paste(et, "the page loads with the table"), abrir())
  e <- estado()
  ok(paste(et, "B1: rows by currency, the fees under their liability; [CAMBIA (a)] the archived one is gone"),
     identical(unlist(e$visibles), c("orphan_p0", "lib1", "lib2", "lib2_fee_1", "lib2_fee_2", "lib3", "lib'4")) &&
       identical(grep("^banda", unlist(e$lista), value = TRUE), c("banda:1:MXN", "banda:1:USD", "banda:1:EUR", "banda:1:CAD")))
  ok(paste(et, "B2: a name with <b> is text: no <b> or <i> element inside the table (C25)"),
     isTRUE(ev(sprintf("!document.querySelector('#%s tbody b, #%s tbody i')", T, T))))
  n <- eco()$n %||% 0L
  clic(paste0(td("lib1", 5), ".querySelector('button.hdt-celda-btn')"))
  ok(paste(et, "B3: ⚡ sends pasivos_convert_request with the provision's id (C7)"), espera_eco("pasivos_convert_request", "p1d", n))
  ok(paste(et, "B3b: ...and only that: the cell's own click doesn't fire too"), { Sys.sleep(0.4); identical(eco()$n, n + 1L) })
  n <- eco()$n
  clic(td("lib1", 1))
  ok(paste(et, "B4: a click on a cell sends pasivos_cell_click with its provision (C11)"), espera_eco("pasivos_cell_click", "p1c", n))
  n <- eco()$n
  clic(paste0(td("lib2", 3), ".querySelector('button.hdt-celda-btn')"))
  ok(paste(et, "B5: ↩ on a pending item sends pasivos_convert_request with its provision (C9)"),
     espera_eco("pasivos_convert_request", "p2p", n))
  n <- eco()$n
  clic(sprintf("HDT.fila('%s', 'lib1').querySelector('button.hdt-accion[data-accion=\"editar\"]')", T))
  ok(paste(et, "B6: ✎ sends pasivos_edit_liability with the liability's id (C16)"), espera_eco("pasivos_edit_liability", "lib1", n))
  n <- eco()$n
  clic(sprintf("HDT.fila('%s', \"lib'4\").querySelector('button.hdt-accion[data-accion=\"editar\"]')", T))
  ok(paste(et, "B7 [CAMBIA C26]: ✎ on a liability whose id has a ' works now"), espera_eco("pasivos_edit_liability", "lib'4", n))
  ok(paste(et, "B7b: no ✎ on an orphan or a fee"),
     isTRUE(ev(sprintf("!HDT.fila('%s', 'orphan_p0').querySelector('button.hdt-accion[data-accion=\"editar\"]:not(.hdt-accion-hueco)') &&
                        !HDT.fila('%s', 'lib2_fee_1').querySelector('button.hdt-accion[data-accion=\"editar\"]:not(.hdt-accion-hueco)')", T, T))))
  n <- eco()$n
  clic("document.querySelector('.pasivos-table-module button.btn-primary')")
  ok(paste(et, "B8: + Agregar pasivo sends pasivos_add_liability (C22)"), espera_eco("pasivos_add_liability", NULL, n))
  clic(sprintf("document.querySelector('#%s button.hdt-filtro')", T))
  ok(paste(et, "B9: the Empresa filter offers only the visible companies, never OCU (C27)"),
     espera(function() identical(unlist(ev("Array.prototype.map.call(document.querySelectorAll('.hdt-menu .hdt-menu-texto'), function (s) { return s.textContent; })")),
                                 c("BOD", "NRS", "NTS"))))
  b$Input$dispatchKeyEvent(type = "keyDown", key = "Escape", code = "Escape", windowsVirtualKeyCode = 27L)
  b$Input$dispatchKeyEvent(type = "keyUp", key = "Escape", code = "Escape", windowsVirtualKeyCode = 27L)
  Sys.sleep(0.3)
  clic(sprintf("document.querySelectorAll('#%s button.hdt-filtro')[1]", T))
  ok(paste(et, "B9b: Categoría explains each option on hover; orphans have none ('(Sin valor)')"),
     espera(function() {
       t <- ev("(function(){ var o = {}; document.querySelectorAll('.hdt-menu .hdt-menu-op').forEach(function (l) { o[l.querySelector('.hdt-menu-texto').textContent] = l.title; }); return o; })()")
       grepl("^Pagos recurrentes", t$Regular) && grepl("^Créditos", t$Financiero) && "(Sin valor)" %in% names(t)
     }))
  b$Input$dispatchKeyEvent(type = "keyDown", key = "Escape", code = "Escape", windowsVirtualKeyCode = 27L)
  b$Input$dispatchKeyEvent(type = "keyUp", key = "Escape", code = "Escape", windowsVirtualKeyCode = 27L)
  Sys.sleep(0.3)
  ok(paste(et, "B10: today's column is shaded, header and cells (C5)"),
     isTRUE(ev(sprintf("Array.prototype.some.call(document.querySelectorAll('#%s thead th.hdt-destacar'), function (th) { return th.textContent === %s; })",
                       T, js(as.character(HOY))))))
  ok(paste(et, "B11 [CAMBIA P5]: no mirror scrollbar any more"), isTRUE(ev("!document.getElementById('pt-pasivos_scroll_bar')")))
  ok(paste(et, "B12: urgency tints the row (overdue), the fee hangs with ↳"),
     isTRUE(ev(sprintf("HDT.fila('%s', 'lib1').classList.contains('hdt-sem-vencida') && !!HDT.fila('%s', 'lib2_fee_1').querySelector('.hdt-sangria-flecha')", T, T))))
  # P4: a cell of several opens its list.
  clic(td("lib1", 20))
  ok(paste(et, "B13 [CAMBIA P4]: a cell of two opens the list of both, with their amounts"),
     espera(function() {
       s <- ev("(function(){ var st = HDT.estado('pt-lista_celda-hdt'); return st ? st.visibles : null; })()")
       identical(unlist(s), c("p1e", "p1h")) && isTRUE(ev("!!document.querySelector('.modal.show')"))
     }))
  n <- eco()$n
  clic("HDT.fila('pt-lista_celda-hdt', 'p1e').querySelector('button.hdt-accion[data-accion=\"provision\"]')")
  ok(paste(et, "B13b: ⚡ in the list sends pasivos_convert_request for that provision and closes the list"),
     espera_eco("pasivos_convert_request", "p1e", n) && espera(function() isTRUE(ev("!document.querySelector('.modal.show')"))))
  clic(sprintf("document.getElementById('%s')", "pt-extend_right"))
  ok(paste(et, "B14: 60 días → shows the provision at today + 54 (C6)"),
     espera(function() startsWith(ev(paste0(td("lib1", 54), ".textContent")) %||% "", "$950.00")))   # then its ⚡
  if (nzchar(CAPTURAS)) { dir.create(CAPTURAS, showWarnings = FALSE, recursive = TRUE)
    b$screenshot(file.path(CAPTURAS, sprintf("pasivos_tabla_motor_%dx%d.png", w, h)), show = FALSE) }
}

ronda(1293, 632)
ronda(1772, 866)

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
