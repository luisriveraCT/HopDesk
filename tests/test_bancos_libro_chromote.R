# =============================================================================
# tests/test_bancos_libro_chromote.R
# Bancos' Libro de Banco and its Eliminar confirmation on hd_table (T12a), in
# a real headless Chrome, against tests/helpers/bancos_fixture_app.R (the
# real bancosServer on the in-memory S3 of the CM09).
#   A. What shows: columns, the counter, Empresa only with every account,
#      Recibo optional, the Tipo badge (Comisión with commissions shown,
#      Conciliado), ✎ only on manual rows.
#   B. Marking by id (click, shift + click); Eliminar shows with rows marked
#      and its confirmation lists exactly those (hd_table "eliminar_mov").
#   C. Confirming: the store holds them deleted, the table drops them at
#      once and nothing stays marked.
#   D. ✎ opens the manual entry's form.
#   H. A save that fails (the fixture's fallar_escritura): the delete is
#      refused, nothing changes and the confirmation stays; the archive
#      in the Bancos trash fails: the user is told.
#   E. Two sessions: B deletes one of the two A marked -> A's marks keep only
#      the one left, and A's confirmation lists that one; B then deletes
#      that one while A's confirmation is open -> A is told, nothing is
#      deleted or archived twice (VP4).
#   A9. Exportar a Excel is logged as a generated file (as the old export).
#   A10-11. Exportar (formato BanBajío), the module's own: logged, and
#      narrowed by the table's search.
#   G. Without can_manage_bancos: no ✎, no Agregar, no Eliminar in the bar.
#   F. 900 more movements, "Todo el historial": window mode, and the search
#      finds the oldest by supplier and by receipt.
#
# Run from project root:  Rscript tests/test_bancos_libro_chromote.R
# =============================================================================
cat("=== test_bancos_libro_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
T <- "bnc-libro-hdt"; E <- "bnc-elim-hdt"
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
estado <- function(t) ev(sprintf("HDT.estado('%s')", t))
vis <- function(t = T) unlist(estado(t)$visibles)
sel <- function(t = T) unlist(estado(t)$seleccion)
centro <- function(js_el) ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', behavior:'instant'});
  var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
raton <- function(tipo, p, botones = 0L, mod = 0L) b$Input$dispatchMouseEvent(type = tipo, x = p[[1]], y = p[[2]],
  button = if (tipo == "mouseMoved" && !botones) "none" else "left", buttons = botones, clickCount = 1L, modifiers = mod)
clic <- function(js_el, mod = 0L) { p <- centro(js_el); raton("mouseMoved", p); raton("mousePressed", p, 1L, mod); raton("mouseReleased", p, 0L, mod); Sys.sleep(0.4) }
celda <- function(id, k = 2L, t = T) sprintf("HDT.fila('%s', '%s').querySelectorAll('td')[%d]", t, id, k)
titulos <- function(t = T) unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.dataset.col != null; }).map(function(th){ return th.firstChild.textContent; })", t)))
modal <- function() ev("(function(){ var m = document.querySelector('.modal.show'); return m ? m.innerText : ''; })()")
registro <- function() ev("document.getElementById('registro').textContent") %||% ""
eliminados <- function() { r <- sub("^eliminados: ", "", strsplit(registro(), "
")[[1]][1]); if (!is.na(r) && nzchar(r)) strsplit(r, ",")[[1]] else character(0) }
periodo <- function(v) { invisible(ev(sprintf("document.getElementById('bnc-libro_periodo').selectize.setValue('%s')", v))); Sys.sleep(1.5) }
cuenta  <- function(v) { invisible(ev(sprintf("document.getElementById('bnc-libro_cuenta').selectize.setValue('%s')", v))); Sys.sleep(1.5) }
cerrar_modal <- function() {
  Sys.sleep(0.8)
  if (!nzchar(modal())) return(invisible(FALSE))
  clic("document.querySelector('.modal.show .modal-footer [data-bs-dismiss=modal], .modal.show .modal-footer [data-dismiss=modal]')")
  poll_until(function() !isTRUE(ev("!!document.querySelector('.modal.show, .modal-backdrop')")), timeout_s = 10); Sys.sleep(0.3)
}
abrir <- function(n, puerto, sesion = NULL, sin_permiso = FALSE) {
  if (is.null(sesion)) {
    if (!is.null(app)) stop_bg_app(app)
    app <<- tryCatch(start_bg_app(sprintf('Sys.setenv(BANCOS_N = "%d", BANCOS_SIN_PERMISO = "%s"); setwd("%s"); source("tests/helpers/bancos_fixture_app.R")$value',
                                          n, if (isTRUE(sin_permiso)) "1" else "0", repo), puerto, timeout_s = 240),
                     error = function(e) { message(conditionMessage(e)); NULL })
    if (is.null(app)) return(FALSE)
  }
  invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", puerto), wait_ = TRUE))
  r <- poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s') && HDT.estado('%s').visibles.length > 0", T, T))), timeout_s = 60)
  Sys.sleep(1); r
}

# ══ A · What shows ═══════════════════════════════════════════════════════════
b <- new_chromote_session()
ok("fixture app started (3 accounts, 60 movements)", abrir(0L, 38961L))
periodo("todo")
ok("A1: every movement but the commission shows (59; commissions hidden by default)",
   poll_until(function() length(vis()) == 59L, timeout_s = 20) && !"MOV-0003" %in% vis())
ok("A2: columns Empresa, Fecha y hora, Moneda, Tipo, Descripción, Cargo, Abono, Saldo (Recibo optional, hidden)",
   identical(titulos(), c("Empresa", "Fecha y hora", "Moneda", "Tipo", "Descripción", "Cargo", "Abono", "Saldo")))
ok("A3: the counter says 59 movimientos",
   poll_until(function() grepl("59 movimientos", ev(sprintf("document.getElementById('%s').innerText", T)), fixed = TRUE), timeout_s = 10))
ok("A4: the reconciled one shows the Conciliado badge",
   isTRUE(ev(sprintf("/Conciliado/.test(HDT.fila('%s','MOV-0004').innerText)", T))))
ok("A5: ✎ only on the manual entry (MOV-0005)",
   poll_until(function() isTRUE(ev(sprintf("!!HDT.fila('%s','MOV-0005')", T))), timeout_s = 10) && isTRUE(ev(sprintf("!!HDT.fila('%s','MOV-0005').querySelector('button[data-accion=editar]') && !HDT.fila('%s','MOV-0001').querySelector('button[data-accion=editar]')", T, T))))
clic("document.getElementById('bnc-btn_comisiones_toggle')"); Sys.sleep(1)
ok("A6: 'Sin comisiones' -> 'Con comisiones': the commission shows, with the Comisión badge",
   poll_until(function() "MOV-0003" %in% vis(), timeout_s = 5) &&
     isTRUE(ev(sprintf("/Comisión/.test(HDT.fila('%s','MOV-0003').innerText)", T))))
clic("document.getElementById('bnc-btn_comisiones_toggle')"); Sys.sleep(1)
cuenta("cta_nts_mxn")
ok("A7: one account: only its movements, and the Empresa column goes",
   poll_until(function() !"MOV-0007" %in% vis() && "MOV-0001" %in% vis(), timeout_s = 5) && !"Empresa" %in% titulos())
cuenta("__todas__")
ok("A8: every account again: Empresa is back", poll_until(function() "Empresa" %in% titulos(), timeout_s = 5))

invisible(b$Browser$setDownloadBehavior(behavior = "allow", downloadPath = normalizePath(tempdir(), winslash = "/")))
clic(sprintf("Array.from(document.querySelectorAll('#%s button')).filter(function(x){ return /Exportar a Excel/.test(x.textContent); })[0]", T))
ok("A9: Exportar a Excel (the engine's) is logged as a generated file, with the rows exported (59)",
   poll_until(function() grepl("bancos_libro_excel (tabla): 59 filas", registro(), fixed = TRUE), timeout_s = 10))
buscar <- function(q) { invisible(ev(sprintf("var i = document.querySelector('#%s .hdt-buscar'); i.value = '%s'; i.dispatchEvent(new Event('input'))", T, q))); Sys.sleep(0.8) }
clic("document.getElementById('bnc-download_libro')")
ok("A10: Exportar (formato BanBajío) is back, logged too (59 rows, the bar's filters)",
   poll_until(function() grepl("bancos_libro_excel (banbajio): 59 filas", registro(), fixed = TRUE), timeout_s = 10))
buscar("aceros")
clic("document.getElementById('bnc-download_libro')")
ok("A11: with a search, the BanBajío Excel takes only what it leaves (1 row)",
   poll_until(function() grepl("bancos_libro_excel (banbajio): 1 filas", registro(), fixed = TRUE), timeout_s = 10))
buscar("")

# ══ B · Marking and Eliminar ═════════════════════════════════════════════════
ok("B0: nothing marked: no Eliminar in the bar",
   !isTRUE(ev(sprintf("(function(){ var b = document.querySelector('#%s .hdt-acciones button[data-accion=eliminar]'); return !!b && b.offsetParent !== null; })()", T))))
clic(celda("MOV-0001")); clic(celda("MOV-0002"))
ok("B1: two rows marked, by id", setequal(sel(), c("MOV-0001", "MOV-0002")))
ok("B2: Eliminar shows in the bar",
   isTRUE(ev(sprintf("(function(){ var b = document.querySelector('#%s .hdt-acciones button[data-accion=eliminar]'); return !!b && b.offsetParent !== null; })()", T))))
clic(sprintf("document.querySelector('#%s .hdt-acciones button[data-accion=eliminar]')", T))
ok("B3: the confirmation asks '¿Eliminar 2 movimientos?'",
   poll_until(function() grepl("Eliminar 2 movimientos", modal(), fixed = TRUE), timeout_s = 10))
ok("B4: its list (eliminar_mov on the engine) holds exactly those two",
   poll_until(function() setequal(tryCatch(vis(E), error = function(e) NULL), c("MOV-0001", "MOV-0002")), timeout_s = 10))
cerrar_modal()
ok("B5: Cancelar deletes nothing", !length(eliminados()) && setequal(sel(), c("MOV-0001", "MOV-0002")))
clic(celda("MOV-0002")); clic(celda("MOV-0010"), mod = 8L)   # shift + click: a range
ok("B6: shift + click marks the range by id", length(sel()) > 2L && all(c("MOV-0001", "MOV-0010") %in% sel()))
n_rango <- length(sel())
clic(sprintf("document.querySelector('#%s .hdt-acciones button[data-accion=eliminar]')", T))
ok("B7: the confirmation lists the range", poll_until(function() length(tryCatch(vis(E), error = function(e) NULL)) == n_rango, timeout_s = 10))

# ══ C · Confirming ═══════════════════════════════════════════════════════════
marcadas <- sel()
Sys.sleep(0.8)
clic("document.getElementById('bnc-do_eliminar_confirm')")
ok("C1: the store holds exactly the marked ones as deleted",
   poll_until(function() setequal(eliminados(), marcadas), timeout_s = 10))
ok("C2: the table drops them at once, and nothing stays marked",
   poll_until(function() !any(marcadas %in% vis()) && !length(sel()), timeout_s = 5))

# ══ D · ✎ ════════════════════════════════════════════════════════════════════
clic(sprintf("HDT.fila('%s','MOV-0030').querySelector('button[data-accion=editar]')", T))
ok("D1: ✎ opens the manual entry's form, filled in (4500)",
   poll_until(function() nzchar(modal()), timeout_s = 10) &&
     isTRUE(ev("(function(){ var m = document.querySelector('.modal.show'); return !!m && Array.from(m.querySelectorAll('input')).some(function(i){ return /4500/.test(i.value); }); })()")))
ok("D2: ...and marked nothing", !length(sel()))
cerrar_modal()

# ══ H · A save that fails ════════════════════════════════════════════════════
avisos <- function() unlist(ev("Array.from(document.querySelectorAll('.shiny-notification')).map(function(n){ return n.innerText; })"))
fallar <- function(...) { invisible(ev(sprintf("Shiny.setInputValue('fallar_escritura', %s, {priority: 'event'})",
                                               jsonlite::toJSON(c(...))))); Sys.sleep(0.5) }
quitar_avisos <- function() invisible(ev("document.querySelectorAll('.shiny-notification').forEach(function(n){ n.remove(); })"))
fallar("acme/bancos_movimientos.rds"); quitar_avisos()
clic(celda("MOV-0040"))
clic(sprintf("document.querySelector('#%s .hdt-acciones button[data-accion=eliminar]')", T))
invisible(poll_until(function() grepl("Eliminar 1 movimiento", modal(), fixed = TRUE), timeout_s = 10)); Sys.sleep(0.8)
clic("document.getElementById('bnc-do_eliminar_confirm')")
ok("H1: S3 refuses the delete: 'No se pudo guardar', and nothing says it was deleted",
   poll_until(function() any(grepl("No se pudo guardar", avisos(), fixed = TRUE)), timeout_s = 10) &&
     !any(grepl("eliminado", avisos(), fixed = TRUE)))
ok("H2: ...the confirmation stays open, the row stays in the table and in the store",
   grepl("Eliminar 1 movimiento", modal(), fixed = TRUE) && "MOV-0040" %in% vis() && !"MOV-0040" %in% eliminados())
fallar(); quitar_avisos()
clic("document.getElementById('bnc-do_eliminar_confirm')")
ok("H3: S3 back: the same confirmation deletes it now",
   poll_until(function() "MOV-0040" %in% eliminados() && !"MOV-0040" %in% vis(), timeout_s = 10) &&
     any(grepl("1 movimiento eliminado", avisos(), fixed = TRUE)))
fallar("acme/bancos_papelera.rds"); quitar_avisos()
invisible(poll_until(function() !nzchar(modal()), timeout_s = 10)); Sys.sleep(0.5)
clic(celda("MOV-0041"))
clic(sprintf("document.querySelector('#%s .hdt-acciones button[data-accion=eliminar]')", T))
invisible(poll_until(function() grepl("Eliminar 1 movimiento", modal(), fixed = TRUE), timeout_s = 10)); Sys.sleep(0.8)
clic("document.getElementById('bnc-do_eliminar_confirm')")
ok("H4: the archive in the Bancos trash fails: the movement is deleted, and the user is told the archive failed",
   poll_until(function() "MOV-0041" %in% eliminados(), timeout_s = 10) &&
     poll_until(function() any(grepl("no se pudieron archivar", avisos(), fixed = TRUE)), timeout_s = 10))
fallar()
invisible(poll_until(function() !nzchar(modal()), timeout_s = 10)); Sys.sleep(0.5)

# ══ E · Two sessions ═════════════════════════════════════════════════════════
bA <- b
bB <- new_chromote_session()
en <- function(s, expr) { b <<- s; on.exit(b <<- bA); force(expr) }
invisible(en(bB, { abrir(0L, 38961L, sesion = "B"); periodo("todo") }))
clic(celda("MOV-0020")); clic(celda("MOV-0021"))
ok("E1: A marks MOV-0020 and MOV-0021", setequal(sel(), c("MOV-0020", "MOV-0021")))
invisible(en(bB, {
  clic(celda("MOV-0020"))
  clic(sprintf("document.querySelector('#%s .hdt-acciones button[data-accion=eliminar]')", T))
  poll_until(function() grepl("Eliminar 1 movimiento", modal(), fixed = TRUE), timeout_s = 10); Sys.sleep(0.8)
  clic("document.getElementById('bnc-do_eliminar_confirm')")
  poll_until(function() "MOV-0020" %in% eliminados(), timeout_s = 10)
}))
ok("E2: B deleted MOV-0020; in A it leaves the table and A's marks keep only MOV-0021",
   poll_until(function() !"MOV-0020" %in% vis() && identical(sel(), "MOV-0021"), timeout_s = 10))
clic(sprintf("document.querySelector('#%s .hdt-acciones button[data-accion=eliminar]')", T))
ok("E3: A's confirmation lists only MOV-0021",
   poll_until(function() grepl("Eliminar 1 movimiento", modal(), fixed = TRUE) &&
                identical(tryCatch(vis(E), error = function(e) NULL), "MOV-0021"), timeout_s = 10))
# While A's confirmation is open, B deletes MOV-0021 too.
invisible(en(bB, {
  clic(celda("MOV-0021"))
  clic(sprintf("document.querySelector('#%s .hdt-acciones button[data-accion=eliminar]')", T))
  poll_until(function() grepl("Eliminar 1 movimiento", modal(), fixed = TRUE), timeout_s = 10); Sys.sleep(0.8)
  clic("document.getElementById('bnc-do_eliminar_confirm')")
  poll_until(function() "MOV-0021" %in% eliminados(), timeout_s = 10)
}))
archivados <- function() { r <- grep("^archivados: ", strsplit(registro(), "\n")[[1]], value = TRUE); r <- sub("^archivados: ", "", r)
                           if (length(r) && nzchar(r)) strsplit(r, ",")[[1]] else character(0) }
invisible(poll_until(function() sum(archivados() == "MOV-0021") == 1L, timeout_s = 10))
clic("document.getElementById('bnc-do_eliminar_confirm')")
ok("E4: A's confirmation then says they are gone, and closes",
   poll_until(function() !nzchar(modal()) &&
                any(grepl("otra sesión los eliminó", unlist(ev("Array.from(document.querySelectorAll('.shiny-notification')).map(function(n){ return n.innerText; })")),
                          fixed = TRUE)), timeout_s = 10))
ok("E5: MOV-0021 is archived once, not twice (VP4)", sum(archivados() == "MOV-0021") == 1L)
try(bB$close(), silent = TRUE)

# ══ F · 900 more movements ═══════════════════════════════════════════════════
ok("F0: fixture with 900 more movements", abrir(900L, 38962L))
periodo("todo")
ok("F1: all of them, in window mode", poll_until(function() length(vis()) == 959L, timeout_s = 20) && identical(estado(T)$modo, "ventana"))
ok("F2: the oldest is not painted before searching", is.null(ev(sprintf("HDT.fila('%s', 'MOV-1900')", T))))
buscar <- function(q) { invisible(ev(sprintf("var i = document.querySelector('#%s .hdt-buscar'); i.value = '%s'; i.dispatchEvent(new Event('input'))", T, q))); Sys.sleep(0.6) }
buscar("ultimo")
ok("F3: the search finds it by supplier", identical(vis(), "MOV-1900"))
buscar("999999")
ok("F4: and by receipt (in the description)", identical(vis(), "MOV-1900"))

# ══ G · Without can_manage_bancos ════════════════════════════════════════════
ok("G0: fixture whose user lacks can_manage_bancos", abrir(0L, 38964L, sin_permiso = TRUE))
periodo("todo")
ok("G1: no ✎, not even on the manual entries", isTRUE(ev(sprintf("!document.querySelector('#%s button[data-accion=editar]')", T))))
ok("G2: no ＋ Agregar", isTRUE(ev("(function(){ var b = document.getElementById('bnc-add_mov_manual'); return !b || b.offsetParent === null; })()")))
clic(celda("MOV-0001"))
ok("G3: rows can still be marked (counter, sums), but the bar's Eliminar doesn't show",
   identical(sel(), "MOV-0001") &&
     !isTRUE(ev(sprintf("(function(){ var b = document.querySelector('#%s .hdt-acciones button[data-accion=eliminar]'); return !!b && b.offsetParent !== null; })()", T))))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) {
  cat("\n-- app stderr (tail) --\n"); try(cat(tail(readLines(app$proc$get_error_file(), warn = FALSE), 25), sep = "\n"))
  quit(status = 1L)
}
