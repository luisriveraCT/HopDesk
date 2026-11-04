# =============================================================================
# tests/test_bancos_t12b_chromote.R
# Bancos' other tables on hd_table (T12b), in a real headless Chrome,
# against tests/helpers/bancos_fixture_app.R (the real bancosServer on the
# CM09's in-memory S3).
#   C. Cuentas (cuentas_bancos, D): the active accounts, read only, Excel.
#   R. Reasignar (reasignar, E): the chosen import session's movements, all.
#   I. Importar TXT (importar_txt, E): a 25-movement TXT with 2 commissions
#      -> the preview shows the 25 (it used to show 20, without commissions)
#      and the button imports those 25.
#   H. Historial (historial_conf, C): confirmations and abonos by id; ↶
#      asks first (T12 P5); two sessions undoing the same confirmation.
#   P. Papelera (papelera, C): its four sources by id; ↶ only on a deleted
#      manual movement, asks first; two sessions restoring the same one.
#   S. Without can_manage_bancos: neither table shows a button that writes.
#
# The fixture runs with BANCOS_PAPELERA=1 (two deleted movements). Its
# confirmations, abonos and trash are shared by every session.
#
# Run from project root:  Rscript tests/test_bancos_t12b_chromote.R
# =============================================================================
cat("=== test_bancos_t12b_chromote ===\n\n")

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
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
estado <- function(t) ev(sprintf("HDT.estado('%s')", t))
vis <- function(t) unlist(estado(t)$visibles)
centro <- function(js_el) ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', behavior:'instant'});
  var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
raton <- function(tipo, p, botones = 0L) b$Input$dispatchMouseEvent(type = tipo, x = p[[1]], y = p[[2]],
  button = if (tipo == "mouseMoved" && !botones) "none" else "left", buttons = botones, clickCount = 1L)
clic <- function(js_el) { p <- centro(js_el); raton("mouseMoved", p); raton("mousePressed", p, 1L); raton("mouseReleased", p); Sys.sleep(0.4) }
titulos <- function(t) unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.dataset.col != null; }).map(function(th){ return th.firstChild.textContent; })", t)))
pestana <- function(v) { clic(sprintf("document.querySelector('a[data-value=\"%s\"]')", v)); Sys.sleep(1) }
sub_pestana <- function(t) { clic(sprintf("Array.from(document.querySelectorAll('.nav-link')).filter(function(a){ return a.textContent.trim() === '%s'; })[0]", t)); Sys.sleep(1) }

app <- tryCatch(start_bg_app(sprintf('Sys.setenv(BANCOS_PAPELERA = "1"); setwd("%s"); source("tests/helpers/bancos_fixture_app.R")$value', repo), 38981L,
                             timeout_s = 240), error = function(e) { message(conditionMessage(e)); NULL })
ok("fixture app started", !is.null(app))
b <- new_chromote_session()
invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
invisible(b$Page$navigate("http://127.0.0.1:38981", wait_ = TRUE))
ok("the page loads (the Libro's table is there)",
   poll_until(function() isTRUE(ev("!!window.HDT && !!HDT.estado('bnc-libro-hdt')")), timeout_s = 180))
Sys.sleep(1)

# ══ C · Cuentas ══════════════════════════════════════════════════════════════
C <- "bnc-cuentas-hdt"
pestana("cuentas")
ok("C1: the 3 active accounts, by company, bank and currency",
   poll_until(function() length(tryCatch(vis(C), error = function(e) NULL)) == 3L, timeout_s = 15))
ok("C2: columns Empresa, Banco, Moneda, Alias, Cuenta, CLABE",
   identical(titulos(C), c("Empresa", "Banco", "Moneda", "Alias", "Cuenta", "CLABE")))
ok("C3: Exportar a Excel is there (Luis, T12 P7)",
   isTRUE(ev(sprintf("Array.from(document.querySelectorAll('#%s button')).some(function(x){ return /Exportar a Excel/.test(x.textContent); })", C))))
ok("C4: read only: no row buttons", isTRUE(ev(sprintf("!document.querySelector('#%s button[data-accion]')", C))))

# ══ R · Reasignar ════════════════════════════════════════════════════════════
R <- "bnc-reasig-hdt"
pestana("historial"); sub_pestana("Papelera")
clic("document.querySelector('[data-bs-target=\"#bnc-reasignar_panel\"]')"); Sys.sleep(1)
ses <- unlist(ev("Array.from(document.querySelectorAll('#bnc-reasig_sesion option')).map(function(o){ return o.value; }).filter(function(v){ return v; })"))
invisible(ev(sprintf("(function(){ var s = document.getElementById('bnc-reasig_sesion'); if (s.selectize) s.selectize.setValue('%s'); else { s.value = '%s'; $(s).trigger('change'); } })()",
                     ses[1], ses[1])))
ok("R1: the session's 60 movements, all of them (no pages of 10)",
   poll_until(function() length(tryCatch(vis(R), error = function(e) NULL)) == 60L, timeout_s = 15))
ok("R2: columns Fecha, Tipo, Parte, Concepto, Cargo, Abono",
   identical(titulos(R), c("Fecha", "Tipo", "Parte", "Concepto", "Cargo", "Abono")))

# ══ I · Importar TXT ═════════════════════════════════════════════════════════
I <- "bnc-import-hdt"
txt <- local({
  movs <- c(lapply(1:23, function(i) list(desc = sprintf("SPEI Enviado: | Beneficiario: PROV %02d SA | Recibo # %d", i, i), cargo = 100 + i)),
            lapply(1:2, function(i) list(desc = sprintf("Comision por SPEI | Recibo # %d", 90 + i), cargo = 5)))
  cg <- sum(vapply(movs, `[[`, numeric(1), "cargo"))
  hdr <- sprintf("NETWORKS TRUCKING SERVICES SA DE CV,0111,Conecta BanBajio,01,26-Oct-2026,12:16:39,Mes Actual,TODOS,01-Oct-2026 al 31-Oct-2026,,%d,%.2f,%.2f",
                 length(movs), cg, 0)
  filas <- vapply(seq_along(movs), function(i) sprintf("%d,%02d-Oct-2026,12:00:%02d,%d,%s,%.2f,,%.2f", i, 1 + i %% 28, i %% 60,
                                                      700000 + i, movs[[i]]$desc, movs[[i]]$cargo, 1000), character(1))
  f <- tempfile(fileext = ".txt"); writeLines(c(hdr, "#,Fecha Movimiento,Hora,Recibo,Descripción,Cargos,Abonos,Saldo", filas), f); f
})
pestana("importar")
doc <- b$DOM$getDocument(); nodo <- b$DOM$querySelector(doc$root$nodeId, "#bnc-txt_file")
invisible(b$DOM$setFileInputFiles(files = list(normalizePath(txt)), nodeId = nodo$nodeId)); Sys.sleep(2)
ok("I1: the preview shows all 25 new movements, the 2 commissions included",
   poll_until(function() length(tryCatch(vis(I), error = function(e) NULL)) == 25L, timeout_s = 20) &&
     isTRUE(ev(sprintf("/Comisi/.test(document.getElementById('%s').innerText)", I))))
ok("I2: ...and the button imports exactly those 25",
   poll_until(function() grepl("Importar 25 movimientos", ev("(document.getElementById('bnc-import_btn_ui') || {}).innerText || ''"),
                               fixed = TRUE), timeout_s = 10))

# ══ H · Historial de confirmaciones ══════════════════════════════════════════
H <- "bnc-historial-hdt"
registro <- function() ev("document.getElementById('registro').textContent")
reg <- function(campo) { r <- grep(paste0("^", campo, ": "), strsplit(registro(), "\n")[[1]], value = TRUE)
                         r <- sub(paste0("^", campo, ": "), "", r)
                         if (length(r) && nzchar(r)) strsplit(r, ",")[[1]] else character(0) }
boton <- function(t, id, acc) sprintf("HDT.fila('%s','%s').querySelector('button[data-accion=%s]')", t, id, acc)
tiene_boton <- function(t, id, acc) isTRUE(ev(sprintf("!!HDT.fila('%s','%s') && !!%s", t, id, boton(t, id, acc))))
barra_conf <- function(t) isTRUE(ev(sprintf("(function(){ var c = document.querySelector('#%s .hdt-confirmar'); return !!c && c.offsetParent !== null; })()", t)))
confirmar_barra <- function(t) clic(sprintf("document.querySelector('#%s .hdt-confirmar-si')", t))
modal <- function() ev("(document.querySelector('.modal.show') || {}).innerText || ''")
sin_modal <- function() poll_until(function() !isTRUE(ev("!!document.querySelector('.modal.show, .modal-backdrop')")), timeout_s = 10)

pestana("historial"); sub_pestana("Historial de confirmaciones")
ok("H1: two live confirmations and the active abono; not the deleted one nor the undo's own row",
   poll_until(function() setequal(tryCatch(vis(H), error = function(e) NULL), c("conf:CONF-1", "conf:CONF-2", "abono:AB-1")),
              timeout_s = 15))
ok("H2: columns Confirmado el, Fecha, Empresa, Parte, Documento, Importe, Moneda, Tipo, Origen",
   identical(titulos(H), c("Confirmado el", "Fecha", "Empresa", "Parte", "Documento", "Importe", "Moneda", "Tipo", "Origen")))
ok("H3: newest confirmed first (T12 P6)", identical(vis(H), c("conf:CONF-1", "abono:AB-1", "conf:CONF-2")))
ok("H4: the engine's filters by Empresa and Tipo (T12 P4), and the badges Pago / Cobro / Abono",
   identical(unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s button.hdt-filtro')).map(function(x){ return x.textContent.trim(); })", H))),
             c("Empresa", "Tipo")) &&
     isTRUE(ev(sprintf("/Pago/.test(HDT.fila('%s','conf:CONF-1').innerText) && /Cobro/.test(HDT.fila('%s','conf:CONF-2').innerText) && /Abono/.test(HDT.fila('%s','abono:AB-1').innerText)", H, H, H))))
ok("H5: ↶ and ✕ on a confirmation, only ↶ on the abono",
   tiene_boton(H, "conf:CONF-1", "deshacer") && tiene_boton(H, "conf:CONF-1", "eliminar") &&
     tiene_boton(H, "abono:AB-1", "deshacer") && !tiene_boton(H, "abono:AB-1", "eliminar"))

# Two sessions: A asks to undo CONF-1; before A confirms, B undoes it.
bA <- b
bB <- new_chromote_session()
en <- function(s, expr) { b <<- s; on.exit(b <<- bA); force(expr) }
invisible(en(bB, {
  invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
  invisible(b$Page$navigate("http://127.0.0.1:38981", wait_ = TRUE))
  poll_until(function() isTRUE(ev("!!window.HDT && !!HDT.estado('bnc-libro-hdt')")), timeout_s = 60); Sys.sleep(1)
  pestana("historial"); sub_pestana("Historial de confirmaciones")
  poll_until(function() "conf:CONF-1" %in% tryCatch(vis(H), error = function(e) NULL), timeout_s = 15)
}))
clic(boton(H, "conf:CONF-1", "deshacer"))
ok("H6: ↶ asks first (T12 P5): the confirmation bar shows and nothing is undone yet",
   poll_until(function() barra_conf(H), timeout_s = 5) && !"CONF-1" %in% reg("conf_deshechas"))
invisible(en(bB, {
  clic(boton(H, "conf:CONF-1", "deshacer")); poll_until(function() barra_conf(H), timeout_s = 5)
  confirmar_barra(H)
  poll_until(function() "CONF-1" %in% reg("conf_deshechas"), timeout_s = 10)
}))
ok("H7: B undid CONF-1; in A it leaves the table", poll_until(function() !"conf:CONF-1" %in% vis(H), timeout_s = 10))
if (barra_conf(H)) confirmar_barra(H)
Sys.sleep(1.5)
ok("H8: A's late confirm undoes nothing twice (CONF-1 reversed once)", sum(reg("conf_deshechas") == "CONF-1") == 1L)
try(bB$close(), silent = TRUE)

clic(boton(H, "abono:AB-1", "deshacer"))
ok("H9: ↶ on the abono asks first", poll_until(function() barra_conf(H), timeout_s = 5) && !"AB-1" %in% reg("abonos_anulados"))
confirmar_barra(H)
ok("H10: ...and once confirmed the abono is voided and leaves the table",
   poll_until(function() "AB-1" %in% reg("abonos_anulados") && !"abono:AB-1" %in% vis(H), timeout_s = 10))
clic(boton(H, "conf:CONF-2", "eliminar"))
invisible(poll_until(function() barra_conf(H), timeout_s = 5)); confirmar_barra(H)
ok("H11: ✕ on a confirmation opens its modal (what goes with it), by that row's id",
   poll_until(function() grepl("Eliminar esta confirmaci", modal()) && grepl("CLIENTE DOS SA", modal(), fixed = TRUE), timeout_s = 10))
Sys.sleep(0.8); clic("document.getElementById('bnc-do_delete_conf_confirm')")
ok("H12: ...and Eliminar moves CONF-2 to the trash",
   poll_until(function() "CONF-2" %in% reg("conf_eliminadas") && !"conf:CONF-2" %in% vis(H), timeout_s = 10))
sin_modal()

# ══ P · Papelera ═════════════════════════════════════════════════════════════
# By now: MOV-0901 (manual) and MOV-0902 (imported) deleted in the fixture;
# CONF-3 deleted there, CONF-2 deleted in H12, CONF-1's original row
# soft-deleted by its undo in H7; and the Calendario's EV-77.
P <- "bnc-papelera-hdt"
eliminados <- function() reg("eliminados")
sub_pestana("Papelera")
ok("P1: the four sources, by id",
   poll_until(function() setequal(tryCatch(vis(P), error = function(e) NULL),
                                  c("mov:MOV-0901", "mov:MOV-0902", "conf:CONF-1", "conf:CONF-2", "conf:CONF-3", "cal:EV-77")),
              timeout_s = 15))
ok("P2: columns Origen, Fecha, Empresa, Parte, Importe, Moneda, Tipo, Doc. vinculado, Eliminado el",
   identical(titulos(P), c("Origen", "Fecha", "Empresa", "Parte", "Importe", "Moneda", "Tipo", "Doc. vinculado", "Eliminado el")))
ok("P3: the Calendario's row: Tipo Pago (its ledger), the document under Doc. vinculado (they were crossed)",
   isTRUE(ev(sprintf("(function(){ var t = HDT.fila('%s','cal:EV-77').innerText; return /Pago/.test(t) && /DOC-77/.test(t) && /PROVEEDOR CALENDARIO SA/.test(t); })()", P))))
ok("P4: ↶ only on the deleted manual movement",
   tiene_boton(P, "mov:MOV-0901", "deshacer") && !tiene_boton(P, "mov:MOV-0902", "deshacer") &&
     !tiene_boton(P, "conf:CONF-3", "deshacer") && !tiene_boton(P, "cal:EV-77", "deshacer"))

# Two sessions: A asks to restore MOV-0901; before A confirms, B restores it.
bB <- new_chromote_session()
invisible(en(bB, {
  invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
  invisible(b$Page$navigate("http://127.0.0.1:38981", wait_ = TRUE))
  poll_until(function() isTRUE(ev("!!window.HDT && !!HDT.estado('bnc-libro-hdt')")), timeout_s = 60); Sys.sleep(1)
  pestana("historial"); sub_pestana("Papelera")
  poll_until(function() "mov:MOV-0901" %in% tryCatch(vis(P), error = function(e) NULL), timeout_s = 15)
}))
clic(boton(P, "mov:MOV-0901", "deshacer"))
ok("P5: ↶ asks first: the confirmation bar shows and the movement is still deleted",
   poll_until(function() barra_conf(P), timeout_s = 5) && "MOV-0901" %in% eliminados())
invisible(en(bB, {
  clic(boton(P, "mov:MOV-0901", "deshacer")); poll_until(function() barra_conf(P), timeout_s = 5)
  confirmar_barra(P)
  poll_until(function() !"MOV-0901" %in% eliminados(), timeout_s = 10)
}))
ok("P6: B restored MOV-0901; in A it leaves the Papelera", poll_until(function() !"mov:MOV-0901" %in% vis(P), timeout_s = 10))
if (barra_conf(P)) confirmar_barra(P)
Sys.sleep(1.5)
ok("P7: A's late confirm changes nothing (still restored, the rest still deleted)",
   !"MOV-0901" %in% eliminados() && "MOV-0902" %in% eliminados() && "conf:CONF-3" %in% vis(P))
try(bB$close(), silent = TRUE)

# ══ S · Without can_manage_bancos ════════════════════════════════════════════
stop_bg_app(app)
app <- tryCatch(start_bg_app(sprintf('Sys.setenv(BANCOS_PAPELERA = "1", BANCOS_SIN_PERMISO = "1"); setwd("%s"); source("tests/helpers/bancos_fixture_app.R")$value', repo),
                             38982L, timeout_s = 240), error = function(e) { message(conditionMessage(e)); NULL })
invisible(b$Page$navigate("http://127.0.0.1:38982", wait_ = TRUE))
ok("S0: fixture whose user lacks can_manage_bancos",
   !is.null(app) && poll_until(function() isTRUE(ev("!!window.HDT && !!HDT.estado('bnc-libro-hdt')")), timeout_s = 120))
Sys.sleep(1)
pestana("historial"); sub_pestana("Historial de confirmaciones")
ok("S1: the Historial lists the same rows, with no ↶ nor ✕",
   poll_until(function() length(tryCatch(vis(H), error = function(e) NULL)) == 3L, timeout_s = 15) &&
     isTRUE(ev(sprintf("!document.querySelector('#%s button[data-accion]')", H))))
sub_pestana("Papelera")
ok("S2: the Papelera lists its rows, with no ↶ (not even on the manual movement)",
   poll_until(function() "mov:MOV-0901" %in% tryCatch(vis(P), error = function(e) NULL), timeout_s = 15) &&
     isTRUE(ev(sprintf("!document.querySelector('#%s button[data-accion]')", P))))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) {
  cat("\n-- app stderr (tail) --\n"); try(cat(tail(readLines(app$proc$get_error_file(), warn = FALSE), 25), sep = "\n"))
  quit(status = 1L)
}
