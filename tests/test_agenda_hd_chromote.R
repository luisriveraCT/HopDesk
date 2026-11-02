# =============================================================================
# tests/test_agenda_hd_chromote.R
# Agenda de Hoy's Pagos and Cobros tables on hd_table (T06), in a real
# headless Chrome, against tests/helpers/agenda_fixture_app.R (the real
# pagarHoyServer over fixed data; confirming, Quitar and the payment file
# write to #registro instead of S3).
#   A. What shows: the catalog badge, the account, the abono, the row
#      buttons by status, Cobros, the currency switch.
#   B. Selection by id: sort by another column, mark two rows, Confirmar ->
#      exactly those two are confirmed (the integrator's requirement 1).
#   C. Nothing marked + a search -> Confirmar and the payment file take only
#      what the search left; no search -> the whole currency (Luis).
#   D. Confirming the currency's last row: the table empties, nothing stays
#      marked and the header says "Sin pagos pendientes", together (the
#      integrator's acceptance criterion).
#   E. Quitar (Cobros) takes the marked row out, by id.
#   F. The ⇕ / 🔍 row buttons reach the module's own handlers.
#   H. Editar -> "Ver datos de origen": the raw SAP rows of the marked one.
#   I. Two sessions (the integrator): B confirms what A had marked -> A's
#      Confirmar warns and writes nothing; B confirms one of A's two marks
#      -> A's preview and confirm modal say so; only the one left is confirmed;
#      B confirms what A had open in the payment preview -> A's Descargar
#      warns and writes no payment.
#   J. Two companies with the same document 4001: each row, its payment
#      preview, its PPL line and the .zip preview carry its own company's
#      Factura (Referencia / concepto).
#   G. 850 payments (window mode): the search finds the last one by
#      supplier, reference and amount; marked, it is the one confirmed and
#      the one in the payment file (Luis: "buscar todo lo que esté en la
#      Agenda").
#
# Run from project root:  Rscript tests/test_agenda_hd_chromote.R
# =============================================================================
cat("=== test_agenda_hd_chromote ===\n\n")

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
arrancar <- function(n, puerto) {
  if (!is.null(app)) stop_bg_app(app)
  app <<- tryCatch(start_bg_app(sprintf('Sys.setenv(AGENDA_N = "%d"); setwd("%s"); source("tests/helpers/agenda_fixture_app.R")$value',
                                        n, repo), puerto, timeout_s = 90), error = function(e) { message(conditionMessage(e)); NULL })
  if (is.null(app)) return(FALSE)
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", puerto), wait_ = TRUE))
  r <- poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s') && HDT.estado('%s').visibles.length > 0", AP, AP))),
                  timeout_s = 60)
  Sys.sleep(1); r
}

b <- new_chromote_session()
invisible(b$Emulation$setDeviceMetricsOverride(width = 1400L, height = 1000L, deviceScaleFactor = 1, mobile = FALSE))
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
EMP <- "Networks Trucking Services"
AP <- "ph-tbl_ap_Networks_Trucking_Services-hdt"; AR <- "ph-tbl_ar_Networks_Trucking_Services-hdt"
estado <- function(t) ev(sprintf("HDT.estado('%s')", t))
vis <- function(t) unlist(estado(t)$visibles)
sel <- function(t) unlist(estado(t)$seleccion)
centro <- function(js_el) ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', behavior:'instant'});
  var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
raton <- function(tipo, p, botones = 0L) b$Input$dispatchMouseEvent(type = tipo, x = p[[1]], y = p[[2]],
  button = if (tipo == "mouseMoved" && !botones) "none" else "left", buttons = botones, clickCount = 1L)
clic <- function(js_el) { p <- centro(js_el); raton("mouseMoved", p); raton("mousePressed", p, 1L); raton("mouseReleased", p); Sys.sleep(0.4) }
celda <- function(t, id, k = 2L) sprintf("HDT.fila('%s', '%s').querySelectorAll('td')[%d]", t, id, k)
boton_fila <- function(t, id, a) sprintf("HDT.fila('%s', '%s').querySelector('button[data-accion=%s]')", t, id, a)
por_id <- function(id) sprintf("document.getElementById('ph-%s')", id)
buscar <- function(t, q) { invisible(ev(sprintf("var i = document.querySelector('#%s .hdt-buscar'); i.value = '%s'; i.dispatchEvent(new Event('input'))", t, q))); Sys.sleep(0.6) }
registro <- function() strsplit(ev("document.getElementById('registro').textContent") %||% "", "\n")[[1]]
nuevas <- function(antes) { r <- registro(); r[seq_along(r) > antes] }
encabezado <- function(libro = "ap") ev(sprintf("document.getElementById('ph-hdr_%s_%s').innerText", libro, EMP))
modal <- function() ev("(function(){ var m = document.querySelector('.modal.show'); return m ? m.innerText : ''; })()")
cerrar_modal <- function() {
  Sys.sleep(0.8)   # Bootstrap ignores a close while the modal is still fading in
  clic("document.querySelector('.modal.show .modal-footer [data-bs-dismiss=modal], .modal.show .modal-footer [data-dismiss=modal]')")
  poll_until(function() !isTRUE(ev("!!document.querySelector('.modal.show, .modal-backdrop')")), timeout_s = 10); Sys.sleep(0.3)
}
# Header Confirmar -> the modal -> its own Confirmar.
confirmar <- function(libro = "ap") {
  clic(por_id(sprintf("confirm_%s_%s", libro, EMP)))
  if (!poll_until(function() nzchar(modal()), timeout_s = 10)) return(NA_character_)
  Sys.sleep(0.5); texto <- modal()
  clic("document.querySelector('.modal.show .modal-footer .btn-success, .modal.show .modal-footer .btn-primary')")
  poll_until(function() !isTRUE(ev("!!document.querySelector('.modal.show, .modal-backdrop')")), timeout_s = 10); Sys.sleep(0.8)
  texto
}

# ══ A · What shows ═══════════════════════════════════════════════════════════
ok("fixture app started (one company, Pagos MXN/USD, Cobros)", arrancar(0L, 38995L))
ok("A1: Pagos MXN shows its 4 pending payments by due date (P2, P3, P1, P4); USD's waits",
   identical(vis(AP), c("P2", "P3", "P1", "P4")))
ok("A2: the catalog badge per row: Sin CLABE / Sin catálogo / Incluido / Incluido",
   identical(unlist(ev(sprintf("['P2','P3','P1','P4'].map(function(id){ return %s.innerText.trim(); })",
                               sprintf("HDT.fila('%s', id).querySelectorAll('td')[0]", AP)))),
             c("Sin CLABE", "Sin catálogo", "Incluido", "Incluido")))
ok("A3: the abono shows its badge (Abono), the others don't",
   isTRUE(ev(sprintf("/Abono/.test(HDT.fila('%s','P4').innerText) && !/Abono/.test(HDT.fila('%s','P1').innerText)", AP, AP))))
ok("A4: the account (alias) shows for a catalog supplier, empty when not in the catalog",
   isTRUE(ev(sprintf("/ACEROS/.test(%s.innerText) && %s.innerText.trim() === ''", celda(AP, "P1", 2L), celda(AP, "P3", 2L)))))
ok("A5: row buttons by status: ⇕ and ✎ on catalog rows, only 🔍 on the one not in the catalog",
   isTRUE(ev(sprintf("!!%s && !!%s && !%s && !!%s && !%s", boton_fila(AP, "P1", "cambiar_cuenta"), boton_fila(AP, "P1", "editar"),
                     boton_fila(AP, "P1", "buscar"), boton_fila(AP, "P3", "buscar"), boton_fila(AP, "P3", "cambiar_cuenta")))))
ok("A6: Cobros MXN shows its 2 collections by due date (C2, C1)", identical(vis(AR), c("C2", "C1")))
ok("A7: the header counts the 4 pending (MXN $ 26,700.00) and the payment file 2 included",
   grepl("4 pendiente(s)", encabezado(), fixed = TRUE) && grepl("26,700.00", encabezado(), fixed = TRUE) &&
     grepl("Archivo de pagos (2)", encabezado(), fixed = TRUE))

# ══ F · Row buttons ══════════════════════════════════════════════════════════
clic(boton_fila(AP, "P3", "buscar"))
ok("F1: 🔍 on the supplier not in the catalog opens the catalog lookup for it",
   poll_until(function() grepl("TRANSPORTES DESCONOCIDOS", modal(), fixed = TRUE), timeout_s = 10))
cerrar_modal()
ok("F2: ...and the click marked nothing", length(sel(AP)) == 0L)
clic(boton_fila(AP, "P1", "editar"))
ok("F3: ✎ opens the same lookup, for that row's supplier",
   poll_until(function() grepl("ACEROS MONTERREY SA", modal(), fixed = TRUE), timeout_s = 10))
cerrar_modal()

# ══ H · Ver datos de origen ══════════════════════════════════════════════════
clic(celda(AP, "P1", 1L))
clic(sprintf("document.querySelector('#%s .hdt-editar')", AP))
clic(sprintf("document.querySelector('#%s .hdt-acciones button[data-accion=crudos]')", AP))
ok("H1: Editar -> Ver datos de origen shows the raw SAP rows of the marked payment only (1 of 4: not 4002, not 4999, not the other company's 4001)",
   poll_until(function() grepl("Datos SAP crudos — 1 filas", modal(), fixed = TRUE), timeout_s = 10))
cerrar_modal()
clic(sprintf("document.querySelector('#%s .hdt-editar')", AP))
clic(celda(AP, "P1", 1L))
ok("H2: Editar off, P1 unmarked again", length(sel(AP)) == 0L)

# ══ B · Selection by id after sorting ════════════════════════════════════════
n0 <- length(registro())
clic(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return /Importe/.test(th.textContent); })[0]", AP))
orden <- vis(AP)
ok("B1: sorted by Importe, the rows are in another order than by date", !identical(orden, c("P2", "P3", "P1", "P4")))
clic(celda(AP, "P1", 1L)); clic(celda(AP, "P3", 1L))
ok("B2: P1 and P3 marked (by id)", setequal(sel(AP), c("P1", "P3")))
ok("B3: the header's payment file counts the marked included one (1)", grepl("Archivo de pagos (1)", encabezado(), fixed = TRUE))
txt <- confirmar()
ok("B4: the modal says 2 payments", grepl("Confirmar 2 pago(s)", txt, fixed = TRUE))
r <- nuevas(n0)
ok("B5: exactly P3 and P1 were confirmed -- not the rows that sat in those positions",
   any(r == "confirmados AP: P3,P1") && any(r == "fuera de la Agenda: P1,P3"))
ok("B6: what's left: P2 and P4", setequal(vis(AP), c("P2", "P4")))

# ══ C · Nothing marked + a search ════════════════════════════════════════════
buscar(AP, "bodegas")
ok("C1: the search leaves P2", identical(vis(AP), "P2") && length(sel(AP)) == 0L)
ok("C1b: ...and the header's payment file counts what the search left (P2 has no CLABE: 0)",
   poll_until(function() grepl("Archivo de pagos (0)", encabezado(), fixed = TRUE), timeout_s = 5))
n0 <- length(registro())
txt <- confirmar()
ok("C2: nothing marked + a search: the modal says 1 payment (what the search left)", grepl("Confirmar 1 pago(s)", txt, fixed = TRUE))
r <- nuevas(n0)
ok("C3: only P2 was confirmed, P4 stays", any(r == "confirmados AP: P2") && !length(vis(AP)))
buscar(AP, "")
ok("C4: search cleared: P4 shows again", identical(vis(AP), "P4"))

# ══ D · The currency's last row ══════════════════════════════════════════════
# Watch every change to the Pagos section: the header must never say there's
# nothing pending while a row is still painted.
invisible(ev(sprintf("(function(){ window.__fotos = []; var sec = document.getElementById('ph-hdr_ap_%s').closest('.ph-section');
  new MutationObserver(function(){ var st = HDT.estado('%s');
    window.__fotos.push({ vacio: /Sin pagos pendientes/.test(document.getElementById('ph-hdr_ap_%s').innerText),
                          filas: document.querySelectorAll('#%s tbody tr:not(.hdt-esp)').length, sel: st.seleccion.length }); })
  .observe(sec, { childList: true, subtree: true, characterData: true }); })()", EMP, AP, EMP, AP)))
n0 <- length(registro())
txt <- confirmar()
ok("D1: nothing marked, no search: the whole currency (1 payment, the abono)", grepl("Confirmar 1 pago(s)", txt, fixed = TRUE))
r <- nuevas(n0)
ok("D2: the abono went to the abonos store and out of the Agenda", any(r == "abonos: 4004") && any(r == "fuera de la Agenda: P4"))
ok("D3: the table is empty, nothing marked, and the header says 'Sin pagos pendientes para MXN'",
   poll_until(function() length(vis(AP)) == 0L && length(sel(AP)) == 0L &&
                grepl("Sin pagos pendientes para MXN", encabezado(), fixed = TRUE), timeout_s = 5))
fotos <- ev("window.__fotos")
ok("D4: never the header's 'nothing pending' with a row still painted or marked (same cycle)",
   length(fotos) > 0L && !any(vapply(fotos, function(f) isTRUE(f$vacio) && (f$filas > 0L || f$sel > 0L), logical(1))))
ok("D5: the table shows its own empty text", isTRUE(ev(sprintf("/Sin pagos en cola/.test(document.querySelector('#%s .hdt-vacio').textContent)", AP))))

# ══ E · Quitar (Cobros) ══════════════════════════════════════════════════════
n0 <- length(registro())
clic(celda(AR, "C1", 1L))
clic(por_id(sprintf("remove_ar_%s", EMP)))
r <- nuevas(n0)
ok("E1: Quitar takes out exactly the marked collection (C1)", any(r == "fuera de la Agenda: C1") && identical(vis(AR), "C2"))
n0 <- length(registro())
clic(por_id(sprintf("remove_ar_%s", EMP)))
ok("E2: Quitar with nothing marked takes nothing out (it asks to mark a row)",
   length(nuevas(n0)) == 0L && identical(vis(AR), "C2"))

# ══ A8 · The currency switch ═════════════════════════════════════════════════
invisible(ev(sprintf("document.getElementById('ph-bal_cur_%s').selectize.setValue('USD')", EMP)))
ok("A8: USD shows its own payments (P5, P6)", poll_until(function() setequal(vis(AP), c("P5", "P6")), timeout_s = 10))

# ══ J · Same document, two companies ═════════════════════════════════════════
invisible(b$Browser$setDownloadBehavior(behavior = "allow", downloadPath = normalizePath(tempdir(), winslash = "/")))
APO <- "ph-tbl_ap_Otra_Empresa_SA-hdt"; OTR <- "Otra Empresa SA"
clic("document.querySelector('a[data-value=\"Otra Empresa SA\"]')")
ok("J1: the other company's tab shows its own 4001 (Q1)",
   poll_until(function() identical(tryCatch(unlist(estado(APO)$visibles), error = function(e) NULL), "Q1"), timeout_s = 10))
ok("J2: its Referencia is its own Factura (F-OTRA), not Networks' F-4001",
   isTRUE(ev(sprintf("/F-OTRA/.test(HDT.fila('%s','Q1').innerText) && !/F-4001/.test(HDT.fila('%s','Q1').innerText)", APO, APO))))
n0 <- length(registro())
clic(sprintf("document.getElementById('ph-dl_preview_%s')", OTR))
ok("J3: its payment preview shows F-OTRA",
   poll_until(function() grepl("F-OTRA", modal(), fixed = TRUE), timeout_s = 10) && !grepl("F-4001", modal(), fixed = TRUE))
Sys.sleep(0.8)
clic(sprintf("document.getElementById('ph-dl_ppl_%s')", OTR))
ok("J4: and its PPL line's concepto is F-OTRA",
   poll_until(function() any(nuevas(n0) == "ppl: 700 F-OTRA"), timeout_s = 10))
if (nzchar(modal())) cerrar_modal()
poll_until(function() !isTRUE(ev("!!document.querySelector('.modal.show, .modal-backdrop')")), timeout_s = 10); Sys.sleep(0.5)
clic("document.getElementById('ph-preview_all')")
ok("J5: the .zip preview (every company) gives each company's 4001 its own Factura",
   poll_until(function() grepl("F-OTRA", modal(), fixed = TRUE), timeout_s = 10))
if (nzchar(modal())) cerrar_modal()

# ══ G · 850 payments ═════════════════════════════════════════════════════════
ok("G0: fixture with 850 more payments", arrancar(850L, 38996L))
ok("G1: all 854 MXN payments are in the table, in window mode",
   length(vis(AP)) == 854L && identical(estado(AP)$modo, "ventana"))
ok("G2: the last one (due last) is not painted before searching", is.null(ev(sprintf("HDT.fila('%s', 'X0850')", AP))))
buscar(AP, "ultimo")
ok("G3: a search by supplier finds it", identical(vis(AP), "X0850"))
buscar(AP, "REF-ULTIMA")
ok("G4: by reference too", identical(vis(AP), "X0850"))
buscar(AP, "950")
ok("G5: and by amount (950.00)", identical(vis(AP), "X0850"))
clic(celda(AP, "X0850", 1L))
ok("G6: marked", identical(sel(AP), "X0850"))
buscar(AP, "")
ok("G7: the mark stays with its id when the search is cleared", identical(sel(AP), "X0850") && length(vis(AP)) == 854L)
ok("G8: the header's payment file counts it (1)", grepl("Archivo de pagos (1)", encabezado(), fixed = TRUE))
n0 <- length(registro())
clic(por_id(sprintf("dl_preview_%s", EMP)))
ok("G9: the payment preview lists that one supplier",
   poll_until(function() grepl("PROVEEDOR ULTIMO SA", modal(), fixed = TRUE), timeout_s = 10) &&
     !grepl("PROVEEDOR 0001 SA", modal(), fixed = TRUE))
cerrar_modal()
txt <- confirmar()
r <- nuevas(n0)
ok("G10: Confirmar takes that one row, by id", grepl("Confirmar 1 pago(s)", txt, fixed = TRUE) && any(r == "confirmados AP: X0850"))
ok("G11: and it left the table", poll_until(function() length(vis(AP)) == 853L && !("X0850" %in% vis(AP)), timeout_s = 10))

# ══ I · Marks another session took away ══════════════════════════════════════
ok("I0: a fresh fixture, opened in two sessions (A and B) sharing one Agenda", arrancar(0L, 38997L))
bA <- b
bB <- new_chromote_session()
invisible(bB$Emulation$setDeviceMetricsOverride(width = 1400L, height = 1000L, deviceScaleFactor = 1, mobile = FALSE))
en <- function(sesion, expr) { b <<- sesion; on.exit(b <<- bA); force(expr) }
en(bB, {
  invisible(b$Page$navigate("http://127.0.0.1:38997", wait_ = TRUE))
  poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s') && HDT.estado('%s').visibles.length > 0", AP, AP))),
             timeout_s = 60)
  Sys.sleep(1)
})
avisos <- function() unlist(ev("Array.from(document.querySelectorAll('.shiny-notification')).map(function(n){ return n.innerText; })"))
escrituras <- function() sum(grepl("^(confirmados|abonos|ppl)", registro()))

clic(celda(AP, "P1", 1L)); clic(celda(AP, "P3", 1L))
ok("I1: A marks P1 and P3", setequal(sel(AP), c("P1", "P3")))
invisible(en(bB, { clic(celda(AP, "P1", 1L)); clic(celda(AP, "P3", 1L)); confirmar() }))
ok("I2: B confirmed them", en(bB, any(registro() == "confirmados AP: P3,P1")))
ok("I3: in A they leave the table and the marks go with them",
   poll_until(function() !any(c("P1", "P3") %in% vis(AP)) && length(sel(AP)) == 0L, timeout_s = 10))
n0 <- escrituras()
clic(por_id(sprintf("confirm_ap_%s", EMP)))
ok("I4: A's Confirmar warns 'Las filas que marcaste ya no están en la Agenda (otra sesión las confirmó o quitó)'",
   poll_until(function() any(grepl("Las filas que marcaste ya no están en la Agenda", avisos(), fixed = TRUE)), timeout_s = 10))
Sys.sleep(1)
ok("I5: ...opens no modal and writes nothing (it does NOT fall back to the whole currency)",
   !nzchar(modal()) && escrituras() == n0 && setequal(vis(AP), c("P2", "P4")))

clic(celda(AP, "P2", 1L)); clic(celda(AP, "P4", 1L))
ok("I6: A marks P2 and P4", setequal(sel(AP), c("P2", "P4")))
invisible(en(bB, { clic(celda(AP, "P2", 1L)); confirmar() }))
ok("I7: B confirmed P2", en(bB, any(registro() == "confirmados AP: P2")))
ok("I8: in A, P2 leaves; P4 stays marked", poll_until(function() identical(vis(AP), "P4") && identical(sel(AP), "P4"), timeout_s = 10))
n0 <- length(registro())
clic(por_id(sprintf("dl_preview_%s", EMP)))
ok("I9: A's payment preview says 1 of the 2 marked is gone",
   poll_until(function() grepl("1 de las 2 filas que marcaste ya no están", modal(), fixed = TRUE), timeout_s = 10))
cerrar_modal()
txt <- confirmar()
ok("I10: A's confirm modal says it too, and confirms 1",
   grepl("1 de las 2 filas que marcaste ya no están en la Agenda; se tomarán las 1 que quedan", txt, fixed = TRUE) &&
     grepl("Confirmar 1 pago(s)", txt, fixed = TRUE))
r <- nuevas(n0)
ok("I11: A confirmed only P4 (the abono), nothing else", any(r == "abonos: 4004") && !any(grepl("^confirmados", r)))
poll_until(function() !any(grepl("Las filas que marcaste", avisos(), fixed = TRUE)), timeout_s = 15)
Sys.sleep(1)
ok("I12: A's own confirmation leaves no 'marks missing' warning behind, and the table is empty",
   !any(grepl("Las filas que marcaste", avisos(), fixed = TRUE)) && !length(vis(AP)) && !nzchar(modal()))

# The PPL download, when every marked row left after the preview opened.
invisible(b$Browser$setDownloadBehavior(behavior = "allow", downloadPath = normalizePath(tempdir(), winslash = "/")))
usd <- function() invisible(ev(sprintf("document.getElementById('ph-bal_cur_%s').selectize.setValue('USD')", EMP)))
usd(); en(bB, usd())
# (USD keeps P6: were P5 its last payment, the currency itself would go and
# the selector would move to MXN, which forgets the marks.)
ok("I13: both on USD (P5, P6)", poll_until(function() identical(vis(AP), c("P5", "P6")) && identical(en(bB, vis(AP)), c("P5", "P6")), timeout_s = 10))
clic(celda(AP, "P5", 1L))
clic(por_id(sprintf("dl_preview_%s", EMP)))
ok("I14: A opens the payment preview with P5 marked", poll_until(function() grepl("ACEROS MONTERREY SA", modal(), fixed = TRUE), timeout_s = 10))
invisible(en(bB, { clic(celda(AP, "P5", 1L)); confirmar() }))
ok("I15: B confirms P5 meanwhile", en(bB, any(registro() == "confirmados AP: P5")) &&
     poll_until(function() identical(vis(AP), "P6") && !length(sel(AP)), timeout_s = 10))
n0 <- length(registro())
Sys.sleep(0.8)
clic(sprintf("document.getElementById('ph-dl_ppl_%s')", EMP))
ok("I16: A's Descargar then says the marked rows are gone, and pays nothing (no PPL line)",
   poll_until(function() any(grepl("Las filas que marcaste ya no están en la Agenda", avisos(), fixed = TRUE)), timeout_s = 10) &&
     !any(grepl("^ppl", nuevas(n0))))
try(bB$close(), silent = TRUE)

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) {
  cat("
-- app stderr (tail) --
"); try(cat(tail(readLines(app$proc$get_error_file(), warn = FALSE), 25), sep = "
"))
  quit(status = 1L)
}
