# =============================================================================
# tests/test_dia_hd_chromote.R
# The Calendario's day modal on the hd_table engine (T05), in a real headless
# Chrome, against tests/helpers/dia_fixture_app.R: the modal's own pieces of
# ledgerModuleServer over one fixed AP day, with the handlers that write
# recorded. One block per group of the T05 inventory
# (hopdesk_coord/tablas/reportes/T05_inventario.md): D data, G groups,
# L selection, M actions, A Agenda, P provisions, and the view switch.
#
# Run from project root:  Rscript tests/test_dia_hd_chromote.R
# =============================================================================
cat("=== test_dia_hd_chromote ===\n\n")

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
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
app <- tryCatch(start_bg_app(sprintf('setwd("%s"); source("tests/helpers/dia_fixture_app.R")$value', repo), PORT, timeout_s = 90),
                error = function(e) { message(conditionMessage(e)); NULL })
ok("fixture app started", !is.null(app))
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
invisible(b$Emulation$setDeviceMetricsOverride(width = 1400L, height = 1000L, deviceScaleFactor = 1, mobile = FALSE))
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
R <- "ap-resumen-hdt"; D <- "ap-detalle-hdt"
estado <- function(t) ev(sprintf("HDT.estado('%s')", t))
vis <- function(t) unlist(estado(t)$visibles)
sel <- function(t) unlist(estado(t)$seleccion)
docs <- function(ids) sub("^[^|]*\\|[^|]*\\|([^|]*)\\|.*$", "\\1", ids)
id_de <- function(t, doc) { x <- c(vis(t), unlist(estado(t)$bloqueadas)); unique(x[docs(x) == doc])[1] }
fila <- function(t, id, s = "") sprintf("HDT.fila('%s', '%s')%s", t, id, s)
centro <- function(js_el) ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', behavior:'instant'});
  var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
raton <- function(tipo, p, botones = 0L) b$Input$dispatchMouseEvent(type = tipo, x = p[[1]], y = p[[2]],
  button = if (tipo == "mouseMoved" && !botones) "none" else "left", buttons = botones, clickCount = 1L)
clic <- function(js_el) { p <- centro(js_el); raton("mouseMoved", p); raton("mousePressed", p, 1L); raton("mouseReleased", p); Sys.sleep(0.35) }
registro <- function() strsplit(ev("document.getElementById('registro').textContent") %||% "", "\n")[[1]]
ultima <- function() { r <- registro(); if (length(r)) r[length(r)] else "" }
n_reg <- function() length(registro())
barra <- function(t, a) sprintf("document.querySelector('#%s .hdt-acciones button[data-accion=%s]')", t, a)
conf_texto <- function(t) ev(sprintf("document.querySelector('#%s .hdt-confirmar-texto').textContent", t))
si <- function(t) clic(sprintf("document.querySelector('#%s .hdt-confirmar-si')", t))
titulos <- function(t) unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.dataset.col != null; }).map(function(th){ return th.firstChild.textContent; })", t)))
visible <- function(t) isTRUE(ev(sprintf("(function(){ var e = document.getElementById('%s'); return !!e && e.offsetParent !== null; })()", t)))
abrir <- function(boton = "abrir") {
  invisible(ev(sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'})", boton)))
  ok <- poll_until(function() isTRUE(ev("!!document.querySelector('.modal.show')")) && length(vis(R)) > 0L, timeout_s = 20)
  Sys.sleep(0.8); ok
}

invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE))
invisible(poll_until(function() isTRUE(ev("!!window.HDT && !!window.Shiny && !!Shiny.setInputValue")), timeout_s = 60))
Sys.sleep(1)
ok("a click on the day opens its window, in Resumen (the default view)", abrir() && visible(R) && !visible(D))

# ── D · data ─────────────────────────────────────────────────────────────────
ok("D1/D2: the day's 9 invoices, paid one included (locked), in both tables",
   length(c(vis(R), unlist(estado(R)$bloqueadas))) >= 9L && setequal(unlist(estado(R)$bloqueadas), id_de(R, "3006")))
ok("P7: Resumen shows Empresa, Parte, Referencia, Importe, Etiqueta, Origen (Documento, Venc. original and Movida optional, hidden)",
   identical(titulos(R), c("Empresa", "Parte", "Referencia", "Importe", "Etiqueta", "Origen")))
ok("D5: the counter leaves the paid one out (8 facturas)",
   identical(ev(sprintf("document.querySelector('#%s .hdt-contador').textContent", R)), "8 facturas"))
ok("P10: the Agenda line stays below the tables ($11,000.00 of this day's companies)",
   grepl("En Agenda (Pagos):$ 11,000.00", gsub("\\s+", "", ev("document.querySelector('.cart-total-staged').textContent")), fixed = TRUE) ||
     grepl("11,000.00", ev("document.querySelector('.cart-total-staged').textContent"), fixed = TRUE))

# ── G · groups (Resumen) ─────────────────────────────────────────────────────
lr <- unlist(estado(R)$lista)
ok("G1: groups by company + counterpart, closed: ACEROS (3, one paid) and TRANSPORTES (2)",
   sum(startsWith(lr, "grupo:cerrado:")) == 2L)
ok("P1: a counterpart with one invoice is a plain row (BODEGAS DEL NORTE)", id_de(R, "3010") %in% lr)
ok("Dif4: the paid invoice is not hidden with its group: it is inside, struck through and locked",
   id_de(R, "3006") %in% unlist(estado(R)$bloqueadas))
clic(sprintf("document.querySelector('#%s .hdt-grupos-todos')", R))
ok("G4: ▶▶ opens every group (it wasn't there before)", all(startsWith(unlist(estado(R)$lista)[startsWith(unlist(estado(R)$lista), "grupo:")], "grupo:abierto:")))

# ── A · Agenda ───────────────────────────────────────────────────────────────
ag <- id_de(R, "3002")
quitar <- fila(R, ag, ".querySelector('button.hdt-quitar-agenda')")
ok("A1: the invoice already in the Agenda shows ✓ (the same button that takes it out)",
   identical(ev(sprintf("%s.textContent", quitar)), "✓"))
k <- n_reg(); clic(quitar)
ok("M8: its ✓ takes it out of the Agenda: quitar [1] 3002",
   poll_until(function() n_reg() > k && identical(ultima(), "quitar [1] 3002"), timeout_s = 8))
ok("... and the row goes back to ＋ (from the real queue)",
   poll_until(function() isTRUE(ev(sprintf("!!%s", fila(R, ag, ".querySelector('button[data-accion=agregar]')")))), timeout_s = 8))
k <- n_reg(); clic(fila(R, ag, ".querySelector('button[data-accion=agregar]')"))
ok("M8: ＋ puts it back: agregar cart_inv_click [1] 3002 (the log's name for a row's ＋, as before T05)",
   poll_until(function() n_reg() > k && identical(ultima(), "agregar cart_inv_click [1] 3002"), timeout_s = 8))

# The group's ＋ (cart_<i> before T05): ACEROS has 3001 (out), 3002 (in the
# Agenda) and 3006 (paid, locked).
grupo_a <- sprintf("Array.from(document.querySelectorAll('#%s tr.hdt-grupo')).filter(function (tr) { return tr.textContent.indexOf('ACEROS') >= 0; })[0]", R)
boton_g <- sprintf("%s.querySelector('td.hdt-acc button.hdt-accion')", grupo_a)
ok("G＋: a group's row has its ＋ in the rows' ＋ column",
   identical(ev(sprintf("%s.textContent + '|' + %s.dataset.accion", boton_g, boton_g)), "＋|agregar") &&
     abs(ev(sprintf("%s.getBoundingClientRect().left", boton_g)) -
         ev(sprintf("%s.getBoundingClientRect().left", fila(R, id_de(R, "3001"), ".querySelector('button[data-accion=agregar]')")))) < 1)
k <- n_reg(); clic(boton_g)
ok("G＋: it sends only the group's invoices not in the Agenda yet (not the paid one): agregar cart_toggle [1] 3001",
   poll_until(function() n_reg() > k && identical(ultima(), "agregar cart_toggle [1] 3001"), timeout_s = 8) && !length(sel(R)))
ok("G✓: with all of them in the Agenda the group's button is ✓",
   poll_until(function() identical(ev(sprintf("%s.textContent", boton_g)), "✓"), timeout_s = 8))
k <- n_reg(); clic(boton_g)
ok("G✓: ✓ takes them all out: quitar [2] 3001,3002; the selection is untouched",
   poll_until(function() n_reg() > k && identical(ultima(), "quitar [2] 3001,3002"), timeout_s = 8) && !length(sel(R)))
ok("... and the group's button is ＋ again",
   poll_until(function() identical(ev(sprintf("%s.textContent", boton_g)), "＋"), timeout_s = 8))
ok("A1: no ＋/✎ on the paid invoice",
   !isTRUE(ev(sprintf("!!%s", fila(R, id_de(R, "3006"), ".querySelector('button[data-accion=agregar], button[data-accion=editar]')")))))

# ── L / M · selection and bar actions ───────────────────────────────────────
grupo_t <- sprintf("document.querySelector('#%s tr.hdt-grupo[data-primera=\"%s\"]')", R, id_de(R, "3007"))
clic(sprintf("%s.children[2]", grupo_t))
ok("L3: a click on a group marks its invoices (TRANSPORTES: 3007, 3008)", setequal(docs(sel(R)), c("3007", "3008")))
k <- n_reg(); clic(barra(R, "agregar_seleccion"))
ok("Dif2: 'Agregar selección' asks first, in Vencidos' words",
   n_reg() == k && identical(conf_texto(R), "¿Agregar 2 facturas seleccionadas a la Agenda del día?"))
si(R)
ok("... and stages both: agregar stage_sel [2] 3007,3008; the selection stays (Dif5)",
   poll_until(function() identical(ultima(), "agregar stage_sel [2] 3007,3008"), timeout_s = 8) && length(sel(R)) == 2L)
clic(sprintf("document.querySelector('#%s .hdt-deseleccionar')", R))
clic(sprintf("document.querySelector('#%s .hdt-editar')", R))
fecha_in <- sprintf("document.querySelector('#%s .hdt-acciones input.hdt-fecha')", R)
ok("P8: 'Mover' opens on the day after the one shown (2026-11-04)", identical(ev(sprintf("%s.value", fecha_in)), "2026-11-04"))
nota <- function(t) ev(sprintf("(function(){ var n = document.querySelector('#%s .hdt-nota'); return n && !n.hidden ? n.textContent : ''; })()", t))
ok("P9: with Editar on, the policy hint for that date: BODEGAS DEL NORTE only pays on fridays (→ 06/11/2026)",
   poll_until(function() identical(nota(R), "La fecha elegida no cumple la política de pago de: BODEGAS DEL NORTE → 06/11/2026"), timeout_s = 8))
invisible(ev(sprintf("var i = %s; i.value = '2026-11-06'; i.dispatchEvent(new Event('input'))", fecha_in)))
ok("... and it follows the date: on a friday there is nothing to say", poll_until(function() identical(nota(R), ""), timeout_s = 8))
r5 <- id_de(R, "3005")
clic(fila(R, r5, ".children[2]"))
k <- n_reg(); clic(barra(R, "mover"))
ok("M2: 'Mover' sends the marked invoice and the date: mover [1] 3005 -> 2026-11-06",
   poll_until(function() n_reg() > k && identical(ultima(), "mover [1] 3005 -> 2026-11-06"), timeout_s = 8))
k <- n_reg(); clic(barra(R, "restaurar"))
ok("M4: 'Restaurar fecha': restaurar [1] 3005", poll_until(function() n_reg() > k && identical(ultima(), "restaurar [1] 3005"), timeout_s = 8))
k <- n_reg(); clic(barra(R, "etiqueta_a"))
ok("Dif6: tags in Resumen too: etiquetar urgent [1] 3005, and tagging unmarks",
   poll_until(function() n_reg() > k && identical(ultima(), "etiquetar urgent [1] 3005"), timeout_s = 8) && !length(sel(R)))
ok("... the pill comes back as A", poll_until(function()
  identical(ev(sprintf("%s.querySelector('.hdt-ins.hdt-tono-rojo') ? 'A' : ''", fila(R, r5))), "A"), timeout_s = 8))
clic(sprintf("document.querySelector('#%s .hdt-editar')", R))
ok("P9: Editar off, the hint goes", poll_until(function() identical(nota(R), ""), timeout_s = 8))

# ── P · provision, edit, raw data ────────────────────────────────────────────
k <- n_reg(); clic(fila(R, id_de(R, "PROV_1"), ".querySelector('button[data-accion=provision]')"))
ok("P1: ⚡ asks the pasivos module to convert it", poll_until(function() n_reg() > k && identical(ultima(), "convertir PRV-1"), timeout_s = 8))
k <- n_reg(); clic(fila(R, id_de(R, "3003"), ".querySelector('button[data-accion=editar]')"))
ok("M9/M10: a row's ✎ hands the day's whole row to the edit (the manual entry, with its source)",
   poll_until(function() n_reg() > k && identical(ultima(), "editar 3003 (manual, notas=)"), timeout_s = 8))
clic(sprintf("document.querySelector('#%s .hdt-editar')", R))
k <- n_reg(); clic(barra(R, "crudos"))
ok("P6: 'Ver datos SAP' in Resumen too, the day's own viewer", poll_until(function() n_reg() > k && identical(ultima(), "crudos"), timeout_s = 8))
clic(sprintf("document.querySelector('#%s .hdt-editar')", R))

# ── The view switch ─────────────────────────────────────────────────────────
invisible(ev("document.querySelector('.modal.show').dataset.marca = 'misma'"))
clic("document.querySelector('.hd-dia-cambio button[data-vista=detalle]')")
ok("Dif10: Detalle shows in the same window (not closed and reopened)",
   visible(D) && !visible(R) && identical(ev("document.querySelector('.modal.show').dataset.marca"), "misma"))
ok("P7: Detalle shows Documento, Venc. original and Movida too",
   identical(titulos(D), c("Empresa", "Parte", "Referencia", "Documento", "Venc. original", "Movida", "Importe", "Etiqueta", "Origen")))
ok("O1: Movida as a pill (BODEGAS: Manual)", identical(ev(sprintf("%s.children[5].textContent", fila(D, id_de(D, "3010")))), "Manual"))
ok("Detalle is flat: one row per invoice, no groups", !any(startsWith(unlist(estado(D)$lista), "grupo:")))
k <- n_reg(); clic(barra(D, "agregar_todo"))
ok("Dif2: 'Agregar todo' asks first, then sends every open invoice (never the paid one)",
   identical(conf_texto(D), "¿Agregar 8 facturas a la Agenda del día?"))
si(D)
ok("... agregar stage_all [8] (the handler leaves the provision out, as it always did)",
   poll_until(function() n_reg() > k && grepl("^agregar stage_all \\[8\\]", ultima()) && !grepl("3006", ultima()), timeout_s = 8))
r10 <- id_de(D, "3010")
clic(fila(D, r10, ".children[2]"))
clic(sprintf("document.querySelector('#%s .hdt-editar')", D))
k <- n_reg(); clic(barra(D, "eliminar"))
ok("Dif1: 'Eliminar' asks first in the table (no dialog replacing the window), in Vencidos' words",
   n_reg() == k && identical(conf_texto(D), "¿Eliminar 1 factura? Se guardará en la papelera.") &&
     identical(ev("document.querySelector('.modal.show').dataset.marca"), "misma"))
si(D)
ok("... eliminar [1] 3010, and the row goes away where it is: same window, same view (Dif9)",
   poll_until(function() identical(ultima(), "eliminar [1] 3010") && is.na(id_de(D, "3010")), timeout_s = 8) &&
     identical(ev("document.querySelector('.modal.show').dataset.marca"), "misma") && visible(D))

# ── The view is remembered ──────────────────────────────────────────────────
invisible(ev("document.querySelector('.modal.show [data-dismiss=modal], .modal.show [data-bs-dismiss=modal]').click()"))
invisible(poll_until(function() !isTRUE(ev("!!document.querySelector('.modal.show')")), timeout_s = 10))
Sys.sleep(0.5)
invisible(ev("Shiny.setInputValue('abrir', Math.random(), {priority: 'event'})"))
ok("the next day opens in the view last chosen (Detalle)",
   poll_until(function() isTRUE(ev("!!document.querySelector('.modal.show')")) && visible(D) && !visible(R), timeout_s = 20))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
