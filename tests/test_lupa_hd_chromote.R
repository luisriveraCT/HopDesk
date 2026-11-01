# =============================================================================
# tests/test_lupa_hd_chromote.R
# The lupa (Facturas del Calendario) on the hd_table engine (T04), in a real
# headless Chrome, against tests/helpers/vencidos_fixture_app.R: the real
# module over fixed data and a simulated `shared` (no S3). It is the same
# table as Vencidos (test_vencidos_hd_chromote.R covers the shared
# behaviours one by one); this covers what is the lupa's own: one month's
# invoices, the month bar that loads the month chosen (B5), opening on the
# Calendario's month, a picture reloaded after each action (N9), paid rows
# locked (Dif6), Eliminar and Agregar selección confirming (M5, Dif4), the
# Agenda check from the real queue (A4), ⚡ (P-2) and "Ver datos SAP" (M7).
#
# Run from project root:  Rscript tests/test_lupa_hd_chromote.R
# =============================================================================
cat("=== test_lupa_hd_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38971L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
# The fixed data counts its days from the 28th of this month (VEN_HOY), so
# the month the lupa opens on (today's) always has the same invoices.
HOY <- format(Sys.Date(), "%Y-%m-28")
app <- tryCatch(start_bg_app(sprintf('Sys.setenv(VEN_HOY = "%s"); setwd("%s"); source("tests/helpers/vencidos_fixture_app.R")$value', HOY, repo),
                             PORT, timeout_s = 90), error = function(e) { message(conditionMessage(e)); NULL })
ok("fixture app started", !is.null(app))
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
invisible(b$Emulation$setDeviceMetricsOverride(width = 1400L, height = 1000L, deviceScaleFactor = 1, mobile = FALSE))
base <- sprintf("http://127.0.0.1:%d", PORT)

ev <- function(js, await = FALSE) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE, awaitPromise = await)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
A <- "lupa-tabla-t-hdt"
estado <- function() ev(sprintf("HDT.estado('%s')", A))
vis <- function() unlist(estado()$visibles)
sel <- function() unlist(estado()$seleccion)
docs <- function(ids) sub("^[^|]*\\|[^|]*\\|[^|]*\\|([^|]*)\\|.*$", "\\1", ids)
centro <- function(js_el, ver = TRUE) ev(sprintf("(function(){ var e = %s; %s
  var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el,
  if (ver) "e.scrollIntoView({block:'center', behavior:'instant'});" else ""))
raton <- function(tipo, p, botones = 0L, n = 1L, shift = FALSE)
  b$Input$dispatchMouseEvent(type = tipo, x = p[[1]], y = p[[2]], button = if (tipo == "mouseMoved" && !botones) "none" else "left",
                             buttons = botones, clickCount = n, modifiers = if (shift) 8L else 0L)
clic <- function(js_el, shift = FALSE, ver = TRUE) {
  p <- centro(js_el, ver)
  raton("mouseMoved", p); raton("mousePressed", p, 1L, shift = shift); raton("mouseReleased", p, shift = shift)
  Sys.sleep(0.3)
}
id_de <- function(doc) {
  todos <- unlist(ev(sprintf("HDT.estado('%s').visibles.concat(HDT.estado('%s').bloqueadas)", A, A)))
  unique(todos[grepl(paste0("\\|", doc, "(\\||$)"), todos)])[1]
}
fila <- function(id, sel = "") sprintf("HDT.fila('%s', '%s')%s", A, id, sel)
celda <- function(id, j = 3L) fila(id, sprintf(".children[%d]", j))
registro <- function() strsplit(ev("document.getElementById('registro').textContent") %||% "", "\n")[[1]]
ultima <- function() { r <- registro(); if (length(r)) r[length(r)] else "" }
n_reg <- function() length(registro())
barra <- function(accion) sprintf("document.querySelector('#%s .hdt-acciones button[data-accion=%s]')", A, accion)
conf_texto <- function() ev(sprintf("document.querySelector('#%s .hdt-confirmar-texto').textContent", A))
confirmar <- function() clic(sprintf("document.querySelector('#%s .hdt-confirmar-si')", A))
modal_abierto <- function() isTRUE(ev(sprintf("!!document.querySelector('.modal.show #%s')", A)))
abrir_lupa <- function(mes = NULL) {
  if (!is.null(mes)) invisible(ev(sprintf("Shiny.setInputValue('month_sel', '%s')", mes)))
  invisible(ev("Shiny.setInputValue('btn_search', Math.random(), {priority: 'event'})"))
  ok <- poll_until(function() modal_abierto() && length(vis()) > 0L, timeout_s = 20)
  Sys.sleep(0.8)
  ok
}
cerrar_lupa <- function() {
  invisible(ev("var m = document.querySelector('.modal.show [data-bs-dismiss=modal]'); if (m) m.click();"))
  poll_until(function() !isTRUE(ev("!!document.querySelector('.modal.show')")), timeout_s = 10)
}

# The fixture's invoices, by document and days before today (see the fixture).
hoy <- as.Date(HOY)   # the fixture's "today" (VEN_HOY)
mes_de <- function(d) format(d, "%Y-%m")
dias <- c(`1001` = 5, `1002` = 5, `1003` = 5, `1004` = 20, `1005` = 1, `2001` = 9, `2002` = 0, `3001` = 3,
          `3002` = 3, `3003` = 12, `3004` = 4, `3005` = 30, `4001` = 6, `3999` = -10)
# 3004's counterpart is one of the companies: apply_ic_filter() leaves it out
# by name when no intercompany codes are registered, in the old lupa too.
este_mes <- setdiff(names(dias)[mes_de(hoy - dias) == mes_de(hoy)], "3004")
mes_lejos <- mes_de(seq(as.Date(format(hoy, "%Y-%m-01")), by = "month", length.out = 3)[3])

invisible(b$Page$navigate(base, wait_ = TRUE))
invisible(poll_until(function() isTRUE(ev("!!window.HDT && !!window.Shiny && !!Shiny.setInputValue")), timeout_s = 60))
Sys.sleep(1)
ok("the lupa opens in its window on the engine", abrir_lupa())

# ── D · the month's data ───────────────────────────────────────────────────
MESES <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre",
           "noviembre", "diciembre")
titulo_de <- function(m) sprintf("Facturas — %s %s", MESES[as.integer(substr(m, 6, 7))], substr(m, 1, 4))
titulo <- function() trimws(ev("document.querySelector('.modal.show .modal-title').textContent"))
ok("D: the title says the month in Spanish ('Facturas — octubre 2026') and the bar shows it too",
   poll_until(function() identical(titulo(), titulo_de(mes_de(hoy))), timeout_s = 5) &&
     identical(estado()$periodo, mes_de(hoy)))
v <- setdiff(docs(c(vis(), unlist(estado()$bloqueadas))), "")
ok("D1: every invoice due this month and only those (not intercompany, not other months)",
   setequal(intersect(v, names(dias)), este_mes) && !"5001" %in% v)
titulos <- unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.dataset.col != null; }).map(function(th){ return th.firstChild.textContent; })", A)))
ok("D2: Vencidos' columns, with Documento shown, as the lupa always did",
   identical(titulos, c("Tipo", "Empresa", "Fecha venc.", "Parte", "Referencia", "Documento", "Importe", "Etiqueta")))
f1001b <- sprintf("Array.from(document.querySelectorAll('#%s tbody tr')).filter(function(tr){ return tr.textContent.indexOf('F-1001-B') >= 0; })[0]", A)
if ("1001" %in% este_mes && (hoy - 2) >= as.Date(format(hoy, "%Y-%m-01")))
  ok("D4: a row with no company gets it from the same document and ledger (NTS), as in Vencidos",
     identical(ev(sprintf("%s.children[1].textContent", f1001b)), "NTS"))
ok("Dif6: a paid invoice is locked (struck through, can't be marked), as in Vencidos",
   "1003" %in% este_mes == (id_de("1003") %in% unlist(estado()$bloqueadas)))
n_libres <- length(setdiff(vis(), unlist(estado()$bloqueadas)))
ok("D5: the counter leaves the paid one out",
   identical(ev(sprintf("document.querySelector('#%s .hdt-contador').textContent", A)), sprintf("%d facturas", n_libres)))
ok("S1/G1: sections by currency and type, and groups, like Vencidos",
   any(startsWith(unlist(estado()$lista), "banda:1:MXN")) && any(startsWith(unlist(estado()$lista), "grupo:")))

# ── B5 · months ────────────────────────────────────────────────────────────
k_mes <- function() estado()$periodo
clic(sprintf("document.querySelectorAll('#%s .hdt-mes-flecha')[1]", A))
clic(sprintf("document.querySelectorAll('#%s .hdt-mes-flecha')[1]", A))
ok("B5: › › goes to the month after next and loads its invoices (the old strip showed it empty)",
   identical(k_mes(), mes_lejos) &&
     poll_until(function() identical(titulo(), titulo_de(mes_lejos)), timeout_s = 10) &&
     poll_until(function() setequal(docs(vis()), c("5001", "5002")), timeout_s = 10))
clic(sprintf("document.querySelectorAll('#%s .hdt-mes-flecha')[0]", A))
clic(sprintf("document.querySelectorAll('#%s .hdt-mes-flecha')[0]", A))
ok("B5: ‹ ‹ comes back to this month with its invoices",
   identical(k_mes(), mes_de(hoy)) && poll_until(function() setequal(intersect(docs(c(vis(), unlist(estado()$bloqueadas))), names(dias)), este_mes), timeout_s = 10))

# ── M · actions (the picture reloads after each one, N9) ───────────────────
r <- id_de("1004")
clic(celda(r))
clic(sprintf("document.querySelector('#%s .hdt-editar')", A))
k <- n_reg(); clic(barra("etiqueta_b"))
ok("M2: 'Etiqueta B' goes to handle_invoice_action() as tag_both, the pill changes and tagging unmarks it",
   poll_until(function() n_reg() > k && grepl("tag_both \\[1\\] AR NCS MXN 1004$", ultima()), timeout_s = 8) &&
     poll_until(function() grepl("^B", ev(sprintf("%s.children[7].textContent", fila(r)))), timeout_s = 8) &&
     !length(sel()))
clic(celda(r))
fecha <- sprintf("document.querySelector('#%s .hdt-acciones input.hdt-fecha')", A)
invisible(ev(sprintf("var i = %s; i.value = '2026-12-15'; i.dispatchEvent(new Event('input'))", fecha)))
k <- n_reg(); clic(barra("mover"))
ok("M3: 'Mover' sends the date", poll_until(function() n_reg() > k && grepl("move \\[1\\] AR NCS MXN 1004 -> 2026-12-15$", ultima()), timeout_s = 8))
k <- n_reg(); clic(barra("eliminar"))
ok("M5: 'Eliminar' asks first now (the old lupa deleted at once), in Vencidos' words",
   n_reg() == k && identical(conf_texto(), "¿Eliminar 1 factura? Se guardará en la papelera."))
confirmar()
ok("M5/N9: ... Confirmar deletes it, and the reloaded month no longer has it",
   poll_until(function() grepl("delete \\[1\\] AR NCS MXN 1004$", ultima()) && is.na(id_de("1004")), timeout_s = 10))
clic(sprintf("document.querySelector('#%s .hdt-editar')", A))

# ── A · Agenda ─────────────────────────────────────────────────────────────
marca <- function(id) isTRUE(ev(sprintf("!!%s.querySelector('.hdt-agregado')", fila(id))))
r2 <- id_de("2002")
if ("3002" %in% este_mes) {
  invisible(ev(sprintf("var g = document.querySelector('#%s tr.hdt-grupo[data-primera=\"%s\"]'); if (g) g.querySelector('.hdt-grupo-abrir').click();", A, id_de("3001"))))
  Sys.sleep(0.4)
  ok("A4: the ✓ comes from the real Agenda queue (3002 is in it), not from this window", marca(id_de("3002")))
}
clic(celda(r2))
k <- n_reg(); clic(barra("agregar_seleccion"))
ok("Dif4: 'Agregar selección' asks first now, in Vencidos' words",
   n_reg() == k && identical(conf_texto(), "¿Agregar 1 factura seleccionada a la Agenda del día?"))
confirmar()
ok("A2: ... and stages it, then its ✓ shows",
   poll_until(function() grepl("stage_all \\[1\\] AR NCS USD 2002$", ultima()) && marca(r2), timeout_s = 8))

# ── P · provision ──────────────────────────────────────────────────────────
prov <- id_de("PRV-1")
if (!is.na(prov)) {
  k <- n_reg(); clic(fila(prov, ".querySelector('button[data-accion=provision]')"))
  ok("P-2: ⚡ asks the pasivos module to convert it (the old lupa's pill did)",
     poll_until(function() n_reg() > k && identical(ultima(), "convertir PRV-1"), timeout_s = 8))
}

# ── Opening again: the Calendario's month ──────────────────────────────────
ok("the window closes", cerrar_lupa())
ok("opened again with the Calendario on the month after next, it opens on that month with its invoices",
   abrir_lupa(paste0(mes_lejos, "-01")) && identical(k_mes(), mes_lejos) && setequal(docs(vis()), c("5001", "5002")))

# ── M7 · Ver datos SAP ─────────────────────────────────────────────────────
invisible(cerrar_lupa()); invisible(abrir_lupa(format(hoy)))
clic(sprintf("document.querySelector('#%s .hdt-editar')", A))
ok("M7: 'Ver datos SAP' is in the edit bar", identical(ev(sprintf("%s.textContent", barra("crudos"))), "Ver datos SAP"))
n_crudos <- sum(mes_de(hoy - c(5, 20, 3)) == mes_de(hoy))
clic(barra("crudos"))
ok(sprintf("M7: ... and shows every raw SAP row of the month (%d), nothing marked", n_crudos),
   poll_until(function() grepl(sprintf("Datos SAP crudos — %d filas", n_crudos),
                               ev("(document.querySelector('.modal.show .modal-title')||{}).textContent || ''"), fixed = TRUE), timeout_s = 10))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
