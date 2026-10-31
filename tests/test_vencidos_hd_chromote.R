# =============================================================================
# tests/test_vencidos_hd_chromote.R
# Vencidos on the hd_table engine (T04), in a real headless Chrome, against
# tests/helpers/vencidos_fixture_app.R: the real module over fixed data and a
# simulated `shared` (no S3). Every click and key is a real input event. One
# block per group of behaviours of the T03c inventory
# (hopdesk_coord/tablas/reportes/T03c_inventario.md): D data, S sections,
# G groups, L selection, B search / filters / sort, E tags, M edit mode,
# A Agenda, P provisions, O the rest (bubble, Excel, Ctrl+C, source data).
#
# Run from project root:  Rscript tests/test_vencidos_hd_chromote.R
# =============================================================================
cat("=== test_vencidos_hd_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38964L
app <- NULL; b <- NULL
descargas <- file.path(tempdir(), "ven_descargas")
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app); unlink(descargas, recursive = TRUE) }, add = TRUE)
arrancar <- function(vacio = FALSE, extra = 0L) {
  if (!is.null(app)) stop_bg_app(app)
  app <<- tryCatch(start_bg_app(sprintf('Sys.setenv(VEN_VACIO = "%s", VEN_EXTRA = "%d"); setwd("%s"); source("tests/helpers/vencidos_fixture_app.R")$value',
                                        if (vacio) "1" else "0", extra, repo), PORT, timeout_s = 90),
                   error = function(e) { message(conditionMessage(e)); NULL })
  !is.null(app)
}
ok("fixture app started", arrancar())
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
invisible(b$Emulation$setDeviceMetricsOverride(width = 1400L, height = 1000L, deviceScaleFactor = 1, mobile = FALSE))
base <- sprintf("http://127.0.0.1:%d", PORT)
invisible(try(b$Browser$grantPermissions(permissions = list("clipboardReadWrite", "clipboardSanitizedWrite"), origin = base), silent = TRUE))
unlink(descargas, recursive = TRUE); dir.create(descargas)
invisible(b$Browser$setDownloadBehavior(behavior = "allow", downloadPath = normalizePath(descargas, winslash = "\\")))

ev <- function(js, await = FALSE) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE, awaitPromise = await)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
A <- "ven-tabla-t-hdt"
abrir <- function() {
  invisible(b$Page$navigate(base, wait_ = TRUE))
  ok <- poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s')", A))), timeout_s = 60)
  Sys.sleep(0.8)
  ok
}
estado <- function() ev(sprintf("HDT.estado('%s')", A))
vis <- function() unlist(estado()$visibles)
lista <- function() unlist(estado()$lista)
sel <- function() unlist(estado()$seleccion)
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
escribir <- function(js_input, texto) {
  p <- centro(js_input); raton("mousePressed", p, 1L); raton("mouseReleased", p)
  ev(sprintf("var i=%s; i.value=''; i.dispatchEvent(new Event('input'))", js_input))
  if (nzchar(texto)) b$Input$insertText(text = texto)
  Sys.sleep(0.4)
}
tecla <- function(key, code = key, vk = 0L) {
  invisible(b$Input$dispatchKeyEvent(type = "rawKeyDown", key = key, code = code, windowsVirtualKeyCode = vk))
  invisible(b$Input$dispatchKeyEvent(type = "keyUp", key = key, code = code, windowsVirtualKeyCode = vk))
  Sys.sleep(0.2)
}
# A row's id by its document (ids are ledger|empresa|moneda|documento|...).
id_de <- function(doc, ledger = NULL) {
  todos <- unlist(ev(sprintf("HDT.estado('%s').visibles.concat(HDT.estado('%s').bloqueadas)", A, A)))
  x <- unique(todos[grepl(paste0("\\|", doc, "(\\||$)"), todos)])
  if (!is.null(ledger)) x <- x[startsWith(x, paste0(ledger, "|"))]
  x[1]
}
fila <- function(id, sel = "") sprintf("HDT.fila('%s', '%s')%s", A, id, sel)
celda <- function(id, j = 3L) fila(id, sprintf(".children[%d]", j))
registro <- function() strsplit(ev("document.getElementById('registro').textContent") %||% "", "\n")[[1]]
ultima <- function() { r <- registro(); if (length(r)) r[length(r)] else "" }
n_reg <- function() length(registro())
conf_texto <- function() ev(sprintf("document.querySelector('#%s .hdt-confirmar-texto').textContent", A))
barra <- function(accion) sprintf("document.querySelector('#%s .hdt-acciones button[data-accion=%s]')", A, accion)
deseleccionar <- function() { invisible(ev(sprintf("(document.querySelector('#%s .hdt-deseleccionar')||{click:function(){}}).click()", A))); Sys.sleep(0.3) }
contador <- function() ev(sprintf("document.querySelector('#%s .hdt-contador').textContent", A))
bandas <- function() unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s tr.hdt-banda')).map(function(tr){ return tr.textContent; })", A)))

ok("Vencidos loads on the engine", abrir())

# ── D · data ────────────────────────────────────────────────────────────────
docs_vis <- sub("^[^|]*\\|[^|]*\\|[^|]*\\|([^|]*)\\|.*$", "\\1", vis())
ok("D1: overdue rows only -- the future one (3999) and the intercompany one by name (3004) are out",
   !any(c("3999", "3004") %in% docs_vis) && all(c("1001", "1004", "2001", "3001", "3005", "4001") %in% docs_vis))
titulos <- unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.dataset.col != null; }).map(function(th){ return th.firstChild.textContent; })", A)))
ok("D2: the columns of today, Documento / Origen / Venc. original / Moneda hidden but optional",
   identical(titulos, c("Tipo", "Empresa", "Fecha venc.", "Parte", "Referencia", "Importe", "Etiqueta")))
l <- lista(); mxn_cobros <- l[(which(l == "banda:2:cobro")[1] + 1):(which(l == "banda:2:pago")[1] - 1)]
ok("D3: factory order -- the tagged invoice first (1004, A), then by amount; the group as one unit",
   grepl("\\|1004\\|", mxn_cobros[1]) && startsWith(mxn_cobros[2], "grupo:") && grepl("\\|1005\\|", mxn_cobros[3]))
ok("D4: a row with no company gets it from the same document and ledger (NTS)",
   identical(ev(sprintf("Array.from(document.querySelectorAll('#%s tbody tr')).filter(function(tr){ return tr.textContent.indexOf('F-1001-B') >= 0; })[0].children[1].textContent", A)), "NTS"))
ok("D5: the counter says 13 overdue invoices (the paid one doesn't count)",
   identical(contador(), "13 facturas vencidas o con vencimiento hoy"))
ok("D7: the tab's badge says 13", identical(ev("document.getElementById('ven_tab_badge').textContent"), "13"))

# ── S · sections and subtotals ─────────────────────────────────────────────
ok("S1: sections by currency (MXN first), then Cobros / Pagos",
   identical(l[startsWith(l, "banda:")], c("banda:1:MXN", "banda:2:cobro", "banda:2:pago", "banda:1:USD", "banda:2:cobro", "banda:2:pago")))
bd <- bandas()
ok("S2/S3: each band counts its invoices without the paid one and adds up (MXN 10 / $123,391.25)",
   grepl("^MXN10 facturasMXN \\$123,391.25$", bd[1]) && grepl("^Cobros5 facturas", bd[2]))
escribir(sprintf("document.querySelector('#%s .hdt-buscar')", A), "acme")
bd <- bandas()
ok("S5: subtotals follow the search (bug fixed: they never did)",
   length(bd) == 2L && grepl("^MXN3 facturasMXN \\$20,400.00$", bd[1]) && identical(contador(), "3 facturas vencidas o con vencimiento hoy"))
ok("S6: sections with nothing left are hidden", !any(grepl("USD", bd)))
escribir(sprintf("document.querySelector('#%s .hdt-buscar')", A), "")

# ── G · groups ──────────────────────────────────────────────────────────────
acme <- c(id_de("1001", "AR"), id_de("1002"), id_de("1003"))
acme <- acme[!grepl("F-1001-B", acme)]
g_acme <- sprintf("document.querySelector('#%s tr.hdt-grupo[data-primera=\"%s\"]')", A, acme[1])
ok("G1/G2: ACME's three invoices of the same company, date and currency make one group, closed",
   isTRUE(ev(sprintf("!!%s", g_acme))) && any(startsWith(lista(), "grupo:cerrado:")))
ok("G3: the group's row counts 2 (the paid one doesn't) and adds $20,000.00",
   identical(ev(sprintf("%s.querySelector('.hdt-grupo-n').textContent", g_acme)), "2") &&
     identical(ev(sprintf("%s.children[5].textContent", g_acme)), "$20,000.00"))
clic(sprintf("%s.querySelector('.hdt-grupo-abrir')", g_acme))
ok("G5: ▶ opens it and marks nothing", any(startsWith(lista(), "grupo:abierto:")) && !length(sel()))
clic(sprintf("%s.querySelector('.hdt-grupo-abrir')", g_acme))
clic(sprintf("%s.children[3]", g_acme))
ok("G6: a click on the closed group marks its open invoices, never the paid one",
   setequal(sel(), acme[1:2]) && !acme[3] %in% sel())
deseleccionar()
clic(sprintf("document.querySelector('#%s .hdt-grupos-todos')", A))
ok("G10: ▶▶ opens every group", all(startsWith(lista()[startsWith(lista(), "grupo:")], "grupo:abierto:")))
clic(sprintf("document.querySelector('#%s .hdt-grupos-todos')", A))

# ── L · selection ──────────────────────────────────────────────────────────
r1 <- id_de("1004"); r2 <- id_de("1005")
clic(celda(r1)); clic(celda(r2), shift = TRUE)
ok("L1/L2: click marks, Shift + click marks the range in visible order (the closed group counts whole)",
   r1 %in% sel() && r2 %in% sel() && all(acme[1:2] %in% sel()) && !acme[3] %in% sel())
clic(celda(r2), shift = TRUE)
ok("L2: Shift + click on a marked one unmarks the range back to it (as today)", !r2 %in% sel())
deseleccionar()
clic(celda(acme[3]))
ok("L4: a paid (ghost) invoice is struck through and can't be marked",
   !length(sel()) && isTRUE(ev(sprintf("%s.classList.contains('hdt-bloq')", fila(acme[3])))))
clic(sprintf("document.querySelector('#%s .hdt-todos')", A))
ok("L5: Seleccionar todos takes the 13 open invoices, closed groups included, never the paid one",
   length(sel()) == 13L && !acme[3] %in% sel())
ok("L7: the footer sums what's marked, per type and currency",
   grepl("13 seleccionadas", ev(sprintf("document.querySelector('#%s .hdt-pie').textContent", A))))
deseleccionar()

# ── B · search, filters, sort ──────────────────────────────────────────────
escribir(sprintf("document.querySelector('#%s .hdt-buscar')", A), "f-1004")
ok("B1: the search finds by reference (and accent- and case-free)", identical(vis(), id_de("1004")))
escribir(sprintf("document.querySelector('#%s .hdt-buscar')", A), "45000.5")
ok("B1: ... and by the raw amount, like the lupa did", identical(vis(), id_de("1004")))
escribir(sprintf("document.querySelector('#%s .hdt-buscar')", A), "")
clic(sprintf("document.querySelectorAll('#%s .hdt-filtro')[0]", A))
clic("document.querySelector('.hdt-menu input[data-valor=\"Pago\"]')", ver = FALSE)
tecla("Escape", "Escape", 27L)
ok("B2: the Tipo filter leaves only Pagos (the provision included)",
   all(startsWith(vis(), "AP|")) && any(grepl("PRV-1", vis())))
clic(sprintf("document.querySelector('#%s .hdt-limpiar')", A))
clic(sprintf("document.querySelectorAll('#%s .hdt-filtro')[1]", A))
clic("document.querySelector('.hdt-menu input[data-valor=\"urgent\"]')", ver = FALSE)
tecla("Escape", "Escape", 27L)
ok("B3: the Etiqueta filter by A (2001 has both tags: that's B)", setequal(vis(), c(id_de("1004"), id_de("3001"))))
clic(sprintf("document.querySelector('#%s .hdt-limpiar')", A))
l0 <- lista()
clic(sprintf("document.querySelectorAll('#%s thead th')[5]", A)); clic(sprintf("document.querySelectorAll('#%s thead th')[5]", A))
clic(sprintf("document.querySelectorAll('#%s thead th')[5]", A))
ok("B6/N7: sorting by amount and a third click back to the factory order", identical(lista(), l0))

# ── E · tags ───────────────────────────────────────────────────────────────
pill <- function(id) ev(sprintf("(function(){ var tr = %s, p = tr.children[6].querySelector('.hdt-ins'); return (p ? p.textContent : '') + '|' + tr.className; })()", fila(id)))
ok("E: A red, B orange, C yellow -- pill and row background (2001 is tagged both: B)",
   grepl("^A\\|.*hdt-et-rojo", pill(id_de("1004"))) && grepl("^B\\|.*hdt-et-naranja", pill(id_de("2001"))) &&
     grepl("^C\\|.*hdt-et-amarillo", pill(id_de("3005"))))
g_ac <- sprintf("document.querySelector('#%s tr.hdt-grupo[data-primera=\"%s\"]')", A, id_de("3001"))
ok("E5: a group shows its strongest tag (ACEROS: A)",
   identical(ev(sprintf("%s.children[6].textContent", g_ac)), "A") && isTRUE(ev(sprintf("%s.classList.contains('hdt-et-rojo')", g_ac))))

# ── M · edit mode ──────────────────────────────────────────────────────────
editar <- sprintf("document.querySelector('#%s .hdt-editar')", A)
ok("M1: the edit actions are behind Editar", !isTRUE(ev(sprintf("!!%s", barra("etiqueta_a")))))
clic(editar)
tonos <- unlist(ev(sprintf("['etiqueta_a','etiqueta_b','etiqueta_c'].map(function(a){ var b = document.querySelector('#%s .hdt-acciones button[data-accion=' + a + ']'); return b.className + '|' + getComputedStyle(b).backgroundColor; })", A)))
ok("M2: 'Etiqueta A/B/C' in their tags' colours: red, orange, yellow",
   grepl("hdt-tono-rojo\\|rgb\\(255, 225, 222\\)", tonos[1]) && grepl("hdt-tono-naranja\\|rgb\\(255, 232, 209\\)", tonos[2]) &&
     grepl("hdt-tono-amarillo\\|rgb\\(255, 241, 184\\)", tonos[3]))
franja <- ev(sprintf("(function(){ var r = document.getElementById('%s'); return r.querySelector('.hdt-acciones').getBoundingClientRect().bottom - r.querySelector('.hdt-barra').getBoundingClientRect().top; })()", A))
ok(sprintf("the tools and the actions make one compact strip (%.0f px, was 69)", franja), franja <= 56)
r5 <- id_de("1005")
clic(celda(r5))
k <- n_reg()
clic(barra("etiqueta_a"))
ok("M2: 'Etiqueta A' goes to handle_invoice_action() as tag_urgent with that invoice",
   poll_until(function() n_reg() > k && grepl("tag_urgent \\[1\\] AR NTS MXN 1005$", ultima()), timeout_s = 8))
ok("... the row comes back with pill A, and tagging unmarks it (Luis, smoke test)",
   poll_until(function() grepl("^A\\|", pill(r5)), timeout_s = 8) && !length(sel()))
# Luis's sequence: remove the tags of two, then tag one -- only that one.
r4 <- id_de("4001"); r22 <- id_de("2002")
clic(celda(r4)); clic(celda(r22))
k <- n_reg(); clic(barra("quitar_etiqueta"))
ok("'Quitar etiqueta' on two: tag_clear [2], then nothing stays marked",
   poll_until(function() n_reg() > k && grepl("tag_clear \\[2\\]", ultima()), timeout_s = 8) && !length(sel()))
clic(celda(r4))
k <- n_reg(); clic(barra("etiqueta_c"))
ok("... and a click on one then 'Etiqueta C' tags only that one",
   poll_until(function() n_reg() > k && grepl("tag_important \\[1\\] AP NCS USD 4001$", ultima()), timeout_s = 8))
clic(celda(r5))
fecha <- sprintf("document.querySelector('#%s .hdt-acciones input.hdt-fecha')", A)
ok("M3: 'Mover' has its date box, tomorrow by default", identical(ev(sprintf("%s.value", fecha)), format(Sys.Date() + 1, "%Y-%m-%d")))
ok("... in Mexican Spanish (lang = es-MX)", identical(ev(sprintf("%s.lang", fecha)), "es-MX"))
invisible(ev(sprintf("var i = %s; i.value = '2026-12-15'; i.dispatchEvent(new Event('input'))", fecha)))
k <- n_reg(); clic(barra("mover"))
ok("M3: ... and sends the date (move_to); the selection stays (Dif5)",
   poll_until(function() n_reg() > k && grepl("move \\[1\\] AR NTS MXN 1005 -> 2026-12-15$", ultima()), timeout_s = 8) && identical(sel(), r5))
k <- n_reg(); clic(barra("restaurar"))
ok("M4: 'Restaurar fecha' -> restore", poll_until(function() n_reg() > k && grepl("restore \\[1\\]", ultima()), timeout_s = 8))
k <- n_reg(); clic(barra("eliminar"))
ok("M5: 'Eliminar' asks first (Dif3) and sends nothing yet",
   isTRUE(ev(sprintf("!document.querySelector('#%s .hdt-confirmar').hidden", A))) && n_reg() == k)
ok("... in the words it always had", identical(conf_texto(), "¿Eliminar 1 factura? Se guardará en la papelera."))
clic(sprintf("document.querySelector('#%s .hdt-confirmar-si')", A))
ok("M5: ... Confirmar deletes: delete, and the row goes away",
   poll_until(function() grepl("delete \\[1\\] AR NTS MXN 1005$", ultima()) && !r5 %in% vis(), timeout_s = 8))
clic(editar)

# ── A · Agenda de Hoy ──────────────────────────────────────────────────────
marca <- function(id) isTRUE(ev(sprintf("!!%s.querySelector('.hdt-agregado')", fila(id))))
r3002 <- id_de("3002")
clic(sprintf("%s.querySelector('.hdt-grupo-abrir')", g_ac))
ok("A4: the ✓ comes from the real Agenda queue (3002 is in it; 3001 isn't)", marca(r3002) && !marca(id_de("3001")))
clic(sprintf("%s.querySelector('.hdt-grupo-abrir')", g_ac))
r4 <- id_de("4001")
clic(celda(r4))
# (The table scrolled to its end: the confirmation bar must still be in
# view -- the old one could end up above the viewport, 2026-07-24.)
invisible(ev(sprintf("(function(){ var s = document.querySelector('#%s .hdt-scroll'); s.scrollTop = s.scrollHeight; })()", A)))
Sys.sleep(0.3)
k <- n_reg(); clic(barra("agregar_seleccion"), ver = FALSE)
ok("A2: 'Agregar selección' asks first (Dif4)", isTRUE(ev(sprintf("!document.querySelector('#%s .hdt-confirmar').hidden", A))) && n_reg() == k)
ok("... in the words it always had", identical(conf_texto(), "¿Agregar 1 factura seleccionada a la Agenda del día?"))
ok("... and its confirmation bar is in view with the table scrolled to its end",
   isTRUE(ev(sprintf("(function(){ var r = document.querySelector('#%s .hdt-confirmar').getBoundingClientRect(); return r.top >= 0 && r.bottom <= innerHeight; })()", A))))
clic(sprintf("document.querySelector('#%s .hdt-confirmar-si')", A))
ok("A2: ... and stages it: stage_all, then its ✓ shows",
   poll_until(function() grepl("stage_all \\[1\\] AP NCS USD 4001$", ultima()) && marca(r4), timeout_s = 8))
deseleccionar()
k <- n_reg(); clic(barra("agregar_todo"))
ok("A1: 'Agregar todo' asks in the words it always had, with a real plural",
   identical(conf_texto(), "¿Agregar 12 facturas a la Agenda del día?"))
clic(sprintf("document.querySelector('#%s .hdt-confirmar-si')", A))
ok("A1: 'Agregar todo' stages every visible open invoice, never the paid one",
   poll_until(function() n_reg() > k && grepl("stage_all \\[12\\]", ultima()) && !grepl("1003", ultima()), timeout_s = 8))

# ── P · provisions ─────────────────────────────────────────────────────────
prov <- id_de("PRV-1")
invisible(ev("window.__entradas = []; (function(){ var o = Shiny.setInputValue; Shiny.setInputValue = function(n, v, op){ window.__entradas.push(n + '=' + JSON.stringify(v)); return o.apply(this, arguments); }; })()"))
ok("P1: the provision row is purple with its badge", isTRUE(ev(sprintf("%s.classList.contains('hdt-prov')", fila(prov)))) &&
     grepl("Provisi", ev(sprintf("%s.children[0].textContent", fila(prov)))))
clic(fila(prov, ".querySelector('button[data-accion=provision]')"))
ok("P2: ⚡ opens the pasivos convert dialog for it (pasivos_convert_request = PRV-1), without marking it",
   poll_until(function() any(grepl('^pasivos_convert_request="PRV-1"$', unlist(ev("window.__entradas")))), timeout_s = 8) &&
     !prov %in% sel())

# ── O · bubble, Excel, Ctrl+C, source data ─────────────────────────────────
p <- centro(celda(id_de("1004"), 5L)); invisible(raton("mouseMoved", p))
ok("O1: the full-value bubble opens on a cell", poll_until(function() isTRUE(ev("(function(){ var t = document.querySelector('.hdt-tip'); return !!t && !t.hidden; })()")), timeout_s = 3))
invisible(raton("mouseMoved", list(5, 5))); Sys.sleep(0.4)
clic(sprintf("document.querySelector('#%s .hdt-excel')", A))
ok("O2: Exportar a Excel downloads vencidos_<date>.xlsx (new for Vencidos)",
   poll_until(function() length(list.files(descargas, pattern = "^vencidos_.*\\.xlsx$")) == 1L, timeout_s = 20))
clic(celda(id_de("3005"))); clic(celda(id_de("1004")))
invisible(tryCatch(ev("navigator.clipboard.writeText('')", await = TRUE), error = function(e) NULL))
invisible(b$Input$dispatchKeyEvent(type = "keyDown", key = "c", code = "KeyC", windowsVirtualKeyCode = 67L, modifiers = 2L, commands = list("copy")))
invisible(b$Input$dispatchKeyEvent(type = "keyUp", key = "c", code = "KeyC", windowsVirtualKeyCode = 67L, modifiers = 2L))
leer <- function() tryCatch(ev("navigator.clipboard.readText()", await = TRUE), error = function(e) "")
invisible(poll_until(function() nzchar(leer()), timeout_s = 8))
ok("O2: Ctrl+C copies the marked rows with the formula neutralized", grepl("'=HIPERVINCULO", leer(), fixed = TRUE))
deseleccionar()
clic(editar)
ok("M7: the raw data button is 'Ver datos SAP', as in the lupa", identical(ev(sprintf("%s.textContent", barra("crudos"))), "Ver datos SAP"))
clic(barra("crudos"))
ok("M7: 'Ver datos SAP' opens the raw SAP viewer with the overdue rows (all of them, nothing marked)",
   poll_until(function() grepl("Datos SAP crudos — 3 filas", ev("(document.querySelector('.modal-title')||{}).textContent || ''"), fixed = TRUE), timeout_s = 10))

# ── Height: the footer always shows whole (smoke test, 2026-10-31) ─────────
# With the app's bar above the table (here a 140 px band) the footer ended
# below the window, under the fixed "Notas del mes" button.
ok("fixture app restarted with 3,500 more overdue invoices", arrancar(extra = 3500L) && abrir())
invisible(ev(sprintf("var w = document.querySelector('.vencidos-wrap'); var d = document.createElement('div'); d.style.height = '140px'; d.textContent = 'barra de la app'; w.parentNode.insertBefore(d, w);", A)))
pie_visible <- function() ev(sprintf("(function(){ var p = document.querySelector('#%s .hdt-pie').getBoundingClientRect(),
  n = document.querySelector('.notes-bar-btn').getBoundingClientRect();
  return { pie: p.bottom, notas: n.top, ventana: innerHeight, alto: document.querySelector('#%s .hdt-scroll').getBoundingClientRect().height }; })()", A, A))
for (alto in c(900L, 1080L)) {
  invisible(b$Emulation$setDeviceMetricsOverride(width = 1400L, height = alto, deviceScaleFactor = 1, mobile = FALSE))
  invisible(ev("window.dispatchEvent(new Event('resize'))")); Sys.sleep(0.8)
  m <- pie_visible()
  cat(sprintf("  1400x%d: table %.0f px, footer ends at %.0f, the notes button starts at %.0f
", alto, m$alto, m$pie, m$notas))
  ok(sprintf("1400x%d: the footer shows whole, above the 'Notas del mes' button", alto),
     m$pie <= m$notas && m$pie <= m$ventana)
}
invisible(b$Emulation$setDeviceMetricsOverride(width = 1400L, height = 1000L, deviceScaleFactor = 1, mobile = FALSE))

# ── D6 · nothing overdue ───────────────────────────────────────────────────
ok("fixture app restarted with no overdue invoices", arrancar(vacio = TRUE) && abrir())
v <- ev(sprintf("(function(){ var v = document.querySelector('#%s .hdt-vacio'); return v.textContent + '|' + v.className; })()", A))
ok("D6: its own message with the green check", identical(v, "Sin vencimientos pendientes — todo al día|hdt-vacio hdt-vacio-exito"))
ok("D7: and the badge hides", poll_until(function() identical(ev("document.getElementById('ven_tab_badge').style.display"), "none"), timeout_s = 5))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
