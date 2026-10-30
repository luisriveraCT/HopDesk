# =============================================================================
# tests/test_hd_table_chromote.R
# hd_table engine in a real headless Chrome, against the demo app
# (dev/hd_table_demo, generated data, no S3). Every click, drag and hover is
# a real input event (Input.dispatchMouseEvent / dispatchKeyEvent), never
# element.click(): DT ignored synthetic clicks and the engine mustn't depend
# on them either.
#
# Run from project root:  Rscript tests/test_hd_table_chromote.R
# =============================================================================
cat("=== test_hd_table_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38953L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)

app <- tryCatch(start_bg_app(sprintf('setwd("%s"); source("dev/hd_table_demo/app.R")$value', repo),
                             PORT, timeout_s = 40),
                error = function(e) { message(conditionMessage(e)); NULL })
ok("demo app started", !is.null(app))
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
base <- sprintf("http://127.0.0.1:%d/", PORT)
try(b$Browser$grantPermissions(permissions = list("clipboardReadWrite", "clipboardSanitizedWrite"),
                               origin = sub("/$", "", base)), silent = TRUE)

ev <- function(js, await = FALSE) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE, awaitPromise = await)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
abrir <- function(query) {
  b$Page$navigate(paste0(base, query), wait_ = TRUE)
  poll_until(function() isTRUE(ev("!!document.querySelector('#a1-hdt tbody tr') && !!document.querySelector('#prov-hdt tbody tr')")),
             timeout_s = 30)
}
A <- "a1-hdt"
estado <- function(id = A) ev(sprintf("HDT.estado('%s')", id))

# Center of an element (scrolled into view first), from a JS expression.
# behavior 'instant': Bootstrap 5 sets scroll-behavior: smooth on :root, and a
# smooth scrollIntoView would be measured mid-animation (clicks landed off-screen).
centro <- function(js_el) {
  ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', inline:'nearest', behavior:'instant'});
               var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
}
# k-th painted row (1-based), col-th cell. Skips the window-mode spacer rows.
fila_td <- function(k, col = 2, id = A)
  sprintf("document.querySelectorAll('#%s tbody tr:not(.hdt-esp)')[%d].children[%d]", id, k - 1L, col - 1L)

raton <- function(tipo, x, y, shift = FALSE, botones = 0L, click = 1L)
  b$Input$dispatchMouseEvent(type = tipo, x = x, y = y, button = if (tipo == "mouseMoved" && !botones) "none" else "left",
                             buttons = botones, clickCount = click, modifiers = if (shift) 8L else 0L)
clic <- function(js_el, shift = FALSE) {
  p <- centro(js_el)
  if (shift) b$Input$dispatchKeyEvent(type = "keyDown", key = "Shift", code = "ShiftLeft",
                                      windowsVirtualKeyCode = 16L, modifiers = 8L)
  raton("mouseMoved", p[[1]], p[[2]], shift)
  raton("mousePressed", p[[1]], p[[2]], shift, 1L)
  raton("mouseReleased", p[[1]], p[[2]], shift, 0L)
  if (shift) b$Input$dispatchKeyEvent(type = "keyUp", key = "Shift", code = "ShiftLeft",
                                      windowsVirtualKeyCode = 16L, modifiers = 0L)
  Sys.sleep(0.15)
}
arrastrar <- function(js_el) {
  r <- ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', behavior:'instant'}); var r = e.getBoundingClientRect();
                    return [r.left + 4, r.top + r.height/2, r.left + Math.min(r.width - 4, 140)]; })()", js_el))
  raton("mouseMoved", r[[1]], r[[2]])
  raton("mousePressed", r[[1]], r[[2]], botones = 1L)
  for (x in seq(r[[1]], r[[3]], length.out = 8)) raton("mouseMoved", x, r[[2]], botones = 1L)
  raton("mouseReleased", r[[3]], r[[2]])
  Sys.sleep(0.15)
}
texto_sombreado <- function() ev("String(window.getSelection())")
limpiar_texto <- function() ev("window.getSelection().removeAllRanges()")
# seleccion() as the SERVER sees it (the demo prints it with str()).
sel_servidor <- function(panel = "panel_a") {
  txt <- ev(sprintf("document.getElementById('%s').innerText", panel))
  linea <- sub("\n.*$", "", sub("^.*?seleccion \\([0-9]+\\): ?", "", txt))
  strsplit(trimws(linea), " +")[[1]]
}
deseleccionar <- function() { ev(sprintf("(document.querySelector('#%s .hdt-deseleccionar')||{click:function(){}}).click()", A)); Sys.sleep(0.3) }

ok("demo loads both tables (60 rows in class A)", abrir("?n=60&seed=1&alto=300px"))

# ── Click marks the row and shades no text ────────────────────────────────
clic(fila_td(2))
id2 <- estado()$visibles[[2]]
ok("a click marks the row", identical(unlist(estado()$seleccion), id2))
ok("... and leaves no text shaded", identical(texto_sombreado(), ""))
ok("... and the server's seleccion() gets it",
   poll_until(function() identical(sel_servidor(), id2), timeout_s = 5))
clic(fila_td(2))
ok("a second click unmarks it", length(unlist(estado()$seleccion)) == 0L)

# ── Drag shades text and doesn't mark ─────────────────────────────────────
arrastrar(fila_td(5))
ok("a drag over a cell shades its text", nzchar(texto_sombreado()))
ok("... and marks nothing", length(unlist(estado()$seleccion)) == 0L)
limpiar_texto()

# ── Sort, then Shift + click: range in visible order, skipping locked rows ─
clic(sprintf("document.querySelectorAll('#%s thead th')[3]", A))    # Importe
ok("clicking a title sorts by its real value (ascending)", {
  imp <- ev(sprintf("Array.from(document.querySelectorAll('#%s tbody tr')).map(function(r){return Number(r.children[3].textContent.replace(/[$,]/g,''))})", A))
  !is.unsorted(unlist(imp))
})
ok("the title shows the sort arrow", grepl("▲", ev(sprintf("document.querySelectorAll('#%s thead th')[3].textContent", A))))
vis <- unlist(estado()$visibles)
bloq_vis <- unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s tbody tr')).map(function(r){return r.classList.contains('hdt-bloq')})", A)))
desde <- which(!bloq_vis)[1]
hasta <- which(!bloq_vis & seq_along(bloq_vis) > desde + 8 & cumsum(bloq_vis) > sum(bloq_vis[seq_len(desde)]))[1]
clic(fila_td(desde))
clic(fila_td(hasta), shift = TRUE)
esperado <- vis[desde:hasta][!bloq_vis[desde:hasta]]
ok(sprintf("Shift + click marks visible rows %d..%d in visible order", desde, hasta),
   identical(unlist(estado()$seleccion), esperado))
ok("... skipping the locked rows in between", any(bloq_vis[desde:hasta]) &&
     !any(vis[desde:hasta][bloq_vis[desde:hasta]] %in% unlist(estado()$seleccion)))
ok("... with no text shaded", identical(texto_sombreado(), ""))
ok("seleccion() reaches the server in visible order",
   poll_until(function() identical(sel_servidor(), esperado), timeout_s = 5))
deseleccionar()

# Click order reversed after sorting descending: still visible order.
clic(sprintf("document.querySelectorAll('#%s thead th')[3]", A))    # Importe, now descending
ok("a second click on the title reverses the order",
   grepl("▼", ev(sprintf("document.querySelectorAll('#%s thead th')[3].textContent", A))))
vis <- unlist(estado()$visibles)
bloq_vis <- unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s tbody tr')).map(function(r){return r.classList.contains('hdt-bloq')})", A)))
dos <- which(!bloq_vis)[1:2]
clic(fila_td(dos[2])); clic(fila_td(dos[1]))
ok("rows clicked bottom-first still arrive in visible order",
   poll_until(function() identical(sel_servidor(), vis[dos]), timeout_s = 5))
deseleccionar()

# ── Locked rows can't be marked ───────────────────────────────────────────
k_bloq <- which(bloq_vis)[1]
clic(fila_td(k_bloq))
ok("clicking a locked row marks nothing", length(unlist(estado()$seleccion)) == 0L)

# ── Select all with a search: only visible, never locked ──────────────────
p <- centro(sprintf("document.querySelector('#%s .hdt-buscar')", A))
raton("mousePressed", p[[1]], p[[2]], botones = 1L); raton("mouseReleased", p[[1]], p[[2]])
b$Input$insertText(text = "acero")
Sys.sleep(0.4)
vis_q <- unlist(estado()$visibles)
ok("search is accent/case-free and narrows the rows ('acero' -> Aceros)", length(vis_q) > 0 && length(vis_q) < 60 &&
     all(grepl("Aceros", unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s tbody tr')).map(function(r){return r.children[1].textContent})", A))))))
ok("the counter follows the search",
   grepl(sprintf("^%d facturas?$", length(vis_q)), ev(sprintf("document.querySelector('#%s .hdt-contador').textContent", A))))
clic(sprintf("document.querySelector('#%s .hdt-barra .hdt-todos')", A))   # Seleccionar todos
bloq_q <- unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s tbody tr')).map(function(r){return r.classList.contains('hdt-bloq')})", A)))
ok("Seleccionar todos marks exactly the visible, unlocked rows",
   identical(unlist(estado()$seleccion), vis_q[!bloq_q]))
ok("... and the button now offers to unmark",
   identical(ev(sprintf("document.querySelector('#%s .hdt-barra .hdt-todos').textContent", A)), "Deseleccionar"))
ok("the footer sums the selection",
   grepl(sprintf("^%d seleccionadas?", sum(!bloq_q)), ev(sprintf("document.querySelector('#%s .hdt-pie').innerText", A))))
clic(sprintf("document.querySelector('#%s .hdt-barra .hdt-todos')", A))
ok("a second click unmarks them", length(unlist(estado()$seleccion)) == 0L)
ev(sprintf("var i=document.querySelector('#%s .hdt-buscar'); i.value=''; i.dispatchEvent(new Event('input'))", A))
Sys.sleep(0.3)

# ── Bubble: full value after 350 ms, copies the raw value ─────────────────
limpiar_texto()
imp_td <- fila_td(3, col = 4)
crudo <- ev(sprintf("(function(){ var td=%s; return HDT_COPIA_PRUEBA = td.textContent; })()", imp_td))
p <- centro(imp_td)
raton("mouseMoved", p[[1]] - 5, p[[2]])
raton("mouseMoved", p[[1]], p[[2]])
Sys.sleep(0.15)
ok("the bubble doesn't show before 350 ms", isTRUE(ev("!document.querySelector('.hdt-tip') || document.querySelector('.hdt-tip').hidden")))
ok("the bubble shows after 350 ms with the full value",
   poll_until(function() isTRUE(ev(sprintf("(function(){var t=document.querySelector('.hdt-tip'); return !!t && !t.hidden && t.querySelector('.hdt-tip-texto').textContent === %s; })()",
                                           jsonlite::toJSON(crudo, auto_unbox = TRUE)))), timeout_s = 2))
clic("document.querySelector('.hdt-tip .hdt-tip-copiar')")
copiado <- tryCatch(ev("navigator.clipboard.readText()", await = TRUE), error = function(e) NA)
esperado_crudo <- sub("\\.?0+$", "", gsub("[$,]", "", crudo))
cat(sprintf("  shown %s -> clipboard %s (expected raw %s)\n", crudo, copiado, esperado_crudo))
ok("the copy button puts the RAW value on the clipboard", identical(copiado, esperado_crudo))

# ── Search also finds the raw value ───────────────────────────────────────
id_crudo <- ev(sprintf("(function(){ return HDT.estado('%s').visibles[2]; })()", A))
p <- centro(sprintf("document.querySelector('#%s .hdt-buscar')", A))
raton("mousePressed", p[[1]], p[[2]], botones = 1L); raton("mouseReleased", p[[1]], p[[2]])
b$Input$insertText(text = esperado_crudo)
Sys.sleep(0.4)
ok(sprintf("searching the raw amount '%s' (shown as %s) finds its row", esperado_crudo, crudo),
   id_crudo %in% unlist(estado()$visibles) && length(unlist(estado()$visibles)) < 60)
ev(sprintf("var i=document.querySelector('#%s .hdt-buscar'); i.value=''; i.dispatchEvent(new Event('input'))", A))
Sys.sleep(0.3)

# ── Footer never covers the last row ──────────────────────────────────────
ev(sprintf("var s=document.querySelector('#%s .hdt-scroll'); s.scrollTop = s.scrollHeight;", A))
Sys.sleep(0.3)
ok("scrolled to the bottom, the last row ends above the footer", isTRUE(ev(sprintf(
  "(function(){ var rows=document.querySelectorAll('#%s tbody tr'); var last=rows[rows.length-1].getBoundingClientRect();
     var pie=document.querySelector('#%s .hdt-pie').getBoundingClientRect(); return last.bottom <= pie.top + 0.5; })()", A, A))))

# ── "Cambiar datos" keeps scroll, sort and selection ──────────────────────
ev(sprintf("var s=document.querySelector('#%s .hdt-scroll'); s.scrollTop = 0;", A))
vis <- unlist(estado()$visibles)
bloq_vis <- unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s tbody tr')).map(function(r){return r.classList.contains('hdt-bloq')})", A)))
tres <- which(!bloq_vis)[c(2, 5, 9)]
for (k in tres) clic(fila_td(k))
ev(sprintf("var s=document.querySelector('#%s .hdt-scroll'); s.scrollTop = 157;", A))
Sys.sleep(0.2)
antes <- estado()
imp_antes <- ev(sprintf("document.querySelectorAll('#%s tbody tr')[0].children[3].textContent", A))
p <- centro("document.getElementById('cambiar')")
raton("mouseMoved", p[[1]], p[[2]]); raton("mousePressed", p[[1]], p[[2]], botones = 1L); raton("mouseReleased", p[[1]], p[[2]])
ok("new data arrived (amounts changed)", poll_until(function() {
  st <- estado(); !identical(unlist(st$visibles), unlist(antes$visibles)) ||
    !identical(ev(sprintf("document.querySelectorAll('#%s tbody tr')[0].children[3].textContent", A)), imp_antes)
}, timeout_s = 8))
Sys.sleep(0.3)
despues <- estado()
ok("... scroll position kept", identical(despues$scrollTop, antes$scrollTop))
ok("... sort kept", identical(despues$orden, antes$orden))
ok("... selection kept", identical(sort(unlist(despues$seleccion)), sort(unlist(antes$seleccion))))
ok("... container not re-created", isTRUE(ev(sprintf("document.querySelectorAll('#%s .hdt-cuerpo').length === 1", A))))

# ── Two tables on one page don't step on each other ───────────────────────
sel_a <- unlist(estado()$seleccion)
clic(sprintf("document.querySelectorAll('#prov-hdt tbody tr')[2].querySelector('input')"))
d_id <- unlist(estado("prov-hdt")$visibles)[3]
ok("marking a row in table D (checkbox) selects it there",
   identical(unlist(estado("prov-hdt")$seleccion), d_id))
ok("... and leaves table A's selection alone", identical(unlist(estado()$seleccion), sel_a))
ok("... each server module gets its own seleccion()",
   poll_until(function() identical(sel_servidor("panel_d"), d_id) && identical(sort(sel_servidor()), sort(sel_a)), timeout_s = 5))
clic("document.querySelectorAll('#prov-hdt tbody tr')[3].children[1]")
ok("with checkboxes on, a plain row click doesn't select", identical(unlist(estado("prov-hdt")$seleccion), d_id))

# ── A container re-created with the same id (modal closed and reopened) ───
# The module's server is created once and datos() doesn't change, so no new
# message arrives: the table must paint from the last one it kept.
ok("table A has a selection before the container goes away", length(unlist(estado()$seleccion)) > 0)
n_msgs <- tryCatch(ev("HDT.recibidos()"), error = function(e) NA)
ev(sprintf("(function(){ var el=document.getElementById('%s'), padre=el.parentNode, ns=el.getAttribute('data-ns');
  el.remove();
  setTimeout(function(){ var nuevo=document.createElement('div'); nuevo.id='%s'; nuevo.className='hdt';
    nuevo.setAttribute('data-ns', ns); nuevo.textContent='Cargando…'; padre.appendChild(nuevo); }, 300); })()", A, A))
ok("the re-created container paints its rows", poll_until(function()
  isTRUE(ev(sprintf("document.querySelectorAll('#%s tbody tr').length === 60", A))), timeout_s = 5))
ok("... without any new data from the server", identical(ev("HDT.recibidos()"), n_msgs))
ok("... with an empty selection", length(unlist(estado()$seleccion)) == 0L)
ok("... and the server's seleccion() is emptied too",
   poll_until(function() length(sel_servidor()) == 0L, timeout_s = 5))
ok("... and it still works: a click marks a row", {
  clic(fila_td(1)); length(unlist(estado()$seleccion)) == 1L || isTRUE(ev(sprintf(
    "document.querySelectorAll('#%s tbody tr')[0].classList.contains('hdt-bloq')", A)))
})

# ── Performance: 5,000 rows ───────────────────────────────────────────────
ok("demo loads with 5,000 rows", abrir("?n=5000&seed=2"))
Sys.sleep(0.5)
pint <- estado()$medidas$pintado
clic(sprintf("document.querySelectorAll('#%s thead th')[3]", A))
ord1 <- estado()$medidas$orden
clic(sprintf("document.querySelectorAll('#%s thead th')[1]", A))
ord2 <- estado()$medidas$orden
cat(sprintf("  5,000 rows: initial paint %.0f ms, sort by Importe %.0f ms, sort by Proveedor %.0f ms\n",
            pint, ord1, ord2))
ok("initial paint of 5,000 rows < 300 ms", pint < 300)
ok("sort of 5,000 rows < 150 ms", max(ord1, ord2) < 150)
# Same sort, timed until the frame is on screen (JS + layout + paint).
a_pantalla <- ev(sprintf("new Promise(function(res){ var th=document.querySelectorAll('#%s thead th')[3], t0=performance.now();
  th.dispatchEvent(new MouseEvent('click',{bubbles:true}));
  requestAnimationFrame(function(){ setTimeout(function(){ res(performance.now()-t0); },0); }); })", A), await = TRUE)
cat(sprintf("  sort of 5,000 rows to the visible frame: %.0f ms\n", a_pantalla))

# ── Window mode (more than HDT_UMBRAL_VENTANA = 800 visible rows) ──────────
st <- estado()
ok("5,000 rows paint in window mode, only the rows in view",
   identical(st$modo, "ventana") && st$pintadas < 200)
anchos <- function() unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s thead th')).map(function(th){return Math.round(th.getBoundingClientRect().width)})", A)))
w0 <- anchos()
ev(sprintf("var s=document.querySelector('#%s .hdt-scroll'); s.scrollTop = s.scrollHeight/2;", A)); Sys.sleep(0.4)
ok("column widths stay frozen while scrolling", identical(anchos(), w0))

ev(sprintf("var s=document.querySelector('#%s .hdt-scroll'); s.scrollTop = 0;", A)); Sys.sleep(0.4)
vis <- unlist(estado()$visibles); bloq <- unlist(estado()$bloqueadas)
k_ini <- which(!vis %in% bloq)[3]
clic(fila_td(k_ini))
ev(sprintf("var s=document.querySelector('#%s .hdt-scroll'); s.scrollTop = s.scrollHeight;", A)); Sys.sleep(0.5)
ultima <- sprintf("(function(){ var r=document.querySelectorAll('#%s tbody tr:not(.hdt-esp)'); return r[r.length-1].children[1]; })()", A)
clic(ultima, shift = TRUE)
esperado <- vis[k_ini:length(vis)]; esperado <- esperado[!esperado %in% bloq]
ok(sprintf("Shift + click from row %d to the last of 5,000 marks rows that were never painted (%d)",
           k_ini, length(esperado)),
   identical(unlist(estado()$seleccion), esperado))
ok("... and the server gets them in visible order",
   poll_until(function() identical(sel_servidor(), esperado), timeout_s = 8))
ok("in window mode, scrolled to the bottom, the last row ends above the footer", isTRUE(ev(sprintf(
  "(function(){ var r=document.querySelectorAll('#%s tbody tr:not(.hdt-esp)'); var last=r[r.length-1].getBoundingClientRect();
     var pie=document.querySelector('#%s .hdt-pie').getBoundingClientRect(); return last.bottom <= pie.top + 0.5; })()", A, A))))
ok("... and that last row is the last visible one", identical(
  ev(sprintf("(function(){ var r=document.querySelectorAll('#%s tbody tr:not(.hdt-esp)'); return r[r.length-1].children[2].textContent; })()", A)),
  ev(sprintf("(function(){ var v=HDT.estado('%s').visibles; return v[v.length-1].replace(/^F0*/, ''); })()", A)) |>
    (\(x) as.character(100000 + as.integer(x)))()))

# Keep where you were, in window mode.
ev(sprintf("var s=document.querySelector('#%s .hdt-scroll'); s.scrollTop = 30000;", A)); Sys.sleep(0.4)
antes <- estado()
n_antes <- ev("HDT.recibidos()")
p <- centro("document.getElementById('cambiar')")
raton("mouseMoved", p[[1]], p[[2]]); raton("mousePressed", p[[1]], p[[2]], botones = 1L); raton("mouseReleased", p[[1]], p[[2]])
ok("window mode: new data arrives", poll_until(function() ev("HDT.recibidos()") > n_antes, timeout_s = 15))
Sys.sleep(0.4)
despues <- estado()
ok("window mode: new data keeps scroll, sort, selection and mode",
   identical(despues$scrollTop, antes$scrollTop) && identical(despues$orden, antes$orden) &&
     identical(sort(unlist(despues$seleccion)), sort(unlist(antes$seleccion))) && identical(despues$modo, "ventana"))
deseleccionar()

# Search below the threshold switches to full painting, and back.
p <- centro(sprintf("document.querySelector('#%s .hdt-buscar')", A))
raton("mousePressed", p[[1]], p[[2]], botones = 1L); raton("mouseReleased", p[[1]], p[[2]])
b$Input$insertText(text = "acero"); Sys.sleep(0.5)
st <- estado()
cat(sprintf("  search 'acero': %d visible rows, mode %s, %d painted\n", length(unlist(st$visibles)), st$modo, st$pintadas))
ok("a search that leaves <= 800 rows paints them all (full mode)",
   length(unlist(st$visibles)) <= 800 && identical(st$modo, "completo") && st$pintadas == length(unlist(st$visibles)))
ev(sprintf("var i=document.querySelector('#%s .hdt-buscar'); i.value=''; i.dispatchEvent(new Event('input'))", A)); Sys.sleep(0.4)
ok("clearing it goes back to window mode", identical(estado()$modo, "ventana"))

# ── 300 rows must not get slower than before the window mode ──────────────
# Median of 3 loads, generous cut: a single timing is noisy when the machine
# is busy (another R process running in parallel gave 103 ms once).
p300 <- vapply(1:3, function(r) {
  abrir(sprintf("?n=300&seed=2&r=%d", r)); Sys.sleep(0.5)
  estado()$medidas$pintado
}, numeric(1))
cat(sprintf("  300 rows: initial paint %s ms -> median %.0f ms (mode %s)\n",
            paste(round(p300), collapse = " / "), stats::median(p300), estado()$modo))
ok("300 rows paint in full mode, median of 3 < 150 ms",
   identical(estado()$modo, "completo") && stats::median(p300) < 150)

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
