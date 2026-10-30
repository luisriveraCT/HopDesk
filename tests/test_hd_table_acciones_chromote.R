# =============================================================================
# tests/test_hd_table_acciones_chromote.R
# T03a pieces of the hd_table engine in a real headless Chrome, against the
# demo app (dev/hd_table_demo: Bootstrap 5 flatly like HopDesk, generated
# data, no S3), at 1400x1080. Every click, double click, key and hover is a
# real input event (Input.dispatchMouseEvent / dispatchKeyEvent).
#
# Run from project root:  Rscript tests/test_hd_table_acciones_chromote.R
# =============================================================================
cat("=== test_hd_table_acciones_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38961L
app <- NULL; b <- NULL
descargas <- file.path(tempdir(), "hdt_descargas")
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app); unlink(descargas, recursive = TRUE) }, add = TRUE)

app <- tryCatch(start_bg_app(sprintf('setwd("%s"); source("dev/hd_table_demo/app.R")$value', repo),
                             PORT, timeout_s = 40),
                error = function(e) { message(conditionMessage(e)); NULL })
ok("demo app started", !is.null(app))
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
b$Emulation$setDeviceMetricsOverride(width = 1400L, height = 1080L, deviceScaleFactor = 1, mobile = FALSE)
base <- sprintf("http://127.0.0.1:%d", PORT)
try(b$Browser$grantPermissions(permissions = list("clipboardReadWrite", "clipboardSanitizedWrite"), origin = base),
    silent = TRUE)
unlink(descargas, recursive = TRUE); dir.create(descargas)
b$Browser$setDownloadBehavior(behavior = "allow", downloadPath = normalizePath(descargas, winslash = "\\"))

ev <- function(js, await = FALSE) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE, awaitPromise = await)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
A <- "a1-hdt"
abrir <- function(query) {
  b$Page$navigate(paste0(base, "/", query, "&periodo=0"), wait_ = TRUE)
  ok <- poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s')", A))), timeout_s = 30)
  Sys.sleep(0.6)   # auto height settles on the next frame
  ok
}
estado <- function(id = A) ev(sprintf("HDT.estado('%s')", id))
# A row's element by id (full mode: every row is painted).
fila <- function(id, sel = "") sprintf("(function(){ var v = HDT.estado('%s').visibles; return document.querySelectorAll('#%s tbody tr:not(.hdt-esp)')[v.indexOf('%s')]%s; })()", A, A, id, sel)
# behavior 'instant': Bootstrap 5 sets scroll-behavior: smooth on :root.
centro <- function(js_el) ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', behavior:'instant'});
  var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
raton <- function(tipo, p, botones = 0L, n = 1L, shift = FALSE)
  b$Input$dispatchMouseEvent(type = tipo, x = p[[1]], y = p[[2]], button = if (tipo == "mouseMoved") "none" else "left",
                             buttons = botones, clickCount = n, modifiers = if (shift) 8L else 0L)
clic <- function(js_el) {
  p <- centro(js_el)
  raton("mouseMoved", p); raton("mousePressed", p, 1L); raton("mouseReleased", p)
  Sys.sleep(0.25)
}
doble <- function(js_el) {
  p <- centro(js_el)
  raton("mouseMoved", p)
  raton("mousePressed", p, 1L, 1L); raton("mouseReleased", p, 0L, 1L)
  raton("mousePressed", p, 1L, 2L); raton("mouseReleased", p, 0L, 2L)
  Sys.sleep(0.5)
}
escribir <- function(js_input, texto) {
  p <- centro(js_input); raton("mousePressed", p, 1L); raton("mouseReleased", p)
  ev(sprintf("var i=%s; i.value=''; i.dispatchEvent(new Event('input'))", js_input))
  if (nzchar(texto)) b$Input$insertText(text = texto)
  Sys.sleep(0.4)
}
# Newest line of the demo's accion() log: "HH:MM:SS  accion  [n] ids".
ultima_accion <- function() {
  txt <- ev("document.getElementById('log_a').innerText")
  linea <- strsplit(txt %||% "", "\n")[[1]][1]
  if (is.na(linea) || !nzchar(trimws(linea))) return(list(accion = NA, n = NA, ids = character(0)))
  m <- regmatches(linea, regexec("^\\S+\\s+(\\S+)\\s+\\[(\\d+)\\]\\s*(.*)$", linea))[[1]]
  list(accion = m[2], n = as.integer(m[3]), ids = strsplit(trimws(m[4]), " +")[[1]])
}
n_log <- function() length(strsplit(ev("document.getElementById('log_a').innerText") %||% "", "\n")[[1]])
sel_a <- function() unlist(estado()$seleccion)
deseleccionar <- function() { ev(sprintf("(document.querySelector('#%s .hdt-deseleccionar')||{click:function(){}}).click()", A)); Sys.sleep(0.3) }
botones <- function(id) unlist(ev(sprintf("Array.from(%s).map(function(b){ return b.dataset.accion; })",
                                          fila(id, ".querySelectorAll('td.hdt-acc button')"))))

ok("demo loads (60 rows, Bootstrap 5)", abrir("?n=60&seed=1"))
vis0 <- unlist(estado()$visibles)
bloq <- unlist(estado()$bloqueadas)

# ── Chips (fichas) ─────────────────────────────────────────────────────────
cifras <- function() {
  x <- unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s .hdt-ficha')).map(function(b){ return (b.dataset.valor || 'todas') + '=' + b.querySelector('.hdt-ficha-cifra').textContent; })", A)))
  setNames(as.integer(sub(".*=", "", x)), sub("=.*", "", x))
}
c0 <- cifras()
ok("chips: 'Todas' counts every row and the values add up to it",
   c0[["todas"]] == 60L && sum(c0[names(c0) != "todas"]) == 60L)
escribir(sprintf("document.querySelector('#%s .hdt-buscar')", A), "aceros")
c1 <- cifras(); n_busq <- length(unlist(estado()$visibles))
cat(sprintf("  search 'aceros': %d rows; chips %s\n", n_busq, paste(names(c1), c1, collapse = ", ")))
ok("chip counts are computed on what the search leaves",
   c1[["todas"]] == n_busq && sum(c1[names(c1) != "todas"]) == n_busq && n_busq < 60)
clic(sprintf("document.querySelector('#%s .hdt-ficha[data-valor=pendiente]')", A))
ok("touching a chip filters (and combines with the search)",
   length(unlist(estado()$visibles)) == c1[["pendiente"]] &&
     isTRUE(ev(sprintf("document.querySelector('#%s .hdt-ficha[data-valor=pendiente]').getAttribute('aria-pressed') === 'true'", A))))
clic(sprintf("document.querySelector('#%s .hdt-ficha[data-valor=pendiente]')", A))
ok("touching it again removes the filter", length(unlist(estado()$visibles)) == n_busq)
escribir(sprintf("document.querySelector('#%s .hdt-buscar')", A), "")

# ── Double click: details, never marking; locked rows too ─────────────────
abierta <- setdiff(vis0, bloq)[2]
doble(fila(abierta, ".children[1]"))
ok("double click sends accion 'detalle' with that row's id",
   identical(ultima_accion()$accion, "detalle") && identical(ultima_accion()$ids, abierta))
ok("... and leaves the row unmarked", length(sel_a()) == 0L)
ok("... and no word shaded", identical(ev("String(window.getSelection())"), ""))
doble(fila(bloq[1], ".children[1]"))
ok("a locked (paid) row also opens its details",
   identical(ultima_accion()$accion, "detalle") && identical(ultima_accion()$ids, bloq[1]))

# ── Row buttons ────────────────────────────────────────────────────────────
clic(fila(abierta, ".querySelector('button[data-accion=editar]')"))
ok("a row button sends its action with the row's id", identical(ultima_accion()$accion, "editar") &&
     identical(ultima_accion()$ids, abierta))
ok("... and doesn't select the row", length(sel_a()) == 0L)
ok(".acciones hides the convert button: F00009 shows it, F00018 (also a provision) doesn't",
   "provision" %in% botones("F00009") && !"provision" %in% botones("F00018") && !"provision" %in% botones("F00001"))
ok("every row's buttons follow the catalog order",
   all(vapply(vis0[1:20], function(id) { x <- botones(id)
     identical(x, intersect(c("crear_provision", "agregar", "editar", "provision", "detalle", "eliminar"), x)) }, logical(1))))
estados <- unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s tbody tr')).map(function(r){ return r.children[6].textContent; })", A)))
pend <- vis0[estados == "Pendiente"][1]; no_pend <- vis0[estados == "Programada"][1]
ok("'Crear provisión' comes first, as text, only on rows whose .acciones include it",
   identical(botones(pend)[1], "crear_provision") && !"crear_provision" %in% botones(no_pend) &&
     identical(ev(fila(pend, ".querySelector('button[data-accion=crear_provision]').textContent")), "Crear provisión"))
clic(fila(pend, ".querySelector('button[data-accion=crear_provision]')"))
ok("... and sends accion 'crear_provision' without selecting", identical(ultima_accion()$accion, "crear_provision") &&
     identical(ultima_accion()$ids, pend) && length(sel_a()) == 0L)
ok("the text button doesn't break the right alignment: every actions cell ends at the same x",
   length(unique(round(unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s td.hdt-acc')).map(function(td){ var b=td.lastElementChild; return b.getBoundingClientRect().right; })", A)))))) == 1L)

# eliminar always confirms
k <- n_log()
clic(fila(abierta, ".querySelector('button[data-accion=eliminar]')"))
ok("✕ on a row asks '¿Eliminar 1 fila?' and sends nothing yet",
   identical(ev(sprintf("document.querySelector('#%s .hdt-confirmar-texto').textContent", A)), "¿Eliminar 1 fila?") &&
     n_log() == k)
clic(sprintf("document.querySelector('#%s .hdt-confirmar-si')", A))
ok("... Confirmar sends 'eliminar' with that id", identical(ultima_accion()$accion, "eliminar") &&
     identical(ultima_accion()$ids, abierta))

# ── Bar actions and confirmation ───────────────────────────────────────────
dos <- setdiff(vis0, bloq)[c(3, 5)]
clic(fila(dos[2], ".children[1]")); clic(fila(dos[1], ".children[1]"))
ok("the action bar appears with the selection and says how many",
   grepl("^2 seleccionadas", ev(sprintf("document.querySelector('#%s .hdt-acciones').innerText", A))))
k <- n_log()
clic(sprintf("document.querySelector('#%s .hdt-acciones button[data-accion=mandar_papelera]')", A))
ok("a confirming bar action opens the confirmation bar and doesn't fire accion()",
   grepl("2 filas", ev(sprintf("document.querySelector('#%s .hdt-confirmar').innerText", A))) && n_log() == k)
tercera <- setdiff(vis0, bloq)[7]
clic(fila(tercera, ".children[1]"))
ok("changing the selection closes the confirmation bar, still without accion()",
   isTRUE(ev(sprintf("document.querySelector('#%s .hdt-confirmar').hidden", A))) && n_log() == k)
clic(sprintf("document.querySelector('#%s .hdt-acciones button[data-accion=mandar_papelera]')", A))
clic(sprintf("document.querySelector('#%s .hdt-confirmar-si')", A))
ok("Confirmar fires accion() with the marked ids in visible order",
   identical(ultima_accion()$accion, "mandar_papelera") &&
     identical(ultima_accion()$ids, vis0[vis0 %in% c(dos, tercera)]))
deseleccionar()
ok("an action without sin_seleccion hides when nothing is marked",
   isTRUE(ev(sprintf("!document.querySelector('#%s .hdt-acciones button[data-accion=agregar_seleccion]')", A))))
clic(sprintf("document.querySelector('#%s .hdt-acciones button[data-accion=agregar_todo]')", A))
ok("'Agregar todo' (sin_seleccion) sends the visible ids, without locked rows",
   identical(ultima_accion()$accion, "agregar_todo") && identical(ultima_accion()$ids[1:8], setdiff(vis0, bloq)[1:8]) &&
     ultima_accion()$n == length(setdiff(vis0, bloq)))

# ── Ctrl+C copies the marked rows as raw, neutralized TSV ─────────────────
formula_id <- vis0[unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s tbody tr')).map(function(r){ return r.children[1].textContent.charAt(0) === '='; })", A)))]
formula_id <- setdiff(formula_id, bloq)[1]
otra <- setdiff(vis0, c(bloq, formula_id))[1]
clic(fila(otra, ".children[1]")); clic(fila(formula_id, ".children[1]"))
b$Input$dispatchKeyEvent(type = "keyDown", key = "c", code = "KeyC", windowsVirtualKeyCode = 67L, modifiers = 2L,
                         commands = list("copy"))
b$Input$dispatchKeyEvent(type = "keyUp", key = "c", code = "KeyC", windowsVirtualKeyCode = 67L, modifiers = 2L)
Sys.sleep(0.3)
tsv <- tryCatch(ev("navigator.clipboard.readText()", await = TRUE), error = function(e) "")
lineas <- strsplit(tsv, "\r\n")[[1]]
cat("  clipboard:\n   ", paste(lineas, collapse = "\n    "), "\n")
celdas <- strsplit(lineas, "\t")
ok("Ctrl+C copies header + marked rows in visible order, tab-separated",
   length(lineas) == 3L && identical(celdas[[1]][1:4], c("Empresa", "Proveedor", "Documento", "Importe")) &&
     identical(vapply(celdas[-1], `[`, "", 3), as.character(100000 + as.integer(sub("^F0*", "", vis0[vis0 %in% c(otra, formula_id)])))))
ok("... with raw values (amount without $ or commas) and the formula neutralized",
   any(grepl("^'=HIPERVINCULO", vapply(celdas[-1], `[`, "", 2))) &&
     all(grepl("^-?[0-9]+(\\.[0-9]+)?$", vapply(celdas[-1], `[`, "", 4))))
ok("... and says how many rows were copied",
   identical(ev(sprintf("document.querySelector('#%s .hdt-aviso').textContent", A)), "2 filas copiadas"))

# ── Two tables don't step on each other ───────────────────────────────────
sel_antes <- sel_a()
clic("document.querySelectorAll('#prov-hdt tbody tr')[1].querySelector('input')")
ok("marking a row in the catalog doesn't touch table A, and A's actions never reach the catalog",
   identical(sel_a(), sel_antes) && length(unlist(estado("prov-hdt")$seleccion)) == 1L &&
     !grepl("accion: List", ev("document.getElementById('panel_d').innerText")))
deseleccionar()

# ── Badges: one height, the same in both densities ────────────────────────
altos <- function() unique(round(unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s .hdt-ins')).map(function(e){ return e.getBoundingClientRect().height; })", A))), 1))
h_comp <- altos()
clic(sprintf("Array.from(document.querySelectorAll('#%s .hdt-barra .hdt-btn')).filter(function(b){ return b.textContent === 'Compacta'; })[0]", A))
Sys.sleep(0.4)
h_norm <- altos()
cat(sprintf("  badge heights: compact %s, normal %s\n", paste(h_comp, collapse = "/"), paste(h_norm, collapse = "/")))
ok("every badge (company, state, provision) has one height, the same in both densities",
   length(h_comp) == 1L && identical(h_comp, h_norm))
clic(sprintf("Array.from(document.querySelectorAll('#%s .hdt-barra .hdt-btn')).filter(function(b){ return b.textContent === 'Normal'; })[0]", A))

# ── The bubble never pops up from a scroll under a still cursor ────────────
ev("var t=document.querySelector('.hdt-tip'); if (t) t.hidden = true;")
p <- centro(fila(vis0[5], ".children[1]"))
raton("mouseMoved", c(p[[1]] - 3, p[[2]])); raton("mouseMoved", p)
Sys.sleep(0.1)
ev(sprintf("document.querySelector('#%s .hdt-scroll').scrollTop += 120;", A))
Sys.sleep(0.7)
ok("scrolling under a still cursor doesn't open the full-value bubble",
   isTRUE(ev("!document.querySelector('.hdt-tip') || document.querySelector('.hdt-tip').hidden")))
raton("mouseMoved", c(p[[1]] + 4, p[[2]] + 1)); raton("mouseMoved", c(p[[1]] + 8, p[[2]] + 1))
ok("... but moving the mouse onto a cell still does",
   poll_until(function() isTRUE(ev("!!document.querySelector('.hdt-tip') && !document.querySelector('.hdt-tip').hidden")), timeout_s = 2))

# ── Excel: same filter and order as the screen ────────────────────────────
escribir(sprintf("document.querySelector('#%s .hdt-buscar')", A), "a")
clic(sprintf("document.querySelectorAll('#%s thead th')[3]", A)); clic(sprintf("document.querySelectorAll('#%s thead th')[3]", A))
pantalla <- unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s tbody tr')).map(function(r){ return r.children[2].textContent; })", A)))
clic(sprintf("document.querySelector('#%s .hdt-excel')", A))
ok("Exportar a Excel downloads <tabla>_<date>.xlsx",
   poll_until(function() length(list.files(descargas, pattern = "\\.xlsx$")) == 1L, timeout_s = 15))
xl <- list.files(descargas, pattern = "\\.xlsx$", full.names = TRUE)
if (length(xl)) {
  Sys.sleep(0.5)
  x <- openxlsx::read.xlsx(xl[1], detectDates = TRUE)
  cat(sprintf("  %s: %d rows (screen %d)\n", basename(xl[1]), nrow(x), length(pantalla)))
  ok("... with the screen's filter and order", identical(as.character(x$Documento), pantalla))
  ok("... amounts as numbers and dates as dates", is.numeric(x$Importe) && inherits(x$Vence, "Date"))
  ok("... and the formula-looking name neutralized", all(!grepl("^=", x$Proveedor)) &&
       (!any(grepl("HIPERVINCULO", x$Proveedor)) || any(grepl("^'=HIPERVINCULO", x$Proveedor))))
  ok("... file name is <tabla>_<YYYY-MM-DD>.xlsx", grepl("^demo_a_1_\\d{4}-\\d{2}-\\d{2}\\.xlsx$", basename(xl[1])))
}

# ── Automatic height (Bootstrap 5, 1080 px window) ─────────────────────────
medir <- function() ev(sprintf("(function(){ var s=document.querySelector('#%s .hdt-scroll'), sr=s.getBoundingClientRect();
  var rows=document.querySelectorAll('#%s tbody tr:not(.hdt-esp)'), vis=0;
  rows.forEach(function(tr){ var r=tr.getBoundingClientRect(); if (r.top >= sr.top - 0.5 && r.bottom <= sr.bottom + 0.5) vis++; });
  var h = rows.length > 1 ? (rows[rows.length-1].getBoundingClientRect().bottom - rows[0].getBoundingClientRect().top) / rows.length : 0;
  return { alto: sr.height, cabeza: document.querySelector('#%s thead').offsetHeight, fila: h, completas: vis, ventana: innerHeight }; })()", A, A, A))
for (n in c(10L, 40L, 150L)) {
  abrir(sprintf("?n=%d&seed=1", n)); Sys.sleep(0.5)
  m <- medir()
  cat(sprintf("  n=%3d: height %.0f px, row %.1f px, header %d px, %d complete rows in view\n",
              n, m$alto, m$fila, m$cabeza, m$completas))
  if (n == 10L) ok("10 rows: as tall as its rows (no scroll, at least 8 rows tall)",
                   abs(m$alto - (m$cabeza + 10 * m$fila)) <= 3 && m$completas == 10L)
  if (n == 40L) ok("40 rows (more than 30): grows to 70% of the window",
                   abs(m$alto - round(0.7 * m$ventana)) <= 2)
  if (n == 150L) {
    ok("150 rows: 70% of the window, and at least 30 compact rows in view (Bootstrap 5)",
       abs(m$alto - round(0.7 * m$ventana)) <= 2 && m$completas >= 30L)
    ev(sprintf("var s=document.querySelector('#%s .hdt-scroll'); s.scrollTop = s.scrollHeight;", A)); Sys.sleep(0.4)
    ok("150 rows, scrolled to the bottom: the last row ends above the footer", isTRUE(ev(sprintf(
      "(function(){ var r=document.querySelectorAll('#%s tbody tr:not(.hdt-esp)'); var l=r[r.length-1].getBoundingClientRect();
         var pie=document.querySelector('#%s .hdt-pie').getBoundingClientRect(); return l.bottom <= pie.top + 0.5; })()", A, A))))
    ok("... and the page itself doesn't scroll because of the table",
       isTRUE(ev(sprintf("document.querySelector('#%s').getBoundingClientRect().bottom <= innerHeight + 1 || document.querySelector('#%s .hdt-scroll').getBoundingClientRect().height <= 0.7 * innerHeight + 2", A, A))))
  }
}
abrir("?n=25&seed=1"); m <- medir()
cat(sprintf("  n= 25: height %.0f px (cap at 15 rows: %.0f px)\n", m$alto, m$cabeza + 15 * m$fila))
ok("25 rows (30 or fewer): capped at ~15 rows", abs(m$alto - (m$cabeza + 15 * m$fila)) <= 3 && m$completas == 15L)

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
