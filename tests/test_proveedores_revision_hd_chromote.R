# =============================================================================
# tests/test_proveedores_revision_hd_chromote.R
# The Excel review table of Proveedores (revision_excel_prov, class E) on the
# hd_table engine (T07), in a real headless Chrome, against
# tests/helpers/proveedores_fixture_app.R (no S3). A real .xlsx goes through
# the real file input. Blocks follow the T07 inventory, section 3 (R1-R9),
# and the whole import still works around the table: conflict card, final
# summary and Importar, which saves exactly the rows left.
#
# Run from project root:  Rscript tests/test_proveedores_revision_hd_chromote.R
# =============================================================================
cat("=== test_proveedores_revision_hd_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38982L
xl <- file.path(tempdir(), "proveedores_prueba.xlsx")
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app); unlink(xl) }, add = TRUE)
app <- tryCatch(start_bg_app(sprintf('setwd("%s"); source("tests/helpers/proveedores_fixture_app.R")$value', repo),
                             PORT, timeout_s = 90),
                error = function(e) { message(conditionMessage(e)); NULL })
ok("fixture app started", !is.null(app))
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

# Rows 1-2 ready, 3 without bank and 6 without account (incomplete), 4 the
# same account as row 1 (duplicate), 5 the account of ACEROS (conflict).
openxlsx::write.xlsx(data.frame(
  Nombre = c("NUEVO UNO SA", "NUEVO DOS SA", "SIN BANCO SA", "DUPLICADO SA", "ACEROS OTRA VEZ", "SIN CUENTA SA"),
  Alias  = c("UNO", "DOS", "SINBANCO", "DUP", "ACEROS2", "SINCTA"),
  Banco  = c("012", "072", "", "012", "002", "014"),
  `Tipo de cuenta` = "40",
  `No. Cuenta` = c("012180000000000071", "072180000000000072", "014180000000000073", "012180000000000071",
                   "012180001111111111", ""),
  RFC = "", Correo = "", Moneda = "Pesos", check.names = FALSE), xl)

b <- new_chromote_session()
invisible(b$Emulation$setDeviceMetricsOverride(width = 1400L, height = 1400L, deviceScaleFactor = 1, mobile = FALSE))
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
A <- "prov_mod-revision-hdt"
estado <- function() ev(sprintf("HDT.estado('%s')", A))
vis <- function() unlist(estado()$visibles)
centro <- function(js_el) ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', behavior:'instant'});
  var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
clic <- function(js_el) {
  p <- centro(js_el)
  b$Input$dispatchMouseEvent(type = "mouseMoved", x = p[[1]], y = p[[2]], button = "none")
  b$Input$dispatchMouseEvent(type = "mousePressed", x = p[[1]], y = p[[2]], button = "left", buttons = 1L, clickCount = 1L)
  b$Input$dispatchMouseEvent(type = "mouseReleased", x = p[[1]], y = p[[2]], button = "left", clickCount = 1L)
  Sys.sleep(0.6)
}
fila <- function(id, sel = "") sprintf("HDT.fila('%s', '%s')%s", A, id, sel)
registro <- function() strsplit(ev("document.getElementById('registro').textContent") %||% "", "\n")[[1]]
oculta <- function() isTRUE(ev("(function(){ var e = document.getElementById('prov_mod-revision_wrap'); return !e || e.offsetParent === null; })()"))
fichas <- function() unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s .hdt-ficha')).map(function(b){ return (b.dataset.valor || 'todas') + '=' + b.querySelector('.hdt-ficha-cifra').textContent; })", A)))
ficha <- function(valor) clic(sprintf("document.querySelector('#%s .hdt-ficha[data-valor=\"%s\"]')", A, valor))
ficha_todas <- function() clic(sprintf("Array.from(document.querySelectorAll('#%s .hdt-ficha')).filter(function(b){ return !b.dataset.valor; })[0]", A))
texto_pagina <- function() ev("document.getElementById('prov_mod-stage5_ui').innerText")

invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE))
poll_until(function() isTRUE(ev("!!document.querySelector(\"a[data-value='importar']\")")), timeout_s = 60)
Sys.sleep(1)
clic("document.querySelector(\"a[data-value='importar']\")")
ok("before a file, the review is hidden", oculta())
nodo <- b$DOM$querySelector(b$DOM$getDocument()$root$nodeId, "#prov_mod-excel_file")$nodeId
invisible(b$DOM$setFileInputFiles(files = list(normalizePath(xl, winslash = "\\")), nodeId = nodo))
ok("the file is read: the review shows its 6 rows, by Excel row number",
   poll_until(function() isTRUE(tryCatch(length(vis()) == 6L, error = function(e) FALSE)), timeout_s = 30) &&
   identical(vis(), as.character(1:6)) && !oculta())
Sys.sleep(0.5)

# ── R1/R2 · columns and badges ──────────────────────────────────────────────
titulos <- unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.dataset.col != null; }).map(function(th){ return th.firstChild.textContent; })", A)))
ok("R1: columns Estado / Fila / Nombre / Alias / RFC / No. Cuenta / Banco / Moneda",
   identical(titulos, c("Estado", "Fila", "Nombre", "Alias", "RFC", "No. Cuenta", "Banco", "Moneda")))
insignia <- function(id) ev(sprintf("(function(){ var s = %s; return s ? s.className.match(/hdt-tono-[a-z]+/)[0] + ' ' + s.textContent : ''; })()",
                                    fila(id, ".querySelector('.hdt-ins')")))
ok("R2: Listo green, Incompleto aviso, Duplicado red (row 4), Conflicto yellow (row 5)",
   identical(insignia("1"), "hdt-tono-exito Listo") && identical(insignia("3"), "hdt-tono-aviso Incompleto") &&
   identical(insignia("4"), "hdt-tono-peligro Duplicado") && identical(insignia("5"), "hdt-tono-amarillo Conflicto"))
# The text under a column title in one row.
celda <- function(id, titulo) ev(sprintf("(function(){
  var ths = Array.from(document.querySelectorAll('#%s thead th'));
  var j = ths.findIndex(function(th){ return th.firstChild && th.firstChild.textContent === '%s'; });
  return %s.children[j].textContent; })()", A, titulo, fila(id)))
ok("R2: the Fila column says which Excel row each one is (the old 'Dup. fila 4')",
   identical(celda("4", "Fila"), "4") && identical(celda("4", "Nombre"), "DUPLICADO SA"))

# ── R3 · chips = the old filter buttons ─────────────────────────────────────
ok("R3: chips Todas 6 / Listo 2 / Incompleto 2 / Duplicado 1 / Conflicto 1",
   identical(fichas(), c("todas=6", "listo=2", "incompleto=2", "duplicado=1", "conflicto=1")))
ficha("duplicado")
ok("R3: the Duplicado chip shows only row 4", identical(vis(), "4"))
ficha("incompleto")
ok("R3: the Incompleto chip shows rows 3 and 6", identical(vis(), c("3", "6")))
ficha_todas()

# ── R5/R6 · no selection, sorting ───────────────────────────────────────────
clic(fila("2", ".children[2]"))
ok("R5: a click selects nothing (class E has no selection)", length(unlist(estado()$seleccion)) == 0L)
clic(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.firstChild && th.firstChild.textContent === 'Nombre'; })[0]", A))
ok("R6: sorting by Nombre", identical(vis(), c("5", "4", "2", "1", "3", "6")))

# ── R4 · ✕ removes a row from the upload, after confirming ──────────────────
n0 <- length(registro())
clic(fila("3", ".querySelector('td.hdt-acc button[data-accion=eliminar]')"))
ok("R4: ✕ asks first in the table's bar (Luis's rule), and nothing is removed yet",
   identical(ev(sprintf("document.querySelector('#%s .hdt-confirmar-texto').textContent", A)), "¿Eliminar 1 fila?") &&
   "3" %in% vis())
clic(sprintf("document.querySelector('#%s .hdt-confirmar-si')", A))
Sys.sleep(0.5)
ok("R4: Confirmar takes row 3 out of the upload; the chip counts follow; nothing is saved",
   !"3" %in% vis() && length(vis()) == 5L && "incompleto=1" %in% fichas() && length(registro()) == n0)
ok("R4: the final summary counts 1 incomplete now", grepl("1 incompletos", texto_pagina()))

# ── R8/R9 · the rest of the import is untouched ─────────────────────────────
clic("document.getElementById('prov_mod-conf_omitir')")
ok("R8: the conflict card still resolves (Omitir) and Importar appears",
   isTRUE(ev("!!document.getElementById('prov_mod-confirmar_upload')")))
clic("document.getElementById('prov_mod-confirmar_upload')")
Sys.sleep(0.8)
r <- registro()
ok("R9: Importar saves the 4 providers plus the 2 ready rows, and nothing else",
   length(r) == n0 + 1L &&
   grepl("^activos 6: ", r[length(r)]) && grepl("NUEVO UNO SA@012180000000000071", r[length(r)]) &&
   grepl("NUEVO DOS SA@072180000000000072", r[length(r)]) && !grepl("SIN BANCO|DUPLICADO|OTRA VEZ|SIN CUENTA", r[length(r)]))
ok("after Importar the review hides again", oculta())

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
