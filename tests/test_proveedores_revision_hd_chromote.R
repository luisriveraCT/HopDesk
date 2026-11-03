# =============================================================================
# tests/test_proveedores_revision_hd_chromote.R
# The Excel review of Proveedores (revision_excel_prov, class E) on the
# hd_table engine (T07), with the T08 import protocol, in a real headless
# Chrome against tests/helpers/proveedores_fixture_app.R (no S3). A real
# .xlsx goes through the real file input: one row in each state (listo,
# actualiza, reactiva, sin cambios, incompleto, revisar, duplicado). Blocks
# follow the T07 inventory (section 3, R1-R9) and T08: the reason a row
# waits for review, the changes (antes -> después), the columns taken from
# the file, the existing-provider cards, Importar, and the template.
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
descargas <- file.path(tempdir(), "prov_descargas")
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app); unlink(xl); unlink(descargas, recursive = TRUE) },
        add = TRUE)
app <- tryCatch(start_bg_app(sprintf('setwd("%s"); source("tests/helpers/proveedores_fixture_app.R")$value', repo),
                             PORT, timeout_s = 90),
                error = function(e) { message(conditionMessage(e)); NULL })
ok("fixture app started", !is.null(app))
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

# (T08b: SIN BANCO SA has a valid CLABE, so it is Listo with the bank taken from it.)
# One row per state against the fixture's catalog (ACEROS, FLETES active;
# PROVEEDOR VIEJO inactive). Row 5 is a NUMBER cell, as Excel keeps it.
local({
  d <- data.frame(
    Nombre = c("NUEVO UNO SA", "SIN BANCO SA", "NUEVO UNO OTRA VEZ", "STP NUMERO SA", "ACEROS MONTERREY SA DE CV",
               "FLETES EL GÜERO", "PROVEEDOR VIEJO SA"),
    Alias  = c("UNO", "SINBANCO", "UNO2", "STP", "ACEROS", "FLETES", "VIEJO"),
    Banco  = c("072", "", "072", "646", "012", "072", "014"),
    `No. Cuenta` = c("072180005555555555", "012180006666666664", "072180005555555555", "", "012180001111111119",
                     "072180002222222224", "014180004444444448"),
    Moneda = "MXN", check.names = FALSE)
  wb <- openxlsx::createWorkbook(); openxlsx::addWorksheet(wb, "P")
  openxlsx::writeData(wb, "P", d)
  openxlsx::writeData(wb, "P", 646180110400000007, startCol = 4, startRow = 5)
  openxlsx::saveWorkbook(wb, xl, overwrite = TRUE)
})

b <- new_chromote_session()
invisible(b$Emulation$setDeviceMetricsOverride(width = 1400L, height = 1600L, deviceScaleFactor = 1, mobile = FALSE))
dir.create(descargas, showWarnings = FALSE)
invisible(b$Browser$setDownloadBehavior(behavior = "allow", downloadPath = normalizePath(descargas, winslash = "\\")))
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
porId <- function(id) sprintf("document.getElementById('%s')", id)
fila <- function(id, sel = "") sprintf("HDT.fila('%s', '%s')%s", A, id, sel)
registro <- function() strsplit(ev("document.getElementById('registro').textContent") %||% "", "\n")[[1]]
oculta <- function(id) isTRUE(ev(sprintf("(function(){ var e = document.getElementById('%s'); return !e || e.offsetParent === null; })()", id)))
fichas <- function() unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s .hdt-ficha')).map(function(b){ return (b.dataset.valor || 'todas') + '=' + b.querySelector('.hdt-ficha-cifra').textContent; })", A)))
ficha <- function(valor) clic(sprintf("document.querySelector('#%s .hdt-ficha[data-valor=\"%s\"]')", A, valor))
ficha_todas <- function() clic(sprintf("Array.from(document.querySelectorAll('#%s .hdt-ficha')).filter(function(b){ return !b.dataset.valor; })[0]", A))
# The text under a column title in one row.
celda <- function(id, titulo) ev(sprintf("(function(){
  var ths = Array.from(document.querySelectorAll('#%s thead th'));
  var j = ths.findIndex(function(th){ return th.firstChild && th.firstChild.textContent === '%s'; });
  return %s.children[j].textContent; })()", A, titulo, fila(id)))
insignia <- function(id) ev(sprintf("(function(){ var s = %s; return s ? s.className.match(/hdt-tono-[a-z]+/)[0] + ' ' + s.textContent : ''; })()",
                                    fila(id, ".querySelector('.hdt-ins')")))
texto <- function(id) ev(sprintf("(document.getElementById('%s')||{}).innerText || ''", id))
# The columns read from the file fold under "Ver las columnas leídas" (T09):
# open it, as Luis would, before reading them.
leidas <- function() {
  if (!isTRUE(ev("!!(document.querySelector('#prov_mod-revision_mapeo details')||{}).open")))
    clic("document.querySelector('#prov_mod-revision_mapeo details summary')")
  texto("prov_mod-revision_mapeo")
}

invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE))
poll_until(function() isTRUE(ev("!!document.querySelector(\"a[data-value='importar']\")")), timeout_s = 60)
Sys.sleep(1)
clic("document.querySelector(\"a[data-value='importar']\")")
ok("before a file: the review and the currency selector are hidden",
   oculta("prov_mod-revision_wrap") && oculta("prov_mod-excel_moneda_wrap"))

# ── T08 · template ──────────────────────────────────────────────────────────
# Shiny enables a download link once its tab shows: wait for that, as a person would see it.
poll_until(function() isTRUE(ev("!document.getElementById('prov_mod-descargar_plantilla').classList.contains('disabled')")), timeout_s = 15)
clic(porId("prov_mod-descargar_plantilla"))
ok("T08-6: Descargar plantilla downloads the catalog with every column, accounts as text",
   poll_until(function() length(list.files(descargas, "\\.xlsx$")) == 1L, timeout_s = 20) && {
     f <- list.files(descargas, "\\.xlsx$", full.names = TRUE)[1]
     d <- readxl::read_excel(f, col_types = "list")
     identical(names(d), c("Nombre", "Alias", "Banco", "Tipo de cuenta", "No. Cuenta", "RFC", "Correo", "Moneda",
                           "Tipo de relación")) && nrow(d) == 4L &&
       all(vapply(d[["No. Cuenta"]], function(v) is.character(v) || is.na(v), logical(1))) &&
       "012180001111111119" %in% unlist(d[["No. Cuenta"]])
   })

nodo <- b$DOM$querySelector(b$DOM$getDocument()$root$nodeId, "#prov_mod-excel_file")$nodeId
invisible(b$DOM$setFileInputFiles(files = list(normalizePath(xl, winslash = "\\")), nodeId = nodo))
# The summary and the link are painted a moment after the table: wait for them.
poll_until(function() grepl("cuentas", texto("prov_mod-revision_mapeo")) && grepl("Ver", texto("prov_mod-revision_ver_todo")),
           timeout_s = 15)
ok("T08b: the review opens on what changes -- 6 of the 7 rows; FLETES (sin cambios) is one click away",
   poll_until(function() isTRUE(tryCatch(length(vis()) == 6L, error = function(e) FALSE)), timeout_s = 30) &&
   identical(vis(), c("2", "3", "4", "5", "6", "8")) && !oculta("prov_mod-revision_wrap") &&
   grepl("Ver también 1 sin cambios", texto("prov_mod-revision_ver_todo")))
ok("T08b: one sentence on top: header row, accounts, with changes, to review, unchanged",
   grepl("Encabezado en la fila 1 · 7 cuentas · 4 con cambios · 2 por revisar · 1 sin cambios", texto("prov_mod-revision_mapeo")))
Sys.sleep(0.5)

# ── R1/R2 · columns and badges, one per state ───────────────────────────────
titulos <- unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.dataset.col != null; }).map(function(th){ return th.firstChild.textContent; })", A)))
ok("R1/T08b/T09: the columns that matter, in order: Estado / Nombre / Banco / No. Cuenta / Moneda / Detalle",
   identical(titulos, c("Estado", "Nombre", "Banco", "No. Cuenta", "Moneda", "Detalle")))
# T09: the rest are optional columns, hidden until asked for (no horizontal
# scroll); Fila is turned on here for R2 below.
clic(sprintf("document.querySelector('#%s .hdt-columnas')", A))
ok("T09: Columnas offers Alias / RFC / Correo / Tipo de cuenta / Fila, all off",
   identical(ev("[].map.call(document.querySelectorAll('.hdt-menu .hdt-menu-op input[data-campo]'), function (c) { return c.dataset.campo + ':' + (c.checked ? 1 : 0); }).join(' ')"),
             "alias:0 rfc:0 correo:0 tipo:0 fila:0"))
clic("document.querySelector('.hdt-menu input[data-campo=fila]')")
invisible(ev("document.body.click(); 0"))
ok("T09: and turning one on shows it",
   poll_until(function() identical(tail(unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.dataset.col != null; }).map(function(th){ return th.firstChild.textContent; })", A))), 1), "Fila"),
              timeout_s = 5))
ok("T08b: the bank says its key and who it is", identical(celda("6", "Banco"), "012 · BBVA"))
ok("R2/T08: one badge per state, in its tone",
   identical(c(insignia("2"), insignia("3"), insignia("4"), insignia("5"), insignia("6"), insignia("8")),
             c("hdt-tono-exito Listo", "hdt-tono-exito Listo", "hdt-tono-peligro Duplicado", "hdt-tono-rojo Revisar",
               "hdt-tono-info Actualiza", "hdt-tono-morado Se reactiva")))
ok("T08b: SIN BANCO has a valid CLABE, so its bank comes from it (with the note) instead of 'Incompleto'",
   identical(celda("3", "Banco"), "012 · BBVA") && grepl("Banco tomado de la CLABE: BBVA (012)", celda("3", "Detalle"), fixed = TRUE))
clic(porId("prov_mod-revision_ver_todo"))
ok("T08b: 'Ver también' shows the unchanged one, with its badge; again hides it",
   "7" %in% vis() && identical(insignia("7"), "hdt-tono-neutro Sin cambios") &&
   { clic(porId("prov_mod-revision_ver_todo")); !"7" %in% vis() })
ok("R2: the Fila column says which Excel row each one is (the old 'Dup. fila N')",
   identical(celda("4", "Fila"), "4") && identical(celda("4", "Nombre"), "NUEVO UNO OTRA VEZ"))
ok("T08-4/T08b: the number cell waits for review; Detalle starts with the reason",
   startsWith(celda("5", "Detalle"), "Excel guardó la cuenta como número"))
# The engine's bubble (after 350 ms on the cell) shows Detalle whole.
local({
  p <- centro(sprintf("(function(){ var ths = Array.from(document.querySelectorAll('#%s thead th'));
    var j = ths.findIndex(function(th){ return th.firstChild && th.firstChild.textContent === 'Detalle'; });
    return %s.children[j]; })()", A, fila("5")))
  b$Input$dispatchMouseEvent(type = "mouseMoved", x = p[[1]] - 5, y = p[[2]], button = "none")
  b$Input$dispatchMouseEvent(type = "mouseMoved", x = p[[1]], y = p[[2]], button = "none")
  Sys.sleep(0.9)
})
ok("T08b: Detalle is never cut -- the bubble shows the whole reason, with the value quoted",
   identical(ev("(document.querySelector('.hdt-tip:not([hidden]) .hdt-tip-texto')||{}).textContent || ''"),
             "Excel guardó la cuenta como número y pudo perder dígitos: escríbela como texto («646180110400000000»)."))
invisible(ev("document.body.dispatchEvent(new MouseEvent('mousemove')); 0"))
ok("T08-5: Actualiza says what changes, antes → después",
   identical(celda("6", "Detalle"), "Nombre: ACEROS MONTERREY SA → ACEROS MONTERREY SA DE CV"))
ok("T08-2: the review says which column of the file each field came from, and what's missing",
   grepl("No. Cuenta ← «No. Cuenta»", leidas()) &&
   grepl("No vienen en el archivo: Tipo de cuenta, RFC, Correo, Tipo de relación", texto("prov_mod-revision_mapeo")))
ok("T08-1: the file has Moneda, so no currency selector", oculta("prov_mod-excel_moneda_wrap"))

# ── R3 · chips ──────────────────────────────────────────────────────────────
ok("R3: one chip per state with its count (of what is shown)",
   identical(fichas(), c("todas=6", "listo=2", "actualiza=1", "reactiva=1", "inactiva=0", "revisar=1",
                         "incompleto=0", "duplicado=1", "sin_cambios=0", "fuera_rango=0")))
ficha("revisar")
ok("R3: the Revisar chip shows only row 5", identical(vis(), "5"))
ficha_todas()

# ── R5/R6 · no selection, sorting ───────────────────────────────────────────
clic(fila("2", ".children[2]"))
ok("R5: a click selects nothing (class E has no selection)", length(unlist(estado()$seleccion)) == 0L)
clic(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.firstChild && th.firstChild.textContent === 'Nombre'; })[0]", A))
ok("R6: sorting by Nombre", identical(vis(), c("6", "4", "2", "8", "3", "5")))

# ── R4 · ✕ removes a row from the upload, after confirming ──────────────────
n0 <- length(registro())
clic(fila("3", ".querySelector('td.hdt-acc button[data-accion=eliminar]')"))
ok("R4: ✕ asks first in the table's bar, and nothing is removed yet",
   identical(ev(sprintf("document.querySelector('#%s .hdt-confirmar-texto').textContent", A)), "¿Eliminar 1 fila?") &&
   "3" %in% vis())
clic(sprintf("document.querySelector('#%s .hdt-confirmar-si')", A))
Sys.sleep(0.5)
ok("R4: Confirmar takes row 3 out of the upload; the chips follow; nothing is saved",
   !"3" %in% vis() && length(vis()) == 5L && "listo=1" %in% fichas() && length(registro()) == n0)

# ── Existing providers: one card at a time, with the changes ────────────────
ok("T08-5: the first card is ACEROS, with the change listed",
   grepl("Actualiza a ACEROS MONTERREY SA DE CV", texto("prov_mod-stage4_ui")) &&
   grepl("Nombre: ACEROS MONTERREY SA → ACEROS MONTERREY SA DE CV", texto("prov_mod-stage4_ui")) &&
   !grepl("S3", texto("prov_mod-stage4_ui")))
clic(porId("prov_mod-conf_reemplazar"))
ok("T08 caso 09: the second card reactivates PROVEEDOR VIEJO, which only moves list",
   grepl("Reactiva a PROVEEDOR VIEJO SA", texto("prov_mod-stage4_ui")) &&
   grepl("solo cambia de lista", texto("prov_mod-stage4_ui")))
clic(porId("prov_mod-conf_reemplazar"))
ok("the summary counts what goes in: 1 new + 2 existing = Importar 3",
   grepl("Importar 3 proveedores", texto("prov_mod-stage5_ui")) &&
   grepl("1 para revisar", texto("prov_mod-stage5_ui")) && grepl("1 sin cambios", texto("prov_mod-stage5_ui")))

# ── R9 · Importar ───────────────────────────────────────────────────────────
clic(porId("prov_mod-confirmar_upload"))
Sys.sleep(0.8)
r <- registro()
nuevos <- r[(n0 + 1):length(r)]
activos <- grep("^activos ", nuevos, value = TRUE)
inactivos <- grep("^inactivos ", nuevos, value = TRUE)
ok("R9: Activos is written first (it receives the reactivated one), then Inactivos",
   length(nuevos) >= 2L && startsWith(nuevos[1], "activos ") && startsWith(nuevos[2], "inactivos "))
ok("R9: Activos = the 4 there were (ACEROS renamed) + NUEVO UNO + PROVEEDOR VIEJO; nothing held back goes in",
   grepl("^activos 6: ", activos[1]) && grepl("ACEROS MONTERREY SA DE CV@012180001111111119", activos[1]) &&
   grepl("NUEVO UNO SA@072180005555555555", activos[1]) && grepl("PROVEEDOR VIEJO SA@014180004444444448", activos[1]) &&
   !grepl("STP|SIN BANCO|OTRA VEZ|646180110400000000", activos[1]))
ok("caso 09: Inactivos no longer has it (never in both lists)", grepl("^inactivos 0: ", inactivos[1]))
ok("the import is logged, and the review hides again",
   any(grepl("^log importar_proveedores ", nuevos)) && oculta("prov_mod-revision_wrap"))

# ── T08b · BanBajío's real export (synthetic: same structure, invented data) ─
bajio <- normalizePath("tests/fixtures/proveedores_excel/10_bajio_export.xlsx", winslash = "\\")
nodo <- b$DOM$querySelector(b$DOM$getDocument()$root$nodeId, "#prov_mod-excel_file")$nodeId
t0 <- Sys.time()
invisible(b$DOM$setFileInputFiles(files = list(bajio), nodeId = nodo))
ok("T08b: BanBajío's export (header on row 6) is read",
   poll_until(function() grepl("451", texto("prov_mod-revision_mapeo")), timeout_s = 30))
t_leer <- as.numeric(Sys.time() - t0, units = "secs")
cat(sprintf("  subir + leer + clasificar 451 filas: %.2f s\n", t_leer))
# This session imported a moment ago, so "last import" is today: by default
# only accounts registered since today are reviewed -- none of this file's.
ok("T08b: the date filter starts on the last import (today here): every account is out of range, nothing to wade through",
   !oculta("prov_mod-excel_desde_wrap") && isTRUE(ev("document.getElementById('prov_mod-excel_desde_on').checked")) &&
   length(vis()) == 0L && grepl("451 registradas antes del", texto("prov_mod-revision_mapeo"), fixed = TRUE))
t0 <- Sys.time()
clic(porId("prov_mod-excel_desde_on"))
ok("T08b: without the filter the whole file is reviewed",
   poll_until(function() isTRUE(tryCatch(length(vis()) == 451L, error = function(e) FALSE)), timeout_s = 20))
cat(sprintf("  reclasificar + pintar 451 filas: %.2f s\n", as.numeric(Sys.time() - t0, units = "secs")))
poll_until(function() grepl("450 con cambios", texto("prov_mod-revision_mapeo")), timeout_s = 15)
ok("T08b: the sentence says where the header was and what is in the file",
   grepl("Encabezado en la fila 6 · se ignoraron 5 filas de datos generales · 451 cuentas · 450 con cambios · 1 por revisar",
         texto("prov_mod-revision_mapeo"), fixed = TRUE))
ok("T08b: the columns it took and the ones it didn't use are listed",
   grepl("Nombre ← «Nombre Beneficiario»", leidas(), fixed = TRUE) &&
   grepl("Moneda ← «Divisa»", texto("prov_mod-revision_mapeo"), fixed = TRUE) &&
   grepl("Columnas no usadas:", texto("prov_mod-revision_mapeo"), fixed = TRUE) &&
   grepl("Otro Tipo de Relación", texto("prov_mod-revision_mapeo"), fixed = TRUE))
ok("T08b: no bank goes unrecognized; the one with a BBVA CLABE that says SANTANDER waits, saying both",
   "revisar=1" %in% fichas() &&
   { ficha("revisar"); r <- vis(); length(r) == 1L &&
       startsWith(celda(r, "Detalle"), "La CLABE es de BBVA (012) y el archivo dice «SANTANDER».") })
ficha_todas()
# Only accounts registered since the 300th (the fixture registers one every 3 days).
desde <- format(as.Date(44924 + 300 * 3, origin = "1899-12-30"), "%Y-%m-%d")
invisible(ev(sprintf("$('#prov_mod-excel_desde input').bsDatepicker('setDate', new Date('%sT12:00:00')); $('#prov_mod-excel_desde input').trigger('change'); 0", desde)))
clic(porId("prov_mod-excel_desde_on"))
ok("T08b: 'Solo cuentas registradas desde' leaves the 152 newer ones; the 299 older are out of range, one click away",
   poll_until(function() isTRUE(tryCatch(length(vis()) == 152L, error = function(e) FALSE)), timeout_s = 20) &&
   grepl("299 registradas antes del", texto("prov_mod-revision_mapeo"), fixed = TRUE) &&
   grepl("Ver también las 299", texto("prov_mod-revision_ver_todo"), fixed = TRUE))
clic(porId("prov_mod-cancelar_upload"))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
