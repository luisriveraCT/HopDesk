# =============================================================================
# tests/test_registros_hd_chromote.R
# Archivos de pago (and, below, Actividad / Bitácora Global) on the hd_table
# engine (T10), in a real headless Chrome, against
# tests/helpers/registros_fixture_app.R: the real modules over fixed data and
# a simulated `shared` (no S3). Every click is a real input event. Blocks
# follow the T10 inventory (hopdesk_coord/tablas/reportes/T10_inventario.md):
# P Archivos de pago (§2).
#
# Run from project root:  Rscript tests/test_registros_hd_chromote.R
# REG_RAIZ=<older checkout> runs the same test against that checkout's modules.
# =============================================================================
cat("=== test_registros_hd_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38996L
app <- NULL; b <- NULL
descargas <- file.path(tempdir(), "reg_descargas")
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app); unlink(descargas, recursive = TRUE) }, add = TRUE)
arrancar <- function(staff = FALSE, muchas = 0L) {
  if (!is.null(app)) stop_bg_app(app)
  app <<- tryCatch(start_bg_app(sprintf('Sys.setenv(REG_STAFF = "%s", REG_MUCHAS = "%d", REG_RAIZ = "%s"); setwd("%s"); source("tests/helpers/registros_fixture_app.R")$value',
                                        if (staff) "1" else "0", muchas, Sys.getenv("REG_RAIZ", repo), repo), PORT, timeout_s = 90),
                   error = function(e) { message(conditionMessage(e)); NULL })
  !is.null(app)
}
ok("fixture app started", arrancar())
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
invisible(b$Emulation$setDeviceMetricsOverride(width = 1772L, height = 866L, deviceScaleFactor = 1, mobile = FALSE))
base <- sprintf("http://127.0.0.1:%d", PORT)
unlink(descargas, recursive = TRUE); dir.create(descargas)
invisible(b$Browser$setDownloadBehavior(behavior = "allow", downloadPath = normalizePath(descargas, winslash = "\\")))
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
centro <- function(js_el) ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', behavior:'instant'});
  var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
raton <- function(tipo, p, botones = 0L, n = 1L)
  b$Input$dispatchMouseEvent(type = tipo, x = p[[1]], y = p[[2]], button = if (tipo == "mouseMoved") "none" else "left",
                             buttons = botones, clickCount = n)
clic <- function(js_el) {
  p <- centro(js_el)
  raton("mouseMoved", p); raton("mousePressed", p, 1L); raton("mouseReleased", p)
  Sys.sleep(0.5)
}
doble <- function(js_el) {
  p <- centro(js_el)
  raton("mouseMoved", p)
  raton("mousePressed", p, 1L, 1L); raton("mouseReleased", p, 0L, 1L)
  raton("mousePressed", p, 1L, 2L); raton("mouseReleased", p, 0L, 2L)
  Sys.sleep(0.8)
}
pestana <- function(v) { clic(sprintf("document.querySelector(\".navbar .nav-link[data-value='%s']\")", v)); Sys.sleep(0.8) }
listo <- function(t) poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s') && !!document.getElementById('%s').offsetParent", t, t))), timeout_s = 30)
estado <- function(t) ev(sprintf("HDT.estado('%s')", t))
vis <- function(t) unlist(estado(t)$visibles)
fila <- function(t, id, sel = "") sprintf("HDT.fila('%s', '%s')%s", t, id, sel)
boton <- function(t, id, accion) fila(t, id, sprintf(".querySelector('td.hdt-acc button[data-accion=%s]')", accion))
titulos <- function(t) unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.dataset.col != null; }).map(function(th){ return th.firstChild.textContent; })", t)))
encabezado <- function(t, texto) sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.firstChild && th.firstChild.textContent === '%s'; })[0]", t, texto)
texto_fila <- function(t, id) ev(sprintf("%s.textContent", fila(t, id)))
modal_titulo <- function() ev("(document.querySelector('.modal.show .modal-title')||{}).textContent || ''")
modal_texto <- function() ev("(document.querySelector('.modal.show .modal-body')||{}).textContent || ''")
cerrar_modal <- function() { ev("$('.modal').modal('hide'); 0"); Sys.sleep(0.8) }
elegir <- function(input_id, valor) {
  ev(sprintf("var z = $('#%s')[0].selectize; z.setValue('%s'); z.close(); z.blur(); 0", input_id, valor)); Sys.sleep(1)
}
opciones <- function(input_id) unlist(ev(sprintf("Object.keys($('#%s')[0].selectize.options)", input_id)))
registro <- function() strsplit(ev("document.getElementById('registro').textContent") %||% "", "\n")[[1]]
abrir <- function() {
  invisible(b$Page$navigate(base, wait_ = TRUE))
  poll_until(function() isTRUE(ev("!!window.Shiny && !!document.querySelector('.navbar')")), timeout_s = 60)
  Sys.sleep(1.5)
  TRUE
}
ok("the app loads", abrir())

# ── P · Archivos de pago (register C) ───────────────────────────────────────
P <- "pag-pagos_t-hdt"
pestana("PAG")
ok("P: Archivos de pago is on the engine", listo(P))
ok("AP5: the last 30 days by default, newest first; the 40-day-old file is out; a row without id still shows",
   identical(vis(P), c("p-01", "p-02", "p-03", "p-04", "sin-id-5", "p-06")))
ok("P: columns Hora / Usuario / Tipo / Empresa / Moneda / Cuenta origen / Cant. / Total / Origen",
   identical(titulos(P), c("Hora", "Usuario", "Tipo", "Empresa", "Moneda", "Cuenta origen", "Cant.", "Total", "Origen")))
ok("AP1: Hora shows date and time with seconds",
   grepl("[0-9]{2}:[0-9]{2}:[0-9]{2}", texto_fila(P, "p-01")))
ok("AP10: the account is masked in the table; Origen says ZIP (todos)",
   grepl("••••5678", texto_fila(P, "p-01")) && !grepl("030180900012345678", texto_fila(P, "p-01")) &&
   grepl("ZIP (todos)", texto_fila(P, "p-01"), fixed = TRUE))
ok("AP11: Tipo is a badge (Pago / Reporte Excel / Libro de Bancos (Excel))",
   grepl("Pago", texto_fila(P, "p-02")) && grepl("Reporte Excel", texto_fila(P, "sin-id-5")) &&
   grepl("Libro de Bancos (Excel)", texto_fila(P, "p-06"), fixed = TRUE))
clic(encabezado(P, "Total"))
ok("AP1: Total sorts by amount (9 < 10 < 2,324.49 < 189,955.59), not as text",
   { v <- vis(P); identical(v[v %in% c("p-01", "p-02", "p-03", "p-04")], c("p-03", "p-04", "p-02", "p-01")) })
clic(encabezado(P, "Hora")); clic(encabezado(P, "Hora"))
ok("P: no 'Ver detalle' button above the table any more; each row has ℹ",
   isTRUE(ev("!document.getElementById('pag-btn_ver_detalle')")) &&
   isTRUE(ev(sprintf("!!%s", boton(P, "p-02", "detalle")))))
clic(boton(P, "p-02", "detalle")); Sys.sleep(0.8)
ok("AP4: ℹ on p-02 opens its detail: beneficiary, full CLABE, receipt download",
   grepl("Detalle del archivo de pagos", modal_titulo()) && grepl("CARGAS BETA", modal_texto()) &&
   grepl("012180001234567891", modal_texto()) && isTRUE(ev("!!document.getElementById('pag-dl_pago_pdf')")))
cerrar_modal()
doble(fila(P, "p-06", ".children[3]"))
ok("AP4: a double click opens the detail too (a non-payment file)",
   grepl("Detalle del archivo generado", modal_titulo()) && grepl("archivo_p-06.xlsx", modal_texto()))
cerrar_modal()

# AP3: the old table kept its selection by position, so after a filter
# "Ver detalle" opened another record. Now every detail goes by id.
elegir("pag-pagos_emp_sel", "NETWORKS CARGO")
ok("AP2: Empresa (server filter) leaves NETWORKS CARGO's files", identical(vis(P), c("p-02", "p-04")))
clic(boton(P, "p-04", "detalle")); Sys.sleep(0.8)
ok("AP3: after filtering, ℹ on p-04 opens p-04 (DELTA DIEZ), not whatever sat in its old position",
   grepl("DELTA DIEZ", modal_texto()) && !grepl("GAMMA NUEVE", modal_texto()))
cerrar_modal()
elegir("pag-pagos_emp_sel", "Todas")
ok("P5: Usuario is a list of the range's users (it was free text)",
   identical(sort(opciones("pag-pagos_user_sel")), c("ana", "luis", "mouse", "Todos")))
elegir("pag-pagos_user_sel", "ana")
ok("P5: choosing ana leaves her files", identical(vis(P), c("p-02", "p-03")))

# AP10: the Excel never carries the full account either.
clic(sprintf("document.querySelector('#%s .hdt-excel')", P))
ok("P: Exportar a Excel downloads archivos_pago_<date>.xlsx",
   poll_until(function() length(list.files(descargas, pattern = "^archivos_pago_.*\\.xlsx$")) == 1L, timeout_s = 20))
xl <- tryCatch(as.data.frame(readxl::read_excel(list.files(descargas, pattern = "^archivos_pago_.*\\.xlsx$", full.names = TRUE)[1],
                                                col_types = "text")), error = function(e) NULL)
ok("AP10: the Excel has the 2 rows shown, the account masked, never the full CLABE",
   !is.null(xl) && nrow(xl) == 2L && any(grepl("5678", unlist(xl))) && !any(grepl("030180900012345678", unlist(xl))))

# P4: the PDF still takes the server's filters, now including the user list.
clic("document.getElementById('pag-pagos_export_pdf')")
ok("P4: Exportar PDF exports the filtered rows and records the user filter (ana)",
   poll_until(function() any(grepl("^log exportar_pagos_pdf fixture n_entradas=2;empresa_filter=Todas;usuario_filter=ana$", registro())),
              timeout_s = 90))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
