# =============================================================================
# tests/test_empresas_hd_chromote.R
# Empresas on the hd_table engine (T07), in a real headless Chrome, against
# tests/helpers/empresas_fixture_app.R: the real module over fixed companies
# and a simulated `shared` (no S3). Every click is a real input event. Blocks
# follow the T07 inventory (hopdesk_coord/tablas/reportes/T07_inventario.md,
# section 4): E1-E13, and above all the ids -- the edit panel and Desactivar
# act on the company that was clicked even after another session reorders
# the list underneath (before, they went by position and hit a neighbour).
#
# Run from project root:  Rscript tests/test_empresas_hd_chromote.R
# =============================================================================
cat("=== test_empresas_hd_chromote ===\n\n")

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
arrancar <- function(falla = FALSE) {
  if (!is.null(app)) stop_bg_app(app)
  app <<- tryCatch(start_bg_app(sprintf('Sys.setenv(EMP_FALLA = "%s"); setwd("%s"); source("tests/helpers/empresas_fixture_app.R")$value',
                                        if (falla) "1" else "0", repo), PORT, timeout_s = 90),
                   error = function(e) { message(conditionMessage(e)); NULL })
  !is.null(app)
}
ok("fixture app started", arrancar())
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
invisible(b$Emulation$setDeviceMetricsOverride(width = 1400L, height = 900L, deviceScaleFactor = 1, mobile = FALSE))
base <- sprintf("http://127.0.0.1:%d", PORT)
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
A <- "emp-tabla-hdt"
abrir <- function() {
  invisible(b$Page$navigate(base, wait_ = TRUE))
  ok <- poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s') && HDT.estado('%s').visibles.length > 0", A, A))),
                   timeout_s = 60)
  Sys.sleep(0.8)
  ok
}
estado <- function() ev(sprintf("HDT.estado('%s')", A))
vis <- function() unlist(estado()$visibles)
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
  Sys.sleep(0.6)
}
escribir <- function(js_input, texto) {
  p <- centro(js_input); raton("mousePressed", p, 1L); raton("mouseReleased", p)
  ev(sprintf("var i=%s; i.value=''; i.dispatchEvent(new Event('input'))", js_input))
  if (nzchar(texto)) b$Input$insertText(text = texto)
  Sys.sleep(0.5)
}
fila <- function(id, sel = "") sprintf("HDT.fila('%s', '%s')%s", A, id, sel)
boton <- function(id, accion) fila(id, sprintf(".querySelector('td.hdt-acc button[data-accion=%s]')", accion))
botones <- function(id) unlist(ev(sprintf("Array.from(%s).map(function(b){ return b.dataset.accion; })",
                                          fila(id, ".querySelectorAll('td.hdt-acc button')"))))
registro <- function() strsplit(ev("document.getElementById('registro').textContent") %||% "", "\n")[[1]]
n_reg <- function() length(registro())
guardados <- function() grep("^guardar ", registro(), value = TRUE)
# The saved state of one company in the last save: "<initials>:<razon>:<activa>:<deleted>".
guardado_de <- function(id) {
  g <- guardados(); if (!length(g)) return(NA_character_)
  partes <- strsplit(sub("^guardar ", "", g[length(g)]), " \\| ")[[1]]
  x <- partes[startsWith(partes, paste0(substr(id, 1, 4), ":"))]
  if (length(x)) sub("^[^:]*:", "", x[1]) else NA_character_
}
panel_titulo <- function() ev("(document.querySelector('#emp-edit_panel_ui h5')||{}).textContent || ''")
panel_iniciales <- function() ev("(document.getElementById('emp-edit_initials')||{}).value || ''")
conf_texto <- function() ev(sprintf("document.querySelector('#%s .hdt-confirmar-texto').textContent", A))
si <- function() clic(sprintf("document.querySelector('#%s .hdt-confirmar-si')", A))
aviso <- function() ev("Array.from(document.querySelectorAll('.shiny-notification')).map(function(n){ return n.textContent; }).join(' / ')")
otra_sesion <- function(btn = "otra_sesion") { clic(sprintf("document.getElementById('%s')", btn)); Sys.sleep(0.6) }

ok("Empresas loads on the engine", abrir())

# ── E1-E5 · data, columns, search, filter ───────────────────────────────────
ok("E2: the deleted company is out; the 4 others in stored order",
   identical(vis(), c("e-ng", "e-nts", "e-ncs", "e-tst")))
titulos <- unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.dataset.col != null; }).map(function(th){ return th.firstChild.textContent; })", A)))
ok("E1: columns # / Iniciales / Razón Social / RFC / Estado (from the dictionary)",
   identical(titulos, c("#", "Iniciales", "Razón Social", "RFC", "Estado")))
ok("E1: Estado is a badge -- Activa green (exito), Inactiva neutral",
   identical(ev(sprintf("%s.textContent", fila("e-ng", ".querySelector('.hdt-tono-exito')"))), "Activa") &&
   isTRUE(ev(sprintf("!%s", fila("e-tst", ".querySelector('.hdt-tono-exito')")))) &&
   grepl("Inactiva", ev(sprintf("%s.textContent", fila("e-tst")))))
ok("E6/E8/E9: each row has Editar, ERP and Desactivar, in the catalog's order",
   identical(botones("e-nts"), c("editar", "erp", "desactivar")))
ok("E4: no row selection (Empresas never had one)",
   { clic(fila("e-nts", ".children[2]")); length(unlist(estado()$seleccion)) == 0L })
ok("E3: the engine's search is there (new) and finds by razón social",
   { escribir(sprintf("document.querySelector('#%s .hdt-buscar')", A), "crossdocking"); identical(vis(), "e-ncs") })
escribir(sprintf("document.querySelector('#%s .hdt-buscar')", A), "")
ok("E3: the filter on Estado offers Activa and Inactiva",
   { clic(sprintf("document.querySelector('#%s button.hdt-filtro')", A))
     txt <- ev(sprintf("document.querySelector('#%s').innerText", A))
     grepl("Activa", txt) && grepl("Inactiva", txt) })
invisible(ev("document.body.click()")); Sys.sleep(0.3)
ok("E5: sorting by Iniciales works and leaves the action column alone",
   { clic(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.firstChild && th.firstChild.textContent === 'Iniciales'; })[0]", A))
     identical(vis(), c("e-ncs", "e-ng", "e-nts", "e-tst")) })
invisible(abrir())

# ── E6/E7 · edit by id ──────────────────────────────────────────────────────
clic(boton("e-ncs", "editar"))
ok("E6: ✎ on NCS opens the side panel for NCS", identical(panel_iniciales(), "NCS"))
otra_sesion()
ok("ids: another session puts a company first -- the table shows it, and the panel STILL edits NCS",
   identical(vis()[1], "e-new") && identical(panel_iniciales(), "NCS"))
invisible(ev("var i=document.getElementById('emp-edit_razon_social'); i.value='NCS Editada SA'; $(i).trigger('change'); 0"))
Sys.sleep(0.5)
k <- n_reg()
clic("document.getElementById('emp-btn_save_edit')")
Sys.sleep(0.6)
ok("E7: Guardar saves NCS's new razón social, and nothing else changes",
   identical(guardado_de("e-ncs"), "NCS:NCS Editada SA:TRUE:FALSE") &&
   identical(guardado_de("e-nts"), "NTS:Networks Trucking Services S.A. de C.V.:TRUE:FALSE") &&
   identical(guardado_de("e-new"), "AAA:Alfa Nueva SA:TRUE:FALSE"))
ok("E7: log_action editar_empresa with NCS's id; the panel closes",
   any(grepl("^log editar_empresa e-ncs$", registro())) && identical(panel_iniciales(), ""))
ok("E11: the table shows the saved value", grepl("NCS Editada SA", ev(sprintf("%s.textContent", fila("e-ncs")))))

# ── detalle · double click = Editar ─────────────────────────────────────────
doble(fila("e-nts", ".children[2]"))
ok("detalle: a double click on NTS opens its edit panel", identical(panel_iniciales(), "NTS"))
otra_sesion("otra_sesion_borra")   # the first one (AAA) goes: every position shifts back
ok("ids: another session drops the first company -- the panel stays on NTS", identical(panel_iniciales(), "NTS"))
clic("document.getElementById('emp-btn_cancel_edit')")
ok("Cancelar closes the panel", identical(panel_iniciales(), ""))

# ── E8 · Desactivar by id, confirmed in the table's bar ─────────────────────
k <- length(guardados())
clic(boton("e-tst", "desactivar"))
ok("E8: ⊘ asks in the table's own bar first, and nothing is saved yet",
   identical(conf_texto(), "¿Desactivar 1 fila?") && length(guardados()) == k)
clic(sprintf("document.querySelector('#%s .hdt-confirmar-no')", A))
ok("E8: Cancelar in the bar saves nothing", length(guardados()) == k)
clic(boton("e-tst", "desactivar"))
otra_sesion()   # positions shift while the confirmation is up
si()
ok("E8: Confirmar deactivates TEST (soft delete), and only TEST",
   identical(guardado_de("e-tst"), "TEST:TEST EMPRESA SA:FALSE:TRUE") &&
   identical(guardado_de("e-ng"), "NG:Networks Group LCT:TRUE:FALSE") &&
   identical(guardado_de("e-new"), "AAA:Alfa Nueva SA:TRUE:FALSE"))
ok("E8: it leaves the table, is logged, and the user is told",
   !"e-tst" %in% vis() && any(grepl("^log desactivar_empresa e-tst$", registro())) &&
   grepl("Empresa 'TEST' desactivada", aviso()))

# ── E9 · ERP by id ──────────────────────────────────────────────────────────
clic(boton("e-nts", "erp"))
Sys.sleep(0.6)
ok("E9: 🔌 on NTS opens Conexiones ERP for NTS",
   grepl("Conexiones ERP — NTS", ev("(document.querySelector('.modal-title')||{}).textContent || ''")))
invisible(ev("$('.modal').modal('hide'); 0")); Sys.sleep(0.8)

# ── Rule 8 · a failed save is never silent ──────────────────────────────────
ok("fixture app restarted with saves failing", arrancar(falla = TRUE) && abrir())
clic(boton("e-ng", "desactivar")); si()
ok("rule 8: a failed Desactivar tells the user and leaves the row",
   grepl("No se pudo guardar el cambio", aviso()) && "e-ng" %in% vis() && !length(guardados()))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
