# =============================================================================
# tests/test_ajustes_hd_chromote.R
# Ajustes on the hd_table engine (T16, T16c), in a real headless Chrome at
# Luis's 1293x632, against tests/helpers/ajustes_tablas_fixture_app.R: the
# real observers over an in-memory S3. Every click is a real input event.
#   C · Cuentas de Empresa: bancos_cat and cuentas_empresa, opened by id even
#       after another session adds an account ahead;
#   R · the saved IC registry (ic_registro);
#   B · Business Partners (bp_sap): no pre-selection, "IC ✓", En catálogo,
#       "Aplicar como IC" saves the marked code;
#   E · the IC scanner (escaner_ic): the same columns without En catálogo,
#       "Aplicar selección" saves the marked code;
#   P · without can_manage_interco, no "Aplicar como IC".
#
# Run from project root:  Rscript tests/test_ajustes_hd_chromote.R
# AJ_RAIZ=<older checkout> runs the same test against that checkout's code.
# =============================================================================
cat("=== test_ajustes_hd_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 39002L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
arrancar <- function(sin_permiso = FALSE) {
  if (!is.null(app)) stop_bg_app(app)
  app <<- tryCatch(start_bg_app(sprintf('Sys.setenv(AJ_SIN_PERMISO = "%s", AJ_RAIZ = "%s"); setwd("%s"); source("tests/helpers/ajustes_tablas_fixture_app.R")$value',
                                        if (sin_permiso) "1" else "0", Sys.getenv("AJ_RAIZ", repo), repo), PORT, timeout_s = 120),
                   error = function(e) { message(conditionMessage(e)); NULL })
  !is.null(app)
}
ok("fixture app started", arrancar())
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
base <- sprintf("http://127.0.0.1:%d", PORT)
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
  Sys.sleep(0.8)
}
doble <- function(js_el) {
  p <- centro(js_el)
  raton("mouseMoved", p)
  raton("mousePressed", p, 1L, 1L); raton("mouseReleased", p, 0L, 1L)
  raton("mousePressed", p, 1L, 2L); raton("mouseReleased", p, 0L, 2L)
  Sys.sleep(0.8)
}
por_id <- function(id) sprintf("document.getElementById('%s')", id)
listo <- function(t) poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!document.getElementById('%s') && !!HDT.estado('%s')", t, t))), timeout_s = 30)
estado <- function(t) ev(sprintf("HDT.estado('%s')", t))
vis <- function(t) unlist(estado(t)$visibles)
fila <- function(t, id, sel = "") sprintf("HDT.fila('%s', '%s')%s", t, id, sel)
boton <- function(t, id, accion) fila(t, id, sprintf(".querySelector('td.hdt-acc button[data-accion=%s]')", accion))
titulos <- function(t) unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.dataset.col != null; }).map(function(th){ return th.firstChild.textContent; })", t)))
texto_fila <- function(t, id) ev(sprintf("%s.textContent", fila(t, id)))
registro <- function() strsplit(ev("document.getElementById('registro').textContent") %||% "", "\n")[[1]]
valor <- function(id) ev(sprintf("(document.getElementById('%s')||{}).value || ''", id))
existe <- function(id) isTRUE(ev(sprintf("!!document.getElementById('%s')", id)))
marcar <- function(t, id) {
  p <- centro(fila(t, id, ".children[1]"))
  raton("mouseMoved", p); raton("mousePressed", p, 1L); raton("mouseReleased", p); Sys.sleep(0.6)
}
abrir <- function() {
  invisible(b$Page$navigate(base, wait_ = TRUE))
  poll_until(function() isTRUE(ev("!!window.Shiny && !!window.Shiny.shinyapp && !!document.getElementById('stg_btn_cuentas')")), timeout_s = 60)
}
ok("the page loads", abrir())

# ── C · Cuentas de Empresa ──────────────────────────────────────────────────
BC <- "bancos_cat_t-hdt"; CU <- "cuentas_t-hdt"
clic(por_id("stg_btn_cuentas"))
ok("C: banks and accounts are on the engine", listo(BC) && listo(CU))
ok("C: the banks by id, columns Banco / Clave", identical(vis(BC), c("b1", "b2")) && identical(titulos(BC), c("Banco", "Clave")))
ok("C: the active accounts; the PPL source carries its badge",
   identical(vis(CU), c("c1", "c2")) && grepl("PPL", texto_fila(CU, "c1")) && !grepl("PPL", texto_fila(CU, "c2")))
clic(por_id("otra_sesion_cuenta")); Sys.sleep(1)
ok("C: another session adds c0 ahead -- the table repaints with it first", identical(vis(CU), c("c0", "c1", "c2")))
clic(boton(CU, "c2", "editar"))
ok("C: ✎ on c2 opens c2 (0200), not the account now in its row", poll_until(function() identical(valor("cta_cuenta"), "0200"), timeout_s = 10))
doble(fila(BC, "b2", ".children[1]"))
ok("C: a double click on Santander opens Santander", poll_until(function() identical(valor("banco_nombre"), "Santander"), timeout_s = 10))

# ── R · the saved IC registry ───────────────────────────────────────────────
RG <- "ic_registro_t-hdt"
clic(por_id("stg_btn_interco"))
ok("R: the saved registry is on the engine, NTS with C1027", listo(RG) && identical(vis(RG), "NTS") && grepl("C1027", texto_fila(RG, "NTS")))

# ── B · Business Partners ───────────────────────────────────────────────────
BP <- "bp_sap_t-hdt"
clic(por_id("stg_btn_catalogo"))
ok("B: Business Partners is on the engine", listo(BP))
ok("B: rows by initials|ledger|code, by company, CxC first, most invoices first",
   identical(vis(BP), c("NCS|AP|P3003", "NTS|AR|C2001", "NTS|AR|C1027")))
ok("B: columns IC / Empresa / Libro / Código / Nombre SAP / RFC / Facturas / En catálogo",
   identical(titulos(BP), c("IC", "Empresa", "Libro", "Código", "Nombre SAP", "RFC", "Facturas", "En catálogo")))
ok("B: nothing comes marked; “IC ✓” only on C1027",
   !length(unlist(estado(BP)$seleccion)) && grepl("IC ✓", texto_fila(BP, "NTS|AR|C1027")) &&
     !grepl("IC ✓", texto_fila(BP, "NTS|AR|C2001")))
ok("B: En catálogo by code on P3003", grepl("✓ Código", texto_fila(BP, "NCS|AP|P3003")))
marcar(BP, "NTS|AR|C2001")
ok("B: a click marks C2001, by its id", identical(unlist(estado(BP)$seleccion), "NTS|AR|C2001"))
clic(por_id("bp_apply_ic_btn")); Sys.sleep(1)
ok("B: “Aplicar como IC” saves 2001 next to 1027",
   any(grepl("^escribe interco_v2.rds NTS ar=1027,2001 ap=;", registro())))
ok("B: and C2001 now carries “IC ✓”", poll_until(function() grepl("IC ✓", texto_fila(BP, "NTS|AR|C2001")), timeout_s = 10))

# ── E · the IC scanner ──────────────────────────────────────────────────────
ES <- "escaner_ic_t-hdt"
clic(por_id("stg_btn_interco"))
clic(por_id("scan_ic_btn"))
ok("E: the scanner's table is on the engine, in its modal", listo(ES))
ok("E: Business Partners' columns without En catálogo",
   identical(titulos(ES), c("IC", "Empresa", "Libro", "Código", "Nombre SAP", "RFC", "Facturas")))
ok("E: nothing comes marked", !length(unlist(estado(ES)$seleccion)))
marcar(ES, "NCS|AP|P3003")
clic(por_id("ic_scan_apply")); Sys.sleep(1.5)
ok("E: “Aplicar selección” saves 3003 as NCS's CxP code, and keeps the rest",
   any(grepl("NTS ar=1027,2001 ap=;NCS ar= ap=3003$", registro())))

# ── P · without can_manage_interco ──────────────────────────────────────────
ok("P: fixture restarted as Finanzas", arrancar(sin_permiso = TRUE) && abrir())
clic(por_id("stg_btn_catalogo"))
ok("P: the table shows, without “Aplicar como IC”", listo(BP) && !existe("bp_apply_ic_btn"))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
