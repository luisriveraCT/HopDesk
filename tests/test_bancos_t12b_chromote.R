# =============================================================================
# tests/test_bancos_t12b_chromote.R
# Bancos' other tables on hd_table (T12b), in a real headless Chrome,
# against tests/helpers/bancos_fixture_app.R (the real bancosServer on the
# CM09's in-memory S3).
#   C. Cuentas (cuentas_bancos, D): the active accounts, read only, Excel.
#   R. Reasignar (reasignar, E): the chosen import session's movements, all.
#   I. Importar TXT (importar_txt, E): a 25-movement TXT with 2 commissions
#      -> the preview shows the 25 (it used to show 20, without commissions)
#      and the button imports those 25.
#
# Run from project root:  Rscript tests/test_bancos_t12b_chromote.R
# =============================================================================
cat("=== test_bancos_t12b_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
estado <- function(t) ev(sprintf("HDT.estado('%s')", t))
vis <- function(t) unlist(estado(t)$visibles)
centro <- function(js_el) ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', behavior:'instant'});
  var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
raton <- function(tipo, p, botones = 0L) b$Input$dispatchMouseEvent(type = tipo, x = p[[1]], y = p[[2]],
  button = if (tipo == "mouseMoved" && !botones) "none" else "left", buttons = botones, clickCount = 1L)
clic <- function(js_el) { p <- centro(js_el); raton("mouseMoved", p); raton("mousePressed", p, 1L); raton("mouseReleased", p); Sys.sleep(0.4) }
titulos <- function(t) unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.dataset.col != null; }).map(function(th){ return th.firstChild.textContent; })", t)))
pestana <- function(v) { clic(sprintf("document.querySelector('a[data-value=\"%s\"]')", v)); Sys.sleep(1) }
sub_pestana <- function(t) { clic(sprintf("Array.from(document.querySelectorAll('.nav-link')).filter(function(a){ return a.textContent.trim() === '%s'; })[0]", t)); Sys.sleep(1) }

app <- tryCatch(start_bg_app(sprintf('setwd("%s"); source("tests/helpers/bancos_fixture_app.R")$value', repo), 38981L,
                             timeout_s = 240), error = function(e) { message(conditionMessage(e)); NULL })
ok("fixture app started", !is.null(app))
b <- new_chromote_session()
invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
invisible(b$Page$navigate("http://127.0.0.1:38981", wait_ = TRUE))
ok("the page loads (the Libro's table is there)",
   poll_until(function() isTRUE(ev("!!window.HDT && !!HDT.estado('bnc-libro-hdt')")), timeout_s = 180))
Sys.sleep(1)

# ══ C · Cuentas ══════════════════════════════════════════════════════════════
C <- "bnc-cuentas-hdt"
pestana("cuentas")
ok("C1: the 3 active accounts, by company, bank and currency",
   poll_until(function() length(tryCatch(vis(C), error = function(e) NULL)) == 3L, timeout_s = 15))
ok("C2: columns Empresa, Banco, Moneda, Alias, Cuenta, CLABE",
   identical(titulos(C), c("Empresa", "Banco", "Moneda", "Alias", "Cuenta", "CLABE")))
ok("C3: Exportar a Excel is there (Luis, T12 P7)",
   isTRUE(ev(sprintf("Array.from(document.querySelectorAll('#%s button')).some(function(x){ return /Exportar a Excel/.test(x.textContent); })", C))))
ok("C4: read only: no row buttons", isTRUE(ev(sprintf("!document.querySelector('#%s button[data-accion]')", C))))

# ══ R · Reasignar ════════════════════════════════════════════════════════════
R <- "bnc-reasig-hdt"
pestana("historial"); sub_pestana("Papelera")
clic("document.querySelector('[data-bs-target=\"#bnc-reasignar_panel\"]')"); Sys.sleep(1)
ses <- unlist(ev("Array.from(document.querySelectorAll('#bnc-reasig_sesion option')).map(function(o){ return o.value; }).filter(function(v){ return v; })"))
invisible(ev(sprintf("(function(){ var s = document.getElementById('bnc-reasig_sesion'); if (s.selectize) s.selectize.setValue('%s'); else { s.value = '%s'; $(s).trigger('change'); } })()",
                     ses[1], ses[1])))
ok("R1: the session's 60 movements, all of them (no pages of 10)",
   poll_until(function() length(tryCatch(vis(R), error = function(e) NULL)) == 60L, timeout_s = 15))
ok("R2: columns Fecha, Tipo, Parte, Concepto, Cargo, Abono",
   identical(titulos(R), c("Fecha", "Tipo", "Parte", "Concepto", "Cargo", "Abono")))

# ══ I · Importar TXT ═════════════════════════════════════════════════════════
I <- "bnc-import-hdt"
txt <- local({
  movs <- c(lapply(1:23, function(i) list(desc = sprintf("SPEI Enviado: | Beneficiario: PROV %02d SA | Recibo # %d", i, i), cargo = 100 + i)),
            lapply(1:2, function(i) list(desc = sprintf("Comision por SPEI | Recibo # %d", 90 + i), cargo = 5)))
  cg <- sum(vapply(movs, `[[`, numeric(1), "cargo"))
  hdr <- sprintf("NETWORKS TRUCKING SERVICES SA DE CV,0111,Conecta BanBajio,01,26-Oct-2026,12:16:39,Mes Actual,TODOS,01-Oct-2026 al 31-Oct-2026,,%d,%.2f,%.2f",
                 length(movs), cg, 0)
  filas <- vapply(seq_along(movs), function(i) sprintf("%d,%02d-Oct-2026,12:00:%02d,%d,%s,%.2f,,%.2f", i, 1 + i %% 28, i %% 60,
                                                      700000 + i, movs[[i]]$desc, movs[[i]]$cargo, 1000), character(1))
  f <- tempfile(fileext = ".txt"); writeLines(c(hdr, "#,Fecha Movimiento,Hora,Recibo,Descripción,Cargos,Abonos,Saldo", filas), f); f
})
pestana("importar")
doc <- b$DOM$getDocument(); nodo <- b$DOM$querySelector(doc$root$nodeId, "#bnc-txt_file")
invisible(b$DOM$setFileInputFiles(files = list(normalizePath(txt)), nodeId = nodo$nodeId)); Sys.sleep(2)
ok("I1: the preview shows all 25 new movements, the 2 commissions included",
   poll_until(function() length(tryCatch(vis(I), error = function(e) NULL)) == 25L, timeout_s = 20) &&
     isTRUE(ev(sprintf("/Comisi/.test(document.getElementById('%s').innerText)", I))))
ok("I2: ...and the button imports exactly those 25",
   poll_until(function() grepl("Importar 25 movimientos", ev("(document.getElementById('bnc-import_btn_ui') || {}).innerText || ''"),
                               fixed = TRUE), timeout_s = 10))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) {
  cat("\n-- app stderr (tail) --\n"); try(cat(tail(readLines(app$proc$get_error_file(), warn = FALSE), 25), sep = "\n"))
  quit(status = 1L)
}
