# =============================================================================
# tests/test_flujo_caracterizacion_chromote.R
# The Flujo de caja preview as it is today, in a real headless Chrome (T17c,
# step 0): what only real clicks and keys show. The fixture is
# tests/helpers/flujo_fixture_app.R with FLU_DATOS=prueba (the real module and
# aggregation over tests/helpers/flujo_datos_prueba.R; no S3); the grid is
# read with the R layer's adapter from what the browser painted. Case
# numbers (B1..) are those of reportes/T17c_paso0.md.
# Sizes: Luis's window (1293 x 632) and 1772 x 866; screenshots to
# FL_CAPTURAS if set.
#
# Run from project root:  Rscript tests/test_flujo_caracterizacion_chromote.R
# =============================================================================
cat("=== test_flujo_caracterizacion_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x

suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
    eval(e, envir = globalenv())
cargar("R/persistence.R", "tag_label")
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/cashflow_export_module.R", "R/cashflow_preview_module.R")) source(f)
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())
source("tests/helpers/flujo_datos_prueba.R")
source("tests/helpers/flujo_adaptador.R")
source("tests/helpers/live_app_harness.R")

LUNES <- lubridate::floor_date(Sys.Date(), "week", week_start = 1)
D <- flujo_datos_prueba(LUNES)
ED <- build_cashflow_export_data(D$df, D$tags, D$empresas, LUNES, LUNES + 6L, "auto")
repo <- normalizePath(".", winslash = "/")
PORT <- 38987L
CAPTURAS <- Sys.getenv("FL_CAPTURAS", "")
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
app <- tryCatch(start_bg_app(sprintf('Sys.setenv(FLU_DATOS = "prueba", FLU_RAIZ = "%s"); setwd("%s"); source("tests/helpers/flujo_fixture_app.R")$value',
                                     repo, repo), PORT, timeout_s = 90),
                error = function(e) { message(conditionMessage(e)); NULL })
ok("fixture app started", !is.null(app))
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
pantalla <- function(w, h) invisible(b$Emulation$setDeviceMetricsOverride(width = w, height = h, deviceScaleFactor = 1, mobile = FALSE))
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
centro <- function(js_el) ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', behavior:'instant'});
  var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
clic <- function(js_el) {
  p <- centro(js_el)
  for (t in c("mouseMoved", "mousePressed", "mouseReleased"))
    b$Input$dispatchMouseEvent(type = t, x = p[[1]], y = p[[2]], button = if (t == "mouseMoved") "none" else "left",
                               buttons = if (t == "mousePressed") 1L else 0L, clickCount = 1L)
  Sys.sleep(0.5)
}
tecla <- function(k, code, vk, texto = NULL) {
  if (is.null(texto)) b$Input$dispatchKeyEvent(type = "rawKeyDown", key = k, code = code, windowsVirtualKeyCode = vk)
  else b$Input$dispatchKeyEvent(type = "keyDown", key = k, code = code, windowsVirtualKeyCode = vk, text = texto)
  b$Input$dispatchKeyEvent(type = "keyUp", key = k, code = code, windowsVirtualKeyCode = vk)
  Sys.sleep(0.4)
}
espera <- function(f, s = 8) poll_until(function() isTRUE(tryCatch(f(), error = function(e) FALSE)), timeout_s = s)
eco <- function() jsonlite::fromJSON(ev("document.getElementById('eco').textContent") %||% "{}")
rejilla <- function(ed = ED, day0 = list()) fl_rejilla(ev("document.querySelector('.cf-table-wrap').outerHTML"), ed, day0)
abierto <- function() isTRUE(ev("document.body.classList.contains('cf-preview-open')"))
dia0 <- function(moneda) sprintf("document.querySelector('.cf-day0-cell[data-currency=\"%s\"]')", moneda)
dialogo <- function() isTRUE(ev("document.getElementById('cf-day0-dlg').style.display === 'block'"))
abrir <- function() {
  b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE)
  poll_until(function() isTRUE(ev("!!window.Shiny && !!document.getElementById('abrir_flujo')")), timeout_s = 60)
  Sys.sleep(0.8)
  ev("try { localStorage.removeItem('hopdesk_cf_preview_state'); } catch (e) {}; 0")
  clic("document.getElementById('abrir_flujo')")
  espera(function() !is.null(ev("document.querySelector('.cf-table')")), 60)
}

ronda <- function(w, h) {
  et <- sprintf("[%dx%d]", w, h)
  pantalla(w, h)
  ok(paste(et, "the preview opens with this week's grid"), abrir())
  ok(paste(et, "B1: the browser paints the same grid as the R layer (texts and order; F1-F10)"),
     identical(rejilla()$bloques, fl_rejilla(render_cashflow_preview_html(ED, FALSE, list()), ED)$bloques))
  ok(paste(et, "B2: counterparties start hidden; the expand button shows them all, and again hides them (F5)"), {
    oculto <- function() isTRUE(ev("Array.prototype.every.call(document.querySelectorAll('.cf-parte-row'), function (t) { return t.style.display === 'none'; })"))
    a <- oculto(); clic("document.getElementById('cfp-btn_expand')")
    v <- espera(function() isTRUE(ev("Array.prototype.every.call(document.querySelectorAll('.cf-parte-row'), function (t) { return t.style.display !== 'none'; })")))
    clic("document.getElementById('cfp-btn_expand')")
    a && v && espera(oculto)
  })
  ok(paste(et, "B3: net tones -- a negative red, a zero grey, a positive green (F8)"), {
    col <- function(k) ev(sprintf("getComputedStyle(document.querySelectorAll('.cf-net-row')[0].children[%d]).color", k + 1L))
    identical(c(col(1), col(2), col(3)), c("rgb(255, 107, 107)", "rgb(154, 181, 204)", "rgb(93, 216, 122)"))
  })
  # Día 0: the dialog, an expression, Enter.
  clic(dia0("MXN"))
  ok(paste(et, "B4: a click on MXN's Día 0 opens its dialog, empty for a 0"), dialogo() && identical(ev("document.getElementById('cf-day0-input').value"), ""))
  b$Input$insertText(text = "1000+500"); Sys.sleep(0.2)
  tecla("Enter", "Enter", 13L, "\r")
  ok(paste(et, "B5: 1000+500 and Enter: the dialog closes, MXN's Día 0 is 1,500 and its position moves; USD's doesn't (F9, F10)"),
     espera(function() identical(ev(sprintf("%s.getAttribute('data-valor')", dia0("MXN"))), "1500")) && !dialogo() && {
       r <- rejilla(day0 = list(MXN = 1500))
       identical(fl_bloque(r, "MXN")$acumulada$dia0$texto, "$2K") &&
         identical(fl_bloque(r, "MXN")$acumulada$celdas$c1$texto, "−$503K") &&
         identical(fl_bloque(r, "USD")$acumulada$dia0$valor, 0)
     })
  clic(dia0("MXN"))
  ok(paste(et, "B6: reopening reads 1500 back, not '$2K'"), identical(ev("document.getElementById('cf-day0-input').value"), "1500"))
  tecla("Escape", "Escape", 27L)
  esc_cierra_todo <- !abierto()
  ok(paste(et, "B7 [visto de paso, PENDIENTES]: Esc in the Día 0 dialog closes it AND the whole preview (Esc's keyup reaches the document)"),
     !dialogo() && esc_cierra_todo)
  if (!abierto()) { clic("document.getElementById('abrir_flujo')"); espera(abierto) }
  tecla("Escape", "Escape", 27L)
  ok(paste(et, "B8: Esc anywhere else closes the preview"), espera(function() !abierto()))
  clic("document.getElementById('abrir_flujo')"); espera(abierto)
  # Toolbar.
  clic("document.getElementById('cfp-btn_grouping')")
  ok(paste(et, "B9: the grouping button reads Diario after Auto, and the grid keeps 7 days (F13)"),
     espera(function() identical(trimws(ev("document.getElementById('cfp-btn_grouping').textContent")), "Diario")) &&
       ev("document.querySelectorAll('.cf-table thead th').length") == 10L)
  clic("document.getElementById('cfp-btn_curmode')")
  ok(paste(et, "B10: Base shows the FX row with USD's rate and no MXN one (F14)"),
     espera(function() isTRUE(ev("!!document.getElementById('cfp-fx_rate_usd') && !document.getElementById('cfp-fx_rate_mxn')"))))
  ev("(function(){ var i = document.getElementById('cfp-fx_rate_usd'); i.focus(); i.value = '20'; i.dispatchEvent(new Event('input', {bubbles: true})); i.blur(); })()")
  ok(paste(et, "B11: a USD rate of 20 turns CLIENT USA's 10,000 into 200,000 MXN in one block (F11)"),
     espera(function() {
       e <- build_cashflow_export_data(D$df, D$tags, D$empresas, LUNES, LUNES + 6L, "daily", "fused", "MXN", c(USD = 20))
       r <- rejilla(e)
       length(r$bloques) == 1L && identical(fl_fila(r, "MXN", "cobros", "CLIENT USA")$celdas$c2$texto, "$200K")
     }, 10))
  clic("document.getElementById('cfp-btn_export')")
  ok(paste(et, "B12: Exportar hands the export window the preview's settings (F18)"),
     espera(function() {
       p <- eco()$prefill
       identical(p[c("date_from", "date_to", "grouping", "currency_mode", "base_cur")],
                 list(date_from = as.character(LUNES), date_to = as.character(LUNES + 6L), grouping = "daily",
                      currency_mode = "fused", base_cur = "MXN")) && identical(as.numeric(p$fx_rates$USD), 20)
     }))
  if (nzchar(CAPTURAS)) { dir.create(CAPTURAS, showWarnings = FALSE, recursive = TRUE)
    b$screenshot(file.path(CAPTURAS, sprintf("flujo_%dx%d.png", w, h)), show = FALSE) }
}

ronda(1293, 632)
ronda(1772, 866)

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
