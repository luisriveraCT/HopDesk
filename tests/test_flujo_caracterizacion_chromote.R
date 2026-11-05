# =============================================================================
# tests/test_flujo_caracterizacion_chromote.R
# The Flujo de caja preview in a real headless Chrome (T17c): what only real
# clicks and keys show. Step 0 recorded today's preview here; since step 2
# the same cases (B1..) run on the engine (table cfp-tabla, "flujo_caja").
# The fixture is tests/helpers/flujo_fixture_app.R with FLU_DATOS=prueba (the
# real module and aggregation over tests/helpers/flujo_datos_prueba.R; no
# S3). What the browser paints is compared with the R layer
# (tests/helpers/flujo_adaptador_motor.R), which the R characterization holds
# to the reference flujo_antes.json. Case numbers (B1..) are those of
# reportes/T17c_paso0.md; [CAMBIA] marks the approved changes.
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
source("tests/helpers/flujo_adaptador_motor.R")
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
T <- "cfp-tabla-hdt"
abierto <- function() isTRUE(ev("document.body.classList.contains('cf-preview-open')"))
lista <- function() unlist(ev(sprintf("HDT.estado('%s').lista", T)))
fila <- function(id) sprintf("HDT.fila('%s', '%s')", T, id)
textos_fila <- function(id) unlist(ev(sprintf("Array.prototype.map.call(%s.children, function (td) { return td.textContent.trim(); })", fila(id))))
grupo <- function(primera) sprintf("document.querySelector('#%s tr.hdt-grupo[data-primera=\"%s\"]')", T, primera)
textos_grupo <- function(primera) unlist(ev(sprintf("Array.prototype.map.call(%s.children, function (td) { return td.textContent.trim(); })", grupo(primera))))
dia0 <- function(moneda) sprintf("%s.children[2]", fila(paste0("acum|", moneda)))   # 0 Contraparte, 1 Etiqueta, 2 Día 0
todos <- function() clic(sprintf("document.querySelector('#%s .hdt-grupos-todos')", T))
abrir <- function(limpiar = TRUE) {
  b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE)
  poll_until(function() isTRUE(ev("!!window.Shiny && !!document.getElementById('abrir_flujo')")), timeout_s = 60)
  Sys.sleep(0.8)
  if (limpiar) ev("try { localStorage.removeItem('hopdesk_cf_preview_state'); localStorage.removeItem('hopdesk_cf_dia0:cliente_a'); localStorage.removeItem('hopdesk_cf_dia0:cliente_b'); } catch (e) {}; 0")
  clic("document.getElementById('abrir_flujo')")
  espera(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s') && HDT.estado('%s').lista.length > 0", T, T))), 60)
}
# What R says each row reads (the engine's payload: hd_filas), by id.
esperado <- function(ed = ED, day0 = list()) {
  r <- fl_rejilla_motor(ed, day0 = day0, expandido = TRUE)
  list(r = r, filas = lapply(setNames(seq_len(nrow(r$datos)), r$datos$id), function(i)
    vapply(r$filas$celdas, function(c) unclass(c$mostrar)[i], character(1))))
}

ronda <- function(w, h) {
  et <- sprintf("[%dx%d]", w, h)
  pantalla(w, h)
  ok(paste(et, "the preview opens with this week's grid"), abrir())
  E <- esperado()
  ok(paste(et, "B2: [CAMBIA] each currency is a band, Cobros / Pagos a closed group (one row, its sums), then NETO and POS."),
     identical(lista(), c("banda:1:MXN", "grupo:cerrado:MXN|ar|1,MXN|ar|2,MXN|ar|3", "grupo:cerrado:MXN|ap|1,MXN|ap|2",
                          "neto|MXN", "acum|MXN", "banda:1:USD", "grupo:cerrado:USD|ar|1", "grupo:cerrado:USD|ap|1",
                          "neto|USD", "acum|USD")))
  ok(paste(et, "B1a: a closed group reads its name and each column's sum, the Total in whole pesos (F7)"), {
    s <- fl_bloque_m(E$r, "MXN")$secciones$cobros$subtotal
    g <- textos_grupo("MXN|ar|1")
    grepl("Cobros", g[1], fixed = TRUE) && identical(g[4:10], unname(vapply(s$celdas, `[[`, "", "texto"))) &&
      identical(g[11], s$total$texto)
  })
  todos()
  ok(paste(et, "B2b: the engine's open-all shows every counterparty, and closes them again"), {
    v <- espera(function() !any(grepl("^grupo:cerrado", lista())))
    todos()
    v && espera(function() sum(grepl("^grupo:cerrado", lista())) == 4L)
  })
  todos(); espera(function() !any(grepl("^grupo:cerrado", lista())))
  ok(paste(et, "B1: every row the browser paints reads what R says (texts; F1-F10 hold there)"),
     all(vapply(names(E$filas), function(id) {
       p <- textos_fila(id)
       identical(p[-1], unname(E$filas[[id]][-1])) && startsWith(p[1], E$filas[[id]][1])
     }, logical(1))))
  ok(paste(et, "B3: net tones -- a negative red, a zero grey, a positive green (F8); counterparties have none"), {
    cls <- function(id, k) ev(sprintf("%s.children[%d].className", fila(id), k))
    grepl("hdt-tc-peligro", cls("neto|MXN", 3)) && grepl("hdt-tc-neutro", cls("neto|MXN", 4)) &&
      grepl("hdt-tc-exito", cls("neto|MXN", 5)) && !grepl("hdt-tc-", cls("MXN|ap|1", 3))
  })
  # Día 0: typed in the position row's cell, an expression, Enter.
  ok(paste(et, "B4: [CAMBIA] Día 0 is typed in MXN's position row (no dialog); the net row can't be"),
     isTRUE(ev(sprintf("%s.classList.contains('hdt-editable') && !%s.children[2].classList.contains('hdt-editable')",
                       dia0("MXN"), fila("neto|MXN")))))
  clic(dia0("MXN"))
  ev("document.activeElement.select()"); b$Input$insertText(text = "1000+500"); tecla("Enter", "Enter", 13L, "\r")
  ok(paste(et, "B5: 1000+500 and Enter: MXN's Día 0 is 1,500 and its position moves; USD's doesn't (F9, F10)"),
     espera(function() {
       E2 <- esperado(day0 = list(MXN = 1500))
       identical(textos_fila("acum|MXN")[-1], unname(E2$filas[["acum|MXN"]][-1])) &&
         identical(textos_fila("acum|MXN")[3], "$2K") && identical(textos_fila("acum|USD")[3], "$0")
     }, 10))
  ok(paste(et, "B5b: [CAMBIA] Día 0 is kept in the browser, per currency, under the client's own key"),
     identical(ev("localStorage.getItem('hopdesk_cf_dia0:cliente_a')"), "{\"MXN\":1500}") && is.null(ev("localStorage.getItem('hopdesk_cf_dia0')")))
  clic(dia0("MXN"))
  ok(paste(et, "B6: reopening the cell starts from 1500, not '$2K'"),
     identical(ev("document.activeElement.value"), "1500") || identical(ev("Number(document.activeElement.value)"), 1500L))
  tecla("Escape", "Escape", 27L)
  # [CAMBIA B7] It used to close the whole preview (Esc's keyup reached the document).
  ok(paste(et, "B7: [CAMBIA] Esc in the Día 0 cell leaves it as it was and the preview stays open"),
     abierto() && !isTRUE(ev(sprintf("!!%s.querySelector('input')", dia0("MXN")))))
  clic("document.getElementById('cfp-btn_date_range')")
  espera(function() isTRUE(ev("!!document.querySelector('.air-datepicker.-active-')")), 5)
  tecla("Escape", "Escape", 27L)
  ok(paste(et, "B7b: [CAMBIA] Esc with the date picker open closes the picker, not the preview"),
     abierto() && !isTRUE(ev("!!document.querySelector('.air-datepicker.-active-')")))
  ev("document.activeElement && document.activeElement.blur(); 0")
  tecla("Escape", "Escape", 27L)
  ok(paste(et, "B8: Esc anywhere else closes the preview"), espera(function() !abierto()))
  abrir(limpiar = FALSE)
  ok(paste(et, "B5c: [CAMBIA] after a reload MXN's Día 0 is still 1,500"),
     espera(function() identical(textos_fila("acum|MXN")[3], "$2K"), 10))
  clic("document.getElementById('cambiar_cliente')")
  ok(paste(et, "B5d: switching to another client shows its own Día 0 (none), never this one's"),
     espera(function() identical(textos_fila("acum|MXN")[3], "$0"), 10))
  clic("document.getElementById('cambiar_cliente')")
  ok(paste(et, "B5e: and back, this client's 1,500 again"), espera(function() identical(textos_fila("acum|MXN")[3], "$2K"), 10))
  # Toolbar.
  clic("document.getElementById('cfp-btn_grouping')")
  ok(paste(et, "B9: the grouping button reads Diario after Auto, and the grid keeps 7 days (F13)"),
     espera(function() identical(trimws(ev("document.getElementById('cfp-btn_grouping').textContent")), "Diario")) &&
       identical(length(textos_fila("neto|MXN")), 11L))
  clic("document.getElementById('cfp-btn_curmode')")
  ok(paste(et, "B10: Base shows the FX row with USD's rate and no MXN one (F14)"),
     espera(function() isTRUE(ev("!!document.getElementById('cfp-fx_rate_usd') && !document.getElementById('cfp-fx_rate_mxn')"))))
  ev("(function(){ var i = document.getElementById('cfp-fx_rate_usd'); i.focus(); i.value = '20'; i.dispatchEvent(new Event('input', {bubbles: true})); i.blur(); })()")
  ok(paste(et, "B11: a USD rate of 20 turns CLIENT USA's 10,000 into 200,000 MXN in one block (F11)"),
     espera(function() {
       l <- lista()
       !any(grepl("USD", l)) && { todos(); TRUE } &&
         any(vapply(grep("^MXN\\|ar\\|", unlist(ev(sprintf("HDT.estado('%s').visibles", T))), value = TRUE), function(id) {
           p <- textos_fila(id); startsWith(p[1], "CLIENT USA") && identical(p[5], "$200K") }, logical(1)))
     }, 12))
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
fl_bloque_m <- function(r, moneda) { for (x in r$bloques) if (identical(x$moneda, moneda)) return(x); NULL }

ronda(1293, 632)
ronda(1772, 866)

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
