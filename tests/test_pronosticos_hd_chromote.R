# =============================================================================
# tests/test_pronosticos_hd_chromote.R
# The three Pronósticos tables on the hd_table engine (T10), in a real
# headless Chrome, against tests/helpers/pronosticos_fixture_app.R: the real
# module over fixed data and a simulated `shared` (no S3). Every click is a
# real input event. Blocks follow the T10 inventory
# (hopdesk_coord/tablas/reportes/T10_inventario.md): M methods (§4), S subscriptions (§5), C the fetch log (§3),
# R refresh after Actualizar (seen in passing 2).
#
# Run from project root:  Rscript tests/test_pronosticos_hd_chromote.R
# PRO_RAIZ=<older checkout> runs the same test against that checkout's module.
# =============================================================================
cat("=== test_pronosticos_hd_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38993L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
arrancar <- function(falla = FALSE) {
  if (!is.null(app)) stop_bg_app(app)
  app <<- tryCatch(start_bg_app(sprintf('Sys.setenv(PRO_FALLA = "%s", PRO_RAIZ = "%s"); setwd("%s"); source("tests/helpers/pronosticos_fixture_app.R")$value',
                                        if (falla) "1" else "0", Sys.getenv("PRO_RAIZ", repo), repo), PORT, timeout_s = 90),
                   error = function(e) { message(conditionMessage(e)); NULL })
  !is.null(app)
}
ok("fixture app started", arrancar())
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
invisible(b$Emulation$setDeviceMetricsOverride(width = 1772L, height = 866L, deviceScaleFactor = 1, mobile = FALSE))
base <- sprintf("http://127.0.0.1:%d", PORT)
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
M <- "fc-metodos_t-hdt"
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
escribir <- function(js_input, texto) {
  p <- centro(js_input); raton("mousePressed", p, 1L); raton("mouseReleased", p)
  ev(sprintf("var i=%s; i.value=''; i.dispatchEvent(new Event('input'))", js_input))
  if (nzchar(texto)) b$Input$insertText(text = texto)
  Sys.sleep(0.5)
}
pestana <- function(texto) clic(sprintf("Array.from(document.querySelectorAll('#fc-fc_tabs a')).filter(function(a){ return a.textContent.trim() === '%s'; })[0]", texto))
listo <- function(t) poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s') && !!document.getElementById('%s').offsetParent", t, t))), timeout_s = 30)
estado <- function(t) ev(sprintf("HDT.estado('%s')", t))
vis <- function(t) unlist(estado(t)$visibles)
fila <- function(t, id, sel = "") sprintf("HDT.fila('%s', '%s')%s", t, id, sel)
titulos <- function(t) unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.dataset.col != null; }).map(function(th){ return th.firstChild.textContent; })", t)))
texto_fila <- function(t, id) ev(sprintf("%s.textContent", fila(t, id)))
modal_titulo <- function() ev("(document.querySelector('.modal.show .modal-title')||{}).textContent || ''")
cerrar_modal <- function() { ev("$('.modal').modal('hide'); 0"); Sys.sleep(0.8) }
registro <- function() strsplit(ev("document.getElementById('registro').textContent") %||% "", "\n")[[1]]
aviso <- function() ev("Array.from(document.querySelectorAll('.shiny-notification')).map(function(n){ return n.textContent; }).join(' / ')")
abrir <- function() {
  invisible(b$Page$navigate(base, wait_ = TRUE))
  poll_until(function() isTRUE(ev("!!window.Shiny && !!document.getElementById('fc-fc_tabs')")), timeout_s = 60)
  Sys.sleep(1)
  TRUE
}

ok("the module loads", abrir())

# ── M · Métodos (catalog D) ─────────────────────────────────────────────────
pestana("Métodos y suscripciones")
ok("M: Métodos is on the engine", listo(M))
ok("M: the 3 methods, by method_id, in stored order",
   identical(vis(M), c("yesterday_spot", "spot", "manual_curve")))
ok("M: columns ID / Nombre / Tipo / Descripción (from the dictionary)",
   identical(titulos(M), c("ID", "Nombre", "Tipo", "Descripción")))
ok("M: a long description is cut, not wrapped (recorte)",
   isTRUE(ev(sprintf("(function(){ var td = %s; return td.scrollWidth > td.clientWidth; })()",
                     fila(M, "manual_curve", ".children[3]")))))
ok("M: no row buttons and no selection (read-only)",
   { clic(fila(M, "spot", ".children[1]"))
     length(unlist(estado(M)$seleccion)) == 0L && isTRUE(ev(sprintf("!%s", fila(M, "spot", ".querySelector('td.hdt-acc button')")))) })
ok("M: the engine's search finds by description",
   { escribir(sprintf("document.querySelector('#%s .hdt-buscar')", M), "publicado"); identical(vis(M), "yesterday_spot") })
escribir(sprintf("document.querySelector('#%s .hdt-buscar')", M), "")
ok("M: sorting by Nombre",
   { clic(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.firstChild && th.firstChild.textContent === 'Nombre'; })[0]", M))
     identical(vis(M), c("manual_curve", "spot", "yesterday_spot")) })

# ── S · Suscripciones (catalog D) ───────────────────────────────────────────
S <- "fc-subs_t-hdt"
boton <- function(t, id, accion) fila(t, id, sprintf(".querySelector('td.hdt-acc button[data-accion=%s]')", accion))
ok("S: Suscripciones is on the engine", listo(S))
ok("S1: the 3 active subscriptions by subscription_id; the inactive one is out",
   identical(vis(S), c("s-usd", "s-tiie", "s-psv")))
ok("S1: columns ID / Consumidor / Tipo / Métrica / Método",
   identical(titulos(S), c("ID", "Consumidor", "Tipo", "Métrica", "Método")))
ok("S7: Tipo is a badge (Global / Pasivo); Métrica shows the metric's label",
   grepl("Global", texto_fila(S, "s-usd")) && grepl("Pasivo", texto_fila(S, "s-psv")) &&
   grepl("TIIE 28d", texto_fila(S, "s-tiie")) && grepl("PSV-001", texto_fila(S, "s-psv")))
ok("S2: a click on a row no longer opens the editor",
   { clic(fila(S, "s-tiie", ".children[3]")); Sys.sleep(0.6); !nzchar(modal_titulo()) })
clic(boton(S, "s-tiie", "editar")); Sys.sleep(0.8)
ok("S2: ✎ on TIIE opens Editar suscripción for TIIE 28d",
   identical(modal_titulo(), "Editar suscripción") &&
   grepl("TIIE 28d", ev("document.querySelector('.modal.show .modal-body').textContent")))
# The modal covers the page, so "another session" arrives without a click.
invisible(ev("Shiny.setInputValue('otra_sesion', 1, {priority: 'event'}); 0")); Sys.sleep(1)
ok("S3: another session puts a subscription first while the editor is open -- the table shows it",
   identical(vis(S)[1], "s-new") && identical(modal_titulo(), "Editar suscripción"))
invisible(ev("$('#fc-sub_method_new')[0].selectize.setValue('manual_curve'); 0")); Sys.sleep(0.5)
clic("Array.from(document.querySelectorAll('.modal.show button')).filter(function(b){ return b.textContent.trim() === 'Guardar'; })[0]")
Sys.sleep(0.8)
guardado <- tail(grep("^guardar ", registro(), value = TRUE), 1)
ok("S5: Guardar saves TIIE's new method, and only TIIE's (by id, after the shift)",
   length(guardado) == 1L && grepl("s-tiie:manual_curve", guardado) && grepl("s-new:spot", guardado) &&
   grepl("s-usd:yesterday_spot", guardado) && grepl("s-psv:yesterday_spot", guardado))
ok("S5: logged, the editor closes and the user is told",
   any(grepl("^log editar_suscripcion s-tiie$", registro())) && !nzchar(modal_titulo()) &&
   grepl("Suscripción actualizada", aviso()))
doble(fila(S, "s-psv", ".children[3]"))
ok("S2: a double click opens the editor too (INPC, PSV-001)",
   identical(modal_titulo(), "Editar suscripción") &&
   grepl("INPC", ev("document.querySelector('.modal.show .modal-body').textContent")))
cerrar_modal()
clic(boton(S, "s-psv", "editar")); Sys.sleep(0.8)
ok("S4: ✎ on the same row again reopens it", identical(modal_titulo(), "Editar suscripción"))
cerrar_modal()

# ── C · Registro de consultas (register C) ──────────────────────────────────
C <- "fc-consultas_t-hdt"
pestana("Registro de consultas")
ok("C: the fetch log is on the engine", listo(C))
ok("BP1: all 250 fetches, not the last 200; the help line no longer says 200",
   length(vis(C)) == 250L && !grepl("200", ev("document.querySelector('#fc-consultas_t-hdt').parentElement.parentElement.textContent")))
sinC <- grep("^sin-id-", vis(C), value = TRUE)
ok("ids: a row with no fetch_id and two sharing one still show (made-up / suffixed ids)",
   length(sinC) == 1L && all(c("f-006", "f-006#1") %in% vis(C)))
ok("C: columns Fecha / Métrica / Fuente / Estado / Filas / ms / Error / Origen",
   identical(titulos(C), c("Fecha", "Métrica", "Fuente", "Estado", "Filas", "ms", "Error", "Origen")))
orden_ids <- sprintf("f-%03d", 1:250); orden_ids[5] <- sinC; orden_ids[7] <- "f-006#1"
ok("BP2: newest first by the instant -- every 30/09 row after every 30/10 row",
   identical(vis(C), orden_ids))
ok("BP2: Fecha shows date and time",
   grepl("31 oct 2026 23:00", texto_fila(C, "f-001")))
clic(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.firstChild && th.firstChild.textContent === 'Fecha'; })[0]", C))
ok("BP2: sorting Fecha ascending puts the oldest first", identical(vis(C)[1], "f-250"))
clic(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.firstChild && th.firstChild.textContent === 'Filas'; })[0]", C))
ok("C: Filas sorts as a number (0 < 9 < 10), not as text",
   { v <- vis(C); f <- function(id) which(v == id)
     f("f-003") < f("f-001") && f("f-001") < f("f-002") })
ok("BP7: Estado and Origen are badges (Error HTTP red, Programada)",
   isTRUE(ev(sprintf("!!%s", fila(C, "f-004", ".querySelector('.hdt-tono-rojo')")))) &&
   grepl("Error HTTP", texto_fila(C, "f-004")) && grepl("Programada", texto_fila(C, "f-001")))
ok("C: filters for Estado, Métrica and Origen",
   length(unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s button.hdt-filtro')).map(function(b){ return b.textContent; })", C)))) == 3L)
clic(boton(C, "f-004", "detalle")); Sys.sleep(0.8)
ok("C: a long Error is cut in the table (recorte), the bubble shows it",
   isTRUE(ev(sprintf("%s.classList.contains('hdt-recorte')", fila(C, "f-004", ".children[6]")))))
ok("BP4: ℹ shows the whole row with the error uncut",
   identical(modal_titulo(), "Detalle de la consulta") &&
   grepl("siguiente corrida programada del día", ev("document.querySelector('.modal.show .modal-body').textContent")) &&
   grepl("Error HTTP", ev("document.querySelector('.modal.show .modal-body').textContent")))
cerrar_modal()
doble(fila(C, "f-002", ".children[1]"))
ok("BP4: a double click opens the same detail",
   identical(modal_titulo(), "Detalle de la consulta") &&
   grepl("tiie_28", ev("document.querySelector('.modal.show .modal-body').textContent")))
cerrar_modal()

# ── R · refresh (seen in passing 2) ─────────────────────────────────────────
# The fetch log is no synced table: before, neither the status badge nor
# the Registro de consultas read it again after a refresh.
badge <- function() ev("(document.querySelector('#fc-fetch_status_badge .badge')||{}).textContent || ''")
pestana("Series"); Sys.sleep(1)
invisible(poll_until(function() nzchar(badge()), timeout_s = 10))
antes <- badge()
clic("document.getElementById('fc-btn_refresh')"); Sys.sleep(1.5)
ok("R: after Actualizar on a metric, its status badge shows today's fetch (it showed 31/10)",
   grepl("^OK .*31/10/2026 23:00", antes) && poll_until(function() !identical(badge(), antes), timeout_s = 10) &&
   grepl(format(Sys.Date(), "%d/%m/%Y"), badge()))
pestana("Registro de consultas"); Sys.sleep(0.8)
ok("R: the log shows that fetch too, and the row without fetch_id keeps its id",
   poll_until(function() length(vis(C)) == 251L, timeout_s = 10) && sinC %in% vis(C))
clic("document.getElementById('fc-btn_refresh_all')"); Sys.sleep(1.5)
ok("R: after Actualizar todas las métricas, the log shows the 3 new fetches",
   poll_until(function() length(vis(C)) == 254L, timeout_s = 10) &&
   sum(startsWith(vis(C), "nuevo-")) == 4L)

# ── F · a failed save is never silent (seen in passing 4, rule 8) ───────────
boton_modal <- function(texto) sprintf("Array.from(document.querySelectorAll('.modal.show button')).filter(function(b){ return b.textContent.trim() === '%s'; })[0]", texto)
ok("fixture app restarted with saves failing", arrancar(falla = TRUE) && abrir())
pestana("Métodos y suscripciones"); invisible(listo(S))
clic(boton(S, "s-usd", "editar")); Sys.sleep(0.8)
invisible(ev("$('#fc-sub_method_new')[0].selectize.setValue('spot'); 0")); Sys.sleep(0.4)
clic(boton_modal("Guardar")); Sys.sleep(1)
ok("F: a failed Guardar tells the user and keeps the editor open to retry",
   grepl("No se pudo guardar", aviso()) && identical(modal_titulo(), "Editar suscripción"))
ok("F: it is logged as editar_suscripcion_fallido, and nothing reads as saved",
   any(grepl("^log editar_suscripcion_fallido s-usd$", registro())) &&
   !any(grepl("^guardar |^log editar_suscripcion s-usd$", registro())) && !grepl("Suscripción actualizada", aviso()))
cerrar_modal()
clic("document.getElementById('fc-btn_add_global_sub')"); Sys.sleep(0.8)
clic(boton_modal("Crear")); Sys.sleep(1)
ok("F: a failed Crear (global subscription) tells the user, is logged, and the dialog stays",
   identical(modal_titulo(), "Nueva suscripción global") &&
   any(grepl("^log crear_suscripcion_global_fallido ", registro())) &&
   !any(grepl("^log crear_suscripcion_global ", registro())))
cerrar_modal()

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
