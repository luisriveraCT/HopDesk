# =============================================================================
# tests/test_sincro_hd_chromote.R
# Ajustes › Sincronizar Agenda (T11), in a real headless Chrome, against
# tests/helpers/sincro_fixture_app.R: the real observer and the real agenda
# persistence over an in-memory S3 (no aws.s3). Every click is a real input
# event. Blocks follow the T11 inventory
# (hopdesk_coord/tablas/reportes/T11_inventario.md): T the preview table (§2);
# A/F turning the sync on, D turning it off, each with failed reads and saves
# (seen in passing 1 and 2: a failed save used to carry on as a success).
#
# Run from project root:  Rscript tests/test_sincro_hd_chromote.R
# SIN_RAIZ=<older checkout> runs the same test against that checkout's code.
# =============================================================================
cat("=== test_sincro_hd_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38998L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
arrancar <- function(falla = "", falla_lee = "", sync = FALSE, hub = FALSE) {
  if (!is.null(app)) stop_bg_app(app)
  app <<- tryCatch(start_bg_app(sprintf('Sys.setenv(SIN_FALLA = "%s", SIN_FALLA_LEE = "%s", SIN_SYNC = "%s", SIN_HUB = "%s", SIN_RAIZ = "%s"); setwd("%s"); source("tests/helpers/sincro_fixture_app.R")$value',
                                        falla, falla_lee, if (sync) "1" else "0", if (hub) "1" else "0",
                                        Sys.getenv("SIN_RAIZ", repo), repo), PORT, timeout_s = 90),
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
raton <- function(tipo, p, botones = 0L)
  b$Input$dispatchMouseEvent(type = tipo, x = p[[1]], y = p[[2]], button = if (tipo == "mouseMoved") "none" else "left",
                             buttons = botones, clickCount = 1L)
clic <- function(js_el) {
  p <- centro(js_el)
  raton("mouseMoved", p); raton("mousePressed", p, 1L); raton("mouseReleased", p)
  Sys.sleep(0.6)
}
T <- "sincro_t-hdt"
estado <- function() ev(sprintf("HDT.estado('%s')", T))
vis <- function() unlist(estado()$visibles)
titulos <- function() unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.dataset.col != null; }).map(function(th){ return th.firstChild.textContent; })", T)))
texto_tabla <- function() ev(sprintf("(document.getElementById('%s')||{}).textContent || ''", T))
registro <- function() strsplit(ev("document.getElementById('registro').textContent") %||% "", "\n")[[1]]
aviso <- function() ev("Array.from(document.querySelectorAll('.shiny-notification')).map(function(n){ return n.textContent; }).join(' / ')")
modal_titulo <- function() gsub("[[:space:]]+", " ", ev("(document.querySelector('.modal.show .modal-title')||{}).textContent || ''"))
abrir <- function() {
  invisible(b$Page$navigate(base, wait_ = TRUE))
  poll_until(function() isTRUE(ev("!!window.Shiny && !!document.getElementById('stg_btn_sincro')")), timeout_s = 60)
  Sys.sleep(1)
  clic("document.getElementById('stg_btn_sincro')")
  poll_until(function() isTRUE(ev("!!document.getElementById('sincro_toggle')")), timeout_s = 20)
}
# Turn the switch on and wait for the account picker.
activar <- function() {
  clic("document.querySelector('label[for=sincro_toggle]')")
  poll_until(function() isTRUE(ev("!!document.getElementById('sincro_r_ana')")), timeout_s = 20)
}
elegir <- function(u) { clic(sprintf("document.getElementById('sincro_r_%s')", u)); Sys.sleep(0.8) }

ok("the Sincronizar panel opens", abrir())

# ── T · the preview table (register E) ──────────────────────────────────────
ok("T: switching it on opens the account picker", activar())
elegir("ana")
ok("T: the preview is on the engine", poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s')", T))), timeout_s = 20))
ok("P2: all 65 pending items, not the first 50, by the item's id",
   poll_until(function() length(vis()) == 65L, timeout_s = 10) && identical(vis()[c(1, 65)], c("ana-01", "ana-65")))
ok("P2: the counter says it (65 registros)", grepl("65 registros", texto_tabla()))
ok("T: columns Empresa / Parte / Importe / Mon. (from the dictionary)",
   identical(titulos(), c("Empresa", "Parte", "Importe", "Mon.")))
ok("T: Importe is money with its currency's format", grepl("$1,000.50", ev(sprintf("HDT.fila('%s','ana-01').textContent", T)), fixed = TRUE))
elegir("beto")
ok("T: choosing another account shows its 3 items", poll_until(function() identical(vis(), c("beto-01", "beto-02", "beto-03")), timeout_s = 10))
elegir("caro")
ok("T: an account with nothing says so", poll_until(function() grepl("Esta cuenta no tiene elementos", texto_tabla()), timeout_s = 10))
ok("T: choosing an account writes nothing", !any(grepl("^escribe |^log ", registro())))

boton_modal <- function(id) sprintf("document.getElementById('%s')", id)
tiene <- function(patron) any(grepl(patron, registro()))
# Turn the sync on from ana's agenda.
activar_con_ana <- function() {
  ok <- abrir() && activar(); elegir("ana")
  clic(boton_modal("do_sincro_activar")); Sys.sleep(1.2)
  ok
}
# Turn it off (sync starts on) and confirm.
desactivar <- function() {
  ok <- abrir()
  clic("document.querySelector('label[for=sincro_toggle]')")
  ok <- ok && poll_until(function() isTRUE(ev("!!document.getElementById('do_sincro_desactivar')")), timeout_s = 20)
  Sys.sleep(0.5)
  clic(boton_modal("do_sincro_desactivar")); Sys.sleep(1.2)
  ok
}

# ── A · turning it on, nothing fails ────────────────────────────────────────
ok("A: fixture restarted", arrancar() && activar_con_ana())
ok("A: it saves ana's 65 items as the shared agenda, then the config, then tells the other sessions",
   tiene("^escribe pagar_hoy_sync.rds 65$") && tiene("^escribe agenda_sync_config.rds 1$") && tiene("^bump$"))
ok("A: logged and announced", tiene("^log activar_sincronizacion_agenda ana$") && grepl("Sincronización activada", aviso()))

# ── F · turning it on, a save or a read fails ───────────────────────────────
falla_activar <- function(etiqueta) {
  ok(paste0(etiqueta, ": the user is told it was NOT turned on, and the picker stays open"),
     grepl("No se pudo activar la sincronización", aviso()) && grepl("Activar sincronización", modal_titulo()))
  ok(paste0(etiqueta, ": never announced as on, no success log, no sync bump"),
     !grepl("Sincronización activada", aviso()) && !tiene("^log activar_sincronizacion_agenda ana$") && !tiene("^bump$"))
  ok(paste0(etiqueta, ": the failure is logged"), tiene("^log activar_sincronizacion_agenda_fallido ana$"))
}
ok("F1: fixture restarted with the shared agenda's save failing", arrancar(falla = "pagar_hoy_sync") && activar_con_ana())
falla_activar("F1")
ok("F1: and the config is not saved as on", !tiene("agenda_sync_config"))
ok("F2: fixture restarted with the config's save failing", arrancar(falla = "agenda_sync_config") && activar_con_ana())
falla_activar("F2")
ok("F3: fixture restarted with ana's agenda failing to read", arrancar(falla_lee = "pagar_hoy_user_ana") && activar_con_ana())
falla_activar("F3")
ok("F3: nothing is written from an unread agenda (it used to start from an empty one)", !tiene("^escribe "))

# ── D · turning it off ──────────────────────────────────────────────────────
ok("D: fixture restarted with the sync on", arrancar(sync = TRUE) && desactivar())
ok("D: every account gets a copy of the shared agenda (3 items), then the config is saved off",
   tiene("^escribe pagar_hoy_user_ana.rds 3$") && tiene("^escribe pagar_hoy_user_beto.rds 3$") &&
   tiene("^escribe pagar_hoy_user_caro.rds 3$") && tiene("^escribe agenda_sync_config.rds 1$") && tiene("^bump$"))
ok("D: logged and announced", tiene("^log desactivar_sincronizacion_agenda") && grepl("Sincronización desactivada", aviso()))
falla_desactivar <- function(etiqueta) {
  ok(paste0(etiqueta, ": never announced as off, no success log, no sync bump, the config is not saved off"),
     !grepl("Sincronización desactivada", aviso()) && !tiene("^log desactivar_sincronizacion_agenda $") &&
     !tiene("^bump$") && !tiene("^escribe agenda_sync_config"))
  ok(paste0(etiqueta, ": the failure is logged"), tiene("^log desactivar_sincronizacion_agenda_fallido"))
}
ok("D1: fixture restarted with beto's copy failing", arrancar(falla = "pagar_hoy_user_beto", sync = TRUE) && desactivar())
ok("D1: the user is told whose copy failed and that the sync is still on",
   grepl("No se pudo copiar la agenda a: beto", aviso()) && grepl("sigue activa", aviso()))
falla_desactivar("D1")
ok("D1: the log names beto", tiene("^log desactivar_sincronizacion_agenda_fallido beto$"))
ok("D2: fixture restarted with the shared agenda failing to read", arrancar(falla_lee = "pagar_hoy_sync", sync = TRUE) && desactivar())
ok("D2: the user is told, and no account gets an (empty) copy",
   grepl("No se pudo desactivar la sincronización", aviso()) && !tiene("^escribe "))
falla_desactivar("D2")
ok("D3: fixture restarted with the config's save failing", arrancar(falla = "agenda_sync_config", sync = TRUE) && desactivar())
ok("D3: the user is told it is still on", grepl("No se pudo desactivar la sincronización", aviso()))
falla_desactivar("D3")

# ── J · the switch after Cancelar or a failure, inside Ajustes (fix 3) ──────
# In the app the panel lives inside the Ajustes modal, and the activation /
# deactivation dialogs REPLACE it (one modal at a time). Cancelar closes it;
# reopening Ajustes paints the panel again from the saved config, so the
# switch shows the real state.
interruptor <- function() ev("(document.getElementById('sincro_toggle')||{}).checked")
cancelar <- function() {
  clic("Array.from(document.querySelectorAll('.modal.show button')).filter(function(b){ return b.textContent.trim() === 'Cancelar'; })[0]")
  Sys.sleep(1.2)
}
ajustes_sincro <- function() {
  clic("document.getElementById('abrir_ajustes')"); Sys.sleep(1)
  clic("document.getElementById('stg_btn_sincro')")
  poll_until(function() isTRUE(ev("!!document.getElementById('sincro_toggle')")), timeout_s = 20)
}
con_ajustes <- function(...) arrancar(..., hub = TRUE) && {
  invisible(b$Page$navigate(base, wait_ = TRUE))
  poll_until(function() isTRUE(ev("!!document.getElementById('abrir_ajustes')")), timeout_s = 60)
}
ok("J: fixture restarted with Ajustes as a modal", con_ajustes())
ok("J1: Ajustes › Sincronización, switch off", ajustes_sincro() && isFALSE(interruptor()))
ok("J1: switching it on replaces Ajustes with the account picker", activar() && is.null(interruptor()))
cancelar()
ok("J1: Cancelar closes it; reopening Ajustes shows the switch OFF (nothing was turned on)",
   !nzchar(modal_titulo()) && ajustes_sincro() && isFALSE(interruptor()) && !tiene("agenda_sync_config"))
ok("J2: fixture restarted, Ajustes as a modal, the shared agenda's save failing", con_ajustes(falla = "pagar_hoy_sync"))
invisible(ajustes_sincro() && activar()); elegir("ana")
clic(boton_modal("do_sincro_activar")); Sys.sleep(1.2)
ok("J2: the activation fails and says so", grepl("No se pudo activar la sincronización", aviso()))
cancelar()
ok("J2: after Cancelar, reopening Ajustes shows the switch OFF", ajustes_sincro() && isFALSE(interruptor()))
ok("J3: fixture restarted, Ajustes as a modal, the sync on", con_ajustes(sync = TRUE))
ok("J3: the switch shows ON", ajustes_sincro() && isTRUE(interruptor()))
clic("document.querySelector('label[for=sincro_toggle]')")
invisible(poll_until(function() isTRUE(ev("!!document.getElementById('do_sincro_desactivar')")), timeout_s = 20))
cancelar()
ok("J3: Cancelar on turning it off; reopening Ajustes shows it still ON", ajustes_sincro() && isTRUE(interruptor()))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
