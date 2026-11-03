# =============================================================================
# tests/test_sincro_hd_chromote.R
# Ajustes › Sincronizar Agenda (T11), in a real headless Chrome, against
# tests/helpers/sincro_fixture_app.R: the real observer and the real agenda
# persistence over an in-memory S3 (no aws.s3). Every click is a real input
# event. Blocks follow the T11 inventory
# (hopdesk_coord/tablas/reportes/T11_inventario.md): T the preview table (§2).
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
arrancar <- function(falla = "") {
  if (!is.null(app)) stop_bg_app(app)
  app <<- tryCatch(start_bg_app(sprintf('Sys.setenv(SIN_FALLA = "%s", SIN_RAIZ = "%s"); setwd("%s"); source("tests/helpers/sincro_fixture_app.R")$value',
                                        falla, Sys.getenv("SIN_RAIZ", repo), repo), PORT, timeout_s = 90),
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

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
