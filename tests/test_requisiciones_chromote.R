# =============================================================================
# tests/test_requisiciones_chromote.R
# The Requisiciones tab in a real headless Chrome, against
# tests/helpers/requisiciones_fixture_app.R (in-memory S3, seed data, user
# "dani" of tier Compras):
#   - Luis's screen, 1293 x 632 CSS px: the list, "+ Nueva requisición" and
#     the detail fit with no horizontal page scroll (nor inside the table);
#   - 390 px wide: still no horizontal page scroll;
#   - Compras never sees anything that converts to payment;
#   - pasting Excel cells (text/plain only, the HTML ignored) shows the
#     preview and the trim notice.
# Chromote needs the machine's turn (shared rule 28).
#
# Run from project root:  Rscript tests/test_requisiciones_chromote.R
# =============================================================================
cat("=== test_requisiciones_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
capturas <- Sys.getenv("REQ_CAPTURAS", "")      # a folder to keep screenshots in (optional)
T <- "req-t-hdt"
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
espera <- function(cond, timeout_s = 15)
  poll_until(function() isTRUE(tryCatch(cond(), error = function(e) FALSE)), timeout_s = timeout_s, interval_s = 0.2)
pantalla <- function(w, h) invisible(b$Emulation$setDeviceMetricsOverride(width = w, height = h, deviceScaleFactor = 1, mobile = FALSE))
foto <- function(nombre) if (nzchar(capturas)) {
  dir.create(capturas, showWarnings = FALSE, recursive = TRUE)
  invisible(b$screenshot(file.path(capturas, nombre), show = FALSE))
}
sin_scroll_h <- function() ev("document.documentElement.scrollWidth <= document.documentElement.clientWidth + 1 && document.body.scrollWidth <= document.body.clientWidth + 1")
desborde <- function(sel) ev(sprintf("(function(){ var x = document.querySelector('%s'); if (!x) return -1; return x.scrollWidth - x.clientWidth; })()", sel))
visibles <- function() unlist(ev(sprintf("(HDT.estado('%s')||{visibles:[]}).visibles", T)))
modal_abierto <- function() isTRUE(ev("!!document.querySelector('.modal.show')"))
cerrar_modal <- function() { ev("(function(){ var m = document.querySelector('.modal.show .btn-close'); if (m) m.click(); return 0; })()")
                             espera(function() !modal_abierto(), 5) }

app <- tryCatch(start_bg_app(sprintf('Sys.setenv(REQ_NIVEL = "compras", REQ_USUARIO = "dani"); setwd("%s"); source("tests/helpers/requisiciones_fixture_app.R")$value', repo),
                             38991L, timeout_s = 120), error = function(e) { message(conditionMessage(e)); NULL })
ok("fixture app started", !is.null(app))
if (is.null(app)) quit(status = 1L)
b <- new_chromote_session()
pantalla(1293L, 632L)
invisible(b$Page$navigate("http://127.0.0.1:38991", wait_ = TRUE))

# ── 1293 x 632 ───────────────────────────────────────────────────────────────
ok("the list paints its 4 rows (paid and trashed ones out)", espera(function() length(visibles()) == 4L, 60))
Sys.sleep(0.5)
foto("lista_1293.png")
ok("1293 x 632: no horizontal page scroll", sin_scroll_h())
ok("1293 x 632: the table itself does not scroll sideways",
   isTRUE(ev(sprintf("(function(){ var s = document.querySelector('#%s .hdt-scroll') || document.getElementById('%s'); return s.scrollWidth <= s.clientWidth + 1; })()", T, T))))
ok("1293 x 632: the table's footer is inside the window",
   isTRUE(ev(sprintf("(function(){ var r = document.getElementById('%s').getBoundingClientRect(); return r.bottom <= window.innerHeight + 1; })()", T))))
ok("Sin provisión rows are pinned on top", length(unlist(ev(sprintf("HDT.estado('%s').fijas", T)))) == 2L)

# ── Compras never sees convert-to-payment ────────────────────────────────────
ok("no row or bar button converts to payment (provision / ⚡)",
   isTRUE(ev("document.querySelectorAll('[data-accion=\"provision\"], .hdt-prov').length === 0")) &&
   !grepl("⚡", ev("document.body.innerText")) &&
   !grepl("Convertir provisión|convertir en pago", ev("document.body.innerHTML"), ignore.case = TRUE))

# ── "+ Nueva requisición" and the paste ─────────────────────────────────────
ev("document.getElementById('req-nueva').click(); 0")
ok("the new requisition window opens", espera(function() modal_abierto() && isTRUE(ev("!!document.getElementById('req-zona')")), 10))
Sys.sleep(0.6)
foto("nueva_1293.png")
ok("new requisition: no horizontal scroll in the window",
   isTRUE(desborde(".modal.show .modal-body") <= 1) && sin_scroll_h())
filas <- paste(c("Concepto\tImporte", sprintf("Partida %d\t%d", 1:340, 1:340)), collapse = "\\n")
ev(sprintf("(function(){ var z = document.getElementById('req-zona'); var dt = new DataTransfer();
  dt.setData('text/plain', '%s'); dt.setData('text/html', '<table><tr><td>HTML</td></tr></table>');
  z.dispatchEvent(new ClipboardEvent('paste', { clipboardData: dt, bubbles: true, cancelable: true })); return 0; })()", filas))
ok("pasted cells: the preview table shows with the trim notice",
   espera(function() grepl("Se pegaron 341 filas; se guardaron 200", ev("document.querySelector('.modal.show').innerText")), 10))
ok("pasted cells: the clipboard's HTML was ignored (no 'HTML' cell)",
   !grepl("\\bHTML\\b", ev("(document.getElementById('req-pegada-hdt')||{innerText:''}).innerText")))
Sys.sleep(0.4)
foto("pegado_1293.png")
ok("with the preview: still no horizontal scroll", isTRUE(desborde(".modal.show .modal-body") <= 1) && sin_scroll_h())
cerrar_modal()

# ── Detail ───────────────────────────────────────────────────────────────────
primera <- visibles()[1]
ev(sprintf("Shiny.setInputValue('req-t-accion', {accion: 'detalle', ids: ['%s'], nonce: Math.random()}, {priority: 'event'}); 0", primera))
ok("the detail opens", espera(function() modal_abierto() && grepl("REQ-", ev("document.querySelector('.modal.show').innerText")), 10))
Sys.sleep(0.5)
foto("detalle_1293.png")
ok("detail: no horizontal scroll", isTRUE(desborde(".modal.show .modal-body") <= 1) && sin_scroll_h())
ev("(function(){ var a = Array.from(document.querySelectorAll('.modal.show .nav-link')).find(function(x){ return /Evidencia/.test(x.textContent); }); if (a) a.click(); return 0; })()")
ok("detail, Evidencia: the gallery shows its image and table", espera(function()
  isTRUE(ev("document.querySelectorAll('.modal.show .hdreq-mini img').length >= 1 && document.querySelectorAll('.modal.show .hdreq-doc').length >= 1")), 10))
foto("detalle_evidencia_1293.png")
ok("detail, Evidencia: no horizontal scroll", isTRUE(desborde(".modal.show .modal-body") <= 1) && sin_scroll_h())
cerrar_modal()

# ── Provisions picker ────────────────────────────────────────────────────────
ev("document.getElementById('req-desde_provision').click(); 0")
ok("the provisions picker opens with the provisions without a requisition", espera(function()
  length(unlist(ev("(HDT.estado('req-selector-hdt')||{visibles:[]}).visibles"))) >= 1L, 15))
foto("selector_1293.png")
ok("picker: no horizontal scroll", isTRUE(desborde(".modal.show .modal-body") <= 1) && sin_scroll_h())
ok("picker: no convert-to-payment there either",
   isTRUE(ev("document.querySelectorAll('.modal.show [data-accion=\"provision\"]').length === 0")))
cerrar_modal()

# ── 390 px ───────────────────────────────────────────────────────────────────
pantalla(390L, 800L)
Sys.sleep(0.8)
foto("lista_390.png")
ok("390 px: no horizontal page scroll", sin_scroll_h())

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
