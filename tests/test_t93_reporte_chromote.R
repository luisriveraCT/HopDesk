# =============================================================================
# tests/test_t93_reporte_chromote.R
# T93: "Subir reporte de BajíoNet" in Archivos de pago, in a real headless
# Chrome, against tests/helpers/registros_fixture_app.R with REG_T93=1 (the
# real pagosAuditViewerServer and .btr_servidor; what would be written is
# recorded in #registro, no S3). Made-up report, the real file shape.
#   R1  the button is there; it opens the upload; the file is read and the
#       preview (hd_table "respuesta_banco") shows one row per payment
#   R2  UNO comes ticked (Aplicado + Correcto, HopDesk's own file); DOS
#       (Editado) and TRES (Rechazado) are locked -- a click on their choice
#       does not change it; no whole account on screen
#   R3  Confirmar: only UNO's Agenda row (A1) goes through Confirmar, with its
#       clave de rastreo and the upload's /parte; the report is recorded with
#       its 3 payments and the event subir_reporte_banco
#
# Run from project root:  Rscript tests/test_t93_reporte_chromote.R
# =============================================================================
cat("=== test_t93_reporte_chromote ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38997L
app <- NULL; b <- NULL
capturas <- file.path(tempdir(), "t93_capturas"); unlink(capturas, recursive = TRUE); dir.create(capturas)
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
app <- tryCatch(start_bg_app(sprintf('Sys.setenv(REG_T93 = "1", REG_STAFF = "0", REG_MUCHAS = "0", REG_RAIZ = "%s"); setwd("%s"); source("tests/helpers/registros_fixture_app.R")$value',
                                     repo, repo), PORT, timeout_s = 90), error = function(e) { message(conditionMessage(e)); NULL })
ok("fixture app started (REG_T93)", !is.null(app))
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
invisible(b$Emulation$setDeviceMetricsOverride(width = 1772L, height = 866L, deviceScaleFactor = 1, mobile = FALSE))
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
clic <- function(js_el) { p <- centro(js_el); raton("mouseMoved", p); raton("mousePressed", p, 1L); raton("mouseReleased", p); Sys.sleep(0.6) }
# As a person does since T51 (2026-11-17): the tabs live in the ☰ menu's
# panel, hidden (and click-through) until #hd-menu-btn opens it. A real click
# on ☰, wait for hd-menu-abierto and the link to show, a real click on the
# link; it fails if the menu did not open or the tab did not change (T98).
pestana <- function(v) {
  enlace <- sprintf("document.querySelector(\".navbar .nav-link[data-value='%s']\")", v)
  visible <- function() isTRUE(ev(sprintf("(function(){ var a = %s; if (!a) return false;
    var s = getComputedStyle(a.closest('.navbar-collapse') || a); return s.visibility === 'visible' && a.offsetParent !== null; })()", enlace)))
  if (!visible()) {
    clic("document.getElementById('hd-menu-btn')")
    if (!poll_until(function() isTRUE(ev("document.querySelector('nav.navbar').classList.contains('hd-menu-abierto')")),
                    timeout_s = 10)) stop("pestana(): the ☰ menu did not open")
    # Right after loading, the tier rules may still hide the link (seen in
    # the real app: ~10 s in a first session); wait for it, apart.
    if (!poll_until(visible, timeout_s = 30)) stop("pestana(): the ☰ menu opened but the link ", v, " is not shown")
  }
  clic(enlace)
  if (!poll_until(function() isTRUE(ev(sprintf("(document.querySelector('.navbar .nav-link.active')||{}).dataset.value === '%s'", v))), timeout_s = 10))
    stop("pestana(): the tab did not change to ", v)
  Sys.sleep(0.8)
  TRUE
}
registro <- function() strsplit(ev("document.getElementById('registro').textContent") %||% "", "\n")[[1]]
foto <- function(n) { s <- b$Page$captureScreenshot(); writeBin(jsonlite::base64_dec(s$data), file.path(capturas, n)) }

ENC <- paste0("#,Fecha,Hora de Operación,Cuenta Origen,Nombre del Ordenante,Cuenta Destino,Tipo Cuenta Destino,",
              "Banco Destino,Nombre del Beneficiario,Alias del Beneficiario,RFC,Correo del beneficiario,Transacción,",
              "Importe de la Operación,IVA,Divisa,Clasificación de la Operación,Concepto de Pago,Referencia,",
              "Comisión,IVA Comisión,No. de Autorización / Clave de Rastreo,Capturó,Autorizó,",
              "Estatus de Transferencia,Mensaje")
linea <- function(n, cuenta, alias, imp, aut, registro = "Correcto", estatus = "Aplicado", mensaje = "")
  paste(n, "24112026", "101500", "73543010201", "EMPRESA SA", cuenta, "CLABE", "BBVA", "PROV", alias, "XAXX010101000", "",
        "Transferencia Interbancaria SPEI", imp, "0.00", "MN", "", "F", "123456", "0.00", "0.00", paste0(aut, "/BB", aut),
        "U 24-Nov-2026", "U 24-Nov-2026 Horas", estatus, mensaje, "", "", "Bajionet", registro, sep = ",")
rep <- file.path(tempdir(), "TransferenciasporArchivo_t93.txt")
writeLines(c("EMPRESA SA,1234567,24112026,152859,pagos_NCS_24112026_001.txt,24112026,152500,3,3,600.00,Aplicado", ENC,
             linea(1, "012180000000000001", "UNO", "100.00", "1111111111111"),
             linea(2, "012180000000000002", "DOS", "250.00", "2222222222222", registro = "Editado"),
             linea(3, "012180000000000003", "TRES", "300.00", "3333333333333", estatus = "Rechazado", mensaje = "Cuenta inexistente")),
           rep, useBytes = TRUE)

invisible(b$Page$navigate(base, wait_ = TRUE))
ok("the app loads", poll_until(function() isTRUE(ev("!!window.Shiny && !!document.querySelector('.navbar')")), timeout_s = 60))
Sys.sleep(1.5)
ok("the tab Archivos de pago opens through the ☰ menu, with real clicks", isTRUE(pestana("PAG")))

# ── R1 · Upload ─────────────────────────────────────────────────────────────
ok("R1: the button \"Subir reporte de BajíoNet\" is in Archivos de pago",
   poll_until(function() isTRUE(ev("!!document.getElementById('pag-btr_subir') && !!document.getElementById('pag-btr_subir').offsetParent")), timeout_s = 30) &&
     grepl("BajíoNet", ev("document.getElementById('pag-btr_subir').textContent"), fixed = TRUE))
clic("document.getElementById('pag-btr_subir')")
ok("R1: it opens the upload", poll_until(function() isTRUE(ev("!!document.getElementById('pag-btr_archivo')")), timeout_s = 15))
doc <- b$DOM$getDocument()
nodo <- b$DOM$querySelector(doc$root$nodeId, "#pag-btr_archivo")
invisible(b$DOM$setFileInputFiles(files = list(normalizePath(rep, winslash = "\\")), nodeId = nodo$nodeId))
T <- "pag-btr_t-hdt"
ok("R1: the preview table is drawn with the 3 payments",
   poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s') && HDT.estado('%s').visibles.length === 3", T, T))), timeout_s = 40))
Sys.sleep(1); foto("R1_vista_previa.png")
texto <- ev(sprintf("document.getElementById('%s').textContent", T))
ok("R1: it says it is HopDesk's own batch (camino A)",
   grepl("generado por HopDesk", ev("(document.querySelector('.modal.show .modal-body')||{}).textContent || ''"), fixed = TRUE))

# ── R2 · States and locks ───────────────────────────────────────────────────
ok("R2: no whole account on screen, the masked one is", !grepl("012180000000000001", texto) && grepl("****0001", texto, fixed = TRUE))
ok("R2: the bank's message shows (Rechazado)", grepl("Cuenta inexistente", texto, fixed = TRUE))
fila_txt <- function(id) gsub("[[:space:]]+", " ", ev(sprintf("HDT.fila('%s','%s').textContent", T, id)))
ok("R2: UNO comes ticked \"Confirmar\" (Aplicado: se confirma); DOS and TRES \"Omitir\"",
   grepl("Confirmar", fila_txt("L3")) && grepl("se confirma", fila_txt("L3")) &&
     grepl("Omitir", fila_txt("L4")) && grepl("Editado en BajíoNet", fila_txt("L4"), fixed = TRUE) &&
     grepl("Omitir", fila_txt("L5")) && grepl("No aplicado", fila_txt("L5")))
ok("R2: DOS (Editado) and TRES (Rechazado) are locked, UNO is not",
   identical(unlist(ev(sprintf("HDT.estado('%s').bloqueadas", T))), c("L4", "L5")) &&
     isTRUE(ev(sprintf("HDT.fila('%s','L4').classList.contains('hdt-bloq') && !HDT.fila('%s','L3').classList.contains('hdt-bloq')", T, T))))

# ── R3 · Confirm ────────────────────────────────────────────────────────────
clic("document.getElementById('pag-btr_confirmar')")
ok("R3: only A1 goes through the Agenda's Confirmar, with its clave de rastreo and the upload's /parte",
   poll_until(function() any(grepl("^confirmar ", registro())), timeout_s = 20) &&
     identical(grep("^confirmar ", registro(), value = TRUE), "confirmar A1 BB1111111111111 subir_reporte_banco"))
ok("R3: the report is recorded (3 payments, their states) with the event subir_reporte_banco",
   poll_until(function() any(grepl("^respuestas ", registro())), timeout_s = 20) &&
     identical(grep("^respuestas ", registro(), value = TRUE), "respuestas subir_reporte_banco 3 confirmar,editado,no_aplicado"))
ok("R3: the modal closes", poll_until(function() isTRUE(ev("!document.querySelector('.modal.show')")), timeout_s = 10))
foto("R3_hecho.png")

cat("\n  capturas:", capturas, "\n")
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
