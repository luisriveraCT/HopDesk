# =============================================================================
# tests/test_arranque_app_chromote.R
# Start-up test of the REAL app before every deploy (T15). The 2026-11-03
# incident: a hd_table datos() threw with its source still NULL at start-up,
# closed the whole session, and production stayed on "Cargando… 14%"; no
# module test saw it because every fixture starts with its data loaded.
#
# For each mode (normal: dev's real data; vacio: every tenant read empty) and
# each test user it starts app.R through tests/helpers/arranque_app.R (own
# Rscript, dev bucket, no S3 writes, no production reads), signs in, and
# requires:
#   - the loading screen reaches 100% and goes, within LIMITE seconds;
#   - the session stays connected;
#   - no "Warning: Error in" / "Execution halted" in the R process's output;
#   - every main tab (and Ajustes) opens with no new error in that output;
#   - client user, normal mode: the Flujo de caja preview opens from the
#     Calendario, and its PDF export downloads with full amounts ("$1,234,567",
#     never "$1.2M") and visible zeros ("$0") -- the .cf_fmt clash of 3e93b92.
#
# Credentials come from the environment (.Renviron.local), never the repo;
# the passwords are never printed:
#   HOPDESK_TEST_USER / HOPDESK_TEST_PASS                 staff (hd-admin, at home: Grupo)
#   HOPDESK_TEST_CLIENTE_USER / HOPDESK_TEST_CLIENTE_PASS a Networks client user
# Without them the test SKIPS with a clear message and exit status 2: a
# skip is never a pass.
#
# Run from project root:  Rscript tests/test_arranque_app_chromote.R
# ARR_RAIZ=<checkout> runs it against that checkout's app.R (ebfbf5c must fail).
# ARR_MODOS / ARR_USUARIOS narrow it ("normal" or "vacio"; "prueba" or "cliente").
# =============================================================================
cat("=== test_arranque_app_chromote ===\n\n")
if (file.exists(".Renviron.local")) readRenviron(".Renviron.local")
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

LIMITE <- as.numeric(Sys.getenv("ARR_LIMITE", "300"))   # from "Entrar" to the overlay gone: ~27 s idle, up to ~180 s on a busy machine
usuarios <- list(
  prueba  = c(user = Sys.getenv("HOPDESK_TEST_USER"),         pass = Sys.getenv("HOPDESK_TEST_PASS")),
  cliente = c(user = Sys.getenv("HOPDESK_TEST_CLIENTE_USER"), pass = Sys.getenv("HOPDESK_TEST_CLIENTE_PASS")))
faltan <- names(usuarios)[!vapply(usuarios, function(u) all(nzchar(u)), logical(1))]
if (length(faltan)) {
  cat("SKIP: faltan las credenciales de prueba de:", paste(faltan, collapse = ", "),
      "(HOPDESK_TEST_USER / HOPDESK_TEST_PASS, HOPDESK_TEST_CLIENTE_USER / HOPDESK_TEST_CLIENTE_PASS",
      "en .Renviron.local).\n     Esta prueba NO paso: no se corrio.\n")
  quit(status = 2L)
}
# The exported PDF is read with poppler's pdftotext (Git for Windows ships it).
PDFTOTEXT <- Sys.which("pdftotext")
if (!nzchar(PDFTOTEXT)) PDFTOTEXT <- "C:/Program Files/Git/mingw64/bin/pdftotext.exe"
modos <- strsplit(Sys.getenv("ARR_MODOS", "normal,vacio"), ",")[[1]]
quienes <- strsplit(Sys.getenv("ARR_USUARIOS", paste(names(usuarios), collapse = ",")), ",")[[1]]

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
  invisible(isTRUE(result))   # callers branch on it (if (!ok(...)) next)
}
source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
raiz <- Sys.getenv("ARR_RAIZ", repo)
PORT <- 39030L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)

registro <- function() c(readLines(app$proc$get_output_file(), warn = FALSE),
                         readLines(app$proc$get_error_file(), warn = FALSE))
errores <- function(desde = 0L) {
  l <- registro(); l <- l[seq_along(l) > desde]
  l[grepl("Warning: Error in|Execution halted", l)]
}
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
conectado <- function() isTRUE(tryCatch(ev("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())"),
                                        error = function(e) FALSE))
# Signed in = the sign-in form is gone and the app's navbar is there. Without
# this, "no loading screen" was also true before signing in at all: the first
# run passed while the session had died at the sign-in gate.
dentro <- function() isTRUE(tryCatch(ev(
  "(function(){ var f = document.getElementById('auth-user_id');
     return (!f || f.offsetParent === null) && !!document.querySelector('.nav-link[data-value]'); })()"),
  error = function(e) FALSE))
carga_terminada <- function() dentro() && isTRUE(tryCatch(ev(
  "(function(){ var o = document.getElementById('hopdesk-loading');
     return !o || o.style.display === 'none' || o.classList.contains('hld-fadeout'); })()"), error = function(e) FALSE))
pct <- function() tryCatch(ev("(document.getElementById('hld-pct')||{}).textContent || ''"), error = function(e) "?")
mostrar <- function(lineas) if (length(lineas)) cat(paste0("      | ", utils::head(lineas, 8)), sep = "\n")

PESTANAS <- c(CAL = "Calendario", VEN = "Vencidos", PH = "Agenda de Hoy", BNC = "Bancos", IC = "Intercompany",
              PSV = "Pasivos", FC = "Pronósticos", RPT = "Reporte", GRUPO = "Grupo")
tiempos <- list()

visible_js <- function(sel) sprintf("(function(){ var a = document.querySelector(%s); return !!a && a.offsetParent !== null; })()",
                                    jsonlite::toJSON(sel, auto_unbox = TRUE))
esperar_visible <- function(sel, timeout_s) poll_until(function() isTRUE(tryCatch(ev(visible_js(sel)), error = function(x) FALSE)),
                                                       timeout_s = timeout_s, interval_s = 1)
clic <- function(sel) ev(sprintf("document.querySelector(%s).click(); true", jsonlite::toJSON(sel, auto_unbox = TRUE)))

# Calendario -> Flujo de caja preview -> Exportar -> PDF, read back with pdftotext.
flujo_pdf <- function(et) {
  n <- length(registro())
  ev("document.querySelector(\".nav-link[data-value='CAL']\").click(); true")
  if (!ok(paste(et, "Flujo: the Calendario shows the preview arrow"), esperar_visible(".cf-preview-toggle-btn", 90))) return()
  clic(".cf-preview-toggle-btn")
  ok(paste(et, "Flujo: the preview opens, with its Exportar button"), esperar_visible("#cfp-btn_export", 60))
  e <- errores(n); ok(paste(et, "Flujo: no new error opening the preview"), conectado() && !length(e)); mostrar(e)

  clic("#cfp-btn_export")
  if (!ok(paste(et, "Flujo: Exportar opens the export window"),
          poll_until(function() isTRUE(tryCatch(ev("!!document.getElementById('export_format')"), error = function(x) FALSE)),
                     timeout_s = 30, interval_s = 1))) return()
  ev("(function(){ var s = document.getElementById('export_format');
       if (s.selectize) s.selectize.setValue('pdf'); else $(s).val('pdf').trigger('change'); return true; })()")
  if (!ok(paste(et, "Flujo: choosing PDF shows its Exportar button"), esperar_visible("#btn_export_pdf", 30))) return()
  # The button is Shiny's download link; a headless session's own browser
  # context ignores download settings, so the page fetches the link itself:
  # the same request a click sends, served by the same downloadHandler.
  # Until Shiny binds the output the link has no download href, and fetching it returns the page (HTML).
  if (!ok(paste(et, "Flujo: the PDF link gets its download href"), poll_until(function() isTRUE(tryCatch(ev("((document.getElementById('btn_export_pdf') || {}).href || '').indexOf('/download/') >= 0"), error = function(x) FALSE)), timeout_s = 30, interval_s = 1))) return()
  n <- length(registro())
  r <- tryCatch(b$Runtime$evaluate(
    "(function(){ var a = document.getElementById('btn_export_pdf');
       return fetch(a.href, {credentials: 'same-origin'}).then(function(r){
         return r.arrayBuffer().then(function(buf){
           var u = new Uint8Array(buf), s = '', i;
           for (i = 0; i < u.length; i += 32768) s += String.fromCharCode.apply(null, u.subarray(i, i + 32768));
           return JSON.stringify({status: r.status, type: r.headers.get('content-type'), b64: btoa(s)});
         });
       }); })()", awaitPromise = TRUE, returnByValue = TRUE, timeout_ = 240),
    error = function(x) { message("  ERROR: ", conditionMessage(x)); NULL })
  res <- tryCatch(jsonlite::fromJSON(r$result$value), error = function(x) NULL)
  pdf <- file.path(tempdir(), "arranque_flujo.pdf")
  if (!is.null(res) && identical(as.integer(res$status), 200L)) writeBin(jsonlite::base64_dec(res$b64), pdf)
  es_pdf <- file.exists(pdf) && identical(readBin(pdf, "raw", 4L), charToRaw("%PDF"))
  if (!ok(sprintf("%s Flujo: the PDF downloads (HTTP %s, %s)", et, res$status %||% "?", res$type %||% "?"), es_pdf)) {
    mostrar(errores(n)); return()
  }
  texto <- tryCatch(system2(PDFTOTEXT, c("-layout", shQuote(pdf), "-"), stdout = TRUE, stderr = FALSE),
                    error = function(x) character())
  if (!ok(sprintf("%s Flujo: the PDF has text (%s, %d lines)", et, basename(pdf), length(texto)), length(texto) > 0L)) return()
  compactos <- regmatches(texto, gregexpr("\\$ ?-?[0-9]+([.][0-9]+)? ?[KkMm]\\b", texto, perl = TRUE))
  compactos <- unlist(compactos)
  ok(sprintf("%s Flujo: no compact amounts in the PDF (%s)", et,
             if (length(compactos)) paste(utils::head(unique(compactos), 4), collapse = " ") else "none"),
     !length(compactos))
  completos <- unlist(regmatches(texto, gregexpr("\\$ ?-?[0-9]{1,3}(,[0-9]{3})+([.][0-9]{2})?", texto, perl = TRUE)))
  ok(sprintf("%s Flujo: full amounts in the PDF (e.g. %s)", et, paste(utils::head(unique(completos), 3), collapse = " ")),
     length(completos) > 0L)
  ceros <- unlist(regmatches(texto, gregexpr("\\$ ?0([.]00)?(?![0-9,.])", texto, perl = TRUE)))
  ok(sprintf("%s Flujo: zeros show as $0 in the PDF (%d)", et, length(ceros)), length(ceros) > 0L)
  e <- errores(n); ok(paste(et, "Flujo: no new error exporting the PDF"), conectado() && !length(e)); mostrar(e)
  ev("$('.modal').modal('hide'); true")
  if (nzchar(Sys.getenv("ARR_REGISTRO"))) file.copy(pdf, file.path(Sys.getenv("ARR_REGISTRO"), "arranque_flujo.pdf"), overwrite = TRUE)
}

for (modo in modos) for (quien in quienes) {
  et <- sprintf("[%s · %s]", modo, quien)
  cat(sprintf("\n── %s ──\n", et))
  if (!is.null(app)) { try(b$close(), silent = TRUE); b <- NULL; stop_bg_app(app); app <- NULL }
  t_arranque <- Sys.time()
  app <- tryCatch(start_bg_app(sprintf('Sys.setenv(ARR_MODO = "%s", ARR_RAIZ = "%s"); setwd("%s"); source("tests/helpers/arranque_app.R")$value',
                                       modo, raiz, repo), PORT, timeout_s = 300),
                  error = function(e) { message(conditionMessage(e)); NULL })
  if (!ok(paste(et, "the real app starts on dev"), !is.null(app))) next
  t_proceso <- as.numeric(Sys.time() - t_arranque, units = "secs")
  b <- new_chromote_session()
  invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE))
  ok(paste(et, "the sign-in form shows"),
     poll_until(function() isTRUE(tryCatch(ev("!!document.getElementById('auth-user_id')"), error = function(e) FALSE)), timeout_s = 60))

  u <- usuarios[[quien]]
  ev(sprintf("(function(){ var u = document.getElementById('auth-user_id'), p = document.getElementById('auth-user_pwd');
     u.value = %s; p.value = %s; $(u).trigger('change'); $(p).trigger('change'); return true; })()",
     jsonlite::toJSON(u[["user"]], auto_unbox = TRUE), jsonlite::toJSON(u[["pass"]], auto_unbox = TRUE)))
  Sys.sleep(0.5)
  t_entrada <- Sys.time()
  ev("document.getElementById('auth-go_auth').click(); true")
  n_antes <- length(registro())

  # A session that died can't finish loading: stop waiting at the first error
  # in R's output. (Not on a dropped connection: signing in reloads the page,
  # so the socket drops for a moment by design.)
  poll_until(function() dentro() || length(errores()) > 0L, timeout_s = LIMITE, interval_s = 1)
  t_dentro <- as.numeric(Sys.time() - t_entrada, units = "secs")
  poll_until(function() carga_terminada() || length(errores()) > 0L,
             timeout_s = max(1, LIMITE - t_dentro), interval_s = 1)
  terminada <- carga_terminada()
  t_carga <- as.numeric(Sys.time() - t_entrada, units = "secs")
  tiempos[[et]] <- c(proceso = round(t_proceso), dentro = round(t_dentro), carga = round(t_carga))
  ok(paste(et, "signed in: the sign-in form is gone and the app is there"), dentro())
  ok(sprintf("%s the loading screen reaches 100%% and goes (%.0f s; it stood at %s)", et, t_carga, pct()), terminada)
  Sys.sleep(5)   # what the first flush queued after the overlay went
  ok(paste(et, "the session is still connected"), conectado())
  e <- errores(); ok(paste(et, "no 'Warning: Error in' / 'Execution halted' in the R output"), !length(e)); mostrar(e)

  # The server reveals the tabs a user may see after the overlay goes (GRUPO
  # for staff, app.R): give it a moment on a busy machine.
  poll_until(function() isTRUE(tryCatch(ev(sprintf("[%s].some(function(v){
      var a = document.querySelector(\".nav-link[data-value='\" + v + \"']\"); return !!a && a.offsetParent !== null; })",
      paste0("'", names(PESTANAS), "'", collapse = ","))), error = function(x) FALSE)), timeout_s = 60, interval_s = 1)
  probadas <- 0L
  for (v in names(PESTANAS)) {
    visible <- isTRUE(tryCatch(ev(sprintf("(function(){ var a = document.querySelector(\".nav-link[data-value='%s']\");
      return !!a && a.offsetParent !== null; })()", v)), error = function(x) FALSE))
    if (!visible) { cat(sprintf("   (pestaña %s no visible para %s)\n", PESTANAS[[v]], quien)); next }
    probadas <- probadas + 1L
    n <- length(registro())
    ev(sprintf("document.querySelector(\".nav-link[data-value='%s']\").click(); true", v))
    Sys.sleep(4)
    e <- errores(n)
    ok(sprintf("%s %s opens with no new error and the session alive", et, PESTANAS[[v]]), conectado() && !length(e)); mostrar(e)
  }
  if (!ok(sprintf("%s at least one main tab was there to open (%d)", et, probadas), probadas > 0L))
    cat("      | pestañas en la página:", tryCatch(ev("Array.from(document.querySelectorAll('.nav-link[data-value]')).map(function(a){
      var s = getComputedStyle(a); return a.getAttribute('data-value') + '(' + s.display + ',' + (a.offsetParent ? 'en' : 'sin') + ' offsetParent)'; }).join(' ')"),
      error = function(x) "?"), "\n")
  if (identical(quien, "cliente") && identical(modo, "normal")) flujo_pdf(et)
  if (isTRUE(tryCatch(ev("!!document.getElementById('btn_settings')"), error = function(x) FALSE))) {
    n <- length(registro())
    ev("document.getElementById('btn_settings').click(); true"); Sys.sleep(4)
    e <- errores(n)
    ok(paste(et, "Ajustes opens with no new error and the session alive"), conectado() && !length(e)); mostrar(e)
    ev("$('.modal').modal('hide'); true")
  }
  escrituras <- grep("\\[ARRANQUE\\] escritura omitida", registro(), value = TRUE)
  cat(sprintf("   (%d escritura(s) que la app intentó y la prueba no dejó llegar a S3)\n", length(escrituras)))
  # ARR_REGISTRO=<folder> keeps the app's R output per mode, to see where the time goes.
  if (nzchar(Sys.getenv("ARR_REGISTRO")))
    writeLines(registro(), file.path(Sys.getenv("ARR_REGISTRO"), sprintf("arranque_%s_%s.log", modo, quien)))
}

cat("\nTiempos (s): proceso = arranque de R hasta servir; dentro = de 'Entrar' a la app en pantalla;",
    "carga = de 'Entrar' a la pantalla de carga fuera\n")
for (k in names(tiempos)) cat(sprintf("  %-22s proceso %4.0f   dentro %4.0f   carga %4.0f\n", k,
                                      tiempos[[k]][["proceso"]], tiempos[[k]][["dentro"]], tiempos[[k]][["carga"]]))
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
