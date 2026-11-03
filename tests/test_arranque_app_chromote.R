# =============================================================================
# tests/test_arranque_app_chromote.R
# Start-up test of the REAL app before every deploy (T15). The 2026-11-03
# incident: a hd_table datos() threw with its source still NULL at start-up,
# closed the whole session, and production stayed on "Cargando… 14%"; no
# module test saw it because every fixture starts with its data loaded.
#
# For each mode (normal: dev's real data; vacio: every tenant read empty) and
# each dev test user (finance, with every write permission; analysis, without
# can_manage_bancos / can_manage_proveedores / can_register_payments), it
# starts app.R through tests/helpers/arranque_app.R (own Rscript, dev bucket,
# no S3 writes, no production reads), signs in, and requires:
#   - the loading screen reaches 100% and goes, within LIMITE seconds;
#   - the session stays connected;
#   - no "Warning: Error in" / "Execution halted" in the R process's output;
#   - every main tab (and Ajustes) opens with no new error in that output.
#
# Credentials come from the environment (.Renviron.local), never the repo:
#   HOPDESK_TEST_USER_FINANCE / HOPDESK_TEST_PASS_FINANCE
#   HOPDESK_TEST_USER_ANALYSIS / HOPDESK_TEST_PASS_ANALYSIS
# Without them the test SKIPS with a clear message and exit status 2: a
# skip is never a pass.
#
# Run from project root:  Rscript tests/test_arranque_app_chromote.R
# ARR_RAIZ=<checkout> runs it against that checkout's app.R (ebfbf5c must fail).
# ARR_MODOS / ARR_USUARIOS narrow it ("normal", "finance"...).
# =============================================================================
cat("=== test_arranque_app_chromote ===\n\n")
if (file.exists(".Renviron.local")) readRenviron(".Renviron.local")
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

LIMITE <- as.numeric(Sys.getenv("ARR_LIMITE", "180"))   # seconds from signing in to the overlay gone
usuarios <- list(
  finance  = c(user = Sys.getenv("HOPDESK_TEST_USER_FINANCE"),  pass = Sys.getenv("HOPDESK_TEST_PASS_FINANCE")),
  analysis = c(user = Sys.getenv("HOPDESK_TEST_USER_ANALYSIS"), pass = Sys.getenv("HOPDESK_TEST_PASS_ANALYSIS")))
faltan <- names(usuarios)[!vapply(usuarios, function(u) all(nzchar(u)), logical(1))]
if (length(faltan)) {
  cat("SKIP: faltan las credenciales de prueba de:", paste(faltan, collapse = ", "), "\n",
      "     (HOPDESK_TEST_USER_<FINANCE|ANALYSIS> / HOPDESK_TEST_PASS_<...> en .Renviron.local).\n",
      "     Esta prueba NO paso: no se corrio.\n")
  quit(status = 2L)
}
modos <- strsplit(Sys.getenv("ARR_MODOS", "normal,vacio"), ",")[[1]]
quienes <- strsplit(Sys.getenv("ARR_USUARIOS", "finance,analysis"), ",")[[1]]

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
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
carga_terminada <- function() isTRUE(tryCatch(ev(
  "(function(){ var o = document.getElementById('hopdesk-loading');
     return !o || o.style.display === 'none' || o.classList.contains('hld-fadeout'); })()"), error = function(e) FALSE))
pct <- function() tryCatch(ev("(document.getElementById('hld-pct')||{}).textContent || ''"), error = function(e) "?")
mostrar <- function(lineas) if (length(lineas)) cat(paste0("      | ", utils::head(lineas, 8)), sep = "\n")

PESTANAS <- c(CAL = "Calendario", VEN = "Vencidos", PH = "Agenda de Hoy", BNC = "Bancos", IC = "Intercompany",
              PSV = "Pasivos", FC = "Pronósticos", RPT = "Reporte")
tiempos <- list()

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

  terminada <- poll_until(carga_terminada, timeout_s = LIMITE, interval_s = 1)
  t_carga <- as.numeric(Sys.time() - t_entrada, units = "secs")
  tiempos[[et]] <- c(proceso = round(t_proceso), carga = round(t_carga))
  ok(sprintf("%s the loading screen reaches 100%% and goes (%.0f s; it stood at %s)", et, t_carga, pct()), terminada)
  ok(paste(et, "the session is still connected"), conectado())
  e <- errores(); ok(paste(et, "no 'Warning: Error in' / 'Execution halted' in the R output"), !length(e)); mostrar(e)

  for (v in names(PESTANAS)) {
    visible <- isTRUE(tryCatch(ev(sprintf("(function(){ var a = document.querySelector(\".navbar .nav-link[data-value='%s']\");
      return !!a && a.offsetParent !== null; })()", v)), error = function(x) FALSE))
    if (!visible) { cat(sprintf("   (pestaña %s no visible para %s)\n", PESTANAS[[v]], quien)); next }
    n <- length(registro())
    ev(sprintf("document.querySelector(\".navbar .nav-link[data-value='%s']\").click(); true", v))
    Sys.sleep(4)
    e <- errores(n)
    ok(sprintf("%s %s opens with no new error and the session alive", et, PESTANAS[[v]]), conectado() && !length(e)); mostrar(e)
  }
  if (isTRUE(tryCatch(ev("!!document.getElementById('btn_settings')"), error = function(x) FALSE))) {
    n <- length(registro())
    ev("document.getElementById('btn_settings').click(); true"); Sys.sleep(4)
    e <- errores(n)
    ok(paste(et, "Ajustes opens with no new error and the session alive"), conectado() && !length(e)); mostrar(e)
    ev("$('.modal').modal('hide'); true")
  }
  escrituras <- grep("\\[ARRANQUE\\] escritura omitida", registro(), value = TRUE)
  cat(sprintf("   (%d escritura(s) que la app intentó y la prueba no dejó llegar a S3)\n", length(escrituras)))
}

cat("\nTiempos (s): proceso = arranque de R hasta servir; carga = de 'Entrar' a la pantalla fuera\n")
for (k in names(tiempos)) cat(sprintf("  %-22s proceso %4.0f   carga %4.0f\n", k, tiempos[[k]][["proceso"]], tiempos[[k]][["carga"]]))
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
