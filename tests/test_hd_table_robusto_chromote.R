# =============================================================================
# tests/test_hd_table_robusto_chromote.R
# Incident 2026-11-03: the Proveedores catalog's datos() failed on a NULL
# source at startup, the error killed the whole Shiny session, and HopDesk
# hung on "Cargando… 14%" for everyone. The engine now catches it: the
# session goes on, the table says "No se pudieron cargar los datos." and
# log_action keeps the failure. In a real headless Chrome against
# tests/helpers/motor_robusto_fixture_app.R.
#
# Run from project root:  Rscript tests/test_hd_table_robusto_chromote.R
# =============================================================================
cat("=== test_hd_table_robusto_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 39431L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
app <- tryCatch(start_bg_app(sprintf('setwd("%s"); source("tests/helpers/motor_robusto_fixture_app.R")$value', repo),
                             PORT, timeout_s = 90),
                error = function(e) { message(conditionMessage(e)); NULL })
ok("fixture app started", !is.null(app))
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
espera <- function(cond, timeout_s = 20)
  poll_until(function() isTRUE(tryCatch(cond(), error = function(e) FALSE)), timeout_s = timeout_s, interval_s = 0.2)
texto <- function(id) ev(sprintf("(document.getElementById('%s') || {}).innerText || ''", id))
vis <- function(t) unlist(ev(sprintf("(HDT.estado('%s') || {}).visibles", t)))
AVISO <- "No se pudieron cargar los datos."

invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE))
ok("the healthy table paints its rows", espera(function() identical(vis("buena-hdt"), c("1", "2")), timeout_s = 60))
ok("the session is alive: no 'disconnected' overlay",
   !isTRUE(ev("document.documentElement.classList.contains('shiny-disconnected') || !!document.getElementById('shiny-disconnected-overlay')")))
ok("a table whose datos() fails says so where 'Cargando…' was",
   espera(function() grepl(AVISO, texto("mala-hdt"), fixed = TRUE)))
invisible(ev("document.getElementById('mas').click(); 0"))
invisible(ev("document.getElementById('mas').click(); 0"))
ok("the rest of the app still answers (the counter goes to 2)", espera(function() identical(texto("contador_caja"), "2")))
ok("log_action keeps each failure, with the table's name and the error",
   espera(function() grepl("hd_table datos_fallido La tabla 'revision_excel_prov' no pudo cargar sus datos", texto("registro"), fixed = TRUE)) &&
   grepl("todavía no", texto("registro"), fixed = TRUE))
ok("a failing aviso(): the rows still show, and the failure shows above them",
   espera(function() identical(vis("aviso-hdt"), c("1", "2"))) &&
   espera(function() grepl(AVISO, ev("(document.querySelector('#aviso-hdt .hdt-nota') || {}).textContent || ''"), fixed = TRUE)))
ok("req() keeps working as before: a NULL source stays on 'Cargando…', with no failure",
   grepl("Cargando", texto("espera-hdt"), fixed = TRUE) && !grepl(AVISO, texto("espera-hdt"), fixed = TRUE))
ok("a table that fails, then gets good data: it paints, and the notice goes",
   grepl(AVISO, texto("vuelve-hdt"), fixed = TRUE) &&
   { invisible(ev("document.getElementById('arreglar').click(); 0"))
     espera(function() identical(vis("vuelve-hdt"), c("1", "2"))) } &&
   !grepl(AVISO, texto("vuelve-hdt"), fixed = TRUE))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
