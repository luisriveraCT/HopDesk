# =============================================================================
# tests/test_hd_table_t09_chromote.R
# Two engine changes of T09, in a real headless Chrome against
# tests/helpers/motor_t09_fixture_app.R:
#   1. fichas$inicial: the table opens on its chip ("b"); after that the chip
#      is the user's (a data change doesn't bring it back).
#   2. The table inside a renderUI, with hdtable.js arriving there for the
#      first time (the session already running): it paints at once, without
#      waiting for the data to change (T10's finding: it stayed "Cargando…").
#      Rendered again, it paints again.
#
# Run from project root:  Rscript tests/test_hd_table_t09_chromote.R
# =============================================================================
cat("=== test_hd_table_t09_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
T <- "t-hdt"
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
espera <- function(cond, timeout_s = 15)
  poll_until(function() isTRUE(tryCatch(cond(), error = function(e) FALSE)), timeout_s = timeout_s, interval_s = 0.15)
vis <- function() unlist(ev(sprintf("HDT.estado('%s').visibles", T)))
activa <- function() ev(sprintf("(document.querySelector('#%s .hdt-ficha.hdt-on')||{}).textContent || ''", T))
pulsar <- function(id) invisible(ev(sprintf("document.getElementById('%s').click(); 0", id)))
arrancar <- function(modo, puerto) {
  try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app)
  app <<- tryCatch(start_bg_app(sprintf('Sys.setenv(T09_MODO = "%s"); setwd("%s"); source("tests/helpers/motor_t09_fixture_app.R")$value',
                                        modo, repo), puerto, timeout_s = 90),
                   error = function(e) { message(conditionMessage(e)); NULL })
  if (is.null(app)) return(FALSE)
  b <<- new_chromote_session()
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", puerto), wait_ = TRUE))
  TRUE
}

# ── 1. fichas$inicial ───────────────────────────────────────────────────────
ok("fixture app started (table on the page)", arrancar("directo", 38986L))
ok("inicial: the table opens on the 'b' chip (its 2 rows)",
   espera(function() identical(vis(), c("2", "4")), timeout_s = 60))
ok("inicial: the 'b' chip shows as the active one", espera(function() startsWith(activa(), "b")))
invisible(ev(sprintf("document.querySelector('#%s .hdt-ficha').click(); 0", T)))   # Todas
ok("the user's choice: Todas shows the 5", espera(function() length(vis()) == 5L))
pulsar("mas")
ok("a data change keeps the user's chip (Todas), not the initial one", espera(function() length(vis()) == 6L))

# ── 2. The engine arriving inside a renderUI ────────────────────────────────
ok("fixture app started (table only inside a renderUI)", arrancar("renderui", 38987L))
# The page's own HTML has no hd_table: hdtable.js comes with the renderUI.
ok("the table and its script come only with the renderUI (the case being tested)",
   espera(function() isTRUE(ev("!!document.querySelector('#caja #t-hdt')")), timeout_s = 60) &&
   isFALSE(ev("(function(){ var x = new XMLHttpRequest(); x.open('GET', location.href, false); x.send(); return /hdtable/.test(x.responseText); })()")))
ok("renderUI: the table paints at once, without a data change (opening on 'b')",
   espera(function() identical(vis(), c("2", "4")), timeout_s = 10))
ok("renderUI: no 'Cargando' left in the table",
   !grepl("Cargando", ev(sprintf("document.getElementById('%s').innerText", T))))
pulsar("rehacer")
ok("rendered again (same data): it paints again",
   espera(function() isTRUE(ev(sprintf("!!HDT.estado('%s') && HDT.estado('%s').visibles.length > 0", T, T))), timeout_s = 10))
pulsar("mas")
ok("and follows a data change", espera(function() "6" %in% vis() || length(vis()) == 6L))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
