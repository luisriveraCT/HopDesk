# =============================================================================
# tests/test_t33_pasivos_chromote.R
# T33 (monedas fase 3, Pasivos) in a real headless Chrome, on Luis's window
# (1293 x 632): the fixture tests/helpers/pasivos_tabla_fixture_app.R (the
# real module, fake shared, no S3) with PASIVOS_BASE unset (not chosen).
#   1. A client in MXN sees what it saw: sections MXN, USD, EUR, CAD; the
#      "next 7 days" badges MXN blue, USD green.
#   2. The base changes to USD live (as Ajustes would): the sections and the
#      rows put USD first, without reloading; the USD badge turns blue.
#   3. "+ Provisión manual" starts on USD and offers the whole catalog.
# Screenshots to PT_CAPTURAS if set.
# Run from project root:  Rscript tests/test_t33_pasivos_chromote.R
# =============================================================================
cat("=== test_t33_pasivos_chromote ===\n\n")

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
CAPTURAS <- Sys.getenv("PT_CAPTURAS", "")
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
Sys.unsetenv("PASIVOS_BASE")
app <- tryCatch(start_bg_app(sprintf('setwd("%s"); source("tests/helpers/pasivos_tabla_fixture_app.R")$value', repo),
                             PORT, timeout_s = 90), error = function(e) { message(conditionMessage(e)); NULL })
ok("fixture app started", !is.null(app))
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
invisible(b$Emulation$setDeviceMetricsOverride(width = 1293, height = 632, deviceScaleFactor = 1, mobile = FALSE))
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
T <- "pt-tabla-hdt"
b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE)
ok("the table loads", poll_until(function()
  isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s') && HDT.estado('%s').visibles.length > 0", T, T))), timeout_s = 60))
Sys.sleep(0.6)
bandas  <- function() grep("^banda", unlist(ev(sprintf("HDT.estado('%s').lista", T))), value = TRUE)
filas   <- function() unlist(ev(sprintf("HDT.estado('%s').visibles", T)))
# The summary's badges: "MXN|rgb(37, 99, 235)" (code, background).
# [CAMBIA T55b] the badge reads "3,680.00 MXN · 4 próx.": the code is the
# three letters before the "·", no longer the first three characters.
insignias <- function() unlist(ev("Array.from(document.querySelectorAll('#pt-summary_line .badge')).map(function(e){
  var m = e.textContent.match(/([A-Z]{3}) \\u00b7/);
  return (m ? m[1] : e.textContent.trim().slice(0, 3)) + '|' + getComputedStyle(e).backgroundColor; })"))
AZUL <- "rgb(37, 99, 235)"; VERDE <- "rgb(22, 163, 74)"; MORADO <- "rgb(124, 58, 237)"

# ── 1. Base not chosen: as before T33 ──
ok("1: sections MXN, USD, EUR, CAD, rows in today's order",
   identical(bandas(), c("banda:1:MXN", "banda:1:USD", "banda:1:EUR", "banda:1:CAD")) &&
     identical(filas(), c("orphan_p0", "lib1", "lib2", "lib2_fee_1", "lib2_fee_2", "lib3", "lib'4")))
i1 <- insignias()
ok(sprintf("1: badges MXN blue, USD green, as before (%s)", paste(i1, collapse = ", ")),
   identical(i1[startsWith(i1, "MXN")], paste0("MXN|", AZUL)) && identical(i1[startsWith(i1, "USD")], paste0("USD|", VERDE)) &&
     startsWith(i1[1], "MXN"))
if (nzchar(CAPTURAS)) { dir.create(CAPTURAS, showWarnings = FALSE, recursive = TRUE)
  b$screenshot(file.path(CAPTURAS, "t33_base_mxn.png"), show = FALSE) }

# ── 2. The base changes to USD, live ──
ev("Shiny.setInputValue('t33_base', 'USD')")
ok("2: sections put USD first, without reloading", poll_until(function()
  identical(bandas()[1], "banda:1:USD"), timeout_s = 20))
ok("2: ...and the rows: the USD liability and its fees on top", identical(filas()[1:3], c("lib2", "lib2_fee_1", "lib2_fee_2")))
ok("2: the USD badge goes first and blue; MXN is no longer blue", poll_until(function() {
  i <- insignias(); length(i) && identical(i[1], paste0("USD|", AZUL)) && !any(i == paste0("MXN|", AZUL)) }, timeout_s = 10))
if (nzchar(CAPTURAS)) b$screenshot(file.path(CAPTURAS, "t33_base_usd.png"), show = FALSE)

# ── 3. + Provisión manual starts on the base and offers the catalog ──
ev("document.getElementById('pt-btn_add_provision_manual').click()")
ok("3: the manual provision's currency starts on USD", poll_until(function()
  identical(ev("(function(){ var s = document.getElementById('pt-ppm_moneda'); return s ? s.value : null; })()"), "USD"),
  timeout_s = 15))
n_ops <- ev("(function(){ var s = document.getElementById('pt-ppm_moneda'); if (!s) return 0;
  return s.selectize ? Object.keys(s.selectize.options).length : s.options.length; })()")
ok(sprintf("3: ...and offers the whole catalog (%s currencies)", n_ops), isTRUE(n_ops > 100))
if (nzchar(CAPTURAS)) b$screenshot(file.path(CAPTURAS, "t33_modal_usd.png"), show = FALSE)

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
