# =============================================================================
# tests/test_bancos_t12b_chromote.R
# Bancos' other tables on hd_table (T12b), in a real headless Chrome,
# against tests/helpers/bancos_fixture_app.R (the real bancosServer on the
# CM09's in-memory S3).
#   C. Cuentas (cuentas_bancos, D): the active accounts, read only, Excel.
#
# Run from project root:  Rscript tests/test_bancos_t12b_chromote.R
# =============================================================================
cat("=== test_bancos_t12b_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
estado <- function(t) ev(sprintf("HDT.estado('%s')", t))
vis <- function(t) unlist(estado(t)$visibles)
centro <- function(js_el) ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', behavior:'instant'});
  var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
raton <- function(tipo, p, botones = 0L) b$Input$dispatchMouseEvent(type = tipo, x = p[[1]], y = p[[2]],
  button = if (tipo == "mouseMoved" && !botones) "none" else "left", buttons = botones, clickCount = 1L)
clic <- function(js_el) { p <- centro(js_el); raton("mouseMoved", p); raton("mousePressed", p, 1L); raton("mouseReleased", p); Sys.sleep(0.4) }
titulos <- function(t) unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.dataset.col != null; }).map(function(th){ return th.firstChild.textContent; })", t)))
pestana <- function(v) { clic(sprintf("document.querySelector('a[data-value=\"%s\"]')", v)); Sys.sleep(1) }
sub_pestana <- function(t) { clic(sprintf("Array.from(document.querySelectorAll('.nav-link')).filter(function(a){ return a.textContent.trim() === '%s'; })[0]", t)); Sys.sleep(1) }

app <- tryCatch(start_bg_app(sprintf('setwd("%s"); source("tests/helpers/bancos_fixture_app.R")$value', repo), 38981L,
                             timeout_s = 240), error = function(e) { message(conditionMessage(e)); NULL })
ok("fixture app started", !is.null(app))
b <- new_chromote_session()
invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
invisible(b$Page$navigate("http://127.0.0.1:38981", wait_ = TRUE))
invisible(poll_until(function() isTRUE(ev("!!window.HDT && !!HDT.estado('bnc-libro-hdt')")), timeout_s = 90))
Sys.sleep(1)

# ══ C · Cuentas ══════════════════════════════════════════════════════════════
C <- "bnc-cuentas-hdt"
pestana("cuentas")
ok("C1: the 3 active accounts, by company, bank and currency",
   poll_until(function() length(tryCatch(vis(C), error = function(e) NULL)) == 3L, timeout_s = 15))
ok("C2: columns Empresa, Banco, Moneda, Alias, Cuenta, CLABE",
   identical(titulos(C), c("Empresa", "Banco", "Moneda", "Alias", "Cuenta", "CLABE")))
ok("C3: Exportar a Excel is there (Luis, T12 P7)",
   isTRUE(ev(sprintf("Array.from(document.querySelectorAll('#%s button')).some(function(x){ return /Exportar a Excel/.test(x.textContent); })", C))))
ok("C4: read only: no row buttons", isTRUE(ev(sprintf("!document.querySelector('#%s button[data-accion]')", C))))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) {
  cat("\n-- app stderr (tail) --\n"); try(cat(tail(readLines(app$proc$get_error_file(), warn = FALSE), 25), sep = "\n"))
  quit(status = 1L)
}
