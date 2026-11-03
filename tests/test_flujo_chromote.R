# =============================================================================
# tests/test_flujo_chromote.R
# The Flujo de caja preview (T11), in a real headless Chrome, against
# tests/helpers/flujo_fixture_app.R: the real module and aggregation over
# fixed rows (no S3). Every click and key is a real input event. Blocks follow
# the T11 inventory (hopdesk_coord/tablas/reportes/T11_inventario.md, seen in
# passing): D Día 0 (5).
#
# Run from project root:  Rscript tests/test_flujo_chromote.R
# FLU_RAIZ=<older checkout> runs the same test against that checkout's code.
# =============================================================================
cat("=== test_flujo_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38999L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
app <- tryCatch(start_bg_app(sprintf('Sys.setenv(FLU_RAIZ = "%s"); setwd("%s"); source("tests/helpers/flujo_fixture_app.R")$value',
                                     Sys.getenv("FLU_RAIZ", repo), repo), PORT, timeout_s = 90),
                error = function(e) { message(conditionMessage(e)); NULL })
ok("fixture app started", !is.null(app))
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
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
  Sys.sleep(0.5)
}
celda   <- "document.querySelector('.cf-day0-cell')"
entrada <- "document.getElementById('cf-day0-input')"
dialogo_abierto <- function() isTRUE(ev("document.getElementById('cf-day0-dlg').style.display === 'block'"))
valor_entrada <- function() ev(sprintf("%s.value", entrada))
valor_celda <- function() ev(sprintf("%s.getAttribute('data-valor')", celda))
texto_celda <- function() trimws(ev(sprintf("%s.textContent", celda)))
# Open Día 0, type an expression with real keys, press Aplicar.
dia0 <- function(expr) {
  clic(celda); Sys.sleep(0.3)
  ev(sprintf("var i=%s; i.select(); 0", entrada))
  b$Input$insertText(text = expr); Sys.sleep(0.2)
  clic("document.getElementById('cf-day0-ok')"); Sys.sleep(1.5)
}

abrir <- function() {
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE))
  poll_until(function() isTRUE(ev("!!window.Shiny && !!document.getElementById('abrir_flujo')")), timeout_s = 60)
  Sys.sleep(1)
  clic("document.getElementById('abrir_flujo')")
}
abrir()
ok("the preview paints this week's grid",
   poll_until(function() isTRUE(ev(sprintf("!!%s", celda))), timeout_s = 60))
invisible(ev("window.__alertas = 0; window.alert = function(){ window.__alertas++; }; 0"))

# ── D · Día 0 ───────────────────────────────────────────────────────────────
dia0("1500*2-300")
ok("D: 1500*2-300 is applied (2,700, shown compact)",
   identical(valor_celda(), "2700") && grepl("$3K", texto_celda(), fixed = TRUE))
clic(celda); Sys.sleep(0.3)
ok("D: reopening reads the stored value back (it read '$3K' and marked an error)",
   dialogo_abierto() && identical(valor_entrada(), "2700"))
clic("document.getElementById('cf-day0-cancel')")
dia0("-(500 + 250)")
ok("D: a negative keeps its sign", identical(valor_celda(), "-750") && grepl("−$750", texto_celda(), fixed = TRUE))
clic(celda); Sys.sleep(0.3)
ok("D: and reopens as -750 (stripping the minus made it 750)", identical(valor_entrada(), "-750"))
clic("document.getElementById('cf-day0-cancel')")
dia0("alert(1)")
ok("D: alert(1) is refused -- nothing runs, the box turns red, the dialog stays",
   ev("window.__alertas") == 0 && dialogo_abierto() &&
   identical(ev(sprintf("%s.style.borderColor", entrada)), "rgb(220, 53, 69)"))
ok("D: and the stored value doesn't change", identical(valor_celda(), "-750"))
clic("document.getElementById('cf-day0-cancel')")
ok("D: the evaluator takes $ and thousands commas as number formatting",
   identical(ev("window.cfEvalArit('$1,500.50 + 499.5')"), 2000L) || identical(ev("window.cfEvalArit('$1,500.50 + 499.5')"), 2000))
ok("D: and refuses anything that isn't + - * / ( ) and numbers",
   all(vapply(c("2**3", "1e3", "Math.PI", "1;2", "(1+2", "", "[]+1", "this"),
              function(x) is.null(ev(sprintf("window.cfEvalArit(%s)", deparse(x)))), logical(1))))
ok("D: precedence and parentheses", identical(as.numeric(ev("window.cfEvalArit('(1+2)*3 - 8/4')")), 7))

# ── H · day letters in the headers (seen in passing 7) ──────────────────────
letras <- unlist(ev("Array.from(document.querySelectorAll('.cf-table thead th.cf-th-bkt')).map(function(t){ return t.textContent.charAt(0); })"))
ok("H: this week's headers read L M X J V S D (Wednesday was a second M)",
   identical(letras, c("L", "M", "X", "J", "V", "S", "D")))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
