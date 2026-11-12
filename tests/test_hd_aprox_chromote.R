# =============================================================================
# tests/test_hd_aprox_chromote.R
# The engine's ".aprox" companion of accion_celda (banxico-fix), in Chrome,
# on tests/helpers/aprox_fixture_app.R (the real hd_table, no S3):
#   1. a cell with a mark AND "≈": the mark first, then "≈", then the amount
#      (the coordinator's question);
#   2. "≈" alone; the mark alone as before; neither: nothing added;
#   3. the help is the span's title, as TEXT: HTML in it is not rendered.
#
# Run from project root:  Rscript tests/test_hd_aprox_chromote.R
# =============================================================================
cat("=== test_hd_aprox_chromote ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38987L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
app <- tryCatch(start_bg_app(sprintf('setwd("%s"); source("tests/helpers/aprox_fixture_app.R")$value', repo), PORT, timeout_s = 90),
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
A <- "t-hdt"
invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE))
ok("la tabla carga en el motor",
   poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s') && HDT.estado('%s').visibles.length == 5", A, A))),
              timeout_s = 60))
Sys.sleep(0.5)
# The money cell of a row: its child nodes, as list(clase, texto, titulo).
celda <- function(id) ev(sprintf("(function(){ var tr = HDT.fila('%s', '%s'); var td = tr.querySelectorAll('td');
  td = Array.from(td).filter(function(x){ return /\\d/.test(x.textContent) && x.querySelector !== undefined; }).pop();
  return { hijos: Array.from(td.childNodes).map(function(n){ return n.nodeType === 1 ? (n.className + '|' + n.textContent + '|' + (n.title || '')) : ('#texto|' + n.textContent); }),
           html: td.innerHTML }; })()", A, id))
AYUDA <- "Aproximado: convertido con el FIX del 12/11/2026 (18.0000)."
ca <- celda("a"); h <- unlist(ca$hijos)
ok("marca y ≈ a la vez: primero la marca, después ≈, después el monto",
   length(h) >= 3L && grepl("hdt-celda-marca", h[1]) && grepl("^hdt-celda-aprox\\|≈\\|", h[2]) && grepl("1,000", paste(h[-(1:2)], collapse = "")))
ok("la ayuda del ≈ es su title", grepl(AYUDA, h[2], fixed = TRUE))
hb <- unlist(celda("b")$hijos)
ok("solo ≈: el ≈ va primero, con su ayuda", grepl("^hdt-celda-aprox\\|≈\\|", hb[1]) && grepl(AYUDA, hb[1], fixed = TRUE))
hc <- unlist(celda("c")$hijos)
ok("solo la marca: como antes, sin ≈", grepl("hdt-celda-marca", hc[1]) && !any(grepl("hdt-celda-aprox", hc)))
hd <- unlist(celda("d")$hijos)
ok("ninguna: la celda no lleva marca ni ≈", !any(grepl("hdt-celda-(aprox|marca)", hd)))
ce <- celda("e")
ok("una ayuda con HTML se queda como texto: no se pinta <b> ni <img>",
   !grepl("<b>", ce$html, fixed = TRUE) && !grepl("<img", ce$html, fixed = TRUE) &&
     grepl("<img src=x", unlist(ce$hijos)[1], fixed = TRUE))
ok("el CSS del ≈ cargó (hdtable.css v30)",
   isTRUE(ev("getComputedStyle(document.querySelector('.hdt-celda-aprox')).cursor === 'help'")))

# ── 4. Pasivos' real table (tests/helpers/pasivos_tabla_fixture_app.R) ──────
T <- "pt-tabla-hdt"
pasivos <- function(fx) {
  stop_bg_app(app)
  app <<- tryCatch(start_bg_app(sprintf('Sys.setenv(PASIVOS_FX = "%s"); setwd("%s"); source("tests/helpers/pasivos_tabla_fixture_app.R")$value',
                                        if (fx) "1" else "0", repo), PORT, timeout_s = 90),
                   error = function(e) { message(conditionMessage(e)); NULL })
  if (is.null(app)) return(FALSE)
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE))
  r <- poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s') && HDT.estado('%s').visibles.length > 0", T, T))),
                  timeout_s = 60)
  Sys.sleep(0.8); r
}
aproxs <- function() ev(sprintf("Array.from(document.querySelectorAll('#%s .hdt-celda-aprox')).map(function(s){
  return s.title + '|' + s.parentNode.textContent; })", T))
ok("Pasivos con FIX: la tabla carga", pasivos(TRUE))
ax <- unlist(aproxs())
ok("Pasivos con FIX: la provisión cotizada en USD 100 se ve ≈ 1,800 con su ayuda (FIX 18)",
   length(ax) == 1L && grepl("18.0000", ax[1], fixed = TRUE) && grepl("1,800", ax[1], fixed = TRUE))
ok("Pasivos sin FIX: la tabla carga", pasivos(FALSE))
ok("Pasivos sin FIX: ninguna celda lleva ≈ (se ve como hoy)", length(unlist(aproxs())) == 0L)

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
