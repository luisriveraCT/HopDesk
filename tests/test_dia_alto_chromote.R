# =============================================================================
# tests/test_dia_alto_chromote.R
# T05, Luis's request: the day's tables "un poco más amplia para ver más
# cosas a la vez". Both are alto = "ventana", like the lupa (T04b). In a real
# headless Chrome, against tests/helpers/dia_fixture_app.R:
#   1. The fixed day (9 invoices): the 8-row minimum, nothing stretched, the
#      modal inside the window.
#   2. 60 more invoices: Resumen takes the whole modal down to the window's
#      bottom (it used to stop at ~15 rows), its ✕ still shows, and the
#      page itself never scrolls. Since T09 the modal is an hd_modal(): no
#      "Cerrar" footer (✕ in the header) and a 1.5vh margin, read from the
#      dialog rather than assumed.
#   3. Detalle, painted while hidden, fills it too once shown.
#   4. A shorter window: the table follows it.
#
# Run from project root:  Rscript tests/test_dia_alto_chromote.R
# =============================================================================
cat("=== test_dia_alto_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

ok("R: dia_ui() gives both tables alto = \"ventana\"", local({
  src <- paste(readLines("R/dia_module.R", warn = FALSE), collapse = "\n")
  grepl('hd_table_ui(ns(v), alto = "ventana")', src, fixed = TRUE)
}))

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
R <- "ap-resumen-hdt"; D <- "ap-detalle-hdt"
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)

ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
tamano <- function(alto) invisible(b$Emulation$setDeviceMetricsOverride(width = 1400L, height = alto, deviceScaleFactor = 1, mobile = FALSE))
# Start the fixture with `extra` more invoices and open the day.
arrancar <- function(extra, puerto) {
  try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app)
  app <<- tryCatch(start_bg_app(sprintf('Sys.setenv(DIA_EXTRA = "%d"); setwd("%s"); source("tests/helpers/dia_fixture_app.R")$value',
                                        extra, repo), puerto, timeout_s = 90),
                   error = function(e) { message(conditionMessage(e)); NULL })
  if (is.null(app)) return(FALSE)
  b <<- new_chromote_session()
  tamano(1000L)
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", puerto), wait_ = TRUE))
  invisible(poll_until(function() isTRUE(ev("!!window.HDT && !!window.Shiny && !!Shiny.setInputValue")), timeout_s = 60))
  Sys.sleep(1)
  invisible(ev("Shiny.setInputValue('abrir', Math.random(), {priority: 'event'})"))
  r <- poll_until(function() isTRUE(ev(sprintf("!!document.querySelector('.modal.show') && !!HDT.estado('%s') && HDT.estado('%s').visibles.length > 0", R, R))),
                  timeout_s = 20)
  Sys.sleep(1.2); r
}
# The shown table's rows area and the modal, in px.
medir <- function(t) ev(sprintf("(function(){ var s = document.querySelector('#%s .hdt-scroll'), c = document.querySelector('.modal.show .modal-content'),
    x = document.querySelector('.modal.show .hd-modal-cerrar'), d = c.closest('.modal-dialog');
  return { ventana: innerHeight, scroll: s.getBoundingClientRect().height, abajo: c.getBoundingClientRect().bottom,
           margen: parseFloat(getComputedStyle(d).marginTop),
           pie: x.getBoundingClientRect().bottom, x_arriba: x.getBoundingClientRect().top, filas: HDT.estado('%s').visibles.length,
           pagina: document.documentElement.scrollHeight > innerHeight + 1 && getComputedStyle(document.body).overflow !== 'hidden'
             && !document.body.classList.contains('modal-open') }; })()", t, t))
llena <- function(m) abs(m$abajo - (m$ventana - m$margen)) <= 2

# ── 1. The fixed day: nothing stretched ─────────────────────────────────────
ok("fixture app started (the fixed day)", arrancar(0L, 38983L))
m <- medir(R)
cat(sprintf("  9 invoices: rows area %.0f px, modal ends at %.0f (window %d)\n", m$scroll, m$abajo, m$ventana))
ok("few rows: the rows area is the 8-row minimum (not stretched to the window)", m$scroll < 300)
ok("few rows: the modal ends inside the window, ✕ shown", m$abajo <= m$ventana && m$pie <= m$ventana && m$x_arriba >= 0)

# ── 2. 60 more invoices: Resumen fills the modal ────────────────────────────
ok("fixture app started (60 more invoices)", arrancar(60L, 38984L))
m <- medir(R)
cat(sprintf("  Resumen: %d rows, rows area %.0f px, modal ends at %.0f (window %d)\n", m$filas, m$scroll, m$abajo, m$ventana))
ok("Resumen: more than 30 rows (beyond the old ~15-row stop)", m$filas > 30)
ok("Resumen: taller than the old 15-row stop (~375 px)", m$scroll > 500)
ok("Resumen: the modal reaches the window's bottom less its margin (+-2 px)", llena(m))
ok("Resumen: ✕ shows and the page doesn't scroll", m$pie <= m$ventana && m$x_arriba >= 0 && !isTRUE(m$pagina))

# ── 3. Detalle, painted while hidden, fills it too ──────────────────────────
invisible(ev("document.querySelector('.hd-dia-cambio button[data-vista=detalle]').click()"))
Sys.sleep(1.2)
m <- medir(D)
cat(sprintf("  Detalle: %d rows, rows area %.0f px, modal ends at %.0f\n", m$filas, m$scroll, m$abajo))
ok("Detalle: shown after being hidden, it fills the modal (+-2 px)", m$scroll > 500 && llena(m))

# ── 4. A shorter window ─────────────────────────────────────────────────────
tamano(760L)
Sys.sleep(1.2)
m2 <- medir(D)
cat(sprintf("  window 760: rows area %.0f px, modal ends at %.0f\n", m2$scroll, m2$abajo))
ok("a shorter window: the table shrinks with it and the modal still fits (+-2 px)", m2$scroll < m$scroll && llena(m2))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
