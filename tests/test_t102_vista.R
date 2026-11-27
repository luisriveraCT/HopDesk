# =============================================================================
# tests/test_t102_vista.R
# T102: with options(hdt.esperar_vista = TRUE) -- set by app.R -- a table's
# data go only while its container can be seen (the browser's hdt_vista,
# {v: 1|0, n: the container}). On the real hd_table_server (testServer),
# counting every read of datos():
#   1  hidden: 0 reads of datos(), 0 hdt-datos
#   2  seen: 1 message with the current data (and its words, T118), 1 read
#   3  the same report again, or datos() invalidated with identical data: no resend
#   4  data change while hidden: still 0 reads; seen again: the LATEST, once
#   5  hidden and seen again unchanged: no resend (the browser keeps it); a
#      new container (another n) gets it
#   6  the M6 gate (T69) comes first: a table the tier may not see never
#      sends, seen or not, and never reads datos()
#   7  without the option: exactly as before (sends at once)
# The browser side (data-vista, ResizeObserver, no repaint from ULTIMO):
# measured in Chrome in the T102 window.
# Run from project root:  Rscript tests/test_t102_vista.R
# HD_TABLE_R = another copy of R/hd_table.R (contraprueba).
# =============================================================================
suppressPackageStartupMessages(library(shiny))
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
source("R/i18n.R"); source("R/hd_registro.R"); source("R/hd_piezas.R"); source("R/monedas.R"); source("R/hd_columnas.R")
source(Sys.getenv("HD_TABLE_R", "R/hd_table.R"))
.i18n_dir <- file.path(getwd(), "www", "i18n")

.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}
cat("=== test_t102_vista ===\n")

cols <- list(col_texto("Parte", "tablas.facturas.parte"), col_dinero("Importe", "tablas.facturas.importe"))
D1 <- data.frame(id = c("a", "b"), Parte = c("UNO", "DOS"), Importe = c(10, 20), stringsAsFactors = FALSE)
D2 <- data.frame(id = c("a", "b", "c"), Parte = c("UNO", "DOS", "TRES"), Importe = c(10, 20, 30), stringsAsFactors = FALSE)

correr <- function(esperar, permitida = NULL, cuerpo) {
  old <- options(hdt.esperar_vista = esperar); on.exit(options(old))
  st <- new.env(); st$mensajes <- list(); st$lecturas <- 0L; st$d <- reactiveVal(D1)
  srv <- function(id) moduleServer(id, function(input, output, session) {
    rs <- session$rootScope()
    unlockBinding("sendCustomMessage", rs)
    rs$sendCustomMessage <- function(type, message) st$mensajes[[length(st$mensajes) + 1L]] <- list(type = type, msg = message)
    if (!is.null(permitida)) session$userData$hd_tabla_permitida <- permitida
    hd_table_server("t", tabla = "detalle_dia_resumen", columnas = cols,
                    datos = reactive({ st$lecturas <- st$lecturas + 1L; st$d() }))
  })
  testServer(srv, { cuerpo(session, st) })
  invisible(st)
}
envios <- function(st) Filter(function(m) identical(m$type, "hdt-datos"), st$mensajes)
vista <- function(session, v, n = 1L) do.call(session$setInputs, setNames(list(list(v = v, n = n)), "t-hdt_vista"))

# ── 1-5 ──────────────────────────────────────────────────────────────────────
correr(TRUE, cuerpo = function(session, st) {
  session$flushReact()
  ok("1 hidden (no report yet): 0 reads of datos(), 0 hdt-datos", st$lecturas == 0L && !length(envios(st)))
  vista(session, 0L); session$flushReact()
  ok("1 reported hidden: still 0 reads, 0 messages", st$lecturas == 0L && !length(envios(st)))

  vista(session, 1L); session$flushReact()
  e <- envios(st)
  ok("2 seen: exactly 1 message, with the current data (2 rows)",
     length(e) == 1L && length(unlist(e[[1]]$msg$filas$ids)) == nrow(D1))
  ok("2 ...its words come with it (T118: i18n in the same message)", "i18n" %in% names(e[[1]]$msg))
  ok("2 ...and datos() was read once", st$lecturas == 1L)

  vista(session, 1L); session$flushReact()
  st$d(D1[, , drop = FALSE]); session$flushReact()           # invalidated, identical value
  ok("3 the same report again, or identical data: no resend", length(envios(st)) == 1L)

  vista(session, 0L); session$flushReact()
  lect <- st$lecturas
  st$d(D2); session$flushReact()
  ok("4 data change while hidden: no read, no message", st$lecturas == lect && length(envios(st)) == 1L)
  vista(session, 1L); session$flushReact()
  e <- envios(st)
  ok("4 seen again: the LATEST data (3 rows), sent once", length(e) == 2L && length(unlist(e[[2]]$msg$filas$ids)) == 3L)
  session$flushReact()
  ok("4 ...and only once", length(envios(st)) == 2L)

  vista(session, 0L); session$flushReact(); vista(session, 1L); session$flushReact()
  ok("5 hidden and seen again unchanged: no resend (the browser keeps what it has)", length(envios(st)) == 2L)
  vista(session, 1L, n = 2L); session$flushReact()
  e <- envios(st)
  ok("5 a new container (another n), unchanged: gets the data", length(e) == 3L && length(unlist(e[[3]]$msg$filas$ids)) == 3L)
  st$d(D1); session$flushReact()
  ok("5 a real change while seen: one message", length(envios(st)) == 4L)
})

# ── 6 · The M6 gate first ────────────────────────────────────────────────────
correr(TRUE, permitida = function(tabla) FALSE, cuerpo = function(session, st) {
  vista(session, 1L); session$flushReact()
  st$d(D2); session$flushReact()
  ok("6 gate closed and seen: never a message, never a read of datos()", !length(envios(st)) && st$lecturas == 0L)
})

# ── 7 · Without the option: as before ────────────────────────────────────────
correr(FALSE, cuerpo = function(session, st) {
  session$flushReact()
  ok("7 without the option: sends at once, no report needed (as before)", length(envios(st)) == 1L && st$lecturas == 1L)
})

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
