# =============================================================================
# tests/test_hd_table_arranque_null.R
# Incident 2026-11-03: before S3 loads, a shared source is NULL; the
# Proveedores catalog's datos() failed on it and (before the engine caught
# it) closed the whole session. Every migrated table must start on a NULL
# source without its datos() failing -- the engine's catch is a net, not the
# plan. Each table's fixture app runs with HD_ARRANQUE_NULL=1 (its sources
# NULL, as at startup) under shiny::testServer, in its own R process (the
# fixtures replace globals such as log_action). It fails if the session
# errors or if any hd_table reports "the datos failed" / "the aviso failed".
#
# Run from project root:  Rscript tests/test_hd_table_arranque_null.R
# =============================================================================
cat("=== test_hd_table_arranque_null ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

repo <- normalizePath(".", winslash = "/")
FIXTURES <- c(
  "Proveedores: catálogo, Inactivos y revisión"        = "proveedores",
  "Empresas"                                           = "empresas",
  "Pronósticos: métodos, suscripciones y consultas"    = "pronosticos",
  "Registros: Actividad, Bitácora Global, Archivos"    = "registros",
  "Agenda de Hoy: Pagos y Cobros"                      = "agenda",
  "Detalle del día: Resumen y Detalle"                 = "dia",
  "Vencidos y la lupa"                                 = "vencidos",
  "Bancos: Libro, Historial, Papelera, Cuentas"        = "bancos"
)

arrancar <- function(fixture) callr::r(function(repo, fixture) {
  setwd(repo)
  Sys.setenv(HD_ARRANQUE_NULL = "1")
  avisos <- character(0)
  error <- tryCatch(withCallingHandlers({
    app <- suppressWarnings(source(sprintf("tests/helpers/%s_fixture_app.R", fixture))$value)
    shiny::testServer(app, { session$flushReact(); session$flushReact() })
    NULL
  }, message = function(m) {
    avisos <<- c(avisos, conditionMessage(m))
    invokeRestart("muffleMessage")
  }), error = function(e) conditionMessage(e))
  list(error = error, fallas = grep("the (datos|aviso) failed", avisos, value = TRUE))
}, args = list(repo = repo, fixture = fixture), timeout = 180)   # a hang fails, it never blocks the run

for (k in seq_along(FIXTURES)) {
  r <- tryCatch(arrancar(FIXTURES[[k]]), error = function(e) list(error = conditionMessage(e), fallas = character(0)))
  if (!is.null(r$error)) message("  ", FIXTURES[[k]], ": ", r$error)
  for (f in r$fallas) message("  ", FIXTURES[[k]], ": ", trimws(f))
  ok(sprintf("%s: starts on NULL sources, the session goes on and no table's data fails", names(FIXTURES)[k]),
     is.null(r$error) && !length(r$fallas))
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
