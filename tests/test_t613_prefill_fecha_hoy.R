# =============================================================================
# tests/test_t613_prefill_fecha_hoy.R
# T6-13: in R/ledger_module.R, the date an invoice's edit form starts at
# (prefill_fecha) falls back, when the row has no usable due date, to TODAY.
# That today was Sys.Date(), the server's day: on a UTC server from 18:00 to
# 23:59 in Mexico, tomorrow. It is now hoy_usuario() (CM27, R/zona.R), as the
# calendar (T6-12).
# The expression is taken from the module's own source (parsed, not copied)
# and evaluated:
#   1. a row with usable dates: the projected date, else the original;
#   2. a row whose dates cannot be read, the clock at 23:30 in Mexico on the
#      10th (05:30 UTC on the 11th): the 10th, the user's day;
#   3. no Sys.Date() in that expression.
# Run from project root:  Rscript tests/test_t613_prefill_fecha_hoy.R
# =============================================================================
cat("=== test_t613_prefill_fecha_hoy ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# The right-hand side of `prefill_fecha <- ...`, wherever it sits in the module.
buscar <- function(e) {
  if (is.call(e)) {
    if (identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("prefill_fecha"))) return(list(e[[3]]))
    return(unlist(lapply(as.list(e)[-1], buscar), recursive = FALSE))
  }
  NULL
}
hallados <- unlist(lapply(parse("R/ledger_module.R", keep.source = FALSE, encoding = "UTF-8"), buscar), recursive = FALSE)
ok("R/ledger_module.R has exactly one prefill_fecha expression", length(hallados) == 1L)
prefill <- hallados[[1]]

source("R/zona.R")
NOCHE <- as.POSIXct("2026-11-11 05:30:00", tz = "UTC")   # 23:30 on the 10th in Mexico City
hoy_usuario <- function(session = NULL, ahora = NOCHE) hoy_en(ZONA_OMISION, ahora)
evaluar <- function(row) eval(prefill, list(row = row), globalenv())

ok("1: a row with a projected date starts there",
   identical(evaluar(list(FechaVenc_Proyectada = "2026-12-05", FechaVenc_Original = "2026-12-01")), as.Date("2026-12-05")))
ok("1: no projected date: the original one",
   identical(evaluar(list(FechaVenc_Proyectada = NA, FechaVenc_Original = "2026-12-01")), as.Date("2026-12-01")))
# Neither date can be read (as.Date of an unparseable string is an error).
ok("2: dates that cannot be read, at 23:30 in Mexico on the 10th: the 10th (the server's UTC day would be the 11th)",
   identical(evaluar(list(FechaVenc_Proyectada = "no-es-fecha", FechaVenc_Original = "tampoco")), as.Date("2026-11-10")) &&
     as.Date(format(NOCHE, "%Y-%m-%d", tz = "UTC")) == as.Date("2026-11-11"))
ok("3: the expression calls hoy_usuario() and no Sys.Date()",
   any(grepl("hoy_usuario()", deparse(prefill), fixed = TRUE)) && !any(grepl("Sys.Date", deparse(prefill), fixed = TRUE)))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
quit(status = if (.fail) 1L else 0L)
