# =============================================================================
# tests/test_t75b_month_sel.R
# T75b (CM27): the Calendario's month (input$month_sel) starts at, and falls
# back to, the USER's day. Sys.Date() is the server's (UTC in prod): from
# 18:00 on the last day of a month in Mexico it was already next month.
#   1  every `input$month_sel %||% <x>` in app.R and R/ has x = hoy_usuario()
#      (Calendario's AR/AP months and the Notas handlers), read from the
#      code itself -- app.R has no harness;
#   2  the month picker (airMonthpickerInput("month_sel")) starts at
#      hoy_usuario();
#   3  what that gives: on November 30th at 19:30 in Mexico (December 1st in
#      UTC) the month is November.
# Run from project root:  Rscript tests/test_t75b_month_sel.R
# Contraprueba: on 38c0a5b (before this change) 1 and 2 fail.
# =============================================================================
Sys.setenv(TZ = "UTC")
cat("=== test_t75b_month_sel ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
# Every call in a file's code, walked (an empty argument is skipped).
llamadas <- function(f) {
  out <- list()
  andar <- function(e) {
    if (!is.call(e)) return(invisible())
    out[[length(out) + 1L]] <<- e
    for (k in seq_along(e)[-1]) { a <- e[[k]]; if (!missing(a) && is.call(a)) andar(a) }
  }
  for (e in parse(f, keep.source = FALSE, encoding = "UTF-8")) andar(e)
  out
}
es <- function(e, x) identical(e, x)
archivos <- c("app.R", list.files("R", "\\.R$", recursive = TRUE, full.names = TRUE))
todas <- unlist(lapply(archivos, llamadas), recursive = FALSE)

# 1. The fallbacks
resp <- Filter(function(e) identical(e[[1]], as.name("%||%")) && es(e[[2]], quote(input$month_sel)), todas)
ok(sprintf("1 every input$month_sel %%||%% <x> falls back to hoy_usuario() (%d found)", length(resp)),
   length(resp) >= 5L && all(vapply(resp, function(e) es(e[[3]], quote(hoy_usuario())), TRUE)))

# 2. The picker
pk <- Filter(function(e) {
  f <- e[[1]]
  (identical(f, quote(shinyWidgets::airMonthpickerInput)) || identical(f, as.name("airMonthpickerInput"))) &&
    length(e) >= 2L && identical(e[[2]], "month_sel")
}, todas)
ok("2 the month picker starts at hoy_usuario()",
   length(pk) == 1L && es(pk[[1]]$value, quote(hoy_usuario())))

# 3. What it gives
source("R/zona.R")
fin_mes <- as.POSIXct("2026-12-01 01:30:00", tz = "UTC")   # November 30th, 19:30 in Mexico
ok("3 on November 30th at 19:30 in Mexico the month is November (the server's day is December 1st)",
   identical(format(hoy_usuario(NULL, fin_mes), "%Y-%m"), "2026-11") && identical(format(fin_mes, "%Y-%m"), "2026-12"))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
