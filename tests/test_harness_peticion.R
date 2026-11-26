# =============================================================================
# tests/test_harness_peticion.R
#
# T108 (2026-11-25): start_bg_app() (tests/helpers/live_app_harness.R) gave
# each probe of "/" 2 s. On a loaded machine a real app's first GET / takes
# > 10 s, so every probe gave up and the app "never became reachable" while
# it was up. peticion_s (default 30) is how long a probe may take.
#   1. an app whose page takes 4 s: with peticion_s = 2 it is never seen
#      (the old behavior, kept reachable by passing it);
#   2. the same app with the default: reachable.
# A tiny app of its own (no S3, no HopDesk). Run from project root.
# =============================================================================
cat("=== test_harness_peticion ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("tests/helpers/live_app_harness.R")
LENTA <- 'shiny::shinyApp(ui = function(req) { Sys.sleep(4); shiny::tags$p("lista") }, server = function(input, output, session) {})'

r1 <- tryCatch(start_bg_app(LENTA, 39181L, timeout_s = 14, www = NULL, peticion_s = 2), error = function(e) e)
ok("1 a page that takes 4 s, probed with 2 s: never seen as reachable (the old 2 s)",
   inherits(r1, "error") && grepl("never became reachable", conditionMessage(r1)))
if (!inherits(r1, "error")) stop_bg_app(r1)

r2 <- tryCatch(start_bg_app(LENTA, 39182L, timeout_s = 60, www = NULL), error = function(e) e)
ok(sprintf("2 the same app with the default peticion_s (30 s): reachable%s",
           if (inherits(r2, "error")) paste0(" -- ", conditionMessage(r2)) else ""),
   !inherits(r2, "error"))
if (!inherits(r2, "error")) stop_bg_app(r2)

ok("3 the default is 30 s, and the 71 callers keep their signature (peticion_s goes last)", {
  f <- formals(start_bg_app)
  identical(f$peticion_s, 30) && identical(names(f), c("app_code", "port", "timeout_s", "www", "peticion_s"))
})

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
