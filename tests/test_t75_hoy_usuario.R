# =============================================================================
# tests/test_t75_hoy_usuario.R
# T75 (CM27): a date field starts at the USER's today, not the server's.
# Prod runs in UTC; from 18:00 to 23:59 in Mexico Sys.Date() is already
# tomorrow, and the fields proposed tomorrow's date every evening.
#   1  hoy_usuario() / zona_navegador() (R/zona.R): the browser's zone
#      (input$client_tz), Mexico (ZONA_OMISION) with no session, no zone yet
#      or a zone that does not exist -- never the server's day.
#   2  the screens: each migrated field's starting value, at 19:30 of the
#      20th in Mexico (01:30 of the 21st in UTC), is the 20th.
# The process runs in UTC like prod; Sys.time() is fixed by a stub.
# Run from project root:  Rscript tests/test_t75_hoy_usuario.R
# Contraprueba: on 29d829e, part 1 has no functions and part 2 gives the 21st.
# =============================================================================
Sys.setenv(TZ = "UTC")
cat("=== test_t75_hoy_usuario ===\n\n")
suppressPackageStartupMessages({ library(shiny) })
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
source("R/zona.R")

AHORA  <- as.POSIXct("2026-11-21 01:30:00", tz = "UTC")   # 19:30 of the 20th in Mexico
EL_20  <- as.Date("2026-11-20"); EL_21 <- as.Date("2026-11-21")
sesion <- function(tz = NULL) {
  s <- shiny::MockShinySession$new()
  if (!is.null(tz)) s$setInputs(client_tz = tz)
  s
}

# ── 1. The functions ───────────────────────────────────────────────────────
ok("1 Mexico's browser at 19:30 of the 20th: the 20th (the server's day is already the 21st)",
   identical(hoy_usuario(sesion("America/Mexico_City"), AHORA), EL_20) && identical(as.Date(format(AHORA, "%Y-%m-%d")), EL_21))
ok("1 a browser in Madrid at that instant: the 21st", identical(hoy_usuario(sesion("Europe/Madrid"), AHORA), EL_21))
ok("1 no session (tests, the email job): Mexico's day, not the server's", identical(hoy_usuario(NULL, AHORA), EL_20))
ok("1 a session whose zone has not arrived yet: Mexico's day", identical(hoy_usuario(sesion(), AHORA), EL_20))
ok("1 a zone that does not exist: Mexico's day", identical(hoy_usuario(sesion("Marte/Olympus"), AHORA), EL_20))
ok("1 zona_navegador gives the browser's zone, or ZONA_OMISION",
   identical(zona_navegador(sesion("Europe/Madrid")), "Europe/Madrid") && identical(zona_navegador(NULL), ZONA_OMISION))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
