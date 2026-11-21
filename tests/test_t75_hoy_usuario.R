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

# ── 2. The screens ─────────────────────────────────────────────────────────
# The real R/ loaded as the app does. The clock is fixed in globalenv: the
# app's functions (sourced there) find these stubs before base's -- Sys.Date
# for the old code (the contraprueba), Sys.time for hoy_usuario()'s default.
source("tests/helpers/aislar_global.R")
Sys.unsetenv("HOPDESK_PRODUCTION")
suppressWarnings(suppressMessages(shiny::loadSupport(".", renv = globalenv(), globalrenv = NULL)))
stopifnot("global.R se cargó con red hacia AWS" = aislar_global_sin_red())
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())
reloj <- function(t) {
  assign("Sys.time", function() t, envir = globalenv())
  assign("Sys.Date", function() as.Date(format(t, "%Y-%m-%d", tz = "UTC")), envir = globalenv())
}
# The starting date of a dateInput, from its HTML.
inicial <- function(tag, id) {
  # The id is on the wrapping div; the date on the <input> inside it.
  h <- as.character(tag)
  m <- regmatches(h, regexpr(sprintf('(?s)<div id="%s"[^>]*>.*?data-initial-date="[0-9-]+"', id), h, perl = TRUE))
  if (!length(m)) return(NA)
  as.Date(sub('(?s).*data-initial-date="([0-9-]+)".*', "\\1", m, perl = TRUE))
}
en_mexico <- function(expr) shiny::withReactiveDomain(sesion("America/Mexico_City"), expr)

reloj(AHORA)
ok("2 Pasivos wizard, Recurrencia: Fecha de inicio starts at the 20th",
   identical(inicial(en_mexico(pasivos_step_ui_recurrencia_regular(list())), "pwiz_fecha_inicio"), EL_20))
ok("2 Pasivos wizard, Términos: Fecha de inicio starts at the 20th",
   identical(inicial(en_mexico(pasivos_step_ui_terminos(list(categoria = "credito"))), "pwiz_fecha_inicio"), EL_20))
ok("2 Pasivos wizard, every N days: Fecha ancla starts at the 20th",
   identical(inicial(en_mexico(pasivos_periodicidad_subfields_ui("interval_days", list())), "pwiz_rp_anchor"), EL_20))
ok("2 Pasivos wizard, a cargo: its Fecha starts at the 20th",
   identical(inicial(en_mexico(pasivos_cargo_row_ui(1L)), "pwiz_cargo_fecha_1"), EL_20))
ok("2 Reporte (UI built with no session): Hasta starts at Mexico's 20th, not the server's 21st",
   identical(inicial(reporteUI("rep"), "rep-fecha_hasta"), EL_20))
ok("2 Proveedores (UI built with no session): Excel desde starts at the 20th",
   identical(inicial(proveedoresUI("prov"), "prov-excel_desde"), EL_20))

# A Sunday night in Mexico is Monday in UTC: the week is the user's.
reloj(as.POSIXct("2026-11-23 01:30:00", tz = "UTC"))   # Sunday 22nd, 19:30 in Mexico
ok("2 Flujo: the default week, on Sunday night in Mexico, is the one that started Monday the 16th (not the 23rd)",
   identical(en_mexico(.cf_default_from()), as.Date("2026-11-16")))
# The last evening of a month in Mexico is the 1st in UTC: the month is the user's.
reloj(as.POSIXct("2026-12-01 01:30:00", tz = "UTC"))   # November 30th, 19:30 in Mexico
ok("2 Reporte: Desde, on November 30th at 19:30 in Mexico, is November 1st (not December 1st)",
   identical(floor_date_month(), as.Date("2026-11-01")))

# Where the field is drawn inside a server (modals), the code itself.
src <- function(f, l) trimws(readLines(f, warn = FALSE, encoding = "UTF-8")[l])
sitios <- list(c("R/bancos_module.R", "man_fecha"), c("R/manual_entry_handlers.R", "me_fecha"),
               c("R/pasivos_table_module.R", "ppm_fecha"), c("R/staging_browse_module.R", "today <-"),
               c("R/pasivos_module.R", "pcm_fecha"), c("R/cashflow_export_module.R", "export_date_from"))
for (s in sitios) {
  x <- readLines(s[1], warn = FALSE, encoding = "UTF-8")
  i <- grep(s[2], x, fixed = TRUE)
  ok(sprintf("2 %s %s: starts at hoy_usuario(), not Sys.Date()", s[1], s[2]),
     length(i) >= 1L && any(vapply(i, function(k) any(grepl("hoy_usuario()", x[k:min(k + 4L, length(x))], fixed = TRUE)), TRUE)) &&
       !any(vapply(i, function(k) any(grepl("Sys.Date()", x[k:min(k + 4L, length(x))], fixed = TRUE)), TRUE)))
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
