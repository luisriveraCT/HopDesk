# =============================================================================
# tests/test_t46_panorama_semana.R
# T46 (Luis, 2026-11-17: "nublado solo cuando se pasa de la semana"; moneda en
# todas las cifras). One rule for "SAP data is stale" (sap_viejo(), calendar
# days in the client's zone, SAP_DIAS_VIEJO = 7) and one wording for amounts
# and for SAP's date (monto_moneda(), sap_al_texto() in R/ui_components.R),
# shared by Panorama and the weekly emails.
#
# Run from project root:  Rscript tests/test_t46_panorama_semana.R
# =============================================================================
cat("=== test_t46_panorama_semana ===\n\n")
`%||%` <- function(a, b) if (is.null(a)) b else a

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

source("R/i18n.R")
source("R/monedas.R")
source(Sys.getenv("T46_PANORAMA_DATOS_R", "R/panorama_datos.R"))
# Only the two formatters from R/ui_components.R (the rest is Shiny UI).
for (e in parse("R/ui_components.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c("monto_moneda", "sap_al_texto"))
    eval(e, globalenv())

MX <- "America/Mexico_City"
mx <- function(s) as.POSIXct(s, tz = MX)
MENOS <- "−"

cat("-- monto_moneda --\n")
ok("842300 MXN -> \"842,300.00 MXN\"", identical(monto_moneda(842300, "MXN"), "842,300.00 MXN"))
ok("negative: true minus sign before the number",
   identical(monto_moneda(-10950296, "MXN"), paste0(MENOS, "10,950,296.00 MXN")))
ok("a negative that rounds to zero carries no sign", identical(monto_moneda(-0.004, "USD"), "0.00 USD"))
ok("the catalog's minor units: JPY 0, KWD 3",
   identical(monto_moneda(1234.4, "JPY"), "1,234 JPY") && identical(monto_moneda(1.5, "KWD"), "1.500 KWD"))
ok("vectorized over amount and currency",
   identical(monto_moneda(c(1, 2500.5), c("MXN", "USD")), c("1.00 MXN", "2,500.50 USD")))
ok("the code is normalized (\" usd \" -> USD)", identical(monto_moneda(5, " usd "), "5.00 USD"))
ok("a code outside the catalog keeps 2 decimals and is shown as is", identical(monto_moneda(5, "ZZZ"), "5.00 ZZZ"))
ok("NA amount is 0", identical(monto_moneda(NA, "EUR"), "0.00 EUR"))
ok("no currency: the number alone", identical(monto_moneda(5, NA), "5.00"))
ok("never a \"$\"", !any(grepl("$", monto_moneda(c(1, -2, 3), c("MXN", "USD", "EUR")), fixed = TRUE)))
ok("empty in, empty out", identical(monto_moneda(numeric(), character()), character()))

cat("\n-- sap_viejo: calendar days in the client's zone --\n")
AHORA <- mx("2026-11-17 14:00")     # a Tuesday
ok("SAP_DIAS_VIEJO is 7", identical(SAP_DIAS_VIEJO, 7L))
ok("today's data: clear", !sap_viejo(mx("2026-11-17 08:54"), AHORA, MX))
ok("day 7 (Tuesday 10, early): clear", !sap_viejo(mx("2026-11-10 00:01"), AHORA, MX))
ok("day 8 (Monday 9, late): stale", sap_viejo(mx("2026-11-09 23:59"), AHORA, MX))
ok("refreshed Monday 08:54: the next Monday is clear all day",
   !sap_viejo(mx("2026-11-09 08:54"), mx("2026-11-16 23:59"), MX) &&
     sap_viejo(mx("2026-11-09 08:54"), mx("2026-11-17 00:01"), MX))
ok("the day is the client's: 05:00 UTC on the 10th is the 9th in Mexico City",
   sap_viejo(as.POSIXct("2026-11-10 05:00", tz = "UTC"), AHORA, MX) &&
     !sap_viejo(as.POSIXct("2026-11-10 05:00", tz = "UTC"), AHORA, "UTC"))
ok("no date: never trusted", sap_viejo(NULL, AHORA, MX) && sap_viejo(as.POSIXct(NA), AHORA, MX))

cat("\n-- sap_al_texto --\n")
ok("es, this week: \"Datos de SAP al martes 17, 08:54\"",
   identical(sap_al_texto(mx("2026-11-17 08:54"), MX, "es", AHORA), "Datos de SAP al martes 17, 08:54"))
ok("en, this week: \"SAP data as of Tuesday 17, 08:54\"",
   identical(sap_al_texto(mx("2026-11-17 08:54"), MX, "en", AHORA), "SAP data as of Tuesday 17, 08:54"))
ok("day 7 still without the month",
   identical(sap_al_texto(mx("2026-11-10 08:54"), MX, "es", AHORA), "Datos de SAP al martes 10, 08:54"))
ok("older: with the month (es)",
   identical(sap_al_texto(mx("2026-11-03 08:54"), MX, "es", AHORA), "Datos de SAP al martes 3 de nov, 08:54"))
ok("older: with the month (en)",
   identical(sap_al_texto(mx("2026-11-03 08:54"), MX, "en", AHORA), "SAP data as of Tuesday, Nov 3, 08:54"))
ok("a UTC time is shown in the client's zone",
   identical(sap_al_texto(as.POSIXct("2026-11-17 14:54", tz = "UTC"), MX, "es", AHORA),
             "Datos de SAP al martes 17, 08:54"))
ok("no date: the \"not loaded\" line",
   identical(sap_al_texto(NULL, MX, "es", AHORA), "Datos de SAP todavía sin cargar") &&
     identical(sap_al_texto(as.POSIXct(NA), MX, "en", AHORA), "SAP data not loaded yet"))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
