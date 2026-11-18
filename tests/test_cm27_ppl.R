# =============================================================================
# tests/test_cm27_ppl.R
# CM27-ppl (2026-11-17). Pagar hoy dated the bank payment files (their name
# and generate_ppl()'s date, the payment's application date) and proposed
# the payment/collection date with Sys.Date(): the SERVER's day. On a UTC
# server, from 18:00 to 23:59 in Mexico that is already tomorrow. Now it is
# the client's day: hoy_en(zona) with the module's zona() (ZONA_OMISION,
# Mexico, until the client setting exists).
#   1  hoy_en(): at 23:00 in Mexico on a UTC clock, Mexico's day; at 10:00,
#      the same day either way;
#   2  "Descargar todos" with TZ=UTC at 23:00 in Mexico: every file's name
#      and every generate_ppl() date is Mexico's day, not the next one;
#   3  at 10:00 in Mexico: the same day as before the change (nothing moves);
#   4  pagarHoyServer has no Sys.Date() left; its 12 sites (both downloads,
#      both proposed dates and their fallbacks) take hoy().
# Run from project root:  Rscript tests/test_cm27_ppl.R
# =============================================================================
cat("=== test_cm27_ppl ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
tz_antes <- Sys.getenv("TZ", unset = NA)
Sys.setenv(TZ = "UTC")   # the server's clock, as shinyapps.io most likely runs

invisible(capture.output(source("tests/helpers/agenda_fixture_app.R")))
# 23:00 in Mexico on 2026-11-17 = 05:00 UTC on 2026-11-18; 10:00 = 16:00 UTC.
NOCHE  <- as.POSIXct("2026-11-18 05:00:00", tz = "UTC")
MANANA <- as.POSIXct("2026-11-17 16:00:00", tz = "UTC")

# ── 1. hoy_en() ──────────────────────────────────────────────────────────────
ok("hoy_en: 23:00 in Mexico on a UTC clock is Mexico's 2026-11-17 (Sys.Date() would say 11-18)",
   exists("hoy_en", mode = "function") && identical(hoy_en("America/Mexico_City", NOCHE), as.Date("2026-11-17")) &&
     identical(as.Date(format(NOCHE, "%Y-%m-%d")), as.Date("2026-11-18")))
ok("hoy_en: 10:00 in Mexico is 2026-11-17 both ways",
   exists("hoy_en", mode = "function") && identical(hoy_en("America/Mexico_City", MANANA), as.Date("2026-11-17")) &&
     identical(as.Date(format(MANANA, "%Y-%m-%d")), as.Date("2026-11-17")))
ok("hoy_en: by default, ZONA_OMISION (Mexico)",
   exists("hoy_en", mode = "function") && identical(hoy_en(ahora = NOCHE), as.Date("2026-11-17")))

# ── 2-3. "Descargar todos" at a fixed clock ─────────────────────────────────
fechas_ppl <- character()
.generate_ppl_real <- generate_ppl
generate_ppl <- function(rows, cuenta, fecha, ...) {
  fechas_ppl <<- c(fechas_ppl, format(as.Date(fecha)))
  .generate_ppl_real(rows, cuenta, fecha, ...)
}
catalogo_zip <- dplyr::bind_rows(catalogo, tibble::tibble(
  nombre = "SERVICIOS BAJIO SA", alias = "SERVBAJIO", clabe = "030180003333333333",
  medio_pago = "SPEI", banco = "030", Empresa = NA_character_))
descargar_a_las <- function(ahora) {
  # The fixed clock, and the server's day on it as base::Sys.Date() reads it
  # (TZ=UTC): what the code before CM27 used.
  assign("Sys.time", function() ahora, envir = globalenv())
  assign("Sys.Date", function() as.Date(format(ahora, "%Y-%m-%d")), envir = globalenv())
  on.exit(rm(list = c("Sys.time", "Sys.Date"), envir = globalenv()))
  fechas_ppl <<- character()
  dir <- tempfile(); dir.create(dir)
  shared <- list(
    company_map = reactiveVal(c(NTS = EMP, OTR = OTR)), proveedores_db = reactiveVal(catalogo_zip),
    parte_alias_map_db = reactiveVal(NULL), pagar_hoy_db = reactiveVal(agenda0),
    sap_data = reactiveVal(list(AP = data.frame(Empresa = "NTS", Documento = "4001", Factura = "F-4001",
                                                Parte = "ACEROS MONTERREY SA", check.names = FALSE))),
    sap_ov_db = reactiveVal(NULL), manual_inv = reactiveVal(NULL),
    bancos_confirmados = reactiveVal(tibble::tibble()), abonos_db = reactiveVal(NULL),
    effective_client_id = function() "dev", current_user = function() "luis",
    usuario = reactiveVal(list(user = "luis", permisos = list(can_register_payments = TRUE))), home_client_id = function() "dev")
  invisible(capture.output(testServer(pagarHoyServer, args = list(shared = shared), {
    session$userData$anota <- function(...) invisible(NULL)
    session$flushReact()
    utils::unzip(output$download_all, exdir = dir)
  })))
  list(archivos = sort(list.files(dir)), fechas = fechas_ppl)
}

noche <- tryCatch(descargar_a_las(NOCHE), error = function(e) { message("  ERROR: ", conditionMessage(e)); NULL })
ok(sprintf("23:00 in Mexico, TZ=UTC: the ZIP has payment files (%d)", length(noche$archivos)),
   length(noche$archivos) >= 1L)
ok("23:00 in Mexico: every file is named with Mexico's day, 17112026",
   length(noche$archivos) && all(grepl("_17112026_", noche$archivos, fixed = TRUE)))
ok("23:00 in Mexico: every generate_ppl() date (the payment's date) is 2026-11-17",
   length(noche$fechas) >= 1L && length(noche$fechas) == length(noche$archivos) && all(noche$fechas == "2026-11-17"))

manana <- tryCatch(descargar_a_las(MANANA), error = function(e) { message("  ERROR: ", conditionMessage(e)); NULL })
ok("10:00 in Mexico: names and dates are 2026-11-17, as before the change",
   length(manana$archivos) && all(grepl("_17112026_", manana$archivos, fixed = TRUE)) &&
     length(manana$fechas) && all(manana$fechas == "2026-11-17"))

# ── 4. No server day left in pagarHoyServer ─────────────────────────────────
srv <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                identical(e[[2]], as.name("pagarHoyServer")),
              as.list(parse("R/pagar_hoy_module.R", keep.source = FALSE, encoding = "UTF-8")))
txt <- paste(deparse(srv[[1]], width.cutoff = 500L), collapse = "\n")
ok("pagarHoyServer: no Sys.Date() left", length(srv) == 1L && !grepl("Sys.Date()", txt, fixed = TRUE))
ok("pagarHoyServer: the client's zone and hoy() are defined once, with ZONA_OMISION",
   grepl("zona <- reactive(", txt, fixed = TRUE) && grepl("ZONA_OMISION", txt, fixed = TRUE) &&
     grepl("hoy <- function() hoy_en(isolate(zona()))", txt, fixed = TRUE))
ok("pagarHoyServer: 12 sites take hoy() (2 downloads' names and dates, 2 proposed dates, 2 fallbacks)",
   lengths(regmatches(txt, gregexpr("hoy()", txt, fixed = TRUE))) == 12L + 0L &&
     lengths(regmatches(txt, gregexpr("value = hoy()", txt, fixed = TRUE))) == 2L &&
     lengths(regmatches(txt, gregexpr("error = function(e) hoy()", txt, fixed = TRUE))) == 2L &&
     lengths(regmatches(txt, gregexpr("generate_ppl\\([^)]*hoy\\(\\)", txt))) == 3L)

if (is.na(tz_antes)) Sys.unsetenv("TZ") else Sys.setenv(TZ = tz_antes)
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
