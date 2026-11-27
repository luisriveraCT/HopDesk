# =============================================================================
# tests/test_t116_f3_adaptadores.R
# T116 F3: the ECB and New York Fed adapters (R/forecasting_source_ecb.R,
# R/forecasting_source_nyfed.R) against a fake server on 127.0.0.1
# (tests/helpers/mercado_falso.R, invented figures). Never the internet.
#   1  ECB: a series; what it asked (path and query); a cross, only on the
#      dates both series have; €STR
#   2  New York Fed: SOFR and EFFR; what it asked
#   3  failing closed: HTTP 500, an empty answer, a malformed one, "NaN",
#      an unknown series -- no rows, never an error
#   4  the base URL: overridable ONLY towards 127.0.0.1
#   5  the dispatcher: SOFR with FRED down falls back to the New York Fed
#      (fetch_params carries both sources' keys); EFFR is the New York Fed's
#   6  Yahoo retired: a metric that still names it never reaches an adapter
#
# Run from project root:  Rscript tests/test_t116_f3_adaptadores.R
# =============================================================================
cat("=== test_t116_f3_adaptadores ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr, detalle = NULL) {
  r <- tryCatch(isTRUE(expr), error = function(e) { message("  error: ", conditionMessage(e)); FALSE })
  if (r) { .pass <<- .pass + 1L; cat(" PASS:", label, "\n") }
  else   { .fail <<- .fail + 1L; cat(" FAIL:", label, "\n"); if (!is.null(detalle)) cat("       ", detalle, "\n") }
}
suppressPackageStartupMessages({ library(httr); library(jsonlite) })
source("tests/helpers/s3_memoria.R")
source("tests/helpers/mercado_falso.R")
for (f in c("R/forecasting_schemas.R", "R/forecasting_persistence.R", "R/forecasting_metric_registry.R",
            "R/forecasting_sources.R", "R/forecasting_source_banxico.R", "R/forecasting_source_fred.R",
            "R/forecasting_source_ecb.R", "R/forecasting_source_nyfed.R"))
  suppressMessages(sys.source(f, envir = globalenv()))
q <- function(expr) suppressWarnings(suppressMessages(expr))
D <- function(...) as.Date(c(...))
FECHAS <- c("2027-11-24", "2027-11-25", "2027-11-26")
CONF <- list(
  ecb   = list(modo = "ok", fechas = FECHAS,
               valores = list(USD = 1.1, MXN = 21.5893, GBP = 0.8, JPY = 174.79, CAD = 1.5617,
                              "B.EU000A2X2A25.WT" = 2.445)),
  nyfed = list(modo = "ok", fechas = FECHAS, valores = list(sofr = 3.94, effr = 3.88)))
f <- mercado_falso_iniciar(CONF)
on.exit(f$parar(), add = TRUE)
Sys.setenv(HD_ECB_URL = f$url, HD_NYFED_URL = f$url)
semilla <- .seed_forecasting_metrics()
metrica <- function(id) as.list(semilla[semilla$metric_id == id, ][1, ])
DESDE <- D("2027-11-13"); HASTA <- D("2027-11-27")

cat("== 1. BCE ==\n")
f$log_borrar()
r <- fcs_fetch_ecb(metrica("eur_usd"), DESDE, HASTA)
ok("1.1 EUR/USD: tres días, dólares por euro, fuente ecb, oficial",
   identical(r$status, "ok") && nrow(r$rows) == 3L && all(r$rows$value == 1.1) &&
     identical(r$rows$fecha, D(FECHAS)) && all(r$rows$source_id == "ecb") && all(r$rows$observation_type == "official"))
l <- f$log()
ok("1.2 pidió /service/data/EXR/D.USD.EUR.SP00.A con startPeriod, endPeriod y format=csvdata",
   nrow(l) == 1L && identical(l$ruta, "/service/data/EXR/D.USD.EUR.SP00.A") &&
     grepl("startPeriod=2027-11-13", l$consulta) && grepl("endPeriod=2027-11-27", l$consulta) &&
     grepl("format=csvdata", l$consulta), paste(l$ruta, l$consulta))
c1 <- CONF; c1$ecb$fechas_de <- list(GBP = FECHAS[2:3]); f$config(c1)
r <- fcs_fetch_ecb(metrica("gbp_usd"), DESDE, HASTA)
ok("1.3 GBP/USD cruzado: USD/EUR ÷ GBP/EUR = 1.375, solo en los dos días que ambas tienen, marcado «cruzado»",
   identical(r$status, "ok") && identical(r$rows$fecha, D(FECHAS[2:3])) && all(abs(r$rows$value - 1.375) < 1e-12) &&
     all(r$rows$notes == "cruzado"))
c2 <- CONF; c2$ecb$fechas_de <- list(GBP = "2027-11-01"); f$config(c2)
ok("1.4 cruce sin días en común: sin filas (nunca un día dividido entre otro)",
   identical(fcs_fetch_ecb(metrica("gbp_usd"), DESDE, HASTA)$status, "no_data"))
f$config(CONF)
r <- fcs_fetch_ecb(metrica("estr"), DESDE, HASTA)
ok("1.5 €STR: flujo EST, su llave", identical(r$status, "ok") && all(r$rows$value == 2.445) &&
     any(f$log()$ruta == "/service/data/EST/B.EU000A2X2A25.WT"))

cat("== 2. Fed de Nueva York ==\n")
f$log_borrar()
r <- fcs_fetch_nyfed(metrica("sofr"), DESDE, HASTA)
ok("2.1 SOFR: tres días, en orden, fuente nyfed", identical(r$status, "ok") && nrow(r$rows) == 3L &&
     all(r$rows$value == 3.94) && identical(r$rows$fecha, D(FECHAS)) && all(r$rows$source_id == "nyfed"))
l <- f$log()
ok("2.2 pidió /api/rates/secured/sofr/search.json con startDate y endDate",
   identical(l$ruta, "/api/rates/secured/sofr/search.json") && grepl("startDate=2027-11-13", l$consulta) &&
     grepl("endDate=2027-11-27", l$consulta), paste(l$ruta, l$consulta))
r <- fcs_fetch_nyfed(metrica("fed_funds"), DESDE, HASTA)
ok("2.3 EFFR: unsecured/effr", identical(r$status, "ok") && all(r$rows$value == 3.88) &&
     any(f$log()$ruta == "/api/rates/unsecured/effr/search.json"))

cat("== 3. Fallan cerrado ==\n")
modo <- function(fuente, m) { c <- CONF; c[[fuente]]$modo <- m; f$config(c) }
sin_filas <- function(r, st) identical(r$status, st) && is.null(r$rows)
modo("ecb", "falla500");   ok("3.1 BCE HTTP 500: http_error, sin filas", sin_filas(fcs_fetch_ecb(metrica("eur_usd"), DESDE, HASTA), "http_error"))
modo("ecb", "vacio");      ok("3.2 BCE sin observaciones: no_data", sin_filas(fcs_fetch_ecb(metrica("eur_usd"), DESDE, HASTA), "no_data"))
modo("ecb", "malformado"); ok("3.3 BCE con HTML en vez de CSV: parse_error", sin_filas(fcs_fetch_ecb(metrica("eur_usd"), DESDE, HASTA), "parse_error"))
modo("ecb", "nan");        ok("3.4 BCE con «NaN» (día sin dato): no_data, nunca un NaN guardado", sin_filas(fcs_fetch_ecb(metrica("eur_usd"), DESDE, HASTA), "no_data"))
f$config(CONF)
m <- metrica("eur_usd"); m$fetch_params <- list(list(flujo = "EXR", llave = "D.XYZ.EUR.SP00.A"))
ok("3.5 BCE, serie que no existe (404): http_error", sin_filas(fcs_fetch_ecb(m, DESDE, HASTA), "http_error"))
m$fetch_params <- list(list(serie = "SF43718"))
ok("3.6 BCE sin flujo/llave: parse_error, sin pedir nada", { f$log_borrar(); sin_filas(fcs_fetch_ecb(m, DESDE, HASTA), "parse_error") && !nrow(f$log()) })
modo("nyfed", "falla500");   ok("3.7 NY Fed HTTP 500: http_error", sin_filas(fcs_fetch_nyfed(metrica("sofr"), DESDE, HASTA), "http_error"))
modo("nyfed", "vacio");      ok("3.8 NY Fed refRates vacío: no_data", sin_filas(fcs_fetch_nyfed(metrica("sofr"), DESDE, HASTA), "no_data"))
modo("nyfed", "malformado"); ok("3.9 NY Fed JSON roto: parse_error", sin_filas(fcs_fetch_nyfed(metrica("sofr"), DESDE, HASTA), "parse_error"))
f$config(CONF)
m <- metrica("sofr"); m$fetch_params <- list(list(tipo = "../x", tasa = "sofr"))
ok("3.10 NY Fed con un tipo que no es una palabra: parse_error, sin pedir nada",
   { f$log_borrar(); sin_filas(fcs_fetch_nyfed(m, DESDE, HASTA), "parse_error") && !nrow(f$log()) })

cat("== 4. La URL base ==\n")
viejo <- Sys.getenv(c("HD_ECB_URL", "HD_NYFED_URL"))
Sys.setenv(HD_ECB_URL = "http://otro.example.com"); ok("4.1 a otro servidor: no, la oficial", identical(.fcs_url_local("HD_ECB_URL", FCS_ECB_URL), FCS_ECB_URL))
Sys.setenv(HD_ECB_URL = "https://127.0.0.1.example.com"); ok("4.2 un nombre que empieza con 127.0.0.1: no", identical(.fcs_url_local("HD_ECB_URL", FCS_ECB_URL), FCS_ECB_URL))
Sys.setenv(HD_ECB_URL = ""); ok("4.3 sin variable: la oficial", identical(.fcs_url_local("HD_ECB_URL", FCS_ECB_URL), "https://data-api.ecb.europa.eu"))
ok("4.4 NY Fed oficial", identical(FCS_NYFED_URL, "https://markets.newyorkfed.org"))
Sys.setenv(HD_ECB_URL = f$url); ok("4.5 el servidor falso de esta prueba: sí", identical(.fcs_url_local("HD_ECB_URL", FCS_ECB_URL), f$url))
Sys.setenv(HD_ECB_URL = viejo[[1]], HD_NYFED_URL = viejo[[2]])

cat("== 5. El despachador ==\n")
llaves <- Sys.getenv(c("FRED_API_KEY", "BANXICO_TOKEN"))
Sys.setenv(FRED_API_KEY = "", BANXICO_TOKEN = "")   # FRED and Banxico down: no network for them either
.fcs_sources_cache <- .seed_forecasting_sources()
.fcs_metrics_cache <- list(.default = semilla)
r <- q(fcs_fetch_metric("sofr", DESDE, HASTA))
ok("5.1 SOFR con FRED caído: viene de la Fed de NY (su respaldo), con las llaves de las dos en fetch_params",
   nrow(r) == 3L && all(r$source_id == "nyfed") && all(r$value == 3.94))
r <- q(fcs_fetch_metric("fed_funds", DESDE, HASTA))
ok("5.2 EFFR: de la Fed de NY", nrow(r) == 3L && all(r$source_id == "nyfed"))
r <- q(fcs_fetch_metric("eur_usd", DESDE, HASTA))
ok("5.3 EUR/USD: del BCE (ya no de Yahoo)", nrow(r) == 3L && all(r$source_id == "ecb"))

cat("== 6. Yahoo retirada ==\n")
llamadas <- 0L
assign("fcs_fetch_yahoo", function(...) { llamadas <<- llamadas + 1L; list(status = "ok", rows = NULL) }, envir = globalenv())
vieja <- semilla[semilla$metric_id == "eur_mxn", ]
vieja$primary_source_id <- "yahoo"; vieja$fetch_params <- list(list(ticker = "EURMXN=X"))
.fcs_metrics_cache <- list(.default = vieja)
r <- q(fcs_fetch_metric("eur_mxn", DESDE, HASTA))
ok("6.1 una métrica que aún nombra a yahoo: sin filas y sin llamar a su adaptador (fuente inactiva)",
   !nrow(r) && llamadas == 0L)
ok("6.2 R/forecasting_source_yahoo.R ya no existe", !file.exists("R/forecasting_source_yahoo.R"))
Sys.setenv(FRED_API_KEY = llaves[[1]], BANXICO_TOKEN = llaves[[2]])

f$parar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
