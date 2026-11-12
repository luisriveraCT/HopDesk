# =============================================================================
# tests/test_fx_banxico_r.R
# R/fx_banxico.R, offline: the in-memory S3 (tests/helpers/s3_memoria.R) and
# Banxico's adapter (fcs_fetch_banxico) replaced by a fake that records its
# calls. fx_fix():
#   1. which FIX it expects (cut at 12:30 CDMX, weekends skipped);
#   2. a cache that already has it: no call to Banxico;
#   3. a cache that lacks it: one call, the new days added, sorted, and the
#      cache only grows; at most one call an hour per process;
#   4. fails closed -- no token, an error, an answer without figures, an
#      unreadable cache, a FIX older than 3 business days: NULL, never a
#      made-up figure, nothing written;
#   5. cambio_pct against the previous FIX.
#
# Run from project root:  Rscript tests/test_fx_banxico_r.R
# =============================================================================
cat("=== test_fx_banxico_r ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("tests/helpers/s3_memoria.R")
suppressMessages(source("R/fx_banxico.R"))
KEY <- paste0(.HD_ADMIN_CID, "/", FX_FIX_LLAVE)
cdmx <- function(s) as.POSIXct(s, tz = FX_FIX_TZ)

# Fake Banxico: answers `respuesta` (rows of fecha/value) and counts calls.
llamadas <- 0L; respuesta <- NULL; falla <- FALSE
fcs_fetch_banxico <- function(metric, date_from, date_to) {
  llamadas <<- llamadas + 1L
  if (falla) stop("timeout")
  if (is.null(respuesta)) return(list(status = "no_data", rows = NULL, error_msg = NULL))
  list(status = "ok", rows = respuesta, error_msg = NULL)
}
filas <- function(fechas, valores) data.frame(fecha = as.Date(fechas), value = valores)
cache <- function(fechas, valores) data.frame(fecha = as.Date(fechas), valor = valores, serie = FX_FIX_SERIE,
                                              obtenido_en = "2026-11-01T00:00:00Z", stringsAsFactors = FALSE)
empezar <- function(df = NULL, token = TRUE) {
  s3_memoria_reset(); llamadas <<- 0L; respuesta <<- NULL; falla <<- FALSE
  rm(list = ls(.fx_fix_estado), envir = .fx_fix_estado)
  if (!is.null(df)) assign(KEY, df, envir = .mem)
  if (token) Sys.setenv(BANXICO_TOKEN = "prueba") else Sys.unsetenv("BANXICO_TOKEN")
}

# ── 1. Which FIX is expected ────────────────────────────────────────────────
ok("jueves 13:00 CDMX: el de hoy", identical(fx_fix_esperado(cdmx("2026-11-12 13:00")), as.Date("2026-11-12")))
ok("jueves 11:00 CDMX: el de ayer (aún no se determina)", identical(fx_fix_esperado(cdmx("2026-11-12 11:00")), as.Date("2026-11-11")))
ok("sábado: el del viernes", identical(fx_fix_esperado(cdmx("2026-11-14 15:00")), as.Date("2026-11-13")))
ok("lunes 09:00: el del viernes", identical(fx_fix_esperado(cdmx("2026-11-16 09:00")), as.Date("2026-11-13")))

# ── 2. The cache has it: no call ────────────────────────────────────────────
empezar(cache(c("2026-11-11", "2026-11-12"), c(20.00, 20.10)))
r <- fx_fix(cdmx("2026-11-12 13:00"))
ok("caché con el FIX del día: lo devuelve sin llamar a Banxico",
   llamadas == 0L && identical(r$valor, 20.10) && identical(r$fecha, as.Date("2026-11-12")) && .mem_escrituras == 0L)
ok("cambio_pct contra el FIX anterior", isTRUE(all.equal(r$cambio_pct, (20.10 / 20.00 - 1) * 100)))
ok("la serie y la fuente", identical(r$serie, "SF43718") && identical(r$fuente, "Banxico FIX"))

# ── 3. The cache lacks it: one call, new days added ────────────────────────
empezar(cache("2026-11-10", 19.90))
respuesta <- filas(c("2026-11-10", "2026-11-11", "2026-11-12"), c(19.90, 20.00, 20.10))
r <- fx_fix(cdmx("2026-11-12 13:00"))
c3 <- get(KEY, envir = .mem)
ok("caché sin el día: una llamada y devuelve el nuevo", llamadas == 1L && identical(r$valor, 20.10))
ok("la caché suma los días que faltaban, ordenados, sin repetir el que ya tenía",
   identical(as.Date(c3$fecha), as.Date(c("2026-11-10", "2026-11-11", "2026-11-12"))) && .mem_escrituras == 1L)
ok("la caché solo crece (la fila vieja sigue)", isTRUE(c3$valor[1] == 19.90))
empezar(cache("2026-11-11", 20.00))
respuesta <- NULL                     # Banxico has nothing new yet
r1 <- fx_fix(cdmx("2026-11-12 13:00")); r2 <- fx_fix(cdmx("2026-11-12 13:30"))
ok("sin dato nuevo: muestra el de ayer y no vuelve a llamar antes de una hora",
   llamadas == 1L && identical(r1$valor, 20.00) && identical(r2$valor, 20.00))
r3 <- fx_fix(cdmx("2026-11-12 14:05"))
ok("pasada la hora, vuelve a intentar", llamadas == 2L)

# ── 4. Fails closed ─────────────────────────────────────────────────────────
empezar(NULL); respuesta <- filas("2026-11-12", 20.1)
ok("sin token: NULL, sin llamar ni escribir",
   { empezar(NULL, token = FALSE); is.null(fx_fix(cdmx("2026-11-12 13:00"))) && llamadas == 0L && .mem_escrituras == 0L })
empezar(NULL); falla <- TRUE
ok("Banxico falla y no hay caché: NULL, nada escrito", is.null(fx_fix(cdmx("2026-11-12 13:00"))) && .mem_escrituras == 0L)
empezar(NULL)
ok("Banxico sin datos y sin caché: NULL", is.null(fx_fix(cdmx("2026-11-12 13:00"))))
empezar(NULL); respuesta <- data.frame(fecha = as.Date("2026-11-12"), value = NA_real_)
ok("un valor N/E (NA) no cuenta: NULL", is.null(fx_fix(cdmx("2026-11-12 13:00"))) && .mem_escrituras == 0L)
empezar(cache("2026-11-05", 19.50)); falla <- TRUE
ok("el último FIX tiene más de 3 días hábiles: NULL (no se muestra uno viejo)", is.null(fx_fix(cdmx("2026-11-12 13:00"))))
empezar(cache("2026-11-10", 19.90)); falla <- TRUE
ok("2 días hábiles de viejo: todavía se muestra", identical(fx_fix(cdmx("2026-11-12 13:00"))$valor, 19.90))
empezar(cache("2026-11-12", 20.10)); .mem_falla <- KEY; falla <- TRUE
ok("caché ilegible y Banxico caído: NULL, nada escrito", is.null(fx_fix(cdmx("2026-11-12 13:00"))) && .mem_escrituras == 0L)
.mem_falla <- character()
empezar(cache("2026-11-11", 20.00)); respuesta <- filas("2026-11-12", 20.10); .mem_falla_escritura <- KEY
r <- fx_fix(cdmx("2026-11-12 13:00"))
.mem_falla_escritura <- character()
ok("no se puede guardar la caché: muestra lo que sí está guardado (ayer), no lo que no se guardó",
   identical(r$valor, 20.00))
ok("el código no tiene un tipo de cambio fijo de respaldo",
   !grepl("18\\.5", paste(readLines("R/fx_banxico.R", warn = FALSE), collapse = "\n")))

# ── 6. Remembered 10 minutes in the process, NULL too (the integrator) ─────
# Panorama's Cinta redraws every minute per session: S3 is read once.
lecturas <- 0L
leer_orig <- leer_hd_admin
leer_hd_admin <- function(...) { lecturas <<- lecturas + 1L; leer_orig(...) }
empezar(cache(c("2026-11-11", "2026-11-12"), c(20.00, 20.10)))
r1 <- fx_fix(cdmx("2026-11-12 13:00")); r2 <- fx_fix(cdmx("2026-11-12 13:01"))
ok("dos llamadas seguidas leen S3 una sola vez y dan lo mismo", lecturas == 1L && identical(r1, r2) && identical(r1$valor, 20.10))
fx_fix(cdmx("2026-11-12 13:09"))
ok("dentro de los 10 minutos, ninguna lectura más", lecturas == 1L)
fx_fix(cdmx("2026-11-12 13:10"))
ok("a los 10 minutos, vuelve a leer", lecturas == 2L)
lecturas <- 0L; empezar(NULL, token = FALSE)
n1 <- fx_fix(cdmx("2026-11-12 13:00")); n2 <- fx_fix(cdmx("2026-11-12 13:05"))
ok("el NULL también se recuerda (sin dato: una sola lectura)", is.null(n1) && is.null(n2) && lecturas == 1L)
lecturas <- 0L; empezar(cache("2026-11-11", 20.00)); falla <- TRUE
viejo <- fx_fix(cdmx("2026-11-12 13:00"))
assign(KEY, cache(c("2026-11-11", "2026-11-12"), c(20.00, 20.10)), envir = .mem)   # another process adds today's
ok("lo que otro proceso guarda en esos 10 minutos se ve al vencer la memoria",
   identical(fx_fix(cdmx("2026-11-12 13:05"))$valor, 20.00) && identical(fx_fix(cdmx("2026-11-12 13:11"))$valor, 20.10))
leer_hd_admin <- leer_orig

s3_memoria_restaurar(); Sys.unsetenv("BANXICO_TOKEN")
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
