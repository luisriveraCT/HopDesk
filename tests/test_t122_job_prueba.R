# =============================================================================
# tests/test_t122_job_prueba.R
# T122: the job's test mode (AVISOS_PRUEBA, avisos.yml's dispatch) runs
# against the PROD bucket, so it must write nothing but avisos/prueba/*.html.
# Before: a test Monday wrote metricas.rds (with AVISOS_FECHA, a false week),
# published the threshold alerts in avisos.rds and overwrote semana.rds (the
# Monday snapshot Friday compares against); a test Friday kept the
# histories; a test «archivo» archived for real; an error wrote
# job_errores.rds. Now each is counted (prueba_no_escrito) and skipped.
# Counter-proof: the real mode writes the same as before.
# Run from project root:  Rscript tests/test_t122_job_prueba.R
# =============================================================================
cat("=== test_t122_job_prueba ===\n\n")
source("tests/helpers/avisos_job_fixture.R")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr, det = NULL) {
  r <- tryCatch(isTRUE(expr), error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (r) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L } else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L; if (!is.null(det)) cat("      ", det, "\n") }
}
lunes   <- function(prueba = FALSE, hoy = FX_LUNES) job_correr("lunes", prueba, hoy = hoy, ahora = FX_AHORA_L)
viernes <- function(prueba = FALSE) job_correr("viernes", prueba, hoy = FX_VIERNES, ahora = FX_AHORA_V)
c_ <- fx_cuenta
# Keys that changed between two signatures (new or different).
cambiadas <- function(antes, despues) names(despues)[!names(despues) %in% names(antes) | despues != antes[names(despues)]]
solo_prueba <- function(k) length(k) > 0L && all(grepl("^(acme|beta)/avisos/prueba/", k))
# An alert every Monday: the threshold says yes to anything.
f_eval <- umbral_evaluar
con_alerta <- function(serie, fecha) {
  a <- f_eval(serie, fecha)
  if (nrow(a)) return(a)
  tibble::tibble(semana = umbral_semana(fecha), empresa = "NTS", moneda = "MXN", pct = 50, delta = 20,
                 umbral = 10, direccion = "sube")
}
umbral_evaluar <- con_alerta

cat("-- lunes --\n")
fx_reset(); a0 <- fx_firma()
l <- lunes(prueba = TRUE)
ch <- cambiadas(a0, fx_firma())
ok("L1 lunes de prueba: solo escribe avisos/prueba/*.html (4)", solo_prueba(ch) && c_(l, "prueba_html") == 4L, paste(ch, collapse = " "))
ok("L2 ...ni metricas.rds, ni semana.rds, ni avisos.rds",
   !any(grepl("avisos/(metricas|semana|avisos)[.]rds$", ch)))
ok("L3 ...y lo cuenta: alertas_umbral (lo que se habría publicado) y prueba_no_escrito",
   c_(l, "alertas_umbral") >= 2L && c_(l, "prueba_no_escrito") >= 3L * 2L)
fx_reset(); a0 <- fx_firma()
l <- lunes(prueba = TRUE, hoy = FX_LUNES + 14L)
ok("L4 con otra fecha (AVISOS_FECHA): tampoco deja una semana falsa en metricas.rds",
   !any(grepl("metricas[.]rds$", cambiadas(a0, fx_firma()))))
fx_reset()
lr <- lunes()
ok("L5 contraprueba: el lunes real escribe métricas, semana y avisos como antes, y publica las mismas alertas",
   all(c("acme/avisos/metricas.rds", "acme/avisos/semana.rds", "acme/avisos/avisos.rds") %in% ls(.mem)) &&
     c_(lr, "prueba_no_escrito") == 0L && c_(lr, "alertas_umbral") == c_(l, "alertas_umbral"))

cat("-- viernes --\n")
fx_reset(); invisible(lunes()); for (cid in c("acme", "beta")) fx_cliente(cid, FX_AHORA_V - 3600)
a0 <- fx_firma()
v <- viernes(prueba = TRUE)
ch <- cambiadas(a0, fx_firma())
ok("V1 viernes de prueba (con cierre): solo avisos/prueba/*.html; los históricos no se guardan",
   solo_prueba(ch) && !any(grepl("semana_(historial|partidas)", ch)) && c_(v, "prueba_html") == 4L, paste(ch, collapse = " "))
ok("V2 ...y se cuentan (2 por cliente)", c_(v, "prueba_no_escrito") == 4L && c_(v, "historial_fallido") == 0L)
vr <- viernes()
ok("V3 contraprueba: el viernes real sí guarda los históricos",
   any(grepl("^acme/avisos/semana_historial[.]rds$", ls(.mem))) && c_(vr, "prueba_no_escrito") == 0L)

cat("-- archivo --\n")
AH <- as.POSIXct("2026-11-21 05:30:00", tz = "UTC")
sembrar <- function() {
  fx_reset()
  for (x in list(list("viejo-1", AH - 70 * 86400), list("viejo-2", AH - 40 * 86400), list("reciente", AH - 5 * 86400)))
    avisos_agregar("acme", list(tipo = "automatico", titulo = x[[1]], vence = x[[2]]))
}
sembrar(); a0 <- fx_firma()
r <- job_correr("archivo", TRUE, ahora = AH)
ch <- cambiadas(a0, fx_firma())
ok("A1 archivo de prueba: no escribe nada, cuenta los 2 que archivaría", !length(ch) && c_(r, "archivados") == 2L &&
     c_(r, "prueba_no_escrito") == 1L, paste(ch, collapse = " "))
rr <- job_correr("archivo", FALSE, ahora = AH)
ok("A2 contraprueba: el archivo real archiva los mismos 2",
   c_(rr, "archivados") == 2L && any(grepl("^acme/avisos/archivo/", ls(.mem))) && c_(rr, "prueba_no_escrito") == 0L)
ok("A3 avisos_archivar(simular = TRUE) no escribe y da el mismo número", {
  sembrar(); n0 <- .mem_escrituras
  s <- avisos_archivar("acme", AH, simular = TRUE)
  .mem_escrituras == n0 && s$archivados == 2L
})

cat("-- error --\n")
fx_reset(); a0 <- fx_firma()
f <- correo_datos_lunes
correo_datos_lunes <- function(...) stop("detalle")
l <- lunes(prueba = TRUE)
ch <- cambiadas(a0, fx_firma())
ok("E1 un error en prueba se cuenta, pero job_errores.rds no se escribe",
   c_(l, "error_cliente") == 2L && !any(grepl("job_errores", ch)) && c_(l, "prueba_no_escrito") >= 2L)
fx_reset()
l <- lunes()
correo_datos_lunes <- f
ok("E2 contraprueba: en real sí se escribe job_errores.rds", exists("acme/avisos/job_errores.rds", envir = .mem))

umbral_evaluar <- f_eval
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
quit(status = if (.fail > 0L) 1L else 0L)
