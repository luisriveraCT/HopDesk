# =============================================================================
# tests/test_avisos_job.R
# Frente D: scripts/avisos_job.R end to end on the in-memory S3, with a fake
# send_email(): test mode (HTML to avisos/prueba/, nothing sent), real mode
# (one email per recipient, never resent), a failed send, a client left out,
# a client that errors, Friday after Monday, and that the job writes only
# under <cliente>/avisos/.
#
# Run from project root:  Rscript tests/test_avisos_job.R
# =============================================================================
cat("=== test_avisos_job ===\n\n")
source("tests/helpers/avisos_job_fixture.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
lunes   <- function(prueba = FALSE) job_correr("lunes", prueba, hoy = FX_LUNES, ahora = FX_AHORA_L)
viernes <- function(prueba = FALSE) job_correr("viernes", prueba, hoy = FX_VIERNES, ahora = FX_AHORA_V)
c_ <- fx_cuenta

cat("-- modo prueba (el de por defecto) --\n")
fx_reset()
antes <- fx_firma()
l <- lunes(prueba = TRUE)
ok("por defecto el job está en modo prueba", {
  Sys.unsetenv("AVISOS_PRUEBA"); identical(formals(job_correr)$prueba, quote(!identical(tolower(Sys.getenv("AVISOS_PRUEBA")), "false")))
})
ok("prueba: 2 clientes activos (el archivado no), nada enviado, un HTML por destinatario", {
  c_(l, "clientes") == 2L && c_(l, "clientes_ok") == 2L && !length(fx_enviados) &&
    c_(l, "destinatarios") == 4L && c_(l, "prueba_html") == 4L &&
    exists("acme/avisos/prueba/2026-11-16_lunes_1.html", envir = .mem)
})
ok("el HTML de prueba es el correo (con su asunto) y su ruta no lleva el usuario", {
  h <- rawToChar(get("acme/avisos/prueba/2026-11-16_lunes_1.html", envir = .mem)$bytes)
  grepl("PANORAMA DE LA SEMANA|THE WEEK AHEAD", h) && !any(grepl("jefa|fin|ti_", ls(.mem)[grepl("prueba", ls(.mem))]))
})
ok("prueba: no marca nada como enviado", !exists("acme/avisos/correos_enviados.rds", envir = .mem))
ok("solo escribe bajo <cliente>/avisos/ (lo que el rol permite)", {
  despues <- fx_firma()
  cambiadas <- names(despues)[!names(despues) %in% names(antes) | despues != antes[names(despues)]]
  length(cambiadas) > 0 && all(grepl("^(acme|beta)/avisos/", cambiadas))
})

cat("\n-- envío real --\n")
fx_reset()
l <- lunes()
ok("un correo por destinatario, sin cola de reintentos", c_(l, "enviado") == 4L && length(fx_enviados) == 4L &&
     all(!vapply(fx_enviados, function(x) x$cola, logical(1))))
ok("quién: dev y admin por defecto; finanzas no (su permiso está apagado)", {
  to <- vapply(fx_enviados, function(x) x$to, "")
  setequal(to, c("jefa@acme-secreto.mx", "ti@acme-secreto.mx", "jefa@beta-secreto.mx", "ti@beta-secreto.mx"))
})
ok("cada quien en su idioma (ti tiene inglés)", {
  s <- vapply(fx_enviados, function(x) paste(x$to, x$subject), "")
  any(grepl("^ti@acme.*The week ahead|^ti@acme.*week", s, ignore.case = TRUE)) && any(grepl("^jefa@acme.*Panorama de la semana", s))
})
ok("los envíos quedan en correos_enviados.rds", nrow(get("acme/avisos/correos_enviados.rds", envir = .mem)) == 2L)
ok("las métricas, la semana y los avisos se escriben bajo avisos/",
   all(c("acme/avisos/metricas.rds", "acme/avisos/semana.rds") %in% ls(.mem)))
ok("correrlo otra vez no reenvía nada", {
  n <- length(fx_enviados); l2 <- lunes()
  length(fx_enviados) == n && c_(l2, "ya_enviado") == 4L && c_(l2, "enviado") == 0L
})

cat("\n-- fallas --\n")
fx_reset(); fx_envio_ok <<- FALSE
l <- lunes()
ok("un envío que falla se cuenta y no se marca: la siguiente corrida lo reintenta", {
  fx_envio_ok <<- TRUE; l2 <- lunes()
  c_(l, "fallido") == 4L && c_(l2, "enviado") == 4L && c_(l2, "ya_enviado") == 0L
})
fx_reset(); .mem_prohibido <<- "beta/bancos_confirmados.rds"; assign("beta/bancos_confirmados.rds", tibble::tibble(), envir = .mem)
l <- lunes()
ok("un cliente que no se lee completo se omite; los demás siguen", {
  .mem_prohibido <<- character()
  c_(l, "omitido_lectura") == 1L && c_(l, "clientes_ok") == 1L && c_(l, "enviado") == 2L
})
fx_reset(); fx_pon("beta", S3_KEYS$usuarios, "esto no es una tabla")
l <- lunes()
ok("un cliente cuyos usuarios no se pueden leer: omitido_destinatarios, sin correo", {
  c_(l, "omitido_destinatarios") == 1L && c_(l, "enviado") == 2L
})
fx_reset()
f <- correo_datos_lunes
correo_datos_lunes <- function(...) stop("detalle con Cliente Secreto Uno y $400,123.45")
l <- lunes()
correo_datos_lunes <- f
ok("un error en un cliente se cuenta; su texto va a job_errores.rds, no al log", {
  e <- get("acme/avisos/job_errores.rds", envir = .mem)
  c_(l, "error_cliente") == 2L && grepl("Cliente Secreto", e$mensaje[1]) && !any(grepl("Secreto", l))
})
ok("si el registro de clientes no se lee, el job no arranca (nunca 'cero clientes')", {
  fx_reset(); .mem_falla <<- "hd-admin/client_registry.rds"
  r <- inherits(tryCatch(lunes(), error = function(e) e), "error")
  .mem_falla <<- character(); r
})

cat("\n-- viernes --\n")
fx_reset()
invisible(lunes())
for (cid in c("acme", "beta")) fx_cliente(cid, FX_AHORA_V - 3600)   # SAP refreshed by Friday
fx_enviados <<- list()
v <- viernes()
ok("el viernes, después del lunes: un correo por destinatario, sin montos", {
  c_(v, "enviado") == 4L && c_(v, "clientes_ok") == 2L &&
    any(grepl("Cierre de semana|Week in review", vapply(fx_enviados, function(x) x$subject, ""), ignore.case = TRUE))
})
ok("viernes en prueba: el HTML no lleva $", {
  fx_reset(); invisible(lunes()); for (cid in c("acme", "beta")) fx_cliente(cid, FX_AHORA_V - 3600)
  invisible(viernes(prueba = TRUE))
  h <- rawToChar(get("acme/avisos/prueba/2026-11-20_viernes_1.html", envir = .mem)$bytes)
  !grepl("$", h, fixed = TRUE) && grepl("PAGOS CONFIRMADOS|PAYMENTS CONFIRMED", h)
})
ok("viernes sin el lunes de esa semana: omitido_sin_lunes", {
  fx_reset(FX_AHORA_V - 3600); v <- viernes()
  c_(v, "omitido_sin_lunes") == 2L && c_(v, "enviado") == 0L
})
ok("un modo desconocido no corre", inherits(tryCatch(job_correr("martes", TRUE), error = function(e) e), "error"))

s3_memoria_restaurar()
cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
