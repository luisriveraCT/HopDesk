# =============================================================================
# tests/test_t80b_job_corridas.R
# T80b: the weekly email job leaves one row per real run, client, week and
# mode in <cliente>/avisos/corridas.rds (R/avisos_correo_vigia.R reads it).
# Real runs only; a later run of the week replaces the row; every way out of
# job_lunes()/job_viernes() lands as its resultado.
#
# Run from project root:  Rscript tests/test_t80b_job_corridas.R
# =============================================================================
cat("=== test_t80b_job_corridas ===\n\n")
source("tests/helpers/avisos_job_fixture.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
lunes   <- function(prueba = FALSE) job_correr("lunes", prueba, hoy = FX_LUNES, ahora = FX_AHORA_L)
viernes <- function(prueba = FALSE) job_correr("viernes", prueba, hoy = FX_VIERNES, ahora = FX_AHORA_V)
k <- function(cid) .s3_key(CORREO_VIGIA_KEY, client_id = cid)
cor <- function(cid) if (exists(k(cid), envir = .mem)) get(k(cid), envir = .mem) else NULL
SEM <- correo_vigia_semana(FX_LUNES)

ok("el job carga R/avisos_correo_vigia.R (la llave y el esquema son uno solo)",
   "R/avisos_correo_vigia.R" %in% AVISOS_JOB_ARCHIVOS && exists("CORREO_VIGIA_KEY"))

cat("-- envío real --\n")
fx_reset()
invisible(lunes())
ok("un lunes real deja 'enviado' por cliente, con sus conteos", {
  a <- cor("acme"); b <- cor("beta")
  nrow(a) == 1L && a$semana == SEM && a$modo == "lunes" && a$resultado == "enviado" &&
    a$destinatarios == 2L && a$enviados == 2L && a$fallidos == 0L && identical(b$resultado, "enviado")
})
ok("el cliente archivado no tiene fila", is.null(cor("zeta")))
ok("repetirlo (ya_corrido) deja una sola fila, 'enviado'", {
  invisible(lunes()); nrow(cor("acme")) == 1L && cor("acme")$resultado == "enviado"
})
ok("si la fila se perdió, el ya_corrido la repone", {
  rm(list = k("acme"), envir = .mem); invisible(lunes())
  identical(cor("acme")$resultado, "enviado")
})

cat("\n-- modo prueba --\n")
fx_reset()
invisible(lunes(prueba = TRUE))
ok("una prueba no escribe corridas.rds", is.null(cor("acme")) && is.null(cor("beta")))

cat("\n-- fallas --\n")
fx_reset(); fx_envio_ok <<- FALSE
invisible(lunes())
ok("todos los envíos fallan: 'sin_envio'", identical(cor("acme")$resultado, "sin_envio"))
fx_envio_ok <<- TRUE
invisible(lunes())
ok("el reintento que manda lo que faltaba la cambia a 'enviado' (misma semana, una fila)",
   nrow(cor("acme")) == 1L && identical(cor("acme")$resultado, "enviado"))
fx_reset()
.mem_prohibido <<- "beta/bancos_confirmados.rds"; assign("beta/bancos_confirmados.rds", tibble::tibble(), envir = .mem)
invisible(lunes())
.mem_prohibido <<- character()
ok("un cliente que no se lee completo: 'omitido_lectura' (alerta); el otro 'enviado'",
   identical(cor("beta")$resultado, "omitido_lectura") && identical(cor("acme")$resultado, "enviado"))

cat("\n-- viernes --\n")
fx_reset(FX_AHORA_V - 3600)
invisible(viernes())
ok("viernes sin su lunes: 'omitido_sin_lunes' (por diseño, no alerta)",
   identical(cor("acme")$resultado, "omitido_sin_lunes") && identical(cor("acme")$modo, "viernes"))
fx_reset()
invisible(lunes()); for (cid in c("acme", "beta")) fx_cliente(cid, FX_AHORA_V - 3600)
invisible(viernes())
ok("lunes y viernes de la misma semana: dos filas", {
  a <- cor("acme"); nrow(a) == 2L && setequal(a$modo, c("lunes", "viernes")) && all(a$resultado == "enviado")
})

cat("\n-- el resultado, regla por regla --\n")
z <- stats::setNames(integer(length(JOB_EVENTOS)), JOB_EVENTOS)
con <- function(...) { x <- z; v <- list(...); for (n in names(v)) x[[n]] <- as.integer(v[[n]]); x }
ok("error > ya_corrido > fallas > omitido > sin destinatarios > enviado", {
  r <- function(d, ok = TRUE) job_corrida_resultado(z, d, ok)
  identical(r(z, ok = FALSE), "error") && identical(r(con(ya_corrido = 1)), "enviado") &&
    identical(r(con(destinatarios = 2, enviado = 1, fallido = 1)), "con_fallas") &&
    identical(r(con(destinatarios = 2, fallido = 2)), "sin_envio") &&
    identical(r(con(omitido_sin_sap = 1)), "omitido_sin_sap") &&
    identical(r(con(clientes_ok = 1)), "sin_destinatarios") &&
    identical(r(con(destinatarios = 2, enviado = 2, clientes_ok = 1)), "enviado")
})
ok("una escritura de corridas.rds que falla nunca detiene el job", {
  fx_reset(); .mem_falla_escritura <<- k("acme")
  l <- lunes(); .mem_falla_escritura <<- character()
  fx_cuenta(l, "enviado") == 4L && fx_cuenta(l, "error_cliente") == 0L && is.null(cor("acme"))
})

s3_memoria_restaurar()
cat(sprintf("\nTOTAL: %d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
