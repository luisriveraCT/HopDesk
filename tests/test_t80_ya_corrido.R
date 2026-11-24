# =============================================================================
# tests/test_t80_ya_corrido.R
# T80: the weekly email is started several times on purpose (AWS trigger plus
# backup crons). A repeated real run whose email already reached everyone
# skips the client whole: semana.rds (the Monday snapshot Friday compares
# against) and the week's histories are not rewritten with later data. A
# half-sent client and a missing record still run as before; a test run
# skips too once the real email went out (it left no HTML anyway).
#
# Run from project root:  Rscript tests/test_t80_ya_corrido.R
# =============================================================================
cat("=== test_t80_ya_corrido ===\n\n")
source("tests/helpers/avisos_job_fixture.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
c_ <- fx_cuenta
k  <- function(cid, x) .s3_key(x, client_id = cid)
# A mark only a rewrite would remove: the fixture's data makes the same
# snapshot twice, so comparing contents could not tell a rewrite apart.
marcar  <- function(llaves) for (x in llaves) { o <- get(x, envir = .mem); attr(o, "marca_t80") <- TRUE; assign(x, o, envir = .mem) }
marcado <- function(llaves) length(llaves) > 0L &&
  all(vapply(llaves, function(x) isTRUE(attr(get(x, envir = .mem), "marca_t80")), logical(1)))
SEMANAS <- c(k("acme", "avisos/semana.rds"), k("beta", "avisos/semana.rds"))
lunes <- function(prueba = FALSE, ahora = FX_AHORA_L) job_correr("lunes", prueba, hoy = FX_LUNES, ahora = ahora)
viernes <- function(prueba = FALSE, ahora = FX_AHORA_V) job_correr("viernes", prueba, hoy = FX_VIERNES, ahora = ahora)

cat("-- lunes repetido --\n")
fx_reset()
l1 <- lunes()
marcar(SEMANAS)
esc <- .mem_escrituras
l2 <- lunes(ahora = FX_AHORA_L + 6 * 3600)
esc_l2 <- .mem_escrituras - esc   # the backup, six hours later
ok("el primero manda los 4; el segundo no manda nada y salta a los 2 clientes",
   c_(l1, "enviado") == 4L && c_(l2, "ya_corrido") == 2L && c_(l2, "enviado") == 0L &&
     c_(l2, "clientes_ok") == 0L && length(fx_enviados) == 4L)
ok("semana.rds se queda con la foto del primer lunes, y el salto no escribe nada", marcado(SEMANAS) && esc_l2 == 0L)
ok("el log del salto es solo 'ya_corrido=<n>' (está en JOB_EVENTOS)",
   "ya_corrido" %in% JOB_EVENTOS && any(l2 == "ya_corrido=2"))

cat("\n-- a medias: la siguiente corrida manda lo que falta --\n")
fx_reset(); fx_envio_ok <<- FALSE
l1 <- lunes()
fx_envio_ok <<- TRUE
l2 <- lunes()
ok("sin nada enviado no hay salto: se recalcula y se manda todo",
   c_(l1, "fallido") == 4L && c_(l2, "ya_corrido") == 0L && c_(l2, "enviado") == 4L)
fx_reset()
l1 <- lunes()
en <- get(k("acme", "avisos/correos_enviados.rds"), envir = .mem)
assign(k("acme", "avisos/correos_enviados.rds"), en[-1, ], envir = .mem)   # one recipient missing
l2 <- lunes()
ok("si a un destinatario le falta, ese cliente corre y solo le manda a él; el otro se salta",
   c_(l2, "ya_corrido") == 1L && c_(l2, "enviado") == 1L && c_(l2, "ya_enviado") == 1L)

cat("\n-- modo prueba --\n")
fx_reset()
lp0 <- lunes(prueba = TRUE)
ok("antes del real, una prueba corre y deja sus 4 HTML", c_(lp0, "ya_corrido") == 0L && c_(lp0, "prueba_html") == 4L)
invisible(lunes())
marcar(SEMANAS)
lp <- lunes(prueba = TRUE, ahora = FX_AHORA_L + 3600)
ok("tras el real, una prueba se salta (antes no dejaba HTML y reescribía semana.rds)",
   c_(lp, "ya_corrido") == 2L && c_(lp, "prueba_html") == 0L && marcado(SEMANAS))

cat("\n-- otra semana y otro modo no cuentan --\n")
ok("el lunes de esta semana no salta el de la siguiente ni el viernes",
   !job_ya_salio("acme", "lunes", FX_LUNES + 7L) && !job_ya_salio("acme", "viernes", FX_VIERNES))

cat("\n-- viernes repetido --\n")
fx_reset()
invisible(lunes())
for (cid in c("acme", "beta")) fx_cliente(cid, FX_AHORA_V - 3600)   # SAP refreshed by Friday: a close, so histories are written
v1 <- viernes()
HIST <- grep("^(acme|beta)/avisos/semana_(historial|partidas)[.]rds$", fx_llaves(), value = TRUE)
for (cid in c("acme", "beta")) fx_cliente(cid, FX_AHORA_V)   # SAP refreshed again: a recompute would differ
esc <- .mem_escrituras
v2 <- viernes(ahora = FX_AHORA_V + 3600)
esc_v2 <- .mem_escrituras - esc
ok("el segundo viernes salta a los 2 clientes sin mandar nada",
   c_(v1, "enviado") == 4L && c_(v2, "ya_corrido") == 2L && c_(v2, "enviado") == 0L)
# The skip writes nothing. This does not tell the skip apart from a full
# recompute: with the fixture's invoices a recompute gives the same tables,
# and modificar_tabla() does not write an unchanged one. The Monday case
# above (semana.rds) is the one that catches a missing skip.
ok("el salto del viernes no escribe nada (los historiales de la semana existen)",
   length(HIST) == 4L && esc_v2 == 0L)

cat("\n-- sin registro legible: corre como siempre --\n")
fx_reset()
invisible(lunes())
.mem_prohibido <<- k("acme", "avisos/correos_enviados.rds")
ok("un correos_enviados.rds que no se lee nunca es 'ya salió'",
   !job_ya_salio("acme", "lunes", FX_LUNES))
.mem_prohibido <<- character()

cat(sprintf("\nTOTAL: %d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
