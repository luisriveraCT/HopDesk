# =============================================================================
# tests/test_avisos_archivo.R
# Frente D: the monthly archive (R/avisos_archivo.R) and the job's archivo
# mode. Avisos expired over 30 days go to avisos/archivo/AAAA-MM.rds by the
# month they expired; the archive is written and read back before
# avisos.rds loses them; a failure leaves avisos.rds whole; a re-run never
# duplicates; the old key is never written.
# In-memory S3 (tests/helpers/avisos_job_fixture.R); nothing reaches the network.
#
# Run from project root:  Rscript tests/test_avisos_archivo.R
# =============================================================================
cat("=== test_avisos_archivo ===\n\n")
source("tests/helpers/avisos_job_fixture.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
falla <- function(expr) inherits(tryCatch({ force(expr); NULL }, error = function(e) e), "error")

CID   <- "acme"
AHORA <- as.POSIXct("2026-11-21 05:30:00", tz = "UTC")
AV    <- "acme/avisos/avisos.rds"
ARCH  <- function(m) paste0("acme/avisos/archivo/", m, ".rds")
dias  <- function(n) AHORA - n * 86400
# Five avisos: two expired in September, one in October (over 30 days), one
# expired 10 days ago, one with no expiry.
sembrar <- function() {
  fx_reset()
  for (x in list(list("sep-1", dias(70)), list("sep-2", dias(60)), list("oct", dias(40)),
                 list("reciente", dias(10)), list("sin-vence", NA)))
    avisos_agregar(CID, list(tipo = "automatico", titulo = x[[1]], vence = x[[2]]))
  .mem_escrituras <<- 0L
}
titulos <- function(k) sort(get(k, envir = .mem)$titulo)
MT <- modificar_tabla   # the real one; two cases wrap it and put it back

cat("-- archivar --\n")
sembrar()
r <- tryCatch(avisos_archivar(CID, AHORA), error = function(e) list(archivados = -1L))
ok("vencidos hace más de 30 días salen al archivo del mes en que vencieron", {
  r$archivados == 3L && identical(titulos(ARCH("2026-09")), c("sep-1", "sep-2")) &&
    identical(titulos(ARCH("2026-10")), "oct")
})
ok("avisos.rds se queda con lo reciente y lo que no vence (más de la mitad salió: permitir_reduccion)",
   identical(titulos(AV), c("reciente", "sin-vence")))
ok("lo archivado conserva sus columnas (id, clave, creado...)", {
  a <- get(ARCH("2026-09"), envir = .mem); all(c("id", "clave", "creado", "vence", "niveles") %in% names(a)) && all(!is.na(a$id))
})
ok("correrlo otra vez no duplica nada", {
  r2 <- avisos_archivar(CID, AHORA)
  r2$archivados == 0L && nrow(get(ARCH("2026-09"), envir = .mem)) == 2L
})
ok("si el archivo ya tenía alguno (una corrida que murió a medias), no lo repite y sí lo saca de avisos.rds", {
  sembrar()
  sep <- get(AV, envir = .mem); sep <- sep[sep$titulo == "sep-1", ]
  assign(ARCH("2026-09"), sep, envir = .mem)
  r <- avisos_archivar(CID, AHORA)
  r$archivados == 3L && nrow(get(ARCH("2026-09"), envir = .mem)) == 2L && !"sep-1" %in% titulos(AV)
})

cat("\n-- fallas --\n")
ok("si escribir el archivo falla, avisos.rds no pierde nada", {
  sembrar(); antes <- get(AV, envir = .mem)
  .mem_falla_escritura <<- ARCH("2026-09")
  f <- falla(avisos_archivar(CID, AHORA))
  .mem_falla_escritura <<- character()
  f && identical(get(AV, envir = .mem), antes)
})
ok("solo cuenta lo que se relee del archivo: una escritura que se perdió sin error no quita nada", {
  sembrar(); antes <- get(AV, envir = .mem)
  modificar_tabla <<- function(key, ...) if (grepl("archivo", key)) invisible(NULL) else MT(key, ...)
  r <- avisos_archivar(CID, AHORA)
  modificar_tabla <<- MT
  r$archivados == 0L && identical(get(AV, envir = .mem), antes)
})
ok("si releer el archivo falla, avisos.rds no pierde nada", {
  sembrar(); antes <- get(AV, envir = .mem)
  modificar_tabla <<- function(key, ...) { r <- MT(key, ...); if (grepl("archivo", key)) .mem_falla <<- .s3_key(key, CID); r }
  f <- falla(avisos_archivar(CID, AHORA))
  modificar_tabla <<- MT; .mem_falla <<- character()
  f && identical(get(AV, envir = .mem), antes)
})
ok("un aviso renovado entre el archivo y el borrado se queda en avisos.rds", {
  sembrar()
  modificar_tabla <<- function(key, client_id, schema, cambio, ...) {
    if (identical(key, .AVISOS_KEY)) {   # someone renews "oct" just before the removal
      d <- get(AV, envir = .mem); d$vence[d$titulo == "oct"] <- AHORA + 86400; assign(AV, d, envir = .mem)
    }
    MT(key, client_id, schema, cambio, ...)
  }
  r <- avisos_archivar(CID, AHORA); modificar_tabla <<- MT
  "oct" %in% titulos(AV) && r$archivados == 2L
})
ok("sin la llave nueva (cliente sin migrar) no archiva ni escribe la vieja", {
  fx_reset()
  avisos_agregar(CID, list(tipo = "automatico", titulo = "viejo", vence = dias(70)))
  assign("acme/avisos.rds", get(AV, envir = .mem), envir = .mem); rm(list = AV, envir = .mem)
  for (env in list(.s3_preload_cache, .s3_missing_cache)) rm(list = ls(env), envir = env)
  .mem_escrituras <<- 0L
  r <- avisos_archivar(CID, AHORA)
  r$motivo == "sin_llave_nueva" && .mem_escrituras == 0L
})

cat("\n-- el job en modo archivo --\n")
sembrar()
avisos_agregar("beta", list(tipo = "automatico", titulo = "b-viejo", vence = dias(45)))
antes <- fx_firma()
l <- job_correr("archivo", TRUE, ahora = AHORA)
ok("modo archivo: cuenta lo archivado en los clientes activos, sin enviar nada", {
  fx_cuenta(l, "archivados") == 4L && fx_cuenta(l, "clientes_ok") == 2L && !length(fx_enviados)
})
ok("modo archivo: solo escribe bajo <cliente>/avisos/", {
  despues <- fx_firma()
  cambiadas <- names(despues)[!names(despues) %in% names(antes) | despues != antes[names(despues)]]
  length(cambiadas) > 0 && all(grepl("^(acme|beta)/avisos/", cambiadas))
})
ok("modo archivo: el log solo trae conteos", all(grepl("^[a-z_]+=[0-9]+$", l)))

s3_memoria_restaurar()
cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
