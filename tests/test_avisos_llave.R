# =============================================================================
# tests/test_avisos_llave.R
# Frente D: avisos move from <cliente>/avisos.rds to <cliente>/avisos/avisos.rds
# (the scheduled job's role may only write under avisos/). R/avisos.R reads
# the old key until the new one exists; the first write starts from the old
# rows (strict read of both), and the old file is never written or deleted.
# In-memory S3 (tests/helpers/s3_memoria.R); nothing reaches the network.
#
# Run from project root:  Rscript tests/test_avisos_llave.R
# =============================================================================
cat("=== test_avisos_llave ===\n\n")
source("tests/helpers/s3_memoria.R")
suppressPackageStartupMessages(library(shiny))
source("R/i18n.R")
source("R/tier_registry.R")
source("R/permisos.R")
source("R/avisos.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
falla <- function(expr) inherits(tryCatch({ force(expr); NULL }, error = function(e) e), "error")

CID   <- "acme"
NUEVA <- paste0(CID, "/avisos/avisos.rds")
VIEJA <- paste0(CID, "/avisos.rds")
VENCE <- Sys.time() + 86400
aviso <- function(t) list(tipo = "automatico", titulo = t, vence = VENCE)
viejas <- function() {
  s3_memoria_reset()
  avisos_agregar(CID, aviso("de antes"))          # written to the new key...
  assign(VIEJA, get(NUEVA, envir = .mem), envir = .mem)
  rm(list = NUEVA, envir = .mem)                  # ...moved to the old one: a client before D
  for (env in list(.s3_preload_cache, .s3_missing_cache)) rm(list = ls(env), envir = env)   # a fresh process
  .mem_escrituras <<- 0L
}

ok("la llave nueva vive bajo avisos/ (donde escribe el rol del job)",
   identical(.AVISOS_KEY, "avisos/avisos.rds") && identical(.AVISOS_KEY_VIEJA, "avisos.rds"))

viejas()
ok("sin la llave nueva, se leen los avisos de la vieja", identical(avisos_leer(CID)$titulo, "de antes"))
ok("leer no escribe nada", .mem_escrituras == 0L && !exists(NUEVA, envir = .mem))

avisos_agregar(CID, aviso("nuevo"))
ok("la primera escritura copia las filas viejas a la llave nueva, más la suya", {
  n <- get(NUEVA, envir = .mem)
  setequal(n$titulo, c("de antes", "nuevo"))
})
ok("la vieja queda intacta: ni se escribe ni se borra",
   exists(VIEJA, envir = .mem) && identical(get(VIEJA, envir = .mem)$titulo, "de antes"))
ok("después, se lee la nueva", setequal(avisos_leer(CID)$titulo, c("de antes", "nuevo")))
ok("cambios en la vieja (una app sin actualizar) ya no se leen", {
  v <- get(VIEJA, envir = .mem); v$titulo <- "otra cosa"; assign(VIEJA, v, envir = .mem)
  !"otra cosa" %in% avisos_leer(CID)$titulo
})

viejas()
ok("si la vieja no se puede leer, la primera escritura no ocurre (nunca empieza vacía)", {
  .mem_falla <<- VIEJA
  f <- falla(avisos_agregar(CID, aviso("nuevo")))
  .mem_falla <<- character()
  f && !exists(NUEVA, envir = .mem)
})
ok("si la nueva no se puede leer, no se cae a la vieja: ni al escribir ni al leer", {
  avisos_agregar(CID, aviso("nuevo"))
  .mem_falla <<- NUEVA
  f <- falla(avisos_agregar(CID, aviso("otro")))
  vista <- avisos_leer(CID)
  .mem_falla <<- character()
  f && !"de antes" %in% vista$titulo[!vista$titulo %in% "nuevo"] && nrow(vista) == 0L
})
ok("un cliente nuevo (sin ninguna de las dos) empieza en la nueva", {
  s3_memoria_reset()
  avisos_agregar(CID, aviso("primero"))
  exists(NUEVA, envir = .mem) && !exists(VIEJA, envir = .mem)
})
ok("las preferencias no se mueven (el job solo las lee)", identical(.PREFERENCIAS_KEY, "preferencias.rds"))

s3_memoria_restaurar()
cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
