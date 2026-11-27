# =============================================================================
# tests/test_t119_refuerzos.R
# T119, two hardenings:
#   1. puede() (R/permisos.R): a key that is not in usuario$permisos is FALSE,
#      whatever shape the permissions have (an atomic vector without that
#      name used to stop()). Contraprueba: c119d720's puede() stops.
#   2. .s3_modificar() (R/avisos.R): when the change leaves the table as it
#      was, nothing is written (avisos_agregar with repetir = FALSE rewrote
#      avisos.rds). A FAILED read still stop()s -- never "empty, unchanged,
#      skip". Contraprueba: c119d720 wrote.
# S3 is an in-memory store local to this test (as tests/test_avisos.R):
# aws.s3 is never patched.
# Run from the repo root: Rscript --vanilla tests/test_t119_refuerzos.R
# =============================================================================
cat("=== test_t119_refuerzos ===\n\n")
suppressPackageStartupMessages(library(shiny))
`%||%` <- function(a, b) if (is.null(a)) b else a
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R", "R/avisos.R")) suppressMessages(source(f, encoding = "UTF-8"))
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
del_commit <- function(archivo, env) {
  src <- tryCatch(system2("git", c("show", paste0("c119d720:", archivo)), stdout = TRUE, stderr = FALSE), error = function(e) character())
  if (length(src)) eval(parse(text = src, keep.source = FALSE), env)
  length(src) > 0L
}

cat("== 1. puede() ==\n")
k <- "can_manage_bancos"
otra <- setdiff(permisos_claves(), k)[1]
casos <- list(
  "vector completo, TRUE"    = list(permisos = stats::setNames(TRUE, k)),
  "vector sin la llave"      = list(permisos = stats::setNames(TRUE, otra)),
  "vector sin nombres"       = list(permisos = c(TRUE, TRUE)),
  "lista completa, TRUE"     = list(permisos = stats::setNames(list(TRUE), k)),
  "lista sin la llave"       = list(permisos = stats::setNames(list(TRUE), otra)),
  "lista con FALSE"          = list(permisos = stats::setNames(list(FALSE), k)),
  "permisos NULL"            = list(permisos = NULL))
esperado <- c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
r <- vapply(casos, function(u) tryCatch(puede(u, k), error = function(e) NA), logical(1))
ok("1.1 every shape answers, never stop()s", !anyNA(r))
ok("1.2 a key that is there and TRUE: TRUE; missing, FALSE or no names: FALSE (fail-closed)", identical(unname(r), esperado))
ok("1.3 no user at all: FALSE", identical(puede(NULL, k), FALSE))
viejo <- new.env(parent = globalenv())
ok("1.4 contraprueba: c119d720's puede() stopped on the vector without the key",
   del_commit("R/permisos.R", viejo) && inherits(tryCatch(viejo$puede(casos[["vector sin la llave"]], k), error = function(e) e), "error"))

cat("== 2. .s3_modificar(): no change, no write ==\n")
.mem <- new.env()
.escrituras <- 0L
.fallar_lectura <- FALSE
.s3_preload_cache <- new.env(); .s3_missing_cache <- new.env()
.s3_key <- function(key, client_id = NULL) paste0(client_id, "/", key)
.s3_read_with <- function(key, client_id = NULL) if (.fallar_lectura) NULL else .mem[[paste0(client_id, "/", key)]]
.s3_existe <- function(key, client_id) !is.null(.mem[[paste0(client_id, "/", key)]])
.s3_leer_para_escribir <- function(key, client_id) {
  if (.fallar_lectura) stop("[S3] could not read ", key, " before writing it; nothing was saved")
  .mem[[paste0(client_id, "/", key)]]
}
.s3_write <- function(obj, key, client_id = NULL, ...) {
  .escrituras <<- .escrituras + 1L
  assign(paste0(client_id, "/", key), obj, envir = .mem)
}
log_action <- function(...) invisible(NULL)
CID <- "cliente_prueba"
cuenta <- function(expr) { n0 <- .escrituras; force(expr); .escrituras - n0 }

n_nuevo <- cuenta(id1 <- avisos_agregar(CID, list(titulo = "A", clave = "evento:1", repetir = FALSE)))
ok("2.1 a new aviso: 1 write", n_nuevo == 1L)
n_rep <- cuenta(id2 <- avisos_agregar(CID, list(titulo = "A otra vez", clave = "evento:1", repetir = FALSE)))
ok("2.2 THE FINDING: repetir = FALSE on a clave already there -- 0 writes, and the same id comes back",
   n_rep == 0L && identical(id1, id2))
ok("2.3 ...and the aviso is as it was (title not replaced)", identical(avisos_leer(CID)$titulo, "A"))
ok("2.4 marcar hecho: 1 write the first time, 0 the second (already done)",
   cuenta(avisos_marcar_hecho(CID, id1, "ana")) == 1L && cuenta(avisos_marcar_hecho(CID, id1, "ana")) == 0L)
ok("2.5 retirar an id that does not exist: 0 writes", cuenta(avisos_retirar(CID, "no-existe", "ana")) == 0L)
ok("2.6 retirar a real one: 1 write", cuenta(avisos_retirar(CID, id1, "ana")) == 1L && isTRUE(avisos_leer(CID)$retirado[1]))
ok("2.7 the default repetir (TRUE) on a live clave still counts it again: 1 write",
   { id3 <- avisos_agregar(CID, list(titulo = "B", clave = "evento:2"))
     cuenta(avisos_agregar(CID, list(titulo = "B", clave = "evento:2"))) == 1L &&
       avisos_leer(CID)$contador[avisos_leer(CID)$id == id3] == 2L })
ok("2.8 preferences stay as they were (they always stamp actualizado): 1 write each time",
   cuenta(prefs_guardar(CID, "ana", list(idioma = "es"))) == 1L && cuenta(prefs_guardar(CID, "ana", list(idioma = "es"))) == 1L)

cat("== 2b. a failed read still stops ==\n")
.fallar_lectura <- TRUE
n0 <- .escrituras
e <- tryCatch(avisos_agregar(CID, list(titulo = "C", clave = "evento:1", repetir = FALSE)), error = function(e) e)
ok("2.9 a failed read stop()s, as before -- never taken as 'empty, unchanged, nothing to write'",
   inherits(e, "error") && grepl("could not read", conditionMessage(e)) && .escrituras == n0)
e2 <- tryCatch(.s3_modificar(.AVISOS_KEY, CID, .schema_avisos, function(df) df), error = function(e) e)
ok("2.10 ...even with a change function that changes nothing", inherits(e2, "error") && .escrituras == n0)
.fallar_lectura <- FALSE

cat("== 2c. contraprueba ==\n")
viejo2 <- new.env(parent = globalenv())
del_commit("R/avisos.R", viejo2)
ok("2.11 c119d720's .s3_modificar wrote avisos.rds again for the same repetir = FALSE",
   cuenta(viejo2$avisos_agregar(CID, list(titulo = "A otra vez", clave = "evento:1", repetir = FALSE))) == 1L)

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
