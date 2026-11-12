# =============================================================================
# tests/test_s3_existe.R
# 2026-11-11. "Does this S3 object exist?" with three answers.
# aws.s3::object_exists() returns FALSE for any non-success HEAD -- a 404 and
# a 403 alike -- and the strict reads before a write took that FALSE for "a
# new file": with credentials that lost read access, a save would have
# started from an empty table and written only its new rows over the real
# one. s3_objeto_existe() (R/persistence.R) reads the status aws.s3 messages:
# TRUE / FALSE only on 404 / NA on anything else, and every caller fails
# closed on NA.
#
# ASSUMPTION, written down: the roles that read prod -- the app's AWS key and
# D's GitHub OIDC role (hopdesk-avisos-github) -- have s3:ListBucket on the
# bucket. Without List, S3 answers 403 even for a key that does not exist;
# here that is NA ("can't tell"), so a write stops and the first file of a
# new client could not be created. That is the safe side, but it would show
# as "could not read ... before writing it". Checked for the app's prod key
# on 2026-11-11 (a HEAD of a missing key answered 404); D's OIDC role still
# to be confirmed by whoever holds its policy.
#
#   A  s3_objeto_existe() alone, with stand-in HEADs (no aws.s3 patched):
#      TRUE; FALSE + 404 -> FALSE; FALSE + 403 -> NA; FALSE without a status
#      -> NA; an error -> NA; the HEAD's message does not leak.
#   B  the strict read before a write (in-memory S3): 404 -> NULL (a new
#      file); 403 -> stop(); a network error on the HEAD -> stop().
#   C  the "missing" cache: only a 404 is cached as missing.
#   D  auth.R's strict read of usuarios.rds: 404 -> NULL; 403 -> stop().
# Run from project root:  Rscript tests/test_s3_existe.R
# =============================================================================
cat("=== test_s3_existe ===\n\n")
source("tests/helpers/s3_memoria.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
falla <- function(expr) inherits(tryCatch({ force(expr); NULL }, error = function(e) e), "error")

# ── A: the function alone ───────────────────────────────────────────────────
cabeza <- function(valor, aviso = NULL, error = FALSE) function(object, bucket, ...) {
  if (error) stop("Could not resolve host: s3.amazonaws.com")
  if (!is.null(aviso)) message(aviso)
  valor
}
ok("A1 exists: TRUE", isTRUE(s3_objeto_existe("k", "b", cabeza(TRUE))))
ok("A2 404: FALSE (it really does not exist)",
   identical(s3_objeto_existe("k", "b", cabeza(FALSE, "Client error: (404) Not Found")), FALSE))
ok("A3 403: NA (no permission is not 'does not exist')",
   identical(s3_objeto_existe("k", "b", cabeza(FALSE, "Client error: (403) Forbidden")), NA))
ok("A4 another status (500): NA",
   identical(s3_objeto_existe("k", "b", cabeza(FALSE, "Server error: (500) Internal Server Error")), NA))
ok("A5 FALSE with no status at all: NA", identical(s3_objeto_existe("k", "b", cabeza(FALSE)), NA))
ok("A6 a network error: NA", identical(s3_objeto_existe("k", "b", cabeza(TRUE, error = TRUE)), NA))
salio <- character()
r7 <- tryCatch(withCallingHandlers(s3_objeto_existe("k", "b", cabeza(FALSE, "Client error: (404) Not Found")),
                    message = function(m) { salio <<- c(salio, conditionMessage(m)); invokeRestart("muffleMessage") }),
               error = function(e) e)
ok("A7 the HEAD's status message is read, not printed", !inherits(r7, "error") && !length(salio))

# ── B: the strict read before a write ───────────────────────────────────────
CID <- "acme"; K <- "tabla.rds"; FULL <- paste0(CID, "/", K)
s3_memoria_reset()
ok("B1 404 (no such file): NULL, a new file", is.null(.s3_leer_para_escribir(K, CID)))
s3_memoria_reset(); assign(FULL, data.frame(id = 1:3), envir = .mem); .mem_prohibido <- FULL
ok("B2 403 (no permission): stop() -- it used to be NULL, an empty table", falla(.s3_leer_para_escribir(K, CID)))
ok("B2 ... with the message that nothing was saved",
   grepl("nothing was saved", tryCatch(.s3_leer_para_escribir(K, CID), error = conditionMessage)))
s3_memoria_reset(); .mem_falla <- FULL; .mem_falla_cabeza <- FULL
ok("B3 the read fails and the HEAD errors (network): stop()", falla(.s3_leer_para_escribir(K, CID)))
s3_memoria_reset(); assign(FULL, data.frame(id = 1:3), envir = .mem)
ok("B4 the object is there: it is read", identical(nrow(.s3_leer_para_escribir(K, CID)), 3L))
s3_memoria_reset(); .mem_prohibido <- FULL
ok("B5 a modificar_tabla() on a 403 writes nothing",
   falla(modificar_tabla(K, CID, function() tibble::tibble(id = integer()), function(d) d)) && .mem_escrituras == 0L)

# ── C: only a 404 is cached as "missing" ────────────────────────────────────
s3_memoria_reset(); .s3_missing_si_no_existe(FULL)
ok("C1 404: cached as missing", FULL %in% ls(.s3_missing_cache))
s3_memoria_reset(); .mem_prohibido <- FULL; .s3_missing_si_no_existe(FULL)
ok("C2 403: not cached (the next read asks again)", !(FULL %in% ls(.s3_missing_cache)))

# ── D: auth.R's strict read of usuarios.rds ─────────────────────────────────
for (e in parse("R/auth.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c(".usuarios_existe", ".usuarios_leer_estricto"))
    eval(e, globalenv())
KU <- "acme/usuarios.rds"
s3_memoria_reset()
ok("D1 usuarios.rds 404: NULL (a new client)", is.null(.usuarios_leer_estricto(KU, intentos = 1L, espera = 0)))
s3_memoria_reset(); .mem_prohibido <- KU
ok("D2 usuarios.rds 403: stop() -- it used to be NULL, and a save would erase every account",
   falla(.usuarios_leer_estricto(KU, intentos = 1L, espera = 0)))

# ── E: app_audit.R's strict read of a log chunk ─────────────────────────────
for (e in parse("R/app_audit.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), ".audit_read_chunk_para_escribir"))
    eval(e, globalenv())
KA <- "acme/app_audit.rds"
s3_memoria_reset()
ok("E1 audit chunk 404: NULL (a new log)", is.null(.audit_read_chunk_para_escribir(KA)))
s3_memoria_reset(); .mem_prohibido <- KA
ok("E2 audit chunk 403: stop() -- it used to start the log from empty", falla(.audit_read_chunk_para_escribir(KA)))

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
