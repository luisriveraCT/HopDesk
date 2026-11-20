# =============================================================================
# tests/test_t72_audit_cola.R
# T72 (A3): app_audit's write queue never loses a batch silently.
#   1. A live chunk with a column of another type (ts as text, user read back
#      as logical): the flush used to fail on every retry (bind_rows) and
#      read_audit_log() showed an EMPTY Actividad. Now both work.
#   2. S3 refuses the live chunk on every retry: after the last one the batch
#      is written to <cid>/app_audit_pendiente_*.rds, read_audit_log() still
#      shows it, and staff get an aviso in hd-admin ("rescatadas").
#   3. The rescue write fails too: the aviso says the entries are lost.
# S3 is tests/helpers/s3_memoria.R (restored at the end); later::later is a
# stub that records the retry instead of scheduling it (restored too).
# Run from project root:  Rscript tests/test_t72_audit_cola.R
# Contraprueba: on 867d251's R/app_audit.R, 1 and 2 fail.
# =============================================================================
cat("=== test_t72_audit_cola ===\n\n")
suppressMessages({ library(dplyr); library(tibble) })
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("tests/helpers/s3_memoria.R")
suppressMessages(source("R/i18n.R"))
suppressMessages(source(Sys.getenv("T72_AUDIT_R", "R/app_audit.R")))
orig_later <- later::later
utils::assignInNamespace("later", function(f, delay = 0) invisible(NULL), "later")
AVISOS <- list()
avisos_agregar <- function(client_id, aviso, ...) { AVISOS[[length(AVISOS) + 1L]] <<- c(list(cid = client_id), aviso); invisible("id") }
niveles_staff <- function() "dev"

CID  <- "acme"
LIVE <- paste0(CID, "/app_audit.rds")
entrada <- function(n) log_action(user = "luis", module = "pasivos", action = paste0("a", n), description = "d",
                                  client_id = CID, viewer_home_client_id = CID)

# 1. A live chunk with other types
s3_memoria_reset()
viejo <- tibble::tibble(id = "v1", ts = "2026-11-01 10:00:00", user = NA, module = "bancos", action = "x",
                        description = "d", target_id = NA, s3_key = NA, client_id = CID, metadata = "{}")
assign(LIVE, viejo, envir = .mem)
entrada(1)
ok1 <- .audit_flush_pending(CID)
ok("1 a live chunk with ts as text and user logical: the flush appends (it used to fail every retry)",
   isTRUE(ok1) && nrow(get(LIVE, envir = .mem)) == 2L)
lee <- read_audit_log(CID)
ok("1 …and read_audit_log() shows both rows (it used to show an EMPTY log)",
   nrow(lee) == 2L && setequal(lee$action, c("x", "a1")) && inherits(lee$ts, "POSIXct"))

# 2. S3 refuses the live chunk on every retry
s3_memoria_reset(); AVISOS <- list()
assign(LIVE, .APP_AUDIT_SCHEMA(), envir = .mem)
.mem_falla_escritura <- LIVE
entrada(2); entrada(3)
for (i in seq_len(.AUDIT_MAX_RETRIES + 1L)) .audit_flush_pending(CID)
.mem_falla_escritura <- character(0)
resc <- grep(paste0("^", CID, "/app_audit_pendiente_.*\\.rds$"), ls(.mem), value = TRUE)
ok("2 after the last retry the batch is written to a key of its own (app_audit_pendiente_*)",
   length(resc) == 1L && nrow(get(resc, envir = .mem)) == 2L)
ok("2 …nothing is left queued", is.null(.audit_pending[[CID]]) || !nrow(.audit_pending[[CID]]))
ok("2 …read_audit_log() still shows the two entries", all(c("a2", "a3") %in% read_audit_log(CID)$action))
ok("2 …staff get ONE aviso in hd-admin, 'rescatadas', with the client and the file",
   length(AVISOS) == 1L && identical(AVISOS[[1]]$cid, "hd-admin") && identical(AVISOS[[1]]$niveles, "dev") &&
     grepl("Se guardaron aparte", AVISOS[[1]]$cuerpo, fixed = TRUE) && grepl(CID, AVISOS[[1]]$cuerpo, fixed = TRUE) &&
     identical(AVISOS[[1]]$clave, "t72:log:acme"))

# 3. The rescue fails too
s3_memoria_reset(); AVISOS <- list()
assign(LIVE, .APP_AUDIT_SCHEMA(), envir = .mem)
escribir_real <- .audit_write_chunk
.audit_write_chunk <- function(df, key) stop("simulated S3 down")
entrada(4)
for (i in seq_len(.AUDIT_MAX_RETRIES + 1L)) .audit_flush_pending(CID)
.audit_write_chunk <- escribir_real
ok("3 the rescue fails too: the aviso says the entries are lost",
   length(AVISOS) == 1L && grepl("Se perdieron 1 entradas", AVISOS[[1]]$cuerpo, fixed = TRUE))

for (k in c("comun.actividad.alerta.titulo", "comun.actividad.alerta.rescatadas", "comun.actividad.alerta.perdidas"))
  ok(sprintf("key %s in es and en", k), nzchar(tr(k, idioma = "es")) && !identical(tr(k, idioma = "en"), k) &&
       !identical(tr(k, idioma = "es"), k))

utils::assignInNamespace("later", orig_later, "later")
s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
