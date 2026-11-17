# =============================================================================
# tests/test_s3_memoria_auditoria.R
# T38b. log_action() queues audit rows and writes them ~2 s later
# (later::later). s3_memoria_reset() flushes that queue -- into the store of
# the case that queued it -- before clearing, so the delayed write never lands
# in the next case and counts as its write (test_pasivos_fecha_requerida
# failed now and then on exactly that).
# Run from project root:  Rscript tests/test_s3_memoria_auditoria.R
# Contraprueba: S3MEM_HELPER=<the helper before T38b> fails checks 2-5.
# =============================================================================
cat("=== test_s3_memoria_auditoria ===\n\n")
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}
source(Sys.getenv("S3MEM_HELPER", "tests/helpers/s3_memoria.R"))
suppressMessages(source("R/app_audit.R"))
CID <- "acme"
llave_audit <- function() paste0(CID, "/", sub("^.*/", "", .audit_current_key(CID)))
# Every write the store gets, by key (the counter is zeroed by each reset).
G <- new.env(); G$llaves <- character()
suppressMessages(trace("s3saveRDS", where = asNamespace("aws.s3"), print = FALSE,
                       tracer = quote(G$llaves <- c(G$llaves, object))))
encolar <- function() suppressMessages(
  log_action(user = "tester", module = "prueba", action = "accion", description = "una fila", client_id = CID))
esperar_later <- function() { Sys.sleep(2.6); later::run_now(0.5); later::run_now(0.1) }

# 1. Sanity: the queue writes on its own after ~2 s.
s3_memoria_reset(); encolar()
ok("1. a log_action is queued, not written yet", .mem_escrituras == 0L && !is.null(.audit_pending[[CID]]))
esperar_later()
ok("   ... and later::later writes it ~2 s after (one write)", .mem_escrituras == 1L && is.null(.audit_pending[[CID]]))

# 2-3. Queue, reset before the 2 s: the row is written BEFORE the store is
# cleared, and the delayed callback then writes nothing into the next case.
s3_memoria_reset(); encolar(); G$llaves <- character()
s3_memoria_reset()
ok("2. reset flushes the queued row (written to the audit chunk before clearing)",
   is.null(.audit_pending[[CID]]) && any(grepl("app_audit", G$llaves)))
esperar_later()
ok("3. the next case's counter stays at 0 when the delayed callback fires", .mem_escrituras == 0L)

# 4. The case left a simulated write failure on the audit chunk: the row is
# still flushed (not dropped) and nothing lands in the next case.
s3_memoria_reset(); encolar()
.mem_falla_escritura <- unique(c(.mem_falla_escritura, grep("app_audit", c(G$llaves, paste0(CID, "/app_audit.rds")), value = TRUE)))
G$llaves <- character()
s3_memoria_reset()
ok("4. with a write failure left on, the row is retried without it and flushed",
   is.null(.audit_pending[[CID]]) && sum(grepl("app_audit", G$llaves)) >= 2L)   # the failed try, then the good one
esperar_later()
ok("5. ... and the next case's counter stays at 0 (retry callback included)", .mem_escrituras == 0L)

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
