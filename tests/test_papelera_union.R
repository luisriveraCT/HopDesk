# =============================================================================
# tests/test_papelera_union.R
# CM09 (2026-11-01): callers saved the papelera from their session's copy plus
# new archive events, so a stale copy dropped events another session had
# archived meanwhile (and with the write net, which refuses any shrink of a
# table that only grows, the action would fail instead). save_papelera() now
# writes a union on S3's current table: S3's rows plus the copy's rows S3 does
# not have yet, by event_id. A failed read writes nothing.
#
# Run from project root:  Rscript tests/test_papelera_union.R
# =============================================================================
cat("=== test_papelera_union ===\n\n")
source("tests/helpers/s3_memoria.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
q <- function(expr) suppressMessages(expr)

evento <- function(ev) {
  p <- .schema_papelera()
  p[1, "id"] <- paste0("id-", ev); p[1, "event_id"] <- ev; p[1, "ledger"] <- "AP"
  p
}
pap <- function(...) dplyr::bind_rows(lapply(c(...), evento))
KEY <- "acme/papelera.rds"

s3_memoria_reset(); assign(KEY, pap("a", "b", "c"), envir = .mem)
copia_vieja <- pap("a", "b")                     # this session never saw "c"
q(save_papelera(dplyr::bind_rows(copia_vieja, evento("d")), client_id = "acme"))
ok("a stale copy plus a new event keeps the other session's event: a, b, c, d",
   setequal(get(KEY, envir = .mem)$event_id, c("a", "b", "c", "d")))

q(save_papelera(pap("a", "b", "c", "d"), client_id = "acme"))
ok("saving rows S3 already has adds nothing (no duplicates)", nrow(get(KEY, envir = .mem)) == 4L)

s3_memoria_reset(); assign(KEY, pap("a", "b"), envir = .mem); .mem_falla <- KEY
r <- tryCatch(q(save_papelera(pap("x"), client_id = "acme")), error = function(e) e)
ok("a failed read writes nothing (it used to replace the papelera with the copy)",
   inherits(r, "error") && .mem_escrituras == 0L && nrow(get(KEY, envir = .mem)) == 2L)

s3_memoria_reset()
q(save_papelera(pap("primero"), client_id = "nuevo"))
ok("a client with no papelera yet starts it", nrow(get("nuevo/papelera.rds", envir = .mem)) == 1L)

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
