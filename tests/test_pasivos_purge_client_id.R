# =============================================================================
# tests/test_pasivos_purge_client_id.R
# pasivos_admin_purge_closed() wrote its pre-purge BACKUP with
# .s3_write(provs, backup_key) -- no client_id -- while the purge itself and
# its audit used the client_id argument. On the shared deployment that
# backup fails (.s3_key() refuses to guess a tenant) and the purge aborts;
# elsewhere the backup would land in whichever client CLIENT_ID names
# (found 2026-10-31, CM07). Every write now goes to the same client, and
# client_id is required.
#
# S3 is never touched: every reader/writer is a fake in this test's own env.
#
# Run from project root:  Rscript tests/test_pasivos_purge_client_id.R
# =============================================================================
cat("=== test_pasivos_purge_client_id ===\n\n")
suppressMessages({ library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

env <- new.env()
for (e in parse("R/pasivos_module.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      identical(as.character(e[[2]]), "pasivos_admin_purge_closed"))
    eval(e, env)
env$`%||%` <- `%||%`

writes <- list()
rec <- function(what) function(..., client_id = NULL) {
  writes[[length(writes) + 1L]] <<- list(what = what, client_id = client_id); invisible(TRUE)
}
env$load_pasivos_provisions <- function(client_id = NULL)
  tibble::tibble(id = c("a", "b", "c"), liability_id = "L", estado = c("closed", "provisional", "closed"),
                 fecha_efectiva = as.Date("2026-05-01"), amount_pago = 1, parte = "P")
env$.s3_write               <- function(obj, key, client_id = NULL) rec("backup")(client_id = client_id)
env$save_pasivos_provisions <- function(df, client_id = NULL, ...) rec("save")(client_id = client_id)
env$bump_sync_version       <- function(name, client_id = NULL) rec("bump")(client_id = client_id)
env$pasivos_log_audit       <- function(..., client_id = NULL) rec("audit")(client_id = client_id)

writes <- list()
r <- tryCatch(suppressMessages(env$pasivos_admin_purge_closed(dry_run = FALSE, user = "t")),
              error = function(e) e)
ok("without client_id it stops before writing anything", inherits(r, "error") && !length(writes))

writes <- list()
n <- suppressMessages(env$pasivos_admin_purge_closed(dry_run = FALSE, user = "t", client_id = "networks"))
cids <- vapply(writes, function(w) w$client_id %||% "<NULL>", "")
whats <- vapply(writes, `[[`, "", "what")
ok("it purges the 2 closed rows", identical(as.integer(n), 2L))
ok("the backup is written to the same client", identical(cids[whats == "backup"], "networks"))
ok("backup, save, sync stamp and audit all go to networks",
   setequal(whats, c("backup", "save", "bump", "audit")) && all(cids == "networks"))
ok("the backup is written before the destructive save",
   which(whats == "backup") < which(whats == "save"))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
