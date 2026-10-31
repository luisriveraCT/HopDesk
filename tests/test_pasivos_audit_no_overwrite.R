# =============================================================================
# tests/test_pasivos_audit_no_overwrite.R
# pasivos_log_audit() appended to pasivos_audit.rds with
#   existing <- tryCatch(load_pasivos_audit(...), error = function(e) <empty>)
#   save_pasivos_audit(bind_rows(existing, new_row))
# so a failed read -- or one that came back empty, since .s3_read_with()
# returns NULL for a failed read as well as for a missing object -- replaced
# the WHOLE Pasivos audit with a single row (found 2026-10-31, CM05/CM06).
# Now a failed or empty read of an object that exists skips this table and
# warns; app_audit (log_action) still records the action.
#
# S3 is never touched: loaders, savers and the existence check are fakes in
# this test's own environment (nothing in aws.s3 or globalenv is patched).
#
# Run from project root:  Rscript tests/test_pasivos_audit_no_overwrite.R
# =============================================================================
cat("=== test_pasivos_audit_no_overwrite ===\n\n")
suppressMessages({ library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

env <- new.env()
for (e in parse("R/pasivos_schemas.R")) eval(e, env)
for (e in parse("R/pasivos_audit.R")) eval(e, env)
env$`%||%` <- `%||%`
env$S3_KEYS <- list(pasivos_audit = "pasivos_audit.rds")
env$.s3_key <- function(key, client_id = NULL) paste0(client_id, "/", key)
env$.s3_preload_cache <- new.env(); env$.s3_missing_cache <- new.env()
env$.s3_bucket <- function() "fake"

five <- tibble::tibble(id = paste0("a", 1:5), ts = Sys.time(), user = "u", empresa = "NCS",
                       action_type = "provision.generated", target_kind = "provision",
                       target_id = paste0("p", 1:5), payload_before = NA_character_,
                       payload_after = NA_character_, session_id = NA_character_, notes = NA_character_)
scenario <- function(read, exists) {
  saved <- NULL; logged <- 0L
  env$load_pasivos_audit <- read
  env$save_pasivos_audit <- function(df, client_id = NULL) saved <<- df
  env$.pasivos_audit_existe <- function(full_key) exists
  env$log_action <- function(...) logged <<- logged + 1L
  w <- NULL
  withCallingHandlers(
    suppressMessages(env$pasivos_log_audit("provision.deleted", user = "dev", target_kind = "provision",
                                           target_id = "p9", client_id = "networks")),
    warning = function(cond) { w <<- conditionMessage(cond); invokeRestart("muffleWarning") })
  list(saved = saved, logged = logged, warning = w)
}

r <- scenario(function(client_id = NULL) five, exists = TRUE)
ok("a normal read appends: 5 rows become 6", !is.null(r$saved) && nrow(r$saved) == 6L)
ok("... and the action reaches app_audit", r$logged == 1L)

r <- scenario(function(client_id = NULL) stop("timeout"), exists = TRUE)
ok("a failed read does NOT write pasivos_audit (it used to overwrite it with 1 row)", is.null(r$saved))
ok("... it warns", !is.null(r$warning) && grepl("not written", r$warning))
ok("... and the action still reaches app_audit", r$logged == 1L)

r <- scenario(function(client_id = NULL) env$.schema_pasivos_audit(), exists = TRUE)
ok("an EMPTY read of an existing file does NOT write (a failed S3 read looks like this)", is.null(r$saved))
ok("... it warns that it refuses to overwrite", !is.null(r$warning) && grepl("refusing to overwrite", r$warning))

r <- scenario(function(client_id = NULL) env$.schema_pasivos_audit(), exists = FALSE)
ok("an empty read with no file yet starts the audit with 1 row", !is.null(r$saved) && nrow(r$saved) == 1L)

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
