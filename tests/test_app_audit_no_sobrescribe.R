# =============================================================================
# tests/test_app_audit_no_sobrescribe.R
# CM09 (2026-11-01): flushing log_action()'s queue read the live app_audit
# chunk with .audit_read_chunk(), which turns any read error into NULL; the
# flush then appended the pending rows to an EMPTY table and wrote it over
# the live chunk (up to 10,000 entries of the activity log). The read before
# appending is now strict: a failed read writes nothing, the rows go back to
# the queue and the flush is retried.
#
# S3 is an in-memory store: aws.s3's s3readRDS / s3saveRDS / object_exists
# are swapped for this test only and ALWAYS restored (on.exit); later::later
# is a stub that records the retry instead of scheduling it.
#
# Run from project root:  Rscript tests/test_app_audit_no_sobrescribe.R
# =============================================================================
cat("=== test_app_audit_no_sobrescribe ===\n\n")
suppressMessages({ library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

store <- new.env(); failing <- FALSE; writes <- 0L
fake_read   <- function(object, bucket, ...) {
  if (failing) stop("simulated S3 timeout")
  if (!exists(object, envir = store, inherits = FALSE)) stop("NoSuchKey")
  get(object, envir = store)
}
fake_save   <- function(x, object, bucket, ...) { writes <<- writes + 1L; assign(object, x, envir = store); invisible(TRUE) }
fake_exists <- function(object, bucket, ...) {   # as aws.s3's HEAD: the status in a message
  if (exists(object, envir = store, inherits = FALSE)) return(TRUE)
  message("Client error: (404) Not Found"); FALSE
}
ns <- asNamespace("aws.s3")
orig <- list(s3readRDS = get("s3readRDS", ns), s3saveRDS = get("s3saveRDS", ns),
             object_exists = get("object_exists", ns))
restore <- function() for (nm in names(orig)) utils::assignInNamespace(nm, orig[[nm]], "aws.s3")
on.exit(restore(), add = TRUE)
utils::assignInNamespace("s3readRDS", fake_read, "aws.s3")
utils::assignInNamespace("s3saveRDS", fake_save, "aws.s3")
utils::assignInNamespace("object_exists", fake_exists, "aws.s3")

.s3_bucket <- function() "fake-bucket"
for (e in parse("R/persistence.R", keep.source = FALSE))   # the chunk's existence check asks it
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "s3_objeto_existe"))
    eval(e, globalenv())
suppressMessages(source("R/app_audit.R"))
reintentos <- 0L
later_stub <- function(f, delay = 0) { reintentos <<- reintentos + 1L; invisible(NULL) }
# .audit_flush_pending calls later::later(): swap it for this test and restore it too.
orig_later <- later::later
utils::assignInNamespace("later", later_stub, "later")
on.exit(utils::assignInNamespace("later", orig_later, "later"), add = TRUE)

fila <- function(n, accion = "x") {
  s <- .APP_AUDIT_SCHEMA()
  s[seq_len(n), ] <- NA
  s$action <- rep(accion, n); s$id <- paste0(accion, seq_len(n))
  s
}
KEY <- "acme/app_audit.rds"
reset <- function() {
  rm(list = ls(store), envir = store); failing <<- FALSE; writes <<- 0L; reintentos <<- 0L
  for (nm in ls(.audit_pending)) rm(list = nm, envir = .audit_pending)
  for (nm in ls(.audit_retry_count)) rm(list = nm, envir = .audit_retry_count)
}

# 1. Normal: the pending rows are appended to the live chunk
reset(); assign(KEY, fila(50), envir = store); .audit_pending[["acme"]] <- fila(2, "nuevo")
suppressMessages(.audit_flush_pending("acme"))
ok("normal flush: 50 + 2 = 52 rows", nrow(get(KEY, envir = store)) == 52L)

# 2. The live chunk cannot be read: nothing is written, rows go back, retry scheduled
reset(); assign(KEY, fila(50), envir = store); .audit_pending[["acme"]] <- fila(2, "nuevo")
failing <- TRUE
suppressMessages(.audit_flush_pending("acme"))
failing <- FALSE
ok("failed read: the live chunk is NOT overwritten (it used to become 2 rows)",
   nrow(get(KEY, envir = store)) == 50L && writes == 0L)
ok("... the 2 rows are back in the queue", nrow(.audit_pending[["acme"]] %||% fila(0)) == 2L)
ok("... and a retry was scheduled", reintentos == 1L)
suppressMessages(.audit_flush_pending("acme"))
ok("... the retry, with S3 back, appends them: 52 rows", nrow(get(KEY, envir = store)) == 52L)

# 3. A new client (no chunk yet) starts its log
reset(); .audit_pending[["nuevo"]] <- fila(3, "primero")
suppressMessages(.audit_flush_pending("nuevo"))
ok("new client: the log starts with the 3 pending rows", nrow(get("nuevo/app_audit.rds", envir = store)) == 3L)

restore(); utils::assignInNamespace("later", orig_later, "later")
ok("aws.s3 and later are restored", identical(get("s3saveRDS", ns), orig$s3saveRDS) &&
     identical(later::later, orig_later))
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
