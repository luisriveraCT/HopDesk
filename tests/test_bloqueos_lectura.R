# =============================================================================
# tests/test_bloqueos_lectura.R
# CM14 (2026-11-04): read_emergency_lock() returned NULL both for "no lock
# file" and for a failed read, and the login check and the active-session
# check took NULL for "nobody is locked": with S3 failing, a locked account
# got in. Decided by Luis: on a failed read, use the last lock list read well
# in this process; with none, deny the login ("intenta de nuevo en un minuto").
#
# Runs the REAL read_emergency_lock() over the in-memory S3 and the REAL
# auth_check_credentials() (shinymanager, scrypt) with one test account.
#
# Run from project root:  Rscript tests/test_bloqueos_lectura.R
# =============================================================================
cat("=== test_bloqueos_lectura ===\n\n")
source("tests/helpers/s3_memoria.R")
suppressPackageStartupMessages(library(shinymanager))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

source("R/i18n.R")
for (e in parse("R/auth.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      identical(as.character(e[[2]]), "auth_check_credentials"))
    eval(e, globalenv())
creds <- data.frame(user = "ana", password = scrypt::hashPassword("clave"),
                    is_hashed_password = TRUE, stringsAsFactors = FALSE)
auth_get_credentials <- function() creds
entrar <- function() auth_check_credentials()("ana", "clave")

k_lk <- "hd-admin/emergency_lock.rds"
candado <- function(u) data.frame(username = u, locked_at = as.POSIXct("2026-11-01", tz = "UTC"),
                                  locked_by = "luis", reason = "r", stringsAsFactors = FALSE)
nuevo_proceso <- function() {          # a fresh R process: no list read yet
  s3_memoria_reset()
  rm(list = ls(.emergency_lock_cache), envir = .emergency_lock_cache)
}
pasa_minuto <- function() .emergency_lock_cache$ts <- .emergency_lock_cache$ts - 61

# 1. No lock file: nobody locked, the login goes in
nuevo_proceso()
l <- read_emergency_lock()
ok("no file: an empty list, not NULL", is.data.frame(l) && nrow(l) == 0L)
ok("no file: the login goes in", isTRUE(entrar()$result))

# 2. ana locked, S3 readable: denied
nuevo_proceso()
assign(k_lk, candado("ana"), envir = .mem)
r <- entrar()
ok("locked: login denied", identical(r$result, FALSE))

# 3. THE BUG: S3 fails, no list read before in this process -> denied, not let in
nuevo_proceso()
assign(k_lk, candado("ana"), envir = .mem)
.mem_falla <- k_lk
ok("failed read, none before: read_emergency_lock() stops (was NULL)",
   inherits(tryCatch(read_emergency_lock(), error = function(e) e), "error"))
r <- entrar()
ok("failed read, none before: login denied", identical(r$result, FALSE))
ok("... with the 'try again' message",
   identical(r$user_info$message, tr("niveles.hdadmin.login_sin_verificar")))

# 4. A list read well earlier, then S3 fails: that list is used
nuevo_proceso()
assign(k_lk, candado("ana"), envir = .mem)
invisible(read_emergency_lock())            # read well: ana locked
pasa_minuto(); .mem_falla <- k_lk
l <- read_emergency_lock()
ok("failed read after a good one: the last good list, marked vieja",
   identical(l$username, "ana") && isTRUE(attr(l, "vieja")))
ok("... a failure does not replace it in the cache",
   identical(.emergency_lock_cache$data$username, "ana") && is.null(attr(.emergency_lock_cache$data, "vieja")))
ok("... and ana stays out", identical(entrar()$result, FALSE))

# 5. An unlocked account still gets in on the last good list during a blip
nuevo_proceso()
assign(k_lk, candado("beto"), envir = .mem)
invisible(read_emergency_lock())
pasa_minuto(); .mem_falla <- k_lk
ok("failed read, last good list without ana: ana gets in", isTRUE(entrar()$result))

# 6. Once S3 answers again the fresh list replaces the stale one
.mem_falla <- character()
assign(k_lk, candado("ana"), envir = .mem)
pasa_minuto()
l <- read_emergency_lock()
ok("S3 back: the fresh list, not marked vieja", identical(l$username, "ana") && is.null(attr(l, "vieja")))

# 7. The active-session check retries while it only has a stale list or none
app <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
blk <- regmatches(app, regexpr("emergency_lock_checked <- reactiveVal\\(FALSE\\).*?session\\$reload\\(\\)", app))
ok("session check: retries every minute on no list or a stale one",
   length(blk) == 1L && grepl('invalidateLater(60000)', blk, fixed = TRUE) &&
   grepl('attr(lock, "vieja")', blk, fixed = TRUE))
ok("session check: marked done only after a fresh read",
   length(blk) == 1L && !grepl("emergency_lock_checked(TRUE)\n    lock <-", blk, fixed = TRUE))

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
