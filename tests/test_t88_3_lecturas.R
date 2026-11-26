# =============================================================================
# tests/test_t88_3_lecturas.R
# T88-3, the sign-in's time.
#  (a) A key S3 confirms does not exist (404) is not retried: .s3_read_raw_
#      with_retry() made 3 GETs with 0.5 s between them (~1.1 s) for every
#      missing file. A 403 or a network error still retries; the answer is
#      still ok = FALSE (A2: a failed read is never an empty table), and only
#      a confirmed 404 goes to the missing-key cache.
#  (b) Panorama's ultima_visita: the previous visit is read BEFORE the new one
#      is written ("new since your last visit"), and the write waits until the
#      first screen is drawn (session$onFlushed), off the sign-in path.
# In-memory S3 (tests/helpers/s3_memoria.R).
# Run from project root:  Rscript tests/test_t88_3_lecturas.R
# =============================================================================
cat("=== test_t88_3_lecturas ===\n\n")
source("tests/helpers/s3_memoria.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
CID <- "acme"; B <- .s3_bucket()
k_no  <- paste0(CID, "/no_existe.rds")
k_403 <- paste0(CID, "/prohibido.rds")
k_red <- paste0(CID, "/red.rds")
k_ok  <- paste0(CID, "/si.rds")
# T114b: wall clock on purpose -- "no wait" means no retry sleep, which costs
# no CPU: only the clock sees it (a1, a6 below).
leer <- function(k) { t0 <- proc.time()[["elapsed"]]; r <- .s3_read_raw_with_retry(k, B); r$seg <- proc.time()[["elapsed"]] - t0; r }

# ── (a) ──────────────────────────────────────────────────────────────────────
s3_memoria_reset()
r <- leer(k_no)
ok("a1 a missing key (404): one attempt, no wait", isTRUE(r$attempts == 1) && r$seg < 0.4)
ok("a2 ... still ok = FALSE, and says no_existe", identical(r$ok, FALSE) && isTRUE(r$no_existe))

s3_memoria_reset(); assign(k_403, data.frame(a = 1), envir = .mem); .mem_prohibido <- k_403
r <- leer(k_403)
ok("a3 a 403 (no permission): retried 3 times, not 'does not exist'",
   isTRUE(r$attempts == 3) && identical(r$ok, FALSE) && !isTRUE(r$no_existe))
.mem_prohibido <- character()

s3_memoria_reset(); assign(k_red, data.frame(a = 1), envir = .mem); .mem_falla <- k_red
r <- leer(k_red)
ok("a4 an object that exists but fails to read: retried 3 times",
   isTRUE(r$attempts == 3) && identical(r$ok, FALSE) && !isTRUE(r$no_existe))
.mem_falla <- character()

s3_memoria_reset(); assign(k_ok, data.frame(a = 1:3), envir = .mem)
r <- leer(k_ok)
ok("a5 a key that exists: read at the first attempt", isTRUE(r$ok) && isTRUE(r$attempts == 1) && nrow(r$value) == 3L)

# Through .s3_read_with(): the missing-key cache only for a confirmed 404
s3_memoria_reset()
t0 <- proc.time()[["elapsed"]]
v <- .s3_read_with("no_existe.rds", client_id = CID)
ok("a6 .s3_read_with on a missing key: NULL, fast, and cached as missing",
   is.null(v) && proc.time()[["elapsed"]] - t0 < 0.4 && isTRUE(.s3_missing_cache_hit(k_no)))
s3_memoria_reset(); assign(k_403, data.frame(a = 1), envir = .mem); .mem_prohibido <- k_403
v <- .s3_read_with("prohibido.rds", client_id = CID)
ok("a7 ... a 403 is NULL too, but never cached as missing (the next read asks again)",
   is.null(v) && !isTRUE(.s3_missing_cache_hit(k_403)))
.mem_prohibido <- character()

# A fixture that takes the reader alone (no s3_objeto_existe): retries as before
ok("a8 without s3_objeto_existe (a fixture by pieces): retries as before, no error", {
  e <- new.env()
  e$.s3_read_raw_with_retry <- .s3_read_raw_with_retry
  environment(e$.s3_read_raw_with_retry) <- e   # cannot see the global s3_objeto_existe
  e$exists <- function(x, mode = "any") if (identical(x, "s3_objeto_existe")) FALSE else base::exists(x, mode = mode)
  s3_memoria_reset()
  r <- e$.s3_read_raw_with_retry(k_no, B, retry_delay = 0)
  isTRUE(r$attempts == 3) && identical(r$ok, FALSE)
})

# ── (b) Panorama: read the previous visit, then write the new one, later ─────
src <- readLines("R/panorama_module.R", warn = FALSE)
src <- src[!grepl("^\\s*#", src)]
i_visita <- grep("visita(p$ultima_visita)", src, fixed = TRUE)
i_leer   <- grep("p <- tryCatch(prefs_leer(c, u$user)", src, fixed = TRUE)
i_flush  <- grep("session$onFlushed(function()", src, fixed = TRUE)
i_guarda <- grep("prefs_guardar(c, u$user, list(ultima_visita", src, fixed = TRUE)
ok("b1 the previous visit is read before the new one is written",
   length(i_leer) == 1L && length(i_guarda) == 1L && i_leer < i_visita && i_visita < i_guarda)
ok("b2 the write waits for the first screen (inside session$onFlushed, once)",
   length(i_flush) == 1L && i_flush < i_guarda && i_guarda - i_flush <= 2L &&
     any(grepl("once = TRUE", src[i_guarda:(i_guarda + 3L)], fixed = TRUE)))
ok("b3 the new visit's time is taken at the visit, not when the write runs",
   any(grepl("ahora_visita <- format(Sys.time()", src[i_visita:i_flush], fixed = TRUE)) &&
     grepl("list(ultima_visita = ahora_visita)", src[i_guarda], fixed = TRUE))
ok("b4 staff inside a client still never write the client's preferences",
   any(grepl("if (!isTRUE(info$is_staff))", src[i_visita:i_flush], fixed = TRUE)))

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
