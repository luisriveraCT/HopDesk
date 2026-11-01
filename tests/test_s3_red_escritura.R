# =============================================================================
# tests/test_s3_red_escritura.R
# CM09 (2026-11-01): a failed S3 read must never end in a save that rewrites
# a table almost empty.
#   - .s3_read_with()/.s3_read() cache a key as "missing" only when S3 says it
#     does not exist (a transient error used to blank the key for 60 s);
#   - .s3_write() refuses, by default for every key, to shrink a table to
#     fewer than half its rows (4+ rows) or to none, and any shrink at all of
#     a table that only grows -- unless permitir_reduccion = TRUE. A shrink is
#     confirmed against a strict fresh read before refusing;
#   - leer_tabla_para_escribir() / modificar_tabla(): strict read-modify-write.
#
# S3 is an in-memory store: aws.s3's s3readRDS / s3saveRDS / object_exists
# are swapped for this test only and ALWAYS restored (on.exit).
#
# Run from project root:  Rscript tests/test_s3_red_escritura.R
# =============================================================================
cat("=== test_s3_red_escritura ===\n\n")
suppressMessages({ library(dplyr); library(tibble) })

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
falla <- function(expr) { r <- tryCatch({ force(expr); NULL }, error = function(e) e); if (inherits(r, "error")) message("    (error: ", conditionMessage(r), ")"); inherits(r, "error") }

# ── In-memory S3, swapped in and always restored ──────────────────────────────
store <- new.env(); failing <- character(); exists_unknown <- FALSE; writes <- 0L
fake_read <- function(object, bucket, ...) {
  if (object %in% failing) stop("simulated S3 timeout")
  if (!exists(object, envir = store, inherits = FALSE)) stop("NoSuchKey")
  get(object, envir = store)
}
fake_save <- function(x, object, bucket, ...) { writes <<- writes + 1L; assign(object, x, envir = store); invisible(TRUE) }
fake_exists <- function(object, bucket, ...) {
  if (exists_unknown) stop("simulated HEAD failure")
  exists(object, envir = store, inherits = FALSE)
}
ns <- asNamespace("aws.s3")
orig <- list(s3readRDS = get("s3readRDS", ns), s3saveRDS = get("s3saveRDS", ns),
             object_exists = get("object_exists", ns))
restore <- function() for (nm in names(orig)) utils::assignInNamespace(nm, orig[[nm]], "aws.s3")
on.exit(restore(), add = TRUE)
utils::assignInNamespace("s3readRDS", fake_read, "aws.s3")
utils::assignInNamespace("s3saveRDS", fake_save, "aws.s3")
utils::assignInNamespace("object_exists", fake_exists, "aws.s3")

# ── The real persistence layer ────────────────────────────────────────────────
Sys.setenv(S3_BUCKET = "fake-bucket", CLIENT_ID = "acme")
`%||%` <- function(a, b) if (is.null(a)) b else a
for (e in parse("R/global.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "S3_KEYS"))
    eval(e, globalenv())
suppressMessages(source("R/persistence.R"))
q <- function(expr) suppressMessages(expr)

df <- function(n) data.frame(id = seq_len(n), x = rep("a", n), stringsAsFactors = FALSE)
reset <- function() {
  rm(list = ls(store), envir = store); failing <<- character(); exists_unknown <<- FALSE; writes <<- 0L
  for (env in Filter(Negate(is.null), list(.s3_preload_cache, .s3_missing_cache, get0(".s3_filas"))))
    rm(list = ls(env), envir = env)
}
K <- "invoice_moves.rds"; FULL <- paste0("acme/", K)

# 1. The "missing" cache
reset(); assign(FULL, df(10), envir = store); failing <- FULL
ok("a failed read of an existing key returns NULL", is.null(q(.s3_read_with(K, "acme"))))
ok("... and does NOT mark it missing (it used to, for 60 s)", !exists(FULL, envir = .s3_missing_cache))
failing <- character()
ok("... so the next read gets the data back", nrow(q(.s3_read_with(K, "acme"))) == 10L)
reset()
q(.s3_read_with("nada.rds", "acme"))
ok("a key S3 says does not exist IS cached as missing", exists("acme/nada.rds", envir = .s3_missing_cache))

# 2. The write net
reset(); assign(FULL, df(2000), envir = store)
ok("0 rows over 2000 is refused", falla(q(.s3_write(df(0), K, "acme"))))
ok("900 over 2000 is refused (less than half)", falla(q(.s3_write(df(900), K, "acme"))))
ok("... and nothing was written", writes == 0L && nrow(get(FULL, envir = store)) == 2000L)
ok("1500 over 2000 is written", !falla(q(.s3_write(df(1500), K, "acme"))) && nrow(get(FULL, envir = store)) == 1500L)
ok("permitir_reduccion = TRUE lets an intended shrink through",
   !falla(q(.s3_write(df(10), K, "acme", permitir_reduccion = TRUE))) && nrow(get(FULL, envir = store)) == 10L)

reset(); assign("acme/papelera.rds", df(10), envir = store)
ok("a table that only grows (papelera) refuses even 10 -> 9", falla(q(.s3_write(df(9), "papelera.rds", "acme"))))
ok("... and accepts growing", !falla(q(.s3_write(df(11), "papelera.rds", "acme"))))

reset(); assign(FULL, df(3), envir = store)
ok("a 3-row table may go to 1", !falla(q(.s3_write(df(1), K, "acme"))))
reset(); assign(FULL, df(3), envir = store)
ok("... but not to 0", falla(q(.s3_write(df(0), K, "acme"))))

reset(); assign(FULL, df(100), envir = store); q(.s3_read_with(K, "acme"))   # registry: 100
assign(FULL, df(10), envir = store)                                          # another process purged it
ok("a stale count never blocks: registry 100, S3 now 10, writing 11 passes",
   !falla(q(.s3_write(df(11), K, "acme"))) && nrow(get(FULL, envir = store)) == 11L)

reset(); assign(FULL, df(50), envir = store); failing <- FULL
ok("count unknown and the read fails: the write is refused", falla(q(.s3_write(df(1), K, "acme"))))
ok("... nothing written", writes == 0L)
failing <- character(); exists_unknown <- TRUE; rm(list = FULL, envir = store)
ok("S3 cannot say whether it exists: refused (fails closed)", falla(q(.s3_write(df(1), K, "acme"))))

reset()
ok("a brand-new key is written", !falla(q(.s3_write(df(5), "nuevo.rds", "acme"))))
ok("a non-table object (list) is not checked", !falla(q(.s3_write(list(a = 1), "interco_v2.rds", "acme"))))

# 3. Keys outside the net: Agenda is a queue ("Vaciar agenda")
reset(); assign("acme/pagar_hoy_sync.rds", df(6), envir = store)
ok("Vaciar agenda: the shared agenda may go to 0", !falla(q(.s3_write(df(0), "pagar_hoy_sync.rds", "acme"))))
reset(); assign("acme/pagar_hoy_user_dev.rds", df(6), envir = store)
ok("... and so may a per-user agenda", !falla(q(.s3_write(df(0), "pagar_hoy_user_dev.rds", "acme"))))

# 4. aviso mode (operational escape hatch)
reset(); assign(FULL, df(2000), envir = store); options(hopdesk.s3_red = "aviso")
ok("aviso mode logs and writes", !falla(q(.s3_write(df(0), K, "acme"))) && nrow(get(FULL, envir = store)) == 0L)
options(hopdesk.s3_red = NULL)

# 5. modificar_tabla
sch <- function() tibble::tibble(id = integer(), x = character())
reset(); assign(FULL, df(20), envir = store); failing <- FULL
ok("modificar_tabla with a failed read writes nothing",
   falla(q(modificar_tabla(K, "acme", sch, function(d) d[0, ]))) && writes == 0L)
failing <- character()
q(modificar_tabla(K, "acme", sch, function(d) d))
ok("modificar_tabla with no change does not write", writes == 0L)
q(modificar_tabla(K, "acme", sch, function(d) dplyr::bind_rows(d, tibble::tibble(id = 21L, x = "b"))))
ok("modificar_tabla applies the change on S3's table", nrow(get(FULL, envir = store)) == 21L)

# 6. The intended purge passes the flag
src <- paste(readLines("R/pasivos_module.R", warn = FALSE), collapse = "\n")
ok("pasivos_admin_purge_closed() saves with permitir_reduccion = TRUE",
   grepl("save_pasivos_provisions(cleaned, client_id = client_id, permitir_reduccion = TRUE)", src, fixed = TRUE))

restore()
ok("aws.s3 is restored", identical(get("s3saveRDS", ns), orig$s3saveRDS))
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
