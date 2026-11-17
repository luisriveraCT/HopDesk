# =============================================================================
# tests/test_avisos_job_put_object.R  (T42)
# The emails job (scripts/avisos_job.R) crashed in its first real run
# (2026-11-17, lunes, prueba): job_guardar_prueba() passed the HTML as raw
# bytes in `file =`, and aws.s3 0.3.22's put_object() takes `file` as a path
# (file.size(file)) -- "invalid filename argument". The offline tests never
# saw it: tests/helpers/s3_memoria.R swaps put_object for a fake that took
# whatever came in `file`.
#
# Here the REAL aws.s3::put_object() runs; only aws.s3's network layer
# (s3HTTP) is replaced, and restored on exit. What would go to S3:
#   1. the save returns TRUE (no error);
#   2. a PUT to the S3_BUCKET bucket, path /<cid>/avisos/prueba/<name>;
#   3. Content-Type text/html; charset=utf-8;
#   4. the body is the HTML's UTF-8 bytes, accents included.
# Contraprueba: today's call (file = charToRaw(...)) fails with "invalid
# filename argument" before reaching the network.
# Run from the project root:  Rscript tests/test_avisos_job_put_object.R
# =============================================================================
cat("=== test_avisos_job_put_object ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(a, b) if (is.null(a)) b else a
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]])[1] %in% nombres)
    eval(e, globalenv())
cargar("R/persistence.R", c(".s3_bucket", ".s3_key", ".DEPLOYMENT_OWNED_KEYS"))
cargar("scripts/avisos_job.R", "job_guardar_prueba")
Sys.setenv(S3_BUCKET = "hopdesk-prueba-t42", CLIENT_ID = "")

# Only the network layer is replaced: what put_object() would send is kept.
enviado <- NULL
red_real <- get("s3HTTP", envir = asNamespace("aws.s3"))
utils::assignInNamespace("s3HTTP", function(verb, bucket, path, headers = list(), request_body = "", ...) {
  enviado <<- list(verb = verb, bucket = bucket, path = path, headers = headers, body = request_body)
  list()   # an empty response: put_object() returns TRUE
}, "aws.s3")
on.exit(utils::assignInNamespace("s3HTTP", red_real, "aws.s3"), add = TRUE)
restaurar <- function() utils::assignInNamespace("s3HTTP", red_real, "aws.s3")

html <- "<html><body><p>Cobros de la semana · Señora Peña: $1,234.50</p></body></html>"
res <- tryCatch(job_guardar_prueba("networks", "2026-11-17_lunes_1.html", html), error = function(e) e)
ok("1: the real put_object() saves the test HTML (no 'invalid filename argument')", isTRUE(res))
ok("2: a PUT to the S3_BUCKET bucket, /networks/avisos/prueba/2026-11-17_lunes_1.html",
   identical(enviado$verb, "PUT") && identical(enviado$bucket, "hopdesk-prueba-t42") &&
     identical(enviado$path, "/networks/avisos/prueba/2026-11-17_lunes_1.html"))
ok("3: Content-Type text/html; charset=utf-8",
   identical(enviado$headers[["Content-Type"]], "text/html; charset=utf-8"))
ok("4: the body is the HTML's UTF-8 bytes, accents included",
   is.raw(enviado$body) && identical(enviado$body, charToRaw(enc2utf8(html))) &&
     identical(rawToChar(enviado$body), enc2utf8(html)))

# Contraprueba: today's line, through the same real put_object().
enviado <- NULL
hoy <- tryCatch(aws.s3::put_object(file = charToRaw(enc2utf8(html)), object = "networks/avisos/prueba/x.html",
                                   bucket = "hopdesk-prueba-t42",
                                   headers = list(`Content-Type` = "text/html; charset=utf-8")),
                error = function(e) conditionMessage(e))
ok("contraprueba: file = charToRaw(...) (the line before T42) fails with 'invalid filename argument', nothing sent",
   identical(hoy, "invalid filename argument") && is.null(enviado))
restaurar()
ok("the network layer is restored", identical(get("s3HTTP", envir = asNamespace("aws.s3")), red_real))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
