# =============================================================================
# tests/helpers/s3_memoria.R
# In-memory S3 for persistence tests (CM09). Source it at the top of a test:
#   - .mem: the store (full key -> object); .mem_falla: full keys whose read
#     fails; .mem_escrituras: how many writes reached the store;
#     .mem_falla_escritura: full keys whose WRITE fails (s3saveRDS and
#     put_object; nothing is stored, nothing counted); .mem_falla_lista:
#     prefixes whose listing (get_bucket) fails;
#   - aws.s3's s3readRDS / s3saveRDS / object_exists, and the three the app
#     also uses -- put_object, get_object, get_bucket -- are swapped and
#     restored when the test's R process exits (reg.finalizer on .mem,
#     onexit = TRUE) and by s3_memoria_restaurar(), which every test should
#     call at the end. None of the six ever reaches the network;
#   - R/persistence.R is sourced, with S3_KEYS from R/global.R.
#
# The six keep aws.s3's contract, only as far as the app uses it:
#   s3saveRDS(x, object, bucket)   stores the R object
#   s3readRDS(object, bucket)      the R object -- also of a .rds file put
#                                  with put_object (.s3_write's fallback, the
#                                  offline queue's replay)
#   put_object(file, object, bucket, headers)  stores a file's bytes (a path
#                                  or a raw vector); TRUE
#   get_object(object, bucket)     the stored bytes (raw); of something saved
#                                  with s3saveRDS, its .rds bytes
#   get_bucket(bucket, prefix, max) the objects under prefix, sorted by key,
#                                  at most max; each has [["Key"]], Size,
#                                  LastModified (what the app reads: Key)
#   object_exists(object, bucket)  TRUE/FALSE
# A missing key stops with "NoSuchKey", as S3 does.
# =============================================================================
suppressMessages({ library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a
.mem <- new.env(); .mem_falla <- character(); .mem_escrituras <- 0L
.mem_falla_escritura <- character(); .mem_falla_lista <- character()
.mem_orig <- lapply(stats::setNames(nm = c("s3readRDS", "s3saveRDS", "object_exists", "put_object", "get_object",
                                           "get_bucket")),
                    function(nm) get(nm, asNamespace("aws.s3")))
s3_memoria_restaurar <- function()
  for (nm in names(.mem_orig)) utils::assignInNamespace(nm, .mem_orig[[nm]], "aws.s3")
reg.finalizer(.mem, function(e) s3_memoria_restaurar(), onexit = TRUE)

# What put_object stored: the bytes, marked, so s3readRDS can tell them from
# an R object saved with s3saveRDS (in S3 both are just bytes).
.mem_bytes <- function(b) structure(list(bytes = b), class = "s3_memoria_bytes")
.mem_rds_bytes <- function(x) {
  tf <- tempfile(fileext = ".rds"); on.exit(unlink(tf))
  saveRDS(x, tf); readBin(tf, "raw", file.info(tf)$size)
}
.mem_leer <- function(object) {
  if (object %in% .mem_falla) stop("simulated S3 timeout")
  if (!exists(object, envir = .mem, inherits = FALSE)) stop("NoSuchKey")
  get(object, envir = .mem)
}
.mem_escribir <- function(object, valor) {
  if (object %in% .mem_falla_escritura) stop("simulated S3 write failure")
  .mem_escrituras <<- .mem_escrituras + 1L; assign(object, valor, envir = .mem); invisible(TRUE)
}

utils::assignInNamespace("s3readRDS", function(object, bucket, ...) {
  x <- .mem_leer(object)
  if (!inherits(x, "s3_memoria_bytes")) return(x)
  tf <- tempfile(fileext = ".rds"); on.exit(unlink(tf))
  writeBin(x$bytes, tf); readRDS(tf)
}, "aws.s3")
utils::assignInNamespace("s3saveRDS", function(x, object, bucket, ...) .mem_escribir(object, x), "aws.s3")
utils::assignInNamespace("object_exists", function(object, bucket, ...)
  exists(object, envir = .mem, inherits = FALSE), "aws.s3")
utils::assignInNamespace("put_object", function(file, object, bucket, ...) {
  b <- if (is.raw(file)) file else readBin(file, "raw", file.info(file)$size)
  .mem_escribir(object, .mem_bytes(b))
}, "aws.s3")
utils::assignInNamespace("get_object", function(object, bucket, ...) {
  x <- .mem_leer(object)
  if (inherits(x, "s3_memoria_bytes")) x$bytes else .mem_rds_bytes(x)
}, "aws.s3")
utils::assignInNamespace("get_bucket", function(bucket, prefix = NULL, max = NULL, ...) {
  prefix <- prefix %||% ""
  if (prefix %in% .mem_falla_lista) stop("simulated S3 timeout (listing)")
  llaves <- sort(ls(.mem, all.names = TRUE), method = "radix")   # by bytes, as S3 lists (not the locale's order)
  llaves <- llaves[startsWith(llaves, prefix)]
  if (!is.null(max) && length(llaves) > max) llaves <- utils::head(llaves, max)
  objetos <- lapply(llaves, function(k) {
    x <- get(k, envir = .mem)
    tam <- length(if (inherits(x, "s3_memoria_bytes")) x$bytes else .mem_rds_bytes(x))
    structure(list(Key = k, LastModified = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
                   ETag = "", Size = tam, StorageClass = "STANDARD", Bucket = bucket), class = "s3_object")
  })
  structure(stats::setNames(objetos, rep("Contents", length(objetos))), class = "s3_bucket")
}, "aws.s3")

Sys.setenv(S3_BUCKET = "fake-bucket", CLIENT_ID = "acme")
for (e in parse("R/global.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "S3_KEYS"))
    eval(e, globalenv())
suppressMessages(source("R/persistence.R"))

s3_memoria_reset <- function() {
  rm(list = ls(.mem, all.names = TRUE), envir = .mem); .mem_falla <<- character(); .mem_escrituras <<- 0L
  .mem_falla_escritura <<- character(); .mem_falla_lista <<- character()
  for (env in Filter(Negate(is.null), list(get0(".s3_preload_cache"), get0(".s3_missing_cache"), get0(".s3_filas"))))
    rm(list = ls(env), envir = env)
  if (exists(".app_data_cache")) {   # a list in the app (R/global.R); an env in some tests
    if (is.environment(.app_data_cache)) rm(list = ls(.app_data_cache), envir = .app_data_cache)
    else assign(".app_data_cache", list(), envir = globalenv())
  }
}
