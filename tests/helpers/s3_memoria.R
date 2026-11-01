# =============================================================================
# tests/helpers/s3_memoria.R
# In-memory S3 for persistence tests (CM09). Source it at the top of a test:
#   - .mem: the store (full key -> object); .mem_falla: full keys whose read
#     fails; .mem_escrituras: how many writes reached the store;
#   - aws.s3's s3readRDS / s3saveRDS / object_exists are swapped and restored
#     when the test's R process exits (reg.finalizer on .mem, onexit = TRUE)
#     and by s3_memoria_restaurar(), which every test should call at the end;
#   - R/persistence.R is sourced, with S3_KEYS from R/global.R.
# =============================================================================
suppressMessages({ library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a
.mem <- new.env(); .mem_falla <- character(); .mem_escrituras <- 0L
.mem_orig <- list(s3readRDS = get("s3readRDS", asNamespace("aws.s3")),
                  s3saveRDS = get("s3saveRDS", asNamespace("aws.s3")),
                  object_exists = get("object_exists", asNamespace("aws.s3")))
s3_memoria_restaurar <- function()
  for (nm in names(.mem_orig)) utils::assignInNamespace(nm, .mem_orig[[nm]], "aws.s3")
reg.finalizer(.mem, function(e) s3_memoria_restaurar(), onexit = TRUE)
utils::assignInNamespace("s3readRDS", function(object, bucket, ...) {
  if (object %in% .mem_falla) stop("simulated S3 timeout")
  if (!exists(object, envir = .mem, inherits = FALSE)) stop("NoSuchKey")
  get(object, envir = .mem)
}, "aws.s3")
utils::assignInNamespace("s3saveRDS", function(x, object, bucket, ...) {
  .mem_escrituras <<- .mem_escrituras + 1L; assign(object, x, envir = .mem); invisible(TRUE)
}, "aws.s3")
utils::assignInNamespace("object_exists", function(object, bucket, ...)
  exists(object, envir = .mem, inherits = FALSE), "aws.s3")

Sys.setenv(S3_BUCKET = "fake-bucket", CLIENT_ID = "acme")
for (e in parse("R/global.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "S3_KEYS"))
    eval(e, globalenv())
suppressMessages(source("R/persistence.R"))

s3_memoria_reset <- function() {
  rm(list = ls(.mem), envir = .mem); .mem_falla <<- character(); .mem_escrituras <<- 0L
  for (env in Filter(Negate(is.null), list(get0(".s3_preload_cache"), get0(".s3_missing_cache"), get0(".s3_filas"))))
    rm(list = ls(env), envir = env)
  if (exists(".app_data_cache")) rm(list = ls(.app_data_cache), envir = .app_data_cache)
}
