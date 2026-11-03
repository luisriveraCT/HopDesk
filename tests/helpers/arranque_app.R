# =============================================================================
# tests/helpers/arranque_app.R
# The REAL app (app.R, not a fixture) for tests/test_arranque_app_chromote.R,
# in its own Rscript (start_bg_app), against the DEV bucket -- and writing
# nothing to S3, reading nothing from production.
#
#  - Writes: in THIS process only, aws.s3's write functions are replaced by
#    no-ops that log "[ARRANQUE] escritura omitida <fn> <key>". The process
#    ends with the test; nothing else ever runs in it (LEEME rule 5).
#  - Reads: real, from antiguedad-rds-dev only (named; every other bucket
#    answers NoSuchKey). app.R's local start copies the user accounts from
#    prod into dev ([DEV SYNC], R/global.R); here that copy can't write
#    anyway, so it is told prod has nothing and skips.
#  - ARR_MODO = "vacio": every read of a tenant's data answers "NoSuchKey"
#    except what signing in needs (hd-admin/*, <client>/usuarios.rds,
#    <client>/tiers_config.rds) -- the cold start with empty catalogs, the
#    2026-11-03 "Cargando… 14%" incident.
#  - The single-instance lock (R/global.R) is keyed on client and bucket
#    under %LOCALAPPDATA%\HopDesk; a dev app already running holds it. This
#    process takes its own LOCALAPPDATA (a temp folder): it can't overwrite
#    anything, so the lock's reason doesn't apply.
#
# ARR_RAIZ: the checkout whose app.R to run (default: this one), with this
# checkout's environment.
# =============================================================================
raiz <- normalizePath(Sys.getenv("ARR_RAIZ", "."), winslash = "/")
# Another checkout has no .Renviron files (git ignores them): take this one's,
# already resolved to dev, instead of copying secrets there. global.R then
# finds no .Renviron.local and leaves the environment as is.
if (!identical(raiz, normalizePath(".", winslash = "/"))) {
  # A folder with its own .Renviron (the deploy folder) would have global.R
  # load production's environment over this one: run a worktree of its
  # commit instead (git worktree add --detach <dir> <commit>).
  if (file.exists(file.path(raiz, ".Renviron")))
    stop("[ARRANQUE] ", raiz, " has its own .Renviron (production?): run a worktree of its commit instead")
  if (file.exists(".Renviron")) readRenviron(".Renviron")
  if (file.exists(".Renviron.local")) readRenviron(".Renviron.local")
}
setwd(raiz)
Sys.setenv(LOCALAPPDATA = normalizePath(file.path(tempdir(), "arranque_lock"), winslash = "\\", mustWork = FALSE))
dir.create(Sys.getenv("LOCALAPPDATA"), showWarnings = FALSE, recursive = TRUE)
Sys.unsetenv("HOPDESK_PRODUCTION")
modo <- Sys.getenv("ARR_MODO", "normal")
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

loadNamespace("aws.s3")
.arr_omitida <- function(nombre) {
  force(nombre)
  function(...) {
    a <- list(...)
    llave <- a$object %||% if (length(a) >= 2L) a[[2]] else "?"
    message(sprintf("[ARRANQUE] escritura omitida %s %s", nombre, if (is.character(llave)) llave[1] else "?"))
    invisible(TRUE)
  }
}
for (f in c("s3saveRDS", "put_object", "s3write_using", "s3save", "delete_object", "copy_object", "put_bucket"))
  if (exists(f, envir = asNamespace("aws.s3"), inherits = FALSE))
    utils::assignInNamespace(f, .arr_omitida(f), "aws.s3")

# Reads: only the dev bucket, by name. (Production's bucket name has no
# "prod" in it, so a pattern on the name protects nothing.) Anything else,
# the [DEV SYNC] copy included, answers NoSuchKey.
DEV <- "antiguedad-rds-dev"
.arr_cubeta_dev <- function(bucket, object) {
  if (!is.null(bucket) && !missing(bucket)) return(identical(as.character(bucket)[1], DEV))
  grepl(paste0("^s3://", DEV, "/"), as.character(object %||% "")[1])
}
.arr_leer <- function(nombre) {   # functions shaped (object, bucket, ...)
  real <- get(nombre, envir = asNamespace("aws.s3")); force(nombre)
  function(object, bucket = NULL, ...) {
    if (!.arr_cubeta_dev(bucket, object))
      stop("NoSuchKey (prueba de arranque: solo se lee ", DEV, "): ", object)
    if (identical(nombre, "s3readRDS") && identical(modo, "vacio")) {
      permitida <- grepl("^hd-admin/", object) || grepl("/(usuarios|tiers_config)[.]rds$", object)
      if (!permitida) stop("NoSuchKey (ARR_MODO = vacio): ", object)
    }
    if (is.null(bucket)) real(object = object, ...) else real(object = object, bucket = bucket, ...)
  }
}
for (f in c("s3readRDS", "get_object", "head_object", "object_exists", "save_object", "s3load"))
  if (exists(f, envir = asNamespace("aws.s3"), inherits = FALSE))
    utils::assignInNamespace(f, .arr_leer(f), "aws.s3")
.arr_listar <- function(nombre) {   # functions shaped (bucket, ...)
  real <- get(nombre, envir = asNamespace("aws.s3")); force(nombre)
  function(bucket, ...) {
    if (!identical(as.character(bucket)[1], DEV)) stop("NoSuchBucket (prueba de arranque: solo ", DEV, ")")
    real(bucket = bucket, ...)
  }
}
for (f in c("get_bucket", "get_bucket_df", "bucket_exists"))
  if (exists(f, envir = asNamespace("aws.s3"), inherits = FALSE))
    utils::assignInNamespace(f, .arr_listar(f), "aws.s3")
.arr_using_real <- get("s3read_using", envir = asNamespace("aws.s3"))
utils::assignInNamespace("s3read_using", function(FUN, ..., object, bucket = NULL, opts = NULL) {
  if (!.arr_cubeta_dev(bucket, object))
    stop("NoSuchKey (prueba de arranque: solo se lee ", DEV, "): ", object)
  .arr_using_real(FUN, ..., object = object, bucket = bucket, opts = opts)
}, "aws.s3")

app <- source(file.path(raiz, "app.R"))$value   # app.R expects the global env (S3_KEYS, ...)
cubeta <- tryCatch(.s3_bucket(), error = function(e) Sys.getenv("S3_BUCKET"))
if (!identical(cubeta, "antiguedad-rds-dev"))
  stop("[ARRANQUE] the app is not on the dev bucket (", cubeta, "): refusing to start")
message(sprintf("[ARRANQUE] app.R from %s, bucket %s, modo %s", raiz, cubeta, modo))
app
