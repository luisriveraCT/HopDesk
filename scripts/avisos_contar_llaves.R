# =============================================================================
# scripts/avisos_contar_llaves.R -- READ ONLY (frente D, paso 0)
#
# How many avisos each client holds in the old key (<cliente>/avisos.rds)
# and in the new one (<cliente>/avisos/avisos.rds), before moving the key.
# It never writes: only aws.s3::object_exists() and aws.s3::s3readRDS().
#
# Run from the repo root, with the environment already pointing at the
# bucket to count (S3_BUCKET + its AWS_* credentials), and that bucket's
# name repeated as the only argument -- it refuses to run if they differ:
#   Rscript --vanilla scripts/avisos_contar_llaves.R antiguedad-rds-dev
#
# Prints only counts per client (client ids are this machine's local log,
# never a public one).
# =============================================================================
args <- commandArgs(trailingOnly = TRUE)
bucket <- Sys.getenv("S3_BUCKET")
if (length(args) != 1L || !nzchar(bucket) || !identical(args[[1]], bucket))
  stop("Pass the bucket to count as the only argument; it must be the S3_BUCKET in the environment.",
       call. = FALSE)
suppressPackageStartupMessages(library(aws.s3))

existe <- function(k) tryCatch(isTRUE(suppressMessages(aws.s3::object_exists(k, bucket = bucket))),
                               error = function(e) NA)
leer <- function(k) tryCatch(suppressMessages(aws.s3::s3readRDS(object = k, bucket = bucket)),
                             error = function(e) structure(NA, error = conditionMessage(e)))
filas <- function(k) {
  e <- existe(k)
  if (is.na(e)) return("error")
  if (!e) return("-")
  x <- leer(k)
  if (is.data.frame(x)) as.character(nrow(x)) else "error"
}

reg <- leer("hd-admin/client_registry.rds")
if (!is.data.frame(reg) || !nrow(reg)) stop("Could not read hd-admin/client_registry.rds", call. = FALSE)
cids <- sort(unique(tolower(trimws(reg$client_id[!is.na(reg$client_id) & nzchar(reg$client_id)]))))

res <- data.frame(
  cliente   = cids,
  estado    = reg$status[match(cids, tolower(trimws(reg$client_id)))],
  vieja     = vapply(cids, function(c) filas(paste0(c, "/avisos.rds")), ""),
  nueva     = vapply(cids, function(c) filas(paste0(c, "/avisos/avisos.rds")), ""),
  preferencias = vapply(cids, function(c) filas(paste0(c, "/preferencias.rds")), ""),
  stringsAsFactors = FALSE, row.names = NULL)
cat("bucket:", bucket, "| clientes en el registro:", length(cids), "\n")
cat("filas por llave ('-' = no existe, 'error' = no se pudo leer):\n")
print(res, row.names = FALSE)
