# =============================================================================
# scripts/diagnose_cm38b_empresas_cruzadas_readonly.R
# READ-ONLY. CM38b: did a staff session ever write one client's companies
# into another client's empresas.rds? (.empresas_local kept the companies of
# the client where staff last saved; after a jump, the next save wrote that
# list into the new client.) For every client in hd-admin/client_registry.rds
# (plus any passed as arguments):
#   - how many companies its empresas.rds has (active / deleted), or that it
#     could not be read (never "0"), or that it has no file;
#   - for every PAIR of clients: how many company ids and how many RFCs both
#     have, how many of the shared ids have the same initials in both, and
#     whether the two lists are identical (same set of ids) -- the
#     footprint of the bug.
# A company id is a UUID made when the company is created, so the same id in
# two clients can only be a copy. An RFC can legitimately repeat (the same
# company set up as two clients), so it is counted apart.
# Prints client ids and counts only -- never a company's name, RFC or id.
#
# Usage (from the repo root, never from a session):
#   Rscript --vanilla scripts/diagnose_cm38b_empresas_cruzadas_readonly.R <path to .Renviron> [client_id ...]
#
# Guarantees no writes: every S3 write path -- the app's and aws.s3's own --
# is replaced by stop() before anything is read. Prints no secret (only the
# bucket name and CLIENT_ID).
# =============================================================================
`%||%` <- function(a, b) if (is.null(a)) b else a
args  <- commandArgs(trailingOnly = TRUE)
renv  <- args[1]
extra <- if (length(args) >= 2) tolower(args[-1]) else character()
renvs <- strsplit(renv %||% NA_character_, ",", fixed = TRUE)[[1]]
if (anyNA(renvs) || !all(file.exists(renvs))) stop("Pass the path(s) to the .Renviron to read.")
for (r in renvs) readRenviron(r)
if (nzchar(Sys.getenv("DIAG_BUCKET"))) Sys.setenv(S3_BUCKET = Sys.getenv("DIAG_BUCKET"))
cat("Bucket:", Sys.getenv("S3_BUCKET"), "| CLIENT_ID:", Sys.getenv("CLIENT_ID"), "\n\n")

suppressPackageStartupMessages({ library(dplyr); library(tibble) })

# ── Read-only guard ───────────────────────────────────────────────────────────
.ro <- function(...) stop("READ-ONLY diagnostic: a write was attempted.", call. = FALSE)
for (fn in c("s3saveRDS", "put_object", "delete_object", "s3write_using", "copy_object"))
  if (exists(fn, envir = asNamespace("aws.s3"), inherits = FALSE))
    utils::assignInNamespace(fn, .ro, "aws.s3")

for (e in parse("R/global.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "S3_KEYS"))
    eval(e, globalenv())
suppressMessages(source("R/persistence.R"))
.s3_write <- .ro
for (fn in ls(globalenv(), pattern = "^(save_|modificar_)")) assign(fn, .ro, envir = globalenv())

# Strict read: the object, NULL (no file), or "no se pudo leer".
leer <- function(key, cid) tryCatch(suppressMessages(.s3_leer_para_escribir(key, cid)),
                                    error = function(e) "no se pudo leer")
falla <- function(x) identical(x, "no se pudo leer")

reg <- leer("client_registry.rds", "hd-admin")
if (falla(reg) || !is.data.frame(reg)) {
  cat("client_registry.rds: NO SE PUDO LEER -- uso solo los clientes pasados como argumento\n")
  clientes <- extra
} else {
  clientes <- unique(c(tolower(trimws(reg$client_id[!is.na(reg$client_id)])), extra))
  cat(sprintf("client_registry: %d clientes (%s)\n\n", nrow(reg),
              paste(sprintf("%s:%s", reg$client_id, reg$status %||% "?"), collapse = ", ")))
}
clientes <- setdiff(clientes[nzchar(clientes)], "hd-admin")
if (!length(clientes)) stop("No clients to check.")

norm <- function(x) toupper(gsub("[^A-Za-z0-9&]", "", as.character(x)))
datos <- list()
for (cid in clientes) {
  x <- leer(S3_KEYS$empresas, cid)
  if (falla(x)) { cat(sprintf("── %-14s empresas.rds: NO SE PUDO LEER\n", cid)); next }
  if (is.null(x)) { cat(sprintf("── %-14s empresas.rds: no existe\n", cid)); next }
  df <- .normalize(x, .schema_empresas)
  borr <- !is.na(df$deleted) & df$deleted
  cat(sprintf("── %-14s empresas: %d (vivas %d, borradas %d) | ids repetidos dentro: %d | sin id: %d\n",
              cid, nrow(df), sum(!borr), sum(borr), sum(duplicated(df$id[!is.na(df$id)])), sum(is.na(df$id) | !nzchar(df$id))))
  datos[[cid]] <- df
}

cat("\nPares de clientes con algo en común (ids = copia segura; RFC = puede ser legítimo):\n")
n_pares <- 0L
cs <- names(datos)
for (i in seq_along(cs)) for (j in seq_along(cs)) if (i < j) {
  a <- datos[[cs[i]]]; b <- datos[[cs[j]]]
  ids <- intersect(a$id[!is.na(a$id) & nzchar(a$id)], b$id[!is.na(b$id) & nzchar(b$id)])
  ra <- norm(a$rfc); rb <- norm(b$rfc)
  rfcs <- intersect(ra[nzchar(ra) & !is.na(a$rfc)], rb[nzchar(rb) & !is.na(b$rfc)])
  if (!length(ids) && !length(rfcs)) next
  n_pares <- n_pares + 1L
  mismas_ini <- sum(a$initials[match(ids, a$id)] == b$initials[match(ids, b$id)], na.rm = TRUE)
  identicas <- length(ids) > 0 && setequal(a$id, b$id)
  cat(sprintf("  %s <-> %s: ids compartidos %d (mismas iniciales %d) | RFC compartidos %d | listas idénticas: %s\n",
              cs[i], cs[j], length(ids), mismas_ini, length(rfcs), if (identicas) "SÍ" else "no"))
}
if (!n_pares) cat("  ninguno\n")
cat("\nFin (solo lectura).\n")
