# =============================================================================
# scripts/diagnose_cm34c_politicas_cruzadas_readonly.R
# READ-ONLY. CM34c: did one client's policies or supplier links land in
# another client? (Ajustes › Políticas: with the session's catalog empty --
# a client without policies -- .get_catalog() fell back to
# load_policy_catalog() WITHOUT client_id, the CLIENT_ID prefix's catalog, and
# creating the first policy saved that catalog plus the new one into the
# client being viewed.) For every client in hd-admin/client_registry.rds
# (plus any passed as arguments):
#   - rows of policy_catalog, partner_policies and parte_alias_map, or that a
#     file could not be read (never "0"), or that it does not exist;
#   - partner_policies rows whose policy_id is not in the client's own
#     catalog (orphans);
#   - for every PAIR of clients:
#       policy_catalog  -- shared policy ids (a UUID made on creation: the
#                          same id in two clients can only be a copy),
#                          identical sets, and whether one client's catalog
#                          holds ALL of the other's (the footprint: the other
#                          catalog plus the first policy created);
#       partner_policies -- shared policy_ids assigned;
#       parte_alias_map -- shared links (Parte + alias, case-insensitive; a
#                          supplier may legitimately exist in two clients, so
#                          it is counted apart) and identical sets.
# Prints client ids and counts only -- never a policy's name, a supplier, an
# alias or an id.
#
# Usage (from the repo root, never from a session):
#   Rscript --vanilla scripts/diagnose_cm34c_politicas_cruzadas_readonly.R <path to .Renviron> [client_id ...]
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
filas <- function(x) if (falla(x)) "NO SE PUDO LEER" else if (is.null(x)) "no existe" else
  if (!is.data.frame(x)) "NO ES TABLA" else as.character(nrow(x))
col <- function(x, nm) if (is.data.frame(x) && nm %in% names(x)) as.character(x[[nm]]) else character()
llena <- function(v) unique(v[!is.na(v) & nzchar(v)])

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

d <- list()
for (cid in clientes) {
  cat_ <- leer(S3_KEYS$policy_catalog, cid)
  pp   <- leer(S3_KEYS$partner_policies, cid)
  am   <- leer(S3_KEYS$parte_alias_map, cid)
  ids_cat <- llena(col(cat_, "id"))
  huerf <- if (is.data.frame(pp) && is.data.frame(cat_)) sum(!is.na(pp$policy_id) & !(pp$policy_id %in% ids_cat)) else NA
  cat(sprintf("── %-14s policy_catalog: %s (ids repetidos %d) | partner_policies: %s (sin política en su catálogo: %s) | parte_alias_map: %s\n",
              cid, filas(cat_), sum(duplicated(col(cat_, "id")[nzchar(col(cat_, "id"))])),
              filas(pp), if (is.na(huerf)) "-" else huerf, filas(am)))
  d[[cid]] <- list(
    cat = if (is.data.frame(cat_)) ids_cat else NULL,
    pp  = if (is.data.frame(pp)) llena(col(pp, "policy_id")) else NULL,
    am  = if (is.data.frame(am)) llena(toupper(trimws(paste(col(am, "Parte"), col(am, "alias"), sep = "\x1f")))) else NULL)
}

cat("\nPares de clientes con algo en común:\n")
n_pares <- 0L
cs <- names(d)
for (i in seq_along(cs)) for (j in seq_along(cs)) if (i < j) {
  a <- d[[cs[i]]]; b <- d[[cs[j]]]
  ic <- intersect(a$cat, b$cat); ip <- intersect(a$pp, b$pp); ia <- intersect(a$am, b$am)
  if (!length(ic) && !length(ip) && !length(ia)) next
  n_pares <- n_pares + 1L
  contiene <- c(if (length(a$cat) && all(a$cat %in% b$cat)) sprintf("%s tiene TODO el de %s", cs[j], cs[i]),
                if (length(b$cat) && all(b$cat %in% a$cat)) sprintf("%s tiene TODO el de %s", cs[i], cs[j]))
  cat(sprintf("  %s <-> %s\n", cs[i], cs[j]))
  cat(sprintf("    policy_catalog: ids compartidos %d | idénticos: %s%s\n", length(ic),
              if (length(ic) && setequal(a$cat, b$cat)) "SÍ" else "no",
              if (length(contiene)) paste0(" | ", paste(contiene, collapse = "; ")) else ""))
  cat(sprintf("    partner_policies: policy_id compartidos %d\n", length(ip)))
  cat(sprintf("    parte_alias_map: vínculos compartidos %d (puede ser legítimo) | idénticos: %s\n", length(ia),
              if (length(ia) && setequal(a$am, b$am)) "SÍ" else "no"))
}
if (!n_pares) cat("  ninguno\n")
cat("\nFin (solo lectura).\n")
