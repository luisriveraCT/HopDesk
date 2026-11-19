# =============================================================================
# scripts/diagnose_cm30_grupos_bancos_readonly.R
# READ-ONLY. CM30 (paso 0): does any client still depend on the two files
# that keep company initials outside the rename cascade?
#   - grupos.rds `companies`: how many groups, how many with a non-empty
#     companies list (nothing in the app reads it today);
#   - bancos.rds: how many rows, how many active; the legacy PPL origin
#     account fallback (.get_cuenta_origen_legacy) reads it only when the
#     client has NO ctas_cuentas rows, so the script also counts ctas_cuentas
#     and bancos_cuentas rows and says whether that fallback can be reached.
# Every read is strict: a file that cannot be read says so, never "0".
# Prints counts only -- never an initial, a name, an account or a config.
#
# Usage (from the repo root, never from a session):
#   Rscript --vanilla scripts/diagnose_cm30_grupos_bancos_readonly.R <path to .Renviron> [client_id ...]
# Default client: "networks". The legacy fallback reads bancos.rds WITHOUT a
# client_id, i.e. from the deployment's own CLIENT_ID: pass that client too.
#
# Guarantees no writes: every S3 write path -- the app's and aws.s3's own --
# is replaced by stop() before anything is read. Prints no secret (only the
# bucket name and CLIENT_ID).
# =============================================================================
`%||%` <- function(a, b) if (is.null(a)) b else a
args     <- commandArgs(trailingOnly = TRUE)
renv     <- args[1]
clientes <- if (length(args) >= 2) args[-1] else "networks"
renvs <- strsplit(renv %||% NA_character_, ",", fixed = TRUE)[[1]]
if (anyNA(renvs) || !all(file.exists(renvs))) stop("Pass the path(s) to the .Renviron to read.")
for (r in renvs) readRenviron(r)
if (nzchar(Sys.getenv("DIAG_BUCKET"))) Sys.setenv(S3_BUCKET = Sys.getenv("DIAG_BUCKET"))
cat("Bucket:", Sys.getenv("S3_BUCKET"), "| CLIENT_ID:", Sys.getenv("CLIENT_ID"),
    "| clients:", paste(clientes, collapse = ", "), "\n\n")

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

# Strict read: the object, NULL when S3 says it does not exist, or the
# string "no se pudo leer" when S3 could not answer.
leer <- function(key, cid) tryCatch(suppressMessages(.s3_leer_para_escribir(key, cid)),
                                    error = function(e) "no se pudo leer")
filas <- function(x) if (identical(x, "no se pudo leer")) x else if (is.null(x)) "no existe" else
  if (is.data.frame(x)) as.character(nrow(x)) else "no es tabla"

for (cid in clientes) {
  cat("── cliente:", cid, "\n")
  grp <- leer(S3_KEYS$grupos, cid)
  cat("  grupos.rds          filas:", filas(grp))
  if (is.data.frame(grp) && "companies" %in% names(grp)) {
    con <- vapply(grp$companies, function(v) {
      a <- tryCatch(jsonlite::fromJSON(v %||% "[]"), error = function(e) character())
      length(a) > 0L
    }, logical(1))
    cat(" | con lista companies no vacía:", sum(con))
  }
  cat("\n")
  # Does each group's companies list say the same as empresas.group_id? The
  # list is dropped from the schema in CM30 (.normalize keeps only schema
  # columns), so anything it says that group_id does not would be lost.
  # Counts only: initials in the list, how many are companies whose group_id
  # is this group, and how many are not (missing company, other group, none).
  emp <- leer(S3_KEYS$empresas, cid)
  if (is.data.frame(grp) && nrow(grp) && "companies" %in% names(grp)) {
    if (!is.data.frame(emp)) {
      cat("  empresas.rds:", filas(emp), "-- no se puede comparar con group_id\n")
    } else {
      ini_emp <- toupper(trimws(as.character(emp$initials)))
      gid_emp <- as.character(emp$group_id)
      for (i in seq_len(nrow(grp))) {
        a <- unique(toupper(trimws(tryCatch(jsonlite::fromJSON(grp$companies[i] %||% "[]"),
                                            error = function(e) character()))))
        if (!length(a)) next
        en_grupo <- vapply(a, function(x) any(ini_emp == x & !is.na(gid_emp) & gid_emp == grp$id[i]), logical(1))
        sin_emp  <- !a %in% ini_emp
        cat(sprintf("  grupo %d de %d: %d en companies | coinciden con group_id: %d | empresa inexistente: %d | en otro grupo o sin grupo: %d\n",
                    i, nrow(grp), length(a), sum(en_grupo), sum(sin_emp), sum(!en_grupo & !sin_emp)))
      }
      cat(sprintf("  empresas con group_id de un grupo que existe: %d de %d\n",
                  sum(!is.na(gid_emp) & gid_emp %in% grp$id), length(gid_emp)))
    }
  }

  ban <- leer(S3_KEYS$bancos, cid)
  cat("  bancos.rds          filas:", filas(ban))
  if (is.data.frame(ban) && "activa" %in% names(ban))
    cat(" | activas:", sum(!is.na(ban$activa) & ban$activa))
  cat("\n")

  cta <- leer(S3_KEYS$ctas_cuentas, cid)
  bcu <- leer(S3_KEYS$bancos_cuentas, cid)
  cat("  ctas_cuentas.rds    filas:", filas(cta), "\n")
  cat("  bancos_cuentas.rds  filas:", filas(bcu), "\n")
  sin_ctas <- is.null(cta) || (is.data.frame(cta) && !nrow(cta))
  cat("  ¿puede llegar al respaldo heredado? (sin ctas_cuentas):",
      if (identical(cta, "no se pudo leer")) "no se sabe (lectura fallida)" else if (sin_ctas) "SÍ" else "no", "\n\n")
}
cat("Fin. Solo lectura; no se escribió nada.\n")
