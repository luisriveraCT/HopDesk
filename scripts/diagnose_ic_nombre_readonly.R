# =============================================================================
# scripts/diagnose_ic_nombre_readonly.R
# READ-ONLY. T47: how intercompany rows are told apart in a client's SAP
# snapshots, today and with the companies' own identity.
#
# For AR and AP it counts the rows apply_ic_filter() (R/data_pipeline.R) takes
# as intercompany TODAY -- with the client's interco v2 registry (CardCodes,
# RFCs) when it has one, or the short-name fallback when not -- and the rows
# the companies' identity in empresas.rds would take: their RFC (exact), or
# their razón social / short name / aliases normalized (accents, punctuation
# and "S.A. de C.V." out; exact, never approximate). Then both differences:
#   solo_hoy        IC today, not by the companies' identity
#   solo_identidad  IC by the companies' identity, missed today
#   conflicto       same normalized name as a company, but another RFC
# Prints counts and the client's own company initials only: no third party's
# name, no amount.
#
# Usage (from the repo root, never from a session):
#   Rscript --vanilla scripts/diagnose_ic_nombre_readonly.R <path to .Renviron> [client_id]
# Default client: "networks".
#
# Guarantees no writes: every S3 write path is replaced by stop() before
# anything is read. Prints no secret (only the bucket name).
# =============================================================================
args      <- commandArgs(trailingOnly = TRUE)
renv      <- args[1]
client_id <- if (length(args) >= 2) args[2] else "networks"
if (is.na(renv) || !file.exists(renv)) stop("Pass the path to the .Renviron to read.")
readRenviron(renv)
cat("Bucket:", Sys.getenv("S3_BUCKET"), "| client:", client_id, "| today:", format(Sys.Date()), "\n\n")

suppressPackageStartupMessages({ library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a

# ── Read-only guard ───────────────────────────────────────────────────────────
.ro <- function(...) stop("READ-ONLY diagnostic: a write was attempted.", call. = FALSE)
for (fn in c("s3saveRDS", "put_object", "delete_object", "s3write_using", "copy_object"))
  if (exists(fn, envir = asNamespace("aws.s3"), inherits = FALSE))
    utils::assignInNamespace(fn, .ro, "aws.s3")

for (e in parse("R/global.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]])[1] %in% c("S3_KEYS", "S3_KEYS_CRITICAL", "strip_accents"))
    eval(e, globalenv())
source("R/persistence.R")
suppressMessages(source("R/monedas.R"))
suppressMessages(source("R/empresas_identidad.R"))
suppressMessages(source("R/data_pipeline.R"))
suppressMessages(source("R/bancos_detect.R"))   # .norm_razon(), .empresa_nombres()
.s3_write <- .ro
for (fn in ls(globalenv(), pattern = "^(save_|modificar_)")) assign(fn, .ro, envir = globalenv())

# ── The client's companies and registry ───────────────────────────────────────
emp <- .normalize(.s3_read_with(S3_KEYS_CRITICAL$empresas, client_id = client_id), .schema_empresas)
emp <- emp[!(emp$deleted %in% TRUE) & !(emp$activa %in% FALSE), , drop = FALSE]
cmap <- .company_map_from_empresas(emp)
reg <- tryCatch(.s3_read_with(S3_KEYS$interco_v2, client_id = client_id), error = function(e) NULL)
cat(sprintf("empresas activas %d | con razón social %d | con RFC %d\n", nrow(emp),
            sum(!is.na(emp$razon_social) & nzchar(emp$razon_social)), sum(!is.na(emp$rfc) & nzchar(emp$rfc))))
rfcs_reg <- unique(toupper(trimws(unname(reg$rfcs %||% character()))))
cat(sprintf("registro IC v2: %s | RFCs en el registro %d\n\n",
            if (is.null(reg)) "no existe (corre el respaldo por nombre)" else paste("empresas", length(reg$companies)),
            sum(nzchar(rfcs_reg))))

nombres <- unique(unlist(lapply(emp$initials, .empresa_nombres, empresas_df = emp)))
norm_emp <- unique(.norm_razon(nombres)); norm_emp <- norm_emp[nzchar(norm_emp)]
rfc_emp <- unique(toupper(trimws(emp$rfc[!is.na(emp$rfc) & nzchar(emp$rfc)])))
ini_de <- function(np, rf) {
  i <- match(rf, toupper(trimws(emp$rfc)))
  j <- match(np, .norm_razon(emp$razon_social))
  ifelse(!is.na(i), emp$initials[i], emp$initials[j])
}

for (t in c("AR", "AP")) {
  d <- load_sap_snapshot(t, client_id)$data
  if (is.null(d) || !nrow(d)) { cat(t, ": sin snapshot\n"); next }
  d$.fila <- seq_len(nrow(d))
  col <- if (t == "AR") "Código de cliente" else "Código de proveedor"
  queda <- suppressMessages(apply_ic_filter(d, mode = "exclude", code_col = col,
                                            ic_codes = if (is.null(reg)) character() else build_ic_fullcodes(reg, t),
                                            ic_rfcs = rfcs_reg[nzchar(rfcs_reg)], cmap = cmap))
  hoy <- !d$.fila %in% queda$.fila
  np <- .norm_razon(d$Parte); rf <- toupper(trimws(as.character(d$RFC)))
  por_rfc <- !is.na(rf) & nzchar(rf) & rf %in% rfc_emp
  por_nombre <- np %in% norm_emp
  ident <- por_rfc | (por_nombre & !(!is.na(rf) & nzchar(rf) & !por_rfc))
  conflicto <- por_nombre & !is.na(rf) & nzchar(rf) & !por_rfc
  cat(sprintf("%s: filas %d | IC hoy %d | IC por identidad %d | solo_hoy %d | solo_identidad %d | conflicto %d\n",
              t, nrow(d), sum(hoy), sum(ident), sum(hoy & !ident), sum(ident & !hoy), sum(conflicto)))
  if (any(ident & !hoy)) { cat("  solo_identidad por empresa:"); print(table(ini_de(np, rf)[ident & !hoy])) }
}
cat("\nLectura: solo_identidad > 0 = filas entre empresas del grupo que hoy pasan como flujo de terceros.\n",
    "solo_hoy > 0 = el registro marca IC filas que la identidad de las empresas no reconoce (revisarlas).\n")
