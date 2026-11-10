# =============================================================================
# scripts/diagnose_pasivos_tabla_vacia_readonly.R
# READ-ONLY. Why is Pasivos > Tabla empty while Lista de pasivos loads?
# (CM21, 2026-11-10: prod logs repeat "[pasivos_table] build_long error:
# missing value where TRUE/FALSE needed", and long_df()'s tryCatch turns it
# into an empty table.)
#
# Runs pasivos_table_build_long() on the client's real data with the same
# inputs and filters as long_df() (R/pasivos_table_module.R): provisions,
# manual_inv with ledger == "AP", confirmaciones_vigentes(bancos_confirmados),
# liabilities. Then it finds the rows that make it fail, one at a time, and
# prints only their ids, dates and estados -- no amounts, no names.
#
# Usage (from the repo root, never from a session):
#   Rscript --vanilla scripts/diagnose_pasivos_tabla_vacia_readonly.R \
#           <path to .Renviron> [client_id]
# Default client: "networks".
#
# Guarantees no writes: every S3 write path -- the app's and aws.s3's own --
# is replaced by stop() before anything is read. Prints no secret (only the
# bucket name).
# =============================================================================
args      <- commandArgs(trailingOnly = TRUE)
renv      <- args[1]
client_id <- if (length(args) >= 2) args[2] else "networks"
if (is.na(renv) || !file.exists(renv)) stop("Pass the path to the .Renviron to read.")
readRenviron(renv)
cat("Bucket:", Sys.getenv("S3_BUCKET"), "| client:", client_id, "| today:", format(Sys.Date()), "\n\n")

suppressPackageStartupMessages({ library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a
options(keep.source = TRUE)

# ── Read-only guard ───────────────────────────────────────────────────────────
.ro <- function(...) stop("READ-ONLY diagnostic: a write was attempted.", call. = FALSE)
for (fn in c("s3saveRDS", "put_object", "delete_object", "s3write_using", "copy_object"))
  if (exists(fn, envir = asNamespace("aws.s3"), inherits = FALSE))
    utils::assignInNamespace(fn, .ro, "aws.s3")

# S3_KEYS and strip_accents live in R/global.R, which is not sourced (it
# starts the app's preload).
for (e in parse("R/global.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]])[1] %in% c("S3_KEYS", "strip_accents"))
    eval(e, globalenv())
source("R/persistence.R")
source("R/pasivos_schemas.R")
source("R/pasivos_persistence.R")
source("R/bancos_persistence.R")
source("R/pasivos_table_pivot.R", keep.source = TRUE)
.s3_write <- .ro
for (fn in ls(globalenv(), pattern = "^(save_|modificar_)")) assign(fn, .ro, envir = globalenv())

# ── Data, as long_df() gets it ────────────────────────────────────────────────
liabs <- load_pasivos_liabilities(client_id = client_id)
provs <- load_pasivos_provisions(client_id = client_id)
mi    <- load_manual(client_id = client_id)
bc    <- load_bancos_confirmados(client_id = client_id)
mi_ap <- if (nrow(mi) && "ledger" %in% names(mi)) mi[mi$ledger == "AP", , drop = FALSE] else mi
bc_ok <- confirmaciones_vigentes(bc)
liabs_v <- liabs   # staff/principal: no company filter
cat(sprintf("liabilities %d | provisions %d | manual_inv %d (AP %d, ledger NA %d) | confirmaciones vigentes %d\n",
            nrow(liabs), nrow(provs), nrow(mi), nrow(mi_ap), sum(is.na(mi$ledger)), nrow(bc_ok)))
filtros <- list(empresa = character(0), categorias = c("regular", "financiero", "tarjeta"),
                subcategorias = character(0), currency = "All", search = "",
                granularity = "day", provision_type = "todas")
correr <- function(p = provs, m = mi_ap)
  pasivos_table_build_long(p, m, bc_ok, liabs_v, filtros)

# ── 1. Does it fail, and where ────────────────────────────────────────────────
donde <- NULL
res <- tryCatch(withCallingHandlers(correr(), error = function(e) {
  calls <- sys.calls()
  refs  <- vapply(calls, function(cl) {
    sr <- attr(cl, "srcref")
    if (is.null(sr)) NA_character_ else
      paste0(basename(attr(sr, "srcfile")$filename %||% "?"), ":", sr[1])
  }, "")
  donde <<- utils::tail(stats::na.omit(refs), 3)
}), error = function(e) e)
if (inherits(res, "error")) {
  cat("\n1. build_long FAILS:", conditionMessage(res), "\n   last source lines on the stack:",
      paste(donde, collapse = " <- "), "\n")
} else {
  cat("\n1. build_long works:", nrow(res), "rows. The bug is not in these data.\n")
}

# ── 2. Which rows: provisions alone, then each manual item alone ──────────────
falla <- function(expr) inherits(tryCatch({ force(expr); NULL }, error = function(e) e), "error")
cat("\n2a. provisions with no manual items:", if (falla(correr(m = mi_ap[0, ]))) "FAILS" else "ok", "\n")

conv_ids <- provs$id[provs$estado %in% c("converted", "item_confirmed")]
mi_link  <- mi_ap[!is.na(mi_ap$provision_id) & mi_ap$provision_id %in% conv_ids, , drop = FALSE]
cat("2b. AP manual items linked to a converted/item_confirmed provision:", nrow(mi_link), "\n")
malas <- Filter(function(k) falla(correr(m = mi_link[k, , drop = FALSE])), seq_len(nrow(mi_link)))
cat("    of them, failing alone:", length(malas), "\n")
if (length(malas)) {
  m <- mi_link[unlist(malas), , drop = FALSE]
  p <- provs[match(m$provision_id, provs$id), , drop = FALSE]
  vigente <- m$provision_id %in% bc_ok$provision_id
  print(data.frame(
    manual_id        = m$id,
    empresa          = m$empresa %||% NA,
    fecha_venc       = format(m$`Fecha de vencimiento`),
    fecha_factura    = if ("Fecha de factura" %in% names(m)) format(m$`Fecha de factura`) else NA,
    created_at       = if ("created_at" %in% names(m)) format(m$created_at) else NA,
    provision_id     = m$provision_id,
    prov_estado      = p$estado,
    prov_fecha_ef    = format(p$fecha_efectiva),
    prov_liability   = p$liability_id,
    pagada           = vigente,
    stringsAsFactors = FALSE), row.names = FALSE)
}

# ── 3. Other NA traps the task asked about ────────────────────────────────────
dup_conv <- conv_ids[duplicated(conv_ids)]
cat("\n3a. converted/item_confirmed provision ids repeated:", length(dup_conv), "\n")
cat("3b. AP manual items with NA 'Fecha de vencimiento' (any, linked or not):",
    sum(is.na(mi_ap$`Fecha de vencimiento`)), "\n")
cat("3c. manual_inv columns:", paste(names(mi), collapse = ", "), "\n")

# ── 4. With the failing rows left out, how much would the table show ─────────
if (length(malas)) {
  sin <- mi_ap[!mi_ap$id %in% mi_link$id[unlist(malas)], , drop = FALSE]
  r <- tryCatch(correr(m = sin), error = function(e) e)
  cat("\n4. without those rows:", if (inherits(r, "error")) paste("still FAILS:", conditionMessage(r)) else
      paste(nrow(r), "rows,", length(unique(r$row_id)), "liabilities/rows"), "\n")
}
# ── 5. Where the bad rows came from: their provision and every confirmation ───
na_mi <- mi[is.na(mi$`Fecha de vencimiento`), , drop = FALSE]
for (i in seq_len(nrow(na_mi))) {
  m <- na_mi[i, ]
  cat("\n5.", i, "manual item", m$id, "\n")
  cols <- intersect(c("ledger", "Empresa", "Moneda", "Documento", "Factura", "Codigo", "created_by",
                      "created_at", "updated_at", "provision_id", "liability_id", "referencia"), names(m))
  for (cc in cols) cat(sprintf("   %-13s %s\n", cc, format(m[[cc]][1])))
  cat("   Importe is NA:", is.na(m$Importe[1]), "\n")
  if (!is.na(m$provision_id)) {
    p <- provs[provs$id %in% m$provision_id, , drop = FALSE]
    pc <- intersect(c("estado", "origin", "empresa", "liability_id", "fecha_calculada", "fecha_efectiva",
                      "fecha_efectiva_override", "created_by", "created_at", "last_edited_by",
                      "last_edited_at"), names(p))
    cat("   provision:\n"); print(as.data.frame(p[, pc]), row.names = FALSE)
    b <- bc[bc$provision_id %in% m$provision_id, , drop = FALSE]
    bcols <- intersect(c("confirmacion_id", "eliminado", "action", "fecha", "confirmado_at",
                         "eliminado_at", "recovered_at", "reverses_confirmacion_id"), names(b))
    cat("   bancos_confirmados rows for it (any):", nrow(b), "\n")
    if (nrow(b)) print(as.data.frame(b[, bcols]), row.names = FALSE)
  }
}
cat("\nbancos_confirmados columns:", paste(names(bc), collapse = ", "), "\n")
cat("\nDone. Nothing was written.\n")
