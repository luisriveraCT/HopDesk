# =============================================================================
# scripts/diagnose_cm35_req_estatus_readonly.R
# READ-ONLY. CM35: what the requisitions' new status and events look like on
# real data, before publishing. Per client:
#   - how many requisitions, how many in each deduced stage (sin_provision
#     "Abierta", provisionada, por_pagar, en_agenda, pagada, papelera
#     "Cancelada") -- the paid ones come back to the list with CM35;
#   - how many events requisiciones_eventos_en() deduces, by tipo, and how
#     many status events have no author (por = NA: written before CM35, or a
#     fallback user) -- by stage;
#   - how many confirmations carry a requisition's provision, and how many
#     of them have no confirmado_por (all of them, before CM35);
#   - the check that matters: the last status event of every requisition
#     lands on the stage the list shows (mismatches counted, never listed);
#   - how long the deduction takes (requisiciones_vista_en and
#     requisiciones_eventos_en), and the size of pasivos_audit.
# Every read is strict: a file that cannot be read says so, never "0".
# Prints counts and timings only -- never a title, a name, an amount or an id.
#
# Usage (from the repo root, never from a session):
#   Rscript --vanilla scripts/diagnose_cm35_req_estatus_readonly.R <path to .Renviron> [client_id ...]
# Default client: "networks".
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
for (e in parse("R/erp_connector_registry.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "SAP_B1_ERP_TYPE"))
    eval(e, globalenv())
suppressMessages({
  source("R/persistence.R"); source("R/bancos_persistence.R"); source("R/pasivos_schemas.R")
  source("R/pasivos_persistence.R"); source("R/agenda_filas.R"); source("R/requisiciones_persistence.R")
})
.s3_write <- .ro
for (fn in ls(globalenv(), pattern = "^(save_|modificar_)")) assign(fn, .ro, envir = globalenv())

# Strict reads: a table, or the string "no se pudo leer".
leer <- function(key, cid, schema) tryCatch(suppressMessages(leer_tabla_para_escribir(key, cid, schema)),
                                            error = function(e) "no se pudo leer")
falla <- function(x) identical(x, "no se pudo leer")
cuenta <- function(x, niveles) { t <- table(factor(x, levels = niveles)); paste(sprintf("%s %d", names(t), as.integer(t)), collapse = " · ") }

for (cid in clientes) {
  cat("── cliente:", cid, "\n")
  t <- list(
    reqs      = leer(REQ_KEYS$requisiciones, cid, .schema_requisiciones),
    historial = leer(REQ_KEYS$historial,     cid, .schema_req_historial),
    carpetas  = leer(REQ_KEYS$carpetas,      cid, .schema_req_carpetas),
    evid      = leer(REQ_KEYS$evidencia,     cid, .schema_req_evidencia),
    coms      = leer(REQ_KEYS$comentarios,   cid, .schema_req_comentarios),
    provs     = leer(S3_KEYS$pasivos_provisions,  cid, .schema_pasivos_provision),
    liabs     = leer(S3_KEYS$pasivos_liabilities, cid, .schema_pasivos_liability),
    audit     = leer(S3_KEYS$pasivos_audit,       cid, .schema_pasivos_audit),
    conf      = leer(S3_KEYS$bancos_confirmados,  cid, .schema_bancos_confirmados),
    manual    = leer(S3_KEYS$manual,              cid, .schema_manual))
  agenda <- tryCatch(suppressMessages(safe_load_pagar_hoy(client_id = cid)), error = function(e) "no se pudo leer")
  malas <- c(names(t)[vapply(t, falla, logical(1))], if (falla(agenda)) "agenda")
  if (length(malas)) { cat("  NO SE PUDO LEER:", paste(malas, collapse = ", "), "-- nada que contar\n\n"); next }

  cat(sprintf("  requisiciones: %d | historial: %d | pasivos_audit: %d filas | bancos_confirmados: %d | agenda: %d\n",
              nrow(t$reqs), nrow(t$historial), nrow(t$audit), nrow(t$conf), nrow(agenda)))
  if (!nrow(t$reqs)) { cat("  sin requisiciones\n\n"); next }

  tv <- system.time(v <- requisiciones_vista_en(t$reqs, t$provs, t$conf, t$manual, t$carpetas, t$evid, t$coms,
                                                liabs = t$liabs, agenda = agenda))[["elapsed"]]
  cat("  etapas:", cuenta(v$etapa, REQ_ETAPAS), "\n")
  cat("  pagadas que vuelven a la lista con CM35:", sum(v$etapa == "pagada"), "\n")

  te <- system.time(ev <- requisiciones_eventos_en(t$reqs, t$historial, t$audit, t$conf, agenda = agenda))[["elapsed"]]
  es <- ev[ev$tipo == "estatus", , drop = FALSE]
  cat("  eventos:", nrow(ev), "|", cuenta(ev$tipo, c("estatus", "edicion", "comentario", "evidencia")), "\n")
  cat("  eventos de estatus sin autor (por = NA):", sum(is.na(es$por)), "de", nrow(es),
      "|", cuenta(es$a[is.na(es$por)], REQ_ETAPAS), "\n")

  ligadas <- unique(c(t$reqs$provision_id, t$historial$detalle[t$historial$accion %in% "ligada"]))
  cr <- t$conf[t$conf$provision_id %in% ligadas[!is.na(ligadas)] & !(t$conf$action %in% "recovered"), , drop = FALSE]
  cat("  confirmaciones de requisiciones:", nrow(cr), "| sin confirmado_por:", sum(is.na(cr$confirmado_por)), "\n")

  ult <- es[order(es$requisicion_id, es$en), , drop = FALSE]
  ult <- ult[!duplicated(ult$requisicion_id, fromLast = TRUE), , drop = FALSE]
  et  <- setNames(v$etapa, v$id)
  dif <- sum(ult$a != et[ult$requisicion_id], na.rm = TRUE) + sum(!(v$id %in% ult$requisicion_id))
  cat("  requisiciones cuyo último evento NO coincide con la etapa de la lista:", dif, "\n")
  cat(sprintf("  tiempo: vista_en %.3f s | eventos_en %.3f s\n\n", tv, te))
}
