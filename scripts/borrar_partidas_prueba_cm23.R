# =============================================================================
# scripts/borrar_partidas_prueba_cm23.R
# CM23, 2026-11-10 (Luis asked for it). Sends two test manual items of the
# client "networks" to the papelera, exactly as the app's delete does
# (R/search_module.R:474-505, R/ledger_module.R:1360-1390):
#   1. add_to_papelera() + save_papelera() (a union on S3's papelera as it is
#      now), so they can be restored from the Papelera;
#   2. delete_manual() on a strict fresh read of manual_inv, then
#      save_manual(m, client_id, base = m_base) (applied on S3 as it is now);
#   3. one log_action() "eliminar_facturas" in module ledger_AP, flushed now.
# They cannot be deleted from the app: neither has a due date, and Vencidos,
# the search and the Calendario drop rows with FechaEff NA.
#
#   efb376cf-acaa-4e52-b222-bab8641c426d  "test"      (dev, 2026-11-01; CM21)
#   716cf3db-bebd-48d5-ade2-01e5754bef15  "Internet"  (anon, no date, no provision)
# Nothing else: provision 7c910911... (item_confirmed, orphan) is left as is.
#
# Usage (from the repo root, never from a session):
#   Rscript --vanilla scripts/borrar_partidas_prueba_cm23.R <path to .Renviron> [client_id]
#   APLICAR=1 Rscript --vanilla scripts/borrar_partidas_prueba_cm23.R <path> [client_id]
# Without APLICAR=1 it is a dry run: every S3 write path -- the app's and
# aws.s3's own -- is replaced by stop() before anything is read.
# It stops without writing if the two ids do not match exactly two live rows.
# If both are already gone from manual_inv and archived, it does nothing.
# Prints no secret (only the bucket name) and no amounts.
#
# Not done: bump_sync_version(). From a fresh process it would overwrite the
# shared sync_versions.rds with this process's near-empty counters. Open
# sessions see the change when they reload the data (or the page).
# =============================================================================
args      <- commandArgs(trailingOnly = TRUE)
renv      <- args[1]
client_id <- if (length(args) >= 2) args[2] else "networks"
aplicar   <- identical(Sys.getenv("APLICAR"), "1")
if (is.na(renv) || !file.exists(renv)) stop("Pass the path to the .Renviron to read.")
readRenviron(renv)
IDS   <- c("efb376cf-acaa-4e52-b222-bab8641c426d", "716cf3db-bebd-48d5-ade2-01e5754bef15")
ACTOR <- "coordinador (CM23, pedido de Luis)"
cat("Bucket:", Sys.getenv("S3_BUCKET"), "| client:", client_id,
    "| mode:", if (aplicar) "APLICAR (writes)" else "dry run (no writes)", "\n\n")

suppressPackageStartupMessages({ library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a

.ro <- function(...) stop("DRY RUN: a write was attempted.", call. = FALSE)
if (!aplicar)
  for (fn in c("s3saveRDS", "put_object", "delete_object", "s3write_using", "copy_object"))
    if (exists(fn, envir = asNamespace("aws.s3"), inherits = FALSE))
      utils::assignInNamespace(fn, .ro, "aws.s3")

# S3_KEYS lives in R/global.R, which is not sourced (it starts the app's preload).
for (e in parse("R/global.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "S3_KEYS"))
    eval(e, globalenv())
suppressMessages(source("R/persistence.R"))
suppressMessages(source("R/app_audit.R"))
if (!aplicar) {
  .s3_write <- .ro
  for (fn in ls(globalenv(), pattern = "^(save_|modificar_)")) assign(fn, .ro, envir = globalenv())
}

# ── Read, strictly (a failed read stops here, nothing written) ────────────────
leer <- function() list(
  m   = .normalize(.s3_leer_para_escribir(S3_KEYS$manual,   client_id), .schema_manual),
  pap = .normalize(.s3_leer_para_escribir(S3_KEYS$papelera, client_id), .schema_papelera))
antes <- leer()
m <- antes$m; pap <- antes$pap
en_m   <- m[m$id %in% IDS, , drop = FALSE]
# Archived now = an "archived" event no "recovered" event reverses (prod has
# a confirm + undo pair for each from 2026-11-01: those do not count).
archivadas <- function(p) {
  a <- p[p$action %in% "archived" | is.na(p$action), , drop = FALSE]
  a <- a[!a$event_id %in% p$reverses_event_id[p$action %in% "recovered"], , drop = FALSE]
  unique(a$id)
}
en_pap <- IDS %in% archivadas(pap)
cat(sprintf("manual_inv: %d rows | papelera: %d rows\n", nrow(m), nrow(pap)))
cat("In manual_inv:", sum(IDS %in% m$id), "of 2 | already archived in papelera:", sum(en_pap), "of 2\n")
ev <- pap[pap$id %in% IDS, intersect(c("id", "ledger", "source", "action", "disposition", "deleted_by", "deleted_at",
                                      "event_id", "reverses_event_id"), names(pap)), drop = FALSE]
if (nrow(ev)) { cat("Their papelera events so far:\n"); print(as.data.frame(ev[order(ev$id, ev$deleted_at), ]), row.names = FALSE) }
cat("\n")

if (!nrow(en_m) && all(en_pap)) {
  cat("Both are already gone from manual_inv and archived in the papelera. Nothing to do.\n")
  quit(status = 0L)
}
if (nrow(en_m) != 2L || !setequal(en_m$id, IDS) || anyDuplicated(m$id[m$id %in% IDS]))
  stop("Expected exactly the 2 ids, once each, in manual_inv; found ", nrow(en_m),
       " (", paste(en_m$id, collapse = ", "), "). Nothing was written.", call. = FALSE)

cat("Would send to the papelera (no amounts):\n")
print(as.data.frame(en_m[, intersect(c("id", "ledger", "Empresa", "Moneda", "Documento", "Factura",
                                       "created_by", "created_at", "provision_id"), names(en_m))]),
      row.names = FALSE)
cat(sprintf("\nmanual_inv: %d -> %d rows | papelera: %d -> %d rows\n",
            nrow(m), nrow(m) - 2L, nrow(pap), nrow(pap) + 2L))

# What the app archives: the row itself, with its id (Calendario/Vencidos hide
# a manual row whose id is in the papelera), source "manual", no FechaEff.
detalle <- as.data.frame(en_m)
detalle$source   <- "manual"
detalle$FechaEff <- as.Date(NA)

if (!aplicar) {
  # The orphan provision of "test" stays: without its item, does any view
  # still paint it? The Tabla only shows converted/item_confirmed provisions
  # through their manual item; the Calendario only "provisional" ones.
  ok_tabla <- tryCatch({
    for (e in parse("R/global.R"))
      if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "strip_accents"))
        eval(e, globalenv())
    suppressMessages({ source("R/pasivos_schemas.R"); source("R/pasivos_persistence.R")
                       source("R/bancos_persistence.R"); source("R/pasivos_table_pivot.R") })
    if (!aplicar) { .s3_write <- .ro; for (fn in ls(globalenv(), pattern = "^(save_|modificar_)")) assign(fn, .ro, envir = globalenv()) }
    provs <- load_pasivos_provisions(client_id = client_id)
    pid   <- en_m$provision_id[!is.na(en_m$provision_id)]
    cat("\nProvision(s) of these items:", paste(pid, provs$estado[match(pid, provs$id)], collapse = ", "), "\n")
    mi_sin <- m[!m$id %in% IDS & m$ledger %in% "AP", , drop = FALSE]
    bc <- confirmaciones_vigentes(.normalize(.s3_read_with(S3_KEYS$bancos_confirmados, client_id = client_id),
                                             .schema_bancos_confirmados))
    lt <- suppressMessages(pasivos_table_build_long(provs, mi_sin, bc, load_pasivos_liabilities(client_id = client_id),
      list(empresa = character(0), categorias = c("regular", "financiero", "tarjeta"), subcategorias = character(0),
           currency = "All", search = "", granularity = "day", provision_type = "todas")))
    cat("Pasivos > Tabla without the 2 items:", nrow(lt), "rows; paints that provision:",
        any(lt$cell_provision_id %in% pid), "| Calendario paints it (only 'provisional'):",
        any(provs$estado[provs$id %in% pid] %in% "provisional"), "\n")
    TRUE
  }, error = function(e) { cat("\n(Could not check the provision in the views: ", conditionMessage(e), ")\n"); FALSE })
  cat("\nDry run: nothing was written. Run with APLICAR=1 to apply.\n")
  quit(status = 0L)
}

# ── Apply, in the app's order: papelera first, then manual_inv, then the log ──
save_papelera(add_to_papelera(pap, detalle, ledger = "AP", deleted_by = ACTOR), client_id = client_id)
cat("papelera saved\n")
m_base <- m
for (mid in IDS) m <- delete_manual(m, mid)
save_manual(m, client_id = client_id, base = m_base)
cat("manual_inv saved\n")

log_action(user = ACTOR, module = "ledger_AP", action = "eliminar_facturas",
           description = "2 factura(s) enviada(s) a papelera (CM23: partidas de prueba sin fecha)",
           target_id = paste(en_m$Documento, collapse = ", "),
           metadata = list(n = 2L, ids = IDS, origen = "scripts/borrar_partidas_prueba_cm23.R"),
           client_id = client_id, viewer_home_client_id = client_id)
log_ok <- isTRUE(.audit_flush_pending(tolower(client_id)))
cat("audit log:", if (log_ok) "written" else "NOT written (the deletion itself stands; see the message above)", "\n")

# ── Check on a fresh read ─────────────────────────────────────────────────────
despues <- leer()
arch <- despues$pap[despues$pap$id %in% IDS & despues$pap$action %in% "archived" &
                      despues$pap$deleted_by %in% ACTOR, , drop = FALSE]
checks <- c(
  "the 2 are no longer in manual_inv"        = !any(IDS %in% despues$m$id),
  "manual_inv lost exactly 2 rows"           = nrow(despues$m) == nrow(antes$m) - 2L,
  "the 2 are archived in the papelera (CM23)" = setequal(arch$id, IDS) && nrow(arch) == 2L &&
                                               all(IDS %in% archivadas(despues$pap)),
  "papelera grew by exactly 2 rows"          = nrow(despues$pap) == nrow(antes$pap) + 2L)
for (k in names(checks)) cat(if (checks[[k]]) " OK  " else " FAIL", k, "\n")
if (!all(checks)) quit(status = 1L)
cat("\nDone.\n")
