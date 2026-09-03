# =============================================================================
# scripts/diagnose_audit_moves_readonly.R
#
# Read-only: pulls the Activity log's own real audit trail (R/app_audit.R,
# a SEPARATE, more resilient S3 write path -- it retries several times
# before giving up, unlike the old moves-save code which had zero retries)
# for every "mover_fecha"/"restaurar_fecha" entry since 2026-08-25 (the last
# confirmed-successful save to networks/invoice_moves.rds). If the audit log
# recorded these even though invoice_moves.rds itself never received them,
# this is the raw material for a manual recovery replay.
#
# Never writes anything.
# =============================================================================

if (file.exists(".Renviron")) readRenviron(".Renviron")
suppressPackageStartupMessages({ library(dplyr); library(jsonlite) })
source("R/persistence.R", local = FALSE)
source("R/app_audit.R", local = FALSE)

log <- read_audit_log(client_id = "networks", since = as.POSIXct("2026-08-25 00:00:00", tz = "UTC"))
cat("Total audit rows since 2026-08-25:", nrow(log), "\n\n")

mv <- log[log$action %in% c("mover_fecha", "restaurar_fecha",
                            "mover_fecha_fallo_guardado", "restaurar_fecha_fallo_guardado"), ]
mv <- mv[order(mv$ts), ]
cat("mover_fecha / restaurar_fecha entries since 2026-08-25:", nrow(mv), "\n\n")

if (nrow(mv)) {
  for (i in seq_len(nrow(mv))) {
    r <- mv[i, ]
    cat(sprintf("[%s] user=%-12s action=%-28s target=%s\n    desc: %s\n",
                as.character(r$ts), r$user %||% "?", r$action, r$target_id %||% "", r$description))
  }
} else {
  cat("No mover_fecha/restaurar_fecha entries found in this window.\n")
}

cat("\n\nAll distinct actions seen in this window (sanity check the log itself has real data):\n")
print(table(log$action))
