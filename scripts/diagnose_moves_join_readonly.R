# =============================================================================
# scripts/diagnose_moves_join_readonly.R
#
# Read-only: checks whether invoice_moves.rds's (Empresa, Moneda, Documento)
# keys actually left_join-match the CURRENT SAP snapshots. df_with_moves()
# (R/ledger_module.R) joins moves onto the SAP data by these three columns --
# if the SAP snapshot's key formatting has drifted since the move was saved
# (e.g. Documento padding/case, Empresa display-name renames), the join
# silently drops the match: the move row still exists untouched in S3, but
# the invoice displays at its raw un-moved SAP date instead. This would look
# exactly like "tesoreria's moves are gone" while implicating no data loss
# at all. Never writes anything.
# =============================================================================

if (file.exists(".Renviron")) readRenviron(".Renviron")
bucket <- Sys.getenv("S3_BUCKET")

.read_raw <- function(key) {
  tryCatch(aws.s3::s3readRDS(object = key, bucket = bucket),
           error = function(e) { cat("  ERROR reading", key, ":", conditionMessage(e), "\n"); NULL })
}

mv <- .read_raw("networks/invoice_moves.rds")
ar <- .read_raw("networks/sap_snapshot_AR.rds")
ap <- .read_raw("networks/sap_snapshot_AP.rds")

cat("moves rows:", nrow(mv), "\n")

for (led in c("AR","AP")) {
  snap <- if (led == "AR") ar else ap
  snap_df <- snap$data
  mv_led <- mv[!is.na(mv$ledger) & toupper(trimws(mv$ledger)) == led, ]
  cat(sprintf("\n== %s: %d move rows, snapshot has %d rows ==\n", led, nrow(mv_led), nrow(snap_df)))
  if (!nrow(mv_led)) next

  key_mv   <- paste(mv_led$Empresa, mv_led$Moneda, mv_led$Documento, sep = "|")
  key_snap <- paste(snap_df$Empresa, snap_df$Moneda, snap_df$Documento, sep = "|")

  matched <- key_mv %in% key_snap
  cat("  Exact-key matches:", sum(matched), "/", length(key_mv), "\n")

  if (any(!matched)) {
    cat("  -- Sample UNMATCHED move rows (up to 10) --\n")
    unmatched <- mv_led[!matched, c("Empresa","Moneda","Documento","FechaVenc_Proyectada")]
    print(utils::head(unmatched, 10))

    # Try to find near-matches to reveal WHAT changed (Documento formatting,
    # Empresa renaming, trailing whitespace, case, etc.)
    bad_doc <- unmatched$Documento[1]
    bad_emp <- unmatched$Empresa[1]
    cat("\n  Looking for near-matches on Documento (trimmed/case-insensitive) for the first unmatched row:\n")
    cat("    moves Documento  = [", bad_doc, "]  (nchar=", nchar(bad_doc), ")\n", sep = "")
    cat("    moves Empresa    = [", bad_emp, "]  (nchar=", nchar(bad_emp), ")\n", sep = "")
    near <- snap_df[toupper(trimws(snap_df$Documento)) == toupper(trimws(bad_doc)), ]
    if (nrow(near)) {
      cat("    FOUND", nrow(near), "row(s) in snapshot with same Documento (case/whitespace-insensitive):\n")
      print(near[, intersect(c("Empresa","Moneda","Documento"), names(near))])
    } else {
      cat("    No row in snapshot matches this Documento at all (even loosely) -- invoice may no longer exist in SAP, or Documento format changed completely.\n")
    }
    near_emp <- unique(snap_df$Empresa[toupper(trimws(snap_df$Empresa)) == toupper(trimws(bad_emp))])
    cat("    Empresa exact form present in snapshot?", bad_emp %in% snap_df$Empresa, "\n")
    if (!bad_emp %in% snap_df$Empresa) {
      cat("    Empresa values actually in snapshot:", paste(sort(unique(snap_df$Empresa)), collapse = " | "), "\n")
    }
  }
}
