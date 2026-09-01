# =============================================================================
# scripts/diagnose_moves_readonly.R
#
# Read-only diagnostic: directly inspects real S3 data for the "networks"
# client -- invoice_moves.rds AND the AR/AP SAP snapshots -- bypassing the
# app entirely (raw aws.s3 calls, no .app_data_cache, no .s3_preload_cache,
# no load_sap_snapshot()/load_moves()). Run this in a FRESH R session
# (restart R first) to get a ground-truth answer on whether the real data
# was ever actually lost, independent of anything the app's own caching
# layers might be showing on screen.
#
# 2026-08-27: extended to also check the AR/AP SAP snapshots after finding
# load_sap_snapshot() had no client-scoping at all (fixed the same day) --
# that function seeds Calendario's data on every session, upstream of
# moves/proveedores/everything else, so a "bad data" report could trace
# back to either one.
#
# This script NEVER writes anything. Safe to run any time.
# =============================================================================

if (file.exists(".Renviron")) readRenviron(".Renviron")

bucket <- Sys.getenv("S3_BUCKET")
cat("S3_BUCKET =", bucket, "\n")
cat("CLIENT_ID =", Sys.getenv("CLIENT_ID"), "(informational only -- this script reads 'networks' explicitly)\n\n")

.read_raw <- function(key) {
  tryCatch(aws.s3::s3readRDS(object = key, bucket = bucket),
          error = function(e) { cat("  ERROR reading", key, ":", conditionMessage(e), "\n"); NULL })
}

cat("============================================================\n")
cat(" 1. invoice_moves.rds (tesoreria's manual date moves)\n")
cat("============================================================\n")
key <- "networks/invoice_moves.rds"
cat("Reading", key, "...\n\n")
mv <- .read_raw(key)

if (is.null(mv)) {
  cat("Could not read the file (see error above) -- this alone doesn't confirm data\n",
      "loss; check the error message (e.g. NoSuchKey means the key never existed at\n",
      "all under this exact bucket/path, a credentials issue means we can't tell yet).\n")
} else {
  cat("Rows in invoice_moves.rds:", nrow(mv), "\n\n")

  cat("Rows moved_by 'tesoreria':\n")
  tes <- mv[!is.na(mv$moved_by) & tolower(trimws(mv$moved_by)) == "tesoreria", ]
  cat(" ", nrow(tes), "row(s)\n\n")
  if (nrow(tes)) {
    tes_sorted <- tes[order(tes$last_updated, decreasing = TRUE), ]
    cols <- intersect(c("ledger","Empresa","Moneda","Documento","FechaVenc_Proyectada","last_updated"),
                       names(tes_sorted))
    print(utils::head(tes_sorted[, cols], 30))
    if (nrow(tes_sorted) > 30) cat("... and", nrow(tes_sorted) - 30, "more row(s)\n")
  }

  cat("\nRows with FechaVenc_Proyectada in August 2026:\n")
  if ("FechaVenc_Proyectada" %in% names(mv)) {
    aug <- mv[!is.na(mv$FechaVenc_Proyectada) &
              mv$FechaVenc_Proyectada >= as.Date("2026-08-01") &
              mv$FechaVenc_Proyectada <= as.Date("2026-08-31"), ]
    cat(" ", nrow(aug), "row(s) moved to a date within August 2026\n")
  }

  cat("\nMost recent 'last_updated' timestamp in the whole table:",
      as.character(if ("last_updated" %in% names(mv) && nrow(mv)) max(mv$last_updated, na.rm = TRUE) else NA),
      "\n")
}

cat("\n============================================================\n")
cat(" 2. SAP snapshots (AR/AP) -- what Calendario seeds from at login\n")
cat("============================================================\n")
for (ledger in c("AR", "AP")) {
  key <- sprintf("networks/sap_snapshot_%s.rds", ledger)
  cat("\nReading", key, "...\n")
  snap <- .read_raw(key)
  if (is.null(snap) || !is.list(snap)) {
    cat("  Could not read or empty -- see error above if any.\n")
    next
  }
  n_rows <- if (!is.null(snap$data)) nrow(snap$data) else 0L
  cat(sprintf("  saved_at = %s\n  rows     = %d\n",
             as.character(snap$saved_at %||% NA), n_rows))
  if (n_rows > 0 && "Empresa" %in% names(snap$data)) {
    cat("  Empresas present:", paste(sort(unique(snap$data$Empresa)), collapse = ", "), "\n")
  }
}

cat("\n============================================================\n")
cat(" 3. Proveedores (supplier catalog)\n")
cat("============================================================\n")
key <- "networks/proveedores.rds"
cat("Reading", key, "...\n\n")
prov <- .read_raw(key)
if (is.null(prov)) {
  cat("Could not read the file (see error above).\n")
} else {
  cat("Rows in proveedores.rds:", nrow(prov), "\n")
  if ("activo" %in% names(prov)) {
    cat("  Active (activo==TRUE):  ", sum(prov$activo == TRUE, na.rm = TRUE), "\n")
    cat("  Inactive/other:         ", sum(prov$activo != TRUE | is.na(prov$activo)), "\n")
  }
}

cat("\nDone. If all three sections above show real, non-empty data with sensible\n",
    "row counts, the underlying S3 data was never actually lost -- only how the\n",
    "app was READING it was broken. If any section looks genuinely empty/wrong\n",
    "here too (not just in the app), that's a different, more serious situation --\n",
    "share this full output and we'll investigate that specific key directly.\n")
