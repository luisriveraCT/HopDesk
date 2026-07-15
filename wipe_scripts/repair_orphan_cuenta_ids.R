# =============================================================================
# repair_orphan_cuenta_ids.R
# ONE-TIME REPAIR: fixes bancos_movimientos rows whose cuenta_id does not exist
# in ctas_cuentas, causing blank Empresa/Moneda in the Libro and Excel export.
#
# Strategy:
#   1. "orphan" = cuenta_id is "" (empty string) OR a UUID not in ctas_cuentas
#   2. For each orphan, look for a row with the same (recibo, tipo, cargo, abono)
#      that DOES have a valid cuenta_id → use that cuenta_id.
#   3. "" cuenta_ids with no matching valid row → convert to NA (sin cuenta).
#   4. UUID orphans with no matching valid row → reported; script stops so you
#      can handle them manually by editing the manual_overrides block below.
#
# Run from the R console:
#   source("wipe_scripts/repair_orphan_cuenta_ids.R")
# =============================================================================

readRenviron("C:/Users/luisr/Antiguedad_App/.Renviron")
setwd("C:/Users/luisr/Antiguedad_App")
source("R/global.R")
s3_init()

# ── 1. Load data ──────────────────────────────────────────────────────────────
movs <- load_bancos_movimientos(include_deleted = TRUE)
ctas <- load_ctas_cuentas()

message("=== REPAIR: orphan cuenta_ids ===")
message("Total movements (incl. deleted): ", nrow(movs))
message("Valid ctas_cuentas entries:       ", nrow(ctas))

valid_cids <- ctas$id

# ── 2. Identify orphan rows ───────────────────────────────────────────────────
# Orphan: cuenta_id is "" (bad empty string) OR a UUID that no longer exists.
# NA cuenta_id is intentional ("sin cuenta") — leave alone.
blank_mask  <- !is.na(movs$cuenta_id) & trimws(movs$cuenta_id) == ""
stale_mask  <- !is.na(movs$cuenta_id) & nzchar(movs$cuenta_id) &
               !(movs$cuenta_id %in% valid_cids)
orphan_mask <- blank_mask | stale_mask

message("\nOrphan rows found:")
message("  cuenta_id = '' (blank):  ", sum(blank_mask))
message("  cuenta_id = stale UUID:  ", sum(stale_mask))
message("  TOTAL:                   ", sum(orphan_mask))

if (!any(orphan_mask)) {
  message("\nNo orphan rows — nothing to repair. Exiting.")
  stop("Clean — no action taken.", call. = FALSE)
}

# Show stale UUID values to help understand the problem
if (any(stale_mask)) {
  message("\nStale UUID values (not in ctas_cuentas):")
  print(table(movs$cuenta_id[stale_mask]))
}

# ── 3. Build dedup key from valid rows ────────────────────────────────────────
# Match orphan rows to their valid-account counterparts using the same 4-tuple
# the dedup engine uses: (recibo, tipo, cargo_key, abono_key).
movs_valid <- movs[movs$cuenta_id %in% valid_cids & !movs$eliminado, ]

dedup_key_valid <- movs_valid |>
  dplyr::filter(!is.na(recibo), nzchar(recibo)) |>
  dplyr::mutate(
    cargo_key = round(cargo, 2),
    abono_key = round(abono, 2)
  ) |>
  dplyr::distinct(recibo, tipo, cargo_key, abono_key, .keep_all = TRUE) |>
  dplyr::select(recibo, tipo, cargo_key, abono_key, resolved_cuenta_id = cuenta_id)

orphan_rows <- movs[orphan_mask, ] |>
  dplyr::mutate(
    cargo_key = round(cargo, 2),
    abono_key = round(abono, 2)
  )

orphan_joined <- dplyr::left_join(
  orphan_rows,
  dedup_key_valid,
  by = c("recibo", "tipo", "cargo_key", "abono_key")
)

n_resolved   <- sum(!is.na(orphan_joined$resolved_cuenta_id))
n_unresolved <- sum( is.na(orphan_joined$resolved_cuenta_id))

message("\nAuto-resolution results:")
message("  Resolvable (have a valid duplicate): ", n_resolved)
message("  Unresolvable (no valid duplicate):   ", n_unresolved)

# ── 4. Manual overrides (edit this block for unresolvable rows) ───────────────
# If n_unresolved > 0, run the script once to see the rows printed below,
# then add entries here: id = "the row's id", cuenta_id = "the correct UUID".
# Find valid UUIDs by running: ctas[, c("id","Empresa","alias","Moneda")]
manual_overrides <- list(
  # Example (uncomment and fill in after first run):
  # list(id = "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
  #      cuenta_id = "yyyyyyyy-yyyy-yyyy-yyyy-yyyyyyyyyyyy")
)

# Apply any manual overrides to orphan_joined
if (length(manual_overrides)) {
  for (ov in manual_overrides) {
    idx <- which(orphan_joined$id == ov$id)
    if (length(idx)) {
      orphan_joined$resolved_cuenta_id[idx] <- ov$cuenta_id
      message("Manual override applied: id=", ov$id, " → ", ov$cuenta_id)
    }
  }
  n_resolved   <- sum(!is.na(orphan_joined$resolved_cuenta_id))
  n_unresolved <- sum( is.na(orphan_joined$resolved_cuenta_id))
}

# ── 5. Report unresolvable rows, stop if any remain ───────────────────────────
if (n_unresolved > 0) {
  message("\n=== UNRESOLVABLE ROWS (need manual_overrides) ===")
  unresolved <- orphan_joined[is.na(orphan_joined$resolved_cuenta_id), ]
  print(unresolved[, c("id", "fecha", "tipo", "recibo",
                        "descripcion_raw", "cargo", "abono", "eliminado")])
  message("\nAction required:")
  message("  1. Run:  print(ctas[, c('id','Empresa','alias','Moneda')])")
  message("     to find the correct cuenta_id for each row above.")
  message("  2. Add entries to the manual_overrides list in this script.")
  message("  3. Re-run the script.")
  message("\nScript stopped — no changes saved.")
  stop("Unresolvable orphan rows remain. See output above.", call. = FALSE)
}

# ── 6. Apply all fixes ────────────────────────────────────────────────────────
movs_fixed <- movs

# 6a. Apply resolved cuenta_ids from dedup matching
for (i in seq_len(nrow(orphan_joined))) {
  row_id   <- orphan_joined$id[i]
  new_cid  <- orphan_joined$resolved_cuenta_id[i]
  idx      <- which(movs_fixed$id == row_id)
  if (length(idx) && !is.na(new_cid)) {
    movs_fixed$cuenta_id[idx] <- new_cid
  }
}

# 6b. Any remaining blank ("") cuenta_ids that had no match → convert to NA
still_blank <- !is.na(movs_fixed$cuenta_id) & trimws(movs_fixed$cuenta_id) == ""
if (any(still_blank)) {
  message("\nConverting ", sum(still_blank), " blank cuenta_id(s) to NA (sin cuenta).")
  movs_fixed$cuenta_id[still_blank] <- NA_character_
}

# ── 7. Verify: count remaining orphans after fix ──────────────────────────────
still_stale <- !is.na(movs_fixed$cuenta_id) & nzchar(movs_fixed$cuenta_id) &
               !(movs_fixed$cuenta_id %in% valid_cids) & !movs_fixed$eliminado
message("\nOrphan rows remaining after fix: ", sum(still_stale))
if (any(still_stale)) {
  message("WARNING: some rows still unresolved:")
  print(movs_fixed[still_stale, c("id", "fecha", "cuenta_id", "descripcion_raw")])
}

# ── 8. Save ───────────────────────────────────────────────────────────────────
message("\nRows modified: ", n_resolved)
message("Saving to S3...")
save_bancos_movimientos(movs_fixed)
bump_sync_version("bancos_movimientos_db")
message("Done. Reload the app (or wait for sync) to see corrected rows.")
