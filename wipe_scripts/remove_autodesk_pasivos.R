# =============================================================================
# wipe_scripts/remove_autodesk_pasivos.R
# ONE-TIME cleanup: soft-deletes the stuck AUTODESK liability and all its
# associated provisions from the Pasivos module.
#
# Rows are NOT physically removed — estado is set to "deleted" so every row
# stays in S3 for audit/compliance.  The app will no longer show them.
#
# Run from the R console (not Shiny). Delete or archive this file afterward.
# =============================================================================

readRenviron("C:/Users/luisr/Antiguedad_App/.Renviron")

# Source only what's needed — no Shiny, no UI
source("C:/Users/luisr/Antiguedad_App/R/persistence.R")

# S3_KEYS lives in global.R; source it in a bare environment to avoid
# loading Shiny / shinymanager / all modules.
local({
  suppressWarnings(suppressMessages(
    sys.source("C:/Users/luisr/Antiguedad_App/R/global.R",
               envir = globalenv(), chdir = FALSE)
  ))
})

source("C:/Users/luisr/Antiguedad_App/R/pasivos_persistence.R")
source("C:/Users/luisr/Antiguedad_App/R/pasivos_schemas.R")

s3_init()

# ── 1. Inspect liabilities ────────────────────────────────────────────────────
liabs <- load_pasivos_liabilities()
message("\n=== ALL liabilities in store (", nrow(liabs), " rows) ===")
if (nrow(liabs)) {
  print(liabs[, intersect(c("id","nombre","parte","codigo_parte","empresa","estado"),
                           names(liabs)), drop = FALSE])
}

autodesk_liab_idx <- which(
  toupper(trimws(liabs$nombre    %||% "")) == "AUTODESK" |
  toupper(trimws(liabs$parte     %||% "")) == "AUTODESK" |
  toupper(trimws(liabs$codigo_parte %||% "")) == "AUTODESK"
)
message("\nMatching AUTODESK liability rows: ", length(autodesk_liab_idx))
if (!length(autodesk_liab_idx)) {
  message("No liability matched — checking provisions only.")
  autodesk_liab_ids <- character(0)
} else {
  print(liabs[autodesk_liab_idx, , drop = FALSE])
  autodesk_liab_ids <- liabs$id[autodesk_liab_idx]
}

# ── 2. Inspect provisions ─────────────────────────────────────────────────────
provs <- load_pasivos_provisions()
message("\n=== ALL provisions (", nrow(provs), " rows) ===")

autodesk_prov_idx <- which(
  (toupper(trimws(provs$parte        %||% "")) == "AUTODESK" |
   toupper(trimws(provs$codigo_parte %||% "")) == "AUTODESK" |
   (!is.na(provs$liability_id) & provs$liability_id %in% autodesk_liab_ids)) &
  provs$estado != "deleted"
)
message("Matching AUTODESK provision rows to soft-delete: ", length(autodesk_prov_idx))
if (length(autodesk_prov_idx)) {
  print(provs[autodesk_prov_idx,
              intersect(c("id","liability_id","estado","fecha_efectiva",
                          "amount_pago","parte","empresa"),
                        names(provs)), drop = FALSE])
}

# ── 3. Confirm before writing ─────────────────────────────────────────────────
message("\n--- READY TO SOFT-DELETE ---")
message("Liabilities to mark deleted : ", length(autodesk_liab_idx))
message("Provisions  to mark deleted : ", length(autodesk_prov_idx))
message("\nIf this looks correct, run the block below manually:")
message('  .do_cleanup()')

.do_cleanup <- function() {
  now  <- Sys.time()
  user <- "luis-cleanup-script"

  # Soft-delete liabilities
  if (length(autodesk_liab_idx)) {
    liabs$estado[autodesk_liab_idx] <- "deleted"
    save_pasivos_liabilities(liabs)
    message("Liabilities soft-deleted and saved.")
  }

  # Soft-delete provisions
  if (length(autodesk_prov_idx)) {
    provs$estado[autodesk_prov_idx]          <- "deleted"
    provs$last_edited_by[autodesk_prov_idx]  <- user
    provs$last_edited_at[autodesk_prov_idx]  <- now
    save_pasivos_provisions(provs)
    message("Provisions soft-deleted and saved.")
  }

  message("\nDone. Restart or reload the app to see the changes.")
}
