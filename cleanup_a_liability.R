# =============================================================================
# cleanup_a_liability.R
# One-time script to remove the test "a" liability and all data it generated.
#
# SOURCE in an R console while the app's working directory is Antiguedad_App/:
#   setwd("C:/Users/luisr/Antiguedad_App")
#   source("R/global.R")
#   source("cleanup_a_liability.R")
# =============================================================================

message("=== Cleanup: 'a' liability ===")

# ── 1. Load all affected tables ───────────────────────────────────────────────
liabs     <- tryCatch(load_pasivos_liabilities(), error = function(e) { stop("Cannot load liabilities: ", e$message) })
provs     <- tryCatch(load_pasivos_provisions(),  error = function(e) { stop("Cannot load provisions: ",  e$message) })
manual_df <- tryCatch(load_manual(),              error = function(e) { stop("Cannot load manual_inv: ",  e$message) })
ph_df     <- tryCatch(load_pagar_hoy(),           error = function(e) { stop("Cannot load pagar_hoy: ",   e$message) })

# ── 2. Identify the "a" liability ─────────────────────────────────────────────
a_liab <- liabs[
  !is.na(liabs$nombre) & trimws(liabs$nombre) == "a" |
  !is.na(liabs$parte)  & trimws(liabs$parte)  == "a",
  , drop = FALSE
]
if (!nrow(a_liab)) stop("No liability named 'a' found — nothing to clean up.")
message("Found ", nrow(a_liab), " liability/ies named 'a':")
print(a_liab[, intersect(c("id","nombre","parte","estado","empresa"), names(a_liab))])

a_lib_ids <- a_liab$id

# ── 3. Find associated provisions ─────────────────────────────────────────────
a_provs <- provs[
  !is.na(provs$liability_id) & provs$liability_id %in% a_lib_ids,
  , drop = FALSE
]
message("\nFound ", nrow(a_provs), " provision(s) for this liability.")
if (nrow(a_provs))
  print(a_provs[, intersect(c("id","estado","manual_inv_id","pagar_hoy_id","parte"), names(a_provs))])

prov_ids <- a_provs$id

# ── 4. Collect manual_inv IDs to hard-delete ──────────────────────────────────
manual_ids <- character(0)
if (nrow(a_provs) && "manual_inv_id" %in% names(a_provs))
  manual_ids <- c(manual_ids, a_provs$manual_inv_id[!is.na(a_provs$manual_inv_id) & nzchar(a_provs$manual_inv_id)])
if (length(prov_ids) && "provision_id" %in% names(manual_df))
  manual_ids <- c(manual_ids, manual_df$id[!is.na(manual_df$provision_id) & manual_df$provision_id %in% prov_ids])
manual_ids <- unique(manual_ids)
message("Removing ", length(manual_ids), " manual_inv entry/ies: ", paste(manual_ids, collapse = ", "))

# ── 5. Collect pagar_hoy IDs to hard-delete ───────────────────────────────────
ph_ids <- character(0)
if (nrow(a_provs) && "pagar_hoy_id" %in% names(a_provs))
  ph_ids <- c(ph_ids, a_provs$pagar_hoy_id[!is.na(a_provs$pagar_hoy_id) & nzchar(a_provs$pagar_hoy_id)])
if (length(prov_ids) && "provision_id" %in% names(ph_df))
  ph_ids <- c(ph_ids, ph_df$id[!is.na(ph_df$provision_id) & ph_df$provision_id %in% prov_ids])
ph_ids <- unique(ph_ids)
message("Removing ", length(ph_ids), " pagar_hoy entry/ies: ", paste(ph_ids, collapse = ", "))

# ── 6. Confirm before writing ─────────────────────────────────────────────────
answer <- readline("Proceed? (y/N): ")
if (!identical(tolower(trimws(answer)), "y")) {
  message("Aborted — no changes written.")
  invisible(NULL)
} else {

  now <- Sys.time()

  # Remove manual_inv rows
  if (length(manual_ids) && "id" %in% names(manual_df))
    manual_df <- manual_df[!manual_df$id %in% manual_ids, , drop = FALSE]

  # Remove pagar_hoy rows (only pending; don't remove confirmed payments)
  if (length(ph_ids) && "id" %in% names(ph_df)) {
    safe_rm <- ph_df$id %in% ph_ids &
               (is.na(ph_df$status) | ph_df$status != "confirmed")
    if (any(safe_rm)) ph_df <- ph_df[!safe_rm, , drop = FALSE]
    n_confirmed <- sum(ph_df$id %in% ph_ids)
    if (n_confirmed > 0)
      message("WARNING: ", n_confirmed, " pagar_hoy entries were already CONFIRMED — left untouched.")
  }

  # Soft-delete provisions
  if (length(prov_ids)) {
    idx <- which(provs$id %in% prov_ids)
    provs$estado[idx]         <- "deleted"
    provs$last_edited_by[idx] <- "cleanup_a_liability"
    provs$last_edited_at[idx] <- now
  }

  # Soft-delete liability
  idx_l <- which(liabs$id %in% a_lib_ids)
  liabs$estado[idx_l] <- "deleted"

  # ── Save ─────────────────────────────────────────────────────────────────────
  tryCatch({ save_pasivos_liabilities(liabs); message("✓ Liabilities saved")  }, error = function(e) stop("save_pasivos_liabilities failed: ", e$message))
  tryCatch({ save_pasivos_provisions(provs);  message("✓ Provisions saved")   }, error = function(e) stop("save_pasivos_provisions failed: ", e$message))
  tryCatch({ save_manual(manual_df);          message("✓ Manual inv saved")   }, error = function(e) stop("save_manual failed: ", e$message))
  tryCatch({ save_pagar_hoy(ph_df, "cleanup_a_liability"); message("✓ Pagar hoy saved") }, error = function(e) stop("save_pagar_hoy failed: ", e$message))

  message("\nDone. Reload the app (or press the refresh button) to confirm removal.")
  invisible(TRUE)
}
