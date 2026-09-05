# =============================================================================
# tests/test_pasivos_audit_completeness.R
#
# 2026-09-05: audit-compliance sweep requested directly -- "who approved
# what and when" must be traceable for every provision/liability creation,
# edit, conversion, and deletion path, not just the best-instrumented ones.
# A prior mapping pass (research only, no code) found the module was
# already well-instrumented for creation/edit/conversion via
# pasivos_log_audit() (R/pasivos_audit.R), but surfaced 4 concrete gaps,
# each confirmed directly rather than assumed:
#
#   1. A user could silently un-convert a provision (same end state as the
#      explicit "Deshacer conversión" button) just by clicking "Quitar" on
#      its staged item in Agenda de hoy -- zero audit trace. Fixed by
#      routing R/pagar_hoy_module.R's "Remove AP" handler through the same
#      pasivos_provision_revive() (R/pasivos_engine.R) the explicit button
#      already uses internally, instead of duplicating the mutation inline
#      with no log call.
#   2. Calendario's day-modal batch delete set a provision's estado to
#      "deleted" but only logged the generic ledger-wide activity entry,
#      never a structured provision.deleted record like the other 3
#      delete paths. Fixed in R/ledger_module.R by adding a per-provision
#      pasivos_log_audit() call, gated on the save actually succeeding.
#   3. "liability.restored" existed in PASIVOS_ACTION_TYPES with zero call
#      sites -- there was no way to un-archive a liability at all. Built a
#      restore button + handler in R/pasivos_list_module.R.
#   4. Pausing or archiving a liability silently dropped its future
#      provisional provisions with no individual log entry, only an
#      uncounted mention in the parent liability's log. Fixed by capturing
#      the dropped rows BEFORE filtering and logging each one individually.
#
# This is a structural/static test file (matches this codebase's existing
# convention for scanning production source rather than re-simulating a
# full Shiny session) plus one functional test of the drop_mask logic
# extracted verbatim from source, per the "extract and eval real code"
# convention used throughout this suite.
# =============================================================================

cat("── Pasivos audit-completeness sweep (2026-09-05) ──────────────────────\n")

.pass <- 0L
.fail <- 0L
.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}
.chk_true  <- function(cond, label) .chk(isTRUE(cond), TRUE, label)
.chk_false <- function(cond, label) .chk(isTRUE(cond), FALSE, label)

# =============================================================================
# Part A -- Gap 1: silent provision revert via Agenda "Quitar" now routed
# through pasivos_provision_revive() (R/pagar_hoy_module.R)
# =============================================================================
cat("\n-- Part A: Agenda 'Quitar' routes provision revert through the logged engine fn --\n")

ph_txt <- readLines("R/pagar_hoy_module.R", warn = FALSE)
rm_ap_start <- grep('observeEvent\\(input\\[\\[paste0\\("remove_ap_", emp\\)\\]\\],', ph_txt)
rm_ar_start <- grep('observeEvent\\(input\\[\\[paste0\\("remove_ar_", emp\\)\\]\\],', ph_txt)
.chk_true(length(rm_ap_start) == 1 && length(rm_ar_start) == 1,
     "found exactly one 'Remove AP' and one 'Remove AR' handler to scan")

if (length(rm_ap_start) == 1 && length(rm_ar_start) == 1) {
  ap_block <- paste(ph_txt[rm_ap_start[1]:(rm_ar_start[1] - 1)], collapse = "\n")

  .chk_true(grepl("pasivos_provision_revive\\(", ap_block),
       "Remove AP calls pasivos_provision_revive() -- the same logged engine fn the explicit undo button uses")
  .chk_false(grepl('provs\\$estado\\[idx_p\\]\\s*<-\\s*"provisional"', ap_block),
       "regression guard: the old silent inline mutation (no audit call) is gone")
  .chk_true(grepl("for \\(pid in unique\\(provs\\$id\\[idx_p\\]\\)\\)", ap_block),
       "reverts each linked provision individually (loop over unique ids), not a single bulk mutation")
  .chk_true(grepl("load_pasivos_provisions\\(client_id = shared\\$effective_client_id\\(\\)\\)", ap_block),
       "reloads provisions fresh from S3 after the revive loop (picks up what the engine actually persisted)")
  .chk_true(grepl("shared\\$pasivos_provisions_db\\(provs_fresh\\)", ap_block),
       "updates the reactive pasivos_provisions_db with the freshly-reloaded state")

  # Each revive call must be individually tryCatch-guarded so one bad id
  # (e.g. a provision already in a non-revivable state) can't abort the
  # whole batch and silently skip reverting the rest.
  .chk_true(grepl("tryCatch\\(\\s*\\n\\s*pasivos_provision_revive\\(", ap_block) ||
            grepl("tryCatch\\(\\s*pasivos_provision_revive\\(", ap_block),
       "each pasivos_provision_revive() call is individually tryCatch-guarded inside the loop")

  # manual_ids must still be captured BEFORE the revive loop runs (revive()
  # clears manual_inv_id on the provision row itself, so capturing it after
  # would find nothing left to clean up).
  manual_pos <- regexpr("manual_ids <- provs\\$manual_inv_id\\[idx_p\\]", ap_block)
  revive_pos <- regexpr("pasivos_provision_revive\\(", ap_block)
  .chk_true(manual_pos > 0 && revive_pos > 0 && manual_pos < revive_pos,
       "manual_ids is captured BEFORE the revive loop clears manual_inv_id on the provision rows")
}

# Confirm the explicit "Deshacer conversión" button (the other path to the
# same end state) is untouched and still logs provision.revived, so this
# fix doesn't accidentally create two DIFFERENT audit shapes for the same
# transition going forward.
mod_txt <- readLines("R/pasivos_module.R", warn = FALSE)
.chk_true(any(grepl('action_type = "provision.revived"', mod_txt)),
     "the explicit 'Deshacer conversión' button still logs provision.revived directly (unchanged reference behavior)")

# =============================================================================
# Part B -- Gap 2: Calendario day-modal batch delete now also logs a
# structured provision.deleted entry per provision (R/ledger_module.R)
# =============================================================================
cat("\n-- Part B: Calendario batch delete logs provision.deleted per provision --\n")

ledger_txt <- readLines("R/ledger_module.R", warn = FALSE)
del_start  <- grep('provs_db\\[del_idx, "estado"\\] <- "deleted"', ledger_txt)
.chk_true(length(del_start) == 1, "found the Calendario provision soft-delete mutation to scan")

if (length(del_start) == 1) {
  # Window: from just before the mutation through ~25 lines after, enough
  # to cover the save, the success gate, and the new per-row audit loop.
  win_start <- max(1, del_start[1] - 6)
  win_end   <- min(length(ledger_txt), del_start[1] + 25)
  del_block <- paste(ledger_txt[win_start:win_end], collapse = "\n")

  .chk_true(grepl("before_rows <- provs_db\\[del_idx, , drop = FALSE\\]", del_block),
       "captures before_rows BEFORE the estado mutation")
  .chk_true(grepl('pasivos_log_audit\\(', del_block) &&
            grepl('action_type = "provision.deleted"', del_block),
       "calls pasivos_log_audit(action_type = \"provision.deleted\") -- previously only the generic ledger log fired")
  .chk_true(grepl("before      = as\\.list\\(before_rows\\[i, , drop = FALSE\\]\\)", del_block),
       "passes a real before= (the pre-mutation row), not just a bare notes string")
  .chk_true(grepl("after       = as\\.list\\(after_row\\[1, , drop = FALSE\\]\\)", del_block),
       "passes a real after= (the post-mutation row)")

  # Ordering safety: the per-row audit loop must be nested INSIDE the
  # `if (isTRUE(prov_result$ok))` success branch, not run unconditionally --
  # a failed save must never produce a log claiming the provision was
  # deleted when it wasn't durably saved.
  ok_pos   <- regexpr("if \\(isTRUE\\(prov_result\\$ok\\)\\)", del_block)
  loop_pos <- regexpr("for \\(i in seq_len\\(nrow\\(before_rows\\)\\)\\)", del_block)
  audit_pos <- regexpr('action_type = "provision.deleted"', del_block)
  .chk_true(ok_pos > 0 && loop_pos > 0 && audit_pos > 0 &&
            ok_pos < loop_pos && loop_pos < audit_pos,
       "the provision.deleted audit loop is nested inside the successful-save branch, in that order (a failed save logs nothing)")
}

# Regression guard: my own explanatory comment near this code must not
# contain the literal substring "log_action(" -- that string is what
# tests/test_saas_log_action_scoping.R's static scanner keys off of to find
# EVERY log_action() call site in this file and verify it passes
# client_id=/viewer_home_client_id=; a comment merely describing the OLD
# generic call in prose would be misdetected as a new call site with a
# 16-line scan window that doesn't reach the real call's arguments,
# producing a false failure unrelated to this fix (hit and fixed once
# already while writing this same change).
comment_zone <- ledger_txt[max(1, del_start[1] - 10):(del_start[1] + 2)]
.chk_false(any(grepl("log_action\\(", comment_zone, fixed = TRUE)),
     "no comment near the new audit block literally contains the substring log_action( (would false-positive the log_action scoping scanner)")

# =============================================================================
# Part C -- Gap 3: liability restore feature built from scratch
# (R/pasivos_list_module.R)
# =============================================================================
cat("\n-- Part C: liability restore feature (previously unimplemented) --\n")

list_txt <- readLines("R/pasivos_list_module.R", warn = FALSE)

.chk_true(any(grepl("is_deleted <- identical\\(estado, \"deleted\"\\)", list_txt)),
     "row-rendering computes is_deleted from the liability's own estado")
.chk_true(any(grepl('pause_btn <- if \\(!is_deleted\\)', list_txt)),
     "pause/resume button only renders for a non-archived liability")
.chk_true(any(grepl('archive_btn <- if \\(!is_deleted\\)', list_txt)),
     "archive button only renders for a non-archived liability")
.chk_true(any(grepl('restore_btn <- if \\(is_deleted\\)', list_txt)),
     "a dedicated restore button renders only for an archived liability")
.chk_true(any(grepl('pasivos_list_restore', list_txt)),
     "restore button fires a dedicated pasivos_list_restore input (not reusing pasivos_list_pause)")
.chk_true(any(grepl('acciones\\s*=\\s*paste0\\(edit_btn, " ", pause_btn, " ", archive_btn, " ", restore_btn\\)', list_txt)),
     "the acciones column includes restore_btn alongside the other action buttons")

restore_start <- grep("observeEvent\\(input\\$pasivos_list_restore, ignoreInit = TRUE", list_txt)
.chk_true(length(restore_start) == 1, "found exactly one pasivos_list_restore observer")

if (length(restore_start) == 1) {
  restore_block <- paste(list_txt[restore_start[1]:min(restore_start[1] + 40, length(list_txt))], collapse = "\n")

  .chk_true(grepl('has_capability\\(user, "pasivos\\.delete_liability"\\)', restore_block),
       "restore is gated behind the same capability as archive/delete (un-archiving is at least as privileged)")
  .chk_true(grepl('if \\(!identical\\(old_estado, "deleted"\\)\\) return\\(\\)', restore_block),
       "guards against restoring a liability that isn't actually archived")
  .chk_true(grepl('liabs\\$estado\\[idx\\[1\\]\\]\\s*<-\\s*"active"', restore_block),
       "restore sets estado back to 'active'")
  .chk_true(grepl('save_pasivos_liabilities\\(liabs,', restore_block),
       "restore persists the liability change")
  .chk_true(grepl('action_type = "liability.restored"', restore_block),
       "restore logs the (previously dead) liability.restored action type")
  .chk_true(grepl("before      = list\\(nombre = nombre, estado = old_estado\\)", restore_block),
       "restore's audit entry captures the real before-estado (was archived), not a placeholder")
  .chk_true(grepl('after       = list\\(nombre = nombre, estado = "active"\\)', restore_block),
       "restore's audit entry captures the real after-estado (active)")
}

# The pause handler must now refuse to run on an already-archived liability
# (previously it would silently flip estado -> "active" via the "Reanudar"
# label with no restore semantics, no matching capability check, and the
# wrong action-type/notes).
pause_start <- grep("observeEvent\\(input\\$pasivos_list_pause, ignoreInit = TRUE", list_txt)
.chk_true(length(pause_start) == 1, "found exactly one pasivos_list_pause observer")
if (length(pause_start) == 1) {
  pause_head <- paste(list_txt[pause_start[1]:min(pause_start[1] + 15, length(list_txt))], collapse = "\n")
  .chk_true(grepl('if \\(identical\\(old_estado, "deleted"\\)\\) return\\(\\)', pause_head),
       "pause/resume now refuses to act on an already-archived liability (must use restore instead)")
}

# liability.restored actually has a real call site now -- previously zero,
# anywhere in R/ (confirmed during the research pass before this fix).
all_r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
restored_hits <- vapply(all_r_files, function(f) {
  any(grepl('action_type\\s*=\\s*"liability\\.restored"', readLines(f, warn = FALSE)))
}, logical(1))
.chk_true(sum(restored_hits) >= 1,
     "liability.restored now has at least one real call site in R/ (was zero before this fix)")

# =============================================================================
# Part D -- Gap 4: side-effect provision drops (pause + archive) are now
# logged individually, not just an uncounted mention on the parent entry
# =============================================================================
cat("\n-- Part D: individual audit entries for provisions dropped by pause/archive --\n")

.chk_true(any(grepl('notes\\s*=\\s*sprintf\\("removed as future provisional provision when liability .* was paused"', list_txt)),
     "pause handler logs each dropped provision individually, naming the liability and the reason")
.chk_true(any(grepl('notes\\s*=\\s*sprintf\\("removed as future provisional provision when liability .* was archived"', list_txt)),
     "archive handler logs each dropped provision individually, naming the liability and the reason")

# Both handlers must capture the full pre-removal row as `before`, not just
# an id -- an auditor reconstructing "what exactly was this provision"
# needs the row content, since the row itself no longer exists afterward
# (these are hard-removed from the table, not soft-deleted in place).
.chk (sum(grepl("before\\s*=\\s*as\\.list\\(dropped_rows\\[i, , drop = FALSE\\]\\)", list_txt)), 2L,
     "exactly 2 sites (pause + archive) capture the full dropped-provision row as before=")

# The archive handler's own liability-level entry must now count how many
# provisions were dropped alongside it (the pause handler's equivalent
# entry doesn't need a count -- each drop already gets its own entry, and
# the liability-level notes there already describes the estado change).
.chk_true(any(grepl('notes\\s*=\\s*sprintf\\("%d provisión\\(es\\) futura\\(s\\) eliminada\\(s\\) junto con el pasivo"', list_txt)),
     "archive's liability.deleted entry names how many provisions were dropped alongside it")

# =============================================================================
# Part E -- functional: the drop_mask boolean logic, extracted verbatim from
# source and evaluated against synthetic data (both call sites use the
# identical 3-line expression -- extract once, treat as representative)
# =============================================================================
cat("\n-- Part E: drop_mask logic (extracted + evaluated against synthetic data) --\n")

dm_start <- grep("^\\s*drop_mask <- !is\\.na\\(provs\\$liability_id\\)", list_txt)
.chk_true(length(dm_start) == 2, "found exactly 2 occurrences of the drop_mask expression (pause + archive)")

if (length(dm_start) >= 1) {
  dm_code <- paste(list_txt[dm_start[1]:(dm_start[1] + 2)], collapse = "\n")
  compute_drop_mask <- tryCatch(
    eval(parse(text = paste0("function(provs, lid) { ", dm_code, " }"))),
    error = function(e) NULL
  )
  .chk_true(!is.null(compute_drop_mask), "drop_mask expression parses and evaluates as a standalone function")

  if (!is.null(compute_drop_mask)) {
    today <- Sys.Date()
    provs <- data.frame(
      id             = c("p1", "p2", "p3", "p4", "p5", "p6"),
      liability_id   = c("L1", "L1", "L1", NA_character_, "L2", "L1"),
      estado         = c("provisional", "converted", "provisional", "provisional", "provisional", NA_character_),
      fecha_efectiva = as.Date(c(today + 10, today + 10, today - 5, today + 10, today + 10, today + 10)),
      stringsAsFactors = FALSE
    )
    mask <- compute_drop_mask(provs, "L1")

    .chk_true(is.logical(mask) && !any(is.na(mask)),
         "drop_mask never returns NA (every row resolves to a clean TRUE/FALSE)")
    .chk_true(mask[1], "p1 (L1, provisional, future) is correctly flagged for removal")
    .chk_false(mask[2], "p2 (L1, but 'converted' not 'provisional') is correctly kept")
    .chk_false(mask[3], "p3 (L1, provisional, but in the PAST) is correctly kept")
    .chk_false(mask[4], "p4 (liability_id is NA) is correctly kept, not wrongly dropped")
    .chk_false(mask[5], "p5 (belongs to a DIFFERENT liability, L2) is correctly kept")
    .chk_false(mask[6], "p6 (estado is NA) is correctly kept, not wrongly dropped")
    .chk (sum(mask), 1L, "exactly 1 of 6 synthetic rows is flagged for removal")
  }
}

cat(sprintf("\n  Pasivos audit-completeness subtotal: %d passed, %d failed\n", .pass, .fail))
