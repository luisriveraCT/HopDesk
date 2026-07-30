# =============================================================================
# tests/test_pagar_hoy_db_remaining_sites_hardening.R
# Stage 19 of docs/LEDGER_INTEGRITY_MASTER_PLAN.md: Stage 16 hardened
# pagar_hoy_db's 7 highest-stakes read-modify-write sites (confirm/archive/
# Vaciar agenda), deliberately deferring the remaining 14 medium-severity
# sites as an explicit, documented open item. This stage closes that gap,
# using the corrected pattern from Stage 16's own correction: read fresh via
# safe_load_pagar_hoy() (mode-aware -- sync/per-user/legacy), NEVER the bare
# load_pagar_hoy() (mode-blind, corrupted the live agenda when Stage 16
# first shipped -- see docs for the full incident).
#
# Sites hardened this stage (13 code locations covering the 14-site count --
# .sync_staged is one shared helper feeding 3 call sites: do_move,
# do_restore, sap_edit_save):
#   1.  stage_all              -- R/ledger_module.R
#   2.  stage_sel               -- R/ledger_module.R
#   3.  cart_<i> (group toggle) -- R/ledger_module.R (both stage & unstage branches)
#   4.  cart_inv_click          -- R/ledger_module.R (both stage & unstage branches)
#   5.  .sync_staged            -- R/ledger_module.R (shared helper: do_move/
#                                  do_restore/sap_edit_save, plus app.R's
#                                  me_save edit-mode caller, which was ALSO
#                                  missing username=/client_id= entirely)
#   6.  handle_invoice_action's stage_all/stage_selected -- R/search_module.R
#   7.  ab_rows (abono staging) -- R/staging_browse_module.R
#   8.  send_to_agenda          -- R/treasury_map_module.R
#   9.  .ic_send_rows           -- R/interco_module.R
#   10. .pasivos_perform_conversion's stage-to-agenda -- R/pasivos_module.R
#   11. me_save insert-mode stage-to-agenda -- app.R (found with the
#       precedence backwards too -- pagar_hoy_db() tried before
#       safe_load_pagar_hoy(), same mistake Stage 16 originally made)
#
# Deliberately NOT touched (confirmed correct, not part of this sweep):
#   - app.R's session-init "Phase 2" block (~line 1731) -- this code IS the
#     mode-detection logic safe_load_pagar_hoy() abstracts (it decides
#     sync/per-user/legacy for the whole session); calling
#     safe_load_pagar_hoy() from inside it would be circular.
#   - Every other shared$pagar_hoy_db()/pagar_hoy_db() read found in R/*.R
#     is a pure display/dependency reactive with no write following it
#     (verified individually: R/ledger_module.R's staged_keys_rv, the
#     EnProceso block, output$cart_table's own display read;
#     R/interco_module.R's reactive-dependency read; R/pagar_hoy_module.R's
#     `staged()` reactive).
# =============================================================================

cat("── pagar_hoy_db: remaining 13 sites read fresh via safe_load_pagar_hoy ──\n")

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
.strip_comments <- function(lines) sub("#.*$", "", lines)

.block_info <- function(file, anchor_pattern, occurrence = 1L, window = 15) {
  txt <- readLines(file, warn = FALSE)
  s <- grep(anchor_pattern, txt)
  if (length(s) < occurrence) return(NULL)
  start <- s[occurrence]
  block <- paste(.strip_comments(txt[start:min(start + window, length(txt))]), collapse = "\n")
  list(
    has_safe = grepl("safe_load_pagar_hoy\\(", block),
    has_bare = grepl("load_pagar_hoy\\(", gsub("safe_load_pagar_hoy\\(", "", block))
  )
}

sites <- list(
  list(file = "R/ledger_module.R", anchor = "observeEvent\\(input\\$stage_all,", occurrence = 1L, label = "stage_all", window = 55),
  list(file = "R/ledger_module.R", anchor = "observeEvent\\(input\\$stage_sel,", occurrence = 1L, label = "stage_sel", window = 60),
  list(file = "R/ledger_module.R", anchor = 'observeEvent\\(input\\[\\[paste0\\("cart_", i\\)\\]\\],', occurrence = 1L, label = "cart_<i> group toggle", window = 30),
  list(file = "R/ledger_module.R", anchor = "observeEvent\\(input\\$cart_inv_click,", occurrence = 1L, label = "cart_inv_click", window = 30),
  list(file = "R/ledger_module.R", anchor = "^\\.sync_staged <- function", occurrence = 1L, label = ".sync_staged (shared helper)", window = 20),
  list(file = "R/search_module.R", anchor = 'action %in% c\\("stage_all", "stage_selected"\\)', occurrence = 1L, label = "handle_invoice_action stage_all/stage_selected", window = 50),
  list(file = "R/staging_browse_module.R", anchor = "observeEvent\\(input\\$ab_rows,", occurrence = 1L, label = "ab_rows (abono staging)", window = 50),
  list(file = "R/treasury_map_module.R", anchor = "observeEvent\\(input\\$send_to_agenda,", occurrence = 1L, label = "send_to_agenda", window = 10),
  list(file = "R/interco_module.R", anchor = "\\.ic_send_rows <- function", occurrence = 1L, label = ".ic_send_rows", window = 10),
  list(file = "R/pasivos_module.R", anchor = "\\.pasivos_perform_conversion <- function", occurrence = 1L, label = ".pasivos_perform_conversion stage-to-agenda", window = 130),
  list(file = "app.R", anchor = "Stage to Agenda de hoy if toggle is active", occurrence = 1L, label = "me_save insert-mode stage-to-agenda", window = 16)
)

for (s in sites) {
  win <- if (!is.null(s$window)) s$window else 15
  res <- .block_info(s$file, s$anchor, s$occurrence, window = win)
  .chk(is.null(res), FALSE, sprintf("found anchor for %s in %s", s$label, s$file))
  if (!is.null(res)) {
    .chk(res$has_safe, TRUE, sprintf("%s reads fresh via safe_load_pagar_hoy()", s$label))
    .chk(res$has_bare, FALSE, sprintf("%s does NOT call the bare (mode-blind) load_pagar_hoy()", s$label))
  }
}

# ── .sync_staged's 4 call sites all pass username=/client_id= ──────────────
# (Required for the fresh read to actually work -- safe_load_pagar_hoy()
# with username=NULL, client_id=NULL falls all the way through to the same
# mode-blind legacy read this stage exists to eliminate.)
{
  ls_txt <- readLines("R/ledger_module.R", warn = FALSE)
  ls_calls <- grep("\\.sync_staged\\(", ls_txt)
  .chk(length(ls_calls), 3L, "found all 3 R/ledger_module.R .sync_staged() call sites (do_move/do_restore/sap_edit_save)")
  for (i in seq_along(ls_calls)) {
    block <- paste(.strip_comments(ls_txt[ls_calls[i]:min(ls_calls[i] + 16, length(ls_txt))]), collapse = "\n")
    .chk(grepl("username\\s*=", block) && grepl("client_id\\s*=", block), TRUE,
         sprintf("ledger_module.R .sync_staged call #%d passes username= and client_id=", i))
  }

  app_txt <- readLines("app.R", warn = FALSE)
  app_calls <- grep("\\.sync_staged\\(", app_txt)
  .chk(length(app_calls) >= 1, TRUE, "found app.R's .sync_staged() call site (me_save edit-mode)")
  if (length(app_calls)) {
    block <- paste(.strip_comments(app_txt[app_calls[1]:min(app_calls[1] + 12, length(app_txt))]), collapse = "\n")
    .chk(grepl("username\\s*=", block) && grepl("client_id\\s*=", block), TRUE,
         "app.R's .sync_staged call now passes username= and client_id= (found 2026-07-25: it previously passed neither)")
  }
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
