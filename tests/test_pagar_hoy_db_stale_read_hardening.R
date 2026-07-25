# =============================================================================
# tests/test_pagar_hoy_db_stale_read_hardening.R
# Stage 16 of docs/LEDGER_INTEGRITY_MASTER_PLAN.md: Stage 14 hardened
# manual_inv's 6 highest-stakes read sites against the cross-session
# staleness race (a stale in-memory reactiveVal silently overwriting a
# fresher session's write on the next save_*() call), but explicitly left
# pagar_hoy_db's own ~19 (verified: 21) read-modify-write sites unhardened.
# This stage closes the same gap at the 7 highest-stakes pagar_hoy_db sites
# -- the ones in the same severity tier as manual_inv's (confirm/archive
# handlers and bulk-destructive removal), using the identical pattern:
# read fresh via load_pagar_hoy(client_id=...) first, fall back to the
# in-memory reactiveVal only on a genuine read error.
#
# The 7 sites hardened this stage:
#   1. do_confirm_ap_<emp>  -- R/pagar_hoy_module.R
#   2. do_confirm_ar_<emp>  -- R/pagar_hoy_module.R (identical block to #1)
#   3. remove_ap_<emp> (Quitar)  -- R/pagar_hoy_module.R
#   4. remove_ar_<emp> (Quitar)  -- R/pagar_hoy_module.R (identical block to #3)
#   5. do_clear_all (Vaciar agenda) -- R/pagar_hoy_module.R -- the largest
#      blast radius of any site: wipes every pending row for every user.
#   6. Pasivos: revert a converted provision -- R/pasivos_module.R
#   7. Pasivos: delete a converted provision -- R/pasivos_module.R (identical
#      block to #6)
#
# Deliberately NOT hardened this stage (documented, not a silent gap --
# see docs/LEDGER_INTEGRITY_MASTER_PLAN.md's open items): the remaining 14
# Medium-severity sites (bulk/single-row stage buttons across ledger_module.R,
# search_module.R, treasury_map_module.R, interco_module.R,
# staging_browse_module.R, pasivos_module.R, app.R, and the three .sync_staged
# call sites) -- lower blast radius per the same reasoning Stage 14 used to
# scope its own 6-of-many pass (Agenda never holds real data; a race there
# is self-healing via the 8-second poll, not permanent data loss).
#
# REGRESSION, found and fixed same day (2026-07-24/25): the initial version
# of this stage used plain load_pagar_hoy() at all 7 sites. That function
# ALWAYS reads the legacy S3_KEYS$pagar_hoy key, regardless of which storage
# mode is actually active -- but save_pagar_hoy() branches three ways
# (sync-shared file, per-user file, or that same legacy file) depending on
# whether "sync mode" is on and whether a username was given. For a client
# with sync mode on, every write in production goes to pagar_hoy_sync.rds,
# while every one of this stage's "fresh" reads kept pulling a frozen,
# months-old snapshot from the abandoned legacy key (confirmed live:
# staged_by="anon", staged_at=NA on every row -- leftover seed data, not a
# single real user action). Because these are read-modify-WHOLE-TABLE-write
# sites, every write silently replaced the correct live sync state with
# that stale legacy snapshot -- reproducing live as a user watching invoices
# they'd already confirmed get resurrected, and confirming one company's
# queue wiping out another's just-confirmed state on the very next save
# ("whack-a-mole"). Fixed by switching all 7 sites to safe_load_pagar_hoy(),
# the existing dispatch helper (R/persistence.R) that already replicates
# save_pagar_hoy()'s own three-way branch -- the same function app.R's
# me_save already trusted for this exact reason.
# =============================================================================

cat("── pagar_hoy_db: 7 highest-stakes sites read fresh from S3 ─────────────\n")

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

.block_info <- function(file, anchor_pattern, occurrence = 1L, window = 20) {
  txt <- readLines(file, warn = FALSE)
  s <- grep(anchor_pattern, txt)
  if (length(s) < occurrence) return(NULL)
  start <- s[occurrence]
  block <- paste(.strip_comments(txt[start:min(start + window, length(txt))]), collapse = "\n")
  list(
    has_safe = grepl("safe_load_pagar_hoy\\(", block),
    # A bare (non-"safe_"-prefixed) load_pagar_hoy( call is the exact
    # regression found 2026-07-24/25 -- strip out every safe_load_pagar_hoy(
    # occurrence first, then check whether the plain call still remains.
    has_bare = grepl("load_pagar_hoy\\(", gsub("safe_load_pagar_hoy\\(", "", block))
  )
}

# ── Static scan, one assertion per hardened site ────────────────────────────
{
  sites <- list(
    list(file = "R/pagar_hoy_module.R",
         anchor = "manual_inv-archiving decision, not for what happens here\\.",
         occurrence = 1L, label = "do_confirm_ap_<emp>"),
    list(file = "R/pagar_hoy_module.R",
         anchor = "manual_inv-archiving decision, not for what happens here\\.",
         occurrence = 2L, label = "do_confirm_ar_<emp>"),
    list(file = "R/pagar_hoy_module.R",
         anchor = "removing a reference from it can never destroy anything; the only",
         occurrence = 1L, label = "remove_ap_<emp> (Quitar)"),
    list(file = "R/pagar_hoy_module.R",
         anchor = "removing a reference from it can never destroy anything; the only",
         occurrence = 2L, label = "remove_ar_<emp> (Quitar)"),
    list(file = "R/pagar_hoy_module.R",
         anchor = "largest blast radius of any pagar_hoy_db site",
         occurrence = 1L, label = "do_clear_all (Vaciar agenda)"),
    list(file = "R/pasivos_module.R",
         anchor = "business key so calendar-staged rows \\(no FK\\) are also cleaned up\\.",
         occurrence = 1L, label = "pcm_do_revert_converted (revert provision)"),
    list(file = "R/pasivos_module.R",
         anchor = "business key so calendar-staged rows \\(no FK\\) are also cleaned up\\.",
         occurrence = 2L, label = "pcm_do_delete_converted (delete provision)")
  )
  for (s in sites) {
    res <- .block_info(s$file, s$anchor, s$occurrence)
    .chk(is.null(res), FALSE, sprintf("found anchor for %s in %s (occurrence %d)", s$label, s$file, s$occurrence))
    if (!is.null(res)) {
      .chk(res$has_safe, TRUE,
           sprintf("%s reads fresh via safe_load_pagar_hoy() before its write-back", s$label))
      .chk(res$has_bare, FALSE,
           sprintf("%s does NOT call the bare (mode-blind) load_pagar_hoy() -- that's the exact regression this file now guards against", s$label))
    }
  }
}

# ── Regression guard: fallback precedence is fresh-first, not stale-first ──
# The pasivos_module.R sites originally read `shared$pagar_hoy_db()` FIRST
# and only fell back to a fresh read if the accessor didn't exist at all
# -- meaning the fresh read almost never actually ran. Assert the corrected
# precedence explicitly, not just that a fresh-read call appears somewhere
# in the block (which the buggy version also technically satisfied).
{
  txt <- readLines("R/pasivos_module.R", warn = FALSE)
  s <- grep("business key so calendar-staged rows \\(no FK\\) are also cleaned up\\.", txt)
  .chk(length(s) >= 2, TRUE, "found both pasivos pagar_hoy read sites to check precedence")
  if (length(s) >= 2) {
    for (i in seq_along(s)) {
      block <- paste(.strip_comments(txt[s[i]:min(s[i] + 6, length(txt))]), collapse = "\n")
      load_pos  <- regexpr("safe_load_pagar_hoy\\(", block)
      share_pos <- regexpr("shared\\$pagar_hoy_db\\(\\)", block)
      .chk(load_pos > 0 && share_pos > 0 && load_pos < share_pos, TRUE,
           sprintf("pasivos site #%d: safe_load_pagar_hoy() is tried BEFORE shared$pagar_hoy_db(), not after", i))
    }
  }
}

# ── Regression guard: every hardened site uses the mode-aware dispatcher,
# passing a username -- safe_load_pagar_hoy(username=NULL) collapses to the
# same mode-blind legacy-key read as the bare function for the sync-off,
# no-username branch, so a call missing username= would silently reintroduce
# a variant of the same regression whenever sync mode is off. ───────────────
{
  for (s in list(
    list(file = "R/pagar_hoy_module.R", anchor = "manual_inv-archiving decision, not for what happens here\\.", occurrence = 1L, label = "do_confirm_ap_<emp>"),
    list(file = "R/pagar_hoy_module.R", anchor = "manual_inv-archiving decision, not for what happens here\\.", occurrence = 2L, label = "do_confirm_ar_<emp>"),
    list(file = "R/pagar_hoy_module.R", anchor = "removing a reference from it can never destroy anything; the only", occurrence = 1L, label = "remove_ap_<emp>"),
    list(file = "R/pagar_hoy_module.R", anchor = "removing a reference from it can never destroy anything; the only", occurrence = 2L, label = "remove_ar_<emp>"),
    list(file = "R/pagar_hoy_module.R", anchor = "largest blast radius of any pagar_hoy_db site", occurrence = 1L, label = "do_clear_all"),
    list(file = "R/pasivos_module.R", anchor = "business key so calendar-staged rows \\(no FK\\) are also cleaned up\\.", occurrence = 1L, label = "pcm_do_revert_converted"),
    list(file = "R/pasivos_module.R", anchor = "business key so calendar-staged rows \\(no FK\\) are also cleaned up\\.", occurrence = 2L, label = "pcm_do_delete_converted")
  )) {
    txt <- readLines(s$file, warn = FALSE)
    idx <- grep(s$anchor, txt)
    if (length(idx) >= s$occurrence) {
      block <- paste(.strip_comments(txt[idx[s$occurrence]:min(idx[s$occurrence] + 20, length(txt))]), collapse = "\n")
      .chk(grepl("safe_load_pagar_hoy\\(\\s*username\\s*=", block), TRUE,
           sprintf("%s passes username= to safe_load_pagar_hoy() (required for the per-user-mode branch to work)", s$label))
    }
  }
}

# ── Behavioral: the stale-vs-fresh read pattern itself ──────────────────────
# Same simulation shape as test_manual_inv_sync_registration.R's Tier 2
# check -- proves the %||% fallback actually prefers a successful fresh
# read over the stale in-memory value, and only falls back on a genuine
# read error, using the real fallback expression pattern.
{
  fresh_ok  <- function() tibble::tibble(id = "fresh-row")
  fresh_err <- function() stop("S3 unreachable")
  stale     <- tibble::tibble(id = "stale-row")

  read_fresh_first <- function(loader) {
    tryCatch(loader(), error = function(e) NULL) %||% stale
  }
  "%||%" <- function(a, b) if (!is.null(a)) a else b

  res_ok  <- read_fresh_first(fresh_ok)
  res_err <- read_fresh_first(fresh_err)
  .chk(res_ok$id, "fresh-row", "when the fresh read succeeds, its result wins over the stale fallback")
  .chk(res_err$id, "stale-row", "when the fresh read errors, the stale in-memory value is still used as a safety net, not a hard failure")
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
