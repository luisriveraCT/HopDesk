# =============================================================================
# tests/test_manual_inv_sync_registration.R
# Real, active concurrency incident found 2026-07-24 (docs/LEDGER_INTEGRITY_
# MASTER_PLAN.md Stage 14, previously deferred as hypothetical, now
# confirmed reproducing): manual_inv was the only shared table completely
# absent from the app's cross-session sync registry (R/sync_bus.R). Every
# other table (pagar_hoy_db, bancos_confirmados_db, papelera_rv, ...) polls
# and self-heals across open tabs every 8 seconds; manual_inv never
# refreshed in an already-open tab, so a stale tab could silently overwrite
# a fresher session's (or a direct S3 repair's) changes on its next write.
#
# A second, independent gap made even registering manual_inv insufficient:
# every loader (load_manual/load_papelera/etc.) goes through
# .s3_read_with(), which checks the process-global preload cache BEFORE
# touching S3 -- a cache only cleared by THIS process's own .s3_write() for
# that exact key. A poll-triggered reload would therefore still serve a
# stale preloaded snapshot even after a correct version bump, which is
# exactly why a direct-S3 repair was never picked up by the running app.
#
# Fixed: manual_inv registered in the sync bus; save_manual() bumps its
# version; setup_sync_bus()'s reload loop clears the preload cache for
# whichever key it's about to reload (closing the gap for every registered
# key, not just manual_inv); and the highest-stakes manual_inv
# archive/confirm/undo/delete read sites now read fresh from S3 instead of
# trusting the in-memory reactiveVal.
#
# Four kinds of checks: static scans for the three sync-bus changes, static
# scans for each Tier 2 site, a synthetic-data behavioral simulation of the
# stale-vs-fresh read pattern itself, and a regression guard against a
# future isolate() re-introduction breaking Tier 1's "no downstream change
# needed" claim.
# =============================================================================

cat("── manual_inv cross-session sync + stale-read hardening ────────────────\n")

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

# ── 1. Tier 1: sync-bus registration and bump ──────────────────────────────
{
  app_txt <- readLines("app.R", warn = FALSE)
  .chk(any(grepl('register_synced\\("manual_inv"', app_txt)), TRUE,
       "app.R registers manual_inv in the sync bus")

  pers_txt <- readLines("R/persistence.R", warn = FALSE)
  sm_start <- grep("^save_manual <- function", pers_txt)
  .chk(length(sm_start) > 0, TRUE, "found save_manual() to scan")
  if (length(sm_start)) {
    sm_block <- paste(.strip_comments(pers_txt[sm_start[1]:min(sm_start[1] + 6, length(pers_txt))]), collapse = "\n")
    .chk(grepl('bump_sync_version\\("manual_inv"', sm_block), TRUE,
         "save_manual() bumps the manual_inv sync version on every write")
  }
}

# ── 2. Tier 1: preload-cache clear in the reload loop ──────────────────────
{
  sb_txt <- readLines("R/sync_bus.R", warn = FALSE)
  loop_start <- grep("Per-key reload", sb_txt)
  .chk(length(loop_start) > 0, TRUE, "found setup_sync_bus()'s per-key reload loop to scan")
  if (length(loop_start)) {
    loop_block <- paste(.strip_comments(sb_txt[loop_start[1]:min(loop_start[1] + 30, length(sb_txt))]), collapse = "\n")
    .chk(grepl("\\.s3_preload_cache", loop_block), TRUE,
         "the reload loop touches .s3_preload_cache")
    .chk(grepl("rm\\(list = intersect\\(key_full", loop_block), TRUE,
         "the reload loop clears the preload-cache entry before calling entry$loader()")
    # The clear must happen BEFORE the loader call, not after -- order matters.
    clear_pos  <- regexpr("rm\\(list = intersect\\(key_full", loop_block)
    loader_pos <- regexpr("entry\\$loader\\(", loop_block)
    .chk(clear_pos > 0 && loader_pos > 0 && clear_pos < loader_pos, TRUE,
         "the preload-cache clear happens BEFORE entry$loader() is called, not after")
  }
}

# ── 3. Tier 2: static scan, one assertion per hardened site ────────────────
{
  .block_contains_load_manual <- function(file, anchor_pattern, window = 20) {
    txt <- readLines(file, warn = FALSE)
    s <- grep(anchor_pattern, txt)
    if (!length(s)) return(NA)
    block <- paste(.strip_comments(txt[s[1]:min(s[1] + window, length(txt))]), collapse = "\n")
    grepl("load_manual\\(", block)
  }

  sites <- list(
    list(file = "R/pagar_hoy_module.R", anchor = "Plain manual entries \\(no provision_id\\)",
         label = "do_confirm_ap_<emp> manual-archive block"),
    list(file = "R/bancos_module.R", anchor = "if \\(!is\\.null\\(restore_result\\)\\) \\{",
         label = "undo_conf/recover flow"),
    list(file = "R/ledger_module.R", anchor = "For manual invoices: remove from manual_inv",
         label = "confirm_delete bulk archive"),
    list(file = "R/search_module.R", anchor = "manual_rows <- item_rows\\[item_rows\\$source",
         label = "handle_invoice_action delete/archive branch"),
    list(file = "R/pasivos_module.R", anchor = "manual_df <- ",
         label = ".pasivos_perform_conversion"),
    list(file = "R/pagar_hoy_module.R", anchor = "if \\(length\\(manual_ids\\)\\) \\{",
         label = "Quitar-cascade (remove_ap/ar)")
  )
  for (s in sites) {
    res <- .block_contains_load_manual(s$file, s$anchor)
    .chk(is.na(res), FALSE, sprintf("found anchor for %s in %s", s$label, s$file))
    if (!is.na(res)) {
      .chk(res, TRUE, sprintf("%s reads manual_inv fresh via load_manual()", s$label))
    }
  }

  # do_confirm_ap_<emp>/do_confirm_ar_<emp> must have EXACTLY one load_manual()
  # call feeding the whole handler (both the plain-manual and provision-
  # derived blocks) -- a second, independent fetch would silently discard
  # the first block's own archive-removal before the second block runs.
  ph_txt <- readLines("R/pagar_hoy_module.R", warn = FALSE)
  archive_starts <- grep("Plain manual entries \\(no provision_id\\)", ph_txt)
  .chk(length(archive_starts), 2L,
       "exactly 2 manual-archive blocks found (AP + AR confirm handlers)")
  for (i in seq_along(archive_starts)) {
    block <- paste(.strip_comments(ph_txt[archive_starts[i]:min(archive_starts[i] + 60, length(ph_txt))]), collapse = "\n")
    n_loads <- length(gregexpr("load_manual\\(", block)[[1]])
    n_loads <- if (identical(gregexpr("load_manual\\(", block)[[1]], -1L)) 0L else n_loads
    .chk(n_loads, 1L,
         sprintf("manual-archive block %d calls load_manual() exactly once (single fetch, threaded through)", i))
  }
}

# ── 4. Behavioral simulation: fresh-read pattern actually prefers fresh ────
{
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  stale <- data.frame(id = c("A", "B"), val = c(1, 2), stringsAsFactors = FALSE)   # this session's old snapshot
  fresh <- data.frame(id = "B", val = 2, stringsAsFactors = FALSE)                  # actual current S3 state -- "A" already removed elsewhere

  mock_load_manual_ok   <- function(client_id = NULL) fresh
  mock_load_manual_fail <- function(client_id = NULL) stop("S3 unreachable")
  mock_shared_manual_inv <- function() stale

  fixed_result <- tryCatch(mock_load_manual_ok(client_id = NULL), error = function(e) NULL) %||% mock_shared_manual_inv()
  .chk(fixed_result, fresh, "fixed pattern (load_manual %||% shared$manual_inv) returns the fresh S3 state")

  old_result <- mock_shared_manual_inv() %||% tryCatch(mock_load_manual_ok(client_id = NULL), error = function(e) NULL)
  .chk(old_result, stale,
       "negative control: the OLD precedence (shared$manual_inv %||% load_manual) would have returned the stale state -- proves this was a real behavioral gap, not just cosmetic")

  fallback_result <- tryCatch(mock_load_manual_fail(client_id = NULL), error = function(e) NULL) %||% mock_shared_manual_inv()
  .chk(fallback_result, stale,
       "fixed pattern degrades gracefully to the in-memory copy if the fresh S3 read fails (no crash, no data loss vs. today's behavior)")
}

# ── 5. Regression guard: no isolate() blocking Tier 1's auto-invalidation ──
{
  led_txt <- readLines("R/ledger_module.R", warn = FALSE)
  .chk(any(grepl("isolate\\(\\s*shared\\$manual_inv\\(", led_txt)), FALSE,
       "R/ledger_module.R never reads manual_inv via isolate() -- a poll-driven push still invalidates df_combined()/the calendar")
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
