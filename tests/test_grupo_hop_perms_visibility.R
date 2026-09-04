# =============================================================================
# tests/test_grupo_hop_perms_visibility.R
#
# Grupo activity-logging audit (2026-09-04), Stage G3: R/tiers_module.R's
# "hop_perms" sub-tab (Accesos de Salto). hop_grants.rds/hop_requests.rds
# physically live only at hd-admin/ (no per-client copy exists), so all 4
# handlers here were ALREADY correctly scoped as hd-admin-only writes --
# this was never a bug. What changes in this stage is a deliberate product
# decision (confirmed with Mouse): a client should be able to see WHICH
# Hopdesk staff had access to their own books and when, not have that live
# purely in Hopdesk's internal bookkeeping. Fixed by changing client_id
# from the hard-coded "hd-admin" to the actual target client for the 3
# single-client handlers (letting log_action()'s existing dual-write also
# keep the hd-admin copy), and by ADDING a per-client loop for the one
# handler that can target several clients in one click (direct grant) --
# that loop deliberately sets viewer_home_client_id = the SAME client_id
# per entry, so log_action()'s dual-write does NOT ALSO duplicate each one
# into hd-admin (the existing hd-admin-only summary call already covers
# that side once).
#
# Static-scan approach, matching test_grupo_clients_scoping.R's rationale,
# plus one functional check of the direct-grant loop's per-client
# no-duplicate-dual-write property (this is a subtle enough invariant that
# a static check alone wouldn't be convincing).
#
# Sourced by _run_confirmed_logic.R.
# =============================================================================

cat("=== test_grupo_hop_perms_visibility ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

tm_txt <- readLines("R/tiers_module.R", warn = FALSE)
ok("R/tiers_module.R still parses without syntax errors",
   { p <- tryCatch({parse("R/tiers_module.R"); TRUE}, error = function(e) FALSE); p })

# Anchored on each action's log_action() call, ending at its
# viewer_home_client_id line (established convention across this whole
# file -- every log_action() call ends with that as its last argument).
.log_action_block <- function(txt, action_literal, occurrence = 1L) {
  al <- grep(sprintf('action\\s*=\\s*"%s"', action_literal), txt)
  ok(sprintf("found occurrence %d of action=\"%s\"", occurrence, action_literal),
     length(al) >= occurrence)
  if (length(al) < occurrence) return(NA_character_)
  start <- al[occurrence]
  while (start > 1 && !grepl("log_action\\(", txt[start])) start <- start - 1
  end <- start
  while (end < length(txt) && !grepl("viewer_home_client_id", txt[end])) end <- end + 1
  paste(txt[start:end], collapse = "\n")
}

# =============================================================================
# Part A -- the 3 single-target handlers now dual-write to the client too
# =============================================================================
cat("\n-- Part A: hop_access_approved / denied / revoked --\n")

b_approved <- .log_action_block(tm_txt, "hop_access_approved")
if (!is.na(b_approved))
  ok("hop_access_approved: client_id is now cid (the granted client), not hard-coded \"hd-admin\"",
     grepl("client_id\\s*=\\s*cid,", b_approved))

b_denied <- .log_action_block(tm_txt, "hop_access_denied")
if (!is.na(b_denied))
  ok("hop_access_denied: client_id is now cid, not hard-coded \"hd-admin\"",
     grepl("client_id\\s*=\\s*cid,", b_denied))

b_revoked <- .log_action_block(tm_txt, "hop_access_revoked")
if (!is.na(b_revoked))
  ok("hop_access_revoked: client_id is now cid_v (the revoked grant's own client), not hard-coded \"hd-admin\"",
     grepl("client_id\\s*=\\s*cid_v,", b_revoked))

# All 3 keep the reviewer's REAL home (not the target client) as
# viewer_home_client_id -- this is what makes log_action()'s own
# dual-write actually deposit a copy in hd-admin too, preserving the
# existing internal bookkeeping visibility these calls always had.
for (blk in list(b_approved, b_denied, b_revoked)) {
  if (!is.na(blk))
    ok("viewer_home_client_id is still the reviewer's real home (shared$home_client_id()), not the target client -- required for the dual-write to actually fire",
       grepl("viewer_home_client_id\\s*=\\s*tryCatch\\(shared\\$home_client_id\\(\\)", blk))
}

# =============================================================================
# Part B -- direct grant: hd-admin summary unchanged, PLUS a new
# per-client loop that reaches each target client's own log without
# duplicating into hd-admin a second time
# =============================================================================
cat("\n-- Part B: hop_access_direct_grant --\n")

# There are now 2 occurrences of this action literal: the pre-existing
# hd-admin summary call, and the new per-client loop call.
dg_occurrences <- grep('action\\s*=\\s*"hop_access_direct_grant"', tm_txt)
ok("found both hop_access_direct_grant log_action() call sites (summary + per-client loop)",
   length(dg_occurrences) == 2L)

b_dg_summary <- .log_action_block(tm_txt, "hop_access_direct_grant", occurrence = 1L)
if (!is.na(b_dg_summary))
  ok("the summary call is STILL hd-admin-only (regression guard -- a multi-client action's Hopdesk-side record shouldn't change)",
     grepl('client_id\\s*=\\s*"hd-admin",', b_dg_summary))

b_dg_loop <- .log_action_block(tm_txt, "hop_access_direct_grant", occurrence = 2L)
if (!is.na(b_dg_loop)) {
  ok("the new per-client loop targets g$client_id (each granted client individually), not a hard-coded value",
     grepl("client_id\\s*=\\s*g\\$client_id,", b_dg_loop))
  ok("the loop's viewer_home_client_id is the SAME g$client_id (deliberately, so the dual-write doesn't ALSO duplicate this into hd-admin -- the summary call above already covers that)",
     grepl("viewer_home_client_id\\s*=\\s*g\\$client_id", b_dg_loop))
}

for_loop_start <- grep("for \\(g in to_grant\\)", tm_txt)
ok("found the per-client for-loop", length(for_loop_start) >= 1)
if (length(for_loop_start)) {
  loop_block <- paste(tm_txt[for_loop_start[1]:(for_loop_start[1] + 12)], collapse = "\n")
  ok("the per-client loop's log_action() call is wrapped in tryCatch (one client's logging failure can't break the others or the grant itself)",
     grepl("tryCatch\\(log_action\\(", loop_block))
}

# =============================================================================
# Part C -- functional: the direct-grant loop's no-duplicate-dual-write
# property, exercised against the REAL log_action() (not a hand-copied
# description of what it does)
# =============================================================================
cat("\n-- Part C: functional -- per-client entries don't duplicate into hd-admin --\n")
{
  .extract_fn <- function(file, fn_name) {
    exprs <- parse(file, keep.source = FALSE)
    for (e in exprs) {
      if (is.call(e) && length(e) >= 2 &&
          identical(e[[1]], as.name("<-")) &&
          identical(e[[2]], as.name(fn_name))) {
        assign(fn_name, eval(e[[3]], envir = globalenv()), envir = globalenv())
        return(invisible(TRUE))
      }
    }
    stop(sprintf("%s not found in %s", fn_name, file))
  }
  assign("%||%", function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x, envir = globalenv())
  queued <- list()
  assign(".audit_queue", function(write_cid, row) { queued[[length(queued) + 1L]] <<- write_cid }, envir = globalenv())
  .extract_fn("R/app_audit.R", "log_action")

  # Mirrors the hd-admin summary call (client_id="hd-admin", home="hd-admin"
  # -- same value, no dual-write) plus 2 per-client loop calls (client_id ==
  # viewer_home_client_id for each, same value, no dual-write either).
  log_action(user = "principal", module = "tiers", action = "hop_access_direct_grant",
            description = "summary", client_id = "hd-admin", viewer_home_client_id = "hd-admin")
  log_action(user = "principal", module = "tiers", action = "hop_access_direct_grant",
            description = "per-client A", client_id = "networks", viewer_home_client_id = "networks")
  log_action(user = "principal", module = "tiers", action = "hop_access_direct_grant",
            description = "per-client B", client_id = "acme", viewer_home_client_id = "acme")

  ok("exactly 3 writes total (1 summary + 2 per-client, NONE duplicated by a spurious dual-write)",
     length(queued) == 3L)
  ok("hd-admin was written to exactly once (the summary only -- not once per client too)",
     sum(queued == "hd-admin") == 1L)
  ok("each granted client's own log was written to exactly once", sum(queued == "networks") == 1L && sum(queued == "acme") == 1L)

  # Control: hop_access_approved's NEW shape (client_id=cid, home="hd-admin",
  # genuinely different) -- THIS one must still dual-write, unlike direct
  # grant's per-client loop. Confirms the two different call shapes in this
  # same stage produce the two different, both-intentional behaviors.
  queued <- list()
  log_action(user = "principal", module = "tiers", action = "hop_access_approved",
            description = "test", client_id = "networks", viewer_home_client_id = "hd-admin")
  ok("hop_access_approved's shape (client_id != viewer_home_client_id) still dual-writes to both logs",
     length(queued) == 2L && "networks" %in% queued && "hd-admin" %in% queued)
}

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0) stop("Tests FAILED.")
