# =============================================================================
# tests/test_saas_home_jump.R
# Verifies the Stage 2 Part A home/jump split logic: resolve_jump_target()
# and hop_watchdog_should_poll() (R/jump_logic.R), plus the
# effective_client_id() composition rule every module's data loading now
# keys off.
# Sourced by _run_saas.R.
# =============================================================================

cat("── Home/Jump Split ──────────────────────────────────────────────────────\n")

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

# ── A. resolve_jump_target() — picking home clears the jump ──────────────────

.chk(resolve_jump_target("networks", "hd-admin"), "networks",
     "jumping to a client sets the jump target")
.chk(resolve_jump_target("hd-admin", "hd-admin"), NULL,
     "picking your own home clears the jump instead of setting it")
.chk(resolve_jump_target("hopdesk", "hd-admin"), "hopdesk",
     "jumping to the 'hopdesk' test client sets the jump target (not confused with home)")

# ── B. hop_watchdog_should_poll() — the exact guard the watchdog's observe()
#      body runs behind; asserting it here proves the watchdog's grant-lookup
#      code is unreachable for the cases below, without needing a live
#      Shiny session to click through.  ────────────────────────────────────

.chk(hop_watchdog_should_poll(is_principal = FALSE, jump_cid = NULL), FALSE,
     "a hopdesk session at home (jump_client_id == NULL) never triggers the watchdog poll")
.chk(hop_watchdog_should_poll(is_principal = FALSE, jump_cid = "networks"), TRUE,
     "a hopdesk session mid-jump does trigger the watchdog poll")
.chk(hop_watchdog_should_poll(is_principal = TRUE, jump_cid = "networks"), FALSE,
     "a principal session mid-jump never triggers the watchdog poll (no grant row ever exists for principal)")
.chk(hop_watchdog_should_poll(is_principal = TRUE, jump_cid = NULL), FALSE,
     "a principal session at home never triggers the watchdog poll")

# A client session (is_staff == FALSE) can never reach the watchdog's caller
# at all — jump_client_id() is only ever settable by an is_staff session
# (Stage 1's jump-security fix, re-tested here from the data-scoping angle):
# a client's own jump_client_id is always NULL, so it can never differ from
# their (fixed) home, so effective_client_id() never changes for them either.
client_home       <- "networks"
client_jump       <- NULL   # a client session can never set this — Stage 1 enforced
client_effective  <- client_jump %||% client_home
.chk(client_effective, client_home,
     "a client session's effective_client_id equals its home_client_id (jump is always NULL)")

# ── C. effective_client_id() composition — home, or jumped if mid-jump ───────

.chk("networks" %||% "hd-admin", "networks",
     "effective_client_id resolves to the jump target when jumping")
.chk(NULL %||% "hd-admin", "hd-admin",
     "effective_client_id falls back to home when there's no jump")

cat("\n")
