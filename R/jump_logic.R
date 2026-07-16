# =============================================================================
# R/jump_logic.R
# Stage 2 Part A: the two decisions in the staff jump mechanism that are
# genuinely logic (not just reactive wiring), extracted so they're testable
# without a running Shiny session. R/tiers_module.R calls these directly —
# not a re-implementation, the actual enforcement.
# =============================================================================

# Picking your own home means going home, not "jumping" to it.
resolve_jump_target <- function(new_cid, home_cid) {
  if (identical(new_cid, home_cid)) NULL else new_cid
}

# The hop-grant watchdog must poll only a session genuinely mid-jump.
# principal never needs a grant row to jump, so polling a principal session
# would find no matching grant and incorrectly read as "revoked".
hop_watchdog_should_poll <- function(is_principal, jump_cid) {
  !isTRUE(is_principal) && !is.null(jump_cid)
}
