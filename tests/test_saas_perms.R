# =============================================================================
# tests/test_saas_perms.R
# Verifies auth_resolve_perms(): tier defaults, override merging, locked perms.
# Sourced by _run_saas.R.
# =============================================================================

cat("── Permission System ────────────────────────────────────────────────────\n")

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

# ── A. Default tier permissions ───────────────────────────────────────────────

TIERS <- c("principal", "hopdesk", "dev", "admin", "finance", "analysis")
for (t in TIERS) {
  p <- auth_resolve_perms(tier = t, permisos_json = "{}")
  .chk(is.list(p), TRUE, sprintf("auth_resolve_perms('%s') returns a list", t))
}

# principal has can_approve_clients and can_manage_hopdesk_perms TRUE by default
pp <- auth_resolve_perms("principal", "{}")
.chk(isTRUE(pp$can_approve_clients),         TRUE, "principal: can_approve_clients=TRUE")
.chk(isTRUE(pp$can_manage_hopdesk_perms),    TRUE, "principal: can_manage_hopdesk_perms=TRUE")
.chk(isTRUE(pp$can_jump_clients),            TRUE, "principal: can_jump_clients=TRUE")
.chk(isTRUE(pp$can_manage_invites),          TRUE, "principal: can_manage_invites=TRUE")
.chk(isTRUE(pp$can_view_global_audit),       TRUE, "principal: can_view_global_audit=TRUE")

# hopdesk: can_manage_invites TRUE, but NOT can_approve_clients
hp <- auth_resolve_perms("hopdesk", "{}")
.chk(isTRUE(hp$can_manage_invites),          TRUE,  "hopdesk: can_manage_invites=TRUE")
.chk(isTRUE(hp$can_approve_clients),         FALSE, "hopdesk: can_approve_clients=FALSE")
.chk(isTRUE(hp$can_manage_hopdesk_perms),    FALSE, "hopdesk: can_manage_hopdesk_perms=FALSE")
.chk(isTRUE(hp$can_jump_clients),            FALSE, "hopdesk: can_jump_clients=FALSE by default")
.chk(isTRUE(hp$can_view_global_audit),       FALSE, "hopdesk: can_view_global_audit=FALSE")

# dev, admin, finance, analysis: all SaaS perms FALSE
for (t in c("dev", "admin", "finance", "analysis")) {
  p <- auth_resolve_perms(t, "{}")
  .chk(isTRUE(p$can_approve_clients),      FALSE, sprintf("%s: can_approve_clients=FALSE", t))
  .chk(isTRUE(p$can_manage_invites),       FALSE, sprintf("%s: can_manage_invites=FALSE",  t))
  .chk(isTRUE(p$can_jump_clients),         FALSE, sprintf("%s: can_jump_clients=FALSE",    t))
  .chk(isTRUE(p$can_view_global_audit),    FALSE, sprintf("%s: can_view_global_audit=FALSE", t))
}

# ── B. Override merging ───────────────────────────────────────────────────────

# hopdesk user with can_jump_clients override
override_json <- jsonlite::toJSON(list(can_jump_clients = TRUE), auto_unbox = TRUE)
op <- auth_resolve_perms("hopdesk", override_json)
.chk(isTRUE(op$can_jump_clients), TRUE, "hopdesk + can_jump_clients override → TRUE")
# Other hopdesk defaults unchanged
.chk(isTRUE(op$can_manage_invites), TRUE, "hopdesk + override preserves can_manage_invites")

# finance user granted can_view_tiers via override
override_json2 <- jsonlite::toJSON(list(can_view_tiers = TRUE), auto_unbox = TRUE)
fp <- auth_resolve_perms("finance", override_json2)
.chk(isTRUE(fp$can_view_tiers), TRUE, "finance + can_view_tiers override → TRUE")
.chk(isTRUE(fp$can_approve_clients), FALSE, "finance + override keeps can_approve_clients=FALSE")

# NULL / empty / malformed overrides JSON don't crash
for (bad in list(NULL, "{}", "[]", "not-json", NA_character_)) {
  ok <- tryCatch({ auth_resolve_perms("dev", bad); TRUE }, error = function(e) FALSE)
  .chk(ok, TRUE, sprintf("auth_resolve_perms handles bad overrides: %s", deparse(bad)))
}

# ── C. Locked permissions (principal tier) ────────────────────────────────────
# can_approve_clients and can_manage_hopdesk_perms must be TRUE for principal
# even if overrides attempt to set them FALSE.

locked_override <- jsonlite::toJSON(
  list(can_approve_clients = FALSE, can_manage_hopdesk_perms = FALSE),
  auto_unbox = TRUE
)
lp <- auth_resolve_perms("principal", locked_override)
.chk(isTRUE(lp$can_approve_clients),      TRUE, "principal: can_approve_clients locked TRUE even with FALSE override")
.chk(isTRUE(lp$can_manage_hopdesk_perms), TRUE, "principal: can_manage_hopdesk_perms locked TRUE even with FALSE override")

# ── D. Tier hierarchy order ───────────────────────────────────────────────────
# can_view_tiers: TRUE for dev+, FALSE for admin/finance/analysis
.chk(isTRUE(auth_resolve_perms("dev",      "{}")$can_view_tiers), TRUE,  "dev can_view_tiers")
.chk(isTRUE(auth_resolve_perms("hopdesk",  "{}")$can_view_tiers), TRUE,  "hopdesk can_view_tiers")
.chk(isTRUE(auth_resolve_perms("principal","{}")$can_view_tiers), TRUE,  "principal can_view_tiers")
.chk(isTRUE(auth_resolve_perms("admin",    "{}")$can_view_tiers), FALSE, "admin cannot view tiers")
.chk(isTRUE(auth_resolve_perms("finance",  "{}")$can_view_tiers), FALSE, "finance cannot view tiers")
.chk(isTRUE(auth_resolve_perms("analysis", "{}")$can_view_tiers), FALSE, "analysis cannot view tiers")

cat("\n")
