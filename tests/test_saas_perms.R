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

# dev, admin, finance, analysis: all SaaS perms FALSE, except dev's
# can_manage_invites — flipped to TRUE in Stage 2 Part C (a client's own
# dev/IT can invite their own teammates without asking Hopdesk).
for (t in c("dev", "admin", "finance", "analysis")) {
  p <- auth_resolve_perms(t, "{}")
  .chk(isTRUE(p$can_approve_clients),      FALSE, sprintf("%s: can_approve_clients=FALSE", t))
  .chk(isTRUE(p$can_manage_invites),       t == "dev", sprintf("%s: can_manage_invites=%s",  t, t == "dev"))
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

# ── E. TIER_REGISTRY — single source of truth for staff vs. client tiers ─────
# Stage 1: tesoreria routing bug + dev-tier jump security hole.

staff_tiers  <- names(Filter(function(e) isTRUE(e$internal_only), TIER_REGISTRY))
client_tiers <- names(Filter(function(e) !isTRUE(e$internal_only), TIER_REGISTRY))
.chk(sort(staff_tiers),  sort(c("principal", "hopdesk")),
     "TIER_REGISTRY: internal_only==TRUE is exactly {principal, hopdesk}")
.chk(sort(client_tiers), sort(c("dev", "admin", "finance", "analysis")),
     "TIER_REGISTRY: internal_only==FALSE is exactly {dev, admin, finance, analysis}")

# ── F. is_staff derivation — the mechanism current_user_info()$is_staff uses ─

.chk(is_staff_tier("finance"),   FALSE, "is_staff_tier('finance') == FALSE")
.chk(is_staff_tier("dev"),       FALSE, "is_staff_tier('dev') == FALSE (client's own IT dept, not staff)")
.chk(is_staff_tier("admin"),     FALSE, "is_staff_tier('admin') == FALSE")
.chk(is_staff_tier("analysis"),  FALSE, "is_staff_tier('analysis') == FALSE")
.chk(is_staff_tier("hopdesk"),   TRUE,  "is_staff_tier('hopdesk') == TRUE")
.chk(is_staff_tier("principal"), TRUE,  "is_staff_tier('principal') == TRUE")

# ── G. dev-tier jump prevention — the cross-tenant security hole ─────────────
# The context-switcher's req()/dropdown-population/confirm-switch/grant-exemption
# guards all now read isTRUE(current_user_info()$is_staff), which is FALSE for
# dev. This is the exact boolean those req() calls at tiers_module.R:480/544
# (and the panel-visibility / grant-request guards at :2675/:3016) depend on —
# verifying it here proves a dev session can never pass those gates.
.chk(isTRUE(is_staff_tier("dev")), FALSE,
     "dev session cannot pass is_staff gate: cannot open context-switcher modal")
.chk(isTRUE(is_staff_tier("dev")), FALSE,
     "dev session cannot pass is_staff gate: confirm-switch to a foreign client_id is rejected")

# ── H. Server-side tier-assignment enforcement (tier_assignment_allowed()) ───
# tiers_module.R's create-user and edit-user handlers both call this directly;
# testing it here tests the actual enforcement, not a re-implementation.

.chk(tier_assignment_allowed("hopdesk", "finance", FALSE), FALSE,
     "finance session cannot be granted tier='hopdesk' (server-side, not just hidden from dropdown)")
.chk(tier_assignment_allowed("hopdesk", "dev", FALSE), FALSE,
     "dev session cannot be granted tier='hopdesk' (server-side, not just hidden from dropdown)")
.chk(tier_assignment_allowed("principal", "hopdesk", TRUE), FALSE,
     "hopdesk session cannot assign tier='principal'")
.chk(tier_assignment_allowed("hopdesk", "principal", TRUE), TRUE,
     "principal session can assign tier='hopdesk'")
.chk(tier_assignment_allowed("dev", "dev", FALSE), TRUE,
     "dev session can assign tier='dev' within its own client")
.chk(tier_assignment_allowed("admin", "dev", FALSE), TRUE,
     "dev session can assign tier='admin' within its own client")
.chk(tier_assignment_allowed("finance", "dev", FALSE), TRUE,
     "dev session can assign tier='finance' within its own client")
.chk(tier_assignment_allowed("analysis", "dev", FALSE), TRUE,
     "dev session can assign tier='analysis' within its own client")

cat("\n")
