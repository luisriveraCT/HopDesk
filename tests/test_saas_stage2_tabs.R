# =============================================================================
# tests/test_saas_stage2_tabs.R
# Verifies tiers_visible_tab_keys() (R/tiers_tab_config.R): which
# Usuarios/Grupo sub-tabs a session may see at all. tiersServer's dynamic
# tab bar calls this function directly, so this tests the actual decision,
# not a re-implementation.
# Sourced by _run_saas.R.
# =============================================================================

cat("── Stage 2 Part C: Internal-Tool Tab Visibility ────────────────────────────\n")

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

STAFF_ONLY <- c("security", "clients", "hop_perms", "notifications", "global_audit")
CLIENT_VISIBLE <- c("usuarios", "actividad", "invites", "tier_config")

# ── Non-staff session (finance/dev/admin/analysis) ────────────────────────────

client_tabs <- tiers_visible_tab_keys(FALSE)

for (hidden in STAFF_ONLY) {
  .chk(hidden %in% client_tabs, FALSE,
       sprintf("non-staff session never receives the '%s' tab", hidden))
}
for (visible in CLIENT_VISIBLE) {
  .chk(visible %in% client_tabs, TRUE,
       sprintf("non-staff session receives the '%s' tab", visible))
}
.chk(length(client_tabs), length(CLIENT_VISIBLE),
     "non-staff session receives exactly the 4 client-visible tabs, nothing else")

# ── Staff session (hopdesk/principal) ─────────────────────────────────────────

staff_tabs <- tiers_visible_tab_keys(TRUE)
.chk(sort(staff_tabs), sort(TIERS_TAB_KEYS),
     "staff session receives all 9 tabs")

cat("\n")
