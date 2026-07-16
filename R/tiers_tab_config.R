# =============================================================================
# R/tiers_tab_config.R
# Stage 2 Part C: which Usuarios/Grupo sub-tabs a session may see at all.
# Metadata only, no Shiny dependency — R/tiers_module.R's dynamic tab bar
# calls tiers_visible_tab_keys() directly; this is the only place that
# decides tab visibility, mirroring R/tier_registry.R's role for tiers.
# =============================================================================

TIERS_TAB_KEYS <- c(
  "usuarios", "actividad", "security", "clients",
  "invites", "hop_perms", "notifications", "global_audit", "tier_config"
)

# Hopdesk-internal tools — a client session must never see that these exist,
# not just be blocked from using them.
TIERS_STAFF_ONLY_TABS <- c("security", "clients", "hop_perms", "notifications", "global_audit")

tiers_visible_tab_keys <- function(is_staff) {
  if (isTRUE(is_staff)) TIERS_TAB_KEYS else setdiff(TIERS_TAB_KEYS, TIERS_STAFF_ONLY_TABS)
}
