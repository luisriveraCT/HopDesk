# =============================================================================
# R/tier_registry.R
# Single source of truth for tier metadata: display label, hierarchy rank, and
# whether a tier is a Hopdesk-staff (internal_only) tier vs. a client tier.
# No other file may independently decide whether a tier is staff-equivalent —
# everything reads TIER_REGISTRY via is_staff_tier() / tier_rank().
# See docs/saas_rebuild/ARCHITECTURE.md §4.1.
# =============================================================================

TIER_REGISTRY <- list(
  principal = list(label = "Principal", rank = 6L, internal_only = TRUE),
  hopdesk   = list(label = "Hopdesk",   rank = 5L, internal_only = TRUE),
  dev       = list(label = "Dev",       rank = 4L, internal_only = FALSE),
  admin     = list(label = "Admin",     rank = 3L, internal_only = FALSE),
  finance   = list(label = "Finanzas",  rank = 2L, internal_only = FALSE),
  analysis  = list(label = "Análisis",  rank = 1L, internal_only = FALSE)
)

is_staff_tier <- function(tier) {
  entry <- TIER_REGISTRY[[tier %||% ""]]
  if (is.null(entry)) return(FALSE)
  isTRUE(entry$internal_only)
}

tier_rank <- function(tier) {
  entry <- TIER_REGISTRY[[tier %||% ""]]
  if (is.null(entry)) return(NA_integer_)
  entry$rank
}

# Generic "assigning tier X requires being tier X-or-higher" rule: a viewer
# may assign a target tier only if it's at or below their own rank, and only
# assign an internal_only (staff) tier if the viewer session itself is_staff.
tier_assignment_allowed <- function(target_tier, viewer_tier, viewer_is_staff) {
  target_rank <- tier_rank(target_tier)
  viewer_rank <- tier_rank(viewer_tier)
  !is.na(target_rank) && !is.na(viewer_rank) &&
    target_rank <= viewer_rank &&
    (!is_staff_tier(target_tier) || isTRUE(viewer_is_staff))
}
