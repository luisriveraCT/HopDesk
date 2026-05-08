# =============================================================================
# R/pasivos_capabilities.R
# Capability registry + access-check stub for the Pasivos module.
# Stage 0: has_capability() always grants known capabilities.
# Real role/permission resolution arrives in a later stage.
# =============================================================================

PASIVOS_CAPABILITIES <- c(
  "pasivos.view",
  "pasivos.create_liability",
  "pasivos.edit_liability",
  "pasivos.delete_liability",
  "pasivos.create_provision_manual",
  "pasivos.edit_provision",
  "pasivos.bulk_edit_provisions",
  "pasivos.convert_to_item",
  "pasivos.manage_estimates",
  "pasivos.manage_modifiers",
  "pasivos.view_history",
  "pasivos.view_credit_card_expenses",
  "pasivos.log_credit_card_expense"
)

# Stage 0 stub — always returns TRUE for known capabilities.
# empresa parameter is accepted but ignored; real scoping comes later.
has_capability <- function(user, capability, empresa = NULL) {
  if (!capability %in% PASIVOS_CAPABILITIES) {
    warning("[pasivos] unknown capability: ", capability)
    return(FALSE)
  }
  TRUE
}
