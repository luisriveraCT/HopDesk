# =============================================================================
# R/pasivos_capabilities.R
# Pasivos capability names, mapped onto the permission registry (R/permisos.R).
# has_capability() resolves against the current Shiny session's user
# (session$userData$usuario, set in app.R from usuario_sesion()). Outside a
# session, or for an unknown capability, it answers FALSE: fail closed.
# =============================================================================

# capability -> permission key; NA = any signed-in user (read-only views).
PASIVOS_CAPABILITIES <- c(
  "pasivos.view"                      = NA,
  "pasivos.view_history"              = NA,
  "pasivos.view_credit_card_expenses" = NA,
  "pasivos.create_liability"          = "can_manage_pasivos",
  "pasivos.edit_liability"            = "can_manage_pasivos",
  "pasivos.delete_liability"          = "can_manage_pasivos",
  "pasivos.manage_modifiers"          = "can_manage_pasivos",
  "pasivos.manage_estimates"          = "can_manage_pasivos",
  "pasivos.create_provision_manual"   = "can_edit_provisiones",
  "pasivos.edit_provision"            = "can_edit_provisiones",
  "pasivos.bulk_edit_provisions"      = "can_edit_provisiones",
  "pasivos.convert_to_item"           = "can_convert_provisiones",
  "pasivos.log_credit_card_expense"   = "can_register_payments"
)

.usuario_de_sesion <- function() {
  s <- shiny::getDefaultReactiveDomain()
  if (is.null(s)) return(NULL)
  u <- s$userData$usuario
  if (is.function(u)) tryCatch(shiny::isolate(u()), error = function(e) NULL) else u
}

# `user` (a username) is kept for the existing call sites and logs; the
# decision comes from the session's resolved permissions.
has_capability <- function(user, capability, empresa = NULL) {
  if (!capability %in% names(PASIVOS_CAPABILITIES)) {
    warning("[pasivos] unknown capability: ", capability)
    return(FALSE)
  }
  permiso <- PASIVOS_CAPABILITIES[[capability]]
  usuario <- .usuario_de_sesion()
  if (is.na(permiso)) return(TRUE)
  puede(usuario, permiso)
}

# For observers without a check of their own: refuses with the standard
# message. Usage: if (!pasivos_autoriza("pasivos.edit_provision")) return()
pasivos_autoriza <- function(capability) {
  if (has_capability(NULL, capability)) return(TRUE)
  permiso <- PASIVOS_CAPABILITIES[[capability]]
  msg <- if (is.null(permiso) || is.na(permiso)) capability else permiso_nombre(permiso)
  tryCatch(shiny::showNotification(tr("permisos.denegado", permiso = msg), type = "error"),
           error = function(e) NULL)
  FALSE
}
