# =============================================================================
# R/permisos.R
# Permission registry + puede(), the only way to check a permission.
#
# Each key has its name in www/i18n/*.json ("permisos.<key>"), a default per
# tier (TIER_REGISTRY$<tier>$permisos in R/tier_registry.R) and at least one
# server-side puede() call. tests/test_permisos_vivos.R fails otherwise, so no
# checkbox is ever decorative again.
#
# Fields:
#   grupo       section in the permission editor.
#   interno     Hopdesk-staff permission: a client session never sees it, and
#               a client tier can never hold it (fail-closed, see
#               perm_visible_keys()).
#   pendiente   front that will add the puede() call (e.g. "E"); the live
#               test skips it until then. Remove the field once wired.
#
# Resolution, lowest to highest precedence:
#   1. tier default (registry)
#   2. the client's own "Configuración de niveles" (tiers_config.rds)
#   3. the user's own overrides (usuarios.rds$permisos JSON)
#   4. locked keys of the tier (always ON)
# Internal keys only ever resolve ON for a staff tier.
# =============================================================================

PERMISOS <- list(
  # ── Usuarios y grupo ────────────────────────────────────────────────────
  can_view_tiers                 = list(grupo = "grupo"),
  can_manage_empresas            = list(grupo = "grupo"),
  can_manage_invites             = list(grupo = "grupo"),
  can_configure_erp              = list(grupo = "grupo"),
  can_manage_sync                = list(grupo = "grupo"),
  can_manage_group_branding      = list(grupo = "grupo"),
  can_view_payment_audit         = list(grupo = "grupo"),
  # ── Requisiciones ───────────────────────────────────────────────────────
  can_view_requisiciones         = list(grupo = "requisiciones", pendiente = "E"),
  can_create_requisiciones       = list(grupo = "requisiciones", pendiente = "E"),
  can_edit_requisiciones         = list(grupo = "requisiciones", pendiente = "E"),
  can_trash_own_requisiciones    = list(grupo = "requisiciones", pendiente = "E"),
  can_trash_others_requisiciones = list(grupo = "requisiciones", pendiente = "E"),
  # ── Provisiones, pagos y pasivos ────────────────────────────────────────
  can_edit_provisiones           = list(grupo = "tesoreria"),
  can_convert_provisiones        = list(grupo = "tesoreria"),
  can_register_payments          = list(grupo = "tesoreria"),
  can_confirm_payments           = list(grupo = "tesoreria"),
  can_manage_pasivos             = list(grupo = "tesoreria"),
  # ── Panorama ────────────────────────────────────────────────────────────
  can_publish_panorama           = list(grupo = "panorama", pendiente = "B"),
  # ── Internos de Hopdesk ─────────────────────────────────────────────────
  can_approve_clients            = list(grupo = "interno", interno = TRUE),
  can_manage_hopdesk_perms       = list(grupo = "interno", interno = TRUE),
  can_jump_clients               = list(grupo = "interno", interno = TRUE),
  can_view_client_audit_logs     = list(grupo = "interno", interno = TRUE),
  can_view_staff_audit_log       = list(grupo = "interno", interno = TRUE),
  can_manage_tables              = list(grupo = "interno", interno = TRUE, pendiente = "T")
)

PERMISOS_GRUPOS <- c("grupo", "tesoreria", "requisiciones", "panorama", "interno")

permisos_claves  <- function() names(PERMISOS)
permiso_interno  <- function(k) isTRUE(PERMISOS[[k]]$interno)
permisos_internos <- function() names(Filter(function(p) isTRUE(p$interno), PERMISOS))
permiso_nombre   <- function(k) tr(paste0("permisos.", k))
permisos_de_grupo <- function(g) names(Filter(function(p) identical(p$grupo, g), PERMISOS))

# Keys a viewer may see. Fail-closed: a caller that forgets is_staff = TRUE
# hides the internal permissions rather than exposing them.
perm_visible_keys <- function(is_staff) {
  if (isTRUE(is_staff)) permisos_claves() else setdiff(permisos_claves(), permisos_internos())
}

# Tier defaults as a named logical vector over every key.
permisos_por_defecto <- function(tier) {
  out <- setNames(rep(FALSE, length(PERMISOS)), permisos_claves())
  on  <- .nivel(tier)$permisos
  if (identical(on, "*")) out[] <- TRUE else out[intersect(on, names(out))] <- TRUE
  if (!is_staff_tier(tier)) out[permisos_internos()] <- FALSE
  out
}

permisos_bloqueados <- function(tier) intersect(.nivel(tier)$bloqueados %||% character(), permisos_claves())

.aplicar_overrides <- function(base, ov, tier) {
  if (is.null(ov) || !length(ov)) return(base)
  for (k in intersect(names(ov), names(base))) {
    if (permiso_interno(k) && !is_staff_tier(tier)) next
    base[[k]] <- isTRUE(as.logical(ov[[k]]))
  }
  base
}

.json_a_lista <- function(json) {
  if (is.list(json)) return(json)
  tryCatch(jsonlite::fromJSON(json %||% "{}", simplifyVector = FALSE),
           error = function(e) list())
}

# Full resolution. Returns a named logical vector over every key; an unknown
# tier gets every key OFF.
#   overrides_json   the user's own permisos JSON (or a list)
#   config_cliente   the client's tiers_config.rds list (tier -> key -> bool)
permisos_resueltos <- function(tier, overrides_json = "{}", config_cliente = NULL) {
  base <- permisos_por_defecto(tier)
  if (!es_nivel(tier)) return(base)
  if (!is.null(config_cliente) && !is.null(config_cliente[[tier]]))
    base <- .aplicar_overrides(base, config_cliente[[tier]], tier)
  base <- .aplicar_overrides(base, .json_a_lista(overrides_json), tier)
  base[permisos_bloqueados(tier)] <- TRUE
  base
}

# Legacy shape: a named list of booleans. Kept for the existing call sites.
auth_resolve_perms <- function(tier, permisos_json) {
  as.list(permisos_resueltos(tier, permisos_json))
}

# `usuario`: list with $permisos (output of permisos_resueltos()), as built by
# usuario_sesion() in app.R. Anything else answers FALSE.
puede <- function(usuario, permiso) {
  if (!permiso %in% permisos_claves()) {
    warning("[permisos] permiso desconocido: ", permiso, call. = FALSE)
    return(FALSE)
  }
  p <- usuario$permisos
  if (is.null(p)) return(FALSE)
  isTRUE(p[[permiso]])
}

# For observers: shows the standard refusal and returns FALSE when the
# session's user lacks `permiso`. Usage: if (!puede_o_avisa(shared, "k")) return()
puede_o_avisa <- function(shared, permiso, session = shiny::getDefaultReactiveDomain()) {
  u <- tryCatch(shiny::isolate(shared$usuario()), error = function(e) NULL)
  if (puede(u, permiso)) return(TRUE)
  message(sprintf("[permisos] denegado %s a %s (%s)", permiso,
                  u$user %||% "?", u$tier %||% "?"))
  if (!is.null(session))
    shiny::showNotification(tr("permisos.denegado", permiso = permiso_nombre(permiso)),
                            type = "error", session = session)
  FALSE
}
