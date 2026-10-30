# =============================================================================
# R/tier_registry.R
# Single source of truth for tiers (niveles). Every other file asks these
# functions; no file outside this one (and R/permisos.R, which reads it) may
# write a tier key as a literal. tests/test_niveles_guardian.R enforces it.
#
# Adding a tier = one entry here + its name in www/i18n/*.json
# ("niveles.nombre.<key>"). Nothing else.
#
# Fields:
#   rango      hierarchy rank, with gaps of 10 so a tier can slot in between
#              without renumbering. Only ever compared, never stored.
#   interno    TRUE = Hopdesk staff tier; a client never sees or assigns it.
#   color      badge colour.
#   invitable  an emailed invite may create this tier (never staff, never the
#              client's own IT tier: an invite link is a credential).
#   ti         the client's own IT tier: at least one live account must keep
#              it, and it is the tier a staff session works as inside a client.
#   unico_vivo the last live account of this tier cannot be demoted/deleted.
#   alta       the tier preselected when creating a user.
#   pestanas   navbar tabs this tier sees; NULL = every client tab.
#   calendario ledger the Calendario is fixed to ("AP" = CxP); NULL = both.
#   permisos   permission keys ON by default ("*" = every key).
#   bloqueados keys forced ON for this tier, not editable.
# Permission keys themselves live in R/permisos.R.
# =============================================================================

TIER_REGISTRY <- list(
  principal = list(rango = 70L, interno = TRUE,  color = "#7b1fa2",
                   invitable = FALSE, ti = FALSE, unico_vivo = FALSE, alta = FALSE,
                   pestanas = NULL, permisos = "*",
                   bloqueados = c("can_approve_clients", "can_manage_hopdesk_perms")),
  hopdesk   = list(rango = 60L, interno = TRUE,  color = "#c2185b",
                   invitable = FALSE, ti = FALSE, unico_vivo = TRUE, alta = FALSE,
                   pestanas = NULL,
                   permisos = c("can_view_tiers", "can_manage_empresas", "can_manage_invites",
                                "can_view_client_audit_logs", "can_view_payment_audit",
                                "can_configure_erp", "can_manage_sync",
                                "can_manage_group_branding", "can_manage_tables",
                                "can_view_requisiciones", "can_create_requisiciones",
                                "can_edit_requisiciones", "can_trash_own_requisiciones",
                                "can_trash_others_requisiciones", "can_edit_provisiones",
                                "can_convert_provisiones", "can_register_payments",
                                "can_confirm_payments", "can_manage_pasivos",
                                "can_publish_panorama"),
                   bloqueados = character()),
  dev       = list(rango = 50L, interno = FALSE, color = "#0d1b3e",
                   invitable = FALSE, ti = TRUE, unico_vivo = TRUE, alta = FALSE,
                   pestanas = NULL,
                   permisos = c("can_view_tiers", "can_manage_empresas", "can_manage_invites",
                                "can_view_payment_audit", "can_configure_erp",
                                "can_manage_sync", "can_manage_group_branding",
                                "can_view_requisiciones", "can_create_requisiciones",
                                "can_edit_requisiciones", "can_trash_own_requisiciones",
                                "can_trash_others_requisiciones", "can_edit_provisiones",
                                "can_convert_provisiones", "can_register_payments",
                                "can_confirm_payments", "can_manage_pasivos",
                                "can_publish_panorama"),
                   bloqueados = "can_view_tiers"),
  admin     = list(rango = 40L, interno = FALSE, color = "#6610f2",
                   invitable = TRUE, ti = FALSE, unico_vivo = FALSE, alta = FALSE,
                   pestanas = NULL,
                   permisos = c("can_manage_empresas", "can_view_payment_audit", "can_manage_sync",
                                "can_view_requisiciones", "can_create_requisiciones",
                                "can_edit_requisiciones", "can_trash_own_requisiciones",
                                "can_trash_others_requisiciones", "can_edit_provisiones",
                                "can_convert_provisiones", "can_register_payments",
                                "can_confirm_payments", "can_manage_pasivos",
                                "can_publish_panorama"),
                   bloqueados = character()),
  finance   = list(rango = 30L, interno = FALSE, color = "#0a58ca",
                   invitable = TRUE, ti = FALSE, unico_vivo = FALSE, alta = TRUE,
                   pestanas = NULL,
                   permisos = c("can_edit_provisiones", "can_convert_provisiones",
                                "can_register_payments", "can_confirm_payments",
                                "can_manage_pasivos"),
                   bloqueados = character()),
  analysis  = list(rango = 20L, interno = FALSE, color = "#6c757d",
                   invitable = TRUE, ti = FALSE, unico_vivo = FALSE, alta = FALSE,
                   pestanas = NULL, permisos = character(), bloqueados = character()),
  compras   = list(rango = 10L, interno = FALSE, color = "#b8860b",
                   invitable = TRUE, ti = FALSE, unico_vivo = FALSE, alta = FALSE,
                   # Panorama and Requisiciones join this list when their tabs exist.
                   pestanas = c("CAL", "PSV"), calendario = "AP",
                   permisos = c("can_view_requisiciones", "can_create_requisiciones",
                                "can_edit_requisiciones", "can_trash_own_requisiciones",
                                "can_trash_others_requisiciones", "can_edit_provisiones"),
                   bloqueados = character())
)

.nivel <- function(tier) {
  if (is.null(tier) || length(tier) != 1L || is.na(tier)) return(NULL)
  TIER_REGISTRY[[tier]]
}

es_nivel <- function(tier) !is.null(.nivel(tier))

is_staff_tier <- function(tier) isTRUE(.nivel(tier)$interno)

tier_rank <- function(tier) .nivel(tier)$rango %||% NA_integer_
nivel_rango <- tier_rank

nivel_nombre <- function(tier) {
  if (!es_nivel(tier)) return(tier %||% "")
  tr(paste0("niveles.nombre.", tier))
}

nivel_color <- function(tier) .nivel(tier)$color %||% "#6c757d"

# Tiers ordered highest rank first.
.niveles_ordenados <- function(filtro = function(e) TRUE) {
  keep <- Filter(filtro, TIER_REGISTRY)
  names(keep)[order(-vapply(keep, `[[`, integer(1), "rango"))]
}

niveles_todos    <- function() .niveles_ordenados()
niveles_cliente  <- function() .niveles_ordenados(function(e) !isTRUE(e$interno))
niveles_staff    <- function() .niveles_ordenados(function(e) isTRUE(e$interno))
niveles_invitables <- function() .niveles_ordenados(function(e) isTRUE(e$invitable))
niveles_unico_vivo <- function() .niveles_ordenados(function(e) isTRUE(e$unico_vivo))

# The client's own IT tier. Staff work inside a client as this tier.
nivel_ti <- function() .niveles_ordenados(function(e) isTRUE(e$ti))[1]

# Highest-ranked staff tier (gets limit-change emails, sees everything).
nivel_tope <- function() niveles_todos()[1]

nivel_alta <- function() .niveles_ordenados(function(e) isTRUE(e$alta))[1]

nivel_es_ti     <- function(tier) isTRUE(.nivel(tier)$ti)
nivel_es_tope   <- function(tier) identical(tier, nivel_tope())
nivel_unico_vivo <- function(tier) isTRUE(.nivel(tier)$unico_vivo)

# NULL = every client tab.
nivel_pestanas <- function(tier) .nivel(tier)$pestanas

# NULL = the tier switches between Cobros and Pagos freely.
nivel_calendario <- function(tier) .nivel(tier)$calendario

# TRUE when `tier` ranks at or above `referencia`.
nivel_al_menos <- function(tier, referencia) {
  a <- tier_rank(tier); b <- tier_rank(referencia)
  !is.na(a) && !is.na(b) && a >= b
}

# Generic "assigning tier X requires being tier X-or-higher" rule: a viewer
# may assign a target tier only if it's at or below their own rank, and only
# assign an internal_only (staff) tier if the viewer session itself is_staff.
tier_assignment_allowed <- function(target_tier, viewer_tier, viewer_is_staff) {
  nivel_al_menos(viewer_tier, target_tier) &&
    (!is_staff_tier(target_tier) || isTRUE(viewer_is_staff))
}

# Tiers a viewer may assign, highest first.
niveles_asignables <- function(viewer_tier, viewer_is_staff) {
  Filter(function(t) tier_assignment_allowed(t, viewer_tier, viewer_is_staff), niveles_todos())
}

# A viewer sees user rows at or below their own rank.
nivel_puede_ver_fila <- function(viewer_tier, fila_tier) {
  nivel_al_menos(viewer_tier, fila_tier)
}
