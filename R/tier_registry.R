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
                                "can_configure_erp", "can_manage_interco", "can_manage_sync",
                                "can_manage_group_branding", "can_manage_tables",
                                "can_view_requisiciones", "can_create_requisiciones",
                                "can_edit_requisiciones", "can_trash_own_requisiciones",
                                "can_trash_others_requisiciones", "can_edit_provisiones",
                                "can_convert_provisiones", "can_register_payments",
                                "can_confirm_payments", "can_manage_pasivos",
                                "can_manage_bancos", "can_manage_policies",
                                "can_refresh_forecasting", "can_refresh_forecasting_all", "can_edit_forecasting_subscriptions",
                                "can_publish_panorama"),
                   bloqueados = character()),
  dev       = list(rango = 50L, interno = FALSE, color = "#0d1b3e",
                   invitable = FALSE, ti = TRUE, unico_vivo = TRUE, alta = FALSE,
                   pestanas = NULL,
                   permisos = c("can_view_tiers", "can_manage_empresas", "can_manage_invites",
                                "can_view_payment_audit", "can_configure_erp",
                                "can_manage_interco", "can_manage_sync", "can_manage_group_branding",
                                "can_view_requisiciones", "can_create_requisiciones",
                                "can_edit_requisiciones", "can_trash_own_requisiciones",
                                "can_trash_others_requisiciones", "can_edit_provisiones",
                                "can_convert_provisiones", "can_register_payments",
                                "can_confirm_payments", "can_manage_pasivos",
                                "can_manage_proveedores", "can_manage_bancos",
                                "can_manage_policies",
                                "can_refresh_forecasting", "can_refresh_forecasting_all", "can_edit_forecasting_subscriptions",
                                "can_publish_panorama", "can_receive_weekly_email"),
                   bloqueados = "can_view_tiers"),
  admin     = list(rango = 40L, interno = FALSE, color = "#6610f2",
                   invitable = TRUE, ti = FALSE, unico_vivo = FALSE, alta = FALSE,
                   pestanas = NULL,
                   permisos = c("can_manage_empresas", "can_view_payment_audit", "can_manage_sync",
                                "can_manage_interco",
                                "can_view_requisiciones", "can_create_requisiciones",
                                "can_edit_requisiciones", "can_trash_own_requisiciones",
                                "can_trash_others_requisiciones", "can_edit_provisiones",
                                "can_convert_provisiones", "can_register_payments",
                                "can_confirm_payments", "can_manage_pasivos",
                                "can_manage_proveedores", "can_manage_bancos",
                                "can_manage_policies",
                                "can_refresh_forecasting", "can_refresh_forecasting_all", "can_edit_forecasting_subscriptions",
                                "can_publish_panorama", "can_receive_weekly_email"),
                   bloqueados = character()),
  finance   = list(rango = 30L, interno = FALSE, color = "#0a58ca",
                   invitable = TRUE, ti = FALSE, unico_vivo = FALSE, alta = TRUE,
                   pestanas = NULL,
                   permisos = c("can_edit_provisiones", "can_convert_provisiones",
                                "can_register_payments", "can_confirm_payments",
                                "can_manage_pasivos", "can_manage_proveedores",
                                "can_manage_bancos", "can_manage_policies",
                                "can_refresh_forecasting", "can_refresh_forecasting_all", "can_edit_forecasting_subscriptions"),
                   bloqueados = character()),
  analysis  = list(rango = 20L, interno = FALSE, color = "#6c757d",
                   invitable = TRUE, ti = FALSE, unico_vivo = FALSE, alta = FALSE,
                   pestanas = NULL, permisos = character(), bloqueados = character()),
  compras   = list(rango = 10L, interno = FALSE, color = "#b8860b",
                   invitable = TRUE, ti = FALSE, unico_vivo = FALSE, alta = FALSE,
                   # Panorama first: it is Compras' home screen too (Luis, 2026-11-10).
                   # Only Panorama and Requisiciones (Luis, 2026-11-20, T69): the
                   # treasury tabs are confidential for a buyer. can_edit_provisiones
                   # stays: "Crear provisión" opens its modal from Requisiciones.
                   pestanas = c("PAN", "REQ"),
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
# Tiers this inviter's invite may create, highest first. The client's IT tier
# is not `invitable` (an invite link is a credential), but the top tier may
# send it on purpose (Luis, 2026-11-03).
niveles_invitables_por <- function(viewer_tier) {
  c(if (nivel_es_tope(viewer_tier)) nivel_ti(), niveles_invitables())
}
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

# ── Which tables travel to a tier's browser (T69) ────────────────────────────
# hd_table_server() pushes a table's data to the browser whenever it changes,
# visible or not: hiding a tab never kept its data from travelling. A tier
# with a `pestanas` list only receives the tables whose module file
# (hd_registro[[tabla]]$archivo) belongs to one of its tabs; any other table
# -- another tab's, a settings one, or one that cannot be classified -- never
# travels. A tier with pestanas = NULL receives every table, as before.
HD_PESTANA_ARCHIVOS <- list(
  PAN = c("panorama_module.R", "panorama_publicar_module.R", "avisos_alertas_module.R"),
  CAL = c("ledger_module.R", "search_module.R", "search_module.R · ledger_module.R",
          "staging_browse_module.R"),
  VEN = "vencidos_module.R",
  PH  = c("pagar_hoy_module.R", "pagos_audit_viewer_module.R"),
  BNC = "bancos_module.R",
  IC  = c("interco_module.R", "treasury_map_module.R"),
  PSV = c("pasivos_list_module.R", "pasivos_table_module.R", "pasivos_wizard_module.R",
          "pasivos_edit_confirm_module.R"),
  REQ = "requisiciones_module.R",
  FC  = c("forecasting_module.R", "cashflow_preview_module.R"),
  RPT = "reporte_module.R")
# Module files with tables that belong to no client tab (Grupo menu, Ajustes,
# staff screens): a tier with a `pestanas` list does not receive them either.
# Listed so that every registered table is classified (test_t69_compras_pestanas).
HD_ARCHIVOS_SIN_PESTANA <- c("tiers_module.R", "empresas_module.R", "proveedores_module.R",
                             "audit_log_viewer_module.R", "settings_cuentas.R", "settings_bp.R",
                             "settings_hub.R", "settings_policies.R", "settings_sincro.R")

# TRUE when `tabla`'s data may travel to a session of `tier`. Never throws:
# a table it cannot classify, for a tier with a tab list, does not travel.
nivel_tabla_permitida <- function(tier, tabla) {
  lista <- tryCatch(nivel_pestanas(tier), error = function(e) NULL)
  if (is.null(lista)) return(TRUE)
  arch <- tryCatch(hd_registro[[tabla]]$archivo, error = function(e) NULL)
  if (is.null(arch) || !length(arch) || is.na(arch[1])) {
    message("[T69] table '", tabla, "' has no module file in hd_registro: not sent to tier ", tier)
    return(FALSE)
  }
  arch[1] %in% unlist(HD_PESTANA_ARCHIVOS[lista], use.names = FALSE)
}

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
