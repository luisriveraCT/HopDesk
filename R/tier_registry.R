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
                                "can_publish_panorama", "can_receive_email_job_notices", "can_moderate_panorama"),
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
                                "can_publish_panorama", "can_receive_weekly_email",
                                "can_receive_account_notices", "can_receive_email_job_notices", "can_moderate_panorama"),
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
                                "can_publish_panorama", "can_receive_weekly_email",
                                "can_receive_account_notices", "can_receive_email_job_notices", "can_moderate_panorama"),
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
                   # Stays: "no fixed Calendario" means "sees CxC" to Panorama,
                   # the weekly emails and the thresholds notice (ver_cxc).
                   calendario = "AP",
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
# TEMA 6-6: a badge paints the tier colour (data, above) mixed with the
# theme's encabezado, so its white text reads at 4.5:1 in every theme
# (R/tema_registro.R explains the 74 %; the theme guardian checks it).
.NIVEL_MEZCLA <- 74L
nivel_fondo <- function(color) sprintf("color-mix(in srgb, %s %d%%, var(--hd-encabezado))", color, .NIVEL_MEZCLA)

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

# ── The ☰ menu: one rule for every entry (T87-M8) ────────────────────────────
# Before M8, five pieces painted the menu (two static CSS rules, the T69c
# first-paint CSS and three observers) and two of those observers wrote the
# same inline style without agreeing: a staff "ver como" from Compras to Admin
# left the tabs that should come back hidden until a jump. Now:
#   1. HD_MENU_CSS_OCULTO: every navbar entry (a[data-value] and its li) is
#      born hidden, in every deployment -- a new entry too, until it is here.
#   2. menu_css_inicial(): the first paint SHOWS what the token's tier may see
#      (a client's tabs without a permission), so a client is not left
#      waiting; anything it cannot know stays hidden.
#   3. menu_visible(): the one decision, from usuario_sesion(), for each entry.
#   4. menu_js_pintar(): the one writer, called by the one observer in app.R;
#      every entry shown or hidden explicitly with an inline !important.
# tests/test_t87_m8.R (with the one-writer guardian), test_t69c_barra_inicial.

# The Grupo dropdown: its toggle and its two entries.
HD_MENU_GRUPO <- c("GRUPO", "TIERS", "EMP")

# An entry that also needs a permission besides the tier's tab list.
HD_PESTANA_PERMISO <- list(
  REQ   = "can_view_requisiciones",
  TIERS = "can_view_tiers",
  EMP   = "can_manage_empresas"
)

# The selectors never use .nav-link: bslib adds that class with its own
# JavaScript after the first paint (T87-M7).
HD_MENU_CSS_OCULTO <- ".navbar a[data-value],.navbar li:has(> a[data-value]){display:none!important}"

# How a shown entry displays: the dropdown's entries as block, the rest flex.
.menu_display <- function(entrada) if (entrada %in% setdiff(HD_MENU_GRUPO, "GRUPO")) "block" else "flex"

# Named logical over `pestanas` (the client tabs, HD_PESTANAS_FINANCIERAS) and
# HD_MENU_GRUPO. `u` is usuario_sesion(): its tier is the EFFECTIVE one (a
# staff "ver como" sees what that tier sees) and its permisos are resolved.
# `en_cliente`: FALSE only for staff at home in the admin deployment, who see
# no client tab. Fail-closed: no user, an unknown tier or any error, all FALSE.
menu_visible <- function(u, pestanas, en_cliente = TRUE) {
  todas <- c(pestanas, HD_MENU_GRUPO)
  vis <- stats::setNames(rep(FALSE, length(todas)), todas)
  r <- tryCatch({
    tier <- u$tier
    if (is.null(u) || !.nivel_conocido(tier)) return(vis)
    permite <- function(e) {
      p <- HD_PESTANA_PERMISO[[e]]
      is.null(p) || isTRUE(tryCatch(puede(u, p), error = function(e) FALSE))
    }
    if (!isTRUE(u$is_staff) || isTRUE(en_cliente))
      for (t in pestanas) vis[[t]] <- nivel_pestana_permitida(tier, t) && permite(t)
    for (e in setdiff(HD_MENU_GRUPO, "GRUPO")) vis[[e]] <- permite(e)
    vis[["GRUPO"]] <- any(vis[setdiff(HD_MENU_GRUPO, "GRUPO")])
    vis
  }, error = function(e) NULL)
  if (is.logical(r) && identical(names(r), todas) && !anyNA(r)) r else vis
}

# The navbar's first paint (T69c, now by showing): the page itself carries
# the entries the token's tier may see, so a client never waits for the
# session's first load (minutes on a busy machine) and never sees another
# tier's tab. Only entries the tier alone decides: one behind a permission
# (HD_PESTANA_PERMISO) waits for the observer, since the token has no
# permission overrides. Staff get nothing (at home in the admin deployment
# they see no client tab). It applies while <html> lacks `hd-nivel-aplicado`,
# which the observer adds; from then on its inline styles rule. `tier` must
# come from the server (the shinymanager token), never from the browser.
# NULL -- everything stays hidden -- for an unknown tier. Pure.
menu_css_inicial <- function(tier, pestanas) {
  if (!.nivel_conocido(tier) || is_staff_tier(tier)) return(NULL)
  ver <- pestanas[vapply(pestanas, function(t)
    nivel_pestana_permitida(tier, t) && is.null(HD_PESTANA_PERMISO[[t]]), logical(1))]
  if (!length(ver)) return(NULL)
  # nav.navbar outranks the hidden rule's .navbar (one more type selector).
  a  <- sprintf("html:not(.hd-nivel-aplicado) nav.navbar a[data-value='%s']", ver)
  li <- sprintf("html:not(.hd-nivel-aplicado) nav.navbar li:has(> a[data-value='%s'])", ver)
  paste0(paste(a, collapse = ",\n"), "{display:flex!important}\n",
         paste(li, collapse = ",\n"), "{display:list-item!important}")
}

# The one writer of the entries' display (T87-M8): JavaScript that shows or
# hides EVERY entry of `vis` (menu_visible()) with an inline !important, the
# link and its li, and then marks <html> with `hd-nivel-aplicado`. Never
# removes a style (a static !important rule would win again).
menu_js_pintar <- function(vis) {
  partes <- vapply(names(vis), function(e) {
    si <- isTRUE(vis[[e]])
    sprintf(paste0(
      "document.querySelectorAll('.navbar a[data-value=\"%s\"]').forEach(function(el) {",
      " el.style.setProperty('display', '%s', 'important');",
      " var li = el.closest('li'); if (li) li.style.setProperty('display', '%s', 'important'); });"),
      e, if (si) .menu_display(e) else "none", if (si) "list-item" else "none")
  }, "")
  paste(c(partes, "document.documentElement.classList.add('hd-nivel-aplicado');"), collapse = "\n")
}

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
  PH  = c("pagar_hoy_module.R", "pagos_audit_viewer_module.R",
          "bancos_respuesta.R"),   # T93: the bank report preview, shown from Archivos de pago
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
# An unknown tier (NULL, "", one not in the registry -- the session's start,
# before its user is known) gets NOTHING: fail closed. The caller reads the
# tier inside its observer, so the table goes out once the tier arrives.
nivel_tabla_permitida <- function(tier, tabla) {
  if (!.nivel_conocido(tier)) return(FALSE)
  lista <- tryCatch(nivel_pestanas(tier), error = function(e) NULL)
  if (is.null(lista)) return(TRUE)
  arch <- tryCatch(hd_registro[[tabla]]$archivo, error = function(e) NULL)
  if (is.null(arch) || !length(arch) || is.na(arch[1])) {
    message("[T69] table '", tabla, "' has no module file in hd_registro: not sent to tier ", tier)
    return(FALSE)
  }
  arch[1] %in% unlist(HD_PESTANA_ARCHIVOS[lista], use.names = FALSE)
}

# ── Screens behind a permission (T87-M6) ─────────────────────────────────────
# The tier gate above says which TABS a tier has; a screen the tier has but
# the person may not open is a permission. Usuarios (with its Actividad, its
# Invitaciones, and the staff tabs) is built in every session (tiersServer),
# and its tables pushed their rows to the browser of anyone -- Admin and
# Finanzas received the whole activity log with the tab hidden. A table whose
# module file is listed here travels only to a session that holds the
# permission. tests/test_t87_m6_tablas_permiso.R: every file is a module of
# hd_registro, every permission exists.
HD_ARCHIVOS_PERMISO <- list(
  "tiers_module.R"            = "can_view_tiers",
  "audit_log_viewer_module.R" = "can_view_tiers")

# nivel_tabla_permitida() for one session: `usuario` as usuario_sesion()
# builds it (app.R) -- tier = the EFFECTIVE tier and permisos resolved for it.
# So "ver como" holds as it does for the tier gate: staff viewing a client as
# Admin gets exactly what that Admin tier gets (its permissions as that client
# configured them, no personal overrides); staff at home, by its own tier.
# No exception by identity. Pure, and fails closed: a user or tier not
# resolved yet, or puede() missing (a fixture that loads this file without
# R/permisos.R), means no.
nivel_tabla_permitida_usuario <- function(usuario, tabla) {
  if (!is.list(usuario)) return(FALSE)
  if (!nivel_tabla_permitida(usuario$tier, tabla)) return(FALSE)
  arch <- tryCatch(hd_registro[[tabla]]$archivo, error = function(e) NULL)
  permiso <- if (length(arch) && !is.na(arch[1])) HD_ARCHIVOS_PERMISO[[arch[1]]] else NULL
  if (is.null(permiso)) return(TRUE)
  isTRUE(tryCatch(puede(usuario, permiso), error = function(e) FALSE))
}

# TRUE when a session of `tier` may receive what tab `tab` pushes on its own
# (the Agenda's badges). Same rule: unknown tier = no; no list = every tab.
nivel_pestana_permitida <- function(tier, tab) {
  if (!.nivel_conocido(tier)) return(FALSE)
  lista <- tryCatch(nivel_pestanas(tier), error = function(e) NULL)
  is.null(lista) || tab %in% lista
}

# ── Which session data a tier may read (T69b) ────────────────────────────────
# The table gate keeps hd_table's data in the server; the rest of a treasury
# tab (calendar cells, balances, the Agenda's totals, charts, modals,
# Panorama's ribbon) reads these session reactives. app.R wraps each one
# listed here: for a tier with a tab list that has none of its tabs -- or an
# unknown tier -- reading it gives its empty start value, whoever reads it
# (module, output, modal). Writing it is untouched, and so are the loads; a
# staff "ver como" that goes back to a full tier sees everything again,
# without reloading. Every synced reactive must be in one of the two lists
# (tests/test_t69b_datos_por_nivel.R).
HD_REACTIVOS_TESORERIA <- list(
  sap_data              = c("CAL", "VEN", "PH", "IC", "FC", "RPT"),
  sap_snapshot_info     = c("CAL", "VEN", "PH", "IC", "FC", "RPT"),
  pagar_hoy_db          = "PH",
  bancos_confirmados_db = c("BNC", "PH"),
  bancos_movimientos_db = "BNC",
  bancos_cuentas_db     = c("BNC", "PH"),
  ctas_cuentas          = c("BNC", "PH"),
  conciliacion_rv       = "BNC",
  pasivos_provisions_db = c("PSV", "CAL", "VEN", "PH"),
  pasivos_liabilities_db = "PSV",
  papelera_rv           = c("BNC", "CAL", "VEN", "PH", "PSV"),
  manual_inv            = c("CAL", "VEN", "PH"),
  notes_df              = c("CAL", "VEN", "PH"),
  tags_db               = c("CAL", "VEN", "PH"),
  moves_db              = c("CAL", "VEN", "PH"),
  policy_moves_db       = c("CAL", "VEN", "PH"),
  policy_catalog_db     = c("CAL", "VEN", "PH"),
  partner_policies_db   = c("CAL", "VEN", "PH"),
  holiday_overrides_db  = c("CAL", "VEN", "PH"),
  parte_alias_map_db    = c("CAL", "VEN", "PH"),
  abonos_db             = c("CAL", "VEN", "PH"),
  sap_ov_db             = c("CAL", "VEN", "PH"),
  interco_v2            = c("IC", "CAL", "VEN", "PH"),
  forecasting_series_observations_db = "FC",
  forecasting_metrics_db             = "FC",
  forecasting_subscriptions_db       = "FC",
  forecasting_manual_curves_db       = "FC")
# Shared by Panorama and Requisiciones (and every screen): never gated.
# usuarios_db / invitaciones_db (T83) are signals, not data: a number that
# changes when another session saved accounts or invitations. What Usuarios
# and Invitaciones then read is behind their own gates (can_view_tiers, T87-M6).
# erp_foto_db (T103 F2) is the same kind of signal: an ERP snapshot was saved
# elsewhere. What it makes a session read is sap_data, gated above (its
# observer asks nivel_reactivo_permitido(tier, "sap_data") before any read).
HD_REACTIVOS_COMUNES <- c("empresas_db", "grupos_db", "proveedores_db", "proveedores_inactivos_db",
                          "hop_grants_db", "usuarios_db", "invitaciones_db", "erp_foto_db")

# TRUE when a session of `tier` may read reactive `nombre`. Unknown tier: no.
# A name in neither list is not gated by app.R (the guardian test keeps the
# lists complete); asked here, it answers as common data.
nivel_reactivo_permitido <- function(tier, nombre) {
  pest <- HD_REACTIVOS_TESORERIA[[nombre]]
  if (is.null(pest)) return(TRUE)
  if (!.nivel_conocido(tier)) return(FALSE)
  lista <- tryCatch(nivel_pestanas(tier), error = function(e) NULL)
  is.null(lista) || any(pest %in% lista)
}

# ── Which treasury tables a tier may write (T69b) ────────────────────────────
# The read gate gives a gated session the EMPTY start value; a save that
# writes the whole session copy would then wipe the table. So a session may
# write a treasury table only if it may read it: .s3_write() asks
# session$userData$hd_escritura_permitida(key) (app.R) before anything is
# written. The S3 file (S3_KEYS value, or its per-user pattern) -> the
# reactive that holds it. pasivos_provisions is not listed on purpose:
# "Crear provisión" is Compras' own path (from Requisiciones), and every
# session write of it rebases on its base (an empty base only adds rows).
HD_ARCHIVOS_TESORERIA <- c(
  "invoice_moves.rds" = "moves_db", "calendar_notes.rds" = "notes_df", "invoice_tags.rds" = "tags_db",
  "manual_invoices.rds" = "manual_inv", "pagar_hoy.rds" = "pagar_hoy_db", "pagar_hoy_sync.rds" = "pagar_hoy_db",
  "agenda_sync_config.rds" = "pagar_hoy_db", "conciliacion.rds" = "conciliacion_rv", "interco_v2.rds" = "interco_v2",
  "abonos.rds" = "abonos_db", "papelera.rds" = "papelera_rv", "ctas_cuentas.rds" = "ctas_cuentas",
  "bancos_movimientos.rds" = "bancos_movimientos_db", "bancos_cuentas.rds" = "bancos_cuentas_db",
  "bancos_confirmados.rds" = "bancos_confirmados_db", "bancos_papelera.rds" = "bancos_movimientos_db",
  "parte_alias_map.rds" = "parte_alias_map_db", "policy_catalog.rds" = "policy_catalog_db",
  "partner_policies.rds" = "partner_policies_db", "policy_moves.rds" = "policy_moves_db",
  "holiday_overrides.rds" = "holiday_overrides_db", "sap_overrides.rds" = "sap_ov_db",
  "pasivos_liabilities.rds" = "pasivos_liabilities_db",
  "forecasting_series_observations.rds" = "forecasting_series_observations_db",
  "forecasting_metrics.rds" = "forecasting_metrics_db",
  "forecasting_subscriptions.rds" = "forecasting_subscriptions_db",
  "forecasting_manual_curves.rds" = "forecasting_manual_curves_db")
HD_ARCHIVOS_TESORERIA_PATRON <- c("^pagar_hoy_user_.*[.]rds$" = "pagar_hoy_db",
                                  "^saldos_apertura_user_.*[.]rds$" = "pagar_hoy_db")

# TRUE when a session of `tier` may write S3 file `key` (a key relative to
# the client, as .s3_write() gets it). Not a treasury file: always.
nivel_escritura_permitida <- function(tier, key) {
  key <- as.character(key %||% "")[1]
  nombre <- unname(HD_ARCHIVOS_TESORERIA[key])
  if (is.na(nombre)) {
    p <- names(HD_ARCHIVOS_TESORERIA_PATRON)[vapply(names(HD_ARCHIVOS_TESORERIA_PATRON), grepl, logical(1), x = key)]
    nombre <- if (length(p)) unname(HD_ARCHIVOS_TESORERIA_PATRON[p[1]]) else NA_character_
  }
  if (is.na(nombre)) return(TRUE)
  nivel_reactivo_permitido(tier, nombre)
}

.nivel_conocido <- function(tier)
  is.character(tier) && length(tier) == 1L && !is.na(tier) && nzchar(tier) && es_nivel(tier)

# NULL = the tier switches between Cobros and Pagos freely.
nivel_calendario <- function(tier) .nivel(tier)$calendario

# T89: does `tier` see the `libro` ledger ("AR" = CxC, "AP" = CxP)? Its
# calendario fixed to that ledger, or NULL (both, T69). Fail-closed: an
# unknown, empty or NULL tier sees neither (.nivel() of it is NULL, and its
# NULL calendario would otherwise read as "both").
nivel_ve_libro <- function(tier, libro) {
  if (!.nivel_conocido(tier)) return(FALSE)
  fijo <- nivel_calendario(tier)
  is.null(fijo) || identical(fijo, libro)
}

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
