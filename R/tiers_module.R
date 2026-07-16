# =============================================================================
# R/tiers_module.R
# Panel "Usuarios" — gestión de cuentas, permisos y configuración de tiers.
# Visible para dev y admin. Dev ve todo (incluyendo otras cuentas dev).
# Admin ve solo cuentas no-dev y no puede asignar tier dev.
# La sección "Config. de Tiers" es exclusiva para dev.
# =============================================================================

PERM_LABELS <- c(
  can_view_cobros          = "Ver Cobros (CxC)",
  can_view_pagos           = "Ver Pagos (CxP)",
  can_view_agenda          = "Ver Agenda de Hoy",
  can_view_bancos          = "Ver Bancos",
  can_view_reportes        = "Ver y exportar Reportes",
  can_edit_invoices        = "Editar facturas (mover, notas)",
  can_move_invoices        = "Mover fechas de facturas",
  can_manage_tags          = "Gestionar etiquetas (Urgente/Importante)",
  can_export_pdf           = "Exportar PDF",
  can_manage_providers     = "Gestionar proveedores",
  can_manage_users         = "Gestionar usuarios",
  can_view_tiers           = "Acceso al panel Usuarios",
  can_manage_empresas      = "Gestionar empresas del grupo",
  # SaaS admin permissions (Stage 1)
  can_approve_clients      = "Aprobar nuevos clientes [Principal]",
  can_manage_hopdesk_perms = "Gestionar permisos de staff HopDesk [Principal]",
  can_jump_clients         = "Acceder a contextos de clientes",
  can_manage_invites       = "Gestionar invitaciones de usuario",
  # Stage 4: split from the old single can_view_global_audit flag — browsing
  # any client's log is a different capability from seeing Hopdesk's own
  # staff activity log (ARCHITECTURE.md §6).
  can_view_client_audit_logs = "Ver bitácora de clientes (con selector)",
  can_view_staff_audit_log   = "Ver bitácora interna de Hopdesk"
)

# Keys that are immutably TRUE for principal accounts — UI renders them as locked.
.PRINCIPAL_LOCKED_KEYS <- c("can_approve_clients", "can_manage_hopdesk_perms")

.TIER_COLORS <- c(principal = "#7b1fa2", hopdesk = "#c2185b", dev = "#0d1b3e",
                  admin = "#6610f2", finance = "#0a58ca", analysis = "#6c757d")
.tier_color  <- function(t) .TIER_COLORS[[t]] %||% "#6c757d"

# ── UI ────────────────────────────────────────────────────────────────────────

tiersUI <- function(id) {
  ns <- NS(id)

  div(
    class = "p-3",
    style = "overflow-y: auto; max-height: calc(100vh - 65px);",

    # Header
    div(
      class = "tiers-header",
      icon("users-gear", class = "fa-2x"),
      div(
        tags$h4(class = "mb-0 fw-bold", "Gesti\u00f3n de Usuarios"),
        tags$p(class = "mb-0 small opacity-75",
               "Cuentas, tiers y permisos granulares del sistema")
      ),
      uiOutput(ns("header_badge"))
    ),

    # Stage 2 Part C: the tab bar is built dynamically server-side (see
    # tiersServer's output$main_tabs_ui) so Hopdesk-internal tabs can be
    # omitted entirely for a non-staff session — not rendered and then
    # locked, genuinely absent. See .tiers_all_panels()/tiers_visible_tab_keys().
    uiOutput(ns("main_tabs_ui"))
  )
}

# All 9 Usuarios/Grupo sub-tabs, keyed by the same tab keys
# tiers_visible_tab_keys() (R/tiers_tab_config.R) filters on. tiersServer's
# output$main_tabs_ui picks which of these to actually include for a given
# session — this function itself applies no visibility logic.
.tiers_all_panels <- function(ns) {
  list(
    # ── Tab: Usuarios ──────────────────────────────────────────────────────
    usuarios = bslib::nav_panel(
      title = tagList(icon("users"), " Usuarios"),
      value = "usuarios",
      div(
        class = "mt-3",
        fluidRow(
          column(
            width = 7,
            div(
              class = "card border-0 shadow-sm",
              div(
                class = "card-header bg-white py-2 d-flex align-items-center justify-content-between",
                tags$span(class = "fw-semibold", tagList(icon("users"), " Cuentas del sistema")),
                uiOutput(ns("new_user_btn_ui"))
              ),
              # Create form (hidden by default)
              shinyjs::hidden(
                div(
                  id = ns("create_form_wrap"),
                  class = "card-body border-bottom",
                  style = "background:#f8f9ff;",
                  tags$h6(class = "fw-semibold mb-3", style = "color:#0a58ca;",
                          tagList(icon("user-plus"), " Nueva cuenta")),
                  fluidRow(
                    column(6,
                      textInput(ns("new_display_name"), "Nombre para mostrar",
                                placeholder = "Ej: Tesorería")
                    ),
                    column(6,
                      textInput(ns("new_username"), "Nombre de usuario",
                                placeholder = "Ej: tesoreria")
                    )
                  ),
                  fluidRow(
                    column(6, passwordInput(ns("new_password"),  "Contraseña")),
                    column(6, passwordInput(ns("new_password2"), "Confirmar contraseña"))
                  ),
                  fluidRow(
                    column(6, uiOutput(ns("new_tier_ui"))),
                    column(6,
                      tags$div(style = "margin-top:26px;",
                        checkboxInput(ns("new_activo"), "Cuenta activa", value = TRUE))
                    )
                  ),
                  div(
                    class = "d-flex gap-2 mt-1",
                    actionButton(ns("btn_save_new_user"),
                                 tagList(icon("floppy-disk"), " Crear cuenta"),
                                 class = "btn btn-primary btn-sm"),
                    actionButton(ns("btn_cancel_new_user"), "Cancelar",
                                 class = "btn btn-outline-secondary btn-sm")
                  )
                )
              ),
              div(class = "card-body p-0",
                  DT::dataTableOutput(ns("usuarios_tbl")))
            )
          ),
          column(width = 5, uiOutput(ns("perm_editor_ui")))
        ),
        div(
          class = "alert alert-light border small mt-3",
          icon("circle-info", class = "text-primary"),
          tags$strong(" Tier vs. Permisos: "),
          "El tier define el rol base del usuario y sus permisos por defecto. ",
          "Los permisos individuales son ", tags$em("overrides"),
          " que se aplican encima del tier. ",
          "Un JSON vacío significa que el usuario tiene exactamente los permisos de su tier."
        )
      )
    ),

    # ── Tab: Actividad ────────────────────────────────────────────────────
    actividad = bslib::nav_panel(
      title = tagList(icon("clock-rotate-left"), " Actividad"),
      value = "actividad",
      div(
        class = "mt-3",
        tags$h6(class = "fw-semibold mb-0",
                tagList(icon("clock-rotate-left"), " Registro de actividad del sistema")),
        uiOutput(ns("activity_section"))
      )
    ),

    # ── Tab: Seguridad (principal only) — hidden entirely from clients ────
    security = bslib::nav_panel(
      title = tagList(icon("shield-halved"), " Seguridad"),
      value = "security",
      uiOutput(ns("security_section"))
    ),

    # ── Tab: Clientes (principal / hopdesk) — hidden entirely from clients ─
    clients = bslib::nav_panel(
      title = tagList(icon("building-user"), " Clientes"),
      value = "clients",
      uiOutput(ns("clients_section"))
    ),

    # ── Tab: Invitaciones (can_manage_invites) — visible to dev too ───────
    invites = bslib::nav_panel(
      title = tagList(icon("envelope-open-text"), " Invitaciones"),
      value = "invites",
      uiOutput(ns("invites_section"))
    ),

    # ── Tab: Accesos de Salto — hidden entirely from clients ──────────────
    hop_perms = bslib::nav_panel(
      title = tagList(icon("key"), " Accesos de Salto"),
      value = "hop_perms",
      uiOutput(ns("hop_section"))
    ),

    # ── Tab: Notificaciones (principal / hopdesk) — hidden entirely from clients
    notifications = bslib::nav_panel(
      title = tagList(icon("bell"), " Notificaciones"),
      value = "notifications",
      uiOutput(ns("notifications_section"))
    ),

    # ── Tab: Bitácora Global — hidden entirely from clients ───────────────
    global_audit = bslib::nav_panel(
      title = tagList(icon("scroll"), " Bitácora Global"),
      value = "global_audit",
      uiOutput(ns("global_audit_section"))
    ),

    # ── Tab: Config. de Tiers (dev only) ──────────────────────────────────
    tier_config = bslib::nav_panel(
      title = tagList(icon("sliders"), " Config. de Tiers"),
      value = "tier_config",
      uiOutput(ns("tier_config_section"))
    )
  )
}

# ── Permission editor panel ────────────────────────────────────────────────────

.tiers_perm_editor_ui <- function(ns, row, viewer_tier = "dev", registry = NULL) {
  tier <- row$tier %||% "finance"

  current_perms <- auth_resolve_perms(tier, row$permisos %||% "{}")
  overrides     <- tryCatch(jsonlite::fromJSON(row$permisos %||% "{}"),
                            error = function(e) list())

  tier_choices <- if (identical(viewer_tier, "principal")) {
    c("Principal" = "principal", "Hopdesk" = "hopdesk", "Dev" = "dev",
      "Admin" = "admin", "Finanzas" = "finance", "Análisis" = "analysis")
  } else if (identical(viewer_tier, "hopdesk")) {
    c("Hopdesk" = "hopdesk", "Dev" = "dev", "Admin" = "admin",
      "Finanzas" = "finance", "Análisis" = "analysis")
  } else if (identical(viewer_tier, "dev")) {
    c("Dev" = "dev", "Admin" = "admin",
      "Finanzas" = "finance", "Análisis" = "analysis")
  } else {
    c("Admin" = "admin", "Finanzas" = "finance", "Análisis" = "analysis")
  }

  is_dev_tier       <- identical(tier, "dev")
  is_principal_tier <- identical(tier, "principal")

  perm_rows <- lapply(names(PERM_LABELS), function(k) {
    is_locked <- (is_dev_tier && k == "can_view_tiers") ||
                 (is_principal_tier && k %in% .PRINCIPAL_LOCKED_KEYS)
    is_override <- k %in% names(overrides)
    current_val <- if (is_locked) TRUE else isTRUE(current_perms[[k]])

    badge <- if (is_locked && is_principal_tier) {
      tags$span(class = "tiers-perm-override", style = "color:#7b1fa2;",
                icon("lock", class = "fa-xs"), " obligatorio — principal")
    } else if (is_locked) {
      tags$span(class = "tiers-perm-override", style = "color:#198754;",
                icon("lock", class = "fa-xs"), " obligatorio")
    } else if (is_override) {
      tags$span(class = "tiers-perm-override", "override")
    } else {
      tags$span(class = "tiers-perm-default", "default (tier)")
    }

    tooltip_text <- if (is_locked && is_principal_tier)
      "Este permiso está bloqueado para cuentas principal. Modifícalo directamente en S3 para cambiarlo."
    else
      NULL

    cb <- if (is_locked) {
      locked_cb <- tags$div(class = "form-check",
               tags$input(type = "checkbox", class = "form-check-input",
                          checked = NA, disabled = NA,
                          style = "cursor:not-allowed; opacity:.6;"))
      if (!is.null(tooltip_text))
        tags$span(title = tooltip_text, locked_cb)
      else
        locked_cb
    } else {
      checkboxInput(ns(paste0("perm_", k)), label = NULL, value = current_val)
    }

    div(class = "tiers-perm-row",
        div(class = "d-flex align-items-center gap-2 flex-grow-1",
            cb,
            tags$span(PERM_LABELS[[k]],
                      class = if (is_locked) "small fw-semibold" else "small")),
        badge)
  })

  display_name <- row$display_name %||% row$username %||% "Usuario"

  div(
    class = "card border-0 shadow-sm",
    div(
      class = "card-header py-3",
      style = "background: linear-gradient(120deg,#1a1a2e 0%,#16213e 100%); color:#fff; border-radius:8px 8px 0 0;",
      div(
        class = "d-flex align-items-center gap-3",
        div(class = "rounded-circle d-flex align-items-center justify-content-center flex-shrink-0",
            style = "width:42px; height:42px; background:rgba(255,255,255,.15); font-size:1.1rem;",
            icon("user-pen")),
        div(
          tags$p(class = "mb-0",
                 style = "font-size:.7rem; opacity:.65; text-transform:uppercase; letter-spacing:.8px;",
                 "Editando permisos de"),
          tags$h5(class = "mb-0 fw-bold", style = "font-size:1.05rem;", display_name),
          div(class = "d-flex align-items-center gap-2 mt-1",
              tags$span(
                style = sprintf("font-size:.65rem; font-weight:700; letter-spacing:1px; text-transform:uppercase; background:%s; border:1px solid rgba(255,255,255,.3); padding:2px 8px; border-radius:20px;",
                                .tier_color(tier)),
                tier),
              tags$span(style = "font-size:.75rem; opacity:.6;",
                        paste0("@", row$username %||% "")))
        )
      )
    ),
    div(
      class = "card-body p-3",
      tags$h6(class = "fw-semibold mb-2 small text-muted text-uppercase", "Datos del usuario"),
      div(class = "mb-2",
          textInput(ns("edit_username"), "Usuario",
                    value = row$username %||% "", width = "100%")),
      div(class = "mb-2",
          textInput(ns("edit_display_name"), "Nombre para mostrar",
                    value = row$display_name %||% "", width = "100%")),
      div(class = "mb-2",
          passwordInput(ns("edit_password"), "Nueva contraseña (vacío = sin cambio)",
                        value = "", width = "100%")),
      div(class = "mb-2",
          passwordInput(ns("edit_password2"), "Confirmar contraseña",
                        value = "", width = "100%")),
      div(class = "mb-3",
          checkboxInput(ns("edit_activo"), "Activo", value = isTRUE(row$activo))),
      tags$hr(class = "my-2"),
      div(class = "mb-3",
          tags$label("Tier", class = "form-label small fw-semibold mb-1"),
          selectInput(ns("edit_tier"), NULL,
                      choices = tier_choices, selected = tier, width = "100%")),
      tags$h6(class = "fw-semibold mb-2 small text-muted text-uppercase", "Permisos granulares"),
      div(class = "border rounded", perm_rows),

      # ── Allowed clients (principal editing hopdesk/principal with can_jump_clients) ──
      if (isTRUE(current_perms$can_jump_clients) &&
          viewer_tier == "principal" &&
          !is.null(registry) && is.data.frame(registry) && nrow(registry)) {
        active_clients  <- registry[registry$status == "active", , drop = FALSE]
        client_choices  <- setNames(active_clients$client_id, active_clients$display_name)
        current_allowed <- tryCatch(
          jsonlite::fromJSON(row$allowed_clients %||% "[]"),
          error = function(e) character(0)
        )
        tagList(
          tags$hr(class = "my-2"),
          tags$h6(class = "fw-semibold mb-2 small text-muted text-uppercase",
                  tagList(icon("arrow-right-arrow-left"), " Contextos de cliente permitidos")),
          checkboxGroupInput(
            ns("edit_allowed_clients"),
            label   = NULL,
            choices  = client_choices,
            selected = intersect(current_allowed, active_clients$client_id),
            inline  = TRUE
          )
        )
      },

      div(class = "mt-3 d-flex gap-2",
          actionButton(ns("btn_save_perms"),
                       tagList(icon("floppy-disk"), " Guardar cambios"),
                       class = "btn btn-primary btn-sm"),
          actionButton(ns("btn_cancel_perms"), "Cancelar",
                       class = "btn btn-outline-secondary btn-sm"))
    )
  )
}

# ── Tier config card (one per tier) ───────────────────────────────────────────

.tier_config_card_ui <- function(ns, tier_key, tier_label, defaults, locked = TRUE) {
  perm_rows <- lapply(names(PERM_LABELS), function(k) {
    is_sys_locked <- identical(tier_key, "dev") && k == "can_view_tiers"
    val           <- if (is_sys_locked) TRUE else isTRUE(defaults[[k]])
    # Disable when section is locked OR it's a system-locked permission
    is_disabled   <- locked || is_sys_locked

    cb <- if (is_disabled) {
      tags$div(class = "form-check",
               tags$input(type = "checkbox", class = "form-check-input",
                          checked = if (val) NA else NULL,
                          disabled = NA,
                          style = if (is_sys_locked) "cursor:not-allowed; opacity:.5;"
                                  else "opacity:.55;"))
    } else {
      checkboxInput(ns(paste0("cfg_", tier_key, "_", k)), label = NULL, value = val)
    }

    div(
      style = "display:flex; align-items:center; gap:6px; padding:5px 10px; border-bottom:1px solid #f0f0f0;",
      cb,
      tags$span(PERM_LABELS[[k]], style = "font-size:.82rem;"),
      if (is_sys_locked)
        tags$span(style = "color:#198754; font-size:.65rem; margin-left:auto; white-space:nowrap;",
                  icon("lock", class = "fa-xs"))
    )
  })

  div(
    class = "card border-0 shadow-sm h-100",
    div(class = "card-header py-2 px-3",
        style = sprintf("background:%s; color:#fff;", .tier_color(tier_key)),
        tags$span(class = "fw-bold small", tier_label)),
    div(class = "card-body p-0 tiers-cfg-body", div(perm_rows))
  )
}

# ── Helpers ───────────────────────────────────────────────────────────────────

# Returns the next account_code ("U0001", "U0002", ...) based on all existing
# accounts (including soft-deleted ones so codes are never reused).
.next_account_code <- function(all_u) {
  if (is.null(all_u) || !nrow(all_u) || !"account_code" %in% names(all_u))
    return("U0001")
  codes <- all_u$account_code
  nums  <- suppressWarnings(as.integer(sub("^U0*", "", codes[grepl("^U\\d+$", codes)])))
  nums  <- nums[!is.na(nums)]
  if (!length(nums)) return("U0001")
  sprintf("U%04d", max(nums) + 1L)
}

# ── Server ────────────────────────────────────────────────────────────────────

tiersServer <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    current_tier <- reactive({
      tryCatch(shared$current_user_info()$tier, error = function(e) "")
    })

    # Usuarios panel is visible to dev / hopdesk / principal
    req_authorized    <- reactive({ current_tier() %in% c("principal", "hopdesk", "dev") })
    is_dev            <- reactive({ identical(current_tier(), "dev") })
    is_hopdesk        <- reactive({ identical(current_tier(), "hopdesk") })
    is_principal      <- reactive({ identical(current_tier(), "principal") })
    # Stage 2 Part A: effective_client_id() (home, or jumped if mid-jump) is
    # the single source of truth — this reactive just exposes it under this
    # module's existing name so its ~10 internal call sites don't all need
    # touching.
    current_client_id <- reactive({
      tryCatch(shared$effective_client_id(), error = function(e) tolower(Sys.getenv("CLIENT_ID")))
    })

    # ── Stage 2 Part C: dynamic tab bar ───────────────────────────────────────
    # Built exactly once, right after the viewer's is_staff is genuinely known
    # (same manual-guard pattern as Stage 1's landing-tab fix) — is_staff never
    # changes after login, so building this once avoids losing the viewer's
    # current sub-tab selection on every unrelated current_user_info() change
    # (e.g. a jump changes client_id, not is_staff).
    built_tabs_ui <- reactiveVal(NULL)
    observe({
      if (!is.null(built_tabs_ui())) return()                # already built
      info <- shared$current_user_info()
      if (is.null(info) || info$user == "unknown") return()  # not resolved yet
      visible <- tiers_visible_tab_keys(isTRUE(info$is_staff))
      panels  <- .tiers_all_panels(ns)[visible]
      built_tabs_ui(do.call(bslib::navset_pill, c(
        list(id = ns("main_tab")),
        unname(panels),
        list(bslib::nav_spacer(), bslib::nav_item(uiOutput(ns("cfg_lock_btn"))))
      )))
    })
    output$main_tabs_ui <- renderUI({ built_tabs_ui() })

    # ── Header badge (+ context switcher for hopdesk) ────────────────────────
    output$header_badge <- renderUI({
      tier <- current_tier()
      if (!nzchar(tier)) return(NULL)
      badge <- tags$span(class = "tiers-dev-badge", toupper(tier))

      if (!is_hopdesk() && !is_principal()) return(tags$span(class = "ms-auto", badge))

      # Resolve display name for the active client context
      grps       <- shared$grupos_db()
      active_cid <- current_client_id()
      ctx_label  <- tryCatch({
        if (!is.null(grps) && "client_id" %in% names(grps)) {
          hit <- grps[!is.na(grps$client_id) & grps$client_id == active_cid, , drop = FALSE]
          if (nrow(hit)) hit$name[1] else active_cid
        } else active_cid
      }, error = function(e) active_cid)

      tags$div(
        class = "ms-auto d-flex align-items-center gap-2",
        tags$span(
          style = "font-size:.75rem; color:#555; white-space:nowrap;",
          icon("circle-nodes", style = "color:#c2185b; margin-right:3px;"),
          "Cliente: ",
          tags$strong(ctx_label)
        ),
        actionButton(
          ns("btn_switch_ctx"),
          tagList(icon("arrow-right-arrow-left"), " Cambiar"),
          class = "btn btn-sm btn-outline-secondary",
          style = "font-size:.73rem; padding:3px 9px;"
        ),
        badge
      )
    })

    # ── Client context switcher modal (hopdesk only) ─────────────────────────
    observeEvent(input$btn_switch_ctx, {
      req(isTRUE(shared$current_user_info()$is_staff))
      # Build choices from user's allowed_clients intersected with live registry.
      # Principal sees all clients; hopdesk sees only grant-covered clients.
      registry <- tryCatch(read_client_registry(), error = function(e) .schema_client_registry())
      allowed_json <- shared$current_user_info()$allowed_clients %||% "[]"
      allowed_vec  <- tryCatch(jsonlite::fromJSON(allowed_json), error = function(e) character(0))

      if (isTRUE(is_principal())) {
        # Principal can jump to any registered active client
        valid <- registry[registry$status == "active", , drop = FALSE]
        # Also include the current deployment even if not formally in the registry
        current_deploy <- tolower(trimws(Sys.getenv("CLIENT_ID")))
        if (nzchar(current_deploy) && current_deploy != "hd-admin" &&
            !current_deploy %in% valid$client_id) {
          valid <- rbind(
            valid,
            data.frame(client_id = current_deploy, display_name = current_deploy,
                       status = "active", stringsAsFactors = FALSE)
          )
        }
      } else {
        # Also include any clients covered by active hop grants
        me  <- shared$current_user() %||% ""
        now <- Sys.time()
        hop <- tryCatch(load_hop_grants(), error = function(e) .schema_hop_grants())
        granted_ids <- if (is.data.frame(hop) && nrow(hop)) {
          active_hop <- hop[
            !is.na(hop$grantee) & hop$grantee == me &
            (is.na(hop$revoked) | hop$revoked != TRUE) &
            (is.na(hop$expires_at) | hop$expires_at > now),
            , drop = FALSE]
          unique(active_hop$client_id[!is.na(active_hop$client_id)])
        } else character(0)
        all_allowed <- unique(c(allowed_vec, granted_ids))
        valid <- registry[registry$client_id %in% all_allowed &
                            registry$status == "active", , drop = FALSE]
      }

      # hd-admin is always a valid destination for staff; prepend it
      hd_choice <- c("hd-admin" = "hd-admin")
      if (nrow(valid)) {
        client_choices <- setNames(valid$client_id, valid$display_name)
        choices <- c(hd_choice, client_choices[order(names(client_choices))])
      } else {
        choices <- hd_choice
      }

      showModal(modalDialog(
        title = tagList(icon("arrow-right-arrow-left"), " Cambiar contexto de cliente"),
        tags$p(class = "text-muted small mb-3",
               "Usuarios, Empresas y datos se filtrarán por el cliente seleccionado."),
        selectInput(ns("modal_ctx_select"), label = "Cliente:",
                    choices = choices, selected = current_client_id(), width = "100%"),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("modal_ctx_confirm"), "Cambiar contexto",
                       class = "btn btn-primary",
                       icon  = icon("arrow-right-arrow-left"))
        ),
        size = "s", easyClose = TRUE
      ))
    })

    observeEvent(input$modal_ctx_confirm, {
      req(isTRUE(shared$current_user_info()$is_staff))
      new_cid <- input$modal_ctx_select
      req(nzchar(new_cid %||% ""))
      # Principal can jump freely; hopdesk requires an active grant per client.
      # Always load fresh from S3 so grants take effect immediately without session restart.
      if (!isTRUE(is_principal()) && new_cid != "hd-admin") {
        grants <- tryCatch(load_hop_grants(), error = function(e) .schema_hop_grants())
        me     <- shared$current_user() %||% ""
        active_rows <- if (is.data.frame(grants) && nrow(grants))
          grants[
            !is.na(grants$grantee)    & grants$grantee    == me &
            !is.na(grants$client_id)  & grants$client_id  == new_cid &
            (is.na(grants$revoked) | grants$revoked != TRUE) &
            (is.na(grants$expires_at) | grants$expires_at > Sys.time()),
            , drop = FALSE]
        else data.frame()
        if (!nrow(active_rows)) {
          showNotification(
            sprintf("Sin acceso a '%s'. Solicita un permiso en la pestaña 'Accesos de Salto'.", new_cid),
            type = "error", duration = 5
          )
          removeModal()
          return()
        }
      }
      shared$jump_client_id(resolve_jump_target(new_cid, shared$home_client_id()))
      message("[CTX] context switched to: ", new_cid)
      removeModal()
    })

    # ── Config lock state ─────────────────────────────────────────────────────
    tier_config_locked <- reactiveVal(TRUE)

    output$cfg_lock_btn <- renderUI({
      if (!is_dev() && !is_hopdesk()) return(NULL)
      if (!identical(input$main_tab, "tier_config")) return(NULL)
      locked <- tier_config_locked()
      if (locked) {
        actionButton(ns("btn_toggle_lock"),
                     tagList(icon("lock"), " Editar configuración"),
                     class = "btn btn-outline-secondary fw-semibold",
                     style = "font-size:.8rem; padding:5px 16px; letter-spacing:.2px;")
      } else {
        actionButton(ns("btn_toggle_lock"),
                     tagList(icon("lock-open"), " Bloquear"),
                     class = "btn btn-danger fw-bold",
                     style = "font-size:.8rem; padding:5px 16px;")
      }
    })

    observeEvent(input$btn_toggle_lock, {
      tier_config_locked(!tier_config_locked())
    })

    # ── Tier config (S3) ──────────────────────────────────────────────────────
    # Stores custom tier defaults; NULL = use hardcoded auth_resolve_perms defaults
    tier_config_rv <- reactiveVal(NULL)

    observe({
      req(is_dev() || is_hopdesk() || is_principal())
      cfg <- tryCatch(.s3_read(S3_KEYS$tiers_config), error = function(e) NULL)
      if (!is.null(cfg)) tier_config_rv(cfg)
    })

    get_tier_defaults <- function(tier_key) {
      cfg <- tier_config_rv()
      if (!is.null(cfg) && !is.null(cfg[[tier_key]])) return(cfg[[tier_key]])
      auth_resolve_perms(tier_key, "{}")
    }

    # ── Usuarios data ─────────────────────────────────────────────────────────
    .usuarios_local <- reactiveVal(NULL)

    # Full list (unfiltered) — used for saves to avoid data loss
    all_usuarios_rv <- reactive({
      ovr <- .usuarios_local()
      if (!is.null(ovr)) return(ovr)
      req(req_authorized())
      auth_load_usuarios(client_id = current_client_id())
    })

    # Clear local cache whenever the client context changes so the next
    # all_usuarios_rv() call fetches the correct folder from S3.
    observeEvent(current_client_id(), {
      .usuarios_local(NULL)
    }, ignoreInit = TRUE)

    # Filtered list shown in the table (excludes soft-deleted and tier-filtered)
    usuarios_rv <- reactive({
      df <- all_usuarios_rv()
      req(!is.null(df))
      # Exclude soft-deleted accounts from display
      if (nrow(df) > 0 && "deleted" %in% names(df))
        df <- df[is.na(df$deleted) | df$deleted != TRUE, , drop = FALSE]
      # S3-folder isolation ensures this deployment's usuarios.rds only contains
      # this client's users — no client_id filter needed here.
      # Principal sees all tiers. Hopdesk sees all except principal.
      # Lower tiers cannot see hopdesk or dev rows.
      if (!is_principal() && !is_hopdesk() && nrow(df) > 0)
        df <- df[df$tier != "hopdesk", , drop = FALSE]
      if (!is_principal() && !is_hopdesk() && nrow(df) > 0)
        df <- df[df$tier != "principal", , drop = FALSE]
      if (!is_dev() && !is_hopdesk() && !is_principal() && nrow(df) > 0)
        df <- df[df$tier != "dev", , drop = FALSE]
      df
    })

    # ── Users table ───────────────────────────────────────────────────────────
    output$usuarios_tbl <- DT::renderDataTable({
      req(req_authorized())
      usuarios <- usuarios_rv()
      req(!is.null(usuarios) && nrow(usuarios) > 0)

      edit_input_id <- ns("trs_edit_click")
      del_input_id  <- ns("trs_del_click")

      tier_labels <- c(principal = "Principal", hopdesk = "Hopdesk", dev = "Dev",
                       admin = "Admin", finance = "Finanzas", analysis = "Análisis")

      df <- data.frame(
        Nombre  = usuarios$display_name,
        Tier    = tier_labels[usuarios$tier] %||% usuarios$tier,
        Estado  = ifelse(isTRUE(usuarios$activo) | usuarios$activo == TRUE,
                         "Activo", "Inactivo"),
        Editar  = vapply(seq_len(nrow(usuarios)), function(i) {
          sprintf(
            '<button class="btn btn-outline-primary btn-sm" onclick="Shiny.setInputValue(\'%s\',%d,{priority:\'event\'})">Editar</button>',
            edit_input_id, i)
        }, character(1)),
        Eliminar = vapply(seq_len(nrow(usuarios)), function(i) {
          sprintf(
            '<button class="btn btn-outline-danger btn-sm" onclick="Shiny.setInputValue(\'%s\',%d,{priority:\'event\'})"><i class=\"fa fa-trash\"></i></button>',
            del_input_id, i)
        }, character(1)),
        stringsAsFactors = FALSE,
        check.names      = FALSE
      )

      if (is_hopdesk() || is_principal()) {
        df <- cbind(
          data.frame(`#` = usuarios$account_code, stringsAsFactors = FALSE, check.names = FALSE),
          df
        )
      }

      DT::datatable(df, escape = FALSE, selection = "none", rownames = FALSE,
                    options = list(
                      pageLength = 15, dom = "tp",
                      language   = list(
                        zeroRecords = "No se encontraron usuarios",
                        info        = "Mostrando _START_ a _END_ de _TOTAL_ usuarios",
                        infoEmpty   = "Sin registros",
                        paginate    = list(previous = "Anterior", `next` = "Siguiente"))))
    })

    # ── New user form ─────────────────────────────────────────────────────────
    # shinyjs in a module server auto-prepends the namespace — use bare IDs here
    observeEvent(input$btn_new_user, { shinyjs::toggle("create_form_wrap") })
    observeEvent(input$btn_cancel_new_user, { shinyjs::hide("create_form_wrap") })

    output$new_tier_ui <- renderUI({
      viewer_tier     <- current_tier()
      viewer_is_staff <- isTRUE(shared$current_user_info()$is_staff)

      # Same rank/internal_only rule the save handler enforces below — kept
      # in sync via tier_assignment_allowed() so the dropdown never offers a
      # tier the server would reject anyway.
      tier_keys <- names(TIER_REGISTRY)[order(-vapply(names(TIER_REGISTRY), tier_rank, numeric(1)))]
      tier_keys <- Filter(function(t) tier_assignment_allowed(t, viewer_tier, viewer_is_staff), tier_keys)

      choices <- setNames(tier_keys, vapply(tier_keys, function(t) TIER_REGISTRY[[t]]$label, character(1)))
      selectInput(ns("new_tier"), "Tier", choices = choices, selected = "finance", width = "100%")
    })

    observeEvent(input$btn_save_new_user, {
      display_name <- trimws(input$new_display_name %||% "")
      username     <- tolower(trimws(input$new_username %||% ""))
      password     <- input$new_password  %||% ""
      password2    <- input$new_password2 %||% ""
      tier         <- input$new_tier      %||% "finance"

      if (!nzchar(display_name)) {
        showNotification("El nombre para mostrar es obligatorio.", type = "error"); return()
      }
      if (!nzchar(username) || !grepl("^[a-z0-9_]+$", username)) {
        showNotification("Usuario inv\u00e1lido (solo letras min\u00fasculas, n\u00fameros y _).",
                         type = "error"); return()
      }
      if (nchar(password) < 8) {
        showNotification("La contrase\u00f1a debe tener al menos 8 caracteres.", type = "error"); return()
      }
      if (!identical(password, password2)) {
        showNotification("Las contrase\u00f1as no coinciden.", type = "error"); return()
      }
      # ── HARD WALL: tier assignment gates ────────────────────────────────────────
      # Generic rank-based rule: a viewer may only assign a tier at or below
      # their own rank, and an internal_only (staff) tier only if the viewer
      # session itself is_staff. Enforced here regardless of what the
      # dropdown (UI layer, new_tier_ui above) offered — a forged request
      # cannot bypass it. Anything not assignable falls back to "admin".
      viewer_is_staff <- isTRUE(shared$current_user_info()$is_staff)
      if (!tier_assignment_allowed(tier, current_tier(), viewer_is_staff)) tier <- "admin"

      all_u <- all_usuarios_rv()
      if (!is.null(all_u) && username %in% tolower(all_u$username)) {
        showNotification("Ya existe un usuario con ese nombre.", type = "error"); return()
      }
      if (!isTRUE(check_username_available(username))) {
        showNotification("Este nombre de usuario ya está registrado en el sistema.",
                         type = "error", duration = 6); return()
      }

      new_row <- data.frame(
        id            = uuid::UUIDgenerate(),
        account_code  = .next_account_code(all_u),
        username      = username,
        password_hash = password,
        display_name  = display_name,
        tier          = tier,
        client_id     = current_client_id(),
        permisos      = "{}",
        activo        = isTRUE(input$new_activo),
        created_at    = as.character(Sys.time()),
        last_login    = NA_character_,
        deleted       = FALSE,
        deleted_at    = NA_character_,
        stringsAsFactors = FALSE
      )

      updated <- dplyr::bind_rows(all_u, new_row)
      saved <- tryCatch({ auth_save_usuarios(updated, client_id = current_client_id()); TRUE },
                        error = function(e) { message("[TIERS] create user save failed: ", e$message); FALSE })
      if (!saved) {
        showNotification("No se pudo guardar la cuenta. Verifica la conexión e intenta de nuevo.",
                         type = "error", duration = 6)
        return()
      }
      tryCatch(
        register_username(username, current_client_id(), new_row$account_code),
        error = function(e) message("[TIERS] username_index register failed: ", e$message)
      )
      tryCatch(
        update_client_user_count(current_client_id()),
        error = function(e) message("[TIERS] user count update failed: ", e$message)
      )
      tryCatch(
        notify_user_limit(current_client_id()),
        error = function(e) message("[TIERS] notify_user_limit failed: ", e$message)
      )
      .usuarios_local(updated)
      shinyjs::hide("create_form_wrap")
      updateTextInput(session, "new_display_name", value = "")
      updateTextInput(session, "new_username",      value = "")
      showNotification(paste0("Cuenta '\u200b", username, "' (", new_row$account_code, ") creada."),
                       type = "message", duration = 3)
      log_action(
        user        = shared$current_user(),
        module      = "usuarios",
        action      = "crear_usuario",
        description = paste0("Cuenta '", username, "' creada (tier: ", tier, ")"),
        target_id   = new_row$account_code,
        s3_key      = paste0(current_client_id(), "/usuarios.rds"),
        client_id   = current_client_id(),
        metadata    = list(tier = tier, display_name = display_name)
      )
      activity_refresh(activity_refresh() + 1L)
    })

    # ── Delete account ────────────────────────────────────────────────────────
    # pending_del_idx holds the row index awaiting confirmation
    pending_del_idx <- reactiveVal(NULL)

    observeEvent(input$trs_del_click, {
      idx      <- input$trs_del_click
      usuarios <- usuarios_rv()
      req(!is.null(usuarios) && idx <= nrow(usuarios))

      row      <- usuarios[idx, ]
      uname    <- row$username
      dname    <- row$display_name %||% uname
      tier     <- row$tier

      # Constraint pre-check: protected tiers need at least one account alive
      all_u          <- all_usuarios_rv()
      live_devs      <- all_u[all_u$tier == "dev"     & (is.na(all_u$deleted) | all_u$deleted != TRUE), ]
      live_hopdesks  <- all_u[all_u$tier == "hopdesk" & (is.na(all_u$deleted) | all_u$deleted != TRUE), ]
      dev_count      <- nrow(live_devs)
      hopdesk_count  <- nrow(live_hopdesks)
      is_dev_row     <- identical(tier, "dev")
      is_hopdesk_row <- identical(tier, "hopdesk")

      if (is_dev_row && dev_count <= 1) {
        showNotification(
          paste0("No se puede eliminar '\u200b", uname,
                 "': debe existir al menos una cuenta dev en el sistema."),
          type = "error", duration = 5)
        return()
      }
      if (is_hopdesk_row && hopdesk_count <= 1) {
        showNotification(
          paste0("No se puede eliminar '\u200b", uname,
                 "': debe existir al menos una cuenta Hopdesk en el sistema."),
          type = "error", duration = 5)
        return()
      }

      pending_del_idx(idx)

      # Warn extra loudly when deleting a protected account
      extra_warning <- if (is_hopdesk_row) {
        tags$p(class = "text-danger fw-semibold mb-0",
               icon("triangle-exclamation"), " Esta es una cuenta Hopdesk. Quedar\u00e1n ",
               hopdesk_count - 1L, " cuenta(s) Hopdesk despu\u00e9s de eliminar.")
      } else if (is_dev_row) {
        tags$p(class = "text-danger fw-semibold mb-0",
               icon("triangle-exclamation"), " Esta es una cuenta dev. Quedar\u00e1n ",
               dev_count - 1L, " cuenta(s) dev despu\u00e9s de eliminar.")
      } else NULL

      showModal(modalDialog(
        title = tagList(icon("trash"), " Eliminar cuenta"),
        tags$p("Vas a eliminar permanentemente la cuenta:"),
        tags$div(
          class = "alert alert-warning py-2",
          tags$strong(dname), tags$span(class = "text-muted ms-2", paste0("@", uname)),
          tags$span(
            class = "badge ms-2",
            style = sprintf("background:%s; color:#fff;", .tier_color(tier)),
            tier)
        ),
        extra_warning,
        tags$p(class = "text-muted small mb-0",
               icon("database"), " La cuenta quedar\u00e1 ",
               tags$strong("oculta"), " en el sistema, pero sus datos, movimientos y entradas ",
               "manuales se conservan en la base de datos para fines de auditor\u00eda."),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("btn_confirm_delete"),
                       tagList(icon("trash"), " S\u00ed, eliminar"),
                       class = "btn btn-danger btn-sm")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$btn_confirm_delete, {
      idx <- pending_del_idx()
      req(!is.null(idx))
      removeModal()

      usuarios <- usuarios_rv()
      req(!is.null(usuarios) && idx <= nrow(usuarios))

      target_user <- usuarios[idx, "username"]
      target_tier <- usuarios[idx, "tier"]
      all_u       <- all_usuarios_rv()

      # Final constraint guards (race-condition safety — re-check at save time)
      if (identical(target_tier, "hopdesk")) {
        live_hopdesks <- all_u[all_u$tier == "hopdesk" & (is.na(all_u$deleted) | all_u$deleted != TRUE), ]
        if (nrow(live_hopdesks) <= 1) {
          showNotification("No se puede eliminar: debe quedar al menos una cuenta Hopdesk.",
                           type = "error", duration = 5)
          return()
        }
      }
      if (identical(target_tier, "dev")) {
        live_devs <- all_u[all_u$tier == "dev" & (is.na(all_u$deleted) | all_u$deleted != TRUE), ]
        dev_count <- nrow(live_devs)
        if (dev_count <= 1) {
          showNotification(
            "Eliminaci\u00f3n cancelada: debe existir al menos una cuenta dev.",
            type = "error", duration = 5)
          pending_del_idx(NULL)
          return()
        }
      }

      # Soft delete — mark the row, keep in S3 for audit trail
      row_idx <- which(all_u$username == target_user)
      if (length(row_idx) == 1) {
        all_u[row_idx, "deleted"]    <- TRUE
        all_u[row_idx, "deleted_at"] <- as.character(Sys.time())
        all_u[row_idx, "activo"]     <- FALSE
      }
      saved <- tryCatch({ auth_save_usuarios(all_u, client_id = current_client_id()); TRUE },
                        error = function(e) { message("[TIERS] delete save failed: ", e$message); FALSE })
      if (!saved) {
        showNotification("No se pudo guardar el cambio. Verifica la conexión e intenta de nuevo.",
                         type = "error", duration = 6)
        pending_del_idx(NULL)
        return()
      }
      tryCatch(
        unregister_username(target_user),
        error = function(e) message("[TIERS] username_index unregister failed: ", e$message)
      )
      tryCatch(
        update_client_user_count(current_client_id()),
        error = function(e) message("[TIERS] user count update failed: ", e$message)
      )
      .usuarios_local(all_u)
      pending_del_idx(NULL)
      selected_user_idx(NULL)
      showNotification(paste0("Cuenta '\u200b", target_user, "' desactivada."),
                       type = "message", duration = 4)
      log_action(
        user        = shared$current_user(),
        module      = "usuarios",
        action      = "eliminar_usuario",
        description = paste0("Cuenta '", target_user, "' eliminada (tier: ", target_tier, ")"),
        target_id   = target_user,
        s3_key      = paste0(current_client_id(), "/usuarios.rds"),
        client_id   = current_client_id(),
        metadata    = list(tier = target_tier)
      )
      activity_refresh(activity_refresh() + 1L)
    })

    # ── Perm editor ───────────────────────────────────────────────────────────
    selected_user_idx <- reactiveVal(NULL)

    observeEvent(input$trs_edit_click, { selected_user_idx(input$trs_edit_click) })
    observeEvent(input$btn_cancel_perms, { selected_user_idx(NULL) })

    output$perm_editor_ui <- renderUI({
      idx <- selected_user_idx()
      req(!is.null(idx))
      usuarios <- usuarios_rv()
      req(!is.null(usuarios) && idx <= nrow(usuarios))
      registry <- tryCatch(read_client_registry(), error = function(e) .schema_client_registry())
      .tiers_perm_editor_ui(ns, usuarios[idx, ], viewer_tier = current_tier(),
                            registry = registry)
    })

    observeEvent(input$edit_tier, {
      req(!is.null(selected_user_idx()))
      base_perms <- auth_resolve_perms(input$edit_tier, "{}")
      for (k in names(PERM_LABELS))
        updateCheckboxInput(session, paste0("perm_", k), value = isTRUE(base_perms[[k]]))
    }, ignoreInit = TRUE)

    observeEvent(input$btn_save_perms, {
      idx <- selected_user_idx()
      req(!is.null(idx))
      usuarios <- usuarios_rv()
      req(!is.null(usuarios) && idx <= nrow(usuarios))

      original_tier <- usuarios[idx, "tier"]
      new_tier      <- input$edit_tier %||% original_tier

      # Safety guards — tier assignment walls (same generic rank/internal_only
      # rule as the create-user handler above; see comment there).
      viewer_is_staff <- isTRUE(shared$current_user_info()$is_staff)
      if (!tier_assignment_allowed(new_tier, current_tier(), viewer_is_staff)) new_tier <- "admin"
      # Hopdesk-tier protection: block demoting the last hopdesk
      if (identical(original_tier, "hopdesk") && !identical(new_tier, "hopdesk")) {
        all_u_check   <- all_usuarios_rv()
        live_hopdesks <- all_u_check[all_u_check$tier == "hopdesk" &
                                       (is.na(all_u_check$deleted) | all_u_check$deleted != TRUE), ]
        if (nrow(live_hopdesks) <= 1) {
          showNotification(
            "No se puede cambiar el tier: debe existir al menos una cuenta Hopdesk en el sistema.",
            type = "error", duration = 5)
          return()
        }
      }
      # Dev-tier protection: block demoting the last dev
      if (identical(original_tier, "dev") && !identical(new_tier, "dev")) {
        all_u_check <- all_usuarios_rv()
        live_devs   <- all_u_check[all_u_check$tier == "dev" &
                                     (is.na(all_u_check$deleted) | all_u_check$deleted != TRUE), ]
        if (nrow(live_devs) <= 1) {
          showNotification(
            "No se puede cambiar el tier: debe existir al menos una cuenta Dev en el sistema.",
            type = "error", duration = 5)
          return()
        }
      }

      base_perms <- auth_resolve_perms(new_tier, "{}")

      overrides <- list()
      for (k in names(PERM_LABELS)) {
        if (identical(new_tier, "dev") && k == "can_view_tiers") next
        val <- isTRUE(input[[paste0("perm_", k)]])
        if (!identical(val, isTRUE(base_perms[[k]]))) overrides[[k]] <- val
      }

      # Write back to full user list (not the filtered view)
      target_user  <- usuarios[idx, "username"]
      all_u        <- all_usuarios_rv()
      full_idx     <- which(all_u$username == target_user)

      # Validate password change before touching user data
      new_pass  <- trimws(input$edit_password  %||% "")
      new_pass2 <- trimws(input$edit_password2 %||% "")
      if (nzchar(new_pass)) {
        if (nchar(new_pass) < 8) {
          showNotification("La contraseña debe tener al menos 8 caracteres.", type = "error"); return()
        }
        if (!identical(new_pass, new_pass2)) {
          showNotification("Las contraseñas no coinciden.", type = "error"); return()
        }
      }

      # Validate username change
      new_username <- tolower(trimws(input$edit_username %||% ""))
      original_username <- usuarios[idx, "username"]
      if (!nzchar(new_username) || !grepl("^[a-z0-9_]+$", new_username)) {
        showNotification("Usuario inválido (solo letras minúsculas, números y _).", type = "error"); return()
      }
      if (!identical(new_username, original_username) && new_username %in% tolower(all_u$username)) {
        showNotification("Ya existe un usuario con ese nombre.", type = "error"); return()
      }

      if (length(full_idx) == 1) {
        # Basic user fields
        all_u[full_idx, "username"]  <- new_username
        new_display <- trimws(input$edit_display_name %||% "")
        if (nzchar(new_display)) all_u[full_idx, "display_name"] <- new_display
        if (nzchar(new_pass)) all_u[full_idx, "password_hash"] <- new_pass
        all_u[full_idx, "activo"] <- isTRUE(input$edit_activo)

        all_u[full_idx, "tier"]     <- new_tier
        all_u[full_idx, "permisos"] <- jsonlite::toJSON(overrides, auto_unbox = TRUE)

        # Save allowed_clients if the field was shown in the editor
        if (!is.null(input$edit_allowed_clients) && is_principal()) {
          all_u[full_idx, "allowed_clients"] <- jsonlite::toJSON(
            as.character(input$edit_allowed_clients), auto_unbox = FALSE)
        }

        saved <- tryCatch({ auth_save_usuarios(all_u, client_id = current_client_id()); TRUE },
                          error = function(e) { message("[TIERS] save perms failed: ", e$message); FALSE })
        if (!saved) {
          showNotification("No se pudieron guardar los permisos. Verifica la conexión e intenta de nuevo.",
                           type = "error", duration = 6)
          return()
        }
        .usuarios_local(all_u)
      }

      selected_user_idx(NULL)
      showNotification("Cambios guardados.", type = "message", duration = 2)
      log_action(
        user        = shared$current_user(),
        module      = "usuarios",
        action      = "editar_usuario",
        description = paste0("Permisos/tier de '", target_user, "' actualizados"),
        target_id   = target_user,
        s3_key      = paste0(current_client_id(), "/usuarios.rds"),
        client_id   = current_client_id(),
        metadata    = list(tier = new_tier, overrides_count = length(overrides))
      )
      activity_refresh(activity_refresh() + 1L)
    })

    # ── Tier config section ───────────────────────────────────────────────────
    TIER_META <- list(
      hopdesk  = "Hopdesk",
      dev      = "Dev",
      admin    = "Admin",
      finance  = "Finanzas",
      analysis = "Análisis"
    )

    selected_tier_rv <- reactiveVal("dev")

    observeEvent(input$btn_tier_prev, {
      tier_keys <- names(TIER_META)
      cur_idx   <- match(selected_tier_rv(), tier_keys)
      if (cur_idx > 1) {
        selected_tier_rv(tier_keys[cur_idx - 1])
        tier_config_locked(TRUE)
      }
    })

    observeEvent(input$btn_tier_next, {
      tier_keys <- names(TIER_META)
      cur_idx   <- match(selected_tier_rv(), tier_keys)
      if (cur_idx < length(tier_keys)) {
        selected_tier_rv(tier_keys[cur_idx + 1])
        tier_config_locked(TRUE)
      }
    })

    # ── Security panel (principal only) ─────────────────────────────────────────

    security_lock_refresh <- reactiveVal(0)   # bump to re-read lock file

    output$security_section <- renderUI({
      if (!isTRUE(is_principal())) {
        return(div(class = "alert alert-warning mt-3",
                   icon("lock"), " Esta sección solo está disponible para cuentas Principal."))
      }

      security_lock_refresh()   # reactive dependency

      lock_df <- tryCatch(read_emergency_lock(), error = function(e) NULL)
      locked_rows <- if (is.data.frame(lock_df) && nrow(lock_df)) lock_df else NULL

      ns <- session$ns

      div(
        class = "mt-3",

        # ── Lock a new account ───────────────────────────────────────────────
        div(class = "card border-0 shadow-sm mb-3",
          div(class = "card-header py-2 fw-semibold",
              style = "background:#fff3cd;",
              tagList(icon("lock"), " Bloquear cuenta de emergencia")),
          div(class = "card-body",
            tags$p(class = "small text-muted",
                   "Bloquea inmediatamente el acceso de una cuenta. ",
                   "Las sesiones activas de esa cuenta se terminarán en el siguiente ciclo de verificación (hasta 60s). ",
                   "El bloqueo persiste hasta que lo reviertas manualmente."),
            fluidRow(
              column(4, {
                # Collect all active usernames across every registered client folder.
                # The username index only covers users created post-Stage 2; scanning
                # client folders directly gives complete coverage including legacy users.
                all_unames <- tryCatch({
                  registry  <- read_client_registry()
                  cids <- c("hd-admin",
                            registry$client_id[registry$status == "active"])
                  cids <- unique(cids)
                  names_per_client <- lapply(cids, function(cid) {
                    u <- tryCatch(auth_load_usuarios(client_id = cid),
                                  error = function(e) NULL)
                    if (is.null(u) || !nrow(u)) return(character(0))
                    active <- u[is.na(u$deleted) | u$deleted != TRUE, , drop = FALSE]
                    tolower(trimws(active$username))
                  })
                  sort(unique(unlist(names_per_client)))
                }, error = function(e) character(0))
                selectizeInput(
                  ns("sec_lock_user"),
                  label    = "Nombre de usuario",
                  choices  = all_unames,
                  multiple = TRUE,
                  options  = list(
                    placeholder    = "Buscar o escribir usuario...",
                    create         = TRUE,
                    maxItems       = 1L,
                    searchField    = "value",
                    dropdownParent = "body"
                  ),
                  width = "100%"
                )
              }),
              column(6, textInput(ns("sec_lock_reason"), "Motivo", placeholder = "Credenciales comprometidas")),
              column(2,
                tags$div(style = "margin-top:26px;",
                  actionButton(ns("btn_lock_account"),
                               tagList(icon("lock"), " Bloquear"),
                               class = "btn btn-warning btn-sm w-100")))
            )
          )
        ),

        # ── Currently locked accounts ────────────────────────────────────────
        div(class = "card border-0 shadow-sm",
          div(class = "card-header py-2 fw-semibold d-flex align-items-center justify-content-between",
              tagList(icon("shield-halved"), " Cuentas bloqueadas actualmente"),
              actionButton(ns("btn_sec_refresh"), tagList(icon("arrows-rotate")),
                           class = "btn btn-sm btn-outline-secondary")),
          div(class = "card-body p-0",
            if (is.null(locked_rows)) {
              div(class = "p-3 text-muted small", icon("check-circle", class = "text-success"),
                  " No hay cuentas bloqueadas.")
            } else {
              tags$table(class = "table table-sm table-hover mb-0",
                tags$thead(tags$tr(
                  tags$th("Usuario"), tags$th("Bloqueado el"),
                  tags$th("Bloqueado por"), tags$th("Motivo"), tags$th("")
                )),
                tags$tbody(
                  lapply(seq_len(nrow(locked_rows)), function(i) {
                    r <- locked_rows[i, , drop = FALSE]
                    tags$tr(
                      tags$td(tags$strong(r$username)),
                      tags$td(class = "small", format(r$locked_at, "%d/%m/%Y %H:%M")),
                      tags$td(class = "small text-muted", r$locked_by),
                      tags$td(class = "small", r$reason),
                      tags$td(
                        tags$button(
                          class   = "btn btn-sm btn-outline-success",
                          type    = "button",
                          onclick = sprintf(
                            "Shiny.setInputValue('%s','%s',{priority:'event'})",
                            ns("sec_unlock_user"), r$username),
                          tagList(icon("unlock"), " Desbloquear")
                        )
                      )
                    )
                  })
                )
              )
            }
          )
        )
      )
    })

    observeEvent(input$btn_sec_refresh, {
      security_lock_refresh(security_lock_refresh() + 1L)
    })

    observeEvent(input$btn_lock_account, {
      req(is_principal())
      username <- tolower(trimws(input$sec_lock_user  %||% ""))
      reason   <- trimws(input$sec_lock_reason %||% "")
      if (!nzchar(username)) {
        showNotification("Especifica el nombre de usuario a bloquear.", type = "error"); return()
      }
      if (!nzchar(reason)) {
        showNotification("Especifica el motivo del bloqueo.", type = "error"); return()
      }
      current_viewer <- shared$current_user() %||% "unknown"
      existing <- tryCatch(read_emergency_lock(), error = function(e) .schema_emergency_lock())
      if (is.null(existing)) existing <- .schema_emergency_lock()
      if (any(tolower(existing$username) == username)) {
        showNotification(paste0("'", username, "' ya está bloqueado."), type = "warning"); return()
      }
      new_row <- data.frame(
        username  = username,
        locked_at = Sys.time(),
        locked_by = current_viewer,
        reason    = reason,
        stringsAsFactors = FALSE
      )
      updated <- rbind(existing, new_row)
      tryCatch({
        write_emergency_lock(updated)
        security_lock_refresh(security_lock_refresh() + 1L)
        updateSelectizeInput(session, "sec_lock_user", selected = character(0))
        updateTextInput(session, "sec_lock_reason", value = "")
        showNotification(paste0("Cuenta '", username, "' bloqueada."),
                         type = "message", duration = 4)
        log_action(user        = shared$current_user(),
                   module      = "seguridad",
                   action      = "emergency_lock",
                   description = paste0("Lock applied to '", username, "' — reason: ", reason),
                   target_id   = username,
                   s3_key      = "hd-admin/emergency_lock.rds",
                   client_id   = "hd-admin")
        activity_refresh(activity_refresh() + 1L)
      }, error = function(e) {
        showNotification(paste0("Error al bloquear: ", e$message), type = "error")
      })
    }, ignoreInit = TRUE)

    observeEvent(input$sec_unlock_user, {
      req(is_principal())
      username <- tolower(trimws(input$sec_unlock_user %||% ""))
      req(nzchar(username))
      current_viewer <- shared$current_user() %||% "unknown"
      existing <- tryCatch(read_emergency_lock(), error = function(e) NULL)
      if (is.null(existing) || !nrow(existing)) return()
      updated <- existing[tolower(existing$username) != username, , drop = FALSE]
      tryCatch({
        write_emergency_lock(updated)
        security_lock_refresh(security_lock_refresh() + 1L)
        showNotification(paste0("Cuenta '", username, "' desbloqueada."),
                         type = "message", duration = 4)
        log_action(user        = shared$current_user(),
                   module      = "seguridad",
                   action      = "emergency_unlock",
                   description = paste0("Lock removed from '", username, "' by ", current_viewer),
                   target_id   = username,
                   s3_key      = "hd-admin/emergency_lock.rds",
                   client_id   = "hd-admin")
        activity_refresh(activity_refresh() + 1L)
      }, error = function(e) {
        showNotification(paste0("Error al desbloquear: ", e$message), type = "error")
      })
    }, ignoreInit = TRUE)

    # ── Clientes panel (principal + hopdesk) ─────────────────────────────────

    clients_refresh         <- reactiveVal(0L)
    requests_refresh        <- reactiveVal(0L)
    activity_refresh        <- reactiveVal(0L)
    solicitar_usuarios_sent <- reactiveVal(FALSE)  # session-level: TRUE after request sent

    # ── New-user button: dynamic based on client's remaining user slots ────────
    output$new_user_btn_ui <- renderUI({
      ns <- session$ns
      cid <- current_client_id()

      # hd-admin has no user-limit concept — always show normal button
      if (!nzchar(cid) || cid == "hd-admin") {
        return(actionButton(ns("btn_new_user"),
                            tagList(icon("user-plus"), " Nueva cuenta"),
                            class = "btn btn-sm btn-outline-primary"))
      }

      registry <- tryCatch(read_client_registry(), error = function(e) NULL)
      row <- if (!is.null(registry))
        registry[registry$client_id == cid, , drop = FALSE]
      else
        NULL

      # If registry entry missing, fall back to normal button
      if (is.null(row) || !nrow(row)) {
        return(actionButton(ns("btn_new_user"),
                            tagList(icon("user-plus"), " Nueva cuenta"),
                            class = "btn btn-sm btn-outline-primary"))
      }

      cur <- as.integer(row$current_users[1])
      max <- as.integer(row$max_users[1])
      rem <- max - cur

      if (rem <= 0) {
        # At capacity: show "Solicitar usuarios" (or "Solicitud enviada" if already sent)
        if (isTRUE(solicitar_usuarios_sent())) {
          tags$span(class = "btn btn-sm btn-secondary disabled",
                    tagList(icon("check"), " Solicitud enviada"))
        } else {
          actionButton(ns("btn_solicitar_usuarios"),
                       tagList(icon("hand-paper"), " Solicitar usuarios"),
                       class = "btn btn-sm btn-warning")
        }
      } else if (rem <= 2) {
        # Warning zone: normal button with an amber badge showing slots left
        tagList(
          actionButton(ns("btn_new_user"),
                       tagList(icon("user-plus"), " Nueva cuenta"),
                       class = "btn btn-sm btn-outline-warning"),
          tags$span(class = "badge bg-warning text-dark ms-1",
                    tagList(icon("triangle-exclamation"), " ", rem))
        )
      } else {
        actionButton(ns("btn_new_user"),
                     tagList(icon("user-plus"), " Nueva cuenta"),
                     class = "btn btn-sm btn-outline-primary")
      }
    })

    observeEvent(input$btn_solicitar_usuarios, {
      cid <- current_client_id()
      req(nzchar(cid), cid != "hd-admin")
      req(!isTRUE(solicitar_usuarios_sent()))

      registry <- tryCatch(read_client_registry(), error = function(e) NULL)
      req(!is.null(registry))
      row <- registry[registry$client_id == cid, , drop = FALSE]
      req(nrow(row) == 1)

      dname   <- row$display_name[1]
      cur     <- as.integer(row$current_users[1])
      max     <- as.integer(row$max_users[1])
      contact <- row$contact_email[1] %||% ""

      append_hd_notification(
        type         = "user_request",
        client_id    = cid,
        message_text = sprintf("'%s' solicita más usuarios (%d/%d).", dname, cur, max),
        metadata     = list(current_users = cur, max_users = max, contact_email = contact)
      )

      # Email to all HopDesk staff with can_manage_invites
      tryCatch({
        hd_staff <- auth_load_usuarios(client_id = "hd-admin")
        staff_emails <- hd_staff[
          (is.na(hd_staff$deleted) | hd_staff$deleted != TRUE) &
          isTRUE(hd_staff$can_manage_invites) &
          !is.na(hd_staff$email) & nzchar(hd_staff$email %||% ""),
          "email", drop = TRUE
        ]
        if (length(staff_emails)) {
          html <- email_user_request_html(dname, cur, max, contact)
          subj <- sprintf("[HopDesk] Solicitud de más usuarios — %s", dname)
          for (addr in staff_emails) send_email(addr, subj, html)
        }
      }, error = function(e) message("[LIMIT] user_request email failed: ", e$message))

      solicitar_usuarios_sent(TRUE)
      showNotification(
        "Tu solicitud fue enviada al equipo HopDesk. Te contactarán a la brevedad.",
        type = "message", duration = 6
      )
      log_action(
        user        = shared$current_user(),
        module      = "usuarios",
        action      = "solicitar_usuarios",
        description = sprintf("'%s' solicitó aumento de límite (%d/%d)", dname, cur, max),
        target_id   = cid,
        s3_key      = paste0(cid, "/app_audit.rds"),
        client_id   = cid
      )
      activity_refresh(activity_refresh() + 1L)
    }, ignoreInit = TRUE)

    output$clients_section <- renderUI({
      if (!isTRUE(is_hopdesk()) && !isTRUE(is_principal())) {
        return(div(class = "alert alert-warning mt-3",
                   icon("lock"), " Esta sección solo está disponible para Hopdesk y Principal."))
      }

      clients_refresh()
      requests_refresh()

      registry <- tryCatch(read_client_registry(), error = function(e) .schema_client_registry())
      requests <- tryCatch(read_client_requests(),  error = function(e) .schema_client_requests())

      ns <- session$ns

      pending <- requests[!is.na(requests$status) & requests$status == "pending", , drop = FALSE]

      div(
        class = "mt-3",

        # ── Toolbar ──────────────────────────────────────────────────────────
        div(class = "d-flex gap-2 mb-3",
          actionButton(ns("btn_new_client_req"),
                       tagList(icon("plus"), " Nueva solicitud"),
                       class = "btn btn-primary btn-sm"),
          actionButton(ns("btn_clients_refresh"),
                       tagList(icon("arrows-rotate")),
                       class = "btn btn-outline-secondary btn-sm")
        ),

        # ── Pending requests (principal only) ─────────────────────────────
        if (isTRUE(is_principal()) && nrow(pending) > 0) {
          div(class = "card border-warning shadow-sm mb-3",
            div(class = "card-header py-2 fw-semibold",
                style = "background:#fff3cd;",
                tagList(icon("clock"), sprintf(" Solicitudes pendientes (%d)", nrow(pending)))),
            div(class = "card-body p-0",
              tags$table(class = "table table-sm table-hover mb-0",
                tags$thead(tags$tr(
                  tags$th("ID propuesto"), tags$th("Nombre"), tags$th("Solicitado por"),
                  tags$th("Fecha"), tags$th("Límite"), tags$th("")
                )),
                tags$tbody(
                  lapply(seq_len(nrow(pending)), function(i) {
                    r <- pending[i, , drop = FALSE]
                    tags$tr(
                      tags$td(tags$code(r$client_id_proposed)),
                      tags$td(r$display_name),
                      tags$td(class = "text-muted small", r$requested_by),
                      tags$td(class = "small",
                              format(as.POSIXct(r$requested_at), "%d/%m/%Y %H:%M",
                                     tz = "America/Mexico_City")),
                      tags$td(r$max_users),
                      tags$td(
                        tags$button(
                          class   = "btn btn-sm btn-outline-primary",
                          type    = "button",
                          onclick = sprintf(
                            "Shiny.setInputValue('%s','%s',{priority:'event'})",
                            ns("review_request_id"), r$id),
                          tagList(icon("magnifying-glass"), " Revisar")
                        )
                      )
                    )
                  })
                )
              )
            )
          )
        },

        # ── Client registry table ─────────────────────────────────────────
        {
          # Tail-read the active audit chunk for each client to show last activity.
          # Reads only app_audit.rds (active chunk) — skips historical chunks for speed.
          last_activity <- setNames(
            lapply(registry$client_id, function(cid) {
              tryCatch({
                key <- paste0(cid, "/app_audit.rds")
                df  <- suppressMessages(suppressWarnings(
                  aws.s3::s3readRDS(object = key, bucket = .s3_bucket())
                ))
                if (!is.null(df) && is.data.frame(df) && nrow(df) && "ts" %in% names(df)) {
                  ts <- suppressWarnings(as.POSIXct(df$ts))
                  ts <- ts[!is.na(ts)]
                  if (length(ts)) max(ts) else NA
                } else NA
              }, error = function(e) NA)
            }),
            registry$client_id
          )

          div(class = "card border-0 shadow-sm",
            div(class = "card-header py-2 fw-semibold",
                tagList(icon("list"), " Clientes registrados")),
            div(class = "card-body p-0",
              if (!nrow(registry)) {
                div(class = "p-3 text-muted small",
                    icon("info-circle"), " No hay clientes registrados aún.")
              } else {
                tags$table(class = "table table-sm table-hover mb-0",
                  tags$thead(tags$tr(
                    tags$th("ID"), tags$th("Nombre"), tags$th("Usuarios"),
                    tags$th("Contacto"), tags$th("Últ. actividad"),
                    tags$th("Estado"), tags$th("")
                  )),
                  tags$tbody(
                    lapply(seq_len(nrow(registry)), function(i) {
                      r   <- registry[i, , drop = FALSE]
                      rem <- r$max_users - r$current_users
                      row_style <- if (rem <= 0) "background:#ffe0e0;"
                                   else if (rem <= 2) "background:#fff8e1;" else ""
                      badge <- if (rem <= 0)
                        tags$span(class = "badge bg-danger ms-1", "Lleno")
                      else if (rem <= 2)
                        tags$span(class = "badge bg-warning text-dark ms-1",
                                  tagList(icon("triangle-exclamation"), " ", rem, " restantes"))
                      else NULL
                      last_ts <- last_activity[[r$client_id]]
                      last_ts_str <- if (!is.na(last_ts) && !is.null(last_ts))
                        format(last_ts, "%d/%m/%Y %H:%M", tz = "America/Mexico_City")
                      else "—"
                      tags$tr(
                        style = row_style,
                        tags$td(tags$code(r$client_id)),
                        tags$td(r$display_name, badge),
                        tags$td(sprintf("%d / %d", r$current_users, r$max_users)),
                        tags$td(class = "small text-muted", r$contact_email %||% "—"),
                        tags$td(class = "small text-muted", last_ts_str),
                        tags$td(
                          if (identical(r$status, "active"))
                            tags$span(class = "badge bg-success", "Activo")
                          else
                            tags$span(class = "badge bg-secondary", "Archivado")
                        ),
                        tags$td(
                          div(class = "d-flex gap-1",
                            tags$button(
                              class   = "btn btn-sm btn-outline-primary",
                              type    = "button",
                              title   = "Editar cliente",
                              onclick = sprintf(
                                "Shiny.setInputValue('%s','%s',{priority:'event'})",
                                ns("edit_client_id"), r$client_id),
                              icon("pencil")
                            ),
                            tags$button(
                              class   = "btn btn-sm btn-outline-secondary",
                              type    = "button",
                              title   = "Gestionar contactos",
                              onclick = sprintf(
                                "Shiny.setInputValue('%s','%s',{priority:'event'})",
                                ns("open_contacts_modal"), r$client_id),
                              icon("address-book")
                            )
                          )
                        )
                      )
                    })
                  )
                )
              }
            )
          )
        }
      )
    })

    observeEvent(input$btn_clients_refresh, {
      clients_refresh(clients_refresh() + 1L)
      requests_refresh(requests_refresh() + 1L)
    })

    # ── New client request modal ──────────────────────────────────────────────
    observeEvent(input$btn_new_client_req, {
      req(is_hopdesk() || is_principal())
      ns <- session$ns
      showModal(modalDialog(
        title  = tagList(icon("building-user"), " Solicitud de nuevo cliente"),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("btn_submit_client_req"), "Enviar solicitud",
                       class = "btn btn-primary")
        ),
        size = "m",
        textInput(ns("req_client_id"), "ID propuesto (slug, sin espacios)",
                  placeholder = "acme-corp"),
        textInput(ns("req_display_name"), "Nombre completo del cliente",
                  placeholder = "Acme Corporation S.A. de C.V."),
        textInput(ns("req_contact_email"), "Correo de contacto",
                  placeholder = "admin@acmecorp.com"),
        numericInput(ns("req_max_users"), "Límite de usuarios", value = 5, min = 1, max = 500),
        textAreaInput(ns("req_notes"), "Notas (opcional)", rows = 2)
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$btn_submit_client_req, {
      req(is_hopdesk() || is_principal())
      cid_prop  <- tolower(trimws(input$req_client_id    %||% ""))
      dname     <- trimws(input$req_display_name %||% "")
      email     <- trimws(input$req_contact_email %||% "")
      max_u     <- as.integer(input$req_max_users %||% 5L)
      notes     <- trimws(input$req_notes %||% "")

      if (!nzchar(cid_prop) || grepl("[^a-z0-9_-]", cid_prop)) {
        showNotification("El ID solo puede contener letras minúsculas, números, _ y -.",
                         type = "error"); return()
      }
      if (!nzchar(dname)) {
        showNotification("El nombre del cliente es obligatorio.", type = "error"); return()
      }

      existing_reg <- tryCatch(read_client_registry(), error = function(e) .schema_client_registry())
      if (any(existing_reg$client_id == cid_prop)) {
        showNotification(paste0("El ID '", cid_prop, "' ya existe en el registro."),
                         type = "error"); return()
      }
      existing_req <- tryCatch(read_client_requests(), error = function(e) .schema_client_requests())
      if (any(existing_req$client_id_proposed == cid_prop &
              existing_req$status == "pending")) {
        showNotification(paste0("Ya hay una solicitud pendiente para '", cid_prop, "'."),
                         type = "warning"); return()
      }

      new_req <- data.frame(
        id                 = uuid::UUIDgenerate(),
        requested_by       = shared$current_user() %||% "unknown",
        client_id_proposed = cid_prop,
        display_name       = dname,
        contact_email      = email,
        max_users          = max_u,
        notes              = notes,
        status             = "pending",
        requested_at       = as.character(Sys.time()),
        reviewed_by        = NA_character_,
        reviewed_at        = NA_character_,
        rejection_reason   = NA_character_,
        stringsAsFactors   = FALSE
      )

      saved <- tryCatch({
        write_client_requests(rbind(existing_req, new_req)); TRUE
      }, error = function(e) { message("[CLIENTS] request save failed: ", e$message); FALSE })

      if (!saved) {
        showNotification("Error al guardar la solicitud. Intenta de nuevo.", type = "error")
        return()
      }
      removeModal()
      requests_refresh(requests_refresh() + 1L)
      showNotification(paste0("Solicitud para '", cid_prop, "' enviada."),
                       type = "message", duration = 4)
      log_action(user        = shared$current_user(),
                 module      = "clientes",
                 action      = "solicitud_nuevo_cliente",
                 description = paste0("Solicitud para '", cid_prop, "' (", dname, ")"),
                 target_id   = cid_prop,
                 s3_key      = "hd-admin/client_requests.rds",
                 client_id   = "hd-admin")
      activity_refresh(activity_refresh() + 1L)
    }, ignoreInit = TRUE)

    # ── Review request modal (principal only) ─────────────────────────────────
    observeEvent(input$review_request_id, {
      req(is_principal())
      rid <- input$review_request_id
      req(nzchar(rid %||% ""))
      reqs <- tryCatch(read_client_requests(), error = function(e) NULL)
      req(!is.null(reqs))
      row <- reqs[reqs$id == rid, , drop = FALSE]
      req(nrow(row) == 1)
      ns <- session$ns

      showModal(modalDialog(
        title  = tagList(icon("magnifying-glass"), " Revisar solicitud"),
        footer = tagList(
          modalButton("Cerrar"),
          actionButton(ns("btn_reject_req"),  "Rechazar",  class = "btn btn-outline-danger"),
          actionButton(ns("btn_approve_req"), "Aprobar",   class = "btn btn-success")
        ),
        size = "m",
        tags$dl(class = "row mb-0",
          tags$dt(class="col-sm-4","ID propuesto"), tags$dd(class="col-sm-8", tags$code(row$client_id_proposed)),
          tags$dt(class="col-sm-4","Nombre"),       tags$dd(class="col-sm-8", row$display_name),
          tags$dt(class="col-sm-4","Contacto"),     tags$dd(class="col-sm-8", row$contact_email %||% "—"),
          tags$dt(class="col-sm-4","Límite"),       tags$dd(class="col-sm-8", row$max_users),
          tags$dt(class="col-sm-4","Solicitado por"), tags$dd(class="col-sm-8", row$requested_by),
          tags$dt(class="col-sm-4","Notas"),        tags$dd(class="col-sm-8", row$notes %||% "—")
        ),
        tags$hr(),
        textAreaInput(ns("review_rejection_reason"), "Motivo de rechazo (opcional)", rows = 2)
      ))

      # Store the request id for the approve/reject handlers
      session$userData$reviewing_req_id <- rid
    }, ignoreInit = TRUE)

    observeEvent(input$btn_approve_req, {
      req(is_principal())
      rid <- session$userData$reviewing_req_id %||% ""
      req(nzchar(rid))

      reqs <- tryCatch(read_client_requests(), error = function(e) NULL)
      req(!is.null(reqs))
      idx <- which(reqs$id == rid)
      req(length(idx) == 1)
      row <- reqs[idx, , drop = FALSE]
      reviewer <- shared$current_user() %||% "unknown"

      # 1. Initialize S3 folder
      init_ok <- tryCatch({
        initialize_client_folder(row$client_id_proposed, created_by = reviewer); TRUE
      }, error = function(e) {
        message("[CLIENTS] initialize_client_folder failed: ", e$message); FALSE
      })
      if (!init_ok) {
        showNotification("Error al inicializar el folder del cliente en S3.", type = "error")
        return()
      }

      # 2. Write to client_registry
      registry <- tryCatch(read_client_registry(), error = function(e) .schema_client_registry())
      new_entry <- data.frame(
        client_id     = row$client_id_proposed,
        display_name  = row$display_name,
        max_users     = row$max_users,
        current_users = 0L,
        contact_email = row$contact_email %||% NA_character_,
        status        = "active",
        created_at    = as.character(Sys.time()),
        created_by    = reviewer,
        stringsAsFactors = FALSE
      )
      saved_reg <- tryCatch({
        write_client_registry(rbind(registry, new_entry)); TRUE
      }, error = function(e) { message("[CLIENTS] registry save failed: ", e$message); FALSE })
      if (!saved_reg) {
        showNotification("Error al guardar el registro del cliente.", type = "error"); return()
      }

      # 3. Update request status
      reqs$status[idx]      <- "approved"
      reqs$reviewed_by[idx] <- reviewer
      reqs$reviewed_at[idx] <- as.character(Sys.time())
      tryCatch(write_client_requests(reqs),
               error = function(e) message("[CLIENTS] request update failed: ", e$message))

      removeModal()
      clients_refresh(clients_refresh()   + 1L)
      requests_refresh(requests_refresh() + 1L)
      showNotification(paste0("Cliente '", row$client_id_proposed, "' aprobado y activado."),
                       type = "message", duration = 5)
      log_action(user        = reviewer,
                 module      = "clientes",
                 action      = "aprobacion_cliente",
                 description = paste0("Cliente '", row$client_id_proposed,
                                      "' (", row$display_name, ") aprobado"),
                 target_id   = row$client_id_proposed,
                 s3_key      = "hd-admin/client_registry.rds",
                 client_id   = "hd-admin")
      activity_refresh(activity_refresh() + 1L)
      session$userData$reviewing_req_id <- NULL
    }, ignoreInit = TRUE)

    observeEvent(input$btn_reject_req, {
      req(is_principal())
      rid <- session$userData$reviewing_req_id %||% ""
      req(nzchar(rid))
      reason   <- trimws(input$review_rejection_reason %||% "")
      reviewer <- shared$current_user() %||% "unknown"

      reqs <- tryCatch(read_client_requests(), error = function(e) NULL)
      req(!is.null(reqs))
      idx <- which(reqs$id == rid)
      req(length(idx) == 1)
      row <- reqs[idx, , drop = FALSE]

      reqs$status[idx]           <- "rejected"
      reqs$reviewed_by[idx]      <- reviewer
      reqs$reviewed_at[idx]      <- as.character(Sys.time())
      reqs$rejection_reason[idx] <- if (nzchar(reason)) reason else NA_character_

      saved <- tryCatch({ write_client_requests(reqs); TRUE },
                        error = function(e) { message("[CLIENTS] reject save failed: ", e$message); FALSE })
      if (!saved) {
        showNotification("Error al guardar el rechazo.", type = "error"); return()
      }
      removeModal()
      requests_refresh(requests_refresh() + 1L)
      showNotification(paste0("Solicitud para '", row$client_id_proposed, "' rechazada."),
                       type = "warning", duration = 4)
      log_action(user        = reviewer,
                 module      = "clientes",
                 action      = "rechazo_cliente",
                 description = paste0("Solicitud '", row$client_id_proposed, "' rechazada",
                                      if (nzchar(reason)) paste0(" — motivo: ", reason) else ""),
                 target_id   = row$client_id_proposed,
                 s3_key      = "hd-admin/client_requests.rds",
                 client_id   = "hd-admin")
      activity_refresh(activity_refresh() + 1L)
      session$userData$reviewing_req_id <- NULL
    }, ignoreInit = TRUE)

    # ── Client edit modal (display_name, contact_email, max_users) ───────────
    pending_client_edit <- reactiveVal(NULL)   # stores validated edit before confirmation

    observeEvent(input$edit_client_id, {
      req(is_hopdesk() || is_principal())
      cid <- input$edit_client_id
      req(nzchar(cid %||% ""))
      ns <- session$ns

      registry <- tryCatch(read_client_registry(), error = function(e) NULL)
      req(!is.null(registry))
      row <- registry[registry$client_id == cid, , drop = FALSE]
      req(nrow(row) == 1)

      showModal(modalDialog(
        title  = tagList(icon("pencil"), sprintf(" Editar — %s", row$display_name[1])),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("btn_save_client_edit"), tagList(icon("floppy-disk"), " Guardar cambios"),
                       class = "btn btn-primary btn-sm")
        ),
        size = "m", easyClose = FALSE,
        textInput(ns("edit_display_name"), "Nombre del cliente",
                  value = row$display_name[1]),
        textInput(ns("edit_contact_email"), "Email de contacto",
                  value = row$contact_email[1] %||% ""),
        numericInput(ns("edit_max_users"), "Límite de usuarios",
                     value = row$max_users[1], min = 1L, max = 500L, step = 1L),
        tags$p(class = "text-muted small mt-2",
               tagList(icon("info-circle"),
                       " Si el nuevo límite es menor que los usuarios activos, se mostrará un ",
                       "panel para desactivar cuentas antes de confirmar."))
      ))
      session$userData$editing_client_id <- cid
    }, ignoreInit = TRUE)

    observeEvent(input$btn_save_client_edit, {
      req(is_hopdesk() || is_principal())
      cid <- session$userData$editing_client_id %||% ""
      req(nzchar(cid))
      ns <- session$ns

      new_dname   <- trimws(input$edit_display_name  %||% "")
      new_contact <- trimws(input$edit_contact_email %||% "")
      new_max     <- as.integer(input$edit_max_users  %||% 1L)

      if (!nzchar(new_dname)) {
        showNotification("El nombre no puede estar vacío.", type = "error"); return()
      }
      if (is.na(new_max) || new_max < 1L) {
        showNotification("El límite debe ser al menos 1.", type = "error"); return()
      }

      registry <- tryCatch(read_client_registry(), error = function(e) NULL)
      req(!is.null(registry))
      row <- registry[registry$client_id == cid, , drop = FALSE]
      req(nrow(row) == 1)
      old_max <- as.integer(row$max_users[1])
      cur     <- as.integer(row$current_users[1])

      needs_deact <- new_max < cur

      # Load active users for the deactivation panel (if needed)
      active_users <- if (needs_deact) {
        tryCatch({
          u <- auth_load_usuarios(client_id = cid)
          u[is.na(u$deleted) | u$deleted != TRUE, , drop = FALSE]
        }, error = function(e) NULL)
      } else NULL

      pending_client_edit(list(
        client_id    = cid,
        display_name = new_dname,
        contact_email = new_contact,
        new_max_users = new_max,
        old_max_users = old_max,
        current_users = cur,
        needs_deact   = needs_deact,
        active_users  = active_users,
        original_reg_row = row   # for rollback
      ))

      removeModal()

      # Build choices for deactivation checklist (values = account_code)
      deact_choices <- if (needs_deact && !is.null(active_users) && nrow(active_users)) {
        setNames(
          active_users$account_code,
          paste0(active_users$display_name %||% active_users$username,
                 " (@", active_users$username, ")")
        )
      } else NULL

      showModal(modalDialog(
        title     = tagList(icon("exclamation-triangle"), " Confirmar cambios"),
        footer    = tagList(
          modalButton("Cancelar"),
          actionButton(ns("btn_confirm_limit_change"),
                       tagList(icon("check"), " Confirmar"),
                       class = "btn btn-warning btn-sm")
        ),
        size      = "m",
        easyClose = FALSE,

        tags$dl(class = "row mb-2",
          tags$dt(class = "col-sm-5", "Cliente"),
          tags$dd(class = "col-sm-7", row$display_name[1]),
          if (new_dname != row$display_name[1]) tagList(
            tags$dt(class = "col-sm-5", "Nuevo nombre"),
            tags$dd(class = "col-sm-7", new_dname)
          ),
          if (old_max != new_max) tagList(
            tags$dt(class = "col-sm-5", "Límite de usuarios"),
            tags$dd(class = "col-sm-7",
              tagList(
                tags$span(class = "text-muted", old_max), " → ",
                tags$strong(
                  style = if (new_max < old_max) "color:#dc2626" else "color:#16a34a",
                  new_max
                )
              )
            )
          )
        ),

        # Deactivation panel — only when new limit < current users
        if (needs_deact && !is.null(deact_choices)) {
          div(class = "alert alert-warning mt-2 mb-0",
            tags$p(class = "fw-semibold mb-2",
                   tagList(icon("triangle-exclamation"),
                           sprintf(" El nuevo límite (%d) es menor que los usuarios activos (%d).",
                                   new_max, cur))),
            tags$p(class = "small mb-2",
                   "Selecciona las cuentas que se mantendrán activas. ",
                   "Las no seleccionadas quedarán desactivadas. No perderán sus datos."),
            checkboxGroupInput(
              ns("limit_confirm_keep"),
              label    = "Cuentas a mantener activas:",
              choices  = deact_choices,
              selected = active_users$account_code   # all start checked
            ),
            uiOutput(ns("limit_confirm_counter"))
          )
        }
      ))
    }, ignoreInit = TRUE)

    # Reactive counter and button gate for the deactivation checklist
    observe({
      p <- pending_client_edit()
      req(!is.null(p), isTRUE(p$needs_deact))
      keep  <- input$limit_confirm_keep %||% character(0)
      ok    <- length(keep) <= as.integer(p$new_max_users)
      shinyjs::toggleState("btn_confirm_limit_change", condition = ok)
    })

    output$limit_confirm_counter <- renderUI({
      p <- pending_client_edit()
      req(!is.null(p), isTRUE(p$needs_deact))
      keep  <- input$limit_confirm_keep %||% character(0)
      n_keep <- length(keep)
      n_max  <- as.integer(p$new_max_users)
      color  <- if (n_keep <= n_max) "text-success" else "text-danger"
      tags$p(class = paste("small fw-semibold mt-1", color),
             sprintf("Manteniendo activos: %d / %d (límite nuevo: %d)",
                     n_keep, as.integer(p$current_users), n_max))
    })

    observeEvent(input$btn_confirm_limit_change, {
      req(is_hopdesk() || is_principal())
      p <- pending_client_edit()
      req(!is.null(p))

      cid      <- p$client_id
      new_max  <- as.integer(p$new_max_users)
      old_max  <- as.integer(p$old_max_users)
      reviewer <- shared$current_user() %||% "unknown"
      now_str  <- as.character(Sys.time())

      # ── Step A: deactivate unchecked users (if needed) ────────────────────
      deact_names <- character(0)
      if (isTRUE(p$needs_deact)) {
        keep_codes <- input$limit_confirm_keep %||% character(0)
        if (length(keep_codes) > new_max) {
          showNotification(
            sprintf("Selecciona como máximo %d cuentas.", new_max), type = "error")
          return()
        }
        usuarios_orig <- tryCatch(
          auth_load_usuarios(client_id = cid), error = function(e) NULL)
        if (is.null(usuarios_orig)) {
          showNotification("No se pudo leer usuarios del cliente.", type = "error"); return()
        }
        usuarios_new  <- usuarios_orig
        active_codes  <- p$active_users$account_code
        to_deact_idx  <- which(
          usuarios_new$account_code %in% active_codes &
          !usuarios_new$account_code %in% keep_codes
        )
        if (length(to_deact_idx)) {
          usuarios_new$deleted[to_deact_idx]    <- TRUE
          usuarios_new$deleted_at[to_deact_idx] <- now_str
          usuarios_new$activo[to_deact_idx]     <- FALSE
          deact_names <- paste0(
            usuarios_new$display_name[to_deact_idx] %||%
            usuarios_new$username[to_deact_idx],
            " (@", usuarios_new$username[to_deact_idx], ")")
        }
        ok_users <- tryCatch({
          auth_save_usuarios(usuarios_new, client_id = cid); TRUE
        }, error = function(e) { message("[CLIENTS] deact save failed: ", e$message); FALSE })
        if (!ok_users) {
          showNotification("Error al desactivar usuarios. Ningún cambio guardado.",
                           type = "error"); return()
        }
        # Per-user audit entries
        for (i in seq_along(to_deact_idx)) {
          log_action(user        = reviewer,
                     module      = "clientes",
                     action      = "user_deactivated_by_limit_reduction",
                     description = paste0("Cuenta '",
                       usuarios_new$username[to_deact_idx[i]],
                       "' desactivada por reducción de límite en '", cid, "'"),
                     target_id   = usuarios_new$account_code[to_deact_idx[i]],
                     s3_key      = paste0(cid, "/usuarios.rds"),
                     client_id   = cid)
        }
      }

      # ── Step B: update registry (max_users + display_name + contact_email) ─
      registry <- tryCatch(read_client_registry(), error = function(e) NULL)
      if (is.null(registry)) {
        showNotification("No se pudo leer el registro de clientes.", type = "error"); return()
      }
      reg_idx <- which(registry$client_id == cid)
      if (!length(reg_idx)) {
        showNotification("Cliente no encontrado en el registro.", type = "error"); return()
      }
      registry$display_name[reg_idx]  <- p$display_name
      registry$contact_email[reg_idx] <- p$contact_email
      registry$max_users[reg_idx]     <- new_max
      ok_reg <- tryCatch({ write_client_registry(registry); TRUE },
                          error = function(e) { message("[CLIENTS] reg save failed: ", e$message); FALSE })
      if (!ok_reg) {
        # Rollback Step A: restore original usuarios
        if (isTRUE(p$needs_deact)) {
          tryCatch(auth_save_usuarios(usuarios_orig, client_id = cid),
                   error = function(e) message("[CLIENTS] rollback failed: ", e$message))
        }
        showNotification("Error al guardar el registro. Los cambios fueron revertidos.",
                         type = "error"); return()
      }

      # ── Step C: recompute current_users ───────────────────────────────────
      tryCatch(update_client_user_count(cid),
               error = function(e) message("[CLIENTS] count update failed: ", e$message))

      # ── Audit log (both hd-admin and client logs) ─────────────────────────
      limit_meta <- list(old_max_users = old_max, new_max_users = new_max,
                         changed_by = reviewer, deactivated_count = length(deact_names))
      log_action(user        = reviewer,
                 module      = "clientes",
                 action      = "cambio_limite_usuarios",
                 description = sprintf("Límite de '%s' cambiado de %d a %d por %s",
                                       cid, old_max, new_max, reviewer),
                 target_id   = cid,
                 s3_key      = "hd-admin/client_registry.rds",
                 client_id   = "hd-admin",
                 metadata    = limit_meta)
      log_action(user        = reviewer,
                 module      = "clientes",
                 action      = "cambio_limite_usuarios",
                 description = sprintf("Límite cambiado de %d a %d por %s",
                                       old_max, new_max, reviewer),
                 target_id   = cid,
                 s3_key      = paste0(cid, "/app_audit.rds"),
                 client_id   = cid,
                 metadata    = limit_meta)

      removeModal()
      pending_client_edit(NULL)
      clients_refresh(clients_refresh() + 1L)
      activity_refresh(activity_refresh() + 1L)
      showNotification(
        sprintf("Límite de '%s' actualizado a %d usuarios.", p$display_name, new_max),
        type = "message", duration = 4)

      # ── 3.7: notify all principal-tier accounts ───────────────────────────
      tryCatch(
        notify_limit_change_to_principals(
          client_id   = cid,
          client_name = p$display_name,
          old_limit   = old_max,
          new_limit   = new_max,
          changed_by  = reviewer,
          deactivated = deact_names
        ),
        error = function(e) message("[CLIENTS] principal notify failed: ", e$message)
      )
    }, ignoreInit = TRUE)

    # ── Contacts modal ───────────────────────────────────────────────────────
    contacts_client_rv <- reactiveVal(NULL)  # client_id whose contacts are open
    contacts_refresh   <- reactiveVal(0L)

    observeEvent(input$open_contacts_modal, {
      req(is_hopdesk() || is_principal())
      cid <- input$open_contacts_modal
      req(nzchar(cid %||% ""))
      contacts_client_rv(cid)
      contacts_refresh(contacts_refresh() + 1L)
      ns <- session$ns

      registry <- tryCatch(read_client_registry(), error = function(e) .schema_client_registry())
      row      <- registry[registry$client_id == cid, , drop = FALSE]
      dname    <- if (nrow(row)) row$display_name[1] else cid

      showModal(modalDialog(
        title  = tagList(icon("address-book"),
                         sprintf(" Contactos — %s", dname)),
        footer = modalButton("Cerrar"),
        size   = "l",
        easyClose = TRUE,

        uiOutput(ns("contacts_modal_body"))
      ))
    }, ignoreInit = TRUE)

    output$contacts_modal_body <- renderUI({
      contacts_refresh()
      cid <- contacts_client_rv()
      req(nzchar(cid %||% ""))
      ns  <- session$ns

      contacts <- tryCatch(read_contacts(cid), error = function(e) .schema_contacts())
      active   <- contacts[!is.na(contacts$active) & contacts$active == TRUE, , drop = FALSE]

      div(
        # ── Current contacts table ───────────────────────────────────────
        if (!nrow(active)) {
          div(class = "alert alert-light border mb-3",
              icon("info-circle"), " No hay contactos registrados para este cliente.")
        } else {
          div(class = "card border-0 shadow-sm mb-4",
            div(class = "card-header py-2 fw-semibold small",
                tagList(icon("users"), sprintf(" Contactos activos (%d)", nrow(active)))),
            div(class = "card-body p-0",
              tags$table(class = "table table-sm table-hover mb-0",
                tags$thead(tags$tr(
                  tags$th("Nombre"), tags$th("Email"), tags$th("Cargo"),
                  tags$th(class = "text-center", "Alertas límite"),
                  tags$th("")
                )),
                tags$tbody(
                  lapply(seq_len(nrow(active)), function(i) {
                    r <- active[i, , drop = FALSE]
                    tags$tr(
                      tags$td(tags$strong(r$name)),
                      tags$td(class = "small", r$email),
                      tags$td(class = "small text-muted", if (nzchar(r$role %||% "")) r$role else "—"),
                      tags$td(class = "text-center",
                        if (isTRUE(r$receives_limit_alerts))
                          tags$span(class = "badge bg-success", icon("check"), " Sí")
                        else
                          tags$span(class = "text-muted small", "—")
                      ),
                      tags$td(
                        tags$button(
                          class   = "btn btn-sm btn-outline-danger",
                          type    = "button",
                          title   = "Desactivar contacto",
                          onclick = sprintf(
                            "Shiny.setInputValue('%s','%s',{priority:'event'})",
                            ns("contact_deactivate_id"), r$id),
                          icon("user-minus")
                        )
                      )
                    )
                  })
                )
              )
            )
          )
        },

        # ── Add new contact form ─────────────────────────────────────────
        div(class = "card border-0 shadow-sm",
          div(class = "card-header py-2 fw-semibold small",
              style = "background:#f0f7ff;",
              tagList(icon("user-plus"), " Agregar contacto")),
          div(class = "card-body",
            fluidRow(
              column(4, textInput(ns("contact_name"),  "Nombre",
                                  placeholder = "Ej: María García")),
              column(4, textInput(ns("contact_email"), "Email",
                                  placeholder = "maria@empresa.com")),
              column(4, textInput(ns("contact_role"),  "Cargo (opcional)",
                                  placeholder = "CFO, IT Admin…"))
            ),
            fluidRow(
              column(6,
                checkboxInput(ns("contact_limit_alerts"),
                              "Recibir alertas de límite de usuarios", value = TRUE)
              ),
              column(6,
                div(style = "margin-top:26px;",
                  actionButton(ns("btn_add_contact"),
                               tagList(icon("plus"), " Agregar"),
                               class = "btn btn-primary btn-sm")
                )
              )
            )
          )
        )
      )
    })

    observeEvent(input$btn_add_contact, {
      req(is_hopdesk() || is_principal())
      cid   <- contacts_client_rv()
      req(nzchar(cid %||% ""))
      name  <- trimws(input$contact_name  %||% "")
      email <- trimws(input$contact_email %||% "")
      role  <- trimws(input$contact_role  %||% "")
      alerts <- isTRUE(input$contact_limit_alerts)

      if (!nzchar(name))  { showNotification("El nombre es obligatorio.", type = "error"); return() }
      if (!nzchar(email)) { showNotification("El email es obligatorio.", type = "error"); return() }
      if (!grepl("@", email)) { showNotification("El email no parece válido.", type = "error"); return() }

      saved <- tryCatch({
        add_contact(cid, name = name, email = email, role = role,
                    receives_limit_alerts = alerts); TRUE
      }, error = function(e) { message("[CONTACTS] add failed: ", e$message); FALSE })

      if (!saved) {
        showNotification("Error al guardar el contacto.", type = "error"); return()
      }
      contacts_refresh(contacts_refresh() + 1L)
      updateTextInput(session, "contact_name",  value = "")
      updateTextInput(session, "contact_email", value = "")
      updateTextInput(session, "contact_role",  value = "")
      showNotification(paste0("Contacto '", name, "' agregado."),
                       type = "message", duration = 3)
      log_action(user        = shared$current_user(),
                 module      = "clientes",
                 action      = "agregar_contacto",
                 description = paste0("Contacto '", name, "' (", email, ") agregado a '", cid, "'"),
                 target_id   = cid,
                 s3_key      = paste0(cid, "/contacts.rds"),
                 client_id   = "hd-admin")
      activity_refresh(activity_refresh() + 1L)
    }, ignoreInit = TRUE)

    observeEvent(input$contact_deactivate_id, {
      req(is_hopdesk() || is_principal())
      cid        <- contacts_client_rv()
      contact_id <- input$contact_deactivate_id
      req(nzchar(cid %||% ""), nzchar(contact_id %||% ""))

      contacts <- tryCatch(read_contacts(cid), error = function(e) NULL)
      req(!is.null(contacts))
      row <- contacts[contacts$id == contact_id, , drop = FALSE]
      if (!nrow(row)) return()

      saved <- tryCatch({
        deactivate_contact(cid, contact_id); TRUE
      }, error = function(e) { message("[CONTACTS] deactivate failed: ", e$message); FALSE })

      if (!saved) {
        showNotification("Error al desactivar el contacto.", type = "error"); return()
      }
      contacts_refresh(contacts_refresh() + 1L)
      showNotification(paste0("Contacto '", row$name[1], "' desactivado."),
                       type = "warning", duration = 3)
      log_action(user        = shared$current_user(),
                 module      = "clientes",
                 action      = "desactivar_contacto",
                 description = paste0("Contacto '", row$name[1], "' desactivado de '", cid, "'"),
                 target_id   = cid,
                 s3_key      = paste0(cid, "/contacts.rds"),
                 client_id   = "hd-admin")
      activity_refresh(activity_refresh() + 1L)
    }, ignoreInit = TRUE)

    # ── Invitaciones tab ─────────────────────────────────────────────────────
    invites_refresh <- reactiveVal(0L)

    output$invites_section <- renderUI({
      # dev can invite its own client's teammates — a client's own IT dept
      # doesn't need to ask Hopdesk to onboard their own team (Stage 2 Part C).
      can_invite <- isTRUE(is_hopdesk()) || isTRUE(is_principal()) || isTRUE(is_dev())
      if (!can_invite) {
        return(div(class = "alert alert-warning mt-3",
                   icon("lock"), " No tienes permiso para gestionar invitaciones."))
      }
      invites_refresh()
      invites <- tryCatch(read_pending_invites(), error = function(e) .schema_pending_invites())
      registry <- tryCatch(read_client_registry(), error = function(e) .schema_client_registry())
      active_clients <- registry[registry$status == "active", , drop = FALSE]
      client_choices <- setNames(active_clients$client_id, active_clients$display_name)

      email_q <- tryCatch(read_email_queue(), error = function(e) .schema_email_queue())
      pending_q <- email_q[!is.na(email_q$status) & email_q$status == "pending", , drop = FALSE]
      failed_q  <- email_q[!is.na(email_q$status) & email_q$status == "failed",  , drop = FALSE]

      tagList(
        div(
          class = "mt-3",
          fluidRow(
            # ── New invite form ──────────────────────────────────────────────
            column(4,
              div(
                class = "card border-0 shadow-sm mb-3",
                div(class = "card-header bg-white py-2 fw-semibold",
                    tagList(icon("envelope"), " Nueva invitación")),
                div(
                  class = "card-body",
                  textInput(ns("inv_email"),        "Email del invitado",
                            placeholder = "usuario@empresa.com"),
                  textInput(ns("inv_display_name"), "Nombre para mostrar",
                            placeholder = "Ej: Ana Martínez"),
                  selectInput(ns("inv_client_id"),  "Cliente destino",
                              choices = client_choices, width = "100%"),
                  selectInput(ns("inv_tier"), "Tier inicial",
                              choices = c("analysis", "finance", "admin"),
                              width = "100%"),
                  uiOutput(ns("inv_send_error")),
                  div(
                    class = "d-flex gap-2 mt-2",
                    actionButton(ns("btn_send_invite"), tagList(icon("paper-plane"), " Enviar"),
                                 class = "btn btn-primary btn-sm"),
                    actionButton(ns("btn_inv_refresh"), icon("rotate"),
                                 class = "btn btn-outline-secondary btn-sm")
                  )
                )
              )
            ),
            # ── Pending invites table ────────────────────────────────────────
            column(8,
              div(
                class = "card border-0 shadow-sm mb-3",
                div(class = "card-header bg-white py-2 fw-semibold",
                    tagList(icon("clock"), " Invitaciones pendientes")),
                div(
                  class = "card-body p-0",
                  if (!nrow(invites[invites$status == "pending", ])) {
                    div(class = "p-3 text-muted small", "Sin invitaciones pendientes.")
                  } else {
                    pending_inv <- invites[invites$status == "pending", , drop = FALSE]
                    tags$table(
                      class = "table table-sm table-hover mb-0",
                      tags$thead(tags$tr(
                        tags$th("Email"), tags$th("Nombre"), tags$th("Cliente"),
                        tags$th("Tier"), tags$th("Expira"), tags$th("")
                      )),
                      tags$tbody(
                        lapply(seq_len(nrow(pending_inv)), function(i) {
                          r <- pending_inv[i, ]
                          tags$tr(
                            tags$td(r$email),
                            tags$td(r$display_name),
                            tags$td(r$client_id),
                            tags$td(r$tier),
                            tags$td(substr(r$expires_at, 1, 10)),
                            tags$td(
                              tags$button(
                                class = "btn btn-outline-danger btn-sm py-0",
                                onclick = sprintf(
                                  "Shiny.setInputValue('%s','%s',{priority:'event'})",
                                  ns("inv_revoke_token"), r$token
                                ),
                                icon("xmark")
                              )
                            )
                          )
                        })
                      )
                    )
                  }
                )
              ),
              # ── Email queue panel ──────────────────────────────────────────
              if (nrow(pending_q) + nrow(failed_q) > 0) {
                div(
                  class = "card border-0 shadow-sm",
                  div(class = "card-header bg-white py-2 fw-semibold",
                      tagList(icon("triangle-exclamation", style = "color:#e65100"),
                              " Cola de emails")),
                  div(
                    class = "card-body p-0",
                    tags$table(
                      class = "table table-sm mb-0",
                      tags$thead(tags$tr(
                        tags$th("Para"), tags$th("Asunto"), tags$th("Estado"),
                        tags$th("Intentos"), tags$th("")
                      )),
                      tags$tbody(
                        lapply(seq_len(nrow(email_q[email_q$status %in% c("pending","failed"), ])), function(i) {
                          r <- email_q[email_q$status %in% c("pending","failed"), ][i, ]
                          tags$tr(
                            tags$td(r$to),
                            tags$td(r$subject),
                            tags$td(r$status),
                            tags$td(r$attempts),
                            tags$td(
                              tags$button(
                                class = "btn btn-outline-primary btn-sm py-0",
                                onclick = sprintf(
                                  "Shiny.setInputValue('%s','%s',{priority:'event'})",
                                  ns("email_retry_id"), r$id
                                ),
                                "Reintentar"
                              )
                            )
                          )
                        })
                      )
                    )
                  )
                )
              }
            )
          )
        )
      )
    })

    output$inv_send_error <- renderUI(NULL)

    observeEvent(input$btn_send_invite, {
      req(isTRUE(is_hopdesk()) || isTRUE(is_principal()))
      email_in   <- tolower(trimws(input$inv_email        %||% ""))
      dname_in   <- trimws(input$inv_display_name %||% "")
      client_in  <- input$inv_client_id
      tier_in    <- input$inv_tier %||% "analysis"

      if (!grepl("^[^@]+@[^@]+\\.[^@]+$", email_in)) {
        output$inv_send_error <- renderUI(
          tags$p(class = "text-danger small mt-1", "Email no válido."))
        return()
      }
      if (!nzchar(dname_in)) {
        output$inv_send_error <- renderUI(
          tags$p(class = "text-danger small mt-1", "El nombre es obligatorio."))
        return()
      }
      token <- tryCatch(
        create_invite(email = email_in, display_name = dname_in,
                      client_id = client_in, tier = tier_in,
                      invited_by = shared$current_user()),
        error = function(e) { message("[INVITE] create failed: ", e$message); NULL }
      )
      if (is.null(token)) {
        output$inv_send_error <- renderUI(
          tags$p(class = "text-danger small mt-1", "Error al crear la invitación."))
        return()
      }
      # Build invite URL — uses current session's base URL
      base_url <- tryCatch({
        prot <- session$clientData$url_protocol
        host <- session$clientData$url_hostname
        port <- session$clientData$url_port
        path <- session$clientData$url_pathname
        if (nzchar(port %||% "")) sprintf("%s//%s:%s%s", prot, host, port, path)
        else sprintf("%s//%s%s", prot, host, path)
      }, error = function(e) "https://your-app.shinyapps.io/app")
      invite_url <- paste0(base_url, "?invite=", token)

      registry <- tryCatch(read_client_registry(), error = function(e) .schema_client_registry())
      client_name <- {
        idx <- which(registry$client_id == client_in)
        if (length(idx)) registry$display_name[idx[1]] else client_in
      }
      html_body <- email_invite_html(dname_in, client_name, invite_url)
      send_email(to = email_in,
                 subject = paste0("Invitación a HopDesk — ", client_name),
                 html    = html_body)
      output$inv_send_error <- renderUI(NULL)
      showNotification(paste0("Invitación enviada a ", email_in), type = "message", duration = 4)
      invites_refresh(invites_refresh() + 1L)
    }, ignoreInit = TRUE)

    observeEvent(input$btn_inv_refresh, { invites_refresh(invites_refresh() + 1L) })

    observeEvent(input$inv_revoke_token, {
      token <- input$inv_revoke_token
      req(nzchar(token %||% ""))
      invites <- tryCatch(read_pending_invites(), error = function(e) NULL)
      req(!is.null(invites))
      idx <- which(invites$token == token)
      if (length(idx)) {
        invites[idx[1], "status"] <- "revoked"
        tryCatch(write_pending_invites(invites), error = function(e) NULL)
      }
      invites_refresh(invites_refresh() + 1L)
    }, ignoreInit = TRUE)

    observeEvent(input$email_retry_id, {
      queue_id <- input$email_retry_id
      req(nzchar(queue_id %||% ""))
      ok <- tryCatch(retry_queued_email(queue_id), error = function(e) FALSE)
      showNotification(
        if (ok) "Email enviado." else "Reintento fallido — revisa la clave API.",
        type = if (ok) "message" else "error", duration = 4
      )
      invites_refresh(invites_refresh() + 1L)
    }, ignoreInit = TRUE)

    # ── Notificaciones panel ─────────────────────────────────────────────────
    notif_refresh <- reactiveVal(0L)

    # ── Accesos de Salto panel ────────────────────────────────────────────────

    hop_refresh     <- reactiveVal(0L)
    dg_selected     <- reactiveVal(list())    # list of list(client_id, label) for direct grant
    hop_grace_start <- reactiveVal(NULL)      # POSIXct when 30-min grace period began, or NULL

    # Clear grace clock whenever the jump changes (new jump, or back to home)
    observeEvent(shared$jump_client_id(), {
      req(IS_ADMIN_DEPLOYMENT)
      hop_grace_start(NULL)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    # ── Grant watchdog: poll every 30 s while staff is mid-jump ─────────────────
    # Revoked  → immediate kick back home
    # Expired  → 30-minute grace, then kick
    # hop_watchdog_should_poll() makes this structurally impossible to reach
    # for a session at home — staff or client — since jump_client_id() is
    # NULL whenever there's no active jump. No more "cid != hd-admin" string
    # check standing in for that. is_principal() stays excluded there —
    # unlike hopdesk, principal jumps without ever needing a grant row, so a
    # missing grant row would otherwise look like "revoked" and kick them out
    # every 30s. is_dev() is dropped: Stage 1 made dev unable to ever set
    # jump_client_id() at all, so that exemption was already dead — this was
    # explicitly deferred to Stage 2 to clean up.
    observe({
      req(IS_ADMIN_DEPLOYMENT)
      cid <- shared$jump_client_id()
      req(hop_watchdog_should_poll(is_principal(), cid))
      invalidateLater(30000)   # reschedule; won't fire again once the jump ends

      me  <- shared$current_user() %||% ""
      now <- Sys.time()
      grants <- tryCatch(load_hop_grants(), error = function(e) NULL)
      if (is.null(grants)) return()

      my_rows <- grants[
        !is.na(grants$grantee)   & grants$grantee   == me &
        !is.na(grants$client_id) & grants$client_id == cid,
        , drop = FALSE]

      # Any non-revoked + non-expired row = still authorised
      valid_rows <- if (nrow(my_rows))
        my_rows[(is.na(my_rows$revoked) | my_rows$revoked != TRUE) &
                (is.na(my_rows$expires_at) | my_rows$expires_at > now), , drop = FALSE]
      else data.frame()

      if (nrow(valid_rows) > 0) {
        hop_grace_start(NULL)   # grant is live — clear any stale grace clock
        return()
      }

      # Determine revoked vs expired
      non_revoked <- if (nrow(my_rows))
        my_rows[is.na(my_rows$revoked) | my_rows$revoked != TRUE, , drop = FALSE]
      else data.frame()

      if (!nrow(non_revoked)) {
        # Grant was explicitly revoked (or no record at all) — kick immediately
        showNotification(
          sprintf("Tu acceso a '%s' fue revocado. Regresando al contexto de administración.", cid),
          type = "error", duration = 8
        )
        hop_grace_start(NULL)
        shared$jump_client_id(NULL)
        return()
      }

      # Grant expired but not revoked — start or continue grace period
      grace <- hop_grace_start()
      if (is.null(grace)) {
        hop_grace_start(now)
        showNotification(
          sprintf("Tu acceso a '%s' ha expirado. Tienes 30 minutos adicionales antes de ser regresado al área de administración.", cid),
          type = "warning", duration = 15
        )
      } else {
        elapsed_mins <- as.numeric(difftime(now, grace, units = "mins"))
        if (elapsed_mins >= 30) {
          showNotification(
            sprintf("Período de gracia terminado. Regresando desde '%s' al contexto de administración.", cid),
            type = "error", duration = 8
          )
          hop_grace_start(NULL)
          shared$jump_client_id(NULL)
        }
      }
    })

    .HOP_DURATIONS <- c(
      "2 horas"   = "7200",
      "6 horas"   = "21600",
      "12 horas"  = "43200",
      "24 horas"  = "86400",
      "1 semana"  = "604800",
      "1 mes"     = "2592000",
      "Sin límite" = "0"
    )

    .fmt_remaining <- function(exp_time) {
      if (length(exp_time) == 1L && is.na(exp_time)) return("Sin expiración")
      secs <- as.numeric(difftime(exp_time, Sys.time(), units = "secs"))
      if (!is.finite(secs) || secs <= 0) return("expirado")
      if (secs < 3600)  return(sprintf("%d min",  round(secs / 60)))
      if (secs < 86400) return(sprintf("%.0fh %02.0fmin", floor(secs / 3600), (secs %% 3600) / 60))
      sprintf("%d días", floor(secs / 86400))
    }

    .client_label <- function(cid, registry) {
      nm <- registry$display_name[registry$client_id == cid]
      if (length(nm) && nzchar(nm[1])) nm[1] else cid
    }

    .dur_label <- function(secs) {
      secs <- suppressWarnings(as.numeric(secs))
      if (!is.finite(secs) || secs == 0) return("Sin límite")
      lut <- c(7200, 21600, 43200, 86400, 604800, 2592000)
      nms <- c("2h", "6h", "12h", "24h", "1 semana", "1 mes")
      nms[which.min(abs(lut - secs))]
    }

    output$hop_section <- renderUI({
      can_see <- isTRUE(shared$current_user_info()$is_staff)
      if (!can_see) {
        return(div(class = "alert alert-warning mt-3",
                   icon("lock"), " Solo disponible para cuentas Hopdesk y Principal."))
      }
      hop_refresh()

      me        <- shared$current_user() %||% ""
      principal <- isTRUE(is_principal())
      reqs      <- tryCatch(load_hop_requests(), error = function(e) .schema_hop_requests())
      grants    <- tryCatch(load_hop_grants(),   error = function(e) .schema_hop_grants())
      registry  <- tryCatch(read_client_registry(), error = function(e) .schema_client_registry())
      active_reg <- registry[
        !is.na(registry$status) & registry$status == "active" &
        registry$client_id != "hd-admin", , drop = FALSE]

      header_row <- div(
        class = "d-flex align-items-center justify-content-between mt-3 mb-3",
        tags$h6(class = "fw-semibold mb-0",
                tagList(icon("key"), " Accesos de Salto")),
        actionButton(ns("btn_hop_refresh"), icon("rotate"),
                     class = "btn btn-sm btn-outline-secondary")
      )

      if (principal) {
        # ── Principal view ────────────────────────────────────────────────────
        # A request stays "pending" until every client in it is decided.
        now <- Sys.time()
        pending_reqs <- if (is.data.frame(reqs) && nrow(reqs))
          reqs[!is.na(reqs$status) & reqs$status == "pending", , drop = FALSE]
        else data.frame()
        if (nrow(pending_reqs))
          pending_reqs <- pending_reqs[order(pending_reqs$requested_at, decreasing = TRUE), , drop = FALSE]

        active_grants <- if (is.data.frame(grants) && nrow(grants))
          grants[(is.na(grants$expires_at) | grants$expires_at > now) &
                 (is.na(grants$revoked) | grants$revoked != TRUE), , drop = FALSE]
        else data.frame()

        req_cards <- if (nrow(pending_reqs)) {
          do.call(tagList, lapply(seq_len(nrow(pending_reqs)), function(ri) {
            req_row   <- pending_reqs[ri, ]
            req_id    <- req_row$id %||% ""
            clients_v <- tryCatch(jsonlite::fromJSON(req_row$clients_json %||% "[]"),
                                  error = function(e) character(0))
            decisions <- tryCatch(jsonlite::fromJSON(req_row$decisions_json %||% "{}"),
                                  error = function(e) list())
            age_secs  <- tryCatch(as.numeric(difftime(now, req_row$requested_at, units = "secs")),
                                  error = function(e) 0)
            age_lbl   <- if (age_secs < 60) "hace un momento"
                         else if (age_secs < 3600) sprintf("hace %d min", round(age_secs / 60))
                         else sprintf("hace %.0fh", age_secs / 3600)

            # One row per client: pending = duration select + buttons; decided = badge
            client_rows <- lapply(seq_along(clients_v), function(ci) {
              cid <- clients_v[[ci]]
              dec <- decisions[[cid]] %||% "pending"
              lbl <- .client_label(cid, active_reg)
              if (dec == "pending") {
                dur_id <- ns(paste0("hop_dur_", ri, "_", ci))
                div(
                  class = "d-flex align-items-center gap-2 py-2 border-bottom flex-wrap",
                  style = "font-size:.84rem;",
                  tags$span(style = "min-width:110px; font-weight:500;", lbl),
                  div(
                    style = "width:145px; flex-shrink:0;",
                    selectInput(dur_id, label = NULL, choices = .HOP_DURATIONS,
                                selected = "86400", width = "100%")
                  ),
                  tags$button(
                    class   = "btn btn-sm btn-success",
                    onclick = sprintf(
                      "var d=document.getElementById('%s').value; Shiny.setInputValue('%s','%s:%s:'+d,{priority:'event'});",
                      dur_id, ns("hop_approve"), req_id, cid),
                    tagList(icon("check"), " Aprobar")
                  ),
                  tags$button(
                    class   = "btn btn-sm btn-outline-danger",
                    onclick = sprintf(
                      "Shiny.setInputValue('%s','%s:%s',{priority:'event'});",
                      ns("hop_deny"), req_id, cid),
                    tagList(icon("xmark"), " Denegar")
                  )
                )
              } else {
                badge <- if (dec == "approved")
                  tags$span(class = "badge bg-success",
                             tagList(icon("check"), " Aprobado"))
                else
                  tags$span(class = "badge bg-danger",
                             tagList(icon("xmark"), " Denegado"))
                div(
                  class = "d-flex align-items-center gap-2 py-2 border-bottom",
                  style = "font-size:.84rem;",
                  tags$span(style = "min-width:110px; font-weight:500;", lbl),
                  badge
                )
              }
            })

            div(
              class = "card border-0 shadow-sm mb-2",
              div(
                class = "card-body py-2 px-3",
                div(
                  class = "d-flex align-items-center gap-2 mb-2",
                  tags$strong(req_row$requester_name %||% req_row$requester,
                              style = "font-size:.88rem;"),
                  tags$span(class = "text-muted", style = "font-size:.75rem;",
                            paste0("@", req_row$requester %||% "")),
                  tags$span(class = "text-muted ms-auto", style = "font-size:.73rem;", age_lbl)
                ),
                if (nzchar(req_row$message %||% ""))
                  tags$p(class = "mb-2 small fst-italic text-muted",
                         tagList(icon("comment", style = "color:#bbb"), " ",
                                 tags$q(req_row$message))),
                do.call(tagList, client_rows)
              )
            )
          }))
        } else {
          div(class = "alert alert-light border small mb-3",
              icon("inbox", class = "text-muted"), " Sin solicitudes pendientes.")
        }

        grants_table <- if (nrow(active_grants)) {
          tagList(
            tags$hr(),
            tags$p(class = "small fw-semibold text-muted mb-2",
                   sprintf("Accesos activos (%d)", nrow(active_grants))),
            div(
              style = "overflow-x:auto;",
              tags$table(
                class = "table table-sm table-hover mb-0",
                style = "font-size:.82rem;",
                tags$thead(tags$tr(
                  tags$th("Usuario"), tags$th("Cliente"), tags$th("Expira"), tags$th("")
                )),
                tags$tbody(do.call(tagList, lapply(seq_len(nrow(active_grants)), function(i) {
                  g         <- active_grants[i, ]
                  remaining <- tryCatch(.fmt_remaining(g$expires_at), error = function(e) "?")
                  tags$tr(
                    tags$td(g$grantee %||% ""),
                    tags$td(.client_label(g$client_id %||% "", active_reg)),
                    tags$td(remaining),
                    tags$td(
                      tags$button(
                        class   = "btn btn-sm btn-outline-danger py-0",
                        style   = "font-size:.74rem;",
                        onclick = sprintf(
                          "Shiny.setInputValue('%s','%s',{priority:'event'});",
                          ns("hop_revoke"), g$id %||% ""),
                        tagList(icon("ban"), " Revocar")
                      )
                    )
                  )
                })))
              )
            )
          )
        } else NULL

        # Staff list for direct grant form
        all_staff <- tryCatch(auth_load_usuarios(client_id = "hd-admin"),
                              error = function(e) NULL)
        staff_choices <- if (!is.null(all_staff) && nrow(all_staff)) {
          cands <- all_staff[
            (is.na(all_staff$deleted) | all_staff$deleted != TRUE) &
            !is.na(all_staff$tier) & all_staff$tier %in% c("hopdesk", "dev"),
            , drop = FALSE]
          if (nrow(cands))
            setNames(cands$username, cands$display_name %||% cands$username)
          else character(0)
        } else character(0)

        direct_section <- div(
          class = "card border-0 shadow-sm mt-3 mb-2",
          div(
            class = "card-body py-3 px-3",
            tags$h6(
              class = "fw-semibold mb-3",
              tagList(icon("circle-arrow-right", style = "color:#c2185b"), " Conceder acceso directo")
            ),
            if (length(staff_choices)) {
              tagList(
                div(
                  class = "row g-2 align-items-end mb-2",
                  div(
                    class = "col-auto",
                    selectInput(
                      ns("direct_grantee"),
                      label    = "Personal:",
                      choices  = c("— seleccionar —" = "", staff_choices),
                      selected = "",
                      width    = "240px"
                    )
                  )
                ),
                uiOutput(ns("direct_grant_clients"))
              )
            } else {
              div(class = "text-muted small",
                  icon("users-slash"), " Sin personal Hopdesk/Dev registrado.")
            }
          )
        )

        tagList(
          header_row,
          tags$p(class = "small fw-semibold text-muted mb-2",
                 sprintf("Solicitudes pendientes (%d)", nrow(pending_reqs))),
          req_cards,
          direct_section,
          grants_table
        )

      } else {
        # ── Staff view ────────────────────────────────────────────────────────
        now <- Sys.time()
        my_grants <- if (is.data.frame(grants) && nrow(grants))
          grants[!is.na(grants$grantee) & grants$grantee == me &
                 (is.na(grants$revoked) | grants$revoked != TRUE) &
                 (is.na(grants$expires_at) | grants$expires_at > now), , drop = FALSE]
        else data.frame()

        my_reqs <- if (is.data.frame(reqs) && nrow(reqs))
          reqs[!is.na(reqs$requester) & reqs$requester == me, , drop = FALSE]
        else data.frame()
        if (nrow(my_reqs))
          my_reqs <- my_reqs[order(my_reqs$requested_at, decreasing = TRUE), , drop = FALSE]
        grants_section <- if (nrow(my_grants)) {
          tagList(
            tags$p(class = "small fw-semibold text-muted mb-1", "Accesos activos:"),
            div(
              style = "overflow-x:auto;",
              tags$table(
                class = "table table-sm mb-3",
                style = "font-size:.83rem;",
                tags$thead(tags$tr(tags$th("Cliente"), tags$th("Expira en"))),
                tags$tbody(do.call(tagList, lapply(seq_len(nrow(my_grants)), function(i) {
                  g <- my_grants[i, ]
                  tags$tr(
                    tags$td(tagList(icon("building-user", style = "color:#1976d2"), " ",
                                    .client_label(g$client_id %||% "", active_reg))),
                    tags$td(tryCatch(.fmt_remaining(g$expires_at), error = function(e) "?"))
                  )
                })))
              )
            )
          )
        } else NULL

        client_choices <- if (nrow(active_reg))
          setNames(active_reg$client_id, active_reg$display_name)
        else character(0)

        form_section <- if (length(client_choices)) {
          div(
            class = "card border-0 bg-light mt-2",
            div(
              class = "card-body py-3 px-3",
              tags$h6(class = "fw-semibold mb-3",
                      tagList(icon("paper-plane"), " Solicitar acceso temporal")),
              tags$p(class = "text-muted small mb-2",
                     "Selecciona los clientes. El principal asignará la duración por cliente."),
              checkboxGroupInput(
                ns("hop_req_clients"),
                label    = "Clientes:",
                choices  = client_choices,
                selected = NULL,
                inline   = TRUE
              ),
              textAreaInput(
                ns("hop_req_message"),
                label       = "Razón / contexto (opcional):",
                value       = "",
                placeholder = "Ej. Revisar configuración intercompany",
                rows        = 2,
                width       = "100%"
              ),
              actionButton(
                ns("btn_hop_request_submit"),
                tagList(icon("paper-plane"), " Enviar solicitud"),
                class = "btn btn-primary btn-sm mt-1"
              )
            )
          )
        } else {
          div(class = "alert alert-warning mt-2",
              tagList(icon("triangle-exclamation"), " No hay clientes registrados disponibles."))
        }

        # History: show per-client decisions from decisions_json
        history_section <- if (nrow(my_reqs)) {
          recent <- my_reqs[seq_len(min(5L, nrow(my_reqs))), , drop = FALSE]
          tagList(
            tags$p(class = "small fw-semibold text-muted mt-3 mb-1", "Historial:"),
            do.call(tagList, lapply(seq_len(nrow(recent)), function(i) {
              r         <- recent[i, ]
              clients_v <- tryCatch(jsonlite::fromJSON(r$clients_json   %||% "[]"),
                                    error = function(e) character(0))
              decisions <- tryCatch(jsonlite::fromJSON(r$decisions_json %||% "{}"),
                                    error = function(e) list())
              overall   <- if (r$status == "pending")
                tags$span(class = "badge bg-warning text-dark me-1", "Pendiente")
              else
                tags$span(class = "badge bg-secondary me-1", "Resuelta")
              client_badges <- lapply(clients_v, function(cid) {
                lbl <- .client_label(cid, active_reg)
                dec <- decisions[[cid]] %||% "pending"
                cls <- switch(dec,
                  approved = "badge bg-success me-1",
                  denied   = "badge bg-danger me-1",
                  "badge bg-warning text-dark me-1"
                )
                tags$span(class = cls, lbl)
              })
              div(
                class = "py-2 border-bottom",
                style = "font-size:.82rem;",
                div(class = "d-flex align-items-center gap-1 mb-1",
                    overall,
                    tags$span(class = "text-muted ms-auto",
                              tryCatch(format(r$requested_at, "%d/%m/%y %H:%M",
                                             tz = "America/Mexico_City"),
                                       error = function(e) ""))),
                if (length(client_badges))
                  div(class = "d-flex flex-wrap gap-1", do.call(tagList, client_badges))
              )
            }))
          )
        } else NULL

        tagList(header_row, grants_section, form_section, history_section)
      }
    })

    observeEvent(input$btn_hop_refresh, { hop_refresh(hop_refresh() + 1L) })

    # Staff submits a request — no duration; principal decides per client
    observeEvent(input$btn_hop_request_submit, {
      req(isTRUE(shared$current_user_info()$is_staff) && !isTRUE(is_principal()))
      clients_sel <- input$hop_req_clients
      req(length(clients_sel) > 0)
      me      <- shared$current_user()           %||% ""
      me_name <- shared$current_user_info()$name %||% me
      msg_txt <- trimws(input$hop_req_message %||% "")
      req(nzchar(me))
      existing <- tryCatch(load_hop_requests(), error = function(e) .schema_hop_requests())
      init_decisions <- setNames(as.list(rep("pending", length(clients_sel))), clients_sel)
      new_req <- data.frame(
        id             = uuid::UUIDgenerate(),
        requester      = me,
        requester_name = me_name,
        clients_json   = jsonlite::toJSON(clients_sel, auto_unbox = FALSE),
        decisions_json = jsonlite::toJSON(init_decisions, auto_unbox = TRUE),
        message        = msg_txt,
        requested_at   = Sys.time(),
        status         = "pending",
        stringsAsFactors = FALSE
      )
      tryCatch({
        save_hop_requests(rbind(existing, new_req))
        append_hd_notification(
          type         = "hop_request",
          client_id    = "hd-admin",
          message_text = sprintf("%s solicita acceso a: %s",
                                 me_name, paste(clients_sel, collapse = ", ")),
          metadata     = list(requester  = me,
                              request_id = new_req$id,
                              clients    = clients_sel,
                              message    = msg_txt)
        )
        showNotification("Solicitud enviada.", type = "message", duration = 3)
        hop_refresh(hop_refresh() + 1L)
      }, error = function(e)
        showNotification(paste("Error:", e$message), type = "error", duration = 5))
    }, ignoreInit = TRUE)

    # Principal approves one client within a request
    # event value: "request_id:client_id:duration_secs"
    observeEvent(input$hop_approve, {
      req(isTRUE(is_principal()))
      parts    <- strsplit(input$hop_approve, ":", fixed = TRUE)[[1]]
      req(length(parts) >= 3)
      req_id   <- parts[1]
      cid      <- parts[2]
      req(nzchar(req_id), nzchar(cid))
      dur_secs <- tryCatch(as.numeric(parts[3]), error = function(e) 86400)
      if (!is.finite(dur_secs) || dur_secs < 0) dur_secs <- 86400
      me <- shared$current_user() %||% ""
      tryCatch({
        reqs    <- load_hop_requests()
        ri      <- which(reqs$id == req_id & reqs$status == "pending")
        req(length(ri) > 0)
        row     <- reqs[ri[1], ]
        clients_v <- tryCatch(jsonlite::fromJSON(row$clients_json %||% "[]"),
                              error = function(e) character(0))
        req(cid %in% clients_v)
        decisions        <- tryCatch(jsonlite::fromJSON(row$decisions_json %||% "{}"),
                                     error = function(e) list())
        decisions[[cid]] <- "approved"
        reqs$decisions_json[ri[1]] <- jsonlite::toJSON(decisions, auto_unbox = TRUE)
        # Create grant for this client
        now    <- Sys.time()
        grants <- load_hop_grants()
        new_g  <- data.frame(
          id         = uuid::UUIDgenerate(),
          request_id = req_id,
          grantee    = row$requester,
          client_id  = cid,
          granted_by = me,
          granted_at = now,
          expires_at = if (dur_secs == 0) as.POSIXct(NA) else now + dur_secs,
          revoked    = FALSE,
          stringsAsFactors = FALSE
        )
        updated_grants <- rbind(grants, new_g)
        save_hop_grants(updated_grants)
        shared$hop_grants_db(updated_grants)
        # Resolve request when all clients decided
        all_done <- all(vapply(clients_v,
          function(c) { d <- decisions[[c]] %||% "pending"; d != "pending" }, logical(1)))
        if (all_done) {
          reqs$status[ri[1]] <- "resolved"
          n_ok  <- sum(vapply(clients_v, function(c) identical(decisions[[c]], "approved"), logical(1)))
          n_no  <- sum(vapply(clients_v, function(c) identical(decisions[[c]], "denied"),   logical(1)))
          append_hd_notification(
            type         = "hop_resolved",
            client_id    = "hd-admin",
            message_text = sprintf("Solicitud de %s resuelta: %d aprobado(s), %d denegado(s).",
                                   row$requester_name %||% row$requester, n_ok, n_no),
            metadata     = list(request_id = req_id, requester = row$requester,
                                decisions  = decisions)
          )
        }
        save_hop_requests(reqs)
        showNotification(
          sprintf("Aprobado: %s → %s (%s).",
                  row$requester_name %||% row$requester, cid, .dur_label(dur_secs)),
          type = "message", duration = 3
        )
        hop_refresh(hop_refresh() + 1L)
      }, error = function(e)
        showNotification(paste("Error al aprobar:", e$message), type = "error", duration = 5))
    }, ignoreInit = TRUE)

    # Principal denies one client within a request
    # event value: "request_id:client_id"
    observeEvent(input$hop_deny, {
      req(isTRUE(is_principal()))
      parts <- strsplit(input$hop_deny, ":", fixed = TRUE)[[1]]
      req(length(parts) >= 2)
      req_id <- parts[1]
      cid    <- parts[2]
      req(nzchar(req_id), nzchar(cid))
      tryCatch({
        reqs    <- load_hop_requests()
        ri      <- which(reqs$id == req_id & reqs$status == "pending")
        req(length(ri) > 0)
        row     <- reqs[ri[1], ]
        clients_v <- tryCatch(jsonlite::fromJSON(row$clients_json %||% "[]"),
                              error = function(e) character(0))
        req(cid %in% clients_v)
        decisions        <- tryCatch(jsonlite::fromJSON(row$decisions_json %||% "{}"),
                                     error = function(e) list())
        decisions[[cid]] <- "denied"
        reqs$decisions_json[ri[1]] <- jsonlite::toJSON(decisions, auto_unbox = TRUE)
        all_done <- all(vapply(clients_v,
          function(c) { d <- decisions[[c]] %||% "pending"; d != "pending" }, logical(1)))
        if (all_done) {
          reqs$status[ri[1]] <- "resolved"
          n_ok  <- sum(vapply(clients_v, function(c) identical(decisions[[c]], "approved"), logical(1)))
          n_no  <- sum(vapply(clients_v, function(c) identical(decisions[[c]], "denied"),   logical(1)))
          append_hd_notification(
            type         = "hop_resolved",
            client_id    = "hd-admin",
            message_text = sprintf("Solicitud de %s resuelta: %d aprobado(s), %d denegado(s).",
                                   row$requester_name %||% row$requester, n_ok, n_no),
            metadata     = list(request_id = req_id, requester = row$requester,
                                decisions  = decisions)
          )
        }
        save_hop_requests(reqs)
        showNotification(sprintf("Denegado: %s → %s.", row$requester_name %||% row$requester, cid),
                         type = "warning", duration = 3)
        hop_refresh(hop_refresh() + 1L)
      }, error = function(e)
        showNotification(paste("Error al denegar:", e$message), type = "error", duration = 5))
    }, ignoreInit = TRUE)

    # Principal revokes an active grant
    observeEvent(input$hop_revoke, {
      req(isTRUE(is_principal()))
      grant_id <- input$hop_revoke
      req(nzchar(grant_id %||% ""))
      tryCatch({
        grants <- load_hop_grants()
        idx    <- which(grants$id == grant_id)
        req(length(idx) > 0)
        grants$revoked[idx[1]] <- TRUE
        save_hop_grants(grants)
        shared$hop_grants_db(grants)
        showNotification("Acceso revocado.", type = "warning", duration = 3)
        hop_refresh(hop_refresh() + 1L)
      }, error = function(e)
        showNotification(paste("Error al revocar:", e$message), type = "error", duration = 5))
    }, ignoreInit = TRUE)

    # ── Direct grant: client list renderUI ───────────────────────────────────

    # Reset selection when grantee changes
    observeEvent(input$direct_grantee, { dg_selected(list()) }, ignoreInit = TRUE)

    output$direct_grant_clients <- renderUI({
      req(isTRUE(is_principal()))
      grantee <- input$direct_grantee
      req(nzchar(grantee %||% ""))

      selected <- dg_selected()

      registry   <- tryCatch(read_client_registry(), error = function(e) .schema_client_registry())
      active_reg <- registry[
        !is.na(registry$status) & registry$status == "active" &
        registry$client_id != "hd-admin", , drop = FALSE]

      if (!nrow(active_reg))
        return(div(class = "text-muted small", "No hay clientes activos."))

      selected_ids <- vapply(selected, `[[`, character(1), "client_id")
      avail_reg    <- active_reg[!active_reg$client_id %in% selected_ids, , drop = FALSE]
      avail_choices <- if (nrow(avail_reg))
        setNames(
          avail_reg$client_id,
          ifelse(is.na(avail_reg$display_name) | !nzchar(avail_reg$display_name %||% ""),
                 avail_reg$client_id, avail_reg$display_name)
        )
      else character(0)

      search_row <- div(
        class = "d-flex align-items-end gap-2 mb-2 mt-1",
        div(
          style = "flex:1; max-width:280px;",
          selectizeInput(
            ns("dg_client_search"),
            label   = NULL,
            choices = c("— buscar cliente —" = "", avail_choices),
            selected = "",
            options = list(placeholder = "Buscar cliente…"),
            width   = "100%"
          )
        ),
        actionButton(
          ns("dg_add_client"),
          tagList(icon("plus"), " Agregar"),
          class = "btn btn-sm btn-outline-secondary"
        )
      )

      added_rows <- if (length(selected)) {
        lapply(selected, function(x) {
          div(
            class = "d-flex align-items-center gap-2 py-1",
            style = "font-size:.84rem;",
            tags$span(style = "min-width:140px; font-weight:500;", x$label),
            div(
              style = "width:160px;",
              selectInput(
                ns(paste0("dg_dur_", x$client_id)),
                label    = NULL,
                choices  = .HOP_DURATIONS,
                selected = "86400",
                width    = "100%"
              )
            ),
            tags$button(
              class   = "btn btn-sm btn-outline-danger py-0 px-2",
              style   = "font-size:.74rem; line-height:1.8;",
              onclick = sprintf(
                "Shiny.setInputValue('%s','%s',{priority:'event'});",
                ns("dg_remove_client"), x$client_id
              ),
              icon("xmark")
            )
          )
        })
      } else list()

      tagList(
        tags$p(class = "text-muted small mb-1",
               sprintf("Agregar clientes para %s:", grantee)),
        search_row,
        if (length(added_rows))
          div(class = "mt-1 mb-1", do.call(tagList, added_rows))
        else
          div(class = "text-muted small fst-italic mb-1", "Sin clientes agregados."),
        if (length(selected))
          actionButton(
            ns("btn_direct_grant"),
            tagList(icon("circle-arrow-right"), " Conceder acceso"),
            class = "btn btn-sm btn-primary mt-2"
          )
      )
    })

    observeEvent(input$dg_add_client, {
      req(isTRUE(is_principal()))
      cid <- input$dg_client_search
      req(nzchar(cid %||% ""))
      current <- dg_selected()
      if (!any(vapply(current, function(x) x$client_id == cid, logical(1)))) {
        registry <- tryCatch(read_client_registry(), error = function(e) .schema_client_registry())
        lbl_v <- registry$display_name[registry$client_id == cid]
        lbl   <- if (length(lbl_v) && nzchar(lbl_v[1] %||% "")) lbl_v[1] else cid
        dg_selected(c(current, list(list(client_id = cid, label = lbl))))
      }
    }, ignoreInit = TRUE)

    observeEvent(input$dg_remove_client, {
      req(isTRUE(is_principal()))
      cid <- input$dg_remove_client
      req(nzchar(cid %||% ""))
      dg_selected(Filter(function(x) x$client_id != cid, dg_selected()))
    }, ignoreInit = TRUE)

    observeEvent(input$btn_direct_grant, {
      req(isTRUE(is_principal()))
      grantee  <- input$direct_grantee
      req(nzchar(grantee %||% ""))
      selected <- dg_selected()
      req(length(selected) > 0)

      to_grant <- Filter(Negate(is.null), lapply(selected, function(x) {
        dur_val <- input[[paste0("dg_dur_", x$client_id)]]
        if (is.null(dur_val)) return(NULL)
        list(
          client_id = x$client_id,
          label     = x$label,
          dur_secs  = { s <- tryCatch(as.numeric(dur_val), error = function(e) 86400); if (!is.finite(s) || s < 0) 86400 else s }
        )
      }))

      if (!length(to_grant)) {
        showNotification("Sin clientes para conceder.", type = "warning", duration = 3)
        return()
      }

      me  <- shared$current_user() %||% ""
      now <- Sys.time()
      tryCatch({
        grants   <- load_hop_grants()
        new_rows <- do.call(rbind, lapply(to_grant, function(g) {
          data.frame(
            id         = uuid::UUIDgenerate(),
            request_id = NA_character_,
            grantee    = grantee,
            client_id  = g$client_id,
            granted_by = me,
            granted_at = now,
            expires_at = if (g$dur_secs == 0) as.POSIXct(NA) else now + g$dur_secs,
            revoked    = FALSE,
            stringsAsFactors = FALSE
          )
        }))
        updated <- rbind(grants, new_rows)
        save_hop_grants(updated)
        shared$hop_grants_db(updated)
        labels <- paste(vapply(to_grant, `[[`, character(1), "label"), collapse = ", ")
        append_hd_notification(
          type         = "hop_resolved",
          client_id    = "hd-admin",
          message_text = sprintf("Acceso directo concedido a %s en: %s.", grantee, labels),
          metadata     = list(grantee = grantee, clients = labels, granted_by = me)
        )
        showNotification(
          sprintf("Acceso concedido a %s (%d cliente(s)).", grantee, length(to_grant)),
          type = "message", duration = 4
        )
        dg_selected(list())
        hop_refresh(hop_refresh() + 1L)
      }, error = function(e)
        showNotification(paste("Error al conceder:", e$message), type = "error", duration = 5))
    }, ignoreInit = TRUE)

    # ── Notificaciones panel ──────────────────────────────────────────────────

    output$notifications_section <- renderUI({
      can_see <- isTRUE(is_hopdesk()) || isTRUE(is_principal())
      if (!can_see) {
        return(div(class = "alert alert-warning mt-3",
                   icon("lock"), " Solo disponible para cuentas Hopdesk y Principal."))
      }
      notif_refresh()
      notifs <- tryCatch(read_hd_notifications(), error = function(e) NULL)

      if (is.null(notifs) || !nrow(notifs)) {
        return(div(class = "mt-3 alert alert-light border",
                   icon("bell-slash", class = "text-muted"),
                   " Sin notificaciones."))
      }

      # Newest first
      if ("created_at" %in% names(notifs)) {
        notifs <- notifs[order(notifs$created_at, decreasing = TRUE), , drop = FALSE]
      }

      viewer <- shared$current_user() %||% ""

      tagList(
        div(
          class = "d-flex align-items-center justify-content-between mt-3 mb-2",
          tags$h6(class = "fw-semibold mb-0",
                  tagList(icon("bell"), " Notificaciones del sistema")),
          div(
            class = "d-flex gap-2",
            actionButton(ns("btn_mark_all_read"), tagList(icon("check-double"), " Marcar todas"),
                         class = "btn btn-sm btn-outline-secondary"),
            actionButton(ns("btn_notif_refresh"), icon("rotate"),
                         class = "btn btn-sm btn-outline-secondary")
          )
        ),
        div(
          class = "d-flex flex-column gap-2",
          lapply(seq_len(min(nrow(notifs), 50L)), function(i) {
            n    <- notifs[i, ]
            read_by <- tryCatch(jsonlite::fromJSON(n$read_by %||% "[]"),
                                error = function(e) character(0))
            is_read <- viewer %in% read_by
            type_icon <- switch(n$type %||% "",
              user_limit_warning = icon("triangle-exclamation", style = "color:#e65100"),
              user_limit_reached = icon("circle-xmark",         style = "color:#dc2626"),
              user_request       = icon("user-plus",            style = "color:#1565c0"),
              cambio_limite      = icon("sliders",              style = "color:#6a1b9a"),
              hop_request        = icon("key",                  style = "color:#c2185b"),
              hop_resolved       = icon("key",                  style = "color:#2e7d32"),
              icon("bell",                                       style = "color:#555")
            )
            div(
              class = paste("card border-0 shadow-sm",
                            if (is_read) "opacity-60" else ""),
              div(
                class = "card-body py-2 px-3 d-flex align-items-start gap-3",
                div(class = "mt-1", type_icon),
                div(
                  class = "flex-grow-1",
                  tags$p(class = "mb-0 small", n$message %||% ""),
                  tags$span(
                    class = "text-muted",
                    style = "font-size:.73rem;",
                    format(as.POSIXct(n$created_at %||% "", tz = "UTC"),
                           "%d %b %Y %H:%M", tz = "America/Mexico_City")
                  )
                ),
                if (!is_read)
                  tags$button(
                    class   = "btn btn-sm btn-link py-0 text-muted",
                    style   = "font-size:.75rem; white-space:nowrap;",
                    onclick = sprintf(
                      "Shiny.setInputValue('%s','%s',{priority:'event'})",
                      ns("mark_notif_read"), n$id),
                    "Marcar leída"
                  )
              )
            )
          })
        )
      )
    })

    observeEvent(input$btn_notif_refresh, { notif_refresh(notif_refresh() + 1L) })

    observeEvent(input$mark_notif_read, {
      nid    <- input$mark_notif_read
      viewer <- shared$current_user() %||% ""
      req(nzchar(nid), nzchar(viewer))
      tryCatch({
        notifs <- read_hd_notifications()
        idx <- which(notifs$id == nid)
        if (!length(idx)) return()
        existing <- tryCatch(jsonlite::fromJSON(notifs$read_by[idx[1]] %||% "[]"),
                             error = function(e) character(0))
        if (!viewer %in% existing) {
          notifs$read_by[idx[1]] <- jsonlite::toJSON(c(existing, viewer),
                                                      auto_unbox = FALSE)
          write_hd_notifications(notifs)
        }
        notif_refresh(notif_refresh() + 1L)
      }, error = function(e) message("[NOTIF] mark read failed: ", e$message))
    }, ignoreInit = TRUE)

    observeEvent(input$btn_mark_all_read, {
      viewer <- shared$current_user() %||% ""
      req(nzchar(viewer))
      tryCatch({
        notifs <- read_hd_notifications()
        for (i in seq_len(nrow(notifs))) {
          existing <- tryCatch(jsonlite::fromJSON(notifs$read_by[i] %||% "[]"),
                               error = function(e) character(0))
          if (!viewer %in% existing)
            notifs$read_by[i] <- jsonlite::toJSON(c(existing, viewer),
                                                   auto_unbox = FALSE)
        }
        write_hd_notifications(notifs)
        notif_refresh(notif_refresh() + 1L)
      }, error = function(e) message("[NOTIF] mark-all failed: ", e$message))
    }, ignoreInit = TRUE)

    # ── Bitácora Global panel (Stage 4 Part A/B/C) ────────────────────────────
    # Gate is can_view_client_audit_logs (renamed/split from the old
    # can_view_global_audit — see ARCHITECTURE.md §6 / Stage 4 Part A). This
    # fixes gap #2: a plain hopdesk session now opens this tab by default and
    # sees a client selector instead of being locked out entirely.
    global_audit_allowed_ids <- reactive({
      registry <- tryCatch(read_client_registry(), error = function(e) .schema_client_registry())
      active   <- registry[registry$status == "active", , drop = FALSE]
      setNames(active$client_id, active$display_name)
    })

    # "Hopdesk (interno)" is only ever offered to a viewer with
    # can_view_staff_audit_log (principal) — absent from choices entirely for
    # everyone else, not just gated on selection.
    global_audit_include_staff <- reactive({
      isTRUE(.audit_viewer_resolve_perms(shared)$can_view_staff_audit_log)
    })

    output$global_audit_section <- renderUI({
      perms   <- .audit_viewer_resolve_perms(shared)
      can_see <- isTRUE(is_principal()) || isTRUE(perms$can_view_client_audit_logs)
      if (!can_see) {
        return(div(class = "alert alert-warning mt-3",
                   icon("lock"),
                   " Requiere permiso ", tags$code("can_view_client_audit_logs"), "."))
      }
      auditLogViewerUI(ns("global_viewer"))
    })

    auditLogViewerServer(
      "global_viewer",
      shared             = shared,
      mode               = "multi_client",
      allowed_client_ids = global_audit_allowed_ids,
      include_staff_log  = global_audit_include_staff
    )

    output$tier_config_section <- renderUI({
      dev <- isTRUE(is_dev()) || isTRUE(is_hopdesk()) || isTRUE(is_principal())
      if (!dev) {
        return(div(class = "alert alert-warning mt-3",
                   icon("lock"), " Esta sección solo está disponible para Dev y Hopdesk."))
      }

      locked    <- tier_config_locked()
      sel_tier  <- selected_tier_rv()
      tier_keys <- names(TIER_META)
      cur_idx   <- match(sel_tier, tier_keys)
      has_prev  <- cur_idx > 1
      has_next  <- cur_idx < length(tier_keys)
      defaults  <- get_tier_defaults(sel_tier)

      perm_rows <- lapply(names(PERM_LABELS), function(k) {
        is_sys_locked <- identical(sel_tier, "dev") && k == "can_view_tiers"
        val           <- if (is_sys_locked) TRUE else isTRUE(defaults[[k]])
        is_disabled   <- locked || is_sys_locked

        cb <- if (is_disabled) {
          tags$div(class = "form-check",
                   tags$input(type = "checkbox", class = "form-check-input",
                              checked  = if (val) NA else NULL,
                              disabled = NA,
                              style    = if (is_sys_locked)
                                           "cursor:not-allowed; opacity:.5;"
                                         else "opacity:.55;"))
        } else {
          checkboxInput(ns(paste0("cfg_", sel_tier, "_", k)), label = NULL, value = val)
        }

        div(
          class = "tiers-cfg-row",
          style = "display:flex; align-items:center; gap:8px; padding:7px 14px; border-bottom:1px solid #f0f0f0;",
          cb,
          tags$span(PERM_LABELS[[k]], style = "font-size:.85rem; flex-grow:1;"),
          if (is_sys_locked)
            tags$span(style = "color:#198754; font-size:.65rem; white-space:nowrap;",
                      icon("lock", class = "fa-xs"), " obligatorio")
        )
      })

      nav_btn_style <- paste0(
        "background:rgba(255,255,255,.18); border:1px solid rgba(255,255,255,.35);",
        " color:#fff; padding:4px 12px; border-radius:6px; line-height:1;"
      )

      tryCatch({
        div(
          class = "mt-3",
          div(
            class = "alert alert-info border-0 small mb-3",
            icon("circle-info"),
            " Aquí defines los permisos que cada tier tiene ",
            tags$strong("por defecto"),
            ". Los overrides individuales por usuario se aplican encima."
          ),
          div(
            class = "card border-0 shadow",
            div(
              class = "card-header d-flex align-items-center justify-content-center gap-2 py-2 px-3",
              style = sprintf(
                "background:%s; color:#fff; border-radius:8px 8px 0 0;",
                .tier_color(sel_tier)
              ),
              if (has_prev)
                actionButton(ns("btn_tier_prev"), icon("chevron-left"),
                             class = "btn btn-sm", style = nav_btn_style)
              else
                tags$span(style = "width:34px; display:inline-block;"),
              div(
                class = "d-flex flex-column align-items-center px-2",
                tags$span(
                  style = "font-size:.65rem; opacity:.7; text-transform:uppercase; letter-spacing:.8px;",
                  "Tier"
                ),
                tags$span(class = "fw-bold fs-6", TIER_META[[sel_tier]])
              ),
              if (has_next)
                actionButton(ns("btn_tier_next"), icon("chevron-right"),
                             class = "btn btn-sm", style = nav_btn_style)
              else
                tags$span(style = "width:34px; display:inline-block;")
            ),
            div(class = "card-body p-0 tiers-cfg-body", perm_rows)
          ),
          div(
            class = "d-flex justify-content-end mt-3",
            if (locked)
              tags$span(
                class = "text-muted small fst-italic",
                style = "line-height:2;",
                icon("lock"),
                " Haz clic en «Editar configuración» para habilitar cambios."
              )
            else
              actionButton(ns("btn_save_tier_config"),
                           tagList(icon("floppy-disk"), " Guardar"),
                           class = "btn btn-primary btn-sm fw-semibold")
          )
        )
      }, error = function(e) {
        div(class = "alert alert-danger mt-3",
            icon("triangle-exclamation"),
            " Error al renderizar la configuración de tiers: ",
            tags$code(conditionMessage(e)),
            tags$br(),
            tags$small(class = "text-muted",
                       "Intenta recargar la página. Si persiste, puede haber un problema con tiers_config.rds en S3."))
      })
    })

    # ── Actividad (audit log viewer — Stage 4 Part B/C shared module) ────────
    # req_authorized() (dev/hopdesk/principal) is unchanged — out of scope for
    # Stage 4 to touch who *within* a client can see their own Actividad tab.
    # The staff-at-home-sees-hd-admin-log leak (Stage 4 Part A gap #1) is fixed
    # inside auditLogViewerServer()'s read_audit_log_scoped() call, not here.
    output$activity_section <- renderUI({
      req(req_authorized())
      auditLogViewerUI(ns("activity_viewer"))
    })

    auditLogViewerServer(
      "activity_viewer",
      shared = c(shared, list(refresh_signal = activity_refresh)),
      mode   = "own_client"
    )

    observeEvent(input$btn_save_tier_config, {
      req(is_dev() || is_hopdesk() || is_principal())
      tk <- selected_tier_rv()

      full_config       <- tier_config_rv() %||% list()
      full_config[[tk]] <- lapply(setNames(names(PERM_LABELS), names(PERM_LABELS)), function(k) {
        if (identical(tk, "dev") && k == "can_view_tiers") return(TRUE)
        isTRUE(input[[paste0("cfg_", tk, "_", k)]])
      })

      tryCatch({
        .s3_write(full_config, S3_KEYS$tiers_config)
        tier_config_rv(full_config)
        tier_config_locked(TRUE)
        showNotification(
          paste0("Configuración de «", TIER_META[[tk]], "» guardada."),
          type = "message", duration = 2)
      }, error = function(e) {
        showNotification(paste0("Error al guardar: ", e$message), type = "error")
      })
    })

  })
}
