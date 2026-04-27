# =============================================================================
# R/tiers_module.R
# Panel "Usuarios" — gestión de cuentas, permisos y configuración de tiers.
# Visible para dev y admin. Dev ve todo (incluyendo otras cuentas dev).
# Admin ve solo cuentas no-dev y no puede asignar tier dev.
# La sección "Config. de Tiers" es exclusiva para dev.
# =============================================================================

PERM_LABELS <- c(
  can_view_cobros      = "Ver Cobros (CxC)",
  can_view_pagos       = "Ver Pagos (CxP)",
  can_view_agenda      = "Ver Agenda de Hoy",
  can_view_bancos      = "Ver Bancos",
  can_view_reportes    = "Ver y exportar Reportes",
  can_edit_invoices    = "Editar facturas (mover, notas)",
  can_move_invoices    = "Mover fechas de facturas",
  can_manage_tags      = "Gestionar etiquetas (Urgente/Importante)",
  can_export_pdf       = "Exportar PDF",
  can_manage_providers = "Gestionar proveedores",
  can_manage_users     = "Gestionar usuarios",
  can_view_tiers       = "Acceso al panel Usuarios (solo dev)",
  can_manage_empresas  = "Gestionar empresas del grupo"
)

.TIER_COLORS <- c(dev = "#0d1b3e", admin = "#6610f2", finance = "#0a58ca", analysis = "#6c757d")
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

    bslib::navset_pill(
      id = ns("main_tab"),

      # ── Tab: Usuarios ──────────────────────────────────────────────────────
      bslib::nav_panel(
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
                  actionButton(ns("btn_new_user"),
                               tagList(icon("user-plus"), " Nueva cuenta"),
                               class = "btn btn-sm btn-outline-primary")
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
                                  placeholder = "Ej: Tesorer\u00eda")
                      ),
                      column(6,
                        textInput(ns("new_username"), "Nombre de usuario",
                                  placeholder = "Ej: tesoreria")
                      )
                    ),
                    fluidRow(
                      column(6, passwordInput(ns("new_password"),  "Contrase\u00f1a")),
                      column(6, passwordInput(ns("new_password2"), "Confirmar contrase\u00f1a"))
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
            "Un JSON vac\u00edo significa que el usuario tiene exactamente los permisos de su tier."
          )
        )
      ),

      # ── Tab: Config. de Tiers (dev only) ──────────────────────────────────
      bslib::nav_panel(
        title = tagList(icon("sliders"), " Config. de Tiers"),
        value = "tier_config",
        uiOutput(ns("tier_config_section"))
      ),

      # Lock/unlock toggle — always visible in tab bar, only affects Config tab
      bslib::nav_spacer(),
      bslib::nav_item(uiOutput(ns("cfg_lock_btn")))
    )
  )
}

# ── Permission editor panel ────────────────────────────────────────────────────

.tiers_perm_editor_ui <- function(ns, row, viewer_tier = "dev") {
  tier <- row$tier %||% "finance"

  current_perms <- auth_resolve_perms(tier, row$permisos %||% "{}")
  overrides     <- tryCatch(jsonlite::fromJSON(row$permisos %||% "{}"),
                            error = function(e) list())

  tier_choices <- if (identical(viewer_tier, "dev")) {
    c("Dev (control total)" = "dev", "Admin" = "admin",
      "Finance" = "finance", "Analysis (solo lectura)" = "analysis")
  } else {
    c("Admin" = "admin", "Finance" = "finance", "Analysis (solo lectura)" = "analysis")
  }

  is_dev_tier <- identical(tier, "dev")

  perm_rows <- lapply(names(PERM_LABELS), function(k) {
    is_locked   <- is_dev_tier && k == "can_view_tiers"
    is_override <- k %in% names(overrides)
    current_val <- if (is_locked) TRUE else isTRUE(current_perms[[k]])

    badge <- if (is_locked) {
      tags$span(class = "tiers-perm-override", style = "color:#198754;",
                icon("lock", class = "fa-xs"), " obligatorio")
    } else if (is_override) {
      tags$span(class = "tiers-perm-override", "override")
    } else {
      tags$span(class = "tiers-perm-default", "default (tier)")
    }

    cb <- if (is_locked) {
      tags$div(class = "form-check",
               tags$input(type = "checkbox", class = "form-check-input",
                          checked = NA, disabled = NA,
                          style = "cursor:not-allowed; opacity:.6;"))
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
      div(class = "mb-3",
          tags$label("Tier", class = "form-label small fw-semibold mb-1"),
          selectInput(ns("edit_tier"), NULL,
                      choices = tier_choices, selected = tier, width = "100%")),
      tags$h6(class = "fw-semibold mb-2 small text-muted text-uppercase", "Permisos granulares"),
      div(class = "border rounded", perm_rows),
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

    # Usuarios panel is dev-only — admins access Empresas, not user accounts
    req_authorized <- reactive({ identical(current_tier(), "dev") })
    is_dev         <- reactive({ identical(current_tier(), "dev") })

    # ── Header badge ──────────────────────────────────────────────────────────
    output$header_badge <- renderUI({
      tier <- current_tier()
      if (!nzchar(tier)) return(NULL)
      tags$span(
        class = "ms-auto tiers-dev-badge",
        toupper(tier)
      )
    })

    # ── Config lock state ─────────────────────────────────────────────────────
    tier_config_locked <- reactiveVal(TRUE)

    output$cfg_lock_btn <- renderUI({
      if (!is_dev()) return(NULL)
      if (!identical(input$main_tab, "tier_config")) return(NULL)
      locked <- tier_config_locked()
      if (locked) {
        actionButton(ns("btn_toggle_lock"),
                     tagList(icon("lock"), " Editar"),
                     class = "btn btn-warning",
                     style = "font-size:.85rem; padding:5px 16px; font-weight:600;")
      } else {
        actionButton(ns("btn_toggle_lock"),
                     tagList(icon("lock-open"), " Bloquear"),
                     class = "btn btn-success",
                     style = "font-size:.85rem; padding:5px 16px; font-weight:600;")
      }
    })

    observeEvent(input$btn_toggle_lock, {
      tier_config_locked(!tier_config_locked())
    })

    # ── Tier config (S3) ──────────────────────────────────────────────────────
    # Stores custom tier defaults; NULL = use hardcoded auth_resolve_perms defaults
    tier_config_rv <- reactiveVal(NULL)

    observe({
      req(is_dev())
      cfg <- tryCatch(.s3_read("tiers_config.rds"), error = function(e) NULL)
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
      auth_load_usuarios()
    })

    # Filtered list shown in the table (excludes soft-deleted and tier-filtered)
    usuarios_rv <- reactive({
      df <- all_usuarios_rv()
      req(!is.null(df))
      # Exclude soft-deleted accounts from display
      if (nrow(df) > 0 && "deleted" %in% names(df))
        df <- df[is.na(df$deleted) | df$deleted != TRUE, , drop = FALSE]
      # Admin users cannot see dev-tier accounts
      if (!is_dev() && nrow(df) > 0) df <- df[df$tier != "dev", , drop = FALSE]
      df
    })

    # ── Users table ───────────────────────────────────────────────────────────
    output$usuarios_tbl <- DT::renderDataTable({
      req(req_authorized())
      usuarios <- usuarios_rv()
      req(!is.null(usuarios) && nrow(usuarios) > 0)

      edit_input_id <- ns("trs_edit_click")
      del_input_id  <- ns("trs_del_click")

      df <- data.frame(
        `#`     = usuarios$account_code,
        Nombre  = usuarios$display_name,
        Usuario = usuarios$username,
        Tier    = usuarios$tier,
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
        stringsAsFactors = FALSE
      )

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
      choices <- if (is_dev()) {
        c("Dev (control total)" = "dev", "Admin" = "admin",
          "Finance" = "finance", "Analysis (solo lectura)" = "analysis")
      } else {
        c("Admin" = "admin", "Finance" = "finance", "Analysis (solo lectura)" = "analysis")
      }
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
      # ── HARD WALL: only dev-tier users can create dev-tier accounts ───────────
      # Three layers:
      # 1. req() silently aborts the entire handler if a non-dev somehow submits dev tier
      # 2. Force-override: coerce tier to "admin" for non-dev users regardless of input value
      # 3. The selectInput in new_tier_ui already excludes "dev" from non-dev choices (UI layer)
      if (identical(tier, "dev")) {
        req(is_dev())          # layer 1 — hard abort; no notification needed, shouldn't happen
      }
      if (!is_dev()) tier <- gsub("^dev$", "admin", tier)   # layer 2 — sanitise

      all_u <- all_usuarios_rv()
      if (!is.null(all_u) && username %in% tolower(all_u$username)) {
        showNotification("Ya existe un usuario con ese nombre.", type = "error"); return()
      }

      new_row <- data.frame(
        id            = uuid::UUIDgenerate(),
        account_code  = .next_account_code(all_u),
        username      = username,
        password_hash = password,
        display_name  = display_name,
        tier          = tier,
        client_id     = tolower(Sys.getenv("CLIENT_ID")),
        permisos      = "{}",
        activo        = isTRUE(input$new_activo),
        created_at    = as.character(Sys.time()),
        last_login    = NA_character_,
        deleted       = FALSE,
        deleted_at    = NA_character_,
        stringsAsFactors = FALSE
      )

      updated <- dplyr::bind_rows(all_u, new_row)
      saved <- tryCatch({ auth_save_usuarios(updated); TRUE },
                        error = function(e) { message("[TIERS] create user save failed: ", e$message); FALSE })
      if (!saved) {
        showNotification("No se pudo guardar la cuenta. Verifica la conexión e intenta de nuevo.",
                         type = "error", duration = 6)
        return()
      }
      .usuarios_local(updated)
      shinyjs::hide("create_form_wrap")
      updateTextInput(session, "new_display_name", value = "")
      updateTextInput(session, "new_username",      value = "")
      showNotification(paste0("Cuenta '\u200b", username, "' (", new_row$account_code, ") creada."),
                       type = "message", duration = 3)
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

      # Dev-constraint pre-check: count non-deleted dev accounts
      all_u      <- all_usuarios_rv()
      live_devs  <- all_u[all_u$tier == "dev" & (is.na(all_u$deleted) | all_u$deleted != TRUE), ]
      dev_count  <- nrow(live_devs)
      is_dev_row <- identical(tier, "dev")

      if (is_dev_row && dev_count <= 1) {
        showNotification(
          paste0("No se puede eliminar '\u200b", uname,
                 "': debe existir al menos una cuenta dev en el sistema."),
          type = "error", duration = 5)
        return()
      }

      pending_del_idx(idx)

      # Warn extra loudly when deleting a dev account
      extra_warning <- if (is_dev_row) {
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

      # Final dev-constraint guard (race-condition safety — re-check at save time)
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
      saved <- tryCatch({ auth_save_usuarios(all_u); TRUE },
                        error = function(e) { message("[TIERS] delete save failed: ", e$message); FALSE })
      if (!saved) {
        showNotification("No se pudo guardar el cambio. Verifica la conexión e intenta de nuevo.",
                         type = "error", duration = 6)
        pending_del_idx(NULL)
        return()
      }
      .usuarios_local(all_u)
      pending_del_idx(NULL)
      selected_user_idx(NULL)
      showNotification(paste0("Cuenta '\u200b", target_user, "' desactivada."),
                       type = "message", duration = 4)
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
      .tiers_perm_editor_ui(ns, usuarios[idx, ], viewer_tier = current_tier())
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

      # Safety guards
      if (identical(original_tier, "dev")) new_tier <- "dev"      # dev tier permanent
      if (!is_dev() && identical(new_tier, "dev")) new_tier <- "admin"  # admin can't assign dev

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

      if (length(full_idx) == 1) {
        all_u[full_idx, "tier"]     <- new_tier
        all_u[full_idx, "permisos"] <- jsonlite::toJSON(overrides, auto_unbox = TRUE)
        saved <- tryCatch({ auth_save_usuarios(all_u); TRUE },
                          error = function(e) { message("[TIERS] save perms failed: ", e$message); FALSE })
        if (!saved) {
          showNotification("No se pudieron guardar los permisos. Verifica la conexión e intenta de nuevo.",
                           type = "error", duration = 6)
          return()
        }
        .usuarios_local(all_u)
      }

      selected_user_idx(NULL)
      showNotification("Permisos guardados.", type = "message", duration = 2)
    })

    # ── Tier config section ───────────────────────────────────────────────────
    TIER_META <- list(
      dev      = "Dev \u2014 Control total",
      admin    = "Admin",
      finance  = "Finance",
      analysis = "Analysis \u2014 Solo lectura"
    )

    output$tier_config_section <- renderUI({
      # Evaluate is_dev() to register the reactive dependency even when FALSE,
      # so the output re-fires as soon as tier resolves to "dev".
      dev <- isTRUE(is_dev())
      if (!dev) {
        return(div(class = "alert alert-warning mt-3",
                   icon("lock"), " Esta sección solo está disponible para el tier Dev."))
      }

      locked <- tier_config_locked()

      tryCatch({
        cards  <- lapply(names(TIER_META), function(tk) {
          column(6, class = "mb-3",
                 .tier_config_card_ui(ns, tk, TIER_META[[tk]], get_tier_defaults(tk), locked = locked))
        })

        div(
          class = "mt-3",
          div(
            class = "alert alert-info border-0 small mb-3",
            icon("circle-info"),
            " Aqu\u00ed defines los permisos que cada tier tiene ",
            tags$strong("por defecto"), ". Los overrides individuales por usuario se aplican encima."
          ),
          do.call(fluidRow, cards),
          div(
            class = "d-flex justify-content-end mt-3",
            if (locked)
              tags$span(class = "text-muted small fst-italic me-2",
                        style = "line-height:2;",
                        icon("lock"), " Haz clic en \u201cEditar config.\u201d para habilitar cambios.")
            else
              actionButton(ns("btn_save_tier_config"),
                           tagList(icon("floppy-disk"), " Guardar configuraci\u00f3n de tiers"),
                           class = "btn btn-primary btn-sm")
          )
        )
      }, error = function(e) {
        div(class = "alert alert-danger mt-3",
            icon("triangle-exclamation"),
            " Error al renderizar la configuraci\u00f3n de tiers: ",
            tags$code(conditionMessage(e)),
            tags$br(),
            tags$small(class = "text-muted",
                       "Intenta recargar la página. Si persiste, puede haber un problema con tiers_config.rds en S3."))
      })
    })

    observeEvent(input$btn_save_tier_config, {
      req(is_dev())

      new_config <- lapply(setNames(names(TIER_META), names(TIER_META)), function(tk) {
        lapply(setNames(names(PERM_LABELS), names(PERM_LABELS)), function(k) {
          if (identical(tk, "dev") && k == "can_view_tiers") return(TRUE)
          isTRUE(input[[paste0("cfg_", tk, "_", k)]])
        })
      })

      tryCatch({
        .s3_write(new_config, "tiers_config.rds")
        tier_config_rv(new_config)
        tier_config_locked(TRUE)   # auto-lock after save
        showNotification("Configuraci\u00f3n de tiers guardada.", type = "message", duration = 2)
      }, error = function(e) {
        showNotification(paste0("Error al guardar: ", e$message), type = "error")
      })
    })

  })
}
