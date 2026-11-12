# =============================================================================
# R/tiers_module.R
# Panel "Usuarios" — gestión de cuentas, permisos y configuración de tiers.
# Visible para dev y admin. Dev ve todo (incluyendo otras cuentas dev).
# Admin ve solo cuentas no-dev y no puede asignar tier dev.
# La sección "Config. de Tiers" es exclusiva para dev.
# =============================================================================

# Permission names, groups, staff-only keys and perm_visible_keys() live in
# R/permisos.R; tiers, colours, invitable tiers and locked keys in
# R/tier_registry.R. This file only renders and saves them.

# ── Activity tab views registry (2026-09-03) ─────────────────────────────────
# The single source of truth for every switchable view inside the Actividad
# tab (see tiersServer's activity_section below, which is entirely generic
# over this list). To add a new structured audit view in a future stage
# (Bancos/Pasivos activity are next), append ONE entry here -- id (used as
# the reactiveVal value and the button's input-id suffix, so keep it a
# simple identifier), label/icon for the button, can_view(shared) -> logical
# for independent per-view permission gating, and ui(ns) -> the view's own
# UI. Module-file scope (not inside tiersServer) since every field here is a
# pure function reference or a static string, safe to build once rather than
# per-session; the function references below (auditLogViewerUI,
# .payment_audit_can_view, etc.) must already exist when this file is
# sourced, which app.R's source() order guarantees.
.ACTIVITY_VIEWS <- list(
  list(id = "actividad", label = "Actividad", icon = "clock-rotate-left",
       can_view = function(shared) TRUE,
       ui       = function(ns) auditLogViewerUI(ns("activity_viewer"))),
  list(id = "pagos", label = "Archivos de pago", icon = "file-invoice-dollar",
       can_view = .payment_audit_can_view,
       ui       = function(ns) pagosAuditViewerUI(ns("pagos_viewer")))
)

.tier_color <- function(t) nivel_color(t)

# ── UI ────────────────────────────────────────────────────────────────────────

tiersUI <- function(id) {
  ns <- NS(id)

  div(
    class = "p-3",
    style = "overflow-y: auto; max-height: calc(100vh - 65px);",

    # The table engine from the start (T28): every table of these tabs sits
    # in a renderUI of a tab not shown yet, and each sends its data when the
    # session starts. Loaded with the first table drawn instead, the engine
    # missed the other tabs' first messages and they stayed "Cargando…"
    # (found in the T28 screenshots: Principal opening Seguridad, then
    # Clientes). With it here, every message is kept until its table shows.
    hd_table_deps(),

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
                  # T31: job title, required for every new account.
                  fluidRow(
                    column(6, textInput(ns("new_puesto"), tr("cuentas.usuarios.puesto")))
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
                  tiers_usuarios_ui(ns("usuarios")))
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

.tiers_perm_editor_ui <- function(ns, row, viewer_tier = "", registry = NULL,
                                  viewer_is_staff = FALSE, config = NULL) {
  tier <- row$tier %||% ""

  current_perms <- permisos_resueltos(tier, row$permisos %||% "{}", config)
  overrides     <- tryCatch(jsonlite::fromJSON(row$permisos %||% "{}"),
                            error = function(e) list())

  asignables   <- niveles_asignables(viewer_tier, viewer_is_staff)
  tier_choices <- setNames(asignables, vapply(asignables, nivel_nombre, character(1)))
  bloqueados   <- permisos_bloqueados(tier)
  tier_es_tope <- nivel_es_tope(tier)

  perm_rows <- lapply(perm_visible_keys(viewer_is_staff), function(k) {
    is_locked <- k %in% bloqueados
    is_override <- k %in% names(overrides)
    current_val <- if (is_locked) TRUE else isTRUE(current_perms[[k]])

    badge <- if (is_locked && tier_es_tope) {
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

    tooltip_text <- if (is_locked && tier_es_tope)
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
            tags$span(permiso_nombre(k),
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
                nivel_nombre(tier)),
              tags$span(style = "font-size:.75rem; opacity:.6;",
                        paste0("@", row$username %||% "")))
        )
      )
    ),
    div(
      class = "card-body p-3",
      tags$h6(class = "fw-semibold mb-2 small text-muted text-uppercase", "Datos del usuario"),
      # T31: the user name is shown, not edited (no input: the server never
      # reads one). Job title and e-mail are edited here, next to the name.
      div(class = "mb-2",
          tags$label(class = "form-label", tr("cuentas.usuarios.usuario")),
          tags$div(class = "form-control-plaintext fw-semibold", row$username %||% ""),
          tags$small(class = "text-muted", tr("cuentas.usuarios.usuario_fijo"))),
      div(class = "mb-2",
          textInput(ns("edit_display_name"), tr("cuentas.usuarios.nombre"),
                    value = row$display_name %||% "", width = "100%")),
      div(class = "mb-2",
          textInput(ns("edit_puesto"), tr("cuentas.usuarios.puesto"),
                    value = if (is.na(row$puesto %||% NA)) "" else row$puesto, width = "100%")),
      div(class = "mb-2",
          textInput(ns("edit_email"), tr("cuentas.usuarios.correo"),
                    value = if (is.na(row$email %||% NA)) "" else row$email, width = "100%")),
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
      # [[ ]]: permisos_resueltos() is a named logical vector; `$` on it throws.
      if (isTRUE(current_perms[["can_jump_clients"]]) &&
          nivel_es_tope(viewer_tier) &&
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

.tier_config_card_ui <- function(ns, tier_key, tier_label, defaults, locked = TRUE,
                                 viewer_is_staff = FALSE) {
  perm_rows <- lapply(perm_visible_keys(viewer_is_staff), function(k) {
    is_sys_locked <- k %in% permisos_bloqueados(tier_key)
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
      tags$span(permiso_nombre(k), style = "font-size:.82rem;"),
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

# Row of `usuarios` (the viewer's filtered list) whose username is `uname`,
# or NULL when that account is not there (or not visible to this viewer).
# Every action on an account looks it up this way, never by row position.
.fila_usuario <- function(usuarios, uname) {
  if (is.null(usuarios) || !nrow(usuarios) || length(uname) != 1 || is.na(uname)) return(NULL)
  i <- which(usuarios$username == as.character(uname))
  if (length(i) == 1) i else NULL
}

# ── Usuarios on the table engine (T28) ──────────────────────────────────────
# The viewer's list (usuarios_rv(): no deleted accounts, only the tiers it
# may see -- decided on the server) as rows, id = the username: the same key
# .fila_usuario() looks every action up by. The tier by its name; Estado
# "activo" only when activo is TRUE (an NA reads Inactivo, not "NA").
.usuarios_filas <- function(usuarios) {
  vacia <- data.frame(id = character(0), codigo = character(0), nombre = character(0), usuario = character(0),
                      puesto = character(0), nivel = character(0), estado = character(0), stringsAsFactors = FALSE)
  if (is.null(usuarios) || !is.data.frame(usuarios) || !nrow(usuarios)) return(vacia)
  u <- usuarios[!is.na(usuarios$username) & nzchar(usuarios$username) & !duplicated(usuarios$username), , drop = FALSE]
  if (!nrow(u)) return(vacia)
  data.frame(id = as.character(u$username),
             codigo = as.character(if ("account_code" %in% names(u)) u$account_code else NA_character_),
             nombre = as.character(u$display_name),
             # T31: who manages users sees name, user name and job title
             # (an old account's empty job title reads as an empty cell).
             usuario = as.character(u$username),
             puesto = as.character(if ("puesto" %in% names(u)) u$puesto else NA_character_),
             nivel = vapply(as.character(u$tier), function(t) if (is.na(t)) "" else nivel_nombre(t), character(1),
                            USE.NAMES = FALSE),
             estado = ifelse(u$activo %in% TRUE, "activo", "inactivo"), stringsAsFactors = FALSE)
}

tiers_usuarios_ui <- function(id) hd_table_ui(shiny::NS(id, "tabla"))

# The table only: what ✎ and ✕ do (the permissions editor; the delete modal
# with the last-live-Dev lock) stays in tiersServer. usuarios(): the
# viewer's list; con_codigo(): staff sees the "#" column. editar() /
# eliminar(): list(id = username, nonce) for ✎ (or a double click) / ✕ --
# NULL (never req(): see tiers_solicitudes_server) for any other action or
# without permitido(). ✕ skips the bar's confirmation (sin_confirmar): the
# module's own modal says more (the lock, the protected tier).
tiers_usuarios_server <- function(id, usuarios, con_codigo, permitido) {
  moduleServer(id, function(input, output, session) {
    # permitido() first: usuarios_rv() answers from the session's local copy
    # (.usuarios_local) before it checks req_authorized(), so the list alone
    # is no gate (integrator's review of 8b9355d).
    datos <- reactive({
      if (!isTRUE(permitido())) return(NULL)
      .usuarios_filas(usuarios())
    })
    columnas <- reactive(c(
      if (isTRUE(con_codigo())) list(col_texto("codigo", "niveles.tablas.usuarios.codigo", ancho = 70)),
      list(col_texto("nombre", "niveles.tablas.usuarios.nombre"),
           col_texto("usuario", "niveles.tablas.usuarios.usuario"),
           col_texto("puesto", "niveles.tablas.usuarios.puesto"),
           col_texto("nivel", "niveles.tablas.usuarios.nivel"),
           col_insignia("estado", "niveles.tablas.usuarios.estado", mapa = list(
             activo   = list(texto = "niveles.tablas.usuarios.activo", tono = "exito"),
             inactivo = list(texto = "niveles.tablas.usuarios.inactivo", tono = "neutro"))))))
    t <- hd_table_server("tabla", tabla = "usuarios", datos = datos, columnas = columnas,
      acciones_fila = c("editar", "eliminar"), sin_confirmar = "eliminar",
      filtros = list(list(campo = "nivel"), list(campo = "estado")),
      vacio = "niveles.tablas.usuarios.vacio")
    accion <- function(nombres) reactive({
      a <- t$accion()
      if (is.null(a) || !a$accion %in% nombres || !isTRUE(permitido()) || !length(a$ids)) return(NULL)
      list(id = a$ids[1], nonce = a$nonce)
    })
    list(datos = datos, editar = accion(c("editar", "detalle")), eliminar = accion("eliminar"))
  })
}

# ── Cuentas bloqueadas on the table engine (T28) ─────────────────────────────
# One row per locked account; its id is the lower-case username, the same key
# the unlock removes (all its spellings), so the action names the account,
# never a row position. No lock list (NULL) or an empty one: 0 rows.
.bloqueadas_filas <- function(lock_df) {
  vacia <- data.frame(id = character(0), usuario = character(0), bloqueado_el = as.POSIXct(character(0)),
                      por = character(0), motivo = character(0), stringsAsFactors = FALSE)
  if (is.null(lock_df) || !is.data.frame(lock_df) || !nrow(lock_df)) return(vacia)
  id <- tolower(trimws(as.character(lock_df$username)))
  quedan <- !is.na(id) & nzchar(id) & !duplicated(id)
  lock_df <- lock_df[quedan, , drop = FALSE]
  data.frame(id = id[quedan], usuario = as.character(lock_df$username),
             bloqueado_el = as.POSIXct(lock_df$locked_at), por = as.character(lock_df$locked_by),
             motivo = as.character(lock_df$reason), stringsAsFactors = FALSE)
}

tiers_bloqueadas_ui <- function(id) hd_table_ui(shiny::NS(id, "tabla"))

# The table and its 🔓. permitido(): Principal (checked here, on the server:
# without it the table gets no rows and an unlock that arrives anyway does
# nothing). refresh / actividad: the tab's reactiveVals, bumped as before.
# Returns list(datos) for the tests.
tiers_bloqueadas_server <- function(id, shared, permitido, refresh, actividad) {
  moduleServer(id, function(input, output, session) {
    datos <- reactive({
      if (!isTRUE(permitido())) return(NULL)
      refresh()
      .bloqueadas_filas(read_emergency_lock())   # a failed read is the table's notice, never "no locks"
    })
    t <- hd_table_server("tabla", tabla = "cuentas_bloqueadas", datos = datos,
      columnas = list(col_texto("usuario", "niveles.tablas.bloqueadas.usuario"),
                      col_fecha_hora("bloqueado_el", "niveles.tablas.bloqueadas.bloqueado_el"),
                      col_texto("por", "niveles.tablas.bloqueadas.por"),
                      col_texto("motivo", "niveles.tablas.bloqueadas.motivo")),
      acciones_fila = "desbloquear",
      vacio = list(texto = "niveles.tablas.bloqueadas.vacio", tono = "exito"))

    observeEvent(t$accion(), {
      a <- t$accion()
      req(identical(a$accion, "desbloquear"), isTRUE(permitido()))
      username <- tolower(trimws(a$ids[1] %||% ""))
      req(nzchar(username))
      current_viewer <- shared$current_user() %||% "unknown"
      tryCatch({
        # On S3's current list (CM13): only this user's lock is removed.
        modificar_emergency_lock(function(l) l[tolower(l$username) != username, , drop = FALSE])
        refresh(refresh() + 1L)
        showNotification(tr("niveles.bloqueadas.aviso.desbloqueada", usuario = username),
                         type = "message", duration = 4)
        log_action(user        = shared$current_user(),
                   module      = "seguridad",
                   action      = "emergency_unlock",
                   description = paste0("Lock removed from '", username, "' by ", current_viewer),
                   target_id   = username,
                   s3_key      = "hd-admin/emergency_lock.rds",
                   client_id             = "hd-admin",
                   viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL))
        actividad(actividad() + 1L)
      }, error = function(e) {
        showNotification(tr("niveles.bloqueadas.aviso.error_desbloquear", error = e$message), type = "error")
      })
    }, ignoreInit = TRUE)

    list(datos = datos)
  })
}

# ── Solicitudes pendientes (client requests) on the table engine (T28) ──────
# The pending requests only, newest last as stored; id = the request's id.
.solicitudes_filas <- function(reqs) {
  vacia <- data.frame(id = character(0), propuesto = character(0), nombre = character(0),
                      solicitado_por = character(0), fecha = as.POSIXct(character(0)),
                      limite = integer(0), stringsAsFactors = FALSE)
  if (is.null(reqs) || !is.data.frame(reqs) || !nrow(reqs)) return(vacia)
  p <- reqs[!is.na(reqs$status) & reqs$status == "pending" & !is.na(reqs$id) & nzchar(reqs$id), , drop = FALSE]
  p <- p[!duplicated(p$id), , drop = FALSE]
  if (!nrow(p)) return(vacia)
  data.frame(id = as.character(p$id), propuesto = as.character(p$client_id_proposed),
             nombre = as.character(p$display_name), solicitado_por = as.character(p$requested_by),
             fecha = as.POSIXct(as.character(p$requested_at), tz = "America/Mexico_City"),
             limite = suppressWarnings(as.integer(p$max_users)), stringsAsFactors = FALSE)
}

tiers_solicitudes_ui <- function(id) hd_table_ui(shiny::NS(id, "tabla"))

# The table only; what a row opens (the review modal, which approves or
# rejects) stays in tiersServer. revisar(): list(id, nonce) each time ℹ or a
# double click names a request -- only for permitido() (Principal), checked
# here on the server; otherwise the table has no rows and nothing is named.
tiers_solicitudes_server <- function(id, permitido, refresh) {
  moduleServer(id, function(input, output, session) {
    datos <- reactive({
      if (!isTRUE(permitido())) return(NULL)
      refresh()
      # The strict reader (read_client_requests() turns a failed read into
      # "no requests"): a failure is the table's notice (P7).
      .solicitudes_filas(leer_hd_admin("client_requests.rds", .schema_client_requests))
    })
    t <- hd_table_server("tabla", tabla = "solicitudes", datos = datos,
      columnas = list(col_texto("propuesto", "niveles.tablas.solicitudes.propuesto"),
                      col_texto("nombre", "niveles.tablas.solicitudes.nombre"),
                      col_texto("solicitado_por", "niveles.tablas.solicitudes.solicitado_por"),
                      col_fecha_hora("fecha", "niveles.tablas.solicitudes.fecha"),
                      col_numero("limite", "niveles.tablas.solicitudes.limite")),
      acciones_fila = "detalle",
      vacio = "niveles.tablas.solicitudes.vacio")
    # NULL, never req(), when nothing is named: an observeEvent(..., ignoreInit
    # = TRUE) on a reactive that fails at start-up drops the first real click
    # (Shiny only marks it initialized once it gives a value; integrator, T28).
    revisar <- reactive({
      a <- t$accion()
      if (is.null(a) || !identical(a$accion, "detalle") || !isTRUE(permitido()) || !length(a$ids)) return(NULL)
      list(id = a$ids[1], nonce = a$nonce)
    })
    list(datos = datos, revisar = revisar)
  })
}

# ── Cola de correos (hd-admin's email queue) on the table engine (T28) ──────
# What is still to go out: pending and failed emails, id = the queue's id.
.cola_filas <- function(q) {
  vacia <- data.frame(id = character(0), para = character(0), asunto = character(0),
                      estado = character(0), intentos = integer(0), stringsAsFactors = FALSE)
  if (is.null(q) || !is.data.frame(q) || !nrow(q)) return(vacia)
  q <- q[!is.na(q$status) & q$status %in% c("pending", "failed") & !is.na(q$id) & nzchar(q$id), , drop = FALSE]
  q <- q[!duplicated(q$id), , drop = FALSE]
  if (!nrow(q)) return(vacia)
  data.frame(id = as.character(q$id), para = as.character(q$to), asunto = as.character(q$subject),
             estado = as.character(q$status), intentos = suppressWarnings(as.integer(q$attempts)),
             stringsAsFactors = FALSE)
}

tiers_cola_ui <- function(id) hd_table_ui(shiny::NS(id, "tabla"))

# The table only; sending again stays in tiersServer. reintentar():
# list(id, nonce) for ↻ on a row; reintentar_todo(): the bar's "Reintentar
# todo" (the whole queue, as before). Both only for permitido() (HopDesk
# staff: the queue is global, every client's mail), checked here on the
# server; otherwise the table has no rows and nothing is retried.
tiers_cola_server <- function(id, permitido, refresh) {
  moduleServer(id, function(input, output, session) {
    datos <- reactive({
      if (!isTRUE(permitido())) return(NULL)
      refresh()
      .cola_filas(read_email_queue())   # strict: a failed read is the table's notice (P7)
    })
    t <- hd_table_server("tabla", tabla = "cola_correos", datos = datos, piezas = c("acc_fila", "barra_acc"),
      columnas = list(col_texto("para", "niveles.tablas.cola.para"),
                      col_texto("asunto", "niveles.tablas.cola.asunto"),
                      col_insignia("estado", "niveles.tablas.cola.estado", mapa = list(
                        pending = list(texto = "niveles.tablas.cola.pendiente", tono = "aviso"),
                        failed  = list(texto = "niveles.tablas.cola.fallido", tono = "peligro"))),
                      col_numero("intentos", "niveles.tablas.cola.intentos")),
      acciones_fila = "reintentar",
      acciones_barra = list(list(accion = "reintentar_todo", texto = "niveles.tablas.cola.reintentar_todo",
                                 sin_seleccion = TRUE)),
      vacio = list(texto = "niveles.tablas.cola.vacio", tono = "exito"))
    # NULL, never req(), when it isn't this action (see revisar() above: a
    # req() at start-up makes ignoreInit drop the first real click).
    accion <- function(nombre) reactive({
      a <- t$accion()
      if (is.null(a) || !identical(a$accion, nombre) || !isTRUE(permitido())) return(NULL)
      list(id = a$ids[1] %||% NA_character_, nonce = a$nonce)
    })
    list(datos = datos, reintentar = accion("reintentar"), reintentar_todo = accion("reintentar_todo"))
  })
}

# ── Invitaciones pendientes on the table engine (T28) ───────────────────────
# pending_invites.rds is ONE global file in hd-admin/ with a client_id per
# row. Staff sees every client's (its job is onboarding clients); anyone else
# only its own client's -- a row with no client_id goes to nobody. Done here,
# on the server: the browser never gets another client's emails. id = the
# invite's token (what the ✕ button already carried); the client shows by
# its name and the tier by its name.
.invitaciones_filas <- function(invites, registry, staff, own_client) {
  vacia <- data.frame(id = character(0), email = character(0), nombre = character(0), cliente = character(0),
                      nivel = character(0), expira = as.Date(character(0)), stringsAsFactors = FALSE)
  if (is.null(invites) || !is.data.frame(invites) || !nrow(invites)) return(vacia)
  p <- invites[!is.na(invites$status) & invites$status == "pending" & !is.na(invites$token) &
                 nzchar(invites$token), , drop = FALSE]
  if (!isTRUE(staff))
    p <- p[!is.na(p$client_id) & p$client_id %in% own_client, , drop = FALSE]   # own_client NA/NULL: none
  p <- p[!duplicated(p$token), , drop = FALSE]
  if (!nrow(p)) return(vacia)
  etiqueta <- function(cid) {
    nm <- if (is.data.frame(registry) && nrow(registry)) registry$display_name[match(cid, registry$client_id)] else NA
    ifelse(is.na(nm) | !nzchar(nm), cid, nm)
  }
  data.frame(id = as.character(p$token), email = as.character(p$email), nombre = as.character(p$display_name),
             cliente = etiqueta(as.character(p$client_id)),
             nivel = vapply(as.character(p$tier), function(t) if (is.na(t)) "" else nivel_nombre(t), character(1),
                            USE.NAMES = FALSE),
             expira = as.Date(substr(as.character(p$expires_at), 1, 10)), stringsAsFactors = FALSE)
}

tiers_invitaciones_ui <- function(id) hd_table_ui(shiny::NS(id, "tabla"))

# ── Clientes registrados on the table engine (T28) ──────────────────────────
# Each client's last activity: the newest ts of its active audit chunk
# (app_audit.rds only, never the historical ones), NA without one. Read as
# before -- one object per client, on each refresh.
.clientes_ultima_actividad <- function(cids) {
  vapply(as.character(cids), function(cid) tryCatch({
    df <- suppressMessages(suppressWarnings(
      aws.s3::s3readRDS(object = paste0(cid, "/app_audit.rds"), bucket = .s3_bucket())))
    if (is.data.frame(df) && nrow(df) && "ts" %in% names(df)) {
      ts <- suppressWarnings(as.POSIXct(df$ts)); ts <- ts[!is.na(ts)]
      if (length(ts)) as.numeric(max(ts)) else NA_real_
    } else NA_real_
  }, error = function(e) NA_real_), numeric(1), USE.NAMES = FALSE)
}

# One row per registered client, id = client_id. cupo: "lleno" with no seat
# left, "quedan_<n>" with n = 1..CUPO_POCOS seats left (the old red /
# yellow row, which said "⚠ N restantes"), "" otherwise (T28 P3: a badge,
# not a tinted row; the number stays, coordinator's review of 209dc08).
# ultima: .clientes_ultima_actividad().
.CUPO_POCOS <- 2L
.clientes_filas <- function(registry, ultima = NULL) {
  vacia <- data.frame(id = character(0), nombre = character(0), cupo = character(0), usuarios = character(0),
                      contacto = character(0), ultima = as.POSIXct(character(0)), estado = character(0),
                      stringsAsFactors = FALSE)
  if (is.null(registry) || !is.data.frame(registry) || !nrow(registry)) return(vacia)
  r <- registry[!is.na(registry$client_id) & nzchar(registry$client_id) & !duplicated(registry$client_id), , drop = FALSE]
  if (!nrow(r)) return(vacia)
  maxu <- suppressWarnings(as.integer(r$max_users)); cur <- suppressWarnings(as.integer(r$current_users))
  resta <- maxu - cur
  if (is.null(ultima)) ultima <- rep(NA_real_, nrow(r))
  data.frame(id = as.character(r$client_id), nombre = as.character(r$display_name),
             cupo = ifelse(is.na(resta), "", ifelse(resta <= 0, "lleno",
                           ifelse(resta <= .CUPO_POCOS, paste0("quedan_", resta), ""))),
             usuarios = sprintf("%s / %s", ifelse(is.na(cur), "?", cur), ifelse(is.na(maxu), "?", maxu)),
             contacto = as.character(r$contact_email),
             ultima = as.POSIXct(as.numeric(ultima), origin = "1970-01-01"),
             estado = ifelse(r$status %in% "active", "active", "archived"), stringsAsFactors = FALSE)
}

tiers_clientes_ui <- function(id) hd_table_ui(shiny::NS(id, "tabla"))

# The table only; editing a client and its contacts stay in tiersServer.
# editar() / contactos(): list(id = client_id, nonce) for ✎ / 📇 -- NULL
# (never req(): see tiers_solicitudes_server) for anyone permitido() doesn't
# allow (HopDesk, Principal) or for any other action.
tiers_clientes_server <- function(id, permitido, refresh) {
  moduleServer(id, function(input, output, session) {
    datos <- reactive({
      if (!isTRUE(permitido())) return(NULL)
      refresh()
      registry <- leer_hd_admin("client_registry.rds", .schema_client_registry)   # strict (P7)
      .clientes_filas(registry, .clientes_ultima_actividad(registry$client_id))
    })
    t <- hd_table_server("tabla", tabla = "clientes", datos = datos,
      columnas = list(col_texto("id", "niveles.tablas.clientes.id"),
                      col_texto("nombre", "niveles.tablas.clientes.nombre"),
                      col_insignia("cupo", "niveles.tablas.clientes.cupo", mapa = c(
                        list(lleno = list(texto = "niveles.tablas.clientes.lleno", tono = "peligro")),
                        # "Quedan 1" / "Quedan 2": the number in the text,
                        # built here in the session's language.
                        stats::setNames(lapply(seq_len(.CUPO_POCOS), function(n)
                          list(texto = tr("niveles.tablas.clientes.quedan", n = n), tono = "aviso")),
                          paste0("quedan_", seq_len(.CUPO_POCOS))))),
                      col_texto("usuarios", "niveles.tablas.clientes.usuarios"),
                      col_texto("contacto", "niveles.tablas.clientes.contacto"),
                      col_fecha_hora("ultima", "niveles.tablas.clientes.ultima"),
                      col_insignia("estado", "niveles.tablas.clientes.estado", mapa = list(
                        active   = list(texto = "niveles.tablas.clientes.activo", tono = "exito"),
                        archived = list(texto = "niveles.tablas.clientes.archivado", tono = "neutro")))),
      acciones_fila = c("editar", "contactos"),
      vacio = "niveles.tablas.clientes.vacio")
    accion <- function(nombre) reactive({
      a <- t$accion()
      if (is.null(a) || !identical(a$accion, nombre) || !isTRUE(permitido()) || !length(a$ids)) return(NULL)
      list(id = a$ids[1], nonce = a$nonce)
    })
    list(datos = datos, editar = accion("editar"), contactos = accion("contactos"))
  })
}

# ── Accesos de salto activos on the table engine (T28) ──────────────────────
# What is left of each access, in words (what the old .fmt_remaining() said,
# now from the dictionary): no expiry, expired, minutes, hours and minutes,
# or days.
.accesos_quedan <- function(expira, ahora = Sys.time()) {
  secs <- as.numeric(difftime(expira, ahora, units = "secs"))
  vapply(seq_along(secs), function(i) {
    s <- secs[i]
    if (is.na(expira[i])) return(tr("niveles.tablas.accesos.sin_expiracion"))
    if (!is.finite(s) || s <= 0) return(tr("niveles.tablas.accesos.expirado"))
    if (s < 3600) return(gsub("{n}", round(s / 60), tr("niveles.tablas.accesos.min"), fixed = TRUE))
    if (s < 86400)
      return(gsub("{m}", sprintf("%02.0f", floor((s %% 3600) / 60)),
                  gsub("{h}", floor(s / 3600), tr("niveles.tablas.accesos.horas"), fixed = TRUE), fixed = TRUE))
    gsub("{n}", floor(s / 86400), tr("niveles.tablas.accesos.dias"), fixed = TRUE)
  }, character(1))
}

# The active accesses (not revoked, not expired), id = the grant's id.
# Principal sees everyone's; any other staff only its own (grantee == me),
# decided here on the server.
.accesos_filas <- function(grants, registry, principal, me, ahora = Sys.time()) {
  vacia <- data.frame(id = character(0), usuario = character(0), cliente = character(0), quedan = character(0),
                      expira = as.POSIXct(character(0)), stringsAsFactors = FALSE)
  if (is.null(grants) || !is.data.frame(grants) || !nrow(grants)) return(vacia)
  expira <- as.POSIXct(grants$expires_at)
  g <- grants[!(grants$revoked %in% TRUE) & (is.na(expira) | expira > ahora) & !is.na(grants$id) &
                nzchar(grants$id), , drop = FALSE]
  if (!isTRUE(principal)) g <- g[!is.na(g$grantee) & g$grantee %in% me, , drop = FALSE]
  g <- g[!duplicated(g$id), , drop = FALSE]
  if (!nrow(g)) return(vacia)
  nm <- if (is.data.frame(registry) && nrow(registry)) registry$display_name[match(g$client_id, registry$client_id)] else NA
  exp_g <- as.POSIXct(g$expires_at)
  data.frame(id = as.character(g$id), usuario = as.character(g$grantee),
             cliente = ifelse(is.na(nm) | !nzchar(nm), as.character(g$client_id), nm),
             quedan = .accesos_quedan(exp_g, ahora), expira = exp_g, stringsAsFactors = FALSE)
}

tiers_accesos_ui <- function(id) hd_table_ui(shiny::NS(id, "tabla"))

# permitido(): HopDesk staff (the tab). principal(): sees everyone's, with
# the Usuario column; anyone else only its own. me(): the viewer's user.
# puede_revocar(): who gets ⊘ (Principal with can_manage_hopdesk_perms);
# for anyone else the button is hidden and revocar() stays NULL.
tiers_accesos_server <- function(id, permitido, principal, me, puede_revocar, refresh) {
  moduleServer(id, function(input, output, session) {
    datos <- reactive({
      if (!isTRUE(permitido())) return(NULL)
      refresh()
      registry <- tryCatch(read_client_registry(), error = function(e) .schema_client_registry())   # names only
      .accesos_filas(leer_hd_admin("hop_grants.rds", .schema_hop_grants),   # strict (P7)
                     registry, principal = isTRUE(principal()), me = me())
    })
    columnas <- reactive(c(
      if (isTRUE(principal())) list(col_texto("usuario", "niveles.tablas.accesos.usuario")),
      list(col_texto("cliente", "niveles.tablas.accesos.cliente"),
           col_texto("quedan", "niveles.tablas.accesos.quedan"),
           col_fecha_hora("expira", "niveles.tablas.accesos.expira"))))
    t <- hd_table_server("tabla", tabla = "accesos_salto", datos = datos, columnas = columnas,
      acciones_fila = "revocar",
      acciones_ocultas = reactive(if (isTRUE(puede_revocar())) character(0) else "revocar"),
      vacio = "niveles.tablas.accesos.vacio")
    revocar <- reactive({
      a <- t$accion()
      if (is.null(a) || !identical(a$accion, "revocar") || !isTRUE(puede_revocar()) || !length(a$ids)) return(NULL)
      list(id = a$ids[1], nonce = a$nonce)
    })
    list(datos = datos, revocar = revocar)
  })
}

# ── Contactos activos (one client's, in its modal) on the table engine (T28) ─
.contactos_filas <- function(contacts) {
  vacia <- data.frame(id = character(0), nombre = character(0), email = character(0), cargo = character(0),
                      alertas = character(0), stringsAsFactors = FALSE)
  if (is.null(contacts) || !is.data.frame(contacts) || !nrow(contacts)) return(vacia)
  a <- contacts[contacts$active %in% TRUE & !is.na(contacts$id) & nzchar(contacts$id) & !duplicated(contacts$id), ,
                drop = FALSE]
  if (!nrow(a)) return(vacia)
  data.frame(id = as.character(a$id), nombre = as.character(a$name), email = as.character(a$email),
             cargo = as.character(a$role), alertas = ifelse(a$receives_limit_alerts %in% TRUE, "si", ""),
             stringsAsFactors = FALSE)
}

tiers_contactos_ui <- function(id) hd_table_ui(shiny::NS(id, "tabla"))

# cliente(): the client whose contacts are open. desactivar(): list(id,
# nonce) once ⊘ is confirmed in the bar; NULL otherwise or without
# permitido(). The contacts are read strictly, so a failed read is the
# table's notice, never "no contacts" (P7).
tiers_contactos_server <- function(id, permitido, cliente, refresh) {
  moduleServer(id, function(input, output, session) {
    datos <- reactive({
      cid <- cliente()
      if (!isTRUE(permitido()) || is.null(cid) || !nzchar(cid)) return(NULL)
      refresh()
      .contactos_filas(.hd_admin_completar(.s3_leer_para_escribir("contacts.rds", cid), .schema_contacts))
    })
    t <- hd_table_server("tabla", tabla = "contactos", datos = datos,
      columnas = list(col_texto("nombre", "niveles.tablas.contactos.nombre"),
                      col_texto("email", "niveles.tablas.contactos.email"),
                      col_texto("cargo", "niveles.tablas.contactos.cargo"),
                      col_insignia("alertas", "niveles.tablas.contactos.alertas", mapa = list(
                        si = list(texto = "niveles.tablas.contactos.si", tono = "exito")))),
      acciones_fila = "desactivar",
      vacio = "niveles.tablas.contactos.vacio")
    desactivar <- reactive({
      a <- t$accion()
      if (is.null(a) || !identical(a$accion, "desactivar") || !isTRUE(permitido()) || !length(a$ids)) return(NULL)
      list(id = a$ids[1], nonce = a$nonce)
    })
    list(datos = datos, desactivar = desactivar)
  })
}

# The table only; revoking stays in tiersServer (with its tenant check).
# revocar(): list(id = token, nonce) once ⊘ is confirmed in the bar -- NULL
# (never req(): see tiers_solicitudes_server) for anyone permitido() doesn't
# allow (Dev, HopDesk, Principal) or for any other action.
tiers_invitaciones_server <- function(id, permitido, staff, own_client, refresh) {
  moduleServer(id, function(input, output, session) {
    datos <- reactive({
      if (!isTRUE(permitido())) return(NULL)
      refresh()
      registry <- tryCatch(read_client_registry(), error = function(e) .schema_client_registry())   # names only
      .invitaciones_filas(leer_hd_admin("pending_invites.rds", .schema_pending_invites),   # strict (P7)
                          registry, staff = isTRUE(staff()), own_client = own_client())
    })
    t <- hd_table_server("tabla", tabla = "invitaciones", datos = datos,
      columnas = list(col_texto("email", "niveles.tablas.invitaciones.email"),
                      col_texto("nombre", "niveles.tablas.invitaciones.nombre"),
                      col_texto("cliente", "niveles.tablas.invitaciones.cliente"),
                      col_texto("nivel", "niveles.tablas.invitaciones.nivel"),
                      col_fecha("expira", "niveles.tablas.invitaciones.expira")),
      acciones_fila = "revocar",
      vacio = "niveles.tablas.invitaciones.vacio")
    revocar <- reactive({
      a <- t$accion()
      if (is.null(a) || !identical(a$accion, "revocar") || !isTRUE(permitido()) || !length(a$ids)) return(NULL)
      list(id = a$ids[1], nonce = a$nonce)
    })
    list(datos = datos, revocar = revocar)
  })
}

tiersServer <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    current_tier <- reactive({
      tryCatch(shared$current_user_info()$tier, error = function(e) "")
    })

    # Usuarios panel is visible to dev / hopdesk / principal
    # Real tier (identity), not "Ver como": staff keep their own powers here.
    is_principal      <- reactive({ nivel_es_tope(current_tier()) })
    is_hopdesk        <- reactive({ is_staff_tier(current_tier()) && !is_principal() })
    is_dev            <- reactive({ nivel_es_ti(current_tier()) })
    # Staff's own permissions (home usuarios.rds), never "Ver como": internal
    # actions are judged on who you are, not on the tier you are viewing as.
    staff_perms       <- reactive({
      if (!is_staff_tier(current_tier())) return(list())
      .audit_viewer_resolve_perms(shared)
    })
    req_authorized    <- reactive({
      is_staff_tier(current_tier()) ||
        puede(tryCatch(shared$usuario(), error = function(e) NULL), "can_view_tiers")
    })
    # Stage 2 Part A: effective_client_id() (home, or jumped if mid-jump) is
    # the single source of truth — this reactive just exposes it under this
    # module's existing name so its ~10 internal call sites don't all need
    # touching.
    current_client_id <- reactive({
      tryCatch(shared$effective_client_id(), error = function(e) tolower(Sys.getenv("CLIENT_ID")))
    })

    # Hopdesk's own staff vs. a client's user. Fails CLOSED: if the viewer's
    # identity can't be resolved we treat them as a client, so a half-loaded
    # session can never be mistaken for staff.
    viewer_is_staff <- reactive({
      isTRUE(tryCatch(shared$current_user_info()$is_staff, error = function(e) FALSE))
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
      badge <- tags$span(class = "tiers-dev-badge", toupper(nivel_nombre(tier)))

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
            tr("niveles.accesos.aviso.sin_acceso", cliente = new_cid),
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
    #
    # Scoping fix (2026-09-04, Grupo activity-logging audit): this used to
    # read/write via .s3_read()/.s3_write() with NO client_id, which fall
    # back to Sys.getenv("CLIENT_ID") -- always "hd-admin" for this single
    # shared deployment (ARCHITECTURE.md §3), regardless of which client a
    # session is jumped into or which client's own dev-tier user is saving.
    # Any client's dev user editing "Config. de Tiers" was silently
    # overwriting the SAME one global file every other client's session
    # also reads from get_tier_defaults() below. In practice this file is
    # only ever consulted to pre-fill THIS SAME admin screen's own preview
    # checkboxes (confirmed: btn_save_perms/btn_save_new_user build real
    # user permissions via auth_resolve_perms() directly, never through
    # tier_config_rv()), so the real-world impact was a shared, cosmetic
    # preview default rather than live permission escalation -- still a
    # genuine cross-tenant write-isolation bug, worth fixing regardless of
    # its blast radius. Now reads/writes .s3_key(key, client_id=
    # current_client_id())'s own per-client file via .s3_read_with()/the
    # existing client_id= param on .s3_write().
    tier_config_rv <- reactiveVal(NULL)

    observe({
      cid <- current_client_id()  # explicit dependency: re-fetch on a jump/home switch
      req(req_authorized())
      cfg <- tryCatch(.s3_read_with(S3_KEYS$tiers_config, client_id = cid), error = function(e) NULL)
      tier_config_rv(cfg)  # NULL on a client with no saved config yet, not last client's stale value
    })

    # Defaults this client gives a tier: registry, then its own tier config.
    get_tier_defaults <- function(tier_key) {
      as.list(permisos_resueltos(tier_key, "{}", tier_config_rv()))
    }

    # ── Usuarios data ─────────────────────────────────────────────────────────
    .usuarios_local <- reactiveVal(NULL)

    # Full list (unfiltered) — used for saves to avoid data loss
    all_usuarios_rv <- reactive({
      ovr <- .usuarios_local()
      if (!is.null(ovr)) return(ovr)
      req(req_authorized())
      # A failed read now stop()s instead of returning an empty table; say so
      # and cancel every consumer (req), so no save goes on with no users.
      u <- tryCatch(auth_load_usuarios(client_id = current_client_id()), error = function(e) {
        message("[TIERS] usuarios read failed: ", conditionMessage(e))
        showNotification(tr("comun.reintentar"), type = "error", duration = 8)
        NULL
      })
      req(!is.null(u))
      u
    })

    # Clear local cache whenever the client context changes so the next
    # all_usuarios_rv() call fetches the correct folder from S3.
    # The account being edited or deleted belongs to the previous client: drop
    # it too, so no action ever lands on a same-named account of the new one.
    observeEvent(current_client_id(), {
      .usuarios_local(NULL)
      selected_user(NULL)
      # Close the delete modal only if it is ours (another modal may be open).
      if (!is.null(isolate(pending_del_user()))) removeModal()
      pending_del_user(NULL)
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
      #
      # The principal exclusion must apply to hopdesk too (not just non-staff
      # viewers) — otherwise a hopdesk viewer can open a principal row's
      # editor, whose tier <select> doesn't offer "Principal" as a choice and
      # silently defaults to its first option on save, demoting the account.
      if (nrow(df) > 0) {
        vt <- current_tier()
        df <- df[vapply(df$tier, function(t) nivel_puede_ver_fila(vt, t), logical(1)), , drop = FALSE]
      }
      df
    })

    # ── Users table (T28: on the table engine) ────────────────────────────────
    # The rows are usuarios_rv() -- already filtered on the server (deleted
    # accounts out, only the tiers this viewer may see) -- by username. ✎ /
    # double click and ✕ name the account; the two handlers below look it up
    # with .fila_usuario() as before. "#" (account code) for staff only.
    usuarios_t <- tiers_usuarios_server("usuarios", usuarios = usuarios_rv, permitido = req_authorized,
                                        con_codigo = reactive(isTRUE(is_hopdesk()) || isTRUE(is_principal())))

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
      tier_keys <- niveles_asignables(viewer_tier, viewer_is_staff)

      choices <- setNames(tier_keys, vapply(tier_keys, nivel_nombre, character(1)))
      selectInput(ns("new_tier"), "Tier", choices = choices, selected = nivel_alta(), width = "100%")
    })

    observeEvent(input$btn_save_new_user, {
      display_name <- trimws(input$new_display_name %||% "")
      username     <- tolower(trimws(input$new_username %||% ""))
      password     <- input$new_password  %||% ""
      password2    <- input$new_password2 %||% ""
      tier         <- input$new_tier      %||% ""

      if (!nzchar(display_name)) {
        showNotification(tr("niveles.usuarios.aviso.nombre_mostrar_obligatorio"), type = "error"); return()
      }
      # T31: name and job title are required for every new account, with
      # the same checks as the activation page.
      err_dato <- c(nombre_error(display_name), puesto_error(input$new_puesto))
      if (length(err_dato)) {
        showNotification(tr(err_dato[[1]]), type = "error"); return()
      }
      if (!nzchar(username) || !grepl("^[a-z0-9_]+$", username)) {
        showNotification(tr("niveles.usuarios.aviso.usuario_invalido"),
                         type = "error"); return()
      }
      # The one password rule (T31): 8 to 128, a digit and a symbol.
      err_pass <- contrasena_error(password)
      if (!is.null(err_pass)) {
        showNotification(tr(err_pass), type = "error"); return()
      }
      if (!identical(password, password2)) {
        showNotification(tr("niveles.usuarios.aviso.contrasenas_distintas"), type = "error"); return()
      }
      # ── HARD WALL: tier assignment gates ────────────────────────────────────────
      # Generic rank-based rule: a viewer may only assign a tier at or below
      # their own rank, and an internal_only (staff) tier only if the viewer
      # session itself is_staff. Enforced here regardless of what the
      # dropdown (UI layer, new_tier_ui above) offered — a forged request
      # cannot bypass it. Anything not assignable is refused.
      viewer_is_staff <- isTRUE(shared$current_user_info()$is_staff)
      if (!tier_assignment_allowed(tier, current_tier(), viewer_is_staff)) {
        showNotification(tr("niveles.usuarios.aviso.nivel_no_permitido"), type = "error"); return()
      }

      all_u <- all_usuarios_rv()
      if (!is.null(all_u) && username %in% tolower(all_u$username)) {
        showNotification(tr("niveles.usuarios.aviso.usuario_existe"), type = "error"); return()
      }
      # NA: the global index could not be read, so the name is not verified (CM18)
      disponible <- check_username_available(username)
      if (is.na(disponible)) {
        showNotification(tr("niveles.hdadmin.no_se_leyo"), type = "error", duration = 6); return()
      }
      if (!isTRUE(disponible)) {
        showNotification(tr("niveles.usuarios.aviso.usuario_registrado"),
                         type = "error", duration = 6); return()
      }

      new_row <- data.frame(
        id            = uuid::UUIDgenerate(),
        account_code  = .next_account_code(all_u),
        username      = username,
        password_hash = hash_password(password),
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
      new_row$puesto              <- texto_limpio(input$new_puesto)
      new_row$password_changed_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

      # T31: reserve the name in the global index, save, and give the name
      # back if the save fails (guardar_cuenta_nueva). The check above is a
      # courtesy; this is the one that counts, read inside the change.
      updated <- .hd_admin_agregar(all_u, new_row)
      saved <- tryCatch({ guardar_cuenta_nueva(all_u, new_row, current_client_id(),
                                               ocupados = .usuarios_en_credenciales()); "ok" },
                        usuario_ocupado = function(e) "ocupado",
                        error = function(e) { message("[TIERS] create user save failed: ", e$message); "error" })
      if (identical(saved, "ocupado")) {
        showNotification(tr("niveles.usuarios.aviso.usuario_registrado"), type = "error", duration = 6)
        return()
      }
      if (!identical(saved, "ok")) {
        showNotification(tr("niveles.usuarios.aviso.no_guardada"),
                         type = "error", duration = 6)
        return()
      }
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
      updateTextInput(session, "new_puesto",        value = "")
      showNotification(tr("niveles.usuarios.aviso.creada", usuario = username, codigo = as.character(new_row$account_code)),
                       type = "message", duration = 3)
      log_action(
        user        = shared$current_user(),
        module      = "usuarios",
        action      = "crear_usuario",
        description = paste0("Cuenta '", username, "' creada (tier: ", tier, ")"),
        target_id   = new_row$account_code,
        s3_key      = paste0(current_client_id(), "/usuarios.rds"),
        client_id             = current_client_id(),
        viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL),
        metadata    = list(tier = tier, display_name = display_name)
      )
      activity_refresh(activity_refresh() + 1L)
    })

    # ── Delete account ────────────────────────────────────────────────────────
    # pending_del_user holds the username awaiting confirmation
    pending_del_user <- reactiveVal(NULL)

    observeEvent(usuarios_t$eliminar(), {
      usuarios <- usuarios_rv()
      idx      <- .fila_usuario(usuarios, usuarios_t$eliminar()$id)
      req(!is.null(idx))

      row      <- usuarios[idx, ]
      uname    <- row$username
      dname    <- row$display_name %||% uname
      tier     <- row$tier

      # Constraint pre-check: protected tiers need at least one account alive
      all_u      <- all_usuarios_rv()
      protegido  <- nivel_unico_vivo(tier)
      sin_fila   <- all_u[all_u$username != uname, , drop = FALSE]
      if (length(usuarios_niveles_perdidos(all_u, sin_fila))) {
        showNotification(
          tr("niveles.usuarios.aviso.no_eliminar_ultima", usuario = uname, nivel = nivel_nombre(tier)),
          type = "error", duration = 5)
        return()
      }

      pending_del_user(uname)

      # Warn extra loudly when deleting a protected account
      extra_warning <- if (protegido) {
        tags$p(class = "text-danger fw-semibold mb-0",
               icon("triangle-exclamation"), " Esta es una cuenta ", nivel_nombre(tier), ". Quedar\u00e1n ",
               usuarios_vivos_nivel(sin_fila, tier), " cuenta(s) de ese nivel despu\u00e9s de eliminar.")
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
            nivel_nombre(tier))
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
      uname <- pending_del_user()
      req(!is.null(uname))
      removeModal()

      # The account named in the modal, wherever its row is now.
      usuarios <- usuarios_rv()
      idx      <- .fila_usuario(usuarios, uname)
      if (is.null(idx)) {
        pending_del_user(NULL)
        showNotification(tr("niveles.usuarios.ya_no_esta"), type = "warning", duration = 5)
        return()
      }

      target_user <- usuarios[idx, "username"]
      target_tier <- usuarios[idx, "tier"]
      all_u       <- all_usuarios_rv()

      # Final constraint guard (race-condition safety — re-check at save time)
      if (nivel_unico_vivo(target_tier)) {
        if (length(usuarios_niveles_perdidos(all_u, all_u[all_u$username != target_user, , drop = FALSE]))) {
          showNotification(
            tr("niveles.usuarios.aviso.eliminacion_cancelada", nivel = nivel_nombre(target_tier)),
            type = "error", duration = 5)
          pending_del_user(NULL)
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
      saved <- tryCatch({ auth_save_usuarios(all_u, client_id = current_client_id(),
                                             base = all_usuarios_rv()); TRUE },
                        error = function(e) { message("[TIERS] delete save failed: ", e$message); FALSE })
      if (!saved) {
        showNotification(tr("niveles.usuarios.aviso.cambio_no_guardado"),
                         type = "error", duration = 6)
        pending_del_user(NULL)
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
      pending_del_user(NULL)
      selected_user(NULL)
      showNotification(tr("niveles.usuarios.aviso.desactivada", usuario = target_user),
                       type = "message", duration = 4)
      log_action(
        user        = shared$current_user(),
        module      = "usuarios",
        action      = "eliminar_usuario",
        description = paste0("Cuenta '", target_user, "' eliminada (tier: ", target_tier, ")"),
        target_id   = target_user,
        s3_key      = paste0(current_client_id(), "/usuarios.rds"),
        client_id             = current_client_id(),
        viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL),
        metadata    = list(tier = target_tier)
      )
      activity_refresh(activity_refresh() + 1L)
    })

    # ── Perm editor ───────────────────────────────────────────────────────────
    # The username being edited, never a row position.
    selected_user <- reactiveVal(NULL)

    observeEvent(usuarios_t$editar(), { selected_user(usuarios_t$editar()$id) })
    observeEvent(input$btn_cancel_perms, { selected_user(NULL) })

    output$perm_editor_ui <- renderUI({
      usuarios <- usuarios_rv()
      idx      <- .fila_usuario(usuarios, selected_user())
      req(!is.null(idx))
      registry <- tryCatch(read_client_registry(), error = function(e) .schema_client_registry())
      .tiers_perm_editor_ui(ns, usuarios[idx, ], viewer_tier = current_tier(),
                            config = tier_config_rv(),
                            registry = registry,
                            viewer_is_staff = isTRUE(tryCatch(shared$current_user_info()$is_staff,
                                                              error = function(e) FALSE)))
    })

    observeEvent(input$edit_tier, {
      req(!is.null(selected_user()))
      base_perms <- get_tier_defaults(input$edit_tier)
      for (k in perm_visible_keys(isTRUE(tryCatch(shared$current_user_info()$is_staff,
                                                  error = function(e) FALSE))))
        updateCheckboxInput(session, paste0("perm_", k), value = isTRUE(base_perms[[k]]))
    }, ignoreInit = TRUE)

    observeEvent(input$btn_save_perms, {
      req(!is.null(selected_user()))
      usuarios <- usuarios_rv()
      idx      <- .fila_usuario(usuarios, selected_user())
      if (is.null(idx)) {
        selected_user(NULL)
        showNotification(tr("niveles.usuarios.ya_no_esta"), type = "warning", duration = 5)
        return()
      }

      original_tier <- usuarios[idx, "tier"]
      new_tier      <- input$edit_tier %||% original_tier

      # Safety guards — tier assignment walls (same generic rank/internal_only
      # rule as the create-user handler above; see comment there).
      viewer_is_staff <- isTRUE(shared$current_user_info()$is_staff)
      if (!tier_assignment_allowed(new_tier, current_tier(), viewer_is_staff)) {
        showNotification(tr("niveles.usuarios.aviso.nivel_no_permitido"), type = "error"); return()
      }

      base_perms <- get_tier_defaults(new_tier)

      # Permissions this viewer never saw were never rendered, so their inputs
      # do not exist. Reading them would yield NULL -> isTRUE(NULL) == FALSE,
      # silently rewriting a staff permission to FALSE every time a client
      # admin edits any user. Carry those stored overrides through untouched
      # and only recompute the ones actually on screen.
      vis_keys  <- perm_visible_keys(viewer_is_staff)
      prev_ovr  <- tryCatch(
        as.list(jsonlite::fromJSON(usuarios[idx, "permisos"] %||% "{}")),
        error = function(e) list()
      )
      overrides <- prev_ovr[setdiff(names(prev_ovr), vis_keys)]

      for (k in vis_keys) {
        if (k %in% permisos_bloqueados(new_tier)) next
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
        # The one password rule (T31): only a NEW password is checked.
        err_pass <- contrasena_error(new_pass)
        if (!is.null(err_pass)) {
          showNotification(tr(err_pass), type = "error"); return()
        }
        if (!identical(new_pass, new_pass2)) {
          showNotification(tr("niveles.usuarios.aviso.contrasenas_distintas"), type = "error"); return()
        }
      }

      # The user name is NOT edited here (T31): it is the key of the audit
      # log, the notices and the preferences, and unique in all of HopDesk.
      # input$edit_username is never read, so a forged value changes nothing.
      # Name, job title and e-mail: checked only when they change, so an
      # old account whose fields are empty can still be edited; job title
      # and e-mail may be left empty (Luis fills them in later).
      .viejo <- function(col) if (length(full_idx) == 1 && col %in% names(all_u)) all_u[[col]][full_idx] else NA
      new_display <- trimws(input$edit_display_name %||% "")
      new_puesto  <- trimws(input$edit_puesto %||% "")
      new_email   <- trimws(input$edit_email %||% "")
      err_dato <- c(
        if (nzchar(new_display) && !identical(new_display, .viejo("display_name"))) nombre_error(new_display),
        if (nzchar(new_puesto) && !identical(new_puesto, .viejo("puesto"))) puesto_error(new_puesto),
        if (nzchar(new_email) && !identical(tolower(new_email), .viejo("email"))) correo_error(new_email))
      if (length(err_dato)) {
        showNotification(tr(err_dato[[1]]), type = "error"); return()
      }

      if (length(full_idx) == 1) {
        # Basic user fields
        if (nzchar(new_display)) all_u[full_idx, "display_name"] <- new_display
        # A field that never reached the server (NULL) leaves its value alone.
        if (!is.null(input$edit_puesto))
          all_u[full_idx, "puesto"] <- if (nzchar(new_puesto)) new_puesto else NA_character_
        if (!is.null(input$edit_email))
          all_u[full_idx, "email"]  <- if (nzchar(new_email)) correo_normalizar(new_email) else NA_character_
        if (nzchar(new_pass)) {
          all_u[full_idx, "password_hash"]       <- hash_password(new_pass)
          all_u[full_idx, "password_changed_at"] <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        }
        all_u[full_idx, "activo"] <- isTRUE(input$edit_activo)

        all_u[full_idx, "tier"]     <- new_tier
        all_u[full_idx, "permisos"] <- jsonlite::toJSON(overrides, auto_unbox = TRUE)

        # Save allowed_clients if the field was shown in the editor
        if (!is.null(input$edit_allowed_clients) && is_principal()) {
          all_u[full_idx, "allowed_clients"] <- jsonlite::toJSON(
            as.character(input$edit_allowed_clients), auto_unbox = FALSE)
        }

        # Protected tiers: demoting or deactivating the last live account of
        # the tier is refused (auth_save_usuarios() refuses it too).
        perdidos <- usuarios_niveles_perdidos(all_usuarios_rv(), all_u)
        if (length(perdidos)) {
          showNotification(
            tr("niveles.usuarios.aviso.falta_nivel_activo", nivel = nivel_nombre(perdidos[[1]])),
            type = "error", duration = 5)
          return()
        }

        saved <- tryCatch({ auth_save_usuarios(all_u, client_id = current_client_id(),
                                               base = all_usuarios_rv()); TRUE },
                          error = function(e) { message("[TIERS] save perms failed: ", e$message); FALSE })
        if (!saved) {
          showNotification(tr("niveles.usuarios.aviso.permisos_no_guardados"),
                           type = "error", duration = 6)
          return()
        }
        .usuarios_local(all_u)
        tryCatch(shared$permisos_version(shared$permisos_version() + 1L), error = function(e) NULL)
      }

      selected_user(NULL)
      showNotification(tr("niveles.usuarios.aviso.cambios_guardados"), type = "message", duration = 2)
      log_action(
        user        = shared$current_user(),
        module      = "usuarios",
        action      = "editar_usuario",
        description = paste0("Permisos/tier de '", target_user, "' actualizados"),
        target_id   = target_user,
        s3_key      = paste0(current_client_id(), "/usuarios.rds"),
        client_id             = current_client_id(),
        viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL),
        metadata    = list(tier = new_tier, overrides_count = length(overrides))
      )
      activity_refresh(activity_refresh() + 1L)
    })

    # ── Tier config section ───────────────────────────────────────────────────
    # Client tiers only: the config is the client's own, staff tiers are not its to set.
    TIER_META <- setNames(lapply(niveles_cliente(), nivel_nombre), niveles_cliente())

    selected_tier_rv <- reactiveVal(nivel_ti())

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

      # Depends on the tier only: the locked accounts are a table of the
      # engine (tiers_bloqueadas_server), refreshed through its datos(), so
      # ↻ or a lock never rebuilds it (T28 P6).
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
          div(class = "card-body p-0", tiers_bloqueadas_ui(ns("bloq")))
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
        showNotification(tr("niveles.bloqueadas.aviso.falta_usuario"), type = "error"); return()
      }
      if (!nzchar(reason)) {
        showNotification(tr("niveles.bloqueadas.aviso.falta_motivo"), type = "error"); return()
      }
      current_viewer <- shared$current_user() %||% "unknown"
      existing <- tryCatch(read_emergency_lock(), error = function(e) .schema_emergency_lock())
      if (is.null(existing)) existing <- .schema_emergency_lock()
      if (any(tolower(existing$username) == username)) {
        showNotification(tr("niveles.bloqueadas.aviso.ya_bloqueado", usuario = username), type = "warning"); return()
      }
      new_row <- data.frame(
        username  = username,
        locked_at = Sys.time(),
        locked_by = current_viewer,
        reason    = reason,
        stringsAsFactors = FALSE
      )
      tryCatch({
        ya <- FALSE
        modificar_emergency_lock(function(l) {
          if (any(tolower(l$username) == username)) { ya <<- TRUE; return(l) }
          .hd_admin_agregar(l, new_row)
        })
        if (ya) {
          showNotification(tr("niveles.bloqueadas.aviso.ya_bloqueado", usuario = username), type = "warning"); return()
        }
        security_lock_refresh(security_lock_refresh() + 1L)
        updateSelectizeInput(session, "sec_lock_user", selected = character(0))
        updateTextInput(session, "sec_lock_reason", value = "")
        showNotification(tr("niveles.bloqueadas.aviso.bloqueada", usuario = username),
                         type = "message", duration = 4)
        log_action(user        = shared$current_user(),
                   module      = "seguridad",
                   action      = "emergency_lock",
                   description = paste0("Lock applied to '", username, "' — reason: ", reason),
                   target_id   = username,
                   s3_key      = "hd-admin/emergency_lock.rds",
                   client_id             = "hd-admin",
                   viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL))
        activity_refresh(activity_refresh() + 1L)
      }, error = function(e) {
        showNotification(tr("niveles.bloqueadas.aviso.error_bloquear", error = e$message), type = "error")
      })
    }, ignoreInit = TRUE)

    # ── Clientes panel (principal + hopdesk) ─────────────────────────────────

    clients_refresh         <- reactiveVal(0L)
    requests_refresh        <- reactiveVal(0L)
    activity_refresh        <- reactiveVal(0L)
    solicitar_usuarios_sent <- reactiveVal(FALSE)  # session-level: TRUE after request sent

    # Seguridad › Cuentas bloqueadas: the table and its 🔓 (T28). Here, once
    # activity_refresh exists. Principal only, checked on the server.
    tiers_bloqueadas_server("bloq", shared, permitido = is_principal,
                            refresh = security_lock_refresh, actividad = activity_refresh)

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
        # Was filtering on hd_staff$can_manage_invites, which is a resolved
        # permission and not a column -- so this list was always empty and
        # the request never reached anyone. See hd_staff_emails_with_perm().
        staff_emails <- hd_staff_emails_with_perm("can_manage_invites")
        if (length(staff_emails)) {
          html <- email_user_request_html(dname, cur, max, contact)
          subj <- sprintf("[HopDesk] Solicitud de más usuarios — %s", dname)
          for (addr in staff_emails) send_email(addr, subj, html)
        }
      }, error = function(e) message("[LIMIT] user_request email failed: ", e$message))

      solicitar_usuarios_sent(TRUE)
      showNotification(
        tr("niveles.solicitudes.aviso.enviada_equipo"),
        type = "message", duration = 6
      )
      log_action(
        user        = shared$current_user(),
        module      = "usuarios",
        action      = "solicitar_usuarios",
        description = sprintf("'%s' solicitó aumento de límite (%d/%d)", dname, cur, max),
        target_id   = cid,
        s3_key      = paste0(cid, "/app_audit.rds"),
        client_id             = cid,
        viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL)
      )
      activity_refresh(activity_refresh() + 1L)
    }, ignoreInit = TRUE)

    output$clients_section <- renderUI({
      if (!isTRUE(is_hopdesk()) && !isTRUE(is_principal())) {
        return(div(class = "alert alert-warning mt-3",
                   icon("lock"), " Esta sección solo está disponible para Hopdesk y Principal."))
      }

      # Depends on the tier only: the pending requests and the registered
      # clients are tables of the engine (tiers_solicitudes_server,
      # tiers_clientes_server), refreshed through requests_refresh and
      # clients_refresh in their own datos() (T28 P6).
      ns <- session$ns

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
        # Always there for Principal, with its own "none" message (T28 P8).
        if (isTRUE(is_principal())) {
          div(class = "card border-warning shadow-sm mb-3",
            div(class = "card-header py-2 fw-semibold",
                style = "background:#fff3cd;",
                tagList(icon("clock"), " ", tr("niveles.tablas.solicitudes.titulo"))),
            div(class = "card-body p-0", tiers_solicitudes_ui(ns("solic")))
          )
        },

        # ── Client registry table ─────────────────────────────────────────
        div(class = "card border-0 shadow-sm",
          div(class = "card-header py-2 fw-semibold",
              tagList(icon("list"), " ", tr("niveles.tablas.clientes.titulo"))),
          div(class = "card-body p-0", tiers_clientes_ui(ns("clientes"))))
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
        showNotification(tr("niveles.solicitudes.aviso.id_invalido"),
                         type = "error"); return()
      }
      if (!nzchar(dname)) {
        showNotification(tr("niveles.solicitudes.aviso.nombre_obligatorio"), type = "error"); return()
      }

      existing_reg <- tryCatch(read_client_registry(), error = function(e) .schema_client_registry())
      if (any(existing_reg$client_id == cid_prop)) {
        showNotification(tr("niveles.solicitudes.aviso.id_existe", id = cid_prop),
                         type = "error"); return()
      }
      existing_req <- tryCatch(read_client_requests(), error = function(e) .schema_client_requests())
      if (any(existing_req$client_id_proposed == cid_prop &
              existing_req$status == "pending")) {
        showNotification(tr("niveles.solicitudes.aviso.pendiente", id = cid_prop),
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

      # On S3's current requests (CM13); the pending check runs again there.
      dup <- FALSE
      saved <- tryCatch({
        modificar_client_requests(function(r) {
          if (any(r$client_id_proposed == cid_prop & r$status == "pending")) { dup <<- TRUE; return(r) }
          .hd_admin_agregar(r, new_req)
        }); TRUE
      }, error = function(e) { message("[CLIENTS] request save failed: ", e$message); FALSE })

      if (!saved) {
        showNotification(tr("niveles.solicitudes.aviso.error_guardar"), type = "error")
        return()
      }
      if (dup) {
        showNotification(tr("niveles.solicitudes.aviso.pendiente", id = cid_prop),
                         type = "warning"); return()
      }
      removeModal()
      requests_refresh(requests_refresh() + 1L)
      showNotification(tr("niveles.solicitudes.aviso.enviada", id = cid_prop),
                       type = "message", duration = 4)
      log_action(user        = shared$current_user(),
                 module      = "clientes",
                 action      = "solicitud_nuevo_cliente",
                 description = paste0("Solicitud para '", cid_prop, "' (", dname, ")"),
                 target_id   = cid_prop,
                 s3_key      = "hd-admin/client_requests.rds",
                 client_id             = "hd-admin",
                 viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL))
      activity_refresh(activity_refresh() + 1L)
    }, ignoreInit = TRUE)

    # ── Review request modal (principal only) ─────────────────────────────────
    # Opened by ℹ or a double click on a pending request (T28): the table
    # names the request by its id.
    solicitudes_t <- tiers_solicitudes_server("solic", permitido = is_principal, refresh = requests_refresh)
    observeEvent(solicitudes_t$revisar(), {
      req(is_principal())
      rid <- solicitudes_t$revisar()$id
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
      req(isTRUE(staff_perms()$can_approve_clients))
      rid <- session$userData$reviewing_req_id %||% ""
      req(nzchar(rid))

      reqs <- tryCatch(read_client_requests(), error = function(e) NULL)
      req(!is.null(reqs))
      idx <- which(reqs$id == rid)
      req(length(idx) == 1)
      row <- reqs[idx, , drop = FALSE]
      reviewer <- shared$current_user() %||% "unknown"

      # 0. The registry, read strictly: if it cannot be read, nothing is done
      #    (CM13); if the client already exists, its folder must not be reset.
      existe <- tryCatch(
        row$client_id_proposed %in% leer_hd_admin("client_registry.rds", .schema_client_registry)$client_id,
        error = function(e) { message("[CLIENTS] registry read failed: ", e$message); NA })
      if (is.na(existe)) {
        showNotification(tr("niveles.hdadmin.no_se_leyo"), type = "error"); return()
      }
      if (existe) {
        showNotification(tr("niveles.hdadmin.cliente_existe", cliente = row$client_id_proposed),
                         type = "error"); return()
      }

      # 1. Initialize S3 folder
      init_ok <- tryCatch({
        initialize_client_folder(row$client_id_proposed, created_by = reviewer); TRUE
      }, error = function(e) {
        message("[CLIENTS] initialize_client_folder failed: ", e$message); FALSE
      })
      if (!init_ok) {
        showNotification(tr("niveles.solicitudes.aviso.error_folder"), type = "error")
        return()
      }

      # 2. Add to client_registry, on S3's current registry
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
        modificar_client_registry(function(registry) {
          if (new_entry$client_id %in% registry$client_id) return(registry)
          .hd_admin_agregar(registry, new_entry)
        }); TRUE
      }, error = function(e) { message("[CLIENTS] registry save failed: ", e$message); FALSE })
      if (!saved_reg) {
        showNotification(tr("niveles.solicitudes.aviso.error_registro"), type = "error"); return()
      }

      # 3. Update request status
      req_ok <- tryCatch({
        modificar_client_requests(function(r) {
          i <- which(r$id == rid)
          r$status[i]      <- "approved"
          r$reviewed_by[i] <- reviewer
          r$reviewed_at[i] <- as.character(Sys.time())
          r
        }); TRUE
      }, error = function(e) { message("[CLIENTS] request update failed: ", e$message); FALSE })
      # The client exists already; only the request's status could not be saved.
      if (!req_ok) showNotification(tr("niveles.hdadmin.no_se_guardo"), type = "warning", duration = 6)

      removeModal()
      clients_refresh(clients_refresh()   + 1L)
      requests_refresh(requests_refresh() + 1L)
      showNotification(tr("niveles.solicitudes.aviso.aprobado", id = row$client_id_proposed),
                       type = "message", duration = 5)
      # client_id is the NEW client (row$client_id_proposed), not "hd-admin" --
      # initialize_client_folder() above has already created that client's
      # own app_audit.rds. viewer_home_client_id stays the reviewer's real
      # home (hd-admin for staff/principal); log_action()'s own dual-write
      # (home_cid != cid) then files this in BOTH hd-admin's log (Hopdesk's
      # own record of "we approved this client") AND the new client's own
      # log -- otherwise their own genesis event, the one entry that would
      # explain their own existence, was never visible to them (found
      # 2026-09-04 auditing Grupo/tiers_module.R activity logging).
      log_action(user        = reviewer,
                 module      = "clientes",
                 action      = "aprobacion_cliente",
                 description = paste0("Cliente '", row$client_id_proposed,
                                      "' (", row$display_name, ") aprobado"),
                 target_id   = row$client_id_proposed,
                 s3_key      = "hd-admin/client_registry.rds",
                 client_id             = row$client_id_proposed,
                 viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL))
      activity_refresh(activity_refresh() + 1L)
      session$userData$reviewing_req_id <- NULL
    }, ignoreInit = TRUE)

    observeEvent(input$btn_reject_req, {
      req(isTRUE(staff_perms()$can_approve_clients))
      rid <- session$userData$reviewing_req_id %||% ""
      req(nzchar(rid))
      reason   <- trimws(input$review_rejection_reason %||% "")
      reviewer <- shared$current_user() %||% "unknown"

      reqs <- tryCatch(read_client_requests(), error = function(e) NULL)
      req(!is.null(reqs))
      idx <- which(reqs$id == rid)
      req(length(idx) == 1)
      row <- reqs[idx, , drop = FALSE]

      saved <- tryCatch({
        modificar_client_requests(function(r) {
          i <- which(r$id == rid)
          r$status[i]           <- "rejected"
          r$reviewed_by[i]      <- reviewer
          r$reviewed_at[i]      <- as.character(Sys.time())
          r$rejection_reason[i] <- if (nzchar(reason)) reason else NA_character_
          r
        }); TRUE
      },
                        error = function(e) { message("[CLIENTS] reject save failed: ", e$message); FALSE })
      if (!saved) {
        showNotification(tr("niveles.solicitudes.aviso.error_rechazo"), type = "error"); return()
      }
      removeModal()
      requests_refresh(requests_refresh() + 1L)
      showNotification(tr("niveles.solicitudes.aviso.rechazada", id = row$client_id_proposed),
                       type = "warning", duration = 4)
      log_action(user        = reviewer,
                 module      = "clientes",
                 action      = "rechazo_cliente",
                 description = paste0("Solicitud '", row$client_id_proposed, "' rechazada",
                                      if (nzchar(reason)) paste0(" — motivo: ", reason) else ""),
                 target_id   = row$client_id_proposed,
                 s3_key      = "hd-admin/client_requests.rds",
                 client_id             = "hd-admin",
                 viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL))
      activity_refresh(activity_refresh() + 1L)
      session$userData$reviewing_req_id <- NULL
    }, ignoreInit = TRUE)

    # ── Client edit modal (display_name, contact_email, max_users) ───────────
    pending_client_edit <- reactiveVal(NULL)   # stores validated edit before confirmation

    # The registered clients' table (T28): ✎ and 📇 name the client by its id.
    clientes_t <- tiers_clientes_server("clientes", refresh = clients_refresh,
                                        permitido = reactive(isTRUE(is_hopdesk()) || isTRUE(is_principal())))

    observeEvent(clientes_t$editar(), {
      req(is_hopdesk() || is_principal())
      cid <- clientes_t$editar()$id
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
        showNotification(tr("niveles.clientes.aviso.nombre_vacio"), type = "error"); return()
      }
      if (is.na(new_max) || new_max < 1L) {
        showNotification(tr("niveles.clientes.aviso.limite_minimo"), type = "error"); return()
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
            tr("niveles.clientes.aviso.maximo_cuentas", n = as.character(new_max)), type = "error")
          return()
        }
        usuarios_orig <- tryCatch(
          auth_load_usuarios(client_id = cid), error = function(e) NULL)
        if (is.null(usuarios_orig)) {
          showNotification(tr("niveles.clientes.aviso.error_leer_usuarios"), type = "error"); return()
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
        perdidos <- usuarios_niveles_perdidos(usuarios_orig, usuarios_new)
        if (length(perdidos)) {
          showNotification(
            tr("niveles.clientes.aviso.falta_nivel", nivel = nivel_nombre(perdidos[[1]])),
            type = "error", duration = 6)
          return()
        }
        ok_users <- tryCatch({
          auth_save_usuarios(usuarios_new, client_id = cid, base = usuarios_orig); TRUE
        }, error = function(e) { message("[CLIENTS] deact save failed: ", e$message); FALSE })
        if (!ok_users) {
          showNotification(tr("niveles.clientes.aviso.error_desactivar"),
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
                     client_id             = cid,
                     viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL))
        }
      }

      # ── Step B: update registry (max_users + display_name + contact_email) ─
      # On S3's current registry (CM13): a failed read saves nothing.
      reg_found <- FALSE
      ok_reg <- tryCatch({
        modificar_client_registry(function(registry) {
          reg_idx <- which(registry$client_id == cid)
          if (!length(reg_idx)) return(registry)
          reg_found <<- TRUE
          registry$display_name[reg_idx]  <- p$display_name
          registry$contact_email[reg_idx] <- p$contact_email
          registry$max_users[reg_idx]     <- new_max
          registry
        }); TRUE
      }, error = function(e) { message("[CLIENTS] reg save failed: ", e$message); FALSE })
      if (ok_reg && !reg_found) {
        if (isTRUE(p$needs_deact)) {
          tryCatch(auth_save_usuarios(usuarios_orig, client_id = cid, base = usuarios_new),
                   error = function(e) message("[CLIENTS] rollback failed: ", e$message))
        }
        showNotification(tr("niveles.clientes.aviso.no_encontrado"), type = "error"); return()
      }
      if (!ok_reg) {
        # Rollback Step A: restore original usuarios
        if (isTRUE(p$needs_deact)) {
          tryCatch(auth_save_usuarios(usuarios_orig, client_id = cid, base = usuarios_new),
                   error = function(e) message("[CLIENTS] rollback failed: ", e$message))
        }
        showNotification(tr("niveles.clientes.aviso.error_revertido"),
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
                 client_id             = "hd-admin",
                 viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL),
                 metadata    = limit_meta)
      log_action(user        = reviewer,
                 module      = "clientes",
                 action      = "cambio_limite_usuarios",
                 description = sprintf("Límite cambiado de %d a %d por %s",
                                       old_max, new_max, reviewer),
                 target_id   = cid,
                 s3_key      = paste0(cid, "/app_audit.rds"),
                 client_id             = cid,
                 viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL),
                 metadata    = limit_meta)

      removeModal()
      pending_client_edit(NULL)
      clients_refresh(clients_refresh() + 1L)
      activity_refresh(activity_refresh() + 1L)
      showNotification(
        tr("niveles.clientes.aviso.limite_actualizado", cliente = p$display_name, n = as.character(new_max)),
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

    # The open client's active contacts (T28): ⊘ desactivar, confirmed in the
    # bar, names the contact by its id.
    contactos_t <- tiers_contactos_server("contactos", cliente = contacts_client_rv, refresh = contacts_refresh,
                                          permitido = reactive(isTRUE(is_hopdesk()) || isTRUE(is_principal())))

    observeEvent(clientes_t$contactos(), {
      req(is_hopdesk() || is_principal())
      cid <- clientes_t$contactos()$id
      req(nzchar(cid %||% ""))
      contacts_client_rv(cid)
      contacts_refresh(contacts_refresh() + 1L)
      ns <- session$ns

      registry <- tryCatch(read_client_registry(), error = function(e) .schema_client_registry())
      row      <- registry[registry$client_id == cid, , drop = FALSE]
      dname    <- if (nrow(row)) row$display_name[1] else cid

      # hd_modal (T28 P5): almost the whole window, ✕ at the top. The table
      # is fixed (its datos() follows contacts_refresh); the form below is
      # never rebuilt -- adding a contact empties it.
      showModal(hd_modal(
        titulo = tagList(icon("address-book"), " ",
                         gsub("{cliente}", dname, tr("niveles.tablas.contactos.titulo"), fixed = TRUE)),
        div(class = "mb-3", tiers_contactos_ui(ns("contactos"))),
        .tiers_contactos_form(ns)
      ))
    }, ignoreInit = TRUE)

    .tiers_contactos_form <- function(ns) {
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
    }

    observeEvent(input$btn_add_contact, {
      req(is_hopdesk() || is_principal())
      cid   <- contacts_client_rv()
      req(nzchar(cid %||% ""))
      name  <- trimws(input$contact_name  %||% "")
      email <- trimws(input$contact_email %||% "")
      role  <- trimws(input$contact_role  %||% "")
      alerts <- isTRUE(input$contact_limit_alerts)

      if (!nzchar(name))  { showNotification(tr("niveles.contactos.aviso.nombre_obligatorio"), type = "error"); return() }
      if (!nzchar(email)) { showNotification(tr("niveles.contactos.aviso.email_obligatorio"), type = "error"); return() }
      if (!grepl("@", email)) { showNotification(tr("niveles.contactos.aviso.email_invalido"), type = "error"); return() }

      saved <- tryCatch({
        add_contact(cid, name = name, email = email, role = role,
                    receives_limit_alerts = alerts); TRUE
      }, error = function(e) { message("[CONTACTS] add failed: ", e$message); FALSE })

      if (!saved) {
        showNotification(tr("niveles.contactos.aviso.error_guardar"), type = "error"); return()
      }
      contacts_refresh(contacts_refresh() + 1L)
      updateTextInput(session, "contact_name",  value = "")
      updateTextInput(session, "contact_email", value = "")
      updateTextInput(session, "contact_role",  value = "")
      showNotification(tr("niveles.contactos.aviso.agregado", nombre = name),
                       type = "message", duration = 3)
      # client_id must be the TARGET client (cid), not "hd-admin" -- contacts
      # genuinely live at <cid>/contacts.rds (s3_key below already says so
      # correctly); this was hard-coded wrong, so the target client's own
      # contact changes never appeared in their own Actividad log and
      # instead got misfiled as if they were a Hopdesk-internal action
      # (found 2026-09-04 auditing Grupo/tiers_module.R activity logging).
      log_action(user        = shared$current_user(),
                 module      = "clientes",
                 action      = "agregar_contacto",
                 description = paste0("Contacto '", name, "' (", email, ") agregado a '", cid, "'"),
                 target_id   = cid,
                 s3_key      = paste0(cid, "/contacts.rds"),
                 client_id             = cid,
                 viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL))
      activity_refresh(activity_refresh() + 1L)
    }, ignoreInit = TRUE)

    observeEvent(contactos_t$desactivar(), {
      req(is_hopdesk() || is_principal())
      cid        <- contacts_client_rv()
      contact_id <- contactos_t$desactivar()$id
      req(nzchar(cid %||% ""), nzchar(contact_id %||% ""))

      # CM20b: the name comes from the row the table showed (its strict
      # read, by id), not from another lenient read -- that one never
      # failed loudly, so a failed read made the click do nothing at all.
      # If the table moved on meanwhile, the id stands for the name.
      filas  <- tryCatch(contactos_t$datos(), error = function(e) NULL)
      nombre <- if (is.data.frame(filas)) filas$nombre[match(contact_id, filas$id)] else NA_character_
      if (is.na(nombre) || !nzchar(nombre)) nombre <- contact_id

      # TRUE: saved inactive. FALSE: nothing to deactivate (already
      # inactive, e.g. by another session): no success, no log. NA: failed.
      res <- tryCatch(isTRUE(deactivate_contact(cid, contact_id)),
                      error = function(e) { message("[CONTACTS] deactivate failed: ", e$message); NA })

      if (is.na(res)) {
        showNotification(tr("niveles.contactos.desactivar_error"), type = "error"); return()
      }
      contacts_refresh(contacts_refresh() + 1L)
      if (!res) {
        showNotification(tr("niveles.contactos.ya_inactivo", nombre = nombre),
                         type = "message", duration = 3)
        return()
      }
      showNotification(tr("niveles.contactos.desactivado", nombre = nombre),
                       type = "warning", duration = 3)
      # Same fix as agregar_contacto above -- client_id is the target client.
      log_action(user        = shared$current_user(),
                 module      = "clientes",
                 action      = "desactivar_contacto",
                 description = paste0("Contacto '", nombre, "' desactivado de '", cid, "'"),
                 target_id   = cid,
                 s3_key      = paste0(cid, "/contacts.rds"),
                 client_id             = cid,
                 viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL))
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
      # The pending invitations and the email queue are tables of the engine
      # (tiers_invitaciones_server, tiers_cola_server), refreshed through
      # invites_refresh in their own datos(): ↻ or a revoke never rebuilds
      # this form (T28 P6).
      registry <- tryCatch(read_client_registry(), error = function(e) .schema_client_registry())
      active_clients <- registry[registry$status == "active", , drop = FALSE]

      # ── Alcance por tenant ──────────────────────────────────────────────────
      # pending_invites.rds es UN SOLO archivo global en hd-admin/ con una
      # columna client_id por fila. Sin filtrar aquí, el dev de un cliente veía
      # los correos, nombres y tiers de TODOS los demás clientes, y podía
      # elegir cualquier cliente destino -- incluido hd-admin. Staff ve todo
      # porque su trabajo es dar de alta clientes; un cliente ve lo suyo.
      staff_view <- isTRUE(viewer_is_staff())
      own_client <- current_client_id()
      own_label  <- {
        idx <- which(registry$client_id == own_client)
        if (length(idx)) registry$display_name[idx[1]] else own_client
      }

      client_choices <- if (staff_view)
        setNames(active_clients$client_id, active_clients$display_name)
      else
        setNames(own_client, own_label)
      # The invitations a client sees are filtered on the server too, in
      # .invitaciones_filas() (the table's datos()), never in the browser.

      tagList(
        # The delivery mode, checked again on ↻ (its own output, T28).
        uiOutput(ns("inv_estado_correo")),
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
                  # Staff elige el cliente destino porque da de alta clientes.
                  # Un cliente no: su cuenta vive en su carpeta y no sale de
                  # ahí. Se muestra de todas formas, en gris, para que quede
                  # claro dónde va a caer la cuenta. El valor NO se toma del
                  # navegador — ver el handler de btn_send_invite.
                  if (staff_view)
                    selectInput(ns("inv_client_id"), "Cliente destino",
                                choices = client_choices,
                                selected = own_client, width = "100%")
                  else
                    div(
                      class = "mb-3",
                      tags$label(class = "form-label", "Cliente destino"),
                      tags$input(type = "text", class = "form-control",
                                 readonly = NA, value = own_label),
                      tags$div(class = "form-text",
                               "Las invitaciones que creas pertenecen a tu empresa.")
                    ),
                  selectInput(ns("inv_tier"), "Tier inicial",
                              choices = setNames(niveles_invitables_por(current_tier()),
                                                 vapply(niveles_invitables_por(current_tier()), nivel_nombre, character(1))),
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
                div(class = "card-body p-0", tiers_invitaciones_ui(ns("invit")))
              ),
              # ── Email queue panel ──────────────────────────────────────────
              # Sólo staff. La cola vive en hd-admin/email_queue.rds y es
              # global: muestra destinatarios y asuntos de TODOS los clientes.
              # Always there for staff, with its own "empty" message (T28
              # P8): a table of the engine (tiers_cola_server), with ↻ per
              # row and "Reintentar todo" in its bar.
              if (staff_view) {
                div(
                  class = "card border-0 shadow-sm",
                  div(class = "card-header bg-white py-2 fw-semibold",
                      tagList(icon("triangle-exclamation", style = "color:#e65100"), " ",
                              tr("niveles.tablas.cola.titulo"))),
                  div(class = "card-body p-0", tiers_cola_ui(ns("cola")))
                )
              }
            )
          )
        )
      )
    })

    # Delivery mode is invisible from the UI otherwise: in sandbox or with a
    # missing key the form looks identical while nobody receives anything.
    # Its own output, so ↻ (invites_refresh) checks it again -- as when the
    # whole tab was rebuilt -- without rebuilding the form or the tables.
    output$inv_estado_correo <- renderUI({
      invites_refresh()
      esvc <- tryCatch(email_service_status(),
                       error = function(e) list(mode = "unconfigured",
                                                label = "Estado desconocido",
                                                detail = ""))
      div(
        class = paste0("alert d-flex align-items-start gap-2 mt-3 mb-0 py-2 ",
                       if (isTRUE(esvc$mode == "live")) "alert-success" else "alert-warning"),
        icon(if (isTRUE(esvc$mode == "live")) "circle-check" else "triangle-exclamation",
             class = "mt-1"),
        div(
          tags$strong(esvc$label),
          tags$div(class = "small", esvc$detail),
          if (!isTRUE(esvc$mode == "live"))
            tags$div(class = "small text-muted mt-1",
                     "Las invitaciones se siguen creando y el enlace se muestra al enviarlas, ",
                     "pero hay que compartirlo a mano. Ver docs/EMAIL_SERVICE_RUNBOOK.md.")
        )
      )
    })

    # Staff's "Cliente destino": ↻ brings clients approved since the form was
    # drawn (it used to rebuild the form), keeping the one chosen.
    observeEvent(invites_refresh(), {
      req(isTRUE(viewer_is_staff()))
      registry <- tryCatch(read_client_registry(), error = function(e) NULL)
      req(is.data.frame(registry))
      activos <- registry[!is.na(registry$status) & registry$status == "active", , drop = FALSE]
      updateSelectInput(session, "inv_client_id", choices = setNames(activos$client_id, activos$display_name),
                        selected = isolate(input$inv_client_id) %||% current_client_id())
    }, ignoreInit = TRUE)

    observeEvent(input$btn_send_invite, {
      # La UI dejaba entrar a dev (un cliente onboardea a su propio equipo),
      # pero este gate sólo admitía hopdesk/principal: req() abortaba en
      # silencio y el botón "Enviar" NO HACÍA NADA para ese usuario, sin un
      # solo mensaje de error. Los dos gates tienen que decir lo mismo.
      req(isTRUE(is_hopdesk()) || isTRUE(is_principal()) || isTRUE(is_dev()))

      email_in   <- tolower(trimws(input$inv_email        %||% ""))
      dname_in   <- trimws(input$inv_display_name %||% "")

      # ── El alcance de tenant se decide AQUÍ, no en el navegador ────────────
      # Para quien no es staff, input$inv_client_id se ignora por completo.
      # Esconder el desplegable no es seguridad: un Shiny.setInputValue desde
      # la consola del navegador bastaría para sembrar una cuenta dentro de
      # otro cliente, o dentro de hd-admin.
      client_in <- if (isTRUE(viewer_is_staff()))
        trimws(input$inv_client_id %||% "")
      else
        current_client_id()

      if (!nzchar(client_in %||% "")) {
        output$inv_send_error <- renderUI(
          tags$p(class = "text-danger small mt-1", "No se pudo determinar el cliente destino."))
        return()
      }

      # Mismo razonamiento para el tier: el desplegable sólo ofrece estos,
      # así que cualquier otro valor llegó forjado. Sin esto, una invitación
      # podría crear una cuenta 'principal' o 'hopdesk'. El tier de TI solo
      # lo ofrece a principal, y aquí se vuelve a juzgar por su tier real.
      tier_in <- trimws(input$inv_tier %||% "")
      if (!tier_in %in% niveles_invitables_por(current_tier())) {
        output$inv_send_error <- renderUI(
          tags$p(class = "text-danger small mt-1", "Tier no permitido."))
        return()
      }

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
      # The invite exists: empty the form so a second "Enviar" can't send the
      # same person another one. The refresh used to rebuild the form and do
      # this; since T28 it only re-reads the tables. A failed create keeps
      # what was typed, to fix and send again.
      updateTextInput(session, "inv_email", value = "")
      updateTextInput(session, "inv_display_name", value = "")
      # Build the invite URL. NOT from this session's address bar: on
      # shinyapps.io that path carries a /_w_<hex>/ worker segment and
      # shinymanager's ?token=, so the mailed link pointed at the inviter's
      # own worker. app_public_url() prefers HOPDESK_APP_URL and scrubs both.
      invite_url <- invite_activation_url(token, session)

      registry <- tryCatch(read_client_registry(), error = function(e) .schema_client_registry())
      client_name <- {
        idx <- which(registry$client_id == client_in)
        if (length(idx)) registry$display_name[idx[1]] else client_in
      }
      # The NAME of whoever pressed Enviar, so the mail can say who invited
      # them -- an invitation that names its sender is the difference between
      # "I'll click this" and "I'll ask someone first". Only the name: the
      # address is deliberately not read, because the mail carries no
      # Reply-To (see email_service_status()) and these invitations go to
      # people who are not users yet.
      #
      # Looked up in the INVITER's own client, not in client_in: staff invite
      # into tenants they are not members of, so their account lives
      # elsewhere. Best-effort -- failing to resolve it drops the name from
      # the sentence rather than inventing one.
      inviter_name <- tryCatch({
        me  <- shared$current_user() %||% ""
        cid <- shared$current_user_info()$client_id %||% ""
        u   <- auth_load_usuarios(client_id = cid)
        idx <- which(tolower(u$username) == tolower(me) & !(u$deleted %in% TRUE))
        nm  <- if (length(idx)) trimws(u$display_name[idx[1]] %||% "") else ""
        if (is.na(nm) || !nzchar(nm)) me else nm
      }, error = function(e) "")

      html_body <- email_invite_html(dname_in, client_name, invite_url,
                                     inviter_name = inviter_name)
      sent <- send_email(to      = email_in,
                         subject = paste0("Invitación a HopDesk — ", client_name),
                         html    = html_body)
      output$inv_send_error <- renderUI(NULL)

      # The token is valid whether or not the mail went out, so ALWAYS surface
      # the link: if delivery failed, staff can still hand it over by other
      # means instead of the invitee waiting for a mail that never arrives.
      showModal(modalDialog(
        title = if (isTRUE(sent))
          tagList(icon("paper-plane"), " Invitación enviada")
        else
          tagList(icon("triangle-exclamation"), " Invitación creada — correo NO enviado"),
        if (isTRUE(sent)) {
          tags$p("Se envió un correo a ", tags$strong(email_in),
                 ". El enlace expira en 48 horas.")
        } else {
          tagList(
            tags$p(class = "text-danger",
                   "La invitación existe, pero el correo no pudo enviarse: ",
                   tags$strong(email_error_of(sent))),
            tags$p(class = "small text-muted",
                   "Quedó en la cola de emails para reintentar. Mientras tanto, ",
                   "comparte manualmente el enlace de activación:")
          )
        },
        tags$div(
          class = "mt-2",
          tags$label(class = "form-label small fw-semibold", "Enlace de activación"),
          tags$input(type = "text", class = "form-control form-control-sm",
                     readonly = NA, value = invite_url,
                     onclick = "this.select()")
        ),
        footer = modalButton("Cerrar"), easyClose = TRUE, size = "l"
      ))
      invites_refresh(invites_refresh() + 1L)
      # Real gap, not just unlogged noise: this creates a usable signup
      # token bound to a tier and a specific client -- exactly the kind of
      # credential-adjacent event that needs an audit trail. client_id is
      # the TARGET client (client_in), not "hd-admin" -- same reasoning as
      # the clients-tab fixes above, and correct here because
      # pending_invites.rds carries a real per-row client_id even though the
      # file itself is physically stored once, globally, at hd-admin/.
      tryCatch(log_action(
        user        = shared$current_user(),
        module      = "invites",
        action      = "enviar_invitacion",
        description = sprintf("Invitación creada para %s (%s) en '%s', tier %s — correo: %s",
                              email_in, dname_in, client_in, tier_in,
                              if (isTRUE(sent)) "enviado" else
                                paste0("NO enviado (", email_error_of(sent), ")")),
        target_id   = token,
        s3_key      = .INVITES_KEY,
        metadata    = list(email = email_in, display_name = dname_in, tier = tier_in,
                           email_sent = isTRUE(sent),
                           email_mode = email_service_status()$mode),
        client_id             = client_in,
        viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL)
      ), error = function(e) NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$btn_inv_refresh, { invites_refresh(invites_refresh() + 1L) })

    # The pending invitations' table (T28): ⊘, confirmed in its bar, names
    # the invite by its token. Tenant-scoped rows in its datos().
    invitaciones_t <- tiers_invitaciones_server("invit", refresh = invites_refresh,
      permitido  = reactive(isTRUE(is_hopdesk()) || isTRUE(is_principal()) || isTRUE(is_dev())),
      staff      = viewer_is_staff, own_client = current_client_id)

    observeEvent(invitaciones_t$revocar(), {
      req(isTRUE(is_hopdesk()) || isTRUE(is_principal()) || isTRUE(is_dev()))
      token <- invitaciones_t$revocar()$id
      req(nzchar(token %||% ""))
      invites <- tryCatch(read_pending_invites(), error = function(e) NULL)
      req(!is.null(invites))
      idx <- which(invites$token == token)
      if (length(idx)) {
        inv_client <- invites$client_id[idx[1]]
        inv_email  <- invites$email[idx[1]]
        # El token llega del navegador (el id de la fila), así que cualquiera
        # puede mandar uno arbitrario. Sin esta comprobación, un cliente podía
        # revocar la invitación de OTRO cliente con sólo conocer su token.
        if (!isTRUE(viewer_is_staff()) && !identical(inv_client, current_client_id())) {
          showNotification(tr("niveles.invitaciones.aviso.otra_empresa"),
                           type = "error", duration = 5)
          return()
        }
        # On S3's current invites (CM13); a failed save is said, not swallowed.
        revocada <- tryCatch({
          modificar_pending_invites(function(inv) {
            i <- which(inv$token == token)[1]
            if (!is.na(i)) inv$status[i] <- "revoked"
            inv
          }); TRUE
        }, error = function(e) { message("[INVITES] revoke save failed: ", e$message); FALSE })
        if (!revocada) {
          showNotification(tr("niveles.hdadmin.no_se_guardo"), type = "error", duration = 6)
          return()
        }
        tryCatch(log_action(
          user        = shared$current_user(),
          module      = "invites",
          action      = "revocar_invitacion",
          description = sprintf("Invitación a %s (%s) revocada", inv_email %||% "?", inv_client %||% "?"),
          target_id   = token,
          s3_key      = .INVITES_KEY,
          client_id             = inv_client,
          viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL)
        ), error = function(e) NULL)
      }
      invites_refresh(invites_refresh() + 1L)
    }, ignoreInit = TRUE)

    # Drains everything that piled up while email was unconfigured — the
    # normal first action after installing the Resend key.
    # The queue's table (T28): ↻ on a row and the bar's "Reintentar todo"
    # reach these two handlers, staff only (checked in the table and here).
    cola_t <- tiers_cola_server("cola", refresh = invites_refresh,
                                permitido = reactive(isTRUE(is_hopdesk()) || isTRUE(is_principal())))

    observeEvent(cola_t$reintentar_todo(), {
      req(isTRUE(is_hopdesk()) || isTRUE(is_principal()))
      res <- tryCatch(retry_all_queued_emails(),
                      error = function(e) list(sent = 0L, failed = 0L))
      showNotification(
        tr("niveles.cola.aviso.reintento", enviados = as.character(res$sent), fallidos = as.character(res$failed)),
        type = if (res$failed > 0L) "warning" else "message", duration = 6
      )
      invites_refresh(invites_refresh() + 1L)
      tryCatch(log_action(
        user        = shared$current_user(),
        module      = "invites",
        action      = "reintentar_cola_email",
        description = sprintf("Reintento masivo de la cola de emails: %d enviado(s), %d fallido(s)",
                              res$sent, res$failed),
        client_id             = "hd-admin",
        viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL)
      ), error = function(e) NULL)
    }, ignoreInit = TRUE)

    observeEvent(cola_t$reintentar(), {
      # La cola es global de hd-admin: reintentar desde aquí reenvía correo de
      # cualquier cliente. El panel ya es staff-only, pero la acción llega del
      # navegador y el gate tiene que estar tambien en el servidor.
      req(isTRUE(is_hopdesk()) || isTRUE(is_principal()))
      queue_id <- cola_t$reintentar()$id
      req(nzchar(queue_id %||% ""))
      ok <- tryCatch(retry_queued_email(queue_id), error = function(e) FALSE)
      showNotification(
        if (ok) tr("niveles.cola.aviso.enviado") else tr("niveles.cola.aviso.reintento_fallido"),
        type = if (ok) "message" else "error", duration = 4
      )
      invites_refresh(invites_refresh() + 1L)
      # A pure retry of Hopdesk's own send queue (hd-admin/email_queue.rds),
      # not a client-data mutation -- hd-admin is the correct (and only
      # possible) target here, unlike the two handlers above.
      tryCatch(log_action(
        user        = shared$current_user(),
        module      = "invites",
        action      = "reintentar_email",
        description = sprintf("Reintento de envío de email (cola '%s'): %s",
                              queue_id, if (ok) "exitoso" else "fallido"),
        target_id   = queue_id,
        client_id             = "hd-admin",
        viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL)
      ), error = function(e) NULL)
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
          tr("niveles.accesos.aviso.revocado", cliente = cid),
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
          tr("niveles.accesos.aviso.expirado", cliente = cid),
          type = "warning", duration = 15
        )
      } else {
        elapsed_mins <- as.numeric(difftime(now, grace, units = "mins"))
        if (elapsed_mins >= 30) {
          showNotification(
            tr("niveles.accesos.aviso.gracia_terminada", cliente = cid),
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

    # The tab's frame depends on the role only (T28 P6): the active accesses
    # are a table of the engine (tiers_accesos_server), fixed here; the
    # request cards and the forms (hop_dinamico) follow hop_refresh.
    output$hop_section <- renderUI({
      can_see <- isTRUE(shared$current_user_info()$is_staff)
      if (!can_see) {
        return(div(class = "alert alert-warning mt-3",
                   icon("lock"), " Solo disponible para cuentas Hopdesk y Principal."))
      }
      header_row <- div(
        class = "d-flex align-items-center justify-content-between mt-3 mb-3",
        tags$h6(class = "fw-semibold mb-0",
                tagList(icon("key"), " Accesos de Salto")),
        actionButton(ns("btn_hop_refresh"), icon("rotate"),
                     class = "btn btn-sm btn-outline-secondary")
      )
      tabla <- div(class = "mb-3",
        tags$p(class = "small fw-semibold text-muted mb-1", tr("niveles.tablas.accesos.titulo")),
        tiers_accesos_ui(ns("accesos")))
      # Principal: requests and the direct grant first, the accesses last;
      # staff: its accesses first, then its request form and history.
      if (isTRUE(is_principal())) tagList(header_row, uiOutput(ns("hop_dinamico")), tags$hr(), tabla)
      else tagList(header_row, tabla, uiOutput(ns("hop_dinamico")))
    })

    output$hop_dinamico <- renderUI({
      if (!isTRUE(shared$current_user_info()$is_staff)) return(NULL)
      hop_refresh()

      me        <- shared$current_user() %||% ""
      principal <- isTRUE(is_principal())
      reqs      <- tryCatch(load_hop_requests(), error = function(e) .schema_hop_requests())
      registry  <- tryCatch(read_client_registry(), error = function(e) .schema_client_registry())
      active_reg <- registry[
        !is.na(registry$status) & registry$status == "active" &
        registry$client_id != "hd-admin", , drop = FALSE]

      if (principal) {
        # ── Principal view ────────────────────────────────────────────────────
        # A request stays "pending" until every client in it is decided.
        now <- Sys.time()
        pending_reqs <- if (is.data.frame(reqs) && nrow(reqs))
          reqs[!is.na(reqs$status) & reqs$status == "pending", , drop = FALSE]
        else data.frame()
        if (nrow(pending_reqs))
          pending_reqs <- pending_reqs[order(pending_reqs$requested_at, decreasing = TRUE), , drop = FALSE]

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

        # Staff list for direct grant form
        all_staff <- tryCatch(auth_load_usuarios(client_id = "hd-admin"),
                              error = function(e) NULL)
        staff_choices <- if (!is.null(all_staff) && nrow(all_staff)) {
          cands <- all_staff[
            (is.na(all_staff$deleted) | all_staff$deleted != TRUE) &
            !is.na(all_staff$tier) &
              all_staff$tier %in% Filter(function(t) !nivel_es_tope(t) && nivel_al_menos(t, nivel_ti()),
                                         niveles_todos()),
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
          tags$p(class = "small fw-semibold text-muted mb-2",
                 sprintf("Solicitudes pendientes (%d)", nrow(pending_reqs))),
          req_cards,
          direct_section
        )

      } else {
        # ── Staff view ────────────────────────────────────────────────────────
        my_reqs <- if (is.data.frame(reqs) && nrow(reqs))
          reqs[!is.na(reqs$requester) & reqs$requester == me, , drop = FALSE]
        else data.frame()
        if (nrow(my_reqs))
          my_reqs <- my_reqs[order(my_reqs$requested_at, decreasing = TRUE), , drop = FALSE]

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

        tagList(form_section, history_section)
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
        modificar_hop_requests(function(r) .hd_admin_agregar(r, new_req))
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
        showNotification(tr("niveles.accesos.aviso.solicitud_enviada"), type = "message", duration = 3)
        log_action(user        = me,
                   module      = "tiers",
                   action      = "hop_access_requested",
                   description = sprintf("%s solicitó acceso a: %s",
                                         me_name, paste(clients_sel, collapse = ", ")),
                   target_id   = new_req$id,
                   metadata    = list(clients = clients_sel, message = msg_txt),
                   client_id             = "hd-admin",
                   viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL))
        activity_refresh(activity_refresh() + 1L)
        hop_refresh(hop_refresh() + 1L)
      }, error = function(e)
        showNotification(tr("niveles.accesos.aviso.error", error = e$message), type = "error", duration = 5))
    }, ignoreInit = TRUE)

    # Records one client's decision on S3's current request (CM13), so two
    # principals deciding different clients of one request no longer undo each
    # other. Returns the request's decisions and whether all are in.
    .hop_decidir <- function(req_id, cid, clients_v, decision) {
      out <- list(decisions = list(), all_done = FALSE)
      modificar_hop_requests(function(r) {
        x <- hop_request_decidir(r, req_id, cid, clients_v, decision)
        out <<- x[c("decisions", "all_done")]
        x$requests
      })
      out
    }
    .hop_aviso_resuelta <- function(row, req_id, clients_v, decisions) {
      n_ok <- sum(vapply(clients_v, function(c) identical(decisions[[c]], "approved"), logical(1)))
      n_no <- sum(vapply(clients_v, function(c) identical(decisions[[c]], "denied"),   logical(1)))
      append_hd_notification(
        type         = "hop_resolved",
        client_id    = "hd-admin",
        message_text = sprintf("Solicitud de %s resuelta: %d aprobado(s), %d denegado(s).",
                               row$requester_name %||% row$requester, n_ok, n_no),
        metadata     = list(request_id = req_id, requester = row$requester,
                            decisions  = decisions)
      )
    }

    # Principal approves one client within a request
    # event value: "request_id:client_id:duration_secs"
    observeEvent(input$hop_approve, {
      req(isTRUE(staff_perms()$can_manage_hopdesk_perms))
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
        # Create grant for this client
        now    <- Sys.time()
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
        updated_grants <- modificar_hop_grants(function(g) .hd_admin_agregar(g, new_g))
        bump_sync_version("hop_grants_db", client_id = .HD_ADMIN_CID)
        shared$hop_grants_db(updated_grants)
        # Record the decision; resolve the request when all clients decided
        dec <- .hop_decidir(req_id, cid, clients_v, "approved")
        if (dec$all_done) .hop_aviso_resuelta(row, req_id, clients_v, dec$decisions)
        showNotification(
          tr("niveles.accesos.aviso.aprobado", usuario = row$requester_name %||% row$requester, cliente = cid, duracion = .dur_label(dur_secs)),
          type = "message", duration = 3
        )
        # client_id is the granted client (cid), not "hd-admin" -- per Mouse's
        # explicit call (2026-09-04, Grupo activity-logging audit): a client
        # should be able to see which Hopdesk staff had access to their own
        # books and when, not just have this live purely in Hopdesk's
        # internal bookkeeping. log_action()'s existing dual-write
        # (viewer_home_client_id stays the reviewer's real "hd-admin" home)
        # still files a copy in hd-admin's own log too, same as before.
        log_action(user        = me,
                   module      = "tiers",
                   action      = "hop_access_approved",
                   description = sprintf("%s aprobó acceso de %s a %s (%s)",
                                         me, row$requester_name %||% row$requester, cid,
                                         .dur_label(dur_secs)),
                   target_id   = req_id,
                   metadata    = list(client_id = cid, grantee = row$requester,
                                      duration_secs = dur_secs, grant_id = new_g$id),
                   client_id             = cid,
                   viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL))
        activity_refresh(activity_refresh() + 1L)
        hop_refresh(hop_refresh() + 1L)
      }, error = function(e)
        showNotification(tr("niveles.accesos.aviso.error_aprobar", error = e$message), type = "error", duration = 5))
    }, ignoreInit = TRUE)

    # Principal denies one client within a request
    # event value: "request_id:client_id"
    observeEvent(input$hop_deny, {
      req(isTRUE(staff_perms()$can_manage_hopdesk_perms))
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
        dec <- .hop_decidir(req_id, cid, clients_v, "denied")
        if (dec$all_done) .hop_aviso_resuelta(row, req_id, clients_v, dec$decisions)
        showNotification(tr("niveles.accesos.aviso.denegado", usuario = row$requester_name %||% row$requester, cliente = cid),
                         type = "warning", duration = 3)
        # Same reasoning as hop_access_approved above -- client_id = cid.
        log_action(user        = shared$current_user() %||% "",
                   module      = "tiers",
                   action      = "hop_access_denied",
                   description = sprintf("%s denegó acceso de %s a %s",
                                         shared$current_user() %||% "", row$requester_name %||% row$requester, cid),
                   target_id   = req_id,
                   metadata    = list(client_id = cid, grantee = row$requester),
                   client_id             = cid,
                   viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL))
        activity_refresh(activity_refresh() + 1L)
        hop_refresh(hop_refresh() + 1L)
      }, error = function(e)
        showNotification(tr("niveles.accesos.aviso.error_denegar", error = e$message), type = "error", duration = 5))
    }, ignoreInit = TRUE)

    # Principal revokes an active grant
    # The active accesses' table (T28): ⊘, confirmed in its bar, names the
    # grant by its id; only Principal with the permission gets it.
    accesos_t <- tiers_accesos_server("accesos", refresh = hop_refresh,
      permitido     = reactive(isTRUE(shared$current_user_info()$is_staff)),
      principal     = is_principal,
      me            = reactive(shared$current_user() %||% ""),
      puede_revocar = reactive(isTRUE(is_principal()) && isTRUE(staff_perms()$can_manage_hopdesk_perms)))

    observeEvent(accesos_t$revocar(), {
      req(isTRUE(staff_perms()$can_manage_hopdesk_perms))
      grant_id <- accesos_t$revocar()$id
      req(nzchar(grant_id %||% ""))
      tryCatch({
        grants <- load_hop_grants()
        idx    <- which(grants$id == grant_id)
        req(length(idx) > 0)
        grantee_v  <- grants$grantee[idx[1]]
        cid_v      <- grants$client_id[idx[1]]
        grants <- modificar_hop_grants(function(g) { g$revoked[g$id == grant_id] <- TRUE; g })
        bump_sync_version("hop_grants_db", client_id = .HD_ADMIN_CID)
        shared$hop_grants_db(grants)
        showNotification(tr("niveles.accesos.aviso.acceso_revocado"), type = "warning", duration = 3)
        # Same reasoning as hop_access_approved above -- client_id = cid_v.
        log_action(user        = shared$current_user() %||% "",
                   module      = "tiers",
                   action      = "hop_access_revoked",
                   description = sprintf("%s revocó acceso de %s a %s",
                                         shared$current_user() %||% "", grantee_v, cid_v),
                   target_id   = grant_id,
                   metadata    = list(client_id = cid_v, grantee = grantee_v),
                   client_id             = cid_v,
                   viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL))
        activity_refresh(activity_refresh() + 1L)
        hop_refresh(hop_refresh() + 1L)
      }, error = function(e)
        showNotification(tr("niveles.accesos.aviso.error_revocar", error = e$message), type = "error", duration = 5))
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
      req(isTRUE(staff_perms()$can_manage_hopdesk_perms))
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
        showNotification(tr("niveles.accesos.aviso.sin_clientes"), type = "warning", duration = 3)
        return()
      }

      me  <- shared$current_user() %||% ""
      now <- Sys.time()
      tryCatch({
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
        updated <- modificar_hop_grants(function(g) .hd_admin_agregar(g, new_rows))
        bump_sync_version("hop_grants_db", client_id = .HD_ADMIN_CID)
        shared$hop_grants_db(updated)
        labels <- paste(vapply(to_grant, `[[`, character(1), "label"), collapse = ", ")
        append_hd_notification(
          type         = "hop_resolved",
          client_id    = "hd-admin",
          message_text = sprintf("Acceso directo concedido a %s en: %s.", grantee, labels),
          metadata     = list(grantee = grantee, clients = labels, granted_by = me)
        )
        showNotification(
          tr("niveles.accesos.aviso.concedido", usuario = grantee, n = as.character(length(to_grant))),
          type = "message", duration = 4
        )
        # Unlike hop_approve/hop_deny/hop_revoke (always exactly one target
        # client), a direct grant can cover several clients in one click --
        # this summary entry stays hd-admin-only (Hopdesk's own record of
        # the whole action), and each granted client ALSO gets its own
        # entry filed directly under their own log (viewer_home_client_id
        # deliberately set equal to that same client_id, so log_action()'s
        # dual-write does NOT also duplicate it into hd-admin -- the
        # summary entry already covers that side).
        log_action(user        = me,
                   module      = "tiers",
                   action      = "hop_access_direct_grant",
                   description = sprintf("%s concedió acceso directo a %s en: %s",
                                         me, grantee, labels),
                   target_id   = grantee,
                   metadata    = list(clients = vapply(to_grant, `[[`, character(1), "client_id"),
                                      durations_secs = vapply(to_grant, `[[`, numeric(1), "dur_secs")),
                   client_id             = "hd-admin",
                   viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL))
        for (g in to_grant) {
          tryCatch(log_action(
            user        = me,
            module      = "tiers",
            action      = "hop_access_direct_grant",
            description = sprintf("%s concedió acceso directo a %s (%s)",
                                  me, grantee, .dur_label(g$dur_secs)),
            target_id   = grantee,
            metadata    = list(grantee = grantee, duration_secs = g$dur_secs),
            client_id             = g$client_id,
            viewer_home_client_id = g$client_id
          ), error = function(e) NULL)
        }
        activity_refresh(activity_refresh() + 1L)
        dg_selected(list())
        hop_refresh(hop_refresh() + 1L)
      }, error = function(e)
        showNotification(tr("niveles.accesos.aviso.error_conceder", error = e$message), type = "error", duration = 5))
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
        modificar_hd_notifications(function(n) hd_notifications_marcar_leidas(n, viewer, ids = nid))
        notif_refresh(notif_refresh() + 1L)
      }, error = function(e) {
        message("[NOTIF] mark read failed: ", e$message)
        showNotification(tr("niveles.hdadmin.no_se_guardo"), type = "error")
      })
    }, ignoreInit = TRUE)

    observeEvent(input$btn_mark_all_read, {
      viewer <- shared$current_user() %||% ""
      req(nzchar(viewer))
      tryCatch({
        modificar_hd_notifications(function(n) hd_notifications_marcar_leidas(n, viewer))
        notif_refresh(notif_refresh() + 1L)
      }, error = function(e) {
        message("[NOTIF] mark-all failed: ", e$message)
        showNotification(tr("niveles.hdadmin.no_se_guardo"), type = "error")
      })
    }, ignoreInit = TRUE)

    # ── Bitácora Global panel (Stage 4 Part A/B/C) ────────────────────────────
    # Gate is can_view_client_audit_logs (renamed/split from the old
    # can_view_global_audit — see ARCHITECTURE.md §6 / Stage 4 Part A). This
    # fixes gap #2: a plain hopdesk session now opens this tab by default and
    # sees a client selector instead of being locked out entirely.
    global_audit_allowed_ids <- reactive({
      registry <- tryCatch(read_client_registry(), error = function(e) .schema_client_registry())
      active   <- registry[registry$status == "active", , drop = FALSE]
      choices  <- setNames(active$client_id, active$display_name)
      choices[order(names(choices))]   # alphabetical, matching the context-switcher modal
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
      dev <- isTRUE(req_authorized())
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

      perm_rows <- lapply(perm_visible_keys(isTRUE(tryCatch(
                            shared$current_user_info()$is_staff,
                            error = function(e) FALSE))), function(k) {
        is_sys_locked <- k %in% permisos_bloqueados(sel_tier)
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
          tags$span(permiso_nombre(k), style = "font-size:.85rem; flex-grow:1;"),
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
    #
    # Multi-view selector (2026-09-03, generalized from the 2026-08-25 binary
    # actividad<->pagos toggle): swaps this same panel between the generic
    # Actividad log and any number of structured audit views (payment files
    # today; Bancos/Pasivos activity are the next planned entries). Adding a
    # future view means appending ONE entry to .ACTIVITY_VIEWS below --
    # nothing else in this block should need to change. Each entry's button
    # is only rendered when that view's own can_view(shared) passes, matching
    # this repo's "genuinely absent, not rendered-then-locked" convention for
    # permission-gated UI (see Stage 2 Part C's tab-bar comment above) --
    # every view gates independently, not behind one shared boolean.
    activity_view <- reactiveVal("actividad")
    # A single actionButton that CYCLES to the next view on each click
    # (2026-09-05) -- NOT a segmented control showing every option at once
    # (that was radioGroupButtons, tried first and explicitly rejected: it
    # rendered as two joined-together buttons, not one). This is one button,
    # one label, at all times -- its own icon/label just changes to name
    # whichever view is now showing after each click, cycling back to the
    # first view after the last.
    observeEvent(input$activity_view_cycle, {
      visible_ids <- vapply(
        Filter(function(v) isTRUE(tryCatch(v$can_view(shared), error = function(e) FALSE)), .ACTIVITY_VIEWS),
        `[[`, character(1), "id")
      if (length(visible_ids) < 2) return()
      cur  <- if (isTRUE(activity_view() %in% visible_ids)) activity_view() else visible_ids[1]
      idx  <- match(cur, visible_ids)
      next_idx <- if (is.na(idx) || idx >= length(visible_ids)) 1L else idx + 1L
      activity_view(visible_ids[next_idx])
    }, ignoreInit = TRUE)

    output$activity_section <- renderUI({
      req(req_authorized())
      visible <- Filter(function(v) isTRUE(tryCatch(v$can_view(shared), error = function(e) FALSE)),
                        .ACTIVITY_VIEWS)
      visible_ids <- vapply(visible, `[[`, character(1), "id")
      # Falls back to "actividad" if the current selection's permission was
      # revoked mid-session (e.g. a staff jump ending) rather than rendering
      # a view the viewer can no longer see.
      view_id    <- if (isTRUE(activity_view() %in% visible_ids)) activity_view() else "actividad"
      view_entry <- Find(function(v) identical(v$id, view_id), .ACTIVITY_VIEWS)

      tagList(
        if (length(visible) > 1)
          div(class = "d-flex justify-content-end mb-2",
              actionButton(ns("activity_view_cycle"),
                           tagList(icon(view_entry$icon), " ", view_entry$label),
                           class = "btn btn-sm btn-primary",
                           title = "Cambiar de vista"))
        else NULL,
        view_entry$ui(ns)
      )
    })

    auditLogViewerServer(
      "activity_viewer",
      shared = c(shared, list(refresh_signal = activity_refresh)),
      mode   = "own_client"
    )

    pagosAuditViewerServer("pagos_viewer", shared = shared)

    observeEvent(input$btn_save_tier_config, {
      req(req_authorized())
      tk  <- selected_tier_rv()
      req(tk %in% niveles_cliente())
      cid <- current_client_id()

      before_tk         <- tier_config_rv()[[tk]]
      full_config       <- tier_config_rv() %||% list()
      # Client keys only: internal keys are never the client's to set, and
      # saving them here used to switch them OFF for every user of the tier.
      claves <- perm_visible_keys(FALSE)
      full_config[[tk]] <- lapply(setNames(claves, claves), function(k) {
        if (k %in% permisos_bloqueados(tk)) return(TRUE)
        isTRUE(input[[paste0("cfg_", tk, "_", k)]])
      })

      tryCatch({
        .s3_write(full_config, S3_KEYS$tiers_config, client_id = cid)
        tier_config_rv(full_config)
        tier_config_locked(TRUE)
        tryCatch(shared$permisos_version(shared$permisos_version() + 1L), error = function(e) NULL)
        showNotification(
          tr("niveles.config.aviso.guardada", nivel = TIER_META[[tk]]),
          type = "message", duration = 2)
        tryCatch(log_action(
          user        = shared$current_user() %||% "",
          module      = "tiers",
          action      = "guardar_tier_config",
          description = sprintf("Configuración del tier «%s» actualizada", TIER_META[[tk]] %||% tk),
          target_id   = tk,
          metadata    = list(tier = tk, before = before_tk, after = full_config[[tk]]),
          client_id             = cid,
          viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL)
        ), error = function(e) NULL)
      }, error = function(e) {
        showNotification(tr("niveles.config.aviso.error_guardar", error = e$message), type = "error")
      })
    })

  })
}
