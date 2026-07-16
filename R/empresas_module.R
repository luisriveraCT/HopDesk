# =============================================================================
# R/empresas_module.R
# Panel "Empresas" — gestión de empresas del grupo.
# Visible para dev y admin.
# Companies are the source of truth for initials/names throughout the app.
# Soft delete only — companies are never truly removed from storage.
# =============================================================================

# ── Helpers ───────────────────────────────────────────────────────────────────

.next_empresa_code <- function(all_e) {
  if (is.null(all_e) || !nrow(all_e) || !"account_code" %in% names(all_e))
    return("E0001")
  codes <- all_e$account_code
  nums  <- suppressWarnings(as.integer(sub("^E0*", "", codes[grepl("^E\\d+$", codes)])))
  nums  <- nums[!is.na(nums)]
  if (!length(nums)) return("E0001")
  sprintf("E%04d", max(nums) + 1L)
}

# ── UI ────────────────────────────────────────────────────────────────────────

empresasUI <- function(id) {
  ns <- NS(id)

  div(
    class = "p-3",
    style = "overflow-y: auto; max-height: calc(100vh - 65px);",

    # Header — same structure as tiers module
    div(
      class = "tiers-header",
      icon("building", class = "fa-2x"),
      div(
        tags$h4(class = "mb-0 fw-bold", "Empresas del Grupo"),
        tags$p(class = "mb-0 small opacity-75",
               "Razones sociales, iniciales y datos de las empresas")
      ),
      uiOutput(ns("header_badge"))
    ),

    div(
      class = "mt-3",
      fluidRow(
        # ── Left: table ──────────────────────────────────────────────────────
        column(
          width = 7,
          div(
            class = "card border-0 shadow-sm",
            div(
              class = "card-header bg-white py-2 d-flex align-items-center justify-content-between",
              tags$span(class = "fw-semibold",
                        tagList(icon("building"), " Empresas registradas")),
              actionButton(ns("btn_new_empresa"),
                           tagList(icon("circle-plus"), " Nueva empresa"),
                           class = "btn btn-sm btn-outline-primary")
            ),

            # ── Create form (hidden by default) ──────────────────────────────
            shinyjs::hidden(
              div(
                id = ns("create_form_wrap"),
                class = "card-body border-bottom",
                style = "background:#f8f9ff;",
                tags$h6(class = "fw-semibold mb-3", style = "color:#0a58ca;",
                        tagList(icon("circle-plus"), " Nueva empresa")),
                fluidRow(
                  column(3,
                    textInput(ns("new_initials"), "Iniciales",
                              placeholder = "Ej: NCS")
                  ),
                  column(9,
                    textInput(ns("new_razon_social"), "Razón Social",
                              placeholder = "Ej: Networks Crossdocking Services S de RL de CV")
                  )
                ),
                fluidRow(
                  column(6,
                    textInput(ns("new_nombre_corto"), "Nombre corto para mostrar",
                              placeholder = "Ej: Networks Crossdocking")
                  ),
                  column(6,
                    textInput(ns("new_rfc"), "RFC",
                              placeholder = "Ej: NCS123456AB1")
                  )
                ),
                textAreaInput(
                  ns("new_nombres_alt"),
                  "Nombres alternativos",
                  placeholder = "Uno por línea — variaciones del nombre para búsquedas y reportes",
                  rows = 2,
                  width = "100%"
                ),
                div(
                  class = "d-flex align-items-center justify-content-between mt-2",
                  checkboxInput(ns("new_activa"), "Empresa activa", value = TRUE),
                  div(
                    class = "d-flex gap-2",
                    actionButton(ns("btn_save_new_empresa"),
                                 tagList(icon("floppy-disk"), " Crear empresa"),
                                 class = "btn btn-primary btn-sm"),
                    actionButton(ns("btn_cancel_new_empresa"), "Cancelar",
                                 class = "btn btn-outline-secondary btn-sm")
                  )
                )
              )
            ),

            div(class = "card-body p-0",
                DT::dataTableOutput(ns("empresas_tbl")))
          )
        ),

        # ── Right: edit panel ─────────────────────────────────────────────────
        column(width = 5, uiOutput(ns("edit_panel_ui")))
      ),

      div(
        class = "alert alert-light border small mt-3",
        icon("circle-info", class = "text-primary"),
        tags$strong(" Empresas del grupo: "),
        "Las ", tags$strong("iniciales"), " identifican a cada empresa en todo el sistema. ",
        "Puedes modificarlas desde el panel de edición — el cambio se propagará automáticamente ",
        "a cuentas bancarias, proveedores y mapas de intercompañía. ",
        "Una empresa desactivada queda oculta, pero sus datos históricos se conservan para auditoría."
      ),

      uiOutput(ns("group_config_section"))
    )
  )
}

# ── Edit panel ────────────────────────────────────────────────────────────────

.empresas_edit_panel_ui <- function(ns, row) {
  nombres_alt_text <- tryCatch(
    paste(jsonlite::fromJSON(row$nombres_alt %||% "[]"), collapse = "\n"),
    error = function(e) ""
  )

  div(
    class = "card border-0 shadow-sm",
    div(
      class = "card-header py-3",
      style = "background: linear-gradient(120deg,#1a1a2e 0%,#16213e 100%); color:#fff; border-radius:8px 8px 0 0;",
      div(
        class = "d-flex align-items-center gap-3",
        div(
          class = "rounded-circle d-flex align-items-center justify-content-center flex-shrink-0",
          style = "width:42px; height:42px; background:rgba(255,255,255,.15); font-size:1.1rem;",
          icon("building-pen")
        ),
        div(
          tags$p(class = "mb-0",
                 style = "font-size:.7rem; opacity:.65; text-transform:uppercase; letter-spacing:.8px;",
                 "Editando empresa"),
          tags$h5(class = "mb-0 fw-bold", style = "font-size:1.05rem;",
                  row$nombre_corto %||% row$razon_social %||% row$initials),
          div(
            class = "d-flex align-items-center gap-2 mt-1",
            tags$span(
              style = "font-size:.65rem; font-weight:700; letter-spacing:1px; text-transform:uppercase; background:#0a58ca; border:1px solid rgba(255,255,255,.3); padding:2px 8px; border-radius:20px;",
              row$initials
            ),
            tags$span(style = "font-size:.75rem; opacity:.6;", row$account_code %||% "")
          )
        )
      )
    ),
    div(
      class = "card-body p-3",

      textInput(ns("edit_razon_social"), "Razón Social",
                value = row$razon_social %||% "", width = "100%"),

      fluidRow(
        column(7,
          textInput(ns("edit_nombre_corto"), "Nombre corto",
                    value = row$nombre_corto %||% "", width = "100%")
        ),
        column(5,
          textInput(ns("edit_rfc"), "RFC",
                    value = row$rfc %||% "", width = "100%")
        )
      ),

      textAreaInput(
        ns("edit_nombres_alt"),
        "Nombres alternativos",
        value       = nombres_alt_text,
        placeholder = "Uno por línea",
        rows        = 3,
        width       = "100%"
      ),

      fluidRow(
        column(4,
          textInput(ns("edit_initials"), "Iniciales",
                    value = row$initials, width = "100%")
        ),
        column(8,
          div(class = "mt-4 pt-1",
              checkboxInput(ns("edit_activa"), "Empresa activa",
                            value = isTRUE(row$activa)))
        )
      ),

      div(
        class = "d-flex gap-2",
        actionButton(ns("btn_save_edit"),
                     tagList(icon("floppy-disk"), " Guardar cambios"),
                     class = "btn btn-primary btn-sm"),
        actionButton(ns("btn_cancel_edit"), "Cancelar",
                     class = "btn btn-outline-secondary btn-sm")
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

empresasServer <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    current_tier  <- reactive({
      tryCatch(shared$current_user_info()$tier, error = function(e) "")
    })
    # Authorized if tier is dev/admin OR if the user has the can_manage_empresas
    # permission explicitly granted (allows future per-user overrides).
    req_authorized <- reactive({
      tier <- current_tier()
      if (tier %in% c("dev", "admin")) return(TRUE)
      # Check per-user permission override
      info <- tryCatch(shared$current_user_info(), error = function(e) NULL)
      if (is.null(info)) return(FALSE)
      all_u <- tryCatch(auth_load_usuarios(client_id = shared$effective_client_id()), error = function(e) NULL)
      if (is.null(all_u) || !nrow(all_u)) return(FALSE)
      row <- all_u[tolower(all_u$username) == tolower(info$user %||% ""), ]
      if (!nrow(row)) return(FALSE)
      perms <- auth_resolve_perms(row$tier[1], row$permisos[1] %||% "{}")
      isTRUE(perms$can_manage_empresas)
    })

    # ── Header badge ──────────────────────────────────────────────────────────
    output$header_badge <- renderUI({
      tier <- current_tier()
      if (!nzchar(tier)) return(NULL)
      tags$span(class = "ms-auto tiers-dev-badge", toupper(tier))
    })

    # ── Data layer ────────────────────────────────────────────────────────────
    .empresas_local <- reactiveVal(NULL)

    all_empresas_rv <- reactive({
      ovr <- .empresas_local()
      if (!is.null(ovr)) return(ovr)
      req(req_authorized())
      # Consume shared$empresas_db — populated by the context-switch observer.
      # Never read from S3 directly here: that would bypass the active client
      # context and always read from the env-var prefix (hd-admin).
      shared$empresas_db()
    })

    # Helper: save + update local cache + broadcast to shared
    .save_and_broadcast <- function(df) {
      save_empresas(df, client_id = shared$effective_client_id())                         # throws on S3 failure
      bump_sync_version("empresas_db")
      .empresas_local(df)
      if (!is.null(shared$empresas_db) && is.function(shared$empresas_db))
        shared$empresas_db(df)
      invisible(TRUE)
    }

    # Visible (non-deleted) companies, filtered to the user's allowed initials.
    empresas_rv <- reactive({
      df <- all_empresas_rv()
      if (is.null(df)) return(.schema_empresas())
      if (nrow(df) > 0 && "deleted" %in% names(df))
        df <- df[is.na(df$deleted) | df$deleted != TRUE, , drop = FALSE]
      # Respect group-based company visibility (NULL = no filter; character(0) = none)
      allowed <- tryCatch(shared$visible_initials(), error = function(e) NULL)
      if (!is.null(allowed) && nrow(df) > 0 && "initials" %in% names(df))
        df <- df[df$initials %in% allowed, , drop = FALSE]
      df
    })

    # ── Companies table ───────────────────────────────────────────────────────
    output$empresas_tbl <- DT::renderDataTable({
      req(req_authorized())
      empresas <- empresas_rv()
      req(!is.null(empresas))

      edit_id <- ns("trs_edit_click")
      del_id  <- ns("trs_del_click")
      erp_id  <- ns("erp_open_click")

      df <- data.frame(
        `#`            = empresas$account_code,
        Iniciales      = empresas$initials,
        `Razón Social` = empresas$razon_social,
        RFC            = empresas$rfc,
        Estado         = ifelse(!is.na(empresas$activa) & empresas$activa == TRUE,
                                "Activa", "Inactiva"),
        Editar = vapply(seq_len(nrow(empresas)), function(i)
          sprintf(
            '<button class="btn btn-outline-primary btn-sm" onclick="Shiny.setInputValue(\'%s\',%d,{priority:\'event\'})">Editar</button>',
            edit_id, i),
          character(1)),
        Eliminar = vapply(seq_len(nrow(empresas)), function(i)
          sprintf(
            '<button class="btn btn-outline-danger btn-sm" onclick="Shiny.setInputValue(\'%s\',%d,{priority:\'event\'})"><i class="fa fa-trash"></i></button>',
            del_id, i),
          character(1)),
        stringsAsFactors = FALSE,
        check.names      = FALSE
      )

      # Stage 3: ERP action column — only added when the ERP feature itself is
      # visible in this context (see erp_ui_visible()). Ordinary client-facing
      # functionality; hidden for a staff-at-home session exactly like
      # FINANCIAL_TABS hides Calendario/Vencidos/etc. for that same context.
      if (isTRUE(erp_ui_visible())) {
        df$ERP <- vapply(seq_len(nrow(empresas)), function(i)
          sprintf(
            '<button class="btn btn-outline-secondary btn-sm" onclick="Shiny.setInputValue(\'%s\',\'%s\',{priority:\'event\'})"><i class="fa fa-plug"></i> ERP</button>',
            erp_id, empresas$initials[i]),
          character(1))
      }

      DT::datatable(
        df, escape = FALSE, selection = "none", rownames = FALSE,
        options = list(
          pageLength = 15,
          dom        = "tp",
          language   = list(
            zeroRecords = "No se encontraron empresas",
            info        = "Mostrando _START_ a _END_ de _TOTAL_ empresas",
            infoEmpty   = "Sin registros",
            paginate    = list(previous = "Anterior", `next` = "Siguiente")
          )
        )
      )
    })

    # ── New empresa form ──────────────────────────────────────────────────────
    observeEvent(input$btn_new_empresa,        { shinyjs::toggle("create_form_wrap") })
    observeEvent(input$btn_cancel_new_empresa, { shinyjs::hide("create_form_wrap")  })

    observeEvent(input$btn_save_new_empresa, {
      initials     <- toupper(trimws(input$new_initials     %||% ""))
      razon_social <- trimws(input$new_razon_social %||% "")
      nombre_corto <- trimws(input$new_nombre_corto %||% "")
      rfc          <- toupper(trimws(input$new_rfc          %||% ""))

      if (!nzchar(initials) || !grepl("^[A-Z0-9]+$", initials)) {
        showNotification("Iniciales inválidas (solo letras mayúsculas y números).",
                         type = "error"); return()
      }
      if (!nzchar(razon_social)) {
        showNotification("La Razón Social es obligatoria.", type = "error"); return()
      }

      all_e <- all_empresas_rv()
      if (!is.null(all_e) && nrow(all_e) &&
          initials %in% toupper(trimws(all_e$initials))) {
        showNotification("Ya existe una empresa con esas iniciales.", type = "error"); return()
      }

      alt_lines <- trimws(strsplit(input$new_nombres_alt %||% "", "\n")[[1]])
      alt_lines <- alt_lines[nzchar(alt_lines)]

      new_row <- data.frame(
        id           = uuid::UUIDgenerate(),
        account_code = .next_empresa_code(all_e),
        initials     = initials,
        razon_social = razon_social,
        nombre_corto = if (nzchar(nombre_corto)) nombre_corto else razon_social,
        nombres_alt  = as.character(jsonlite::toJSON(alt_lines, auto_unbox = FALSE)),
        rfc          = rfc,
        activa       = isTRUE(input$new_activa),
        deleted      = FALSE,
        deleted_at   = NA_character_,
        created_at   = as.character(Sys.time()),
        updated_at   = as.character(Sys.time()),
        stringsAsFactors = FALSE
      )

      updated <- dplyr::bind_rows(all_e, new_row)
      saved <- tryCatch({ .save_and_broadcast(updated); TRUE },
                        error = function(e) { message("[EMP] create failed: ", e$message); FALSE })
      if (!saved) {
        showNotification(
          "No se pudo guardar la empresa. Verifica la conexión e intenta de nuevo.",
          type = "error", duration = 6)
        return()
      }
      shinyjs::hide("create_form_wrap")
      updateTextInput(session, "new_initials",     value = "")
      updateTextInput(session, "new_razon_social", value = "")
      updateTextInput(session, "new_nombre_corto", value = "")
      updateTextInput(session, "new_rfc",          value = "")
      showNotification(
        paste0("Empresa '", initials, "' (", new_row$account_code, ") creada."),
        type = "message", duration = 3)
    })

    # ── Edit panel ────────────────────────────────────────────────────────────
    selected_idx <- reactiveVal(NULL)

    observeEvent(input$trs_edit_click, { selected_idx(input$trs_edit_click) })
    observeEvent(input$btn_cancel_edit, { selected_idx(NULL) })

    output$edit_panel_ui <- renderUI({
      idx <- selected_idx()
      req(!is.null(idx))
      empresas <- empresas_rv()
      req(!is.null(empresas) && idx <= nrow(empresas))
      .empresas_edit_panel_ui(ns, empresas[idx, ])
    })

    observeEvent(input$btn_save_edit, {
      idx <- selected_idx()
      req(!is.null(idx))
      empresas <- empresas_rv()
      req(!is.null(empresas) && idx <= nrow(empresas))

      old_ini <- trimws(empresas[idx, "initials"])
      new_ini <- toupper(trimws(input$edit_initials %||% ""))

      if (!nzchar(new_ini) || !grepl("^[A-Z0-9]+$", new_ini)) {
        showNotification("Iniciales inválidas (solo letras mayúsculas y números).",
                         type = "error"); return()
      }

      all_e    <- all_empresas_rv()
      full_idx <- which(trimws(all_e$initials) == old_ini)
      req(length(full_idx) == 1)

      # Cascade-rename across all related S3 tables when initials change
      if (new_ini != old_ini) {
        others <- all_e[-full_idx, , drop = FALSE]
        if (new_ini %in% toupper(trimws(others$initials[!is.na(others$initials)]))) {
          showNotification("Ya existe otra empresa con esas iniciales.", type = "error")
          return()
        }
        cascade_err <- tryCatch({ rename_empresa_initials(old_ini, new_ini, client_id = shared$effective_client_id()); NULL },
                                error = function(e) e$message)
        if (!is.null(cascade_err)) {
          showNotification(paste0("Error actualizando tablas relacionadas: ", cascade_err),
                           type = "error", duration = 8)
          return()
        }
        all_e[full_idx, "initials"] <- new_ini
      }

      alt_lines <- trimws(strsplit(input$edit_nombres_alt %||% "", "\n")[[1]])
      alt_lines <- alt_lines[nzchar(alt_lines)]

      all_e[full_idx, "razon_social"] <- trimws(input$edit_razon_social %||% "")
      all_e[full_idx, "nombre_corto"] <- trimws(input$edit_nombre_corto %||% "")
      all_e[full_idx, "rfc"]          <- toupper(trimws(input$edit_rfc  %||% ""))
      all_e[full_idx, "activa"]       <- isTRUE(input$edit_activa)
      all_e[full_idx, "nombres_alt"]  <- as.character(jsonlite::toJSON(alt_lines, auto_unbox = FALSE))
      all_e[full_idx, "updated_at"]   <- as.character(Sys.time())

      saved <- tryCatch({ .save_and_broadcast(all_e); TRUE },
                        error = function(e) { message("[EMP] edit failed: ", e$message); FALSE })
      if (!saved) {
        showNotification("No se pudieron guardar los cambios. Intenta de nuevo.",
                         type = "error", duration = 6)
        return()
      }

      # After initials rename: reload in-memory reactive vals that were updated
      # in S3 by rename_empresa_initials(), so all modules see the new initials
      # immediately without requiring a page refresh.
      if (new_ini != old_ini) {
        tryCatch(
          if (!is.null(shared$ctas_cuentas) && is.function(shared$ctas_cuentas))
            shared$ctas_cuentas(load_ctas_cuentas(client_id = shared$effective_client_id())),
          error = function(e) message("[EMP rename] reload ctas_cuentas: ", e$message)
        )
        tryCatch(
          if (!is.null(shared$interco_v2) && is.function(shared$interco_v2))
            shared$interco_v2(
              load_interco_v2(client_id = shared$effective_client_id()) %||%
                list(ar_prefix = "C", ap_prefix = "P", companies = list())
            ),
          error = function(e) message("[EMP rename] reload interco_v2: ", e$message)
        )
        tryCatch(
          if (!is.null(shared$proveedores_db) && is.function(shared$proveedores_db))
            shared$proveedores_db(load_proveedores(client_id = shared$effective_client_id())),
          error = function(e) message("[EMP rename] reload proveedores: ", e$message)
        )
        tryCatch(
          if (!is.null(shared$parte_alias_map_db) && is.function(shared$parte_alias_map_db))
            shared$parte_alias_map_db(load_parte_alias_map(client_id = shared$effective_client_id())),
          error = function(e) message("[EMP rename] reload parte_alias_map: ", e$message)
        )
      }

      selected_idx(NULL)
      showNotification(
        if (new_ini != old_ini)
          paste0("Iniciales actualizadas: '", old_ini, "' \u2192 '", new_ini,
                 "'. Tablas relacionadas sincronizadas.")
        else
          "Empresa actualizada.",
        type = "message", duration = 3)
    })

    # ── Delete (soft) ─────────────────────────────────────────────────────────
    pending_del_idx <- reactiveVal(NULL)

    observeEvent(input$trs_del_click, {
      idx      <- input$trs_del_click
      empresas <- empresas_rv()
      req(!is.null(empresas) && idx <= nrow(empresas))
      row <- empresas[idx, ]
      pending_del_idx(idx)

      showModal(modalDialog(
        title = tagList(icon("trash"), " Desactivar empresa"),
        tags$p("Vas a desactivar la empresa:"),
        tags$div(
          class = "alert alert-warning py-2",
          tags$strong(row$razon_social %||% row$initials),
          tags$span(class = "badge ms-2",
                    style = "background:#0a58ca; color:#fff;",
                    row$initials)
        ),
        tags$p(class = "text-muted small mb-0",
               icon("circle-info", class = "me-1"),
               " La empresa quedará ",
               tags$strong("oculta"), " en el sistema. Sus datos históricos y ",
               "transacciones asociadas se conservan para auditoría."),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("btn_confirm_delete"),
                       tagList(icon("trash"), " Sí, desactivar"),
                       class = "btn btn-danger btn-sm")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$btn_confirm_delete, {
      idx <- pending_del_idx()
      req(!is.null(idx))
      removeModal()

      empresas        <- empresas_rv()
      req(!is.null(empresas) && idx <= nrow(empresas))
      target_initials <- empresas[idx, "initials"]

      all_e    <- all_empresas_rv()
      row_idx  <- which(trimws(all_e$initials) == trimws(target_initials))

      if (length(row_idx) == 1) {
        all_e[row_idx, "deleted"]    <- TRUE
        all_e[row_idx, "deleted_at"] <- as.character(Sys.time())
        all_e[row_idx, "activa"]     <- FALSE
        all_e[row_idx, "updated_at"] <- as.character(Sys.time())
      }

      saved <- tryCatch({ .save_and_broadcast(all_e); TRUE },
                        error = function(e) { message("[EMP] delete failed: ", e$message); FALSE })
      if (!saved) {
        showNotification("No se pudo guardar el cambio. Intenta de nuevo.",
                         type = "error", duration = 6)
        pending_del_idx(NULL)
        return()
      }
      pending_del_idx(NULL)
      selected_idx(NULL)
      showNotification(paste0("Empresa '", target_initials, "' desactivada."),
                       type = "message", duration = 4)
    })

    # ── Stage 3: ERP connections ───────────────────────────────────────────────
    # Per-company ERP credentials. See docs/saas_rebuild/STAGE_3_ERP_CREDENTIALS.md.
    #
    # Visibility (spec §5): FINANCIAL_TABS-style, not Stage 2's staff-only-tab
    # hiding — this is ordinary client-facing functionality. Invisible for a
    # staff-at-home session (nothing to configure, hd-admin has no
    # companies), visible for a native client session and for staff mid-jump.
    erp_is_staff <- reactive(isTRUE(tryCatch(shared$current_user_info()$is_staff, error = function(e) FALSE)))
    erp_jumped   <- reactive(!is.null(tryCatch(shared$jump_client_id(), error = function(e) NULL)))

    erp_ui_visible <- reactive(if (erp_is_staff()) erp_jumped() else TRUE)

    # Server-side access gate (spec §5 "Access") — the actual enforcement.
    # Every handler below calls this itself; the UI only hides the option.
    erp_access <- reactive(erp_access_allowed(current_tier(), erp_is_staff(), erp_jumped()))

    .reject_erp_action <- function() {
      showNotification("No tienes permisos para administrar conexiones ERP.",
                       type = "error", duration = 5)
    }

    .erp_connections_local <- reactiveVal(NULL)

    # NOTE: .erp_connections_local() is a DISPLAY cache only — it may contain
    # rows just flagged deleted=TRUE (the write path needs the full set to
    # persist that flag correctly). load_erp_connections() already excludes
    # deleted rows on a fresh read, so this reactive re-applies the same
    # filter to the cached path too — otherwise a just-deleted row keeps
    # showing in the UI after the very first mutation in a session, even
    # though it really was removed from what's active.
    all_erp_rv <- reactive({
      ovr <- .erp_connections_local()
      df <- if (!is.null(ovr)) {
        ovr
      } else {
        req(erp_ui_visible())
        tryCatch(load_erp_connections(shared$effective_client_id()),
                error = function(e) { message("[EMP ERP] load failed: ", e$message); .schema_erp_connections() })
      }
      if (!is.null(df) && nrow(df) && "deleted" %in% names(df))
        df <- df[is.na(df$deleted) | df$deleted != TRUE, , drop = FALSE]
      df
    })

    .save_erp_and_broadcast <- function(df) {
      save_erp_connections(df, client_id = shared$effective_client_id())   # throws on failure
      .erp_connections_local(df)
      invisible(TRUE)
    }

    # Every mutation (save/test/delete) MUST go through this, never through
    # .save_erp_and_broadcast(all_erp_rv()) directly. all_erp_rv()/
    # .erp_connections_local() are a per-session display cache — if a write
    # ever merged its change into that cached snapshot instead of the live
    # S3 state, a snapshot that was even briefly stale (a slow reactive
    # flush, a second tab, another session) would silently overwrite S3 with
    # a smaller set of rows and drop whatever wasn't in that snapshot — with
    # no "deleted" flag, no trace, just gone. This function re-fetches the
    # current live state immediately before merging in `mutate_fn`'s change
    # and writing, so the write can never regress rows that changed since
    # this session's own copy was cached.
    .erp_mutate_and_save <- function(mutate_fn) {
      cid   <- shared$effective_client_id()
      fresh <- load_erp_connections(cid)   # throws on failure — never fall back to a stale df here
      updated <- mutate_fn(fresh)
      save_erp_connections(updated, client_id = cid)
      .erp_connections_local(updated)
      invisible(updated)
    }

    erp_modal <- reactiveValues(company = NULL, form_open = FALSE, editing_id = NULL, confirm_delete_id = NULL)

    company_erp_rows <- reactive({
      req(erp_modal$company)
      df <- all_erp_rv()
      if (is.null(df) || !nrow(df)) return(df)
      has_company <- vapply(df$company_initials, function(x) {
        inits <- tryCatch(jsonlite::fromJSON(x), error = function(e) character(0))
        isTRUE(erp_modal$company %in% inits)
      }, logical(1))
      df[has_company, , drop = FALSE]
    })

    editing_erp_row <- reactive({
      id <- erp_modal$editing_id
      req(!is.null(id))
      rows <- company_erp_rows()
      req(!is.null(rows) && nrow(rows))
      match_row <- rows[rows$id == id, , drop = FALSE]
      req(nrow(match_row) == 1)
      match_row
    })

    # Blocks browser autofill on a rendered input by patching the actual
    # <input> element (not the wrapping div textInput()/passwordInput()
    # return). Plain autocomplete="off" is routinely ignored by Chrome for
    # login-shaped fields, so password fields get "new-password" instead —
    # the one value Chrome actually honors for suppressing saved-credential
    # suggestions. Needed because a text-input-next-to-password-input pair
    # on the same origin as this app's own login screen gets autofilled with
    # the logged-in user's own Hopdesk credentials by the browser itself —
    # not a server-side leak, but confusing and worth blocking outright.
    .erp_no_autofill <- function(tag, password = FALSE) {
      htmltools::tagQuery(tag)$find("input")$
        addAttrs(autocomplete = if (password) "new-password" else "off")$
        allTags()
    }

    # `secret` = TRUE for fields that go into secrets_encrypted (prefix "secret") —
    # these are NEVER pre-filled with the real value, even on edit, per the
    # "leave blank to keep unchanged" convention. Non-secret config fields
    # (prefix "cfg") are ordinary settings and MUST show their real current
    # value on edit — leaving them blank-with-a-hint like the secret fields
    # would silently wipe the URL/company/etc. on save if the user didn't
    # notice and retype them.
    .erp_render_field <- function(prefix, field, value = NULL, secret = FALSE) {
      input_id <- ns(paste0(prefix, "_", field$name))
      if (identical(field$type, "checkbox")) {
        checkboxInput(input_id, field$label, value = if (is.null(value)) TRUE else isTRUE(value))
      } else if (secret) {
        if (identical(field$type, "password")) {
          .erp_no_autofill(passwordInput(input_id, field$label, value = "",
                        placeholder = if (!is.null(value)) "•••••• (dejar en blanco para no modificar)" else ""),
                        password = TRUE)
        } else {
          .erp_no_autofill(textInput(input_id, field$label, value = "",
                    placeholder = if (!is.null(value)) "(dejar en blanco para no modificar)" else ""))
        }
      } else {
        .erp_no_autofill(textInput(input_id, field$label, value = if (is.null(value)) "" else as.character(value)))
      }
    }

    output$erp_modal_body <- renderUI({
      req(erp_modal$company)
      rows <- company_erp_rows()

      # Inline delete confirmation — NOT a nested showModal(), which would
      # silently replace this "Conexiones ERP" modal (Shiny shows only one
      # modal at a time) and strand the user without a way back to the list.
      if (!is.null(erp_modal$confirm_delete_id)) {
        target <- if (!is.null(rows) && nrow(rows)) rows[rows$id == erp_modal$confirm_delete_id, , drop = FALSE] else rows[0, ]
        return(tagList(
          tags$p("Vas a eliminar la conexión:"),
          tags$div(class = "alert alert-warning py-2",
                   tags$strong(if (nrow(target)) target$label[1] else "")),
          tags$p(class = "text-muted small",
                 icon("circle-info", class = "me-1"),
                 " Esta acción no se puede deshacer desde la interfaz. Tendrás que volver a ",
                 "capturar las credenciales si la necesitas de nuevo."),
          div(class = "d-flex gap-2",
              actionButton(ns("btn_confirm_erp_delete"), tagList(icon("trash"), " Sí, eliminar"),
                          class = "btn btn-danger btn-sm"),
              actionButton(ns("btn_cancel_erp_delete"), "Cancelar", class = "btn btn-outline-secondary btn-sm"))
        ))
      }

      list_ui <- if (is.null(rows) || !nrow(rows)) {
        tags$p(class = "text-muted small", "Sin conexiones configuradas todavía.")
      } else {
        tagList(lapply(seq_len(nrow(rows)), function(i) {
          r <- rows[i, ]
          badge <- if (isTRUE(r$last_test_result == "ok")) list(color = "success", text = "OK")
                   else if (isTRUE(r$last_test_result == "error")) list(color = "danger", text = "Error")
                   else list(color = "secondary", text = "Sin probar")
          div(
            class = "d-flex align-items-center justify-content-between border rounded p-2 mb-2",
            div(
              tags$strong(r$label), " ",
              tags$span(class = paste0("badge bg-", badge$color), badge$text),
              tags$div(class = "text-muted small", erp_connector_label(r$erp_type))
            ),
            div(
              class = "d-flex gap-1",
              tags$button(class = "btn btn-sm btn-outline-primary", title = "Editar",
                onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                                  ns("erp_edit_click"), r$id),
                icon("pen")),
              tags$button(class = "btn btn-sm btn-outline-secondary", title = "Probar conexión",
                onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                                  ns("erp_test_click"), r$id),
                icon("plug")),
              tags$button(class = "btn btn-sm btn-outline-danger", title = "Eliminar",
                onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                                  ns("erp_del_click"), r$id),
                icon("trash"))
            )
          )
        }))
      }

      form_ui <- if (isTRUE(erp_modal$form_open)) {
        editing <- tryCatch(editing_erp_row(), error = function(e) NULL)
        erp_type <- if (!is.null(editing)) editing$erp_type[1] else "sap_b1_service_layer"
        cfg <- if (!is.null(editing)) tryCatch(jsonlite::fromJSON(editing$config[1]), error = function(e) list()) else list()

        tagList(
          tags$hr(),
          tags$h6(class = "fw-semibold", if (!is.null(editing)) "Editar conexión" else "Nueva conexión"),
          .erp_no_autofill(textInput(ns("erp_label"), "Nombre",
                    value = if (!is.null(editing)) editing$label[1] else "",
                    placeholder = "Ej: SAP — Networks Group (NG)")),
          lapply(erp_connector_config_fields(erp_type), function(f) .erp_render_field("cfg", f, cfg[[f$name]])),
          tags$p(class = "text-muted small mb-1 mt-2", "Credenciales"),
          lapply(erp_connector_secret_fields(erp_type), function(f)
            .erp_render_field("secret", f, value = if (!is.null(editing)) TRUE else NULL, secret = TRUE)),
          div(
            class = "d-flex gap-2 mt-2",
            actionButton(ns("btn_save_erp"), tagList(icon("floppy-disk"), " Guardar"),
                        class = "btn btn-primary btn-sm"),
            actionButton(ns("btn_cancel_erp_form"), "Cancelar", class = "btn btn-outline-secondary btn-sm")
          )
        )
      } else {
        actionButton(ns("btn_new_erp"), tagList(icon("circle-plus"), " Nueva conexión"),
                    class = "btn btn-outline-primary btn-sm mt-2")
      }

      tagList(list_ui, form_ui)
    })

    observeEvent(input$erp_open_click, {
      req(erp_ui_visible())
      erp_modal$company           <- input$erp_open_click
      erp_modal$form_open         <- FALSE
      erp_modal$editing_id        <- NULL
      erp_modal$confirm_delete_id <- NULL
      showModal(modalDialog(
        title = tagList(icon("plug"), paste0(" Conexiones ERP — ", input$erp_open_click)),
        uiOutput(ns("erp_modal_body")),
        footer = modalButton("Cerrar"),
        size = "m", easyClose = TRUE
      ))
    })

    observeEvent(input$btn_new_erp, {
      req(erp_access())
      erp_modal$editing_id <- NULL; erp_modal$form_open <- TRUE; erp_modal$confirm_delete_id <- NULL
    })
    observeEvent(input$erp_edit_click, {
      req(erp_access())
      erp_modal$editing_id <- input$erp_edit_click; erp_modal$form_open <- TRUE; erp_modal$confirm_delete_id <- NULL
    })
    observeEvent(input$btn_cancel_erp_form, {
      erp_modal$form_open <- FALSE; erp_modal$editing_id <- NULL
    })

    observeEvent(input$btn_save_erp, {
      if (!isTRUE(erp_access())) { .reject_erp_action(); return() }

      editing  <- tryCatch(editing_erp_row(), error = function(e) NULL)
      erp_type <- if (!is.null(editing)) editing$erp_type[1] else "sap_b1_service_layer"
      label    <- trimws(input$erp_label %||% "")
      if (!nzchar(label)) { showNotification("El nombre es obligatorio.", type = "error"); return() }

      cfg_fields <- erp_connector_config_fields(erp_type)
      config <- setNames(lapply(cfg_fields, function(f) {
        v <- input[[paste0("cfg_", f$name)]]
        if (identical(f$type, "checkbox")) isTRUE(v) else trimws(v %||% "")
      }), vapply(cfg_fields, function(f) f$name, character(1)))

      secret_fields  <- erp_connector_secret_fields(erp_type)
      secret_inputs  <- lapply(secret_fields, function(f) trimws(input[[paste0("secret_", f$name)]] %||% ""))
      names(secret_inputs) <- vapply(secret_fields, function(f) f$name, character(1))
      any_secret_filled <- any(vapply(secret_inputs, nzchar, logical(1)))
      all_secret_filled <- all(vapply(secret_inputs, nzchar, logical(1)))

      if (is.null(editing) && !all_secret_filled) {
        showNotification("Completa todos los campos de credenciales.", type = "error"); return()
      }
      if (any_secret_filled && !all_secret_filled) {
        showNotification("Completa todos los campos de credenciales, o déjalos todos en blanco para no modificarlas.",
                         type = "error", duration = 6)
        return()
      }

      secrets_encrypted <- if (!is.null(editing) && !any_secret_filled) {
        editing$secrets_encrypted[1]
      } else {
        encrypt_secret(secret_inputs)
      }

      info <- tryCatch(shared$current_user_info(), error = function(e) NULL)
      who  <- info$account_code %||% info$user %||% "unknown"
      now  <- as.character(Sys.time())
      cid  <- shared$effective_client_id()
      erp_company <- erp_modal$company
      editing_id  <- if (!is.null(editing)) editing$id[1] else NULL

      saved <- tryCatch({
        .erp_mutate_and_save(function(all_df) {
          if (!is.null(editing_id)) {
            idx <- which(all_df$id == editing_id)
            if (length(idx) != 1)
              stop("Esta conexión ya no existe — puede haber sido eliminada en otra sesión. Cierra y vuelve a abrir el panel.")
            all_df[idx, "label"]             <- label
            all_df[idx, "config"]            <- as.character(jsonlite::toJSON(config, auto_unbox = TRUE))
            all_df[idx, "secrets_encrypted"] <- secrets_encrypted
            all_df[idx, "updated_at"]        <- now
            all_df[idx, "updated_by"]        <- who
            all_df
          } else {
            new_row <- tibble::tibble(
              id = uuid::UUIDgenerate(), client_id = cid, label = label, erp_type = erp_type,
              company_initials = as.character(jsonlite::toJSON(erp_company, auto_unbox = FALSE)),
              config = as.character(jsonlite::toJSON(config, auto_unbox = TRUE)),
              secrets_encrypted = secrets_encrypted,
              created_at = now, created_by = who, updated_at = now, updated_by = who,
              last_tested_at = NA_character_, last_test_result = NA_character_, last_test_message = NA_character_,
              active = TRUE, deleted = FALSE, deleted_at = NA_character_
            )
            dplyr::bind_rows(all_df, new_row)
          }
        })
        TRUE
      }, error = function(e) {
        message("[EMP ERP] save failed: ", e$message)
        showNotification(conditionMessage(e), type = "error", duration = 8)
        FALSE
      })
      if (!saved) return()

      erp_modal$form_open <- FALSE
      erp_modal$editing_id <- NULL
      showNotification(paste0("Conexión '", label, "' guardada."), type = "message", duration = 3)
    })

    observeEvent(input$erp_test_click, {
      if (!isTRUE(erp_access())) { .reject_erp_action(); return() }

      test_id <- input$erp_test_click
      cid <- shared$effective_client_id()
      current <- tryCatch(load_erp_connections(cid), error = function(e) NULL)
      if (is.null(current) || !any(current$id == test_id)) {
        showNotification("Esta conexión ya no existe (fue eliminada o modificada en otra sesión).",
                         type = "error", duration = 6)
        return()
      }
      row <- current[current$id == test_id, , drop = FALSE]

      config  <- tryCatch(jsonlite::fromJSON(row$config[1]), error = function(e) list())
      secrets <- tryCatch(decrypt_secret(row$secrets_encrypted[1]),
                          error = function(e) { list(.decrypt_error = conditionMessage(e)) })

      result <- if (!is.null(secrets$.decrypt_error)) {
        list(ok = FALSE, message = paste0("No se pudo descifrar la credencial: ", secrets$.decrypt_error))
      } else {
        tryCatch(run_erp_connection_test(row$erp_type[1], config, secrets),
                error = function(e) list(ok = FALSE, message = paste0("Error inesperado: ", conditionMessage(e))))
      }

      tryCatch({
        .erp_mutate_and_save(function(all_df) {
          idx <- which(all_df$id == test_id)
          if (length(idx) != 1) stop("La conexión fue eliminada mientras se probaba — resultado no guardado.")
          all_df[idx, "last_tested_at"]    <- as.character(Sys.time())
          all_df[idx, "last_test_result"]  <- if (isTRUE(result$ok)) "ok" else "error"
          all_df[idx, "last_test_message"] <- result$message %||% ""
          all_df
        })
      }, error = function(e) message("[EMP ERP] test-result save failed: ", e$message))

      showNotification(result$message %||% (if (isTRUE(result$ok)) "OK" else "Error"),
                       type = if (isTRUE(result$ok)) "message" else "error", duration = 6)
    })

    observeEvent(input$erp_del_click, {
      if (!isTRUE(erp_access())) { .reject_erp_action(); return() }
      all_df <- all_erp_rv()
      req(any(all_df$id == input$erp_del_click))
      erp_modal$confirm_delete_id <- input$erp_del_click
    })

    observeEvent(input$btn_cancel_erp_delete, {
      erp_modal$confirm_delete_id <- NULL
    })

    observeEvent(input$btn_confirm_erp_delete, {
      if (!isTRUE(erp_access())) { .reject_erp_action(); return() }

      id <- erp_modal$confirm_delete_id
      req(!is.null(id))

      saved <- tryCatch({
        .erp_mutate_and_save(function(all_df) {
          idx <- which(all_df$id == id)
          if (length(idx) != 1) stop("Esta conexión ya no existe (fue eliminada en otra sesión).")
          all_df[idx, "deleted"]    <- TRUE
          all_df[idx, "deleted_at"] <- as.character(Sys.time())
          all_df[idx, "active"]     <- FALSE
          all_df
        })
        TRUE
      }, error = function(e) {
        message("[EMP ERP] delete failed: ", e$message)
        showNotification(conditionMessage(e), type = "error", duration = 6)
        FALSE
      })
      erp_modal$confirm_delete_id <- NULL
      if (!saved) return()
      showNotification("Conexión eliminada.", type = "message", duration = 3)
    })

    # ── Group configuration section (dev only) ────────────────────────────────

    output$group_config_section <- renderUI({
      req(req_authorized())
      if (!identical(current_tier(), "dev")) return(NULL)

      cfg <- tryCatch(shared$group_config(), error = function(e) NULL) %||%
             list(group_name = "Networks Group", logo_raw = NULL, logo_ext = "png")
      has_logo <- !is.null(cfg$logo_raw) && length(cfg$logo_raw) > 0

      tagList(
        tags$hr(class = "mt-4"),
        div(
          class = "card border-0 shadow-sm mt-3",
          div(
            class = "card-header bg-white py-2 d-flex align-items-center gap-2",
            icon("gear", class = "text-primary"),
            tags$span(class = "fw-semibold", "Configuración del Grupo"),
            tags$span(class = "badge bg-primary ms-1 small", "DEV")
          ),
          div(
            class = "card-body",
            fluidRow(
              column(8,
                tags$label("Nombre del grupo", class = "form-label small fw-semibold"),
                tags$p(class = "text-muted small mb-1",
                       "Aparece en la portada del reporte Word y como prefijo del nombre de archivo."),
                textInput(ns("group_name_input"), NULL,
                          value = cfg$group_name %||% "Networks Group",
                          placeholder = "Ej: Networks Group",
                          width = "100%")
              )
            ),
            fluidRow(
              column(8,
                tags$label("Logo del grupo", class = "form-label small fw-semibold"),
                tags$p(class = "text-muted small mb-1",
                       "PNG o JPG, máx. 2 MB. Aparece en la portada del reporte Word."),
                fileInput(ns("group_logo_upload"), NULL,
                          accept = c("image/png", "image/jpeg", ".png", ".jpg", ".jpeg"),
                          buttonLabel = "Elegir imagen…",
                          width = "100%")
              ),
              if (has_logo) {
                column(4,
                  div(class = "mt-4 pt-2",
                      icon("circle-check", class = "text-success"),
                      tags$span(class = "small text-muted ms-1", "Logo guardado"))
              ) } else NULL
            ),
            actionButton(ns("btn_save_group_config"),
                         tagList(icon("floppy-disk"), " Guardar configuración"),
                         class = "btn btn-sm btn-primary mt-2")
          )
        )
      )
    })

    observeEvent(input$btn_save_group_config, {
      name <- trimws(input$group_name_input %||% "")
      if (!nzchar(name)) {
        showNotification("El nombre del grupo no puede estar vacío.", type = "warning")
        return()
      }
      if (nchar(name) > 100) {
        showNotification("El nombre del grupo no puede superar 100 caracteres.", type = "warning")
        return()
      }

      cfg <- tryCatch(shared$group_config(), error = function(e) NULL) %||%
             list(group_name = "Networks Group", logo_raw = NULL, logo_ext = "png")
      cfg$group_name <- name

      logo_file <- input$group_logo_upload
      if (!is.null(logo_file) && file.exists(logo_file$datapath)) {
        ext <- tolower(tools::file_ext(logo_file$name))
        if (!ext %in% c("png", "jpg", "jpeg")) {
          showNotification("Formato inválido. Usa PNG o JPG.", type = "error")
          return()
        }
        size_mb <- file.size(logo_file$datapath) / 1024 / 1024
        if (size_mb > 2) {
          showNotification("El logo supera 2 MB. Usa una imagen más pequeña.", type = "error")
          return()
        }
        cfg$logo_raw <- readBin(logo_file$datapath, "raw",
                                n = file.size(logo_file$datapath))
        cfg$logo_ext <- if (ext == "jpeg") "jpg" else ext
      }

      tryCatch({
        save_group_config(cfg, client_id = shared$effective_client_id())
        shared$group_config(cfg)
        showNotification("Configuración del grupo guardada correctamente.",
                         type = "message", duration = 4)
      }, error = function(e) {
        showNotification(paste("Error al guardar:", conditionMessage(e)),
                         type = "error", duration = 6)
      })
    })

  })
}
