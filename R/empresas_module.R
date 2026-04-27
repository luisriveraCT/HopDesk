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
      )
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
      all_u <- tryCatch(auth_load_usuarios(), error = function(e) NULL)
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
      df <- load_empresas()
      # Broadcast to shared so company_map_rv and empresa buttons update
      if (!is.null(shared$empresas_db) && is.function(shared$empresas_db))
        shared$empresas_db(df)
      df
    })

    # Helper: save + update local cache + broadcast to shared
    .save_and_broadcast <- function(df) {
      save_empresas(df)                         # throws on S3 failure
      .empresas_local(df)
      if (!is.null(shared$empresas_db) && is.function(shared$empresas_db))
        shared$empresas_db(df)
      invisible(TRUE)
    }

    # Visible (non-deleted) companies
    empresas_rv <- reactive({
      df <- all_empresas_rv()
      req(!is.null(df))
      if (nrow(df) > 0 && "deleted" %in% names(df))
        df <- df[is.na(df$deleted) | df$deleted != TRUE, , drop = FALSE]
      df
    })

    # ── Companies table ───────────────────────────────────────────────────────
    output$empresas_tbl <- DT::renderDataTable({
      req(req_authorized())
      empresas <- empresas_rv()
      req(!is.null(empresas) && nrow(empresas) > 0)

      edit_id <- ns("trs_edit_click")
      del_id  <- ns("trs_del_click")

      df <- data.frame(
        `#`            = empresas$account_code,
        Iniciales      = empresas$initials,
        `Razón Social` = empresas$razon_social,
        RFC            = empresas$rfc,
        Estado         = ifelse(isTRUE(empresas$activa) | empresas$activa == TRUE,
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
        cascade_err <- tryCatch({ rename_empresa_initials(old_ini, new_ini); NULL },
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
            shared$ctas_cuentas(load_ctas_cuentas()),
          error = function(e) message("[EMP rename] reload ctas_cuentas: ", e$message)
        )
        tryCatch(
          if (!is.null(shared$interco_v2) && is.function(shared$interco_v2))
            shared$interco_v2(
              load_interco_v2() %||%
                list(ar_prefix = "C", ap_prefix = "P", companies = list())
            ),
          error = function(e) message("[EMP rename] reload interco_v2: ", e$message)
        )
        tryCatch(
          if (!is.null(shared$proveedores_db) && is.function(shared$proveedores_db))
            shared$proveedores_db(load_proveedores()),
          error = function(e) message("[EMP rename] reload proveedores: ", e$message)
        )
        tryCatch(
          if (!is.null(shared$parte_alias_map_db) && is.function(shared$parte_alias_map_db))
            shared$parte_alias_map_db(load_parte_alias_map()),
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

  })
}
