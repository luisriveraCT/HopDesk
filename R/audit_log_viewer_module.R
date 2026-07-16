# =============================================================================
# R/audit_log_viewer_module.R
# Stage 4 Part B — one shared filter/viewer component, reused by both the
# Actividad tab (mode = "own_client") and the Bitácora Global tab
# (mode = "multi_client"). Mouse's explicit instruction: "I want the filters
# to be the same for everyone who gets access to the logs."
#
# This module is the only caller of read_audit_log_scoped() (R/app_audit.R) —
# it never calls read_audit_log() directly (Stage 4 Part C railguard).
#
# auditLogViewerServer(id, shared, mode, allowed_client_ids = NULL,
#                       include_staff_log = FALSE)
#   mode = "own_client"    — no client selector; renders effective_client_id()'s
#                            own log. Used by Actividad.
#   mode = "multi_client"  — renders a client selector built from
#                            allowed_client_ids (already resolved by the
#                            caller), plus a "Hopdesk (interno)" option when
#                            include_staff_log is TRUE. Used by Bitácora Global.
#
# allowed_client_ids / include_staff_log may each be passed as a plain value
# OR as a zero-arg reactive/function — the caller's viewer-permission
# resolution depends on shared$current_user_info(), which isn't reliably
# resolved at the single synchronous instant a module is wired up, so both
# params are resolved lazily, at render/fetch time, via .resolve_param().
# =============================================================================

.resolve_param <- function(x) if (is.function(x)) x() else x

auditLogViewerUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("panel"))
}

# Resolve the *viewer's own* effective permissions: their tier default plus
# their own permisos override, looked up in their home client's usuarios.rds
# (never the jumped/effective client — a viewer's own permission grants live
# in their home folder regardless of where they're currently looking).
.audit_viewer_resolve_perms <- function(shared) {
  info <- tryCatch(shared$current_user_info(), error = function(e) NULL)
  tier <- info$tier %||% ""
  home_cid <- tryCatch(shared$home_client_id(), error = function(e) NULL)
  home_cid <- home_cid %||% info$client_id %||% tolower(Sys.getenv("CLIENT_ID"))
  username <- tolower(trimws(info$user %||% ""))

  overrides_json <- "{}"
  if (nzchar(username)) {
    u <- tryCatch(auth_load_usuarios(client_id = home_cid), error = function(e) NULL)
    if (!is.null(u) && is.data.frame(u) && nrow(u)) {
      row <- u[tolower(trimws(u$username)) == username, , drop = FALSE]
      if (nrow(row)) overrides_json <- row$permisos[1] %||% "{}"
    }
  }
  auth_resolve_perms(tier, overrides_json)
}

auditLogViewerServer <- function(id, shared, mode,
                                 allowed_client_ids = NULL,
                                 include_staff_log = FALSE) {
  stopifnot(mode %in% c("own_client", "multi_client"))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    viewer_perms <- reactive({ .audit_viewer_resolve_perms(shared) })

    viewer_home_cid <- reactive({
      info <- tryCatch(shared$current_user_info(), error = function(e) NULL)
      cid  <- tryCatch(shared$home_client_id(), error = function(e) NULL)
      cid %||% info$client_id %||% tolower(Sys.getenv("CLIENT_ID"))
    })

    viewer_is_staff <- reactive({
      isTRUE(tryCatch(shared$current_user_info()$is_staff, error = function(e) FALSE))
    })

    # Named vector of selectable clients — multi_client mode only.
    client_choices <- reactive({
      if (!identical(mode, "multi_client")) return(NULL)
      base <- .resolve_param(allowed_client_ids)
      if (is.null(base)) base <- character(0)
      if (isTRUE(.resolve_param(include_staff_log)))
        base <- c("Hopdesk (interno)" = "hd-admin", base)
      base
    })

    # NULL (not an error) while the multi_client selector hasn't rendered yet —
    # fetched() below treats that as "nothing to show", never a permission
    # denial, so a transient render-order gap can't masquerade as one.
    requested_client_id <- reactive({
      if (identical(mode, "own_client")) {
        tryCatch(shared$effective_client_id(), error = function(e) NULL) %||% viewer_home_cid()
      } else {
        input$audit_client_sel
      }
    })

    refresh_tick <- reactiveVal(0L)
    observeEvent(input$audit_refresh, { refresh_tick(refresh_tick() + 1L) }, ignoreInit = TRUE)

    # ── Fetch — the only call site for read_audit_log_scoped() ────────────────
    fetched <- reactive({
      refresh_tick()
      invalidateLater(20000)   # poll for cross-session updates, matching Actividad's prior behavior
      tryCatch(shared$refresh_signal(), error = function(e) NULL)  # optional: caller-side "just acted" signal

      cid <- requested_client_id()
      if (is.null(cid) || !nzchar(cid)) return(NULL)

      rng <- input$audit_range
      since <- if (!is.null(rng) && length(rng) == 2 && !is.na(rng[[1]])) rng[[1]] else Sys.time() - 7 * 86400
      until <- if (!is.null(rng) && length(rng) == 2 && !is.na(rng[[2]])) rng[[2]] else Sys.time()

      tryCatch(
        read_audit_log_scoped(
          requested_client_id         = cid,
          viewer_is_staff             = viewer_is_staff(),
          viewer_can_view_client_logs = isTRUE(viewer_perms()$can_view_client_audit_logs),
          viewer_can_view_staff_log   = isTRUE(viewer_perms()$can_view_staff_audit_log),
          viewer_home_client_id       = viewer_home_cid(),
          since = since, until = until
        ),
        error = function(e) e
      )
    })

    # Módulo / Acción choices derived from whatever the current range returned.
    observeEvent(fetched(), {
      df <- fetched()
      if (inherits(df, "error") || is.null(df)) return()
      mods <- sort(unique(df$module[!is.na(df$module) & nzchar(df$module)]))
      accs <- sort(unique(df$action[!is.na(df$action) & nzchar(df$action)]))
      updateSelectInput(session, "audit_mod_sel",
                        choices = c("Todos", mods),
                        selected = if (isTRUE(input$audit_mod_sel %in% mods)) input$audit_mod_sel else "Todos")
      updateSelectInput(session, "audit_accion_sel",
                        choices = c("Todos", accs),
                        selected = if (isTRUE(input$audit_accion_sel %in% accs)) input$audit_accion_sel else "Todos")
    })

    filtered <- reactive({
      df <- fetched()
      if (inherits(df, "error") || is.null(df) || !nrow(df)) return(df)
      if (!is.null(input$audit_mod_sel) && !identical(input$audit_mod_sel, "Todos"))
        df <- df[!is.na(df$module) & df$module == input$audit_mod_sel, , drop = FALSE]
      if (!is.null(input$audit_accion_sel) && !identical(input$audit_accion_sel, "Todos"))
        df <- df[!is.na(df$action) & df$action == input$audit_accion_sel, , drop = FALSE]
      u_filter <- trimws(input$audit_user_filter %||% "")
      if (nzchar(u_filter))
        df <- df[!is.na(df$user) & grepl(u_filter, df$user, ignore.case = TRUE), , drop = FALSE]
      df
    })

    # ── UI ──────────────────────────────────────────────────────────────────
    output$panel <- renderUI({
      if (identical(mode, "multi_client") && !isTRUE(viewer_perms()$can_view_client_audit_logs)) {
        return(div(class = "alert alert-warning mt-3", icon("lock"),
                   " Requiere permiso ", tags$code("can_view_client_audit_logs"), "."))
      }

      choices <- client_choices()

      tagList(
        div(
          class = "d-flex align-items-end gap-2 flex-wrap mb-2 mt-3",
          if (identical(mode, "multi_client"))
            div(style = "min-width:170px;",
                tags$label("Cliente", class = "form-label small fw-semibold mb-1"),
                selectInput(ns("audit_client_sel"), NULL,
                           choices  = choices,
                           selected = if (length(choices)) choices[[1]] else NULL,
                           width    = "100%")),
          div(style = "min-width:230px;",
              tags$label("Rango (fecha y hora)", class = "form-label small fw-semibold mb-1"),
              shinyWidgets::airDatepickerInput(
                ns("audit_range"), label = NULL,
                range = TRUE, timepicker = TRUE,
                value = c(Sys.time() - 7 * 86400, Sys.time()),
                dateFormat = "dd/MM/yy HH:mm",
                update_on = "close", width = "100%")),
          div(style = "min-width:130px;",
              tags$label("Módulo", class = "form-label small fw-semibold mb-1"),
              selectInput(ns("audit_mod_sel"), NULL, choices = c("Todos"), width = "100%")),
          div(style = "min-width:130px;",
              tags$label("Acción", class = "form-label small fw-semibold mb-1"),
              selectInput(ns("audit_accion_sel"), NULL, choices = c("Todos"), width = "100%")),
          div(style = "min-width:150px; flex:1;",
              tags$label("Usuario", class = "form-label small fw-semibold mb-1"),
              textInput(ns("audit_user_filter"), NULL,
                       placeholder = "Filtrar por usuario...", width = "100%")),
          actionButton(ns("audit_refresh"), icon("rotate"),
                      class = "btn btn-sm btn-outline-secondary", title = "Actualizar")
        ),
        uiOutput(ns("audit_results"))
      )
    })

    # Swaps between the results table and a permission-denied alert — this is
    # what surfaces read_audit_log_scoped()'s loud stop() as "requires higher
    # permission" instead of silently showing an empty table (Stage 4 Part A
    # fix for the Actividad staff-log leak).
    output$audit_results <- renderUI({
      if (inherits(fetched(), "error")) {
        return(div(class = "alert alert-warning mt-2", icon("lock"),
                   " No tienes permiso para ver esta bitácora."))
      }
      DT::dataTableOutput(ns("audit_tbl"))
    })

    output$audit_tbl <- DT::renderDataTable({
      df <- filtered()
      cols <- if (identical(mode, "multi_client"))
        c(Hora = "ts", Usuario = "user", Cliente = "client_id", `Módulo` = "module",
         `Acción` = "action", `Descripción` = "description", `ID destino` = "target_id")
      else
        c(Hora = "ts", Usuario = "user", `Módulo` = "module",
         `Acción` = "action", `Descripción` = "description", `ID destino` = "target_id")

      if (inherits(df, "error") || is.null(df) || !nrow(df)) {
        empty <- setNames(
          lapply(names(cols), function(x) character()),
          names(cols)
        )
        return(as.data.frame(empty, check.names = FALSE, stringsAsFactors = FALSE))
      }

      df <- df[order(df$ts, decreasing = TRUE), , drop = FALSE]
      out <- data.frame(
        Hora = format(as.POSIXct(df$ts), "%d/%m/%Y %H:%M:%S"),
        stringsAsFactors = FALSE
      )
      for (label in names(cols)[-1]) {
        src <- cols[[label]]
        out[[label]] <- ifelse(is.na(df[[src]]), "", df[[src]])
      }
      out
    }, options = list(
      pageLength = 25, dom = "ftp", order = list(),
      language = list(
        search      = "Filtrar:",
        zeroRecords = "Sin registros",
        info        = "Mostrando _START_ a _END_ de _TOTAL_ entradas",
        infoEmpty   = "Sin registros",
        paginate    = list(previous = "Anterior", `next` = "Siguiente")
      )
    ), selection = "none", rownames = FALSE)
  })
}
