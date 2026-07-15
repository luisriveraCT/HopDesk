# =============================================================================
# R/settings/settings_sincro.R
# Sincronización panel UI and settings_sincro_observer().
# =============================================================================
# (Originally part of R/settings_module.R; split in Stage 7.1 for session efficiency.)

# =============================================================================
# Sincronización panel UI
# =============================================================================
.sincro_panel_ui <- function(cfg) {
  is_on      <- isTRUE(cfg$is_enabled)
  enabled_by <- cfg$enabled_by %||% NA_character_
  enabled_at <- cfg$enabled_at

  tagList(
    div(class = "d-flex align-items-center gap-2 mb-2",
      tags$h6(class = "fw-semibold mb-0",
              tagList(icon("rotate"), " Sincronizar Agenda entre usuarios")),
      div(class = "ms-auto",
        tags$span(
          class = if (is_on) "badge bg-success" else "badge bg-secondary",
          if (is_on) "Activa" else "Inactiva"
        )
      )
    ),
    div(class = "p-3 bg-light rounded mb-3",
      tags$p(class = "text-muted small mb-3",
        "Cuando está activa, todos los usuarios ven y editan la misma Agenda de hoy en tiempo real.",
        tags$br(),
        "Al desactivarla, cada cuenta conserva su propia versión independiente y puede editarla sin afectar a las demás."
      ),
      div(class = "form-check form-switch d-flex align-items-center gap-3",
        tags$input(
          type  = "checkbox",
          class = "form-check-input flex-shrink-0",
          id      = "sincro_toggle",
          role    = "switch",
          style   = "width:3em; height:1.5em; cursor:pointer;",
          checked = if (is_on) NA else NULL
        ),
        tags$label(
          class = "form-check-label fw-semibold mb-0",
          `for` = "sincro_toggle",
          if (is_on) "Sincronización activa" else "Sincronización inactiva"
        )
      ),
      if (is_on && !is.na(enabled_by %||% NA_character_)) {
        at_fmt <- tryCatch({
          d <- as.POSIXct(enabled_at)
          if (is.na(d)) "—" else format(d, "%d/%m/%Y %H:%M")
        }, error = function(e) "—")
        tags$small(class = "text-muted d-block mt-2",
          sprintf("Activada por: %s — %s", enabled_by, at_fmt))
      }
    ),
    tags$script(HTML("
      $(document).off('change.sincrotgl').on('change.sincrotgl', '#sincro_toggle', function() {
        Shiny.setInputValue('sincro_toggle_change',
          {checked: this.checked, nonce: Math.random()}, {priority:'event'});
      });
    "))
  )
}

.sincro_activar_modal <- function(usuarios_df, agendas) {
  active_users <- usuarios_df[
    (is.na(usuarios_df$deleted) | usuarios_df$deleted != TRUE) &
    (usuarios_df$activo %in% TRUE), , drop = FALSE]

  user_rows <- lapply(seq_len(nrow(active_users)), function(i) {
    u  <- active_users$username[i]
    nm <- active_users$display_name[i]
    n  <- nrow(agendas[[u]] %||% data.frame())

    div(class = "border rounded p-2 mb-2",
      div(class = "form-check d-flex align-items-center gap-2",
        tags$input(
          type  = "radio", name = "sincro_src_radio",
          class = "form-check-input sincro-user-radio flex-shrink-0",
          id    = paste0("sincro_r_", u), value = u
        ),
        tags$label(
          class = "form-check-label d-flex justify-content-between align-items-center w-100 mb-0",
          `for` = paste0("sincro_r_", u),
          tagList(
            tags$span(tags$strong(nm), tags$small(class = "text-muted ms-1", paste0("@", u))),
            tags$span(class = "badge bg-primary ms-2", paste0(n, " elemento(s)"))
          )
        )
      )
    )
  })

  showModal(modalDialog(
    title     = tagList(icon("rotate"), " Activar sincronización"),
    size      = "l",
    easyClose = FALSE,
    footer    = tagList(
      modalButton("Cancelar"),
      actionButton("do_sincro_activar", tagList(icon("check"), " Activar con esta agenda"),
                   class = "btn btn-success", disabled = NA)
    ),
    tags$p(class = "text-muted small mb-3",
      "Selecciona cuya Agenda de hoy se usará como punto de partida. Todos los usuarios sincronizarán con esa versión."
    ),
    div(class = "mb-3", user_rows),
    uiOutput("sincro_preview_panel"),
    tags$script(HTML("
      $(document).off('change.sincrorad').on('change.sincrorad', '.sincro-user-radio', function() {
        var u = $(this).val();
        Shiny.setInputValue('sincro_src_selected', {user: u, nonce: Math.random()}, {priority:'event'});
        $('#do_sincro_activar').prop('disabled', false).removeAttr('disabled');
      });
    "))
  ))
}

# =============================================================================
# settings_sincro_observer  — Sincronizar Agenda entre usuarios
# Tier gate: admin + dev only (stg_btn_sincro is only rendered for those tiers)
# =============================================================================
settings_sincro_observer <- function(input, output, session, shared, pagar_hoy_db) {

  # ── Open panel ──────────────────────────────────────────────────────────────
  observeEvent(input$stg_btn_sincro, {
    cfg <- tryCatch(load_agenda_sync_config(client_id = shared$active_client_id()),
                    error = function(e) list(is_enabled = FALSE, .missing = TRUE))
    output$settings_panel <- renderUI({ .sincro_panel_ui(cfg) })
  }, ignoreInit = TRUE)

  # ── Toggle changed ───────────────────────────────────────────────────────────
  observeEvent(input$sincro_toggle_change, {
    chk  <- isTRUE(input$sincro_toggle_change$checked)
    tier <- tryCatch(shared$current_user_info()$tier, error = function(e) "")
    if (!tier %in% c("dev", "admin")) {
      showNotification("Sin permiso para cambiar la sincronización.", type = "error")
      return()
    }

    if (!chk) {
      # ── Turning OFF ─────────────────────────────────────────────────────────
      showModal(modalDialog(
        title     = tagList(icon("triangle-exclamation"), " Desactivar sincronización"),
        size      = "s",
        easyClose = FALSE,
        footer    = tagList(
          modalButton("Cancelar"),
          actionButton("do_sincro_desactivar",
                       tagList(icon("power-off"), " Sí, desactivar"),
                       class = "btn btn-danger")
        ),
        tags$p(
          "Los datos dejarán de compartirse entre cuentas.",
          tags$br(),
          tags$strong("Cada usuario conservará una copia de la Agenda actual"),
          " y podrá editarla de forma independiente. Esta acción no afecta los datos registrados."
        )
      ))

    } else {
      # ── Turning ON: load each user's personal agenda for selection ──────────
      all_users <- tryCatch(auth_load_usuarios(), error = function(e) NULL)
      if (is.null(all_users) || !nrow(all_users)) {
        showNotification("No se pudo cargar la lista de usuarios.", type = "error")
        return()
      }
      active_users <- all_users[
        (is.na(all_users$deleted) | all_users$deleted != TRUE) &
        (all_users$activo %in% TRUE), , drop = FALSE]

      # Build named list: username → pending items tibble
      agendas <- setNames(
        lapply(active_users$username, function(u) {
          tryCatch(
            load_pagar_hoy_user(u, client_id = shared$active_client_id()) |>
              dplyr::filter(status == "pending"),
            error = function(e) .schema_pagar_hoy()
          )
        }),
        active_users$username
      )

      output$sincro_preview_panel <- renderUI({ NULL })
      .sincro_activar_modal(active_users, agendas)
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # ── Preview selected user's agenda ──────────────────────────────────────────
  observeEvent(input$sincro_src_selected, {
    u     <- input$sincro_src_selected$user
    req(nzchar(u %||% ""))
    items <- tryCatch(
      load_pagar_hoy_user(u, client_id = shared$active_client_id()) |>
        dplyr::filter(status == "pending"),
      error = function(e) .schema_pagar_hoy()
    )
    output$sincro_preview_panel <- renderUI({
      if (!nrow(items)) {
        return(div(class = "text-muted small p-2 border rounded",
                   "Esta cuenta no tiene elementos en su Agenda de hoy."))
      }
      div(
        tags$h6(class = "fw-semibold mb-2 mt-2", "Vista previa:"),
        div(style = "max-height:180px; overflow-y:auto;",
          tags$table(class = "table table-sm table-striped mb-0",
            tags$thead(tags$tr(
              tags$th("Empresa"), tags$th("Parte"),
              tags$th(class = "text-end", "Importe"), tags$th("Mon.")
            )),
            tags$tbody(
              lapply(seq_len(min(nrow(items), 50L)), function(i) {
                tags$tr(
                  tags$td(items$Empresa[i]),
                  tags$td(items$Parte[i]),
                  tags$td(class = "text-end",
                          format(items$Importe[i], big.mark = ",", nsmall = 2, scientific = FALSE)),
                  tags$td(items$Moneda[i])
                )
              })
            )
          )
        )
      )
    })
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # ── Confirm ACTIVATE ─────────────────────────────────────────────────────────
  observeEvent(input$do_sincro_activar, {
    u <- tryCatch(input$sincro_src_selected$user, error = function(e) NULL)
    if (is.null(u) || !nzchar(u %||% "")) {
      showNotification("Selecciona un usuario primero.", type = "warning"); return()
    }
    chosen <- tryCatch(load_pagar_hoy_user(u, client_id = shared$active_client_id()),
                       error = function(e) .schema_pagar_hoy())

    tryCatch({
      save_pagar_hoy_sync(chosen, client_id = shared$active_client_id())
    }, error = function(e) {
      showNotification(paste("Error al guardar agenda sincronizada:", e$message), type = "error")
      return()
    })

    actor <- tryCatch(shared$current_user(), error = function(e) "admin")
    tryCatch(save_agenda_sync_config(TRUE, actor, client_id = shared$active_client_id()), error = function(e) NULL)

    .GlobalEnv$.agenda_sync$is_on   <- TRUE
    .GlobalEnv$.agenda_sync$data    <- chosen
    .GlobalEnv$.agenda_sync$version <- paste0(
      sample(c(letters, as.character(0:9)), 12, replace = TRUE), collapse = "")
    bump_sync_version("pagar_hoy_db")

    pagar_hoy_db(chosen)

    cfg_new <- list(is_enabled = TRUE, enabled_by = actor, enabled_at = Sys.time(),
                    .missing = FALSE)
    removeModal()
    show_settings_modal(input, output, session, shared)
    output$settings_panel <- renderUI({ .sincro_panel_ui(cfg_new) })
    showNotification(
      paste0("Sincronización activada con la agenda de «", u, "»."),
      type = "message", duration = 4)
  }, ignoreInit = TRUE)

  # ── Confirm DEACTIVATE ───────────────────────────────────────────────────────
  observeEvent(input$do_sincro_desactivar, {
    current_data <- tryCatch(
      .GlobalEnv$.agenda_sync$data %||% load_pagar_hoy_sync(client_id = shared$active_client_id()),
      error = function(e) .schema_pagar_hoy()
    )

    # Copy current shared agenda to every active user's personal file.
    # Also populate .agenda_user_cache so running sessions pick up their personal
    # copy on the next sync-bus poll without needing a page refresh.
    all_users <- tryCatch(auth_load_usuarios(), error = function(e) NULL)
    if (!is.null(all_users)) {
      active <- all_users[
        (is.na(all_users$deleted) | all_users$deleted != TRUE) &
        (all_users$activo %in% TRUE), , drop = FALSE]
      if (!is.list(tryCatch(.GlobalEnv$.agenda_user_cache, error = function(e) NULL)))
        .GlobalEnv$.agenda_user_cache <- list()
      for (u in active$username) {
        tryCatch(save_pagar_hoy_user(current_data, u, client_id = shared$active_client_id()),
                 error = function(e)
                   message("[SINCRO] copy-to-user failed for '", u, "': ", e$message))
        .GlobalEnv$.agenda_user_cache[[tolower(trimws(u))]] <- current_data
      }
    }

    actor <- tryCatch(shared$current_user(), error = function(e) "unknown")
    tryCatch(save_agenda_sync_config(FALSE, actor, client_id = shared$active_client_id()), error = function(e) NULL)

    .GlobalEnv$.agenda_sync$is_on   <- FALSE
    .GlobalEnv$.agenda_sync$data    <- NULL
    .GlobalEnv$.agenda_sync$version <- paste0(
      sample(c(letters, as.character(0:9)), 12, replace = TRUE), collapse = "")
    bump_sync_version("pagar_hoy_db")

    my_data <- tryCatch(load_pagar_hoy_user(actor, client_id = shared$active_client_id()),
                        error = function(e) current_data)
    pagar_hoy_db(my_data)

    cfg_new <- list(is_enabled = FALSE, enabled_by = actor, enabled_at = Sys.time(),
                    .missing = FALSE)
    removeModal()
    show_settings_modal(input, output, session, shared)
    output$settings_panel <- renderUI({ .sincro_panel_ui(cfg_new) })
    showNotification(
      "Sincronización desactivada. Cada cuenta mantiene su propia Agenda de hoy.",
      type = "message", duration = 4)
  }, ignoreInit = TRUE)
}

