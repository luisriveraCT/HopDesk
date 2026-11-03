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

  # ── Preview of the chosen account's agenda (hd_table) ───────────────────────
  # Every pending item (it stopped at 50 without saying so), with the
  # engine's counter. preview_visible only changes when the panel must
  # appear or go, so choosing another account repaints the rows, not the
  # container.
  preview_items   <- reactiveVal(NULL)
  preview_visible <- reactiveVal(FALSE)
  hd_table_server(
    "sincro_t", tabla = "sincro_preview",
    datos = reactive({
      items <- preview_items()
      req(!is.null(items))
      ids <- as.character(items$id)
      sin <- is.na(ids) | !nzchar(ids)
      ids[sin] <- paste(items$ledger, items$Empresa, items$Moneda, items$Documento, sep = "|")[sin]
      data.frame(id = make.unique(ids, sep = "#"), Empresa = items$Empresa, Parte = items$Parte,
                 Importe = items$Importe, Moneda = items$Moneda, stringsAsFactors = FALSE)
    }),
    columnas = list(
      col_empresa("Empresa", "tablas.sincro.empresa"),
      col_texto("Parte",     "tablas.sincro.parte"),
      col_dinero("Importe",  "tablas.sincro.importe", moneda = "Moneda", sumar = FALSE),
      col_texto("Moneda",    "tablas.sincro.moneda", recorte = FALSE)
    ),
    piezas = "recorte",
    vacio  = "tablas.sincro.vacio"
  )
  output$sincro_preview_panel <- renderUI({
    if (!isTRUE(preview_visible())) return(NULL)
    div(
      tags$h6(class = "fw-semibold mb-2 mt-2", tr("tablas.sincro.vista_previa")),
      hd_table_ui("sincro_t", alto = "auto")
    )
  })

  # A failed read or save stops the step where it is: the user is told what
  # did not happen, and it is logged as <accion>_fallido. Nothing after it
  # runs -- no "activada", no config saved as on, no success log.
  .sincro_fallo <- function(accion, aviso, detalle, target_id = NA_character_) {
    showNotification(aviso, type = "error", duration = 10)
    tryCatch(log_action(
      user        = tryCatch(shared$current_user(), error = function(e) "sistema"),
      module      = "settings_sincro",
      action      = paste0(accion, "_fallido"),
      description = detalle,
      target_id   = target_id,
      client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
      viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
    ), error = function(e) NULL)
    invisible(NULL)
  }

  # ── Open panel ──────────────────────────────────────────────────────────────
  observeEvent(input$stg_btn_sincro, {
    cfg <- tryCatch(load_agenda_sync_config(client_id = shared$effective_client_id()),
                    error = function(e) list(is_enabled = FALSE, .missing = TRUE))
    output$settings_panel <- renderUI({ .sincro_panel_ui(cfg) })
  }, ignoreInit = TRUE)

  # ── Toggle changed ───────────────────────────────────────────────────────────
  observeEvent(input$sincro_toggle_change, {
    chk  <- isTRUE(input$sincro_toggle_change$checked)
    if (!puede_o_avisa(shared, "can_manage_sync")) return()

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
      all_users <- tryCatch(auth_load_usuarios(client_id = shared$effective_client_id()), error = function(e) NULL)
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
            load_pagar_hoy_user(u, client_id = shared$effective_client_id()) |>
              dplyr::filter(status == "pending"),
            error = function(e) .schema_pagar_hoy()
          )
        }),
        active_users$username
      )

      preview_visible(FALSE)
      preview_items(NULL)
      .sincro_activar_modal(active_users, agendas)
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # ── Preview selected user's agenda ──────────────────────────────────────────
  observeEvent(input$sincro_src_selected, {
    u     <- input$sincro_src_selected$user
    req(nzchar(u %||% ""))
    items <- tryCatch(
      load_pagar_hoy_user(u, client_id = shared$effective_client_id()) |>
        dplyr::filter(status == "pending"),
      error = function(e) .schema_pagar_hoy()
    )
    preview_items(items)
    preview_visible(TRUE)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # ── Confirm ACTIVATE ─────────────────────────────────────────────────────────
  observeEvent(input$do_sincro_activar, {
    u <- tryCatch(input$sincro_src_selected$user, error = function(e) NULL)
    if (is.null(u) || !nzchar(u %||% "")) {
      showNotification("Selecciona un usuario primero.", type = "warning"); return()
    }
    cid <- shared$effective_client_id()
    # The chosen account's agenda: a failed read must not start the sync from
    # an empty one (it used to become an empty agenda here).
    chosen <- tryCatch(load_pagar_hoy_user(u, client_id = cid), error = function(e) e)
    if (inherits(chosen, "error")) {
      return(.sincro_fallo("activar_sincronizacion_agenda", tr("tablas.sincro.error_activar"),
                           paste0("No se pudo leer la agenda de '", u, "': ", conditionMessage(chosen)), u))
    }
    ok <- tryCatch({ save_pagar_hoy_sync(chosen, client_id = cid); TRUE }, error = function(e) e)
    if (!isTRUE(ok)) {
      return(.sincro_fallo("activar_sincronizacion_agenda", tr("tablas.sincro.error_activar"),
                           paste0("No se pudo guardar la agenda compartida: ", conditionMessage(ok)), u))
    }

    actor <- tryCatch(shared$current_user(), error = function(e) "sistema")
    ok <- tryCatch({ save_agenda_sync_config(TRUE, actor, client_id = cid); TRUE }, error = function(e) e)
    if (!isTRUE(ok)) {
      return(.sincro_fallo("activar_sincronizacion_agenda", tr("tablas.sincro.error_activar"),
                           paste0("No se pudo guardar la configuración de la sincronización: ", conditionMessage(ok)), u))
    }

    bump_sync_version("pagar_hoy_db", client_id = shared$effective_client_id())

    pagar_hoy_db(chosen)

    cfg_new <- list(is_enabled = TRUE, enabled_by = actor, enabled_at = Sys.time(),
                    .missing = FALSE)
    removeModal()
    show_settings_modal(input, output, session, shared)
    output$settings_panel <- renderUI({ .sincro_panel_ui(cfg_new) })
    showNotification(
      paste0("Sincronización activada con la agenda de «", u, "»."),
      type = "message", duration = 4)
    log_action(
      user        = actor,
      module      = "settings_sincro",
      action      = "activar_sincronizacion_agenda",
      description = paste0("Sincronización de Agenda de hoy activada, usando como base la agenda de '", u, "'"),
      target_id   = u,
      metadata    = list(source_user = u),
      client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
      viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
    )
  }, ignoreInit = TRUE)

  # ── Confirm DEACTIVATE ───────────────────────────────────────────────────────
  observeEvent(input$do_sincro_desactivar, {
    cid <- shared$effective_client_id()
    # The shared agenda every account gets a copy of. A failed read must stop
    # here: it used to become an empty agenda, copied to every account.
    current_data <- tryCatch(.pagar_hoy_vivo(TRUE, NULL, cid), error = function(e) e)
    if (inherits(current_data, "error")) {
      return(.sincro_fallo("desactivar_sincronizacion_agenda", tr("tablas.sincro.error_desactivar"),
                           paste0("No se pudo leer la agenda compartida: ", conditionMessage(current_data))))
    }

    # Copy current shared agenda to every active user's personal file.
    # Running sessions pick up their personal copy on the next sync-bus poll
    # (the loader reads S3). If any copy fails the sync stays on: that account
    # would otherwise be left with an old or empty agenda of its own.
    all_users <- tryCatch(auth_load_usuarios(client_id = cid), error = function(e) NULL)
    if (is.null(all_users)) {
      return(.sincro_fallo("desactivar_sincronizacion_agenda", tr("tablas.sincro.error_desactivar"),
                           "No se pudo cargar la lista de usuarios para copiarles la agenda."))
    }
    active <- all_users[
      (is.na(all_users$deleted) | all_users$deleted != TRUE) &
      (all_users$activo %in% TRUE), , drop = FALSE]
    fallidos <- character(0)
    for (u in active$username) {
      ok <- tryCatch({ save_pagar_hoy_user(current_data, u, client_id = cid); TRUE }, error = function(e) e)
      if (!isTRUE(ok)) {
        message("[SINCRO] copy-to-user failed for '", u, "': ", conditionMessage(ok))
        fallidos <- c(fallidos, u)
      }
    }
    if (length(fallidos)) {
      return(.sincro_fallo("desactivar_sincronizacion_agenda",
                           tr("tablas.sincro.error_copia", usuarios = paste(fallidos, collapse = ", ")),
                           paste0("No se pudo copiar la agenda a: ", paste(fallidos, collapse = ", "),
                                  ". La sincronización sigue activa."),
                           paste(fallidos, collapse = ",")))
    }

    actor <- tryCatch(shared$current_user(), error = function(e) "unknown")
    ok <- tryCatch({ save_agenda_sync_config(FALSE, actor, client_id = cid); TRUE }, error = function(e) e)
    if (!isTRUE(ok)) {
      return(.sincro_fallo("desactivar_sincronizacion_agenda", tr("tablas.sincro.error_desactivar"),
                           paste0("No se pudo guardar la configuración de la sincronización: ", conditionMessage(ok))))
    }

    bump_sync_version("pagar_hoy_db", client_id = shared$effective_client_id())

    my_data <- tryCatch(load_pagar_hoy_user(actor, client_id = shared$effective_client_id()),
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
    log_action(
      user        = actor,
      module      = "settings_sincro",
      action      = "desactivar_sincronizacion_agenda",
      description = "Sincronización de Agenda de hoy desactivada; cada cuenta vuelve a su propia agenda",
      client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
      viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
    )
  }, ignoreInit = TRUE)
}

