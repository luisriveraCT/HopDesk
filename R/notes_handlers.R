# =============================================================================
# R/notes_handlers.R
# Fixed IDs, easyClose=TRUE, observers at proper server scope.
# =============================================================================

notes_handlers <- function(input, output, session, shared,
                            notes_df, current_user) {

  notes_visible_ar <- reactiveVal(FALSE)
  notes_visible_ap <- reactiveVal(FALSE)
  active_note_id   <- reactiveVal(NULL)
  active_note_lgr  <- reactiveVal(NULL)

  # ── Toggles ────────────────────────────────────────────────────────────────
  observeEvent(input$toggle_notes_ar, { notes_visible_ar(!notes_visible_ar()) })
  observeEvent(input$toggle_notes_ap, { notes_visible_ap(!notes_visible_ap()) })

  # ── Month filter ───────────────────────────────────────────────────────────
  .notes_month <- function(ledger) {
    req(input$month_sel)
    notes_df() |>
      dplyr::filter(
        .data$ledger == !!ledger,
        year  == lubridate::year(input$month_sel),
        month == lubridate::month(input$month_sel)
      )
  }

  # ── Panel renders ──────────────────────────────────────────────────────────
  output$notes_panel_ar <- renderUI({
    if (!notes_visible_ar()) return(NULL)
    div(class = "notes-panel-wrapper",
      div(class = "d-flex justify-content-between align-items-center px-3 pt-2 pb-1 border-bottom",
        tags$strong(class = "small", "Notas – ", mes_es(req(input$month_sel))),
        actionButton("add_note_ar", "+ Agregar", class = "btn btn-sm btn-outline-primary")
      ),
      notes_panel_ui(.notes_month("AR"), "AR", "ar")
    )
  })

  output$notes_panel_ap <- renderUI({
    if (!notes_visible_ap()) return(NULL)
    div(class = "notes-panel-wrapper",
      div(class = "d-flex justify-content-between align-items-center px-3 pt-2 pb-1 border-bottom",
        tags$strong(class = "small", "Notas – ", mes_es(req(input$month_sel))),
        actionButton("add_note_ap", "+ Agregar", class = "btn btn-sm btn-outline-primary")
      ),
      notes_panel_ui(.notes_month("AP"), "AP", "ap")
    )
  })

  # ── Open note modal (fixed IDs: note_save, note_cancel) ───────────────────
  .show_note_modal <- function(ledger, row = NULL) {
    is_new <- is.null(row)
    active_note_lgr(ledger)
    active_note_id(if (!is_new) row$id else NULL)

    showModal(modalDialog(
      title     = if (is_new) "Nueva nota" else "Editar nota",
      easyClose = TRUE,
      footer = tagList(
        actionButton("note_cancel", "Cancelar", class = "btn btn-secondary"),
        actionButton("note_save",   "Guardar",  class = "btn btn-primary")
      ),
      textInput("note_title_input", "Título",
        value = if (!is_new) row$title %||% "" else ""),
      textAreaInput("note_body_input", "Detalle", rows = 6,
        value = if (!is_new) row$body %||% "" else "")
    ))
  }

  # ── Add note buttons ───────────────────────────────────────────────────────
  observeEvent(input$add_note_ar, {
    .show_note_modal("AR")
  }, ignoreInit = TRUE)

  observeEvent(input$add_note_ap, {
    .show_note_modal("AP")
  }, ignoreInit = TRUE)

  # ── Save note ──────────────────────────────────────────────────────────────
  observeEvent(input$note_save, {
    ledger <- active_note_lgr(); req(ledger)
    nid    <- active_note_id()
    df     <- notes_df()
    now    <- Sys.time()

    if (is.null(nid)) {
      df <- dplyr::bind_rows(df, tibble::tibble(
        ledger  = ledger,
        year    = lubridate::year(input$month_sel),
        month   = lubridate::month(input$month_sel),
        id      = uuid::UUIDgenerate(),
        title   = input$note_title_input %||% "(Sin título)",
        body    = input$note_body_input  %||% "",
        author  = current_user(),
        created = now,
        updated = now
      ))
    } else {
      idx <- which(df$ledger == ledger & df$id == nid)
      if (length(idx)) {
        df$title[idx]   <- input$note_title_input %||% "(Sin título)"
        df$body[idx]    <- input$note_body_input  %||% ""
        df$updated[idx] <- now
      }
    }

    notes_df(df)
    save_notes(df)
    removeModal()
    active_note_id(NULL)
    active_note_lgr(NULL)
    showNotification("Nota guardada.", type = "message", duration = 2)
  }, ignoreInit = TRUE)

  # ── Cancel note ────────────────────────────────────────────────────────────
  observeEvent(input$note_cancel, {
    removeModal()
    active_note_id(NULL)
    active_note_lgr(NULL)
  }, ignoreInit = TRUE)

  # ── Dynamic edit/delete observers ─────────────────────────────────────────
  note_obs_ar <- reactiveVal(list())
  note_obs_ap <- reactiveVal(list())

  .rebuild_observers <- function(ledger) {
    obs_rv <- if (ledger == "AR") note_obs_ar else note_obs_ap
    ns_pre <- tolower(ledger)
    notes  <- .notes_month(ledger)
    ids    <- notes$id %||% character()

    lapply(obs_rv(), function(o) {
      try(o$edit$destroy(),   silent = TRUE)
      try(o$delete$destroy(), silent = TRUE)
    })

    obs_rv(lapply(ids, function(nid) {
      list(
        edit = observeEvent(
          input[[paste0("edit_note_", ns_pre, "_", nid)]], {
            df  <- notes_df()
            row <- df[df$ledger == ledger & df$id == nid, , drop = FALSE]
            if (!nrow(row)) return()
            .show_note_modal(ledger, row = row[1, ])
          }, ignoreInit = TRUE),

        delete = observeEvent(
          input[[paste0("delete_note_", ns_pre, "_", nid)]], {
            df  <- notes_df()
            row <- df[df$ledger == ledger & df$id == nid, , drop = FALSE]
            if (!nrow(row)) return()
            # Store which note to delete, confirm via fixed-ID modal
            active_note_id(nid)
            active_note_lgr(ledger)
            showModal(modalDialog(
              title = "Eliminar nota", size = "s", easyClose = TRUE,
              paste0("¿Eliminar \"", row$title[1], "\"?"),
              footer = tagList(
                actionButton("note_delete_cancel",  "Cancelar",
                             class = "btn btn-secondary"),
                actionButton("note_delete_confirm", "Eliminar",
                             class = "btn btn-danger")
              )
            ))
          }, ignoreInit = TRUE)
      )
    }))
  }

  # ── Delete confirm/cancel (fixed IDs) ─────────────────────────────────────
  observeEvent(input$note_delete_confirm, {
    ledger <- active_note_lgr(); nid <- active_note_id()
    req(ledger, nid)
    df <- dplyr::filter(notes_df(), !(ledger == !!ledger & .data$id == !!nid))
    notes_df(df)
    save_notes(df)
    removeModal()
    active_note_id(NULL)
    active_note_lgr(NULL)
    showNotification("Nota eliminada.", type = "message", duration = 2)
  }, ignoreInit = TRUE)

  observeEvent(input$note_delete_cancel, {
    removeModal()
    active_note_id(NULL)
    active_note_lgr(NULL)
  }, ignoreInit = TRUE)

  # ── Rebuild observers when notes or month changes ─────────────────────────
  observe({
    .rebuild_observers("AR")
  }) |> bindEvent(notes_df(), input$month_sel, ignoreNULL = FALSE)

  observe({
    .rebuild_observers("AP")
  }) |> bindEvent(notes_df(), input$month_sel, ignoreNULL = FALSE)
}