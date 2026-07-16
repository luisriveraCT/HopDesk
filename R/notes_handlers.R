# =============================================================================
# R/notes_handlers.R
# Unified notes panel — AR and AP notes shown together, single toggle button.
# =============================================================================

notes_handlers <- function(input, output, session, shared,
                            notes_df, current_user) {

  notes_visible   <- reactiveVal(FALSE)
  active_note_id  <- reactiveVal(NULL)
  active_note_lgr <- reactiveVal(NULL)

  .last_notes_refresh <- local(Sys.time() - 60)

  .ensure_notes_fresh <- function() {
    now <- Sys.time()
    if (as.numeric(now - .last_notes_refresh, units = "secs") < 30)
      return(invisible(NULL))
    fresh <- tryCatch(load_notes(client_id = shared$effective_client_id()), error = function(e) NULL)
    if (!is.null(fresh) && is.data.frame(fresh)) notes_df(fresh)
    .last_notes_refresh <<- now
  }

  # ── Toggle ──────────────────────────────────────────────────────────────────
  observeEvent(input$toggle_notes, {
    notes_visible(!notes_visible())
    if (notes_visible()) .ensure_notes_fresh()
  }, ignoreInit = TRUE)

  # ── Month filter — all ledgers visible together ────────────────────────────
  .notes_month <- function() {
    ms        <- input$month_sel %||% Sys.Date()
    df        <- notes_df()
    if (is.null(df) || !is.data.frame(df)) df <- .schema_notes()
    info      <- shared$current_user_info()
    user_code <- info$account_code %||% ""
    user_name <- info$name %||% ""
    df |>
      dplyr::filter(
        year  == lubridate::year(ms),
        month == lubridate::month(ms),
        is.na(visibility) | visibility == "public" |
          (visibility == "personal" & (
            (nzchar(!!user_code) & !is.na(.data$author_code) & .data$author_code == !!user_code) |
            (nzchar(!!user_name) & !is.na(.data$author) & .data$author == !!user_name) |
            (!nzchar(!!user_code) & (is.na(.data$author_code) | !nzchar(.data$author_code %||% "")))
          ))
      ) |>
      dplyr::slice(rev(seq_len(dplyr::n())))
  }

  # ── Panel render ────────────────────────────────────────────────────────────
  output$notes_panel <- renderUI({
    if (!notes_visible()) return(NULL)
    tagList(
      div(class = "notes-panel-wrapper",
        div(class = "notes-resize-handle"),
        div(class = "notes-panel-scroll",
          div(class = "notes-panel-header",
            tags$strong(class = "small text-primary", "Notas — ",
                        mes_es(input$month_sel %||% Sys.Date())),
            actionButton("add_note", tagList(icon("plus"), " Agregar"),
                         class = "btn btn-sm btn-primary")
          ),
          notes_panel_ui(.notes_month(),
                         current_user(),
                         shared$current_user_info()$account_code %||% "")
        )
      ),
      tags$script(HTML("
        (function() {
          var saved = sessionStorage.getItem('notesH');
          if (saved) {
            $('.notes-panel-wrapper').css({ height: saved + 'px', maxHeight: 'none' });
          }
          $(document).off('mousedown.notesResize').on('mousedown.notesResize', '.notes-resize-handle', function(e) {
            e.preventDefault();
            var $w = $('.notes-panel-wrapper');
            var startY = e.clientY, startH = $w.outerHeight();
            $(document).on('mousemove.notesResizeMove', function(e) {
              var delta = startY - e.clientY;
              var newH = Math.max(100, Math.min(window.innerHeight * 0.8, startH + delta));
              $w.css({ height: newH + 'px', maxHeight: 'none' });
            });
            $(document).on('mouseup.notesResizeMove', function() {
              sessionStorage.setItem('notesH', $('.notes-panel-wrapper').outerHeight());
              $(document).off('mousemove.notesResizeMove mouseup.notesResizeMove');
            });
          });
        })();
      "))
    )
  })

  # ── Open note modal ──────────────────────────────────────────────────────────
  .show_note_modal <- function(row = NULL) {
    is_new <- is.null(row)
    active_note_lgr("AR")
    active_note_id(if (!is_new) row$id else NULL)

    showModal(modalDialog(
      title     = if (is_new) "Nueva nota" else "Editar nota",
      easyClose = TRUE,
      footer = tagList(
        actionButton("note_cancel",        "Cancelar",      class = "btn btn-secondary"),
        actionButton("note_save_personal", tagList(icon("lock"),  " Nota personal"),
                     class = "btn btn-outline-secondary"),
        actionButton("note_save_public",   tagList(icon("globe"), " Nota pública"),
                     class = "btn btn-primary")
      ),
      textInput("note_title_input", "Título",
        value = if (!is_new) row$title %||% "" else ""),
      textAreaInput("note_body_input", "Detalle", rows = 6,
        value = if (!is_new) row$body %||% "" else "")
    ))
  }

  # ── Save logic ───────────────────────────────────────────────────────────────
  .do_save_note <- function(visibility) {
    ledger <- active_note_lgr() %||% "AR"
    nid    <- active_note_id()
    df     <- notes_df()
    now    <- Sys.time()
    info   <- shared$current_user_info()

    if (is.null(nid)) {
      ms <- input$month_sel %||% Sys.Date()
      df <- dplyr::bind_rows(df, tibble::tibble(
        ledger      = ledger,
        year        = lubridate::year(ms),
        month       = lubridate::month(ms),
        id          = uuid::UUIDgenerate(),
        title       = input$note_title_input %||% "(Sin título)",
        body        = input$note_body_input  %||% "",
        author      = info$name        %||% info$user %||% "?",
        author_code = info$account_code %||% "",
        visibility  = visibility,
        created     = now,
        updated     = now
      ))
    } else {
      idx <- which(df$id == nid)
      if (length(idx)) {
        df$title[idx]      <- input$note_title_input %||% "(Sin título)"
        df$body[idx]       <- input$note_body_input  %||% ""
        df$visibility[idx] <- visibility
        df$updated[idx]    <- now
      }
    }

    notes_df(df)
    save_notes(df, client_id = shared$effective_client_id())
    bump_sync_version("notes_df")
    removeModal()
    active_note_id(NULL)
    active_note_lgr(NULL)
    lbl <- if (visibility == "personal") "personal" else "pública"
    showNotification(paste0("Nota ", lbl, " guardada."), type = "message", duration = 2)
  }

  observeEvent(input$add_note,           { .show_note_modal()          }, ignoreInit = TRUE)
  observeEvent(input$note_save_public,   { .do_save_note("public")     }, ignoreInit = TRUE)
  observeEvent(input$note_save_personal, { .do_save_note("personal")   }, ignoreInit = TRUE)

  observeEvent(input$note_cancel, {
    removeModal()
    active_note_id(NULL)
    active_note_lgr(NULL)
  }, ignoreInit = TRUE)

  # ── Dynamic edit/delete observers ───────────────────────────────────────────
  note_obs <- reactiveVal(list())

  .rebuild_observers <- function() {
    notes <- .notes_month()
    ids   <- notes$id %||% character()

    lapply(note_obs(), function(o) {
      try(o$edit$destroy(),   silent = TRUE)
      try(o$delete$destroy(), silent = TRUE)
    })

    note_obs(lapply(ids, function(nid) {
      list(
        edit = observeEvent(
          input[[paste0("edit_note_", nid)]], {
            df  <- notes_df()
            row <- df[df$id == nid, , drop = FALSE]
            if (!nrow(row)) return()
            .show_note_modal(row = row[1, ])
          }, ignoreInit = TRUE),

        delete = observeEvent(
          input[[paste0("delete_note_", nid)]], {
            df  <- notes_df()
            row <- df[df$id == nid, , drop = FALSE]
            if (!nrow(row)) return()
            active_note_id(nid)
            active_note_lgr(row$ledger[1] %||% "AR")
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

  observeEvent(input$note_delete_confirm, {
    nid <- active_note_id(); req(nid)
    df <- dplyr::filter(notes_df(), .data$id != !!nid)
    notes_df(df)
    save_notes(df, client_id = shared$effective_client_id())
    bump_sync_version("notes_df")
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

  observe({
    .rebuild_observers()
  }) |> bindEvent(notes_df(), input$month_sel, ignoreNULL = FALSE)
}
