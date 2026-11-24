# =============================================================================
# R/notes_handlers.R
# Unified notes panel — AR and AP notes shown together, single toggle button.
# =============================================================================

notes_handlers <- function(input, output, session, shared,
                            notes_df, current_user) {

  notes_visible   <- reactiveVal(FALSE)
  active_note_id  <- reactiveVal(NULL)
  active_note_lgr <- reactiveVal(NULL)

  # T91a: may this session's user edit / delete this note? Its author
  # (nota_es_autor()); deleting someone else's needs can_moderate_panorama.
  # Checked on the server at every step: the browser only hides the icons.
  .nota_puede <- function(row, accion = c("editar", "borrar")) {
    accion <- match.arg(accion)
    info <- tryCatch(shared$current_user_info(), error = function(e) list())
    autor <- nota_es_autor(row, info$account_code %||% "", tryCatch(current_user(), error = function(e) ""))
    ok <- autor || (identical(accion, "borrar") &&
                      isTRUE(tryCatch(puede(shared$usuario(), "can_moderate_panorama"), error = function(e) FALSE)))
    if (!ok) showNotification(tr("tablas.notas.aviso.solo_autor"), type = "warning", duration = 6)
    ok
  }

  .last_notes_refresh <- local(Sys.time() - 60)

  .ensure_notes_fresh <- function(forzar = FALSE) {
    now <- Sys.time()
    if (!forzar && as.numeric(now - .last_notes_refresh, units = "secs") < 30)
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

  # ── Opened from a Panorama aviso (T81): NOTA:<year>-<month> ────────────────
  # The panel opens (never toggles closed) in the note's month, read fresh.
  observeEvent(input$abrir_notas, {
    mes <- notas_enlace_mes(input$abrir_notas)
    if (is.null(mes)) return()
    shinyWidgets::updateAirDateInput(session, "month_sel", value = mes)
    .ensure_notes_fresh(forzar = TRUE)
    notes_visible(TRUE)
  }, ignoreInit = TRUE)

  # ── Month filter — all ledgers visible together ────────────────────────────
  .notes_month <- function() {
    ms        <- input$month_sel %||% hoy_usuario()
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
                        mes_es(input$month_sel %||% hoy_usuario())),
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
    df_base <- df   # CM09: only this note is applied on S3 as it is now
    now    <- Sys.time()
    info   <- shared$current_user_info()

    if (is.null(nid)) {
      ms <- input$month_sel %||% hoy_usuario()
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
      if (length(idx) && !.nota_puede(df[idx[1], ], "editar")) return(invisible(NULL))   # T91a, again at the write
      era_publica <- length(idx) > 0L && notas_es_publica(df$visibility[idx[1]])
      if (length(idx)) {
        df$title[idx]      <- input$note_title_input %||% "(Sin título)"
        df$body[idx]       <- input$note_body_input  %||% ""
        df$visibility[idx] <- visibility
        df$updated[idx]    <- now
      }
    }

    notes_df(df)
    # T72-G: the save carries the action's one log row (the write layer
    # writes it once the save worked). A personal note is its author's: its
    # row says only that one was created or edited, never its title.
    nota_id <- nid %||% df$id[nrow(df)]
    titulo  <- input$note_title_input %||% "(Sin título)"
    nueva   <- is.null(nid)
    clave   <- paste0("tablas.notas.log.", if (nueva) "creada" else "editada",
                      if (notas_es_publica(visibility)) "" else "_personal")
    ev_nota <- list(accion = if (nueva) "crear_nota" else "editar_nota", modulo = "notes",
                    usuario = tryCatch(current_user(), error = function(e) "system"),
                    home = tryCatch(shared$home_client_id(), error = function(e) NULL),
                    descripcion = tr(clave, titulo = titulo), target_id = nota_id,
                    metadata = list(ledger = ledger, visibilidad = visibility))
    guardado <- tryCatch({ save_notes(df, client_id = shared$effective_client_id(), base = df_base, evento = ev_nota); TRUE },
                         error = function(e) { message("[NOTES] save failed: ", conditionMessage(e)); FALSE })
    if (!guardado) {
      notes_df(df_base)
      showNotification(tr("comun.error_guardar"), type = "error", duration = NULL)
      return(invisible(NULL))
    }
    bump_sync_version("notes_df", client_id = shared$effective_client_id())
    # T81: news for everyone when it becomes public (new, or personal made
    # public); editing a public note does not tell anyone again.
    if (notas_es_publica(visibility) && (is.null(nid) || !isTRUE(era_publica))) {
      nota <- df[df$id == (nid %||% df$id[nrow(df)]), , drop = FALSE]
      if (nrow(nota)) notas_aviso_publicar(shared$effective_client_id(), nota[1, ],
                                           por = tryCatch(current_user(), error = function(e) ""))
    }
    removeModal()
    active_note_id(NULL)
    active_note_lgr(NULL)
    showNotification(tr(if (visibility == "personal") "tablas.notas.aviso.guardada.personal" else "tablas.notas.aviso.guardada.publica"),
                     type = "message", duration = 2)
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
            # T91a: only its author edits it, checked here (the pencil is only
            # hidden in the browser; an input can be sent by hand).
            if (!.nota_puede(row[1, ], "editar")) return()
            .show_note_modal(row = row[1, ])
          }, ignoreInit = TRUE),

        delete = observeEvent(
          input[[paste0("delete_note_", nid)]], {
            df  <- notes_df()
            row <- df[df$id == nid, , drop = FALSE]
            if (!nrow(row)) return()
            if (!.nota_puede(row[1, ], "borrar")) return()   # T91a: author or moderator
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
    deleted_row <- notes_df()[notes_df()$id == nid, , drop = FALSE]
    if (nrow(deleted_row) && !.nota_puede(deleted_row[1, ], "borrar")) return()   # T91a, again at the write
    df_base <- notes_df()
    df <- dplyr::filter(df_base, .data$id != !!nid)
    notes_df(df)
    # T72-G: its log row (the same words and metadata as the log_action it
    # replaces) rides with the save.
    ev_nota <- list(accion = "eliminar_nota", modulo = "notes",
                    usuario = tryCatch(current_user(), error = function(e) "system"),
                    home = tryCatch(shared$home_client_id(), error = function(e) NULL),
                    descripcion = paste0("Nota eliminada: ", if (nrow(deleted_row)) deleted_row$title[1] else nid),
                    target_id = nid,
                    metadata = list(ledger = if (nrow(deleted_row)) deleted_row$ledger[1] else NA_character_))
    guardado <- tryCatch({ save_notes(df, client_id = shared$effective_client_id(), base = df_base, evento = ev_nota); TRUE },
                         error = function(e) { message("[NOTES] save failed: ", conditionMessage(e)); FALSE })
    if (!guardado) {
      notes_df(df_base)
      showNotification(tr("comun.error_guardar"), type = "error", duration = NULL)
      return(invisible(NULL))
    }
    bump_sync_version("notes_df", client_id = shared$effective_client_id())
    removeModal()
    active_note_id(NULL)
    active_note_lgr(NULL)
    showNotification(tr("tablas.notas.aviso.eliminada"), type = "message", duration = 2)
    # T81: nobody is sent to a note that is gone.
    notas_aviso_retirar(shared$effective_client_id(), nid,
                        por = tryCatch(current_user(), error = function(e) ""))
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

# ── T81: public notes as Panorama avisos ─────────────────────────────────────
# NA visibility reads as public (see .schema_notes).
# T91a: is the user (account code, display name) the author of note `row`?
# The same rule the notes panel used only to hide its icons
# (ui_components.R), minus its "both without an account code" case, which
# made anyone without a code the author of every note without one: the code
# when both have it, else the display name when both have it, else no.
nota_es_autor <- function(row, user_code = "", user_name = "") {
  limpio <- function(x) { x <- as.character(x %||% "")[1]; if (is.na(x)) "" else trimws(x) }
  nc <- limpio(row$author_code); uc <- limpio(user_code)
  if (nzchar(nc) && nzchar(uc)) return(identical(nc, uc))
  na <- limpio(row$author); un <- limpio(user_name)
  nzchar(na) && nzchar(un) && !identical(un, "unknown") && identical(na, un)
}

notas_es_publica <- function(visibility) {
  v <- as.character(visibility %||% NA_character_)[1]
  is.na(v) || identical(v, "public")
}

.NOTAS_AVISO_CUERPO_MAX <- 200L

# One aviso for every client tier (automatico: niveles_cliente(), no
# permission: every tier reads notes). clave nota|<id> with repetir = FALSE:
# the same note never tells anyone twice (two sessions, a retry, made
# personal and public again). `por` is the author's user: their own aviso is
# not news to them (panorama_feed). It never throws: the note is already
# saved, and an aviso that fails must not look like a failed save.
notas_aviso_publicar <- function(client_id, nota, por = "") {
  tryCatch({
    cuerpo <- trimws(as.character(nota$body %||% ""))
    if (is.na(cuerpo)) cuerpo <- ""
    if (nchar(cuerpo) > .NOTAS_AVISO_CUERPO_MAX)
      cuerpo <- paste0(substr(cuerpo, 1L, .NOTAS_AVISO_CUERPO_MAX - 1L), "…")
    avisos_agregar(client_id, list(
      tipo     = "automatico",
      titulo   = tr("avisos.nota.titulo", autor = as.character(nota$author %||% ""),
                    titulo = as.character(nota$title %||% "")),
      cuerpo   = cuerpo,
      severidad = "info",
      clave    = paste0("nota|", nota$id),
      repetir  = FALSE,
      enlace   = notas_enlace(nota$year, nota$month)),
      por = if (nzchar(por %||% "")) por else "sistema")
  }, error = function(e) {
    message("[NOTES] T81: the note's aviso failed: ", conditionMessage(e))
    invisible(NULL)
  })
}

# Retires the live aviso of a deleted note (nothing is deleted from avisos).
# Never throws, as above.
notas_aviso_retirar <- function(client_id, nota_id, por = "") {
  tryCatch({
    a <- avisos_leer(client_id)
    ids <- a$id[a$clave %in% paste0("nota|", nota_id) & !(a$retirado %in% TRUE)]
    for (id in ids) avisos_retirar(client_id, id, por = if (nzchar(por %||% "")) por else "sistema")
    invisible(length(ids))
  }, error = function(e) {
    message("[NOTES] T81: the note's aviso was not retired: ", conditionMessage(e))
    invisible(NULL)
  })
}
