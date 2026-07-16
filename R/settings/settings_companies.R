# =============================================================================
# R/settings/settings_companies.R
# Socios Comerciales UI helpers and settings_companies_observer().
# =============================================================================
# (Originally part of R/settings_module.R; split in Stage 7.1 for session efficiency.)

# =============================================================================
# SOCIOS COMERCIALES (COMPANIES) — UI helpers
# =============================================================================

.companies_panel_ui <- function() {
  div(
    div(class = "d-flex align-items-center gap-2 mb-2",
      tags$h6(class = "fw-semibold mb-0",
              tagList(icon("handshake"), " Socios Comerciales")),
      uiOutput("cmp_apply_btn_ui", class = "ms-auto d-flex align-items-center gap-2")
    ),

    div(class = "mb-2",
      tags$label(class = "form-label small fw-semibold mb-1",
                 "Socio comercial"),
      selectizeInput("cmp_partner_sel", NULL, choices = character(0),
        width = "100%",
        options = list(
          placeholder = "Buscar proveedor / cliente...",
          maxOptions  = 300
        )
      )
    ),

    # Configured partners summary (visible when no partner is selected)
    div(id = "cmp_list_section",
      uiOutput("cmp_assigned_list_ui")
    ),

    shinyjs::hidden(
      div(id = "cmp_editor_section",
        tags$hr(class = "my-2"),
        div(class = "d-flex align-items-center mb-2",
          div(
            tags$span(class = "small fw-semibold", "Pila de políticas"),
            uiOutput("cmp_editor_title_ui", inline = TRUE)
          ),
          div(class = "ms-auto",
            actionButton("cmp_btn_clear_partner",
              tagList(icon("xmark"), " Limpiar"),
              class = "btn btn-sm btn-outline-secondary py-0 px-2")
          )
        ),

        uiOutput("cmp_stack_ui"),

        div(class = "d-flex align-items-center gap-2 mt-2 mb-3",
          div(class = "flex-grow-1",
            selectizeInput("cmp_add_sel", NULL, choices = character(0),
              width = "100%",
              options = list(placeholder = "Buscar política...", maxOptions = 100))
          ),
          actionButton("cmp_btn_add",
            tagList(icon("plus"), " Agregar"),
            class = "btn btn-sm btn-outline-primary")
        ),

        tags$hr(class = "my-2"),
        div(class = "row g-2 mb-3",
          div(class = "col-md-6",
            tags$label(class = "form-label small fw-semibold mb-1",
                       "Aplicar a cuentas por:"),
            radioButtons("cmp_ledger", NULL, inline = TRUE,
              choices = c("Pagar" = "AP", "Cobrar" = "AR", "Ambas" = ""),
              selected = "AP"
            )
          ),
          div(class = "col-md-6",
            tags$label(class = "form-label small fw-semibold mb-1 d-block",
                       "¿Es intercompañía?"),
            uiOutput("cmp_interco_toggle_ui")
          )
        ),

        actionButton("cmp_btn_save",
          tagList(icon("floppy-disk"), " Guardar cambios"),
          class = "btn btn-outline-secondary btn-sm")
      )
    )
  )
}

.cmp_stack_row <- function(idx, pol_name, pol_type, total) {
  div(class = "d-flex align-items-center gap-2 py-1 border-bottom",
    tags$span(class = "text-muted small", sprintf("%d.", idx)),
    .policy_type_badge(pol_type),
    tags$span(class = "flex-grow-1 small", pol_name),
    tags$button(
      class    = "btn btn-sm btn-outline-secondary py-0 px-1",
      disabled = if (idx == 1L) NA else NULL,
      onclick  = sprintf(
        "Shiny.setInputValue('cmp_action',{action:'up',idx:%d,nonce:Math.random()},{priority:'event'})",
        idx),
      icon("chevron-up")
    ),
    tags$button(
      class    = "btn btn-sm btn-outline-secondary py-0 px-1",
      disabled = if (idx == total) NA else NULL,
      onclick  = sprintf(
        "Shiny.setInputValue('cmp_action',{action:'down',idx:%d,nonce:Math.random()},{priority:'event'})",
        idx),
      icon("chevron-down")
    ),
    tags$button(
      class   = "btn btn-sm btn-outline-danger py-0 px-1",
      onclick = sprintf(
        "Shiny.setInputValue('cmp_action',{action:'remove',idx:%d,nonce:Math.random()},{priority:'event'})",
        idx),
      icon("xmark")
    )
  )
}

.cmp_apply_modal <- function(n_interco_partes) {
  modalDialog(
    title = tagList(icon("wand-magic-sparkles"), " Aplicar políticas de pago"),
    tagList(
      uiOutput("cmp_apply_count_ui"),
      if (n_interco_partes > 0L)
        div(class = "alert alert-warning small py-2 mb-2",
          icon("triangle-exclamation"), " ",
          strong(n_interco_partes),
          if (n_interco_partes == 1L) " socio intercompañía será omitido."
          else                        " socios intercompañía serán omitidos."
        )
      else NULL,
      tags$hr(class = "my-2"),
      tags$p(class = "fw-semibold small mb-2", "Alcance:"),
      radioButtons("cmp_apply_scope", NULL,
        choices = c(
          "Solo nuevas — documentos que aún no tienen fecha de política" = "new",
          "Sin fecha previamente modificada"                             = "skip_manual",
          "Todos sin excepción"                                          = "all"
        ),
        selected = "new"
      ),
      tags$p(class = "small text-muted mb-0",
             "Los documentos sin socio asignado no se ven afectados.")
    ),
    footer = tagList(
      actionButton("cmp_apply_cancel", "Cancelar",
                   class = "btn btn-secondary btn-sm"),
      actionButton("cmp_apply_confirm",
                   tagList(icon("check"), " Confirmar"),
                   class = "btn btn-success btn-sm")
    ),
    size = "m", easyClose = FALSE
  )
}

# =============================================================================
# settings_companies_observer
# =============================================================================
settings_companies_observer <- function(input, output, session, shared) {

  cmp_stack          <- reactiveVal(character())
  cmp_active_parte   <- reactiveVal(NULL)
  policies_dirty     <- reactiveVal(FALSE)
  apply_ctx          <- reactiveVal(NULL)   # preflight context for dynamic counter
  cmp_interco_state  <- reactiveVal(FALSE)  # TRUE = Es intercompañía
  cmp_has_changes    <- reactiveVal(FALSE)  # TRUE = unsaved edits in editor
  cmp_loading        <- reactiveVal(FALSE)  # TRUE while programmatic ledger update in flight

  # Drive "Guardar cambios" button colour: gray = no changes, blue = dirty
  observe({
    if (isTRUE(cmp_has_changes())) {
      shinyjs::removeClass("cmp_btn_save", "btn-outline-secondary")
      shinyjs::addClass("cmp_btn_save", "btn-primary")
    } else {
      shinyjs::removeClass("cmp_btn_save", "btn-primary")
      shinyjs::addClass("cmp_btn_save", "btn-outline-secondary")
    }
  })

  # compute_policy_moves() now self-normalises Parte / FechaVenc_Original when
  # they are missing (raw SAP data), so no heavy pre-processing is needed here.
  .prep_sap <- function(raw_list) raw_list

  # Revert button — only shown when policy moves exist
  output$cmp_revert_btn_ui <- renderUI({
    pm <- tryCatch(shared$policy_moves_db(), error = function(e) NULL)
    if (!is.null(pm) && nrow(pm) > 0L)
      actionButton("cmp_btn_revert",
                   tagList(icon("rotate-left"), " Revertir"),
                   class = "btn btn-sm btn-outline-warning")
    else NULL
  })

  # Tab switch — re-render panel on Reglas <-> Socios toggle
  observeEvent(input$pol_active_tab, {
    tab <- input$pol_active_tab %||% "reglas"
    output$settings_panel <- renderUI({ .policies_merged_panel_ui(tab) })
    if (identical(tab, "socios")) {
      .refresh_partner_choices()
      .refresh_add_sel()
    }
  }, ignoreInit = TRUE)

  # Revert: open modal
  observeEvent(input$cmp_btn_revert, {
    pm <- tryCatch(shared$policy_moves_db(), error = function(e) NULL)
    if (is.null(pm) || !nrow(pm)) return()
    showModal(.cmp_revert_modal(unique(pm$Parte)))
  }, ignoreInit = TRUE)

  # Revert: confirm
  observeEvent(input$cmp_revert_confirm, {
    sel <- input$cmp_revert_partes_sel
    if (!length(sel)) { removeModal(); return() }
    pm <- tryCatch(shared$policy_moves_db(), error = function(e) NULL)
    if (is.null(pm)) pm <- .schema_policy_moves()[0L, ]
    sel_lc <- tolower(trimws(sel))
    pm_new <- pm[!tolower(trimws(pm$Parte)) %in% sel_lc, , drop = FALSE]
    tryCatch({
      save_policy_moves(pm_new, client_id = shared$effective_client_id())
      shared$policy_moves_db(pm_new)
      removeModal()
      show_settings_modal(input, output, session, shared)
      output$settings_panel <- renderUI({ .policies_merged_panel_ui("socios") })
      .refresh_partner_choices()
      .refresh_add_sel()
      showNotification(
        sprintf("Fechas de política revertidas para %d socio%s.",
                length(sel), if (length(sel) == 1L) "" else "s"),
        type = "message", duration = 4)
    }, error = function(e) {
      showNotification(paste("Error al revertir:", conditionMessage(e)),
                       type = "error", duration = 5)
    })
  }, ignoreInit = TRUE)

  # Revert: cancel
  observeEvent(input$cmp_revert_cancel2, {
    removeModal()
    show_settings_modal(input, output, session, shared)
    output$settings_panel <- renderUI({ .policies_merged_panel_ui("socios") })
    .refresh_partner_choices()
    .refresh_add_sel()
  }, ignoreInit = TRUE)

  # Mark dirty whenever partner policies are saved from any observer
  observeEvent(shared$partner_policies_db(), {
    policies_dirty(TRUE)
  }, ignoreInit = TRUE)

  # Interco toggle — flip state on each click
  observeEvent(input$cmp_interco_click, {
    cmp_interco_state(!cmp_interco_state())
    cmp_has_changes(TRUE)
  }, ignoreInit = TRUE)

  output$cmp_interco_toggle_ui <- renderUI({
    is_ic <- cmp_interco_state()
    tags$button(
      class   = paste("btn btn-sm",
                      if (is_ic) "btn-warning" else "btn-outline-secondary"),
      onclick = paste0("Shiny.setInputValue('cmp_interco_click',",
                       "{nonce:Math.random()},{priority:'event'})"),
      if (is_ic)
        tagList(icon("check"), " Sí")
      else
        tagList(icon("xmark"), " No")
    )
  })

  output$cmp_apply_btn_ui <- renderUI({
    dirty <- policies_dirty()
    tagList(
      if (dirty)
        tags$span(class = "small text-warning fst-italic me-1",
                  icon("circle-exclamation", class = "me-1"), "Cambios sin aplicar")
      else NULL,
      actionButton("cmp_btn_apply",
        tagList(icon("wand-magic-sparkles"), " Aplicar políticas"),
        class = if (dirty) "btn btn-sm btn-success" else "btn btn-sm btn-outline-secondary"
      )
    )
  })

  # Dynamic counter inside the apply modal — re-renders when scope radio changes
  output$cmp_apply_count_ui <- renderUI({
    scope <- input$cmp_apply_scope %||% "all"
    ctx   <- apply_ctx()
    if (is.null(ctx)) return(NULL)
    n <- ctx$count_for_scope(scope)
    tags$p(
      "Se calcularán fechas ajustadas para",
      strong(format(n, big.mark = ",")),
      if (n == 1L) "documento." else "documentos."
    )
  })

  .get_catalog_cmp <- function() {
    db <- tryCatch(shared$policy_catalog_db(), error = function(e) NULL)
    if (!is.null(db) && nrow(db) > 0) db
    else tryCatch(load_policy_catalog(), error = function(e) .schema_policy_catalog()[0L, ])
  }

  .get_pp_cmp <- function() {
    db <- tryCatch(shared$partner_policies_db(), error = function(e) NULL)
    if (!is.null(db)) db
    else tryCatch(load_partner_policies(), error = function(e) NULL)
  }

  .all_partes <- function() {
    from_sap <- tryCatch({
      d <- shared$sap_data()
      c(
        if (!is.null(d$AP) && "Parte" %in% names(d$AP)) unique(d$AP$Parte) else character(),
        if (!is.null(d$AR) && "Parte" %in% names(d$AR)) unique(d$AR$Parte) else character()
      )
    }, error = function(e) character())

    from_pp <- tryCatch({
      pp <- .get_pp_cmp()
      if (!is.null(pp) && nrow(pp)) pp$parte else character()
    }, error = function(e) character())

    from_pasivos <- tryCatch({
      liabs <- shared$pasivos_liabilities_db()
      if (!is.null(liabs) && nrow(liabs) && "parte" %in% names(liabs))
        unique(liabs$parte[liabs$estado %in% c("active", "paused") & nzchar(liabs$parte %||% "")])
      else character()
    }, error = function(e) character())

    all_p <- c(from_sap, from_pp, from_pasivos)
    sort(unique(all_p[nzchar(trimws(all_p))]))
  }

  # ── Refresh partner choices when the panel becomes visible ───────────────
  # The selectize doesn't exist until renderUI fires; delay 200ms so it's ready.
  # Never pass `selected` — we must not reset any partner the user picked between
  # the button click and the delayed callback firing.
  .refresh_partner_choices <- function() {
    partes <- .all_partes()
    shinyjs::delay(200, {
      updateSelectizeInput(session, "cmp_partner_sel",
        choices = setNames(partes, partes)
      )
    })
  }

  # Direct panel open
  observeEvent(input$stg_btn_companies, {
    .refresh_partner_choices()
  }, ignoreInit = TRUE)

  # IC-discard → companies path (safe no-op if the panel shown is not companies)
  observeEvent(input$ic_discard_confirm, {
    partes <- .all_partes()
    shinyjs::delay(250, {
      updateSelectizeInput(session, "cmp_partner_sel",
        choices = setNames(partes, partes)
      )
    })
  }, ignoreInit = TRUE)

  # ── Populate "add policy" selectInput from catalog ────────────────────────
  .refresh_add_sel <- function() {
    catalog <- .get_catalog_cmp()
    choices <- if (!is.null(catalog) && nrow(catalog)) {
      setNames(catalog$id, catalog$name)
    } else {
      c("(Sin políticas en catálogo)" = "")
    }
    shinyjs::delay(150, {
      updateSelectizeInput(session, "cmp_add_sel", choices = choices)
    })
  }
  observeEvent(input$stg_btn_companies, { .refresh_add_sel() }, ignoreInit = TRUE)
  observeEvent(shared$policy_catalog_db(), { .refresh_add_sel() }, ignoreInit = TRUE)

  # Ledger radio change driven by user (not programmatic updateRadioButtons)
  observeEvent(input$cmp_ledger, {
    if (!isTRUE(cmp_loading())) cmp_has_changes(TRUE)
  }, ignoreInit = TRUE)

  # ── Partner selection → load stack ────────────────────────────────────────
  observeEvent(input$cmp_partner_sel, {
    parte <- input$cmp_partner_sel
    if (is.null(parte) || !nzchar(trimws(parte))) {
      cmp_active_parte(NULL)
      cmp_stack(character())
      cmp_has_changes(FALSE)
      shinyjs::hide("cmp_editor_section")
      return()
    }

    cmp_loading(TRUE)
    cmp_active_parte(parte)

    pp <- .get_pp_cmp()
    if (!is.null(pp) && nrow(pp)) {
      rows <- pp[tolower(trimws(pp$parte)) == tolower(trimws(parte)), , drop = FALSE]
      rows <- rows[order(rows$policy_order), , drop = FALSE]
      cmp_stack(rows$policy_id)
      if (nrow(rows)) {
        led <- rows$ledger[[1]] %||% "AP"
        updateRadioButtons(session, "cmp_ledger",
          selected = if (is.na(led) || !nzchar(led)) "" else led)
        cmp_interco_state(isTRUE(rows$is_interco[[1]]))
      } else {
        updateRadioButtons(session, "cmp_ledger", selected = "AP")
        cmp_interco_state(FALSE)
      }
    } else {
      cmp_stack(character())
      updateRadioButtons(session, "cmp_ledger", selected = "AP")
      cmp_interco_state(FALSE)
    }

    cmp_has_changes(FALSE)
    shinyjs::delay(100, cmp_loading(FALSE))

    shinyjs::hide("cmp_list_section")
    shinyjs::show("cmp_editor_section")
    .refresh_add_sel()
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # ── Configured partners summary list ─────────────────────────────────────
  output$cmp_assigned_list_ui <- renderUI({
    pp      <- .get_pp_cmp()
    catalog <- .get_catalog_cmp()
    if (is.null(pp) || !nrow(pp)) {
      return(div(class = "text-muted small py-3 text-center fst-italic",
        icon("circle-info", class = "me-1"),
        "Busca un socio para asignarle políticas."
      ))
    }

    id_to_type <- if (!is.null(catalog) && nrow(catalog))
      setNames(as.list(catalog$type), catalog$id) else list()

    # Build one row per unique parte
    partes_conf <- sort(unique(pp$parte))
    rows <- lapply(partes_conf, function(p) {
      p_rows <- pp[pp$parte == p, , drop = FALSE]
      p_rows <- p_rows[order(p_rows$policy_order), , drop = FALSE]
      n_pol  <- nrow(p_rows)
      is_ic  <- isTRUE(p_rows$is_interco[[1]])
      pol_badges <- tagList(lapply(seq_len(min(n_pol, 3L)), function(i) {
        pid <- p_rows$policy_id[[i]]
        .policy_type_badge(id_to_type[[pid]] %||% "?")
      }))
      p_esc <- gsub("'", "\\'", p, fixed = TRUE)
      div(class = "d-flex align-items-center gap-2 py-1 border-bottom",
        pol_badges,
        tags$span(class = "flex-grow-1 small", p),
        if (is_ic) tags$span(class = "badge bg-secondary rounded-pill",
                             style = "font-size:0.65em;", "IC") else NULL,
        tags$button(
          class   = "btn btn-sm btn-outline-secondary py-0 px-2",
          title   = "Editar",
          onclick = sprintf(
            "Shiny.setInputValue('cmp_partner_sel','%s',{priority:'event'})", p_esc),
          icon("pen-to-square")
        )
      )
    })

    tagList(
      tags$p(class = "small text-muted mb-2",
             sprintf("%d socio%s configurado%s",
                     length(partes_conf),
                     if (length(partes_conf) != 1L) "s" else "",
                     if (length(partes_conf) != 1L) "s" else "")),
      div(tagList(rows))
    )
  })

  # ── Editor section title (shows active partner name) ─────────────────────
  output$cmp_editor_title_ui <- renderUI({
    parte <- cmp_active_parte()
    if (is.null(parte)) return(NULL)
    tags$span(class = "small text-muted ms-2",
              paste0("— ", parte))
  })

  # ── Render stack ──────────────────────────────────────────────────────────
  output$cmp_stack_ui <- renderUI({
    stack   <- cmp_stack()
    catalog <- .get_catalog_cmp()

    if (!length(stack)) {
      return(div(class = "text-muted small py-2 fst-italic",
                 "Sin políticas asignadas."))
    }

    id_to_type <- if (!is.null(catalog) && nrow(catalog))
      setNames(as.list(catalog$type), catalog$id) else list()
    id_to_name <- if (!is.null(catalog) && nrow(catalog))
      setNames(as.list(catalog$name), catalog$id) else list()

    rows <- lapply(seq_along(stack), function(i) {
      pid  <- stack[[i]]
      .cmp_stack_row(i,
        pol_name = id_to_name[[pid]] %||% pid,
        pol_type = id_to_type[[pid]] %||% "?",
        total    = length(stack)
      )
    })
    div(tagList(rows))
  })

  # ── Stack manipulation ────────────────────────────────────────────────────
  observeEvent(input$cmp_action, {
    act <- input$cmp_action
    req(!is.null(act$action), !is.null(act$idx))
    idx   <- as.integer(act$idx)
    stack <- cmp_stack()

    if      (act$action == "up"     && idx > 1L)             stack[c(idx - 1L, idx)] <- stack[c(idx, idx - 1L)]
    else if (act$action == "down"   && idx < length(stack))  stack[c(idx, idx + 1L)] <- stack[c(idx + 1L, idx)]
    else if (act$action == "remove" && idx >= 1L && idx <= length(stack)) stack <- stack[-idx]

    cmp_stack(stack)
    cmp_has_changes(TRUE)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # ── Add policy ────────────────────────────────────────────────────────────
  observeEvent(input$cmp_btn_add, {
    pid <- input$cmp_add_sel
    req(!is.null(pid), nzchar(pid))
    cmp_stack(c(cmp_stack(), pid))
    cmp_has_changes(TRUE)
  }, ignoreInit = TRUE)

  # ── Clear partner selection ───────────────────────────────────────────────
  observeEvent(input$cmp_btn_clear_partner, {
    cmp_active_parte(NULL)
    cmp_stack(character())
    cmp_has_changes(FALSE)
    updateSelectizeInput(session, "cmp_partner_sel",
      choices  = setNames(.all_partes(), .all_partes()),
      selected = character(0))
    shinyjs::hide("cmp_editor_section")
    shinyjs::show("cmp_list_section")
  }, ignoreInit = TRUE)

  # ── Save partner policies ─────────────────────────────────────────────────
  observeEvent(input$cmp_btn_save, {
    parte <- isolate(cmp_active_parte())
    if (is.null(parte) || !nzchar(parte)) {
      showNotification("No hay socio seleccionado.", type = "warning")
      return()
    }

    stack      <- isolate(cmp_stack())
    ledger     <- input$cmp_ledger %||% "AP"
    is_interco <- isTRUE(cmp_interco_state())
    user       <- tryCatch(shared$current_user_info()$username, error = function(e) "")
    now        <- Sys.time()

    pp_old <- .get_pp_cmp()
    pp_new <- if (!is.null(pp_old) && nrow(pp_old)) {
      pp_old[tolower(trimws(pp_old$parte)) != tolower(trimws(parte)), , drop = FALSE]
    } else {
      .schema_partner_policies()[0L, ]
    }

    if (length(stack)) {
      new_rows <- tibble::tibble(
        parte        = rep(parte, length(stack)),
        policy_id    = stack,
        policy_order = seq_along(stack),
        ledger       = rep(ledger, length(stack)),
        is_interco   = rep(is_interco, length(stack)),
        is_manual    = TRUE,
        linked_by    = rep(user, length(stack)),
        linked_at    = rep(now, length(stack))
      )
      pp_new <- dplyr::bind_rows(pp_new, new_rows)
    }

    tryCatch({
      save_partner_policies(pp_new, client_id = shared$effective_client_id())
      shared$partner_policies_db(pp_new)

      # Clear stale policy moves for this partner — they'll recompute on next Apply.
      # Captured: user = user, now = now (for future audit log use).
      existing_moves <- tryCatch(shared$policy_moves_db(), error = function(e) NULL)
      if (!is.null(existing_moves) && nrow(existing_moves)) {
        kept_moves <- existing_moves[
          tolower(trimws(existing_moves$Parte)) != tolower(trimws(parte)), , drop = FALSE
        ]
        if (nrow(kept_moves) < nrow(existing_moves)) {
          tryCatch({
            save_policy_moves(kept_moves, client_id = shared$effective_client_id())
            shared$policy_moves_db(kept_moves)
          }, error = function(e) NULL)
        }
      }

      cmp_active_parte(NULL)
      cmp_stack(character())
      cmp_has_changes(FALSE)
      updateSelectizeInput(session, "cmp_partner_sel",
        choices  = setNames(.all_partes(), .all_partes()),
        selected = character(0))
      shinyjs::hide("cmp_editor_section")
      shinyjs::show("cmp_list_section")
      showNotification(
        sprintf('Políticas de "%s" guardadas.', parte),
        type = "message", duration = 3
      )
    }, error = function(e) {
      showNotification(paste("Error al guardar:", e$message), type = "error", duration = 5)
    })
  }, ignoreInit = TRUE)

  # ── Apply: preflight → show modal ─────────────────────────────────────────
  observeEvent(input$cmp_btn_apply, {
    pp      <- .get_pp_cmp()
    catalog <- .get_catalog_cmp()

    if (is.null(pp) || !nrow(pp)) {
      showNotification("No hay políticas asignadas a ningún socio.", type = "warning")
      return()
    }
    if (is.null(catalog) || !nrow(catalog)) {
      showNotification("El catálogo de políticas está vacío.", type = "warning")
      return()
    }

    sap     <- .prep_sap(tryCatch(shared$sap_data(), error = function(e) list()))
    all_inv <- dplyr::bind_rows(
      if (!is.null(sap$AP) && nrow(sap$AP)) sap$AP else NULL,
      if (!is.null(sap$AR) && nrow(sap$AR)) sap$AR else NULL
    )
    if (!"Parte" %in% names(all_inv)) all_inv$Parte <- NA_character_

    is_ic     <- !is.na(pp$is_interco) & pp$is_interco
    n_interco <- length(unique(pp$parte[is_ic]))
    non_ic_pp <- pp[!is_ic, , drop = FALSE]
    non_ic_lc <- tolower(trimws(non_ic_pp$parte))

    ex_moves  <- tryCatch(shared$policy_moves_db(), error = function(e) NULL)
    manual_db <- tryCatch(shared$moves_db(),          error = function(e) NULL)

    all_inv_lc   <- tolower(trimws(all_inv$Parte))
    has_doc_cols <- all(c("Empresa", "Moneda", "Documento") %in% names(all_inv))

    # Document keys already in policy_moves / manually overridden
    ex_doc_keys <- if (has_doc_cols && !is.null(ex_moves) && nrow(ex_moves))
      paste(ex_moves$Empresa, ex_moves$Moneda, ex_moves$Documento, sep = "·")
    else character()
    man_doc_keys <- if (has_doc_cols && !is.null(manual_db) && nrow(manual_db))
      paste(manual_db$Empresa, manual_db$Moneda, manual_db$Documento, sep = "·")
    else character()

    # Count invoices per scope at document level (falls back to partner-level when
    # Empresa/Moneda/Documento columns are not present in the raw data)
    count_for_scope <- function(scope) {
      base_mask <- all_inv_lc %in% non_ic_lc
      if (!has_doc_cols) return(sum(base_mask))
      inv_keys <- paste(all_inv$Empresa, all_inv$Moneda, all_inv$Documento, sep = "·")
      scope_mask <- switch(scope,
        "new"         = base_mask & !inv_keys %in% ex_doc_keys,
        "skip_manual" = base_mask & !inv_keys %in% man_doc_keys,
        base_mask
      )
      sum(scope_mask)
    }

    apply_ctx(list(
      count_for_scope = count_for_scope,
      n_interco       = n_interco
    ))

    showModal(.cmp_apply_modal(n_interco))
  }, ignoreInit = TRUE)

  # ── Apply: confirmed → compute and save ───────────────────────────────────
  observeEvent(input$cmp_apply_confirm, {
    scope   <- isolate(input$cmp_apply_scope) %||% "all"
    pp_all  <- .get_pp_cmp()
    catalog <- .get_catalog_cmp()
    sap     <- .prep_sap(tryCatch(shared$sap_data(), error = function(e) list()))
    ov      <- tryCatch(shared$holiday_overrides_db(), error = function(e) NULL)

    existing_moves <- tryCatch(shared$policy_moves_db(), error = function(e) NULL)
    if (is.null(existing_moves)) existing_moves <- .schema_policy_moves()[0L, ]

    # Step 1 — strip interco
    is_ic <- !is.na(pp_all$is_interco) & pp_all$is_interco
    pp    <- pp_all[!is_ic, , drop = FALSE]

    removeModal()
    show_settings_modal(input, output, session, shared)
    output$settings_panel <- renderUI({ .policies_merged_panel_ui("socios") })
    .refresh_partner_choices()
    .refresh_add_sel()

    if (!nrow(pp)) {
      showNotification("Solo hay socios intercompañía configurados.", type = "warning")
      return()
    }

    user <- tryCatch(shared$current_user_info()$username, error = function(e) "system")

    message(sprintf(
      "[POLICY_APPLY] scope=%s  pp_partes=%s  catalog_ids=%s  AP_rows=%s  AR_rows=%s",
      scope,
      paste(unique(pp$parte), collapse = ","),
      paste(if (!is.null(catalog)) catalog$id else "NULL", collapse = ","),
      if (!is.null(sap$AP)) nrow(sap$AP) else "NULL",
      if (!is.null(sap$AR)) nrow(sap$AR) else "NULL"
    ))
    if (!is.null(sap$AR) && nrow(sap$AR))
      message("[POLICY_APPLY] sap$AR cols: ", paste(names(sap$AR), collapse = ", "))
    if (!is.null(sap$AP) && nrow(sap$AP))
      message("[POLICY_APPLY] sap$AP cols: ", paste(names(sap$AP), collapse = ", "))

    new_moves <- dplyr::bind_rows(
      if (!is.null(sap$AP) && nrow(sap$AP))
        tryCatch(
          compute_policy_moves(sap$AP, pp, catalog, overrides_df = ov,
                               ledger = "AP", applied_by = user),
          error = function(e) {
            msg <- conditionMessage(e)
            message("[POLICY_ERROR] AP: ", msg)
            showNotification(paste("[Políticas AP]", msg),
                             type = "error", duration = 15)
            NULL
          }
        )
      else NULL,
      if (!is.null(sap$AR) && nrow(sap$AR))
        tryCatch(
          compute_policy_moves(sap$AR, pp, catalog, overrides_df = ov,
                               ledger = "AR", applied_by = user),
          error = function(e) {
            msg <- conditionMessage(e)
            message("[POLICY_ERROR] AR: ", msg)
            showNotification(paste("[Políticas AR]", msg),
                             type = "error", duration = 15)
            NULL
          }
        )
      else NULL
    )

    if (is.null(new_moves) || !nrow(new_moves)) {
      # Diagnostic: surface the actual Parte values on both sides so mismatch is visible
      sap_partes_lc <- tolower(trimws(unique(c(
        if (!is.null(sap$AP) && "Parte" %in% names(sap$AP)) sap$AP$Parte else character(),
        if (!is.null(sap$AR) && "Parte" %in% names(sap$AR)) sap$AR$Parte else character()
      ))))
      sap_partes_lc <- sap_partes_lc[nzchar(sap_partes_lc) & !is.na(sap_partes_lc)]
      pp_partes_lc  <- tolower(trimws(unique(pp$parte)))
      matched       <- intersect(sap_partes_lc, pp_partes_lc)

      if (length(sap_partes_lc) == 0L) {
        showNotification(
          paste0("Sin fechas: columna Parte no encontrada en datos SAP. ",
                 "Revisa si los datos han cargado (AP rows=",
                 if (!is.null(sap$AP)) nrow(sap$AP) else "NULL",
                 ", AR rows=",
                 if (!is.null(sap$AR)) nrow(sap$AR) else "NULL", ")."),
          type = "error", duration = 15
        )
      } else if (length(matched) == 0L) {
        sap_s <- paste0('"', paste(head(sort(sap_partes_lc), 3L), collapse='","'), '"')
        pp_s  <- paste0('"', paste(head(sort(pp_partes_lc),  3L), collapse='","'), '"')
        showNotification(
          paste0("Sin coincidencias de Socio: SAP tiene [", sap_s,
                 "] pero políticas tienen [", pp_s,
                 "]. Verifica el nombre del socio en Empresas y Socios."),
          type = "error", duration = 15
        )
      } else {
        # Check for broken policy UUIDs (assigned but not in catalog)
        catalog_ids <- if (!is.null(catalog)) catalog$id else character()
        broken_ids  <- unique(pp$policy_id[!pp$policy_id %in% catalog_ids])
        if (length(broken_ids)) {
          showNotification(
            paste0("Política no encontrada en catálogo (UUID inválido). ",
                   "Edita el socio, elimina la política marcada con '?' y reasígnala."),
            type = "error", duration = 15
          )
        } else {
          # Check if AR partners have no AR data
          ar_partes_lc <- tolower(trimws(unique(pp$parte[pp$ledger == "AR"])))
          if (length(ar_partes_lc) && (is.null(sap$AR) || !nrow(sap$AR))) {
            showNotification(
              paste0("Sin fechas: socios con ledger AR pero no hay datos AR cargados. ",
                     "Verifica que los datos de Cobrar estén disponibles."),
              type = "warning", duration = 10
            )
          } else {
            showNotification(
              "No se generaron fechas (sin documentos con socio asignado).",
              type = "warning"
            )
          }
        }
      }
      return()
    }

    computed_partes_lc <- tolower(trimws(unique(new_moves$Parte)))

    # ── Merge new_moves with existing per scope ────────────────────────────
    # "new"         → keep all existing; only add docs not yet computed
    # "skip_manual" → apply only to docs without a manual date override;
    #                 preserve existing policy moves for manually-dated docs
    # "all"         → replace for computed partners; keep for others;
    #                 also clear manual date overrides so policy date shows
    moves <- switch(scope,
      "new" = {
        if (nrow(existing_moves)) {
          exist_keys <- paste(existing_moves$ledger, existing_moves$Empresa,
                              existing_moves$Moneda, existing_moves$Documento, sep = "·")
          new_keys   <- paste(new_moves$ledger, new_moves$Empresa,
                              new_moves$Moneda, new_moves$Documento, sep = "·")
          dplyr::bind_rows(existing_moves,
                           new_moves[!new_keys %in% exist_keys, , drop = FALSE])
        } else {
          new_moves
        }
      },
      "skip_manual" = {
        man_db   <- tryCatch(shared$moves_db(), error = function(e) NULL)
        man_keys <- if (!is.null(man_db) && nrow(man_db))
          paste(man_db$ledger, man_db$Empresa, man_db$Moneda, man_db$Documento, sep = "·")
        else character()
        new_keys <- paste(new_moves$ledger, new_moves$Empresa,
                          new_moves$Moneda, new_moves$Documento, sep = "·")
        apply_these <- new_moves[!new_keys %in% man_keys, , drop = FALSE]

        if (nrow(existing_moves)) {
          exist_keys <- paste(existing_moves$ledger, existing_moves$Empresa,
                              existing_moves$Moneda, existing_moves$Documento, sep = "·")
          # Keep existing for: manually-dated docs OR uncomputed partners
          keep_ex <- existing_moves[
            exist_keys %in% man_keys |
              !tolower(trimws(existing_moves$Parte)) %in% computed_partes_lc, ,
            drop = FALSE
          ]
          dplyr::bind_rows(keep_ex, apply_these)
        } else {
          apply_these
        }
      },
      {
        # "all" — replace for computed partners, keep for others
        unaffected <- if (nrow(existing_moves))
          existing_moves[
            !tolower(trimws(existing_moves$Parte)) %in% computed_partes_lc, ,
            drop = FALSE
          ]
        else
          existing_moves
        dplyr::bind_rows(unaffected, new_moves)
      }
    )

    tryCatch({
      save_policy_moves(moves, client_id = shared$effective_client_id())
      shared$policy_moves_db(moves)

      # "Todos sin excepción": clear manual date overrides for computed docs so
      # FechaEff shows the policy date rather than the superseded override.
      if (scope == "all") {
        man_db <- tryCatch(shared$moves_db(), error = function(e) NULL)
        if (!is.null(man_db) && nrow(man_db)) {
          pol_keys <- paste(new_moves$Empresa, new_moves$Moneda,
                            new_moves$Documento, sep = "·")
          man_keys <- paste(man_db$Empresa, man_db$Moneda,
                            man_db$Documento, sep = "·")
          pruned <- man_db[!man_keys %in% pol_keys, , drop = FALSE]
          if (nrow(pruned) < nrow(man_db)) {
            save_moves(pruned, client_id = shared$effective_client_id())
            bump_sync_version("moves_db")
            shared$moves_db(pruned)
          }
        }
      }

      policies_dirty(FALSE)

      # Diagnostic: which partners had matching invoices vs which didn't
      expected_partes_lc <- tolower(trimws(unique(pp$parte)))
      unmatched          <- expected_partes_lc[!expected_partes_lc %in% computed_partes_lc]
      scope_lbl <- switch(scope, new = "nuevos", skip_manual = "sin modificar", "todos")
      msg <- sprintf("Políticas aplicadas (%s): %d documentos calculados (%d socios).",
                     scope_lbl, nrow(new_moves), length(computed_partes_lc))
      if (length(unmatched)) {
        display <- paste(sort(unmatched)[seq_len(min(3L, length(unmatched)))], collapse = ", ")
        if (length(unmatched) > 3L) display <- paste0(display, "…")
        msg <- paste0(msg, sprintf(" Sin documentos: %s", display))
      }
      showNotification(msg, type = "message", duration = 6)
    }, error = function(e) {
      showNotification(paste("Error al guardar:", e$message), type = "error", duration = 5)
    })
  }, ignoreInit = TRUE)

  observeEvent(input$cmp_apply_cancel, {
    removeModal()
    show_settings_modal(input, output, session, shared)
    output$settings_panel <- renderUI({ .policies_merged_panel_ui("socios") })
    .refresh_partner_choices()
    .refresh_add_sel()
  }, ignoreInit = TRUE)

  # ── Auto-apply "Solo nuevas" on every SAP data refresh ────────────────────
  # Whenever new SAP data arrives (live fetch, cache hit, or snapshot seed after
  # the initial flush), compute policy dates for any document that does not yet
  # have one.  Existing policy dates are NEVER overwritten by this path.
  observeEvent(shared$sap_data(), {
    sap     <- tryCatch(shared$sap_data(), error = function(e) list())
    pp_all  <- tryCatch(shared$partner_policies_db(), error = function(e) NULL)
    catalog <- tryCatch(shared$policy_catalog_db(),    error = function(e) NULL)
    ov      <- tryCatch(shared$holiday_overrides_db(), error = function(e) NULL)

    if (is.null(sap) || (is.null(sap$AP) && is.null(sap$AR))) return()
    if (is.null(pp_all) || !nrow(pp_all))                      return()
    if (is.null(catalog) || !nrow(catalog))                    return()

    is_ic <- !is.na(pp_all$is_interco) & pp_all$is_interco
    pp    <- pp_all[!is_ic, , drop = FALSE]
    if (!nrow(pp)) return()

    existing_moves <- tryCatch(shared$policy_moves_db(), error = function(e) NULL)
    if (is.null(existing_moves)) existing_moves <- .schema_policy_moves()[0L, ]

    new_moves <- dplyr::bind_rows(
      if (!is.null(sap$AP) && nrow(sap$AP))
        tryCatch(
          compute_policy_moves(sap$AP, pp, catalog, overrides_df = ov,
                               ledger = "AP", applied_by = "auto"),
          error = function(e) {
            message("[POLICY_AUTO_ERROR] AP: ", conditionMessage(e))
            NULL
          }
        )
      else NULL,
      if (!is.null(sap$AR) && nrow(sap$AR))
        tryCatch(
          compute_policy_moves(sap$AR, pp, catalog, overrides_df = ov,
                               ledger = "AR", applied_by = "auto"),
          error = function(e) {
            message("[POLICY_AUTO_ERROR] AR: ", conditionMessage(e))
            NULL
          }
        )
      else NULL
    )

    if (is.null(new_moves) || !nrow(new_moves)) return()

    # Keep all existing policy dates; add only documents not yet computed.
    # Key includes ledger so an AR and AP document with the same number are
    # treated as distinct and both receive their policy dates.
    if (nrow(existing_moves)) {
      exist_keys <- paste(existing_moves$ledger, existing_moves$Empresa,
                          existing_moves$Moneda, existing_moves$Documento, sep = "·")
      new_keys   <- paste(new_moves$ledger, new_moves$Empresa,
                          new_moves$Moneda, new_moves$Documento, sep = "·")
      truly_new <- new_moves[!new_keys %in% exist_keys, , drop = FALSE]
      if (!nrow(truly_new)) return()
      moves <- dplyr::bind_rows(existing_moves, truly_new)
    } else {
      moves <- new_moves
    }

    tryCatch({
      save_policy_moves(moves, client_id = shared$effective_client_id())
      shared$policy_moves_db(moves)
      policies_dirty(FALSE)
    }, error = function(e) NULL)
  }, ignoreInit = TRUE)

  # ── Retry auto-apply when partner policies first become available ─────────
  # Fixes startup race: the SAP dispatch (priority=-1) fires sap_data() before
  # Phase 2 (priority=-2) loads partner_policies_db, so the observer above
  # finds pp_all=NULL and returns early.  This observer catches the Phase-2
  # load and fills in policy dates for any document not yet computed.
  observeEvent(shared$partner_policies_db(), {
    sap     <- tryCatch(shared$sap_data(),            error = function(e) list())
    pp_all  <- tryCatch(shared$partner_policies_db(), error = function(e) NULL)
    catalog <- tryCatch(shared$policy_catalog_db(),   error = function(e) NULL)
    ov      <- tryCatch(shared$holiday_overrides_db(),error = function(e) NULL)

    if (is.null(sap) || (is.null(sap$AP) && is.null(sap$AR))) return()
    if (is.null(pp_all) || !nrow(pp_all))                      return()
    if (is.null(catalog) || !nrow(catalog))                    return()

    is_ic <- !is.na(pp_all$is_interco) & pp_all$is_interco
    pp    <- pp_all[!is_ic, , drop = FALSE]
    if (!nrow(pp)) return()

    existing_moves <- tryCatch(shared$policy_moves_db(), error = function(e) NULL)
    if (is.null(existing_moves)) existing_moves <- .schema_policy_moves()[0L, ]

    new_moves <- dplyr::bind_rows(
      if (!is.null(sap$AP) && nrow(sap$AP))
        tryCatch(
          compute_policy_moves(sap$AP, pp, catalog, overrides_df = ov,
                               ledger = "AP", applied_by = "auto"),
          error = function(e) {
            message("[POLICY_AUTO_ERROR] AP (pp_load): ", conditionMessage(e))
            NULL
          }
        )
      else NULL,
      if (!is.null(sap$AR) && nrow(sap$AR))
        tryCatch(
          compute_policy_moves(sap$AR, pp, catalog, overrides_df = ov,
                               ledger = "AR", applied_by = "auto"),
          error = function(e) {
            message("[POLICY_AUTO_ERROR] AR (pp_load): ", conditionMessage(e))
            NULL
          }
        )
      else NULL
    )

    if (is.null(new_moves) || !nrow(new_moves)) return()

    if (nrow(existing_moves)) {
      exist_keys <- paste(existing_moves$ledger, existing_moves$Empresa,
                          existing_moves$Moneda, existing_moves$Documento, sep = "·")
      new_keys   <- paste(new_moves$ledger, new_moves$Empresa,
                          new_moves$Moneda, new_moves$Documento, sep = "·")
      truly_new  <- new_moves[!new_keys %in% exist_keys, , drop = FALSE]
      if (!nrow(truly_new)) return()
      moves <- dplyr::bind_rows(existing_moves, truly_new)
    } else {
      moves <- new_moves
    }

    tryCatch({
      save_policy_moves(moves, client_id = shared$effective_client_id())
      shared$policy_moves_db(moves)
      policies_dirty(FALSE)
    }, error = function(e) NULL)
  }, ignoreInit = TRUE)
}
