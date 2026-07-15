# =============================================================================
# R/settings/settings_hub.R
# show_settings_modal() + settings_observers() — the orchestration hub.
# =============================================================================
# (Originally part of R/settings_module.R; split in Stage 7.1 for session efficiency.)

# =============================================================================
# show_settings_modal
# =============================================================================
show_settings_modal <- function(input, output, session, shared) {

  # Resolve tier before building the modal — must happen outside tagList/div
  tier <- tryCatch(shared$current_user_info()$tier, error = function(e) "")
  is_dev           <- identical(tier, "dev")
  is_admin_or_dev  <- tier %in% c("dev", "admin")

  sincro_btn <- if (is_admin_or_dev) {
    tagList(
      tags$hr(class = "my-1"),
      tags$p(class = "text-muted small px-2 mb-1 text-uppercase fw-semibold", "Admin"),
      actionButton("stg_btn_sincro",
        tagList(icon("rotate"), " Sincronización"),
        class = "btn btn-sm btn-outline-secondary text-start w-100")
    )
  } else NULL

  showModal(modalDialog(
    title     = tagList(icon("gear"), " Configuración"),
    size      = "xl",
    easyClose = FALSE,
    footer    = actionButton("stg_close_btn",
                             tagList(icon("xmark"), " Cerrar"),
                             class = "btn btn-secondary"),

    div(class = "settings-hub d-flex", style = "min-height: 460px;",

      # ── Sidebar ──────────────────────────────────────────────────────────
      div(class = "settings-sidebar d-flex flex-column gap-1 p-2 border-end bg-light flex-shrink-0",
        style = "width: 215px;",
        tags$p(class = "text-muted small px-2 pt-1 mb-1 text-uppercase fw-semibold",
               "Módulos"),
        actionButton("stg_btn_interco",
          tagList(icon("arrows-left-right"), " Intercompany"),
          class = "btn btn-sm btn-outline-secondary text-start w-100"),
        actionButton("stg_btn_catalogo",
          tagList(icon("building"), " Business Partners"),
          class = "btn btn-sm btn-outline-secondary text-start w-100"),
        actionButton("stg_btn_cuentas",
          tagList(icon("building-columns"), " Cuentas de Empresa"),
          class = "btn btn-sm btn-outline-secondary text-start w-100"),
        actionButton("stg_btn_policies",
          tagList(icon("calendar-check"), " Políticas de Pago"),
          class = "btn btn-sm btn-outline-secondary text-start w-100"),
        sincro_btn
      ),

      # ── Content ──────────────────────────────────────────────────────────
      div(class = "settings-content flex-grow-1 p-3 overflow-auto",
        uiOutput("settings_panel")
      )
    )
  ))

  output$settings_panel <- renderUI({ .settings_welcome_ui() })
}

.settings_welcome_ui <- function() {
  div(class = "text-center text-muted pt-5",
    icon("gear", style = "font-size:2.5rem; opacity:0.2;"),
    tags$p(class = "mt-3", "Selecciona una sección en el menú de la izquierda.")
  )
}

# =============================================================================
# settings_observers  (called once from app.R server)
# =============================================================================
settings_observers <- function(input, output, session, shared) {

  # ── Intercompany ─────────────────────────────────────────────────────────────

  # ic_trigger: increment to re-render all company/ledger code lists from saved state.
  # renderUI outputs depend ONLY on this — nothing else — so they never fire
  # mid-edit and overwrite what the user is typing.
  ic_trigger      <- reactiveVal(0L)
  # Number of visible input slots per company/ledger ("{initials}_ar" / "_ap")
  ic_slots        <- reactiveValues()
  # TRUE while the IC panel is currently displayed
  ic_panel_active <- reactiveVal(FALSE)
  # Stores the pending nav target when a dirty-guard modal is shown
  ic_pending_nav  <- reactiveVal(NULL)
  # Business Partners: cached scan of unique SAP codes from invoice snapshots
  bp_candidates        <- reactiveVal(NULL)
  # Sorted version of bp_candidates (matches DT display order for row-index alignment)
  bp_sorted_candidates <- reactiveVal(NULL)


  # Auto-grow: when the current last slot is non-empty, append a new empty one
  # via insertUI so existing inputs are never touched or re-rendered.
  observe({
    cmap <- shared$company_map()   # reactive dep — re-fires when companies change
    lapply(names(cmap), function(initials) {
      local({
        ini <- initials
        lapply(c("ar", "ap"), function(ledger) {
          local({
            l   <- ledger
            key <- paste0(ini, "_", l)
            n   <- ic_slots[[key]] %||% 1L
            val <- input[[paste0("ic_", l, "_", ini, "_", n)]] %||% ""
            if (nzchar(trimws(val))) {
              new_n <- n + 1L
              ic_slots[[key]] <- new_n
              insertUI(
                selector  = paste0("#ic_", l, "_container_", ini),
                where     = "beforeEnd",
                immediate = TRUE,
                ui = div(
                  id = paste0("ic_", l, "_slot_", ini, "_", new_n),
                  class = "mb-2",
                  textInput(
                    inputId = paste0("ic_", l, "_", ini, "_", new_n),
                    label   = NULL, value = "", placeholder = "", width = "100%"
                  ),
                  tags$small(
                    id    = paste0("ic_lbl_", l, "_", ini, "_", new_n),
                    class = "d-block fst-italic",
                    style = "margin-top:-10px; font-size:0.7em; min-height:0.85em; padding-left:3px; color:#aaa;",
                    ""
                  )
                )
              )
            }
          })
        })
      })
    })
  })

  # ── Unsaved-changes helpers ───────────────────────────────────────────────────

  # Snapshot of current UI state (codes stripped of any accidental prefix, sorted)
  .ic_current <- function() {
    cmap <- shared$company_map()
    list(
      ar_prefix = trimws(input$ic_ar_prefix %||% "C"),
      ap_prefix = trimws(input$ic_ap_prefix %||% "P"),
      companies = setNames(lapply(names(cmap), function(ini) {
        ar_n <- isolate(ic_slots[[paste0(ini, "_ar")]]) %||% 1L
        ap_n <- isolate(ic_slots[[paste0(ini, "_ap")]]) %||% 1L
        list(
          ar = sort(unique(Filter(nzchar, sub("^[A-Za-z]+", "", trimws(
            vapply(seq_len(ar_n), function(j)
              input[[paste0("ic_ar_", ini, "_", j)]] %||% "", character(1))))))),
          ap = sort(unique(Filter(nzchar, sub("^[A-Za-z]+", "", trimws(
            vapply(seq_len(ap_n), function(j)
              input[[paste0("ic_ap_", ini, "_", j)]] %||% "", character(1)))))))
        )
      }), names(cmap))
    )
  }

  .ic_saved <- function() {
    cmap <- shared$company_map()
    cur  <- shared$interco_v2()
    list(
      ar_prefix = cur$ar_prefix %||% "C",
      ap_prefix = cur$ap_prefix %||% "P",
      companies = setNames(lapply(names(cmap), function(ini) {
        co <- cur$companies[[ini]] %||% list(ar = character(), ap = character())
        list(ar = sort(co$ar %||% character()), ap = sort(co$ap %||% character()))
      }), names(cmap))
    )
  }

  .ic_has_changes <- function() ic_panel_active() && !identical(.ic_current(), .ic_saved())

  .ic_unsaved_modal <- function() {
    modalDialog(
      title     = tagList(icon("triangle-exclamation"), " Cambios sin guardar"),
      p("Tienes cambios sin guardar en la configuraci\u00f3n Intercompany."),
      p(strong("\u00bfDeseas salir sin guardar?")),
      footer    = tagList(
        actionButton("ic_discard_confirm", "Salir sin guardar",
                     class = "btn btn-warning btn-sm"),
        tags$span("\u00a0"),
        actionButton("ic_discard_cancel",  "Seguir editando",
                     class = "btn btn-primary btn-sm")
      ),
      easyClose = FALSE
    )
  }

  # Execute a deferred nav after the user confirms discarding changes.
  # Note: showModal(.ic_unsaved_modal()) replaced the settings modal, so after
  # removeModal() the settings container is gone.  For non-close actions we
  # must re-open it before rendering the target panel.
  observeEvent(input$ic_discard_confirm, {
    removeModal()          # close the warning dialog
    action <- ic_pending_nav()
    ic_pending_nav(NULL)
    ic_panel_active(FALSE)

    if (action == "close") {
      # Settings modal already gone — nothing more to do
    } else if (action == "catalogo") {
      show_settings_modal(input, output, session, shared)
      output$settings_panel <- renderUI({
        .bp_panel_ui(shared, bp_candidates())
      })
    } else if (action == "cuentas") {
      show_settings_modal(input, output, session, shared)
      output$settings_panel <- renderUI({ .cuentas_panel_ui(cmap = shared$company_map()) })
    } else if (action == "policies") {
      show_settings_modal(input, output, session, shared)
      output$settings_panel <- renderUI({ .policies_merged_panel_ui("reglas") })
    } else if (action == "companies") {
      show_settings_modal(input, output, session, shared)
      output$settings_panel <- renderUI({ .policies_merged_panel_ui("socios") })
    }
  })

  observeEvent(input$ic_discard_cancel, {
    removeModal()
    ic_pending_nav(NULL)
  })

  # Settings modal close button (replaces modalButton so we can guard it)
  observeEvent(input$stg_close_btn, {
    if (.ic_has_changes()) {
      ic_pending_nav("close")
      showModal(.ic_unsaved_modal())
    } else {
      ic_panel_active(FALSE)
      removeModal()
    }
  })

  # ── Open Intercompany panel ───────────────────────────────────────────────────
  # Helper: initialize ic_slots from registry and render the IC settings panel.
  # Call whenever navigating to IC panel (button click or post-scanner-apply).
  .show_ic_panel <- function() {
    cmap <- shared$company_map()
    cur  <- shared$interco_v2()
    ic_panel_active(TRUE)
    for (ini in names(cmap)) {
      co <- cur$companies[[ini]] %||% list(ar = character(), ap = character())
      ic_slots[[paste0(ini, "_ar")]] <- max(1L, length(co$ar %||% character()) + 1L)
      ic_slots[[paste0(ini, "_ap")]] <- max(1L, length(co$ap %||% character()) + 1L)
    }
    # Build + send code→name lookup so labels render correctly
    lkp <- tryCatch(.build_code_lookup(), error = function(e) list())
    ic_code_lookup(lkp)
    session$sendCustomMessage("icLookupData", jsonlite::toJSON(lkp, auto_unbox = TRUE))
    ic_trigger(ic_trigger() + 1L)

    output$settings_panel <- renderUI({
      ic_trigger()   # reactive dependency: re-render whenever codes are applied/loaded
      cmap <- shared$company_map()   # reactive dep — re-renders when companies change
      cur2 <- isolate(shared$interco_v2())
      lkp  <- isolate(ic_code_lookup())

      .slots_ui <- function(ledger, ini) {
        n        <- isolate(ic_slots[[paste0(ini, "_", ledger)]]) %||% 1L
        existing <- (cur2$companies[[ini]] %||%
                       list(ar = character(), ap = character()))[[ledger]] %||% character()
        ph <- if (ledger == "ar") "Ej: 1027" else "Ej: 1426"
        div(
          id = paste0("ic_", ledger, "_container_", ini),
          tagList(lapply(seq_len(n), function(j) {
            val      <- if (j <= length(existing)) existing[[j]] else ""
            lbl_text <- if (nzchar(val) && !is.null(lkp[[ledger]][[ini]])) {
              lkp[[ledger]][[ini]][[paste0("C", val)]] %||%
              lkp[[ledger]][[ini]][[paste0("P", val)]] %||%
              lkp[[ledger]][[ini]][[val]]              %||% ""
            } else ""
            div(id = paste0("ic_", ledger, "_slot_", ini, "_", j), class = "mb-2",
              textInput(
                inputId     = paste0("ic_", ledger, "_", ini, "_", j),
                label       = NULL,
                value       = val,
                placeholder = if (j == 1) ph else "",
                width       = "100%"
              ),
              tags$small(
                id    = paste0("ic_lbl_", ledger, "_", ini, "_", j),
                class = "d-block fst-italic",
                style = paste0("margin-top:-10px; font-size:0.7em; min-height:0.85em;",
                               " padding-left:3px;",
                               if (nzchar(lbl_text)) " color:#0d6efd;" else " color:#aaa;"),
                lbl_text
              )
            )
          }))
        )
      }

      panels <- lapply(names(cmap), function(initials) {
        ini <- initials
        bslib::accordion_panel(
          title = paste0(cmap[[initials]], " (", initials, ")"),
          value = initials,
          fluidRow(
            column(6,
              tags$label("Clientes IC \u2014 CxC", class = "fw-semibold small mb-1 d-block"),
              tags$p(class = "text-muted small mb-2",
                     "Un c\u00f3digo por caja. Al escribir aparece otra autom\u00e1ticamente."),
              .slots_ui("ar", ini)
            ),
            column(6,
              tags$label("Proveedores IC \u2014 CxP", class = "fw-semibold small mb-1 d-block"),
              tags$p(class = "text-muted small mb-2",
                     "Un c\u00f3digo por caja. Al escribir aparece otra autom\u00e1ticamente."),
              .slots_ui("ap", ini)
            )
          )
        )
      })

      # ── Registry summary card ──────────────────────────────────────────────
      ar_pre  <- cur2$ar_prefix %||% "C"
      ap_pre  <- cur2$ap_prefix %||% "P"
      reg_rows <- Filter(Negate(is.null), lapply(names(cmap), function(ini) {
        co    <- cur2$companies[[ini]] %||% list(ar = character(), ap = character())
        ar_cs <- co$ar %||% character()
        ap_cs <- co$ap %||% character()
        if (!length(ar_cs) && !length(ap_cs)) return(NULL)
        tags$tr(
          tags$td(tags$code(ini)),
          tags$td(class = "small font-monospace",
                  if (length(ar_cs)) paste(paste0(ar_pre, ar_cs), collapse = ", ")
                  else tags$span(class = "text-muted", "\u2014")),
          tags$td(class = "small font-monospace",
                  if (length(ap_cs)) paste(paste0(ap_pre, ap_cs), collapse = ", ")
                  else tags$span(class = "text-muted", "\u2014"))
        )
      }))
      total_ar <- sum(vapply(cur2$companies %||% list(),
                             function(co) length(co$ar %||% character()), integer(1)))
      total_ap <- sum(vapply(cur2$companies %||% list(),
                             function(co) length(co$ap %||% character()), integer(1)))

      summary_card <- if (length(reg_rows)) {
        div(class = "border rounded p-3 mb-3 border-success bg-success-subtle",
          tags$p(class = "fw-semibold small mb-2 text-success-emphasis",
                 tagList(icon("circle-check"),
                         sprintf(" Registro guardado \u2014 %d c\u00f3digo(s) CxC \u00b7 %d c\u00f3digo(s) CxP",
                                 total_ar, total_ap))),
          tags$div(class = "overflow-auto", style = "max-height:140px;",
            tags$table(class = "table table-sm table-borderless mb-0",
              tags$thead(tags$tr(
                tags$th(class = "small", "Empresa"),
                tags$th(class = "small", "Clientes IC (CxC)"),
                tags$th(class = "small", "Proveedores IC (CxP)")
              )),
              tags$tbody(reg_rows)
            )
          )
        )
      } else {
        div(class = "border rounded p-3 mb-3 text-muted small",
            tagList(icon("circle-info"), " Sin c\u00f3digos IC guardados todav\u00eda."))
      }

      div(
        tags$h6(class = "fw-semibold mb-3",
                tagList(icon("arrows-left-right"), " Configurar Intercompany")),
        summary_card,
        div(
          class = "border rounded p-3 mb-3 bg-light",
          tags$p(class = "fw-semibold small mb-2", "Prefijos de c\u00f3digos SAP",
                 tags$span(class = "text-muted fw-normal ms-2 fst-italic",
                           "(modificar solo si tu SAP usa otra convenci\u00f3n)")),
          fluidRow(
            column(3, textInput("ic_ar_prefix", "Prefijo CxC",
                                value = cur2$ar_prefix %||% "C", width = "70px")),
            column(3, textInput("ic_ap_prefix", "Prefijo CxP",
                                value = cur2$ap_prefix %||% "P", width = "70px"))
          )
        ),
        do.call(bslib::accordion,
          c(list(id = "ic_empresa_accordion", open = FALSE), panels)
        ),
        div(class = "mt-3 d-flex align-items-center gap-2 flex-wrap",
          actionButton("save_interco_v2", tagList(icon("floppy-disk"), " Guardar"),
                       class = "btn btn-primary btn-sm"),
          if (isTRUE(isolate(shared$current_user_info())$tier == "dev"))
            tagList(
              tags$span(class = "text-muted", "|"),
              actionButton("scan_ic_btn",
                           tagList(icon("magnifying-glass"), " Esc\u00e1ner SAP"),
                           class = "btn btn-outline-secondary btn-sm"),
              tags$span(class = "text-muted small",
                        "Detecta c\u00f3digos IC desde las facturas cargadas en memoria.")
            )
        )
      )
    })
  }

  observeEvent(input$stg_btn_interco, {
    .show_ic_panel()
  }, ignoreInit = TRUE)

  # ── Save ─────────────────────────────────────────────────────────────────────
  observeEvent(input$save_interco_v2, {
    cmap <- isolate(shared$company_map())

    parse_code <- function(val) {
      v        <- trimws(val %||% "")
      stripped <- sub("^[A-Za-z]+", "", v)
      if (nzchar(stripped)) stripped else character(0)
    }

    had_prefix <- any(vapply(names(cmap), function(initials) {
      ar_n <- ic_slots[[paste0(initials, "_ar")]] %||% 1L
      ap_n <- ic_slots[[paste0(initials, "_ap")]] %||% 1L
      any(grepl("^[A-Za-z]", c(
        vapply(seq_len(ar_n), function(j)
          input[[paste0("ic_ar_", initials, "_", j)]] %||% "", character(1)),
        vapply(seq_len(ap_n), function(j)
          input[[paste0("ic_ap_", initials, "_", j)]] %||% "", character(1))
      )))
    }, logical(1)))

    ar_prefix <- trimws(input$ic_ar_prefix %||% "C")
    ap_prefix <- trimws(input$ic_ap_prefix %||% "P")

    companies <- setNames(
      lapply(names(cmap), function(initials) {
        ar_n <- ic_slots[[paste0(initials, "_ar")]] %||% 1L
        ap_n <- ic_slots[[paste0(initials, "_ap")]] %||% 1L
        list(
          ar = unique(unlist(lapply(seq_len(ar_n), function(j)
            parse_code(input[[paste0("ic_ar_", initials, "_", j)]])))),
          ap = unique(unlist(lapply(seq_len(ap_n), function(j)
            parse_code(input[[paste0("ic_ap_", initials, "_", j)]]))))
        )
      }),
      names(cmap)
    )

    registry <- list(
      ar_prefix = if (nzchar(ar_prefix)) ar_prefix else "C",
      ap_prefix = if (nzchar(ap_prefix)) ap_prefix else "P",
      rfcs      = isolate(shared$interco_v2())$rfcs %||% character(),
      companies = companies
    )

    saved_ok <- tryCatch({
      save_interco_v2(registry, client_id = shared$active_client_id())
      TRUE
    }, error = function(e) {
      showNotification(
        paste("\u274c Error al guardar en S3:", e$message),
        type = "error", duration = 10
      )
      FALSE
    })

    if (!saved_ok) return()

    shared$interco_v2(registry)

    # Re-render code lists from the clean saved state
    for (ini in names(cmap)) {
      co <- companies[[ini]]
      ic_slots[[paste0(ini, "_ar")]] <- max(1L, length(co$ar) + 1L)
      ic_slots[[paste0(ini, "_ap")]] <- max(1L, length(co$ap) + 1L)
    }
    ic_trigger(ic_trigger() + 1L)

    msg <- if (had_prefix)
      "\u2705 Guardado. Se eliminaron prefijos de letra autom\u00e1ticamente."
    else
      "\u2705 Configuraci\u00f3n intercompany guardada en S3."
    showNotification(msg, type = "message", duration = 4)
  }, ignoreInit = TRUE)

  # ── IC Scanner (dev only) ─────────────────────────────────────────────────────
  # Scans loaded SAP snapshots for unique CardCodes + RFCs, presents them in a
  # selectable DT so the dev can identify IC codes in one pass during setup.

  ic_scan_candidates <- reactiveVal(NULL)
  ic_code_lookup     <- reactiveVal(list())   # ledger → ini → code → nombre

  # Build code→name lookup from S3 snapshots.
  # Used to label IC code inputs with the matched company name.
  .build_code_lookup <- function() {
    snap_ar <- tryCatch(load_sap_snapshot("AR")$data, error = function(e) NULL)
    snap_ap <- tryCatch(load_sap_snapshot("AP")$data, error = function(e) NULL)
    cmap    <- shared$company_map()
    inv_map <- setNames(names(cmap), unname(cmap))

    extract_lkp <- function(df, code_col) {
      if (is.null(df) || !is.data.frame(df) || !code_col %in% names(df)) return(tibble())
      df |>
        dplyr::filter(!is.na(.data[[code_col]]), nzchar(trimws(.data[[code_col]])),
                      !is.na(Parte), nzchar(Parte)) |>
        dplyr::transmute(
          ini    = unname(inv_map[Empresa]),
          code   = toupper(trimws(.data[[code_col]])),
          nombre = Parte
        ) |>
        dplyr::filter(!is.na(ini)) |>
        dplyr::group_by(ini, code) |>
        dplyr::summarise(nombre = dplyr::first(nombre), .groups = "drop")
    }

    to_named_list <- function(df) {
      if (!nrow(df)) return(list())
      setNames(lapply(unique(df$ini), function(i) {
        rows <- df[df$ini == i, ]
        as.list(setNames(rows$nombre, rows$code))
      }), unique(df$ini))
    }

    list(
      ar = to_named_list(extract_lkp(snap_ar, "C\u00f3digo de cliente")),
      ap = to_named_list(extract_lkp(snap_ap, "C\u00f3digo de proveedor"))
    )
  }

  observeEvent(input$scan_ic_btn, {
    # Prefer S3 snapshots — they include ALL companies even when SAP was unreachable
    # this session.  Fall back to in-memory session data when snapshots are absent.
    sap_ar <- tryCatch(load_sap_snapshot("AR")$data, error = function(e) NULL) %||%
              shared$sap_data()[["AR"]]
    sap_ap <- tryCatch(load_sap_snapshot("AP")$data, error = function(e) NULL) %||%
              shared$sap_data()[["AP"]]

    if (is.null(sap_ar) && is.null(sap_ap)) {
      showNotification("No hay datos SAP disponibles. Refresca los datos primero.",
                       type = "warning")
      return()
    }

    candidates <- scan_ic_candidates(sap_ar, sap_ap, shared$interco_v2(), shared$company_map())

    if (!nrow(candidates)) {
      showNotification("No se encontraron c\u00f3digos en las facturas cargadas.",
                       type = "warning")
      return()
    }

    # Sort candidates to EXACTLY match DT display order so rows_selected indices align.
    # DT col 0 = Empresa string (asc), col 1 = Ledger "CxC"/"CxP" (asc), col 5 = Facturas (desc).
    # IMPORTANT: sort by the DISPLAY ledger label ("CxC"/"CxP"), NOT internal "AR"/"AP",
    # because "AP"<"AR" but "CxC"<"CxP" — opposite alphabetical order, which causes index misalignment.
    cmap          <- shared$company_map()
    emp_labels    <- paste0(candidates$initials, " \u2014 ",
                            unname(cmap[candidates$initials]))
    ledger_labels <- dplyr::if_else(candidates$ledger == "AR", "CxC", "CxP")
    ord <- order(emp_labels, ledger_labels, -candidates$n_facturas)
    candidates <- candidates[ord, ]
    ic_scan_candidates(candidates)

    presel <- which(candidates$is_ic)

    tbl <- candidates |>
      dplyr::transmute(
        Empresa     = paste0(initials, " \u2014 ",
                             unname(cmap[initials]) %||% initials),
        Ledger      = dplyr::if_else(ledger == "AR", "CxC", "CxP"),
        Codigo      = code,
        NombreSAP   = nombre,
        RFC         = dplyr::if_else(is.na(rfc) | !nzchar(rfc %||% ""), "\u2014", rfc),
        Facturas    = n_facturas
      )
    names(tbl)[3] <- "C\u00f3digo"
    names(tbl)[4] <- "Nombre SAP"

    showModal(modalDialog(
      title     = tagList(icon("magnifying-glass"),
                          " Esc\u00e1ner IC \u2014 facturas en memoria"),
      size      = "xl",
      easyClose = FALSE,
      p(class = "text-muted small mb-2",
        "Selecciona los c\u00f3digos que son Intercompany.",
        "Los ya configurados aparecen pre-seleccionados.",
        "Aplicar", strong("agrega"), "los c\u00f3digos seleccionados \u2014 no elimina los existentes.",
        "Para eliminar un c\u00f3digo, borra su campo en la pesta\u00f1a Intercompany."),
      DT::DTOutput("ic_scan_tbl"),
      footer = tagList(
        actionButton("ic_scan_apply",
                     tagList(icon("check"), " Aplicar selecci\u00f3n"),
                     class = "btn btn-primary btn-sm"),
        tags$span("\u00a0"),
        modalButton("Cancelar")
      )
    ))

    output$ic_scan_tbl <- DT::renderDT({
      DT::datatable(
        tbl,
        selection  = list(mode = "multiple", selected = presel),
        rownames   = FALSE,
        class      = "table table-sm table-hover",
        options    = list(
          pageLength = 30,
          order      = list(list(0L, "asc"), list(1L, "asc"), list(5L, "desc"))
        )
      )
    }, server = FALSE)
  })

  observeEvent(input$ic_scan_apply, {
    sel        <- input$ic_scan_tbl_rows_selected
    candidates <- ic_scan_candidates()
    removeModal()

    if (is.null(candidates) || !length(sel)) {
      showNotification("No se seleccionaron c\u00f3digos.", type = "warning")
      return()
    }

    selected  <- candidates[sel, ]
    cur       <- shared$interco_v2()
    ar_prefix <- toupper(cur$ar_prefix %||% "C")
    ap_prefix <- toupper(cur$ap_prefix %||% "P")

    # Strip whichever prefix matches (AR or AP) to get the raw numeric part.
    # SAP B1 uses the same CardCode in both AR and AP invoices for the same BP,
    # so the same numeric suffix appears in both snapshots — just with different
    # leading letters (C1027 in AR, P1027 in AP, or sometimes C1027 in both).
    # Stripping both prefixes and mirroring to both directions ensures the
    # filter works regardless of which snapshot the code was scanned from.
    strip_to_numeric <- function(code) {
      code <- toupper(trimws(code))
      s <- sub(paste0("^", ar_prefix), "", code)
      if (s != code) return(s)
      s <- sub(paste0("^", ap_prefix), "", code)
      if (s != code) return(s)
      code   # no known prefix — store as-is
    }

    cmap <- isolate(shared$company_map())

    new_companies <- setNames(
      lapply(names(cmap), function(ini) {
        ini_rows <- selected[selected$initials == ini, ]
        if (!nrow(ini_rows)) {
          # No scanner hits for this company — preserve any existing registry codes
          # rather than wiping them (company may just have no open IC invoices right now)
          existing <- cur$companies[[ini]] %||% list(ar = character(), ap = character())
          return(existing)
        }
        existing <- cur$companies[[ini]] %||% list(ar = character(), ap = character())
        ar_rows  <- ini_rows[ini_rows$ledger == "AR", ]
        ap_rows  <- ini_rows[ini_rows$ledger == "AP", ]
        # MERGE: union selected codes with existing so that pagination never
        # silently drops codes the user didn't scroll past this session.
        # To remove a code, delete it manually from the IC settings inputs.
        new_ar <- if (nrow(ar_rows)) { v <- unique(sapply(ar_rows$code, strip_to_numeric)); v[nzchar(v)] } else character()
        new_ap <- if (nrow(ap_rows)) { v <- unique(sapply(ap_rows$code, strip_to_numeric)); v[nzchar(v)] } else character()
        list(
          ar = unique(c(existing$ar %||% character(), new_ar)),
          ap = unique(c(existing$ap %||% character(), new_ap))
        )
      }),
      names(cmap)
    )

    # RFC lookup: full_code → RFC (named character vector, merged with existing)
    rfc_rows  <- selected[!is.na(selected$rfc) & nzchar(selected$rfc %||% ""), ]
    new_rfcs  <- cur$rfcs %||% character()
    if (nrow(rfc_rows)) {
      extra <- setNames(rfc_rows$rfc, rfc_rows$code)
      new_rfcs[names(extra)] <- extra
    }

    new_registry <- list(
      ar_prefix = cur$ar_prefix %||% "C",
      ap_prefix = cur$ap_prefix %||% "P",
      rfcs      = new_rfcs,
      companies = new_companies
    )

    # Log what we're about to save so mis-matches are visible in console
    for (ini in names(cmap)) {
      co <- new_companies[[ini]]
      message("[IC_SCAN_APPLY] ", ini,
              "  AR=", paste(co$ar, collapse=",") %||% "(none)",
              "  AP=", paste(co$ap, collapse=",") %||% "(none)")
    }

    shared$interco_v2(new_registry)
    # Auto-persist scanner results immediately — no manual Guardar required
    tryCatch(
      save_interco_v2(new_registry, client_id = shared$active_client_id()),
      error = function(e)
        showNotification(paste("No se pudo guardar en S3:", e$message), type = "warning")
    )

    # The scanner modal replaced the settings modal (Shiny supports only one modal at a time).
    # Re-open the settings modal shell, then immediately navigate to the IC panel so
    # the user sees the freshly loaded codes without having to reopen settings manually.
    show_settings_modal(input, output, session, shared)
    .show_ic_panel()

    showNotification(
      sprintf("%d c\u00f3digo(s) IC guardados.", nrow(selected)),
      type = "message", duration = 4
    )
  })

  # ── Business Partners ─────────────────────────────────────────────────────────
  observeEvent(input$stg_btn_catalogo, {
    if (.ic_has_changes()) {
      ic_pending_nav("catalogo")
      showModal(.ic_unsaved_modal())
      return()
    }
    ic_panel_active(FALSE)
    # Populate bp_candidates on first open (or if empty)
    if (is.null(bp_candidates())) {
      snap_ar <- tryCatch(load_sap_snapshot("AR"), error = function(e) NULL)
      snap_ap <- tryCatch(load_sap_snapshot("AP"), error = function(e) NULL)
      sap_ar  <- snap_ar$data
      sap_ap  <- snap_ap$data
      if (!is.null(sap_ar) || !is.null(sap_ap)) {
        cands <- scan_ic_candidates(sap_ar, sap_ap, isolate(shared$interco_v2()), isolate(shared$company_map()))
        bp_candidates(cands)
      }
    }
    output$settings_panel <- renderUI({
      .bp_panel_ui(shared, bp_candidates())
    })
  }, ignoreInit = TRUE)

  # Refresh SAP BP table
  observeEvent(input$bp_refresh_btn, {
    snap_ar <- tryCatch(load_sap_snapshot("AR"), error = function(e) NULL)
    snap_ap <- tryCatch(load_sap_snapshot("AP"), error = function(e) NULL)
    sap_ar  <- snap_ar$data
    sap_ap  <- snap_ap$data
    if (is.null(sap_ar) && is.null(sap_ap)) {
      showNotification("No hay snapshots SAP disponibles.", type = "warning")
      return()
    }
    cands <- scan_ic_candidates(sap_ar, sap_ap, isolate(shared$interco_v2()))
    bp_candidates(cands)
    output$settings_panel <- renderUI({ .bp_panel_ui(shared, bp_candidates()) })
    showNotification("Tabla de Business Partners actualizada.", type = "message", duration = 3)
  })

  # Apply selected BP rows as IC codes
  observeEvent(input$bp_apply_ic_btn, {
    sel      <- input$tbl_bp_sap_rows_selected
    sorted_c <- bp_sorted_candidates()
    if (is.null(sorted_c) || !length(sel)) {
      showNotification("Selecciona al menos un c\u00f3digo primero.", type = "warning")
      return()
    }

    selected  <- sorted_c[sel, ]
    cur       <- shared$interco_v2()
    ar_prefix <- toupper(cur$ar_prefix %||% "C")
    ap_prefix <- toupper(cur$ap_prefix %||% "P")

    strip_num <- function(code) {
      code <- toupper(trimws(code))
      s <- sub(paste0("^", ar_prefix), "", code); if (s != code) return(s)
      s <- sub(paste0("^", ap_prefix), "", code); if (s != code) return(s)
      code
    }

    cmap <- isolate(shared$company_map())

    new_companies <- setNames(
      lapply(names(cmap), function(ini) {
        ini_rows <- selected[selected$initials == ini, ]
        existing <- cur$companies[[ini]] %||% list(ar = character(), ap = character())
        if (!nrow(ini_rows)) return(existing)
        ar_rows <- ini_rows[ini_rows$ledger == "AR", ]
        ap_rows <- ini_rows[ini_rows$ledger == "AP", ]
        new_ar  <- if (nrow(ar_rows)) { v <- unique(sapply(ar_rows$code, strip_num)); v[nzchar(v)] } else character()
        new_ap  <- if (nrow(ap_rows)) { v <- unique(sapply(ap_rows$code, strip_num)); v[nzchar(v)] } else character()
        list(
          ar = unique(c(existing$ar %||% character(), new_ar)),
          ap = unique(c(existing$ap %||% character(), new_ap))
        )
      }),
      names(cmap)
    )

    rfc_rows <- selected[!is.na(selected$rfc) & nzchar(selected$rfc %||% ""), ]
    new_rfcs <- cur$rfcs %||% character()
    if (nrow(rfc_rows)) new_rfcs[rfc_rows$code] <- rfc_rows$rfc

    new_registry <- list(
      ar_prefix = cur$ar_prefix %||% "C",
      ap_prefix = cur$ap_prefix %||% "P",
      rfcs      = new_rfcs,
      companies = new_companies
    )

    shared$interco_v2(new_registry)
    tryCatch(
      save_interco_v2(new_registry, client_id = shared$active_client_id()),
      error = function(e)
        showNotification(paste("No se pudo guardar en S3:", e$message), type = "warning")
    )

    # Refresh is_ic flags in bp_candidates so preselection updates
    new_cands <- tryCatch({
      sap_ar <- tryCatch(load_sap_snapshot("AR")$data, error = function(e) NULL)
      sap_ap <- tryCatch(load_sap_snapshot("AP")$data, error = function(e) NULL)
      scan_ic_candidates(sap_ar, sap_ap, new_registry, isolate(shared$company_map()))
    }, error = function(e) NULL)
    if (!is.null(new_cands) && nrow(new_cands)) bp_candidates(new_cands)

    showNotification(
      sprintf("%d c\u00f3digo(s) IC guardados desde Business Partners.", nrow(selected)),
      type = "message", duration = 4
    )
  })

  # Render SAP BP table with auto-match against catálogo.
  # Sort to match DT display order so row indices align with bp_sorted_candidates().
  output$tbl_bp_sap <- DT::renderDT({
    cands <- bp_candidates()
    req(!is.null(cands), nrow(cands) > 0)
    cmap    <- shared$company_map()
    prov    <- tryCatch(shared$proveedores_db(), error = function(e) NULL)
    emp_lbl <- paste0(cands$initials, " \u2014 ",
                      unname(cmap[cands$initials]) %||% cands$initials)
    ldg_lbl <- dplyr::if_else(cands$ledger == "AR", "CxC", "CxP")
    ord     <- order(emp_lbl, ldg_lbl, -cands$n_facturas)
    sorted  <- cands[ord, ]
    bp_sorted_candidates(sorted)
    presel  <- which(sorted$is_ic)
    .bp_datatable(sorted, prov, presel = presel, cmap = cmap)
  }, server = FALSE)

  output$tbl_catalogo <- DT::renderDataTable({
    .catalogo_datatable(shared)
  }, server = TRUE)

  observeEvent(input$cat_empresa_filter, {
    output$tbl_catalogo <- DT::renderDataTable({
      .catalogo_datatable(shared, empresa_filter = input$cat_empresa_filter)
    }, server = TRUE)
  }, ignoreInit = TRUE)

  # ── Cuentas de Empresa ────────────────────────────────────────────────────────
  observeEvent(input$stg_btn_cuentas, {
    if (.ic_has_changes()) {
      ic_pending_nav("cuentas")
      showModal(.ic_unsaved_modal())
      return()
    }
    ic_panel_active(FALSE)
    output$settings_panel <- renderUI({ .cuentas_panel_ui(cmap = shared$company_map()) })
  }, ignoreInit = TRUE)

  proveedoresServer("prov_mod", shared)

  # ── Políticas de Pago ─────────────────────────────────────────────────────────
  observeEvent(input$stg_btn_policies, {
    if (.ic_has_changes()) {
      ic_pending_nav("policies")
      showModal(.ic_unsaved_modal())
      return()
    }
    ic_panel_active(FALSE)
    output$settings_panel <- renderUI({ .policies_merged_panel_ui("reglas") })
  }, ignoreInit = TRUE)

  # ── Empresas y Socios (redirects to merged Socios tab) ────────────────────────
  observeEvent(input$stg_btn_companies, {
    if (.ic_has_changes()) {
      ic_pending_nav("companies")
      showModal(.ic_unsaved_modal())
      return()
    }
    ic_panel_active(FALSE)
    output$settings_panel <- renderUI({ .policies_merged_panel_ui("socios") })
  }, ignoreInit = TRUE)

}

