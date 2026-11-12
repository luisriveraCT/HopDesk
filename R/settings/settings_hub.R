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
  usuario <- tryCatch(shared$usuario(), error = function(e) NULL)

  sincro_btn <- if (puede(usuario, "can_manage_sync")) {
    tagList(
      tags$hr(class = "my-1"),
      tags$p(class = "text-muted small px-2 mb-1 text-uppercase fw-semibold", "Admin"),
      actionButton("stg_btn_sincro",
        tagList(icon("rotate"), " Sincronización"),
        class = "btn btn-sm btn-outline-secondary text-start w-100")
    )
  } else NULL

  # The table's window (T09): ✕ sends stg_close_btn, which checks for
  # unsaved changes before closing (so no Esc / click outside either).
  showModal(hd_modal(
    titulo    = tagList(icon("gear"), " Configuración"),
    easyClose = FALSE,
    cerrar_id = "stg_close_btn",

    div(class = "settings-hub d-flex", style = "min-height: 460px;",

      # ── Sidebar ──────────────────────────────────────────────────────────
      div(class = "settings-sidebar d-flex flex-column gap-1 p-2 border-end bg-light flex-shrink-0",
        style = "width: 170px;",
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
      # No scroll of its own: a long panel scrolls the window, and a table
      # with alto = "ventana" (the Excel review) fills it -- one scroll.
      div(class = "settings-content flex-grow-1 p-2",
        uiOutput("settings_panel")
      )
    )
  ))

  output$settings_panel <- renderUI({ .settings_welcome_ui() })
}

# Rows of the saved IC registry (ic_registro): one per company of `cmap`
# with a code, its codes with their prefix. An empty registry (or one not
# loaded yet, NULL) gives no rows -- with the columns, never an error.
.ic_registro_filas <- function(reg, cmap) {
  vacia <- data.frame(id = character(), Empresa = character(), cxc = character(),
                      cxp = character(), stringsAsFactors = FALSE)
  if (!is.list(reg) || !length(reg$companies) || !length(cmap)) return(vacia)
  ar_pre <- reg$ar_prefix %||% "C"
  ap_pre <- reg$ap_prefix %||% "P"
  codigos <- function(cs, pre) if (length(cs)) paste(paste0(pre, cs), collapse = ", ") else "—"
  filas <- lapply(names(cmap), function(ini) {
    co <- reg$companies[[ini]] %||% list()
    ar <- co$ar %||% character(); ap <- co$ap %||% character()
    if (!length(ar) && !length(ap)) return(NULL)
    data.frame(id = ini, Empresa = ini, cxc = codigos(ar, ar_pre), cxp = codigos(ap, ap_pre),
               stringsAsFactors = FALSE)
  })
  filas <- Filter(Negate(is.null), filas)
  if (!length(filas)) vacia else do.call(rbind, filas)
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
  # The table (bp_sap, T16): "Aplicar como IC" takes the marked ids
  # (initials|ledger|code), never row positions. No pre-selection: "IC ✓"
  # shows the codes already in the registry.
  t_bp <- hd_table_server(
    "bp_sap_t", tabla = "bp_sap",
    datos = reactive(.ic_candidatos_filas(bp_candidates(), shared$company_map(),
                                          tryCatch(shared$proveedores_db(), error = function(e) NULL))),
    columnas = .ic_candidatos_columnas(catalogo = TRUE),
    piezas = c("sel_fila", "sel_rango", "sel_todos"),
    vacio = "tablas.ajustes.candidatos.vacio"
  )

  # ── Saved IC registry (ic_registro, class E) ─────────────────────────────────
  # What is saved, not what the boxes hold: follows shared$interco_v2().
  hd_table_server(
    "ic_registro_t", tabla = "ic_registro",
    datos = reactive(.ic_registro_filas(shared$interco_v2(), shared$company_map())),
    columnas = list(
      col_empresa("Empresa", "tablas.ajustes.ic_registro.empresa"),
      col_texto("cxc", "tablas.ajustes.ic_registro.cxc"),
      col_texto("cxp", "tablas.ajustes.ic_registro.cxp")
    ),
    vacio = "tablas.ajustes.ic_registro.vacio"
  )
  output$ic_registro_titulo <- renderUI({
    reg <- shared$interco_v2()
    n   <- function(l) sum(vapply(reg$companies %||% list(),
                                  function(co) length(co[[l]] %||% character()), integer(1)))
    if (!n("ar") && !n("ap")) return(NULL)
    tags$p(class = "fw-semibold small mb-2 text-success-emphasis",
           icon("circle-check"), " ",
           tr("tablas.ajustes.ic_registro.titulo", cxc = n("ar"), cxp = n("ap")))
  })


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
      output$settings_panel <- renderUI({
        .cuentas_panel_ui(cmap = shared$company_map(),
                          editar = puede(isolate(shared$usuario()), "can_manage_bancos"))
      })
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

    # The saved registry (ic_registro, T16) is the engine's table: drawn once
    # per opening, outside the editor's renderUI, so re-drawing the code
    # boxes never rebuilds it.
    output$settings_panel <- renderUI({
      div(
        tags$h6(class = "fw-semibold mb-3",
                tagList(icon("arrows-left-right"), " Configurar Intercompany")),
        div(class = "border rounded p-3 mb-3",
          uiOutput("ic_registro_titulo"),
          hd_table_ui("ic_registro_t", alto = "auto")),
        uiOutput("ic_editor")
      )
    })

    output$ic_editor <- renderUI({
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

      div(
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
        # Guardar and the scanner write the IC registry: only with
        # can_manage_interco (each save checks again on the server).
        if (puede(isolate(shared$usuario()), "can_manage_interco"))
          div(class = "mt-3 d-flex align-items-center gap-2 flex-wrap",
            actionButton("save_interco_v2", tagList(icon("floppy-disk"), " Guardar"),
                         class = "btn btn-primary btn-sm"),
            tags$span(class = "text-muted", "|"),
            actionButton("scan_ic_btn",
                         tagList(icon("magnifying-glass"), " ", tr("tablas.ajustes.escaner.boton")),
                         class = "btn btn-outline-secondary btn-sm"),
            tags$span(class = "text-muted small",
                      tr("tablas.ajustes.escaner.boton_ayuda"))
          )
      )
    })
  }

  observeEvent(input$stg_btn_interco, {
    .show_ic_panel()
  }, ignoreInit = TRUE)

  # ── Save ─────────────────────────────────────────────────────────────────────
  observeEvent(input$save_interco_v2, {
    if (!puede_o_avisa(shared, "can_manage_interco")) return()
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

    # Merged onto S3's current registry against the one this session opened
    # (CM12c): another session's codes saved since are kept.
    saved_ok <- tryCatch({
      registry <- save_interco_v2(registry, client_id = shared$effective_client_id(),
                                  base = isolate(shared$interco_v2()))
      companies <- registry$companies
      bump_sync_version("interco_v2", client_id = shared$effective_client_id())
      TRUE
    }, error = function(e) {
      showNotification(
        tr("tablas.ajustes.aviso.error_guardar_s3", error = e$message),
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
      tr("tablas.ajustes.aviso.ic_guardada_prefijos")
    else
      tr("tablas.ajustes.aviso.ic_guardada")
    showNotification(msg, type = "message", duration = 4)
    log_action(
      user        = tryCatch(shared$current_user(), error = function(e) "system"),
      module      = "settings_hub",
      action      = "guardar_interco_v2",
      description = "Configuraci\u00f3n intercompany (registry AR/AP por empresa) guardada",
      metadata    = list(ar_prefix = registry$ar_prefix, ap_prefix = registry$ap_prefix,
                         companies = names(registry$companies)),
      client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
      viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
    )
  }, ignoreInit = TRUE)

  # ── IC Scanner (dev only) ─────────────────────────────────────────────────────
  # Scans loaded SAP snapshots for unique CardCodes + RFCs, presents them in a
  # selectable table so the dev can identify IC codes in one pass during setup.

  ic_scan_candidates <- reactiveVal(NULL)
  ic_code_lookup     <- reactiveVal(list())   # ledger → ini → code → nombre
  # The scanner's table (escaner_ic, T16): the same columns as Business
  # Partners without En catálogo; "Aplicar selección" takes the marked ids.
  t_esc <- hd_table_server(
    "escaner_ic_t", tabla = "escaner_ic",
    datos = reactive(.ic_candidatos_filas(ic_scan_candidates(), shared$company_map())),
    columnas = .ic_candidatos_columnas(),
    piezas = c("sel_fila", "sel_rango", "sel_todos"),
    vacio = "tablas.ajustes.candidatos.vacio"
  )
  # The candidates behind the marked ids, in the table's order; ids no longer
  # there (another scan in between) are dropped.
  # The codes "Aplicar" really added (T23): in what it asked for, in what got
  # saved, and not in the registry before. "Aplicar" joins, so marking codes
  # already there adds nothing -- the notice counted every marked row.
  .ic_codigos_nuevos <- function(antes, pedido, guardado) {
    sum(vapply(names(pedido$companies %||% list()), function(ini) {
      sum(vapply(c("ar", "ap"), function(l) length(setdiff(
        intersect(pedido$companies[[ini]][[l]] %||% character(), guardado$companies[[ini]][[l]] %||% character()),
        antes$companies[[ini]][[l]] %||% character())), integer(1)))
    }, integer(1)))
  }
  .avisar_codigos_nuevos <- function(n, clave) {
    if (n > 0L) showNotification(tr(clave, n = n), type = "message", duration = 4)
    else showNotification(tr("tablas.ajustes.ic.ninguno_nuevo"), type = "message", duration = 4)
  }
  .candidatos_marcados <- function(cands, ids) {
    if (!is.data.frame(cands) || !nrow(cands) || !length(ids)) return(NULL)
    rows <- cands[match(ids, paste(cands$initials, cands$ledger, cands$code, sep = "|")), , drop = FALSE]
    rows <- rows[!is.na(rows$code), , drop = FALSE]
    if (nrow(rows)) rows else NULL
  }

  # The session's SAP snapshots, list(AR, AP), read with its own client (T23).
  # Without an effective client (or if it fails) nothing is read and the user
  # is told: a NULL client_id falls to the deployment's default key, which can
  # be another client's (or the shared one's) snapshots. Where a snapshot is
  # missing, the session's own SAP data (shared$sap_data(), already this
  # client's) stands in -- for every caller, so Business Partners and the
  # scanner see the same codes (only the scanner fell back before).
  .sap_snapshots <- function() {
    cid <- tryCatch(shared$effective_client_id(), error = function(e) NULL)
    if (!is.character(cid) || length(cid) != 1L || is.na(cid) || !nzchar(cid)) {
      showNotification(tr("tablas.ajustes.sap.sin_cliente"), type = "error")
      return(NULL)
    }
    en_sesion <- tryCatch(shared$sap_data(), error = function(e) NULL)
    list(AR = tryCatch(load_sap_snapshot("AR", client_id = cid)$data, error = function(e) NULL) %||% en_sesion[["AR"]],
         AP = tryCatch(load_sap_snapshot("AP", client_id = cid)$data, error = function(e) NULL) %||% en_sesion[["AP"]])
  }

  # Build code→name lookup from S3 snapshots.
  # Used to label IC code inputs with the matched company name.
  .build_code_lookup <- function() {
    snaps   <- .sap_snapshots()
    snap_ar <- snaps$AR
    snap_ap <- snaps$AP
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
    snaps <- .sap_snapshots()
    if (is.null(snaps)) return()
    sap_ar <- snaps$AR
    sap_ap <- snaps$AP

    if (is.null(sap_ar) && is.null(sap_ap)) {
      showNotification(tr("tablas.ajustes.sap.sin_datos"),
                       type = "warning")
      return()
    }

    candidates <- scan_ic_candidates(sap_ar, sap_ap, shared$interco_v2(), shared$company_map())

    if (!nrow(candidates)) {
      showNotification(tr("tablas.ajustes.sap.sin_codigos"),
                       type = "warning")
      return()
    }

    # The table (t_esc) sorts and marks "IC ✓" itself; the rows are read back by id.
    ic_scan_candidates(candidates)

    showModal(modalDialog(
      title     = tagList(icon("magnifying-glass"),
                          " ", tr("tablas.ajustes.escaner.titulo")),
      size      = "xl",
      easyClose = FALSE,
      p(class = "text-muted small mb-2", tr("tablas.ajustes.candidatos.ayuda_escaner")),
      hd_table_ui("escaner_ic_t", alto = "60vh"),
      footer = tagList(
        if (puede(isolate(shared$usuario()), "can_manage_interco"))
          actionButton("ic_scan_apply",
                       tagList(icon("check"), " ", tr("tablas.ajustes.escaner.aplicar")),
                       class = "btn btn-primary btn-sm"),
        tags$span(" "),
        modalButton(tr("comun.cancelar"))
      )
    ))
  })

  observeEvent(input$ic_scan_apply, {
    if (!puede_o_avisa(shared, "can_manage_interco")) return()
    selected <- .candidatos_marcados(ic_scan_candidates(), t_esc$seleccion())
    removeModal()

    if (is.null(selected)) {
      showNotification(tr("tablas.ajustes.escaner.ninguno"), type = "warning")
      return()
    }

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

    # Auto-persist scanner results immediately — no manual Guardar required.
    # Merged onto S3's current registry (CM12c); the session shows it only
    # once saved, and a failure is an error, not a warning over unsaved codes.
    guardado <- tryCatch(
      save_interco_v2(new_registry, client_id = shared$effective_client_id(), base = cur),
      error = function(e) { message("[IC_SCAN_APPLY] save failed: ", e$message); NULL })
    if (is.null(guardado)) { showNotification(tr("comun.error_guardar"), type = "error"); return() }
    bump_sync_version("interco_v2", client_id = shared$effective_client_id())
    shared$interco_v2(guardado)

    # The scanner modal replaced the settings modal (Shiny supports only one modal at a time).
    # Re-open the settings modal shell, then immediately navigate to the IC panel so
    # the user sees the freshly loaded codes without having to reopen settings manually.
    show_settings_modal(input, output, session, shared)
    .show_ic_panel()

    .avisar_codigos_nuevos(.ic_codigos_nuevos(cur, new_registry, guardado), "tablas.ajustes.ic.nuevos")
    log_action(
      user        = tryCatch(shared$current_user(), error = function(e) "system"),
      module      = "settings_hub",
      action      = "aplicar_scan_interco",
      description = sprintf("%d c\u00f3digo(s) IC aplicados desde el esc\u00e1ner de SAP", nrow(selected)),
      metadata    = list(n_codes = nrow(selected), companies = unique(selected$initials)),
      client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
      viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
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
      snaps  <- .sap_snapshots()
      sap_ar <- snaps$AR
      sap_ap <- snaps$AP
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
    snaps  <- .sap_snapshots()
    if (is.null(snaps)) return()
    sap_ar <- snaps$AR
    sap_ap <- snaps$AP
    if (is.null(sap_ar) && is.null(sap_ap)) {
      showNotification(tr("tablas.ajustes.sap.sin_datos"), type = "warning")
      return()
    }
    cands <- scan_ic_candidates(sap_ar, sap_ap, isolate(shared$interco_v2()), isolate(shared$company_map()))
    bp_candidates(cands)
    output$settings_panel <- renderUI({ .bp_panel_ui(shared, bp_candidates()) })
    showNotification(tr("tablas.ajustes.bp.actualizada"), type = "message", duration = 3)
  })

  # Apply selected BP rows as IC codes
  observeEvent(input$bp_apply_ic_btn, {
    if (!puede_o_avisa(shared, "can_manage_interco")) return()
    selected <- .candidatos_marcados(bp_candidates(), t_bp$seleccion())
    if (is.null(selected)) {
      showNotification(tr("tablas.ajustes.bp.ninguno"), type = "warning")
      return()
    }

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

    # Merged onto S3's current registry (CM12c); shown only once saved.
    guardado <- tryCatch(
      save_interco_v2(new_registry, client_id = shared$effective_client_id(), base = cur),
      error = function(e) { message("[BP_APPLY_IC] save failed: ", e$message); NULL })
    if (is.null(guardado)) { showNotification(tr("comun.error_guardar"), type = "error"); return() }
    bump_sync_version("interco_v2", client_id = shared$effective_client_id())
    shared$interco_v2(guardado)
    pedido       <- new_registry   # what was asked for: the notice counts what it added
    new_registry <- guardado

    # Refresh is_ic flags in bp_candidates so "IC ✓" updates
    new_cands <- tryCatch({
      snaps <- .sap_snapshots()
      scan_ic_candidates(snaps$AR, snaps$AP, new_registry, isolate(shared$company_map()))
    }, error = function(e) NULL)
    if (!is.null(new_cands) && nrow(new_cands)) bp_candidates(new_cands)

    .avisar_codigos_nuevos(.ic_codigos_nuevos(cur, pedido, guardado), "tablas.ajustes.ic.nuevos_bp")
    log_action(
      user        = tryCatch(shared$current_user(), error = function(e) "system"),
      module      = "settings_hub",
      action      = "aplicar_bp_interco",
      description = sprintf("%d c\u00f3digo(s) IC aplicados desde Business Partners", nrow(selected)),
      metadata    = list(n_codes = nrow(selected), companies = unique(selected$initials)),
      client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
      viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
    )
  })

  # ── Cuentas de Empresa ────────────────────────────────────────────────────────
  observeEvent(input$stg_btn_cuentas, {
    if (.ic_has_changes()) {
      ic_pending_nav("cuentas")
      showModal(.ic_unsaved_modal())
      return()
    }
    ic_panel_active(FALSE)
    output$settings_panel <- renderUI({
        .cuentas_panel_ui(cmap = shared$company_map(),
                          editar = puede(isolate(shared$usuario()), "can_manage_bancos"))
      })
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
