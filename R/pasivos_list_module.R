# =============================================================================
# R/pasivos_list_module.R
# Stage 4: "Lista de pasivos" sub-tab — flat one-row-per-liability view.
#
# Shiny module entry points:
#   pasivos_list_module_ui(id)
#   pasivos_list_module_server(id, shared)
# =============================================================================

# ── UI ─────────────────────────────────────────────────────────────────────────
pasivos_list_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "pasivos-list-module p-2",

    # Filters bar (mirrors Tabla view)
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-end py-2 border-bottom mb-2",
      shiny::div(class = "flex-shrink-0",
        shiny::selectizeInput(ns("empresa_sel"), "Empresa",
                              choices  = character(0), multiple = TRUE,
                              options  = list(placeholder = "Todas"),
                              width    = "180px")
      ),
      shiny::div(class = "flex-shrink-0",
        shiny::checkboxGroupInput(ns("categoria_sel"), "Categoría",
          choices  = c("Pago regular" = "regular",
                       "Pasivo financiero" = "financiero",
                       "Tarjeta" = "tarjeta"),
          selected = c("regular", "financiero", "tarjeta"),
          inline   = TRUE)
      ),
      shiny::div(class = "flex-shrink-0",
        shiny::selectizeInput(ns("subcategoria_sel"), "Sub-categoría",
                              choices  = character(0), multiple = TRUE,
                              options  = list(placeholder = "Todas"),
                              width    = "140px")
      ),
      shiny::div(class = "flex-shrink-0",
        shiny::selectInput(ns("currency_sel"), "Moneda",
                           choices  = c("All", CURRENCIES),
                           selected = "All", width = "90px")
      ),
      shiny::div(class = "flex-grow-1",
        shiny::textInput(ns("search_text"), "Buscar",
                         placeholder = "Nombre, Parte...", width = "200px")
      ),
      shiny::div(class = "flex-shrink-0 align-self-end",
        shiny::checkboxInput(ns("show_archived"), "Mostrar archivados", value = FALSE)
      )
    ),

    # Table
    shiny::div(
      class = "pasivos-table-wrap",
      DT::dataTableOutput(ns("list_table"))
    ),

    # Inline CSS for action buttons within DT
    shiny::tags$style(shiny::HTML("
      .pasivos-list-btn { border:none; background:transparent; cursor:pointer; padding:2px 5px; }
      .pasivos-list-btn:hover { opacity:.7; }
    "))
  )
}

# ── Server ────────────────────────────────────────────────────────────────────
pasivos_list_module_server <- function(id, shared) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    archive_target_rv <- shiny::reactiveVal("")

    # ── Populate choices ────────────────────────────────────────────────────
    shiny::observe({
      liabs <- tryCatch(shared$pasivos_liabilities_db(), error = function(e) NULL)
      if (is.null(liabs) || !nrow(liabs)) return()
      emps <- sort(unique(liabs$empresa[!is.na(liabs$empresa)]))
      shiny::updateSelectizeInput(session, "empresa_sel", choices = emps)
      subs <- sort(unique(liabs$subcategoria[
        !is.na(liabs$subcategoria) & nzchar(liabs$subcategoria %||% "")]))
      shiny::updateSelectizeInput(session, "subcategoria_sel", choices = subs)
    })

    # ── Filtered liabilities ────────────────────────────────────────────────
    filtered_liabs <- shiny::reactive({
      liabs <- tryCatch(shared$pasivos_liabilities_db(),
                        error = function(e) .schema_pasivos_liability())
      if (!nrow(liabs)) return(liabs)

      # Estado filter
      if (!isTRUE(input$show_archived)) {
        liabs <- liabs[liabs$estado %in% c("active", "paused"), , drop = FALSE]
      }

      # Empresa
      emps <- input$empresa_sel %||% character(0)
      if (length(emps)) liabs <- liabs[liabs$empresa %in% emps, , drop = FALSE]

      # Categoria
      cats <- input$categoria_sel %||% c("regular", "financiero", "tarjeta")
      liabs <- liabs[liabs$categoria %in% cats, , drop = FALSE]

      # Sub-cat
      subs <- input$subcategoria_sel %||% character(0)
      if (length(subs)) liabs <- liabs[liabs$subcategoria %in% subs, , drop = FALSE]

      # Currency
      cur <- input$currency_sel %||% "All"
      if (!identical(cur, "All")) liabs <- liabs[liabs$moneda_pago == cur, , drop = FALSE]

      # Search
      q <- trimws(input$search_text %||% "")
      if (nzchar(q)) {
        mask <- grepl(q, liabs$nombre %||% "", ignore.case = TRUE) |
                grepl(q, liabs$parte  %||% "", ignore.case = TRUE)
        liabs <- liabs[mask, , drop = FALSE]
      }

      # Default sort: (empresa, categoria, nombre)
      if (nrow(liabs))
        liabs <- liabs[order(liabs$empresa, liabs$categoria, liabs$nombre), , drop = FALSE]

      liabs
    })

    # ── Next provision lookup ───────────────────────────────────────────────
    next_provision <- shiny::reactive({
      provs <- tryCatch(shared$pasivos_provisions_db(),
                        error = function(e) .schema_pasivos_provision())
      liabs <- filtered_liabs()
      if (!nrow(liabs) || !nrow(provs)) return(tibble::tibble(id = character(),
                                                               next_fecha = as.Date(character()),
                                                               next_monto = numeric()))
      future_provs <- provs[
        provs$estado == "provisional" & !is.na(provs$fecha_efectiva) &
        provs$fecha_efectiva >= Sys.Date(), , drop = FALSE
      ]
      if (!nrow(future_provs)) return(tibble::tibble(id = character(),
                                                      next_fecha = as.Date(character()),
                                                      next_monto = numeric()))

      by_liab <- lapply(liabs$id, function(lid) {
        rows <- future_provs[!is.na(future_provs$liability_id) &
                               future_provs$liability_id == lid, , drop = FALSE]
        if (!nrow(rows)) return(tibble::tibble(id = lid,
                                               next_fecha = as.Date(NA),
                                               next_monto = NA_real_))
        first_row <- rows[which.min(rows$fecha_efectiva), , drop = FALSE]
        tibble::tibble(id = lid,
                       next_fecha = first_row$fecha_efectiva[1],
                       next_monto = tryCatch(pasivos_pago_amount(first_row),
                                             error = function(e) NA_real_))
      })
      do.call(dplyr::bind_rows, by_liab)
    })

    # ── Build display table ─────────────────────────────────────────────────
    display_df <- shiny::reactive({
      liabs <- filtered_liabs()
      np    <- next_provision()

      if (!nrow(liabs)) return(tibble::tibble(
        `✏` = character(), Nombre = character(), Categoría = character(),
        Empresa = character(), Parte = character(), Moneda = character(),
        Estado = character(), `Próxima provisión` = character(),
        Acciones = character()
      ))

      cat_label <- function(cat, flav) {
        switch(cat,
          regular    = "Pago regular",
          financiero = paste0("Financiero — ", switch(flav %||% "",
            credito_simple  = "Crédito simple",
            arrendamiento   = "Arrendamiento",
            linea_revolvente = "Línea revolvente",
            otro            = "Otro",
            flav %||% "")),
          tarjeta    = "Tarjeta",
          cat
        )
      }

      estado_label <- function(e) switch(e %||% "",
        active  = "Activo", paused = "Pausado",
        deleted = "Archivado", closed = "Cerrado", e %||% "—"
      )

      moneda_str <- function(mp, ce) {
        if (!is.na(ce) && nzchar(ce %||% ""))
          sprintf("%s (cotizado %s)", mp, ce)
        else mp
      }

      np_lookup <- if (nrow(np)) {
        setNames(as.list(seq_len(nrow(np))), np$id)
      } else list()

      rows <- lapply(seq_len(nrow(liabs)), function(i) {
        r  <- liabs[i, , drop = FALSE]
        lid <- r$id[1]

        np_str <- tryCatch({
          np_idx <- which(np$id == lid)
          if (length(np_idx)) {
            nd <- np$next_fecha[np_idx[1]]
            nm <- np$next_monto[np_idx[1]]
            if (!is.na(nd)) sprintf("%s  %s", format(nd, "%Y-%m-%d"),
                                    if (!is.na(nm)) fmt_money(nm) else "")
            else "—"
          } else "—"
        }, error = function(e) "—")

        # Button HTML (fires global Shiny inputs)
        edit_btn <- sprintf(
          '<button class="pasivos-list-btn text-primary" title="Editar" onclick="Shiny.setInputValue(\'pasivos_edit_liability\',\'%s\',{priority:\'event\'})">&#9998;</button>',
          htmltools::htmlEscape(lid, attribute = TRUE)
        )
        estado <- r$estado[1] %||% "active"
        pause_icon <- if (identical(estado, "active")) "&#9646;&#9646;" else "&#9654;"
        pause_btn <- sprintf(
          '<button class="pasivos-list-btn text-warning" title="%s" onclick="Shiny.setInputValue(\'%s\',\'%s\',{priority:\'event\'})">%s</button>',
          if (identical(estado, "active")) "Pausar" else "Reanudar",
          ns("pasivos_list_pause"),
          htmltools::htmlEscape(lid, attribute = TRUE),
          pause_icon
        )
        archive_btn <- sprintf(
          '<button class="pasivos-list-btn text-danger" title="Archivar" onclick="Shiny.setInputValue(\'%s\',\'%s\',{priority:\'event\'})">&#128465;</button>',
          ns("pasivos_list_archive"),
          htmltools::htmlEscape(lid, attribute = TRUE)
        )

        list(
          pencil      = edit_btn,
          nombre      = r$nombre[1]   %||% "",
          categoria   = cat_label(r$categoria[1] %||% "", r$flavor[1]),
          empresa     = r$empresa[1]  %||% "",
          parte       = r$parte[1]    %||% "",
          moneda      = moneda_str(r$moneda_pago[1] %||% "", r$cotizado_en[1]),
          estado      = estado_label(estado),
          next_prov   = np_str,
          acciones    = paste0(edit_btn, " ", pause_btn, " ", archive_btn)
        )
      })

      df <- do.call(dplyr::bind_rows, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
      names(df) <- c("✏", "Nombre", "Categoría", "Empresa", "Parte",
                      "Moneda", "Estado", "Próxima provisión", "Acciones")
      df
    })

    # ── DT render ───────────────────────────────────────────────────────────
    output$list_table <- DT::renderDataTable({
      df <- display_df()
      DT::datatable(
        df,
        rownames  = FALSE,
        escape    = FALSE,
        selection = "none",
        options   = list(
          dom        = "tp",
          pageLength = 25,
          ordering   = TRUE,
          columnDefs = list(
            list(orderable = FALSE, targets = c(0, 8)),  # pencil + acciones cols
            list(width = "30px", targets = 0)
          )
        ),
        class = "table table-sm table-hover"
      )
    }, server = TRUE)

    # ── Pause / Resume ──────────────────────────────────────────────────────
    shiny::observeEvent(input$pasivos_list_pause, ignoreInit = TRUE, {
      lid  <- input$pasivos_list_pause %||% ""
      if (!nzchar(lid)) return()
      user <- tryCatch(shared$current_user(), error = function(e) "system")
      if (!has_capability(user, "pasivos.edit_liability")) {
        shiny::showNotification("Sin permiso.", type = "error"); return()
      }
      liabs <- tryCatch(shared$pasivos_liabilities_db(),
                        error = function(e) .schema_pasivos_liability())
      idx <- which(liabs$id == lid)
      if (!length(idx)) return()
      new_estado <- if (identical(liabs$estado[idx[1]], "active")) "paused" else "active"
      liabs$estado[idx[1]] <- new_estado
      liabs$updated_at[idx[1]] <- Sys.time()
      liabs$updated_by[idx[1]] <- user
      tryCatch(save_pasivos_liabilities(liabs), error = function(e) NULL)
      shared$pasivos_liabilities_db(liabs)

      if (identical(new_estado, "paused")) {
        # Remove future provisional provisions
        provs <- tryCatch(shared$pasivos_provisions_db(),
                          error = function(e) .schema_pasivos_provision())
        provs_drop <- provs$liability_id == lid &
                      provs$estado == "provisional" &
                      !is.na(provs$fecha_efectiva) &
                      provs$fecha_efectiva >= Sys.Date()
        if (any(!is.na(provs_drop) & provs_drop)) {
          provs <- provs[!(!is.na(provs_drop) & provs_drop), , drop = FALSE]
          tryCatch(save_pasivos_provisions(provs), error = function(e) NULL)
          shared$pasivos_provisions_db(provs)
        }
        shiny::showNotification("Pasivo pausado. Provisiones futuras eliminadas.",
                                type = "message", duration = 3)
      } else {
        shiny::showNotification(
          "Pasivo reanudado. Usa '+ Agregar pasivo' o el lápiz para regenerar provisiones.",
          type = "message", duration = 5)
      }

      tryCatch(pasivos_log_audit(
        action_type = if (new_estado == "paused") "liability.edited" else "liability.edited",
        user        = user,
        empresa     = liabs$empresa[idx[1]],
        target_kind = "liability",
        target_id   = lid,
        notes       = sprintf("estado changed to %s", new_estado)
      ), error = function(e) NULL)
    })

    # ── Archive ─────────────────────────────────────────────────────────────
    shiny::observeEvent(input$pasivos_list_archive, ignoreInit = TRUE, {
      lid <- input$pasivos_list_archive %||% ""
      if (!nzchar(lid)) return()
      user <- tryCatch(shared$current_user(), error = function(e) "system")
      if (!has_capability(user, "pasivos.delete_liability")) {
        shiny::showNotification("Sin permiso para archivar.", type = "error"); return()
      }
      archive_target_rv(lid)
      shiny::showModal(shiny::modalDialog(
        "¿Archivar este pasivo? Sus provisiones futuras se eliminarán.",
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("plist_confirm_archive"), "Archivar",
                              class = "btn btn-danger")
        ),
        size = "s"
      ))
    })

    shiny::observeEvent(input$plist_confirm_archive, ignoreInit = TRUE, {
      lid  <- archive_target_rv()
      if (!nzchar(lid %||% "")) { shiny::removeModal(); return() }
      user <- tryCatch(shared$current_user(), error = function(e) "system")
      liabs <- tryCatch(shared$pasivos_liabilities_db(),
                        error = function(e) .schema_pasivos_liability())
      idx <- which(liabs$id == lid)
      if (!length(idx)) { shiny::removeModal(); return() }
      liabs$estado[idx[1]]     <- "deleted"
      liabs$updated_at[idx[1]] <- Sys.time()
      liabs$updated_by[idx[1]] <- user
      tryCatch(save_pasivos_liabilities(liabs), error = function(e) NULL)
      shared$pasivos_liabilities_db(liabs)

      # Remove future provisions
      provs <- tryCatch(shared$pasivos_provisions_db(),
                        error = function(e) .schema_pasivos_provision())
      keep <- !(provs$liability_id == lid &
                provs$estado == "provisional" &
                !is.na(provs$fecha_efectiva) &
                provs$fecha_efectiva >= Sys.Date())
      keep[is.na(keep)] <- TRUE
      provs <- provs[keep, , drop = FALSE]
      tryCatch(save_pasivos_provisions(provs), error = function(e) NULL)
      shared$pasivos_provisions_db(provs)

      tryCatch(pasivos_log_audit(
        action_type = "liability.deleted",
        user        = user,
        empresa     = liabs$empresa[idx[1]],
        target_kind = "liability",
        target_id   = lid
      ), error = function(e) NULL)

      shiny::removeModal()
      shiny::showNotification("Pasivo archivado.", type = "message", duration = 3)
    })

    invisible(NULL)
  })
}
