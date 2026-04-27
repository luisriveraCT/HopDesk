# =============================================================================
# R/proveedores_module.R
# Full Proveedores management module.
# Tabs: Catálogo activo | Importar Excel | Inactivos
# =============================================================================

# ── UI ────────────────────────────────────────────────────────────────────────
proveedoresUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      .prov-badge-completo { background:#d1fae5; color:#065f46; border:1px solid #6ee7b7; border-radius:4px; padding:2px 8px; font-size:0.78rem; }
      .prov-badge-incompleto { background:#fef9c3; color:#713f12; border:1px solid #fde68a; border-radius:4px; padding:2px 8px; font-size:0.78rem; }
      .prov-badge-expirado { background:#fee2e2; color:#991b1b; border:1px solid #fca5a5; border-radius:4px; padding:2px 8px; font-size:0.78rem; }
      .prov-conflict-card { border:1px solid #e5e7eb; border-radius:8px; padding:16px; margin-bottom:12px; }
      .prov-conflict-side { flex:1; padding:8px; background:#f9fafb; border-radius:4px; font-size:0.85rem; }
      .prov-row-duplicate { background:#fce7f3 !important; }
    ")),
    # Static event delegation — works regardless of render order
    tags$script(HTML(sprintf("
      $(document).off('click.prov').on('click.prov', '.prov-edit-btn', function() {
        Shiny.setInputValue('%s', $(this).data('id'), {priority:'event'});
      });
      $(document).on('click.prov', '.prov-inactivar-btn', function() {
        Shiny.setInputValue('%s', $(this).data('id'), {priority:'event'});
      });
      $(document).on('click.prov', '.prov-del-btn', function() {
        if (confirm('\\u00bfEliminar permanentemente este proveedor?'))
          Shiny.setInputValue('%s', $(this).data('id'), {priority:'event'});
      });
      $(document).on('click.prov', '.prov-reactivar-btn', function() {
        Shiny.setInputValue('%s', $(this).data('id'), {priority:'event'});
      });
      $(document).on('click.prov', '.prov-del-inac-btn', function() {
        if (confirm('\\u00bfEliminar permanentemente?'))
          Shiny.setInputValue('%s', $(this).data('id'), {priority:'event'});
      });
      $(document).on('click.prov', '.prov-upload-del-btn', function() {
        Shiny.setInputValue('%s', $(this).data('rownum'), {priority:'event'});
      });
    ",
      ns("prov_edit_id"), ns("prov_inactivar_id"), ns("prov_delete_id"),
      ns("prov_reactivar_id"), ns("prov_del_inac_id"),
      ns("prov_upload_del_rownum")
    ))),

    tabsetPanel(id = ns("prov_tabs"),
      tabPanel("Catálogo activo",  value = "catalogo",  .prov_catalogo_tab(ns)),
      tabPanel("Importar Excel",   value = "importar",  .prov_importar_tab(ns)),
      tabPanel("Inactivos",        value = "inactivos", .prov_inactivos_tab(ns))
    )
  )
}

# ── Tab UIs ───────────────────────────────────────────────────────────────────

.prov_catalogo_tab <- function(ns) {
  div(class = "p-3",
    div(class = "d-flex align-items-center gap-2 mb-3",
      div(style = "flex:1;",
        textInput(ns("cat_search"), NULL, placeholder = "Buscar nombre, alias, RFC, No. Cuenta...",
                  width = "100%")
      ),
      actionButton(ns("cat_new"), tagList(icon("plus"), " Nuevo proveedor"),
                   class = "btn btn-sm btn-outline-primary"),
      actionButton(ns("cat_inactivar_bulk"), tagList(icon("ban"), " Inactivar selección"),
                   class = "btn btn-sm btn-outline-secondary")
    ),
    DT::dataTableOutput(ns("tbl_catalogo_prov"))
  )
}

.prov_importar_tab <- function(ns) {
  div(class = "p-3",
    # Stage 1 — Upload
    div(id = ns("stage1_panel"),
      tags$h6(class = "fw-semibold", tagList(icon("upload"), " Importar desde Excel")),
      div(class = "row g-3 align-items-end",
        div(class = "col-md-5",
          fileInput(ns("excel_file"), "Archivo Excel (.xlsx / .xls)",
                    accept = c(".xlsx", ".xls"), width = "100%")
        ),
        div(class = "col-md-3",
          tags$label("Empresa", class = "form-label small fw-semibold mb-1"),
          selectInput(ns("excel_empresa"), NULL,
                      choices = c("Todos (todas las empresas)" = "TODOS",
                                  setNames(names(COMPANY_MAP), unname(COMPANY_MAP))),
                      selected = "TODOS",
                      width = "100%")
        ),
        div(class = "col-md-3",
          tags$label("Tipo", class = "form-label small fw-semibold mb-1"),
          shinyWidgets::radioGroupButtons(
            ns("excel_tipo"), NULL,
            choices = c("Activos" = "activos", "Inactivos" = "inactivos"),
            selected = "activos", size = "sm", status = "outline-primary"
          )
        )
      ),
      # Collapsed help panel
      shinyWidgets::panel(
        heading = tagList(icon("circle-question"), " Formato requerido (clic para expandir)"),
        status  = "default",
        tags$p(class = "small text-muted mb-2",
          tags$strong("Formato A (sin encabezados): "),
          "Archivo directo de BanBajío — sin fila de títulos. Columnas leídas por posición."
        ),
        tags$p(class = "small text-muted mb-2",
          tags$strong("Formato B (con encabezados): "),
          "Primera fila contiene los nombres de columna. Se detecta automáticamente."
        ),
        tags$table(class = "table table-sm table-bordered small mb-0",
          tags$thead(tags$tr(
            tags$th("Columna"), tags$th("Contenido"), tags$th("Notas")
          )),
          tags$tbody(
            tags$tr(tags$td("A"), tags$td("Nombre del proveedor"),   tags$td("Máx. 40 caracteres")),
            tags$tr(tags$td("B"), tags$td("Alias corto"),             tags$td("Máx. 15 caracteres")),
            tags$tr(tags$td("C"), tags$td("Banco"),                   tags$td("Ej: 012-BBVA BANCOMER")),
            tags$tr(tags$td("D"), tags$td("Tipo de cuenta"),          tags$td("40 = CLABE, 1 = cuenta BanBajío")),
            tags$tr(tags$td("E"), tags$td("No. de cuenta / CLABE"),   tags$td("18 dígitos (o 12 para cuentas BanBajío)")),
            tags$tr(tags$td("F"), tags$td("RFC"),                     tags$td("Opcional")),
            tags$tr(tags$td("G"), tags$td("Correo electrónico"),      tags$td("Separados por coma si son varios")),
            tags$tr(tags$td("H"), tags$td("Moneda"),                  tags$td("Opcional: Pesos / Dólar"))
          )
        )
      )
    ),

    # Stage 2/3 — Review table (shown after parse)
    uiOutput(ns("stage2_ui")),

    # Stage 4 — Conflict resolution
    uiOutput(ns("stage4_ui")),

    # Stage 5 — Final summary + commit
    uiOutput(ns("stage5_ui"))
  )
}

.prov_inactivos_tab <- function(ns) {
  div(class = "p-3",
    div(class = "d-flex align-items-center gap-2 mb-3",
      div(style = "flex:1;",
        textInput(ns("inac_search"), NULL,
                  placeholder = "Buscar nombre, alias, RFC, No. Cuenta...",
                  width = "100%")
      )
    ),
    DT::dataTableOutput(ns("tbl_inactivos_prov"))
  )
}

# ── Server ────────────────────────────────────────────────────────────────────
proveedoresServer <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      cmap <- shared$company_map()
      updateSelectInput(session, "excel_empresa",
                        choices = c("Todos (todas las empresas)" = "TODOS",
                                    setNames(names(cmap), unname(cmap))))
    })

    # ── Helpers ────────────────────────────────────────────────────────────
    get_provs <- function() {
      tryCatch(
        if (!is.null(shared$proveedores_db)) shared$proveedores_db()
        else load_proveedores(),
        error = function(e) .schema_proveedores()
      )
    }

    get_inac <- function() {
      tryCatch(
        if (!is.null(shared$proveedores_inactivos_db)) shared$proveedores_inactivos_db()
        else load_proveedores_inactivos(),
        error = function(e) .schema_proveedores()
      )
    }

    set_provs <- function(df) {
      save_proveedores(df)
      if (!is.null(shared$proveedores_db)) shared$proveedores_db(df)
    }

    set_inac <- function(df) {
      save_proveedores_inactivos(df)
      if (!is.null(shared$proveedores_inactivos_db)) shared$proveedores_inactivos_db(df)
    }

    # ── Render active catalog DT ────────────────────────────────────────────
    .render_catalogo <- function(search_txt = "") {
      provs <- get_provs()
      # Filter only active rows
      provs <- provs[!is.na(provs$activo) & provs$activo == TRUE, ]

      if (nzchar(trimws(search_txt))) {
        s <- tolower(trimws(search_txt))
        provs <- provs[
          grepl(s, tolower(provs$nombre   %||% ""), fixed = TRUE) |
          grepl(s, tolower(provs$alias    %||% ""), fixed = TRUE) |
          grepl(s, tolower(provs$rfc      %||% ""), fixed = TRUE) |
          grepl(s, tolower(provs$no_cuenta %||% ""), fixed = TRUE),
        ]
      }

      if (!nrow(provs)) {
        return(DT::datatable(
          data.frame(Info = "Sin proveedores activos."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }

      today <- Sys.Date()

      disp <- provs |> dplyr::mutate(
        Status_html = dplyr::case_when(
          !is.na(status) & status == "completo" ~
            '<span class="prov-badge-completo">\u2713 Completo</span>',
          TRUE ~
            '<span class="prov-badge-incompleto">\u26a0 Incompleto</span>'
        ),
        Activo_hasta_html = dplyr::case_when(
          is.na(activo_hasta) | activo_hasta == "" | activo_hasta == "indefinido" ~
            "Indefinido",
          tryCatch(as.Date(activo_hasta) < today, error = function(e) FALSE) ~
            paste0('<span class="prov-badge-expirado">EXPIRADO ', activo_hasta, '</span>'),
          TRUE ~ as.character(activo_hasta)
        ),
        Empresas_disp = dplyr::if_else(
          !is.na(empresas) & nzchar(empresas), empresas, Empresa %||% ""
        ),
        Acciones = paste0(
          '<button class="btn btn-xs btn-outline-secondary me-1 prov-edit-btn" data-id="', id,
          '" title="Editar">\u270f\ufe0f</button>',
          '<button class="btn btn-xs btn-outline-warning me-1 prov-inactivar-btn" data-id="', id,
          '" title="Inactivar">\U0001f6ab</button>',
          '<button class="btn btn-xs btn-outline-danger prov-del-btn" data-id="', id,
          '" title="Eliminar">\U0001f5d1</button>'
        )
      ) |> dplyr::select(
        Nombre = nombre, Alias = alias, Banco = banco, `No. Cuenta` = no_cuenta,
        RFC = rfc, Moneda = moneda, Empresas = Empresas_disp,
        `Activo hasta` = Activo_hasta_html, Status = Status_html, Acciones
      )

      DT::datatable(
        disp, escape = FALSE, rownames = FALSE,
        selection = "multiple",
        options = list(
          pageLength = 20, dom = "tip",
          scrollX = TRUE, scrollY = "420px", scrollCollapse = TRUE,
          autoWidth = FALSE,
          language = list(emptyTable = "Sin proveedores activos"),
          columnDefs = list(
            list(orderable = FALSE, targets = ncol(disp) - 1),
            list(width = "180px", targets = 0),   # Nombre
            list(width = "100px", targets = 1),   # Alias
            list(width = "130px", targets = 2),   # Banco
            list(width = "155px", targets = 3),   # No. Cuenta
            list(width = "100px", targets = 4),   # RFC
            list(width = "70px",  targets = 5),   # Moneda
            list(width = "90px",  targets = 6),   # Empresas
            list(width = "90px",  targets = 7),   # Activo hasta
            list(width = "80px",  targets = 8),   # Status
            list(width = "90px",  targets = 9)    # Acciones
          )
        )
      )
    }

    output$tbl_catalogo_prov <- DT::renderDataTable({
      .render_catalogo(input$cat_search %||% "")
    }, server = FALSE)

    observe({
      input$cat_search
      output$tbl_catalogo_prov <- DT::renderDataTable({
        .render_catalogo(input$cat_search %||% "")
      }, server = FALSE)
    })

    # ── Edit/New modal ──────────────────────────────────────────────────────
    editing_id_cat <- reactiveVal(NULL)

    .show_edit_modal <- function(row = NULL) {
      is_new  <- is.null(row)
      nom_val <- if (!is_new) row$nombre      %||% "" else ""
      ali_val <- if (!is_new) row$alias       %||% "" else ""
      rfc_val <- if (!is_new) row$rfc         %||% "" else ""
      nc_val  <- if (!is_new) row$no_cuenta   %||% "" else ""
      ban_val <- if (!is_new) row$banco       %||% "" else ""
      tc_val  <- if (!is_new) row$tipo_cuenta %||% "" else ""
      cor_val <- if (!is_new) row$correo      %||% "" else ""
      mon_val <- if (!is_new) row$moneda      %||% "Pesos" else "Pesos"
      tr_val  <- if (!is_new) row$tipo_relacion %||% "" else ""
      emp_val <- if (!is_new) {
        emps <- strsplit(row$empresas %||% row$Empresa %||% "", ",")[[1]]
        trimws(emps)
      } else character(0)

      # activo_hasta
      ah_val  <- if (!is_new) row$activo_hasta %||% "indefinido" else "indefinido"
      ah_sel  <- if (ah_val %in% c("48h","1s","1m","1a","indefinido")) ah_val
                 else if (nzchar(ah_val)) "personalizado" else "indefinido"

      showModal(modalDialog(
        title = if (is_new) tagList(icon("plus"), " Nuevo proveedor")
                else        tagList(icon("pencil"), " Editar proveedor"),
        size = "l", easyClose = TRUE,
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("prov_modal_save"),
                       tagList(icon("floppy-disk"), if (is_new) " Agregar" else " Guardar"),
                       class = "btn btn-primary")
        ),
        div(class = "row g-2",
          div(class = "col-md-8",
            tags$label("Nombre", class = "form-label small fw-semibold"),
            textInput(ns("prov_nombre"), NULL, value = nom_val, width = "100%")
          ),
          div(class = "col-md-4",
            tags$label("Alias (máx 15)", class = "form-label small fw-semibold"),
            textInput(ns("prov_alias"), NULL, value = ali_val, width = "100%")
          ),
          div(class = "col-md-4",
            tags$label("RFC", class = "form-label small fw-semibold"),
            textInput(ns("prov_rfc"), NULL, value = rfc_val, width = "100%")
          ),
          div(class = "col-md-4",
            tags$label("No. Cuenta / CLABE", class = "form-label small fw-semibold"),
            textInput(ns("prov_no_cuenta"), NULL, value = nc_val, width = "100%")
          ),
          div(class = "col-md-4",
            tags$label("Banco", class = "form-label small fw-semibold"),
            textInput(ns("prov_banco"), NULL, value = ban_val, width = "100%")
          ),
          div(class = "col-md-3",
            tags$label("Tipo Cuenta", class = "form-label small fw-semibold"),
            textInput(ns("prov_tipo_cuenta"), NULL, value = tc_val, width = "100%")
          ),
          div(class = "col-md-5",
            tags$label("Correo (separados por coma)", class = "form-label small fw-semibold"),
            textInput(ns("prov_correo"), NULL, value = cor_val, width = "100%")
          ),
          div(class = "col-md-4",
            tags$label("Moneda", class = "form-label small fw-semibold"),
            selectInput(ns("prov_moneda"), NULL,
                        choices = c("Pesos", "Dólares"), selected = mon_val, width = "100%")
          ),
          div(class = "col-md-12",
            tags$label("Tipo de Relación", class = "form-label small fw-semibold"),
            textInput(ns("prov_tipo_relacion"), NULL, value = tr_val, width = "100%")
          ),
          div(class = "col-md-12",
            tags$label("Empresas", class = "form-label small fw-semibold"),
            checkboxGroupInput(ns("prov_empresas"), NULL,
                               choices  = names(shared$company_map()),
                               selected = emp_val,
                               inline   = TRUE)
          ),
          div(class = "col-md-6",
            tags$label("Activo hasta", class = "form-label small fw-semibold"),
            selectInput(ns("prov_activo_hasta_sel"), NULL,
                        choices = c("Indefinido" = "indefinido",
                                    "48 horas"   = "48h",
                                    "1 semana"   = "1s",
                                    "1 mes"      = "1m",
                                    "1 año"      = "1a",
                                    "Personalizado" = "personalizado"),
                        selected = ah_sel, width = "100%")
          ),
          div(class = "col-md-6",
            uiOutput(ns("prov_fecha_personalizada_ui"))
          )
        )
      ))
    }

    output$prov_fecha_personalizada_ui <- renderUI({
      if (input$prov_activo_hasta_sel %||% "indefinido" == "personalizado") {
        dateInput(ns("prov_activo_hasta_date"), "Fecha personalizada",
                  value = Sys.Date() + 30, width = "100%")
      }
    })

    observeEvent(input$cat_new, {
      editing_id_cat(NULL)
      .show_edit_modal(NULL)
    }, ignoreInit = TRUE)

    # Handle edit button clicks via JS
    observeEvent(input$prov_edit_id, {
      provs <- get_provs()
      row   <- provs[provs$id == input$prov_edit_id, ]
      if (!nrow(row)) return()
      editing_id_cat(row$id[1])
      .show_edit_modal(row[1, ])
    }, ignoreInit = TRUE)

    # Handle inactivar button clicks
    observeEvent(input$prov_inactivar_id, {
      showModal(modalDialog(
        title = tagList(icon("ban"), " Inactivar proveedor"),
        size = "s", easyClose = TRUE,
        "¿Mover a inactivos? Dejará de aparecer en sugerencias y búsquedas.",
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("prov_confirm_inactivar"),
                       tagList(icon("ban"), " Inactivar"),
                       class = "btn btn-warning")
        )
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$prov_confirm_inactivar, {
      pid <- input$prov_inactivar_id
      provs <- get_provs()
      row   <- provs[provs$id == pid, ]
      if (!nrow(row)) { removeModal(); return() }

      # Remove from active
      updated_provs <- provs[provs$id != pid, ]

      # Move to inactive
      inac <- get_inac()
      row$activo <- FALSE
      row$updated_at <- as.character(Sys.time())
      inac <- dplyr::bind_rows(inac, row)

      set_provs(updated_provs)
      set_inac(inac)
      removeModal()
      showNotification("Proveedor movido a inactivos.", type = "message", duration = 2)
      output$tbl_catalogo_prov <- DT::renderDataTable({
        .render_catalogo(input$cat_search %||% "")
      }, server = FALSE)
    }, ignoreInit = TRUE)

    # Handle delete button clicks
    observeEvent(input$prov_delete_id, {
      provs <- get_provs()
      updated <- provs[provs$id != input$prov_delete_id, ]
      set_provs(updated)
      showNotification("Proveedor eliminado.", type = "message", duration = 2)
      output$tbl_catalogo_prov <- DT::renderDataTable({
        .render_catalogo(input$cat_search %||% "")
      }, server = FALSE)
    }, ignoreInit = TRUE)

    # Bulk inactivar
    observeEvent(input$cat_inactivar_bulk, {
      sel <- input$tbl_catalogo_prov_rows_selected
      if (!length(sel)) {
        showNotification("Selecciona filas primero.", type = "warning"); return()
      }
      provs <- get_provs()
      active_provs <- provs[!is.na(provs$activo) & provs$activo == TRUE, ]
      rows_to_move <- active_provs[sel, ]
      if (!nrow(rows_to_move)) return()

      showModal(modalDialog(
        title = tagList(icon("ban"), " Inactivar selección"),
        size = "s", easyClose = TRUE,
        paste0("¿Mover ", nrow(rows_to_move), " proveedor(es) a inactivos?"),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("bulk_inactivar_confirm"),
                       tagList(icon("ban"), " Inactivar"),
                       class = "btn btn-warning")
        )
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$bulk_inactivar_confirm, {
      sel <- input$tbl_catalogo_prov_rows_selected
      provs <- get_provs()
      active_provs <- provs[!is.na(provs$activo) & provs$activo == TRUE, ]
      rows_to_move <- active_provs[sel, ]
      remaining    <- active_provs[-sel, ]

      rows_to_move$activo     <- FALSE
      rows_to_move$updated_at <- as.character(Sys.time())

      inac <- dplyr::bind_rows(get_inac(), rows_to_move)
      set_provs(remaining)
      set_inac(inac)
      removeModal()
      showNotification(paste0(nrow(rows_to_move), " proveedor(es) inactivados."),
                       type = "message", duration = 2)
      output$tbl_catalogo_prov <- DT::renderDataTable({
        .render_catalogo(input$cat_search %||% "")
      }, server = FALSE)
    }, ignoreInit = TRUE)

    # Save modal
    observeEvent(input$prov_modal_save, {
      nom  <- trimws(input$prov_nombre    %||% "")
      ali  <- trimws(input$prov_alias     %||% "")
      rfc  <- trimws(input$prov_rfc       %||% "")
      nc   <- trimws(input$prov_no_cuenta %||% "")
      ban  <- trimws(input$prov_banco     %||% "")
      tc   <- trimws(input$prov_tipo_cuenta %||% "")
      cor  <- trimws(input$prov_correo    %||% "")
      mon  <- input$prov_moneda           %||% "Pesos"
      tr   <- trimws(input$prov_tipo_relacion %||% "")
      emps <- paste(input$prov_empresas %||% character(0), collapse = ",")
      ah_sel <- input$prov_activo_hasta_sel %||% "indefinido"
      ah <- if (ah_sel == "personalizado") {
        as.character(input$prov_activo_hasta_date %||% Sys.Date())
      } else {
        parse_activo_hasta(ah_sel)
      }

      if (!nzchar(nom)) {
        showNotification("El Nombre es obligatorio.", type = "warning"); return()
      }

      # Determine status
      st <- if (nzchar(nom) && nzchar(mon) && nzchar(ban) && nzchar(nc))
              "completo" else "incompleto"

      eid   <- editing_id_cat()
      provs <- get_provs()
      now   <- as.character(Sys.time())

      new_row <- tibble::tibble(
        id            = if (is.null(eid)) uuid::UUIDgenerate() else eid,
        Empresa       = if (nzchar(emps)) strsplit(emps, ",")[[1]][1] else "",
        codigo        = "",
        nombre        = nom,
        alias         = ali,
        clabe         = nc,  # keep clabe = no_cuenta for PPL compatibility
        medio_pago    = "SPI",
        rfc           = rfc,
        tipo          = "012",
        banco_destino = ban,
        activo        = TRUE,
        no_cuenta     = nc,
        banco         = ban,
        tipo_cuenta   = tc,
        correo        = cor,
        moneda        = mon,
        tipo_relacion = tr,
        empresas      = emps,
        status        = st,
        fuente        = "manual",
        upload_batch  = NA_character_,
        activo_hasta  = ah,
        created_at    = if (is.null(eid)) now else {
          existing_created <- provs$created_at[provs$id == eid]
          if (length(existing_created) && !is.na(existing_created[1]) &&
              nzchar(existing_created[1])) existing_created[1] else now
        },
        updated_at    = now
      )

      if (is.null(eid)) {
        updated <- dplyr::bind_rows(provs, new_row)
      } else {
        updated <- provs |> dplyr::rows_update(new_row, by = "id", unmatched = "ignore")
      }

      set_provs(updated)
      editing_id_cat(NULL)
      removeModal()
      showNotification(
        if (is.null(eid)) "Proveedor agregado." else "Proveedor actualizado.",
        type = "message", duration = 2)
      output$tbl_catalogo_prov <- DT::renderDataTable({
        .render_catalogo(input$cat_search %||% "")
      }, server = FALSE)
    }, ignoreInit = TRUE)

    # ==========================================================================
    # IMPORTAR EXCEL TAB
    # ==========================================================================

    # Reactive: parsed + classified upload data
    upload_data_rv         <- reactiveVal(NULL)
    conflict_decisions     <- reactiveVal(list())
    incluir_incompletos_rv <- reactiveVal(FALSE)
    incluir_duplicados_rv  <- reactiveVal(FALSE)

    observeEvent(input$incluir_incompletos, {
      incluir_incompletos_rv(isTRUE(input$incluir_incompletos))
    }, ignoreInit = TRUE)

    observeEvent(input$incluir_duplicados, {
      incluir_duplicados_rv(isTRUE(input$incluir_duplicados))
    }, ignoreInit = TRUE)

    # Column mapping helper — matches column name against a list of regex patterns.
    .map_excel_col <- function(nms, patterns) {
      nms_lo <- tolower(trimws(nms))
      for (p in patterns) {
        idx <- which(grepl(p, nms_lo, ignore.case = TRUE, perl = TRUE))[1]
        if (!is.na(idx)) return(nms[idx])
      }
      NULL
    }

    # Smart Excel reader — handles two formats:
    #
    # Format A (no header): row 1 is data. readxl col_names=FALSE names all
    #   columns "...1","...2",... We assign standard positional names directly.
    #   Detection: col 5 of row 1 looks like an account number (≥12 digits).
    #
    # Format B (has header): row 1 has Spanish column labels. We re-read using
    #   that row as the header so downstream col_map patterns can match.
    #   Detection: col 5 of row 1 does NOT look like an account number.
    #
    # After reading, strips blank columns that readxl auto-names "...N" (only
    # applicable to Format B where col_names=TRUE is used).
    .read_excel_smart <- function(path) {

      # Read row 1 only (with col_names=FALSE) to decide format
      probe <- tryCatch(
        suppressMessages(
          readxl::read_excel(path, col_names = FALSE, col_types = "text", n_max = 1L)
        ),
        error = function(e) { message("[PROV_IMPORT] probe read error: ", e$message); NULL }
      )
      if (is.null(probe) || ncol(probe) == 0) return(NULL)

      # Check col 5 of row 1: if it looks like an account number → Format A
      c5 <- trimws(as.character(probe[[min(5L, ncol(probe))]][1] %||% ""))
      is_format_a <- grepl("^[0-9]{10,18}$", c5)

      if (is_format_a) {
        # ── Format A: positional, no header ──────────────────────────────────
        message("[PROV_IMPORT] Format A detected (no header row)")
        df <- tryCatch(
          suppressMessages(
            readxl::read_excel(path, col_names = FALSE, col_types = "text")
          ),
          error = function(e) { message("[PROV_IMPORT] read error: ", e$message); NULL }
        )
        if (is.null(df) || !nrow(df)) return(NULL)

        # Assign standard names by position — DO NOT strip (all cols are ...N)
        pos_names <- c("nombre", "alias", "banco", "tipo_cuenta",
                       "no_cuenta", "rfc", "correo", "moneda", "tipo_relacion")
        n_assign  <- min(ncol(df), length(pos_names))
        names(df)[seq_len(n_assign)] <- pos_names[seq_len(n_assign)]
        return(df)
      }

      # ── Format B: has header row ──────────────────────────────────────────
      message("[PROV_IMPORT] Format B detected (header row present)")
      df <- tryCatch(
        suppressMessages(
          readxl::read_excel(path, col_names = TRUE, col_types = "text")
        ),
        error = function(e) { message("[PROV_IMPORT] read error: ", e$message); NULL }
      )
      if (is.null(df) || !nrow(df)) return(NULL)

      # Strip auto-named blank columns ("...1", "...8", etc.) — only safe here
      df <- df[, !grepl("^\\.\\.\\.\\d+$", names(df)), drop = FALSE]
      df
    }

    # Parse and classify uploaded Excel
    observeEvent(input$excel_file, {
      req(input$excel_file)
      tryCatch({
        raw <- .read_excel_smart(input$excel_file$datapath)
        if (is.null(raw) || !nrow(raw)) {
          showNotification("El archivo está vacío o no pudo leerse.", type = "error")
          return()
        }
        nms <- names(raw)

        # Map columns — patterns cover both positional names (Format A)
        # and verbose Spanish header labels (Format B).
        col_map <- list(
          nombre        = .map_excel_col(nms, c(
                            "^nombre$", "nombre.{0,5}beneficiario",
                            "beneficiario", "nombre")),
          alias         = .map_excel_col(nms, c(
                            "^alias$", "alias.{0,5}beneficiario", "alias")),
          banco         = .map_excel_col(nms, c("^banco$", "banco")),
          tipo_cuenta   = .map_excel_col(nms, c(
                            "^tipo_cuenta$", "tipo.{0,10}cuenta", "tipo_cuenta")),
          no_cuenta     = .map_excel_col(nms, c(
                            "^no_cuenta$", "no\\.?.{0,5}cuenta",
                            "no_cuenta", "clabe")),
          rfc           = .map_excel_col(nms, c("^rfc$", "rfc")),
          correo        = .map_excel_col(nms, c(
                            "^correo$", "correo.{0,20}electr",
                            "correo", "email")),
          moneda        = .map_excel_col(nms, c("^moneda$", "moneda")),
          tipo_relacion = .map_excel_col(nms, c(
                            "^tipo_relacion$", "tipo.{0,10}relaci", "tipo_relacion"))
        )

        # Log mapping result to console for debugging
        message("[PROV_IMPORT] col_map: ",
                paste(names(col_map), "=",
                      vapply(col_map, function(x) x %||% "NULL", character(1)),
                      collapse = " | "))

        emp_input   <- input$excel_empresa %||% "TODOS"
        empresa_sel <- if (emp_input == "TODOS") "" else emp_input

        # Extract and clean column values
        get_col <- function(field) {
          col <- col_map[[field]]
          if (is.null(col)) return(rep(NA_character_, nrow(raw)))
          vals <- trimws(as.character(raw[[col]]))
          dplyr::if_else(vals == "NA" | vals == "", NA_character_, vals)
        }

        rows <- tibble::tibble(
          .row_num      = seq_len(nrow(raw)),
          nombre        = get_col("nombre"),
          alias         = get_col("alias"),
          banco         = get_col("banco"),
          tipo_cuenta   = get_col("tipo_cuenta"),
          no_cuenta     = get_col("no_cuenta"),
          rfc           = get_col("rfc"),
          correo        = get_col("correo"),
          moneda        = get_col("moneda"),
          tipo_relacion = get_col("tipo_relacion"),
          empresas      = empresa_sel,
          Empresa       = empresa_sel
        )

        # Drop entirely blank rows (trailing empty rows from Excel)
        rows <- rows[!(is.na(rows$nombre) & is.na(rows$no_cuenta) &
                       is.na(rows$banco)  & is.na(rows$rfc)), , drop = FALSE]

        message("[PROV_IMPORT] nrow=", nrow(rows),
                " | nombre[1]=",    rows$nombre[1]    %||% "NULL",
                " | alias[1]=",     rows$alias[1]     %||% "NULL",
                " | banco[1]=",     rows$banco[1]     %||% "NULL",
                " | no_cuenta[1]=", rows$no_cuenta[1] %||% "NULL")

        if (!nrow(rows)) {
          showNotification(
            "No se encontraron filas con datos. Verifica el formato del archivo.",
            type = "warning")
          return()
        }

        # Classify rows
        # Dedup key: no_cuenta alone (verified: no file has same no_cuenta at
        # two different banks). First occurrence is kept; subsequent ones are
        # marked duplicado_interno regardless of how many times they appear.
        provs_activos   <- get_provs()
        provs_inactivos <- get_inac()
        nc_active   <- trimws(provs_activos$no_cuenta   %||% provs_activos$clabe   %||% "")
        nc_inactive <- trimws(provs_inactivos$no_cuenta %||% provs_inactivos$clabe %||% "")
        # Remove empty strings from S3 sets (avoid false conflict matches)
        nc_active   <- nc_active[nzchar(nc_active)]
        nc_inactive <- nc_inactive[nzchar(nc_inactive)]

        # Pass 1: classify each row ignoring internal duplicates
        rows <- rows |> dplyr::mutate(
          .nc_t = trimws(no_cuenta),
          .clasificacion = dplyr::case_when(
            is.na(nombre)    | !nzchar(trimws(nombre))    ~ "incompleto",
            is.na(no_cuenta) | !nzchar(trimws(no_cuenta)) ~ "incompleto",
            .nc_t %in% nc_active                          ~ "conflicto_s3_activo",
            .nc_t %in% nc_inactive                        ~ "conflicto_s3_inactivo",
            is.na(banco)     | !nzchar(trimws(banco))     ~ "incompleto",
            TRUE                                          ~ "listo"
          )
        ) |> dplyr::select(-.nc_t)

        # Pass 2: among non-incomplete, non-conflict rows, keep only the FIRST
        # occurrence of each no_cuenta; mark all subsequent as duplicado_interno.
        seen_nc <- character(0)
        for (ii in seq_len(nrow(rows))) {
          cl <- rows$.clasificacion[ii]
          if (cl %in% c("incompleto", "conflicto_s3_activo", "conflicto_s3_inactivo")) next
          nc_ii <- trimws(rows$no_cuenta[ii])
          if (nc_ii %in% seen_nc) {
            rows$.clasificacion[ii] <- "duplicado_interno"
          } else {
            seen_nc <- c(seen_nc, nc_ii)
          }
        }

        message("[PROV_IMPORT] classification: listo=",
                sum(rows$.clasificacion == "listo"),
                " incompleto=", sum(rows$.clasificacion == "incompleto"),
                " duplicado=",  sum(rows$.clasificacion == "duplicado_interno"),
                " conflicto=",  sum(grepl("^conflicto", rows$.clasificacion)))

        upload_data_rv(list(rows = rows, empresa = empresa_sel,
                            tipo = input$excel_tipo %||% "activos"))
        conflict_decisions(list())

      }, error = function(e) {
        showNotification(paste("Error al leer el archivo:", e$message), type = "error")
      })
    }, ignoreInit = TRUE)

    # Stage 2/3 UI
    output$stage2_ui <- renderUI({
      ud <- upload_data_rv()
      if (is.null(ud)) return(NULL)
      rows <- ud$rows

      n_listo  <- sum(rows$.clasificacion == "listo")
      n_incom  <- sum(rows$.clasificacion == "incompleto")
      n_dup    <- sum(rows$.clasificacion == "duplicado_interno")
      n_conf   <- sum(grepl("^conflicto", rows$.clasificacion))

      tagList(
        tags$hr(),
        tags$h6(class = "fw-semibold", tagList(icon("table"), " Revisión de datos")),
        # Filter tabs
        div(class = "d-flex gap-1 mb-2",
          actionButton(ns("filter_todos"),     "Todos",
                       class = "btn btn-sm btn-outline-secondary"),
          actionButton(ns("filter_listo"),
                       paste0("\u2705 Listos (", n_listo, ")"),
                       class = "btn btn-sm btn-outline-success"),
          actionButton(ns("filter_incompleto"),
                       paste0("\u26a0\ufe0f Incompletos (", n_incom, ")"),
                       class = "btn btn-sm btn-outline-warning"),
          actionButton(ns("filter_duplicado"),
                       paste0("\U0001f534 Duplicados (", n_dup, ")"),
                       class = "btn btn-sm btn-outline-danger"),
          actionButton(ns("filter_conflicto"),
                       paste0("\U0001f7e1 Conflictos (", n_conf, ")"),
                       class = "btn btn-sm btn-outline-warning")
        ),
        div(style = "position: relative;",
          DT::dataTableOutput(ns("tbl_upload_review")),
          div(id = ns("tbl_review_hscroll_anchor"), style = "height: 1px;")
        )
      )
    })

    # Active filter for upload review
    upload_filter_rv <- reactiveVal("todos")
    observeEvent(input$filter_todos,      { upload_filter_rv("todos") },      ignoreInit = TRUE)
    observeEvent(input$filter_listo,      { upload_filter_rv("listo") },      ignoreInit = TRUE)
    observeEvent(input$filter_incompleto, { upload_filter_rv("incompleto") }, ignoreInit = TRUE)
    observeEvent(input$filter_duplicado,  { upload_filter_rv("duplicado") },  ignoreInit = TRUE)
    observeEvent(input$filter_conflicto,  { upload_filter_rv("conflicto") },  ignoreInit = TRUE)

    output$tbl_upload_review <- DT::renderDataTable({
      ud <- upload_data_rv()
      if (is.null(ud)) return(DT::datatable(data.frame()))

      rows <- ud$rows
      filt <- upload_filter_rv()
      if (filt != "todos") {
        if (filt == "conflicto") {
          rows <- rows[grepl("^conflicto", rows$.clasificacion), ]
        } else {
          rows <- rows[rows$.clasificacion == filt, ]
        }
      }

      if (!nrow(rows)) {
        return(DT::datatable(
          data.frame(Info = "Sin filas en esta categoría."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }

      disp <- rows |> dplyr::mutate(
        Status = dplyr::case_when(
          .clasificacion == "listo" ~
            '<span class="badge bg-success">\u2705 Listo</span>',
          .clasificacion == "incompleto" ~
            '<span class="badge bg-warning text-dark">\u26a0 Incompleto</span>',
          .clasificacion == "duplicado_interno" ~
            paste0('<span class="badge bg-danger">\U0001f534 Dup. fila ',
                   .row_num, '</span>'),
          grepl("^conflicto", .clasificacion) ~
            '<span class="badge bg-warning text-dark">\U0001f7e1 Conflicto S3</span>',
          TRUE ~ .clasificacion
        ),
        Quitar = paste0('<button class="btn btn-xs btn-outline-danger prov-upload-del-btn"',
                        ' data-rownum="', .row_num, '" title="Quitar">\U0001f5d1</button>')
      ) |> dplyr::select(
        Status, Nombre = nombre, Alias = alias, RFC = rfc,
        `No. Cuenta` = no_cuenta, Banco = banco, Moneda = moneda, Quitar
      )

      DT::datatable(
        disp, escape = FALSE, rownames = FALSE,
        options = list(
          pageLength = 20,
          dom        = "tip",
          scrollX    = TRUE,
          scrollY    = "340px",
          scrollCollapse = TRUE,
          autoWidth  = FALSE,
          fixedHeader = FALSE,
          columnDefs = list(
            list(orderable = FALSE, targets = ncol(disp) - 1),
            list(width = "90px",  targets = 0),
            list(width = "180px", targets = 1),
            list(width = "100px", targets = 2),
            list(width = "115px", targets = 3),
            list(width = "160px", targets = 4),
            list(width = "130px", targets = 5),
            list(width = "80px",  targets = 6),
            list(width = "44px",  targets = 7)
          )
        )
      )
    }, server = TRUE)

    # Handle delete from upload preview
    observeEvent(input$prov_upload_del_rownum, {
      ud <- upload_data_rv()
      if (is.null(ud)) return()
      rows <- ud[["rows"]]
      rows <- rows[rows$.row_num != as.integer(input$prov_upload_del_rownum), ]
      upload_data_rv(list(rows = rows, empresa = ud$empresa, tipo = ud$tipo))
    }, ignoreInit = TRUE)

    # Stage 4 — Conflict resolution
    conflict_idx_rv <- reactiveVal(1L)

    output$stage4_ui <- renderUI({
      ud <- upload_data_rv()
      if (is.null(ud)) return(NULL)
      rows    <- ud$rows
      conf    <- rows[grepl("^conflicto", rows$.clasificacion), ]
      if (!nrow(conf)) return(NULL)

      decisions <- conflict_decisions()
      n_resolved <- length(decisions)
      n_total    <- nrow(conf)

      idx <- conflict_idx_rv()
      if (idx > n_total) idx <- n_total

      conf_row  <- conf[idx, ]
      nc        <- trimws(conf_row$no_cuenta)

      # Resolve account number: use no_cuenta when present, fall back to clabe.
      # %||% only handles NULL, not NA — use coalesce for NA-safe fallback.
      .nc_key <- function(df) {
        trimws(dplyr::coalesce(
          dplyr::na_if(as.character(df$no_cuenta), "NA"),
          dplyr::na_if(as.character(df$clabe),     "NA")
        ))
      }
      # Find S3 counterpart
      if (conf_row$.clasificacion == "conflicto_s3_activo") {
        provs_now <- get_provs()
        s3_row    <- provs_now[.nc_key(provs_now) == nc, , drop = FALSE][1, ]
        src_lbl   <- "EN S3 (activo)"
      } else {
        inac_now <- get_inac()
        s3_row   <- inac_now[.nc_key(inac_now) == nc, , drop = FALSE][1, ]
        src_lbl  <- "EN S3 (inactivo)"
      }

      tagList(
        tags$hr(),
        tags$h6(class = "fw-semibold", tagList(icon("triangle-exclamation"), " Resolver conflictos")),
        tags$p(class = "small text-muted",
               paste0(n_resolved, " de ", n_total, " conflictos resueltos")),

        div(class = "prov-conflict-card",
          tags$strong(paste0("Conflicto: No. Cuenta ", nc)),
          tags$hr(class = "my-2"),
          div(class = "d-flex gap-3",
            div(class = "prov-conflict-side",
              tags$strong("SUBIENDO"),
              tags$br(),
              paste0("Nombre: ", conf_row$nombre %||% ""), tags$br(),
              paste0("RFC: ", conf_row$rfc %||% ""), tags$br(),
              paste0("Banco: ", conf_row$banco %||% "")
            ),
            div(class = "prov-conflict-side",
              tags$strong(src_lbl),
              tags$br(),
              paste0("Nombre: ", if (!is.null(s3_row) && nrow(s3_row))
                dplyr::coalesce(s3_row$nombre[1], "\u2014") else "N/A"), tags$br(),
              paste0("RFC: ", if (!is.null(s3_row) && nrow(s3_row))
                dplyr::coalesce(s3_row$rfc[1], "\u2014") else "N/A"), tags$br(),
              paste0("Banco: ", if (!is.null(s3_row) && nrow(s3_row))
                dplyr::coalesce(s3_row$banco[1], "\u2014") else "N/A")
            )
          ),
          tags$hr(class = "my-2"),
          div(class = "d-flex gap-2 flex-wrap",
            actionButton(ns("conf_conservar"),        "Conservar S3",
                         class = "btn btn-sm btn-outline-secondary"),
            actionButton(ns("conf_reemplazar"),       "Reemplazar con nuevo",
                         class = "btn btn-sm btn-outline-primary"),
            actionButton(ns("conf_omitir"),           "Omitir ambos",
                         class = "btn btn-sm btn-outline-danger"),
            actionButton(ns("conf_reemplazar_todos"), "Reemplazar todos los restantes \u2192",
                         class = "btn btn-sm btn-warning",
                         title = "Marca todos los conflictos pendientes como Reemplazar")
          )
        )
      )
    })

    .advance_conflict <- function(decision) {
      ud   <- upload_data_rv()
      conf <- ud$rows[grepl("^conflicto", ud$rows$.clasificacion), ]
      idx  <- conflict_idx_rv()
      if (idx > nrow(conf)) return()
      nc   <- trimws(conf[idx, ]$no_cuenta)
      dec  <- conflict_decisions()
      dec[[nc]] <- decision
      conflict_decisions(dec)
      conflict_idx_rv(idx + 1L)
    }

    observeEvent(input$conf_conservar,  { .advance_conflict("conservar") },  ignoreInit = TRUE)
    observeEvent(input$conf_reemplazar, { .advance_conflict("reemplazar") }, ignoreInit = TRUE)
    observeEvent(input$conf_omitir,     { .advance_conflict("omitir") },     ignoreInit = TRUE)

    # Bulk-resolve all remaining unresolved conflicts as "reemplazar"
    observeEvent(input$conf_reemplazar_todos, {
      ud <- upload_data_rv()
      if (is.null(ud)) return()
      conf <- ud$rows[grepl("^conflicto", ud$rows$.clasificacion), ]
      if (!nrow(conf)) return()
      dec <- conflict_decisions()
      for (ii in seq_len(nrow(conf))) {
        nc <- trimws(conf$no_cuenta[ii])
        if (is.null(dec[[nc]])) dec[[nc]] <- "reemplazar"
      }
      conflict_decisions(dec)
      conflict_idx_rv(nrow(conf) + 1L)
      n_bulk <- sum(vapply(names(dec), function(k) dec[[k]] == "reemplazar", logical(1)))
      showNotification(
        paste0(n_bulk, " conflicto(s) marcados como Reemplazar. Haz clic en 'Importar' para confirmar."),
        type = "message", duration = 4)
    }, ignoreInit = TRUE)

    # Stage 5 — Final summary
    output$stage5_ui <- renderUI({
      ud <- upload_data_rv()
      if (is.null(ud)) return(NULL)
      rows <- ud$rows

      n_listo  <- sum(rows$.clasificacion == "listo")
      n_incom  <- sum(rows$.clasificacion == "incompleto")
      n_dup    <- sum(rows$.clasificacion == "duplicado_interno")
      n_conf   <- sum(grepl("^conflicto", rows$.clasificacion))
      dec      <- conflict_decisions()

      # conf_pending = unique no_cuenta values among conflict rows with no decision yet
      conf_rows_nc <- unique(trimws(rows$no_cuenta[grepl("^conflicto", rows$.clasificacion)]))
      conf_pending <- sum(vapply(conf_rows_nc, function(nc) is.null(dec[[nc]]), logical(1)))

      # Count of conflict rows whose decision is "reemplazar" (deduplicated)
      n_reemplazar <- length(unique(trimws(
        conf_rows_nc[vapply(conf_rows_nc, function(nc) identical(dec[[nc]], "reemplazar"), logical(1))]
      )))

      # Total that will be imported if user clicks now
      n_to_import <- n_listo +
        (if (incluir_incompletos_rv()) n_incom else 0L) +
        (if (incluir_duplicados_rv())  n_dup   else 0L) +
        n_reemplazar

      n_resolved <- length(dec)

      tagList(
        tags$hr(),
        tags$h6(class = "fw-semibold", tagList(icon("check-circle"), " Resumen final")),
        tags$p(class = "small text-muted fst-italic mb-2",
          icon("circle-info"),
          " Las decisiones de conflicto se aplican al hacer clic en ",
          tags$strong("Importar"), " \u2014 nada se guarda hasta entonces."
        ),
        div(class = "d-flex flex-column gap-1 mb-3",
          div(paste0("\u2705 ", n_listo, " proveedores listos")),
          if (n_incom > 0)
            div(paste0("\u26a0\ufe0f ", n_incom,
                       " incompletos (serán omitidos a menos que marques la opción)"))
          else NULL,
          if (n_dup > 0)
            div(paste0("\U0001f534 ", n_dup,
                       " duplicados (serán omitidos a menos que marques la opción)"))
          else NULL,
          if (n_conf > 0)
            div(paste0("\U0001f7e1 ", n_resolved, " de ", n_conf, " conflictos resueltos"))
          else NULL
        ),
        if (n_incom > 0)
          checkboxInput(ns("incluir_incompletos"), "Incluir incompletos de todas formas",
                        value = incluir_incompletos_rv())
        else NULL,
        if (n_dup > 0)
          checkboxInput(ns("incluir_duplicados"),
                        paste0("Incluir duplicados de todas formas (", n_dup, " filas)"),
                        value = incluir_duplicados_rv())
        else NULL,
        if (conf_pending > 0)
          div(class = "alert alert-warning small mt-2",
              paste0(conf_pending, " conflicto(s) sin resolver. Resuélvelos antes de importar."))
        else NULL,
        div(class = "d-flex gap-2 mt-2",
          actionButton(ns("cancelar_upload"), "Cancelar",
                       class = "btn btn-sm btn-outline-secondary"),
          if (conf_pending == 0)
            actionButton(ns("confirmar_upload"),
                         tagList(icon("file-import"),
                                 paste0(" Importar ", n_to_import, " proveedores \u2192")),
                         class = "btn btn-sm btn-primary")
          else NULL
        )
      )
    })

    observeEvent(input$cancelar_upload, {
      upload_data_rv(NULL)
      conflict_decisions(list())
      conflict_idx_rv(1L)
      incluir_incompletos_rv(FALSE)
      incluir_duplicados_rv(FALSE)
      shinyjs::reset(ns("excel_file"))
    }, ignoreInit = TRUE)

    observeEvent(input$confirmar_upload, {
      ud   <- upload_data_rv()
      if (is.null(ud)) return()
      rows <- ud$rows
      dec  <- conflict_decisions()
      incluir_incom <- incluir_incompletos_rv()
      incluir_dups  <- incluir_duplicados_rv()

      # ── Select rows to import ────────────────────────────────────────────
      # 1. All "listo" rows
      listo_idx <- which(rows$.clasificacion == "listo")

      # 2. Incomplete rows if checkbox ticked
      incom_idx <- if (incluir_incom) which(rows$.clasificacion == "incompleto") else integer(0)

      # 3. Duplicate rows if checkbox ticked — keep first occurrence per no_cuenta
      dup_idx <- integer(0)
      if (incluir_dups) {
        seen_dup_nc <- character(0)
        for (ii in which(rows$.clasificacion == "duplicado_interno")) {
          nc_ii <- trimws(rows$no_cuenta[ii])
          if (!nc_ii %in% seen_dup_nc) {
            dup_idx     <- c(dup_idx, ii)
            seen_dup_nc <- c(seen_dup_nc, nc_ii)
          }
        }
      }

      # 4. Conflict rows marked "reemplazar" — keep first occurrence per no_cuenta
      seen_conf_nc <- character(0)
      conf_idx     <- integer(0)
      for (ii in which(grepl("^conflicto", rows$.clasificacion))) {
        nc_ii <- trimws(rows$no_cuenta[ii])
        d     <- dec[[nc_ii]] %||% "conservar"
        if (d == "reemplazar" && !nc_ii %in% seen_conf_nc) {
          conf_idx     <- c(conf_idx, ii)
          seen_conf_nc <- c(seen_conf_nc, nc_ii)
        }
      }

      to_import <- rows[sort(unique(c(listo_idx, incom_idx, dup_idx, conf_idx))), , drop = FALSE]

      if (!nrow(to_import)) {
        showNotification(
          "Nada que importar. Marca 'Reemplazar' en los conflictos o revisa los filtros.",
          type = "warning")
        return()
      }

      batch <- uuid::UUIDgenerate()
      now   <- as.character(Sys.time())

      new_rows <- to_import |> dplyr::transmute(
        id            = vapply(seq_len(nrow(to_import)), function(i) uuid::UUIDgenerate(), character(1)),
        Empresa       = empresas,
        codigo        = NA_character_,
        nombre        = nombre,
        alias         = alias %||% NA_character_,
        clabe         = no_cuenta,  # PPL compatibility
        medio_pago    = "SPI",
        rfc           = rfc %||% NA_character_,
        tipo          = "012",
        banco_destino = banco %||% NA_character_,
        activo        = TRUE,
        no_cuenta     = no_cuenta,
        banco         = banco %||% NA_character_,
        tipo_cuenta   = tipo_cuenta %||% NA_character_,
        correo        = correo %||% NA_character_,
        moneda        = moneda %||% NA_character_,
        tipo_relacion = tipo_relacion %||% NA_character_,
        empresas      = empresas,
        status        = dplyr::if_else(
          !is.na(nombre) & !is.na(moneda) & !is.na(banco) & !is.na(no_cuenta) &
          nzchar(trimws(nombre)) & nzchar(trimws(moneda)) & nzchar(trimws(banco)) &
          nzchar(trimws(no_cuenta)), "completo", "incompleto"),
        fuente        = "excel",
        upload_batch  = batch,
        activo_hasta  = "indefinido",
        created_at    = now,
        updated_at    = now
      )

      if (ud$tipo == "activos") {
        current <- get_provs()
        # Remove replaced conflicts
        reemplazados <- names(dec)[vapply(names(dec), function(nc) dec[[nc]] == "reemplazar", logical(1))]
        if (length(reemplazados)) {
          current <- current[!trimws(current$no_cuenta %||% current$clabe) %in% reemplazados, ]
        }
        updated <- dplyr::bind_rows(current, new_rows)
        set_provs(updated)
      } else {
        current <- get_inac()
        reemplazados <- names(dec)[vapply(names(dec), function(nc) dec[[nc]] == "reemplazar", logical(1))]
        if (length(reemplazados)) {
          current <- current[!trimws(current$no_cuenta %||% current$clabe) %in% reemplazados, ]
        }
        updated <- dplyr::bind_rows(current, new_rows)
        set_inac(updated)
      }

      n_imp <- nrow(new_rows)
      upload_data_rv(NULL)
      conflict_decisions(list())
      conflict_idx_rv(1L)
      showNotification(paste0(n_imp, " proveedores importados."),
                       type = "message", duration = 4)
      output$tbl_catalogo_prov <- DT::renderDataTable({
        .render_catalogo(input$cat_search %||% "")
      }, server = FALSE)
    }, ignoreInit = TRUE)

    # ==========================================================================
    # INACTIVOS TAB
    # ==========================================================================
    reactivar_id_rv <- reactiveVal(NULL)

    .render_inactivos <- function(search_txt = "") {
      inac <- get_inac()
      inac <- inac[!is.na(inac$activo) & inac$activo == FALSE, ]

      if (nzchar(trimws(search_txt))) {
        s <- tolower(trimws(search_txt))
        inac <- inac[
          grepl(s, tolower(inac$nombre   %||% ""), fixed = TRUE) |
          grepl(s, tolower(inac$alias    %||% ""), fixed = TRUE) |
          grepl(s, tolower(inac$rfc      %||% ""), fixed = TRUE) |
          grepl(s, tolower(inac$no_cuenta %||% ""), fixed = TRUE),
        ]
      }

      if (!nrow(inac)) {
        return(DT::datatable(
          data.frame(Info = "Sin proveedores inactivos."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }

      disp <- inac |> dplyr::mutate(
        Acciones = paste0(
          '<button class="btn btn-xs btn-outline-success me-1 prov-reactivar-btn" data-id="', id,
          '" title="Reactivar">\U0001f504</button>',
          '<button class="btn btn-xs btn-outline-danger prov-del-inac-btn" data-id="', id,
          '" title="Eliminar permanentemente">\U0001f5d1</button>'
        )
      ) |> dplyr::select(
        Nombre = nombre, Alias = alias, Banco = banco, `No. Cuenta` = no_cuenta,
        RFC = rfc, Moneda = moneda, Acciones
      )

      DT::datatable(
        disp, escape = FALSE, rownames = FALSE,
        options = list(
          pageLength = 20, dom = "tip", scrollX = TRUE,
          columnDefs = list(list(orderable = FALSE, targets = ncol(disp) - 1))
        )
      )
    }

    output$tbl_inactivos_prov <- DT::renderDataTable({
      .render_inactivos(input$inac_search %||% "")
    }, server = FALSE)

    observe({
      input$inac_search
      output$tbl_inactivos_prov <- DT::renderDataTable({
        .render_inactivos(input$inac_search %||% "")
      }, server = FALSE)
    })

    observeEvent(input$prov_reactivar_id, {
      reactivar_id_rv(input$prov_reactivar_id)
      showModal(modalDialog(
        title = tagList(icon("rotate"), " Reactivar proveedor"),
        size = "s", easyClose = TRUE,
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("prov_confirm_reactivar"),
                       tagList(icon("rotate"), " Reactivar"),
                       class = "btn btn-success")
        ),
        tags$label("Activo por", class = "form-label fw-semibold"),
        selectInput(ns("prov_reactivar_hasta"), NULL,
                    choices = c("Indefinido" = "indefinido",
                                "48 horas"   = "48h",
                                "1 semana"   = "1s",
                                "1 mes"      = "1m",
                                "1 año"      = "1a"),
                    selected = "indefinido", width = "100%")
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$prov_confirm_reactivar, {
      pid <- reactivar_id_rv()
      inac <- get_inac()
      row  <- inac[inac$id == pid, ]
      if (!nrow(row)) { removeModal(); return() }

      ah  <- parse_activo_hasta(input$prov_reactivar_hasta %||% "indefinido")
      row$activo       <- TRUE
      row$activo_hasta <- ah
      row$updated_at   <- as.character(Sys.time())

      updated_inac <- inac[inac$id != pid, ]
      set_inac(updated_inac)

      current_provs <- get_provs()
      updated_provs <- dplyr::bind_rows(current_provs, row)
      set_provs(updated_provs)

      reactivar_id_rv(NULL)
      removeModal()
      showNotification("Proveedor reactivado.", type = "message", duration = 2)
      output$tbl_inactivos_prov <- DT::renderDataTable({
        .render_inactivos(input$inac_search %||% "")
      }, server = FALSE)
      output$tbl_catalogo_prov <- DT::renderDataTable({
        .render_catalogo(input$cat_search %||% "")
      }, server = FALSE)
    }, ignoreInit = TRUE)

    observeEvent(input$prov_del_inac_id, {
      inac <- get_inac()
      updated <- inac[inac$id != input$prov_del_inac_id, ]
      set_inac(updated)
      showNotification("Proveedor eliminado permanentemente.", type = "message", duration = 2)
      output$tbl_inactivos_prov <- DT::renderDataTable({
        .render_inactivos(input$inac_search %||% "")
      }, server = FALSE)
    }, ignoreInit = TRUE)

  }) # end moduleServer
}
