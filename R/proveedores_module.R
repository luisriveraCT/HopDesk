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
    ",
      ns("prov_edit_id"), ns("prov_inactivar_id"), ns("prov_delete_id"),
      ns("prov_reactivar_id"), ns("prov_del_inac_id")
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
                      # The session's companies are added by the observe() in proveedoresServer.
                      choices = c("Todos (todas las empresas)" = "TODOS"),
                      selected = "TODOS",
                      width = "100%")
        ),
        div(class = "col-md-2",
          tags$label("Tipo", class = "form-label small fw-semibold mb-1"),
          shinyWidgets::radioGroupButtons(
            ns("excel_tipo"), NULL,
            choices = c("Activos" = "activos", "Inactivos" = "inactivos"),
            selected = "activos", size = "sm", status = "outline-primary"
          )
        ),
        # Shown once a file without a Moneda column is read: the currency of
        # its new providers (never guessed from the account).
        shinyjs::hidden(div(id = ns("excel_moneda_wrap"), class = "col-md-2",
          tags$label(tr("tablas.proveedores.importar.moneda_archivo"), class = "form-label small fw-semibold mb-1"),
          selectInput(ns("excel_moneda"), NULL, choices = c("MXN", "USD"), selected = "MXN", width = "100%")
        ))
      ),
      div(class = "mb-2",
        downloadButton(ns("descargar_plantilla"), tr("tablas.proveedores.importar.plantilla"),
                       class = "btn btn-sm btn-outline-secondary"),
        tags$span(class = "small text-muted ms-2", tr("tablas.proveedores.importar.plantilla_ayuda"))
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

    # Stage 2/3 — Review table (shown after parse). Static, not in a
    # renderUI: the table keeps its chip, sort and scroll while rows change.
    shinyjs::hidden(div(id = ns("revision_wrap"),
      tags$hr(),
      tags$h6(class = "fw-semibold", tagList(icon("table"), " Revisión de datos")),
      uiOutput(ns("revision_mapeo")),
      hd_table_ui(ns("revision"), alto = "auto")
    )),

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
        else load_proveedores(client_id = shared$effective_client_id()),
        error = function(e) .schema_proveedores()
      )
    }

    get_inac <- function() {
      tryCatch(
        if (!is.null(shared$proveedores_inactivos_db)) shared$proveedores_inactivos_db()
        else load_proveedores_inactivos(client_id = shared$effective_client_id()),
        error = function(e) .schema_proveedores()
      )
    }

    # The catalog's alias and CLABE are where the PPL sends money, so every
    # action that changes it checks can_manage_proveedores on the server: the
    # panel being reachable is not a permission. Each write observer starts
    # with .puede_editar(); set_provs()/set_inac() refuse as a last line.
    .puede_editar <- function() puede_o_avisa(shared, "can_manage_proveedores", session)
    .exigir_permiso <- function() {
      if (!puede(isolate(shared$usuario()), "can_manage_proveedores"))
        stop("[proveedores] can_manage_proveedores required to save the catalog", call. = FALSE)
    }

    set_provs <- function(df) {
      .exigir_permiso()
      save_proveedores(df, client_id = shared$effective_client_id())
      bump_sync_version("proveedores_db")
      if (!is.null(shared$proveedores_db)) shared$proveedores_db(df)
    }

    set_inac <- function(df) {
      .exigir_permiso()
      save_proveedores_inactivos(df, client_id = shared$effective_client_id())
      bump_sync_version("proveedores_inactivos_db")
      if (!is.null(shared$proveedores_inactivos_db)) shared$proveedores_inactivos_db(df)
    }

    # ── Render active catalog DT ────────────────────────────────────────────
    # ids of the rows the catalog table currently shows, in table order. The
    # search filters in R, so DT's _rows_selected are positions in the
    # filtered table: map them through this, never into get_provs().
    catalogo_ids <- reactiveVal(character(0))

    .render_catalogo <- function(search_txt = "") {
      provs <- get_provs()
      # Filter only active rows
      provs <- provs[!is.na(provs$activo) & provs$activo == TRUE, ]

      if (nzchar(trimws(search_txt))) {
        s <- strip_accents(tolower(trimws(search_txt)))
        provs <- provs[
          grepl(s, strip_accents(tolower(provs$nombre    %||% "")), fixed = TRUE) |
          grepl(s, strip_accents(tolower(provs$alias     %||% "")), fixed = TRUE) |
          grepl(s, strip_accents(tolower(provs$rfc       %||% "")), fixed = TRUE) |
          grepl(s, strip_accents(tolower(provs$no_cuenta %||% "")), fixed = TRUE),
        ]
      }
      catalogo_ids(if (is.null(provs)) character(0) else as.character(provs$id))

      if (is.null(provs) || !nrow(provs)) {
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
      nc_val  <- if (!is_new) row$no_cuenta %||% "" else ""
      # Normalize stored banco (3-digit clave or "Nombre|CLAVE") → matching choice value
      ban_raw <- if (!is_new) row$banco %||% "" else ""
      ban_val <- {
        if (!nzchar(ban_raw)) "" else {
          clave <- if (grepl("^[0-9]{3}$", trimws(ban_raw))) {
            trimws(ban_raw)
          } else if (grepl("\\|", ban_raw)) {
            tail(strsplit(ban_raw, "\\|")[[1]], 1)
          } else {
            m <- regmatches(ban_raw, regexpr("[0-9]{3}", ban_raw))
            if (length(m)) m[1] else ""
          }
          hit <- grep(paste0("\\|", trimws(clave), "$"), .BANCOS_MX_CHOICES, value = TRUE)
          if (length(hit)) hit[1] else ""
        }
      }
      cor_val <- if (!is_new) row$correo %||% "" else ""
      # Normalize stored moneda: legacy Spanish labels → ISO codes
      mon_raw <- if (!is_new) row$moneda %||% "MXN" else "MXN"
      mon_val <- if (mon_raw %in% CURRENCIES) {
        mon_raw
      } else if (grepl("^peso", mon_raw, ignore.case = TRUE)) {
        "MXN"
      } else if (grepl("^d.lar", mon_raw, ignore.case = TRUE)) {
        "USD"
      } else {
        "MXN"
      }
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
            selectInput(ns("prov_banco"), NULL,
                        choices  = c("— Sin banco —" = "", .BANCOS_MX_CHOICES[-1]),
                        selected = ban_val, width = "100%")
          ),
          div(class = "col-md-8",
            tags$label("Correo (separados por coma)", class = "form-label small fw-semibold"),
            textInput(ns("prov_correo"), NULL, value = cor_val, width = "100%")
          ),
          div(class = "col-md-4",
            tags$label("Moneda", class = "form-label small fw-semibold"),
            selectInput(ns("prov_moneda"), NULL,
                        choices = CURRENCIES, selected = mon_val, width = "100%")
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
      if (!.puede_editar()) return()
      editing_id_cat(NULL)
      .show_edit_modal(NULL)
    }, ignoreInit = TRUE)

    # Handle edit button clicks via JS
    observeEvent(input$prov_edit_id, {
      if (!.puede_editar()) return()
      provs <- get_provs()
      row   <- provs[provs$id == input$prov_edit_id, ]
      if (!nrow(row)) return()
      editing_id_cat(row$id[1])
      .show_edit_modal(row[1, ])
    }, ignoreInit = TRUE)

    # Handle inactivar button clicks
    observeEvent(input$prov_inactivar_id, {
      if (!.puede_editar()) return()
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
      if (!.puede_editar()) return()
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
      if (!.puede_editar()) return()
      provs <- get_provs()
      updated <- provs[provs$id != input$prov_delete_id, ]
      set_provs(updated)
      showNotification("Proveedor eliminado.", type = "message", duration = 2)
      output$tbl_catalogo_prov <- DT::renderDataTable({
        .render_catalogo(input$cat_search %||% "")
      }, server = FALSE)
    }, ignoreInit = TRUE)

    # Bulk inactivar
    # ids picked when the modal opens; the confirm acts on exactly these.
    bulk_inactivar_ids <- reactiveVal(character(0))

    observeEvent(input$cat_inactivar_bulk, {
      if (!.puede_editar()) return()
      sel <- input$tbl_catalogo_prov_rows_selected
      if (!length(sel)) {
        showNotification("Selecciona filas primero.", type = "warning"); return()
      }
      ids   <- catalogo_ids()[sel[sel <= length(catalogo_ids())]]
      provs <- get_provs()
      rows_to_move <- provs[provs$id %in% ids &
                              !is.na(provs$activo) & provs$activo == TRUE, , drop = FALSE]
      if (!nrow(rows_to_move)) return()
      bulk_inactivar_ids(as.character(rows_to_move$id))

      showModal(modalDialog(
        title = tagList(icon("ban"), " Inactivar selección"),
        size = "s", easyClose = TRUE,
        tags$p(sprintf("¿Mover %d proveedor(es) a inactivos?", nrow(rows_to_move))),
        tags$ul(class = "small mb-0", style = "max-height:240px; overflow-y:auto;",
                lapply(rows_to_move$nombre, tags$li)),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("bulk_inactivar_confirm"),
                       tagList(icon("ban"), " Inactivar"),
                       class = "btn btn-warning")
        )
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$bulk_inactivar_confirm, {
      if (!.puede_editar()) return()
      ids   <- bulk_inactivar_ids()
      provs <- get_provs()
      rows_to_move <- provs[provs$id %in% ids, , drop = FALSE]
      remaining    <- provs[!provs$id %in% ids, , drop = FALSE]
      if (!nrow(rows_to_move)) { removeModal(); return() }

      rows_to_move$activo     <- FALSE
      rows_to_move$updated_at <- as.character(Sys.time())

      inac <- dplyr::bind_rows(get_inac(), rows_to_move)
      # Inactive list first: if the second write fails the providers are
      # left in both lists rather than in neither.
      saved <- tryCatch({ set_inac(inac); set_provs(remaining); TRUE },
                        error = function(e) { message("[PROV] bulk inactivar failed: ", e$message); e })
      if (!isTRUE(saved)) {
        removeModal()
        showNotification("No se pudieron inactivar los proveedores. Verifica la conexión e intenta de nuevo.",
                         type = "error", duration = 6)
        tryCatch(log_action(
          user        = tryCatch(shared$current_user(), error = function(e) "system"),
          module      = "proveedores",
          action      = "inactivar_proveedores_fallido",
          description = paste0("Falló al guardar la inactivación de ", nrow(rows_to_move),
                               " proveedor(es): ", conditionMessage(saved)),
          target_id   = paste(ids, collapse = ","),
          client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
          viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
        ), error = function(e) NULL)
        return()
      }
      bulk_inactivar_ids(character(0))
      removeModal()
      showNotification(paste0(nrow(rows_to_move), " proveedor(es) inactivados."),
                       type = "message", duration = 2)
      output$tbl_catalogo_prov <- DT::renderDataTable({
        .render_catalogo(input$cat_search %||% "")
      }, server = FALSE)
    }, ignoreInit = TRUE)

    # Save modal
    observeEvent(input$prov_modal_save, {
      if (!.puede_editar()) return()
      nom  <- trimws(input$prov_nombre    %||% "")
      ali  <- trimws(input$prov_alias     %||% "")
      rfc  <- trimws(input$prov_rfc       %||% "")
      nc   <- trimws(input$prov_no_cuenta %||% "")
      ban  <- trimws(input$prov_banco     %||% "")
      cor  <- trimws(input$prov_correo    %||% "")
      mon  <- input$prov_moneda           %||% "MXN"
      # Extract 3-digit CECOBAN clave from "Nombre|CLAVE" choice value
      ban_clave <- if (nzchar(ban) && grepl("\\|", ban)) {
        tail(strsplit(ban, "\\|")[[1]], 1)
      } else ban
      # Derive transaction type from currency + destination bank
      derived_medio_pago <- if (mon == "USD")                       "SPD"
                       else if (mon == "MXN" && ban_clave == "030") "BCO"
                       else                                         "SPI"
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

      # Determine status (use ban_clave so empty-bank counts as incomplete)
      st <- if (nzchar(nom) && nzchar(mon) && nzchar(ban_clave) && nzchar(nc))
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
        medio_pago    = derived_medio_pago,
        rfc           = rfc,
        tipo          = "012",
        banco_destino = ban_clave,
        activo        = TRUE,
        no_cuenta     = nc,
        banco         = ban_clave,
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
    # Reading, cleaning, classifying and applying are the pure functions of
    # R/proveedores_excel.R (T08); this part shows their result and saves it.

    # Reactive: parsed + classified upload data
    upload_data_rv         <- reactiveVal(NULL)
    conflict_decisions     <- reactiveVal(list())
    incluir_incompletos_rv <- reactiveVal(FALSE)
    incluir_duplicados_rv  <- reactiveVal(FALSE)
    cambiar_alias_rv       <- reactiveVal(FALSE)

    observeEvent(input$incluir_incompletos, {
      incluir_incompletos_rv(isTRUE(input$incluir_incompletos))
    }, ignoreInit = TRUE)

    observeEvent(input$incluir_duplicados, {
      incluir_duplicados_rv(isTRUE(input$incluir_duplicados))
    }, ignoreInit = TRUE)

    observeEvent(input$cambiar_alias, {
      cambiar_alias_rv(isTRUE(input$cambiar_alias))
    }, ignoreInit = TRUE)

    .catalogo_bancos <- function() if (exists(".BANCOS_MX_CHOICES")) .BANCOS_MX_CHOICES else character(0)
    .alias_mapa <- function() tryCatch(
      if (is.function(shared$parte_alias_map_db)) shared$parte_alias_map_db() else NULL,
      error = function(e) NULL)
    # Rows that touch a provider already in the catalog: they need a decision.
    .es_existente <- function(estado) estado %in% c("actualiza", "reactiva", "inactiva")

    # The round-trip template: the active catalog with every column the import
    # reads, accounts / RFC / bank as text cells.
    output$descargar_plantilla <- downloadHandler(
      filename = function() paste0("proveedores_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
      content  = function(file) prov_plantilla_escribir(get_provs(), file, .catalogo_bancos())
    )

    # Parse and classify uploaded Excel
    observeEvent(input$excel_file, {
      if (!.puede_editar()) return()
      req(input$excel_file)
      tryCatch({
        lectura <- prov_leer_excel(input$excel_file$datapath)
        if (is.null(lectura) || !nrow(lectura$filas)) {
          showNotification(tr("tablas.proveedores.importar.vacio"), type = "error")
          return()
        }
        tipo <- input$excel_tipo %||% "activos"
        rows <- prov_clasificar(lectura$filas, get_provs(), get_inac(), tipo = tipo,
                                catalogo = .catalogo_bancos(), alias_mapa = .alias_mapa())
        rows$.clasificacion <- rows$.estado
        emp_input <- input$excel_empresa %||% "TODOS"
        upload_data_rv(list(rows = rows, empresa = if (emp_input == "TODOS") "" else emp_input,
                            tipo = tipo, lectura = lectura))
        conflict_decisions(list())
        conflict_idx_rv(1L)
      }, error = function(e) {
        showNotification(paste(tr("tablas.proveedores.importar.error_leer"), conditionMessage(e)), type = "error")
      })
    }, ignoreInit = TRUE)

    # No Moneda column in the file: the new providers take the one chosen here.
    observe(shinyjs::toggle("excel_moneda_wrap",
                            condition = "moneda" %in% (upload_data_rv()$lectura$faltan %||% character(0))))

    # Stage 2/3 — the review table (hd_table, class E). Each row's id is its
    # row number in the Excel (.row_num), the same key the ✕ removes by. The
    # chips are the old filter buttons with their counts; "Dup. fila N" is now
    # the Duplicado badge plus the Fila column, which every row has. Detalle
    # says why a row is held back, what changes (antes → después) or what was
    # repaired.
    observe(shinyjs::toggle("revision_wrap", condition = !is.null(upload_data_rv())))

    .campo_texto <- function(campo) tr(paste0("tablas.proveedores.", campo))
    .detalle <- function(rows) vapply(seq_len(nrow(rows)), function(i) {
      partes <- character(0)
      # (the bank-vs-CLABE motive names both banks; other keys ignore the values)
      if (!is.na(rows$.motivo[i])) partes <- c(partes, tr(rows$.motivo[i], clabe = substr(rows$no_cuenta[i], 1, 3),
                                                          banco = rows$banco[i] %||% ""))
      if (!is.na(rows$.nota[i]))   partes <- c(partes, tr(rows$.nota[i]))
      for (c in rows$.cambios[[i]])
        partes <- c(partes, tr("tablas.proveedores.importar.cambio", campo = .campo_texto(c$campo),
                               antes = c$antes %||% "—", despues = c$despues))
      if (rows$.alias_vinculos[i] > 0L)
        partes <- c(partes, tr_n("tablas.proveedores.importar.alias_vinculos", rows$.alias_vinculos[i]))
      paste(partes, collapse = " · ")
    }, "")

    revision_datos <- reactive({
      ud <- upload_data_rv()
      req(!is.null(ud))
      rows <- ud$rows
      data.frame(
        id        = as.character(rows$.row_num),
        estado    = rows$.estado,
        fila      = as.character(rows$.row_num),
        nombre    = rows$nombre,
        alias     = rows$alias,
        rfc       = rows$rfc,
        no_cuenta = rows$no_cuenta,
        banco     = rows$banco,
        moneda    = rows$moneda,
        detalle   = .detalle(rows),
        stringsAsFactors = FALSE
      )
    })

    PROV_ESTADOS <- list(
      listo       = list(texto = "tablas.proveedores.listo",       tono = "exito"),
      actualiza   = list(texto = "tablas.proveedores.actualiza",   tono = "info"),
      reactiva    = list(texto = "tablas.proveedores.reactiva",    tono = "morado"),
      inactiva    = list(texto = "tablas.proveedores.inactiva",    tono = "morado"),
      sin_cambios = list(texto = "tablas.proveedores.sin_cambios", tono = "neutro"),
      incompleto  = list(texto = "tablas.proveedores.incompleto",  tono = "aviso"),
      revisar     = list(texto = "tablas.proveedores.revisar",     tono = "rojo"),
      duplicado   = list(texto = "tablas.proveedores.duplicado",   tono = "peligro"))

    revision <- hd_table_server(
      "revision", tabla = "revision_excel_prov", datos = revision_datos,
      piezas = c("fichas", "ordenar", "acc_fila", "recorte"),
      columnas = list(
        col_insignia("estado", "tablas.proveedores.estado", mapa = PROV_ESTADOS),
        col_texto("fila",      "tablas.proveedores.fila", recorte = FALSE),
        col_texto("nombre",    "tablas.proveedores.nombre"),
        col_texto("alias",     "tablas.proveedores.alias"),
        col_texto("rfc",       "tablas.proveedores.rfc", recorte = FALSE),
        col_texto("no_cuenta", "tablas.proveedores.no_cuenta", recorte = FALSE),
        col_texto("banco",     "tablas.proveedores.banco", recorte = FALSE),
        col_texto("moneda",    "tablas.proveedores.moneda", recorte = FALSE),
        col_texto("detalle",   "tablas.proveedores.detalle")
      ),
      fichas        = list(campo = "estado", orden = names(PROV_ESTADOS)),
      acciones_fila = "eliminar",
      vacio         = "tablas.proveedores.revision_vacia"
    )

    # Which column of the file each field came from, and what it lacks.
    output$revision_mapeo <- renderUI({
      lectura <- upload_data_rv()$lectura
      req(!is.null(lectura))
      tomadas <- vapply(names(lectura$columnas), function(campo)
        tr("tablas.proveedores.importar.columna_tomada", campo = .campo_texto(campo),
           columna = lectura$columnas[[campo]]), "")
      faltan <- vapply(lectura$faltan, .campo_texto, "")
      tags$div(class = "small text-muted mb-2",
        tags$div(tr("tablas.proveedores.importar.columnas_tomadas"), " ", paste(tomadas, collapse = " · ")),
        if (length(faltan)) tags$div(tr("tablas.proveedores.importar.columnas_faltan"), " ", paste(faltan, collapse = ", ")),
        if ("moneda" %in% lectura$faltan)
          tags$div(tr("tablas.proveedores.importar.moneda_selector", moneda = input$excel_moneda %||% "MXN")))
    })

    # ✕ (confirmed in the table's bar): the row leaves this upload only;
    # nothing is saved until Importar.
    observeEvent(revision$accion(), {
      a <- revision$accion()
      if (!identical(a$accion, "eliminar")) return()
      ud <- upload_data_rv()
      if (is.null(ud)) return()
      rows <- ud[["rows"]]
      rows <- rows[!as.character(rows$.row_num) %in% a$ids, , drop = FALSE]
      ud$rows <- rows                       # (modifyList would merge the data frames column by column)
      upload_data_rv(ud)
    })

    # Stage 4 — Conflict resolution: one existing provider at a time, with
    # what the file would change.
    conflict_idx_rv <- reactiveVal(1L)

    output$stage4_ui <- renderUI({
      ud <- upload_data_rv()
      if (is.null(ud)) return(NULL)
      rows    <- ud$rows
      conf    <- rows[.es_existente(rows$.estado), , drop = FALSE]
      if (!nrow(conf)) return(NULL)

      decisions <- conflict_decisions()
      n_resolved <- length(decisions)
      n_total    <- nrow(conf)

      idx <- conflict_idx_rv()
      if (idx > n_total) idx <- n_total
      conf_row <- conf[idx, ]
      nc       <- conf_row$no_cuenta
      cambios  <- conf_row$.cambios[[1]]

      tagList(
        tags$hr(),
        tags$h6(class = "fw-semibold", tagList(icon("triangle-exclamation"), " ",
                                                tr("tablas.proveedores.importar.resolver"))),
        tags$p(class = "small text-muted",
               tr("tablas.proveedores.importar.resueltos", n = n_resolved, total = n_total)),

        div(class = "prov-conflict-card",
          tags$strong(tr(paste0("tablas.proveedores.importar.conflicto_", conf_row$.estado),
                         nombre = conf_row$nombre %||% "", cuenta = nc)),
          tags$hr(class = "my-2"),
          if (length(cambios))
            tags$ul(class = "small mb-0", lapply(cambios, function(c)
              tags$li(tr("tablas.proveedores.importar.cambio", campo = .campo_texto(c$campo),
                         antes = c$antes %||% "—", despues = c$despues))))
          else tags$p(class = "small mb-0", tr("tablas.proveedores.importar.solo_mueve")),
          if (conf_row$.alias_vinculos > 0L)
            div(class = "alert alert-warning small mt-2 mb-0",
                tr_n("tablas.proveedores.importar.alias_vinculos", conf_row$.alias_vinculos)),
          tags$hr(class = "my-2"),
          div(class = "d-flex gap-2 flex-wrap",
            actionButton(ns("conf_conservar"),  tr("tablas.proveedores.importar.dejar"),
                         class = "btn btn-sm btn-outline-secondary"),
            actionButton(ns("conf_reemplazar"), tr("tablas.proveedores.importar.aplicar"),
                         class = "btn btn-sm btn-outline-primary"),
            actionButton(ns("conf_omitir"),     tr("tablas.proveedores.importar.omitir"),
                         class = "btn btn-sm btn-outline-danger"),
            actionButton(ns("conf_reemplazar_todos"), tr("tablas.proveedores.importar.aplicar_todos"),
                         class = "btn btn-sm btn-warning",
                         title = tr("tablas.proveedores.importar.aplicar_todos_ayuda"))
          )
        )
      )
    })

    .advance_conflict <- function(decision) {
      ud   <- upload_data_rv()
      conf <- ud$rows[.es_existente(ud$rows$.estado), , drop = FALSE]
      idx  <- conflict_idx_rv()
      if (idx > nrow(conf)) return()
      nc   <- conf$no_cuenta[idx]
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
      conf <- ud$rows[.es_existente(ud$rows$.estado), , drop = FALSE]
      if (!nrow(conf)) return()
      dec <- conflict_decisions()
      for (nc in conf$no_cuenta) if (is.null(dec[[nc]])) dec[[nc]] <- "reemplazar"
      conflict_decisions(dec)
      conflict_idx_rv(nrow(conf) + 1L)
      n_bulk <- sum(vapply(names(dec), function(k) dec[[k]] == "reemplazar", logical(1)))
      showNotification(tr("tablas.proveedores.importar.aplicar_todos_hecho", n = n_bulk),
                       type = "message", duration = 4)
    }, ignoreInit = TRUE)

    # Stage 5 — Final summary
    output$stage5_ui <- renderUI({
      ud <- upload_data_rv()
      if (is.null(ud)) return(NULL)
      rows <- ud$rows
      n_de <- function(e) sum(rows$.estado == e)
      n_listo <- n_de("listo"); n_incom <- n_de("incompleto"); n_dup <- n_de("duplicado")
      n_rev   <- n_de("revisar"); n_igual <- n_de("sin_cambios")
      exist   <- rows[.es_existente(rows$.estado), , drop = FALSE]
      dec     <- conflict_decisions()
      conf_pending <- sum(vapply(exist$no_cuenta, function(nc) is.null(dec[[nc]]), logical(1)))
      aplica  <- vapply(exist$no_cuenta, function(nc) identical(dec[[nc]], "reemplazar"), logical(1))
      n_alias <- sum(aplica & exist$.alias_vinculos > 0L)
      n_aplicar <- sum(aplica) - if (cambiar_alias_rv()) 0L else n_alias

      n_to_import <- n_listo +
        (if (incluir_incompletos_rv()) n_incom else 0L) +
        (if (incluir_duplicados_rv())  length(unique(rows$no_cuenta[rows$.estado == "duplicado"])) else 0L) +
        n_aplicar

      tagList(
        tags$hr(),
        tags$h6(class = "fw-semibold", tagList(icon("check-circle"), " Resumen final")),
        tags$p(class = "small text-muted fst-italic mb-2",
          icon("circle-info"),
          " Las decisiones de conflicto se aplican al hacer clic en ",
          tags$strong("Importar"), " — nada se guarda hasta entonces."
        ),
        div(class = "d-flex flex-column gap-1 mb-3",
          div(paste0("✅ ", n_listo, " proveedores listos")),
          if (nrow(exist) > 0)
            div(tr("tablas.proveedores.importar.resumen_existentes", n = nrow(exist), resueltos = length(dec)))
          else NULL,
          if (n_igual > 0) div(tr("tablas.proveedores.importar.resumen_sin_cambios", n = n_igual)) else NULL,
          if (n_rev > 0)
            div(class = "text-danger", tr("tablas.proveedores.importar.resumen_revisar", n = n_rev))
          else NULL,
          if (n_incom > 0)
            div(paste0("⚠️ ", n_incom,
                       " incompletos (serán omitidos a menos que marques la opción)"))
          else NULL,
          if (n_dup > 0)
            div(paste0("\U0001f534 ", n_dup,
                       " duplicados (serán omitidos a menos que marques la opción)"))
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
        if (n_alias > 0)
          checkboxInput(ns("cambiar_alias"), tr_n("tablas.proveedores.importar.cambiar_alias", n_alias),
                        value = cambiar_alias_rv())
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
                                 paste0(" Importar ", n_to_import, " proveedores →")),
                         class = "btn btn-sm btn-primary")
          else NULL
        )
      )
    })

    .limpiar_carga <- function() {
      upload_data_rv(NULL)
      conflict_decisions(list())
      conflict_idx_rv(1L)
      incluir_incompletos_rv(FALSE)
      incluir_duplicados_rv(FALSE)
      cambiar_alias_rv(FALSE)
    }

    observeEvent(input$cancelar_upload, {
      .limpiar_carga()
      shinyjs::reset(ns("excel_file"))
    }, ignoreInit = TRUE)

    observeEvent(input$confirmar_upload, {
      if (!.puede_editar()) return()
      ud   <- upload_data_rv()
      if (is.null(ud)) return()
      res <- prov_aplicar(ud$rows, get_provs(), get_inac(), decisiones = conflict_decisions(), opciones = list(
        tipo = ud$tipo, empresa = ud$empresa, moneda = input$excel_moneda %||% "MXN",
        incluir_incompletos = incluir_incompletos_rv(), incluir_duplicados = incluir_duplicados_rv(),
        cambiar_alias = cambiar_alias_rv(), lote = uuid::UUIDgenerate()))
      n_imp <- res$nuevos + res$actualizados + res$movidos
      if (n_imp == 0L) {
        showNotification(tr("tablas.proveedores.importar.nada"), type = "warning")
        return()
      }
      # The list that receives moved providers is written first: if the second
      # write fails they are left in both lists rather than in neither.
      orden <- if (ud$tipo == "activos") c("activos", "inactivos") else c("inactivos", "activos")
      saved <- tryCatch({
        for (lista in orden) if (isTRUE(res[[paste0("cambia_", lista)]]))
          if (lista == "activos") set_provs(res$activos) else set_inac(res$inactivos)
        TRUE
      }, error = function(e) { message("[PROV] import failed: ", conditionMessage(e)); e })
      resumen <- tr("tablas.proveedores.importar.hecho", nuevos = res$nuevos,
                    actualizados = res$actualizados, movidos = res$movidos)
      if (!isTRUE(saved)) {
        showNotification(tr("tablas.proveedores.importar.error_guardar"), type = "error", duration = 8)
        tryCatch(log_action(
          user        = tryCatch(shared$current_user(), error = function(e) "system"),
          module      = "proveedores",
          action      = "importar_proveedores_fallido",
          description = paste0("Falló al guardar la importación (", resumen, "): ", conditionMessage(saved)),
          client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
          viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
        ), error = function(e) NULL)
        return()
      }
      tryCatch(log_action(
        user        = tryCatch(shared$current_user(), error = function(e) "system"),
        module      = "proveedores",
        action      = "importar_proveedores",
        description = paste0("Importación de Excel: ", resumen),
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      ), error = function(e) NULL)
      .limpiar_carga()
      showNotification(resumen, type = "message", duration = 5)
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
      if (is.null(inac)) inac <- data.frame()
      inac <- inac[!is.na(inac$activo) & inac$activo == FALSE, ]

      if (nzchar(trimws(search_txt))) {
        s <- strip_accents(tolower(trimws(search_txt)))
        inac <- inac[
          grepl(s, strip_accents(tolower(inac$nombre    %||% "")), fixed = TRUE) |
          grepl(s, strip_accents(tolower(inac$alias     %||% "")), fixed = TRUE) |
          grepl(s, strip_accents(tolower(inac$rfc       %||% "")), fixed = TRUE) |
          grepl(s, strip_accents(tolower(inac$no_cuenta %||% "")), fixed = TRUE),
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
      if (!.puede_editar()) return()
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
      if (!.puede_editar()) return()
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
      if (!.puede_editar()) return()
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
