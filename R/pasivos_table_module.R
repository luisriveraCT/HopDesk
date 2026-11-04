# =============================================================================
# R/pasivos_table_module.R
# Stage 3 (modified in Stage 4): Pasivos tab — two sub-tabs (Tabla / Lista),
# action buttons (+ Agregar pasivo, + Provisión manual), pencil-icon row edit,
# and cell-click handler for the single-provision editor.
# =============================================================================

# ── UI ─────────────────────────────────────────────────────────────────────────
pasivos_table_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "pasivos-table-module",
    pasivos_table_css(),

    # ── Action buttons (always visible, above both sub-tabs) ────────────────
    shiny::div(
      class = "d-flex gap-2 px-3 py-2 border-bottom bg-white",
      shiny::actionButton(ns("btn_add_liability"),
                          shiny::tagList(shiny::icon("plus"), " Agregar pasivo"),
                          class = "btn btn-primary btn-sm",
                          onclick = "Shiny.setInputValue('pasivos_add_liability', Math.random(), {priority:'event'})"),
      shiny::actionButton(ns("btn_add_provision_manual"),
                          shiny::tagList(shiny::icon("plus"), " Provisión manual"),
                          class = "btn btn-outline-secondary btn-sm")
    ),

    # ── Sub-tabs ────────────────────────────────────────────────────────────
    shiny::tabsetPanel(
      id       = ns("subtabs"),
      selected = "tabla",

      # ── Tabla sub-tab ───────────────────────────────────────────────────
      # The table engine (T17b): filters, search, Copiar and Excel are the
      # table's own; above it stay what only Pasivos has -- the granularity,
      # the window (← 60 / 60 →) and the summary line.
      shiny::tabPanel(
        "Tabla", value = "tabla",
        shiny::div(
          shiny::div(
            class = "d-flex flex-wrap gap-2 align-items-center px-3 py-2 border-bottom bg-white",
            shinyWidgets::radioGroupButtons(
              inputId      = ns("granularity"), label = NULL,
              choiceValues = c("day", "week", "month", "year"),
              choiceNames  = list(
                shiny::tags$span("Día",    title = "Ver cada provisión en su fecha exacta"),
                shiny::tags$span("Semana", title = "Agrupar provisiones por semana"),
                shiny::tags$span("Mes",    title = "Agrupar provisiones por mes"),
                shiny::tags$span("Año",    title = "Agrupar provisiones por año")
              ),
              selected = "day", size = "sm", status = "outline-secondary"
            ),
            shiny::actionButton(ns("extend_left"), "← 60 días",
                                class = "btn btn-outline-secondary btn-sm pasivos-extend-btn"),
            shiny::actionButton(ns("extend_right"), "60 días →",
                                class = "btn btn-outline-secondary btn-sm pasivos-extend-btn"),
            shiny::div(class = "d-flex flex-wrap align-items-center gap-2 small ms-2",
                       shiny::uiOutput(ns("summary_line"), inline = TRUE))
          ),
          shiny::div(class = "px-2 py-1", hd_table_ui(ns("tabla"), alto = "ventana"))
        )
      ),

      # ── Lista sub-tab ────────────────────────────────────────────────────
      shiny::tabPanel(
        "Lista de pasivos", value = "lista",
        pasivos_list_module_ui(ns("list"))
      )
    ),

    # Hidden modal for orphan provision creation
    shiny::uiOutput(ns("provision_manual_modal_placeholder"))
  )
}


# ── Server ─────────────────────────────────────────────────────────────────────
pasivos_table_module_server <- function(id, shared) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Start the list sub-module ───────────────────────────────────────────
    pasivos_list_module_server("list", shared)

    # ── Action: + Provisión manual ──────────────────────────────────────────
    shiny::observeEvent(input$btn_add_provision_manual, ignoreInit = TRUE, {
      # Use full names (same as show_combined_entry_modal) so stored empresa matches
      # the full-name empresa_sel_rv filter in df_combined → Calendario picks it up.
      empresa_choices <- tryCatch(sort(unique(shared$empresa_sel())),
                                  error = function(e) character(0))
      shiny::showModal(.ppm_modal_ui(empresa_choices, ns = ns))
    })

    # Vendor suggestions for the manual provision modal
    output$ppm_parte_suggestions <- shiny::renderUI({
      q <- input$ppm_parte %||% ""
      if (!nzchar(trimws(q))) return(NULL)
      prov <- tryCatch(shared$proveedores_db(), error = function(e) NULL)
      if (is.null(prov) || !nrow(prov)) return(NULL)
      matches <- tryCatch(
        find_proveedor_matches(
          query          = list(parte = q, rfc = "", no_cuenta = "", alias = ""),
          proveedores_df = prov,
          threshold      = 15L,
          top_n          = 6L
        ),
        error = function(e) NULL
      )
      if (is.null(matches) || !nrow(matches)) return(NULL)
      shiny::div(
        class = "border rounded bg-white shadow-sm mt-1",
        style = "max-height:220px; overflow-y:auto; z-index:9999; position:relative;",
        lapply(seq_len(nrow(matches)), function(i) {
          m   <- matches[i, ]
          nom <- m$nombre %||% ""
          ali <- m$alias  %||% ""
          sc  <- round(m$.score %||% 0)
          payload <- jsonlite::toJSON(list(nombre = nom, codigo = ali), auto_unbox = TRUE)
          shiny::div(
            class = "d-flex align-items-start gap-2 py-1 px-1 rounded",
            style = "cursor:pointer;",
            onmouseover = "this.style.background='#f0f4ff'",
            onmouseout  = "this.style.background=''",
            onclick = sprintf(
              "Shiny.setInputValue('%s',%s,{priority:'event'})",
              ns("ppm_parte_pick"), payload
            ),
            shiny::div(
              class = "flex-grow-1",
              shiny::tags$div(class = "fw-semibold", nom),
              if (nzchar(ali)) shiny::tags$div(class = "text-muted small", ali) else NULL
            ),
            shiny::tags$span(
              class = "badge rounded-pill ms-auto",
              style = "background:#6c757d; font-size:11px; align-self:center;",
              paste0(sc, "%")
            )
          )
        })
      )
    })

    shiny::observeEvent(input$ppm_parte_pick, ignoreInit = TRUE, {
      pick <- input$ppm_parte_pick
      if (is.null(pick)) return()
      nom <- if (is.list(pick)) pick$nombre %||% "" else ""
      cod <- if (is.list(pick)) pick$codigo %||% "" else ""
      if (nzchar(nom))
        shiny::updateTextInput(session, "ppm_parte", value = nom)
      shiny::updateTextInput(session, "ppm_codigo", value = cod)
    })

    # ── Helper: build a new orphan provision tibble from modal inputs ──────────
    # pagar_hoy_id is always NA_character_ here: a freshly-created provision
    # is never staged to Agenda directly (Mouse's rule) -- only its converted
    # derivative item can be, via the separate convert-to-comprobante modal.
    .ppm_build_new_prov <- function(new_id, now, user, empresa) {
      tibble::tibble(
        id                       = new_id,
        liability_id             = NA_character_,
        origin                   = "manual",
        occurrence_index         = NA_integer_,
        estado                   = "provisional",
        fecha_calculada          = as.Date(input$ppm_fecha %||% Sys.Date()),
        fecha_efectiva           = as.Date(input$ppm_fecha %||% Sys.Date()),
        policy_ids               = NA_character_,
        empresa                  = empresa,
        parte                    = input$ppm_parte       %||% "",
        codigo_parte             = input$ppm_codigo      %||% "",
        moneda_pago              = input$ppm_moneda      %||% "MXN",
        cotizado_en              = NA_character_,
        amount_pago              = as.numeric(input$ppm_importe %||% 0),
        amount_cotizado          = NA_real_,
        fx_rate_used             = NA_real_,
        componente_capital       = NA_real_,
        componente_interes       = NA_real_,
        componente_fees          = NA_real_,
        componente_iva           = NA_real_,
        amount_pago_override     = NA_real_,
        amount_cotizado_override = NA_real_,
        fecha_efectiva_override  = as.Date(NA),
        documento                = input$ppm_documento   %||% "",
        referencia               = input$ppm_referencia  %||% "",
        notas                    = input$ppm_notas       %||% "",
        manual_inv_id            = NA_character_,
        pagar_hoy_id             = NA_character_,
        bancos_conf_id           = NA_character_,
        reverted_count           = 0L,
        generated_by             = user,
        generated_at             = now,
        last_edited_by           = user,
        last_edited_at           = now
      )
    }

    # Button 1 — Agregar a calendario: save provision; calendar updates in real time
    shiny::observeEvent(input$ppm_save, ignoreInit = TRUE, {
      user    <- tryCatch(shared$current_user(), error = function(e) "")
      empresa <- input$ppm_empresa %||% ""

      if (!has_capability(user, "pasivos.create_provision_manual")) {
        shiny::showNotification("Sin permiso.", type = "error"); return()
      }

      shinyjs::disable("ppm_save")
      on.exit(shinyjs::enable("ppm_save"), add = TRUE)

      new_id   <- uuid::UUIDgenerate()
      now      <- Sys.time()
      new_prov <- .ppm_build_new_prov(new_id, now, user, empresa)

      # Read from the in-memory reactiveVal (same pattern as wizard) so we merge
      # with the most current state, not a potentially stale S3 snapshot.
      existing <- tryCatch(shared$pasivos_provisions_db(),
                           error = function(e) .schema_pasivos_provision())
      if (is.null(existing) || !is.data.frame(existing))
        existing <- .schema_pasivos_provision()

      new_all <- dplyr::bind_rows(existing, new_prov)

      ok <- tryCatch({ save_pasivos_provisions(new_all, client_id = shared$effective_client_id(), base = existing); TRUE },
                     error = function(e) {
                       shiny::showNotification(
                         paste0("Error al guardar: ", conditionMessage(e)),
                         type = "error"); FALSE })
      if (!ok) return()

      # Push to in-memory reactiveVal first so Calendario / Vencidos re-render
      # immediately — same pattern the wizard uses.
      # Flag prevents the ledger auto-refresh observer from re-opening the
      # calendar day modal after this external save.
      tryCatch(shared$suppress_ledger_prov_refresh(TRUE), error = function(e) NULL)
      shared$pasivos_provisions_db(new_all)
      bump_sync_version("pasivos_provisions_db", client_id = shared$effective_client_id())

      tryCatch(pasivos_log_audit(
        action_type = "provision.generated", user = user,
        empresa = empresa, target_kind = "provision", target_id = new_id,
        after = as.list(new_prov),
        notes = "manual orphan provision via Pasivos tab",
        client_id = shared$effective_client_id()
      ), error = function(e) NULL)

      shiny::removeModal()
      shiny::showNotification("Provisión manual creada.", type = "message", duration = 3)
    })

    # ── Visible window ──────────────────────────────────────────────────────
    vis_window <- shiny::reactiveVal(list(
      start = Sys.Date() - 7L,
      end   = Sys.Date() + 53L
    ))

    shiny::observeEvent(input$extend_right, {
      w <- vis_window(); w$end <- w$end + 60L; vis_window(w)
    })
    shiny::observeEvent(input$extend_left, {
      w <- vis_window(); w$start <- w$start - 60L; vis_window(w)
    })
    granularidad <- shiny::reactive(input$granularity %||% "day")

    # ── Long-form data ──────────────────────────────────────────────────────
    # Everything the user may see (company visibility); the table engine
    # filters and searches it in the browser (T17b).
    liabs_visibles <- shiny::reactive({
      liabs <- tryCatch(shared$pasivos_liabilities_db(), error = function(e) NULL)
      if (is.null(liabs)) liabs <- .schema_pasivos_liability()
      allowed <- tryCatch(shared$visible_initials(), error = function(e) NULL)
      if (!is.null(allowed) && nrow(liabs) && "empresa" %in% names(liabs))
        liabs <- liabs[liabs$empresa %in% allowed, , drop = FALSE]
      liabs
    })
    long_df <- shiny::reactive({
      provs <- tryCatch(shared$pasivos_provisions_db(), error = function(e) NULL)
      mi    <- tryCatch(shared$manual_inv(),             error = function(e) NULL)
      bc    <- tryCatch(shared$bancos_confirmados(),     error = function(e) NULL)

      if (is.null(provs)) provs <- .schema_pasivos_provision()
      if (is.null(mi))    mi    <- data.frame()
      if (is.null(bc))    bc    <- data.frame()

      # Respect group-based company visibility (NULL = no filter; character(0) = none)
      allowed <- tryCatch(shared$visible_initials(), error = function(e) NULL)
      if (!is.null(allowed)) {
        if (nrow(provs) && "empresa" %in% names(provs))
          provs <- provs[provs$empresa %in% allowed, , drop = FALSE]
        if (nrow(mi) && "empresa" %in% names(mi))
          mi    <- mi[mi$empresa %in% allowed, , drop = FALSE]
      }

      mi_ap <- if (nrow(mi) && "ledger" %in% names(mi))
        mi[mi$ledger == "AP", , drop = FALSE] else mi
      bc_ok <- confirmaciones_vigentes(bc)

      tryCatch(
        pasivos_table_build_long(
          provisions         = provs,
          manual_items       = mi_ap,
          bancos_confirmados = bc_ok,
          liabilities        = liabs_visibles(),
          filters            = list(empresa = character(0), categorias = c("regular", "financiero", "tarjeta"),
                                    subcategorias = character(0), currency = "All", search = "",
                                    granularity = granularidad(), provision_type = "todas")
        ),
        error = function(e) {
          message("[pasivos_table] build_long error: ", conditionMessage(e))
          pasivos_table_build_long(.schema_pasivos_provision(), data.frame(), data.frame(),
                                   .schema_pasivos_liability(), list())
        }
      )
    })

    # ── Wide form, and the engine's rows and columns ────────────────────────
    wide <- shiny::reactive({
      w <- vis_window()
      pasivos_table_pivot_wide(
        long_df      = long_df(),
        granularity  = granularidad(),
        window_start = w$start,
        window_end   = w$end
      )
    })
    filas_tabla <- shiny::reactive(pasivos_tabla_datos(wide(), liabs_visibles()))
    cols_tabla  <- shiny::reactive(pasivos_tabla_columnas(wide(), Sys.Date(), granularidad()))

    tabla <- hd_table_server(
      "tabla", tabla = "pasivos_tabla", datos = filas_tabla, columnas = cols_tabla,
      piezas = c("accion_celda", "tono_celda", "sangria", "copiar_celda", "copiar_tsv"),
      accion_celda = list(clic = "editar_celda"),
      acciones_fila = "editar",
      secciones = list(list(campo = "Moneda", orden = c("MXN", "USD", "EUR"))),
      filtros = list(
        list(campo = "Empresa", titulo = "tablas.pasivos.filtro.empresa"),
        list(campo = "Categoria", titulo = "tablas.pasivos.filtro.categoria",
             ayuda = stats::setNames(paste0("tablas.pasivos.cat_ayuda.", c("regular", "financiero", "tarjeta")),
                                     vapply(paste0("tablas.pasivos.cat.", c("regular", "financiero", "tarjeta")), tr, character(1)))),
        list(campo = "Subcategoria", titulo = "tablas.pasivos.filtro.subcategoria"),
        list(campo = "Moneda", titulo = "tablas.pasivos.filtro.moneda"),
        list(campo = "Tipo", titulo = "tablas.pasivos.filtro.tipo",
             ayuda = stats::setNames(paste0("tablas.pasivos.tipo_ayuda.", c("programada", "manual")),
                                     vapply(paste0("tablas.pasivos.tipo.", c("programada", "manual")), tr, character(1))))),
      vacio = "tablas.pasivos.vacio",
      visibles = TRUE)

    # ── The table's actions, to the same global inputs as before ────────────
    # The cell's click, ⚡ / ↩ and ✎ keep sending what the rest of Pasivos
    # already listens to (pasivos_module.R, the wizard of T18): the same
    # input, the same value. The id goes as JSON, never written into code.
    .pt_emitir <- function(input_id, valor)
      shinyjs::runjs(sprintf("Shiny.setInputValue(%s, %s, {priority: 'event'});",
                             jsonlite::toJSON(input_id, auto_unbox = TRUE),
                             jsonlite::toJSON(as.character(valor), auto_unbox = TRUE)))
    shiny::observeEvent(tabla$accion(), {
      a <- tabla$accion()
      if (is.null(a)) return()
      switch(a$accion,
        editar_celda = {
          # A cell that adds up several opens its first one, as before (P4's
          # list comes in its own commit).
          ids <- pasivos_tabla_ids_celda(wide(), a$celda %||% "")
          if (length(ids)) .pt_emitir("pasivos_cell_click", ids[1])
        },
        provision = , comprobante = .pt_emitir("pasivos_convert_request", a$celda),
        editar = if (length(a$ids)) .pt_emitir("pasivos_edit_liability", a$ids[1]),
        NULL)
    }, ignoreInit = TRUE)

    # ── Summary ──────────────────────────────────────────────────────────────
    # Over what the table shows (its search and filters).
    output$summary_line <- shiny::renderUI({
      ldf <- long_df()
      vis <- tabla$visibles()
      if (!is.null(vis)) ldf <- ldf[ldf$row_id %in% vis, , drop = FALSE]
      if (!nrow(ldf))
        return(shiny::tags$span(class = "text-muted", "Sin datos para los filtros seleccionados."))

      n_provs <- sum(ldf$cell_kind %in% c("provision", "overdue_provision"), na.rm = TRUE)
      n_items <- sum(ldf$cell_kind %in%
                       c("manual_item", "confirmed_item", "overdue_manual"), na.rm = TRUE)
      n_liabs <- length(unique(ldf$row_id))
      summary_text <- sprintf("Mostrando %d pasivos · %d provisiones · %d items", n_liabs, n_provs, n_items)

      # Per-currency badges: provisions due in the next 7 natural days
      today <- Sys.Date()
      fut <- ldf[ldf$cell_kind == "provision" & !is.na(ldf$fecha) &
                   ldf$fecha >= today & ldf$fecha <= today + 7L,
                 , drop = FALSE]
      currencies <- .pasivos_cur_order(unique(fut$cell_currency))

      badges <- lapply(currencies, function(cur) {
        rows_cur <- fut[fut$cell_currency == cur, , drop = FALSE]
        total    <- sum(rows_cur$cell_amount, na.rm = TRUE)
        n        <- nrow(rows_cur)
        col      <- .pasivos_cur_color(cur)
        shiny::tags$span(
          class = "badge rounded-1",
          style = sprintf(
            "background:%s; color:#fff; font-size:11px; font-weight:500; padding:3px 9px;",
            col),
          sprintf("%s %s · %d próx.", cur, fmt_money(total), n)
        )
      })

      shiny::tagList(
        shiny::tags$span(class = "text-muted", summary_text),
        badges
      )
    })

    invisible(list(tabla = tabla, filas = filas_tabla, columnas = cols_tabla, wide = wide, long = long_df))
  })
}


# ── Manual provision modal UI ─────────────────────────────────────────────────
.ppm_modal_ui <- function(empresa_choices = character(), ns = identity) {
  shiny::modalDialog(
    title     = "Nueva provisión manual",
    size      = "m",
    easyClose = FALSE,
    shiny::fluidRow(
      shiny::column(6,
        shiny::selectInput(ns("ppm_empresa"),   "Empresa",
                           choices  = empresa_choices,
                           selected = empresa_choices[1] %||% ""),
        shiny::selectInput(ns("ppm_moneda"),    "Moneda",
                           choices = CURRENCIES, selected = "MXN"),
        shiny::textInput(ns("ppm_documento"),   "Documento", value = "")
      ),
      shiny::column(6,
        shiny::textInput(ns("ppm_parte"),       "Parte / Proveedor", value = ""),
        shiny::uiOutput(ns("ppm_parte_suggestions")),
        shiny::textInput(ns("ppm_codigo"),      "Código",            value = ""),
        shiny::numericInput(ns("ppm_importe"),  "Importe",           value = 0, min = 0),
        shiny::textInput(ns("ppm_referencia"),  "Referencia",        value = "")
      )
    ),
    shiny::fluidRow(
      shiny::column(6,
        shiny::dateInput(ns("ppm_fecha"), "Fecha",
                         value = Sys.Date(), weekstart = 1, language = "es")
      ),
      shiny::column(6,
        shiny::textAreaInput(ns("ppm_notas"), "Notas", rows = 3, value = "")
      )
    ),
    footer = shiny::tagList(
      shiny::modalButton("Cancelar"),
      # No "send to Agenda" option here, deliberately: a provision may never
      # enter Agenda directly (Mouse's rule) -- only its converted derivative
      # item can, via the separate convert-to-comprobante modal.
      shiny::actionButton(ns("ppm_save"), "Agregar a calendario",
                          class = "btn btn-primary")
    )
  )
}
