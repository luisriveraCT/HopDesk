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
      shiny::tabPanel(
        "Tabla", value = "tabla",
        shiny::div(
          # Filters bar — single compact row
          shiny::div(
            class = "d-flex flex-nowrap gap-3 align-items-end px-3 py-2 border-bottom bg-white",
            style = "overflow-x: auto;",
            shiny::div(class = "flex-shrink-0",
              shiny::tags$label(class = "form-label d-block mb-1 small fw-semibold", "Empresa"),
              shiny::selectizeInput(ns("empresa_sel"), label = NULL,
                choices = character(0), multiple = TRUE,
                options = list(placeholder = "Todas"), width = "160px")
            ),
            shiny::div(class = "flex-shrink-0",
              shiny::tags$label(class = "form-label d-block mb-1 small fw-semibold", "Categoría"),
              shinyWidgets::checkboxGroupButtons(
                inputId      = ns("categoria_sel"), label = NULL,
                choiceValues = c("regular", "financiero", "tarjeta"),
                choiceNames  = list(
                  shiny::tags$span("Regular",    title = "Pagos recurrentes: servicios, suscripciones y arrendamientos operativos"),
                  shiny::tags$span("Financiero", title = "Créditos, arrendamientos financieros y líneas de crédito"),
                  shiny::tags$span("Tarjeta",    title = "Liquidaciones de tarjeta corporativa")
                ),
                selected = c("regular", "financiero", "tarjeta"),
                size = "sm", status = "outline-secondary"
              )
            ),
            shiny::div(class = "flex-shrink-0",
              shiny::tags$label(class = "form-label d-block mb-1 small fw-semibold", "Sub-categoría"),
              shiny::selectizeInput(ns("subcategoria_sel"), label = NULL,
                choices = character(0), multiple = TRUE,
                options = list(placeholder = "Todas"), width = "130px")
            ),
            shiny::div(class = "flex-shrink-0",
              shiny::tags$label(class = "form-label d-block mb-1 small fw-semibold", "Moneda"),
              shiny::selectInput(ns("currency_sel"), label = NULL,
                choices = c("All", "MXN", "USD"), selected = "All", width = "80px")
            ),
            shiny::div(class = "flex-shrink-0",
              shiny::tags$label(class = "form-label d-block mb-1 small fw-semibold", "Granularidad"),
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
              )
            ),
            shiny::div(class = "flex-shrink-0",
              shiny::tags$label(class = "form-label d-block mb-1 small fw-semibold", "Provisiones"),
              shinyWidgets::radioGroupButtons(
                inputId      = ns("provision_type"), label = NULL,
                choiceValues = c("todas", "programadas", "manuales"),
                choiceNames  = list(
                  shiny::tags$span("Todas",       title = "Mostrar todas las provisiones"),
                  shiny::tags$span("Programadas", title = "Solo provisiones generadas desde un pasivo configurado (+ Agregar pasivo)"),
                  shiny::tags$span("Manuales",    title = "Solo provisiones creadas individualmente (+ Provisión manual)")
                ),
                selected = "todas", size = "sm", status = "outline-secondary"
              )
            ),
            shiny::div(class = "flex-grow-1",
              shiny::tags$label(class = "form-label d-block mb-1 small fw-semibold", "Buscar"),
              shiny::textInput(ns("search_text"), label = NULL,
                placeholder = "Nombre o Parte...", width = "100%")
            )
          ),

          # Summary line with per-currency badges
          shiny::div(
            class = "d-flex flex-wrap align-items-center gap-2 px-3 py-1 small border-bottom",
            shiny::uiOutput(ns("summary_line"))
          ),

          # Table + extend buttons
          shiny::div(
            class = "d-flex align-items-start gap-1 px-2 py-1",
            shiny::actionButton(ns("extend_left"), "← 60 días",
                                class = "btn btn-outline-secondary btn-sm pasivos-extend-btn"),
            shiny::div(
              class = "flex-grow-1 pasivos-tbl-col",
              shiny::div(class = "pasivos-table-wrap",
                         id    = ns("pasivos_tbl_wrap"),
                         shiny::uiOutput(ns("pasivos_table"))),
              shiny::div(class = "pasivos-scroll-bar",
                         id    = ns("pasivos_scroll_bar"),
                         shiny::div(class = "pasivos-scroll-bar-inner",
                                    id    = ns("pasivos_scroll_bar_inner")))
            ),
            shiny::actionButton(ns("extend_right"), "60 días →",
                                class = "btn btn-outline-secondary btn-sm pasivos-extend-btn")
          ),
          shiny::tags$script(shiny::HTML(sprintf("
(function() {
  var wId = '%s', bId = '%s', iId = '%s';
  function init() {
    var wrap  = document.getElementById(wId);
    var bar   = document.getElementById(bId);
    var inner = document.getElementById(iId);
    if (!wrap || !bar || !inner) { setTimeout(init, 300); return; }
    function syncWidth() {
      var tbl = wrap.querySelector('table');
      inner.style.width = (tbl ? tbl.scrollWidth : wrap.scrollWidth) + 'px';
    }
    syncWidth();
    new MutationObserver(syncWidth).observe(wrap, { childList: true, subtree: true });
    var busy = false;
    bar.addEventListener('scroll', function() {
      if (!busy) { busy = true; wrap.scrollLeft = bar.scrollLeft; busy = false; }
    });
    wrap.addEventListener('scroll', function() {
      if (!busy) { busy = true; bar.scrollLeft = wrap.scrollLeft; busy = false; }
    });
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
  $(document).on('shiny:value', function(e) {
    if (e.name && e.name.indexOf('pasivos_table') !== -1) setTimeout(init, 200);
  });
})();
", ns("pasivos_tbl_wrap"), ns("pasivos_scroll_bar"), ns("pasivos_scroll_bar_inner"))))
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
                                  error = function(e) sort(unique(unname(COMPANY_MAP))))
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
    .ppm_build_new_prov <- function(new_id, now, user, empresa,
                                    pagar_hoy_id = NA_character_) {
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
        pagar_hoy_id             = pagar_hoy_id,
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
      shinyjs::disable("ppm_save_and_stage")
      on.exit({ shinyjs::enable("ppm_save"); shinyjs::enable("ppm_save_and_stage") }, add = TRUE)

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

      ok <- tryCatch({ save_pasivos_provisions(new_all, client_id = shared$effective_client_id()); TRUE },
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
      bump_sync_version("pasivos_provisions_db")

      tryCatch(pasivos_log_audit(
        action_type = "provision.generated", user = user,
        empresa = empresa, target_kind = "provision", target_id = new_id,
        after = list(id = new_id, origin = "manual", estado = "provisional"),
        notes = "manual orphan provision via Pasivos tab",
        client_id = shared$effective_client_id()
      ), error = function(e) NULL)

      shiny::removeModal()
      shiny::showNotification("Provisión manual creada.", type = "message", duration = 3)
    })

    # Button 2 — Agregar a calendario y Agenda de hoy: save provision first, then
    # stage to pagar_hoy; both calendar and Agenda update in real time.
    shiny::observeEvent(input$ppm_save_and_stage, ignoreInit = TRUE, {
      user    <- tryCatch(shared$current_user(), error = function(e) "")
      empresa <- input$ppm_empresa %||% ""

      if (!has_capability(user, "pasivos.create_provision_manual")) {
        shiny::showNotification("Sin permiso.", type = "error"); return()
      }

      shinyjs::disable("ppm_save")
      shinyjs::disable("ppm_save_and_stage")
      on.exit({ shinyjs::enable("ppm_save"); shinyjs::enable("ppm_save_and_stage") }, add = TRUE)

      new_id    <- uuid::UUIDgenerate()
      now       <- Sys.time()
      new_ph_id <- uuid::UUIDgenerate()

      # empresa is already a full name (dropdown shows unname(company_map))
      # so no translation needed for pagar_hoy.
      doc_val <- trimws(input$ppm_documento %||% "")
      if (!nzchar(doc_val)) doc_val <- paste0("PROV_", new_id)

      # ── Step 1: calendar provision (always first; this is the source of truth) ─
      new_prov <- .ppm_build_new_prov(new_id, now, user, empresa,
                                      pagar_hoy_id = new_ph_id)

      existing <- tryCatch(shared$pasivos_provisions_db(),
                           error = function(e) .schema_pasivos_provision())
      if (is.null(existing) || !is.data.frame(existing))
        existing <- .schema_pasivos_provision()

      new_all <- dplyr::bind_rows(existing, new_prov)

      prov_ok <- tryCatch({ save_pasivos_provisions(new_all, client_id = shared$effective_client_id()); TRUE },
                          error = function(e) {
                            shiny::showNotification(
                              paste0("Error al guardar provisión: ", conditionMessage(e)),
                              type = "error"); FALSE })
      if (!prov_ok) return()

      tryCatch(shared$suppress_ledger_prov_refresh(TRUE), error = function(e) NULL)
      shared$pasivos_provisions_db(new_all)
      bump_sync_version("pasivos_provisions_db")

      tryCatch(pasivos_log_audit(
        action_type = "provision.generated", user = user,
        empresa = empresa, target_kind = "provision", target_id = new_id,
        after = list(id = new_id, origin = "manual", estado = "provisional"),
        notes = "manual orphan provision via Pasivos tab (with agenda staging)",
        client_id = shared$effective_client_id()
      ), error = function(e) NULL)

      # ── Step 2: stage to Agenda de hoy ─────────────────────────────────────
      new_ph_row <- tibble::tibble(
        id           = new_ph_id,
        ledger       = "AP",
        Empresa      = empresa,
        Moneda       = input$ppm_moneda %||% "MXN",
        Documento    = doc_val,
        Parte        = input$ppm_parte  %||% "",
        Codigo       = input$ppm_codigo %||% "",
        tipo_item    = "factura",
        Importe      = as.numeric(input$ppm_importe %||% 0),
        FechaVenc    = as.Date(input$ppm_fecha %||% Sys.Date()),
        staged_by    = user,
        staged_at    = now,
        status       = "pending",
        provision_id = new_id,
        liability_id = NA_character_,
        source       = "provision"
      )

      ph_existing <- tryCatch(shared$pagar_hoy_db(),
                              error = function(e) NULL) %||% load_pagar_hoy(client_id = shared$effective_client_id())
      ph_new <- upsert_pagar_hoy(ph_existing, new_ph_row,
                                 keys = c("ledger", "Empresa", "Moneda", "Documento"))

      shared$pagar_hoy_db(ph_new)
      tryCatch(save_pagar_hoy(ph_new, user, client_id = shared$effective_client_id()), error = function(e)
        warning("[pasivos] save_pagar_hoy failed: ", conditionMessage(e)))

      shiny::removeModal()
      shiny::showNotification(
        "Provisión manual creada e ingresada a Agenda de hoy.",
        type = "message", duration = 4)
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

    # ── Populate choices ────────────────────────────────────────────────────
    shiny::observe({
      liabs   <- tryCatch(shared$pasivos_liabilities_db(), error = function(e) NULL)
      allowed <- tryCatch(shared$visible_initials(), error = function(e) NULL)
      if (!is.null(allowed) && !is.null(liabs) && nrow(liabs) && "empresa" %in% names(liabs))
        liabs <- liabs[liabs$empresa %in% allowed, , drop = FALSE]
      emps <- if (!is.null(liabs) && nrow(liabs) && "empresa" %in% names(liabs))
        sort(unique(liabs$empresa[!is.na(liabs$empresa)])) else character(0)
      shiny::updateSelectizeInput(session, "empresa_sel", choices = emps)
    })

    shiny::observe({
      liabs <- tryCatch(shared$pasivos_liabilities_db(), error = function(e) NULL)
      subs <- if (!is.null(liabs) && nrow(liabs) && "subcategoria" %in% names(liabs))
        sort(unique(liabs$subcategoria[!is.na(liabs$subcategoria) &
                                         nzchar(liabs$subcategoria %||% "")])) else character(0)
      shiny::updateSelectizeInput(session, "subcategoria_sel", choices = subs)
    })

    # ── Filters ─────────────────────────────────────────────────────────────
    filters <- shiny::reactive({
      list(
        empresa        = input$empresa_sel       %||% character(0),
        categorias     = input$categoria_sel     %||% c("regular", "financiero", "tarjeta"),
        subcategorias  = input$subcategoria_sel  %||% character(0),
        currency       = input$currency_sel      %||% "All",
        search         = input$search_text       %||% "",
        granularity    = input$granularity       %||% "day",
        provision_type = input$provision_type    %||% "todas"
      )
    })

    # ── Long-form data ──────────────────────────────────────────────────────
    long_df <- shiny::reactive({
      provs <- tryCatch(shared$pasivos_provisions_db(), error = function(e) NULL)
      liabs <- tryCatch(shared$pasivos_liabilities_db(), error = function(e) NULL)
      mi    <- tryCatch(shared$manual_inv(),             error = function(e) NULL)
      bc    <- tryCatch(shared$bancos_confirmados(),     error = function(e) NULL)

      if (is.null(provs)) provs <- .schema_pasivos_provision()
      if (is.null(liabs)) liabs <- .schema_pasivos_liability()
      if (is.null(mi))    mi    <- data.frame()
      if (is.null(bc))    bc    <- data.frame()

      # Respect group-based company visibility (NULL = no filter; character(0) = none)
      allowed <- tryCatch(shared$visible_initials(), error = function(e) NULL)
      if (!is.null(allowed)) {
        if (nrow(liabs) && "empresa" %in% names(liabs))
          liabs <- liabs[liabs$empresa %in% allowed, , drop = FALSE]
        if (nrow(provs) && "empresa" %in% names(provs))
          provs <- provs[provs$empresa %in% allowed, , drop = FALSE]
        if (nrow(mi) && "empresa" %in% names(mi))
          mi    <- mi[mi$empresa %in% allowed, , drop = FALSE]
      }

      mi_ap <- if (nrow(mi) && "ledger" %in% names(mi))
        mi[mi$ledger == "AP", , drop = FALSE] else mi
      bc_ok <- if (nrow(bc) && "eliminado" %in% names(bc))
        bc[!isTRUE_safe(bc$eliminado), , drop = FALSE] else bc

      tryCatch(
        pasivos_table_build_long(
          provisions         = provs,
          manual_items       = mi_ap,
          bancos_confirmados = bc_ok,
          liabilities        = liabs,
          filters            = filters()
        ),
        error = function(e) {
          message("[pasivos_table] build_long error: ", conditionMessage(e))
          tibble::tibble(
            row_id = character(), row_label = character(), row_empresa = character(),
            row_categoria = character(), row_subcategoria = character(),
            row_parte = character(), row_moneda = character(), fecha = as.Date(character()),
            cell_kind = character(), cell_amount = numeric(), cell_currency = character(),
            cell_provision_id = character(), cell_manual_id = character(),
            cell_bancos_conf_id = character(), cell_has_override = logical(),
            cell_overdue_severity = character()
          )
        }
      )
    })

    # ── Wide form ────────────────────────────────────────────────────────────
    wide <- shiny::reactive({
      w <- vis_window()
      pasivos_table_pivot_wide(
        long_df      = long_df(),
        granularity  = filters()$granularity,
        window_start = w$start,
        window_end   = w$end
      )
    })

    # ── Summary ──────────────────────────────────────────────────────────────
    output$summary_line <- shiny::renderUI({
      ldf <- long_df()
      if (!nrow(ldf))
        return(shiny::tags$span(class = "text-muted", "Sin datos para los filtros seleccionados."))

      ptype   <- filters()$provision_type %||% "todas"
      n_provs <- sum(ldf$cell_kind %in% c("provision", "overdue_provision"), na.rm = TRUE)
      n_items <- sum(ldf$cell_kind %in%
                       c("manual_item", "confirmed_item", "overdue_manual"), na.rm = TRUE)

      summary_text <- if (ptype == "manuales") {
        sprintf("Mostrando %d provisiones manuales · %d items", n_provs, n_items)
      } else {
        n_liabs <- length(unique(ldf$row_id))
        sprintf("Mostrando %d pasivos · %d provisiones · %d items", n_liabs, n_provs, n_items)
      }

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

    # ── Table render ─────────────────────────────────────────────────────────
    output$pasivos_table <- shiny::renderUI({
      liabs <- tryCatch(shared$pasivos_liabilities_db(),
                        error = function(e) .schema_pasivos_liability())
      pasivos_render_table(wide(), today = Sys.Date(), ns = ns,
                           liabilities = liabs)
    })

    invisible(NULL)
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
      shiny::actionButton(ns("ppm_save"),           "Agregar a calendario",
                          class = "btn btn-outline-secondary"),
      shiny::actionButton(ns("ppm_save_and_stage"), "Agregar a calendario y Agenda de hoy",
                          class = "btn btn-primary")
    )
  )
}


# ── pasivos_render_table ───────────────────────────────────────────────────────
# Pure function — builds the tags$table from the wide pivot.
# Stage 4 additions:
#   - Pencil icon in the sticky label cell (fires pasivos_edit_liability globally)
#   - Cell-click div wrapper (fires pasivos_cell_click globally, excluding bolt)
pasivos_render_table <- function(wide, today = Sys.Date(), ns = identity,
                                  liabilities = NULL) {
  meta  <- wide$metadata_cols
  dcols <- wide$date_cols
  cells <- wide$cells

  if (!nrow(meta) || !length(dcols)) {
    return(shiny::div(class = "text-muted p-3",
                      "No hay pasivos para los filtros seleccionados."))
  }

  today_col <- as.character(today)

  # Build a lookup: row_id -> liability_id (for pencil visibility)
  liability_ids <- if (!is.null(liabilities) && nrow(liabilities))
    setNames(liabilities$id, liabilities$id)
  else
    list()

  # ── Header ────────────────────────────────────────────────────────────────
  meta_th <- shiny::tags$th(
    class = "pasivos-meta-col",
    style = "min-width:200px; max-width:360px;",
    "Pasivo"
  )
  date_ths <- lapply(dcols, function(d) {
    cls <- paste(c("text-center",
                   if (d == today_col) "pasivos-table-today-col" else NULL),
                 collapse = " ")
    shiny::tags$th(class = cls, style = "min-width:80px;", d)
  })
  header <- shiny::tags$thead(shiny::tags$tr(meta_th, date_ths))

  # ── Body ─────────────────────────────────────────────────────────────────
  body_rows <- unlist(lapply(seq_len(nrow(meta)), function(r) {
    rid          <- meta$row_id[r]
    row_label    <- meta$row_display_label[r] %||% meta$row_label[r]
    row_cur      <- meta$row_moneda[r] %||% ""
    is_fee_row   <- isTRUE(meta$row_is_fee[r])
    has_ov       <- isTRUE_safe(meta$has_overdue[r])
    has_v_soon   <- !has_ov && isTRUE_safe(meta$has_very_soon[[r]])
    has_soon     <- !has_ov && !has_v_soon && isTRUE_safe(meta$has_due_soon[r])
    tr_cls       <- paste(c("pasivos-data-row",
                            if (is_fee_row)  "pasivos-fee-row"
                            else if (has_ov)     "has-overdue"
                            else if (has_v_soon) "has-very-soon"
                            else if (has_soon)   "has-due-soon"
                            else NULL),
                          collapse = " ")

    # Currency group separator — only before non-fee rows when currency changes
    prev_cur <- if (r > 1L) (meta$row_moneda[r - 1L] %||% "") else ""
    sep_row <- if (!is_fee_row && (r == 1L || !identical(row_cur, prev_cur))) {
      col <- .pasivos_cur_color(row_cur)
      shiny::tags$tr(
        class = "pasivos-currency-group-row",
        shiny::tags$td(
          class = "pasivos-currency-group-cell pasivos-meta-col",
          style = sprintf("border-left: 3px solid %s;", col),
          shiny::tags$span(
            style = sprintf("color:%s; font-size:11px; font-weight:600; letter-spacing:.5px;",
                            col),
            row_cur
          )
        ),
        shiny::tags$td(
          colspan = length(dcols),
          class   = "pasivos-currency-group-cell"
        )
      )
    } else NULL

    if (is_fee_row) {
      # Fee sub-row: indented, smaller, no pencil, no currency pill, no parte
      label_td <- shiny::tags$td(
        class = "pasivos-meta-col",
        style = "max-width:360px; overflow:hidden; text-overflow:ellipsis; padding-left:28px;",
        shiny::div(
          class = "d-flex align-items-center gap-1",
          style = "font-size:11px; color:#6d28d9;",
          shiny::tags$span(style = "opacity:.6; margin-right:2px;", "↳"),
          shiny::tags$span(class = "text-truncate", row_label)
        )
      )
    } else {
      # Pencil icon — only on real liability rows, not orphan provisions
      is_real_liability <- nzchar(rid %||% "") && rid %in% names(liability_ids)
      pencil_html <- if (is_real_liability) {
        shiny::HTML(sprintf(
          '<button class="pasivos-row-edit-btn" title="Editar pasivo" onclick="Shiny.setInputValue(\'pasivos_edit_liability\',\'%s\',{priority:\'event\'})">&#9998;</button>',
          htmltools::htmlEscape(rid, attribute = TRUE)
        ))
      } else NULL

      # Tiny currency pill next to the liability name
      cur_pill <- shiny::HTML(sprintf(
        '<span class="pasivos-cur-pill" style="background:%s;">%s</span>',
        .pasivos_cur_color(row_cur),
        htmltools::htmlEscape(row_cur)
      ))

      label_td <- shiny::tags$td(
        class = "pasivos-meta-col",
        style = "max-width:360px; overflow:hidden; text-overflow:ellipsis;",
        shiny::div(
          class = "d-flex flex-column gap-0",
          shiny::div(
            class = "d-flex align-items-center gap-1",
            pencil_html,
            shiny::tags$span(class = "fw-semibold text-truncate", row_label),
            cur_pill
          ),
          shiny::tags$span(class = "text-muted small text-truncate",
                           meta$row_parte[r] %||% "")
        )
      )
    }

    # Date cells — wrapped in a div that fires pasivos_cell_click (not on bolt)
    date_tds <- lapply(dcols, function(d) {
      cell    <- cells[[rid]][[d]]
      col_cls <- if (d == today_col) "pasivos-table-today-col" else ""
      if (is.null(cell))
        return(shiny::tags$td(class = col_cls, style = "min-width:80px;", ""))
      css_classes <- paste(c(col_cls, pasivos_cell_classes(cell, today)), collapse = " ")
      content <- .pasivos_cell_content(cell)
      prov_id <- cell$cell_provision_id %||% ""
      shiny::tags$td(
        class = css_classes,
        style = "min-width:80px; text-align:right; position:relative;",
        if (nzchar(prov_id)) {
          shiny::div(
            style  = "cursor:pointer;",
            onclick = sprintf(
              "Shiny.setInputValue('pasivos_cell_click','%s',{priority:'event'})",
              htmltools::htmlEscape(prov_id, attribute = TRUE)
            ),
            content
          )
        } else content
      )
    })

    data_row <- shiny::tags$tr(class = tr_cls, label_td, date_tds)
    if (!is.null(sep_row)) list(sep_row, data_row) else list(data_row)
  }), recursive = FALSE)

  shiny::tags$table(
    class = "pasivos-table",
    header,
    shiny::tags$tbody(body_rows)
  )
}


# ── Internal: cell content builder ────────────────────────────────────────────
.pasivos_cell_content <- function(cell) {
  kind   <- cell$cell_kind   %||% ""
  amount <- cell$cell_amount %||% NA_real_
  amt_fmt <- if (!is.na(amount)) fmt_money(amount) else ""

  switch(kind,
    provision = shiny::tagList(
      amt_fmt, " ",
      shiny::HTML(sprintf(
        '<button class="pasivos-convert-btn" title="Convertir provisión" onclick="event.stopPropagation();Shiny.setInputValue(\'pasivos_convert_request\',\'%s\',{priority:\'event\'})">&#x26A1;</button>',
        htmltools::htmlEscape(cell$cell_provision_id %||% "", attribute = TRUE)
      ))
    ),
    overdue_provision = shiny::tagList(
      amt_fmt, " ",
      shiny::HTML(sprintf(
        '<button class="pasivos-convert-btn" title="Provisión vencida — convertir" onclick="event.stopPropagation();Shiny.setInputValue(\'pasivos_convert_request\',\'%s\',{priority:\'event\'})">&#x26A1;</button>',
        htmltools::htmlEscape(cell$cell_provision_id %||% "", attribute = TRUE)
      ))
    ),
    manual_item = {
      prov_id_m <- cell$cell_provision_id %||% ""
      shiny::tagList(
        shiny::tags$span(style = "margin-right:3px;", "◷"), amt_fmt, " ",
        if (nzchar(prov_id_m)) shiny::HTML(sprintf(
          '<button class="pasivos-convert-btn" title="Gestionar comprobante" style="font-size:0.75em;" onclick="event.stopPropagation();Shiny.setInputValue(\'pasivos_convert_request\',\'%s\',{priority:\'event\'})">&#x21A9;</button>',
          htmltools::htmlEscape(prov_id_m, attribute = TRUE)
        )) else NULL
      )
    },
    overdue_manual = {
      prov_id_o <- cell$cell_provision_id %||% ""
      shiny::tagList(
        shiny::tags$span(style = "margin-right:3px;color:#c00;", "!"), amt_fmt, " ",
        if (nzchar(prov_id_o)) shiny::HTML(sprintf(
          '<button class="pasivos-convert-btn" title="Gestionar comprobante (vencido)" style="font-size:0.75em;" onclick="event.stopPropagation();Shiny.setInputValue(\'pasivos_convert_request\',\'%s\',{priority:\'event\'})">&#x21A9;</button>',
          htmltools::htmlEscape(prov_id_o, attribute = TRUE)
        )) else NULL
      )
    },
    confirmed_item = shiny::tagList(shiny::tags$span(style = "margin-right:3px;", "✓"), amt_fmt),
    amt_fmt
  )
}
