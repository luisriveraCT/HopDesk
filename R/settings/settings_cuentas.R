# =============================================================================
# R/settings/settings_cuentas.R
# Cuentas de Empresa UI helpers and settings_cuentas_observer().
# =============================================================================
# (Originally part of R/settings_module.R; split in Stage 7.1 for session efficiency.)

# =============================================================================
# Cuentas de Empresa
# =============================================================================
.cuentas_panel_ui <- function(cmap = COMPANY_MAP) {
  div(
    div(class = "d-flex align-items-center gap-2 mb-2",
      tags$h6(class = "fw-semibold mb-0",
        tagList(icon("building-columns"), " Cuentas de Empresa")),
      tags$span(class = "text-muted small",
        "— La cuenta marcada ",
        tags$span(class = "badge bg-primary", "PPL"),
        " se usa como cuenta origen al generar archivos Baj\u00edo.")
    ),

    div(class = "row g-3",

      # ── Banco catalog (left) ─────────────────────────────────────────────
      div(class = "col-md-4",
        div(class = "card h-100",
          div(class = "card-header d-flex align-items-center py-2",
            tags$span(class = "fw-semibold small flex-grow-1",
                      tagList(icon("university"), " Bancos")),
            actionButton("cta_new_banco", tagList(icon("plus"), " Banco"),
                         class = "btn btn-sm btn-outline-primary py-0 px-2")
          ),
          div(class = "card-body p-0", style = "max-height: 260px; overflow-y: auto;",
            DT::dataTableOutput("tbl_bancos_cat")
          )
        )
      ),

      # ── Accounts (right) ─────────────────────────────────────────────────
      div(class = "col-md-8",
        div(class = "card h-100",
          div(class = "card-header d-flex align-items-center gap-2 py-2",
            tags$span(class = "fw-semibold small", tagList(icon("credit-card"), " Cuentas")),
            div(class = "ms-auto d-flex gap-2",
              div(style = "width:175px;",
                selectInput("cta_empresa_filter", NULL,
                  choices = c("Todas las empresas" = "",
                              setNames(names(cmap), unname(cmap))),
                  width = "100%")
              ),
              actionButton("cta_new_cuenta", tagList(icon("plus"), " Cuenta"),
                           class = "btn btn-sm btn-outline-primary")
            )
          ),
          div(class = "card-body p-0", style = "max-height: 260px; overflow-y: auto;",
            DT::dataTableOutput("tbl_cuentas")
          )
        )
      )
    ),

    tags$hr(class = "my-3"),
    uiOutput("cuentas_edit_form")
  )
}

.bancos_cat_datatable <- function(client_id = NULL) {
  bancos <- tryCatch(load_ctas_bancos(client_id = client_id), error = function(e) .schema_ctas_bancos())
  if (!nrow(bancos)) {
    return(DT::datatable(
      data.frame(Info = "Sin bancos. Haz clic en '+ Banco'."),
      options = list(dom = "t"), rownames = FALSE
    ))
  }
  DT::datatable(
    bancos |> dplyr::select(Banco = nombre, Clave = clave),
    escape = FALSE, rownames = FALSE, selection = "single",
    options = list(dom = "t", pageLength = 30,
      columnDefs = list(list(width = "55px", targets = 1)))
  )
}

.cuentas_datatable <- function(shared, empresa_filter = "", client_id = NULL) {
  ctas <- tryCatch({
    if (!is.null(shared$ctas_cuentas)) shared$ctas_cuentas()
    else load_ctas_cuentas()
  }, error = function(e) .schema_ctas_cuentas())
  bancos <- tryCatch(load_ctas_bancos(client_id = client_id), error = function(e) .schema_ctas_bancos())

  if (!nrow(ctas)) {
    return(DT::datatable(
      data.frame(Info = "Sin cuentas. Haz clic en '+ Cuenta'."),
      options = list(dom = "t"), rownames = FALSE
    ))
  }
  if (nzchar(empresa_filter)) ctas <- ctas |> dplyr::filter(Empresa == empresa_filter)
  ctas <- ctas |> dplyr::filter(activa == TRUE)
  if (!nrow(ctas)) {
    return(DT::datatable(
      data.frame(Info = "Sin cuentas activas para esa empresa."),
      options = list(dom = "t"), rownames = FALSE
    ))
  }

  if (nrow(bancos)) {
    ctas <- ctas |>
      dplyr::left_join(bancos |> dplyr::select(banco_id = id, banco_nombre = nombre),
                       by = "banco_id")
  } else { ctas$banco_nombre <- NA_character_ }

  disp <- ctas |> dplyr::mutate(
    PPL = dplyr::if_else(isTRUE(is_ppl_default),
                         "<span class='badge bg-primary'>PPL</span>", ""),
    Cuenta_disp = paste0('<span class="font-monospace small">', cuenta, '</span>'),
    Alias_disp  = dplyr::if_else(
      !is.na(alias) & nzchar(trimws(alias %||% "")), alias,
      "<span class='text-muted fst-italic small'>\u2014</span>")
  ) |> dplyr::select(
    PPL, Empresa, Banco = banco_nombre, Moneda,
    Alias = Alias_disp, `Cuenta origen` = Cuenta_disp, `Razon Social` = razon_social
  )

  DT::datatable(
    disp, escape = FALSE, rownames = FALSE, selection = "single",
    options = list(dom = "ftp", pageLength = 10, scrollX = TRUE,
      columnDefs = list(
        list(width = "40px",  targets = 0), list(width = "50px",  targets = 1),
        list(width = "90px",  targets = 2), list(width = "50px",  targets = 3),
        list(width = "90px",  targets = 4), list(width = "140px", targets = 5)
      )
    )
  )
}

# ── Cuentas observer (called once from app.R server) ──────────────────────────
settings_cuentas_observer <- function(input, output, session, shared) {

  editing_cuenta_id <- reactiveVal(NULL)
  editing_banco_id  <- reactiveVal(NULL)

  output$tbl_bancos_cat <- DT::renderDataTable({
    .bancos_cat_datatable(client_id = shared$effective_client_id())
  }, server = TRUE)

  output$tbl_cuentas <- DT::renderDataTable({
    .cuentas_datatable(shared, isolate(input$cta_empresa_filter %||% ""), client_id = shared$effective_client_id())
  }, server = TRUE)

  observeEvent(input$cta_empresa_filter, {
    output$tbl_cuentas <- DT::renderDataTable({
      .cuentas_datatable(shared, input$cta_empresa_filter %||% "", client_id = shared$effective_client_id())
    }, server = TRUE)
  }, ignoreInit = TRUE)

  # ── + Banco ──────────────────────────────────────────────────────────────────
  observeEvent(input$cta_new_banco, {
    editing_banco_id(NULL); editing_cuenta_id(NULL)
    output$cuentas_edit_form <- renderUI({ .banco_form_ui(NULL) })
  }, ignoreInit = TRUE)

  # ── Select banco ─────────────────────────────────────────────────────────────
  observeEvent(input$tbl_bancos_cat_rows_selected, {
    sel <- input$tbl_bancos_cat_rows_selected
    if (!length(sel)) {
      editing_banco_id(NULL)
      output$cuentas_edit_form <- renderUI({ .cuentas_form_empty() }); return()
    }
    bancos <- tryCatch(load_ctas_bancos(client_id = shared$effective_client_id()), error = function(e) .schema_ctas_bancos())
    if (!nrow(bancos) || sel > nrow(bancos)) return()
    row <- bancos[sel, ]
    editing_banco_id(row$id); editing_cuenta_id(NULL)
    output$cuentas_edit_form <- renderUI({ .banco_form_ui(row) })
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # ── Save banco ────────────────────────────────────────────────────────────────
  observeEvent(input$banco_save, {
    eid    <- editing_banco_id()
    nombre <- trimws(input$banco_nombre %||% "")
    clave  <- trimws(input$banco_clave  %||% "")
    if (!nzchar(nombre)) {
      showNotification("El nombre del banco es obligatorio.", type = "warning"); return()
    }
    bancos  <- tryCatch(load_ctas_bancos(client_id = shared$effective_client_id()), error = function(e) .schema_ctas_bancos())
    new_row <- tibble::tibble(
      id = if (is.null(eid)) uuid::UUIDgenerate() else eid,
      nombre = nombre, clave = clave
    )
    updated <- if (is.null(eid)) dplyr::bind_rows(bancos, new_row)
               else bancos |> dplyr::rows_update(new_row, by = "id", unmatched = "ignore")
    save_ctas_bancos(updated, client_id = shared$effective_client_id())
    editing_banco_id(NULL)
    output$cuentas_edit_form <- renderUI({ .cuentas_form_empty() })
    output$tbl_bancos_cat <- DT::renderDataTable({ .bancos_cat_datatable(client_id = shared$effective_client_id()) }, server = TRUE)
    showNotification(if (is.null(eid)) "Banco agregado." else "Banco actualizado.",
                     type = "message", duration = 2)
  }, ignoreInit = TRUE)

  # ── Delete banco ──────────────────────────────────────────────────────────────
  observeEvent(input$banco_delete, {
    eid <- editing_banco_id(); if (is.null(eid)) return()
    bancos  <- tryCatch(load_ctas_bancos(client_id = shared$effective_client_id()), error = function(e) .schema_ctas_bancos())
    updated <- bancos |> dplyr::filter(id != eid)
    save_ctas_bancos(updated, client_id = shared$effective_client_id())
    editing_banco_id(NULL)
    output$cuentas_edit_form <- renderUI({ .cuentas_form_empty() })
    output$tbl_bancos_cat <- DT::renderDataTable({ .bancos_cat_datatable(client_id = shared$effective_client_id()) }, server = TRUE)
    showNotification("Banco eliminado.", type = "message", duration = 2)
  }, ignoreInit = TRUE)

  # ── + Cuenta ──────────────────────────────────────────────────────────────────
  observeEvent(input$cta_new_cuenta, {
    editing_cuenta_id(NULL); editing_banco_id(NULL)
    sel_b  <- input$tbl_bancos_cat_rows_selected
    bancos <- tryCatch(load_ctas_bancos(client_id = shared$effective_client_id()), error = function(e) .schema_ctas_bancos())
    pre_b  <- if (length(sel_b) && nrow(bancos) >= sel_b) bancos$id[sel_b] else NULL
    pre_e  <- isolate(input$cta_empresa_filter %||% "")
    output$cuentas_edit_form <- renderUI({
      .cuenta_form_ui(NULL, bancos, pre_b, pre_e, cmap = shared$company_map())
    })
  }, ignoreInit = TRUE)

  # ── Select cuenta ─────────────────────────────────────────────────────────────
  observeEvent(input$tbl_cuentas_rows_selected, {
    sel <- input$tbl_cuentas_rows_selected
    if (!length(sel)) {
      editing_cuenta_id(NULL)
      output$cuentas_edit_form <- renderUI({ .cuentas_form_empty() }); return()
    }
    ctas <- tryCatch({
      if (!is.null(shared$ctas_cuentas)) shared$ctas_cuentas() else load_ctas_cuentas()
    }, error = function(e) .schema_ctas_cuentas())
    ef <- isolate(input$cta_empresa_filter %||% "")
    if (nzchar(ef)) ctas <- ctas |> dplyr::filter(Empresa == ef)
    ctas <- ctas |> dplyr::filter(activa == TRUE)
    if (!nrow(ctas) || sel > nrow(ctas)) return()
    row    <- ctas[sel, ]
    bancos <- tryCatch(load_ctas_bancos(client_id = shared$effective_client_id()), error = function(e) .schema_ctas_bancos())
    editing_cuenta_id(row$id); editing_banco_id(NULL)
    output$cuentas_edit_form <- renderUI({
      .cuenta_form_ui(row, bancos, row$banco_id, ef, cmap = shared$company_map())
    })
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # ── Save cuenta ───────────────────────────────────────────────────────────────
  observeEvent(input$cuenta_save, {
    shinyjs::disable("cuenta_save")
    on.exit(shinyjs::enable("cuenta_save"), add = TRUE)

    eid         <- editing_cuenta_id()
    empresa_val <- input$cta_empresa %||% ""
    cuenta_val  <- trimws(input$cta_cuenta %||% "")
    moneda_val  <- input$cta_moneda  %||% "MXN"

    if (!nzchar(empresa_val)) {
      showNotification("Selecciona una empresa.", type = "warning"); return()
    }
    if (!nzchar(cuenta_val)) {
      showNotification("El n\u00famero de cuenta es obligatorio.", type = "warning"); return()
    }

    ctas       <- tryCatch({
      if (!is.null(shared$ctas_cuentas)) shared$ctas_cuentas() else load_ctas_cuentas()
    }, error = function(e) .schema_ctas_cuentas())
    is_default <- isTRUE(input$cta_is_ppl_default)

    # Clear PPL default flag from sibling accounts (same empresa + moneda)
    if (is_default) {
      if (!"is_ppl_default" %in% names(ctas)) ctas$is_ppl_default <- FALSE
      mask <- !is.na(ctas$Empresa) & ctas$Empresa == empresa_val &
              !is.na(ctas$Moneda)  & toupper(ctas$Moneda) == toupper(moneda_val) &
              (is.null(eid) | (!is.na(ctas$id) & ctas$id != eid))
      ctas$is_ppl_default[mask] <- FALSE
    }

    new_row <- tibble::tibble(
      id                  = if (is.null(eid)) uuid::UUIDgenerate() else eid,
      banco_id            = input$cta_banco_id %||% NA_character_,
      Empresa             = empresa_val,
      razon_social        = trimws(input$cta_razon_social %||% ""),
      rfc                 = trimws(input$cta_rfc          %||% ""),
      Moneda              = toupper(moneda_val),
      alias               = trimws(substr(input$cta_alias %||% "", 1, 15)),
      cuenta              = cuenta_val,
      clabe_interbancaria = trimws(input$cta_clabe        %||% ""),
      is_ppl_default      = is_default,
      saldo_inicial       = as.numeric(input$cta_saldo_inicial %||% 0),
      activa              = TRUE
    )

    updated <- if (is.null(eid)) dplyr::bind_rows(ctas, new_row)
               else dplyr::bind_rows(ctas |> dplyr::filter(id != eid), new_row)

    save_ctas_cuentas(updated, client_id = shared$effective_client_id())
    bump_sync_version("ctas_cuentas")
    if (!is.null(shared$ctas_cuentas)) shared$ctas_cuentas(updated)

    editing_cuenta_id(NULL)
    output$cuentas_edit_form <- renderUI({ .cuentas_form_empty() })
    output$tbl_cuentas <- DT::renderDataTable({
      .cuentas_datatable(shared, isolate(input$cta_empresa_filter %||% ""), client_id = shared$effective_client_id())
    }, server = TRUE)
    showNotification(
      if (is.null(eid)) "Cuenta agregada." else "Cuenta actualizada.",
      type = "message", duration = 2)
  }, ignoreInit = TRUE)

  # ── Delete cuenta ─────────────────────────────────────────────────────────────
  observeEvent(input$cuenta_delete, {
    eid <- editing_cuenta_id(); if (is.null(eid)) return()
    ctas <- tryCatch({
      if (!is.null(shared$ctas_cuentas)) shared$ctas_cuentas() else load_ctas_cuentas()
    }, error = function(e) .schema_ctas_cuentas())
    updated <- ctas |> dplyr::mutate(
      activa = dplyr::if_else(id == eid, FALSE, activa))
    save_ctas_cuentas(updated, client_id = shared$effective_client_id())
    bump_sync_version("ctas_cuentas")
    if (!is.null(shared$ctas_cuentas)) shared$ctas_cuentas(updated)
    editing_cuenta_id(NULL)
    output$cuentas_edit_form <- renderUI({ .cuentas_form_empty() })
    output$tbl_cuentas <- DT::renderDataTable({
      .cuentas_datatable(shared, isolate(input$cta_empresa_filter %||% ""), client_id = shared$effective_client_id())
    }, server = TRUE)
    showNotification("Cuenta desactivada.", type = "message", duration = 2)
  }, ignoreInit = TRUE)

  # ── Cancel ────────────────────────────────────────────────────────────────────
  observeEvent(input$cta_cancel, {
    editing_cuenta_id(NULL); editing_banco_id(NULL)
    output$cuentas_edit_form <- renderUI({ .cuentas_form_empty() })
  }, ignoreInit = TRUE)
}

# ── Empty placeholder ─────────────────────────────────────────────────────────
.cuentas_form_empty <- function() {
  div(class = "text-muted small fst-italic",
    icon("hand-pointer"),
    " Selecciona un banco o cuenta para editar, o usa los botones + para agregar.")
}

# ── Bank form ─────────────────────────────────────────────────────────────────
.banco_form_ui <- function(row = NULL) {
  is_new  <- is.null(row)
  nom_val <- if (!is_new) row$nombre %||% "" else ""
  cla_val <- if (!is_new) row$clave  %||% "" else ""

  div(class = "catalogo-form border rounded p-3 bg-light",
    tags$h6(class = "fw-semibold mb-3",
      if (is_new) tagList(icon("plus"), " Agregar banco")
      else        tagList(icon("pencil"), " Editar banco")),

    div(class = "row g-2",
      div(class = "col-12 mb-1",
        tags$label("Selecci\u00f3n r\u00e1pida", class = "form-label small text-muted mb-1"),
        selectInput("banco_picker", NULL,
                    choices = .BANCOS_MX_CHOICES, selected = "", width = "270px")
      ),
      div(class = "col-md-7",
        tags$label("Nombre del banco", class = "form-label small fw-semibold mb-1"),
        textInput("banco_nombre", NULL, value = nom_val, width = "100%",
                  placeholder = "BanBaj\u00edo")
      ),
      div(class = "col-md-3",
        tags$label("Clave CECOBAN", class = "form-label small fw-semibold mb-1"),
        textInput("banco_clave", NULL, value = cla_val, width = "100%",
                  placeholder = "030")
      )
    ),

    # Autofill: picker value is "Nombre|clave"; JS splits and fills the two inputs
    tags$script(HTML("
      $(document).on('change', '#banco_picker', function() {
        var parts = $(this).val().split('|');
        if (parts.length === 2 && parts[0] !== '' && parts[0] !== 'Otro') {
          $('#banco_nombre').val(parts[0]);
          $('#banco_clave' ).val(parts[1]);
          Shiny.setInputValue('banco_nombre', parts[0], {priority:'event'});
          Shiny.setInputValue('banco_clave',  parts[1], {priority:'event'});
        }
      });
    ")),

    div(class = "d-flex gap-2 mt-3",
      actionButton("banco_save",
        tagList(icon("floppy-disk"), if (is_new) " Agregar banco" else " Guardar"),
        class = "btn btn-primary btn-sm"),
      if (!is_new)
        actionButton("banco_delete", tagList(icon("trash"), " Eliminar"),
                     class = "btn btn-outline-danger btn-sm") else NULL,
      actionButton("cta_cancel", "Cancelar", class = "btn btn-outline-secondary btn-sm")
    )
  )
}

# ── Account form ──────────────────────────────────────────────────────────────
.cuenta_form_ui <- function(row = NULL, bancos = NULL, pre_banco_id = NULL,
                             pre_empresa = "", cmap = COMPANY_MAP) {
  is_new <- is.null(row)

  banco_choices <- if (!is.null(bancos) && nrow(bancos)) {
    setNames(bancos$id, bancos$nombre)
  } else { c("(agrega un banco primero)" = "") }

  sel_banco   <- if (!is_new) row$banco_id %||% "" else pre_banco_id %||% ""
  emp_val     <- if (!is_new) row$Empresa  %||% "" else pre_empresa
  razon_val   <- if (!is_new) row$razon_social %||% "" else ""
  rfc_val     <- if (!is_new) row$rfc  %||% "" else ""
  moneda_val  <- if (!is_new) row$Moneda   %||% "MXN" else "MXN"
  alias_val   <- if (!is_new) row$alias    %||% "" else ""
  cuenta_val  <- if (!is_new) row$cuenta   %||% "" else ""
  clabe_val   <- if (!is_new) row$clabe_interbancaria %||% "" else ""
  default_val <- if (!is_new) isTRUE(row$is_ppl_default) else TRUE  # default TRUE for convenience

  razon_ph <- if (nzchar(emp_val) && emp_val %in% names(cmap))
    unname(cmap[emp_val]) else "Empresa Modelo SA de CV"

  div(class = "catalogo-form border rounded p-3 bg-light",
    tags$h6(class = "fw-semibold mb-3",
      if (is_new) tagList(icon("plus"), " Nueva cuenta bancaria")
      else        tagList(icon("pencil"), " Editar cuenta bancaria")),

    div(class = "row g-2",

      # Row 1: Empresa + Banco + Moneda + PPL checkbox
      div(class = "col-md-3",
        tags$label("Empresa", class = "form-label small fw-semibold mb-1"),
        selectInput("cta_empresa", NULL,
          choices  = c("\u2014 seleccionar \u2014" = "",
                       setNames(names(cmap), unname(cmap))),
          selected = emp_val, width = "100%")
      ),
      div(class = "col-md-4",
        tags$label("Banco", class = "form-label small fw-semibold mb-1"),
        selectInput("cta_banco_id", NULL,
                    choices = banco_choices, selected = sel_banco, width = "100%")
      ),
      div(class = "col-md-2",
        tags$label("Moneda", class = "form-label small fw-semibold mb-1"),
        selectInput("cta_moneda", NULL,
                    choices = c("MXN","USD","EUR"), selected = moneda_val, width = "100%")
      ),
      div(class = "col-md-3 d-flex align-items-end pb-1",
        div(class = "form-check",
          tags$input(
            type  = "checkbox", class = "form-check-input",
            id    = "cta_is_ppl_default",
            if (default_val) list(checked = "checked") else list()
          ),
          tags$label(class = "form-check-label small fw-semibold",
                     `for` = "cta_is_ppl_default",
            tagList(tags$span(class = "badge bg-primary me-1", "PPL"),
                    "usar como origen")
          )
        )
      ),

      # Row 2: Cuenta origen + CLABE interbancaria + Alias
      div(class = "col-md-4",
        tags$label(
          tagList("Cuenta origen",
            tags$span(class = "text-muted fw-normal", " \u2014 11 d\u00edgitos")),
          class = "form-label small fw-semibold mb-1"),
        textInput("cta_cuenta", NULL, value = cuenta_val, width = "100%",
                  placeholder = "08153004987")
      ),
      div(class = "col-md-5",
        tags$label(
          tagList("CLABE interbancaria",
            tags$span(class = "text-muted fw-normal", " \u2014 18 d\u00edgitos")),
          class = "form-label small fw-semibold mb-1"),
        textInput("cta_clabe", NULL, value = clabe_val, width = "100%",
                  placeholder = "030310012345678901")
      ),
      div(class = "col-md-3",
        tags$label("Alias interno", class = "form-label small fw-semibold mb-1"),
        textInput("cta_alias", NULL, value = alias_val, width = "100%",
                  placeholder = "EMP-MXN")
      ),

      # Row 3: Saldo inicial + Razón social + RFC
      div(class = "col-md-3",
        tags$label(
          tagList("Saldo inicial",
            tags$span(class = "text-muted fw-normal", " \u2014 apertura")),
          class = "form-label small fw-semibold mb-1"),
        numericInput("cta_saldo_inicial", NULL,
                     value = if (!is_new) (row$saldo_inicial %||% 0) else 0,
                     min = 0, step = 1000, width = "100%")
      ),
      div(class = "col-md-6",
        tags$label("Raz\u00f3n Social", class = "form-label small fw-semibold mb-1"),
        textInput("cta_razon_social", NULL, value = razon_val, width = "100%",
                  placeholder = razon_ph)
      ),
      div(class = "col-md-3",
        tags$label("RFC", class = "form-label small fw-semibold mb-1"),
        textInput("cta_rfc", NULL, value = rfc_val, width = "100%",
                  placeholder = "EMP850101HX1")
      )
    ),

    div(class = "small text-muted mt-2 border-top pt-2",
      icon("circle-info"),
      " La ", tags$strong("Cuenta origen"), " es el n\u00famero de 12 d\u00edgitos en el ",
      "campo origen del archivo PPL (ej: ", tags$code("08153004987"),
      "). La CLABE interbancaria es el n\u00famero de 18 d\u00edgitos para recibir transferencias SPEI."
    ),

    div(class = "d-flex gap-2 mt-3",
      actionButton("cuenta_save",
        tagList(icon("floppy-disk"), if (is_new) " Agregar cuenta" else " Guardar cambios"),
        class = "btn btn-primary btn-sm"),
      if (!is_new)
        actionButton("cuenta_delete", tagList(icon("trash"), " Desactivar"),
                     class = "btn btn-outline-danger btn-sm") else NULL,
      actionButton("cta_cancel", "Cancelar", class = "btn btn-outline-secondary btn-sm")
    )
  )
}


