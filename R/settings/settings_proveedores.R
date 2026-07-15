# =============================================================================
# R/settings/settings_proveedores.R
# Catálogo de Proveedores UI helpers and observers.
# =============================================================================
# (Originally part of R/settings_module.R; split in Stage 7.1 for session efficiency.)

# =============================================================================
# Catálogo de Proveedores
# =============================================================================
.catalogo_panel_ui <- function(cmap = COMPANY_MAP) {
  div(
    div(class = "d-flex align-items-center gap-2 mb-2",
      tags$h6(class = "fw-semibold mb-0",
              tagList(icon("address-book"), " Catálogo de Proveedores")),
      tags$span(class = "text-muted small",
        "— Mapea el nombre SAP \u2192 alias Baj\u00edo + CLABE para generar archivos PPL")
    ),
    div(class = "d-flex align-items-center gap-2 mb-2",
      div(style = "width: 180px;",
        selectInput("cat_empresa_filter", NULL,
                    choices = c("Todas" = "", setNames(names(cmap), unname(cmap))),
                    selected = "", width = "100%")
      ),
      div(class = "ms-auto",
        actionButton("stg_new_proveedor", tagList(icon("plus"), " Nuevo proveedor"),
                     class = "btn btn-sm btn-outline-primary")
      )
    ),
    div(class = "d-flex gap-3 align-items-center mb-2",
      tags$span(class = "small text-muted", "Estado:"),
      tags$span(class = "ph-legend-dot ph-ok"),
      tags$span(class = "small", "Completo"),
      tags$span(class = "ph-legend-dot ph-partial"),
      tags$span(class = "small text-warning", "Sin CLABE"),
      tags$span(class = "ph-legend-dot ph-nomatch"),
      tags$span(class = "small text-danger", "Sin alias")
    ),
    div(style = "max-height: 300px; overflow-y: auto;",
      DT::dataTableOutput("tbl_catalogo")
    ),
    tags$hr(),
    uiOutput("catalogo_edit_form")
  )
}

.catalogo_datatable <- function(shared, empresa_filter = "") {
  provs <- tryCatch(
    if (!is.null(shared$proveedores_db)) shared$proveedores_db()
    else load_proveedores(),
    error = function(e) tibble::tibble()
  )
  if (!nrow(provs)) {
    return(DT::datatable(
      data.frame(Info = "Sin proveedores. Haz clic en '+ Nuevo proveedor'."),
      options = list(dom = "t"), rownames = FALSE
    ))
  }
  if (nzchar(empresa_filter))
    provs <- provs |> dplyr::filter(Empresa == empresa_filter | Empresa == "")

  disp <- provs |>
    dplyr::mutate(
      Estado = dplyr::case_when(
        nzchar(trimws(alias %||% "")) & nzchar(trimws(clabe %||% "")) &
          clabe != "000000000000000000" ~ "\u2705",
        nzchar(trimws(alias %||% "")) ~ "\u26a0\ufe0f",
        TRUE ~ "\u274c"
      ),
      Alias_disp = dplyr::if_else(
        !is.na(alias) & nzchar(alias),
        paste0(alias, dplyr::if_else(nchar(alias) > 12,
          paste0(" <span class='text-warning small'>(", nchar(alias), "/15)</span>"), "")),
        "<span class='text-muted fst-italic small'>sin alias</span>"
      ),
      CLABE_disp = dplyr::if_else(
        !is.na(clabe) & nzchar(clabe) & clabe != "000000000000000000",
        paste0('<span class="font-monospace small">', clabe, '</span>'),
        "<span class='text-muted fst-italic small'>sin CLABE</span>"
      )
    ) |>
    dplyr::select(Estado, Empresa, `Nombre SAP` = nombre, Alias = Alias_disp,
                  CLABE = CLABE_disp, Medio = medio_pago, RFC = rfc)

  DT::datatable(disp, escape = FALSE, rownames = FALSE, selection = "single",
    options = list(pageLength = 15, dom = "ftp", scrollX = TRUE,
      columnDefs = list(
        list(width = "28px",  targets = 0), list(width = "55px",  targets = 1),
        list(width = "200px", targets = 2), list(width = "110px", targets = 3),
        list(width = "160px", targets = 4), list(width = "50px",  targets = 5),
        list(width = "100px", targets = 6)
      )
    )
  )
}

settings_catalogo_edit_observer <- function(input, output, session, shared) {
  editing_id <- reactiveVal(NULL)

  observeEvent(input$tbl_catalogo_rows_selected, {
    sel <- input$tbl_catalogo_rows_selected
    if (!length(sel)) {
      editing_id(NULL)
      output$catalogo_edit_form <- renderUI({ .catalogo_form_empty() }); return()
    }
    provs <- tryCatch(
      if (!is.null(shared$proveedores_db)) shared$proveedores_db() else load_proveedores(),
      error = function(e) tibble::tibble()
    )
    ef <- isolate(input$cat_empresa_filter %||% "")
    if (nzchar(ef)) provs <- provs |> dplyr::filter(Empresa == ef | Empresa == "")
    if (!nrow(provs) || sel > nrow(provs)) return()
    row <- provs[sel, ]
    editing_id(row$id)
    output$catalogo_edit_form <- renderUI({ .catalogo_form_ui(row, cmap = shared$company_map()) })
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  observeEvent(input$stg_new_proveedor, {
    editing_id(NULL)
    output$catalogo_edit_form <- renderUI({ .catalogo_form_ui(NULL, cmap = shared$company_map()) })
  }, ignoreInit = TRUE)

  observeEvent(input$cat_save_row, {
    eid       <- editing_id()
    provs     <- tryCatch(load_proveedores(), error = function(e) .schema_proveedores_local())
    alias_val <- trimws(input$cat_alias %||% "")
    if (nchar(alias_val) > 15) {
      showNotification("El alias no puede tener m\u00e1s de 15 caracteres.", type = "warning")
      return()
    }
    clabe_val <- trimws(input$cat_clabe %||% "")
    medio_val <- input$cat_medio %||% "SPI"
    if (medio_val == "SPI" && nzchar(clabe_val) && nchar(clabe_val) != 18) {
      showNotification("La CLABE debe tener exactamente 18 d\u00edgitos.", type = "warning")
      return()
    }
    new_row <- tibble::tibble(
      id = if (is.null(eid)) uuid::UUIDgenerate() else eid,
      Empresa = input$cat_empresa %||% "", codigo = trimws(input$cat_codigo %||% ""),
      nombre = trimws(input$cat_nombre %||% ""), alias = alias_val,
      clabe = clabe_val, medio_pago = medio_val, rfc = trimws(input$cat_rfc %||% ""),
      tipo = if (medio_val == "BCO") "021" else "012",
      banco_destino = NA_character_, activo = TRUE
    )
    if (is.null(eid)) {
      dup <- provs |> dplyr::filter(
        Empresa == new_row$Empresa, toupper(alias) == toupper(alias_val), activo == TRUE)
      if (nrow(dup)) {
        showNotification(paste0("El alias '", alias_val, "' ya existe."), type = "warning")
        return()
      }
      updated <- dplyr::bind_rows(provs, new_row)
    } else {
      updated <- provs |> dplyr::rows_update(new_row, by = "id", unmatched = "ignore")
    }
    save_proveedores(updated, client_id = shared$active_client_id())
    bump_sync_version("proveedores_db")
    if (!is.null(shared$proveedores_db)) shared$proveedores_db(updated)
    editing_id(NULL)
    output$catalogo_edit_form <- renderUI({ .catalogo_form_empty() })
    output$tbl_catalogo <- DT::renderDataTable({
      .catalogo_datatable(shared, input$cat_empresa_filter %||% "")
    }, server = TRUE)
    showNotification(
      if (is.null(eid)) "Proveedor agregado." else "Proveedor actualizado.",
      type = "message", duration = 2)
  }, ignoreInit = TRUE)

  observeEvent(input$cat_delete_row, {
    eid <- editing_id(); if (is.null(eid)) return()
    provs   <- tryCatch(load_proveedores(), error = function(e) .schema_proveedores_local())
    updated <- provs |> dplyr::mutate(activo = dplyr::if_else(id == eid, FALSE, activo))
    save_proveedores(updated, client_id = shared$active_client_id())
    bump_sync_version("proveedores_db")
    if (!is.null(shared$proveedores_db)) shared$proveedores_db(updated)
    editing_id(NULL)
    output$catalogo_edit_form <- renderUI({ .catalogo_form_empty() })
    output$tbl_catalogo <- DT::renderDataTable({
      .catalogo_datatable(shared, input$cat_empresa_filter %||% "")
    }, server = TRUE)
    showNotification("Proveedor eliminado.", type = "message", duration = 2)
  }, ignoreInit = TRUE)
}

.catalogo_form_empty <- function() {
  div(class = "text-muted small fst-italic pt-1",
    icon("hand-pointer"),
    " Selecciona un proveedor para editarlo, o haz clic en '+ Nuevo proveedor'.")
}

.catalogo_form_ui <- function(row = NULL, cmap = COMPANY_MAP) {
  is_new  <- is.null(row)
  emp_val <- if (!is_new) row$Empresa    %||% "" else ""
  nom_val <- if (!is_new) row$nombre     %||% "" else ""
  ali_val <- if (!is_new) row$alias      %||% "" else ""
  cla_val <- if (!is_new) row$clabe      %||% "" else ""
  med_val <- if (!is_new) row$medio_pago %||% "SPI" else "SPI"
  rfc_val <- if (!is_new) row$rfc        %||% "" else ""
  cod_val <- if (!is_new) row$codigo     %||% "" else ""

  div(class = "catalogo-form border rounded p-3 bg-light mt-2",
    tags$h6(class = "fw-semibold mb-3",
      if (is_new) tagList(icon("plus"), " Nuevo proveedor")
      else        tagList(icon("pencil"), " Editar proveedor")),
    div(class = "row g-2",
      div(class = "col-md-3",
        tags$label("Empresa", class = "form-label small fw-semibold mb-1"),
        selectInput("cat_empresa", NULL,
          choices = c("Todas" = "", setNames(names(cmap), unname(cmap))),
          selected = emp_val, width = "100%")
      ),
      div(class = "col-md-9",
        tags$label(tagList("Nombre SAP",
          tags$span(class = "text-muted fw-normal", " \u2014 exactamente como aparece en CxP")),
          class = "form-label small fw-semibold mb-1"),
        textInput("cat_nombre", NULL, value = nom_val, width = "100%",
                  placeholder = "TRANSPORTES FICTICIOS SA DE CV")
      ),
      div(class = "col-md-4",
        tags$label(tagList("Alias Baj\u00edo",
          tags$span(class = "badge bg-primary ms-1", "m\u00e1x 15")),
          class = "form-label small fw-semibold mb-1"),
        textInput("cat_alias", NULL, value = ali_val, width = "100%",
                  placeholder = "TRANSFICTICIAS"),
        uiOutput("cat_alias_counter")
      ),
      div(class = "col-md-3",
        tags$label("Medio de pago", class = "form-label small fw-semibold mb-1"),
        selectInput("cat_medio", NULL, choices = c("SPI","BCO","TEF","SPD"),
                    selected = med_val, width = "100%")
      ),
      div(class = "col-md-5",
        tags$label(tagList("CLABE / Cuenta destino",
          tags$span(class = "text-muted fw-normal", " \u2014 18 d\u00edgitos")),
          class = "form-label small fw-semibold mb-1"),
        textInput("cat_clabe", NULL, value = cla_val, width = "100%",
                  placeholder = "012310098765432101")
      ),
      div(class = "col-md-4",
        tags$label("RFC", class = "form-label small fw-semibold mb-1"),
        textInput("cat_rfc", NULL, value = rfc_val, width = "100%",
                  placeholder = "TFS850312HX1")
      ),
      div(class = "col-md-4",
        tags$label("C\u00f3digo SAP", class = "form-label small fw-semibold mb-1"),
        textInput("cat_codigo", NULL, value = cod_val, width = "100%",
                  placeholder = "V00456")
      )
    ),
    div(class = "d-flex gap-2 mt-3",
      actionButton("cat_save_row",
        tagList(icon("floppy-disk"), if (is_new) " Agregar" else " Guardar cambios"),
        class = "btn btn-primary btn-sm"),
      if (!is_new)
        actionButton("cat_delete_row", tagList(icon("trash"), " Eliminar"),
                     class = "btn btn-outline-danger btn-sm") else NULL,
      actionButton("cat_cancel_edit", "Cancelar",
                   class = "btn btn-outline-secondary btn-sm")
    )
  )
}

settings_alias_counter <- function(input, output, session) {
  output$cat_alias_counter <- renderUI({
    val <- trimws(input$cat_alias %||% "")
    n   <- nchar(val)
    if (n == 0) return(NULL)
    cls <- if (n > 15) "text-danger fw-bold" else if (n > 12) "text-warning" else "text-success"
    tags$span(class = paste("small", cls), paste0(n, " / 15 caracteres"))
  })
}

settings_cancel_edit_observer <- function(input, output, session) {
  observeEvent(input$cat_cancel_edit, {
    output$catalogo_edit_form <- renderUI({ .catalogo_form_empty() })
  }, ignoreInit = TRUE)
}

.schema_proveedores_local <- function() tibble::tibble(
  id = character(), Empresa = character(), codigo = character(),
  nombre = character(), alias = character(), clabe = character(),
  medio_pago = character(), rfc = character(), tipo = character(),
  banco_destino = character(), activo = logical()
)

