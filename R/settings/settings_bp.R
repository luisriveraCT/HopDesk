# =============================================================================
# R/settings/settings_bp.R
# Business Partners panel helpers.
# =============================================================================
# (Originally part of R/settings_module.R; split in Stage 7.1 for session efficiency.)

# =============================================================================
# Business Partners
# =============================================================================
.bp_panel_ui <- function(shared, candidates) {
  div(
    # ── Section 1: SAP Business Partners ────────────────────────────────────
    div(class = "d-flex align-items-center gap-2 mb-2",
      tags$h6(class = "fw-semibold mb-0",
              tagList(icon("building"), " Business Partners (SAP)")),
      tags$span(class = "text-muted small",
        "— C\u00f3digos \u00fanicos extra\u00eddos de los snapshots de facturas"),
      div(class = "ms-auto",
        actionButton("bp_refresh_btn", tagList(icon("rotate"), " Actualizar"),
                     class = "btn btn-sm btn-outline-secondary")
      )
    ),
    if (is.null(candidates) || !nrow(candidates)) {
      div(class = "alert alert-secondary small py-2 mb-3",
          icon("circle-info"), " No hay datos SAP cargados. ",
          "Haz clic en \u201cActualizar\u201d o carga facturas primero.")
    } else {
      tagList(
        tags$p(class = "text-muted small mb-1",
               "Selecciona los c\u00f3digos IC y haz clic en",
               strong("Aplicar como IC"), ". Pre-seleccionados = ya configurados.",
               "Aplicar", strong("agrega"), "c\u00f3digos; para eliminar uno, borra su campo en Intercompany."),
        div(style = "max-height: 260px; overflow-y: auto; margin-bottom: 0.5rem;",
            DT::DTOutput("tbl_bp_sap")),
        actionButton("bp_apply_ic_btn",
                     tagList(icon("arrows-left-right"), " Aplicar como IC"),
                     class = "btn btn-primary btn-sm mb-3")
      )
    },

    tags$hr(class = "my-3"),

    # ── Section 2: Catálogo de Proveedores ───────────────────────────────────
    div(class = "d-flex align-items-center gap-2 mb-2",
      tags$h6(class = "fw-semibold mb-0",
              tagList(icon("address-book"), " Cat\u00e1logo de Proveedores")),
      tags$span(class = "text-muted small",
        "— Alias Baj\u00edo + CLABE para generar archivos PPL")
    ),
    proveedoresUI("prov_mod")
  )
}

# Build the SAP BP datatable with auto-match column
.bp_datatable <- function(candidates, prov_db = NULL, presel = integer(0),
                           cmap = COMPANY_MAP) {
  tbl <- candidates |>
    dplyr::transmute(
      Empresa   = paste0(initials, " \u2014 ",
                         unname(cmap[initials]) %||% initials),
      Ledger    = dplyr::if_else(ledger == "AR", "CxC", "CxP"),
      Codigo    = code,
      Nombre    = dplyr::coalesce(nombre, "\u2014"),
      RFC       = dplyr::if_else(is.na(rfc) | !nzchar(rfc %||% ""), "\u2014", rfc),
      Facturas  = n_facturas,
      .rfc_raw  = rfc,
      .code_raw = code
    )

  # Auto-match: mark codes already in catálogo via RFC or CardCode
  if (!is.null(prov_db) && nrow(prov_db) > 0) {
    cat_rfcs   <- toupper(trimws(prov_db$rfc   %||% character()))
    cat_codes  <- toupper(trimws(prov_db$codigo %||% character()))
    tbl <- tbl |>
      dplyr::mutate(
        En_Catalogo = dplyr::case_when(
          toupper(trimws(.rfc_raw))  %in% cat_rfcs[nzchar(cat_rfcs)]   ~ "\u2705 RFC",
          toupper(trimws(.code_raw)) %in% cat_codes[nzchar(cat_codes)] ~ "\u2705 C\u00f3digo",
          TRUE ~ "\u2014"
        )
      )
  } else {
    tbl$En_Catalogo <- "\u2014"
  }

  tbl <- dplyr::select(tbl, -dplyr::starts_with("."))
  names(tbl)[names(tbl) == "En_Catalogo"] <- "En Cat\u00e1logo"

  DT::datatable(
    tbl,
    rownames  = FALSE,
    class     = "table table-sm table-hover",
    selection = list(mode = "multiple", selected = presel),
    options   = list(
      pageLength = 20,
      order      = list(list(0L, "asc"), list(1L, "asc"), list(5L, "desc")),
      dom        = "ftp"
    )
  )
}

