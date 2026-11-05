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
    # (steps aside while an Excel of providers is in review -- T09)
    div(class = "bp-sap-seccion",
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
        tags$p(class = "text-muted small mb-1", tr("tablas.ajustes.candidatos.ayuda_bp")),
        div(class = "mb-2", hd_table_ui("bp_sap_t", alto = "260px")),
        # Writes the IC registry: only with can_manage_interco (the server checks too).
        if (puede(tryCatch(isolate(shared$usuario()), error = function(e) NULL), "can_manage_interco"))
          actionButton("bp_apply_ic_btn",
                       tagList(icon("arrows-left-right"), " Aplicar como IC"),
                       class = "btn btn-primary btn-sm mb-3")
      )
    },

    tags$hr(class = "my-3")
    ),

    # ── Section 2: Catálogo de Proveedores ───────────────────────────────────
    div(class = "d-flex align-items-center gap-2 mb-2 bp-catalogo-titulo",
      tags$h6(class = "fw-semibold mb-0",
              tagList(icon("address-book"), " Cat\u00e1logo de Proveedores")),
      tags$span(class = "text-muted small",
        "— Alias Baj\u00edo + CLABE para generar archivos PPL")
    ),
    proveedoresUI("prov_mod")
  )
}

# Rows of the SAP code candidates (bp_sap and escaner_ic, T16): one per
# initials|ledger|code -- the id "Aplicar" reads back, never a position.
# Only the session's companies (cmap); sorted by company, CxC before CxP, most
# invoices first. is_ic = "ic" marks the codes already in the registry (they
# used to come pre-selected). prov_db: the supplier catalog, for En catálogo
# ("rfc" / "codigo"), only in Business Partners.
.ic_candidatos_filas <- function(candidates, cmap, prov_db = NULL) {
  vacia <- data.frame(id = character(), is_ic = character(), Empresa = character(), ledger = character(),
                      codigo = character(), nombre = character(), rfc = character(), facturas = integer(),
                      en_catalogo = character(), stringsAsFactors = FALSE)
  if (!is.data.frame(candidates) || !nrow(candidates)) return(vacia)
  c0 <- candidates[!is.na(candidates$initials) & candidates$initials %in% names(cmap), , drop = FALSE]
  if (!nrow(c0)) return(vacia)
  emp <- paste0(c0$initials, " — ", unname(cmap[c0$initials]))
  c0  <- c0[order(emp, c0$ledger != "AR", -c0$n_facturas), , drop = FALSE]
  rfc <- ifelse(is.na(c0$rfc) | !nzchar(c0$rfc %||% ""), NA_character_, as.character(c0$rfc))
  en_cat <- rep(NA_character_, nrow(c0))
  if (is.data.frame(prov_db) && nrow(prov_db)) {
    limpio    <- function(x) toupper(trimws(as.character(x %||% character())))
    cat_rfcs  <- limpio(prov_db$rfc);    cat_rfcs  <- cat_rfcs[!is.na(cat_rfcs) & nzchar(cat_rfcs)]
    cat_codes <- limpio(prov_db$codigo); cat_codes <- cat_codes[!is.na(cat_codes) & nzchar(cat_codes)]
    en_cat[limpio(c0$code) %in% cat_codes] <- "codigo"
    en_cat[!is.na(rfc) & limpio(rfc) %in% cat_rfcs] <- "rfc"
  }
  data.frame(id = paste(c0$initials, c0$ledger, c0$code, sep = "|"),
             is_ic = ifelse(!is.na(c0$is_ic) & c0$is_ic, "ic", NA_character_),
             Empresa = as.character(c0$initials), ledger = as.character(c0$ledger),
             codigo = as.character(c0$code), nombre = as.character(c0$nombre), rfc = rfc,
             facturas = as.integer(c0$n_facturas), en_catalogo = en_cat, stringsAsFactors = FALSE)
}

# The candidates' columns, one definition for both tables; En catálogo only
# in Business Partners (catalogo = TRUE).
.ic_candidatos_columnas <- function(catalogo = FALSE) {
  cols <- list(
    col_insignia("is_ic", "tablas.ajustes.candidatos.ic", mapa = list(
      ic = list(texto = "tablas.ajustes.candidatos.ic_si", tono = "exito"))),
    col_empresa("Empresa", "tablas.ajustes.candidatos.empresa"),
    col_insignia("ledger", "tablas.ajustes.candidatos.ledger", mapa = list(
      AR = list(texto = "tablas.ajustes.candidatos.cxc", tono = "neutro"),
      AP = list(texto = "tablas.ajustes.candidatos.cxp", tono = "neutro"))),
    col_texto("codigo",   "tablas.ajustes.candidatos.codigo", recorte = FALSE),
    col_texto("nombre",   "tablas.ajustes.candidatos.nombre"),
    col_texto("rfc",      "tablas.ajustes.candidatos.rfc", recorte = FALSE),
    col_numero("facturas", "tablas.ajustes.candidatos.facturas")
  )
  if (catalogo)
    cols <- c(cols, list(col_insignia("en_catalogo", "tablas.ajustes.candidatos.en_catalogo", mapa = list(
      rfc    = list(texto = "tablas.ajustes.candidatos.cat_rfc",    tono = "exito"),
      codigo = list(texto = "tablas.ajustes.candidatos.cat_codigo", tono = "exito")))))
  cols
}

