# =============================================================================
# R/pasivos_calendar_glue.R
# Provision → ledger row adapter.
# Pure functions: no reactives, no S3 access inside.
# =============================================================================

.empty_provision_ledger_rows <- function() {
  tibble::tibble(
    Empresa                = character(),
    Parte                  = character(),
    Codigo                 = character(),
    `Saldo vencido`        = numeric(),
    Importe                = numeric(),
    `Fecha de vencimiento` = as.Date(character()),
    FechaVenc_Original     = as.Date(character()),
    FechaEff               = as.Date(character()),
    # FechaVenc_Proyectada intentionally omitted — added by left_join(ledger_moves)
    Tipo                   = character(),
    source                 = character(),
    Documento              = character(),
    Factura                = character(),
    Moneda                 = character(),
    notas                  = character(),
    confirmed              = logical(),
    has_sap_override       = logical(),
    Movida                 = character(),
    provision_id           = character(),
    liability_id           = character(),
    pasivos_categoria      = character(),
    pasivos_subcategoria   = character()
  )
}

# Convert the live pasivos_provisions tibble into rows shaped like the merged ledger.
#
# @param provisions   tibble from load_pasivos_provisions()
# @param liabilities  tibble from load_pasivos_liabilities()
# @param ledger       "AP" — provisions are always payables
# @param user         character or NULL; NULL skips the capability gate
# @return tibble with source = "provision"; filtered to provisional estado only
pasivos_provisions_as_ledger_rows <- function(provisions, liabilities, ledger = "AP", user = NULL) {
  if (!is.null(user) && !has_capability(user, "pasivos.view")) {
    return(.empty_provision_ledger_rows())
  }

  if (is.null(provisions) || !nrow(provisions)) {
    return(.empty_provision_ledger_rows())
  }

  # Only provisional provisions appear in the ledger; converted/confirmed/closed
  # are represented by their linked manual_inv row (source = "manual") to avoid double-counting.
  prov <- provisions[
    !is.na(provisions$estado) & provisions$estado == "provisional",
    , drop = FALSE
  ]

  if (!nrow(prov)) return(.empty_provision_ledger_rows())

  liabs <- if (!is.null(liabilities) && nrow(liabilities)) liabilities else NULL

  rows <- lapply(seq_len(nrow(prov)), function(i) {
    p   <- prov[i, , drop = FALSE]
    lid <- p$liability_id[1]

    liability <- if (!is.na(lid) && nzchar(lid %||% "") && !is.null(liabs)) {
      liabs[liabs$id == lid, , drop = FALSE]
    } else {
      NULL
    }

    empresa              <- if (!is.null(liability) && nrow(liability)) liability$empresa[1]     else p$empresa[1]
    pasivos_categoria    <- if (!is.null(liability) && nrow(liability)) liability$categoria[1]   else NA_character_
    pasivos_subcategoria <- if (!is.null(liability) && nrow(liability)) liability$subcategoria[1] else NA_character_

    saldo     <- tryCatch(pasivos_pago_amount(p),    error = function(e) NA_real_)
    mon       <- tryCatch(pasivos_pago_currency(p),  error = function(e) NA_character_)
    fecha_ef  <- as.Date(p$fecha_efectiva[1])
    fecha_ca  <- as.Date(p$fecha_calculada[1])

    is_fee  <- identical(as.character(p$origin[1]), "initial_fee")
    doc_val <- as.character(p$documento[1] %||% NA_character_)
    # Fee provisions and provisions without a real Documento get a synthetic key
    # so they are never aggregated with each other or with SAP items in the calendar.
    if (is_fee || is.na(doc_val) || !nzchar(doc_val)) {
      doc_val <- paste0("PROV_", p$id[1])
    }

    tibble::tibble(
      Empresa                = as.character(empresa     %||% NA_character_),
      Parte                  = as.character(p$parte[1]  %||% NA_character_),
      Codigo                 = as.character(p$codigo_parte[1] %||% NA_character_),
      `Saldo vencido`        = as.numeric(saldo),
      Importe                = as.numeric(saldo),
      `Fecha de vencimiento` = fecha_ef,
      FechaVenc_Original     = fecha_ca,
      FechaEff               = fecha_ef,
      # FechaVenc_Proyectada intentionally omitted — left_join(ledger_moves) adds it as NA
      Tipo                   = "AP",
      source                 = "provision",
      Documento              = doc_val,
      Factura                = as.character(p$referencia[1]  %||% NA_character_),
      Moneda                 = as.character(mon),
      notas                  = as.character(p$notas[1]       %||% NA_character_),
      confirmed              = FALSE,
      has_sap_override       = FALSE,
      Movida                 = "Falso",
      provision_id           = as.character(p$id[1]),
      liability_id           = as.character(p$liability_id[1] %||% NA_character_),
      pasivos_categoria      = as.character(pasivos_categoria    %||% NA_character_),
      pasivos_subcategoria   = as.character(pasivos_subcategoria %||% NA_character_)
    )
  })

  dplyr::bind_rows(rows)
}

# Remove provision rows from a ledger rows data frame.
# Must be called at every action site that mass-moves items into the agenda,
# before any pagar_hoy staging or confirmation logic runs.
pasivos_filter_out_provisions <- function(rows) {
  if (is.null(rows) || !nrow(rows) || !"source" %in% names(rows)) return(rows)
  rows[rows$source != "provision", , drop = FALSE]
}
