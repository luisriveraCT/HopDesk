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
    pasivos_subcategoria   = character(),
    # banxico-fix: display only, never saved (Importe is the Agenda's key)
    importe_aprox          = numeric(),
    aprox                  = logical()
  )
}

# Convert the live pasivos_provisions tibble into rows shaped like the merged ledger.
#
# @param provisions   tibble from load_pasivos_provisions()
# @param liabilities  tibble from load_pasivos_liabilities()
# @param ledger       "AP" — provisions are always payables
# @param user         character or NULL; NULL skips the capability gate
# @return tibble with source = "provision"; filtered to provisional estado only
# @param fx           the Banxico FIX to approximate USD->MXN amounts for display
#                     (importe_aprox / aprox); NULL: none. Importe never changes.
pasivos_provisions_as_ledger_rows <- function(provisions, liabilities, ledger = "AP", user = NULL,
                                              fx = if (exists(".fx_para_mostrar", mode = "function")) .fx_para_mostrar() else NULL) {
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

  # A provision of an archived liability (estado "deleted") is not a payment
  # anyone will make. Archiving only removes FUTURE provisional provisions, so
  # past ones used to stay in the Calendar and the cash flow as overdue
  # payments (found 2026-10-31, CM06). Manual provisions have no liability.
  if (!is.null(liabs) && "estado" %in% names(liabs)) {
    archivados <- liabs$id[!is.na(liabs$estado) & liabs$estado == "deleted"]
    prov <- prov[is.na(prov$liability_id) | !prov$liability_id %in% archivados, , drop = FALSE]
    if (!nrow(prov)) return(.empty_provision_ledger_rows())
  }

  # T88-2: whole columns at once (it built one tibble per provision: 900
  # provisions took 5.6 s and froze the R process for every session). The
  # result is identical() to the row-by-row version it replaces, NA and
  # types included (tests/test_t88_provisiones_vector.R keeps that version
  # as its oracle). Each rule below says which line of it it reproduces.
  n   <- nrow(prov)
  col <- function(df, nm) if (nm %in% names(df)) df[[nm]] else NULL
  # p$x[1] %||% NA, as character: a missing column or NA gives NA.
  texto <- function(x) if (is.null(x)) rep(NA_character_, n) else as.character(x)

  # The liability: liabs[liabs$id == lid, ][1, ]. A comparison against an NA
  # id is NA, and that subset puts an all-NA row there: the first row taken
  # is the first exact match OR the first NA id, whichever comes first, and
  # an NA id's row gives NA for every field (not the provision's empresa).
  lid    <- prov$liability_id
  busca  <- !is.na(lid) & nzchar(ifelse(is.na(lid), "", lid)) & !is.null(liabs)
  pos    <- rep(NA_integer_, n)
  es_na  <- rep(FALSE, n)
  if (!is.null(liabs) && any(busca)) {
    ids      <- as.character(liabs$id)
    exacta   <- match(lid[busca], ids)
    primer_na <- if (anyNA(ids)) which(is.na(ids))[1] else NA_integer_
    elegida  <- if (is.na(primer_na)) exacta else pmin(exacta, primer_na, na.rm = TRUE)
    pos[busca]   <- elegida
    es_na[busca] <- !is.na(elegida) & !is.na(primer_na) & elegida == primer_na
  }
  hay_liab <- !is.na(pos)
  de_liab <- function(nm) {
    v <- col(liabs, nm)
    out <- rep(NA_character_, n)
    if (!is.null(v)) out[hay_liab] <- as.character(v[pos[hay_liab]])
    out[es_na] <- NA_character_
    out
  }
  empresa <- ifelse(hay_liab, de_liab("empresa"), texto(col(prov, "empresa")))
  pasivos_categoria    <- de_liab("categoria")
  pasivos_subcategoria <- de_liab("subcategoria")

  # pasivos_pago_amount(p): the override when it is not NA, else amount_pago;
  # an error (no override column) gave NA.
  ov    <- col(prov, "amount_pago_override")
  saldo <- if (is.null(ov)) rep(NA_real_, n) else as.numeric(ifelse(!is.na(ov), ov, prov$amount_pago))
  # pasivos_pago_currency(p): moneda_pago, or NA where it is missing/NA/"" (it stop()ed).
  mp  <- col(prov, "moneda_pago")
  mon <- if (is.null(mp)) rep(NA_character_, n) else {
    m <- as.character(mp); m[is.na(m) | !nzchar(ifelse(is.na(m), "", m))] <- NA_character_; m }
  # fx_monto_aprox() is vectorized already; without R/fx_banxico.R, nothing.
  ax <- if (exists("fx_monto_aprox", mode = "function"))
    fx_monto_aprox(prov$amount_cotizado, prov$cotizado_en, prov$moneda_pago, prov$amount_pago_override, fx)
  else list(aprox = rep(FALSE, n), importe_aprox = rep(NA_real_, n))

  # A fee provision or one without a real Documento: the synthetic key.
  origin  <- texto(col(prov, "origin"))
  is_fee  <- !is.na(origin) & origin == "initial_fee"
  doc_val <- texto(col(prov, "documento"))
  sintet  <- is_fee | is.na(doc_val) | !nzchar(ifelse(is.na(doc_val), "", doc_val))
  doc_val[sintet] <- paste0("PROV_", prov$id[sintet])

  fecha_ef <- as.Date(prov$fecha_efectiva)
  tibble::tibble(
    Empresa                = as.character(empresa),
    Parte                  = texto(col(prov, "parte")),
    Codigo                 = texto(col(prov, "codigo_parte")),
    `Saldo vencido`        = saldo,
    Importe                = saldo,
    `Fecha de vencimiento` = fecha_ef,
    FechaVenc_Original     = as.Date(prov$fecha_calculada),
    FechaEff               = fecha_ef,
    # FechaVenc_Proyectada intentionally omitted — left_join(ledger_moves) adds it as NA
    Tipo                   = rep("AP", n),
    source                 = rep("provision", n),
    Documento              = doc_val,
    Factura                = texto(col(prov, "referencia")),
    Moneda                 = mon,
    notas                  = texto(col(prov, "notas")),
    confirmed              = rep(FALSE, n),
    has_sap_override       = rep(FALSE, n),
    Movida                 = rep("Falso", n),
    provision_id           = as.character(prov$id),
    liability_id           = texto(lid),
    pasivos_categoria      = pasivos_categoria,
    pasivos_subcategoria   = pasivos_subcategoria,
    importe_aprox          = rep_len(as.numeric(ax$importe_aprox), n),
    aprox                  = rep_len(as.logical(ax$aprox), n)
  )
}

# Remove provision rows from a ledger rows data frame.
# Must be called at every action site that mass-moves items into the agenda,
# before any pagar_hoy staging or confirmation logic runs.
pasivos_filter_out_provisions <- function(rows) {
  if (is.null(rows) || !nrow(rows) || !"source" %in% names(rows)) return(rows)
  rows[rows$source != "provision", , drop = FALSE]
}

# found 2026-08-04: Calendario's per-selection buttons (stage_sel/do_move/
# do_restore in ledger_module.R) were calling pasivos_filter_out_provisions()
# on the WRONG object -- on `keys`, the small Empresa/Moneda/Documento/Importe
# (+Parte/provision_id) frame returned by .audit_sel_to_keys()/
# .resolve_summary_sel() AFTER the user's row selection had already been
# resolved, not on `detail` (the full day's rows, which still carries
# `source`). pasivos_filter_out_provisions()'s own early-return
# (!"source" %in% names(rows)) fired silently every time, since `keys` never
# has a `source` column -- the call looked correct (it's right there in the
# code, matching the pattern in stage_all/cart_<i>/cart_inv_click) but was a
# complete no-op. A selected provision then flowed straight through
# .fresh_lu() (which reads live from the FULL, provision-inclusive
# df_combined(), not a filtered copy) into new_rows and from there into the
# real pagar_hoy_db upsert -- i.e. "Agregar selección" could actually
# register a provision in the real Agenda, the one thing this whole mechanism
# exists to prevent. Confirmed by reproducing the exact call chain with
# synthetic data (a real SAP invoice + a provision selected together): the
# provision survived to the final new_rows every time.
#
# This is the one correct place to close that gap: restrict `keys` to just
# the (Empresa,Moneda,Documento,Importe) combinations that ALSO survive
# pasivos_filter_out_provisions(detail) -- `detail` still has `source`, so
# that filter is effective, and any extra columns already on `keys`
# (Parte, provision_id) pass through the merge untouched.
pasivos_exclude_provision_keys <- function(keys, detail) {
  if (is.null(keys) || !nrow(keys)) return(keys)
  detail_np <- pasivos_filter_out_provisions(detail)
  join_cols <- intersect(c("Empresa", "Moneda", "Documento", "Importe"), names(keys))
  join_cols <- intersect(join_cols, names(detail_np %||% data.frame()))
  if (!length(join_cols) || is.null(detail_np) || !nrow(detail_np)) {
    return(keys[0, , drop = FALSE])
  }
  np_key_set <- unique(detail_np[, join_cols, drop = FALSE])
  merge(keys, np_key_set, by = join_cols)
}
