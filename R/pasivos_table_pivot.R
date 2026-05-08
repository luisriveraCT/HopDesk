# =============================================================================
# R/pasivos_table_pivot.R
# Stage 3: Pure data-shaping helpers for the Pasivos table view.
# No I/O, no reactives — fully testable standalone.
# =============================================================================

# ── pasivos_table_build_long ──────────────────────────────────────────────────
# Build a LONG-form tibble of (row, date, cell_payload) used by the renderer.
#
# Inputs:
#   provisions         : tibble matching .schema_pasivos_provision()
#   manual_items       : manual_inv tibble filtered to ledger == "AP"
#   bancos_confirmados : bancos_confirmados tibble (eliminado=FALSE pre-filtered by caller)
#   liabilities        : tibble matching .schema_pasivos_liability()
#   filters            : list(empresa, categorias, subcategorias, currency, search, granularity)
#
# Returns: tibble with one row per (liability + date) combination.
pasivos_table_build_long <- function(provisions, manual_items,
                                     bancos_confirmados, liabilities,
                                     filters) {
  today <- Sys.Date()

  # ── 1. Apply liability filters ───────────────────────────────────────────────
  liabs_in_scope <- liabilities
  if (length(filters$empresa) > 0L) {
    liabs_in_scope <- liabs_in_scope[liabs_in_scope$empresa %in% filters$empresa, , drop = FALSE]
  }
  if (length(filters$categorias) > 0L) {
    liabs_in_scope <- liabs_in_scope[liabs_in_scope$categoria %in% filters$categorias, , drop = FALSE]
  }
  if (length(filters$subcategorias) > 0L) {
    liabs_in_scope <- liabs_in_scope[liabs_in_scope$subcategoria %in% filters$subcategorias, , drop = FALSE]
  }
  if (!is.null(filters$search) && nzchar(trimws(filters$search))) {
    pat <- trimws(filters$search)
    nm_hit   <- grepl(pat, liabs_in_scope$nombre, ignore.case = TRUE)
    parte_hit <- grepl(pat, liabs_in_scope$parte,  ignore.case = TRUE)
    liabs_in_scope <- liabs_in_scope[nm_hit | parte_hit, , drop = FALSE]
  }

  # ── 2. Filter provisions to in-scope liabilities OR in-scope orphans ─────────
  if (!is.null(provisions) && nrow(provisions)) {
    in_scope_lib  <- !is.na(provisions$liability_id) &
                     provisions$liability_id %in% liabs_in_scope$id
    is_orphan     <- is.na(provisions$liability_id) | !nzchar(provisions$liability_id %||% "")
    emp_match     <- if (length(filters$empresa) > 0L)
                       provisions$empresa %in% filters$empresa
                     else rep(TRUE, nrow(provisions))
    provisions_in <- provisions[in_scope_lib | (is_orphan & emp_match), , drop = FALSE]
  } else {
    provisions_in <- provisions[integer(0), , drop = FALSE]
  }

  # ── 2b. Warn on unknown estados; keep only active lifecycle rows ─────────────
  known_estados <- c("provisional", "converted", "item_confirmed", "closed", "deleted")
  if (nrow(provisions_in)) {
    unknown <- setdiff(unique(provisions_in$estado), c(known_estados, NA_character_))
    if (length(unknown)) {
      warning("[pasivos_table] unknown estado values seen: ",
              paste(unknown, collapse = ", "),
              " — these rows will be dropped from the table.")
    }
    provisions_in <- provisions_in[
      !is.na(provisions_in$estado) &
        provisions_in$estado %in% c("provisional", "converted", "item_confirmed"),
      , drop = FALSE
    ]
  }

  # ── 3. Build per-provision rows ───────────────────────────────────────────────
  out_rows <- lapply(seq_len(nrow(provisions_in)), function(k) {
    p <- provisions_in[k, , drop = FALSE]

    # Skip states that are surfaced via manual_items instead
    if (p$estado %in% c("converted", "item_confirmed")) return(NULL)

    # Liability metadata
    lib_row <- if (!is.na(p$liability_id) && nzchar(p$liability_id %||% "") &&
                   nrow(liabs_in_scope)) {
      liabs_in_scope[liabs_in_scope$id == p$liability_id, , drop = FALSE]
    } else NULL

    is_fee          <- identical(as.character(p$origin[[1L]]), "initial_fee")
    parent_lid      <- if (!is.null(lib_row) && nrow(lib_row)) p$liability_id[[1L]] else NA_character_
    parent_label    <- if (!is.null(lib_row) && nrow(lib_row)) lib_row$nombre[1] else
                         (p$parte[1] %||% "Sin nombre")
    if (is_fee) {
      occ_abs  <- abs(as.integer(p$occurrence_index[[1L]]))
      row_id   <- paste0(parent_lid %||% paste0("orphan_", p$id[[1L]]), "_fee_", occ_abs)
      fee_disp <- p$referencia[[1L]] %||% "Cargo inicial"
      # Sort key: parent label + separator + fee index so fees always follow parent
      row_label <- paste0(parent_label, "\x01", sprintf("%04d", occ_abs))
    } else {
      row_id    <- if (!is.null(lib_row) && nrow(lib_row)) p$liability_id[[1L]] else paste0("orphan_", p$id[[1L]])
      row_label <- parent_label
    }
    row_empresa     <- p$empresa[1] %||% ""
    row_categoria   <- if (!is.null(lib_row) && nrow(lib_row)) lib_row$categoria[1] else "regular"
    row_subcategoria <- if (!is.null(lib_row) && nrow(lib_row)) lib_row$subcategoria[1] else ""
    row_parte       <- p$parte[1] %||% ""
    row_moneda      <- p$moneda_pago[1] %||% "MXN"

    eff_date  <- tryCatch(as.Date(p$fecha_efectiva[1]), error = function(e) as.Date(NA))
    if (is.na(eff_date)) return(NULL)
    amount    <- if (!is.na(p$amount_pago_override[1])) p$amount_pago_override[1] else p$amount_pago[1]

    cell_kind   <- if (eff_date < today) "overdue_provision" else "provision"
    overdue_sev <- if (eff_date < today) "overdue_provision" else
                   if (eff_date == today) "due_today" else "none"

    tibble::tibble(
      row_id            = row_id,
      row_label         = row_label,
      row_display_label = if (is_fee) fee_disp else parent_label,
      row_is_fee        = is_fee,
      row_empresa       = row_empresa,
      row_categoria     = row_categoria,
      row_subcategoria  = row_subcategoria,
      row_parte         = row_parte,
      row_moneda        = row_moneda,
      fecha             = eff_date,
      cell_kind         = cell_kind,
      cell_amount       = amount,
      cell_currency     = row_moneda,
      cell_provision_id = p$id[1],
      cell_manual_id    = NA_character_,
      cell_bancos_conf_id = NA_character_,
      cell_has_override = !is.na(p$amount_pago_override[1]) ||
                          !is.na(p$fecha_efectiva_override[1]),
      cell_overdue_severity = overdue_sev
    )
  })

  # ── 4. Build rows for converted/item_confirmed provisions via manual_items ────
  if (!is.null(manual_items) && nrow(manual_items) &&
      "provision_id" %in% names(manual_items)) {
    conv_provs <- provisions_in[provisions_in$estado %in% c("converted", "item_confirmed"), , drop = FALSE]
    mi_match   <- manual_items[!is.na(manual_items$provision_id) &
                                manual_items$provision_id %in% conv_provs$id, , drop = FALSE]

    mi_rows <- lapply(seq_len(nrow(mi_match)), function(k) {
      mi <- mi_match[k, , drop = FALSE]
      p  <- conv_provs[conv_provs$id == mi$provision_id[1], , drop = FALSE]
      if (!nrow(p)) return(NULL)

      lib_row <- if (!is.na(p$liability_id) && nzchar(p$liability_id %||% "") &&
                     nrow(liabs_in_scope)) {
        liabs_in_scope[liabs_in_scope$id == p$liability_id, , drop = FALSE]
      } else NULL

      row_id          <- if (!is.null(lib_row) && nrow(lib_row)) p$liability_id[1] else paste0("orphan_", p$id[1])
      row_label       <- if (!is.null(lib_row) && nrow(lib_row)) lib_row$nombre[1] else (p$parte[1] %||% "")
      row_empresa     <- p$empresa[1] %||% ""
      row_categoria   <- if (!is.null(lib_row) && nrow(lib_row)) lib_row$categoria[1] else "regular"
      row_subcategoria <- if (!is.null(lib_row) && nrow(lib_row)) lib_row$subcategoria[1] else ""
      row_parte       <- p$parte[1] %||% ""
      row_moneda      <- p$moneda_pago[1] %||% "MXN"

      fecha_col <- if ("FechaVenc" %in% names(mi)) "FechaVenc" else "Fecha de vencimiento"
      item_date <- tryCatch(as.Date(mi[[fecha_col]][1]), error = function(e) today)

      bc_col  <- if ("id" %in% names(bancos_confirmados)) "id" else
                 if ("confirmacion_id" %in% names(bancos_confirmados)) "confirmacion_id" else ""
      bc_prov_col <- if ("provision_id" %in% names(bancos_confirmados)) "provision_id" else ""
      bc_row <- if (nzchar(bc_col) && nzchar(bc_prov_col) &&
                    !is.null(bancos_confirmados) && nrow(bancos_confirmados)) {
        bancos_confirmados[!is.na(bancos_confirmados[[bc_prov_col]]) &
                           bancos_confirmados[[bc_prov_col]] == p$id[1], , drop = FALSE]
      } else data.frame()

      cell_kind <- if (nrow(bc_row)) "confirmed_item" else
                   if (item_date < today) "overdue_manual" else "manual_item"
      overdue_sev <- switch(cell_kind,
        overdue_manual  = "overdue_manual",
        manual_item     = if (item_date == today) "due_today" else "none",
        confirmed_item  = "none",
        "none"
      )
      bc_id_val <- if (nrow(bc_row) && nzchar(bc_col)) bc_row[[bc_col]][1] else NA_character_
      mi_id_col <- if ("id" %in% names(mi)) "id" else character(0)
      mi_id_val <- if (length(mi_id_col)) mi[[mi_id_col]][1] else NA_character_

      tibble::tibble(
        row_id            = row_id,
        row_label         = row_label,
        row_display_label = row_label,
        row_is_fee        = FALSE,
        row_empresa       = row_empresa,
        row_categoria     = row_categoria,
        row_subcategoria  = row_subcategoria,
        row_parte         = row_parte,
        row_moneda        = row_moneda,
        fecha             = item_date,
        cell_kind         = cell_kind,
        cell_amount       = if ("Importe" %in% names(mi)) mi$Importe[1] else NA_real_,
        cell_currency     = row_moneda,
        cell_provision_id = p$id[1],
        cell_manual_id    = mi_id_val,
        cell_bancos_conf_id = bc_id_val,
        cell_has_override = !is.na(p$amount_pago_override[1]) ||
                            !is.na(p$fecha_efectiva_override[1]),
        cell_overdue_severity = overdue_sev
      )
    })
    out_rows <- c(out_rows, mi_rows)
  }

  # ── 5. Combine, de-dup, filter by currency ───────────────────────────────────
  rows_bind <- Filter(Negate(is.null), out_rows)
  if (!length(rows_bind)) {
    return(tibble::tibble(
      row_id = character(), row_label = character(),
      row_display_label = character(), row_is_fee = logical(),
      row_empresa = character(), row_categoria = character(),
      row_subcategoria = character(), row_parte = character(),
      row_moneda = character(), fecha = as.Date(character()),
      cell_kind = character(), cell_amount = numeric(), cell_currency = character(),
      cell_provision_id = character(), cell_manual_id = character(),
      cell_bancos_conf_id = character(), cell_has_override = logical(),
      cell_overdue_severity = character()
    ))
  }
  long_df <- dplyr::bind_rows(rows_bind)

  # Deduplicate (row_id + fecha): if collision, sum amounts and keep first kind
  long_df <- long_df %>%
    dplyr::group_by(row_id, row_label, row_display_label, row_is_fee,
                    row_empresa, row_categoria, row_subcategoria,
                    row_parte, row_moneda, fecha, cell_currency) %>%
    dplyr::summarise(
      cell_kind         = dplyr::first(cell_kind),
      cell_amount       = sum(cell_amount, na.rm = TRUE),
      cell_provision_id = dplyr::first(cell_provision_id),
      cell_manual_id    = dplyr::first(cell_manual_id),
      cell_bancos_conf_id = dplyr::first(cell_bancos_conf_id),
      cell_has_override = any(cell_has_override, na.rm = TRUE),
      cell_overdue_severity = dplyr::first(cell_overdue_severity),
      .groups = "drop"
    )

  if (!is.null(filters$currency) && filters$currency != "All" && nzchar(filters$currency)) {
    long_df <- long_df[long_df$cell_currency == filters$currency, , drop = FALSE]
  }

  # ── 6. Sort: currency first (MXN → USD → EUR → others), then by empresa/label ──
  cur_prio <- c("MXN", "USD", "EUR")
  cur_fac  <- factor(
    long_df$row_moneda,
    levels = c(cur_prio, sort(setdiff(unique(long_df$row_moneda), cur_prio)))
  )
  long_df <- long_df[order(cur_fac, long_df$row_empresa, long_df$row_categoria,
                            long_df$row_subcategoria, long_df$row_label, long_df$fecha), ]
  long_df
}


# ── pasivos_table_pivot_wide ──────────────────────────────────────────────────
# Pivot the long tibble into a wide grid structure for display.
#
# Returns list(metadata_cols, date_cols, cells) where cells is indexed
# [[row_id]][[date_col_label]].
pasivos_table_pivot_wide <- function(long_df, granularity = "day",
                                     window_start, window_end) {

  # Generate date column labels for the window
  date_cols <- .pasivos_date_seq(window_start, window_end, granularity)

  # Empty-input shortcut
  if (is.null(long_df) || !nrow(long_df)) {
    return(list(
      metadata_cols = tibble::tibble(
        row_id = character(), row_label = character(),
        row_display_label = character(), row_is_fee = logical(),
        row_empresa = character(), row_categoria = character(),
        row_subcategoria = character(), row_parte = character(),
        row_moneda = character(), has_overdue = logical(), has_due_soon = logical()
      ),
      date_cols = date_cols,
      cells     = list()
    ))
  }

  # Map each fecha to a column label bucket
  long_df[["col_label"]] <- .pasivos_fecha_to_col(long_df$fecha, granularity)

  # Filter to window
  long_df <- long_df[long_df$col_label %in% date_cols, , drop = FALSE]

  # Aggregate within bucket for non-day granularities (sum amounts, merge kinds)
  if (granularity != "day") {
    long_df <- long_df %>%
      dplyr::group_by(row_id, row_label, row_display_label, row_is_fee,
                      row_empresa, row_categoria, row_subcategoria,
                      row_parte, row_moneda, col_label, cell_currency) %>%
      dplyr::summarise(
        cell_kind         = dplyr::first(cell_kind),
        cell_amount       = sum(cell_amount, na.rm = TRUE),
        fecha             = min(fecha),
        cell_provision_id = dplyr::first(cell_provision_id),
        cell_manual_id    = dplyr::first(cell_manual_id),
        cell_bancos_conf_id = dplyr::first(cell_bancos_conf_id),
        cell_has_override = any(cell_has_override, na.rm = TRUE),
        cell_overdue_severity = dplyr::first(cell_overdue_severity),
        .groups = "drop"
      )
  }

  # Build metadata: one row per row_id (using first occurrence for labels)
  today_piv <- Sys.Date()
  meta_raw <- long_df %>%
    dplyr::group_by(row_id) %>%
    dplyr::summarise(
      row_label         = dplyr::first(row_label),
      row_display_label = dplyr::first(row_display_label),
      row_is_fee        = dplyr::first(row_is_fee),
      row_empresa       = dplyr::first(row_empresa),
      row_categoria     = dplyr::first(row_categoria),
      row_subcategoria  = dplyr::first(row_subcategoria),
      row_parte         = dplyr::first(row_parte),
      row_moneda        = dplyr::first(row_moneda),
      has_overdue      = any(cell_overdue_severity %in% c("overdue_provision","overdue_manual"),
                             na.rm = TRUE),
      has_due_soon     = any(
        cell_kind %in% c("provision", "manual_item") &
        !is.na(fecha) & fecha >= today_piv & fecha <= today_piv + 7L,
        na.rm = TRUE
      ),
      # More-intense highlight: due today or within 2 days
      has_very_soon    = any(
        cell_kind %in% c("provision", "manual_item") &
        !is.na(fecha) & fecha >= today_piv & fecha <= today_piv + 2L,
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  # summarise() doesn't preserve sort order — re-sort by currency priority then label
  # so the render loop sees MXN rows first, USD next, etc., producing exactly one
  # separator per currency group instead of one per adjacent-currency boundary.
  cur_prio <- c("MXN", "USD", "EUR")
  meta_raw <- meta_raw[order(
    factor(meta_raw$row_moneda,
           levels = c(cur_prio, sort(setdiff(unique(meta_raw$row_moneda), cur_prio)))),
    meta_raw$row_empresa, meta_raw$row_categoria,
    meta_raw$row_subcategoria, meta_raw$row_label
  ), ]

  # Build cells: named list [[row_id]][[col_label]] = list of cell payload
  cells <- list()
  for (k in seq_len(nrow(long_df))) {
    r  <- long_df$row_id[k]
    cl <- long_df$col_label[k]
    cells[[r]][[cl]] <- list(
      cell_kind           = long_df$cell_kind[k],
      cell_amount         = long_df$cell_amount[k],
      cell_currency       = long_df$cell_currency[k],
      cell_provision_id   = long_df$cell_provision_id[k],
      cell_manual_id      = long_df$cell_manual_id[k],
      cell_bancos_conf_id = long_df$cell_bancos_conf_id[k],
      cell_has_override   = long_df$cell_has_override[k],
      cell_overdue_severity = long_df$cell_overdue_severity[k],
      fecha               = long_df$fecha[k]
    )
  }

  list(metadata_cols = meta_raw, date_cols = date_cols, cells = cells)
}


# ── Internal helpers ──────────────────────────────────────────────────────────

.pasivos_date_seq <- function(start, end, granularity) {
  start <- as.Date(start); end <- as.Date(end)
  switch(granularity,
    day   = as.character(seq(start, end, by = "day")),
    week  = {
      days  <- seq(start, end, by = "day")
      unique(format(days, "%G-W%V"))
    },
    month = {
      days  <- seq(start, end, by = "day")
      unique(format(days, "%Y-%m"))
    },
    year  = {
      days  <- seq(start, end, by = "day")
      unique(format(days, "%Y"))
    },
    as.character(seq(start, end, by = "day"))
  )
}

.pasivos_fecha_to_col <- function(fechas, granularity) {
  switch(granularity,
    day   = as.character(fechas),
    week  = format(as.Date(fechas), "%G-W%V"),
    month = format(as.Date(fechas), "%Y-%m"),
    year  = format(as.Date(fechas), "%Y"),
    as.character(fechas)
  )
}
