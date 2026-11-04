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
#   filters            : list(empresa, categorias, subcategorias, currency, search, granularity,
#                             provision_type) — "todas" | "programadas" | "manuales"
#
# Returns: tibble with one row per (liability + date) combination.
pasivos_table_build_long <- function(provisions, manual_items,
                                     bancos_confirmados, liabilities,
                                     filters) {
  today <- Sys.Date()

  # ── 1. Apply liability filters ───────────────────────────────────────────────
  # An archived liability (estado "deleted") is out, provisions and all, as
  # in the Calendario since CM06 (Luis, 2026-11-04: T17b (a)).
  liabs_in_scope <- liabilities
  if (nrow(liabs_in_scope) && "estado" %in% names(liabs_in_scope))
    liabs_in_scope <- liabs_in_scope[is.na(liabs_in_scope$estado) | liabs_in_scope$estado != "deleted", , drop = FALSE]
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
    pat <- strip_accents(tolower(trimws(filters$search)))
    nm_hit    <- grepl(pat, strip_accents(tolower(liabs_in_scope$nombre %||% "")), fixed = TRUE)
    parte_hit <- grepl(pat, strip_accents(tolower(liabs_in_scope$parte  %||% "")), fixed = TRUE)
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

  # ── 2b. Filter by provision type (todas / programadas / manuales) ────────────
  ptype <- filters$provision_type %||% "todas"
  if (ptype != "todas" && nrow(provisions_in)) {
    is_linked <- !is.na(provisions_in$liability_id) & nzchar(provisions_in$liability_id %||% "")
    if (ptype == "programadas") {
      provisions_in <- provisions_in[is_linked, , drop = FALSE]
    } else if (ptype == "manuales") {
      provisions_in <- provisions_in[!is_linked, , drop = FALSE]
    }
  }

  # ── 2c. Warn on unknown estados; keep only active lifecycle rows ─────────────
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

      # Paid (a current confirmation) leaves the table, no exception (rule
      # 22; Luis, 2026-11-04: T17b (b)). A deleted confirmation doesn't
      # count: bancos_confirmados comes through confirmaciones_vigentes().
      if (nrow(bc_row)) return(NULL)
      cell_kind <- if (item_date < today) "overdue_manual" else "manual_item"
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
      # T17b (Luis, P4): how many it adds up, and all their provision ids,
      # so the cell can say so and a click can list them. First: summarise()
      # goes in order, and below cell_provision_id becomes the first one.
      cell_n            = dplyr::n(),
      cell_ids          = paste(unique(stats::na.omit(cell_provision_id)), collapse = ","),
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

  # Rows built by hand (tests) may come without the count / ids of T17b.
  if (!"cell_n" %in% names(long_df)) long_df$cell_n <- 1L
  if (!"cell_ids" %in% names(long_df)) long_df$cell_ids <- ifelse(is.na(long_df$cell_provision_id), "", long_df$cell_provision_id)

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
        cell_n            = sum(cell_n),
        cell_ids          = paste(unique(unlist(strsplit(cell_ids[nzchar(cell_ids)], ",", fixed = TRUE))), collapse = ","),
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
      fecha               = long_df$fecha[k],
      cell_n              = long_df$cell_n[k],
      cell_ids            = long_df$cell_ids[k]
    )
  }

  list(metadata_cols = meta_raw, date_cols = date_cols, cells = cells)
}


# ── For the table engine (T17b step 3) ────────────────────────────────────────
# pasivos_tabla_datos(wide, liabilities): the wide pivot as hd_table rows --
# one per liability (or orphan, or fee), one money column per period
# (field .pasivos_campo(label)) with the cell's companion fields for the
# piece accion_celda:
#   .id     the provision's id; a cell that adds up several: "agg:<row>\t<label>"
#           (a click lists them, Luis P4); none: ""
#   .marca  pendiente (an item to pay) / vencida (an overdue item) / confirmada
#   .boton  provision (⚡ convert) / comprobante (↩ a converted one's receipt);
#           none on a cell that adds up several (its list has them)
#   .n      how many it adds up;  .mano  amount or date changed by hand
#   .tono   provision morado, overdue provision aviso, overdue item peligro,
#           pending item neutro, confirmed exito
# and per row .semaforo (vencida / muy_pronto / pronto), .nivel (1: a fee
# under its liability) and .acciones ("editar" only on a real liability).
.pasivos_campo <- function(lbl) paste0("c_", gsub("[^0-9A-Za-z]", "_", lbl))
.pasivos_celda_marca <- c(manual_item = "pendiente", overdue_manual = "vencida", confirmed_item = "confirmada")
.pasivos_celda_tono  <- c(provision = "morado", overdue_provision = "aviso", overdue_manual = "peligro",
                          manual_item = "neutro", confirmed_item = "exito")

pasivos_tabla_datos <- function(wide, liabilities = NULL) {
  meta <- wide$metadata_cols
  n <- nrow(meta)
  fee <- as.logical(meta$row_is_fee %||% rep(FALSE, n)); fee[is.na(fee)] <- FALSE
  bandera <- function(x) { x <- as.logical(x %||% rep(FALSE, n)); x[is.na(x)] <- FALSE; x }
  ov <- bandera(meta$has_overdue); vs <- bandera(meta$has_very_soon); ds <- bandera(meta$has_due_soon)
  reales <- if (!is.null(liabilities) && nrow(liabilities)) as.character(liabilities$id) else character(0)
  # For the engine's filters: an orphan (a manual provision, no liability)
  # has no category -- it shows under "(Sin valor)" (Luis: one filter, like
  # Buscar) -- and is "Manual"; the rest are "Programada".
  huerfana <- startsWith(as.character(meta$row_id), "orphan_")
  cat_txt <- vapply(as.character(meta$row_categoria %||% rep("", n)), function(k)
    if (is.na(k) || !k %in% c("regular", "financiero", "tarjeta")) "" else tr(paste0("tablas.pasivos.cat.", k)),
    character(1), USE.NAMES = FALSE)
  d <- data.frame(
    id = as.character(meta$row_id),
    Pasivo = ifelse(is.na(meta$row_display_label), meta$row_label, meta$row_display_label),
    # A fee's Parte is its liability's name (row_label is "<name>\001<index>"):
    # it says whose it is, and searching the liability finds its fees, as
    # today's search (by liability) did.
    Parte = ifelse(fee, sub("\001.*$", "", meta$row_label), meta$row_parte %||% ""),
    Empresa = meta$row_empresa %||% "",
    Moneda = meta$row_moneda %||% "",
    Categoria = ifelse(huerfana, "", cat_txt),
    Subcategoria = ifelse(huerfana | is.na(meta$row_subcategoria), "", meta$row_subcategoria),
    Tipo = ifelse(huerfana, tr("tablas.pasivos.tipo.manual"), tr("tablas.pasivos.tipo.programada")),
    .semaforo = ifelse(fee, "", ifelse(ov, "vencida", ifelse(vs, "muy_pronto", ifelse(ds, "pronto", "")))),
    .nivel = as.integer(fee),
    .acciones = ifelse(!fee & meta$row_id %in% reales, "editar", ""),
    stringsAsFactors = FALSE, check.names = FALSE)
  for (lbl in wide$date_cols) {
    campo <- .pasivos_campo(lbl)
    celdas <- lapply(d$id, function(r) wide$cells[[r]][[lbl]])
    hay <- !vapply(celdas, is.null, logical(1))
    toma <- function(nombre, vacio) vapply(celdas, function(c) {
      v <- if (is.null(c)) NULL else c[[nombre]]
      if (is.null(v) || length(v) != 1L || is.na(v)) vacio else v
    }, vacio)
    tipo  <- toma("cell_kind", "")
    cuant <- toma("cell_n", 1L)
    prov  <- toma("cell_provision_id", "")
    varias <- hay & cuant > 1L
    d[[campo]] <- ifelse(hay, toma("cell_amount", NA_real_), NA_real_)
    d[[paste0(campo, ".id")]] <- ifelse(varias, paste0("agg:", d$id, "\t", lbl), prov)
    d[[paste0(campo, ".marca")]] <- unname(ifelse(tipo %in% names(.pasivos_celda_marca), .pasivos_celda_marca[tipo], ""))
    d[[paste0(campo, ".boton")]] <- ifelse(varias | !nzchar(prov), "",
      ifelse(tipo %in% c("provision", "overdue_provision"), "provision",
             ifelse(tipo %in% c("manual_item", "overdue_manual"), "comprobante", "")))
    d[[paste0(campo, ".n")]] <- ifelse(hay, cuant, 0L)
    d[[paste0(campo, ".mano")]] <- toma("cell_has_override", FALSE)
    d[[paste0(campo, ".tono")]] <- unname(ifelse(tipo %in% names(.pasivos_celda_tono), .pasivos_celda_tono[tipo], ""))
  }
  d
}

# The ids a cell's click stands for: one provision, or the list behind an
# "agg:<row>\t<label>" cell (from the same wide pivot the rows came from).
pasivos_tabla_ids_celda <- function(wide, celda) {
  if (!startsWith(celda, "agg:")) return(celda)
  partes <- strsplit(sub("^agg:", "", celda), "\t", fixed = TRUE)[[1]]
  if (length(partes) != 2L) return(character(0))
  c1 <- wide$cells[[partes[1]]][[partes[2]]]
  if (is.null(c1) || !nzchar(c1$cell_ids %||% "")) return(character(0))
  strsplit(c1$cell_ids, ",", fixed = TRUE)[[1]]
}

# The rows of provisiones_celda (Luis, P4): what a cell of several adds up,
# one row per provision -- its date, liability (or part), amount (the item's
# for a converted one, else the override or the amount), currency and state;
# ✎ opens it, ⚡ converts a provisional one, ↩ handles a converted one's
# receipt (the same as the table's cells).
pasivos_tabla_lista_celda <- function(ids, provisions, liabilities = NULL, manual_items = NULL, hoy = Sys.Date()) {
  vacia <- data.frame(id = character(0), Fecha = as.Date(character(0)), Pasivo = character(0), Importe = numeric(0),
                      Moneda = character(0), Estado = character(0), .provision = logical(0), .acciones = character(0),
                      stringsAsFactors = FALSE, check.names = FALSE)
  if (!length(ids) || is.null(provisions) || !nrow(provisions)) return(vacia)
  p <- provisions[match(ids, provisions$id), , drop = FALSE]
  p <- p[!is.na(p$id), , drop = FALSE]
  if (!nrow(p)) return(vacia)
  nombre <- if (!is.null(liabilities) && nrow(liabilities)) liabilities$nombre[match(p$liability_id, liabilities$id)] else NA_character_
  convertida <- p$estado %in% c("converted", "item_confirmed")
  item <- if (!is.null(manual_items) && nrow(manual_items) && "provision_id" %in% names(manual_items))
    manual_items[match(p$id, manual_items$provision_id), , drop = FALSE] else NULL
  fecha_item <- if (!is.null(item)) as.Date(item[["FechaVenc"]] %||% item[["Fecha de vencimiento"]]) else as.Date(rep(NA, nrow(p)))
  fecha <- as.Date(ifelse(convertida & !is.na(fecha_item), fecha_item, p$fecha_efectiva), origin = "1970-01-01")
  importe <- ifelse(convertida & !is.null(item) & !is.na(item$Importe %||% NA), item$Importe %||% NA,
                    ifelse(is.na(p$amount_pago_override), p$amount_pago, p$amount_pago_override))
  estado <- ifelse(!convertida, "provision", ifelse(!is.na(fecha) & fecha < as.Date(hoy), "vencido", "pendiente"))
  data.frame(
    id = p$id, Fecha = fecha, Pasivo = ifelse(is.na(nombre), p$parte %||% "", nombre), Importe = as.numeric(importe),
    Moneda = p$moneda_pago %||% "", Estado = vapply(estado, function(e) tr(paste0("tablas.pasivos.estado.", e)), character(1)),
    .provision = !convertida, .acciones = ifelse(convertida, "editar,comprobante", "editar,provision"),
    stringsAsFactors = FALSE, check.names = FALSE, row.names = NULL)
}

# The columns: the liability fixed at the left, its part and company, then
# one money column per period (today's shaded), titled with its label.
pasivos_tabla_columnas <- function(wide, hoy = Sys.Date(), granularidad = "day") {
  hoy_lbl <- .pasivos_fecha_to_col(as.Date(hoy), granularidad)
  c(list(col_texto("Pasivo", "tablas.pasivos.col.pasivo", fija = TRUE),
         col_texto("Parte", "tablas.pasivos.col.parte"),
         col_empresa("Empresa", "tablas.pasivos.col.empresa")),
    lapply(wide$date_cols, function(lbl) {
      campo <- .pasivos_campo(lbl)
      col_dinero(campo, lbl, sumar = FALSE, accion = TRUE, tono = paste0(campo, ".tono"),
                 destacar = identical(lbl, hoy_lbl))
    }))
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
