# =============================================================================
# R/data_pipeline.R
# Pure data transformation functions.
# Input: raw data frames (from SAP API or RDS cache)
# Output: clean, standardized, move-applied data frames ready for the calendar
# No reactives, no S3, no UI concerns.
# =============================================================================

# ── Column detection helpers ───────────────────────────────────────────────────

# Find the first column matching preferred names or a regex fallback
guess_col <- function(df, preferred = character(), regex = NULL) {
  nms <- names(df)
  hit <- preferred[preferred %in% nms][1]
  if (!is.na(hit)) return(hit)
  if (!is.null(regex)) {
    i <- which(grepl(regex, nms, ignore.case = TRUE))[1]
    if (length(i) && !is.na(i)) return(nms[i])
  }
  NULL
}

# Find the invoice/document number column
guess_doc_col <- function(df) {
  guess_col(df,
    preferred = c("Nº de documento","Número de documento","No. de documento",
                  "Documento","DocNum","DocEntry","Nº Factura","Número de factura"),
    regex     = "(?i)doc(ument|\\.)|factur"
  )
}

# ── Type coercion ──────────────────────────────────────────────────────────────

parse_sap_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  xc  <- as.character(x)
  num <- suppressWarnings(as.numeric(xc))
  out <- as.Date(rep(NA_real_, length(xc)), origin = "1970-01-01")

  is_num <- !is.na(num)
  if (any(is_num)) {
    d <- as.Date(num[is_num], origin = "1899-12-30")
    bad <- d < as.Date("1900-01-01") | d > as.Date("2100-01-01")
    d[bad] <- as.Date(num[is_num][bad], origin = "1904-01-01")
    out[is_num] <- d
  }
  if (any(!is_num)) {
    xs <- gsub("\\.", "/", xc[!is_num])
    d1 <- suppressWarnings(lubridate::dmy(xs))
    d2 <- suppressWarnings(lubridate::ymd(xs))
    out[!is_num] <- as.Date(ifelse(!is.na(d1), d1, d2), origin = "1970-01-01")
  }
  out
}

parse_currency_num <- function(x) {
  if (is.numeric(x)) return(x)
  x2 <- str_replace_all(as.character(x), "[^0-9,.-]", "")
  if (any(str_detect(x2, ",\\d{1,2}$"), na.rm = TRUE)) {
    x2 <- str_replace_all(x2, "\\.", "")
    x2 <- str_replace(x2, ",", ".")
  } else {
    x2 <- str_replace_all(x2, ",", "")
  }
  suppressWarnings(as.numeric(x2))
}

clean_str <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "-", "—", "–", ".", "NA", "N/A", "****")] <- NA_character_
  x
}

# ── Standardization ────────────────────────────────────────────────────────────

# Ensure a Moneda column exists and is normalized
ensure_moneda <- function(df) {
  if (!"Moneda" %in% names(df)) {
    col <- guess_col(df,
      preferred = c("Currency","Divisa","Saldo vencido (moneda)"),
      regex     = "moneda|currency|divisa"
    )
    df$Moneda <- if (!is.null(col)) df[[col]] else "MXN"
  }
  df |>
    mutate(
      Moneda = toupper(trimws(as.character(Moneda))),
      # SAP B1 sometimes sends "MXP" (legacy ISO 4217 code for Mexican Peso)
      # or other variants — normalize all to the current standard "MXN"
      Moneda = dplyr::case_when(
        Moneda == "MXP" ~ "MXN",
        Moneda == "MXV" ~ "MXN",
        TRUE            ~ Moneda
      )
    ) |>
    fill(Moneda, .direction = "down")
}

# Ensure a Documento column exists
ensure_documento <- function(df) {
  if ("Documento" %in% names(df)) return(df)
  col <- guess_doc_col(df)
  df$Documento <- if (!is.null(col)) as.character(df[[col]]) else NA_character_
  df
}

# Ensure date columns are Date type
ensure_dates <- function(df) {
  for (col in c("Fecha de vencimiento","Fecha de contabilización")) {
    if (col %in% names(df))
      df[[col]] <- parse_sap_date(df[[col]])
  }
  df
}

# Ensure amount columns are numeric
ensure_amounts <- function(df) {
  if ("Saldo vencido" %in% names(df))
    df[["Saldo vencido"]] <- parse_currency_num(df[["Saldo vencido"]])
  df
}

# Standardize party (client/vendor) name → always stored in `Parte`
standardize_party <- function(df, ledger) {
  pref <- if (ledger == "AR")
    c("Parte","Nombre del cliente","Cliente","Customer","CardName","Socio de negocios")
  else
    c("Parte","Nombre de acreedor","Nombre del proveedor","Proveedor","Vendor","CardName")

  regex <- if (ledger == "AR") "(cliente|cardname|customer|socio.*neg)"
           else                "(acreedor|proveedor|vendor|cardname)"

  col <- guess_col(df, preferred = pref, regex = regex)
  if (!"Parte" %in% names(df)) df$Parte <- NA_character_
  if (!is.null(col)) df$Parte <- clean_str(df[[col]])
  df |> fill(Parte, .direction = "down")
}

# ── Deduplication ──────────────────────────────────────────────────────────────
# Keep the newest snapshot per (Empresa, Moneda, Documento, due date).
# Rows without a Documento are kept as-is (cannot safely collapse them).

dedupe_invoices <- function(df) {
  has_doc  <- !is.na(df$Documento) & nzchar(df$Documento)

  with_doc <- df[has_doc, ] |>
    arrange(desc(!is.na(FechaVenc_Original)), desc(FechaVenc_Original)) |>
    group_by(Empresa, Moneda, Documento, FechaVenc_Original) |>
    slice_head(n = 1) |>
    ungroup()

  bind_rows(with_doc, df[!has_doc, ])
}

# ── Main pipeline ──────────────────────────────────────────────────────────────
# Accepts raw data from any source (SAP API response or legacy RDS).
# Returns a clean data frame ready for the calendar reactive.

build_ledger_df <- function(raw_df, ledger, empresa, moves_df, manual_df = NULL,
                            abonos_df = NULL) {
  ledger <- toupper(ledger)

  # Build SAP portion (may be empty)
  if (!is.null(raw_df) && nrow(raw_df)) {
    df <- raw_df |>
      ensure_dates() |>
      ensure_amounts() |>
      ensure_moneda() |>
      standardize_party(ledger) |>
      ensure_documento() |>
      mutate(
        Tipo               = ledger,
        source             = "sap",
        FechaVenc_Original = as.Date(`Fecha de vencimiento`)
      ) |>
      dedupe_invoices()
  } else {
    df <- tibble()
  }

  # Merge manual entries — works even when SAP data is empty
  if (!is.null(manual_df) && nrow(manual_df)) {
    manual_sub <- manual_df |>
      filter(.data$ledger == !!ledger) |>
      mutate(
        source             = "manual",
        FechaVenc_Original = as.Date(`Fecha de vencimiento`),
        Tipo               = ledger,
        Parte              = Parte %||% "",
        `Saldo vencido`    = Importe   # manual schema stores this as Importe
      )
    df <- bind_rows(df, manual_sub)
  }

  # Harmonize Codigo across SAP and manual rows. SAP carries CardCode under
  # `Código de proveedor` (AP) or `Código de cliente` (AR); manual carries it
  # as `Codigo`. Coalesce into a single canonical `Codigo` column so every
  # downstream slice (keep lists, .fresh_lu, vencidos disp_df, search keep)
  # picks it up uniformly.
  if (!"Codigo" %in% names(df)) df[["Codigo"]] <- NA_character_
  if ("Código de proveedor" %in% names(df)) {
    df[["Codigo"]] <- dplyr::coalesce(
      as.character(df[["Codigo"]]),
      as.character(df[["Código de proveedor"]])
    )
  }
  if ("Código de cliente" %in% names(df)) {
    df[["Codigo"]] <- dplyr::coalesce(
      as.character(df[["Codigo"]]),
      as.character(df[["Código de cliente"]])
    )
  }
  df[["Codigo"]] <- trimws(df[["Codigo"]] %||% "")
  df[["Codigo"]][df[["Codigo"]] == ""] <- NA_character_

  # Nothing at all — return NULL
  if (!nrow(df)) return(NULL)

  # Apply date moves (moves_df may be NULL if .data_loaded hasn't completed yet)
  ledger_moves <- if (!is.null(moves_df) && nrow(moves_df) > 0) {
    moves_df |>
      dplyr::filter(.data$ledger == !!ledger) |>
      dplyr::select(Empresa, Moneda, Documento, FechaVenc_Proyectada,
                    dplyr::any_of("notas")) |>
      # Defensive dedup: duplicate (Empresa, Moneda, Documento) keys in moves_df
      # cause a dplyr 1.1+ many-to-many join error and silently wipe the calendar.
      # Keep the row with the latest projected date when duplicates exist.
      dplyr::arrange(dplyr::desc(FechaVenc_Proyectada)) |>
      dplyr::distinct(Empresa, Moneda, Documento, .keep_all = TRUE)
  } else {
    tibble(Empresa = character(), Moneda = character(),
           Documento = character(), FechaVenc_Proyectada = as.Date(character()),
           notas = character())
  }

  result <- df |>
    left_join(ledger_moves, by = c("Empresa","Moneda","Documento")) |>
    mutate(
      FechaVenc_Proyectada = as.Date(FechaVenc_Proyectada),
      FechaEff             = coalesce(FechaVenc_Proyectada, FechaVenc_Original),
      .row_id              = row_number()
    )
  if (!"notas" %in% names(result)) result[["notas"]] <- NA_character_

  # ── Apply abonos parciales ───────────────────────────────────────────────────
  # Always add the three abono columns so downstream code (to_calendar_data,
  # calendar_html) can depend on them unconditionally.
  ab_summary <- active_abonos_summary(abonos_df) |>
    dplyr::filter(.data$ledger == !!toupper(ledger))

  if (nrow(ab_summary) > 0) {
    result <- result |>
      left_join(
        ab_summary |> dplyr::select(Empresa, Moneda, Documento, abono_total),
        by = c("Empresa", "Moneda", "Documento")
      ) |>
      mutate(
        abono_total    = replace_na(abono_total, 0),
        Saldo_original = `Saldo vencido`,
        `Saldo vencido` = pmax(0, `Saldo vencido` - abono_total),
        has_abono      = abono_total > 0
      )
  } else {
    result <- result |>
      mutate(
        abono_total    = 0,
        Saldo_original = `Saldo vencido`,
        has_abono      = FALSE
      )
  }
  result
}

# ── Intercompany filter ────────────────────────────────────────────────────────
# mode: "exclude" | "include" (no-op) | "only"

apply_ic_filter <- function(df, mode, code_col, ic_codes, ic_rfcs = character()) {
  if (mode == "include") return(df)

  has_codes <- length(ic_codes) > 0
  has_rfcs  <- length(ic_rfcs) > 0 && "RFC" %in% names(df)

  if (!has_codes && !has_rfcs) {
    message("[IC_FILTER] mode=", mode, " — ic_codes and ic_rfcs both empty, no filter applied")
    return(df)
  }

  # Seed is_ic from CardCode matching (when codes are registered for this company)
  if (has_codes && !is.null(code_col) && code_col %in% names(df)) {
    codes <- normalize_code(df[[code_col]])
    is_ic <- codes %in% normalize_code(ic_codes)
  } else {
    is_ic <- rep(FALSE, nrow(df))
  }

  # RFC overrides — two-way:
  #   positive: RFC in IC list   → confirmed IC  (works even when no CardCodes registered)
  #   negative: RFC not in list  → confirmed NOT IC (eliminates false positives like GEODIS)
  if (has_rfcs) {
    inv_rfc      <- toupper(trimws(df[["RFC"]]))
    has_rfc      <- nzchar(inv_rfc) & !is.na(inv_rfc)
    ic_rfcs_norm <- toupper(trimws(ic_rfcs[nzchar(ic_rfcs)]))
    is_ic[has_rfc &  (inv_rfc %in% ic_rfcs_norm)] <- TRUE
    is_ic[has_rfc & !(inv_rfc %in% ic_rfcs_norm)] <- FALSE
  }

  n_ic <- sum(is_ic, na.rm = TRUE)
  message("[IC_FILTER] mode=", mode,
          " codes=", length(ic_codes), " rfcs=", length(ic_rfcs),
          " rows_in=", nrow(df), " rows_ic=", n_ic)
  if (mode == "exclude") df[!is_ic | is.na(is_ic), , drop = FALSE]
  else                   df[ is_ic & !is.na(is_ic), , drop = FALSE]
}

# Build the full CardCode set for one ledger from the v2 registry.
# Handles both the new per-empresa format and the legacy flat-list format
# so the app degrades gracefully if the S3 key is temporarily unavailable.
#
# registry: result of load_interco_v2() or shared$interco_v2()
# ledger:   "AR" | "AP"
# returns:  character vector of normalized full CardCodes, e.g. c("C1027","C1047",...)
build_ic_fullcodes <- function(registry, ledger) {
  ledger <- toupper(ledger)

  # Backward compat: old format had ar_clients / ap_suppliers flat vectors
  if (!is.null(registry$ar_clients) || !is.null(registry$ap_suppliers)) {
    key <- if (ledger == "AR") "ar_clients" else "ap_suppliers"
    return(normalize_code(registry[[key]] %||% character()))
  }

  if (is.null(registry$companies) || !length(registry$companies))
    return(character())

  prefix <- if (ledger == "AR") registry$ar_prefix %||% "C"
            else                registry$ap_prefix %||% "P"

  numeric_codes <- unlist(lapply(registry$companies, function(co) {
    if (ledger == "AR") co$ar %||% character()
    else                co$ap %||% character()
  }), use.names = FALSE)

  if (!length(numeric_codes)) return(character())
  unique(toupper(paste0(prefix, numeric_codes)))
}

# ── IC Scanner ────────────────────────────────────────────────────────────────
# Aggregates unique CardCodes + RFCs from loaded invoice snapshots into a
# candidate table that the dev can review in Settings › Intercompany.
#
# Returns a tibble: initials, ledger, code (full, e.g. "C1027"), nombre, rfc,
#                   n_facturas, is_ic (already in registry)
scan_ic_candidates <- function(sap_ar, sap_ap, registry, cmap = COMPANY_MAP) {
  inv_map <- setNames(names(cmap), unname(cmap))

  .extract <- function(df, ledger) {
    if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(tibble())
    code_col <- if (ledger == "AR") "C\u00f3digo de cliente" else "C\u00f3digo de proveedor"
    if (!code_col %in% names(df)) return(tibble())
    has_rfc  <- "RFC" %in% names(df)

    df |>
      dplyr::filter(!is.na(.data[[code_col]]), nzchar(trimws(.data[[code_col]]))) |>
      dplyr::transmute(
        initials = inv_map[Empresa],
        ledger   = ledger,
        code     = toupper(trimws(.data[[code_col]])),
        nombre   = Parte,
        rfc      = if (has_rfc) toupper(trimws(RFC)) else NA_character_
      ) |>
      dplyr::filter(!is.na(initials), nzchar(code)) |>
      dplyr::group_by(initials, ledger, code) |>
      dplyr::summarise(
        nombre     = dplyr::first(nombre),
        rfc        = dplyr::first(rfc[!is.na(rfc) & nzchar(rfc)]),
        n_facturas = dplyr::n(),
        .groups    = "drop"
      )
  }

  candidates <- dplyr::bind_rows(
    .extract(sap_ar, "AR"),
    .extract(sap_ap, "AP")
  )

  if (!nrow(candidates)) return(candidates)

  ar_prefix <- toupper(registry$ar_prefix %||% "C")
  ap_prefix <- toupper(registry$ap_prefix %||% "P")

  candidates |>
    dplyr::rowwise() |>
    dplyr::mutate(
      .prefix  = if (ledger == "AR") ar_prefix else ap_prefix,
      .numeric = sub(paste0("^", .prefix), "", code),
      is_ic    = !is.na(initials) &&
                   !is.na(nombre) && nzchar(nombre %||% "") &&
                   .numeric %in%
                   (registry$companies[[initials]][[if (ledger == "AR") "ar" else "ap"]] %||% character())
    ) |>
    dplyr::select(-.prefix, -.numeric) |>
    dplyr::ungroup() |>
    dplyr::arrange(initials, ledger, dplyr::desc(n_facturas))
}

# ── Calendar-ready aggregation ─────────────────────────────────────────────────
# Reduces the full invoice df to (Fecha, Moneda, Parte, Importe)
# which is exactly what calendar_plot() needs.

to_calendar_data <- function(df, amount_col = "Saldo vencido") {
  # Exclude confirmed payments from the calendar heat-map
  if ("confirmed" %in% names(df))
    df <- df[is.na(df[["confirmed"]]) | !df[["confirmed"]], , drop = FALSE]

  # ── Column audit ────────────────────────────────────────────────────────────
  required_cols <- c("FechaEff", "Moneda", "Parte", amount_col)
  missing_cols  <- setdiff(required_cols, names(df))
  if (length(missing_cols)) {
    warning("[to_calendar_data] Missing columns: ", paste(missing_cols, collapse=", "),
            " — available: ", paste(names(df), collapse=", "))
    return(tibble(Fecha=as.Date(NA_character_), Moneda=NA_character_,
                  Parte=NA_character_, Importe=NA_real_)[0, ])
  }

  # Ensure abono columns exist with safe defaults before summarising
  if (!"abono_total"    %in% names(df)) df$abono_total    <- 0
  if (!"has_abono"      %in% names(df)) df$has_abono      <- FALSE
  if (!"Saldo_original" %in% names(df)) df$Saldo_original <- df[[amount_col]]

  result <- df |>
    mutate(
      Importe        = abs(replace_na(.data[[amount_col]], 0)),
      Fecha          = as.Date(FechaEff),
      Moneda         = toupper(trimws(Moneda)),
      abono_total    = replace_na(abono_total,    0),
      Saldo_original = dplyr::coalesce(Saldo_original, Importe),
      has_abono      = replace_na(has_abono,      FALSE)
    ) |>
    filter(!is.na(Fecha)) |>
    group_by(Fecha, Moneda, Parte) |>
    summarise(
      Importe        = sum(Importe,        na.rm = TRUE),
      abono_total    = sum(abono_total,    na.rm = TRUE),
      Saldo_original = sum(Saldo_original, na.rm = TRUE),
      has_abono      = any(has_abono,      na.rm = TRUE),
      .groups = "drop"
    )

  message("[to_calendar_data] amount_col='", amount_col,
          "' rows_in=", nrow(df), " rows_out=", nrow(result),
          " monedas=", paste(sort(unique(result$Moneda)), collapse=","))
  result
}