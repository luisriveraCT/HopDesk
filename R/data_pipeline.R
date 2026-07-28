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
                            abonos_df = NULL, policy_moves_df = NULL, sap_ov = NULL,
                            provs_df = NULL, company_map = NULL, liabs_df = NULL) {
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
    # Translate empresa initials to full names (manual items from provision
    # conversion may store initials; the empresa filter in df_combined() uses
    # full names — same translation already done below for provision rows).
    if (nrow(manual_sub) && !is.null(company_map) && length(company_map)) {
      manual_sub[["Empresa"]] <- vapply(
        manual_sub[["Empresa"]],
        function(e) company_map[[e %||% ""]] %||% e %||% NA_character_,
        character(1)
      )
    }
    df <- bind_rows(df, manual_sub)
  }

  # Merge provisions — AP only; provisional provisions appear in the ledger
  # as placeholder rows until converted to real manual_inv items.
  # provs_df is passed from a reactive so df_combined() re-fires on provision saves.
  if (identical(ledger, "AP")) {
    provs <- if (!is.null(provs_df)) provs_df
             else tryCatch(load_pasivos_provisions(), error = function(e) NULL)
    liabs <- liabs_df
    if (!is.null(provs) && nrow(provs)) {
      prov_rows <- pasivos_provisions_as_ledger_rows(provs, liabs, ledger = "AP")
      if (nrow(prov_rows)) {
        # Provisions store empresa as a company initial (e.g. "NG"); the empresa
        # filter in df_combined() uses full names from the Empresas module.
        # Translate here so provisions survive the filter.
        if (!is.null(company_map) && length(company_map)) {
          prov_rows[["Empresa"]] <- vapply(
            prov_rows[["Empresa"]],
            function(e) company_map[[e %||% ""]] %||% e %||% NA_character_,
            character(1)
          )
        }
        df <- bind_rows(df, prov_rows)
      }
    }
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

  # Guarantee FechaEff exists before the left_join+mutate pipeline.
  # Provision rows already carry it (set by pasivos_calendar_glue); SAP/manual
  # rows do not — add an NA column so .data$FechaEff is always resolvable.
  if (!"FechaEff" %in% names(df)) df[["FechaEff"]] <- as.Date(NA_character_)

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

  # Prepare policy moves (same dedup pattern as ledger_moves)
  ledger_policy_moves <- if (!is.null(policy_moves_df) && nrow(policy_moves_df) > 0) {
    policy_moves_df |>
      dplyr::filter(.data$ledger == !!ledger) |>
      dplyr::select(Empresa, Moneda, Documento, FechaVenc_Politica) |>
      dplyr::arrange(dplyr::desc(FechaVenc_Politica)) |>
      dplyr::distinct(Empresa, Moneda, Documento, .keep_all = TRUE)
  } else {
    tibble(Empresa = character(), Moneda = character(),
           Documento = character(), FechaVenc_Politica = as.Date(character()))
  }

  result <- df |>
    # na_matches = "never": provisions often have Documento = NA (no template);
    # without this, dplyr treats NA == NA and accidentally joins all such rows to
    # any moves/policy entry that also has Documento = NA, displacing their dates.
    left_join(ledger_moves,        by = c("Empresa","Moneda","Documento"),
              na_matches = "never") |>
    left_join(ledger_policy_moves, by = c("Empresa","Moneda","Documento"),
              na_matches = "never") |>
    mutate(
      FechaVenc_Proyectada = as.Date(FechaVenc_Proyectada),
      FechaVenc_Politica   = as.Date(FechaVenc_Politica),
      # .data$FechaEff carries the policy-adjusted date already set by
      # pasivos_calendar_glue for provision rows; fall back to it before
      # FechaVenc_Original so that policy adjustments are preserved.
      FechaEff             = dplyr::coalesce(FechaVenc_Proyectada, FechaVenc_Politica,
                                             .data$FechaEff, FechaVenc_Original),
      Movida               = dplyr::case_when(
        !is.na(FechaVenc_Proyectada) ~ "Manual",
        !is.na(FechaVenc_Politica)   ~ "Políticas",
        TRUE                         ~ "Falso"
      ),
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

  # ── Apply SAP field overrides (Parte, Codigo, Factura, Notas) ───────────────
  # Overrides paint on top of SAP data for UI display only.
  # The underlying sap_data() snapshot and dedup keys are never touched.
  # Deliberately literal source == "sap" below, not is_erp_sourced(): this
  # whole feature (table, columns, settings UI) is SAP-specific by name, not
  # a generic ERP-override mechanism yet — generalizing it is a bigger,
  # separate decision than this stage's "don't hardcode the sap/manual split
  # at every confirm/delete/stage site" scope. Also a no-op either way today
  # — `source` is always explicitly "sap" by the time rows reach here, never
  # NA (see build_ledger_df() above), so the two checks are equivalent now.
  if (!is.null(sap_ov) && is.data.frame(sap_ov) && nrow(sap_ov)) {
    if (!"Factura" %in% names(result)) result[["Factura"]] <- NA_character_
    ov_filt <- sap_ov |>
      dplyr::filter(.data$ledger == !!ledger) |>
      dplyr::select(Empresa, Moneda, Documento,
                    Parte_override, Codigo_override, Factura_override, Notas_override,
                    Moneda_override, Importe_override)
    if (nrow(ov_filt)) {
      result <- dplyr::left_join(result, ov_filt,
                                 by = c("Empresa", "Moneda", "Documento")) |>
        dplyr::mutate(
          has_sap_override = source == "sap" & (
            !is.na(Parte_override) | !is.na(Codigo_override) |
            !is.na(Factura_override) | !is.na(Notas_override) |
            !is.na(Moneda_override) | !is.na(Importe_override)),
          Parte   = dplyr::if_else(source == "sap" & !is.na(Parte_override),
                                   Parte_override,   Parte),
          Codigo  = dplyr::if_else(source == "sap" & !is.na(Codigo_override),
                                   Codigo_override,  Codigo),
          Factura = dplyr::if_else(source == "sap" & !is.na(Factura_override),
                                   Factura_override, Factura),
          notas   = dplyr::if_else(source == "sap" & !is.na(Notas_override),
                                   Notas_override,   notas),
          Moneda  = dplyr::if_else(source == "sap" & !is.na(Moneda_override),
                                   Moneda_override,  Moneda),
          Importe = dplyr::if_else(source == "sap" & !is.na(Importe_override),
                                   dplyr::if_else(Importe < 0,
                                                  -abs(Importe_override),
                                                  abs(Importe_override)),
                                   Importe)
        ) |>
        dplyr::select(-Parte_override, -Codigo_override,
                      -Factura_override, -Notas_override,
                      -Moneda_override, -Importe_override)
    } else {
      result[["has_sap_override"]] <- FALSE
    }
  } else {
    result[["has_sap_override"]] <- FALSE
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
    # Fallback: when no CardCodes or RFCs are registered, detect IC by matching
    # the Parte column against company display names from COMPANY_MAP.
    # This is the same Layer-2 mechanism used by the Vencidos module.
    if ("Parte" %in% names(df)) {
      cmap <- tryCatch(get("COMPANY_MAP", envir = .GlobalEnv, inherits = FALSE),
                       error = function(e) list())
      if (length(cmap)) {
        company_names_up <- toupper(unname(cmap))
        is_ic_by_name    <- toupper(trimws(df[["Parte"]])) %in% company_names_up
        n_ic <- sum(is_ic_by_name, na.rm = TRUE)
        message("[IC_FILTER] mode=", mode, " name-fallback rows_in=", nrow(df), " rows_ic=", n_ic)
        if (mode == "exclude") return(df[!is_ic_by_name | is.na(is_ic_by_name), , drop = FALSE])
        else                   return(df[ is_ic_by_name & !is.na(is_ic_by_name), , drop = FALSE])
      }
    }
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
  # Exclude confirmed payments AND soft-deleted ghosts from the calendar heat-map
  if ("confirmed" %in% names(df))
    df <- df[is.na(df[["confirmed"]]) | !df[["confirmed"]], , drop = FALSE]
  if ("is_ghost" %in% names(df))
    df <- df[is.na(df[["is_ghost"]]) | !df[["is_ghost"]], , drop = FALSE]

  # ── Column audit ────────────────────────────────────────────────────────────
  required_cols <- c("Empresa", "FechaEff", "Moneda", "Parte", amount_col)
  missing_cols  <- setdiff(required_cols, names(df))
  if (length(missing_cols)) {
    warning("[to_calendar_data] Missing columns: ", paste(missing_cols, collapse=", "),
            " — available: ", paste(names(df), collapse=", "))
    return(tibble(Empresa=character(), Fecha=as.Date(NA_character_), Moneda=NA_character_,
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
    group_by(Empresa, Fecha, Moneda, Parte) |>
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

# ── Canonical "is this invoice confirmed?" computation ─────────────────────────
# Ledger-integrity master plan, Stage 9: the single reference implementation
# every consumer (calendar, and eventually Cash Flow Preview/Export, Reporte's
# Cash Flow Pulse, Intercompany) reads from, instead of each one growing its
# own reimplementation. Two sources only -- a third (pagar_hoy_db.status==
# "confirmed") existed pre-Stage-4 but is now structurally dead: every confirm
# handler unconditionally unstages the Agenda row regardless of source, so no
# code path can ever leave one behind with status=="confirmed" again.
#
#   1. bancos_confirmados matching (Empresa+Documento+Moneda, case/whitespace
#      normalized) -- confirmations made via Agenda de Hoy. Never applied to
#      manual or provision rows (see below).
#   2. papelera SAP ghosts -- SAP items soft-deleted via the calendar/search
#      trash mechanic remain visible, struck through, excluded from sums.
#
# Manual rows: matched only by their own UUID (papelera anti-join, done by the
# caller before this runs) or explicit deletion -- never by document-key
# matching here, which would wrongly hide a brand-new manual entry that
# happens to reuse a past, already-confirmed Documento. A manual row that DOES
# end up confirmed (via the archive-on-confirm mechanism) is removed from df
# entirely below, rather than kept as a ghost like a SAP row.
#
# Provision rows: forcibly cleared on all three flags regardless of any match
# -- they can only change state through the explicit conversion modal, never
# through payment/confirmation matching (which would wrongly mark a provision
# "paid" whenever a real invoice happens to share its Documento key).
#
# df: combined ledger rows (SAP + manual + provisions), already empresa-
# filtered and past the manual-papelera anti-join -- the caller's concern,
# not this function's; papelera_df is still needed here for Source 2 (SAP
# ghosts), a distinct concern from the manual anti-join.
# bancos_confirmados_df/papelera_df: plain data frames, not reactives --
# callers resolve their own shared$xxx() and pass the result in, keeping this
# file's "no reactives, no S3" contract (see file header) intact.
compute_confirmed_flags <- function(df, ledger, bancos_confirmados_df, papelera_df) {
  tipo_val <- if (ledger == "AR") "cobro" else "pago"
  if (!"confirmed" %in% names(df)) df[["confirmed"]] <- FALSE
  na_conf <- is.na(df[["confirmed"]])
  if (any(na_conf)) df[["confirmed"]][na_conf] <- FALSE

  # A caller with no "source" column at all (e.g. Reporte's Cash Flow Pulse,
  # which is deliberately SAP-only) must get an all-FALSE mask, not
  # logical(0) -- "source" %in% names(df) is a length-1 scalar, so `&`-ing
  # it against !is.na(df[["source"]]) on a NULL/absent column silently
  # collapses the whole mask to zero length, which then breaks every
  # downstream `df[["confirmed"]] | bc_mask` recycling (found 2026-07-24 via
  # Reporte's own tests -- every other caller always has a real "source"
  # column, so this was never exercised until now).
  if ("source" %in% names(df)) {
    is_manual    <- !is.na(df[["source"]]) & df[["source"]] == "manual"
    is_provision <- !is.na(df[["source"]]) & df[["source"]] == "provision"
  } else {
    is_manual    <- rep(FALSE, nrow(df))
    is_provision <- rep(FALSE, nrow(df))
  }

  # Source 1: bancos_confirmados
  # Amount-match guard (Stage 10): standard everywhere bancos_confirmados
  # is matched, deliberately WITHOUT a date-window check -- Mouse's explicit
  # reasoning is that a date guard would treat a normal month-end SAP delay
  # as staleness and reopen a still-valid confirmation. Protects against
  # SAP reusing a DocNum years later for an unrelated future invoice, using
  # the exact key shape already proven in production at
  # R/interco_module.R's .ckey(): 2-decimal-rounded amount appended to the
  # existing (Empresa, Documento, Moneda) key.
  # Matched against df$Saldo_original -- the balance BEFORE this render's
  # abono-netting is applied (build_ledger_df() always sets it, for SAP and
  # manual rows alike, before subtracting active abonos into "Saldo
  # vencido"). df has no "Importe" column at all for SAP rows (confirmed
  # directly against a real SAP snapshot -- only Saldo vencido/Saldo_original
  # exist there); Saldo_original is also stable across time in the one way
  # that matters here: matching the live, ever-shrinking "Saldo vencido"
  # instead would cause exactly the false-negative Mouse is worried about --
  # an abono applied after confirmation would change Saldo vencido and make
  # a genuinely still-valid confirmation silently fail to match, reopening it.
  conf_db <- bancos_confirmados_df
  if (!is.null(conf_db) && nrow(conf_db)) {
    conf_active <- conf_db[!(conf_db[["eliminado"]] %in% TRUE) &
                           conf_db[["tipo"]] == tipo_val &
                           !is.na(conf_db[["importe"]]), , drop = FALSE]
    if (nrow(conf_active)) {
      bc_keys   <- unique(conf_active[, c("empresa","documento","moneda","importe"),
                                      drop = FALSE])
      amt_col   <- if ("Saldo_original" %in% names(df)) "Saldo_original" else "Saldo vencido"
      match_key <- paste(toupper(trimws(df[["Empresa"]])),
                         toupper(trimws(df[["Documento"]])),
                         toupper(trimws(df[["Moneda"]])),
                         sprintf("%.2f", round(as.numeric(df[[amt_col]]), 2)))
      conf_key  <- paste(toupper(trimws(bc_keys[["empresa"]])),
                         toupper(trimws(bc_keys[["documento"]])),
                         toupper(trimws(bc_keys[["moneda"]])),
                         sprintf("%.2f", round(as.numeric(bc_keys[["importe"]]), 2)))
      bc_mask   <- (match_key %in% conf_key) & !is_manual & !is_provision
      df[["confirmed"]]   <- df[["confirmed"]] | bc_mask
      if (!"is_paid_ghost" %in% names(df)) df[["is_paid_ghost"]] <- FALSE
      df[["is_paid_ghost"]] <- df[["is_paid_ghost"]] | bc_mask
    }
  }

  # Source 2: papelera SAP ghosts
  if (!is.null(papelera_df) && nrow(papelera_df)) {
    pap_this <- papelera_df[papelera_df[["ledger"]] == ledger |
                           papelera_df[["ledger"]] == "MIXED", , drop = FALSE]
    if (nrow(pap_this)) {
      # Deliberately NOT is_erp_sourced() here: an ambiguous/legacy NA source
      # in papelera should NOT be treated as a SAP ghost (which stays
      # visible, excluded from sums) -- safer to fall through to the manual
      # anti-join path (fully hidden) for unknown provenance.
      sap_pap <- pap_this[!is.na(pap_this[["source"]]) &
                            pap_this[["source"]] == "sap",
                          c("Empresa","Moneda","Documento"), drop = FALSE]
      if (nrow(sap_pap)) {
        match_key <- paste(df[["Empresa"]], df[["Moneda"]], df[["Documento"]])
        pap_key   <- paste(sap_pap[["Empresa"]], sap_pap[["Moneda"]], sap_pap[["Documento"]])
        # !is_manual guard (found 2026-07-24): this source only ever means
        # "an ERP row was trashed" -- without the guard, a brand-new manual
        # invoice that happens to reuse the same Empresa+Moneda+Documento key
        # as some unrelated, previously-archived SAP invoice (e.g. a generic
        # placeholder like "test") gets wrongly treated as that SAP ghost,
        # marked confirmed, and then deleted outright by the manual-removal
        # block below -- silently vanishing from the calendar even though it
        # was never staged, confirmed, or deleted by any real user action.
        # Source 1 (bancos_confirmados, above) already has this guard; this
        # one was missing it.
        ghost_mask <- (match_key %in% pap_key) & !is_manual & !is_provision
        df[["confirmed"]] <- df[["confirmed"]] | ghost_mask
        if (!"is_ghost" %in% names(df)) df[["is_ghost"]] <- FALSE
        df[["is_ghost"]]  <- df[["is_ghost"]] | ghost_mask
      }
    }
  }

  # Manual rows: keep in df with confirmed=TRUE → calendar excludes from
  # totals, day modal shows with strikethrough (SAP rows). Manual rows:
  # remove entirely from df → disappear from calendar and modal (their real
  # data survives in the archive, restorable via undo).
  if ("source" %in% names(df) && any(df[["confirmed"]] & df[["source"]] == "manual")) {
    df <- df[!(df[["confirmed"]] & df[["source"]] == "manual"), , drop = FALSE]
  }

  # Provisions cannot receive ANY payment/confirmation flag.
  # Belt: masks above already exclude is_provision.
  # Suspenders: forcibly clear all three flags here so the '✓ Pagado' badge
  # can never render on a provision row regardless of future mask changes.
  if ("source" %in% names(df) && "confirmed" %in% names(df)) {
    prov_mask <- !is.na(df[["source"]]) & df[["source"]] == "provision"
    if (any(prov_mask)) {
      df[["confirmed"]][prov_mask] <- FALSE
      if ("is_paid_ghost" %in% names(df)) df[["is_paid_ghost"]][prov_mask] <- FALSE
      if ("is_ghost"      %in% names(df)) df[["is_ghost"]][prov_mask]      <- FALSE
    }
  }

  df
}

# ── "Send straight to Agenda" — derive, never fabricate ────────────────────────
# Builds the pagar_hoy staging row for the one-click "create/convert and
# immediately stage to Agenda" convenience (direct manual-entry creation,
# Pasivos conversion modal). Takes the manual_inv row exactly as it now
# exists in the root table (post-write) — every field comes from that row,
# never from the raw form inputs that produced it. This is what keeps
# Agenda a mirror of Calendario's root data instead of a second,
# independently-computed copy that can silently drift from it.
# manual_row: single-row data frame, already bound into manual_inv/manual_df.
stage_manual_row_to_agenda <- function(manual_row, user) {
  stopifnot(is.data.frame(manual_row), nrow(manual_row) == 1)
  prov_id  <- manual_row[["provision_id"]][1]
  has_prov <- !is.na(prov_id) && nzchar(prov_id %||% "")
  now      <- Sys.time()
  tibble::tibble(
    id           = manual_row[["id"]][1],
    ledger       = manual_row[["ledger"]][1],
    Empresa      = manual_row[["Empresa"]][1],
    Moneda       = manual_row[["Moneda"]][1],
    Documento    = manual_row[["Documento"]][1],
    Parte        = manual_row[["Parte"]][1]  %||% "",
    Codigo       = manual_row[["Codigo"]][1] %||% "",
    tipo_item    = "factura",
    Importe      = manual_row[["Importe"]][1],
    FechaVenc    = as.Date(manual_row[["Fecha de vencimiento"]][1]),
    staged_by    = user,
    staged_at    = now,
    status       = "pending",
    provision_id = if (has_prov) prov_id else NA_character_,
    liability_id = if (has_prov) manual_row[["liability_id"]][1] else NA_character_,
    source       = if (has_prov) "provision" else "manual"
  )
}