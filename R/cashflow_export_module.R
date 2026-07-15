# =============================================================================
# R/cashflow_export_module.R
# Cash Flow Word export feature.
#
# Stage 1 : Modal UI + reactive state (date range, grouping, currency, language)
# Stage 2 : build_cashflow_export_data() aggregation pipeline
# Stage 3 : Word document generation via officer + flextable
# Stage 4 : Empresas module will supply group name + logo.
#           The logo placeholder block in the cover page (Stage 3) is tagged
#           "LOGO_PLACEHOLDER" so Stage 4 can swap it programmatically once
#           the Empresas module exposes an upload path via shared$group_logo.
#           The group name placeholder is tagged "GROUP_NAME_PLACEHOLDER"
#           and will feed both the cover title and the auto-generated filename.
# =============================================================================

# ── Constants ─────────────────────────────────────────────────────────────────

EXPORT_LANG_CHOICES <- c("Español" = "es", "English" = "en")

# ── Background Excel worker ────────────────────────────────────────────────────
# Runs inside callr::r_bg() — receives plain R data, no reactives, no Shiny.
# Loads minimum packages, sources this module to get computation functions,
# then calls build_cashflow_export_data() + generate_cashflow_excel_wb().
.cf_excel_worker <- function(snapshot_path, output_path, app_dir) {
  library(dplyr,     warn.conflicts = FALSE)
  library(lubridate, warn.conflicts = FALSE)
  library(openxlsx)
  library(stringr,   warn.conflicts = FALSE)
  library(purrr,     warn.conflicts = FALSE)
  library(scales,    warn.conflicts = FALSE)
  library(tidyr,     warn.conflicts = FALSE)

  # Reproduce %||% from global.R
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
  }

  # Source this module in an isolated env so shiny-dependent server functions
  # are defined but never called. Only the pure computation functions are used.
  e <- new.env(parent = globalenv())
  e$`%||%` <- `%||%`
  source(file.path(app_dir, "R", "cashflow_export_module.R"), local = e)

  sn <- readRDS(snapshot_path)

  export_data <- e$build_cashflow_export_data(
    df            = sn$combined_df,
    tags_df       = sn$tags_df,
    emp_sel       = sn$emp_sel,
    date_from     = sn$date_from,
    date_to       = sn$date_to,
    grouping      = sn$grouping,
    currency_mode = sn$currency_mode,
    base_cur      = sn$base_cur,
    fx_rates      = sn$fx_rates
  )
  if (is.null(export_data))
    stop("Sin datos disponibles para el período seleccionado.")

  wb <- e$generate_cashflow_excel_wb(
    export_data    = export_data,
    emp_initials   = sn$initials,
    emp_full       = sn$emp_sel,
    ic_label       = sn$ic_label,
    user_name      = sn$user_name,
    primary_lang   = sn$lang_primary,
    secondary_lang = sn$lang_secondary,
    group_name     = sn$g_name,
    group_logo_raw = sn$g_logo_raw,
    group_logo_ext = sn$g_logo_ext
  )

  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  output_path
}

# ── Helpers ───────────────────────────────────────────────────────────────────

# Suggested exchange rate FROM → TO.
# Uses forecasting_get_estimate("usd_mxn") when available; falls back to
# hard-coded market-order approximations for other pairs.
.suggest_fx_rate <- function(from_cur, to_cur) {
  if (from_cur == "USD" && to_cur == "MXN") {
    v <- tryCatch(
      forecasting_get_estimate("usd_mxn", Sys.Date()),
      error = function(e) NA_real_
    )
    if (!is.na(v) && v > 0) return(round(v, 2))
    return(18.50)
  }
  fallbacks <- c(
    EUR_MXN = 20.00, GBP_MXN = 23.50, CAD_MXN = 13.50,
    JPY_MXN = 0.12,  CHF_MXN = 21.00, AUD_MXN = 12.00,
    CNY_MXN = 2.55,  BRL_MXN =  3.20,
    EUR_USD =  1.08, GBP_USD =  1.27, CAD_USD =  0.73
  )
  key <- paste0(from_cur, "_", to_cur)
  v   <- fallbacks[ key ]
  if (is.na(v)) 1.00 else unname(v)
}

# ── Stage 2: Data pipeline helpers ───────────────────────────────────────────

# Human-readable labels for date buckets.
.bucket_labels <- function(buckets, grouping_used) {
  m_es <- c("Ene","Feb","Mar","Abr","May","Jun",
            "Jul","Ago","Sep","Oct","Nov","Dic")
  if (grouping_used == "daily") {
    day_ltr <- c("L","M","M","J","V","S","D")[lubridate::wday(buckets, week_start = 1)]
    paste0(day_ltr, " ", sprintf("%02d", lubridate::day(buckets)),
           " ", m_es[lubridate::month(buckets)])
  } else {
    # Last day of each week bucket (capped at bucket+6 within range)
    week_ends <- buckets + 6L
    paste0(
      sprintf("%02d", lubridate::day(buckets)), "–",
      sprintf("%02d", lubridate::day(week_ends)),
      " ", m_es[lubridate::month(week_ends)]
    )
  }
}

# Join tag flags (tag_urgent, tag_important) to every row of df.
.attach_tags_to_df <- function(df, tags_df) {
  empty <- function() dplyr::mutate(df, tag_urgent = FALSE, tag_important = FALSE)
  if (is.null(tags_df) || !is.data.frame(tags_df) || !nrow(tags_df)) return(empty())
  if (!all(c("ledger","Empresa","Moneda","Documento","tag") %in% names(tags_df)))
    return(empty())

  tag_sum <- tags_df |>
    dplyr::group_by(ledger, Empresa, Moneda, Documento) |>
    dplyr::summarise(
      tag_urgent    = any(tag == "urgent",    na.rm = TRUE),
      tag_important = any(tag == "important", na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename(Tipo = ledger)

  df |>
    dplyr::select(-dplyr::any_of(c("tag_urgent", "tag_important"))) |>
    dplyr::left_join(tag_sum, by = c("Tipo", "Empresa", "Moneda", "Documento")) |>
    dplyr::mutate(
      tag_urgent    = dplyr::coalesce(tag_urgent,    FALSE),
      tag_important = dplyr::coalesce(tag_important, FALSE)
    )
}

# Convert all non-base currencies to base_cur using fx_rates.
# Adds .is_converted flag so Stage 3 can mark cells with (*).
.apply_fx_conversion <- function(df, base_cur, fx_rates) {
  sv <- "Saldo vencido"
  if (!(sv %in% names(df))) return(df)

  rates_vec <- vapply(df$Moneda %||% "MXN", function(cur) {
    if (is.na(cur) || cur == base_cur) return(1.0)
    fx_rates[[ cur ]] %||% 1.0
  }, numeric(1))

  df$.orig_moneda  <- df$Moneda                                  # preserve before overwriting
  df$.is_converted <- !is.na(df$Moneda) & (df$Moneda != base_cur)
  df[[ sv ]]       <- abs(as.numeric(df[[ sv ]])) * rates_vec
  df$Moneda        <- base_cur
  df
}

# Aggregate df (already filtered to one ledger type) to a wide data frame.
# Rows  = one per (Parte, is_provision).
# Cols  = one per date bucket + Total + Pct + tag flags.
.aggregate_to_wide <- function(df, all_buckets, grouping_used) {
  sv    <- "Saldo vencido"
  empty <- tibble::tibble(
    Parte = character(), is_provision = logical(),
    Total = numeric(),   Pct = numeric(),
    tag_urgent = logical(), tag_important = logical()
  )
  if (is.null(df) || !nrow(df) || !(sv %in% names(df))) return(empty)

  bucket_labels <- .bucket_labels(all_buckets, grouping_used)
  bucket_map    <- setNames(bucket_labels, as.character(all_buckets))

  df <- df |>
    dplyr::mutate(
      is_provision = (source == "provision"),
      bucket_label = bucket_map[ as.character(date_bucket) ]
    ) |>
    dplyr::filter(!is.na(bucket_label))

  if (!nrow(df)) return(empty)

  # Long aggregation: sum amounts per (Parte, is_provision, bucket_label)
  agg_long <- df |>
    dplyr::group_by(Parte, is_provision, bucket_label) |>
    dplyr::summarise(
      amount        = sum(abs(as.numeric(.data[[ sv ]])), na.rm = TRUE),
      tag_urgent    = any(tag_urgent,    na.rm = TRUE),
      tag_important = any(tag_important, na.rm = TRUE),
      .groups = "drop"
    )

  # Tag per (Parte, is_provision) — worst across all buckets
  tag_by_parte <- agg_long |>
    dplyr::group_by(Parte, is_provision) |>
    dplyr::summarise(
      tag_urgent    = any(tag_urgent,    na.rm = TRUE),
      tag_important = any(tag_important, na.rm = TRUE),
      .groups = "drop"
    )

  # Original currency per Parte — populated only when amounts were FX-converted
  orig_cur_by_parte <- if (".orig_moneda" %in% names(df) && ".is_converted" %in% names(df)) {
    df |>
      dplyr::filter(.data$.is_converted) |>
      dplyr::group_by(Parte, is_provision) |>
      dplyr::summarise(
        orig_cur = paste(sort(unique(.data$.orig_moneda[!is.na(.data$.orig_moneda)])),
                         collapse = "/"),
        .groups = "drop"
      )
  } else {
    tibble::tibble(Parte = character(), is_provision = logical(), orig_cur = character())
  }

  # Pivot to wide
  wide <- agg_long |>
    dplyr::select(Parte, is_provision, bucket_label, amount) |>
    tidyr::pivot_wider(
      id_cols      = c(Parte, is_provision),
      names_from   = bucket_label,
      values_from  = amount,
      values_fill  = 0
    )

  # Ensure every expected bucket column exists
  for (lbl in bucket_labels) {
    if (!(lbl %in% names(wide))) wide[[ lbl ]] <- 0
  }

  wide <- wide |>
    dplyr::mutate(
      Total = rowSums(dplyr::across(dplyr::all_of(bucket_labels)), na.rm = TRUE)
    ) |>
    dplyr::arrange(is_provision, dplyr::desc(Total))

  tot_sum  <- sum(wide$Total, na.rm = TRUE)
  wide$Pct <- if (tot_sum > 0) round(wide$Total / tot_sum * 100, 1) else rep(0, nrow(wide))

  dplyr::left_join(wide, tag_by_parte, by = c("Parte", "is_provision")) |>
    dplyr::left_join(orig_cur_by_parte, by = c("Parte", "is_provision")) |>
    dplyr::mutate(
      tag_urgent    = dplyr::coalesce(tag_urgent,    FALSE),
      tag_important = dplyr::coalesce(tag_important, FALSE),
      orig_cur      = dplyr::if_else(!is.na(orig_cur) & nzchar(orig_cur), orig_cur, NA_character_)
    ) |>
    # Canonical column order
    dplyr::select(Parte, is_provision,
                  dplyr::all_of(bucket_labels),
                  Total, Pct, tag_urgent, tag_important,
                  dplyr::any_of("orig_cur"))
}

# One-row-per-bucket summary: cobros, pagos, net_day, net_cum.
.build_summary <- function(ar_df, ap_df, all_buckets, grouping_used) {
  sv <- "Saldo vencido"

  sum_df <- function(df) {
    if (is.null(df) || !nrow(df) || !(sv %in% names(df)))
      return(tibble::tibble(date_bucket = as.Date(character()), amount = numeric()))
    df |>
      dplyr::group_by(date_bucket) |>
      dplyr::summarise(
        amount = sum(abs(as.numeric(.data[[ sv ]])), na.rm = TRUE),
        .groups = "drop"
      )
  }

  bucket_labels <- .bucket_labels(all_buckets, grouping_used)

  tibble::tibble(date_bucket = all_buckets, bucket_label = bucket_labels) |>
    dplyr::left_join(dplyr::rename(sum_df(ar_df), cobros = amount),
                     by = "date_bucket") |>
    dplyr::left_join(dplyr::rename(sum_df(ap_df), pagos  = amount),
                     by = "date_bucket") |>
    dplyr::mutate(
      cobros  = dplyr::coalesce(cobros, 0),
      pagos   = dplyr::coalesce(pagos,  0),
      net_day = cobros - pagos,
      net_cum = cumsum(net_day)
    ) |>
    dplyr::select(date_bucket, bucket_label, cobros, pagos, net_day, net_cum)
}

# ── Stage 2: Build combined AR+AP data frame ─────────────────────────────────
# Mirrors ledger_module df_combined but without session reactives.
# Calls the same build_ledger_df() pipeline (defined in data_pipeline.R).

build_export_combined_df <- function(shared, ic_mode_val) {

  cmap <- as.list(tryCatch(shared$company_map(), error = function(e) NULL))

  .build_one <- function(ledger) {
    raw <- tryCatch(shared$sap_data()[[ ledger ]], error = function(e) NULL)
    tryCatch(
      build_ledger_df(
        raw_df          = raw,
        ledger          = ledger,
        empresa         = NULL,
        moves_df        = tryCatch(shared$moves_db(),          error = function(e) NULL),
        manual_df       = tryCatch(shared$manual_inv(),        error = function(e) NULL),
        abonos_df       = tryCatch(shared$abonos_db(),        error = function(e) NULL),
        policy_moves_df = tryCatch(shared$policy_moves_db(),  error = function(e) NULL),
        sap_ov          = tryCatch(shared$sap_ov_db(),        error = function(e) NULL),
        provs_df        = if (ledger == "AP")
          tryCatch(shared$pasivos_provisions_db(), error = function(e) NULL) else NULL,
        liabs_df        = if (ledger == "AP")
          tryCatch(shared$pasivos_liabilities_db(), error = function(e) NULL) else NULL,
        company_map     = cmap
      ),
      error = function(e) {
        warning("build_export_combined_df: ", ledger, " pipeline error: ",
                conditionMessage(e))
        NULL
      }
    )
  }

  ar_df <- .build_one("AR")
  ap_df <- .build_one("AP")

  # Filter soft-deleted entries (papelera)
  papelera <- tryCatch(shared$papelera_rv(), error = function(e) NULL)
  if (!is.null(papelera) && nrow(papelera)) {
    .rm_pap <- function(df, led) {
      if (is.null(df) || !nrow(df)) return(df)
      pap <- papelera[ papelera$ledger == led,
                       c("Empresa","Moneda","Documento"), drop = FALSE ]
      if (nrow(pap)) dplyr::anti_join(df, pap, by = c("Empresa","Moneda","Documento"))
      else df
    }
    ar_df <- .rm_pap(ar_df, "AR")
    ap_df <- .rm_pap(ap_df, "AP")
  }

  # Apply IC filter
  interco <- tryCatch(shared$interco_v2(), error = function(e) NULL)
  ic_rfcs <- {
    r <- unique(toupper(trimws(unname(interco$rfcs %||% character()))))
    r[nzchar(r)]
  }

  .ic_filter <- function(df, led, col) {
    if (is.null(df) || !nrow(df)) return(df)
    codes <- tryCatch(build_ic_fullcodes(interco, led), error = function(e) character())
    tryCatch(
      apply_ic_filter(df, mode = ic_mode_val,
                      code_col = col, ic_codes = codes, ic_rfcs = ic_rfcs),
      error = function(e) df
    )
  }

  ar_df <- .ic_filter(ar_df, "AR", "Código de cliente")
  ap_df <- .ic_filter(ap_df, "AP", "Código de proveedor")

  dplyr::bind_rows(ar_df, ap_df)
}

# ── Stage 2: Main aggregation function ───────────────────────────────────────
# Returns a list ready for Stage 3 Word generation.
#
# @param df            combined AR+AP data frame from build_export_combined_df()
# @param tags_df       tags data frame from shared$tags_db()
# @param emp_sel       character vector of selected company full names
# @param date_from     Date
# @param date_to       Date
# @param grouping      "daily" | "weekly" | "auto"
# @param currency_mode "separate" | "fused"
# @param base_cur      base currency code (used when currency_mode == "fused")
# @param fx_rates      named numeric vector, e.g. c(USD = 18.5, EUR = 20.0)
#
# @return list(currencies, data, grouping_used, all_buckets, bucket_labels,
#              date_from, date_to, currency_mode, base_cur, fx_rates, emp_sel)
#   data[[i]] = list(currency, ar_wide, ap_wide, summary)
build_cashflow_export_data <- function(
    df, tags_df, emp_sel,
    date_from, date_to,
    grouping      = "auto",
    currency_mode = "separate",
    base_cur      = "MXN",
    fx_rates      = c()
) {
  if (is.null(df) || !nrow(df)) return(NULL)

  date_from <- as.Date(date_from)
  date_to   <- as.Date(date_to)
  if (is.na(date_from) || is.na(date_to) || date_from > date_to) return(NULL)

  # Filter by empresa + date range
  df <- df |>
    dplyr::filter(Empresa %in% emp_sel,
                  !is.na(FechaEff),
                  as.Date(FechaEff) >= date_from,
                  as.Date(FechaEff) <= date_to)

  if (!nrow(df)) return(NULL)

  # Resolve "auto" grouping
  n_days        <- as.integer(date_to - date_from) + 1L
  grouping_used <- if (grouping == "auto") {
    if (n_days <= 14L) "daily" else "weekly"
  } else grouping

  # Assign date_bucket column
  if (grouping_used == "daily") {
    df$date_bucket <- as.Date(df$FechaEff)
    all_buckets    <- seq.Date(date_from, date_to, by = "day")
  } else {
    df$date_bucket <- lubridate::floor_date(as.Date(df$FechaEff),
                                            "week", week_start = 1)
    first_wk    <- lubridate::floor_date(date_from, "week", week_start = 1)
    last_wk     <- lubridate::floor_date(date_to,   "week", week_start = 1)
    all_buckets <- seq.Date(first_wk, last_wk, by = "week")
  }

  # Attach tag columns
  df <- .attach_tags_to_df(df, tags_df)

  # Currency handling
  if (currency_mode == "fused") {
    df          <- .apply_fx_conversion(df, base_cur, fx_rates)
    result_curs <- base_cur
  } else {
    result_curs <- sort(unique(df$Moneda[ !is.na(df$Moneda) ]))
  }

  bucket_labels <- .bucket_labels(all_buckets, grouping_used)

  # Build per-currency output
  data_list <- lapply(result_curs, function(cur) {
    df_cur <- if (currency_mode == "fused") df
              else dplyr::filter(df, Moneda == cur)

    ar_df <- dplyr::filter(df_cur, Tipo == "AR")
    ap_df <- dplyr::filter(df_cur, Tipo == "AP")

    list(
      currency = cur,
      ar_wide  = .aggregate_to_wide(ar_df, all_buckets, grouping_used),
      ap_wide  = .aggregate_to_wide(ap_df, all_buckets, grouping_used),
      summary  = .build_summary(ar_df, ap_df, all_buckets, grouping_used)
    )
  })

  list(
    currencies    = result_curs,
    data          = data_list,
    grouping_used = grouping_used,
    all_buckets   = all_buckets,
    bucket_labels = bucket_labels,
    date_from     = date_from,
    date_to       = date_to,
    currency_mode = currency_mode,
    base_cur      = base_cur,
    fx_rates      = fx_rates,
    emp_sel       = emp_sel
  )
}

# ── Stage 3: Word document generation ────────────────────────────────────────

# Label lookup for bilingual support.
.cf_lbl <- function(key, lang = "es") {
  labels <- list(
    title          = c(es = "FLUJO DE CAJA PROYECTADO",
                       en = "PROJECTED CASH FLOW"),
    subtitle       = c(es = "Análisis de Cuentas por Cobrar y Pagar",
                       en = "Accounts Receivable & Payable Analysis"),
    period         = c(es = "Período",        en = "Period"),
    companies      = c(es = "Empresas",       en = "Companies"),
    ic_pos         = c(es = "Posición IC",    en = "IC Position"),
    currency       = c(es = "Moneda",         en = "Currency"),
    prepared_by    = c(es = "Preparado por",  en = "Prepared by"),
    generated      = c(es = "Generado",       en = "Generated"),
    confidential   = c(es = "CONFIDENCIAL",   en = "CONFIDENTIAL"),
    exec_summary   = c(es = "Resumen del Período",
                       en = "Period Summary"),
    inflows        = c(es = "Cobros Esperados",  en = "Expected Inflows"),
    outflows       = c(es = "Pagos Programados", en = "Scheduled Payments"),
    net_pos        = c(es = "Posición Neta",     en = "Net Position"),
    period_col     = c(es = "Período",           en = "Period"),
    daily_inflow   = c(es = "Cobros",            en = "Inflows"),
    daily_outflow  = c(es = "Pagos",             en = "Payments"),
    net_day        = c(es = "Neto del Día",      en = "Day Net"),
    net_cum        = c(es = "Pos. Acumulada",    en = "Cumul. Position"),
    cxc_header     = c(es = "COBROS — Cuentas por Cobrar",
                       en = "INFLOWS — Accounts Receivable"),
    cxp_header     = c(es = "PAGOS — Cuentas por Pagar",
                       en = "PAYMENTS — Accounts Payable"),
    counterparty   = c(es = "Contraparte",       en = "Counterparty"),
    total          = c(es = "Total",             en = "Total"),
    pct            = c(es = "%",                 en = "%"),
    subtotal_cxc   = c(es = "SUBTOTAL COBROS",   en = "SUBTOTAL INFLOWS"),
    subtotal_cxp   = c(es = "SUBTOTAL PAGOS",    en = "SUBTOTAL PAYMENTS"),
    net_row        = c(es = "POSICIÓN NETA",      en = "NET POSITION"),
    cum_row        = c(es = "POS. ACUMULADA",     en = "CUMUL. POSITION"),
    glossary       = c(es = "Notas y Glosario",  en = "Notes & Glossary"),
    g_tag_imp      = c(es = "Etiqueta: Importante",  en = "Tag: Important"),
    g_tag_imp_desc = c(es = "Facturas marcadas como importantes — prioridad de seguimiento.",
                       en = "Invoices marked as important — follow-up priority."),
    g_tag_urg      = c(es = "Etiqueta: Urgente",     en = "Tag: Urgent"),
    g_tag_urg_desc = c(es = "Facturas marcadas como urgentes — atención inmediata.",
                       en = "Invoices marked as urgent — requires immediate attention."),
    g_prov         = c(es = "Provisión",         en = "Provision"),
    g_prov_desc    = c(es = "Compromisos de pago programados (pasivos). Los montos son estimados.",
                       en = "Scheduled payment commitments (liabilities). Amounts are estimated."),
    g_method       = c(es = "Metodología",       en = "Methodology"),
    g_method_desc  = c(es = paste0(
      "Las fechas utilizadas corresponden a la Fecha Efectiva de cada documento, ",
      "que considera movimientos manuales, políticas de pago y fechas SAP originales ",
      "(en ese orden de prioridad). Los montos corresponden al saldo vigente neto de abonos."),
                       en = paste0(
      "Dates reflect the Effective Date of each document, ",
      "considering manual moves, payment policies, and original SAP dates ",
      "(in that priority order). Amounts reflect the outstanding balance net of partial payments.")),
    g_disclaimer   = c(es = paste0(
      "Este reporte es un instrumento de planeación; los montos son proyectados ",
      "y no constituyen compromisos formales. La información es confidencial."),
                       en = paste0(
      "This report is a planning instrument; amounts are projected ",
      "and do not constitute formal commitments. Information is confidential.")),
    no_movements   = c(es = "Sin movimientos en este período",
                       en = "No movements in this period")
  )
  v <- labels[[ key ]]
  if (is.null(v)) return(key)
  v[[ lang ]] %||% v[["es"]]
}

# Format a money amount for display (no currency symbol — label is in header).
.cf_fmt <- function(x, prefix = "$") {
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return("—")
  paste0(prefix, format(round(x), big.mark = ",", scientific = FALSE))
}

# ── Cover page ────────────────────────────────────────────────────────────────
.add_cover_page <- function(doc, export_data, emp_initials,
                             ic_label, user_name, lang, bilingual,
                             group_name = "Networks Group",
                             group_logo_raw = NULL,
                             group_logo_ext = "png") {
  L <- function(k) {
    if (bilingual) paste0(.cf_lbl(k, "es"), " / ", .cf_lbl(k, "en"))
    else .cf_lbl(k, lang)
  }

  m_es <- c("enero","febrero","marzo","abril","mayo","junio",
            "julio","agosto","septiembre","octubre","noviembre","diciembre")
  fmt_date_long <- function(d) {
    sprintf("%d de %s de %d",
            lubridate::day(d), m_es[lubridate::month(d)], lubridate::year(d))
  }

  # ── Logo block ───────────────────────────────────────────────────────────
  doc <- officer::body_add_par(doc, "", style = "Normal")
  if (!is.null(group_logo_raw) && length(group_logo_raw) > 0) {
    logo_tmp <- tempfile(fileext = paste0(".", group_logo_ext %||% "png"))
    tryCatch({
      writeBin(group_logo_raw, logo_tmp)
      doc <- officer::body_add_fpar(doc,
        officer::fpar(
          officer::external_img(src = logo_tmp, width = 4, height = 1.5),
          fp_p = officer::fp_par(text.align = "center")
        )
      )
    }, error = function(e) {
      doc <<- officer::body_add_fpar(doc,
        officer::fpar(officer::ftext("[ Logo ]",
          officer::fp_text(font.size = 11, font.family = "Yoxall", color = "#999999")),
          fp_p = officer::fp_par(text.align = "center")))
    }, finally = {
      if (file.exists(logo_tmp)) unlink(logo_tmp)
    })
  } else {
    doc <- officer::body_add_fpar(doc,
      officer::fpar(officer::ftext("[ Logo ]",
        officer::fp_text(font.size = 11, font.family = "Yoxall", color = "#999999")),
        fp_p = officer::fp_par(text.align = "center")))
  }
  doc <- officer::body_add_par(doc, "", style = "Normal")

  # ── Title block ──────────────────────────────────────────────────────────
  group_display <- group_name %||% "Networks Group"

  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_fpar(doc,
    officer::fpar(officer::ftext(.cf_lbl("title", lang),
      officer::fp_text(font.size = 22, bold = TRUE,
                       font.family = "Yoxall", color = "#1E3A5F")),
      fp_p = officer::fp_par(text.align = "center")))
  doc <- officer::body_add_fpar(doc,
    officer::fpar(officer::ftext(.cf_lbl("subtitle", lang),
      officer::fp_text(font.size = 12, font.family = "Yoxall", color = "#444444")),
      fp_p = officer::fp_par(text.align = "center")))
  doc <- officer::body_add_fpar(doc,
    officer::fpar(officer::ftext(group_display,
      officer::fp_text(font.size = 14, bold = TRUE,
                       font.family = "Yoxall", color = "#1E3A5F")),
      fp_p = officer::fp_par(text.align = "center")))

  # ── Metadata table ───────────────────────────────────────────────────────
  cur_str <- if (export_data$currency_mode == "fused") {
    rates_str <- paste(
      mapply(function(cur, rate) sprintf("%s/%s %s", cur, export_data$base_cur,
                                         format(rate, nsmall = 2)),
             names(export_data$fx_rates), export_data$fx_rates),
      collapse = " | "
    )
    paste0(export_data$base_cur,
           if (nzchar(rates_str)) paste0(" (TC: ", rates_str, ")") else "")
  } else {
    paste(export_data$currencies, collapse = " · ")
  }

  meta_df <- data.frame(
    Clave = c(
      L("period"),
      L("companies"),
      L("ic_pos"),
      L("currency"),
      L("generated"),
      L("prepared_by")
    ),
    Valor = c(
      paste0(fmt_date_long(export_data$date_from), " — ",
             fmt_date_long(export_data$date_to)),
      paste(emp_initials, collapse = " · "),
      ic_label,
      cur_str,
      format(Sys.time(), "%d/%m/%Y %H:%M"),
      user_name %||% "—"
    ),
    stringsAsFactors = FALSE
  )

  meta_ft <- flextable::flextable(meta_df) |>
    flextable::delete_part("header") |>
    flextable::width(j = 1, width = 4.5, unit = "cm") |>
    flextable::width(j = 2, width = 11.5, unit = "cm") |>
    flextable::bold(j = 1) |>
    flextable::fontsize(size = 10) |>
    flextable::font(fontname = "Yoxall") |>
    flextable::border_remove() |>
    flextable::border(border.bottom = officer::fp_border(color = "#E0E0E0", width = 0.5)) |>
    flextable::align(j = 1, align = "right") |>
    flextable::align(j = 2, align = "left") |>
    flextable::padding(padding = 4)

  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- flextable::body_add_flextable(doc, meta_ft, align = "center")
  doc <- officer::body_add_par(doc, "", style = "Normal")

  # Confidential footer
  doc <- officer::body_add_fpar(doc,
    officer::fpar(officer::ftext(L("confidential"),
      officer::fp_text(font.size = 8, italic = TRUE,
                       font.family = "Yoxall", color = "#888888")),
      fp_p = officer::fp_par(text.align = "center")))

  doc
}

# ── Executive summary section ─────────────────────────────────────────────────
.add_summary_section <- function(doc, cd, lang, bilingual) {
  L <- function(k) {
    if (bilingual) paste0(.cf_lbl(k, "es"), " / ", .cf_lbl(k, "en"))
    else .cf_lbl(k, lang)
  }

  summ          <- cd$summary
  total_cobros  <- sum(summ$cobros, na.rm = TRUE)
  total_pagos   <- sum(summ$pagos,  na.rm = TRUE)
  net_period    <- total_cobros - total_pagos

  # ── Currency + section heading ────────────────────────────────────────────
  doc <- officer::body_add_par(doc, "", style = "Normal")
  hdr_ft <- flextable::flextable(data.frame(V = paste0(L("exec_summary"), " — ", cd$currency))) |>
    flextable::delete_part("header") |>
    flextable::bg(bg = "#1E3A5F") |>
    flextable::color(color = "#FFFFFF") |>
    flextable::bold(bold = TRUE) |>
    flextable::font(fontname = "Yoxall") |>
    flextable::fontsize(size = 11) |>
    flextable::set_table_properties(width = 1, layout = "autofit") |>
    flextable::border_remove() |>
    flextable::padding(padding.top = 6, padding.bottom = 6,
                       padding.left = 10, padding.right = 10)
  doc <- flextable::body_add_flextable(doc, hdr_ft)

  # ── KPI row (3-cell mini-table) ───────────────────────────────────────────
  kpi_df <- data.frame(
    A = c(L("inflows"),  .cf_fmt(total_cobros)),
    B = c(L("outflows"), .cf_fmt(total_pagos)),
    C = c(L("net_pos"),  .cf_fmt(net_period)),
    stringsAsFactors = FALSE
  )

  net_bg <- if (net_period >= 0) "#1A5C2E" else "#7B1111"

  kpi_ft <- flextable::flextable(kpi_df) |>
    flextable::delete_part("header") |>
    flextable::width(j = 1:3, width = 5.4, unit = "cm") |>
    flextable::fontsize(size = 11) |>
    flextable::font(fontname = "Yoxall") |>
    flextable::bold(i = 2) |>
    flextable::fontsize(i = 2, size = 14) |>
    flextable::bg(i = 1:2, j = 1, bg = "#1A5C2E") |>
    flextable::bg(i = 1:2, j = 2, bg = "#8B3A0A") |>
    flextable::bg(i = 1:2, j = 3, bg = "#1E3A5F") |>
    flextable::color(i = 1:2, j = 1:3, color = "#FFFFFF") |>
    flextable::align(i = 1:2, j = 1:3, align = "center") |>
    flextable::padding(padding = 6) |>
    flextable::border_remove()

  doc <- flextable::body_add_flextable(doc, kpi_ft, align = "center")
  doc <- officer::body_add_par(doc, "", style = "Normal")

  # ── Daily / weekly summary table ─────────────────────────────────────────
  summ_disp <- summ |>
    dplyr::mutate(
      `_Período`    = bucket_label,
      `_Cobros`     = sapply(cobros,  .cf_fmt),
      `_Pagos`      = sapply(pagos,   .cf_fmt),
      `_Neto`       = sapply(net_day, .cf_fmt),
      `_Acumulado`  = sapply(net_cum, .cf_fmt)
    ) |>
    dplyr::select(`_Período`, `_Cobros`, `_Pagos`, `_Neto`, `_Acumulado`)

  names(summ_disp) <- c(
    L("period_col"), L("daily_inflow"), L("daily_outflow"),
    L("net_day"), L("net_cum")
  )

  summ_ft <- flextable::flextable(summ_disp) |>
    flextable::fontsize(size = 9) |>
    flextable::font(fontname = "Yoxall") |>
    flextable::bold(i = 1, part = "header") |>
    flextable::bg(part = "header", bg = "#1E3A5F") |>
    flextable::color(part = "header", color = "#FFFFFF") |>
    flextable::align(j = 2:5, align = "right") |>
    flextable::align(j = 1, align = "left") |>
    flextable::border(part = "all",
                      border = officer::fp_border(color = "#CCCCCC", width = 0.5)) |>
    flextable::padding(padding = 3) |>
    flextable::autofit()

  # Color net rows: positive green, negative red
  net_vals <- summ$net_day
  for (i in seq_along(net_vals)) {
    clr <- if (net_vals[i] >= 0) "#1A5C2E" else "#7B1111"
    summ_ft <- flextable::color(summ_ft, i = i, j = 4, color = clr)
    summ_ft <- flextable::color(summ_ft, i = i, j = 5,
                                color = if (summ$net_cum[i] >= 0) "#1A5C2E" else "#7B1111")
    if (i %% 2 == 0) summ_ft <- flextable::bg(summ_ft, i = i, bg = "#F5F8FC")
  }

  doc <- flextable::body_add_flextable(doc, summ_ft, align = "center")
  doc
}

# ── Detail section (CxC or CxP) ──────────────────────────────────────────────
.add_detail_section <- function(doc, wide_df, header_key,
                                 subtotal_key, lang, bilingual,
                                 amount_color = "#1A5C2E") {
  L <- function(k) {
    if (bilingual) paste0(.cf_lbl(k, "es"), " / ", .cf_lbl(k, "en"))
    else .cf_lbl(k, lang)
  }

  doc <- officer::body_add_par(doc, "", style = "Normal")
  sec_ft <- flextable::flextable(data.frame(V = L(header_key))) |>
    flextable::delete_part("header") |>
    flextable::bg(bg = "#1E3A5F") |>
    flextable::color(color = "#FFFFFF") |>
    flextable::bold(bold = TRUE) |>
    flextable::font(fontname = "Yoxall") |>
    flextable::fontsize(size = 11) |>
    flextable::set_table_properties(width = 1, layout = "autofit") |>
    flextable::border_remove() |>
    flextable::padding(padding.top = 6, padding.bottom = 6,
                       padding.left = 10, padding.right = 10)
  doc <- flextable::body_add_flextable(doc, sec_ft)

  if (is.null(wide_df) || !nrow(wide_df)) {
    doc <- officer::body_add_fpar(doc,
      officer::fpar(officer::ftext(paste0("— ", L("no_movements"), " —"),
        officer::fp_text(font.size = 10, italic = TRUE,
                         font.family = "Calibri", color = "#888888")),
        fp_p = officer::fp_par(text.align = "center")))
    return(doc)
  }

  # Identify bucket columns (all between is_provision and Total)
  fixed_end <- c("Total", "Pct", "tag_urgent", "tag_important", "is_provision", "orig_cur")
  bucket_cols <- setdiff(names(wide_df), c("Parte", fixed_end))

  # Build display frame (drop internal flag columns, rename)
  # Append subtle (CURR) tag to counterparty names when amounts were FX-converted
  parte_labels <- if ("orig_cur" %in% names(wide_df)) {
    dplyr::if_else(!is.na(wide_df$orig_cur),
                   paste0(wide_df$Parte, " (", wide_df$orig_cur, ")"),
                   wide_df$Parte)
  } else {
    wide_df$Parte
  }

  disp <- wide_df |>
    dplyr::select(Parte, dplyr::all_of(bucket_cols), Total, Pct) |>
    dplyr::mutate(Parte = parte_labels)

  # Compute subtotal row
  sub_row <- data.frame(
    Parte = L(subtotal_key),
    stringsAsFactors = FALSE
  )
  for (col in bucket_cols) sub_row[[ col ]] <- sum(wide_df[[ col ]], na.rm = TRUE)
  sub_row$Total <- sum(wide_df$Total, na.rm = TRUE)
  sub_row$Pct   <- 100

  # Format all amount columns
  amount_cols <- c(bucket_cols, "Total")
  for (col in amount_cols) {
    disp[[ col ]] <- sapply(disp[[ col ]], .cf_fmt)
  }
  disp$Pct <- paste0(round(wide_df$Pct, 1), "%")

  sub_row_disp <- sub_row
  for (col in amount_cols) sub_row_disp[[ col ]] <- .cf_fmt(sub_row[[ col ]])
  sub_row_disp$Pct <- "100%"

  names(disp)[1] <- L("counterparty")
  names(sub_row_disp)[1] <- L("counterparty")
  names(disp)[ names(disp) == "Total" ] <- L("total")
  names(disp)[ names(disp) == "Pct"   ] <- L("pct")
  names(sub_row_disp)[ names(sub_row_disp) == "Total" ] <- L("total")
  names(sub_row_disp)[ names(sub_row_disp) == "Pct"   ] <- L("pct")

  full_df <- rbind(disp, sub_row_disp)
  n_data  <- nrow(disp)

  ft <- flextable::flextable(full_df) |>
    flextable::fontsize(size = 9) |>
    flextable::font(fontname = "Yoxall") |>
    flextable::bold(i = 1, part = "header") |>
    flextable::bg(part = "header", bg = "#1E3A5F") |>
    flextable::color(part = "header", color = "#FFFFFF") |>
    flextable::align(j = 2:ncol(full_df), align = "right") |>
    flextable::align(j = 1, align = "left") |>
    flextable::border(part = "all",
                      border = officer::fp_border(color = "#CCCCCC", width = 0.5)) |>
    flextable::padding(padding = 3) |>
    # Subtotal row: light blue background, bold
    flextable::bg(i = nrow(full_df), bg = "#D6E4F0") |>
    flextable::bold(i = nrow(full_df)) |>
    # Alternate row shading
    flextable::bg(i = which(seq_len(n_data) %% 2 == 0), bg = "#F5F8FC") |>
    # Amount columns: tinted color (all cols except label and Pct)
    flextable::color(i = seq_len(n_data), j = 2:(ncol(full_df) - 1L), color = amount_color) |>
    flextable::set_table_properties(width = 1, layout = "autofit")

  # Tag colors for data rows
  for (i in seq_len(n_data)) {
    if (isTRUE(wide_df$tag_urgent[i]) && isTRUE(wide_df$tag_important[i])) {
      ft <- flextable::color(ft, i = i, j = 1, color = "#8B0000")
    } else if (isTRUE(wide_df$tag_urgent[i])) {
      ft <- flextable::color(ft, i = i, j = 1, color = "#8B1A1A")
    } else if (isTRUE(wide_df$tag_important[i])) {
      ft <- flextable::color(ft, i = i, j = 1, color = "#8B6914")
    }
    # Provision rows: italic gray
    if (isTRUE(wide_df$is_provision[i])) {
      ft <- flextable::color(ft,  i = i, color = "#606060")
      ft <- flextable::italic(ft, i = i)
    }
  }

  doc <- flextable::body_add_flextable(doc, ft, align = "center")
  doc
}

# ── Glossary page ─────────────────────────────────────────────────────────────
.add_glossary <- function(doc, export_data, lang, bilingual) {
  L <- function(k) {
    if (bilingual) paste0(.cf_lbl(k, "es"), " / ", .cf_lbl(k, "en"))
    else .cf_lbl(k, lang)
  }
  Lp <- function(k) .cf_lbl(k, lang)  # pure lang (no bilingual merge) for body text

  doc <- officer::body_add_break(doc)
  glos_ft <- flextable::flextable(data.frame(V = L("glossary"))) |>
    flextable::delete_part("header") |>
    flextable::bg(bg = "#1E3A5F") |>
    flextable::color(color = "#FFFFFF") |>
    flextable::bold(bold = TRUE) |>
    flextable::font(fontname = "Yoxall") |>
    flextable::fontsize(size = 11) |>
    flextable::set_table_properties(width = 1, layout = "autofit") |>
    flextable::border_remove() |>
    flextable::padding(padding.top = 6, padding.bottom = 6,
                       padding.left = 10, padding.right = 10)
  doc <- flextable::body_add_flextable(doc, glos_ft)

  gloss_rows <- list(
    c(L("g_tag_imp"), Lp("g_tag_imp_desc"), "#8B6914", FALSE),
    c(L("g_tag_urg"), Lp("g_tag_urg_desc"), "#8B1A1A", FALSE),
    c(L("g_prov"),    Lp("g_prov_desc"),    "#606060", TRUE),
    c(L("g_method"),  Lp("g_method_desc"),  "#000000", FALSE),
    c(L("g_disclaimer"), Lp("g_disclaimer"),  "#555555", FALSE)
  )

  # FX disclosure (only in fused mode)
  if (export_data$currency_mode == "fused" && length(export_data$fx_rates)) {
    rates_str <- paste(
      mapply(function(cur, rate) sprintf("%s/%s %s",
                                         cur, export_data$base_cur,
                                         format(rate, nsmall = 2)),
             names(export_data$fx_rates), export_data$fx_rates),
      collapse = " | "
    )
    fx_label <- if (lang == "en") "Exchange Rates Used" else "Tipos de Cambio Utilizados"
    fx_desc  <- if (lang == "en")
      paste0("(*) Amounts converted using: ", rates_str,
             ". These are estimates and may differ from actual settlement rates.")
    else
      paste0("(*) Montos convertidos usando: ", rates_str,
             ". Estos son estimados y pueden diferir del tipo de cambio de liquidación.")
    gloss_rows <- c(list(c(fx_label, fx_desc, "#000000", FALSE)), gloss_rows)
  }

  gdf <- data.frame(
    Concepto    = sapply(gloss_rows, `[[`, 1),
    Descripcion = sapply(gloss_rows, `[[`, 2),
    stringsAsFactors = FALSE
  )

  gft <- flextable::flextable(gdf) |>
    flextable::delete_part("header") |>
    flextable::width(j = 1, width = 4.5, unit = "cm") |>
    flextable::width(j = 2, width = 12.5, unit = "cm") |>
    flextable::fontsize(size = 9) |>
    flextable::font(fontname = "Yoxall") |>
    flextable::bold(j = 1) |>
    flextable::border_remove() |>
    flextable::border(border.bottom = officer::fp_border(color = "#E0E0E0", width = 0.5)) |>
    flextable::align(j = 1, align = "right") |>
    flextable::align(j = 2, align = "left") |>
    flextable::padding(padding = 5)

  for (i in seq_along(gloss_rows)) {
    clr <- gloss_rows[[i]][3]
    gft <- flextable::color(gft, i = i, j = 1, color = clr)
    if (isTRUE(as.logical(gloss_rows[[i]][4])))
      gft <- flextable::italic(gft, i = i)
  }

  doc <- flextable::body_add_flextable(doc, gft, align = "left")
  doc
}

# ── Main Word document builder ────────────────────────────────────────────────
generate_cashflow_word_doc <- function(export_data, emp_initials, emp_full,
                                        ic_label, user_name,
                                        primary_lang   = "es",
                                        secondary_lang = NULL,
                                        group_name     = "Networks Group",
                                        group_logo_raw = NULL,
                                        group_logo_ext = "png") {

  bilingual <- !is.null(secondary_lang) && secondary_lang != primary_lang
  lang      <- primary_lang

  doc <- officer::read_docx()

  # ── Cover page ──────────────────────────────────────────────────────────
  doc <- .add_cover_page(doc, export_data, emp_initials,
                          ic_label, user_name, lang, bilingual,
                          group_name     = group_name,
                          group_logo_raw = group_logo_raw,
                          group_logo_ext = group_logo_ext)

  # ── Per-currency sections ────────────────────────────────────────────────
  for (cd in export_data$data) {
    doc <- officer::body_add_break(doc)

    # Executive summary
    doc <- .add_summary_section(doc, cd, lang, bilingual)

    # CxC detail
    doc <- officer::body_add_break(doc)
    doc <- .add_detail_section(doc, cd$ar_wide,
                                header_key   = "cxc_header",
                                subtotal_key = "subtotal_cxc",
                                lang = lang, bilingual = bilingual,
                                amount_color = "#1A5C2E")

    # CxP detail
    doc <- officer::body_add_break(doc)
    doc <- .add_detail_section(doc, cd$ap_wide,
                                header_key   = "cxp_header",
                                subtotal_key = "subtotal_cxp",
                                lang = lang, bilingual = bilingual,
                                amount_color = "#7B1111")
  }

  # ── Glossary ─────────────────────────────────────────────────────────────
  doc <- .add_glossary(doc, export_data, lang, bilingual)

  # Apply landscape + tight margins as the document default section.
  # body_set_default_section writes into <w:sectPr> without a section-break
  # paragraph, so Word does not append a trailing blank page.
  doc <- officer::body_set_default_section(
    doc,
    officer::prop_section(
      page_size    = officer::page_size(orient = "landscape"),
      page_margins = officer::page_mar(top = 0.75, bottom = 0.75,
                                       left = 0.75, right = 0.75)
    )
  )

  doc
}

# ── HTML document builder (for PDF export via pagedown) ───────────────────────

.CF_CSS <- "
  @page { size: A4 landscape; margin: 1.5cm 1.5cm; }
  body  { font-family: 'Yoxall', Calibri, Arial, sans-serif; font-size: 9pt;
          margin: 0; color: #222; }
  .pb   { page-break-after: always; }
  .cover{ text-align:center; padding: 50px 60px 30px; }
  .cover h1 { font-size:22pt; color:#1E3A5F; font-weight:bold; margin:20px 0 4px; }
  .cover h2 { font-size:14pt; color:#1E3A5F; font-weight:bold; margin:0 0 20px; }
  .cover h3 { font-size:12pt; color:#444; font-weight:normal; margin:0 0 16px; }
  .meta-tbl { width:100%; border-collapse:collapse; font-size:9pt; margin:16px auto;
              max-width:660px; }
  .meta-tbl td { padding:3px 8px; border-bottom:1px solid #E0E0E0; }
  .meta-tbl .k { text-align:right; font-weight:bold; color:#333; width:35%; }
  .meta-tbl .v { text-align:left; }
  .conf { font-size:8pt; font-style:italic; color:#888; margin-top:20px; }
  .sec  { margin: 14px 0 4px; }
  .ft-wrap { overflow-x:auto; margin:4px 0 10px; }
  .no-data { text-align:center; font-style:italic; color:#888; padding:10px; }
"

generate_cashflow_html_doc <- function(export_data, emp_initials, emp_full,
                                        ic_label, user_name,
                                        primary_lang   = "es",
                                        secondary_lang = NULL,
                                        group_name     = "Networks Group",
                                        group_logo_raw = NULL,
                                        group_logo_ext = "png") {

  bilingual <- !is.null(secondary_lang) && secondary_lang != primary_lang
  lang      <- primary_lang
  L  <- function(k) {
    if (bilingual) paste0(.cf_lbl(k, "es"), " / ", .cf_lbl(k, "en"))
    else .cf_lbl(k, lang)
  }

  m_es <- c("enero","febrero","marzo","abril","mayo","junio",
            "julio","agosto","septiembre","octubre","noviembre","diciembre")
  fmt_date_long <- function(d)
    sprintf("%d de %s de %d", lubridate::day(d),
            m_es[lubridate::month(d)], lubridate::year(d))

  h  <- htmltools::tags
  sp <- function(...) h$span(...)
  dv <- function(...) h$div(...)

  # ── Logo ──────────────────────────────────────────────────────────────────
  logo_el <- if (!is.null(group_logo_raw) && length(group_logo_raw) > 0) {
    mime <- if ((group_logo_ext %||% "png") == "png") "image/png" else "image/jpeg"
    b64  <- base64enc::base64encode(group_logo_raw)
    h$img(src   = paste0("data:", mime, ";base64,", b64),
          style = "max-height:70px; max-width:180px;")
  } else {
    sp("[ Logo ]", style = "color:#aaa; font-size:11pt;")
  }

  # ── Metadata table ────────────────────────────────────────────────────────
  cur_str <- if (export_data$currency_mode == "fused") {
    r_str <- paste(mapply(
      function(c, r) sprintf("%s/%s %s", c, export_data$base_cur, format(r, nsmall = 2)),
      names(export_data$fx_rates), export_data$fx_rates), collapse = " | ")
    paste0(export_data$base_cur,
           if (nzchar(r_str)) paste0(" (TC: ", r_str, ")") else "")
  } else paste(export_data$currencies, collapse = " · ")

  meta_rows <- list(
    c(L("period"),     paste0(fmt_date_long(export_data$date_from),
                              " — ", fmt_date_long(export_data$date_to))),
    c(L("companies"),  paste(emp_initials, collapse = " · ")),
    c(L("ic_pos"),     ic_label),
    c(L("currency"),   cur_str),
    c(L("generated"),  format(Sys.time(), "%d/%m/%Y %H:%M")),
    c(L("prepared_by"), user_name %||% "—")
  )
  meta_html <- h$table(class = "meta-tbl",
    lapply(meta_rows, function(r)
      h$tr(h$td(class = "k", r[1]), h$td(class = "v", r[2]))))

  cover <- dv(class = "cover pb",
    logo_el,
    h$h1(.cf_lbl("title", lang)),
    h$h3(.cf_lbl("subtitle", lang)),
    h$h2(group_name %||% "Networks Group"),
    meta_html,
    dv(class = "conf", L("confidential"))
  )

  # ── Helper: flextable → HTML wrapper ─────────────────────────────────────
  ft_html <- function(ft) dv(class = "ft-wrap",
    flextable::htmltools_value(ft, ft.align = "center"))

  # ── Helper: section header ────────────────────────────────────────────────
  sec_hdr <- function(txt) dv(
    style = paste("background:#1E3A5F; color:#fff; font-weight:bold;",
                  "font-size:10pt; padding:5px 10px; margin:10px 0 3px;"), txt)

  # ── Per-currency sections ─────────────────────────────────────────────────
  currency_sections <- lapply(export_data$data, function(cd) {
    summ         <- cd$summary
    total_cobros <- sum(summ$cobros, na.rm = TRUE)
    total_pagos  <- sum(summ$pagos,  na.rm = TRUE)
    net_period   <- total_cobros - total_pagos

    # KPI flextable
    kpi_df <- data.frame(
      A = c(L("inflows"),  .cf_fmt(total_cobros)),
      B = c(L("outflows"), .cf_fmt(total_pagos)),
      C = c(L("net_pos"),  .cf_fmt(net_period)), stringsAsFactors = FALSE)
    kpi_ft <- flextable::flextable(kpi_df) |>
      flextable::delete_part("header") |>
      flextable::width(j = 1:3, width = 5.4, unit = "cm") |>
      flextable::fontsize(size = 11) |> flextable::font(fontname = "Yoxall") |>
      flextable::bold(i = 2) |> flextable::fontsize(i = 2, size = 14) |>
      flextable::bg(i = 1:2, j = 1, bg = "#1A5C2E") |>
      flextable::bg(i = 1:2, j = 2, bg = "#8B3A0A") |>
      flextable::bg(i = 1:2, j = 3, bg = "#1E3A5F") |>
      flextable::color(i = 1:2, j = 1:3, color = "#FFFFFF") |>
      flextable::align(i = 1:2, j = 1:3, align = "center") |>
      flextable::padding(padding = 6) |> flextable::border_remove()

    # Summary flextable
    summ_disp <- summ |>
      dplyr::mutate(
        `_P` = bucket_label,
        `_C` = sapply(cobros,  .cf_fmt),
        `_G` = sapply(pagos,   .cf_fmt),
        `_N` = sapply(net_day, .cf_fmt),
        `_A` = sapply(net_cum, .cf_fmt)
      ) |> dplyr::select(`_P`, `_C`, `_G`, `_N`, `_A`)
    names(summ_disp) <- c(L("period_col"), L("daily_inflow"),
                          L("daily_outflow"), L("net_day"), L("net_cum"))
    summ_ft <- flextable::flextable(summ_disp) |>
      flextable::fontsize(size = 9) |> flextable::font(fontname = "Yoxall") |>
      flextable::bold(i = 1, part = "header") |>
      flextable::bg(part = "header", bg = "#1E3A5F") |>
      flextable::color(part = "header", color = "#FFFFFF") |>
      flextable::align(j = 1, align = "left") |>
      flextable::align(j = 2:5, align = "right") |>
      flextable::border(part = "all",
        border = officer::fp_border(color = "#CCCCCC", width = 0.5)) |>
      flextable::padding(padding = 3) |> flextable::autofit()
    for (i in seq_along(summ$net_day)) {
      clr <- if (summ$net_day[i] >= 0) "#1A5C2E" else "#7B1111"
      summ_ft <- flextable::color(summ_ft, i = i, j = 4, color = clr)
      summ_ft <- flextable::color(summ_ft, i = i, j = 5,
        color = if (summ$net_cum[i] >= 0) "#1A5C2E" else "#7B1111")
      if (i %% 2 == 0) summ_ft <- flextable::bg(summ_ft, i = i, bg = "#F5F8FC")
    }

    # Detail section builder
    make_detail <- function(wide_df, header_key, subtotal_key, amount_color) {
      if (is.null(wide_df) || !nrow(wide_df)) {
        return(htmltools::tagList(
          sec_hdr(L(header_key)),
          dv(class = "no-data", paste0("— ", L("no_movements"), " —"))
        ))
      }
      fixed_end <- c("Total","Pct","tag_urgent","tag_important","is_provision","orig_cur")
      bucket_cols <- setdiff(names(wide_df), c("Parte", fixed_end))
      parte_labels <- if ("orig_cur" %in% names(wide_df)) {
        dplyr::if_else(!is.na(wide_df$orig_cur),
                       paste0(wide_df$Parte, " (", wide_df$orig_cur, ")"),
                       wide_df$Parte)
      } else {
        wide_df$Parte
      }
      disp <- wide_df |>
        dplyr::select(Parte, dplyr::all_of(bucket_cols), Total, Pct) |>
        dplyr::mutate(Parte = parte_labels)
      sub_row <- data.frame(Parte = L(subtotal_key), stringsAsFactors = FALSE)
      for (col in bucket_cols) sub_row[[col]] <- sum(wide_df[[col]], na.rm = TRUE)
      sub_row$Total <- sum(wide_df$Total, na.rm = TRUE)
      sub_row$Pct   <- 100
      amount_cols <- c(bucket_cols, "Total")
      for (col in amount_cols) disp[[col]] <- sapply(disp[[col]], .cf_fmt)
      disp$Pct <- paste0(round(wide_df$Pct, 1), "%")
      sub_disp <- sub_row
      for (col in amount_cols) sub_disp[[col]] <- .cf_fmt(sub_row[[col]])
      sub_disp$Pct <- "100%"
      names(disp)[1] <- L("counterparty")
      names(sub_disp)[1] <- L("counterparty")
      names(disp)[names(disp) == "Total"] <- L("total")
      names(disp)[names(disp) == "Pct"]   <- L("pct")
      names(sub_disp)[names(sub_disp) == "Total"] <- L("total")
      names(sub_disp)[names(sub_disp) == "Pct"]   <- L("pct")
      full_df <- rbind(disp, sub_disp)
      n_data  <- nrow(disp)
      ft <- flextable::flextable(full_df) |>
        flextable::fontsize(size = 9) |> flextable::font(fontname = "Yoxall") |>
        flextable::bold(i = 1, part = "header") |>
        flextable::bg(part = "header", bg = "#1E3A5F") |>
        flextable::color(part = "header", color = "#FFFFFF") |>
        flextable::align(j = 2:ncol(full_df), align = "right") |>
        flextable::align(j = 1, align = "left") |>
        flextable::border(part = "all",
          border = officer::fp_border(color = "#CCCCCC", width = 0.5)) |>
        flextable::padding(padding = 3) |>
        flextable::bg(i = nrow(full_df), bg = "#D6E4F0") |>
        flextable::bold(i = nrow(full_df)) |>
        flextable::bg(i = which(seq_len(n_data) %% 2 == 0), bg = "#F5F8FC") |>
        flextable::color(i = seq_len(n_data), j = 2:(ncol(full_df) - 1L),
                         color = amount_color) |>
        flextable::autofit()
      for (i in seq_len(n_data)) {
        if (isTRUE(wide_df$tag_urgent[i]) && isTRUE(wide_df$tag_important[i]))
          ft <- flextable::color(ft, i = i, j = 1, color = "#8B0000")
        else if (isTRUE(wide_df$tag_urgent[i]))
          ft <- flextable::color(ft, i = i, j = 1, color = "#8B1A1A")
        else if (isTRUE(wide_df$tag_important[i]))
          ft <- flextable::color(ft, i = i, j = 1, color = "#8B6914")
        if (isTRUE(wide_df$is_provision[i])) {
          ft <- flextable::color(ft,  i = i, color = "#606060")
          ft <- flextable::italic(ft, i = i)
        }
      }
      htmltools::tagList(sec_hdr(L(header_key)), ft_html(ft))
    }

    htmltools::tagList(
      dv(style = "page-break-before:always;"),
      sec_hdr(paste0(L("exec_summary"), " — ", cd$currency)),
      ft_html(kpi_ft),
      ft_html(summ_ft),
      dv(style = "page-break-before:always;"),
      make_detail(cd$ar_wide, "cxc_header", "subtotal_cxc", "#1A5C2E"),
      dv(style = "page-break-before:always;"),
      make_detail(cd$ap_wide, "cxp_header", "subtotal_cxp", "#7B1111")
    )
  })

  # ── Glossary ──────────────────────────────────────────────────────────────
  Lp <- function(k) .cf_lbl(k, lang)
  gloss_rows <- list(
    c(L("g_tag_imp"), Lp("g_tag_imp_desc"), "#8B6914", FALSE),
    c(L("g_tag_urg"), Lp("g_tag_urg_desc"), "#8B1A1A", FALSE),
    c(L("g_prov"),    Lp("g_prov_desc"),    "#606060", TRUE),
    c(L("g_method"),  Lp("g_method_desc"),  "#000000", FALSE),
    c(L("g_disclaimer"), Lp("g_disclaimer"), "#555555", FALSE)
  )
  if (export_data$currency_mode == "fused" && length(export_data$fx_rates)) {
    r_str    <- paste(mapply(function(c, r)
      sprintf("%s/%s %s", c, export_data$base_cur, format(r, nsmall = 2)),
      names(export_data$fx_rates), export_data$fx_rates), collapse = " | ")
    fx_label <- if (lang == "en") "Exchange Rates Used" else "Tipos de Cambio Utilizados"
    fx_desc  <- if (lang == "en")
      paste0("(*) Amounts converted using: ", r_str,
             ". These are estimates and may differ from actual settlement rates.")
    else
      paste0("(*) Montos convertidos usando: ", r_str,
             ". Estos son estimados y pueden diferir del tipo de cambio de liquidación.")
    gloss_rows <- c(list(c(fx_label, fx_desc, "#000000", FALSE)), gloss_rows)
  }
  gdf <- data.frame(
    Concepto    = sapply(gloss_rows, `[[`, 1),
    Descripcion = sapply(gloss_rows, `[[`, 2),
    stringsAsFactors = FALSE)
  gft <- flextable::flextable(gdf) |>
    flextable::delete_part("header") |>
    flextable::width(j = 1, width = 4.5, unit = "cm") |>
    flextable::width(j = 2, width = 14,  unit = "cm") |>
    flextable::fontsize(size = 9) |> flextable::font(fontname = "Yoxall") |>
    flextable::bold(j = 1) |> flextable::border_remove() |>
    flextable::border(border.bottom = officer::fp_border(color = "#E0E0E0", width = 0.5)) |>
    flextable::align(j = 1, align = "right") |>
    flextable::align(j = 2, align = "left") |>
    flextable::padding(padding = 5)
  for (i in seq_along(gloss_rows)) {
    gft <- flextable::color(gft, i = i, j = 1, color = gloss_rows[[i]][3])
    if (isTRUE(as.logical(gloss_rows[[i]][4])))
      gft <- flextable::italic(gft, i = i)
  }
  glossary_section <- htmltools::tagList(
    dv(style = "page-break-before:always;"),
    sec_hdr(L("glossary")),
    ft_html(gft)
  )

  # ── Assemble full HTML doc ────────────────────────────────────────────────
  html_doc <- htmltools::tagList(
    htmltools::tags$html(
      htmltools::tags$head(
        htmltools::tags$meta(charset = "UTF-8"),
        htmltools::tags$title(paste0(.cf_lbl("title", lang), " — ", group_name)),
        htmltools::tags$style(htmltools::HTML(.CF_CSS))
      ),
      htmltools::tags$body(
        cover,
        currency_sections,
        glossary_section
      )
    )
  )

  tmp_html <- tempfile(fileext = ".html")
  htmltools::save_html(html_doc, tmp_html, background = "white")
  tmp_html
}

# ── Modal UI ──────────────────────────────────────────────────────────────────

show_cashflow_export_modal <- function() {
  showModal(modalDialog(
    title     = tagList(icon("file-export"), " Exportar Flujo de Caja"),
    size      = "m",
    easyClose = TRUE,

    # ── Date range ─────────────────────────────────────────────────────────
    fluidRow(
      column(6,
        tags$label("Desde", class = "form-label fw-semibold small mb-1"),
        dateInput("export_date_from", label = NULL,
                  value    = lubridate::floor_date(Sys.Date(), "week", week_start = 1),
                  format   = "dd/M/yyyy",
                  language = "es",
                  width    = "100%")
      ),
      column(6,
        tags$label("Hasta", class = "form-label fw-semibold small mb-1"),
        dateInput("export_date_to", label = NULL,
                  value    = lubridate::floor_date(Sys.Date(), "week", week_start = 1) + 6L,
                  format   = "dd/M/yyyy",
                  language = "es",
                  width    = "100%")
      )
    ),

    tags$hr(class = "my-3"),

    # ── Grouping cycle button + currency mode toggle ────────────────────────
    fluidRow(
      column(6,
        tags$label("Agrupación de fechas",
                   class = "form-label fw-semibold small mb-1"),
        uiOutput("export_grouping_btn_ui")
      ),
      column(6,
        tags$label("Modo de moneda",
                   class = "form-label fw-semibold small mb-1"),
        uiOutput("export_cur_mode_btn_ui")
      )
    ),

    # ── FX rate inputs (only visible in "Una sola moneda" mode) ────────────
    uiOutput("export_fx_section_ui"),

    tags$hr(class = "my-3"),

    # ── Language selector ───────────────────────────────────────────────────
    tags$label("Idioma del reporte",
               class = "form-label fw-semibold small mb-1"),
    uiOutput("export_lang_ui"),

    tags$hr(class = "my-3"),

    # ── Active-filter summary (reads current toolbar state) ────────────────
    uiOutput("export_info_bar_ui"),

    # ── Format picker: force dropdown to open upward ──────────────────────
    tags$style(HTML("
      .cf-fmt-pick { position: relative; }
      .cf-fmt-pick .selectize-dropdown {
        top: auto !important;
        bottom: calc(100% + 2px) !important;
        border-radius: 4px 4px 0 0;
        border-bottom: none;
        box-shadow: 0 -4px 10px rgba(0,0,0,.12);
      }
    ")),

    # ── Loading feedback + double-download guard ──────────────────────────
    # Uses window._cfDlLast (timestamp) instead of a CSS class so the guard
    # survives renderUI re-renders that replace the DOM element mid-download.
    tags$script(HTML("
      (function() {
        window._cfDlLast = window._cfDlLast || 0;

        $(document).off('click.cfexport')
          .on('click.cfexport', '#export_dl_btn a', function(e) {
            var now = Date.now();
            if (now - window._cfDlLast < 2000) {
              e.preventDefault(); e.stopImmediatePropagation(); return false;
            }
            window._cfDlLast = now;

            var $btn = $(this);
            if ($btn.hasClass('cf-xlsx-trigger')) return; // Excel uses its own state UI

            var fmt      = $('#export_format').val() || 'docx';
            var label    = fmt === 'pdf' ? 'Generando PDF…' : 'Generando Word…';
            var resetMs  = fmt === 'pdf' ? 90000 : 12000;
            var origHtml = $btn.html();

            $btn.css('pointer-events', 'none')
                .html('<span class=\"spinner-border spinner-border-sm me-1\" ' +
                      'role=\"status\" aria-hidden=\"true\"></span>' + label);

            var tid = setTimeout(function() {
              $btn.css('pointer-events', '').html(origHtml);
            }, resetMs);

            $('#export_format').one('change.cfexport', function() {
              clearTimeout(tid);
              $btn.css('pointer-events', '');
            });
          });
      })();
    ")),

    footer = div(
      class = "d-flex align-items-center justify-content-between w-100 cf-footer",
      modalButton("Cancelar"),
      div(class = "d-flex align-items-center gap-2",
        div(class = "cf-fmt-pick",
          selectInput("export_format", label = NULL,
                      choices  = c("Word" = "docx", "PDF" = "pdf", "Excel" = "xlsx"),
                      selected = "docx",
                      width    = "100px")
        ),
        uiOutput("export_dl_btn")
      )
    )
  ))
}

# ── Excel workbook builder ────────────────────────────────────────────────────
generate_cashflow_excel_wb <- function(export_data, emp_initials, emp_full,
                                        ic_label, user_name,
                                        primary_lang   = "es",
                                        secondary_lang = NULL,
                                        group_name     = "Networks Group",
                                        group_logo_raw = NULL,
                                        group_logo_ext = "png") {

  bilingual <- !is.null(secondary_lang) && secondary_lang != primary_lang
  lang      <- primary_lang
  L  <- function(k) {
    if (bilingual) paste0(.cf_lbl(k, "es"), " / ", .cf_lbl(k, "en"))
    else .cf_lbl(k, lang)
  }
  Lp <- function(k) .cf_lbl(k, lang)

  m_es <- c("enero","febrero","marzo","abril","mayo","junio",
            "julio","agosto","septiembre","octubre","noviembre","diciembre")
  fmt_date_long <- function(d)
    sprintf("%d de %s de %d", lubridate::day(d),
            m_es[lubridate::month(d)], lubridate::year(d))

  wb <- openxlsx::createWorkbook(creator = user_name %||% "HopDesk")
  openxlsx::modifyBaseFont(wb, fontName = "Calibri", fontSize = 10)

  # ── Style palette (pre-built once, reused everywhere) ─────────────────────
  ST <- list(
    title       = openxlsx::createStyle(fontSize=22, fontName="Calibri",
                    fontColour="#1E3A5F", textDecoration="bold",
                    halign="center", valign="center"),
    sub         = openxlsx::createStyle(fontSize=12, fontName="Calibri",
                    fontColour="#444444", halign="center", valign="center"),
    grp         = openxlsx::createStyle(fontSize=14, fontName="Calibri",
                    fontColour="#1E3A5F", textDecoration="bold",
                    halign="center", valign="center"),
    conf        = openxlsx::createStyle(fontSize=8,  fontName="Calibri",
                    fontColour="#888888", textDecoration="italic", halign="center"),
    meta_k      = openxlsx::createStyle(fontSize=10, fontName="Calibri",
                    fontColour="#333333", textDecoration="bold", halign="right",
                    border="Bottom", borderColour="#E0E0E0"),
    meta_v      = openxlsx::createStyle(fontSize=10, fontName="Calibri",
                    fontColour="#222222", halign="left",
                    border="Bottom", borderColour="#E0E0E0"),
    sec_hdr     = openxlsx::createStyle(fgFill="#1E3A5F", fontColour="#FFFFFF",
                    fontSize=11, fontName="Calibri", textDecoration="bold",
                    halign="left", valign="center"),
    kpi_lbl_g   = openxlsx::createStyle(fgFill="#1A5C2E", fontColour="#FFFFFF",
                    fontSize=10, fontName="Calibri", textDecoration="bold",
                    halign="center", valign="center"),
    kpi_val_g   = openxlsx::createStyle(fgFill="#1A5C2E", fontColour="#FFFFFF",
                    fontSize=14, fontName="Calibri", textDecoration="bold",
                    halign="center", valign="center", numFmt="#,##0"),
    kpi_lbl_o   = openxlsx::createStyle(fgFill="#8B3A0A", fontColour="#FFFFFF",
                    fontSize=10, fontName="Calibri", textDecoration="bold",
                    halign="center", valign="center"),
    kpi_val_o   = openxlsx::createStyle(fgFill="#8B3A0A", fontColour="#FFFFFF",
                    fontSize=14, fontName="Calibri", textDecoration="bold",
                    halign="center", valign="center", numFmt="#,##0"),
    kpi_lbl_n   = openxlsx::createStyle(fgFill="#1E3A5F", fontColour="#FFFFFF",
                    fontSize=10, fontName="Calibri", textDecoration="bold",
                    halign="center", valign="center"),
    kpi_val_n   = openxlsx::createStyle(fgFill="#1E3A5F", fontColour="#FFFFFF",
                    fontSize=14, fontName="Calibri", textDecoration="bold",
                    halign="center", valign="center", numFmt="#,##0"),
    kpi_val_n_neg = openxlsx::createStyle(fgFill="#1E3A5F", fontColour="#FF8080",
                      fontSize=14, fontName="Calibri", textDecoration="bold",
                      halign="center", valign="center", numFmt="#,##0"),
    tbl_hdr_l   = openxlsx::createStyle(fgFill="#1E3A5F", fontColour="#FFFFFF",
                    fontSize=9, fontName="Calibri", textDecoration="bold",
                    halign="left",  valign="center",
                    border="TopBottomLeftRight", borderColour="#CCCCCC"),
    tbl_hdr_r   = openxlsx::createStyle(fgFill="#1E3A5F", fontColour="#FFFFFF",
                    fontSize=9, fontName="Calibri", textDecoration="bold",
                    halign="right", valign="center",
                    border="TopBottomLeftRight", borderColour="#CCCCCC"),
    sub_l       = openxlsx::createStyle(fgFill="#D6E4F0", fontColour="#000000",
                    fontSize=9, fontName="Calibri", textDecoration="bold",
                    halign="left",  border="TopBottomLeftRight", borderColour="#CCCCCC"),
    sub_r       = openxlsx::createStyle(fgFill="#D6E4F0", fontColour="#000000",
                    fontSize=9, fontName="Calibri", textDecoration="bold",
                    halign="right", numFmt="#,##0",
                    border="TopBottomLeftRight", borderColour="#CCCCCC"),
    sub_pct     = openxlsx::createStyle(fgFill="#D6E4F0", fontColour="#000000",
                    fontSize=9, fontName="Calibri", textDecoration="bold",
                    halign="right", numFmt="0.0%",
                    border="TopBottomLeftRight", borderColour="#CCCCCC"),
    dat_l       = openxlsx::createStyle(fontSize=9, fontName="Calibri", halign="left",
                    border="TopBottomLeftRight", borderColour="#CCCCCC"),
    dat_r       = openxlsx::createStyle(fontSize=9, fontName="Calibri", halign="right",
                    numFmt="#,##0", border="TopBottomLeftRight", borderColour="#CCCCCC"),
    dat_pct     = openxlsx::createStyle(fontSize=9, fontName="Calibri", halign="right",
                    numFmt="0.0%", border="TopBottomLeftRight", borderColour="#CCCCCC"),
    alt_l       = openxlsx::createStyle(fgFill="#F5F8FC", fontSize=9, fontName="Calibri",
                    halign="left",  border="TopBottomLeftRight", borderColour="#CCCCCC"),
    alt_r       = openxlsx::createStyle(fgFill="#F5F8FC", fontSize=9, fontName="Calibri",
                    halign="right", numFmt="#,##0",
                    border="TopBottomLeftRight", borderColour="#CCCCCC"),
    alt_pct     = openxlsx::createStyle(fgFill="#F5F8FC", fontSize=9, fontName="Calibri",
                    halign="right", numFmt="0.0%",
                    border="TopBottomLeftRight", borderColour="#CCCCCC"),
    net_pos     = openxlsx::createStyle(fontSize=9, fontName="Calibri", halign="right",
                    fontColour="#1A5C2E", numFmt="#,##0",
                    border="TopBottomLeftRight", borderColour="#CCCCCC"),
    net_neg     = openxlsx::createStyle(fontSize=9, fontName="Calibri", halign="right",
                    fontColour="#7B1111", numFmt="#,##0",
                    border="TopBottomLeftRight", borderColour="#CCCCCC"),
    net_pos_alt = openxlsx::createStyle(fgFill="#F5F8FC", fontSize=9, fontName="Calibri",
                    halign="right", fontColour="#1A5C2E", numFmt="#,##0",
                    border="TopBottomLeftRight", borderColour="#CCCCCC"),
    net_neg_alt = openxlsx::createStyle(fgFill="#F5F8FC", fontSize=9, fontName="Calibri",
                    halign="right", fontColour="#7B1111", numFmt="#,##0",
                    border="TopBottomLeftRight", borderColour="#CCCCCC"),
    no_mov      = openxlsx::createStyle(fontSize=9, fontName="Calibri", halign="center",
                    fontColour="#888888", textDecoration="italic")
  )

  # ── Page setup helper ─────────────────────────────────────────────────────
  # Note: setHeaderFooter() is intentionally omitted — openxlsx 4.x creates
  # rIdvml and rId drawing rels for every sheet it touches but never writes
  # the vmlDrawing/drawing files, producing corrupt ZIPs that Excel cannot open.
  setup_page <- function(sht) {
    openxlsx::pageSetup(wb, sheet = sht,
      orientation = "landscape", paperSize = 9,
      fitToWidth = TRUE, fitToHeight = FALSE,
      left   = 0.75, right  = 0.75,
      top    = 0.75, bottom = 0.75,
      header = 0.3,  footer = 0.3)
  }

  # ── Cover sheet ───────────────────────────────────────────────────────────
  COV  <- "Portada"
  N_COV <- 8L
  openxlsx::addWorksheet(wb, sheetName = COV, gridLines = FALSE)
  setup_page(COV)
  r <- 1L

  if (!is.null(group_logo_raw) && length(group_logo_raw) > 0) {
    logo_tmp <- tempfile(fileext = paste0(".", group_logo_ext %||% "png"))
    writeBin(group_logo_raw, logo_tmp)
    openxlsx::insertImage(wb, sheet = COV, file = logo_tmp,
      startRow = r, startCol = 1, width = 2.5, height = 0.9,
      units = "in", dpi = 96)
    openxlsx::setRowHeights(wb, COV, rows = r:(r+1L), heights = 40)
    r <- r + 3L
  } else {
    openxlsx::writeData(wb, COV, x = "[ Logo ]", startRow = r, startCol = 1)
    openxlsx::mergeCells(wb, COV, cols = 1:N_COV, rows = r)
    openxlsx::addStyle(wb, COV,
      style = openxlsx::createStyle(fontColour="#AAAAAA", halign="center",
               fontSize=11, fontName="Calibri"),
      rows = r, cols = 1:N_COV, gridExpand = FALSE)
    r <- r + 2L
  }

  for (info in list(
    list(txt = .cf_lbl("title",    lang), st = ST$title, ht = 40L),
    list(txt = .cf_lbl("subtitle", lang), st = ST$sub,   ht = 24L),
    list(txt = group_name %||% "Networks Group", st = ST$grp, ht = 28L)
  )) {
    openxlsx::writeData(wb, COV, x = info$txt, startRow = r, startCol = 1)
    openxlsx::mergeCells(wb, COV, cols = 1:N_COV, rows = r)
    openxlsx::addStyle(wb, COV, style = info$st,
                       rows = r, cols = 1:N_COV, gridExpand = FALSE)
    openxlsx::setRowHeights(wb, COV, rows = r, heights = info$ht)
    r <- r + 1L
  }
  r <- r + 1L

  cur_str <- if (export_data$currency_mode == "fused") {
    rs <- paste(mapply(function(cu, rt)
      sprintf("%s/%s %s", cu, export_data$base_cur, format(rt, nsmall=2)),
      names(export_data$fx_rates), export_data$fx_rates), collapse=" | ")
    paste0(export_data$base_cur, if (nzchar(rs)) paste0(" (TC: ", rs, ")") else "")
  } else paste(export_data$currencies, collapse=" · ")

  meta_kvs <- list(
    c(L("period"),      paste0(fmt_date_long(export_data$date_from),
                               " — ", fmt_date_long(export_data$date_to))),
    c(L("companies"),   paste(emp_initials, collapse=" · ")),
    c(L("ic_pos"),      ic_label),
    c(L("currency"),    cur_str),
    c(L("generated"),   format(Sys.time(), "%d/%m/%Y %H:%M")),
    c(L("prepared_by"), user_name %||% "—")
  )
  KC <- 2L; VC <- 4L
  openxlsx::setColWidths(wb, COV, cols = KC, widths = 24)
  openxlsx::setColWidths(wb, COV, cols = VC, widths = 44)
  for (mv in meta_kvs) {
    openxlsx::writeData(wb, COV, x = mv[1], startRow = r, startCol = KC)
    openxlsx::writeData(wb, COV, x = mv[2], startRow = r, startCol = VC)
    openxlsx::addStyle(wb, COV, style = ST$meta_k, rows = r, cols = KC)
    openxlsx::addStyle(wb, COV, style = ST$meta_v, rows = r, cols = VC)
    openxlsx::mergeCells(wb, COV, cols = VC:(N_COV-1L), rows = r)
    r <- r + 1L
  }
  r <- r + 1L

  openxlsx::writeData(wb, COV, x = L("confidential"), startRow = r, startCol = 1)
  openxlsx::mergeCells(wb, COV, cols = 1:N_COV, rows = r)
  openxlsx::addStyle(wb, COV, style = ST$conf,
                     rows = r, cols = 1:N_COV, gridExpand = FALSE)
  openxlsx::showGridLines(wb, COV, showGridLines = FALSE)

  # ── Per-currency sheets ───────────────────────────────────────────────────
  FC <- c("Parte","Total","Pct","tag_urgent","tag_important","is_provision","orig_cur")

  for (cd in export_data$data) {
    cur <- cd$currency
    sht <- cur
    openxlsx::addWorksheet(wb, sheetName = sht, gridLines = FALSE)
    setup_page(sht)

    summ         <- cd$summary
    total_cobros <- sum(summ$cobros, na.rm = TRUE)
    total_pagos  <- sum(summ$pagos,  na.rm = TRUE)
    net_period   <- total_cobros - total_pagos

    bk_ar  <- if (!is.null(cd$ar_wide) && nrow(cd$ar_wide))
                setdiff(names(cd$ar_wide), FC) else character(0)
    bk_ap  <- if (!is.null(cd$ap_wide) && nrow(cd$ap_wide))
                setdiff(names(cd$ap_wide), FC) else character(0)
    N_SHT  <- 1L + max(length(bk_ar), length(bk_ap), 5L) + 2L
    r <- 1L

    # ── Exec summary section ────────────────────────────────────────────────
    openxlsx::writeData(wb, sht,
      x = paste0(L("exec_summary"), " — ", cur),
      startRow = r, startCol = 1)
    openxlsx::mergeCells(wb, sht, cols = 1:N_SHT, rows = r)
    openxlsx::addStyle(wb, sht, style = ST$sec_hdr,
                       rows = r, cols = 1:N_SHT, gridExpand = FALSE)
    openxlsx::setRowHeights(wb, sht, rows = r, heights = 22)
    r <- r + 1L

    # Pre-compute row positions for KPI formulas.
    # Layout (r=2 here): r=KPI labels, r+1=KPI values, r+2=spacer, r+3=summ hdr, r+4=first data.
    n_s_pre     <- nrow(summ)
    n_ar_pre    <- if (!is.null(cd$ar_wide) && nrow(cd$ar_wide) > 0L) nrow(cd$ar_wide) else 0L
    n_ap_pre    <- if (!is.null(cd$ap_wide) && nrow(cd$ap_wide) > 0L) nrow(cd$ap_wide) else 0L
    n_bk_ar_pre <- length(bk_ar)
    n_bk_ap_pre <- length(bk_ap)

    # r when write_detail(AR) fires = 7 + n_s_pre
    r_ar_sec   <- 7L + n_s_pre
    # AR subtot row = section_hdr(+1) + tbl_hdr(+1) + data rows(+n_ar) = r_ar_sec + 2 + n_ar
    ar_sub_row <- if (n_ar_pre > 0L) r_ar_sec + 2L + n_ar_pre else NULL
    ar_tot_col <- if (n_ar_pre > 0L) 2L + n_bk_ar_pre else NULL

    # r when write_detail(AP) fires depends on whether AR was empty or not
    r_ap_sec   <- if (n_ar_pre > 0L) r_ar_sec + n_ar_pre + 4L else r_ar_sec + 3L
    ap_sub_row <- if (n_ap_pre > 0L) r_ap_sec + 2L + n_ap_pre else NULL
    ap_tot_col <- if (n_ap_pre > 0L) 2L + n_bk_ap_pre else NULL

    summ_first_row <- r + 4L
    summ_last_row  <- summ_first_row + n_s_pre - 1L

    # KPI boxes — 3 groups of 3 columns; reference detail subtotal Total cells when available
    kpis <- list(
      list(lbl=L("inflows"),  val=total_cobros, ls="g",
           formula = if (!is.null(ar_sub_row) && !is.null(ar_tot_col))
             paste0(openxlsx::int2col(ar_tot_col), ar_sub_row)
           else paste0("SUM(B", summ_first_row, ":B", summ_last_row, ")")),
      list(lbl=L("outflows"), val=total_pagos,  ls="o",
           formula = if (!is.null(ap_sub_row) && !is.null(ap_tot_col))
             paste0(openxlsx::int2col(ap_tot_col), ap_sub_row)
           else paste0("SUM(C", summ_first_row, ":C", summ_last_row, ")")),
      list(lbl=L("net_pos"),  val=net_period,   ls="n",
           formula = if (!is.null(ar_sub_row) && !is.null(ar_tot_col) &&
                        !is.null(ap_sub_row) && !is.null(ap_tot_col))
             paste0(openxlsx::int2col(ar_tot_col), ar_sub_row,
                    "-", openxlsx::int2col(ap_tot_col), ap_sub_row)
           else paste0("SUM(D", summ_first_row, ":D", summ_last_row, ")"))
    )
    for (j in seq_along(kpis)) {
      c1 <- (j-1L)*3L + 1L; c3 <- c1 + 2L
      openxlsx::mergeCells(wb, sht, cols=c1:c3, rows=r)
      openxlsx::mergeCells(wb, sht, cols=c1:c3, rows=r+1L)
      openxlsx::writeData(wb, sht, x=kpis[[j]]$lbl, startRow=r, startCol=c1)
      openxlsx::writeFormula(wb, sht, x=kpis[[j]]$formula, startRow=r+1L, startCol=c1)
      ls <- kpis[[j]]$ls
      lbl_st <- switch(ls, g=ST$kpi_lbl_g, o=ST$kpi_lbl_o, n=ST$kpi_lbl_n)
      val_st <- switch(ls,
        g = ST$kpi_val_g, o = ST$kpi_val_o,
        n = if (kpis[[j]]$val >= 0) ST$kpi_val_n else ST$kpi_val_n_neg)
      openxlsx::addStyle(wb, sht, style=lbl_st, rows=r,    cols=c1:c3, gridExpand=TRUE)
      openxlsx::addStyle(wb, sht, style=val_st, rows=r+1L, cols=c1:c3, gridExpand=TRUE)
    }
    openxlsx::setRowHeights(wb, sht, rows=r,    heights=18)
    openxlsx::setRowHeights(wb, sht, rows=r+1L, heights=28)
    r <- r + 3L  # 2 KPI rows + 1 spacer

    # Summary table header
    summ_hdr <- c(L("period_col"), L("daily_inflow"), L("daily_outflow"),
                  L("net_day"), L("net_cum"))
    for (ci in seq_along(summ_hdr)) {
      openxlsx::writeData(wb, sht, x=summ_hdr[ci], startRow=r, startCol=ci)
      openxlsx::addStyle(wb, sht,
        style = if (ci==1L) ST$tbl_hdr_l else ST$tbl_hdr_r,
        rows=r, cols=ci)
    }
    openxlsx::setRowHeights(wb, sht, rows=r, heights=18)
    r <- r + 1L

    # ── Summary table — vectorised (was O(n_rows × 9 calls), now O(13 calls)) ──
    {
      n_s         <- nrow(summ)
      s_seq       <- seq_len(n_s)
      s_rows      <- r + s_seq - 1L
      s_even      <- s_rows[ s_seq %% 2L == 0L]
      s_odd       <- s_rows[ s_seq %% 2L != 0L]

      # Write labels + cobros + pagos as whole columns (3 calls)
      # Cobros/Pagos reference the detail subtotal Total cells so the summary
      # stays live-linked to the AR/AP tables below.
      openxlsx::writeData(wb, sht, summ$bucket_label, startRow=r, startCol=1, colNames=FALSE)
      if (!is.null(ar_sub_row)) {
        openxlsx::writeFormula(wb, sht,
          x        = vapply(seq_len(n_s_pre), function(j)
                       paste0(openxlsx::int2col(1L + j), ar_sub_row), character(1)),
          startRow = r, startCol = 2)
      } else {
        openxlsx::writeData(wb, sht, summ$cobros, startRow=r, startCol=2, colNames=FALSE)
      }
      if (!is.null(ap_sub_row)) {
        openxlsx::writeFormula(wb, sht,
          x        = vapply(seq_len(n_s_pre), function(j)
                       paste0(openxlsx::int2col(1L + j), ap_sub_row), character(1)),
          startRow = r, startCol = 3)
      } else {
        openxlsx::writeData(wb, sht, summ$pagos, startRow=r, startCol=3, colNames=FALSE)
      }

      # net_day = B-C and net_cum = running sum of net_day (2 vectorised formula calls)
      openxlsx::writeFormula(wb, sht,
        x = paste0("B", s_rows, "-C", s_rows), startRow=r, startCol=4)
      openxlsx::writeFormula(wb, sht,
        x = c(paste0("D", r),
              paste0("E", s_rows[-n_s], "+D", s_rows[-1L])),
        startRow = r, startCol = 5)

      # Column-1 styles: batch by even/odd (2 calls)
      if (length(s_odd))  openxlsx::addStyle(wb, sht, ST$dat_l, rows=s_odd,  cols=1)
      if (length(s_even)) openxlsx::addStyle(wb, sht, ST$alt_l, rows=s_even, cols=1)

      # Columns 2-3 styles: batch by even/odd (2 calls)
      if (length(s_odd))  openxlsx::addStyle(wb, sht, ST$dat_r, rows=s_odd,  cols=2:3, gridExpand=TRUE)
      if (length(s_even)) openxlsx::addStyle(wb, sht, ST$alt_r, rows=s_even, cols=2:3, gridExpand=TRUE)

      # Net columns 4 and 5: group rows by (sign × parity) → at most 4 addStyle calls each
      .apply_net_styles <- function(col_idx, vals) {
        neg <- !is.na(vals) & vals < 0
        grps <- list(
          list(rows=s_odd[!neg[s_seq %% 2L != 0L]],  st=ST$net_pos),
          list(rows=s_even[!neg[s_seq %% 2L == 0L]], st=ST$net_pos_alt),
          list(rows=s_odd[ neg[s_seq %% 2L != 0L]],  st=ST$net_neg),
          list(rows=s_even[neg[s_seq %% 2L == 0L]],  st=ST$net_neg_alt)
        )
        for (g in grps)
          if (length(g$rows)) openxlsx::addStyle(wb, sht, g$st, rows=g$rows, cols=col_idx)
      }
      .apply_net_styles(4L, summ$net_day)
      .apply_net_styles(5L, summ$net_cum)

      r <- r + n_s
    }
    r <- r + 1L  # spacer

    # ── Detail section helper — vectorised (uses <<- on r) ──────────────────
    # Was O(n_rows × n_buckets) openxlsx calls; now O(~25) regardless of size.
    write_detail <- function(wide_df, header_key, subtotal_key, amt_hex) {
      # 3 amount-column styles (created once per call, not per row)
      st_amt      <- openxlsx::createStyle(fontSize=9, fontName="Calibri",
                       halign="right", fontColour=amt_hex, numFmt="#,##0",
                       border="TopBottomLeftRight", borderColour="#CCCCCC")
      st_amt_alt  <- openxlsx::createStyle(fgFill="#F5F8FC", fontSize=9,
                       fontName="Calibri", halign="right", fontColour=amt_hex,
                       numFmt="#,##0", border="TopBottomLeftRight", borderColour="#CCCCCC")
      st_amt_prov <- openxlsx::createStyle(fontSize=9, fontName="Calibri",
                       halign="right", fontColour="#606060", textDecoration="italic",
                       numFmt="#,##0", border="TopBottomLeftRight", borderColour="#CCCCCC")

      # 10-entry Parte style cache: (font-colour × bg × italic) — no createStyle in the loop
      .mk_p <- function(fc, bg, td)
        openxlsx::createStyle(fontSize=9, fontName="Calibri", halign="left",
          fontColour=fc, textDecoration=td, fgFill=bg,
          border="TopBottomLeftRight", borderColour="#CCCCCC")
      pst <- list(
        `#000000.W` = .mk_p("#000000", "#FFFFFF", NULL),
        `#000000.A` = .mk_p("#000000", "#F5F8FC", NULL),
        `#8B0000.W` = .mk_p("#8B0000", "#FFFFFF", NULL),
        `#8B0000.A` = .mk_p("#8B0000", "#F5F8FC", NULL),
        `#8B1A1A.W` = .mk_p("#8B1A1A", "#FFFFFF", NULL),
        `#8B1A1A.A` = .mk_p("#8B1A1A", "#F5F8FC", NULL),
        `#8B6914.W` = .mk_p("#8B6914", "#FFFFFF", NULL),
        `#8B6914.A` = .mk_p("#8B6914", "#F5F8FC", NULL),
        `#606060.W` = .mk_p("#606060", "#FFFFFF", "italic"),
        `#606060.A` = .mk_p("#606060", "#F5F8FC", "italic")
      )

      # Section header
      openxlsx::writeData(wb, sht, x=L(header_key), startRow=r, startCol=1)
      openxlsx::mergeCells(wb, sht, cols=1:N_SHT, rows=r)
      openxlsx::addStyle(wb, sht, style=ST$sec_hdr,
                         rows=r, cols=1:N_SHT, gridExpand=FALSE)
      openxlsx::setRowHeights(wb, sht, rows=r, heights=22)
      r <<- r + 1L

      if (is.null(wide_df) || !nrow(wide_df)) {
        openxlsx::writeData(wb, sht,
          x=paste0("— ", L("no_movements"), " —"), startRow=r, startCol=1)
        openxlsx::mergeCells(wb, sht, cols=1:N_SHT, rows=r)
        openxlsx::addStyle(wb, sht, style=ST$no_mov,
                           rows=r, cols=1:N_SHT, gridExpand=FALSE)
        r <<- r + 2L
        return(invisible(NULL))
      }

      bucket_cols <- setdiff(names(wide_df), FC)
      n_bk        <- length(bucket_cols)
      tot_col     <- 1L + n_bk + 1L
      pct_col     <- 1L + n_bk + 2L
      bk_last_ltr <- openxlsx::int2col(1L + n_bk)
      tot_ltr     <- openxlsx::int2col(tot_col)

      parte_labels <- if ("orig_cur" %in% names(wide_df))
        dplyr::if_else(!is.na(wide_df$orig_cur),
                       paste0(wide_df$Parte, " (", wide_df$orig_cur, ")"),
                       wide_df$Parte)
      else wide_df$Parte

      # Table header: 1 writeData + 2 addStyle calls
      col_names <- c(L("counterparty"), bucket_cols, L("total"), L("pct"))
      openxlsx::writeData(wb, sht,
        x = matrix(col_names, nrow=1),
        startRow=r, startCol=1, colNames=FALSE, rowNames=FALSE)
      openxlsx::addStyle(wb, sht, ST$tbl_hdr_l, rows=r, cols=1)
      if (length(col_names) > 1L)
        openxlsx::addStyle(wb, sht, ST$tbl_hdr_r,
                           rows=r, cols=2:length(col_names), gridExpand=TRUE)
      openxlsx::setRowHeights(wb, sht, rows=r, heights=18)
      r <<- r + 1L

      n_rows         <- nrow(wide_df)
      first_data_row <- r
      last_data_row  <- r + n_rows - 1L
      subtot_row     <- r + n_rows
      seq_rows       <- seq_len(n_rows)
      abs_rows       <- seq_rows + first_data_row - 1L
      is_even        <- seq_rows %% 2L == 0L
      even_abs       <- abs_rows[ is_even]
      odd_abs        <- abs_rows[!is_even]

      # ── 1. Parte labels: 1 writeData call ────────────────────────────────
      openxlsx::writeData(wb, sht, x=parte_labels,
                          startRow=first_data_row, startCol=1, colNames=FALSE)

      # ── 2. Bucket values: write entire matrix in 1 call ──────────────────
      if (n_bk > 0L) {
        mat <- as.matrix(wide_df[, bucket_cols, drop=FALSE])
        storage.mode(mat) <- "numeric"
        mat[is.na(mat)] <- 0
        openxlsx::writeData(wb, sht, x=mat,
                            startRow=first_data_row, startCol=2,
                            colNames=FALSE, rowNames=FALSE)
      }

      # ── 3. Total formulas: 1 vectorised writeFormula call ─────────────────
      openxlsx::writeFormula(wb, sht,
        x = paste0("SUM(B", abs_rows, ":", bk_last_ltr, abs_rows, ")"),
        startRow = first_data_row, startCol = tot_col)

      # ── 4. Pct formulas: 1 vectorised writeFormula call ───────────────────
      openxlsx::writeFormula(wb, sht,
        x = paste0("IFERROR(", tot_ltr, abs_rows, "/", tot_ltr, subtot_row, ",0)"),
        startRow = first_data_row, startCol = pct_col)

      # ── 5. Bucket column styles: batch even/odd + provision override ────────
      prov_vec <- !is.na(wide_df$is_provision) & as.logical(wide_df$is_provision)
      prov_abs <- abs_rows[prov_vec]
      if (n_bk > 0L) {
        if (length(odd_abs))
          openxlsx::addStyle(wb, sht, st_amt,     rows=odd_abs,  cols=2:(1L+n_bk), gridExpand=TRUE)
        if (length(even_abs))
          openxlsx::addStyle(wb, sht, st_amt_alt, rows=even_abs, cols=2:(1L+n_bk), gridExpand=TRUE)
        if (length(prov_abs))
          openxlsx::addStyle(wb, sht, st_amt_prov, rows=prov_abs, cols=2:(1L+n_bk), gridExpand=TRUE)
      }

      # ── 6. Total column styles ──────────────────────────────────────────────
      if (length(odd_abs))  openxlsx::addStyle(wb, sht, st_amt,      rows=odd_abs,  cols=tot_col)
      if (length(even_abs)) openxlsx::addStyle(wb, sht, st_amt_alt,  rows=even_abs, cols=tot_col)
      if (length(prov_abs)) openxlsx::addStyle(wb, sht, st_amt_prov, rows=prov_abs, cols=tot_col)

      # ── 7. Pct column styles ────────────────────────────────────────────────
      if (length(odd_abs))  openxlsx::addStyle(wb, sht, ST$dat_pct, rows=odd_abs,  cols=pct_col)
      if (length(even_abs)) openxlsx::addStyle(wb, sht, ST$alt_pct, rows=even_abs, cols=pct_col)

      # ── 8. Parte column styles: batch by style key (≤10 addStyle calls) ────
      urg_vec <- !is.na(wide_df$tag_urgent)    & as.logical(wide_df$tag_urgent)
      imp_vec <- !is.na(wide_df$tag_important) & as.logical(wide_df$tag_important)
      fc_vec  <- rep("#000000", n_rows)
      fc_vec[imp_vec]            <- "#8B6914"
      fc_vec[urg_vec]            <- "#8B1A1A"
      fc_vec[urg_vec & imp_vec]  <- "#8B0000"
      fc_vec[prov_vec]           <- "#606060"
      sk_vec  <- paste0(fc_vec, ifelse(is_even, ".A", ".W"))

      for (sk in unique(sk_vec)) {
        rows <- abs_rows[sk_vec == sk]
        st   <- pst[[sk]]
        if (!is.null(st) && length(rows))
          openxlsx::addStyle(wb, sht, st, rows=rows, cols=1)
      }

      # ── 9. Subtotal row: vectorised formulas + 1 range addStyle each ─────
      openxlsx::writeData(wb, sht, x=L(subtotal_key), startRow=subtot_row, startCol=1)
      openxlsx::addStyle(wb, sht, ST$sub_l, rows=subtot_row, cols=1)
      if (n_bk > 0L) {
        for (bi in seq_len(n_bk)) {
          cl <- openxlsx::int2col(1L + bi)
          openxlsx::writeFormula(wb, sht,
            x        = paste0("SUM(", cl, first_data_row, ":", cl, last_data_row, ")"),
            startRow = subtot_row,
            startCol = 1L + bi)
        }
        openxlsx::addStyle(wb, sht, ST$sub_r,
                           rows=subtot_row, cols=2:(1L+n_bk), gridExpand=TRUE)
      }
      openxlsx::writeFormula(wb, sht,
        x = paste0("SUM(", tot_ltr, first_data_row, ":", tot_ltr, last_data_row, ")"),
        startRow = subtot_row, startCol = tot_col)
      openxlsx::addStyle(wb, sht, ST$sub_r,   rows=subtot_row, cols=tot_col)
      openxlsx::writeData(wb, sht, x=1.0, startRow=subtot_row, startCol=pct_col)
      openxlsx::addStyle(wb, sht, ST$sub_pct, rows=subtot_row, cols=pct_col)
      openxlsx::setRowHeights(wb, sht, rows=subtot_row, heights=18)

      r <<- subtot_row + 2L
      invisible(NULL)
    }

    write_detail(cd$ar_wide, "cxc_header", "subtotal_cxc", "#1A5C2E")
    write_detail(cd$ap_wide, "cxp_header", "subtotal_cxp", "#7B1111")

    openxlsx::setColWidths(wb, sht, cols=1,      widths=32)
    if (N_SHT > 1L)
      openxlsx::setColWidths(wb, sht, cols=2:N_SHT, widths="auto")
    openxlsx::setColWidths(wb, sht, cols=N_SHT, widths=8)  # pct col — auto undersizes it
    openxlsx::freezePane(wb, sht, firstCol=TRUE)
    openxlsx::showGridLines(wb, sht, showGridLines=FALSE)
  }

  # ── Glossary sheet ────────────────────────────────────────────────────────
  glos_sht <- if (lang == "en") "Glossary" else "Glosario"
  openxlsx::addWorksheet(wb, sheetName=glos_sht, gridLines=FALSE)
  setup_page(glos_sht)
  r <- 1L

  openxlsx::writeData(wb, glos_sht, x=L("glossary"), startRow=r, startCol=1)
  openxlsx::mergeCells(wb, glos_sht, cols=1:2, rows=r)
  openxlsx::addStyle(wb, glos_sht, style=ST$sec_hdr,
                     rows=r, cols=1:2, gridExpand=FALSE)
  openxlsx::setRowHeights(wb, glos_sht, rows=r, heights=22)
  r <- r + 1L

  gloss_rows <- list(
    c(L("g_tag_imp"),    Lp("g_tag_imp_desc"),  "#8B6914", FALSE),
    c(L("g_tag_urg"),    Lp("g_tag_urg_desc"),  "#8B1A1A", FALSE),
    c(L("g_prov"),       Lp("g_prov_desc"),     "#606060", TRUE),
    c(L("g_method"),     Lp("g_method_desc"),   "#000000", FALSE),
    c(L("g_disclaimer"), Lp("g_disclaimer"),    "#555555", FALSE)
  )
  if (export_data$currency_mode == "fused" && length(export_data$fx_rates)) {
    rs2 <- paste(mapply(function(cu, rt)
      sprintf("%s/%s %s", cu, export_data$base_cur, format(rt, nsmall=2)),
      names(export_data$fx_rates), export_data$fx_rates), collapse=" | ")
    fx_lbl <- if (lang=="en") "Exchange Rates Used" else "Tipos de Cambio Utilizados"
    fx_dsc <- if (lang=="en")
      paste0("(*) Amounts converted using: ", rs2,
             ". These are estimates and may differ from actual settlement rates.")
    else
      paste0("(*) Montos convertidos usando: ", rs2,
             ". Estos son estimados y pueden diferir del tipo de cambio de liquidación.")
    gloss_rows <- c(list(c(fx_lbl, fx_dsc, "#000000", FALSE)), gloss_rows)
  }

  openxlsx::setColWidths(wb, glos_sht, cols=1, widths=30)
  openxlsx::setColWidths(wb, glos_sht, cols=2, widths=70)
  for (gr in gloss_rows) {
    clr     <- gr[3]
    is_ital <- isTRUE(as.logical(gr[4]))
    openxlsx::writeData(wb, glos_sht, x=gr[1], startRow=r, startCol=1)
    openxlsx::writeData(wb, glos_sht, x=gr[2], startRow=r, startCol=2)
    openxlsx::addStyle(wb, glos_sht,
      style=openxlsx::createStyle(fontSize=9, fontName="Calibri", fontColour=clr,
        textDecoration=if(is_ital) c("bold","italic") else "bold",
        halign="right", border="Bottom", borderColour="#E0E0E0"),
      rows=r, cols=1)
    openxlsx::addStyle(wb, glos_sht,
      style=openxlsx::createStyle(fontSize=9, fontName="Calibri", fontColour=clr,
        textDecoration=if(is_ital) "italic" else NULL,
        halign="left", wrapText=TRUE, border="Bottom", borderColour="#E0E0E0"),
      rows=r, cols=2)
    openxlsx::setRowHeights(wb, glos_sht, rows=r, heights=40)
    r <- r + 1L
  }
  openxlsx::showGridLines(wb, glos_sht, showGridLines=FALSE)

  # ── Fix openxlsx 4.x pre-allocation bug ───────────────────────────────────
  # addWorksheet() registers drawing/vmlDrawing rels and Content_Types entries
  # for EVERY sheet regardless of whether insertImage was called. saveWorkbook()
  # only writes the actual drawing XML for sheets with content, leaving broken
  # package references that cause Excel to replace all worksheets with empty
  # content. Strip out refs for sheets with no drawing content.
  for (.i in seq_along(wb$drawings)) {
    if (length(wb$drawings[[.i]]) == 0L) {
      # No drawing content: remove both the drawing Content-Type and rels entries
      wb$Content_Types <- wb$Content_Types[
        !grepl(sprintf("/xl/drawings/drawing%d\\.xml", .i), wb$Content_Types,
               perl = TRUE)]
      wb$worksheets_rels[[.i]] <- wb$worksheets_rels[[.i]][
        !grepl('/relationships/drawing"|vmlDrawing', wb$worksheets_rels[[.i]],
               perl = TRUE)]
    } else {
      # Has drawing content: keep drawing ref but strip vmlDrawing (we never use VML)
      wb$worksheets_rels[[.i]] <- wb$worksheets_rels[[.i]][
        !grepl('vmlDrawing', wb$worksheets_rels[[.i]], perl = TRUE)]
    }
  }

  wb
}

# ── Server setup (call once from app server) ──────────────────────────────────

setup_cashflow_export_server <- function(input, output, session, shared) {

  # ── Persistent modal state ────────────────────────────────────────────────
  # These survive modal open/close so user preferences are remembered.
  .grp_rv      <- reactiveVal("auto")     # "daily" | "weekly" | "auto"
  .cur_mode_rv <- reactiveVal("separate") # "separate" | "fused"
  .sec_lang_rv <- reactiveVal(FALSE)      # secondary language shown?
  # One-shot FX prefill from preview panel: consumed by export_fx_section_ui renderUI
  .pending_fx  <- reactiveVal(NULL)       # list(base_cur, fx_rates) or NULL

  # ── Async Excel job state ─────────────────────────────────────────────────
  # idle | building | warn60 | done | error | cancelled
  .xl_state    <- reactiveVal("idle")
  # list(job = callr::r_process, snap_path = chr, out_path = chr) or NULL
  .xl_job      <- reactiveVal(NULL)
  .xl_file     <- reactiveVal(NULL)   # path to generated xlsx (state=="done")
  .xl_basename <- reactiveVal(NULL)   # download filename
  .xl_start    <- reactiveVal(NULL)   # POSIXct when job was launched
  .xl_timer    <- reactiveTimer(1000) # 1-second poll tick
  # Elapsed-seconds threshold for next warning dialog; bumped by 60 each time shown
  # so clicking "Seguir esperando" doesn't re-trigger the modal immediately.
  .xl_warn_at  <- reactiveVal(60)

  # ── Open modal ────────────────────────────────────────────────────────────
  observeEvent(input$btn_export, {
    show_cashflow_export_modal()
  })

  # ── Pre-fill from Cash Flow Preview (Stage 5) ─────────────────────────────
  # Fired when the user clicks "Exportar" inside the preview panel.
  # Applies preview settings to the modal before opening it.
  observeEvent(shared$cf_preview_prefill(), {
    pf <- shared$cf_preview_prefill()
    req(!is.null(pf))

    # Apply grouping and currency mode
    if (!is.null(pf$grouping)      && pf$grouping      %in% c("auto","daily","weekly"))
      .grp_rv(pf$grouping)
    if (!is.null(pf$currency_mode) && pf$currency_mode %in% c("separate","fused"))
      .cur_mode_rv(pf$currency_mode)

    # Store FX prefill BEFORE opening modal so renderUI picks it up on first render
    if (pf$currency_mode == "fused") {
      .pending_fx(list(
        base_cur = pf$base_cur %||% "MXN",
        fx_rates = pf$fx_rates %||% list()
      ))
    }

    show_cashflow_export_modal()

    # Date range (these inputs are always in the DOM — update works immediately)
    if (!is.null(pf$date_from) && !is.na(as.Date(pf$date_from)))
      updateDateInput(session, "export_date_from", value = as.Date(pf$date_from))
    if (!is.null(pf$date_to)   && !is.na(as.Date(pf$date_to)))
      updateDateInput(session, "export_date_to",   value = as.Date(pf$date_to))

    # Reset so a second click always fires
    shared$cf_preview_prefill(NULL)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # ── Grouping cycle: Auto → Diario → Semanal → Auto … ─────────────────────
  observeEvent(input$btn_toggle_grouping, {
    cycle <- c(auto = "daily", daily = "weekly", weekly = "auto")
    .grp_rv(cycle[[ .grp_rv() ]])
  }, ignoreInit = TRUE)

  # ── Currency mode toggle ──────────────────────────────────────────────────
  observeEvent(input$btn_toggle_cur_mode, {
    .cur_mode_rv(if (.cur_mode_rv() == "separate") "fused" else "separate")
  }, ignoreInit = TRUE)

  # ── Secondary language ────────────────────────────────────────────────────
  observeEvent(input$btn_add_lang, {
    .sec_lang_rv(TRUE)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(input$btn_remove_lang, {
    .sec_lang_rv(FALSE)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # ── Render: grouping cycle button ─────────────────────────────────────────
  output$export_grouping_btn_ui <- renderUI({
    lbl <- c(
      auto   = "Auto (recomendado)",
      daily  = "Diario",
      weekly = "Semanal"
    )[[ .grp_rv() ]]
    actionButton(
      "btn_toggle_grouping",
      tagList(icon("rotate"), " ", lbl),
      class = "btn btn-outline-secondary btn-sm w-100"
    )
  })

  # ── Render: currency mode toggle button ───────────────────────────────────
  output$export_cur_mode_btn_ui <- renderUI({
    if (.cur_mode_rv() == "separate") {
      actionButton("btn_toggle_cur_mode",
                   tagList(icon("coins"), " Por moneda"),
                   class = "btn btn-outline-secondary btn-sm w-100")
    } else {
      actionButton("btn_toggle_cur_mode",
                   tagList(icon("dollar-sign"), " Una sola moneda"),
                   class = "btn btn-secondary btn-sm w-100")
    }
  })

  # ── Render: FX section (only in fused mode) ───────────────────────────────
  output$export_fx_section_ui <- renderUI({
    if (.cur_mode_rv() != "fused") return(NULL)

    # Consume any pending prefill from the preview panel (isolate = no dependency,
    # so this renderUI won't re-run when .pending_fx resets to NULL)
    pending <- isolate(.pending_fx())
    if (!is.null(pending)) isolate(.pending_fx(NULL))

    base <- if (!is.null(pending) && !is.null(pending$base_cur) && nzchar(pending$base_cur))
              pending$base_cur
            else
              input$export_base_cur %||% "MXN"

    # Determine which non-base currencies appear in the data for
    # the currently selected companies (date-range-agnostic).
    sap     <- shared$sap_data()
    emp     <- shared$empresa_sel()
    # sap is list(AR=df, AP=df) — combine both ledgers for currency detection
    sap_df  <- dplyr::bind_rows(sap[["AR"]], sap[["AP"]])
    if (nrow(sap_df) > 0L && length(emp) > 0L) {
      other_curs <- setdiff(
        intersect(unique(sap_df$Moneda[sap_df$Empresa %in% emp]), CURRENCIES),
        c(base, NA_character_)
      )
    } else {
      other_curs <- setdiff(c("USD", "EUR"), base)
    }

    tagList(
      tags$hr(class = "my-3"),
      tags$label("Moneda base y tipos de cambio",
                 class = "form-label fw-semibold small mb-1"),

      # Base currency selector
      fluidRow(
        column(3,
          tags$label("Base", class = "form-label small text-muted mb-1"),
          selectInput("export_base_cur", label = NULL,
                      choices  = intersect(c("MXN", "USD", "EUR"), CURRENCIES),
                      selected = base,
                      width    = "100%")
        )
      ),

      # One numericInput per non-base currency present in the data
      if (length(other_curs)) {
        do.call(fluidRow, lapply(other_curs, function(cur) {
          # Use prefilled rate if available, otherwise suggest from market data.
          # Guard pending AND key existence before [[]] — NULL[[key]] is an error.
          sug <- if (!is.null(pending) &&
                     cur %in% names(pending$fx_rates) &&
                     is.numeric(pending$fx_rates[[cur]]) &&
                     pending$fx_rates[[cur]] > 0)
                   pending$fx_rates[[cur]]
                 else
                   .suggest_fx_rate(cur, base)
          column(3,
            tags$label(paste0(cur, "/", base),
                       class = "form-label small text-muted mb-1"),
            numericInput(paste0("export_fx_", cur), label = NULL,
                         value = sug, min = 0.0001, step = 0.0001,
                         width = "100%"),
            tags$small(class = "text-muted fst-italic",
                       paste0("sugerido: ", format(round(sug, 4), nsmall = 2)))
          )
        }))
      },

      tags$small(
        class = "text-muted mt-1 d-block",
        icon("circle-info"),
        " Importes convertidos se marcarán con (*) en el reporte."
      )
    )
  })

  # ── Render: language selector ─────────────────────────────────────────────
  output$export_lang_ui <- renderUI({
    has_sec <- .sec_lang_rv()
    pri     <- input$export_lang_primary %||% "es"

    fluidRow(
      class = "align-items-center g-2 cf-lang-row",
      column(5,
        selectInput("export_lang_primary", label = NULL,
                    choices  = EXPORT_LANG_CHOICES,
                    selected = pri,
                    width    = "100%")
      ),
      column(2, class = "text-center",
        if (!has_sec) {
          actionButton("btn_add_lang", "+",
                       class = "btn btn-outline-secondary btn-sm",
                       title = "Agregar segundo idioma (bilingüe)")
        } else {
          actionButton("btn_remove_lang", "−",
                       class = "btn btn-outline-danger btn-sm",
                       title = "Quitar segundo idioma")
        }
      ),
      if (has_sec) {
        sec_choices <- EXPORT_LANG_CHOICES[ EXPORT_LANG_CHOICES != pri ]
        sec_sel     <- input$export_lang_secondary %||%
                       setdiff(c("en", "es"), pri)[1]
        column(5,
          selectInput("export_lang_secondary", label = NULL,
                      choices  = sec_choices,
                      selected = sec_sel,
                      width    = "100%")
        )
      }
    )
  })

  # ── Render: active-filter info bar ────────────────────────────────────────
  output$export_info_bar_ui <- renderUI({
    emp_sel  <- shared$empresa_sel() %||% character(0)
    cmap     <- shared$company_map() %||% COMPANY_MAP
    ic_val   <- input$ic_mode %||% "exclude"

    rev_map  <- setNames(names(cmap), unname(cmap))
    initials <- na.omit(unname(rev_map[ emp_sel ]))
    co_str   <- if (length(initials)) paste(initials, collapse = " · ") else "—"

    ic_label <- c(exclude = "Sin transacciones entre compañías",
                  include = "Con transacciones entre compañías",
                  only    = "Sólo transacciones entre compañías")[[ ic_val ]] %||% "Sin transacciones entre compañías"

    div(
      class = "alert alert-light border small py-2 mb-0",
      icon("circle-info"), " ",
      tags$strong("Empresas incluidas:"), " ", co_str,
      tags$span(class = "mx-2 text-muted", "|"),
      tags$strong("Intercompañía:"), " ", ic_label
    )
  })

  # ── Download handler ─────────────────────────────────────────────────────
  # Stage 2: runs the data pipeline; writes a structured text preview.
  # Stage 3: replace content() body with officer/flextable Word generation.
  output$btn_export_word <- downloadHandler(
    filename = function() {
      from  <- format(input$export_date_from %||% Sys.Date(), "%d%b%Y")
      to    <- format(input$export_date_to   %||% (lubridate::floor_date(Sys.Date(), "week", week_start = 1) + 6L), "%d%b%Y")
      emp   <- shared$empresa_sel() %||% character(0)
      cmap  <- shared$company_map() %||% COMPANY_MAP
      rev   <- setNames(names(cmap), unname(cmap))
      inits <- paste(na.omit(unname(rev[ emp ])), collapse = "-")
      if (!nzchar(inits)) inits <- "GRUPO"
      grp_raw <- tryCatch(shared$group_name(), error = function(e) "Networks Group")
      prefix  <- gsub("[^A-Za-z0-9]", "", grp_raw %||% "Networks Group")
      if (!nzchar(prefix)) prefix <- "FlujoEfectivo"
      paste0(prefix, "_", inits, "_", from, "-", to, ".docx")
    },
    content = function(file) {
      showNotification(
        tagList(tags$span(class = "spinner-border spinner-border-sm me-1"),
                "Generando Word…"),
        id = "cf_export_notif", type = "message", duration = NULL)
      on.exit(removeNotification("cf_export_notif"), add = TRUE)

      # ── Collect inputs ──────────────────────────────────────────────────
      date_from <- input$export_date_from %||% Sys.Date()
      date_to   <- input$export_date_to   %||% (lubridate::floor_date(Sys.Date(), "week", week_start = 1) + 6L)
      emp_sel   <- shared$empresa_sel()   %||% character(0)
      ic_val    <- input$ic_mode          %||% "exclude"

      base_cur  <- input$export_base_cur  %||% "MXN"
      cur_mode  <- .cur_mode_rv()
      grouping  <- .grp_rv()

      # Collect user-edited FX rates for fused mode
      fx_rates <- if (cur_mode == "fused") {
        sap <- tryCatch(shared$sap_data(), error = function(e) NULL)
        other_curs <- if (!is.null(sap)) {
          setdiff(intersect(unique(c(sap$AR$Moneda, sap$AP$Moneda)), CURRENCIES),
                  c(base_cur, NA_character_))
        } else setdiff(c("USD","EUR"), base_cur)

        rates <- vapply(other_curs, function(cur) {
          input[[ paste0("export_fx_", cur) ]] %||% .suggest_fx_rate(cur, base_cur)
        }, numeric(1))
        setNames(rates, other_curs)
      } else c()

      # ── Run pipeline ────────────────────────────────────────────────────
      combined_df <- tryCatch(
        build_export_combined_df(shared, ic_val),
        error = function(e) {
          showNotification(paste("Error en datos:", conditionMessage(e)),
                           type = "error", duration = 8)
          NULL
        }
      )

      if (is.null(combined_df) || !nrow(combined_df)) {
        showNotification(
          paste0("Sin datos para el período seleccionado (",
                 date_from, " – ", date_to, ")."),
          type = "warning", duration = 8)
        stop("Sin datos disponibles.")
      }

      tags_df <- tryCatch(shared$tags_db(), error = function(e) NULL)

      export_data <- tryCatch(
        build_cashflow_export_data(
          df            = combined_df,
          tags_df       = tags_df,
          emp_sel       = emp_sel,
          date_from     = date_from,
          date_to       = date_to,
          grouping      = grouping,
          currency_mode = cur_mode,
          base_cur      = base_cur,
          fx_rates      = fx_rates
        ),
        error = function(e) {
          showNotification(paste("Error al agregar datos:", conditionMessage(e)),
                           type = "error", duration = 8)
          NULL
        }
      )

      if (is.null(export_data)) stop("Error al procesar los datos.")

      # ── Build Word document ──────────────────────────────────────────────
      cmap     <- shared$company_map() %||% COMPANY_MAP
      rev_map  <- setNames(names(cmap), unname(cmap))
      initials <- na.omit(unname(rev_map[ emp_sel ]))
      if (!length(initials)) initials <- "—"

      ic_labels <- c(exclude = "Sin transacciones entre compañías",
                     include = "Con transacciones entre compañías",
                     only    = "Sólo transacciones entre compañías")
      ic_label  <- ic_labels[[ ic_val ]] %||% "Sin transacciones entre compañías"

      user_info <- tryCatch(shared$current_user_info(), error = function(e) NULL)
      user_name <- user_info$name %||% user_info$user %||% "—"

      g_name     <- tryCatch(shared$group_name(),     error = function(e) "Networks Group")
      g_logo_raw <- tryCatch(shared$group_logo_raw(), error = function(e) NULL)
      g_logo_ext <- tryCatch(shared$group_logo_ext(), error = function(e) "png")

      doc <- tryCatch(
        generate_cashflow_word_doc(
          export_data    = export_data,
          emp_initials   = initials,
          emp_full       = emp_sel,
          ic_label       = ic_label,
          user_name      = user_name,
          primary_lang   = input$export_lang_primary   %||% "es",
          secondary_lang = if (isTRUE(.sec_lang_rv()))
                             input$export_lang_secondary %||% NULL
                           else NULL,
          group_name     = g_name,
          group_logo_raw = g_logo_raw,
          group_logo_ext = g_logo_ext
        ),
        error = function(e) {
          showNotification(paste("Error generando Word:", conditionMessage(e)),
                           type = "error", duration = 10)
          NULL
        }
      )

      if (is.null(doc)) stop("Error al generar el documento Word.")

      print(doc, target = file)
    }
  )

  # ── Trigger async Excel export ────────────────────────────────────────────
  observeEvent(input$btn_trigger_xlsx, {
    # Kill any in-flight job before starting fresh
    old <- .xl_job()
    if (!is.null(old)) {
      tryCatch(old$job$kill(), error = function(e) NULL)
      if (file.exists(old$snap_path)) unlink(old$snap_path)
    }
    .xl_file(NULL)
    .xl_state("idle")

    # ── Collect reactive inputs ──────────────────────────────────────────
    date_from <- input$export_date_from %||% Sys.Date()
    date_to   <- input$export_date_to   %||%
                 (lubridate::floor_date(Sys.Date(), "week", week_start = 1) + 6L)
    emp_sel   <- shared$empresa_sel()   %||% character(0)
    ic_val    <- input$ic_mode          %||% "exclude"
    base_cur  <- input$export_base_cur  %||% "MXN"
    cur_mode  <- .cur_mode_rv()
    grouping  <- .grp_rv()

    fx_rates <- if (cur_mode == "fused") {
      sap        <- tryCatch(shared$sap_data(), error = function(e) NULL)
      other_curs <- if (!is.null(sap))
        setdiff(intersect(unique(c(sap$AR$Moneda, sap$AP$Moneda)), CURRENCIES),
                c(base_cur, NA_character_))
      else setdiff(c("USD", "EUR"), base_cur)
      rates <- vapply(other_curs, function(cur)
        input[[paste0("export_fx_", cur)]] %||% .suggest_fx_rate(cur, base_cur),
        numeric(1))
      setNames(rates, other_curs)
    } else c()

    # ── Build combined_df synchronously (fast: ~2-3 s, uses cached SAP data)
    combined_df <- tryCatch(
      build_export_combined_df(shared, ic_val),
      error = function(e) {
        showNotification(paste("Error en datos:", conditionMessage(e)),
                         type = "error", duration = 8)
        NULL
      }
    )
    if (is.null(combined_df) || !nrow(combined_df)) {
      showNotification(
        paste0("Sin datos para el período (", date_from, " – ", date_to, ")."),
        type = "warning", duration = 8)
      return()
    }

    tags_df    <- tryCatch(shared$tags_db(),           error = function(e) NULL)
    user_info  <- tryCatch(shared$current_user_info(), error = function(e) NULL)
    g_name     <- tryCatch(shared$group_name(),        error = function(e) "Networks Group")
    g_logo_raw <- tryCatch(shared$group_logo_raw(),    error = function(e) NULL)
    g_logo_ext <- tryCatch(shared$group_logo_ext(),    error = function(e) "png")

    cmap     <- shared$company_map() %||% COMPANY_MAP
    rev_map  <- setNames(names(cmap), unname(cmap))
    initials <- na.omit(unname(rev_map[emp_sel]))
    if (!length(initials)) initials <- "—"

    ic_labels <- c(exclude = "Sin transacciones entre compañías",
                   include = "Con transacciones entre compañías",
                   only    = "Sólo transacciones entre compañías")
    ic_label  <- ic_labels[[ic_val]] %||% "Sin transacciones entre compañías"
    user_name <- user_info$name %||% user_info$user %||% "—"

    lang_primary   <- input$export_lang_primary   %||% "es"
    lang_secondary <- if (isTRUE(.sec_lang_rv())) input$export_lang_secondary %||% NULL else NULL

    # ── Build download filename ──────────────────────────────────────────
    from_str  <- format(date_from, "%d%b%Y")
    to_str    <- format(date_to,   "%d%b%Y")
    inits_str <- paste(initials, collapse = "-")
    if (!nzchar(inits_str) || identical(inits_str, "—")) inits_str <- "GRUPO"
    prefix    <- gsub("[^A-Za-z0-9]", "", g_name %||% "Networks Group")
    if (!nzchar(prefix)) prefix <- "FlujoEfectivo"
    .xl_basename(paste0(prefix, "_", inits_str, "_", from_str, "-", to_str, ".xlsx"))

    # ── Serialize snapshot and launch callr background job ───────────────
    snap_path <- tempfile(fileext = ".rds")
    out_path  <- tempfile(fileext = ".xlsx")

    saveRDS(list(
      combined_df   = combined_df,
      tags_df       = tags_df,
      emp_sel       = emp_sel,
      date_from     = date_from,
      date_to       = date_to,
      grouping      = grouping,
      currency_mode = cur_mode,
      base_cur      = base_cur,
      fx_rates      = fx_rates,
      initials      = initials,
      ic_label      = ic_label,
      user_name     = user_name,
      g_name        = g_name,
      g_logo_raw    = g_logo_raw,
      g_logo_ext    = g_logo_ext,
      lang_primary  = lang_primary,
      lang_secondary = lang_secondary
    ), snap_path)

    app_dir <- normalizePath(getwd())
    job <- callr::r_bg(
      func     = .cf_excel_worker,
      args     = list(snapshot_path = snap_path,
                      output_path   = out_path,
                      app_dir       = app_dir),
      supervise = TRUE
    )

    .xl_job(list(job = job, snap_path = snap_path, out_path = out_path))
    .xl_start(Sys.time())
    .xl_warn_at(60)
    .xl_state("building")
  }, ignoreInit = TRUE)

  # ── Poll for job completion (every 1 s) ───────────────────────────────────
  observe({
    .xl_timer()
    state <- .xl_state()
    if (!(state %in% c("building", "warn60"))) return()

    info <- .xl_job()
    if (is.null(info)) { .xl_state("idle"); return() }

    job     <- info$job
    elapsed <- as.numeric(difftime(Sys.time(), .xl_start(), units = "secs"))

    if (!job$is_alive()) {
      status <- tryCatch(job$get_exit_status(), error = function(e) -1L)
      if (identical(status, 0L) && file.exists(info$out_path)) {
        .xl_file(info$out_path)
        .xl_state("done")
        showNotification(
          tagList(icon("file-excel"), " Excel listo — haga clic en 'Descargar' para guardarlo."),
          type = "message", duration = 12)
      } else {
        err <- tryCatch(trimws(job$read_all_error_text()), error = function(e) "")
        if (!nzchar(err)) err <- "Error desconocido al generar el archivo."
        .xl_state("error")
        showNotification(paste("Error al generar Excel:", err),
                         type = "error", duration = 12)
      }
      if (file.exists(info$snap_path)) unlink(info$snap_path)
      .xl_job(NULL)
      if (state == "warn60") removeModal()
      return()
    }

    # Warning dialog at threshold (60 s first time, then every 60 s more if user continues)
    if (elapsed >= .xl_warn_at() && state == "building") {
      .xl_warn_at(.xl_warn_at() + 60)  # push next warning 60 s forward before showing
      .xl_state("warn60")
      showModal(modalDialog(
        title = tagList(icon("clock"), " Generación en progreso"),
        p(sprintf("Han transcurrido %.0f segundos. El rango de fechas puede ser muy amplio.", elapsed)),
        p("¿Desea continuar esperando o cancelar la exportación?"),
        footer = tagList(
          actionButton("btn_xl_cancel",   "Cancelar exportación", class = "btn btn-danger btn-sm"),
          actionButton("btn_xl_continue", "Seguir esperando",     class = "btn btn-secondary btn-sm")
        ),
        easyClose = FALSE
      ))
      return()
    }

    # Hard stop at 5 minutes
    if (elapsed >= 300 && state == "warn60") {
      tryCatch(job$kill(), error = function(e) NULL)
      if (file.exists(info$snap_path)) unlink(info$snap_path)
      .xl_job(NULL)
      .xl_state("error")
      removeModal()
      showNotification(
        "El proceso superó 5 minutos y fue cancelado. Reduzca el rango de fechas e intente de nuevo.",
        type = "error", duration = 15)
    }
  })

  # ── Cancel export ─────────────────────────────────────────────────────────
  observeEvent(input$btn_xl_cancel, {
    info <- .xl_job()
    if (!is.null(info)) {
      tryCatch(info$job$kill(), error = function(e) NULL)
      if (file.exists(info$snap_path)) unlink(info$snap_path)
    }
    .xl_job(NULL)
    .xl_state("cancelled")
    removeModal()
    showNotification("Exportación cancelada.", type = "warning", duration = 5)
  }, ignoreInit = TRUE)

  # ── Continue waiting (dismiss timeout dialog, stay in building) ───────────
  observeEvent(input$btn_xl_continue, {
    .xl_state("building")
    removeModal()
  }, ignoreInit = TRUE)

  # ── Download pre-generated xlsx (fast — just copies a temp file) ──────────
  output$btn_download_excel_ready <- downloadHandler(
    filename = function() .xl_basename() %||% "flujo_caja.xlsx",
    content  = function(file) {
      src <- .xl_file()
      req(!is.null(src), file.exists(src))
      file.copy(src, file, overwrite = TRUE)
    }
  )

  # ── Dynamic download button (Word, PDF, or async Excel) ───────────────────
  output$export_dl_btn <- renderUI({
    fmt   <- input$export_format %||% "docx"
    state <- .xl_state()

    if (fmt == "pdf") {
      downloadButton("btn_export_pdf", " Exportar",
                     class = "btn btn-primary btn-sm",
                     icon  = icon("file-pdf"))
    } else if (fmt == "xlsx") {
      switch(state,
        "done" = downloadButton(
          "btn_download_excel_ready", " Descargar Excel",
          class = "btn btn-success btn-sm cf-xlsx-trigger",
          icon  = icon("download")),
        "building" = ,
        "warn60"   = tagList(
          tags$button(
            class    = "btn btn-primary btn-sm disabled cf-xlsx-trigger",
            disabled = NA,
            tagList(tags$span(class = "spinner-border spinner-border-sm me-1"),
                    " Generando…")
          ),
          actionButton("btn_xl_cancel", " Cancelar",
                       class = "btn btn-outline-danger btn-sm ms-1 cf-xlsx-trigger",
                       icon  = icon("times"))
        ),
        # idle / error / cancelled
        actionButton("btn_trigger_xlsx", " Exportar",
                     class = "btn btn-primary btn-sm cf-xlsx-trigger",
                     icon  = icon("file-excel"))
      )
    } else {
      downloadButton("btn_export_word", " Exportar",
                     class = "btn btn-primary btn-sm",
                     icon  = icon("file-word"))
    }
  })

  # ── PDF download handler (pagedown + Chrome) ─────────────────────────────
  output$btn_export_pdf <- downloadHandler(
    filename = function() {
      from  <- format(input$export_date_from %||% Sys.Date(), "%d%b%Y")
      to    <- format(input$export_date_to   %||% (lubridate::floor_date(Sys.Date(), "week", week_start = 1) + 6L), "%d%b%Y")
      emp   <- shared$empresa_sel() %||% character(0)
      cmap  <- shared$company_map() %||% COMPANY_MAP
      rev   <- setNames(names(cmap), unname(cmap))
      inits <- paste(na.omit(unname(rev[ emp ])), collapse = "-")
      if (!nzchar(inits)) inits <- "GRUPO"
      grp_raw <- tryCatch(shared$group_name(), error = function(e) "Networks Group")
      prefix  <- gsub("[^A-Za-z0-9]", "", grp_raw %||% "Networks Group")
      if (!nzchar(prefix)) prefix <- "FlujoEfectivo"
      paste0(prefix, "_", inits, "_", from, "-", to, ".pdf")
    },
    content = function(file) {
      showNotification(
        tagList(tags$span(class = "spinner-border spinner-border-sm me-1"),
                "Generando PDF… (puede tomar unos segundos)"),
        id = "cf_export_notif", type = "message", duration = NULL)
      on.exit(removeNotification("cf_export_notif"), add = TRUE)

      date_from <- input$export_date_from %||% Sys.Date()
      date_to   <- input$export_date_to   %||% (lubridate::floor_date(Sys.Date(), "week", week_start = 1) + 6L)
      emp_sel   <- shared$empresa_sel()   %||% character(0)
      ic_val    <- input$ic_mode          %||% "exclude"
      base_cur  <- input$export_base_cur  %||% "MXN"
      cur_mode  <- .cur_mode_rv()
      grouping  <- .grp_rv()

      fx_rates <- if (cur_mode == "fused") {
        sap <- tryCatch(shared$sap_data(), error = function(e) NULL)
        other_curs <- if (!is.null(sap)) {
          setdiff(intersect(unique(c(sap$AR$Moneda, sap$AP$Moneda)), CURRENCIES),
                  c(base_cur, NA_character_))
        } else setdiff(c("USD","EUR"), base_cur)
        rates <- vapply(other_curs, function(cur) {
          input[[ paste0("export_fx_", cur) ]] %||% .suggest_fx_rate(cur, base_cur)
        }, numeric(1))
        setNames(rates, other_curs)
      } else c()

      combined_df <- tryCatch(
        build_export_combined_df(shared, ic_val),
        error = function(e) {
          showNotification(paste("Error en datos:", conditionMessage(e)),
                           type = "error", duration = 8); NULL })
      if (is.null(combined_df) || !nrow(combined_df)) {
        showNotification(
          paste0("Sin datos para el período seleccionado (",
                 date_from, " – ", date_to, ")."),
          type = "warning", duration = 8)
        stop("Sin datos disponibles.")
      }

      tags_df     <- tryCatch(shared$tags_db(), error = function(e) NULL)
      export_data <- tryCatch(
        build_cashflow_export_data(df = combined_df, tags_df = tags_df,
          emp_sel = emp_sel, date_from = date_from, date_to = date_to,
          grouping = grouping, currency_mode = cur_mode,
          base_cur = base_cur, fx_rates = fx_rates),
        error = function(e) {
          showNotification(paste("Error al agregar:", conditionMessage(e)),
                           type = "error", duration = 8); NULL })
      if (is.null(export_data)) stop("Error al procesar los datos.")

      cmap     <- shared$company_map() %||% COMPANY_MAP
      rev_map  <- setNames(names(cmap), unname(cmap))
      initials <- na.omit(unname(rev_map[ emp_sel ]))
      if (!length(initials)) initials <- "—"

      ic_labels <- c(exclude = "Sin IC", include = "Con IC", only = "Sólo IC")
      ic_label  <- ic_labels[[ ic_val ]] %||% "Sin IC"

      user_info  <- tryCatch(shared$current_user_info(), error = function(e) NULL)
      user_name  <- user_info$name %||% user_info$user %||% "—"
      g_name     <- tryCatch(shared$group_name(),     error = function(e) "Networks Group")
      g_logo_raw <- tryCatch(shared$group_logo_raw(), error = function(e) NULL)
      g_logo_ext <- tryCatch(shared$group_logo_ext(), error = function(e) "png")

      # ── Build HTML → PDF via Chrome ──────────────────────────────────────
      html_path <- tryCatch(
        generate_cashflow_html_doc(
          export_data    = export_data,
          emp_initials   = initials,
          emp_full       = emp_sel,
          ic_label       = ic_label,
          user_name      = user_name,
          primary_lang   = input$export_lang_primary   %||% "es",
          secondary_lang = if (isTRUE(.sec_lang_rv()))
                             input$export_lang_secondary %||% NULL else NULL,
          group_name     = g_name,
          group_logo_raw = g_logo_raw,
          group_logo_ext = g_logo_ext
        ),
        error = function(e) {
          showNotification(paste("Error generando HTML:", conditionMessage(e)),
                           type = "error", duration = 10); NULL })
      if (is.null(html_path)) stop("Error al generar el documento HTML.")

      # Linux Shiny servers need --no-sandbox to run Chrome headless
      chrome_extra <- if (.Platform$OS.type == "unix") {
        c("--no-sandbox", "--disable-setuid-sandbox", "--disable-dev-shm-usage")
      } else character(0)

      tryCatch({
        pagedown::chrome_print(
          input       = html_path,
          output      = file,
          timeout     = 120,
          extra_args  = chrome_extra,
          options     = list(printBackground = TRUE, preferCSSPageSize = TRUE,
                             paperWidth = 11.69, paperHeight = 8.27,
                             marginTop = 0.59, marginBottom = 0.59,
                             marginLeft = 0.59, marginRight = 0.59)
        )
      }, error = function(e) {
        showNotification(paste("Error al generar PDF:", conditionMessage(e)),
                         type = "error", duration = 10)
        stop(conditionMessage(e))
      }, finally = {
        if (!is.null(html_path) && file.exists(html_path)) unlink(html_path)
      })
    }
  )

}
