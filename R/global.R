# =============================================================================
# global.R
# Loaded once at startup. Libraries, constants, pure helper functions only.
# No reactives, no server logic, no side effects beyond data loading.
# =============================================================================

# Load project .Renviron before anything else — ensures CLIENT_ID, S3_BUCKET, and
# AWS_* vars are set even when R is started outside the project directory.
if (file.exists(".Renviron")) readRenviron(".Renviron")

IS_ADMIN_DEPLOYMENT <- tolower(trimws(Sys.getenv("CLIENT_ID"))) == "hd-admin"

# Reset the S3 preload guard on re-source so a fresh global.R source always
# re-runs the preload with the correct CLIENT_ID (needed after .Renviron changes).
if (exists(".s3_preload_done", envir = .GlobalEnv))
  suppressWarnings(rm(".s3_preload_done", envir = .GlobalEnv))

# ── Libraries ─────────────────────────────────────────────────────────────────
library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(scales)
library(purrr)
library(DT)
library(shinyWidgets)
library(aws.s3)
library(httr)       # SAP Service Layer API calls
library(jsonlite)   # SAP response parsing
library(uuid)       # note / manual entry IDs
library(readxl)     # Excel import in proveedores module
library(openxlsx)   # Excel export in cashflow report
library(shinymanager)
library(callr)
# htmltools loaded transitively by Shiny; htmltools::HTML() used directly in calendar_html()

# ── Early utility definitions ─────────────────────────────────────────────────
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}

strip_accents <- function(x) {
  stringi::stri_trans_general(x %||% "", "Latin-ASCII")
}

CURRENCIES <- c("MXN","USD","EUR","GBP","CAD","JPY","CHF","AUD","CNY","BRL")

# ── Source modules ────────────────────────────────────────────────────────────
source("R/persistence.R")
source("R/email_service.R")
source("R/policy_engine.R")
source("R/tier_registry.R")
source("R/sap_cache.R")
source("R/auth.R")
source("R/data_pipeline.R")
source("R/sap_api.R")
source("R/ledger_module.R")
source("R/ui_components.R")
source("R/notes_handlers.R")
source("R/manual_entry_handlers.R")
# Settings — split across R/settings/ for session efficiency.
# MUST use local=TRUE so these files execute in sharedEnv (same as global.R).
# Without it they land in .GlobalEnv whose closure chain cannot reach S3_KEYS,
# strip_accents, or any symbol defined directly in global.R.
source("R/settings/settings_helpers.R",   local = TRUE)
source("R/settings/settings_bp.R",        local = TRUE)
source("R/settings/settings_proveedores.R", local = TRUE)
source("R/settings/settings_cuentas.R",   local = TRUE)
source("R/settings/settings_sincro.R",    local = TRUE)
source("R/settings/settings_policies.R",  local = TRUE)
source("R/settings/settings_companies.R", local = TRUE)
source("R/settings/settings_hub.R",       local = TRUE)
source("R/ppl_generator.R")
source("R/bancos_parser.R")
source("R/bancos_persistence.R")
source("R/bancos_module.R")
source("R/proveedores_match.R")
source("R/proveedores_module.R")
source("R/pasivos_schemas.R")
source("R/pasivos_persistence.R")
source("R/pasivos_capabilities.R")
source("R/pasivos_audit.R")
source("R/app_audit.R")
source("R/forecasting_schemas.R")
source("R/forecasting_persistence.R")
source("R/forecasting_metric_registry.R")
source("R/forecasting_sources.R")
source("R/forecasting_source_banxico.R")
source("R/forecasting_source_yahoo.R")
source("R/forecasting_source_fred.R")
source("R/forecasting_methods.R")
source("R/forecasting_indicators.R")
source("R/forecasting_resolve.R")
source("R/forecasting_module.R")
source("R/pasivos_calendar_glue.R")
source("R/pasivos_observers.R")
source("R/pasivos_module.R")
source("R/sync_bus.R")
source("R/cashflow_export_module.R")

# ── Constants ─────────────────────────────────────────────────────────────────

# Fallback company registry — used only on fresh deployments (before empresas.rds
# exists) and as the seed for load_empresas(). In all normal sessions this is
# overwritten immediately after the critical preload reads empresas.rds from S3.
# To add a company to a fresh deployment, add it here; after the first run the
# Empresas module in Settings is the authoritative source.
COMPANY_MAP <- c(
  NG  = "Networks Group",
  NTS = "Networks Trucking Services",
  NCS = "Networks Crossdocking Services",
  NL  = "Networks & Logistics",
  NRS = "Networks Realtors",
  PL  = "Paragon Logistics"
)

# Fetched synchronously at startup — must be ready before any session connects.
# empresas.rds is included so COMPANY_MAP is rebuilt from the live registry
# before any UI function (e.g. pagarHoyUI) captures it at render time.
S3_KEYS_CRITICAL <- list(
  sap_snap_ar = "sap_snapshot_AR.rds",
  sap_snap_ap = "sap_snapshot_AP.rds",
  empresas    = "empresas.rds"
)

# All other keys — loaded lazily by .data_loaded observer via load_*() functions.
# Listed here so .s3_read() key lookups work correctly.
S3_KEYS <- list(
  moves                 = "invoice_moves.rds",
  notes                 = "calendar_notes.rds",
  tags                  = "invoice_tags.rds",
  manual                = "manual_invoices.rds",
  pagar_hoy             = "pagar_hoy.rds",
  conciliacion          = "conciliacion.rds",
  bancos                = "bancos.rds",
  proveedores           = "proveedores.rds",
  proveedores_inactivos = "proveedores_inactivos.rds",
  interco               = "interco_settings.rds",
  interco_v2            = "interco_v2.rds",
  papelera              = "papelera.rds",
  ctas_bancos           = "ctas_bancos.rds",
  ctas_cuentas          = "ctas_cuentas.rds",
  bancos_movimientos    = "bancos_movimientos.rds",
  bancos_cuentas        = "bancos_cuentas.rds",
  bancos_confirmados    = "bancos_confirmados.rds",
  bancos_papelera       = "bancos_papelera.rds",
  usuarios              = "usuarios.rds",
  empresas              = "empresas.rds",
  parte_alias_map       = "parte_alias_map.rds",  # Parte → alias overrides for AP matching
  pagar_hoy_sync        = "pagar_hoy_sync.rds",    # shared agenda when sync is ON
  agenda_sync_config    = "agenda_sync_config.rds", # sync enabled/disabled + metadata
  policy_catalog        = "policy_catalog.rds",    # payment policy definitions
  partner_policies      = "partner_policies.rds",  # parte → policy assignments
  policy_moves          = "policy_moves.rds",       # computed FechaVenc_Politica per invoice
  holiday_overrides     = "holiday_overrides.rds",  # add/remove specific holiday dates
  sap_overrides         = "sap_overrides.rds",       # field-level overrides for SAP invoices (Parte, Codigo, Factura, Notas)
  pasivos_liabilities   = "pasivos_liabilities.rds",
  pasivos_provisions    = "pasivos_provisions.rds",
  pasivos_modifiers     = "pasivos_modifiers.rds",
  pasivos_estimates     = "pasivos_estimates.rds",
  pasivos_card_expenses = "pasivos_card_expenses.rds",
  pasivos_audit         = "pasivos_audit.rds",
  app_audit             = "app_audit.rds",
  # Forecasting module (Stage 6.1)
  forecasting_series_observations = "forecasting_series_observations.rds",
  forecasting_metrics             = "forecasting_metrics.rds",
  forecasting_sources             = "forecasting_sources.rds",
  forecasting_methods             = "forecasting_methods.rds",
  forecasting_indicators          = "forecasting_indicators.rds",
  forecasting_subscriptions       = "forecasting_subscriptions.rds",
  forecasting_manual_curves       = "forecasting_manual_curves.rds",
  forecasting_fetch_log           = "forecasting_fetch_log.rds",
  group_config                    = "group_config.rds",
  # Multi-tenant: conglomerate / client groups — see R/persistence.R
  grupos                          = "grupos.rds",
  # Cross-process sync bus: tiny named-integer list written on every bump so
  # other R workers can detect version changes even across process boundaries.
  sync_versions                   = "sync_versions.rds",
  # Tier display configuration (labels, colors) — kept in S3_KEYS so the missing-key
  # cache does not blacklist it after the first failed read per session.
  tiers_config                    = "tiers_config.rds"
)

# Validate required env vars and configure AWS credentials at startup.
# CLIENT_ID drives all credential lookups — see R/persistence.R for full convention.
.check_env_vars <- function() {
  # CLIENT_ID must be present — it is the only deployment-specific variable
  if (!nzchar(Sys.getenv("CLIENT_ID"))) {
    warning("CLIENT_ID is not set. S3 persistence will not work.\n",
            "Add CLIENT_ID=<slug> to your .Renviron (e.g. CLIENT_ID=networks).",
            call. = FALSE)
    return(invisible(FALSE))
  }

  required <- c("S3_BUCKET", "AWS_ACCESS_KEY_ID",
                "AWS_SECRET_ACCESS_KEY", "AWS_DEFAULT_REGION")
  missing  <- required[!nzchar(Sys.getenv(required))]
  if (length(missing))
    warning("Missing env vars: ", paste(missing, collapse = ", "),
            "\nS3 persistence will not work.", call. = FALSE)

  invisible(TRUE)
}
.check_env_vars()

# Configure AWS credentials for this client — must run after persistence.R is sourced
# (s3_init is defined there). Called here so credentials are ready before any
# load_*() call in the server function.
s3_init()

# Cross-session SAP cache — populated after first live fetch, consumed by the
# subsequent app session to avoid the double SAP fetch that shinymanager causes
# by calling server() twice (auth session + app session after login redirect).
# Keyed by client id (see R/sap_cache.R) so one client's session can never seed
# a different client's session with its data. Reset on every runApp() so
# development restarts always get a fresh fetch.
.GlobalEnv$.sap_global_cache <- list()
.GlobalEnv$.session_count    <- 0L

# ── S3 pre-load (runs once at startup, before any session connects) ─────────────
# On Linux/macOS: mclapply (fork-based, zero overhead, true parallel).
# On Windows:     plain lapply — spawning R worker processes costs more than
#                 the network overlap saves for ~18 small RDS files.
# Guard prevents double-execution if global.R is sourced more than once.
if (!exists(".s3_preload_done", envir = globalenv())) {
  .s3_preload_done <- TRUE
  local({
    message("[LOAD] Fetching S3 objects...")
    t0     <- proc.time()
    keys   <- unlist(S3_KEYS_CRITICAL, use.names = TRUE)
    cid_lo <- tolower(toupper(trimws(Sys.getenv("CLIENT_ID"))))
    bucket <- Sys.getenv("S3_BUCKET")

    reader <- function(key_suffix) {
      tryCatch({
        out <- NULL
        suppressMessages(
          capture.output(
            out <- aws.s3::s3readRDS(
              object = paste0(cid_lo, "/", key_suffix),
              bucket = bucket
            ),
            type = "output"
          )
        )
        out
      }, error = function(e) NULL)
    }

    raw <- if (.Platform$OS.type != "windows") {
      parallel::mclapply(as.list(keys), reader, mc.cores = min(4L, length(keys)))
    } else {
      lapply(as.list(keys), reader)
    }

    for (i in seq_along(keys)) assign(keys[[i]], raw[[i]], envir = .s3_preload_cache)

    message(sprintf("[LOAD] %d S3 objects fetched in %.1fs",
                    length(keys), (proc.time() - t0)[["elapsed"]]))
  })
}

# Rebuild COMPANY_MAP from the just-fetched empresas.rds so every module that
# reads COMPANY_MAP at UI-build or startup time (e.g. pagarHoyUI, sap_api) sees
# the live registry rather than the hardcoded seed above.
local({
  df <- tryCatch(.s3_preload_cache[["empresas.rds"]], error = function(e) NULL)
  # Always rebuild COMPANY_MAP from empresas.rds — even if absent or zero rows.
  # A NULL df means a fresh client with no empresas.rds yet; set empty map so
  # the hardcoded Networks Group fallback above is never inherited by other clients.
  if (is.null(df) || !is.data.frame(df) ||
      !all(c("initials", "nombre_corto", "activa", "deleted") %in% names(df))) {
    assign("COMPANY_MAP", c(), envir = .GlobalEnv)
    message("[LOAD] COMPANY_MAP: no empresas.rds — empty map")
    return(invisible(NULL))
  }
  active <- df[
    (is.na(df$deleted) | df$deleted != TRUE) &
    (is.na(df$activa)  | df$activa  == TRUE),  , drop = FALSE]
  cmap <- if (nrow(active) > 0) setNames(active$nombre_corto, active$initials) else c()
  assign("COMPANY_MAP", cmap, envir = .GlobalEnv)
  message("[LOAD] COMPANY_MAP: ", length(cmap), " companies from empresas.rds (",
          paste(names(cmap), collapse = ", "), ")")
})

# ── Runtime cache (populated on first session, shared across all sessions) ──────
# Nothing is loaded here at process start — S3 reads happen in the server's
# first observe() and are cached here so subsequent sessions pay nothing.
# .cache_get / .cache_set are used throughout the app.

.sap_snapshot_cache <- list(AR = NULL, AP = NULL)
.app_data_cache     <- list()

.cache_get <- function(key) .app_data_cache[[key]]
.cache_set <- function(key, value) { .app_data_cache[[key]] <<- value }

# Calendar visual theme
CALENDAR_THEME <- list(
  bg           = "white",
  tile_empty   = "white",
  tile_has     = "white",
  weekend      = "#F4F8FF",
  border       = "#0A58CA",
  daynum       = "#CD4F39",
  text         = "#0B2038",
  divider      = "#B6C8FF",
  title        = "#CD4F39",
  subtitle     = "#8B3626",
  today_border = "#0A58CA",
  # Tag highlight colours
  tag_imp_bg     = "#FFF4E5",
  tag_imp_border = "#FFC107",
  tag_urg_bg     = "#FFE5E5",
  tag_urg_border = "#DC3545",
  tag_both_bg    = "#FFE0CC",
  tag_both_border= "#FF6B35"
)

# ── Formatting helpers ─────────────────────────────────────────────────────────

fmt_money <- function(x) {
  paste0("$ ", scales::number(x, big.mark = ",", accuracy = 0.01))
}

currency_formatter <- function(cur) {
  cur <- toupper(trimws(cur))
  prefix <- switch(cur, MXN = "$", USD = "$", EUR = "€", GBP = "£", "#")
  scales::label_dollar(prefix = prefix, big.mark = ",", decimal.mark = ".", accuracy = 0.01)
}

# Spanish month label: "febrero 2026"
mes_es <- function(date) {
  meses <- c("enero","febrero","marzo","abril","mayo","junio",
             "julio","agosto","septiembre","octubre","noviembre","diciembre")
  paste0(meses[month(as.Date(date))], " ", year(as.Date(date)))
}

# ── BP Reference No. column guesser ───────────────────────────────────────────
# SAP exports NumAtCard under different column names depending on UI language.
# Preferred names cover ES / EN / DE / FR / PT / IT / NL / PL.
# Falls back to a regex that matches any "ref + SN/BP/partner/tiers/GP" pattern.
guess_bp_ref_col <- function(df) {
  nms <- names(df)
  preferred <- c(
    # Spanish
    "N\u00famero de referencia del SN", "N\u00fam. ref. SN", "Ref. SN",
    "No. ref. SN", "N\u00ba ref. SN",
    # English
    "BP Ref. No.", "BP Reference No.", "Customer Ref. No.",
    "Vendor Ref. No.", "Ref. No.", "NumAtCard",
    # German
    "Referenz des GP", "GP-Referenz",
    # French
    "R\u00e9f. tiers", "No r\u00e9f. tiers",
    # Portuguese
    "No. ref. PN", "Ref. PN",
    # Italian
    "Rif. fornitore", "Rif. cliente",
    # Dutch
    "Ref.nr. relatie",
    # Polish
    "Nr ref. kontrahenta"
  )
  hit <- preferred[preferred %in% nms][1]
  if (!is.na(hit)) return(hit)
  # Regex fallback
  idx <- which(grepl(
    "(?i)(ref.*sn|sn.*ref|bp.*ref|ref.*bp|ref.*partner|partner.*ref|numatcard|ref.*tiers|ref.*gp|gp.*ref|ref.*pn|ref.*kontrah)",
    nms, perl = TRUE
  ))[1]
  if (length(idx) && !is.na(idx)) return(nms[idx])
  NULL
}

# ── Calendar grid helper ───────────────────────────────────────────────────────
# Returns a tibble with one row per day in the month, with week/wday coords
# Returns a tibble with one row per day in the month, with week/wday coords.

calendar_grid <- function(month_start) {
  ms  <- as.Date(floor_date(as.Date(month_start), "month"))
  me  <- as.Date(ceiling_date(ms, "month") - days(1))
  tibble(Fecha = seq.Date(ms, me, by = "day")) |>
    mutate(
      wday       = wday(Fecha, week_start = 1),
      start_wday = wday(ms, week_start = 1),
      week       = ((day(Fecha) + start_wday - 2L) %/% 7L) + 1L
    )
}

# ── Calendar HTML renderer ─────────────────────────────────────────────────────
# Pure function: data in, htmltools tag out. No reactives, no side effects.
# data_due columns: Fecha (Date), Moneda (chr), Parte (chr), Importe (num)
# tags_day: optional named list(date_string -> tag_type) for tile badges
#
# Clicking a day fires: Shiny.setInputValue(input_id, {date, ledger, nonce})

calendar_html <- function(
    data_due,
    month_start,
    currency,
    title_prefix = "Cobros esperados",
    ledger       = "AR",
    input_id     = "cal_click",
    max_rows     = 4,
    tags_day     = list(),     # named list: "2026-02-14" -> "urgent"|"important"|"both"
    staged_keys  = NULL,       # data.frame with cols Empresa, Moneda, Documento — staged invoices
    party_tags          = list(),     # named list: "2026-02-14|Parte" -> "urgent"|"important"|"both"|""
    available_currencies = character(0) # currencies active this month for inline selector
) {

  ms    <- as.Date(floor_date(as.Date(month_start), "month"))
  me    <- as.Date(ceiling_date(ms, "month") - days(1))
  cur      <- toupper(trimws(currency))
  # Currency-specific color for the badge
  cur_col  <- tryCatch(.currency_color(cur), error = function(e) "#5F5E5A")
  cur_tint <- local({
    h <- gsub("^#", "", cur_col)
    r <- strtoi(substr(h, 1, 2), 16L)
    g <- strtoi(substr(h, 3, 4), 16L)
    b <- strtoi(substr(h, 5, 6), 16L)
    sprintf("rgba(%d,%d,%d,0.13)", r, g, b)
  })
  today <- Sys.Date()
  fmt   <- function(x) formatC(round(x), format = "f", digits = 0, big.mark = ",")

  # ── Aggregate data ──────────────────────────────────────────────────────────
  dcur <- data_due |>
    mutate(Moneda = toupper(trimws(Moneda))) |>
    filter(Moneda == cur, Fecha >= ms, Fecha <= me, !is.na(Fecha)) |>
    mutate(Importe = abs(Importe))
  message("[CAL_HTML] cur=", cur, " data_due_rows=", nrow(data_due), 
        " dcur_rows=", nrow(dcur),
        " monedas_in=", paste(unique(data_due$Moneda), collapse=","))

  # Tag priority weight helper — tagged rows float to the top
  tag_weight <- function(tag) {
    switch(tag,
      "urgent"    = 1L,
      "both"      = 2L,
      "important" = 3L,
      4L
    )
  }

  per_party_raw <- dcur |>
    group_by(Fecha, Parte) |>
    summarise(Importe = sum(Importe, na.rm = TRUE), .groups = "drop")

  # Fast path: party_tags is almost always empty — avoid per-row list lookups
  per_party <- if (length(party_tags) == 0L) {
    per_party_raw |> arrange(Fecha, desc(Importe))
  } else {
    per_party_raw |>
      mutate(
        .tag_key    = paste0(as.character(Fecha), "|", Parte),
        .party_tag  = vapply(.tag_key, function(k) party_tags[[k]] %||% "", character(1)),
        .tag_weight = vapply(.party_tag, tag_weight, integer(1))
      ) |>
      arrange(Fecha, .tag_weight, desc(Importe)) |>
      select(-.tag_key, -.tag_weight)
  }

  # Urgent/important tags per day from tags_day argument.
  # Guard: only emit a tag badge when the day actually has visible items in the
  # current currency view.  tags_day_map_rv is currency-agnostic by design (to
  # avoid re-running on currency switches), so without this check a tagged item
  # in USD would leave a badge on an otherwise-empty day of the MXN calendar,
  # and moved/deleted items whose day became empty would still show a badge.
  tag_for_day <- function(d) {
    key <- as.character(d)
    if (!key %in% names(day_parties)) return("")
    tags_day[[key]] %||% ""
  }

  # ── Build day-level summary ──────────────────────────────────────────────────
  # Split per_party by Fecha into a named list for fast lookup
  day_parties <- split(per_party, per_party$Fecha)

  # ── Staged (en proceso) count per day ───────────────────────────────────────
  # staged_keys is a data.frame with Empresa, Moneda, Documento, FechaVenc.
  # Use FechaVenc directly so manual/custom entries also show on the calendar
  # without needing to match against SAP data_due rows.
  staged_dates <- character(0)
  staged_count_by_day <- list()
  if (!is.null(staged_keys) && nrow(staged_keys) > 0) {
    if ("FechaVenc" %in% names(staged_keys)) {
      # Primary path: date comes from pagar_hoy FechaVenc — works for all entry types
      sk_dated <- staged_keys |>
        dplyr::filter(!is.na(FechaVenc)) |>
        dplyr::mutate(Fecha = as.Date(FechaVenc))
      if (nrow(sk_dated) > 0) {
        day_staged <- sk_dated |>
          group_by(Fecha) |>
          summarise(n_staged = n(), .groups = "drop")
        staged_count_by_day <- setNames(
          as.list(day_staged$n_staged),
          as.character(day_staged$Fecha)
        )
        staged_dates <- names(staged_count_by_day)
      }
    } else if (nrow(dcur) > 0 && all(c("Empresa","Documento") %in% names(data_due))) {
      # Fallback: join against SAP data_due (for entries that lack FechaVenc)
      staged_in_view <- data_due |>
        mutate(Moneda = toupper(trimws(Moneda))) |>
        filter(Moneda == cur, !is.na(Fecha)) |>
        inner_join(
          staged_keys |> select(Empresa, Moneda, Documento) |>
            mutate(Moneda = toupper(trimws(Moneda))),
          by = c("Empresa", "Moneda", "Documento")
        )
      if (nrow(staged_in_view) > 0) {
        day_staged <- staged_in_view |>
          group_by(Fecha) |>
          summarise(n_staged = n(), .groups = "drop")
        staged_count_by_day <- setNames(
          as.list(day_staged$n_staged),
          as.character(day_staged$Fecha)
        )
        staged_dates <- names(staged_count_by_day)
      }
    }
  }

  # ── Calendar grid (Mon–Sun, weeks as rows) ──────────────────────────────────
  grid <- calendar_grid(ms)

  # Week rows needed
  n_weeks <- max(grid$week)

  # Day-of-week header
  dow_labels <- c("Lun", "Mar", "Mié", "Jue", "Vie", "Sáb", "Dom")

  # ── Render one tile ─────────────────────────────────────────────────────────
  # is_weekend is precomputed from the grid column index (d >= 6) by the caller.
  render_tile <- function(fecha, is_weekend) {
    if (is.na(fecha)) {
      return('<div class="cal-tile cal-tile--empty"></div>')
    }

    date_str   <- as.character(fecha)
    is_today   <- fecha == today
    parties    <- day_parties[[date_str]]
    has_data   <- !is.null(parties) && nrow(parties) > 0
    day_tag    <- tag_for_day(fecha)
    n_staged   <- staged_count_by_day[[date_str]] %||% 0L
    has_staged <- n_staged > 0

    tile_class <- paste0(
      "cal-tile",
      if (is_today)   " cal-tile--today"   else "",
      if (is_weekend) " cal-tile--weekend" else "",
      if (has_data)   " cal-tile--has-data" else "",
      if (has_staged) " cal-tile--en-proceso" else "",
      if (day_tag == "urgent")    " cal-tile--urgent"    else "",
      if (day_tag == "important") " cal-tile--important" else "",
      if (day_tag == "both")      " cal-tile--both"      else ""
    )

    # Day number header row
    day_num_html <- paste0('<span class="cal-daynum">', day(fecha), '</span>')

    # Tag badge pills in the header
    tag_badge_html <- if (nzchar(day_tag)) {
      badge_class <- switch(day_tag,
        urgent    = "cal-badge cal-badge--urgent",
        important = "cal-badge cal-badge--important",
        both      = "cal-badge cal-badge--both",
        ""
      )
      badge_label <- switch(day_tag,
        urgent    = "\u25cf URG",
        important = "\u25cf IMP",
        both      = "\u25cf URG+IMP",
        ""
      )
      paste0('<span class="', badge_class, '">', badge_label, '</span>')
    } else ""

    staged_pill_html <- if (has_staged) {
      paste0('<span class="cal-badge cal-badge--staged">\u23f3 ', n_staged, '</span>')
    } else ""

    tile_header_html <- paste0(
      '<div class="cal-tile-header">',
        day_num_html,
        '<div class="d-flex gap-1 flex-wrap justify-content-end">',
          staged_pill_html,
          tag_badge_html,
        '</div>',
      '</div>'
    )

    # Invoice rows
    body_html <- if (has_data) {
      total   <- sum(parties$Importe, na.rm = TRUE)
      n_total <- nrow(parties)
      shown   <- head(parties, max_rows)
      hidden  <- n_total - nrow(shown)

      rows_html <- vapply(seq_len(nrow(shown)), function(i) {
        p <- shown[i, ]
        paste0(
          '<div class="cal-row">',
            '<span class="cal-party">', str_trunc(p$Parte, 18), '</span>',
            '<span class="cal-amount">$ ', fmt(p$Importe), '</span>',
          '</div>'
        )
      }, character(1))

      more_html <- if (hidden > 0) {
        paste0('<div class="cal-more">+ ', hidden, ' m\u00e1s</div>')
      } else ""

      paste0(
        '<div class="cal-rows">', paste(rows_html, collapse = ""), more_html, '</div>',
        '<div class="cal-divider"></div>',
        '<div class="cal-total">',
          '<span>Total</span>',
          '<span>$ ', fmt(total), '</span>',
        '</div>'
      )
    } else ""

    # Click target — fires Shiny input
    onclick_js <- sprintf(
      "Shiny.setInputValue('%s', {date: '%s', ledger: '%s', nonce: Math.random()}, {priority: 'event'});",
      input_id, date_str, ledger
    )

    paste0(
      '<div class="', tile_class, '"',
      ' onclick="', onclick_js, '"',
      if (has_data) ' style="cursor:pointer;"' else "",
      '>',
        tile_header_html,
        body_html,
      '</div>'
    )
  }

  # ── Assemble grid ────────────────────────────────────────────────────────────
  # Store Dates directly — avoids as.character() + as.Date() round-trip per tile.
  date_lookup <- setNames(grid$Fecha, paste(grid$week, grid$wday))

  week_rows_html <- vapply(seq_len(n_weeks), function(w) {
    cells_html <- vapply(1:7, function(d) {
      render_tile(date_lookup[paste(w, d)], is_weekend = d >= 6)
    }, character(1))
    paste0('<div class="cal-week">', paste(cells_html, collapse = ""), '</div>')
  }, character(1))

  # ── Title ────────────────────────────────────────────────────────────────────
  meses <- c("enero","febrero","marzo","abril","mayo","junio",
             "julio","agosto","septiembre","octubre","noviembre","diciembre")
  month_label <- paste0(meses[month(ms)], " ", year(ms))

  toggle_onclick_ar <- "Shiny.setInputValue('cal_ledger_toggle','AR',{priority:'event'})"
  toggle_onclick_ap <- "Shiny.setInputValue('cal_ledger_toggle','AP',{priority:'event'})"

  # Build interactive month picker label
  picker_id <- paste0("cal-mpicker-", ledger)
  yr_opts   <- (year(ms) - 2):(year(ms) + 2)
  yr_btns <- paste0(
    vapply(yr_opts, function(y)
      paste0('<button class="cal-mpy-btn', if (y == year(ms)) " cal-mpy-sel" else "",
             '" onclick="hopMPickYear(\'', picker_id, '\',', y, ');">',
             y, '</button>'),
      character(1)),
    collapse = "")
  mon_btns <- paste0(
    vapply(seq_along(meses), function(i) {
      paste0('<button class="cal-mpm-btn', if (i == month(ms)) " cal-mpm-sel" else "",
             '" onclick="hopMPickSel(\'', picker_id, '\',', i, ');">',
             substr(meses[i], 1, 3), '</button>')
    }, character(1)),
    collapse = "")
  period_label_html <- paste0(
    '<span class="cal-period-label" onclick="hopMPick(this)" title="Cambiar mes">',
    month_label,
    '<svg class="cal-pick-arr" viewBox="0 0 10 6"><path d="M1 1l4 4 4-4" stroke="currentColor" stroke-width="1.5" fill="none" stroke-linecap="round"/></svg>',
    '<div class="cal-mpicker" id="', picker_id, '" data-sel-year="', year(ms), '" onclick="event.stopPropagation()">',
    '<div class="cal-mp-nav">',
    '<button class="cal-mp-navbtn" onclick="hopMPickNav(\'', picker_id, '\',-1)">&#8249;</button>',
    '<span class="cal-mp-navlbl">', year(ms), '</span>',
    '<button class="cal-mp-navbtn" onclick="hopMPickNav(\'', picker_id, '\',1)">&#8250;</button>',
    '</div>',
    '<div class="cal-mp-months">', mon_btns, '</div>',
    '<div class="cal-mp-years">', yr_btns, '</div>',
    '</div>',
    '</span>')

  # Build interactive currency badge
  if (length(available_currencies) > 0) {
    cur_opts_html <- paste0(
      vapply(available_currencies, function(c_opt)
        paste0('<button class="cal-cur-opt', if (c_opt == cur) " cal-cur-sel" else "",
               '" onclick="hopCurPick(\'', ledger, '\',\'', c_opt, '\');">', c_opt, '</button>'),
        character(1)),
      collapse = "")
    cur_badge_html <- paste0(
      '<span class="cal-currency-badge cal-currency-interactive"',
      ' style="background:', cur_tint, ';color:', cur_col, ';"',
      ' onclick="hopCurPick(\'', ledger, '\',null,this)" title="Cambiar moneda">',
      cur,
      '<svg class="cal-pick-arr" viewBox="0 0 10 6"><path d="M1 1l4 4 4-4" stroke="currentColor" stroke-width="1.5" fill="none" stroke-linecap="round"/></svg>',
      '<div class="cal-cur-picker" id="cal-curpicker-', ledger, '" onclick="event.stopPropagation()">',
      cur_opts_html,
      '</div>',
      '</span>')
  } else {
    cur_badge_html <- paste0('<span class="cal-currency-badge" style="background:', cur_tint, ';color:', cur_col, ';">', cur, '</span>')
  }

  # Preview arrow fires btn_preview_open via Shiny.setInputValue so the server
  # observer in app.R (observeEvent(input$btn_preview_open, ...)) can toggle
  # cf_preview_open without the button needing to be a Shiny actionButton.
  preview_arrow_html <- paste0(
    '<button class="cf-preview-toggle-btn" ',
    'onclick="Shiny.setInputValue(\'btn_preview_open\',Math.random(),{priority:\'event\'})" ',
    'title="Ver flujo de caja" aria-label="Abrir vista de flujo de caja">',
    '&#8250;',   # \u203a right-pointing angle quotation \u2014 matches the chevron in the spec
    '</button>'
  )

  title_bar_html <- paste0(
    '<div class="cal-title-bar">',
    '<div class="cal-title-left">',
    '<div class="cal-ledger-toggle" role="group">',
    '<button class="cal-toggle-btn', if (ledger == "AR") " cal-toggle-active" else "", '"',
    ' onclick="', toggle_onclick_ar, '">Cobros</button>',
    '<button class="cal-toggle-btn', if (ledger == "AP") " cal-toggle-active" else "", '"',
    ' onclick="', toggle_onclick_ap, '">Pagos</button>',
    '</div>',
    period_label_html,
    cur_badge_html,
    preview_arrow_html,
    '</div>',
    '<div class="cal-title-hint">Haz clic en un d\u00eda para ver el detalle</div>',
    '</div>'
  )

  # Day-of-week header row
  dow_row_html <- paste0(
    '<div class="cal-dow-row">',
    paste(vapply(dow_labels, function(d) {
      paste0('<div class="cal-dow',
             if (d %in% c("Sáb", "Dom")) " cal-dow--weekend" else "",
             '">', d, '</div>')
    }, character(1)), collapse = ""),
    '</div>'
  )

  theme_class <- paste0("cal-theme-", tolower(ledger))
  htmltools::HTML(paste0(
    '<div class="', theme_class, '">',
    title_bar_html,
    '<div class="cal-grid-wrapper">',
      dow_row_html,
      '<div class="cal-grid">', paste(week_rows_html, collapse = ""), '</div>',
    '</div>',
    '</div>'
  ))
}

# Bootstrap blocks removed — auto-seeding eliminated per CLAUDIO_SPEC Stage 0.
# Accounts are created via scripts/seed_hd_admin.R (admin) or the invite flow.

# ── Cross-session agenda sync state (single-process ShinyApps.io) ─────────────
# Sessions share this in-memory state to propagate changes without re-reading S3.
# .agenda_sync$version bumps on every write — polling observers detect the change.
if (!exists(".agenda_sync", envir = .GlobalEnv)) {
  assign(".agenda_sync", list(
    is_on   = FALSE,
    data    = NULL,
    version = as.character(proc.time()[["elapsed"]])
  ), envir = .GlobalEnv)
}

