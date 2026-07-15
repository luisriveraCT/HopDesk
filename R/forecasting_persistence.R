# =============================================================================
# R/forecasting_persistence.R
# load_* / save_* for every Forecasting table.
# List-column tables use the same round-trip helper as pasivos_persistence.R.
# =============================================================================

# ── Helpers ───────────────────────────────────────────────────────────────────

# Serialize a list-column to JSON strings for RDS round-trip stability.
.fcp_pack_list_col <- function(df, col) {
  if (!col %in% names(df)) return(df)
  df[[col]] <- lapply(df[[col]], function(x) {
    if (is.null(x)) "" else jsonlite::toJSON(x, auto_unbox = TRUE)
  })
  df
}

.fcp_unpack_list_col <- function(df, col) {
  if (!col %in% names(df)) return(df)
  df[[col]] <- lapply(df[[col]], function(x) {
    if (is.null(x) || identical(x, "") || identical(x, "null")) list()
    else tryCatch(jsonlite::fromJSON(as.character(x), simplifyVector = FALSE),
                  error = function(e) list())
  })
  df
}

# ── series_observations ───────────────────────────────────────────────────────

load_forecasting_series_observations <- function(client_id = NULL) {
  raw <- tryCatch(.s3_read_with(S3_KEYS$forecasting_series_observations, client_id = client_id),
                  error = function(e) NULL)
  if (is.null(raw) || !nrow(raw)) return(.schema_forecasting_series_observations())
  .normalize(raw, .schema_forecasting_series_observations)
}

save_forecasting_series_observations <- function(df, client_id = NULL) {
  .s3_write(.normalize(df, .schema_forecasting_series_observations),
            S3_KEYS$forecasting_series_observations, client_id = client_id)
  if (exists("bump_sync_version", mode = "function")) bump_sync_version("forecasting_series_observations_db", client_id = client_id)
  invisible(NULL)
}

# ── metrics ───────────────────────────────────────────────────────────────────

load_forecasting_metrics <- function(client_id = NULL) {
  raw <- tryCatch(.s3_read_with(S3_KEYS$forecasting_metrics, client_id = client_id), error = function(e) NULL)
  if (is.null(raw) || !nrow(raw)) return(.schema_forecasting_metrics())
  raw <- .normalize(raw, .schema_forecasting_metrics)
  raw <- .fcp_unpack_list_col(raw, "fallback_source_ids")
  raw <- .fcp_unpack_list_col(raw, "fetch_params")
  raw
}

save_forecasting_metrics <- function(df, client_id = NULL) {
  packed <- .fcp_pack_list_col(df, "fallback_source_ids")
  packed <- .fcp_pack_list_col(packed, "fetch_params")
  packed <- .normalize(packed, .schema_forecasting_metrics)
  .s3_write(packed, S3_KEYS$forecasting_metrics, client_id = client_id)
  if (exists("bump_sync_version", mode = "function")) bump_sync_version("forecasting_metrics_db", client_id = client_id)
  invisible(NULL)
}

# ── sources (static config — no sync) ────────────────────────────────────────

load_forecasting_sources <- function() {
  raw <- tryCatch(.s3_read(S3_KEYS$forecasting_sources), error = function(e) NULL)
  if (is.null(raw) || !nrow(raw)) return(.schema_forecasting_sources())
  .normalize(raw, .schema_forecasting_sources)
}

save_forecasting_sources <- function(df) {
  .s3_write(.normalize(df, .schema_forecasting_sources), S3_KEYS$forecasting_sources)
  invisible(NULL)
}

# ── methods (static config — no sync) ────────────────────────────────────────

load_forecasting_methods <- function() {
  raw <- tryCatch(.s3_read(S3_KEYS$forecasting_methods), error = function(e) NULL)
  if (is.null(raw) || !nrow(raw)) return(.schema_forecasting_methods())
  raw <- .normalize(raw, .schema_forecasting_methods)
  .fcp_unpack_list_col(raw, "params_schema")
}

save_forecasting_methods <- function(df) {
  packed <- .fcp_pack_list_col(df, "params_schema")
  .s3_write(.normalize(packed, .schema_forecasting_methods), S3_KEYS$forecasting_methods)
  invisible(NULL)
}

# ── indicators (static config — no sync) ─────────────────────────────────────

load_forecasting_indicators <- function() {
  raw <- tryCatch(.s3_read(S3_KEYS$forecasting_indicators), error = function(e) NULL)
  if (is.null(raw) || !nrow(raw)) return(.schema_forecasting_indicators())
  raw <- .normalize(raw, .schema_forecasting_indicators)
  .fcp_unpack_list_col(raw, "params_schema")
}

save_forecasting_indicators <- function(df) {
  packed <- .fcp_pack_list_col(df, "params_schema")
  .s3_write(.normalize(packed, .schema_forecasting_indicators), S3_KEYS$forecasting_indicators)
  invisible(NULL)
}

# ── subscriptions ─────────────────────────────────────────────────────────────

load_forecasting_subscriptions <- function(client_id = NULL) {
  raw <- tryCatch(.s3_read_with(S3_KEYS$forecasting_subscriptions, client_id = client_id), error = function(e) NULL)
  if (is.null(raw) || !nrow(raw)) return(.schema_forecasting_subscriptions())
  raw <- .normalize(raw, .schema_forecasting_subscriptions)
  .fcp_unpack_list_col(raw, "method_params")
}

save_forecasting_subscriptions <- function(df, client_id = NULL) {
  packed <- .fcp_pack_list_col(df, "method_params")
  .s3_write(.normalize(packed, .schema_forecasting_subscriptions),
            S3_KEYS$forecasting_subscriptions, client_id = client_id)
  if (exists("bump_sync_version", mode = "function")) bump_sync_version("forecasting_subscriptions_db", client_id = client_id)
  invisible(NULL)
}

# ── manual_curves ─────────────────────────────────────────────────────────────

load_forecasting_manual_curves <- function(client_id = NULL) {
  raw <- tryCatch(.s3_read_with(S3_KEYS$forecasting_manual_curves, client_id = client_id), error = function(e) NULL)
  if (is.null(raw) || !nrow(raw)) return(.schema_forecasting_manual_curves())
  .normalize(raw, .schema_forecasting_manual_curves)
}

save_forecasting_manual_curves <- function(df, client_id = NULL) {
  .s3_write(.normalize(df, .schema_forecasting_manual_curves),
            S3_KEYS$forecasting_manual_curves, client_id = client_id)
  if (exists("bump_sync_version", mode = "function")) bump_sync_version("forecasting_manual_curves_db", client_id = client_id)
  invisible(NULL)
}

# ── fetch_log (append-only diagnostic — no sync) ─────────────────────────────

load_forecasting_fetch_log <- function() {
  raw <- tryCatch(.s3_read(S3_KEYS$forecasting_fetch_log), error = function(e) NULL)
  if (is.null(raw) || !nrow(raw)) return(.schema_forecasting_fetch_log())
  .normalize(raw, .schema_forecasting_fetch_log)
}

append_forecasting_fetch_log <- function(row) {
  current <- load_forecasting_fetch_log()
  updated <- dplyr::bind_rows(current, row)
  .s3_write(.normalize(updated, .schema_forecasting_fetch_log),
            S3_KEYS$forecasting_fetch_log)
  invisible(NULL)
}
