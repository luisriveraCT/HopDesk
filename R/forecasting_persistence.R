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

# The client's metrics catalog changed on S3 as it is now (T63): strict
# re-read right before writing (a failed read stop()s: nothing is written),
# `cambio` applied to that fresh copy, and written only if it changed.
modificar_forecasting_metrics <- function(client_id, cambio, evento = NULL) {
  if (is.null(client_id) || !nzchar(client_id))
    stop("modificar_forecasting_metrics: sin client_id", call. = FALSE)
  raw <- .s3_leer_para_escribir(S3_KEYS$forecasting_metrics, client_id)
  fresco <- if (is.null(raw) || !nrow(raw)) .schema_forecasting_metrics()
            else .fcp_unpack_list_col(.fcp_unpack_list_col(.normalize(raw, .schema_forecasting_metrics),
                                                           "fallback_source_ids"), "fetch_params")
  nuevo <- cambio(fresco)
  if (identical(nuevo, fresco)) return(invisible(fresco))
  if (is.null(evento)) save_forecasting_metrics(nuevo, client_id = client_id)
  else save_forecasting_metrics(nuevo, client_id = client_id, evento = evento)
  invisible(nuevo)
}

# evento (T72-C): the write's log entry (evento_sistema() for the seed); without
# it the .s3_write() call is exactly the one before.
save_forecasting_metrics <- function(df, client_id = NULL, evento = NULL) {
  packed <- .fcp_pack_list_col(df, "fallback_source_ids")
  packed <- .fcp_pack_list_col(packed, "fetch_params")
  packed <- .normalize(packed, .schema_forecasting_metrics)
  if (is.null(evento)) .s3_write(packed, S3_KEYS$forecasting_metrics, client_id = client_id)
  else .s3_write(packed, S3_KEYS$forecasting_metrics, client_id = client_id, evento = evento)
  if (exists("bump_sync_version", mode = "function")) bump_sync_version("forecasting_metrics_db", client_id = client_id)
  invisible(NULL)
}

# ── sources (static config — no sync) ────────────────────────────────────────

load_forecasting_sources <- function() {
  raw <- tryCatch(.s3_read(S3_KEYS$forecasting_sources), error = function(e) NULL)
  if (is.null(raw) || !nrow(raw)) return(.schema_forecasting_sources())
  .normalize(raw, .schema_forecasting_sources)
}

save_forecasting_sources <- function(df, evento = NULL) {
  if (is.null(evento)) .s3_write(.normalize(df, .schema_forecasting_sources), S3_KEYS$forecasting_sources)
  else .s3_write(.normalize(df, .schema_forecasting_sources), S3_KEYS$forecasting_sources, evento = evento)
  invisible(NULL)
}

# T116 F3: the deployment's sources table, changed safely -- a strict re-read
# right before writing (a failed read stop()s, nothing written), and an
# unchanged table neither written nor logged.
modificar_forecasting_sources <- function(cambio, evento = NULL) {
  raw <- .s3_leer_para_escribir(S3_KEYS$forecasting_sources, NULL)
  fresco <- if (is.null(raw) || !nrow(raw)) .schema_forecasting_sources() else .normalize(raw, .schema_forecasting_sources)
  nuevo <- cambio(fresco)
  if (identical(nuevo, fresco)) return(invisible(fresco))
  save_forecasting_sources(nuevo, evento = evento)
  if (exists("fcs_flush_caches", mode = "function")) fcs_flush_caches()
  invisible(nuevo)
}

# ── methods (static config — no sync) ────────────────────────────────────────

load_forecasting_methods <- function() {
  raw <- tryCatch(.s3_read(S3_KEYS$forecasting_methods), error = function(e) NULL)
  if (is.null(raw) || !nrow(raw)) return(.schema_forecasting_methods())
  raw <- .normalize(raw, .schema_forecasting_methods)
  .fcp_unpack_list_col(raw, "params_schema")
}

save_forecasting_methods <- function(df, evento = NULL) {
  packed <- .fcp_pack_list_col(df, "params_schema")
  if (is.null(evento)) .s3_write(.normalize(packed, .schema_forecasting_methods), S3_KEYS$forecasting_methods)
  else .s3_write(.normalize(packed, .schema_forecasting_methods), S3_KEYS$forecasting_methods, evento = evento)
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

save_forecasting_subscriptions <- function(df, client_id = NULL, evento = NULL) {
  packed <- .fcp_pack_list_col(df, "method_params")
  if (is.null(evento)) .s3_write(.normalize(packed, .schema_forecasting_subscriptions),
                                 S3_KEYS$forecasting_subscriptions, client_id = client_id)
  else .s3_write(.normalize(packed, .schema_forecasting_subscriptions),
                 S3_KEYS$forecasting_subscriptions, client_id = client_id, evento = evento)
  if (exists("bump_sync_version", mode = "function")) bump_sync_version("forecasting_subscriptions_db", client_id = client_id)
  invisible(NULL)
}

# Changes the subscriptions on S3 as they are now (T26): a strict read (a
# read that fails stop()s, never an empty table), `cambio(fresco)` applied to
# it, and a write only if something changed. Returns the table now on S3.
# load_forecasting_subscriptions() turns a failed read into an empty table --
# fine to show, but its writers saved on it: creating one global subscription
# with the read down rewrote them all, and the start-up seed took a failed
# read for "no subscriptions" and wrote the defaults over the real ones.
modificar_forecasting_subscriptions <- function(client_id, cambio, evento = NULL) {
  if (is.null(client_id) || !nzchar(client_id))
    stop("modificar_forecasting_subscriptions: sin client_id", call. = FALSE)
  raw <- .s3_leer_para_escribir(S3_KEYS$forecasting_subscriptions, client_id)
  fresco <- if (is.null(raw) || !nrow(raw)) .schema_forecasting_subscriptions()
            else .fcp_unpack_list_col(.normalize(raw, .schema_forecasting_subscriptions), "method_params")
  nuevo <- cambio(fresco)
  if (identical(nuevo, fresco)) return(invisible(fresco))
  if (is.null(evento)) save_forecasting_subscriptions(nuevo, client_id = client_id)
  else save_forecasting_subscriptions(nuevo, client_id = client_id, evento = evento)
  invisible(nuevo)
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
