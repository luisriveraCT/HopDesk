# =============================================================================
# R/forecasting_source_yahoo.R
# Yahoo Finance adapter via quantmod — non-official, scraped data.
# =============================================================================

fcs_fetch_yahoo <- function(metric, date_from, date_to) {
  ticker <- metric$fetch_params[[1]]$ticker %||% ""
  if (!nzchar(ticker)) {
    return(list(status = "parse_error", rows = NULL, error_msg = "missing ticker"))
  }

  data <- tryCatch(
    quantmod::getSymbols(
      ticker, src = "yahoo",
      from         = as.Date(date_from),
      to           = as.Date(date_to),
      auto.assign  = FALSE,
      warnings     = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(data) || nrow(data) == 0L) {
    return(list(status = "no_data", rows = NULL, error_msg = "no quantmod data"))
  }

  adj_col <- grep("\\.Adjusted$", colnames(data), value = TRUE)
  if (!length(adj_col)) adj_col <- grep("\\.Close$", colnames(data), value = TRUE)
  if (!length(adj_col)) {
    return(list(status = "parse_error", rows = NULL, error_msg = "no Adjusted/Close column"))
  }

  rows <- tibble::tibble(
    metric_id        = metric$metric_id,
    fecha            = as.Date(zoo::index(data)),
    value            = as.numeric(data[, adj_col[1]]),
    source_id        = "yahoo",
    observation_type = "scraped",
    fetched_at       = Sys.time(),
    fetched_by       = "system",
    notes            = NA_character_
  )
  rows <- rows[!is.na(rows$value) & !is.na(rows$fecha), , drop = FALSE]

  list(
    status    = if (nrow(rows)) "ok" else "no_data",
    rows      = rows,
    error_msg = NULL
  )
}
