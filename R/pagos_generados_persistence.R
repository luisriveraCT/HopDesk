# =============================================================================
# R/pagos_generados_persistence.R
#
# Permanent, structured audit trail of every payment file generated (bank
# PPL-layout downloads) -- one row per generated file, per client, kept
# forever for financial auditability ("I need to be able to see what my
# users did months or years ago from this special log," Mouse, 2026-08-25).
# Never overwritten in place -- rotated into archived chunks the same way
# R/app_audit.R's app_audit.rds already is, so a years-long history stays
# bounded per S3 object.
#
# Deliberately NOT deferred/batched like log_action()'s .audit_queue: this
# data is explicitly "delicate" (Mouse's own word) and payment-file
# generation is a rare, deliberate, one-user-at-a-time action, not a hot
# path with 60+ call sites needing sub-second responsiveness -- a
# synchronous, retried read-modify-write trades a few hundred ms of latency
# for zero risk of losing a record in the crash window log_action()'s
# in-memory buffer explicitly accepts as a tradeoff. Retry pattern mirrors
# save_pagar_hoy_with_retry() (R/persistence.R) exactly.
#
# Deliberately does NOT store the actual generated bank-layout file bytes
# (Mouse's explicit call, 2026-08-25: avoids duplicative storage and a
# growing legacy-format liability as banks change layouts over time) --
# only the fully structured contents of what was generated. A PDF "receipt"
# can be rendered on demand from this record (R/pagos_audit_viewer_module.R)
# as proof nothing was edited after the fact, without needing to store or
# re-open the original file format.
# =============================================================================

.MAX_PAGOS_ROWS <- 10000L

.schema_pagos_generados <- function() tibble::tibble(
  id            = character(),
  ts            = as.POSIXct(character()),
  user          = character(),
  client_id     = character(),
  empresa       = character(),
  moneda        = character(),
  cuenta_origen = character(),
  banco_origen  = character(),
  n_pagos       = integer(),
  total         = double(),
  beneficiarios = character(),  # JSON-encoded list of {alias, clabe, banco, importe, concepto} -- "pago_ppl" only
  source        = character(),  # "individual" | "zip_bulk" | "word" | "excel" | "pdf" | ...
  archivo       = character(),  # filename of the generated file (bytes not stored, see header)
  # Extended 2026-08-26 to cover every generated/downloaded file app-wide,
  # not just bank-payment files ("every time I download anything... it
  # needs to appear here" -- Mouse, 2026-08-26): tipo distinguishes a
  # payment file ("pago_ppl", the only kind that ever populates
  # beneficiarios/cuenta_origen/banco_origen) from every other generated
  # file ("reporte_word"/"reporte_excel"/"reporte_pdf"/...), which instead
  # carries whatever it needs in the free-form detalle JSON blob.
  tipo          = character(),
  detalle       = character()   # JSON-encoded named list, free-form per tipo; NA for "pago_ppl"
)

.pagos_current_key <- function(cid) paste0(tolower(trimws(cid)), "/pagos_generados.rds")

.pagos_read_chunk <- function(key) {
  tryCatch(
    suppressMessages(suppressWarnings(
      aws.s3::s3readRDS(object = key, bucket = .s3_bucket())
    )),
    error = function(e) NULL
  )
}

.pagos_write_chunk <- function(df, key) {
  aws.s3::s3saveRDS(df, object = key, bucket = .s3_bucket())
}

# ── Rotation ──────────────────────────────────────────────────────────────────
rotate_pagos_if_needed <- function(cid, current_df) {
  if (is.null(current_df) || nrow(current_df) < .MAX_PAGOS_ROWS)
    return(invisible(FALSE))

  prefix <- paste0(cid, "/pagos_generados_")
  existing_keys <- tryCatch({
    objs <- aws.s3::get_bucket(bucket = .s3_bucket(), prefix = prefix, max = 200L)
    vapply(objs, function(o) o[["Key"]], character(1))
  }, error = function(e) character(0))

  existing_nums <- suppressWarnings(as.integer(
    sub(paste0("^", cid, "/pagos_generados_(\\d+)\\.rds$"), "\\1", existing_keys)
  ))
  existing_nums <- existing_nums[!is.na(existing_nums)]
  next_idx      <- if (length(existing_nums)) max(existing_nums) + 1L else 1L
  archive_key   <- sprintf("%s/pagos_generados_%03d.rds", cid, next_idx)

  tryCatch({
    .pagos_write_chunk(current_df, archive_key)
    .pagos_write_chunk(.schema_pagos_generados(), .pagos_current_key(cid))
    message(sprintf("[PAGOS_AUDIT] Rotated %d rows to %s", nrow(current_df), archive_key))
    invisible(TRUE)
  }, error = function(e) {
    message("[PAGOS_AUDIT] Rotation failed: ", e$message)
    invisible(FALSE)
  })
}

# ── Read ──────────────────────────────────────────────────────────────────────
# Read ALL chunks (current + archived) for a client, newest first.
read_pagos_generados <- function(client_id = NULL, since = NULL, until = NULL) {
  cid    <- tolower(trimws(client_id %||% Sys.getenv("CLIENT_ID")))
  prefix <- paste0(cid, "/pagos_generados")

  all_keys <- tryCatch({
    objs <- aws.s3::get_bucket(bucket = .s3_bucket(), prefix = prefix, max = 200L)
    vapply(objs, function(o) o[["Key"]], character(1))
  }, error = function(e) character(0))

  chunks <- if (length(all_keys)) Filter(Negate(is.null), lapply(all_keys, .pagos_read_chunk)) else list()

  result <- if (length(chunks)) tryCatch(
    dplyr::bind_rows(lapply(chunks, function(ch) {
      for (col in names(.schema_pagos_generados()))
        if (!col %in% names(ch)) ch[[col]] <- NA
      ch[, names(.schema_pagos_generados()), drop = FALSE]
    })),
    error = function(e) .schema_pagos_generados()
  ) else .schema_pagos_generados()

  if (!nrow(result)) return(.schema_pagos_generados())

  if (!is.null(since)) {
    since_ts <- tryCatch(as.POSIXct(since), error = function(e) NULL)
    if (!is.null(since_ts))
      result <- result[!is.na(result$ts) & result$ts >= since_ts, , drop = FALSE]
  }
  if (!is.null(until)) {
    until_ts <- tryCatch(as.POSIXct(until), error = function(e) NULL)
    if (!is.null(until_ts))
      result <- result[!is.na(result$ts) & result$ts <= until_ts, , drop = FALSE]
  }

  result[order(result$ts, decreasing = TRUE), , drop = FALSE]
}

# ── Write ─────────────────────────────────────────────────────────────────────
# Append one permanent, immutable row. Never call this to modify/remove an
# existing row -- there is deliberately no update/delete path, matching
# papelera's/app_audit's own "never edited once written" guarantee.
add_pago_generado <- function(record, client_id = NULL, max_attempts = 3, retry_delay = 0.5) {
  cid <- tolower(trimws(client_id %||% Sys.getenv("CLIENT_ID")))
  key <- .pagos_current_key(cid)

  row <- tibble::as_tibble(record)
  for (col in names(.schema_pagos_generados()))
    if (!col %in% names(row)) row[[col]] <- .schema_pagos_generados()[[col]][NA_integer_]
  row <- row[, names(.schema_pagos_generados()), drop = FALSE]

  last_err <- NULL
  for (attempt in seq_len(max_attempts)) {
    ok <- tryCatch({
      current <- .pagos_read_chunk(key)
      if (is.null(current) || !is.data.frame(current) || !nrow(current))
        current <- .schema_pagos_generados()
      rotated <- isTRUE(rotate_pagos_if_needed(cid, current))
      if (rotated) {
        current <- tryCatch({
          ch <- .pagos_read_chunk(key)
          if (is.null(ch) || !nrow(ch)) .schema_pagos_generados() else ch
        }, error = function(e) .schema_pagos_generados())
      }
      .pagos_write_chunk(dplyr::bind_rows(current, row), key)
      TRUE
    }, error = function(e) { last_err <<- e; FALSE })
    if (ok) return(invisible(list(ok = TRUE, error = NULL, attempts = attempt)))
    if (attempt < max_attempts) Sys.sleep(retry_delay)
  }
  message("[PAGOS_AUDIT] add_pago_generado failed after ", max_attempts,
          " attempts: ", if (!is.null(last_err)) conditionMessage(last_err) else "?")
  invisible(list(ok = FALSE, error = last_err, attempts = max_attempts))
}

# ── Generic logging helper for non-payment downloads ─────────────────────────
# Payment files (PPL bank layouts) go through .log_pago_generado()
# (R/pagar_hoy_module.R), which fills beneficiarios/cuenta_origen/
# banco_origen. Every OTHER generated/downloaded file (Word/Excel/PDF cash
# flow exports, and any future export type) goes through this instead --
# same permanent table, same viewer, distinguished by `tipo`. Never throws;
# a logging failure must never block the user's actual download.
#
# detalle_list: a plain named list of whatever's relevant to this export
# (date range, empresas included, currency mode, ...) -- stored as-is, JSON-
# encoded, and shown as simple key/value pairs in the viewer's detail modal.
log_archivo_generado <- function(shared, tipo, empresa, moneda, detalle_list,
                                 total = NA_real_, n_items = NA_integer_,
                                 fname = NA_character_) {
  tryCatch({
    cid  <- tryCatch(shared$effective_client_id(), error = function(e) NULL) %||% Sys.getenv("CLIENT_ID")
    user <- tryCatch(shared$current_user(), error = function(e) NULL) %||% "unknown"
    add_pago_generado(
      list(
        id            = uuid::UUIDgenerate(),
        ts            = Sys.time(),
        user          = user,
        client_id     = cid,
        empresa       = as.character(empresa %||% NA_character_),
        moneda        = as.character(moneda %||% NA_character_),
        cuenta_origen = NA_character_,
        banco_origen  = NA_character_,
        n_pagos       = as.integer(n_items %||% NA_integer_),
        total         = as.numeric(total %||% NA_real_),
        beneficiarios = NA_character_,
        source        = as.character(tipo),
        archivo       = as.character(fname %||% NA_character_),
        tipo          = as.character(tipo),
        # as.character() strips jsonlite::toJSON()'s "json" S3 class -- see
        # the identical, hard-learned comment in R/pagar_hoy_module.R's
        # .log_pago_generado() for why this is required, not cosmetic.
        detalle       = as.character(jsonlite::toJSON(detalle_list %||% list(), auto_unbox = TRUE))
      ),
      client_id = cid
    )
  }, error = function(e) message("[PAGOS_AUDIT] log_archivo_generado failed: ", e$message))
}

# ── Helpers for consumers (viewer module + tests) ──────────────────────────────

# Parses the JSON beneficiarios blob back into a data frame. Never errors --
# an unparseable/empty blob returns a zero-row frame with the right columns.
parse_pago_beneficiarios <- function(json_str) {
  empty <- data.frame(alias = character(), clabe = character(), banco = character(),
                       importe = double(), concepto = character(), stringsAsFactors = FALSE)
  if (is.null(json_str) || is.na(json_str) || !nzchar(json_str)) return(empty)
  parsed <- tryCatch(jsonlite::fromJSON(json_str), error = function(e) NULL)
  if (is.null(parsed)) return(empty)
  df <- tryCatch(as.data.frame(parsed, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df) || !nrow(df)) return(empty)
  for (col in names(empty)) if (!col %in% names(df)) df[[col]] <- empty[[col]][NA_integer_]
  df[, names(empty), drop = FALSE]
}

# Parses the generic `detalle` JSON blob (non-"pago_ppl" rows) back into a
# plain named list of scalars, for the viewer to render as key/value pairs.
# Never errors -- an unparseable/empty blob returns an empty list.
parse_pago_detalle <- function(json_str) {
  if (is.null(json_str) || is.na(json_str) || !nzchar(json_str)) return(list())
  parsed <- tryCatch(jsonlite::fromJSON(json_str, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(parsed) || !length(parsed)) return(list())
  as.list(parsed)
}

# Masks a CLABE/account number to its last 4 digits (e.g. "••••1234").
# Anything shorter than 4 characters, or NA/blank, returns "" rather than
# a misleadingly "safe-looking" partial mask.
mask_clabe <- function(clabe) {
  clabe <- trimws(as.character(clabe %||% ""))
  vapply(clabe, function(c) {
    if (!nzchar(c) || is.na(c) || nchar(c) < 4) return("")
    paste0(strrep("•", 4), substr(c, nchar(c) - 3, nchar(c)))
  }, character(1), USE.NAMES = FALSE)
}
