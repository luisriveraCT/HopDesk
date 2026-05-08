# =============================================================================
# R/pasivos_persistence.R
# load_*() / save_*() for all Pasivos S3 keys.
# Mirrors the conventions in persistence.R:
#   - load_*() returns the empty schema on missing/invalid data (never NULL)
#   - save_*() normalizes via schema before writing, returns invisible(TRUE)
# Schemas with list-columns use a custom loader (policy_catalog pattern,
# persistence.R:257) to preserve list columns through the round-trip.
# =============================================================================

# ── Internal list-column loader helper ────────────────────────────────────────
# For a schema with list columns, .normalize() cannot round-trip them correctly
# because methods::as() coerces lists to character. This helper applies the same
# fix as load_policy_catalog(): rebuild non-list cols via schema[NA] coercion,
# then ensure list cols are actual lists.
.pasivos_load_with_list_cols <- function(key, schema_fn, list_col_names) {
  obj <- tryCatch(.s3_read(key), error = function(e) NULL)
  if (is.null(obj) || !is.data.frame(obj) || !nrow(obj)) return(schema_fn())

  schema <- schema_fn()
  for (col in setdiff(names(schema), list_col_names)) {
    if (!col %in% names(obj)) {
      obj[[col]] <- schema[[col]][NA_integer_]
    } else {
      obj[[col]] <- tryCatch(
        methods::as(obj[[col]], class(schema[[col]])),
        error = function(e) schema[[col]][NA_integer_]
      )
    }
  }
  for (col in list_col_names) {
    if (!col %in% names(obj)) {
      obj[[col]] <- vector("list", nrow(obj))
    } else if (!is.list(obj[[col]])) {
      obj[[col]] <- as.list(obj[[col]])
    }
  }
  dplyr::select(obj, dplyr::any_of(names(schema)))
}

# ── pasivos_liabilities ────────────────────────────────────────────────────────
# List columns: recurrence_params, schedule_imported, cargos_iniciales,
#               valor_residual, modifier_ids

.PASIVOS_LIABILITY_LIST_COLS <- c(
  "recurrence_params", "schedule_imported",
  "cargos_iniciales", "valor_residual", "modifier_ids"
)

load_pasivos_liabilities <- function() {
  df <- .pasivos_load_with_list_cols(
    S3_KEYS$pasivos_liabilities,
    .schema_pasivos_liability,
    .PASIVOS_LIABILITY_LIST_COLS
  )
  # Backward-compat: pre-frecuencia_pago records load as NA → default to mensual
  if ("frecuencia_pago" %in% names(df))
    df$frecuencia_pago <- dplyr::coalesce(df$frecuencia_pago, "mensual")
  df
}

save_pasivos_liabilities <- function(df) {
  .s3_write(df, S3_KEYS$pasivos_liabilities)
  invisible(TRUE)
}

# ── pasivos_provisions ────────────────────────────────────────────────────────

load_pasivos_provisions <- function() {
  .normalize(.s3_read(S3_KEYS$pasivos_provisions), .schema_pasivos_provision)
}

save_pasivos_provisions <- function(df) {
  .s3_write(.normalize(df, .schema_pasivos_provision), S3_KEYS$pasivos_provisions)
  invisible(TRUE)
}

# ── pasivos_modifiers ─────────────────────────────────────────────────────────
# No list columns — standard normalize works.

load_pasivos_modifiers <- function() {
  .normalize(.s3_read(S3_KEYS$pasivos_modifiers), .schema_pasivos_modifier)
}

save_pasivos_modifiers <- function(df) {
  .s3_write(.normalize(df, .schema_pasivos_modifier), S3_KEYS$pasivos_modifiers)
  invisible(TRUE)
}

# ── pasivos_estimates ─────────────────────────────────────────────────────────

load_pasivos_estimates <- function() {
  .normalize(.s3_read(S3_KEYS$pasivos_estimates), .schema_pasivos_estimates)
}

save_pasivos_estimates <- function(df) {
  .s3_write(.normalize(df, .schema_pasivos_estimates), S3_KEYS$pasivos_estimates)
  invisible(TRUE)
}

# ── pasivos_card_expenses ─────────────────────────────────────────────────────

load_pasivos_card_expenses <- function() {
  .normalize(.s3_read(S3_KEYS$pasivos_card_expenses), .schema_pasivos_card_expense)
}

save_pasivos_card_expenses <- function(df) {
  .s3_write(.normalize(df, .schema_pasivos_card_expense), S3_KEYS$pasivos_card_expenses)
  invisible(TRUE)
}

# ── pasivos_audit ─────────────────────────────────────────────────────────────
# Append-only logically: load → bind_rows → save_pasivos_audit.
# The loader never deletes rows.

load_pasivos_audit <- function() {
  .normalize(.s3_read(S3_KEYS$pasivos_audit), .schema_pasivos_audit)
}

save_pasivos_audit <- function(df) {
  .s3_write(.normalize(df, .schema_pasivos_audit), S3_KEYS$pasivos_audit)
  invisible(TRUE)
}
