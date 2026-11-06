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
# One stored column as the schema's class. POSIXct goes through
# as.POSIXct(): methods::as() with its two-class target (POSIXct, POSIXt)
# fails, and the tryCatch below left every created_at / updated_at as NA --
# which each save of the session's copy then wrote back to S3 for every
# liability (found 2026-11-06, T22).
.pasivos_como_columna <- function(x, destino) {
  if (inherits(destino, "POSIXct")) {
    if (inherits(x, "POSIXct")) return(x)
    if (is.logical(x)) return(as.POSIXct(rep(NA_real_, length(x))))
    return(as.POSIXct(x))
  }
  methods::as(x, class(destino))
}

.pasivos_load_with_list_cols <- function(key, schema_fn, list_col_names, client_id = NULL) {
  obj <- tryCatch(.s3_read_with(key, client_id = client_id), error = function(e) NULL)
  if (is.null(obj) || !is.data.frame(obj) || !nrow(obj)) return(schema_fn())

  schema <- schema_fn()
  for (col in setdiff(names(schema), list_col_names)) {
    if (!col %in% names(obj)) {
      obj[[col]] <- schema[[col]][NA_integer_]
    } else {
      obj[[col]] <- tryCatch(
        .pasivos_como_columna(obj[[col]], schema[[col]]),
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

load_pasivos_liabilities <- function(client_id = NULL) {
  df <- .pasivos_load_with_list_cols(
    S3_KEYS$pasivos_liabilities,
    .schema_pasivos_liability,
    .PASIVOS_LIABILITY_LIST_COLS,
    client_id = client_id
  )
  # Backward-compat: pre-frecuencia_pago records load as NA → default to mensual
  if ("frecuencia_pago" %in% names(df))
    df$frecuencia_pago <- dplyr::coalesce(df$frecuencia_pago, "mensual")
  df
}

save_pasivos_liabilities <- function(df, client_id = NULL) {
  .s3_write(df, S3_KEYS$pasivos_liabilities, client_id = client_id)
  invisible(TRUE)
}

# ── pasivos_provisions ────────────────────────────────────────────────────────

load_pasivos_provisions <- function(client_id = NULL) {
  .normalize(.s3_read_with(S3_KEYS$pasivos_provisions, client_id = client_id), .schema_pasivos_provision)
}

# `base`: the copy `df` was built from (often the session's). With it only
# this action's provisions are applied on S3's table as it is now (CM09), so
# a provision another session created, converted or paid meanwhile is kept.
save_pasivos_provisions <- function(df, client_id = NULL, permitir_reduccion = FALSE, base = NULL) {
  if (!is.null(base) && !is.null(client_id) && nzchar(client_id)) {
    nuevo <- .normalize(df, .schema_pasivos_provision)
    base  <- .normalize(base, .schema_pasivos_provision)
    modificar_tabla(S3_KEYS$pasivos_provisions, client_id, .schema_pasivos_provision,
                    function(fresco) rebase_por_id(nuevo, base, fresco, "id"),
                    permitir_reduccion = permitir_reduccion)
    return(invisible(TRUE))
  }
  # The flag only when set: a normal save stays the exact call it always was.
  if (isTRUE(permitir_reduccion))
    .s3_write(.normalize(df, .schema_pasivos_provision), S3_KEYS$pasivos_provisions, client_id = client_id,
              permitir_reduccion = TRUE)
  else
    .s3_write(.normalize(df, .schema_pasivos_provision), S3_KEYS$pasivos_provisions, client_id = client_id)
  invisible(TRUE)
}

# A provision whose item was already confirmed in Bancos is paid. Calendario
# and undo-conversion must leave it alone: marking it "deleted" or
# "provisional" erased that it was paid while its Bancos row stayed, so the
# obligation could be paid twice (found 2026-10-31). Only Pasivos'
# "Eliminar definitivamente" removes a paid one, Bancos row included.
PROVISION_PAGADA <- c("item_confirmed", "closed")

provisiones_pagadas <- function(provs, prov_ids) {
  if (is.null(provs) || !nrow(provs) || !length(prov_ids)) return(character())
  hit <- !is.na(provs$id) & provs$id %in% prov_ids & provs$estado %in% PROVISION_PAGADA
  unique(provs$id[hit])
}

# Where each provision is, for other screens (Requisiciones): "provisionada",
# "en_pago" (converted to an item, not paid yet), "pagada" or "borrada"; NA
# for an id this client does not have. "pagada" comes ONLY from a live
# bancos_confirmados row carrying the provision_id -- never from `estado`,
# which has drifted from the payment before. Pure core + a fresh-read wrapper.
etapa_de_provision_en <- function(provs, conf, provision_id) {
  provision_id <- as.character(provision_id)
  vivas  <- confirmaciones_vigentes(conf)
  pagado <- if (!is.null(vivas) && nrow(vivas) && "provision_id" %in% names(vivas))
    unique(vivas$provision_id[!is.na(vivas$provision_id)]) else character()
  idx    <- match(provision_id, provs$id)
  estado <- provs$estado[idx]
  etapa  <- ifelse(estado %in% "deleted", "borrada",
            ifelse(provision_id %in% pagado, "pagada",
            ifelse(estado %in% c("converted", PROVISION_PAGADA), "en_pago",
            ifelse(estado %in% "provisional", "provisionada", NA_character_))))
  # A closed provision with no live payment was cancelled, not paid.
  etapa[estado %in% "closed" & !(provision_id %in% pagado)] <- "borrada"
  etapa[is.na(idx)] <- NA_character_
  unname(etapa)
}

etapa_de_provision <- function(client_id, provision_id) {
  if (is.null(client_id) || !nzchar(client_id)) stop("etapa_de_provision: sin client_id")
  etapa_de_provision_en(load_pasivos_provisions(client_id = client_id),
                        load_bancos_confirmados(client_id = client_id),
                        provision_id)
}

# ── pasivos_modifiers ─────────────────────────────────────────────────────────
# No list columns — standard normalize works.

load_pasivos_modifiers <- function(client_id = NULL) {
  .normalize(.s3_read_with(S3_KEYS$pasivos_modifiers, client_id = client_id), .schema_pasivos_modifier)
}

save_pasivos_modifiers <- function(df, client_id = NULL) {
  .s3_write(.normalize(df, .schema_pasivos_modifier), S3_KEYS$pasivos_modifiers, client_id = client_id)
  invisible(TRUE)
}

# ── pasivos_estimates ─────────────────────────────────────────────────────────

load_pasivos_estimates <- function(client_id = NULL) {
  .normalize(.s3_read_with(S3_KEYS$pasivos_estimates, client_id = client_id), .schema_pasivos_estimates)
}

save_pasivos_estimates <- function(df, client_id = NULL) {
  .s3_write(.normalize(df, .schema_pasivos_estimates), S3_KEYS$pasivos_estimates, client_id = client_id)
  invisible(TRUE)
}

# ── pasivos_card_expenses ─────────────────────────────────────────────────────

load_pasivos_card_expenses <- function(client_id = NULL) {
  .normalize(.s3_read_with(S3_KEYS$pasivos_card_expenses, client_id = client_id), .schema_pasivos_card_expense)
}

save_pasivos_card_expenses <- function(df, client_id = NULL) {
  .s3_write(.normalize(df, .schema_pasivos_card_expense), S3_KEYS$pasivos_card_expenses, client_id = client_id)
  invisible(TRUE)
}

# ── pasivos_audit ─────────────────────────────────────────────────────────────
# Append-only logically: load → bind_rows → save_pasivos_audit.
# The loader never deletes rows.

load_pasivos_audit <- function(client_id = NULL) {
  .normalize(.s3_read_with(S3_KEYS$pasivos_audit, client_id = client_id), .schema_pasivos_audit)
}

save_pasivos_audit <- function(df, client_id = NULL) {
  .s3_write(.normalize(df, .schema_pasivos_audit), S3_KEYS$pasivos_audit, client_id = client_id)
  invisible(TRUE)
}
