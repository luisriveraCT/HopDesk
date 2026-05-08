# =============================================================================
# R/pasivos_audit.R
# Audit logger for the Pasivos module.
# Append-only: load → bind_rows(new_row) → save_pasivos_audit.
# =============================================================================

PASIVOS_ACTION_TYPES <- c(
  "liability.created", "liability.edited", "liability.deleted", "liability.restored",
  "provision.generated", "provision.edited", "provision.deleted",
  "provision.converted_to_item", "provision.item_confirmed",
  "provision.item_reversed", "provision.revived",
  "modifier.added", "modifier.toggled", "modifier.frozen", "modifier.removed",
  "estimate.changed", "estimate.frozen", "estimate.unfrozen",
  "bulk.aplicar_pagos_futuros", "bulk.recalculate_schedule",
  "capability.denied",
  "card_expense.logged", "card_expense.deleted"
)

# Write one audit row and return its id.
# before / after: any R object; serialized to JSON automatically.
pasivos_log_audit <- function(action_type,
                               user,
                               empresa        = NA_character_,
                               target_kind,
                               target_id,
                               before         = NULL,
                               after          = NULL,
                               session_id     = NA_character_,
                               notes          = NA_character_) {
  if (!action_type %in% PASIVOS_ACTION_TYPES) {
    warning("[pasivos] unknown action_type: ", action_type)
  }

  json_opts <- list(auto_unbox = TRUE, null = "null", na = "string")

  serialize_payload <- function(x) {
    if (is.null(x)) return(NA_character_)
    tryCatch(
      as.character(do.call(jsonlite::toJSON, c(list(x), json_opts))),
      error = function(e) NA_character_
    )
  }

  row_id <- uuid::UUIDgenerate()

  new_row <- tibble::tibble(
    id             = row_id,
    ts             = Sys.time(),
    user           = as.character(user %||% NA_character_),
    empresa        = as.character(empresa %||% NA_character_),
    action_type    = as.character(action_type),
    target_kind    = as.character(target_kind),
    target_id      = as.character(target_id %||% NA_character_),
    payload_before = serialize_payload(before),
    payload_after  = serialize_payload(after),
    session_id     = as.character(session_id %||% NA_character_),
    notes          = as.character(notes %||% NA_character_)
  )

  existing <- tryCatch(load_pasivos_audit(), error = function(e) .schema_pasivos_audit())
  save_pasivos_audit(dplyr::bind_rows(existing, new_row))

  invisible(row_id)
}
