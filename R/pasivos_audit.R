# =============================================================================
# R/pasivos_audit.R
# Audit logger for the Pasivos module.
# Append-only: load → bind_rows(new_row) → save_pasivos_audit.
# =============================================================================

PASIVOS_ACTION_TYPES <- c(
  "liability.created", "liability.edited", "liability.deleted", "liability.restored",
  "provision.generated", "provision.edited", "provision.deleted", "provision.cancelled",
  "provision.converted_to_item", "provision.item_confirmed",
  "provision.item_reversed", "provision.revived",
  "modifier.added", "modifier.toggled", "modifier.frozen", "modifier.removed",
  "estimate.changed", "estimate.frozen", "estimate.unfrozen",
  "bulk.aplicar_pagos_futuros", "bulk.recalculate_schedule", "bulk.purge_closed_provisions",
  "capability.denied",
  "card_expense.logged", "card_expense.deleted"
)

# Fresh read of pasivos_audit for an append. Drops the key from the read caches
# (so the append rebases on what S3 holds now, not on what this process read
# earlier), and refuses an empty result when the object exists in S3:
# .s3_read_with() returns NULL both for "no such object" and for a failed read,
# so an empty table is only trusted when there really is no file yet.
.pasivos_audit_leer_para_agregar <- function(client_id) {
  full <- .s3_key(S3_KEYS$pasivos_audit, client_id = client_id)
  for (env in list(.s3_preload_cache, .s3_missing_cache))
    suppressWarnings(rm(list = intersect(full, ls(env)), envir = env))
  existing <- load_pasivos_audit(client_id = client_id)
  if (!nrow(existing) && .pasivos_audit_existe(full))
    stop("pasivos_audit exists in S3 but read back empty; refusing to overwrite it", call. = FALSE)
  existing
}

# Fails closed: if S3 cannot answer, assume the object exists. Only a 404 is
# "does not exist" (s3_objeto_existe(), R/persistence.R): a 403 used to read
# as FALSE here, and the audit was rewritten from empty.
.pasivos_audit_existe <- function(full_key) !identical(s3_objeto_existe(full_key, .s3_bucket()), FALSE)

# Write one audit row and return its id.
# before / after: any R object; serialized to JSON automatically.
#
# Also dual-writes into the app-wide audit log via log_action (R/app_audit.R)
# -- found 2026-07-30:
# this function's own pasivos_audit.rds table was never read by anything.
# R/audit_log_viewer_module.R (Gestión de Usuarios > Actividad, the ONLY
# real activity screen in the app) exclusively reads app_audit.rds via
# read_audit_log_scoped(); it has no code path that touches pasivos_audit
# at all. So every pasivos action "logged" here -- provision conversions,
# reverts, deletes, generation, capability denials -- was being written to
# a store nobody's Activity screen could ever show. This was flagged as
# critical specifically because provision conversion is exactly the kind of
# "who approved what, when, how" moment that needs to be auditable, and it
# wasn't, silently, the whole time. Fixed at this one choke point instead of
# the ~20 call sites individually: every pasivos_log_audit() call now also
# produces a real, visible entry in that log, with before/after passed
# through as structured metadata (not flattened into a description string),
# so a viewer can see exactly what changed, not just that something did.
pasivos_log_audit <- function(action_type,
                               user,
                               empresa        = NA_character_,
                               target_kind,
                               target_id,
                               before         = NULL,
                               after          = NULL,
                               session_id     = NA_character_,
                               notes          = NA_character_,
                               client_id      = NULL,
                               viewer_home_client_id = NULL) {
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

  # Append on a fresh, strict read. This used to fall back to an EMPTY table
  # when the read failed and then save that plus the new row -- overwriting the
  # whole pasivos audit with one row (found 2026-10-31, CM05/CM06). A read that
  # fails, or comes back empty for an object that exists, now skips this table
  # (and says so); the app_audit dual-write below still records the action.
  existing <- tryCatch(.pasivos_audit_leer_para_agregar(client_id), error = function(e) e)
  if (inherits(existing, "error")) {
    msg <- sprintf("[pasivos] pasivos_audit not written (%s): %s", action_type, conditionMessage(existing))
    message(msg)
    warning(msg, call. = FALSE)
  } else {
    save_pasivos_audit(dplyr::bind_rows(existing, new_row), client_id = client_id)
  }

  notes_txt   <- if (!is.na(notes) && nzchar(notes)) notes else NULL
  description <- if (!is.null(notes_txt)) sprintf("%s: %s", action_type, notes_txt) else action_type
  tryCatch(
    log_action(
      user        = user,
      module      = "pasivos",
      action      = action_type,
      description = description,
      target_id   = target_id,
      metadata    = list(target_kind = target_kind, empresa = empresa,
                         before = before, after = after),
      client_id             = client_id,
      viewer_home_client_id = viewer_home_client_id
    ),
    error = function(e) warning("[pasivos] log_action dual-write failed: ", e$message)
  )

  invisible(row_id)
}
