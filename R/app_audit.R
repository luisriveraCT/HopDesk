# =============================================================================
# R/app_audit.R — Application audit log with chunk rotation
#
# Layout per client:
#   <client_id>/app_audit.rds          ← current live chunk
#   <client_id>/app_audit_001.rds      ← archived (oldest)
#   <client_id>/app_audit_002.rds, …
#
# Rotation triggers when the live chunk reaches .MAX_AUDIT_ROWS (10,000).
# The live file is renamed to the next sequential index; a fresh file starts.
#
# Dual-write: when log_action() is called with client_id != CLIENT_ID env var,
# it also appends to the deployment's own log — capturing cross-client staff
# actions in both the target client's log and the home deployment's log.
# =============================================================================

.APP_AUDIT_SCHEMA <- function() tibble::tibble(
  id          = character(),
  ts          = as.POSIXct(character()),
  user        = character(),
  module      = character(),
  action      = character(),
  description = character(),
  target_id   = character(),
  s3_key      = character(),
  client_id   = character(),
  metadata    = character()
)

.MAX_AUDIT_ROWS <- 10000L

.audit_current_key <- function(cid) {
  paste0(tolower(trimws(cid)), "/app_audit.rds")
}

.audit_read_chunk <- function(key) {
  tryCatch(
    suppressMessages(suppressWarnings(
      aws.s3::s3readRDS(object = key, bucket = .s3_bucket())
    )),
    error = function(e) NULL
  )
}

.audit_write_chunk <- function(df, key) {
  aws.s3::s3saveRDS(df, object = key, bucket = .s3_bucket())
}

# ── Rotation ──────────────────────────────────────────────────────────────────
rotate_log_if_needed <- function(cid, current_df) {
  if (is.null(current_df) || nrow(current_df) < .MAX_AUDIT_ROWS)
    return(invisible(FALSE))

  prefix <- paste0(cid, "/app_audit_")
  existing_keys <- tryCatch({
    objs <- aws.s3::get_bucket(bucket = .s3_bucket(), prefix = prefix, max = 200L)
    vapply(objs, function(o) o[["Key"]], character(1))
  }, error = function(e) character(0))

  existing_nums <- suppressWarnings(as.integer(
    sub(paste0("^", cid, "/app_audit_(\\d+)\\.rds$"), "\\1", existing_keys)
  ))
  existing_nums <- existing_nums[!is.na(existing_nums)]
  next_idx      <- if (length(existing_nums)) max(existing_nums) + 1L else 1L
  archive_key   <- sprintf("%s/app_audit_%03d.rds", cid, next_idx)

  tryCatch({
    .audit_write_chunk(current_df, archive_key)
    .audit_write_chunk(.APP_AUDIT_SCHEMA(), .audit_current_key(cid))
    message(sprintf("[AUDIT] Rotated %d rows to %s", nrow(current_df), archive_key))
    invisible(TRUE)
  }, error = function(e) {
    message("[AUDIT] Rotation failed: ", e$message)
    invisible(FALSE)
  })
}

# ── Read ──────────────────────────────────────────────────────────────────────

# Read ALL audit chunks for a client (current + archived), newest first.
# since: optional POSIXct or character cutoff — filters ts >= since.
# until: optional POSIXct or character cutoff — filters ts <= until.
read_audit_log <- function(client_id = NULL, since = NULL, until = NULL) {
  cid    <- tolower(trimws(client_id %||% Sys.getenv("CLIENT_ID")))
  prefix <- paste0(cid, "/app_audit")

  all_keys <- tryCatch({
    objs <- aws.s3::get_bucket(bucket = .s3_bucket(), prefix = prefix, max = 200L)
    vapply(objs, function(o) o[["Key"]], character(1))
  }, error = function(e) character(0))

  if (!length(all_keys)) return(.APP_AUDIT_SCHEMA())

  chunks <- Filter(Negate(is.null), lapply(all_keys, .audit_read_chunk))
  if (!length(chunks)) return(.APP_AUDIT_SCHEMA())

  result <- tryCatch(
    dplyr::bind_rows(lapply(chunks, function(ch) {
      for (col in names(.APP_AUDIT_SCHEMA()))
        if (!col %in% names(ch)) ch[[col]] <- NA
      ch[, names(.APP_AUDIT_SCHEMA()), drop = FALSE]
    })),
    error = function(e) .APP_AUDIT_SCHEMA()
  )

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

# ── Scoped read — the only path the audit log viewer UI may use ─────────────
# Enforces the three visibility rules from ARCHITECTURE.md §6 / Stage 4 Part C.
# Throws loudly (stop()) on any attempt outside those rules — a UI that reaches
# this without the right permission is a bug worth surfacing, not a case to
# silently return empty for.
read_audit_log_scoped <- function(requested_client_id,
                                  viewer_is_staff,
                                  viewer_can_view_client_logs,
                                  viewer_can_view_staff_log,
                                  viewer_home_client_id,
                                  since = NULL,
                                  until = NULL) {
  requested_cid <- tolower(trimws(requested_client_id %||% ""))
  home_cid      <- tolower(trimws(viewer_home_client_id %||% ""))

  if (identical(requested_cid, "hd-admin")) {
    if (!isTRUE(viewer_can_view_staff_log))
      stop("read_audit_log_scoped: viewer lacks can_view_staff_audit_log — cannot view the hd-admin staff log")
  } else if (!isTRUE(viewer_is_staff)) {
    if (!identical(requested_cid, home_cid))
      stop("read_audit_log_scoped: a client session may only request its own client's log")
  } else if (!isTRUE(viewer_can_view_client_logs)) {
    stop("read_audit_log_scoped: viewer lacks can_view_client_audit_logs")
  }

  read_audit_log(client_id = requested_cid, since = since, until = until)
}

# Backward-compatible wrapper used by Actividad tab.
load_app_audit <- function(client_id = NULL) {
  read_audit_log(client_id)
}

# ── Write ─────────────────────────────────────────────────────────────────────
save_app_audit <- function(log_df, client_id = NULL) {
  cid <- tolower(trimws(client_id %||% Sys.getenv("CLIENT_ID")))
  .audit_write_chunk(log_df, .audit_current_key(cid))
}

# ── log_action() ─────────────────────────────────────────────────────────────
# Fire-and-forget — never throws. Returns the new row invisibly.
#
# client_id             — target folder this entry is filed under, i.e. where
#                         the action actually happened (default: CLIENT_ID env
#                         var — legacy fallback, see below).
# viewer_home_client_id — the ACTING USER's own true home client (their login-
#                         time client_id; always "hd-admin" for staff). Default
#                         falls back to Sys.getenv("CLIENT_ID") for any call
#                         site that hasn't been updated to pass it explicitly.
# s3_key                — the S3 object that was created/modified by this action
#
# Dual-write: fires when client_id differs from the ACTOR's own home, i.e. a
# staff member acting inside a jumped client — never for a native client
# user's own actions, and never just because this is a single shared
# deployment. This app runs as ONE shared deployment (CLIENT_ID env var is
# always "hd-admin", ARCHITECTURE.md §3) — comparing client_id against that
# fixed env var instead of the actual actor's home meant EVERY native client
# user's own action (not just staff jump actions) was dual-written into
# hd-admin's own log, exactly the "CLIENT_ID as an is-this-session-staff
# stand-in" anti-pattern §3 warns against elsewhere. Fixed 2026-07-16 while
# testing Stage 4 (found via a client dev user's invoice-move entries leaking
# into Bitácora Global / Hopdesk's own log).
log_action <- function(user,
                       module,
                       action,
                       description,
                       target_id             = NA_character_,
                       s3_key                = NA_character_,
                       metadata              = NULL,
                       client_id             = NULL,
                       viewer_home_client_id = NULL) {
  cid      <- tolower(trimws(client_id %||% Sys.getenv("CLIENT_ID")))
  home_cid <- tolower(trimws(viewer_home_client_id %||% Sys.getenv("CLIENT_ID")))

  # When staff acts in a client context, stamp home_client in metadata so the
  # audit record is clearly marked as an outsider write, not a client-user write.
  if (nzchar(home_cid) && home_cid != cid) {
    metadata <- c(as.list(metadata %||% list()), list(home_client = home_cid))
  }

  new_row <- tibble::tibble(
    id          = uuid::UUIDgenerate(),
    ts          = Sys.time(),
    user        = as.character(user        %||% "unknown"),
    module      = as.character(module),
    action      = as.character(action),
    description = as.character(description),
    target_id   = as.character(target_id   %||% NA_character_),
    s3_key      = as.character(s3_key      %||% NA_character_),
    client_id   = cid,
    metadata    = as.character(if (!is.null(metadata))
                    tryCatch(jsonlite::toJSON(metadata, auto_unbox = TRUE),
                             error = function(e) "{}")
                  else "{}")
  )

  .append_to <- function(write_cid) {
    tryCatch({
      current <- .audit_read_chunk(.audit_current_key(write_cid))
      if (is.null(current) || !is.data.frame(current) || !nrow(current))
        current <- .APP_AUDIT_SCHEMA()
      rotate_log_if_needed(write_cid, current)
      # Re-read after potential rotation (may be a fresh empty schema)
      current <- tryCatch({
        ch <- .audit_read_chunk(.audit_current_key(write_cid))
        if (is.null(ch) || !nrow(ch)) .APP_AUDIT_SCHEMA() else ch
      }, error = function(e) .APP_AUDIT_SCHEMA())
      .audit_write_chunk(dplyr::bind_rows(current, new_row),
                         .audit_current_key(write_cid))
      invisible(TRUE)
    }, error = function(e) {
      message("[APP_AUDIT] write failed (", write_cid, "): ", e$message)
      invisible(FALSE)
    })
  }

  .append_to(cid)

  # Dual-write when a staff user acts in a different client's context
  if (nzchar(home_cid) && home_cid != cid)
    .append_to(home_cid)

  invisible(new_row)
}
