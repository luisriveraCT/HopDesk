# =============================================================================
# R/sync_bus.R
# Registry-based cross-session sync for shared mutable state.
#
# Every reactiveVal that must propagate across user sessions is registered once.
# A single polling observer per session checks all registered keys every N s;
# on version bump it re-reads from S3 and pushes to the local reactiveVal.
#
# Writers MUST call bump_sync_version(name) after every save_*() that mutates
# a registered key, otherwise other sessions stay stale.
#
# Cross-process compatibility: bump_sync_version() writes a tiny version stamp
# to S3 ("sync_versions.rds") so workers in OTHER R processes (e.g. multi-worker
# Shiny Server) can detect changes too.  The poller merges the S3 stamp with the
# in-memory counter before doing the per-key diff.
#
# Usage:
#   register_synced(name, s3_key, loader)   # call before setup_sync_bus()
#   bump_sync_version(name)                 # call after save_*()
#   setup_sync_bus(session, shared)         # call once per session from server()
# =============================================================================

.SYNC_REGISTRY <- list()

register_synced <- function(name, s3_key, loader, per_session_loader = NULL) {
  .SYNC_REGISTRY[[name]] <<- list(s3_key = s3_key, loader = loader,
                                   per_session_loader = per_session_loader)
  if (is.null(.GlobalEnv$.sync_versions)) .GlobalEnv$.sync_versions <- list()
  if (is.null(.GlobalEnv$.sync_versions[[name]])) .GlobalEnv$.sync_versions[[name]] <- 0L
  invisible(NULL)
}

bump_sync_version <- function(name, client_id = NULL) {
  if (!name %in% names(.SYNC_REGISTRY)) {
    warning("[sync_bus] bump_sync_version called for unregistered key: ", name)
    return(invisible(NULL))
  }
  .GlobalEnv$.sync_versions[[name]] <-
    (.GlobalEnv$.sync_versions[[name]] %||% 0L) + 1L

  # Write a cross-process stamp to S3 so other R workers detect this bump.
  # Suppress the verbose .s3_write log and swallow errors so a transient S3
  # failure never breaks the save that triggered this bump.
  tryCatch(
    suppressMessages(.s3_write(.GlobalEnv$.sync_versions, S3_KEYS$sync_versions, client_id = client_id)),
    error = function(e) message("[sync_bus] version stamp write failed: ", e$message)
  )

  invisible(NULL)
}

# active_client_rv: optional reactive (function) that returns the current session
# client context slug when a staff member has jumped to another client.
# When non-NULL and returning a non-empty string different from CLIENT_ID, the
# per-key loaders are called with client_id = active_cid so the reload fetches
# data from the correct prefix rather than the env-var default.
setup_sync_bus <- function(session, shared, poll_ms = 8000,
                           active_client_rv = NULL) {
  if (!exists(".sync_versions", envir = .GlobalEnv)) .GlobalEnv$.sync_versions <- list()
  local_versions <- as.list(.GlobalEnv$.sync_versions)

  observe({
    invalidateLater(poll_ms, session)

    # Resolve current client context for this session tick.
    active_cid <- tryCatch(
      if (is.function(active_client_rv)) active_client_rv() else NULL,
      error = function(e) NULL
    )
    env_cid <- tolower(trimws(Sys.getenv("CLIENT_ID")))
    in_jump_context <- !is.null(active_cid) && nzchar(active_cid) &&
                       tolower(active_cid) != env_cid

    # ── Cross-process merge ─────────────────────────────────────────────────
    # The missing-key cache in .s3_read() permanently marks absent keys so
    # subsequent reads return NULL without hitting S3.  sync_versions.rds is
    # created lazily on the first bump, so we must clear that gate before
    # every poll to allow discovery once the file appears in S3.
    sv_full_key <- tryCatch(.s3_key(S3_KEYS$sync_versions), error = function(e) NULL)
    if (!is.null(sv_full_key))
      suppressWarnings(
        rm(list = intersect(sv_full_key, ls(.s3_missing_cache)),
           envir  = .s3_missing_cache)
      )

    s3_versions <- tryCatch(
      suppressWarnings(.s3_read(S3_KEYS$sync_versions)),
      error = function(e) NULL
    )
    if (!is.null(s3_versions) && is.list(s3_versions)) {
      for (nm in names(s3_versions)) {
        s3_ver  <- s3_versions[[nm]]  %||% 0L
        mem_ver <- .GlobalEnv$.sync_versions[[nm]] %||% 0L
        if (s3_ver > mem_ver)
          .GlobalEnv$.sync_versions[[nm]] <- s3_ver
      }
    }

    # ── Per-key reload ──────────────────────────────────────────────────────
    for (name in names(.SYNC_REGISTRY)) {
      entry      <- .SYNC_REGISTRY[[name]]
      global_ver <- .GlobalEnv$.sync_versions[[name]] %||% 0L
      local_ver  <- local_versions[[name]] %||% 0L
      if (global_ver == local_ver) next

      # When in a jump context, pass active_cid so the reload fetches data
      # from the correct prefix (not the env-var CLIENT_ID prefix).
      new_value <- tryCatch(
        if (in_jump_context)
          entry$loader(client_id = active_cid)
        else
          entry$loader(),
        error = function(e) NULL
      )
      # If global loader returned NULL and a per-session loader is registered,
      # try it with the shared reactive list (has access to current_user() etc.)
      if (is.null(new_value) && is.function(entry$per_session_loader)) {
        new_value <- tryCatch(entry$per_session_loader(shared), error = function(e) NULL)
      }

      if (!is.null(new_value)) {
        rv <- shared[[name]]
        if (is.function(rv)) rv(new_value)
      }
      # Always advance local_ver even on load failure to avoid hammering S3
      # every tick when a key is persistently unreadable.
      local_versions[[name]] <<- global_ver
    }
  })

  invisible(NULL)
}
