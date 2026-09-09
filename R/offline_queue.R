# =============================================================================
# R/offline_queue.R
# Stage 1 of the durable offline write queue (see OFFLINE_WRITE_QUEUE_DESIGN.md).
#
# Scope of THIS stage: when .s3_write() exhausts its upload attempt and is
# about to raise an error, durably stage the already-written payload to a
# local, per-client queue directory instead of just losing it -- so a later
# stage (background flush + replay, not implemented yet) has something
# durable to work with once S3 is reachable again. This stage does NOT
# retry, does NOT replay, and does NOT change any caller-visible behavior:
# .s3_write() still raises the exact same error it always has, on every
# path, staged or not -- staging failure itself is swallowed so it can never
# be the thing that changes .s3_write()'s outcome.
#
# Eligibility: only tables registered via register_synced() (R/sync_bus.R)
# are staged -- a future replay stage needs the per-table version stamp that
# mechanism already maintains to detect "did someone else write this while
# I was offline." Everything else is untouched by this file and keeps
# today's synchronous-write-or-raise behavior.
#
# Tenant isolation: every entry is filed under the client_id actually
# resolved for the failed write (read back out of the already-resolved
# full S3 key, never Sys.getenv("CLIENT_ID")), in its own subdirectory with
# its own manifest -- no function in this file ever reads or writes two
# clients' entries in the same operation, and staging refuses outright if
# the resolved S3 key doesn't start with that same client_id's own prefix.
# =============================================================================

# ── Local queue root ─────────────────────────────────────────────────────────

# Overridable for tests via option("hopdesk.queue_dir"); production default is
# a per-OS user-data directory (tools::R_user_dir), deliberately NOT the
# Windows-only %LOCALAPPDATA%\HopDesk path the single-instance file lock uses
# (that one is intentionally skipped on Linux/shinyapps.io -- this queue must
# work there too).
.queue_root_dir <- function() {
  override <- getOption("hopdesk.queue_dir")
  if (!is.null(override) && nzchar(override)) return(override)
  tools::R_user_dir("hopdesk", "data")
}

# Same normalization .s3_key() applies to client_id, so the on-disk partition
# name always matches the S3 prefix the entry would eventually replay to.
.queue_slug <- function(client_id) {
  tolower(trimws(client_id))
}

# Refuses to ever turn an unexpected client_id shape into a filesystem path
# (empty, whitespace-only, path separators, traversal) rather than trying to
# sanitize it -- fail closed, don't queue, let the caller's original error
# surface exactly as it does today.
.queue_valid_client_id <- function(client_id) {
  if (!is.character(client_id) || length(client_id) != 1L || is.na(client_id)) return(FALSE)
  slug <- .queue_slug(client_id)
  if (!nzchar(slug)) return(FALSE)
  if (!grepl("^[a-z0-9._-]+$", slug)) return(FALSE)
  # "." and ".." are otherwise-valid-looking under the charset above but
  # resolve to the queue root itself / its parent via file.path() -- reject
  # them explicitly rather than relying on the charset alone.
  if (slug %in% c(".", "..")) return(FALSE)
  TRUE
}

.queue_client_dir    <- function(client_id) file.path(.queue_root_dir(), "clients", .queue_slug(client_id))
.queue_entries_dir   <- function(client_id) file.path(.queue_client_dir(client_id), "entries")
.queue_manifest_path <- function(client_id) file.path(.queue_client_dir(client_id), "manifest.rds")
.queue_pending_index_path <- function() file.path(.queue_root_dir(), "pending_client_ids.rds")

.queue_manifest_schema <- function() {
  data.frame(
    seq                   = integer(0),
    s3_key                = character(0),
    table_name             = character(0),
    client_id              = character(0),
    expected_sync_version   = integer(0),
    acting_user             = character(0),
    acting_home_client_id   = character(0),
    status                  = character(0),
    staged_at               = as.POSIXct(character(0)),
    synced_at               = as.POSIXct(character(0)),
    stringsAsFactors        = FALSE
  )
}

.queue_read_manifest <- function(client_id) {
  path <- .queue_manifest_path(client_id)
  if (!file.exists(path)) return(.queue_manifest_schema())
  df <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(df) || !is.data.frame(df)) return(.queue_manifest_schema())
  df
}

# Atomic write: saveRDS() to a tempfile in the SAME directory as the real
# manifest (guarantees same filesystem, so file.rename() is atomic), then
# rename over the real path. A crash mid-write leaves either the untouched
# old manifest (rename never happened) or the fully-written new one (rename
# happened) -- never a half-written file, because the half-written bytes
# only ever exist under the tempfile's own name.
.queue_write_manifest <- function(client_id, df) {
  dir <- .queue_client_dir(client_id)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  final <- .queue_manifest_path(client_id)
  tmp   <- tempfile(tmpdir = dir, fileext = ".rds.tmp")
  saveRDS(df, tmp, compress = TRUE)
  ok <- file.rename(tmp, final)
  if (!ok) { unlink(tmp); stop("[offline_queue] manifest rename failed for client_id=", client_id) }
  invisible(TRUE)
}

.queue_next_seq <- function(client_id) {
  df <- .queue_read_manifest(client_id)
  if (nrow(df) == 0L) return(1L)
  as.integer(max(df$seq, na.rm = TRUE) + 1L)
}

# ── Per-client mutex around seq-generation + manifest-append ────────────────
#
# Found in review (2026-09-09): .queue_next_seq() reads the manifest and
# computes max(seq)+1 with no lock held across that read and the eventual
# manifest write -- two overlapping .queue_stage_entry() calls for the SAME
# client (two concurrent failed S3 writes in the same R process, which
# serves multiple Shiny sessions at once) can compute the identical seq
# number. The second one's file.copy(overwrite=FALSE) then refuses (this
# was already provably true before this fix -- confirmed directly in
# tests/test_offline_queue_review_findings.R Part R1), so nothing gets
# corrupted, but that second write silently loses its durability guarantee,
# falling back to today's pre-queue behavior for that one write.
#
# Fix: an exclusive-directory-creation mutex, not a new package dependency.
# dir.create() only succeeds for the FIRST caller of a given path -- backed
# by the OS's own atomic mkdir(), identical guarantee on Windows and POSIX
# -- matching this design's own "no new dependency" constraint (RSQLite/
# filelock were both deliberately kept out; this needs neither). A bounded
# spin-with-backoff handles the normal (sub-millisecond) hold time, mirroring
# the shape of s3_save_with_retry() elsewhere in this codebase. A stale-lock
# reclaim (age > .QUEUE_LOCK_STALE_SECS) means a process that died mid-
# critical-section can never permanently wedge one client's queue for
# everyone else.
.QUEUE_LOCK_STALE_SECS <- 30

.queue_lock_dir <- function(client_id) file.path(.queue_client_dir(client_id), ".lock")

.queue_with_client_lock <- function(client_id, fn, max_wait = 5, poll = 0.05) {
  dir.create(.queue_client_dir(client_id), recursive = TRUE, showWarnings = FALSE)
  lock_dir <- .queue_lock_dir(client_id)
  deadline <- Sys.time() + max_wait
  repeat {
    if (dir.create(lock_dir, showWarnings = FALSE)) break
    age <- tryCatch(as.numeric(difftime(Sys.time(), file.info(lock_dir)$mtime, units = "secs")),
                    error = function(e) NA_real_)
    if (!is.na(age) && age > .QUEUE_LOCK_STALE_SECS) {
      unlink(lock_dir, recursive = TRUE, force = TRUE)
      next
    }
    if (Sys.time() >= deadline)
      stop("[offline_queue] could not acquire client lock for '", client_id, "' within ", max_wait, "s")
    Sys.sleep(poll)
  }
  on.exit(unlink(lock_dir, recursive = TRUE, force = TRUE), add = TRUE)
  fn()
}

# Same atomic tempfile-then-rename pattern as the manifest, applied to the
# one small top-level index of "which clients currently have pending work"
# (Stage 2's flush loop will use this to avoid scanning every client
# directory that has ever existed).
.queue_mark_client_pending <- function(client_id) {
  path <- .queue_pending_index_path()
  ids  <- if (file.exists(path)) tryCatch(readRDS(path), error = function(e) character(0)) else character(0)
  if (!is.character(ids)) ids <- character(0)
  slug <- .queue_slug(client_id)
  if (slug %in% ids) return(invisible(TRUE))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(tmpdir = dirname(path), fileext = ".rds.tmp")
  saveRDS(sort(unique(c(ids, slug))), tmp, compress = TRUE)
  ok <- file.rename(tmp, path)
  if (!ok) unlink(tmp)
  invisible(TRUE)
}

# Reverse lookup: register_synced() ties a name ("moves_db") to a bare S3_KEYS
# value ("invoice_moves.rds"); .s3_write() only ever sees the bare key, so
# this is how staging decides "is this one of the tables Phase 1 covers."
.queue_table_name_for_key <- function(key) {
  for (nm in names(.SYNC_REGISTRY)) {
    if (identical(.SYNC_REGISTRY[[nm]]$s3_key, key)) return(nm)
  }
  NULL
}

# The main entry point, called from .s3_write() only after every upload
# attempt (including the put_object fallback) has already failed.
#
#   payload_path: the tempfile .s3_write() already saveRDS()'d -- copied
#     (never moved) into the queue, since .s3_write()'s own on.exit() still
#     owns and will unlink() the original right after this returns.
#   s3_key: the FULL, already-prefixed key (e.g. "networks/invoice_moves.rds").
#   key: the bare S3_KEYS value (e.g. "invoice_moves.rds"), used only to look
#     up which register_synced() name this table was registered under.
#   client_id: the resolved (already-lowercased) client_id used to build
#     s3_key -- the caller must pass the value actually used, not an
#     independently-recomputed one, so this can never drift from what the
#     write itself was attempting.
#
# Returns TRUE if an entry was durably staged, FALSE for every other case
# (ineligible table, invalid client_id, tenant-prefix mismatch, or any
# unexpected error) -- in every FALSE case the caller's original S3 error
# still propagates completely unchanged.
.queue_stage_entry <- function(payload_path, s3_key, key, client_id,
                                acting_user = NA_character_,
                                acting_home_client_id = NA_character_) {
  tryCatch({
    if (!.queue_valid_client_id(client_id)) return(FALSE)
    slug <- .queue_slug(client_id)

    # Defense in depth: refuse to stage anything whose resolved S3 key isn't
    # actually under this client's own prefix. Should be unreachable given
    # how .s3_key() builds s3_key in the first place, but this is the last
    # check before anything touches local disk under a client-named path.
    if (!startsWith(s3_key, paste0(slug, "/"))) return(FALSE)

    table_name <- .queue_table_name_for_key(key)
    if (is.null(table_name)) return(FALSE)  # Phase 1: sync_bus-registered tables only

    if (!file.exists(payload_path)) return(FALSE)

    expected_sync_version <- tryCatch(
      as.integer(.GlobalEnv$.sync_versions[[table_name]] %||% NA_integer_),
      error = function(e) NA_integer_
    )

    # seq-generation through the manifest write must be mutually exclusive
    # per client -- see .queue_with_client_lock()'s own header comment for
    # why (a genuine race here used to let two overlapping stagings compute
    # the same seq number).
    .queue_with_client_lock(slug, function() {
      seq         <- .queue_next_seq(client_id)
      entries_dir <- .queue_entries_dir(client_id)
      dir.create(entries_dir, recursive = TRUE, showWarnings = FALSE)
      dest <- file.path(entries_dir, sprintf("%012d.rds", seq))
      if (!file.copy(payload_path, dest, overwrite = FALSE)) return(FALSE)

      row <- data.frame(
        seq = seq, s3_key = s3_key, table_name = table_name, client_id = slug,
        expected_sync_version = expected_sync_version,
        acting_user = as.character(acting_user %||% NA_character_),
        acting_home_client_id = as.character(acting_home_client_id %||% NA_character_),
        status = "pending", staged_at = Sys.time(), synced_at = as.POSIXct(NA),
        stringsAsFactors = FALSE
      )
      df <- rbind(.queue_read_manifest(client_id), row)
      .queue_write_manifest(client_id, df)
      .queue_mark_client_pending(client_id)
      TRUE
    })
  }, error = function(e) {
    message("[offline_queue] staging failed, write will surface its normal error instead: ", e$message)
    FALSE
  })
}
