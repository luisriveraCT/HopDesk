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
    bucket                 = character(0),
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

# ── Stage 2 additions: purely additive, .queue_mark_client_pending() above is
# untouched from Stage 1 so tests/test_offline_queue_stage1.R and
# tests/test_offline_queue_review_findings.R keep working unmodified (each
# extracts real function bodies individually; changing an existing function's
# own dependencies would silently break whichever test file doesn't also
# extract the new callee -- confirmed the hard way while building this).
.queue_read_pending_index <- function() {
  path <- .queue_pending_index_path()
  ids  <- if (file.exists(path)) tryCatch(readRDS(path), error = function(e) character(0)) else character(0)
  if (!is.character(ids)) ids <- character(0)
  ids
}

.queue_write_pending_index <- function(ids) {
  path <- .queue_pending_index_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(tmpdir = dirname(path), fileext = ".rds.tmp")
  saveRDS(sort(unique(ids)), tmp, compress = TRUE)
  ok <- file.rename(tmp, path)
  if (!ok) unlink(tmp)
  invisible(TRUE)
}

# Called once a client's manifest has no "pending" rows left, so the flush
# loop stops visiting a client with nothing left to do.
.queue_unmark_client_pending <- function(client_id) {
  ids  <- .queue_read_pending_index()
  slug <- .queue_slug(client_id)
  if (!slug %in% ids) return(invisible(TRUE))
  .queue_write_pending_index(setdiff(ids, slug))
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
        seq = seq, s3_key = s3_key, bucket = .s3_bucket(),
        table_name = table_name, client_id = slug,
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

# =============================================================================
# Stage 2: background flush + replay (see OFFLINE_WRITE_QUEUE_DESIGN.md).
#
# IMPORTANT constraint this whole section is built around: R/global.R is
# unconditionally RE-SOURCED on every app.R launch, including repeated ones
# in the same persistent R session (tests/test_stage7_global_always_resourced.R
# enforces this and explicitly forbids the literal substring "later::later("
# from ever appearing in R/global.R's own text, precisely because a bare
# scheduled callback there would duplicate on every re-source). So the actual
# later::later() call lives here, in this file, behind its own idempotency
# guard (.queue_start_flush_loop()) -- global.R only ever calls that guarded
# starter, which is safe to call any number of times.
# =============================================================================

.QUEUE_FLUSH_INTERVAL_SECS <- 15

# Idempotent: the first call schedules the self-rescheduling tick chain and
# returns TRUE; every call after that (i.e. every later re-source of
# global.R within the same process) is a no-op that returns FALSE. This is
# what makes it safe for global.R to call this unconditionally on every
# re-source, instead of needing global.R itself to somehow run "only once."
.queue_start_flush_loop <- function() {
  if (isTRUE(.GlobalEnv$.queue_flush_loop_started)) return(invisible(FALSE))
  .GlobalEnv$.queue_flush_loop_started <- TRUE
  # delay=0, not the full interval -- this is the design's "eager scan on
  # boot" requirement: whatever was left pending from a previous process
  # gets picked up as soon as the event loop starts spinning, not after
  # waiting out one full 15s cycle first.
  later::later(.queue_flush_tick, delay = 0)
  invisible(TRUE)
}

# Fires on httpuv's event loop regardless of whether any browser session is
# open (same primitive .audit_queue() already uses, just driven by a
# heartbeat instead of a buffered-row event). Never lets a bad tick kill the
# chain, and always reschedules the next one regardless of outcome.
.queue_flush_tick <- function() {
  tryCatch(.queue_flush_all_pending(), error = function(e) {
    message("[offline_queue] flush tick failed, will retry next cycle: ", e$message)
  })
  later::later(.queue_flush_tick, delay = .QUEUE_FLUSH_INTERVAL_SECS)
}

.queue_flush_all_pending <- function() {
  for (client_id in .queue_read_pending_index()) {
    tryCatch(.queue_flush_client(client_id), error = function(e) {
      message(sprintf("[offline_queue] flush failed for client_id='%s', will retry next cycle: %s",
                      client_id, e$message))
    })
  }
  invisible(TRUE)
}

# Processes every "pending" row for ONE client, in seq order (the order the
# writes were originally attempted), under that client's own lock -- the
# same mutex .queue_stage_entry() uses, and for the same reason: a live
# write can fail and stage a brand new entry for this client while its
# backlog is mid-replay, and the manifest read-modify-write here must not
# race against that.
.queue_flush_client <- function(client_id) {
  .queue_with_client_lock(client_id, function() {
    mf <- .queue_read_manifest(client_id)
    pending_idx <- which(mf$status == "pending")
    if (!length(pending_idx)) {
      .queue_unmark_client_pending(client_id)
      return(invisible(TRUE))
    }
    pending_idx <- pending_idx[order(mf$seq[pending_idx])]

    changed <- FALSE
    for (i in pending_idx) {
      new_status <- tryCatch(.queue_replay_entry(mf[i, ], client_id),
                              error = function(e) "pending")
      if (!identical(new_status, "pending")) {
        mf$status[i]    <- new_status
        mf$synced_at[i] <- Sys.time()  # terminal-resolution time, for "blocked" too, not only "synced"
        changed <- TRUE
      }
    }
    if (changed) .queue_write_manifest(client_id, mf)
    if (!any(mf$status == "pending")) .queue_unmark_client_pending(client_id)
    invisible(TRUE)
  })
}

# Replays exactly one manifest row. Returns the row's new status:
#   "synced"  -- uploaded successfully (version matched or not -- see below)
#   "blocked" -- a permanent, non-retryable problem (tenant-prefix mismatch,
#                or the staged payload file itself is gone); never retried
#                again, unlike "pending"
#   "pending" -- upload attempt failed again (S3 presumably still down);
#                left completely unchanged, retried again next tick
#
# Deliberately does NOT call .s3_write() -- that would re-run ITS OWN
# failure path and stage a brand new duplicate queue entry for what is
# already a queued retry. The payload is already durably sitting on local
# disk from Stage 1, so this uploads that existing file directly.
.queue_conflict_backups_dir <- function(client_id) file.path(.queue_client_dir(client_id), "conflict_backups")
.queue_conflict_backup_path <- function(client_id, seq) file.path(.queue_conflict_backups_dir(client_id), sprintf("%012d.rds", seq))

.queue_replay_entry <- function(row, client_id) {
  slug <- .queue_slug(client_id)
  # %||% (R/global.R) substitutes on a length-1 NA as well as NULL, so this
  # correctly falls back when .queue_stage_entry() wasn't given these.
  acting_user <- row$acting_user %||% "system"
  acting_home <- row$acting_home_client_id %||% slug

  # Defense in depth, mirrored from .queue_stage_entry()'s own identical
  # check: should be unreachable given staging already verified this, but
  # this is the last line before anything leaves local disk for S3.
  if (!startsWith(row$s3_key, paste0(slug, "/"))) {
    tryCatch(
      log_action(user = acting_user, module = "offline_queue", action = "_queue_tenant_mismatch",
                 description = sprintf("Refused to replay seq=%s: s3_key '%s' does not match client_id '%s'",
                                       row$seq, row$s3_key, slug),
                 target_id = row$table_name, s3_key = row$s3_key,
                 client_id = slug, viewer_home_client_id = acting_home),
      error = function(e) NULL
    )
    return("blocked")
  }

  # Environment isolation (2026-09-25). The manifest records which bucket the
  # write was originally aimed at, because .s3_bucket() answers "which bucket am
  # I pointed at RIGHT NOW", not "which bucket was this write for" -- and those
  # differ whenever the process that stages an entry is not the process that
  # replays it. A write staged by a local dev run (antiguedad-rds-dev) would
  # otherwise be replayed into whatever bucket the next process happened to
  # target, silently pushing sandbox rows over real client data in production.
  # Fail closed on any mismatch, and on entries staged before this field
  # existed (bucket unknown = not provably safe), and never retry: a bucket
  # mismatch is a property of the entry, so retrying can only repeat it.
  entry_bucket <- tryCatch(as.character(row$bucket %||% NA_character_), error = function(e) NA_character_)
  if (!isTRUE(nzchar(entry_bucket)) || is.na(entry_bucket) ||
      !identical(entry_bucket, .s3_bucket())) {
    tryCatch(
      log_action(user = acting_user, module = "offline_queue", action = "_queue_bucket_mismatch",
                 description = sprintf(
                   "Refused to replay seq=%s for table '%s': staged for bucket '%s' but this process targets '%s'.",
                   row$seq, row$table_name,
                   if (is.na(entry_bucket) || !nzchar(entry_bucket)) "<unknown>" else entry_bucket,
                   .s3_bucket()),
                 target_id = row$table_name, s3_key = row$s3_key,
                 client_id = slug, viewer_home_client_id = acting_home),
      error = function(e) NULL
    )
    return("blocked")
  }

  dest <- file.path(.queue_entries_dir(client_id), sprintf("%012d.rds", row$seq))
  if (!file.exists(dest)) return("blocked")  # the staged payload itself is gone -- retrying forever can't fix that

  # Payload integrity: a local disk isn't immune to bit rot or a truncated
  # write, especially over however long an entry might sit pending during a
  # real outage. Confirm the staged bytes are still valid RDS BEFORE ever
  # uploading them -- put_object() itself would happily push corrupt bytes
  # into a real client's live table with no complaint at all.
  payload_ok <- tryCatch({ readRDS(dest); TRUE }, error = function(e) FALSE)
  if (!payload_ok) {
    tryCatch(
      log_action(user = acting_user, module = "offline_queue", action = "_queue_corrupt_payload",
                 description = sprintf("Refused to replay seq=%s for table '%s': staged payload at '%s' is not valid RDS (local corruption)",
                                       row$seq, row$table_name, dest),
                 target_id = row$table_name, s3_key = row$s3_key,
                 client_id = slug, viewer_home_client_id = acting_home),
      error = function(e) NULL
    )
    return("blocked")
  }

  # Conflict check happens BEFORE the upload, not after: read the CURRENT
  # version fresh from S3, scoped to this entry's own client_id -- never the
  # process-global in-memory .sync_versions counter, which is shared across
  # every client this one process happens to serve and would compare
  # against whichever client's bump happened to run most recently, not
  # necessarily this one. Doing this BEFORE uploading is what makes the
  # pre-overwrite backup below possible at all -- checking only after the
  # upload would mean reading back this replay's OWN just-written content,
  # not the victim's.
  current_version <- tryCatch({
    stamp <- .s3_read_with(S3_KEYS$sync_versions, client_id = slug)
    if (is.list(stamp)) suppressWarnings(as.integer(stamp[[row$table_name]] %||% NA_integer_)) else NA_integer_
  }, error = function(e) NA_integer_)

  expected <- suppressWarnings(as.integer(row$expected_sync_version))
  # A missing baseline (NA) means Stage 1 never had a version snapshot to
  # compare against in the first place -- there is genuinely nothing to
  # compare, so this stays silent rather than logging a "conflict" that
  # can't actually be confirmed one way or the other.
  is_conflict <- !is.na(expected) && !is.na(current_version) && current_version != expected

  # If this IS a conflict, back up whatever is CURRENTLY in S3 for this
  # table (the victim of the impending overwrite) before replaying, so the
  # audit trail isn't just two version numbers with no way to recover what
  # was actually lost. Best-effort: a failure to back up must never block
  # the replay itself, matching this design's "the queue must never make
  # things worse than not having it" posture everywhere else. Forces a real
  # S3 read (clears both caches for this key first) rather than risking a
  # stale preload-cached copy of the very thing about to be overwritten.
  backup_path <- NA_character_
  if (is_conflict) {
    bare_key <- tryCatch(.SYNC_REGISTRY[[row$table_name]]$s3_key, error = function(e) NULL)
    if (!is.null(bare_key)) {
      full_key_for_backup <- tryCatch(.s3_key(bare_key, client_id = slug), error = function(e) NULL)
      if (!is.null(full_key_for_backup)) {
        suppressWarnings(rm(list = intersect(full_key_for_backup, ls(.s3_preload_cache)), envir = .s3_preload_cache))
        suppressWarnings(rm(list = intersect(full_key_for_backup, ls(.s3_missing_cache)), envir = .s3_missing_cache))
      }
      victim_content <- tryCatch(.s3_read_with(bare_key, client_id = slug), error = function(e) NULL)
      if (!is.null(victim_content)) {
        bpath <- tryCatch(.queue_conflict_backup_path(client_id, row$seq), error = function(e) NA_character_)
        if (!is.na(bpath)) {
          backed_up <- tryCatch({
            dir.create(.queue_conflict_backups_dir(client_id), recursive = TRUE, showWarnings = FALSE)
            saveRDS(victim_content, bpath, compress = TRUE)
            TRUE
          }, error = function(e) FALSE)
          if (backed_up) backup_path <- bpath
        }
      }
    }
  }

  ok <- tryCatch({
    aws.s3::put_object(file = dest, object = row$s3_key, bucket = .s3_bucket())
    TRUE
  }, error = function(e) FALSE)
  if (!ok) return("pending")

  if (is_conflict) {
    tryCatch(
      log_action(user = acting_user, module = "offline_queue", action = "_conflict_replay",
                 description = sprintf(
                   "Replayed seq=%s for table '%s' after its version changed while offline (expected=%s, now=%s).%s",
                   row$seq, row$table_name, expected, current_version,
                   if (!is.na(backup_path))
                     sprintf(" Pre-overwrite content backed up at '%s'.", backup_path)
                   else
                     " Pre-overwrite content could NOT be backed up."),
                 target_id = row$table_name, s3_key = row$s3_key,
                 client_id = slug, viewer_home_client_id = acting_home),
      error = function(e) NULL
    )
  }

  # bump_sync_version() now reports whether ITS OWN write succeeded. The
  # actual data is already safely delivered above regardless -- but if the
  # staleness broadcast for this table failed, that's worth a durable,
  # findable record rather than staying silent the way bump_sync_version()
  # swallows it internally: other sessions may not learn about this change
  # until some unrelated later write to the same table happens to succeed.
  bump_ok <- tryCatch(bump_sync_version(row$table_name, client_id = slug), error = function(e) FALSE)
  if (!isTRUE(bump_ok)) {
    tryCatch(
      log_action(user = acting_user, module = "offline_queue", action = "_queue_stale_broadcast",
                 description = sprintf(
                   "Replay of seq=%s for table '%s' succeeded, but the version-stamp write failed -- other sessions may not see this change until a later write to the same table succeeds.",
                   row$seq, row$table_name),
                 target_id = row$table_name, s3_key = row$s3_key,
                 client_id = slug, viewer_home_client_id = acting_home),
      error = function(e) NULL
    )
  }

  "synced"
}
