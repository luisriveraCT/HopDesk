# =============================================================================
# scripts/emergency_rewind.R
# Emergency S3 version rewind for compromised accounts.
#
# Purpose: after an account is locked via the Security panel, use this script
#   to restore S3 objects that the compromised account modified after a given
#   timestamp. Each restore is individually confirmed before it happens.
#
# Prerequisites:
#   - S3 bucket versioning MUST be enabled (script checks at startup)
#   - .Renviron with AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY / S3_BUCKET
#   - Run OUTSIDE the Shiny app, from an RStudio console or terminal
#
# Usage (run interactively — copy-paste into console):
#   source("scripts/emergency_rewind.R")
#   # then call:
#   emergency_rewind(
#     compromised_usernames = c("tesoreria"),
#     since_timestamp       = as.POSIXct("2026-06-23 10:00:00", tz = "UTC")
#   )
# =============================================================================

if (!requireNamespace("aws.s3", quietly = TRUE)) stop("aws.s3 package required")
if (file.exists(".Renviron")) readRenviron(".Renviron")

# ── Helpers ───────────────────────────────────────────────────────────────────

.er_bucket <- function() {
  b <- Sys.getenv("S3_BUCKET")
  if (!nzchar(b)) stop("S3_BUCKET env var not set")
  b
}

.check_versioning_enabled <- function() {
  resp <- tryCatch(
    aws.s3::s3HTTP(
      verb   = "GET",
      bucket = .er_bucket(),
      query  = list(versioning = "")
    ),
    error = function(e) stop("Could not query bucket versioning: ", e$message)
  )
  enabled <- is.character(resp) && length(resp) >= 1 && resp[1] == "Enabled"
  if (!enabled) {
    stop(
      "S3 bucket versioning is NOT enabled on '", .er_bucket(), "'.\n",
      "Run scripts/check_versioning.R first to enable it, then re-run this script."
    )
  }
  message("[REWIND] Bucket versioning: Enabled")
  invisible(TRUE)
}

# List all versions of a given S3 key.
# Returns a data.frame: version_id, last_modified, is_latest, is_delete_marker
.list_versions <- function(key) {
  resp <- tryCatch(
    aws.s3::s3HTTP(
      verb   = "GET",
      bucket = .er_bucket(),
      query  = list(versions = "", prefix = key)
    ),
    error = function(e) {
      message("[REWIND]   Failed to list versions for ", key, ": ", e$message)
      return(NULL)
    }
  )
  if (is.null(resp)) return(NULL)

  # aws.s3 returns a parsed list; extract Version and DeleteMarker elements
  versions <- c(
    if (!is.null(resp$Version))       resp$Version,
    if (!is.null(resp$DeleteMarker))  resp$DeleteMarker
  )

  if (!length(versions)) return(data.frame())

  rows <- lapply(versions, function(v) {
    data.frame(
      version_id      = v$VersionId   %||% NA_character_,
      last_modified   = as.POSIXct(v$LastModified %||% NA_character_,
                                   format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
      is_latest       = identical(v$IsLatest, "true"),
      is_delete_marker = !is.null(v$Type) && v$Type == "DeleteMarker",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Restore a specific S3 key to the version immediately BEFORE since_timestamp.
# Strategy: copy the newest version whose last_modified < since_timestamp
#   to the same key (creating a new "current" version from the old state).
.restore_key <- function(key, since_timestamp) {
  versions <- .list_versions(key)
  if (is.null(versions) || !nrow(versions)) {
    message("[REWIND]   No versions found for ", key)
    return(invisible(FALSE))
  }

  # Versions before the compromise window
  clean <- versions[
    !is.na(versions$last_modified) &
      versions$last_modified < since_timestamp &
      !versions$is_delete_marker,
  ]

  if (!nrow(clean)) {
    message("[REWIND]   No clean version found before ", since_timestamp, " for ", key)
    return(invisible(FALSE))
  }

  # Pick the most-recent clean version
  clean <- clean[order(clean$last_modified, decreasing = TRUE), ]
  target_vid <- clean$version_id[1]
  target_ts  <- clean$last_modified[1]

  message("[REWIND]   Restoring ", key)
  message("[REWIND]     Clean version : ", target_vid)
  message("[REWIND]     Version date  : ", format(target_ts, "%Y-%m-%d %H:%M:%S %Z"))

  # S3 server-side copy — copies the specified version to become the new current
  tryCatch({
    aws.s3::s3HTTP(
      verb    = "PUT",
      bucket  = .er_bucket(),
      path    = paste0("/", key),
      headers = list(
        `x-amz-copy-source`            = paste0(.er_bucket(), "/", key,
                                                  "?versionId=", target_vid),
        `x-amz-copy-source-version-id` = target_vid,
        `x-amz-metadata-directive`     = "COPY"
      )
    )
    message("[REWIND]   Restored OK")
    return(invisible(TRUE))
  }, error = function(e) {
    message("[REWIND]   Restore FAILED: ", e$message)
    return(invisible(FALSE))
  })
}

# ── Main entry point ──────────────────────────────────────────────────────────

#' Rewind S3 objects modified by compromised accounts after a given timestamp.
#'
#' @param compromised_usernames character vector of locked account usernames
#' @param since_timestamp       POSIXct — restore everything written AFTER this moment
emergency_rewind <- function(compromised_usernames, since_timestamp) {
  stopifnot(
    is.character(compromised_usernames), length(compromised_usernames) >= 1,
    inherits(since_timestamp, "POSIXct")
  )

  cat("\n====================================================\n")
  cat("  HopDesk Emergency Rewind\n")
  cat("====================================================\n")
  cat("  Bucket    :", .er_bucket(), "\n")
  cat("  Accounts  :", paste(compromised_usernames, collapse = ", "), "\n")
  cat("  Since     :", format(since_timestamp, "%Y-%m-%d %H:%M:%S %Z"), "\n")
  cat("====================================================\n\n")

  # 1. Verify versioning
  .check_versioning_enabled()

  # 2. Load audit log
  audit_rds <- "hd-admin/app_audit.rds"
  audit_df <- tryCatch(
    aws.s3::s3readRDS(object = audit_rds, bucket = .er_bucket()),
    error = function(e) NULL
  )

  if (is.null(audit_df) || !is.data.frame(audit_df) || !nrow(audit_df)) {
    cat("\n[REWIND] Audit log is empty or not found at", audit_rds, "\n")
    cat("[REWIND] Cannot determine which keys were modified. Aborting.\n")
    return(invisible(NULL))
  }

  # Normalize column names (handle both 'user'/'username' and 'ts'/'timestamp')
  if ("user" %in% names(audit_df) && !"username" %in% names(audit_df))
    audit_df$username <- audit_df$user
  if ("ts" %in% names(audit_df) && !"timestamp" %in% names(audit_df))
    audit_df$timestamp <- audit_df$ts

  # Filter: compromised account + after timestamp + has an s3_key column
  has_ts  <- "timestamp" %in% names(audit_df)
  has_key <- "s3_key"    %in% names(audit_df)
  has_usr <- "username"  %in% names(audit_df)

  if (!has_ts || !has_usr) {
    cat("[REWIND] Audit log missing required columns (ts/timestamp, user/username). Aborting.\n")
    return(invisible(NULL))
  }

  audit_df$timestamp <- as.POSIXct(audit_df$timestamp, tz = "UTC")
  hits <- audit_df[
    tolower(audit_df$username) %in% tolower(compromised_usernames) &
      audit_df$timestamp >= since_timestamp,
  ]

  if (!nrow(hits)) {
    cat("[REWIND] No audit entries match those accounts after the given timestamp.\n")
    cat("[REWIND] Nothing to restore.\n")
    return(invisible(NULL))
  }

  cat(sprintf("[REWIND] Found %d audit entries in the compromise window.\n", nrow(hits)))

  # Collect distinct S3 keys mentioned in the window.
  # Exclude hd-admin/incident_reports/ — these are the audit trail of past rewinds
  # and must never themselves be rewound (that would destroy evidence).
  affected_keys <- character(0)
  if (has_key) {
    all_keys      <- unique(hits$s3_key[nzchar(hits$s3_key %||% "")])
    protected     <- grepl("^hd-admin/incident_reports/", all_keys)
    if (any(protected))
      cat(sprintf("[REWIND] Skipping %d incident_reports/ key(s) — protected path.\n",
                  sum(protected)))
    affected_keys <- all_keys[!protected]
  }

  # Fallback: if audit log doesn't carry s3_key, infer from action/description
  if (!length(affected_keys)) {
    cat("[REWIND] Audit log has no s3_key column — listing affected entries for manual review:\n\n")
    print(hits[, intersect(c("timestamp","username","action","description"), names(hits))])
    cat("\n[REWIND] Identify the S3 keys manually and call .restore_key(key, since_timestamp).\n")
    return(invisible(hits))
  }

  cat("\nKeys to be reviewed for restore:\n")
  for (k in seq_along(affected_keys)) cat(sprintf("  %d. %s\n", k, affected_keys[k]))
  cat("\n")

  # 3. Per-key confirmation + restore
  results <- data.frame(
    s3_key       = character(),
    action_taken = character(),
    success      = logical(),
    stringsAsFactors = FALSE
  )

  for (key in affected_keys) {
    cat("----------------------------------------------------\n")
    cat("Key:", key, "\n")
    ans <- readline(prompt = "Restore this key? [y/N/q to quit]: ")
    if (tolower(trimws(ans)) == "q") {
      cat("[REWIND] Aborted by user.\n")
      break
    }
    if (tolower(trimws(ans)) != "y") {
      cat("[REWIND] Skipped.\n")
      results <- rbind(results, data.frame(s3_key=key, action_taken="skipped", success=NA))
      next
    }
    ok <- .restore_key(key, since_timestamp)
    results <- rbind(results,
                     data.frame(s3_key=key, action_taken="restore_attempted", success=ok))
  }

  # 4. Incident report
  report <- list(
    incident_time         = since_timestamp,
    report_generated_at   = Sys.time(),
    compromised_usernames = compromised_usernames,
    bucket                = .er_bucket(),
    audit_hits            = hits,
    restore_results       = results
  )

  ts_str    <- format(Sys.time(), "%Y%m%d_%H%M%S")
  report_key <- paste0("hd-admin/incident_reports/", ts_str, ".rds")

  cat("\n[REWIND] Saving incident report to", report_key, "...\n")
  tryCatch({
    aws.s3::s3saveRDS(report, object = report_key, bucket = .er_bucket())
    cat("[REWIND] Incident report saved.\n")
  }, error = function(e) {
    cat("[REWIND] WARNING: Could not save incident report:", e$message, "\n")
    cat("[REWIND] Printing report to console:\n")
    print(report)
  })

  cat("\n====================================================\n")
  cat("  Rewind complete. Summary:\n")
  print(results)
  cat("====================================================\n\n")

  invisible(report)
}
