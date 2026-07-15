# =============================================================================
# scripts/check_versioning.R
#
# Checks and optionally enables S3 bucket versioning.
# Versioning is REQUIRED for the emergency rewind capability (Stage 6).
#
# Usage: source("scripts/check_versioning.R")
#
# What this does:
#   1. Reads the current versioning status of S3_BUCKET
#   2. Reports clearly: Enabled / Suspended / Not configured
#   3. If not enabled, offers to enable it (with a confirmation prompt)
#
# What versioning means for this bucket:
#   - Every s3_write() keeps the previous version automatically
#   - emergency_rewind.R uses these versions to restore compromised keys
#   - Versioning cannot be disabled once enabled (only suspended)
#   - Old versions count toward storage costs (~$0.023/GB/month on us-east-1)
#   - Recommendation: add a lifecycle rule to expire old versions after 90 days
#     (see instructions at the bottom of this script)
# =============================================================================

source("R/persistence.R")
readRenviron(".Renviron")
s3_init()

BUCKET <- Sys.getenv("S3_BUCKET")
if (!nzchar(BUCKET)) stop("S3_BUCKET not set — check .Renviron")

cat("═══════════════════════════════════════════════════════════\n")
cat(" S3 Versioning Check — bucket:", BUCKET, "\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# ── 1. Check current status via S3 GET ?versioning ───────────────────────────
cat("Checking versioning status...\n")

resp <- tryCatch({
  aws.s3::s3HTTP(
    verb   = "GET",
    bucket = BUCKET,
    query  = list(versioning = "")
  )
}, error = function(e) {
  cat("[ERROR] Could not read versioning status:", e$message, "\n")
  NULL
})

parse_versioning_status <- function(resp) {
  if (is.null(resp)) return("UNKNOWN")
  # aws.s3 parses the XML and returns the Status value directly as a character vector
  if (is.character(resp) && length(resp) >= 1) {
    if (resp[1] == "Enabled")   return("Enabled")
    if (resp[1] == "Suspended") return("Suspended")
  }
  if (length(resp) == 0) return("NotConfigured")
  return("UNKNOWN")
}

status <- parse_versioning_status(resp)

cat("Status:", status, "\n\n")

if (status == "Enabled") {
  cat("✓ Versioning is ENABLED. Emergency rewind is operational.\n\n")
  cat("Tip: confirm you have a lifecycle rule to expire old versions after 90 days\n")
  cat("to control storage costs (see AWS console → Bucket → Management → Lifecycle).\n")
  invisible(TRUE)

} else {

  if (status == "Suspended") {
    cat("⚠ Versioning is SUSPENDED. Old versions exist but new writes are not versioned.\n")
  } else {
    cat("✗ Versioning is NOT ENABLED. Emergency rewind will NOT work.\n")
  }

  cat("\nThis script can enable versioning now.\n")
  answer <- readline("Enable versioning? (yes/no): ")

  if (tolower(trimws(answer)) != "yes") {
    cat("\nAborted. Enable versioning manually:\n")
    cat("  AWS Console → S3 → ", BUCKET, " → Properties → Bucket Versioning → Enable\n")
    invisible(FALSE)
  } else {
    cat("\nEnabling versioning...\n")
    body <- paste0(
      '<VersioningConfiguration xmlns="http://s3.amazonaws.com/doc/2006-03-01/">',
      '<Status>Enabled</Status>',
      '</VersioningConfiguration>'
    )
    enable_resp <- tryCatch({
      aws.s3::s3HTTP(
        verb         = "PUT",
        bucket       = BUCKET,
        query        = list(versioning = ""),
        request_body = body,
        headers      = list(`Content-Type` = "application/xml")
      )
      TRUE
    }, error = function(e) {
      cat("[ERROR] Could not enable versioning:", e$message, "\n")
      cat("\nEnable manually instead:\n")
      cat("  AWS Console → S3 → ", BUCKET, " → Properties → Bucket Versioning → Enable\n")
      FALSE
    })

    if (isTRUE(enable_resp)) {
      cat("✓ Versioning ENABLED.\n\n")

      cat("═══════════════════════════════════════════════════════════\n")
      cat(" IMPORTANT: Add a lifecycle rule to control storage costs\n")
      cat("═══════════════════════════════════════════════════════════\n")
      cat("AWS Console → S3 →", BUCKET, "→ Management → Lifecycle rules → Create\n")
      cat("  Rule name:     expire-old-versions\n")
      cat("  Scope:         Apply to ALL objects in the bucket\n")
      cat("  Actions:       [x] Permanently delete noncurrent versions of objects\n")
      cat("  Days after:    90\n")
      cat("  Keep versions: 1 (minimum)\n")
      cat("\nThis keeps the last version for 90 days — enough for rewind after any\n")
      cat("incident, while preventing unlimited version accumulation.\n")
    }

    invisible(isTRUE(enable_resp))
  }
}
