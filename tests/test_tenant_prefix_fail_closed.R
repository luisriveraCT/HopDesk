# =============================================================================
# tests/test_tenant_prefix_fail_closed.R
#
# Locks in the 2026-09-25 fail-closed fix for tenant prefix resolution, and the
# bucket binding on the offline write queue.
#
# Background: CLIENT_ID answers "which deployment am I". On a single-tenant
# deployment that is also "whose data is this", so defaulting to it is correct.
# On the shared hd-admin deployment the two diverge, and defaulting silently
# wrote one tenant's data under another's prefix -- Networks' manual invoices,
# SAP snapshots and audit rows ended up under hd-admin/, where they froze into
# a stale snapshot that later sessions read back as if it were live, showing
# invoices users had already deleted.
#
# Separately, the offline queue resolved its target bucket at REPLAY time from
# ambient state, so an entry staged by a local dev run could be replayed into
# production. Nothing had been staged when this was found, so the path was
# closed before it ever fired.
#
# Pure function-level tests: no S3, no network, no real credentials.
# =============================================================================

cat("\n== Tenant prefix fail-closed + queue bucket binding ==\n")

.pass <- 0L; .fail <- 0L
.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else { cat(sprintf("  FAIL  %s\n        esperado: %s\n        obtenido: %s\n",
                     label, deparse(expected), deparse(actual))); .fail <<- .fail + 1L }
}
.chk_true <- function(cond, label) .chk(isTRUE(cond), TRUE, label)
.errs <- function(expr) inherits(tryCatch(expr, error = function(e) e), "error")

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
source("R/persistence.R")
source("R/offline_queue.R")

.orig_client <- Sys.getenv("CLIENT_ID", unset = NA)
.orig_bucket <- Sys.getenv("S3_BUCKET", unset = NA)

tryCatch({

  # ── Explicit client_id always wins, on any deployment ─────────────────────
  Sys.setenv(CLIENT_ID = "hd-admin")
  .chk(.s3_key("invoice_moves.rds", client_id = "networks"), "networks/invoice_moves.rds",
       "explicit client_id resolves to that tenant's prefix")
  .chk(.s3_key("invoice_moves.rds", client_id = "  NETWORKS  "), "networks/invoice_moves.rds",
       "explicit client_id is trimmed and lowercased")

  # ── Shared deployment: refuse to guess a tenant ───────────────────────────
  for (k in c("invoice_moves.rds", "manual_invoices.rds", "papelera.rds",
              "sap_snapshot_AR.rds", "pasivos_provisions.rds", "policy_moves.rds",
              "app_audit.rds", "sync_versions.rds", "interco_v2.rds")) {
    .chk_true(.errs(.s3_key(k)),
              sprintf("shared deployment refuses to default tenant key '%s'", k))
  }

  # ── ...but the control plane's OWN keys still resolve ──────────────────────
  .chk(.s3_key("client_registry.rds"), "hd-admin/client_registry.rds",
       "deployment-owned key still resolves without client_id")
  .chk(.s3_key("hop_grants.rds"), "hd-admin/hop_grants.rds",
       "hop_grants.rds is deployment-owned")
  .chk(.s3_key("forecasting_sources.rds"), "hd-admin/forecasting_sources.rds",
       "forecasting catalogs are deployment-owned (read via .s3_read(), no client_id)")

  # ── Single-tenant deployments keep the old behavior exactly ───────────────
  Sys.setenv(CLIENT_ID = "networks")
  .chk(.s3_key("invoice_moves.rds"), "networks/invoice_moves.rds",
       "single-tenant deployment still defaults from CLIENT_ID")
  .chk(.s3_key("manual_invoices.rds"), "networks/manual_invoices.rds",
       "single-tenant default is unchanged for every tenant key")

  # ── Offline queue records the bucket it was staged for ────────────────────
  .chk_true("bucket" %in% names(.queue_manifest_schema()),
            "queue manifest schema carries a bucket column")

  # ── ...and refuses to replay across buckets ───────────────────────────────
  Sys.setenv(CLIENT_ID = "hd-admin", S3_BUCKET = "antiguedad-rds-prod")
  .logged <- NULL
  log_action <<- function(..., action = NA) { .logged <<- c(.logged, action); invisible(TRUE) }

  mkrow <- function(bucket) data.frame(
    seq = 1L, s3_key = "networks/papelera.rds", bucket = bucket,
    table_name = "papelera_db", client_id = "networks",
    expected_sync_version = NA_integer_, acting_user = "dev",
    acting_home_client_id = "networks", status = "pending",
    staged_at = Sys.time(), synced_at = as.POSIXct(NA), stringsAsFactors = FALSE)

  .logged <- NULL
  .chk(.queue_replay_entry(mkrow("antiguedad-rds-dev"), "networks"), "blocked",
       "entry staged for the DEV bucket is blocked, never replayed into prod")
  .chk_true("_queue_bucket_mismatch" %in% .logged,
            "the cross-bucket refusal is recorded in the audit log")

  .logged <- NULL
  .chk(.queue_replay_entry(mkrow(NA_character_), "networks"), "blocked",
       "legacy entry with no recorded bucket is blocked (not provably safe)")

  .logged <- NULL
  .chk(.queue_replay_entry(mkrow(""), "networks"), "blocked",
       "entry with an empty bucket is blocked")

  # A matching bucket must get PAST the bucket gate -- it still ends up
  # "blocked" here because no payload file exists on disk, but it must not be
  # the bucket check that stopped it.
  .logged <- NULL
  invisible(.queue_replay_entry(mkrow("antiguedad-rds-prod"), "networks"))
  .chk_true(!("_queue_bucket_mismatch" %in% .logged),
            "matching bucket passes the bucket gate (stopped later, on the missing payload)")

}, finally = {
  if (is.na(.orig_client)) Sys.unsetenv("CLIENT_ID") else Sys.setenv(CLIENT_ID = .orig_client)
  if (is.na(.orig_bucket)) Sys.unsetenv("S3_BUCKET") else Sys.setenv(S3_BUCKET = .orig_bucket)
  if (exists("log_action", envir = .GlobalEnv)) rm("log_action", envir = .GlobalEnv)
})

cat(sprintf("\n  %d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) stop("test_tenant_prefix_fail_closed.R: ", .fail, " failure(s)")
