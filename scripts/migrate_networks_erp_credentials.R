# =============================================================================
# scripts/migrate_networks_erp_credentials.R
#
# Stage 3 §6: migrates Networks Group's 5 hardcoded SAP_{INITIALS}_* env vars
# (currently the only way any client's ERP connects) into encrypted rows in
# networks/erp_connections.rds, and creates an empty hopdesk/erp_connections.rds
# for the "hopdesk" test client, consistent with every other client folder.
#
# This is a ONE-TIME migration. It does not modify .Renviron or remove the
# SAP_{INITIALS}_* env var reads from R/sap_api.R — those stay in place until
# the new credential-store path has been proven end to end against live SAP
# data (Manual test plan step 5 in docs/saas_rebuild/STAGE_3_ERP_CREDENTIALS.md)
# and Mouse has signed off. Re-running this script is safe in the sense that
# it always OVERWRITES networks/erp_connections.rds from the current .Renviron
# values — it is not additive/idempotent-merge, by design (this is meant to
# run once, at migration time, not routinely).
#
# Prerequisites before running:
#   1. HOPDESK_SECRETS_KEY must be set in .Renviron — generate one with:
#        openssl rand -base64 32
#      and add it as HOPDESK_SECRETS_KEY=<value> to .Renviron BEFORE running
#      this script (encrypt_secret() will stop() loudly otherwise).
#   2. Run from a working directory whose .Renviron has the real, live
#      SAP_NG_*/SAP_NTS_*/SAP_NCS_*/SAP_NL_*/SAP_NRS_* values (i.e. the main
#      app worktree, not this docs/stage-3 worktree, which has no .Renviron).
#   3. AWS credentials in that same .Renviron must point at the real
#      production S3 bucket — this script WRITES to production S3.
#
# Usage: Rscript scripts/migrate_networks_erp_credentials.R
# =============================================================================

library(tibble)
library(uuid)
library(jsonlite)

source("R/persistence.R")
source("R/secrets_encryption.R")
S3_KEYS <- list(erp_connections = "erp_connections.rds")

readRenviron(".Renviron")
s3_init()

if (!nzchar(Sys.getenv("HOPDESK_SECRETS_KEY"))) {
  stop("HOPDESK_SECRETS_KEY is not set. Generate one with `openssl rand -base64 32` ",
       "and add it to .Renviron before running this migration.")
}

# ── The 5 Networks Group companies, matching R/sap_api.R's SAP_{INITIALS}_* convention ──
NETWORKS_COMPANIES <- list(
  list(initials = "NG",  label = "SAP — Networks Group (NG)"),
  list(initials = "NTS", label = "SAP — Networks Trade Services (NTS)"),
  list(initials = "NCS", label = "SAP — Networks Crossdocking Services (NCS)"),
  list(initials = "NL",  label = "SAP — Networks Logistics (NL)"),
  list(initials = "NRS", label = "SAP — Networks Realty Services (NRS)")
)

.read_sap_env <- function(initials) {
  i <- toupper(initials)
  url      <- Sys.getenv(paste0("SAP_", i, "_URL"))
  company  <- Sys.getenv(paste0("SAP_", i, "_COMPANY"))
  user     <- Sys.getenv(paste0("SAP_", i, "_USER"))
  password <- Sys.getenv(paste0("SAP_", i, "_PASSWORD"))
  ssl_raw  <- tolower(trimws(Sys.getenv(paste0("SAP_", i, "_SSL_VERIFY"), unset = "true")))
  ssl_verify <- !(ssl_raw %in% c("false", "0", "no"))

  missing <- c(
    if (!nzchar(url))      paste0("SAP_", i, "_URL"),
    if (!nzchar(company))  paste0("SAP_", i, "_COMPANY"),
    if (!nzchar(user))     paste0("SAP_", i, "_USER"),
    if (!nzchar(password)) paste0("SAP_", i, "_PASSWORD")
  )
  if (length(missing))
    stop("Missing SAP env vars for ", i, ": ", paste(missing, collapse = ", "))

  list(url = url, company = company, user = user, password = password, ssl_verify = ssl_verify)
}

now <- as.character(Sys.time())

rows <- lapply(NETWORKS_COMPANIES, function(co) {
  creds <- .read_sap_env(co$initials)
  tibble(
    id                = uuid::UUIDgenerate(),
    client_id         = "networks",
    label             = co$label,
    erp_type          = "sap_b1_service_layer",
    company_initials  = as.character(jsonlite::toJSON(co$initials, auto_unbox = FALSE)),
    config            = as.character(jsonlite::toJSON(
                           list(url = creds$url, company = creds$company, ssl_verify = creds$ssl_verify),
                           auto_unbox = TRUE)),
    secrets_encrypted = encrypt_secret(list(user = creds$user, password = creds$password)),
    created_at        = now, created_by = "migration_script",
    updated_at        = now, updated_by = "migration_script",
    last_tested_at     = NA_character_, last_test_result = NA_character_, last_test_message = NA_character_,
    active            = TRUE, deleted = FALSE, deleted_at = NA_character_
  )
})

networks_df <- dplyr::bind_rows(rows)

cat(sprintf("About to write %d row(s) to networks/erp_connections.rds (bucket: %s):\n",
            nrow(networks_df), Sys.getenv("S3_BUCKET")))
print(networks_df[, c("label", "erp_type", "company_initials")])

save_erp_connections(networks_df, client_id = "networks")
cat("[OK] networks/erp_connections.rds written —", nrow(networks_df), "connection(s).\n")

# ── hopdesk (test client) — empty store, for consistency with every other client folder ──
save_erp_connections(.schema_erp_connections(), client_id = "hopdesk")
cat("[OK] hopdesk/erp_connections.rds created (empty).\n")
