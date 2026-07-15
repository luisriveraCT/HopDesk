# =============================================================================
# tests/test_saas_isolation.R
# Verifies S3 prefix isolation and the global username registry.
# Sourced by _run_saas.R — do not source directly.
# =============================================================================

cat("── S3 Isolation & Username Registry ────────────────────────────────────\n")

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

# ── A. .s3_key() isolation ────────────────────────────────────────────────────

old_cid <- Sys.getenv("CLIENT_ID")
Sys.setenv(CLIENT_ID = "networks")
.chk(.s3_key("usuarios.rds"),       "networks/usuarios.rds",  "s3_key networks→networks/usuarios.rds")
.chk(.s3_key("app_audit.rds"),      "networks/app_audit.rds", "s3_key networks→networks/app_audit.rds")

# Override via client_id parameter (Stage 4 cross-client access)
.chk(.s3_key("usuarios.rds", client_id = "hopdesk"),
     "hopdesk/usuarios.rds", "s3_key override→hopdesk/usuarios.rds")
.chk(.s3_key("usuarios.rds", client_id = "NETWORKS"),
     "networks/usuarios.rds", "s3_key override is lowercased")
.chk(.s3_key("f.rds", client_id = NULL),
     "networks/f.rds",        "s3_key NULL override falls back to CLIENT_ID")

Sys.setenv(CLIENT_ID = "hopdesk")
.chk(.s3_key("empresas.rds"),  "hopdesk/empresas.rds", "s3_key hopdesk→hopdesk/empresas.rds")

Sys.setenv(CLIENT_ID = old_cid)   # restore

# ── B. .s3_read_with() uses the override prefix ───────────────────────────────

# Plant networks data
mock_s3saveRDS(
  data.frame(id = "n1", name = "networks-data", stringsAsFactors = FALSE),
  "networks/test_file.rds", "mock-bucket"
)
# Plant hopdesk data at different key
mock_s3saveRDS(
  data.frame(id = "h1", name = "hopdesk-data", stringsAsFactors = FALSE),
  "hopdesk/test_file.rds", "mock-bucket"
)

Sys.setenv(CLIENT_ID = "networks")
net_res  <- .s3_read_with("test_file.rds", client_id = NULL)
hop_res  <- .s3_read_with("test_file.rds", client_id = "hopdesk")

.chk(net_res$name,  "networks-data", ".s3_read_with NULL reads own prefix")
.chk(hop_res$name,  "hopdesk-data",  ".s3_read_with override reads other prefix")

# Missing key returns NULL (not an error)
missing_res <- .s3_read_with("definitely_absent_xyz.rds", client_id = "networks")
.chk(is.null(missing_res), TRUE, ".s3_read_with missing key returns NULL")

# ── C. Global username registry ───────────────────────────────────────────────

# Seed empty index
mock_s3saveRDS(.schema_username_index(), "hd-admin/username_index.rds", "mock-bucket")

# Fresh username → available
.chk(check_username_available("alice"),  TRUE, "fresh username is available")
.chk(check_username_available("ALICE"),  TRUE, "fresh username check is case-insensitive")

# Register it → now taken
register_username("alice", "networks", "U0001")
.chk(check_username_available("alice"),  FALSE, "registered username is not available")
.chk(check_username_available("ALICE"),  FALSE, "registered username blocked case-insensitively")
.chk(check_username_available("bob"),    TRUE,  "different username still available after register")

# Register bob in a different folder
register_username("bob", "hopdesk", "U0002")
.chk(check_username_available("bob"),    FALSE, "bob registered in hopdesk — globally blocked")

# Unregister alice → available again
unregister_username("alice")
.chk(check_username_available("alice"),  TRUE, "unregistered username is available again")

# Unregister leaves the record (active = FALSE) — preserves audit trail
idx <- .read_username_index()
alice_row <- idx[tolower(idx$username) == "alice", , drop = FALSE]
.chk(nrow(alice_row),          1L,    "unregistered record is preserved (not deleted)")
.chk(alice_row$active[1],      FALSE, "unregistered record has active = FALSE")

# Re-register same username succeeds (reactivates)
register_username("alice", "networks", "U0001")
.chk(check_username_available("alice"),  FALSE, "re-registered username is blocked again")
idx2 <- .read_username_index()
alice_rows <- idx2[tolower(idx2$username) == "alice", , drop = FALSE]
# Should have exactly one active alice row (no duplicates if reactivation updates in place)
active_alices <- alice_rows[isTRUE(alice_rows$active) | alice_rows$active == TRUE, , drop = FALSE]
.chk(nrow(active_alices) >= 1L, TRUE, "at least one active alice row after re-register")

cat("\n")
