# =============================================================================
# tests/test_saas_erp_isolation.R
# Stage 3: load_erp_connections()/save_erp_connections() isolation guarantee.
# Sourced by _run_saas.R — do not source directly.
# =============================================================================

cat("── ERP Connections Isolation Guarantee ──────────────────────────────────\n")

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

.chk_throws <- function(expr_fn, label) {
  ok <- tryCatch({ expr_fn(); FALSE }, error = function(e) TRUE)
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected an error, none was thrown\n", label))
    .fail <<- .fail + 1L
  }
}

.mk_row <- function(client_id, label = "test", id = uuid::UUIDgenerate()) {
  tibble::tibble(
    id = id, client_id = client_id, label = label, erp_type = "sap_b1_service_layer",
    company_initials = as.character(jsonlite::toJSON("NG")),
    config = as.character(jsonlite::toJSON(list(url = "https://x"))),
    secrets_encrypted = "unused-in-these-tests",
    created_at = as.character(Sys.time()), created_by = "U0001",
    updated_at = as.character(Sys.time()), updated_by = "U0001",
    last_tested_at = NA_character_, last_test_result = NA_character_, last_test_message = NA_character_,
    active = TRUE, deleted = FALSE, deleted_at = NA_character_
  )
}

# ── A. client_id is required — no silent fallback ─────────────────────────────

.chk_throws(function() load_erp_connections(NULL),  "load_erp_connections(NULL) throws")
.chk_throws(function() load_erp_connections(""),    "load_erp_connections('') throws")
.chk_throws(function() load_erp_connections(),       "load_erp_connections() with no arg throws")
.chk_throws(function() save_erp_connections(.mk_row("networks"), NULL), "save_erp_connections(df, NULL) throws")
.chk_throws(function() save_erp_connections(.mk_row("networks")),       "save_erp_connections(df) with no client_id throws")

# ── B. Round-trip for the right client ────────────────────────────────────────

row_net <- .mk_row("networks", label = "SAP — NG")
save_erp_connections(row_net, client_id = "networks")
loaded_net <- load_erp_connections(client_id = "networks")
.chk(nrow(loaded_net), 1L, "save+load round-trip for networks returns 1 row")
.chk(loaded_net$label[1], "SAP — NG", "round-tripped row has the expected label")

row_hop <- .mk_row("hopdesk", label = "SAP — HD")
save_erp_connections(row_hop, client_id = "hopdesk")
loaded_hop <- load_erp_connections(client_id = "hopdesk")
.chk(nrow(loaded_hop), 1L, "hopdesk's own store is independent of networks'")
.chk(nrow(load_erp_connections(client_id = "networks")), 1L, "networks' store unaffected by hopdesk's write")

# ── C. Cross-client write rejected (loud error, not silent drop) ─────────────
# Simulates a session at a different client attempting to write a row that
# claims to belong to "networks".

.chk_throws(
  function() save_erp_connections(.mk_row("networks"), client_id = "hopdesk"),
  "writing a client_id='networks' row while saving under 'hopdesk' is rejected"
)
# Confirm the rejected write didn't partially land anywhere
.chk(nrow(load_erp_connections(client_id = "hopdesk")), 1L,
     "hopdesk's store still has only its own original row after the rejected write")

# ── D. Corrupted file with a stray foreign row — defense-in-depth assertion ──
# Directly plant a bad file (bypassing save_erp_connections' own guard) to
# simulate the S3-key-mistake scenario the assertion exists to catch.

corrupted <- dplyr::bind_rows(.mk_row("hopdesk", label = "ok row"),
                               .mk_row("networks", label = "stray foreign row"))
mock_s3saveRDS(corrupted, "hopdesk/erp_connections.rds", "mock-bucket")

.chk_throws(function() load_erp_connections(client_id = "hopdesk"),
            "loading a corrupted file with a stray foreign-client row fails loudly")

# Restore hopdesk's store to a clean state for any tests that run after this one
save_erp_connections(.mk_row("hopdesk", label = "SAP — HD"), client_id = "hopdesk")

# ── E. Soft-deleted rows excluded from load ───────────────────────────────────

del_row <- .mk_row("networks", label = "to be deleted")
del_row$deleted <- TRUE
del_row$deleted_at <- as.character(Sys.time())
combined <- dplyr::bind_rows(load_erp_connections(client_id = "networks"), del_row)
save_erp_connections(combined, client_id = "networks")
after_delete <- load_erp_connections(client_id = "networks")
.chk(all(after_delete$label != "to be deleted"), TRUE,
     "soft-deleted row is excluded from load_erp_connections()")

cat("\n")
