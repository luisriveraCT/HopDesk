# =============================================================================
# tests/test_saas_erp_fallback.R
# Stage 3b: .sap_creds() fallback-aware resolution — store wins when it
# decrypts, falls back to legacy env vars on decrypt failure or missing row,
# and preserves today's error behavior when neither exists.
# Sourced by _run_saas.R — do not source directly.
# =============================================================================

cat("── .sap_creds() Fallback Resolution (Stage 3b) ──────────────────────────\n")

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

.mk_erp_row <- function(client_id, initials, url, company, user, password,
                        active = TRUE, secrets_encrypted = NULL) {
  tibble::tibble(
    id = uuid::UUIDgenerate(), client_id = client_id,
    label = paste0("SAP — ", initials), erp_type = "sap_b1_service_layer",
    company_initials = as.character(jsonlite::toJSON(initials, auto_unbox = FALSE)),
    config = as.character(jsonlite::toJSON(list(url = url, company = company, ssl_verify = TRUE), auto_unbox = TRUE)),
    secrets_encrypted = secrets_encrypted %||% encrypt_secret(list(user = user, password = password)),
    created_at = as.character(Sys.time()), created_by = "test", updated_at = as.character(Sys.time()), updated_by = "test",
    last_tested_at = NA_character_, last_test_result = NA_character_, last_test_message = NA_character_,
    active = active, deleted = FALSE, deleted_at = NA_character_
  )
}

.set_env_creds <- function(initials, url, company, user, password) {
  vars <- setNames(
    list(url, company, user, password),
    paste0("SAP_", initials, c("_URL", "_COMPANY", "_USER", "_PASSWORD"))
  )
  do.call(Sys.setenv, vars)
}
.clear_env_creds <- function(initials) {
  Sys.unsetenv(paste0("SAP_", initials, c("_URL", "_COMPANY", "_USER", "_PASSWORD")))
}

# ── A. Store wins when it decrypts — two deliberately different values ──────

.set_env_creds("ZZA", "https://env-zza.example", "ENV_DB", "env_user", "env_pass")
save_erp_connections(
  .mk_erp_row("networks", "ZZA", "https://store-zza.example", "STORE_DB", "store_user", "store_pass"),
  client_id = "networks"
)

creds_a <- .sap_creds("ZZA", client_id = "networks")
.chk(creds_a$url,      "https://store-zza.example", "store wins: url is the store's value, not env's")
.chk(creds_a$company,  "STORE_DB",                  "store wins: company is the store's value")
.chk(creds_a$user,     "store_user",                "store wins: user is the store's value")
.chk(creds_a$password, "store_pass",                "store wins: password is the store's value")
.chk(sap_creds_source("ZZA")$source, "store", "sap_creds_source() records 'store' after a store resolution")
.clear_env_creds("ZZA")

# ── B. Decrypt failure falls back to env var, does not throw ─────────────────

.set_env_creds("ZZB", "https://env-zzb.example", "ENV_DB", "env_user2", "env_pass2")
good_row <- .mk_erp_row("networks", "ZZB", "https://store-zzb.example", "STORE_DB", "x", "y")
# Corrupt the ciphertext so decrypt_secret() fails loudly — same tamper
# technique as test_saas_erp_secrets.R.
raw_ct <- openssl::base64_decode(good_row$secrets_encrypted[1])
raw_ct[length(raw_ct)] <- as.raw((as.integer(raw_ct[length(raw_ct)]) + 1L) %% 256L)
good_row$secrets_encrypted <- openssl::base64_encode(raw_ct)
save_erp_connections(good_row, client_id = "networks")

creds_b <- NULL
.chk(tryCatch({ creds_b <- .sap_creds("ZZB", client_id = "networks"); TRUE }, error = function(e) FALSE),
     TRUE, "decrypt failure does not throw — falls through instead")
.chk(creds_b$url, "https://env-zzb.example", "decrypt failure: falls back to the env var's url")
.chk(creds_b$user, "env_user2", "decrypt failure: falls back to the env var's user")
.chk(sap_creds_source("ZZB")$source, "legacy", "sap_creds_source() records 'legacy' after a decrypt-failure fallback")
.clear_env_creds("ZZB")

# ── C. No matching row at all — falls back to env var, unchanged behavior ────

.set_env_creds("ZZC", "https://env-zzc.example", "ENV_DB", "env_user3", "env_pass3")
# Deliberately no erp_connections row for ZZC in networks' store.
creds_c <- .sap_creds("ZZC", client_id = "networks")
.chk(creds_c$url,  "https://env-zzc.example", "no store row: falls back to env var's url")
.chk(creds_c$user, "env_user3",               "no store row: falls back to env var's user")
.clear_env_creds("ZZC")

# ── D. Neither store nor env var — same "Missing SAP credentials" error ──────

.chk_throws(function() .sap_creds("ZZD", client_id = "networks"),
            "neither store nor env var: throws, same as today's baseline")

# ── E. An inactive (active=FALSE) store row is treated as if absent ──────────

.set_env_creds("ZZE", "https://env-zze.example", "ENV_DB", "env_user5", "env_pass5")
save_erp_connections(
  .mk_erp_row("networks", "ZZE", "https://store-zze.example", "STORE_DB", "store_user5", "store_pass5", active = FALSE),
  client_id = "networks"
)
creds_e <- .sap_creds("ZZE", client_id = "networks")
.chk(creds_e$url, "https://env-zze.example", "inactive store row is skipped — falls back to env var")
.clear_env_creds("ZZE")

cat("\n")
