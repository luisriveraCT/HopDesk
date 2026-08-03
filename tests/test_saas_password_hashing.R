# =============================================================================
# tests/test_saas_password_hashing.R
# Stage 7: password hashing.
#
# R/auth.R's header comment used to claim shinymanager only supports hashed
# passwords via its SQLite backend — that's wrong. shinymanager::check_
# credentials_df() natively supports hashed passwords on the data.frame path
# via an is_hashed_password column + scrypt::verifyPassword(). This stage
# leans on that native support directly instead of writing a parallel
# comparator: hash_password()/verify_password() (scrypt), a transparent
# migration of legacy plaintext rows on every real read path
# (.migrate_legacy_passwords()), and is_hashed_password = TRUE on every row
# .normalize_credentials() produces.
#
# Sourced by _run_saas.R.
# =============================================================================

cat("── Password Hashing (Stage 7) ──────────────────────────────────────────\n")

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

# ── A. hash_password()/verify_password() round-trip ──────────────────────────

h_a <- hash_password("Correcto123!")
.chk(is.character(h_a) && nzchar(h_a), TRUE, "hash_password() returns a non-empty string")
.chk(verify_password("Correcto123!", h_a), TRUE,
     "verify_password(): the correct password verifies")
.chk(verify_password("Incorrecto999?", h_a), FALSE,
     "verify_password(): a wrong password does NOT verify")
.chk(verify_password("Correcto123!", "not-a-real-hash"), FALSE,
     "verify_password(): a malformed hash fails closed, doesn't throw")

# ── B. .is_scrypt_hash() detection ────────────────────────────────────────────

.chk(.is_scrypt_hash(h_a), TRUE, ".is_scrypt_hash(): a real scrypt hash is detected as one")
.chk(.is_scrypt_hash("plainTextPassword1!"), FALSE,
     ".is_scrypt_hash(): a plaintext string is NOT mistaken for a hash")
.chk(.is_scrypt_hash(NA_character_), FALSE, ".is_scrypt_hash(): NA is not a hash")
.chk(.is_scrypt_hash(""), FALSE, ".is_scrypt_hash(): empty string is not a hash")

# ── C. The single most important test: legacy plaintext is transparently
# migrated on load, AND the ORIGINAL plaintext password still authenticates
# immediately afterward through the real login path. This is the direct
# proof that Stage 7 does not lock anyone out. ───────────────────────────────

Sys.setenv(CLIENT_ID = "networks")

legacy_plain_pw <- "OldPlainPw1!"
legacy_row <- data.frame(
  id = "u-legacy-1", account_code = "U0009", username = "legacyuser",
  password_hash = legacy_plain_pw,   # <- plaintext, simulating a pre-Stage-7 row
  display_name = "Legacy User", tier = "finance", client_id = "networks",
  permisos = "{}", group_ids = "[]", allowed_clients = "[]",
  email = NA_character_, requires_password_change = FALSE,
  activo = TRUE, created_at = as.character(Sys.time()), last_login = NA_character_,
  deleted = FALSE, deleted_at = NA_character_,
  stringsAsFactors = FALSE
)
mock_s3saveRDS(legacy_row, "networks/usuarios.rds", "mock-bucket")

auth_invalidate_credentials()   # force a fresh read through .load_or_init_credentials()

creds <- auth_get_credentials()
row   <- creds[creds$user == "legacyuser", , drop = FALSE]

.chk(nrow(row), 1L, "migration: legacyuser appears in resolved credentials")
.chk(isTRUE(row$is_hashed_password[1]), TRUE,
     "migration: is_hashed_password is TRUE on the resolved row")
.chk(.is_scrypt_hash(row$password[1]), TRUE,
     "migration: the credentials-path password column now holds a scrypt hash, not plaintext")

# The underlying S3 object itself must also have been upgraded in place.
persisted <- mock_s3readRDS("networks/usuarios.rds", "mock-bucket")
.chk(.is_scrypt_hash(persisted$password_hash[persisted$username == "legacyuser"]), TRUE,
     "migration: the persisted usuarios.rds row was upgraded to a scrypt hash")

# THE critical proof: the original plaintext password still logs in correctly
# through the exact real login function, right after migration.
checker      <- auth_check_credentials()
login_result <- checker("legacyuser", legacy_plain_pw)
.chk(isTRUE(login_result$result), TRUE,
     "migration: the ORIGINAL plaintext password still authenticates successfully after the upgrade — nobody is locked out")

wrong_result <- checker("legacyuser", "totally wrong password")
.chk(isTRUE(wrong_result$result), FALSE,
     "migration: a wrong password is still correctly rejected after the upgrade")

# Re-running the migration on an already-hashed row must be a no-op (no
# double-hashing, no unnecessary S3 write).
raw_after_first_migration <- mock_s3readRDS("networks/usuarios.rds", "mock-bucket")
re_migrated <- .migrate_legacy_passwords(raw_after_first_migration, "networks")
.chk(identical(raw_after_first_migration$password_hash, re_migrated$password_hash), TRUE,
     "migration: re-running on an already-hashed row does not re-hash it")

# ── D. auth_load_usuarios() (the general-purpose loader) also migrates ──────
# _run_saas.R overrides auth_load_usuarios() at the top level with a
# simplified mock (a raw read, no backfills) so OTHER tests in this suite get
# predictable behavior without R/auth.R's side effects — which means calling
# the global `auth_load_usuarios` name here would test that mock, not the
# real function this stage actually changed. Extract the REAL definition
# straight out of R/auth.R's source instead (same technique
# tests/test_pasivos_audit_visibility.R uses), so this test exercises the
# actual code shipped, not the test harness's stand-in for it.
.extract_fn_local <- function(file, fn_name) {
  exprs <- parse(file, keep.source = FALSE)
  for (e in exprs) {
    if (is.call(e) && length(e) >= 2 &&
        identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(fn_name)))
      return(eval(e[[3]], envir = globalenv()))
  }
  stop(sprintf("%s not found in %s", fn_name, file))
}
real_auth_load_usuarios <- .extract_fn_local("R/auth.R", "auth_load_usuarios")

legacy_row2 <- data.frame(
  id = "u-legacy-2", account_code = "U0010", username = "legacyuser2",
  password_hash = "AnotherPlainPw2!", display_name = "Legacy User 2",
  tier = "finance", client_id = "networks", permisos = "{}", group_ids = "[]",
  allowed_clients = "[]", email = NA_character_, requires_password_change = FALSE,
  activo = TRUE, created_at = as.character(Sys.time()), last_login = NA_character_,
  deleted = FALSE, deleted_at = NA_character_, stringsAsFactors = FALSE
)
mock_s3saveRDS(legacy_row2, "networks/usuarios.rds", "mock-bucket")

loaded <- real_auth_load_usuarios(client_id = "networks")
.chk(.is_scrypt_hash(loaded$password_hash[loaded$username == "legacyuser2"]), TRUE,
     "auth_load_usuarios() (real implementation): also transparently migrates legacy plaintext on load")

# ── E. Static scan: every real password-write site hashes before writing ────
# Mirrors tests/test_saas_log_action_scoping.R's approach — cheap, durable,
# catches a future write site that forgets to hash without needing a full
# Shiny testServer harness for each admin flow.

.WRITE_SITES <- list(
  list(file = "app.R", pattern = "usuarios\\$password_hash\\[idx\\]\\s*<-"),
  list(file = "app.R", pattern = "password_hash\\s*=\\s*hash_password\\(pw1\\)"),
  list(file = "R/tiers_module.R", pattern = "password_hash\\s*=\\s*hash_password\\(password\\)"),
  list(file = "R/tiers_module.R", pattern = 'all_u\\[full_idx, "password_hash"\\]\\s*<-'),
  list(file = "scripts/recover_mouse.R", pattern = "raw\\$password_hash\\[mouse_rows\\]\\s*<-"),
  list(file = "scripts/recover_mouse.R", pattern = "password_hash\\s*=\\s*hash_password\\(new_pw\\)"),
  list(file = "scripts/add_bunny.R",     pattern = "password_hash\\s*=\\s*hash_password\\(bunny_pw\\)"),
  list(file = "scripts/seed_hd_admin.R", pattern = "password_hash\\s*=\\s*hash_password\\(mouse_pw\\)"),
  list(file = "scripts/seed_hd_admin.R", pattern = 'password_hash\\s*=\\s*hash_password\\("PBunny129!"\\)')
)

for (site in .WRITE_SITES) {
  txt   <- readLines(site$file, warn = FALSE)
  lines <- grep(site$pattern, txt)
  label <- sprintf("%s: matches %s", site$file, site$pattern)
  .chk(length(lines) >= 1L, TRUE, label)
  if (length(lines)) {
    # The assignment-target patterns above are deliberately loose (they match
    # the LHS regardless of what's assigned) so this second check catches a
    # future regression back to a raw plaintext assignment on the same line.
    line_txt   <- txt[lines[1]]
    calls_hash <- grepl("hash_password\\(", line_txt)
    .chk(calls_hash, TRUE,
         sprintf("%s:%d hashes before writing (no raw plaintext assignment)",
                 site$file, lines[1]))
  }
}

# Guard against this whole scan silently checking nothing (e.g. every
# pattern failing to match due to a future refactor renaming variables).
.total_write_site_matches <- sum(vapply(.WRITE_SITES, function(site) {
  length(grep(site$pattern, readLines(site$file, warn = FALSE)))
}, integer(1)))
.chk(.total_write_site_matches >= length(.WRITE_SITES), TRUE,
     "at least one match per write site was found (guard against this scan silently checking nothing)")

cat("\n")
