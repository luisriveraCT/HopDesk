# =============================================================================
# tests/test_lower_urgency_hardening.R
# Regression tests for the 4 "lower urgency" findings from the same multi-pass
# code review (2026-08): an S3 "missing key" cache with no TTL (stale forever
# across processes), a debounce-free Bancos manual-entry save that could
# double-insert on a double-click, a scrypt-hash-detection heuristic that only
# checked the magic-header prefix (not length), and a hopdesk viewer able to
# see and silently demote a principal-tier account via the users editor.
#
# Sourced by _run_saas.R.
# =============================================================================

cat("── Lower-urgency hardening (post-review) ───────────────────────────────\n")

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

# ── A. persistence.R: .s3_missing_cache TTL bounds cross-process staleness ──
# (e.g. a value created by another process/replica after this one cached it
# as "missing" -- the exact same class of bug already fixed for the auth
# credential cache.)
{
  Sys.setenv(CLIENT_ID = "networks")
  full_key <- "networks/ghost_key_for_test.rds"

  # Nothing has ever been written under this key -- .s3_read() should miss
  # and record it, without throwing.
  first <- .s3_read("ghost_key_for_test.rds")
  .chk(is.null(first), TRUE, ".s3_read(): a nonexistent key returns NULL (not an error)")
  .chk(.s3_missing_cache_hit(full_key), TRUE,
       ".s3_read(): the miss is recorded in .s3_missing_cache")

  # Simulate an EXTERNAL process creating the object immediately afterward --
  # this process has no way to be told directly.
  mock_s3saveRDS(list(created = "externally"), full_key, "mock-bucket")

  # Still within the TTL: the (now stale) miss is still served -- documented,
  # bounded staleness, not zero staleness (same tradeoff as the auth cache).
  still_missing <- .s3_read("ghost_key_for_test.rds")
  .chk(is.null(still_missing), TRUE,
       "within the TTL window, the cache still reports missing (expected -- bounded staleness)")

  # Simulate TTL expiry by backdating the recorded timestamp instead of
  # sleeping (deterministic, no real-time wait in the test suite).
  .s3_missing_cache[[full_key]] <- proc.time()[["elapsed"]] - (.S3_MISSING_CACHE_TTL + 5)
  .chk(.s3_missing_cache_hit(full_key), FALSE,
       "after the TTL elapses, the stale miss entry expires")

  found <- .s3_read("ghost_key_for_test.rds")
  .chk(identical(found$created, "externally"), TRUE,
       "after the TTL elapses, .s3_read() picks up the externally-created object automatically")
}

# ── B. persistence.R: a genuine write still invalidates the missing-cache
# immediately, regardless of TTL (no regression from the TTL change) ────────
{
  full_key <- "networks/ghost_key_for_test_2.rds"
  miss <- .s3_read("ghost_key_for_test_2.rds")
  .chk(is.null(miss), TRUE, ".s3_read(): second ghost key also misses cleanly")
  .chk(.s3_missing_cache_hit(full_key), TRUE, "second ghost key is recorded as missing")

  .s3_write(list(created = "same-process"), "ghost_key_for_test_2.rds", client_id = "networks")
  .chk(.s3_missing_cache_hit(full_key), FALSE,
       ".s3_write(): a same-process write still invalidates the missing-cache immediately (no TTL wait needed)")
}

# ── C. auth.R: .is_scrypt_hash() checks length, not just the magic prefix ───
{
  real_hash <- hash_password("Whatever123!")
  .chk(nchar(real_hash), 128L, "sanity: a real scrypt hash is 128 chars")
  .chk(.is_scrypt_hash(real_hash), TRUE, ".is_scrypt_hash(): a real hash is still detected correctly")

  # The exact misclassification scenario: a plaintext password that happens
  # to start with the magic header but isn't a real 128-char hash.
  fake_short <- "c2NyeXB0"
  .chk(nchar(fake_short) == 128L, FALSE, "sanity: the adversarial string is NOT 128 chars")
  .chk(.is_scrypt_hash(fake_short), FALSE,
       ".is_scrypt_hash(): a short string sharing only the magic prefix is correctly rejected as NOT a hash (previously misclassified as one)")

  .chk(.is_scrypt_hash("plainpassword"), FALSE, ".is_scrypt_hash(): ordinary legacy plaintext is still rejected (no regression)")
  .chk(.is_scrypt_hash(NA_character_), FALSE, ".is_scrypt_hash(): NA is still rejected (no regression)")

  # End-to-end: .migrate_legacy_passwords() must actually re-hash the
  # adversarial value instead of leaving it alone as "already hashed".
  raw <- data.frame(username = "ghostuser", password_hash = fake_short, stringsAsFactors = FALSE)
  migrated <- .migrate_legacy_passwords(raw, "networks")
  .chk(.is_scrypt_hash(migrated$password_hash[1]), TRUE,
       ".migrate_legacy_passwords(): the adversarial-prefix value is now correctly migrated to a real hash instead of staying misclassified forever")
}

# ── D. tiers_module.R: hopdesk viewers can no longer see/edit principal rows ─
# (previously usuarios_rv() only excluded "principal" tier rows for viewers
# who were ALSO not hopdesk -- so a hopdesk admin could open a principal
# account's editor, whose tier <select> doesn't offer "Principal" and
# silently defaults to "hopdesk" on save, demoting the account as a side
# effect of e.g. an unrelated password reset.)
{
  txt   <- readLines("R/tiers_module.R", warn = FALSE)
  start <- grep("^\\s*usuarios_rv <- reactive\\(\\{", txt)[1]
  block <- paste(txt[start:min(start + 20, length(txt))], collapse = "\n")

  .chk(grepl('if\\s*\\(!is_principal\\(\\)\\s*&&\\s*nrow\\(df\\)\\s*>\\s*0\\)\\s*\\n\\s*df\\s*<-\\s*df\\[df\\$tier != "principal"',
             block),
       TRUE,
       "usuarios_rv(): the principal-tier filter applies to ALL non-principal viewers, unconditionally on hopdesk status")

  .chk(grepl('!is_principal\\(\\)\\s*&&\\s*!is_hopdesk\\(\\)\\s*&&\\s*nrow\\(df\\)\\s*>\\s*0\\)\\s*\\n\\s*df\\s*<-\\s*df\\[df\\$tier != "principal"',
             block),
       FALSE,
       "usuarios_rv(): hopdesk is no longer exempted from the principal-tier filter (the actual bug)")
}

# ── E. bancos_module.R: manual-entry save is now debounced against a
# double-click, mirroring the exact shinyjs::disable()/on.exit(enable()) idiom
# already used by pwiz_save/me_save_as_provision/ppm_save/cuenta_save
# elsewhere in this codebase (previously do_add_manual had no such guard, so
# a genuine double-click before removeModal() fired created two rows with two
# distinct UUIDs instead of erroring or deduping).
{
  txt   <- readLines("R/bancos_module.R", warn = FALSE)
  start <- grep("observeEvent\\(input\\$do_add_manual,", txt)[1]
  block <- paste(txt[start:min(start + 5, length(txt))], collapse = "\n")
  .chk(grepl('shinyjs::disable\\("do_add_manual"\\)', block), TRUE,
       "R/bancos_module.R: do_add_manual disables its own button on entry (blocks a double-click double-insert)")
  .chk(grepl('on\\.exit\\(shinyjs::enable\\("do_add_manual"\\)', block), TRUE,
       "R/bancos_module.R: do_add_manual re-enables the button via on.exit (survives early returns, not just the happy path)")
}

cat("\n")
