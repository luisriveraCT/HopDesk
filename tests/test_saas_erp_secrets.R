# =============================================================================
# tests/test_saas_erp_secrets.R
# Stage 3: encrypt_secret()/decrypt_secret() round-trip and tamper detection.
# Sourced by _run_saas.R — do not source directly.
# =============================================================================

cat("── ERP Secret Encryption (AES-256-GCM + HMAC-SHA256) ───────────────────\n")

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

# ── A. Round-trip ─────────────────────────────────────────────────────────────

secret <- list(user = "svc_ng", password = "S3cr3t!#$%^&*()")
ct     <- encrypt_secret(secret)

.chk(is.character(ct) && length(ct) == 1L, TRUE, "encrypt_secret returns a single character blob")
.chk(grepl("S3cr3t", ct, fixed = TRUE), FALSE, "ciphertext does not contain the plaintext password")

dec <- decrypt_secret(ct)
.chk(dec$user,     secret$user,     "decrypt_secret round-trip: user matches")
.chk(dec$password, secret$password, "decrypt_secret round-trip: password matches")

# Two encryptions of the same secret must not produce the same ciphertext
# (fresh random IV each time) — a basic sanity check against ECB-style reuse.
ct2 <- encrypt_secret(secret)
.chk(identical(ct, ct2), FALSE, "encrypting the same secret twice yields different ciphertext (fresh IV)")
.chk(decrypt_secret(ct2)$password, secret$password, "second ciphertext still decrypts correctly")

# ── B. Tamper detection ───────────────────────────────────────────────────────

raw_ct <- openssl::base64_decode(ct)
tampered_raw <- raw_ct
last <- length(tampered_raw)
tampered_raw[last] <- as.raw((as.integer(tampered_raw[last]) + 1L) %% 256L)
tampered_ct <- openssl::base64_encode(tampered_raw)

.chk_throws(function() decrypt_secret(tampered_ct),
            "decrypt_secret throws on a single tampered byte (tail)")

tampered_raw2 <- raw_ct
tampered_raw2[1] <- as.raw((as.integer(tampered_raw2[1]) + 1L) %% 256L)
tampered_ct2 <- openssl::base64_encode(tampered_raw2)
.chk_throws(function() decrypt_secret(tampered_ct2),
            "decrypt_secret throws on a single tampered byte (head — inside the IV)")

.chk_throws(function() decrypt_secret(openssl::base64_encode(as.raw(1:10))),
            "decrypt_secret throws on a too-short/malformed blob")

# ── C. Wrong key ───────────────────────────────────────────────────────────────

old_key <- Sys.getenv("HOPDESK_SECRETS_KEY")
Sys.setenv(HOPDESK_SECRETS_KEY = openssl::base64_encode(openssl::rand_bytes(32)))
.chk_throws(function() decrypt_secret(ct), "decrypt_secret throws when the master key doesn't match")
Sys.setenv(HOPDESK_SECRETS_KEY = old_key)

# Confirm the original key still works after restoring it (sanity on the test itself)
.chk(decrypt_secret(ct)$user, secret$user, "decrypt_secret works again once the correct key is restored")

# ── D. Missing key ─────────────────────────────────────────────────────────────

Sys.unsetenv("HOPDESK_SECRETS_KEY")
.chk_throws(function() encrypt_secret(list(a = "b")), "encrypt_secret throws when HOPDESK_SECRETS_KEY is unset")
Sys.setenv(HOPDESK_SECRETS_KEY = old_key)

cat("\n")
