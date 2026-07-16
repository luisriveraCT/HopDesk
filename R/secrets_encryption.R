# =============================================================================
# R/secrets_encryption.R
# Encrypts ERP connection secrets (Stage 3) before they ever reach S3.
#
# Cipher: AES-256-GCM (via the `openssl` package) for confidentiality, plus
# an explicit HMAC-SHA256 (encrypt-then-MAC) for authentication.
#
# NOTE on a real gap found while building this: openssl's R binding for
# aes_gcm_encrypt()/aes_gcm_decrypt() does not expose or verify the GCM
# authentication tag — empirically, decrypting a byte-tampered ciphertext
# through aes_gcm_decrypt() alone succeeds silently with no error. Without
# the HMAC layer below, "AES-256-GCM" here would give no more tamper
# detection than plain CBC, contradicting the design's tamper-detection
# requirement. The HMAC-SHA256 (keyed with its own subkey, §below) is what
# actually delivers "tampering causes a loud failure" — flagged to Mouse,
# not a silent deviation.
#
# Master key: HOPDESK_SECRETS_KEY env var — 32 random bytes, base64-encoded.
# Generate one with:  openssl rand -base64 32
# Add the result to .Renviron as HOPDESK_SECRETS_KEY=<value>.
#
# Upgrade path (not needed yet, noted for later): move from one shared
# HOPDESK_SECRETS_KEY to a per-client key (derived or stored in AWS KMS),
# so compromising the one key doesn't expose every client's secrets at
# once. Revisit if/when a client's contract requires it, or once there are
# enough clients that blast radius becomes a real concern.
# =============================================================================

.secrets_master_key <- function() {
  key_b64 <- Sys.getenv("HOPDESK_SECRETS_KEY")
  if (!nzchar(key_b64)) {
    stop("HOPDESK_SECRETS_KEY is not set — generate one with `openssl rand -base64 32` ",
         "and add it to .Renviron")
  }
  key <- openssl::base64_decode(key_b64)
  if (length(key) != 32L) {
    stop("HOPDESK_SECRETS_KEY must decode to exactly 32 raw bytes (AES-256) — got ", length(key))
  }
  key
}

# Two independent subkeys derived from the one master key, so the AES key
# and the HMAC key are never the same bytes.
.secrets_enc_key <- function(master_key) openssl::sha256(c(as.raw(0x01), master_key))
.secrets_mac_key <- function(master_key) openssl::sha256(c(as.raw(0x02), master_key))

#' Encrypt a named list of secret fields into a single base64 ciphertext blob.
encrypt_secret <- function(named_list) {
  master    <- .secrets_master_key()
  plaintext <- charToRaw(jsonlite::toJSON(named_list, auto_unbox = TRUE))
  iv        <- openssl::rand_bytes(12)
  enc       <- openssl::aes_gcm_encrypt(plaintext, key = .secrets_enc_key(master), iv = iv)
  mac       <- as.raw(openssl::sha256(c(iv, enc), key = .secrets_mac_key(master)))
  openssl::base64_encode(c(iv, mac, enc))
}

#' Decrypt a base64 ciphertext blob (from encrypt_secret()) back into the
#' original named list. Fails loudly via stop() if the blob is malformed,
#' truncated, or has been tampered with — never returns garbage silently.
decrypt_secret <- function(ciphertext) {
  master <- .secrets_master_key()
  packed <- openssl::base64_decode(ciphertext)
  if (length(packed) < 12L + 32L) {
    stop("decrypt_secret: ciphertext too short to be valid — corrupted or truncated")
  }
  iv  <- packed[1:12]
  mac <- packed[13:44]
  enc <- packed[-(1:44)]

  expected_mac <- as.raw(openssl::sha256(c(iv, enc), key = .secrets_mac_key(master)))
  if (!identical(mac, expected_mac)) {
    stop("decrypt_secret: authentication check failed — ciphertext has been tampered with or corrupted")
  }

  dec <- openssl::aes_gcm_decrypt(enc, key = .secrets_enc_key(master), iv = iv)
  jsonlite::fromJSON(rawToChar(dec))
}
