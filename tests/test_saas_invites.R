# =============================================================================
# tests/test_saas_invites.R
# Verifies invite token lifecycle: create, resolve, expire, revoke, accept.
# Sourced by _run_saas.R.
# =============================================================================

cat("── Invite Token Lifecycle ───────────────────────────────────────────────\n")

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

# Seed an empty invites store
mock_s3saveRDS(.schema_pending_invites(), "hd-admin/pending_invites.rds", "mock-bucket")

# ── A. Create + resolve a valid invite ────────────────────────────────────────

tok_a <- create_invite(
  email        = "alice@test.com",
  display_name = "Alice T",
  client_id    = "networks",
  tier         = "analysis",
  invited_by   = "mouse"
)
.chk(is.character(tok_a) && nzchar(tok_a), TRUE, "create_invite returns non-empty token")

row_a <- resolve_invite(tok_a)
.chk(!is.null(row_a),           TRUE,          "valid token resolves to a row")
.chk(row_a$email,               "alice@test.com", "resolved row has correct email")
.chk(row_a$tier,                "analysis",    "resolved row has correct tier")
.chk(row_a$client_id,           "networks",    "resolved row has correct client_id")
.chk(row_a$status,              "pending",     "resolved row status is pending")

# ── B. Resolve a non-existent token returns NULL ──────────────────────────────

bad_row <- resolve_invite("00000000000000000000000000000000")
.chk(is.null(bad_row), TRUE, "non-existent token returns NULL")

# ── C. Revoked / killed token returns NULL ────────────────────────────────────

tok_b <- create_invite("bob@test.com", "Bob", "networks", "finance", "mouse")
invites <- read_pending_invites()
invites$status[invites$token == tok_b] <- "revoked"
write_pending_invites(invites)

row_b <- resolve_invite(tok_b)
.chk(is.null(row_b), TRUE, "revoked token returns NULL")

# ── D. Already-accepted token returns NULL ────────────────────────────────────

tok_c <- create_invite("carol@test.com", "Carol", "networks", "admin", "mouse")
accept_invite(tok_c)
row_c <- resolve_invite(tok_c)
.chk(is.null(row_c), TRUE, "accepted token returns NULL on re-resolve")

# Confirm accepted_at was stamped
inv_after <- read_pending_invites()
c_row <- inv_after[inv_after$token == tok_c, , drop = FALSE]
.chk(c_row$status[1], "accepted", "accepted token has status=accepted in store")
.chk(!is.na(c_row$accepted_at[1]), TRUE, "accepted token has accepted_at stamped")

# ── E. Expired token returns NULL and writes expired status ──────────────────

tok_d <- create_invite("dan@test.com", "Dan", "networks", "analysis", "mouse",
                        expires_hours = -1)   # already expired
row_d <- resolve_invite(tok_d)
.chk(is.null(row_d), TRUE, "expired token returns NULL")

inv_exp <- read_pending_invites()
d_row <- inv_exp[inv_exp$token == tok_d, , drop = FALSE]
.chk(d_row$status[1], "expired", "expired token status written back as 'expired'")

# ── F. Multiple pending invites — correct one is resolved ─────────────────────

tok_e1 <- create_invite("eve1@test.com", "Eve1", "hopdesk", "dev",   "mouse")
tok_e2 <- create_invite("eve2@test.com", "Eve2", "hopdesk", "admin", "mouse")

r_e1 <- resolve_invite(tok_e1)
r_e2 <- resolve_invite(tok_e2)
.chk(!is.null(r_e1) && r_e1$email == "eve1@test.com", TRUE, "first of two pending tokens resolves correctly")
.chk(!is.null(r_e2) && r_e2$email == "eve2@test.com", TRUE, "second of two pending tokens resolves correctly")

# ── G. User limit: confirm count update mechanics ─────────────────────────────
# Seed registry
reg <- .schema_client_registry()
reg <- rbind(reg, data.frame(
  id           = uuid::UUIDgenerate(),
  client_id    = "networks",
  display_name = "Networks Group",
  max_users    = 10L,
  current_users = 0L,
  contact_email = "contact@networks.com",
  status       = "active",
  created_at   = as.character(Sys.time()),
  created_by   = "mouse",
  archived_at  = NA_character_,
  stringsAsFactors = FALSE
))
mock_s3saveRDS(reg, "hd-admin/client_registry.rds", "mock-bucket")

# Seed 3 active users in networks  (build schema inline — no named helper)
u3 <- data.frame(
  id=character(), account_code=character(), username=character(),
  password_hash=character(), display_name=character(), tier=character(),
  client_id=character(), permisos=character(), group_ids=character(),
  allowed_clients=character(), email=character(),
  requires_password_change=logical(), activo=logical(), created_at=character(),
  last_login=character(), deleted=logical(), deleted_at=character(),
  stringsAsFactors=FALSE
)
for (i in 1:3) {
  u3 <- rbind(u3, data.frame(
    id                       = uuid::UUIDgenerate(),
    account_code             = paste0("U000", i),
    username                 = paste0("user", i),
    password_hash            = "pw",
    display_name             = paste0("User ", i),
    tier                     = "analysis",
    client_id                = "networks",
    permisos                 = "{}",
    group_ids                = "[]",
    allowed_clients          = "[]",
    email                    = NA_character_,
    requires_password_change = FALSE,
    activo                   = TRUE,
    created_at               = as.character(Sys.time()),
    last_login               = NA_character_,
    deleted                  = FALSE,
    deleted_at               = NA_character_,
    stringsAsFactors         = FALSE
  ))
}
mock_s3saveRDS(u3, "networks/usuarios.rds", "mock-bucket")

update_client_user_count("networks")
reg_after <- read_client_registry()
.chk(reg_after$current_users[reg_after$client_id == "networks"],
     3L, "update_client_user_count reflects 3 active users")

# Soft-delete one user, recount
u3_del <- u3
u3_del$deleted[1] <- TRUE
mock_s3saveRDS(u3_del, "networks/usuarios.rds", "mock-bucket")
update_client_user_count("networks")
reg_after2 <- read_client_registry()
.chk(reg_after2$current_users[reg_after2$client_id == "networks"],
     2L, "update_client_user_count drops deleted user from count")

cat("\n")
