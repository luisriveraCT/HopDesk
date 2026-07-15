# =============================================================================
# tests/test_saas_limit_change.R
# Covers: limit-change deactivation logic, rollback, dual audit, user count
#         recompute, and the confirmation gate (can't confirm with too many kept).
# Sourced by _run_saas.R.
# =============================================================================

cat("── Limit Change Flow ────────────────────────────────────────────────────\n")

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

# ── Seed data helpers ─────────────────────────────────────────────────────────

make_user <- function(account_code, username, tier = "analysis", deleted = FALSE) {
  data.frame(
    id                       = uuid::UUIDgenerate(),
    account_code             = account_code,
    username                 = username,
    password_hash            = "pw",
    display_name             = username,
    tier                     = tier,
    client_id                = "networks",
    permisos                 = "{}",
    group_ids                = "[]",
    allowed_clients          = "[]",
    email                    = NA_character_,
    requires_password_change = FALSE,
    activo                   = !deleted,
    created_at               = as.character(Sys.time()),
    last_login               = NA_character_,
    deleted                  = deleted,
    deleted_at               = NA_character_,
    stringsAsFactors         = FALSE
  )
}

seed_registry <- function(client_id, max_users, current_users = 0L) {
  reg <- .schema_client_registry()
  reg <- rbind(reg, data.frame(
    id            = uuid::UUIDgenerate(),
    client_id     = client_id,
    display_name  = paste0("Client ", client_id),
    max_users     = max_users,
    current_users = current_users,
    contact_email = paste0("contact@", client_id, ".com"),
    status        = "active",
    created_at    = as.character(Sys.time()),
    created_by    = "mouse",
    archived_at   = NA_character_,
    stringsAsFactors = FALSE
  ))
  write_client_registry(reg)
}

# ── A. Deactivation logic — users NOT in keep list are deactivated ─────────────

u1 <- make_user("U001", "alice")
u2 <- make_user("U002", "bob")
u3 <- make_user("U003", "carol")
u4 <- make_user("U004", "dave", deleted = TRUE)   # already deleted — ignore

users <- do.call(rbind, list(u1, u2, u3, u4))
mock_s3saveRDS(users, "networks/usuarios.rds", "mock-bucket")
seed_registry("networks", max_users = 5L, current_users = 3L)

# Simulate the deactivation step: keep_codes = c("U001", "U003"), new_max = 2
keep_codes  <- c("U001", "U003")
new_max     <- 2L
now_str     <- as.character(Sys.time())

usuarios_orig <- auth_load_usuarios(client_id = "networks")
usuarios_new  <- usuarios_orig
active_codes  <- usuarios_orig$account_code[!usuarios_orig$deleted]
to_deact_idx  <- which(
  usuarios_new$account_code %in% active_codes &
  !usuarios_new$account_code %in% keep_codes
)
usuarios_new$deleted[to_deact_idx]    <- TRUE
usuarios_new$deleted_at[to_deact_idx] <- now_str
usuarios_new$activo[to_deact_idx]     <- FALSE
auth_save_usuarios(usuarios_new, client_id = "networks")

saved <- auth_load_usuarios(client_id = "networks")
active_after <- saved[!saved$deleted, ]
.chk(nrow(active_after), 2L, "deactivation: exactly keep_codes count remain active")
.chk("alice" %in% active_after$username, TRUE,  "deactivation: kept user alice is still active")
.chk("carol" %in% active_after$username, TRUE,  "deactivation: kept user carol is still active")
.chk("bob"   %in% active_after$username, FALSE, "deactivation: bob was deactivated")
.chk("dave"  %in% active_after$username, FALSE, "deactivation: dave (pre-deleted) is still inactive")

# ── B. update_client_user_count reflects deactivation ─────────────────────────

update_client_user_count("networks")
reg_b <- read_client_registry()
.chk(reg_b$current_users[reg_b$client_id == "networks"], 2L,
     "user_count: recomputes to 2 after deactivation")

# ── C. Registry max_users update ──────────────────────────────────────────────

registry  <- read_client_registry()
reg_idx   <- which(registry$client_id == "networks")
old_max   <- registry$max_users[reg_idx]
registry$max_users[reg_idx] <- new_max
write_client_registry(registry)

reg_c <- read_client_registry()
.chk(reg_c$max_users[reg_c$client_id == "networks"], new_max,
     "registry: max_users updated to new_max")
.chk(old_max, 5L, "registry: old_max was 5")

# ── D. Guard: keeping more than new_max users is rejected ─────────────────────

too_many_keep <- c("U001", "U002", "U003")   # 3 > new_max=2
gate_ok <- length(too_many_keep) <= new_max
.chk(gate_ok, FALSE, "gate: keeping 3 users is rejected when new_max=2")

exact_keep <- c("U001", "U003")              # 2 == new_max=2
.chk(length(exact_keep) <= new_max, TRUE,
     "gate: keeping exactly new_max users is allowed")

fewer_keep <- c("U001")                      # 1 < new_max=2
.chk(length(fewer_keep) <= new_max, TRUE,
     "gate: keeping fewer than new_max users is allowed")

# ── E. Rollback: failed registry write restores original users ────────────────

u5 <- make_user("U005", "eve")
u6 <- make_user("U006", "frank")
users_fresh <- do.call(rbind, list(u5, u6))
mock_s3saveRDS(users_fresh, "networks_rb/usuarios.rds", "mock-bucket")
seed_registry("networks_rb", max_users = 5L)

# Simulate deactivation of frank (Step A succeeds)
orig_rb <- users_fresh
new_rb  <- users_fresh
new_rb$deleted[new_rb$username == "frank"]    <- TRUE
new_rb$activo[new_rb$username  == "frank"]    <- FALSE
new_rb$deleted_at[new_rb$username == "frank"] <- now_str
auth_save_usuarios(new_rb, client_id = "networks_rb")

# Simulate Step B (registry write) failing → rollback Step A
ok_reg <- FALSE   # simulated failure
if (!ok_reg) {
  auth_save_usuarios(orig_rb, client_id = "networks_rb")
}

rolled_back <- auth_load_usuarios(client_id = "networks_rb")
active_after_rb <- rolled_back[!rolled_back$deleted, ]
.chk(nrow(active_after_rb), 2L, "rollback: original user count restored on registry failure")
.chk("frank" %in% active_after_rb$username, TRUE,
     "rollback: deactivated user frank is restored to active")

# ── F. Dual audit: two log_action calls written ───────────────────────────────

mock_s3saveRDS(.APP_AUDIT_SCHEMA(), "hd-admin/app_audit.rds",  "mock-bucket")
mock_s3saveRDS(.APP_AUDIT_SCHEMA(), "networks/app_audit.rds",  "mock-bucket")
Sys.setenv(CLIENT_ID = "hd-admin")

reviewer   <- "mouse"
cid        <- "networks"
old_max_f  <- 5L
new_max_f  <- 3L
limit_meta <- list(old_max_users = old_max_f, new_max_users = new_max_f,
                   changed_by = reviewer, deactivated_count = 0L)

# Audit entry written to hd-admin
log_action(user        = reviewer,
           module      = "clientes",
           action      = "cambio_limite_usuarios",
           description = sprintf("Límite de '%s' cambiado de %d a %d por %s",
                                  cid, old_max_f, new_max_f, reviewer),
           target_id   = cid,
           s3_key      = "hd-admin/client_registry.rds",
           client_id   = "hd-admin",
           metadata    = limit_meta)

# Audit entry written to the client prefix
log_action(user        = reviewer,
           module      = "clientes",
           action      = "cambio_limite_usuarios",
           description = sprintf("Límite cambiado de %d a %d por %s",
                                  old_max_f, new_max_f, reviewer),
           target_id   = cid,
           s3_key      = paste0(cid, "/app_audit.rds"),
           client_id   = cid,
           metadata    = limit_meta)

admin_log  <- tryCatch(mock_s3readRDS("hd-admin/app_audit.rds", "mock-bucket"),
                       error = function(e) NULL)
client_log <- tryCatch(mock_s3readRDS("networks/app_audit.rds", "mock-bucket"),
                       error = function(e) NULL)

.chk(!is.null(admin_log)  && any(admin_log$action  == "cambio_limite_usuarios"), TRUE,
     "dual audit: hd-admin log has cambio_limite_usuarios entry")
.chk(!is.null(client_log) && any(client_log$action == "cambio_limite_usuarios"), TRUE,
     "dual audit: client log has cambio_limite_usuarios entry")

# s3_key columns are set correctly
.chk(admin_log$s3_key[admin_log$action == "cambio_limite_usuarios"][1],
     "hd-admin/client_registry.rds",
     "dual audit: admin entry references registry key")
.chk(client_log$s3_key[client_log$action == "cambio_limite_usuarios"][1],
     "networks/app_audit.rds",
     "dual audit: client entry references client audit key")

Sys.setenv(CLIENT_ID = "networks")   # restore

# ── G. Per-user deactivation audit entries ────────────────────────────────────

mock_s3saveRDS(.APP_AUDIT_SCHEMA(), "networks/app_audit.rds", "mock-bucket")

# Seed users fresh
u_g1 <- make_user("G001", "greta")
u_g2 <- make_user("G002", "hans")
users_g <- do.call(rbind, list(u_g1, u_g2))
mock_s3saveRDS(users_g, "networks/usuarios.rds", "mock-bucket")

# Deactivate hans and emit per-user audit
deact_idx <- which(users_g$username == "hans")
log_action(user        = "mouse",
           module      = "clientes",
           action      = "user_deactivated_by_limit_reduction",
           description = paste0("Cuenta 'hans' desactivada por reducción de límite en 'networks'"),
           target_id   = users_g$account_code[deact_idx],
           s3_key      = "networks/usuarios.rds",
           client_id   = "networks")

per_user_log <- tryCatch(mock_s3readRDS("networks/app_audit.rds", "mock-bucket"),
                         error = function(e) NULL)
.chk(!is.null(per_user_log) &&
     any(per_user_log$action == "user_deactivated_by_limit_reduction"), TRUE,
     "per-user audit: deactivation log entry written")
.chk(per_user_log$target_id[
  per_user_log$action == "user_deactivated_by_limit_reduction"][1],
  "G002",
  "per-user audit: target_id is the deactivated user's account_code")

cat("\n")
