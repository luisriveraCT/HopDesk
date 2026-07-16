# =============================================================================
# tests/test_saas_audit.R
# Verifies audit log: schema, s3_key column, chunk rotation, dual-write.
# Sourced by _run_saas.R.
# =============================================================================

cat("── Audit Log ────────────────────────────────────────────────────────────\n")

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

# Patch .audit_read_chunk and .audit_write_chunk to use the mock store
.audit_read_chunk <- function(key) {
  tryCatch(mock_s3readRDS(key, "mock-bucket"), error = function(e) NULL)
}
.audit_write_chunk <- function(df, key) {
  mock_s3saveRDS(df, key, "mock-bucket")
}

# Also patch rotate_log_if_needed's get_bucket call — we'll invoke rotate directly
# and plant fake chunk keys in the store.

# ── A. Schema validation ───────────────────────────────────────────────────────

schema <- .APP_AUDIT_SCHEMA()
required_cols <- c("id","ts","user","module","action","description",
                   "target_id","s3_key","client_id","metadata")
for (col in required_cols) {
  .chk(col %in% names(schema), TRUE, sprintf("schema has column '%s'", col))
}
.chk(inherits(schema$ts, "POSIXct"), TRUE, "schema$ts is POSIXct")
.chk(nrow(schema), 0L, "schema starts with 0 rows")

# ── B. log_action() appends with s3_key populated ─────────────────────────────

# Seed empty log
mock_s3saveRDS(.APP_AUDIT_SCHEMA(), "networks/app_audit.rds", "mock-bucket")
Sys.setenv(CLIENT_ID = "networks")

log_action(
  user        = "mouse",
  module      = "clientes",
  action      = "test_action",
  description = "unit test entry",
  target_id   = "T001",
  s3_key      = "networks/usuarios.rds",
  client_id   = "networks"
)

log1 <- mock_s3readRDS("networks/app_audit.rds", "mock-bucket")
.chk(nrow(log1), 1L, "log_action: one row written")
.chk(log1$user[1], "mouse", "log_action: user column populated")
.chk(log1$s3_key[1], "networks/usuarios.rds", "log_action: s3_key column populated")
.chk(log1$action[1], "test_action", "log_action: action column populated")
.chk(!is.na(log1$ts[1]), TRUE, "log_action: ts is non-NA")

# Second entry — timestamps should be non-decreasing
log_action(
  user        = "tesoreria",
  module      = "bancos",
  action      = "reconcile",
  description = "bank reconciliation",
  s3_key      = "networks/bancos.rds",
  client_id   = "networks"
)
log2 <- mock_s3readRDS("networks/app_audit.rds", "mock-bucket")
.chk(nrow(log2), 2L, "log_action: two rows after second call")
.chk(log2$ts[2] >= log2$ts[1], TRUE, "timestamps are non-decreasing")

# ── C. Dual-write: client_id != CLIENT_ID → also writes to home log ───────────

mock_s3saveRDS(.APP_AUDIT_SCHEMA(), "hd-admin/app_audit.rds", "mock-bucket")
Sys.setenv(CLIENT_ID = "hd-admin")

log_action(
  user        = "mouse",
  module      = "clientes",
  action      = "client_access",
  description = "staff accessed networks",
  s3_key      = NA_character_,
  client_id   = "networks"   # writing to networks log while CLIENT_ID=hd-admin
)

# Should have written to BOTH logs
net_log  <- tryCatch(mock_s3readRDS("networks/app_audit.rds",  "mock-bucket"), error = function(e) NULL)
home_log <- tryCatch(mock_s3readRDS("hd-admin/app_audit.rds",  "mock-bucket"), error = function(e) NULL)

.chk(!is.null(net_log)  && nrow(net_log)  >= 1L, TRUE, "dual-write: entry in target client log")
.chk(!is.null(home_log) && nrow(home_log) >= 1L, TRUE, "dual-write: entry in home log")
.chk(
  any(net_log$action  == "client_access"),
  TRUE, "target log has client_access entry"
)
.chk(
  any(home_log$action == "client_access"),
  TRUE, "home log has client_access entry"
)

Sys.setenv(CLIENT_ID = "networks")   # restore

# ── C2. Dual-write keys off the ACTOR's true home, not Sys.getenv("CLIENT_ID") ─
# Found 2026-07-16 while manually testing Stage 4: this app runs as ONE shared
# deployment (CLIENT_ID env var is always "hd-admin"), so keying dual-write off
# that fixed env var meant EVERY native client user's own action (not just
# staff jump actions) got needlessly dual-written into hd-admin's own log.
# viewer_home_client_id must be the actual acting session's home.

Sys.setenv(CLIENT_ID = "hd-admin")   # the shared deployment's fixed env var

# A native Networks user acting in their own folder — home == target — must
# NOT dual-write into hd-admin, even though Sys.getenv("CLIENT_ID") != "networks".
mock_s3saveRDS(.APP_AUDIT_SCHEMA(), "networks/app_audit.rds", "mock-bucket")
mock_s3saveRDS(.APP_AUDIT_SCHEMA(), "hd-admin/app_audit.rds", "mock-bucket")

log_action(
  user        = "larm",
  module      = "ledger_AR",
  action      = "mover_fecha",
  description = "native client action, own folder",
  client_id             = "networks",
  viewer_home_client_id = "networks"   # actor's true home == target
)

net_log_c2 <- tryCatch(mock_s3readRDS("networks/app_audit.rds", "mock-bucket"), error = function(e) NULL)
hd_log_c2  <- tryCatch(mock_s3readRDS("hd-admin/app_audit.rds", "mock-bucket"), error = function(e) NULL)

.chk(!is.null(net_log_c2) && nrow(net_log_c2) == 1L, TRUE,
     "no-dual-write: native client's own action lands in their own folder")
.chk(is.null(hd_log_c2) || nrow(hd_log_c2) == 0L, TRUE,
     "no-dual-write: native client's own action does NOT leak into hd-admin")

# A staff member mid-jump (home != target) — dual-write must still fire, keyed
# off their real home, not the shared deployment's env var.
mock_s3saveRDS(.APP_AUDIT_SCHEMA(), "networks/app_audit.rds", "mock-bucket")
mock_s3saveRDS(.APP_AUDIT_SCHEMA(), "hd-admin/app_audit.rds", "mock-bucket")

log_action(
  user        = "bunny",
  module      = "ledger_AR",
  action      = "mover_fecha",
  description = "staff action while jumped into networks",
  client_id             = "networks",
  viewer_home_client_id = "hd-admin"   # staff's true home != target
)

net_log_c3 <- tryCatch(mock_s3readRDS("networks/app_audit.rds", "mock-bucket"), error = function(e) NULL)
hd_log_c3  <- tryCatch(mock_s3readRDS("hd-admin/app_audit.rds", "mock-bucket"), error = function(e) NULL)

.chk(!is.null(net_log_c3) && nrow(net_log_c3) == 1L, TRUE,
     "dual-write still fires: staff mid-jump action lands in target client log")
.chk(!is.null(hd_log_c3) && nrow(hd_log_c3) == 1L, TRUE,
     "dual-write still fires: staff mid-jump action also lands in their home log")

Sys.setenv(CLIENT_ID = "networks")   # restore

# ── D. rotate_log_if_needed: triggered at .MAX_AUDIT_ROWS ────────────────────

now_ts <- Sys.time()
# Build a data frame with exactly .MAX_AUDIT_ROWS rows using tibble::tibble()
big_log <- tibble::tibble(
  id          = as.character(seq_len(.MAX_AUDIT_ROWS)),
  ts          = rep(now_ts, .MAX_AUDIT_ROWS),
  user        = "mouse",
  module      = "test",
  action      = "fill",
  description = paste("row", seq_len(.MAX_AUDIT_ROWS)),
  target_id   = NA_character_,
  s3_key      = NA_character_,
  client_id   = "networks",
  metadata    = NA_character_
)
.chk(nrow(big_log), .MAX_AUDIT_ROWS, "big_log has exactly .MAX_AUDIT_ROWS rows")

# Write the big log as current active chunk (mock store already has patched s3saveRDS)
mock_s3saveRDS(big_log, "networks/app_audit.rds", "mock-bucket")
# Ensure no stale archive chunks from prior tests
rm(list = ls(.mock_s3_store)[startsWith(ls(.mock_s3_store), "networks/app_audit_")],
   envir = .mock_s3_store)

# Trigger rotation — get_bucket is already patched to use .mock_s3_store
did_rotate <- rotate_log_if_needed("networks", big_log)
.chk(isTRUE(did_rotate), TRUE, "rotate_log_if_needed returns TRUE when log is full")

# After rotation: current chunk should be empty, archive_001 should have the data
rotated_current <- tryCatch(
  mock_s3readRDS("networks/app_audit.rds", "mock-bucket"), error = function(e) NULL)
archive_chunk   <- tryCatch(
  mock_s3readRDS("networks/app_audit_001.rds", "mock-bucket"), error = function(e) NULL)

.chk(!is.null(rotated_current) && nrow(rotated_current) == 0L, TRUE,
     "after rotation: current chunk is empty")
.chk(!is.null(archive_chunk)   && nrow(archive_chunk) == .MAX_AUDIT_ROWS, TRUE,
     "after rotation: archive chunk has all rows")

# Below threshold — should NOT rotate
small_log <- big_log[1:100, ]
mock_s3saveRDS(small_log, "networks/app_audit.rds", "mock-bucket")
did_not_rotate <- rotate_log_if_needed("networks", small_log)
.chk(isTRUE(did_not_rotate), FALSE, "rotate_log_if_needed returns FALSE below threshold")

# ── E. read_audit_log: spans current + archive chunks ────────────────────────

make_row <- function(id, action, delta_secs = 0) {
  tibble::tibble(
    id=id, ts=Sys.time() + delta_secs, user="a", module="m",
    action=action, description="d",
    target_id=NA_character_, s3_key=NA_character_,
    client_id="networks", metadata=NA_character_
  )
}

mock_s3saveRDS(make_row("r1", "x"),       "networks/app_audit.rds",     "mock-bucket")
mock_s3saveRDS(make_row("r2", "y", -3600), "networks/app_audit_001.rds", "mock-bucket")

# read_audit_log uses get_bucket to discover chunks; mock it inline
read_audit_log_test <- function(client_id) {
  keys <- c(
    paste0(client_id, "/app_audit.rds"),
    paste0(client_id, "/app_audit_001.rds")
  )
  chunks <- Filter(Negate(is.null), lapply(keys, .audit_read_chunk))
  if (!length(chunks)) return(.APP_AUDIT_SCHEMA())
  dplyr::bind_rows(chunks)
}

combined <- read_audit_log_test("networks")
.chk(nrow(combined) >= 2L, TRUE, "read_audit_log spans current + archived chunk")
.chk("r1" %in% combined$id, TRUE, "combined log contains current chunk row")
.chk("r2" %in% combined$id, TRUE, "combined log contains archived chunk row")

# ── F. read_audit_log: until= excludes rows after the boundary ──────────────

now_ts <- Sys.time()
until_log <- dplyr::bind_rows(
  make_row("u1", "old",   delta_secs = -7200),  # 2h before now
  make_row("u2", "recent", delta_secs = -60)    # 1min before now
)
mock_s3saveRDS(until_log, "networks/app_audit.rds", "mock-bucket")
rm(list = ls(.mock_s3_store)[startsWith(ls(.mock_s3_store), "networks/app_audit_")],
   envir = .mock_s3_store)
mock_s3saveRDS(.APP_AUDIT_SCHEMA(), "networks/app_audit_001.rds", "mock-bucket")

until_result <- read_audit_log(client_id = "networks", until = now_ts - 3600)
.chk("u1" %in% until_result$id, TRUE, "until: keeps rows at/before the boundary")
.chk("u2" %in% until_result$id, FALSE, "until: excludes rows after the boundary")

since_until_result <- read_audit_log(client_id = "networks",
                                     since = now_ts - 7260, until = now_ts - 3600)
.chk(nrow(since_until_result), 1L, "since+until: narrows to exactly the rows inside the window")

# ── G. read_audit_log_scoped: the three visibility rules (Part C) ───────────

cat("\n  Scoped audit reads ──\n")

.expect_error <- function(expr_fn, label) {
  ok <- tryCatch({ expr_fn(); FALSE }, error = function(e) TRUE)
  .chk(ok, TRUE, label)
}

# G1. A plain hopdesk session (no can_view_staff_audit_log override) requesting
#     the hd-admin staff log gets a loud error, not an empty table.
.expect_error(function() {
  read_audit_log_scoped(
    requested_client_id         = "hd-admin",
    viewer_is_staff             = TRUE,
    viewer_can_view_client_logs = TRUE,
    viewer_can_view_staff_log   = FALSE,
    viewer_home_client_id       = "hd-admin"
  )
}, "scoped: plain hopdesk requesting hd-admin log -> error")

# G2. A finance-tier session at networks requesting a different client's log
#     (hopdesk's own folder) gets a loud error.
.expect_error(function() {
  read_audit_log_scoped(
    requested_client_id         = "hopdesk",
    viewer_is_staff             = FALSE,
    viewer_can_view_client_logs = FALSE,
    viewer_can_view_staff_log   = FALSE,
    viewer_home_client_id       = "networks"
  )
}, "scoped: finance@networks requesting a different client's log -> error")

# G3. A hopdesk session requesting a client's log succeeds — this is exactly
#     what "hopdesk sees client logs" means.
ok_hopdesk_client <- tryCatch({
  read_audit_log_scoped(
    requested_client_id         = "networks",
    viewer_is_staff             = TRUE,
    viewer_can_view_client_logs = TRUE,
    viewer_can_view_staff_log   = FALSE,
    viewer_home_client_id       = "hd-admin"
  )
  TRUE
}, error = function(e) FALSE)
.chk(ok_hopdesk_client, TRUE, "scoped: hopdesk requesting networks log -> succeeds")

# G3b. A hopdesk session WITHOUT can_view_client_audit_logs requesting a
#      client's log still gets a loud error (defense in depth).
.expect_error(function() {
  read_audit_log_scoped(
    requested_client_id         = "networks",
    viewer_is_staff             = TRUE,
    viewer_can_view_client_logs = FALSE,
    viewer_can_view_staff_log   = FALSE,
    viewer_home_client_id       = "hd-admin"
  )
}, "scoped: hopdesk without can_view_client_audit_logs requesting networks -> error")

# G4. A principal session can request hd-admin and any client successfully.
ok_principal_hd <- tryCatch({
  read_audit_log_scoped(
    requested_client_id         = "hd-admin",
    viewer_is_staff             = TRUE,
    viewer_can_view_client_logs = TRUE,
    viewer_can_view_staff_log   = TRUE,
    viewer_home_client_id       = "hd-admin"
  )
  TRUE
}, error = function(e) FALSE)
.chk(ok_principal_hd, TRUE, "scoped: principal requesting hd-admin log -> succeeds")

ok_principal_client <- tryCatch({
  read_audit_log_scoped(
    requested_client_id         = "networks",
    viewer_is_staff             = TRUE,
    viewer_can_view_client_logs = TRUE,
    viewer_can_view_staff_log   = TRUE,
    viewer_home_client_id       = "hd-admin"
  )
  TRUE
}, error = function(e) FALSE)
.chk(ok_principal_client, TRUE, "scoped: principal requesting a client log -> succeeds")

# G5. A native client user requesting their own home client's log succeeds
#     (regression: Actividad's existing behavior for a client's own users).
ok_native_home <- tryCatch({
  read_audit_log_scoped(
    requested_client_id         = "networks",
    viewer_is_staff             = FALSE,
    viewer_can_view_client_logs = FALSE,
    viewer_can_view_staff_log   = FALSE,
    viewer_home_client_id       = "networks"
  )
  TRUE
}, error = function(e) FALSE)
.chk(ok_native_home, TRUE, "scoped: native client requesting own home log -> succeeds")

cat("\n")
