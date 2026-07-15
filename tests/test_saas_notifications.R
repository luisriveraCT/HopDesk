# =============================================================================
# tests/test_saas_notifications.R
# Covers: append_hd_notification, read_hd_notifications, write_hd_notifications,
#         mark-as-read (single + bulk), notify_limit_change_to_principals.
# Sourced by _run_saas.R.
# =============================================================================

cat("── Notifications ────────────────────────────────────────────────────────\n")

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

# ── Helpers also needed by this module ────────────────────────────────────────
# Override hd-admin notification helpers to hit mock store.
read_hd_notifications <- function() {
  raw <- tryCatch(mock_s3readRDS("hd-admin/notifications.rds", "mock-bucket"),
                  error = function(e) NULL)
  if (is.null(raw) || !is.data.frame(raw) || !nrow(raw)) return(.schema_notifications())
  raw
}
write_hd_notifications <- function(df) {
  mock_s3saveRDS(df, "hd-admin/notifications.rds", "mock-bucket")
  invisible(TRUE)
}

# notify_limit_change_to_principals calls auth_load_usuarios; ensure hd-admin
# staff data exists (empty is fine — no emails → skips email loop).
mock_s3saveRDS(.schema_usuarios_init(), "hd-admin/usuarios.rds", "mock-bucket")

# ── A. Schema ─────────────────────────────────────────────────────────────────

mock_s3saveRDS(.schema_notifications(), "hd-admin/notifications.rds", "mock-bucket")
empty <- read_hd_notifications()
.chk(nrow(empty), 0L, "schema: starts empty")
.chk("id"         %in% names(empty), TRUE, "schema has id column")
.chk("type"       %in% names(empty), TRUE, "schema has type column")
.chk("client_id"  %in% names(empty), TRUE, "schema has client_id column")
.chk("message"    %in% names(empty), TRUE, "schema has message column")
.chk("created_at" %in% names(empty), TRUE, "schema has created_at column")
.chk("read_by"    %in% names(empty), TRUE, "schema has read_by column")
.chk("metadata"   %in% names(empty), TRUE, "schema has metadata column")

# ── B. append_hd_notification ─────────────────────────────────────────────────

result <- append_hd_notification(
  type         = "user_limit_warning",
  client_id    = "networks",
  message_text = "Límite al 80% en networks.",
  metadata     = list(pct = 80L)
)
.chk(isTRUE(result), TRUE, "append_hd_notification returns TRUE on success")

notifs1 <- read_hd_notifications()
.chk(nrow(notifs1), 1L, "append: one row persisted")
.chk(notifs1$type[1],      "user_limit_warning", "append: type stored correctly")
.chk(notifs1$client_id[1], "networks",           "append: client_id stored")
.chk(notifs1$read_by[1],   "[]",                 "append: read_by defaults to []")
.chk(!is.na(notifs1$id[1]) && nzchar(notifs1$id[1]), TRUE, "append: id is non-empty UUID")
meta_parsed <- tryCatch(jsonlite::fromJSON(notifs1$metadata[1]), error = function(e) NULL)
.chk(!is.null(meta_parsed) && isTRUE(meta_parsed$pct == 80L), TRUE, "append: metadata JSON round-trips")

# Second append — accumulates
append_hd_notification("user_request", "hopdesk", "Nueva solicitud.")
notifs2 <- read_hd_notifications()
.chk(nrow(notifs2), 2L, "append: second row accumulates")

# ── C. Mark single notification as read ───────────────────────────────────────

nid    <- notifs2$id[1]
viewer <- "mouse"

# Simulate the mark-as-read logic from the server handler
existing <- tryCatch(jsonlite::fromJSON(notifs2$read_by[1] %||% "[]"),
                     error = function(e) character(0))
notifs2$read_by[1] <- jsonlite::toJSON(c(existing, viewer), auto_unbox = FALSE)
write_hd_notifications(notifs2)

after_mark <- read_hd_notifications()
read_by_vec <- tryCatch(jsonlite::fromJSON(after_mark$read_by[after_mark$id == nid]),
                        error = function(e) character(0))
.chk(viewer %in% read_by_vec, TRUE, "mark-as-read: viewer appears in read_by after marking")
.chk(length(read_by_vec), 1L, "mark-as-read: exactly one entry in read_by")

# Idempotent: marking again should not duplicate
existing2 <- tryCatch(jsonlite::fromJSON(after_mark$read_by[after_mark$id == nid]),
                      error = function(e) character(0))
if (!viewer %in% existing2) {
  after_mark$read_by[after_mark$id == nid] <- jsonlite::toJSON(c(existing2, viewer),
                                                                 auto_unbox = FALSE)
  write_hd_notifications(after_mark)
}
after_mark2 <- read_hd_notifications()
read_by2 <- tryCatch(jsonlite::fromJSON(after_mark2$read_by[after_mark2$id == nid]),
                     error = function(e) character(0))
.chk(sum(read_by2 == viewer), 1L, "mark-as-read: idempotent — viewer not duplicated")

# Second notification still unread
second_read_by <- tryCatch(jsonlite::fromJSON(after_mark2$read_by[2] %||% "[]"),
                            error = function(e) character(0))
.chk(viewer %in% second_read_by, FALSE, "mark-as-read: only targeted row changed")

# ── D. Mark all as read ───────────────────────────────────────────────────────

current_all <- read_hd_notifications()
for (i in seq_len(nrow(current_all))) {
  ex <- tryCatch(jsonlite::fromJSON(current_all$read_by[i] %||% "[]"),
                 error = function(e) character(0))
  if (!viewer %in% ex)
    current_all$read_by[i] <- jsonlite::toJSON(c(ex, viewer), auto_unbox = FALSE)
}
write_hd_notifications(current_all)

after_all <- read_hd_notifications()
all_read <- all(vapply(seq_len(nrow(after_all)), function(i) {
  rb <- tryCatch(jsonlite::fromJSON(after_all$read_by[i] %||% "[]"),
                 error = function(e) character(0))
  viewer %in% rb
}, logical(1)))
.chk(all_read, TRUE, "mark-all: all rows show viewer as reader")

# Second viewer reading — does not wipe first viewer
viewer2 <- "hopdesk_staff"
current_all2 <- read_hd_notifications()
for (i in seq_len(nrow(current_all2))) {
  ex <- tryCatch(jsonlite::fromJSON(current_all2$read_by[i] %||% "[]"),
                 error = function(e) character(0))
  if (!viewer2 %in% ex)
    current_all2$read_by[i] <- jsonlite::toJSON(c(ex, viewer2), auto_unbox = FALSE)
}
write_hd_notifications(current_all2)

after_all2 <- read_hd_notifications()
both_read <- all(vapply(seq_len(nrow(after_all2)), function(i) {
  rb <- tryCatch(jsonlite::fromJSON(after_all2$read_by[i] %||% "[]"),
                 error = function(e) character(0))
  viewer %in% rb && viewer2 %in% rb
}, logical(1)))
.chk(both_read, TRUE, "mark-all: adding a second viewer preserves the first viewer")

# ── E. notify_limit_change_to_principals ──────────────────────────────────────

# Reset store
mock_s3saveRDS(.schema_notifications(), "hd-admin/notifications.rds", "mock-bucket")

# Stub send_email to be a no-op (avoids real network call)
send_email <- function(...) invisible(TRUE)

notify_limit_change_to_principals(
  client_id   = "networks",
  client_name = "Networks Group",
  old_limit   = 10L,
  new_limit   = 8L,
  changed_by  = "mouse",
  deactivated = c("@ana", "@bob")
)

notifs_limit <- read_hd_notifications()
.chk(nrow(notifs_limit) >= 1L, TRUE, "notify_limit_change: appends at least one notification")
limit_row <- notifs_limit[notifs_limit$type %in%
                            c("limit_change", "cambio_limite"), , drop = FALSE]
.chk(nrow(limit_row) >= 1L, TRUE, "notify_limit_change: row has correct type")
.chk(grepl("Networks Group", limit_row$message[1]), TRUE,
     "notify_limit_change: message contains client name")
.chk(grepl("8", limit_row$message[1]), TRUE,
     "notify_limit_change: message contains new limit")

meta_lim <- tryCatch(jsonlite::fromJSON(limit_row$metadata[1]), error = function(e) NULL)
.chk(!is.null(meta_lim), TRUE, "notify_limit_change: metadata is valid JSON")
.chk(isTRUE(meta_lim$deactivated_count == 2L), TRUE,
     "notify_limit_change: deactivated_count in metadata")

# ── F. Global-audit gate: can_view_global_audit permission ───────────────────

# Verify the perm gate logic using auth_resolve_perms directly
p_principal <- auth_resolve_perms("principal", "{}")
p_hopdesk   <- auth_resolve_perms("hopdesk",   "{}")
p_finance   <- auth_resolve_perms("finance",   "{}")

.chk(isTRUE(p_principal$can_view_global_audit), TRUE,
     "global_audit gate: principal always has can_view_global_audit")
.chk(isTRUE(p_hopdesk$can_view_global_audit), FALSE,
     "global_audit gate: hopdesk does NOT have can_view_global_audit by default")
.chk(isTRUE(p_finance$can_view_global_audit), FALSE,
     "global_audit gate: finance does NOT have can_view_global_audit")

# A hopdesk user granted the perm via override should gain access
override_json <- jsonlite::toJSON(list(can_view_global_audit = TRUE), auto_unbox = TRUE)
p_hopdesk_granted <- auth_resolve_perms("hopdesk", override_json)
.chk(isTRUE(p_hopdesk_granted$can_view_global_audit), TRUE,
     "global_audit gate: hopdesk + override can_view_global_audit → TRUE")

cat("\n")
