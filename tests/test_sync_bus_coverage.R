# =============================================================================
# tests/test_sync_bus_coverage.R
# Found 2026-07-29 while designing HopDesk's real-time-refresh strategy:
# 8 real, actively-used datasets (each with a genuine save_*()/load_*() pair)
# were never wired into R/sync_bus.R's registry -- a change to any of them by
# one session was invisible to every other open session until that session's
# next full login, not slow, just never, until then. abonos_db in particular
# had a SEPARATE bug on top: S3_KEYS$abonos didn't exist at all, so every
# save/load resolved to a malformed S3 key (paste0(prefix, "/", NULL) ==
# "<client>/", confirmed live against production S3) -- fixed alongside this.
#
# This is a static regression guard, same convention as
# tests/test_saas_log_action_scoping.R's scan for every log_action( call
# site: confirms each of the 8 names is both register_synced()'d in app.R
# AND has at least one bump_sync_version() call at its real save site(s),
# so a future save site for these datasets can't quietly skip the bump again.
# =============================================================================
cat("=== test_sync_bus_coverage ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) {
    cat(" PASS:", label, "\n"); .pass <<- .pass + 1L
  } else {
    cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L
  }
}

app_txt <- readLines("app.R", warn = FALSE)

# ── 1. Every one of the 8 keys is register_synced() in app.R ────────────────
NEWLY_SYNCED <- c("abonos_db", "interco_v2", "sap_ov_db", "hop_grants_db",
                  "proveedores_inactivos_db", "partner_policies_db",
                  "policy_moves_db", "holiday_overrides_db")
for (key in NEWLY_SYNCED) {
  pat <- sprintf('register_synced\\("%s"', key)
  ok(sprintf("app.R: register_synced(\"%s\", ...) exists", key),
     any(grepl(pat, app_txt)))
}

# ── 2. Every save site for these datasets (except holiday_overrides_db,
# which has no save call site anywhere yet -- registered for when it does)
# bumps the matching version somewhere in the repo ──────────────────────────
.count_bump <- function(name) {
  files <- list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  hits <- 0L
  pat <- sprintf('bump_sync_version\\("%s"', name)
  for (f in files) hits <- hits + sum(grepl(pat, readLines(f, warn = FALSE)))
  hits
}

ok("abonos_db: bump_sync_version(\"abonos_db\") appears at 3 real save sites",
   .count_bump("abonos_db") == 3L)
ok("interco_v2: bump_sync_version(\"interco_v2\") appears at 5 real save sites",
   .count_bump("interco_v2") == 5L)
ok("sap_ov_db: bump_sync_version(\"sap_ov_db\") appears at its real save site",
   .count_bump("sap_ov_db") >= 1L)
ok("hop_grants_db: bump_sync_version(\"hop_grants_db\") appears at all 3 real save sites",
   .count_bump("hop_grants_db") == 3L)
ok("proveedores_inactivos_db: bump_sync_version(...) appears at its real save site",
   .count_bump("proveedores_inactivos_db") >= 1L)
ok("partner_policies_db: bump_sync_version(...) appears at all 3 real save sites",
   .count_bump("partner_policies_db") == 3L)
ok("policy_moves_db: bump_sync_version(...) appears at all 5 real save sites",
   .count_bump("policy_moves_db") == 5L)

# holiday_overrides_db: no save_holiday_overrides( call site exists anywhere
# yet (confirmed via grep during this stage) -- so nothing to bump. Guard
# that this control fact hasn't silently changed without anyone noticing
# (if it has, holiday_overrides_db needs the same bump treatment too).
{
  files <- list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  save_hits <- 0L
  for (f in files) save_hits <- save_hits + sum(grepl("save_holiday_overrides\\(", readLines(f, warn = FALSE)))
  ok("control: save_holiday_overrides( still has no real call site (if this fails, it needs bump_sync_version too)",
     save_hits == 0L)
}

# ── 3. S3_KEYS$abonos exists and points at a real filename (the actual bug
# found this stage -- a missing entry, not just a missing sync registration) ─
{
  global_txt <- readLines("R/global.R", warn = FALSE)
  ok("R/global.R defines S3_KEYS$abonos",
     any(grepl('^\\s*abonos\\s*=\\s*"[^"]+\\.rds"', global_txt)))
}

# ── 4. hop_grants_db's registered loader tolerates being called with
# client_id (setup_sync_bus() always passes one in a staff jump-context;
# load_hop_grants() itself takes no arguments at all) ────────────────────────
{
  start <- grep('register_synced\\("hop_grants_db"', app_txt)
  ok("found the hop_grants_db registration to scan", length(start) > 0)
  if (length(start)) {
    block <- paste(app_txt[start[1]:min(start[1] + 3, length(app_txt))], collapse = "\n")
    ok("hop_grants_db's loader is wrapped to accept client_id (function(client_id = NULL) load_hop_grants())",
       grepl("function\\(client_id\\s*=\\s*NULL\\)\\s*load_hop_grants\\(\\)", block))
  }
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0) stop("Tests FAILED.")
invisible(NULL)
