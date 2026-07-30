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

# ── 5. No dataset is registered twice (a duplicate register_synced() call
# would silently overwrite the first entry in .SYNC_REGISTRY -- not a crash,
# just quietly wrong, exactly the kind of "vulnerable to failure" case worth
# a permanent guard) ─────────────────────────────────────────────────────────
for (key in NEWLY_SYNCED) {
  pat <- sprintf('register_synced\\("%s"', key)
  ok(sprintf("app.R: register_synced(\"%s\", ...) appears exactly once, not duplicated", key),
     sum(grepl(pat, app_txt)) == 1L)
}

# ── 6. Every loader function passed to register_synced() for these 8 keys
# actually exists as a real function somewhere in R/ (catches a typo'd
# loader name, which would silently no-op every poll tick for that key
# rather than error visibly) ─────────────────────────────────────────────────
LOADER_OF <- c(
  abonos_db                = "load_abonos",
  interco_v2                = "load_interco_v2",
  sap_ov_db                 = "load_sap_overrides",
  hop_grants_db             = "load_hop_grants",
  proveedores_inactivos_db  = "load_proveedores_inactivos",
  partner_policies_db       = "load_partner_policies",
  policy_moves_db           = "load_policy_moves",
  holiday_overrides_db      = "load_holiday_overrides"
)
r_files <- list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
r_all_txt <- unlist(lapply(r_files, readLines, warn = FALSE))
for (key in names(LOADER_OF)) {
  fn <- LOADER_OF[[key]]
  ok(sprintf("%s's loader %s() is a real function definition somewhere in R/", key, fn),
     any(grepl(sprintf("^%s\\s*<-\\s*function", fn), r_all_txt)))
}

# ── 7. Every one of the 8 real save_*() functions used by these datasets
# accepts client_id (except save_hop_grants(), which is deliberately global-
# scoped, not per-client -- the one confirmed, intentional exception) ───────
SAVE_OF <- c(
  abonos_db                = "save_abonos",
  interco_v2                = "save_interco_v2",
  sap_ov_db                 = "save_sap_overrides",
  proveedores_inactivos_db  = "save_proveedores_inactivos",
  partner_policies_db       = "save_partner_policies",
  policy_moves_db           = "save_policy_moves",
  holiday_overrides_db      = "save_holiday_overrides"
)
for (key in names(SAVE_OF)) {
  fn <- SAVE_OF[[key]]
  def_line <- grep(sprintf("^%s\\s*<-\\s*function", fn), r_all_txt, value = TRUE)
  ok(sprintf("%s's save function %s() accepts client_id (per-client scoping preserved)", key, fn),
     length(def_line) > 0 && any(grepl("client_id", def_line)))
}
ok("save_hop_grants() deliberately does NOT take client_id (global admin-scoped, by design -- see .HOP_GRANTS_KEY)",
   {
     def_line <- grep("^save_hop_grants\\s*<-\\s*function", r_all_txt, value = TRUE)
     length(def_line) > 0 && !grepl("client_id", def_line)
   })

# ── 8. .s3_key() resolves to a well-formed, non-malformed key for every one
# of the 7 client-scoped S3_KEYS entries, across several different client
# ids -- this is the exact vulnerability class found this stage (a missing
# S3_KEYS entry silently resolving to "<client>/" with no filename at all).
# Extracts the REAL .s3_key()/.client_id() implementation, not a hand-copied
# mirror, so this catches a regression in .s3_key() itself too. ────────────
{
  .extract_fn <- function(file, fn_name, envir) {
    exprs <- parse(file, keep.source = FALSE)
    for (e in exprs) {
      if (is.call(e) && length(e) >= 2 &&
          identical(e[[1]], as.name("<-")) &&
          identical(e[[2]], as.name(fn_name))) {
        assign(fn_name, eval(e[[3]], envir = envir), envir = envir)
        return(invisible(TRUE))
      }
    }
    NULL
  }
  keyenv <- new.env()
  assign("S3_KEYS", list(
    abonos = "abonos.rds", interco_v2 = "interco_v2.rds",
    sap_overrides = "sap_overrides.rds",
    proveedores_inactivos = "proveedores_inactivos.rds",
    partner_policies = "partner_policies.rds",
    policy_moves = "policy_moves.rds",
    holiday_overrides = "holiday_overrides.rds"
  ), envir = keyenv)
  assign(".client_id", function() "hd-admin", envir = keyenv)
  .extract_fn("R/persistence.R", ".s3_key", keyenv)

  CLIENT_SCOPED_KEYS <- c("abonos", "interco_v2", "sap_overrides",
                          "proveedores_inactivos", "partner_policies",
                          "policy_moves", "holiday_overrides")
  TEST_CLIENTS <- c("networks", "hd-admin", "another-client-99")

  if (is.null(get0(".s3_key", envir = keyenv))) {
    ok("could extract the real .s3_key() implementation to test against", FALSE)
  } else {
    s3_key_fn <- get(".s3_key", envir = keyenv)
    s3_keys <- get("S3_KEYS", envir = keyenv)
    for (key in CLIENT_SCOPED_KEYS) {
      for (cid in TEST_CLIENTS) {
        resolved <- s3_key_fn(s3_keys[[key]], client_id = cid)
        expected <- paste0(tolower(cid), "/", s3_keys[[key]])
        ok(sprintf(".s3_key() resolves %s for client '%s' to a real filename, not \"<client>/\" (%s)",
                   key, cid, resolved),
           identical(resolved, expected) && !grepl("/$", resolved))
      }
    }
  }
}

# ── 9. Global defense in depth: NO two entries in the real S3_KEYS list
# (not just the 8 from this stage) resolve to the same filename -- a
# collision here would mean two datasets silently overwrite each other's
# S3 object. Directly generalizes the abonos bug (a MISSING entry) to its
# sibling risk (a DUPLICATE entry), across the entire list, not just the 8
# touched this stage. ────────────────────────────────────────────────────
{
  global_txt <- readLines("R/global.R", warn = FALSE)
  s3_start <- grep("^S3_KEYS\\s*<-\\s*list\\(", global_txt)
  ok("found the real S3_KEYS <- list(...) definition to scan", length(s3_start) > 0)
  if (length(s3_start)) {
    s3_end <- s3_start[1] - 1L + which(grepl("^\\)\\s*$", global_txt[s3_start[1]:length(global_txt)]))[1]
    block <- global_txt[s3_start[1]:s3_end]
    values <- regmatches(block, regexpr('=\\s*"[^"]+\\.rds"', block))
    values <- sub('^=\\s*"', "", values); values <- sub('"$', "", values)
    ok(sprintf("S3_KEYS has no duplicate filename values across its %d entries (every dataset has its own S3 object)",
               length(values)),
       length(values) == length(unique(values)))
  }
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0) stop("Tests FAILED.")
invisible(NULL)
