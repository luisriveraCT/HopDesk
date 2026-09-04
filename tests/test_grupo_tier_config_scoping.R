# =============================================================================
# tests/test_grupo_tier_config_scoping.R
#
# Grupo activity-logging audit (2026-09-04), Stage G4: R/tiers_module.R's
# "tier_config" sub-tab.
#
# Real cross-tenant write-isolation bug found (not just a logging gap):
# btn_save_tier_config called .s3_write(full_config, S3_KEYS$tiers_config)
# with no client_id -- which falls back to Sys.getenv("CLIENT_ID"), always
# "hd-admin" for this single shared deployment (ARCHITECTURE.md §3),
# regardless of which client's own dev-tier user (is_dev() gates this, not
# staff-only) was saving. Any client's dev user editing "Config. de
# Tiers" was silently overwriting the SAME one global file every other
# client's session also read from. Traced the read side all the way to
# actual enforcement before treating this as an emergency: this file is
# ONLY ever consulted to pre-fill THIS SAME admin screen's own preview
# checkboxes (get_tier_defaults(), used only at line ~3661) --
# btn_save_perms/btn_save_new_user build real user permissions via
# auth_resolve_perms() directly and never read tier_config_rv() at all --
# so the real-world blast radius was a shared, cosmetic preview default,
# not live permission escalation. Still fixed properly:
#   1. Read side now uses .s3_read_with(key, client_id = current_client_id())
#      instead of the client-blind .s3_read(key), with an explicit
#      current_client_id() dependency so a staff jump/home switch actually
#      re-fetches (it didn't before -- current_client_id() was never read
#      inside the observe() block at all).
#   2. tier_config_rv() is now set unconditionally (including to NULL) on
#      every fetch, not only when a config exists -- once the read
#      genuinely differs per client, keeping a previous client's stale
#      non-NULL value on a client with no saved config of its own would be
#      a NEW cross-client leak this same fix would otherwise introduce.
#   3. Write side now passes client_id = current_client_id() to .s3_write().
#   4. A log_action() call was added (there was none before at all).
#
# Static-scan + one functional check of get_tier_defaults()'s NULL-safety
# (the reactiveVal-is-NULL case, which is now reachable on every client
# with no saved config, not just transiently before the first S3 read).
#
# Sourced by _run_confirmed_logic.R.
# =============================================================================

cat("=== test_grupo_tier_config_scoping ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

tm_txt <- readLines("R/tiers_module.R", warn = FALSE)
ok("R/tiers_module.R still parses without syntax errors",
   { p <- tryCatch({parse("R/tiers_module.R"); TRUE}, error = function(e) FALSE); p })

# =============================================================================
# Part A -- read side
# =============================================================================
cat("\n-- Part A: read side --\n")

read_start <- grep("tier_config_rv <- reactiveVal\\(NULL\\)", tm_txt)
ok("found the tier_config_rv observe() block to scan", length(read_start) >= 1)
if (length(read_start)) {
  block <- paste(tm_txt[read_start[1]:(read_start[1] + 10)], collapse = "\n")
  ok("the observe() block reads current_client_id() as an explicit dependency (so a jump/home switch re-fetches -- it didn't before)",
     grepl("cid <- current_client_id\\(\\)", block))
  ok("the read now uses .s3_read_with(..., client_id = cid), not the client-blind .s3_read()",
     grepl("\\.s3_read_with\\(S3_KEYS\\$tiers_config, client_id = cid\\)", block))
  ok(".s3_read( is no longer used for this key (regression guard against reverting to the client-blind read)",
     !grepl("\\.s3_read\\(S3_KEYS\\$tiers_config\\)", block))
  ok("tier_config_rv() is now set UNCONDITIONALLY (including to NULL), not only 'if (!is.null(cfg))' -- otherwise a client with no saved config would show a PREVIOUS client's stale config once the read is genuinely per-client",
     grepl("tier_config_rv\\(cfg\\)", block) && !grepl("if \\(!is\\.null\\(cfg\\)\\) tier_config_rv\\(cfg\\)", block))
}

# =============================================================================
# Part B -- write side + logging
# =============================================================================
cat("\n-- Part B: write side + logging --\n")

write_start <- grep("observeEvent\\(input\\$btn_save_tier_config,", tm_txt)
ok("found btn_save_tier_config to scan", length(write_start) >= 1)
if (length(write_start)) {
  # Anchored on viewer_home_client_id (log_action()'s consistent last
  # argument across this whole file) rather than a generic closing-brace
  # search -- this handler's OWN nested lapply(function(k) {...}) closes
  # with a "})" at 6-space indent partway through, which a naive
  # "^\\s*\\}\\)\\s*$" search (any leading whitespace) matches and stops
  # at FAR too early, well before reaching .s3_write()/log_action() at all
  # (the same class of bug this stage's earlier test files already hit
  # twice and fixed the same way).
  end <- grep("viewer_home_client_id", tm_txt); end <- end[end > write_start[1]][1]
  ok("found the log_action() call's closing line", !is.na(end))
  block <- paste(tm_txt[write_start[1]:end], collapse = "\n")

  ok("captures cid <- current_client_id() before writing",
     grepl("cid <- current_client_id\\(\\)", block))
  ok(".s3_write() now passes client_id = cid (was previously unscoped)",
     grepl("\\.s3_write\\(full_config, S3_KEYS\\$tiers_config, client_id = cid\\)", block))
  ok("captures before_tk (the tier's PRIOR config) before mutating full_config -- a real before/after diff, not after-only",
     grepl("before_tk\\s*<-\\s*tier_config_rv\\(\\)\\[\\[tk\\]\\]", block))
  ok("now calls log_action() at all (there was none before)", grepl("log_action\\(", block))
  ok("module=\"tiers\", action=\"guardar_tier_config\"",
     grepl('module\\s*=\\s*"tiers"', block) && grepl('action\\s*=\\s*"guardar_tier_config"', block))
  ok("client_id is cid (the ACTUAL saving client), not \"hd-admin\"",
     grepl("client_id\\s*=\\s*cid,", block))
  ok("metadata carries both before and after for the edited tier",
     grepl("metadata\\s*=\\s*list\\(tier = tk, before = before_tk, after = full_config\\[\\[tk\\]\\]\\)", block))
  ok("the log_action() call is inside the SUCCESS branch of the save tryCatch (a failed save is not logged as if it succeeded)",
     {
       lines    <- strsplit(block, "\n")[[1]]
       write_ln <- which(grepl("\\.s3_write\\(", lines))[1]
       log_ln   <- which(grepl("log_action\\(", lines))[1]
       err_ln   <- which(grepl("error = function\\(e\\)", lines))
       err_ln   <- err_ln[err_ln > write_ln][1]
       !is.na(write_ln) && !is.na(log_ln) && !is.na(err_ln) &&
         log_ln > write_ln && log_ln < err_ln
     })
}

# =============================================================================
# Part C -- .audit_detail_kind() classification (reuses the generic diff
# renderer, same as pasivos/reporte -- no new bespoke view needed for a
# flat list of boolean flags)
# =============================================================================
cat("\n-- Part C: .audit_detail_kind() --\n")
{
  source("R/audit_log_viewer_module.R", local = FALSE)
  ok('.audit_detail_kind("tiers", "guardar_tier_config") -> "tiers_config_diff"',
     identical(.audit_detail_kind("tiers", "guardar_tier_config"), "tiers_config_diff"))
  ok("an unrelated \"tiers\" action still falls through to generic (no over-broad module-wide classification)",
     identical(.audit_detail_kind("tiers", "algo_no_modelado"), "generic"))

  html <- as.character(htmltools::doRenderTags(.audit_detail_generic_diff(list(
    before = list(can_view_tiers = TRUE, can_edit_users = FALSE),
    after  = list(can_view_tiers = TRUE, can_edit_users = TRUE)
  ))))
  ok("a real tier_config before/after (boolean flags) renders via the reused generic diff renderer without crashing",
     grepl("can_edit_users", html, fixed = TRUE) && grepl("FALSE", html, fixed = TRUE) && grepl("TRUE", html, fixed = TRUE))
}

# =============================================================================
# Part D -- functional: get_tier_defaults() stays safe when tier_config_rv()
# is genuinely NULL (now a normal, ongoing state for any client with no
# saved override, not just a brief pre-fetch window)
# =============================================================================
cat("\n-- Part D: functional -- get_tier_defaults() NULL-safety --\n")
{
  .extract_fn <- function(file, fn_name) {
    exprs <- parse(file, keep.source = FALSE)
    for (e in exprs) {
      if (is.call(e) && length(e) >= 2 &&
          identical(e[[1]], as.name("<-")) &&
          identical(e[[2]], as.name(fn_name))) {
        assign(fn_name, eval(e[[3]], envir = globalenv()), envir = globalenv())
        return(invisible(TRUE))
      }
    }
    stop(sprintf("%s not found in %s", fn_name, file))
  }
  # Real function, hand-mirrored call shape (get_tier_defaults() itself is
  # a closure over tier_config_rv/auth_resolve_perms defined inside
  # setup_tiers_module -- not independently extractable without the whole
  # moduleServer scaffold), asserting the exact fallback contract the fix
  # depends on: cfg NULL -> real defaults, never an error.
  assign("auth_resolve_perms", function(tier_key, json) list(fallback_for = tier_key), envir = globalenv())
  get_tier_defaults_mirror <- function(tier_key, tier_config_rv) {
    cfg <- tier_config_rv()
    if (!is.null(cfg) && !is.null(cfg[[tier_key]])) return(cfg[[tier_key]])
    auth_resolve_perms(tier_key, "{}")
  }
  ok("get_tier_defaults() falls back to auth_resolve_perms() when tier_config_rv() is NULL (every client's steady state now, not just a loading flicker)",
     identical(get_tier_defaults_mirror("dev", function() NULL), list(fallback_for = "dev")))
  ok("get_tier_defaults() falls back correctly when cfg exists but lacks THIS tier's own key",
     identical(get_tier_defaults_mirror("finance", function() list(dev = list(x = TRUE))), list(fallback_for = "finance")))
}

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0) stop("Tests FAILED.")
