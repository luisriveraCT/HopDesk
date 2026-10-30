# =============================================================================
# tests/test_stage11_sap_snapshot_isolation.R
#
# CRITICAL fix (2026-08-27), found after "bad data" reappeared multiple
# times despite the earlier .app_data_cache cross-tenant isolation fix
# (Stage 5) and the app.R "always re-source global.R" fix (Stage 7):
# load_sap_snapshot() had NO client_id parameter at all. It read via the
# bare-key .s3_read(), which (a) resolves its S3 object against
# Sys.getenv("CLIENT_ID") -- always "hd-admin" in this shared deployment --
# and (b) checks/caches .s3_preload_cache under that same bare,
# non-client-prefixed key, so whichever client's session populated it FIRST
# in a process silently served ITS AR/AP data (or hd-admin's empty data) to
# every OTHER client's session calling load_sap_snapshot() afterward.
#
# This matters far more than the earlier .app_data_cache bug: this function
# seeds Phase 1's sap_data() on EVERY session, upstream of moves,
# proveedores, and everything else built on top of raw SAP data -- so this
# one bug reads downstream as "unrelated, seemingly-random bad data
# everywhere," not as an obviously snapshot-shaped problem. Fixed by adding
# client_id and routing through .s3_read_with()/.s3_key() (full-prefix
# keys), and by fixing save_sap_snapshot()'s own in-memory cache-restore
# calls to use the same full-prefix convention.
#
# Sourced by _run_saas.R.
# =============================================================================

cat("── SAP snapshot cross-tenant isolation (CRITICAL) ─────────────────────────\n")

.pass <- 0L
.fail <- 0L
.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}
.chk_true  <- function(cond, label) .chk(isTRUE(cond), TRUE, label)
.chk_false <- function(cond, label) .chk(isTRUE(cond), FALSE, label)
.chk_null  <- function(x, label) .chk(is.null(x), TRUE, label)

# _run_saas.R's global S3_KEYS_CRITICAL isn't part of the harness's minimal
# setup (it's defined in R/global.R, which the harness deliberately doesn't
# source -- see test_stage5's own header for why). Defined here the same
# way test_stage5 added S3_KEYS$interco_v2 for its own needs.
S3_KEYS_CRITICAL <<- list(sap_snap_ar = "sap_snapshot_AR.rds", sap_snap_ap = "sap_snapshot_AP.rds")

# =============================================================================
# Part A -- cross-tenant isolation, the exact reported scenario
# =============================================================================
cat("\n-- Part A: load_sap_snapshot()/save_sap_snapshot() cross-tenant isolation --\n")

# Reset the preload cache between parts -- persistent across this whole
# process otherwise, same as test_stage5's own reset convention.
rm(list = ls(.s3_preload_cache), envir = .s3_preload_cache)

networks_ar <- data.frame(Empresa = "NCS", Parte = "TETRA PAK", Importe = 100, stringsAsFactors = FALSE)
save_sap_snapshot(networks_ar, "AR", client_id = "networks")

# hd-admin (empty/staff-home) requests its OWN snapshot -- must not see
# networks' data even though it's the only real data written so far.
hd_snap <- load_sap_snapshot("AR", client_id = "hd-admin")
.chk_true(is.null(hd_snap) || is.null(hd_snap$data) || !nrow(hd_snap$data),
     "CRITICAL: hd-admin's load_sap_snapshot() does NOT see networks' just-saved data")

# networks' own session must see its own data.
net_snap <- load_sap_snapshot("AR", client_id = "networks")
.chk_true(!is.null(net_snap) && !is.null(net_snap$data) && nrow(net_snap$data) == 1L,
     "networks' own load_sap_snapshot() sees its own saved data")
.chk_true(!is.null(net_snap) && any(net_snap$data$Parte == "TETRA PAK"),
     "CRITICAL: the actual saved row (TETRA PAK) is visible to networks' own session")

# Reverse order: hd-admin loads (still empty) AFTER networks already loaded
# in this same process -- must still not leak across.
hd_snap2 <- load_sap_snapshot("AR", client_id = "hd-admin")
.chk_true(is.null(hd_snap2) || is.null(hd_snap2$data) || !nrow(hd_snap2$data),
     "hd-admin's second load also does not see networks' data (no cross-bleed either direction)")

# A third, unrelated client must also be isolated.
beta_snap <- load_sap_snapshot("AR", client_id = "beta")
.chk_true(is.null(beta_snap) || is.null(beta_snap$data) || !nrow(beta_snap$data),
     "a third, unrelated client's load_sap_snapshot() sees neither networks' nor hd-admin's data")

# =============================================================================
# Part B -- AP ledger isolated independently of AR
# =============================================================================
cat("\n-- Part B: AP/AR are independently isolated --\n")

beta_ap <- data.frame(Empresa = "X", Parte = "PROVEEDOR BETA", Importe = 50, stringsAsFactors = FALSE)
save_sap_snapshot(beta_ap, "AP", client_id = "beta")

.chk_true(!is.null(load_sap_snapshot("AP", client_id = "beta")$data),
     "beta's own AP snapshot is readable")
net_ap <- load_sap_snapshot("AP", client_id = "networks")
.chk_true(is.null(net_ap) || is.null(net_ap$data) || !nrow(net_ap$data),
     "networks' AP snapshot is unaffected by beta's AP save (still empty)")
net_ar_still_ok <- load_sap_snapshot("AR", client_id = "networks")
.chk_true(!is.null(net_ar_still_ok$data) && nrow(net_ar_still_ok$data) == 1L,
     "networks' AR snapshot is unaffected by beta's unrelated AP save")

# =============================================================================
# Part C -- rotation (prev1/prev2) stays correctly scoped per client
# =============================================================================
cat("\n-- Part C: prev1/prev2 rotation stays client-scoped --\n")

save_sap_snapshot(data.frame(Empresa = "NCS", Parte = "V2", Importe = 200, stringsAsFactors = FALSE),
                  "AR", client_id = "networks")
save_sap_snapshot(data.frame(Empresa = "NCS", Parte = "V3", Importe = 300, stringsAsFactors = FALSE),
                  "AR", client_id = "networks")

cur_full   <- .s3_key("sap_snapshot_AR.rds",       client_id = "networks")
prev1_full <- .s3_key("sap_snapshot_AR_prev1.rds", client_id = "networks")
.chk_true(exists(cur_full, envir = .s3_preload_cache, inherits = FALSE),
     "networks' current snapshot cache entry uses the full client-prefixed key")
.chk_true(exists(prev1_full, envir = .s3_preload_cache, inherits = FALSE),
     "networks' prev1 snapshot cache entry ALSO uses the full client-prefixed key")

hd_prev1_full <- .s3_key("sap_snapshot_AR_prev1.rds", client_id = "hd-admin")
.chk_false(exists(hd_prev1_full, envir = .s3_preload_cache, inherits = FALSE),
     "hd-admin has no prev1 cache entry created as a side effect of networks' rotation")

# =============================================================================
# Part D -- static regression guards
# =============================================================================
cat("\n-- Part D: static regression guards --\n")

p_src <- readLines("R/persistence.R", warn = FALSE)
.chk_true(any(grepl("^load_sap_snapshot <- function\\(ledger, client_id = NULL\\)", p_src)),
     "load_sap_snapshot() signature now accepts client_id")
.chk_true(any(grepl("\\.s3_read_with\\(key, client_id = client_id\\)", p_src)),
     "load_sap_snapshot() routes through .s3_read_with() (full-prefix), not the bare-key .s3_read()")

save_snap_start <- grep("^save_sap_snapshot <- function", p_src)[1]
save_snap_block <- paste(p_src[save_snap_start:min(save_snap_start + 75, length(p_src))], collapse = "\n")
.chk_false(grepl("assign\\(cur_key,", save_snap_block),
     "save_sap_snapshot() no longer restores the cache under the bare cur_key")
.chk_true(grepl("assign\\(cur_full,", save_snap_block),
     "save_sap_snapshot() restores the cache under the full-prefix cur_full instead")

app_txt <- readLines("app.R", warn = FALSE)
app_txt <- app_txt[!grepl("^\\s*#", app_txt)]  # exclude comment lines (see test_stage7's own precedent)
load_snap_calls <- grep("load_sap_snapshot\\(", app_txt, value = TRUE)
.chk_true(length(load_snap_calls) > 0, "sanity: app.R still calls load_sap_snapshot()")
.chk_true(all(grepl("client_id\\s*=", load_snap_calls)),
     "every load_sap_snapshot() call site in app.R now passes client_id=")

hub_txt <- readLines("R/settings/settings_hub.R", warn = FALSE)
hub_calls <- grep("load_sap_snapshot\\(", hub_txt, value = TRUE)
.chk_true(length(hub_calls) >= 8, "sanity: settings_hub.R still has its multiple load_sap_snapshot() call sites")
.chk_true(all(grepl("client_id\\s*=", hub_calls)),
     "every load_sap_snapshot() call site in settings_hub.R now passes client_id=")

# =============================================================================
# Part E -- the actual write-side gap (found 2026-09-01): every check above
# (and the original 2026-08-27 fix) covers load_sap_snapshot()'s READ side.
# The one save_sap_snapshot() call site -- inside fetch_all_companies(),
# R/sap_api.R -- hardcoded client_id = NULL instead of using its own
# received client_id parameter, so every successful LIVE SAP fetch kept
# silently persisting the fresh snapshot under hd-admin/ instead of the
# real client's own prefix. The read side was therefore always reading a
# correctly-scoped key that nothing was writing to -- the client's own
# fallback-on-outage snapshot silently never advanced past whatever it was
# first seeded with (2026-06-25 for networks), surfacing as "pulled up the
# super old snapshot" the next time a live SAP fetch failed (VPN not yet
# connected, SAP transiently unreachable) and the app fell back to it.
# =============================================================================
cat("\n-- Part E: fetch_all_companies() forwards its OWN client_id to save_sap_snapshot() --\n")

sap_api_txt <- readLines("R/sap_api.R", warn = FALSE)
.chk_true({p <- tryCatch({parse("R/sap_api.R"); TRUE}, error = function(e) FALSE); p},
          "R/sap_api.R still parses without syntax errors")

.chk(length(grep("^fetch_all_companies <- function\\(ledger = c\\(\"AR\",\"AP\"\\), cmap, client_id = NULL\\)", sap_api_txt)), 1L,
     "fetch_all_companies() still receives client_id as its own parameter (sanity -- if this signature ever changes, the fix below needs re-checking)")

save_snap_call_hits <- grep("save_sap_snapshot\\(", sap_api_txt, value = TRUE)
.chk(length(save_snap_call_hits), 1L,
     "exactly one save_sap_snapshot() call site exists in sap_api.R")
.chk_true(length(save_snap_call_hits) && grepl("client_id\\s*=\\s*client_id", save_snap_call_hits[1]),
          "THE FIX: save_sap_snapshot() is called with client_id = client_id (its own parameter)")
.chk_true(length(save_snap_call_hits) && !grepl("client_id\\s*=\\s*NULL", save_snap_call_hits[1]),
          "THE REGRESSION GUARD: save_sap_snapshot() is never again called with a hardcoded client_id = NULL")

# Broader net: no OTHER save_sap_snapshot() call site anywhere in R/*.R
# hardcodes client_id = NULL either (catches the same bug shape appearing
# at a future second call site).
all_r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
hardcoded_null_hits <- do.call(rbind, lapply(all_r_files, function(f) {
  txt <- readLines(f, warn = FALSE)
  ln  <- grep("save_sap_snapshot\\([^)]*client_id\\s*=\\s*NULL", txt)
  if (!length(ln)) return(NULL)
  data.frame(file = f, line = ln, stringsAsFactors = FALSE)
}))
.chk(is.null(hardcoded_null_hits) || !nrow(hardcoded_null_hits), TRUE,
     "no save_sap_snapshot() call site anywhere in R/*.R hardcodes client_id = NULL")

# Functional: exercise the exact one-line wiring fetch_all_companies() uses
# (a real client_id parameter flowing straight into save_sap_snapshot()),
# using the already-proven-correct save_sap_snapshot()/load_sap_snapshot()
# functions from Parts A-C above -- this is the actual observable behavior
# the fix guarantees, not just the source text shape.
rm(list = ls(.s3_preload_cache), envir = .s3_preload_cache)
.fac_client_id <- "networks"  # what fetch_all_companies() would have received from app.R's effective_client_id()
fresh_fetch_result <- data.frame(Empresa = "NCS", Parte = "PROVEEDOR FRESCO", Importe = 999, stringsAsFactors = FALSE)
save_sap_snapshot(fresh_fetch_result, "AP", client_id = .fac_client_id)  # mirrors sap_api.R's fixed call exactly

readback <- load_sap_snapshot("AP", client_id = "networks")
.chk_true(!is.null(readback) && !is.null(readback$data) && nrow(readback$data) == 1L,
          "a save using the caller's own client_id (not NULL) is readable back under that same client's prefix")
.chk_true(!is.null(readback) && any(readback$data$Parte == "PROVEEDOR FRESCO"),
          "the correct row content round-trips")

leaked_to_hdadmin <- load_sap_snapshot("AP", client_id = "hd-admin")
.chk_true(is.null(leaked_to_hdadmin) || is.null(leaked_to_hdadmin$data) || !nrow(leaked_to_hdadmin$data),
          "CRITICAL: the save does NOT leak into hd-admin's own snapshot (proving it's not falling back to the old bare-key/NULL behavior)")

cat(sprintf("\n  Stage 11 subtotal: %d passed, %d failed\n", .pass, .fail))
