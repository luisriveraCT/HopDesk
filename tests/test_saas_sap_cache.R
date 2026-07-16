# =============================================================================
# tests/test_saas_sap_cache.R
# Verifies .sap_cache_get()/.sap_cache_set()/.sap_cache_fresh(): the SAP
# snapshot cache is keyed per client id and can never leak one client's data
# into another client's (or a staff-at-home session's) lookup.
# Sourced by _run_saas.R.
# =============================================================================

cat("── SAP Cache Isolation ───────────────────────────────────────────────────\n")

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

# Reset to a clean slate — other test modules may have written to this cache.
.GlobalEnv$.sap_global_cache <- list()

# ── A. Basic get/set round-trip ───────────────────────────────────────────────

.sap_cache_set("networks", data.frame(x = 1), data.frame(y = 2))
entry <- .sap_cache_get("networks")
.chk(is.list(entry), TRUE, "networks entry is a list after set")
.chk(entry$AR, data.frame(x = 1), "networks entry AR round-trips")
.chk(entry$AP, data.frame(y = 2), "networks entry AP round-trips")
.chk(inherits(entry$fetched_at, "POSIXct"), TRUE, "networks entry has a POSIXct fetched_at")

# Key lookup is case-insensitive (client ids are lower-cased elsewhere too)
.chk(is.list(.sap_cache_get("NETWORKS")), TRUE, "cache key lookup is case-insensitive")

# ── B. THE regression test — a fetch for one client must never leak into
#      another client's (or a staff-at-home session's) lookup ────────────────

.GlobalEnv$.sap_global_cache <- list()
.sap_cache_set("networks", data.frame(invoice = "NG-001", amount = 5000), NULL)

hd_entry <- .sap_cache_get("hd-admin")
.chk(is.null(hd_entry), TRUE,
     "a Networks cache write never seeds a hd-admin (staff-at-home) lookup")

other_entry <- .sap_cache_get("hopdesk")
.chk(is.null(other_entry), TRUE,
     "a Networks cache write never seeds an unrelated client's lookup")

networks_entry <- .sap_cache_get("networks")
.chk(networks_entry$AR$invoice, "NG-001",
     "the writing client's own lookup still sees its own data")

# ── C. Freshness window ───────────────────────────────────────────────────────

fresh_entry <- list(AR = NULL, AP = NULL, fetched_at = Sys.time())
.chk(.sap_cache_fresh(fresh_entry), TRUE, "an entry fetched just now is fresh")

stale_entry <- list(AR = NULL, AP = NULL, fetched_at = Sys.time() - 301)
.chk(.sap_cache_fresh(stale_entry), FALSE, "an entry older than 300s is not fresh")

.chk(.sap_cache_fresh(NULL), FALSE, "a NULL entry (never cached) is not fresh")
.chk(.sap_cache_fresh(list(AR = NULL, AP = NULL, fetched_at = NULL)), FALSE,
     "an entry with no fetched_at is not fresh")

cat("\n")
