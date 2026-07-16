# =============================================================================
# tests/test_saas_erp_tiers.R
# Stage 3: erp_access_allowed() — who may add/edit/test/delete ERP connections.
# Sourced by _run_saas.R — do not source directly.
# =============================================================================

cat("── ERP Connection Access Gate ───────────────────────────────────────────\n")

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

# ── Client-side tiers: only dev is allowed ────────────────────────────────────

.chk(erp_access_allowed("dev",      FALSE, FALSE), TRUE,  "client dev-tier user is allowed")
.chk(erp_access_allowed("admin",    FALSE, FALSE), FALSE, "client admin-tier user is rejected")
.chk(erp_access_allowed("finance",  FALSE, FALSE), FALSE, "client finance-tier user is rejected")
.chk(erp_access_allowed("analysis", FALSE, FALSE), FALSE, "client analysis-tier user is rejected")

# ── Staff sessions: only while mid-jump — being staff is never a standing bypass

.chk(erp_access_allowed("hopdesk",   TRUE, TRUE),  TRUE,  "hopdesk staff mid-jump is allowed")
.chk(erp_access_allowed("principal", TRUE, TRUE),  TRUE,  "principal mid-jump is allowed")
.chk(erp_access_allowed("hopdesk",   TRUE, FALSE), FALSE, "hopdesk staff at home (not jumped) is rejected")
.chk(erp_access_allowed("principal", TRUE, FALSE), FALSE, "principal at home (not jumped) is rejected — outranking dev is not enough")

# ── Unknown/malformed tier never accidentally passes ──────────────────────────

.chk(erp_access_allowed("not_a_real_tier", FALSE, FALSE), FALSE, "unknown tier is rejected, not treated as NA-passes")
.chk(erp_access_allowed(NULL, FALSE, FALSE), FALSE, "NULL tier is rejected")

cat("\n")
