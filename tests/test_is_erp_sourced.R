# =============================================================================
# tests/test_is_erp_sourced.R
# Stage 2 of the ledger-integrity master plan (see
# docs/LEDGER_INTEGRITY_MASTER_PLAN.md): generalize the recurring
# `is.na(source) | source == "sap"` idiom into one shared helper,
# is_erp_sourced() (R/global.R), and migrate every confirm/delete/stage
# call site that used to hardcode it onto the helper instead. The point is
# that a FUTURE second ERP integration only needs to assign its rows a
# distinct `source` value — no call site should need to change.
#
# Three kinds of checks:
#   1. Unit tests on is_erp_sourced() itself.
#   2. Static source scan — confirms the target call sites now use the
#      helper (same style as tests/test_confirmed_logic_stage_a.R's scan).
#   3. Behavioral regression with a SECOND, non-"sap" synthetic ERP-like
#      source value, proving the two migrated masking computations
#      (the Quitar SAP-confirmed guard, and the confirm handler's
#      sap_fact_ids/man_fact_ids split) treat every ERP source alike.
#
# CM43: the rule is the multi-ERP one of 2026-09-14 (2364f4c, R/global.R
# above is_erp_sourced()): ERP-sourced = NA or anything that is not
# "manual"/"provision", so a second connector's erp_type (e.g.
# "sap_b1_service_layer", "quickbooks") is recognized with no code to touch.
# This file still asserted the earlier, narrow rule (only "sap"/NA).
# =============================================================================

cat("── Stage 2: is_erp_sourced() helper ─────────────────────────────────────\n")

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

# ── 1. Unit tests on is_erp_sourced() ────────────────────────────────────────
{
  .chk(is_erp_sourced(NA_character_), TRUE,  "NA source is ERP-sourced (documented default)")
  .chk(is_erp_sourced("sap"),         TRUE,  "'sap' is ERP-sourced")
  .chk(is_erp_sourced("manual"),      FALSE, "'manual' is not ERP-sourced")
  .chk(is_erp_sourced("provision"),   FALSE, "'provision' is not ERP-sourced")
  .chk(is_erp_sourced("quickbooks"),  TRUE,
       "another connector's source ('quickbooks') is ERP-sourced (multi-ERP rule, 2026-09-14)")
  .chk(is_erp_sourced("sap_b1_service_layer"), TRUE,
       "SAP_B1_ERP_TYPE ('sap_b1_service_layer') is ERP-sourced")
  .chk(is_erp_sourced(c("sap", NA, "manual", "provision")),
       c(TRUE, TRUE, FALSE, FALSE),
       "vectorized input works elementwise")
}

# ── 2. Static scan: target call sites use the helper ────────────────────────
{
  .count_calls <- function(file) {
    txt <- readLines(file, warn = FALSE)
    sum(grepl("is_erp_sourced\\(", txt))
  }
  .no_literal_leftover <- function(file, pattern) {
    txt <- readLines(file, warn = FALSE)
    !any(grepl(pattern, txt))
  }

  .chk(.count_calls("R/pagar_hoy_module.R") >= 4, TRUE,
       "R/pagar_hoy_module.R: at least 4 is_erp_sourced() call sites (2 Quitar guards + AP/AR confirm split)")
  .chk(.count_calls("R/ledger_module.R") >= 2, TRUE,
       "R/ledger_module.R: at least 2 is_erp_sourced() call sites (stage_all/stage_selected re-stage guards)")

  # The old literal idiom should be gone from the migrated sites. A couple of
  # sites were deliberately LEFT as literal "sap" checks (papelera ghost
  # detection's stricter !is.na()&==, and the already-normalized key_sources
  # vapply) — so this only asserts the OLD is.na(x)|x=="sap" OR-form is gone,
  # not that literally every "sap" string vanished from either file.
  .chk(.no_literal_leftover("R/pagar_hoy_module.R",
                            'is\\.na\\([a-zA-Z_$\\.\\[\\]"]+\\)\\s*\\|\\s*[a-zA-Z_$\\.\\[\\]"]+\\s*==\\s*"sap"'),
       TRUE,
       "R/pagar_hoy_module.R: no leftover is.na(x)|x=='sap' literal idiom")
  .chk(.no_literal_leftover("R/ledger_module.R",
                            'is\\.na\\([a-zA-Z_$\\.\\[\\]"]+\\)\\s*\\|\\s*[a-zA-Z_$\\.\\[\\]"]+\\s*==\\s*"sap"'),
       TRUE,
       "R/ledger_module.R: no leftover is.na(x)|x=='sap' literal idiom")
}

# ── 3. Behavioral regression with a synthetic non-"sap" source ──────────────
{
  # Simulates the Quitar ERP-confirmed guard (pagar_hoy_module.R): a
  # confirmed row from ANY ERP (another connector included) is ERP-safe;
  # a confirmed manual row is not (multi-ERP rule, 2026-09-14).
  rows <- data.frame(
    status = c("confirmed", "confirmed", "confirmed", "confirmed"),
    source = c("sap", "quickbooks", NA_character_, "manual"),
    stringsAsFactors = FALSE
  )
  sap_conf <- rows[!is.na(rows$status) & rows$status == "confirmed" &
                   is_erp_sourced(rows$source), , drop = FALSE]
  .chk(nrow(sap_conf), 3L,
       "Quitar guard: matches the 'sap', 'quickbooks' and NA rows, not the 'manual' one")
  .chk(sort(ifelse(is.na(sap_conf$source), "NA", sap_conf$source)), c("NA", "quickbooks", "sap"),
       "Quitar guard: the matched rows are exactly sap + quickbooks + NA")

  # Simulates the confirm handler's sap_fact_ids/man_fact_ids split
  # (pagar_hoy_module.R:1391-1398 / 1594-1601).
  factura_rows <- data.frame(
    id     = c("a", "b", "c", "d"),
    source = c("sap", "manual", "provision", NA_character_),
    stringsAsFactors = FALSE
  )
  sap_fact_ids <- factura_rows$id[is_erp_sourced(factura_rows$source)]
  man_fact_ids <- factura_rows$id[!is_erp_sourced(factura_rows$source)]
  .chk(sort(sap_fact_ids), c("a", "d"),
       "confirm split: sap_fact_ids is exactly the 'sap' row and the NA row (kept, status flipped)")
  .chk(sort(man_fact_ids), c("b", "c"),
       "confirm split: man_fact_ids is exactly 'manual' and 'provision' (physically removed)")
}

cat(sprintf("\n=== is_erp_sourced() results: %d passed, %d failed ===\n", .pass, .fail))
