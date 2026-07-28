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
#      sap_fact_ids/man_fact_ids split) still treat "sap" correctly and do
#      NOT accidentally start treating other non-manual/provision values as
#      ERP-safe just because they're not literally "sap" — is_erp_sourced()
#      is deliberately narrow (only "sap" or NA), not "anything unknown."
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
  .chk(is_erp_sourced("quickbooks"),  FALSE,
       "an unrecognized value is NOT treated as ERP-sourced (only 'sap'/NA are)")
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
  # Simulates the Quitar SAP-confirmed guard (pagar_hoy_module.R:1160-1162 /
  # 1221-1223): a confirmed row from a hypothetical future non-SAP ERP must
  # NOT be treated as ERP-safe by this helper alone -- until that ERP is
  # actually onboarded and its own source value is added wherever the
  # product decides ERP sources are enumerated. is_erp_sourced() itself only
  # ever recognizes "sap"/NA; this test guards against it silently widening.
  rows <- data.frame(
    status = c("confirmed", "confirmed", "confirmed"),
    source = c("sap", "quickbooks", NA_character_),
    stringsAsFactors = FALSE
  )
  sap_conf <- rows[!is.na(rows$status) & rows$status == "confirmed" &
                   is_erp_sourced(rows$source), , drop = FALSE]
  .chk(nrow(sap_conf), 2L,
       "Quitar guard: matches the 'sap' row and the NA row, not the synthetic 'quickbooks' row")
  .chk(sort(sap_conf$source %||% "NA"), sort(c("sap", NA_character_)) %||% "NA",
       "Quitar guard: the two matched rows are exactly sap + NA")

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
