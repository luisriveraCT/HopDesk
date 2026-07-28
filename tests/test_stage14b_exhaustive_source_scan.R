# =============================================================================
# tests/test_stage14b_exhaustive_source_scan.R
# Open item raised by Stage 14b (docs/LEDGER_INTEGRITY_MASTER_PLAN.md): the
# "no source column at all" / "source hardcoded to sap" bug class has now
# been found twice by incident-by-incident investigation -- once in
# stage_all/stage_sel/handle_invoice_action (Stage 6), once in
# cart_inv_click (Stage 14b, same day). Neither discovery came from a test
# that looks at ALL staging sites as a set; each came from chasing one
# specific live incident. A fifth site could hide the same way.
#
# This test closes that blind spot with two layers:
#
# 1. An exhaustive, grep-based inventory of every upsert_pagar_hoy() call
#    site in R/*.R -- the one function every staging path funnels through
#    to actually write a new/updated row into pagar_hoy_db. If a future
#    change adds or removes a call site, the count assertions below FAIL
#    LOUDLY, forcing a conscious decision (bump the count here AND audit
#    the new site for source-propagation) instead of silently missing it.
#
# 2. Static-scan coverage for the two known sites that had no dedicated
#    source-propagation test until now (both are currently correct, just
#    previously unverified by any test):
#      - ledger_module.R's cart_<i> group-level "+" button
#      - staging_browse_module.R's abono-staging observer
#    The other 7 sites already have dedicated coverage elsewhere (see the
#    "site inventory" list below for exactly where).
#
# Site inventory (9 total, as of 2026-07-24) -- keep this list in sync with
# the grep below; every entry names the test file that actually verifies it:
#   1. R/ledger_module.R      stage_all             -- tests/test_stage_source_propagation.R
#   2. R/ledger_module.R      stage_sel             -- tests/test_stage_source_propagation.R
#   3. R/ledger_module.R      cart_<i> (group btn)  -- THIS FILE (previously untested)
#   4. R/ledger_module.R      cart_inv_click        -- tests/test_stage_source_propagation.R
#   5. R/search_module.R      handle_invoice_action -- tests/test_stage_source_propagation.R
#   6. R/interco_module.R     .ic_send_rows         -- tests/test_stage_source_propagation.R
#   7. R/treasury_map_module.R send_to_agenda       -- tests/test_stage_source_propagation.R
#   8. R/pasivos_module.R     (via stage_manual_row_to_agenda) -- tests/test_stage6_agenda_derivation.R
#   9. R/staging_browse_module.R ab_rows (abono)    -- THIS FILE (previously untested;
#      source is inert for abono rows -- do_confirm_ap_<emp> splits on tipo_item=="abono"
#      BEFORE ever checking is_erp_sourced -- but it's still asserted here so a future
#      refactor can't silently drop the column without this test noticing)
# =============================================================================

cat("── Exhaustive scan: every pagar_hoy_db staging site sets source ────────\n")

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

r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
hits <- do.call(rbind, lapply(r_files, function(f) {
  txt <- readLines(f, warn = FALSE)
  ln  <- grep("upsert_pagar_hoy\\(", txt)
  if (!length(ln)) return(NULL)
  data.frame(file = f, line = ln, stringsAsFactors = FALSE)
}))

# ── 1. Exhaustive inventory: exact total, and exact per-file counts ────────
{
  .chk(nrow(hits), 9L,
       "exactly 9 upsert_pagar_hoy() call sites exist across R/*.R -- if this changed, a staging site was added or removed; audit the new one for source-propagation (see this file's header inventory) before updating this expected count")

  by_file <- table(basename(hits$file))
  expected_by_file <- c(
    "interco_module.R"        = 1L,
    "ledger_module.R"         = 4L,
    "pasivos_module.R"        = 1L,
    "search_module.R"         = 1L,
    "staging_browse_module.R" = 1L,
    "treasury_map_module.R"   = 1L
  )
  for (fn in names(expected_by_file)) {
    got <- if (fn %in% names(by_file)) as.integer(by_file[[fn]]) else 0L
    .chk(got, expected_by_file[[fn]],
         sprintf("%s has exactly %d call site(s) (catches a site moving between files while the total stays the same)",
                 fn, expected_by_file[[fn]]))
  }
}

# ── 2a. ledger_module.R's cart_<i> group button -- previously untested ─────
{
  txt <- readLines("R/ledger_module.R", warn = FALSE)
  start <- grep('observeEvent\\(input\\[\\[paste0\\("cart_", i\\)\\]\\],', txt)
  .chk(length(start) > 0, TRUE, "found the cart_<i> group-button observer to scan")
  if (length(start)) {
    end <- grep("Expand toggle", txt)
    end <- end[end > start[1]][1] %||% (start[1] + 90)
    block <- paste(sub("#.*$", "", txt[start[1]:end]), collapse = "\n")
    .chk(grepl("src_lookup", block), TRUE,
         "cart_<i> group button builds a src_lookup table (reads the row's real source)")
    .chk(grepl('!= "manual"', block), TRUE,
         "cart_<i> normalizes anything that isn't literally \"manual\" to \"sap\", matching every other fixed site's rule")
  }
}

# ── 2b. staging_browse_module.R's abono-staging observer -- previously
# untested (source is inert here, but must stay explicitly set, not NA) ────
{
  txt <- readLines("R/staging_browse_module.R", warn = FALSE)
  start <- grep("observeEvent\\(input\\$ab_rows,", txt)
  .chk(length(start) > 0, TRUE, "found the ab_rows (abono staging) observer to scan")
  if (length(start)) {
    block <- paste(txt[start[1]:min(start[1] + 90, length(txt))], collapse = "\n")
    .chk(grepl('source\\s*=\\s*"manual"', block), TRUE,
         "abono rows staged here always carry an explicit source (\"manual\"), never left unset")
  }
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
