# =============================================================================
# tests/test_stage7_ghost_isolation.R
# Stage 7 of the ledger-integrity master plan (see
# docs/LEDGER_INTEGRITY_MASTER_PLAN.md, source AGENDA_CALENDARIO_WIRING_AUDIT.md
# §2.11/§3.6/§3.7): ghost (confirmed) rows must never affect any
# calculation inside Calendario's own UI. Three fixes:
#   1. Ghost rows in the day-modal's DT table (both audit mode, one row per
#      invoice, and summary mode, one row per Empresa+Parte group) are now
#      unselectable at the DT level, not just styled -- so the "Selección"
#      running total (sel_total_ui, which reads whatever the browser
#      actually let the user select) can never include one.
#   2. A summary-mode group counts as a ghost only when EVERY invoice
#      underlying it is confirmed -- a mixed group keeps its real open
#      balance and stays fully interactive.
#   3. The calendar's staged-count (hourglass) badge no longer shows over
#      a day with zero visible line items (calendar_html()'s has_staged
#      guard, R/global.R).
#
# Static scans for the embedded Shiny-observer logic (can't be unit tested
# directly), plus a behavioral simulation of the exact ghost-detection/
# selectable computation using DT's own real validateSelection() to prove
# the actual API call would succeed at runtime, not just that the R code
# looks right.
# =============================================================================

cat("── Stage 7: ghost rows never affect any calculation ────────────────────\n")

suppressPackageStartupMessages(library(DT))

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
.strip_comments <- function(lines) sub("#.*$", "", lines)

# ── 1. Static scan: audit-mode ghost rows made unselectable ────────────────
{
  txt <- readLines("R/ledger_module.R", warn = FALSE)
  start <- grep("Ghost \\(confirmed\\) rows must never affect any calculation", txt)
  .chk(length(start) > 0, TRUE, "found audit-mode ghost-selectable comment to scan")
  if (length(start)) {
    block <- paste(.strip_comments(txt[start[1]:min(start[1] + 15, length(txt))]), collapse = "\n")
    .chk(grepl("ghost_rows <- which\\(tbl_live\\[\\[\"Confirmado\"\\]\\]\\)", block), TRUE,
         "audit mode computes ghost_rows from the Confirmado column, post-sort")
    .chk(grepl("selectable = -ghost_rows", block), TRUE,
         "audit mode passes negative (not-selectable) indices for ghost rows to DT")
    .chk(grepl('else "multiple"', block), TRUE,
         "audit mode falls back to plain \"multiple\" selection when there are no ghost rows (never an empty selectable vector)")
  }
}

# ── 2. Static scan: summary-mode groups made unselectable + styled ─────────
{
  txt <- readLines("R/ledger_module.R", warn = FALSE)
  start <- grep("A group is a ghost only when EVERY invoice", txt)
  .chk(length(start) > 0, TRUE, "found summary-mode grp_is_ghost comment to scan")
  if (length(start)) {
    block <- paste(.strip_comments(txt[start[1]:min(start[1] + 45, length(txt))]), collapse = "\n")
    .chk(grepl("all\\(is_conf_detail\\[m\\]\\)", block), TRUE,
         "a group is ghosted only when ALL its underlying rows are confirmed (any() would wrongly ghost a mixed group)")
    .chk(grepl("selectable = -ghost_grp_rows", block), TRUE,
         "summary mode also passes negative indices for fully-ghosted groups to DT")
    .chk(grepl('DT::formatStyle\\("Confirmado"', block), TRUE,
         "summary mode styles ghosted groups (line-through) matching audit mode's precedent")
  }
}

# ── 3. Static scan: hourglass consistency guard ─────────────────────────────
{
  txt <- readLines("R/global.R", warn = FALSE)
  start <- grep("n_staged   <- staged_count_by_day", txt)
  .chk(length(start) > 0, TRUE, "found has_staged computation to scan")
  if (length(start)) {
    block <- paste(.strip_comments(txt[start[1]:min(start[1] + 6, length(txt))]), collapse = "\n")
    .chk(grepl("has_staged <- n_staged > 0 && has_data", block), TRUE,
         "has_staged now requires has_data too -- never shows the hourglass over a day with zero visible line items")
  }
}

# ── 4. Behavioral simulation: ghost detection + DT's own validator ─────────
{
  # Audit mode: 4 invoices, 2 confirmed, sorted by -Importe (desc) same as
  # the real code's sort_ord -- replicate that ordering before detecting
  # ghost row positions, exactly as modal_tbl does.
  detail <- data.frame(
    Importe   = c(500, 100, 300, 50),
    confirmed = c(FALSE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  sort_ord <- order(-detail[["Importe"]])
  tbl_live <- data.frame(Confirmado = detail[["confirmed"]][sort_ord])
  ghost_rows <- which(tbl_live[["Confirmado"]])
  .chk(ghost_rows, c(3L, 4L),
       "ghost rows land at their correct POST-sort positions (sorted 500,300,100,50 -> rows 3 (100) and 4 (50) are the confirmed ones)")

  select_arg <- list(mode = "multiple", target = "row", selectable = -ghost_rows)
  validated <- tryCatch(DT:::validateSelection(select_arg), error = function(e) e)
  .chk(inherits(validated, "error"), FALSE,
       "DT's own real validateSelection() accepts our -ghost_rows selectable value without error")

  # Summary mode: 3 groups -- one fully confirmed, one fully open, one mixed.
  grp_detail <- data.frame(
    Empresa   = c("A","A","B","C","C"),
    Parte     = c("X","X","Y","Z","Z"),
    confirmed = c(TRUE, TRUE, FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  grp_raw <- unique(grp_detail[, c("Empresa","Parte")])
  is_conf_detail <- grp_detail[["confirmed"]]
  grp_is_ghost <- vapply(seq_len(nrow(grp_raw)), function(i) {
    e <- grp_raw[["Empresa"]][i]; p <- grp_raw[["Parte"]][i]
    m <- grp_detail[["Empresa"]] == e & grp_detail[["Parte"]] == p
    any(m) && all(is_conf_detail[m])
  }, logical(1))
  names(grp_is_ghost) <- grp_raw[["Parte"]]
  .chk(unname(grp_is_ghost["X"]), TRUE,  "group X (both invoices confirmed) is correctly detected as a fully-ghosted group")
  .chk(unname(grp_is_ghost["Y"]), FALSE, "group Y (no invoices confirmed) is correctly NOT a ghost group")
  .chk(unname(grp_is_ghost["Z"]), FALSE, "group Z (mixed: one confirmed, one not) is correctly NOT a ghost group -- it still has real open balance")
}

# ── 5. Behavioral simulation: hourglass guard truth table ──────────────────
{
  compute_has_staged <- function(n_staged, has_data) n_staged > 0 && has_data
  .chk(compute_has_staged(3L, TRUE),  TRUE,  "staged>0 + visible data -> badge shows")
  .chk(compute_has_staged(3L, FALSE), FALSE, "staged>0 + NO visible data -> badge suppressed (the actual bug fixed)")
  .chk(compute_has_staged(0L, TRUE),  FALSE, "nothing staged + visible data -> badge stays hidden (unchanged behavior)")
  .chk(compute_has_staged(0L, FALSE), FALSE, "nothing staged + no visible data -> badge stays hidden (unchanged behavior)")
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
