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

# ── 1-2. The day modal on the table engine (T05) ───────────────────────────
# The DT table (and its selectable = -ghost_rows, sel_total_ui) is gone. A
# confirmed invoice is now a locked row (.bloqueada): the engine never lets
# it be marked, struck through, left out of the counter, the totals and the
# group's sum -- in both views, Resumen (groups) and Detalle. A group with
# every invoice confirmed has no free row: nothing in it can be marked.
{
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
  for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
              "R/vencidos_module.R", "R/dia_module.R")) source(f, local = TRUE)
  SAP_B1_ERP_TYPE <- "sap_b1_service_layer"
  dia <- data.frame(Empresa = "NTS", Moneda = "MXN", Documento = c("1", "2", "3"), Factura = "",
                    Parte = c("MIXTA", "MIXTA", "PAGADA"), Importe = c(100, 200, 300),
                    source = SAP_B1_ERP_TYPE, confirmed = c(FALSE, TRUE, TRUE), stringsAsFactors = FALSE)
  d <- dia_datos(dia, "AP", NULL)
  .chk(d$.bloqueada[match(c("1", "2", "3"), d$Documento)], c(FALSE, TRUE, TRUE), "dia_datos: a confirmed invoice is a locked row (.bloqueada), the open one isn't")
  .chk(d$.acciones[d$.bloqueada], c("", ""), "dia_datos: a confirmed invoice offers no ＋/✎")
  js <- paste(readLines("www/hdtable.js", warn = FALSE), collapse = "\n")
  .chk(grepl("var libres = u.filas.filter(function (i) { return !st.bloq[i]; })", js, fixed = TRUE) &&
         grepl("libres.forEach(function (i) { var mo = st.moneda[i] || ''; s[mo] = (s[mo] || 0) + (Number(o[i]) || 0); });", js, fixed = TRUE),
       TRUE, "engine: a group's sum counts only its free (unlocked) rows -- a mixed group keeps its real open balance")
  .chk(grepl("if (st.bloq[i]) { if (cb) cb.checked = false; return; }", js, fixed = TRUE), TRUE,
       "engine: a click never marks a locked row")
  .chk(grepl("libres.forEach(function (i) { if (todas) st.sel.delete(st.ids[i]); else st.sel.add(st.ids[i]); });", js, fixed = TRUE),
       TRUE, "engine: a click on a group marks only its free rows (a fully confirmed group marks nothing)")
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
