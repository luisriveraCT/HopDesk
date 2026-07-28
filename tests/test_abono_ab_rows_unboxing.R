# =============================================================================
# tests/test_abono_ab_rows_unboxing.R
# Live bug found 2026-07-27: checking exactly ONE invoice in the Abono
# Parcial modal and clicking "Enviar a Agenda" did nothing -- no error
# visible to the user, no row staged, modal stayed open.
#
# Root cause: the client sends a JSON array of checked-row objects to
# Shiny.setInputValue('ab_rows', rows, ...). jsonlite/Shiny simplifies that
# array differently depending on how many rows it contains -- for exactly
# one row, the array gets unboxed into a single flat named list (the row's
# fields directly), not a list containing one list. The observer's
# `for (r_raw in rows_data)` then iterated over the row's individual FIELD
# VALUES (a string, then another string, a number, ...) instead of over
# rows, and `r_raw$saldo` on a plain scalar throws ("$ operator is invalid
# for atomic vectors") -- silently, since nothing in the click path surfaces
# a Shiny observer error to the user by default.
#
# This is the exact same failure class R/ledger_module.R's
# .cart_inv_sel_to_keys() already had to guard against for cart_inv_sel
# (see tests/test_cart_inv_sel.R) -- the abono staging code just never got
# the same treatment. Fixed with an analogous normalizer,
# .normalize_ab_rows() in R/staging_browse_module.R.
#
# Also covers the sibling live bug found in the same report: the amount
# input was editable before its checkbox was ever checked (no `disabled`
# attribute on initial render).
# =============================================================================

cat("── Abono Parcial: ab_rows shape normalization + disabled-by-default ───\n")

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
.extract_fn("R/staging_browse_module.R", ".normalize_ab_rows")

# ── 1. The exact live scenario: exactly one row checked ─────────────────────
{
  # Shape (c): single row, auto-unboxed by jsonlite -- a flat named list,
  # not a list containing one list. This is what input$ab_rows actually
  # looked like live when only one checkbox was checked.
  single_unboxed <- list(idx = "1", empresa = "Networks & Logistics",
                          moneda = "MXN", documento = "6563",
                          parte = "Networks Trucking Services", codigo = "",
                          referencia = "6563", fecha_venc = "2026-07-22",
                          saldo = 23209, importe = 23209)

  out <- .normalize_ab_rows(single_unboxed)
  .chk(length(out), 1L, "a single unboxed row normalizes to a list of exactly 1 row")
  .chk(is.list(out[[1]]), TRUE, "that 1 row is itself a list (has $-accessible fields), not a bare scalar")
  .chk(out[[1]]$saldo, 23209, "the row's saldo field survives normalization intact")
  .chk(out[[1]]$importe, 23209, "the row's importe field survives normalization intact")

  # Control: reproduce the ORIGINAL crash this fix prevents, proving the
  # bug is real and not just a hypothesis.
  crashes <- tryCatch({ single_unboxed$saldo; for (r in single_unboxed) r$saldo; FALSE },
                       error = function(e) TRUE)
  .chk(crashes, TRUE,
       "control: iterating the UN-normalized single-row shape directly does crash ($ on atomic vector) -- confirms the bug was real")
}

# ── 2. Two rows checked -- jsonlite's other simplification shapes ──────────
{
  # Shape (b): named list of vectors (2 rows, no nested data.frame).
  two_rows_vecs <- list(idx = c("1", "2"), empresa = c("A", "B"),
                        moneda = c("MXN", "MXN"), documento = c("D1", "D2"),
                        parte = c("P1", "P2"), codigo = c("", ""),
                        referencia = c("D1", "D2"),
                        fecha_venc = c("2026-07-01", "2026-07-02"),
                        saldo = c(100, 200), importe = c(50, 150))
  out2 <- .normalize_ab_rows(two_rows_vecs)
  .chk(length(out2), 2L, "two rows as a named-list-of-vectors normalizes to a list of 2 rows")
  .chk(out2[[1]]$documento, "D1", "row 1 keeps its own documento, not row 2's")
  .chk(out2[[2]]$documento, "D2", "row 2 keeps its own documento, not row 1's")
  .chk(out2[[1]]$importe, 50, "row 1 keeps its own importe")
  .chk(out2[[2]]$importe, 150, "row 2 keeps its own importe")

  # Shape (a): jsonlite simplified straight to a data.frame.
  two_rows_df <- data.frame(idx = c("1", "2"), empresa = c("A", "B"),
                            moneda = c("MXN", "MXN"), documento = c("D1", "D2"),
                            saldo = c(100, 200), importe = c(50, 150),
                            stringsAsFactors = FALSE)
  out3 <- .normalize_ab_rows(two_rows_df)
  .chk(length(out3), 2L, "two rows as a data.frame normalizes to a list of 2 rows")
  .chk(out3[[1]]$documento, "D1", "data.frame row 1 keeps its own documento")
  .chk(out3[[2]]$importe, 150, "data.frame row 2 keeps its own importe")

  # Shape (d): already correct -- passes through unchanged.
  already_correct <- list(list(idx = "1", saldo = 100, importe = 50),
                          list(idx = "2", saldo = 200, importe = 150))
  out4 <- .normalize_ab_rows(already_correct)
  .chk(identical(out4, already_correct), TRUE,
       "an already-correct list-of-lists passes through completely unchanged")
}

# ── 3. Edge cases ────────────────────────────────────────────────────────────
{
  .chk(length(.normalize_ab_rows(NULL)), 0L, "NULL input normalizes to an empty list, not an error")
  .chk(length(.normalize_ab_rows(list())), 0L, "an empty list stays empty")
}

# ── 4. Static scan: the observer actually uses the normalizer ──────────────
{
  txt <- readLines("R/staging_browse_module.R", warn = FALSE)
  start <- grep("observeEvent\\(input\\$ab_rows,", txt)
  .chk(length(start) > 0, TRUE, "found the ab_rows observer to scan")
  if (length(start)) {
    block <- paste(sub("#.*$", "", txt[start[1]:min(start[1] + 15, length(txt))]), collapse = "\n")
    .chk(grepl("rows_data <- \\.normalize_ab_rows\\(input\\$ab_rows\\)", block), TRUE,
         "the observer normalizes input$ab_rows before iterating over it")
  }
}

# ── 5. Static scan: the amount input starts disabled ────────────────────────
{
  txt <- readLines("R/staging_browse_module.R", warn = FALSE)
  start <- grep('class="form-control form-control-sm ab-amt"', txt)
  .chk(length(start) > 0, TRUE, "found the amount <input> markup to scan")
  if (length(start)) {
    block <- paste(txt[start[1]:min(start[1] + 12, length(txt))], collapse = "\n")
    .chk(grepl("'\\s*disabled'", block), TRUE,
         "the amount input now renders with disabled by default, matching the unchecked checkbox state")
  }
}

# ── 6. Shape (e): single row unboxes to a plain NAMED ATOMIC VECTOR ────────
# Live crash found 2026-07-27 (session #6, ~30 min in, during Stage 21's own
# hands-on verification): "$ operator is invalid for atomic vectors" at the
# same r_raw$saldo line -- is.list() is FALSE for a named atomic vector even
# though `[["idx"]]` still resolves, so this shape fell through shape (c)'s
# guard undetected and reproduced the exact bug shape (c) was meant to fix.
{
  single_atomic <- c(idx = "1", empresa = "Networks & Logistics",
                      moneda = "MXN", documento = "6563",
                      parte = "Networks Trucking Services", codigo = "",
                      referencia = "6563", fecha_venc = "2026-07-22",
                      saldo = "23209", importe = "23209")
  .chk(is.list(single_atomic), FALSE, "control: this shape really is atomic, not a list (proves it's distinct from shape (c))")

  out5 <- .normalize_ab_rows(single_atomic)
  .chk(length(out5), 1L, "a named atomic vector normalizes to a list of exactly 1 row")
  .chk(is.list(out5[[1]]), TRUE, "that 1 row is coerced into a list (has $-accessible fields)")
  .chk(as.numeric(out5[[1]]$saldo), 23209, "the row's saldo field survives normalization intact")
  .chk(as.numeric(out5[[1]]$importe), 23209, "the row's importe field survives normalization intact")

  # Control: reproduce the ORIGINAL crash this fix prevents.
  crashes5 <- tryCatch({ for (r in single_atomic) r$saldo; FALSE },
                       error = function(e) TRUE)
  .chk(crashes5, TRUE,
       "control: iterating the UN-normalized atomic-vector shape directly does crash ($ on atomic vector) -- confirms the bug was real")
}

# ── 7. Static scan: the observer never crashes on an unrecognized shape ────
{
  txt <- readLines("R/staging_browse_module.R", warn = FALSE)
  start <- grep("observeEvent\\(input\\$ab_rows,", txt)
  .chk(length(start) > 0, TRUE, "found the ab_rows observer to scan")
  if (length(start)) {
    end <- grep("^  \\}, ignoreInit = TRUE\\)$", txt)
    end <- end[end > start[1]][1]
    block <- paste(sub("#.*$", "", txt[start[1]:end]), collapse = "\n")
    .chk(grepl("tryCatch\\(\\{", block), TRUE,
         "the observer body is wrapped in tryCatch -- an unanticipated shape notifies, it doesn't crash the session")
    .chk(grepl("vapply\\(rows_data, is\\.list, logical\\(1\\)\\)", block), TRUE,
         "malformed (non-list) rows are detected defensively before the loop that does $ access")
  }
}

# ── 8. Shape (f): MULTIPLE rows flatten into ONE vector with repeated keys ──
# Live bug found 2026-07-28: "no matter how many items I select, only the
# first one gets sent" -- checking 2+ rows and clicking "Enviar a Agenda"
# always staged only the first row, silently. Reproduced live via a headless
# Chrome session (chromote) driving the actual .ab_js click handler against
# a minimal Shiny harness: checking 2 rows produced this exact 20-element
# named character vector server-side (idx/empresa/.../importe repeated
# once per row, in order) -- not shapes (a)-(e), a distinct failure mode.
{
  two_flat <- c(idx = "1", empresa = "Networks & Logistics", moneda = "MXN",
                documento = "D001", parte = "Parte Uno", codigo = "C1",
                referencia = "D001", fecha_venc = "2026-08-01",
                saldo = "1234.56", importe = "1234.56",
                idx = "2", empresa = "Networks Trucking", moneda = "MXN",
                documento = "D002", parte = "Parte Dos", codigo = "C2",
                referencia = "D002", fecha_venc = "2026-08-02",
                saldo = "789.1", importe = "300.25")
  .chk(is.list(two_flat), FALSE, "control: this shape really is atomic, not a list")
  .chk(sum(names(two_flat) == "idx"), 2L, "control: 'idx' really does repeat twice in this shape (proves it's distinct from shape (e))")

  out6 <- .normalize_ab_rows(two_flat)
  .chk(length(out6), 2L, "two flattened rows normalize to a list of exactly 2 rows, not 1")
  .chk(out6[[1]]$documento, "D001", "row 1 keeps its own documento")
  .chk(out6[[2]]$documento, "D002", "row 2 keeps its own documento, not row 1's")
  .chk(as.numeric(out6[[1]]$saldo), 1234.56, "row 1's saldo survives with full decimal precision")
  .chk(as.numeric(out6[[2]]$saldo), 789.1, "row 2 keeps its OWN saldo, not row 1's")
  .chk(as.numeric(out6[[2]]$importe), 300.25, "row 2's edited partial-abono importe (different from its own saldo) survives intact -- proves amounts aren't cross-contaminated between rows")

  # Control: reproduce the ORIGINAL bug this fix prevents -- the old code's
  # `n <- length(rows_data[["idx"]])` sees only the FIRST "idx" match.
  old_n <- length(two_flat[["idx"]])
  .chk(old_n, 1L,
       "control: the OLD length-check saw only 1 row here (rows_data[[\"idx\"]] only ever returns the first match) -- confirms the bug was real and would have silently dropped row 2")

  # Three rows, to confirm this isn't special-cased to exactly two.
  three_flat <- c(two_flat, c(idx = "3", empresa = "NCS Company", moneda = "MXN",
                              documento = "D003", parte = "Parte Tres", codigo = "C3",
                              referencia = "D003", fecha_venc = "2026-08-03",
                              saldo = "50000.99", importe = "50000.99"))
  out7 <- .normalize_ab_rows(three_flat)
  .chk(length(out7), 3L, "three flattened rows normalize to a list of exactly 3 rows")
  .chk(out7[[3]]$documento, "D003", "row 3 keeps its own documento")
  .chk(as.numeric(out7[[3]]$saldo), 50000.99, "row 3's large saldo survives without truncation or scientific notation")
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
