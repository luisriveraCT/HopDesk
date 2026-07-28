# =============================================================================
# tests/test_cart_inv_sel.R
# Verify .cart_inv_sel_to_keys normalises inv_sel regardless of how
# Shiny/jsonlite deserialised the JS [{i,j},...] array.
# Run with: source("tests/test_cart_inv_sel.R")
# =============================================================================
cat("=== test_cart_inv_sel (move-mix bug) ===\n\n")

pass <- 0L; fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) {
    cat(" PASS:", label, "\n"); pass <<- pass + 1L
  } else {
    cat(" FAIL:", label, "\n"); fail <<- fail + 1L
  }
}

# ── Extract the normalisation logic so we can test it standalone ─────────────
# Kept in sync by hand with the real (nested, unextractable) function in
# R/ledger_module.R's .cart_inv_sel_to_keys() -- same convention this file
# already used before the shape (e)/(f)/defense-in-depth additions below.
normalize_inv_sel <- function(inv_sel) {
  .nm <- names(inv_sel)
  if (!is.null(.nm) && !is.null(inv_sel[["i"]])) {
    i_pos <- which(.nm == "i")
    if (length(i_pos) > 1L) {
      bounds <- c(i_pos, length(.nm) + 1L)
      return(lapply(seq_along(i_pos), function(k)
        as.list(inv_sel[bounds[k]:(bounds[k + 1L] - 1L)])))
    }
  }
  if (is.data.frame(inv_sel)) {
    inv_sel <- lapply(seq_len(nrow(inv_sel)), function(k)
      list(i = inv_sel[["i"]][k], j = inv_sel[["j"]][k]))
  } else if (is.list(inv_sel) && !is.null(inv_sel[["i"]])) {
    if (length(inv_sel[["i"]]) > 1L) {
      n <- length(inv_sel[["i"]])
      inv_sel <- lapply(seq_len(n), function(k)
        list(i = inv_sel[["i"]][k], j = inv_sel[["j"]][k]))
    } else {
      inv_sel <- list(inv_sel)
    }
  } else if (is.atomic(inv_sel) && !is.null(names(inv_sel)) && !is.null(inv_sel[["i"]])) {
    inv_sel <- list(as.list(inv_sel))
  }
  if (is.list(inv_sel)) {
    malformed <- !vapply(inv_sel, is.list, logical(1))
    if (any(malformed)) inv_sel <- inv_sel[!malformed]
  } else {
    inv_sel <- list()
  }
  inv_sel
}

# Helper: check result is list-of-lists with correct i/j
check_items <- function(norm, expected_ij) {
  if (!is.list(norm) || length(norm) != length(expected_ij)) return(FALSE)
  all(mapply(function(item, exp) {
    is.list(item) && !is.null(item$i) && !is.null(item$j) &&
      as.integer(item$i) == exp[1] && as.integer(item$j) == exp[2]
  }, norm, expected_ij))
}

# ── (a) data.frame — jsonlite simplifyDataFrame, multiple items ───────────────
{
  inv_sel_df <- data.frame(i = c(2L, 2L, 2L, 2L), j = c(1L, 2L, 3L, 4L))
  norm <- normalize_inv_sel(inv_sel_df)
  ok("(a) data.frame: 4 items normalised to list-of-lists",
     check_items(norm, list(c(2,1), c(2,2), c(2,3), c(2,4))))
}

# ── (b) named list with vector values — also from jsonlite ───────────────────
{
  inv_sel_vec <- list(i = c(2L, 2L, 2L, 2L), j = c(1L, 2L, 3L, 4L))
  norm <- normalize_inv_sel(inv_sel_vec)
  ok("(b) named list with vectors: 4 items normalised",
     check_items(norm, list(c(2,1), c(2,2), c(2,3), c(2,4))))
}

# ── (c) single item auto-unboxed — list(i=2, j=1) ────────────────────────────
{
  inv_sel_single <- list(i = 2L, j = 1L)
  norm <- normalize_inv_sel(inv_sel_single)
  ok("(c) single auto-unboxed item wrapped in list",
     check_items(norm, list(c(2, 1))))
}

# ── (d) already correct list-of-lists — passes through unchanged ─────────────
{
  inv_sel_ok <- list(list(i = 2L, j = 1L), list(i = 2L, j = 2L))
  norm <- normalize_inv_sel(inv_sel_ok)
  ok("(d) already-correct list-of-lists passes through unchanged",
     check_items(norm, list(c(2, 1), c(2, 2))))
}

# ── Previous crash scenario: mix of group + sub-rows ────────────────────────
# group row 1 selected (via cart_rows_sel, not tested here),
# 4 sub-rows of group 2 selected → inv_sel arrives as data.frame
{
  inv_sel_crash <- data.frame(i = c(2L, 2L, 2L, 2L), j = c(1L, 2L, 3L, 4L))
  norm <- tryCatch(normalize_inv_sel(inv_sel_crash), error = function(e) NULL)
  ok("crash scenario (mix group+subrows): no error, 4 items",
     !is.null(norm) && length(norm) == 4L)

  # Simulate the lapply that used to crash
  crash_result <- tryCatch({
    lapply(norm, function(item) {
      i <- as.integer(item$i); j <- as.integer(item$j)
      paste0("i=", i, " j=", j)
    })
  }, error = function(e) NULL)
  ok("crash scenario: lapply over normalised inv_sel does not crash",
     !is.null(crash_result) && length(crash_result) == 4L)
}

# ── Ensure old crash still crashes WITHOUT the fix (validates the test) ──────
{
  inv_sel_crash <- data.frame(i = c(2L, 2L), j = c(1L, 2L))
  crash_triggered <- tryCatch({
    lapply(inv_sel_crash, function(item) item$i)
    FALSE  # no crash = bug still present
  }, error = function(e) TRUE)
  ok("control: un-normalised data.frame DOES crash (validates test logic)",
     crash_triggered)
}

# ── (e) single item unboxes to a plain NAMED ATOMIC VECTOR ──────────────────
{
  inv_sel_atomic <- c(i = 2L, j = 1L)
  .chk_atomic <- !is.list(inv_sel_atomic)
  ok("(e) control: this shape really is atomic, not a list", .chk_atomic)
  norm <- normalize_inv_sel(inv_sel_atomic)
  ok("(e) single atomic-vector item normalises to a list of exactly 1 item",
     check_items(norm, list(c(2, 1))))
}

# ── (f) MULTIPLE items flatten into ONE vector with repeated keys ───────────
# Live bug found 2026-07-28, same incident class as ab_rows: selecting
# individual cart sub-rows (2+) and moving them crashed with "$ operator is
# invalid for atomic vectors". Reproduced live via a headless Chrome session
# (chromote) driving the actual www/cal_cart.js click handler
# (calCartToggleSubRow) against a minimal Shiny harness -- selecting 3
# sub-rows produced this exact 6-element named integer vector server-side.
{
  three_flat <- c(i = 1L, j = 1L, i = 1L, j = 2L, i = 1L, j = 3L)
  ok("(f) control: this shape really is atomic, not a list", !is.list(three_flat))
  ok("(f) control: 'i' really does repeat 3 times (proves it's distinct from shape (e))",
     sum(names(three_flat) == "i") == 3L)

  norm <- normalize_inv_sel(three_flat)
  ok("(f) three flattened items normalise to a list of exactly 3 items, not 1",
     check_items(norm, list(c(1, 1), c(1, 2), c(1, 3))))

  # Control: the OLD code's `length(inv_sel[["i"]])` sees only the FIRST "i".
  old_n <- length(three_flat[["i"]])
  ok("(f) control: the OLD length-check saw only 1 item here -- confirms the bug was real",
     old_n == 1L)

  # Control: the OLD code (no atomic/list branch matched at all) really did
  # crash iterating this shape directly, unlike ab_rows's narrower crash.
  old_crash <- tryCatch({ for (item in three_flat) { x <- item$i }; FALSE },
                        error = function(e) TRUE)
  ok("(f) control: iterating the UN-normalized flattened shape directly does crash -- confirms the live incident",
     old_crash)
}

# ── Defense in depth: an unrecognized shape degrades, doesn't crash ────────
{
  norm <- normalize_inv_sel(list(1, 2, "not a row"))
  ok("an unrecognized shape (list of non-list items) drops the malformed entries instead of crashing",
     is.list(norm) && length(norm) == 0L)
}

cat("\n=== results:", pass, "passed,", fail, "failed ===\n")
if (fail > 0) stop("Tests FAILED.")
invisible(NULL)
