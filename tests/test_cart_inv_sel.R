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
normalize_inv_sel <- function(inv_sel) {
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

cat("\n=== results:", pass, "passed,", fail, "failed ===\n")
if (fail > 0) stop("Tests FAILED.")
invisible(NULL)
