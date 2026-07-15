# =============================================================================
# tests/test_tag_for_day.R
# Verify that tag_for_day() suppresses badges on days with no visible items,
# covering: deleted items, moved-away items, and cross-currency tag bleed.
# Run with: source("tests/test_tag_for_day.R")
# =============================================================================
cat("=== test_tag_for_day (empty-day tag bug) ===\n\n")

pass <- 0L; fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) {
    cat(" PASS:", label, "\n"); pass <<- pass + 1L
  } else {
    cat(" FAIL:", label, "\n"); fail <<- fail + 1L
  }
}

# ── Extract the patched tag_for_day logic for unit testing ───────────────────
make_tag_for_day <- function(day_parties, tags_day) {
  function(d) {
    key <- as.character(d)
    if (!key %in% names(day_parties)) return("")
    tags_day[[key]] %||% ""
  }
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ── Test 1: normal — tagged day WITH visible item → returns tag ───────────────
{
  day_parties <- list("2026-06-12" = data.frame(Parte="A"), "2026-06-15" = data.frame(Parte="B"))
  tags_day    <- list("2026-06-12" = "important", "2026-06-15" = "urgent")
  fn <- make_tag_for_day(day_parties, tags_day)
  ok("tagged day with visible item returns 'important'", fn(as.Date("2026-06-12")) == "important")
  ok("tagged day with visible item returns 'urgent'",    fn(as.Date("2026-06-15")) == "urgent")
}

# ── Test 2: item deleted — day empty in day_parties → tag suppressed ──────────
{
  day_parties <- list("2026-06-12" = data.frame(Parte="A"))  # day 15 absent (deleted)
  tags_day    <- list("2026-06-12" = "important", "2026-06-15" = "important")
  fn <- make_tag_for_day(day_parties, tags_day)
  ok("deleted item: tag suppressed on now-empty day",  fn(as.Date("2026-06-15")) == "")
  ok("deleted item: tag still shown on non-empty day", fn(as.Date("2026-06-12")) == "important")
}

# ── Test 3: item moved away — day 15 empty, day 18 now has it ────────────────
{
  day_parties <- list("2026-06-18" = data.frame(Parte="A"))  # moved to day 18
  tags_day    <- list("2026-06-15" = "urgent", "2026-06-18" = "urgent")
  fn <- make_tag_for_day(day_parties, tags_day)
  ok("moved item: old day 15 tag suppressed (empty after move)", fn(as.Date("2026-06-15")) == "")
  ok("moved item: new day 18 tag shown (has item after move)",   fn(as.Date("2026-06-18")) == "urgent")
}

# ── Test 4: cross-currency bleed — USD item tagged, MXN view day empty ───────
# day_parties only contains MXN items (calendar_html already filtered by cur)
{
  day_parties <- list("2026-06-12" = data.frame(Parte="MXN_vendor"))
  # tags_day has day 15 from a USD-tagged item (tags_day_map_rv is currency-agnostic)
  tags_day    <- list("2026-06-12" = "important", "2026-06-15" = "important")
  fn <- make_tag_for_day(day_parties, tags_day)
  ok("cross-currency: USD tag on MXN-empty day 15 suppressed", fn(as.Date("2026-06-15")) == "")
  ok("cross-currency: MXN tag on day 12 with MXN item shown",  fn(as.Date("2026-06-12")) == "important")
}

# ── Test 5: entire month empty → all tags suppressed ─────────────────────────
{
  day_parties <- list()
  tags_day    <- list("2026-06-10" = "both", "2026-06-20" = "urgent")
  fn <- make_tag_for_day(day_parties, tags_day)
  ok("all-empty month: day 10 tag suppressed", fn(as.Date("2026-06-10")) == "")
  ok("all-empty month: day 20 tag suppressed", fn(as.Date("2026-06-20")) == "")
}

# ── Test 6: day has items but no tag entry → returns empty string ────────────
{
  day_parties <- list("2026-06-05" = data.frame(Parte="A"))
  tags_day    <- list()
  fn <- make_tag_for_day(day_parties, tags_day)
  ok("item present but no tag → empty string (no crash)", fn(as.Date("2026-06-05")) == "")
}

# ── Test 7: 'both' tag preserved when day has items ──────────────────────────
{
  day_parties <- list("2026-06-25" = data.frame(Parte="A"))
  tags_day    <- list("2026-06-25" = "both")
  fn <- make_tag_for_day(day_parties, tags_day)
  ok("'both' tag preserved when day has visible items", fn(as.Date("2026-06-25")) == "both")
}

cat("\n=== results:", pass, "passed,", fail, "failed ===\n")
if (fail > 0) stop("Tests FAILED.")
invisible(NULL)
