# =============================================================================
# tests/test_stage7_global_always_resourced.R
#
# CRITICAL fix (2026-08-27): app.R used to guard its global.R bootstrap with
# `if (!exists("S3_KEYS", envir = .GlobalEnv)) source("R/global.R")`. In any
# persistent R session (an RStudio console kept open across multiple presses
# of "Run App"), S3_KEYS survives from the first run, so every later launch
# in that session silently skipped reloading global.R -- and, critically,
# R/persistence.R too, since persistence.R is ONLY ever source()'d from
# inside global.R, never directly by app.R. Every fix landing in
# persistence.R or global.R itself (the .app_data_cache cross-tenant
# isolation fix, the .load_proveedores_raw fix) sat correctly on disk but
# never actually loaded into that session, while fixes in files app.R
# sources unconditionally (pagar_hoy_module.R, audit_log_viewer_module.R)
# took effect immediately -- the exact "some fixes worked, others silently
# didn't, no error anywhere" pattern the user hit twice in a row. Also meant
# .app_data_cache itself never got a fresh start on a mere app relaunch.
#
# This file guards against the guard ever coming back, in this exact shape
# or an equivalent one, on the global.R bootstrap or on any other file only
# reachable through it.
#
# Sourced by _run_saas.R.
# =============================================================================

cat("── global.R always re-sourced on every app.R launch (CRITICAL) ────────────\n")

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
.chk_true  <- function(cond, label) .chk(isTRUE(cond), TRUE, label)
.chk_false <- function(cond, label) .chk(isTRUE(cond), FALSE, label)

app_txt_raw <- readLines("app.R", warn = FALSE)
# Comment-stripped view for the structural checks below -- this test file's
# own header comment quotes the OLD guarded pattern verbatim as part of
# explaining the fix (same "code" substring appearing in prose that tripped
# up the log_action()-in-comments false positive earlier this session), so
# checks here must look at live code, not comment text.
app_txt    <- app_txt_raw[!grepl("^\\s*#", app_txt_raw)]
global_txt <- paste(readLines("R/global.R", warn = FALSE), collapse = "\n")

# ── A. The bootstrap line itself ─────────────────────────────────────────────
boot_lines <- grep('source\\("R/global\\.R"\\)', app_txt, value = TRUE)
.chk(length(boot_lines), 1L, "app.R sources R/global.R exactly once")
.chk_true(length(boot_lines) == 1L && !grepl("^\\s*if\\s*\\(", boot_lines),
     "the source(\"R/global.R\") line is NOT itself the start of an if(...) guard")

# The dangerous shape specifically: any exists()-guarded conditional wrapping
# a source() call anywhere in app.R (not just the historical S3_KEYS one --
# guards against an equivalent guard reappearing on some OTHER file that is
# itself only reachable through an unconditionally-sourced one).
guarded_source_lines <- grep('if\\s*\\(\\s*!?\\s*exists\\([^)]*\\)[^{]*\\)\\s*source\\(', app_txt)
.chk(length(guarded_source_lines), 0L,
     "no exists()-guarded source() call remains anywhere in app.R")

# ── B. Every source() line in app.R's bootstrap section is unconditional ────
# (first ~40 lines is where all the module sources live, per app.R's own
# structure -- a generous window, not exact-line-coupled).
head_block <- app_txt[seq_len(min(60, length(app_txt)))]
source_lines <- grep('^\\s*source\\(', head_block)
guarded_head <- Filter(function(i) {
  ctx <- paste(head_block[max(1, i - 2):i], collapse = " ")
  grepl("if\\s*\\(", ctx) && !grepl("^\\s*#", head_block[i])
}, source_lines)
.chk(length(guarded_head), 0L,
     "no source() call in app.R's bootstrap block sits behind a conditional guard")

# ── C. persistence.R's dependency chain -- document + guard the mechanism ──
# persistence.R must remain reachable ONLY through global.R for this fix's
# reasoning to hold (if some future change also sources persistence.R
# directly and unconditionally from app.R, that would independently protect
# it even if global.R's own guard ever came back -- fine either way, but the
# CURRENT fix's correctness depends on global.R itself never being skipped).
.chk_true(grepl('source\\("R/persistence\\.R"\\)', global_txt),
     "R/global.R still sources R/persistence.R (the dependency this fix protects)")
app_sources_persistence_directly <- any(grepl('source\\("R/persistence\\.R"\\)', app_txt))
.chk_false(app_sources_persistence_directly,
     "sanity: app.R does NOT source persistence.R directly (confirms it's still only reachable via global.R -- the exact chain this fix's reasoning depends on)")

# ── D. global.R contains no non-idempotent side effects that would make ────
#      unconditional re-sourcing unsafe (scheduled callbacks, namespace
#      patches, registrations that error or duplicate on a second call).
DANGEROUS_PATTERNS <- c("later::later\\(", "assignInNamespace\\(", "setup_sync_bus\\(",
                        "onStop\\(", "onSessionEnded\\(", "registerHook\\(")
for (pat in DANGEROUS_PATTERNS) {
  .chk_false(grepl(pat, global_txt),
       sprintf("R/global.R contains no '%s' (would make unconditional re-sourcing unsafe)",
              gsub("\\\\", "", pat)))
}

cat(sprintf("\n  Stage 7 subtotal: %d passed, %d failed\n", .pass, .fail))
