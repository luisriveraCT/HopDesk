# =============================================================================
# tests/test_phase2_stale_preload_cache.R
#
# Incident (2026-08-20): a treasurer's ~268 AP invoice-date moves were written
# correctly to S3 (`invoice_moves.rds`, sync-registered as "moves_db"), but
# stayed invisible to every session connecting to a long-lived shinyapps.io
# worker process for over a day -- including a hard browser refresh -- until
# the process itself was restarted. Root cause: app.R's Phase 2 aux-data
# loader treated ANY S3 key already sitting in the process-wide
# `.s3_preload_cache` as safe to reuse at a brand-new session's first load,
# with only two hand-picked exceptions ("notes", "sync_versions") carved out
# after a near-identical past incident. "moves" was never added to that list,
# so any worker that had `invoice_moves.rds` cached before the move served
# stale data to every subsequent new session, forever, until restart -- the
# 8-second sync_bus poll only protects a session ALREADY connected when a
# change happens; it does nothing for a session's own first load.
#
# The fix generalizes the "notes" exception into a principled rule: any S3
# key backed by a register_synced() entry can be mutated at any time by
# another session/process, so it must never be trusted from a stale process
# cache at session start.
#
# This file does NOT reimplement the fix's logic by hand -- every functional
# test below extracts the REAL current text of app.R's Phase 2 block and
# eval()s it verbatim in a controlled sandbox. A hand-copied reimplementation
# would keep passing even if app.R's actual code drifted or regressed; only
# executing the real source text catches that.
# =============================================================================

cat("── Phase 2 stale-preload-cache fix (sync-registered keys) ─────────────\n")

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
.chk_true <- function(cond, label) .chk(isTRUE(cond), TRUE, label)
`%+%` <- function(a, b) paste0(a, b)

app_r_path <- "app.R"
.chk(file.exists(app_r_path), TRUE, "app.R exists at expected path")
app_txt <- readLines(app_r_path, warn = FALSE)

# =============================================================================
# Part A -- static scan of app.R's real source text
# =============================================================================
cat("\n-- Part A: static scan of app.R --\n")

start_ln <- grep("\\.synced_s3_keys <- unique\\(vapply\\(\\.SYNC_REGISTRY", app_txt)
local_start_ln <- grep("^\\s*local\\(\\{\\s*$", app_txt)
end_ln <- grep("# Load aux objects with per-item timing", app_txt)
clear_loop_ln <- grep("for \\(\\.rk in intersect\\(unname\\(S3_KEYS\\), \\.synced_s3_keys\\)\\)", app_txt)
skip_preload_ln <- grep("SKIP_PRELOAD <- union\\(c\\(\"sync_versions\", \"notes\"\\)", app_txt)
moves_registration_ln <- grep('register_synced\\("moves_db",\\s*S3_KEYS\\$moves,\\s*load_moves\\)', app_txt)

.chk(length(start_ln) >= 1, TRUE, "found the .synced_s3_keys computation in app.R")
.chk(length(end_ln) >= 1, TRUE, "found the 'Load aux objects' anchor comment after the fix")
.chk(length(clear_loop_ln) >= 1, TRUE, "found the explicit post-block cache-clear loop")
.chk(length(skip_preload_ln) >= 1, TRUE, "found the generalized SKIP_PRELOAD assignment")
.chk(length(moves_registration_ln) == 1, TRUE,
     "register_synced(\"moves_db\", S3_KEYS$moves, load_moves) still exists exactly once -- "
     %+% "the fix depends on this registration; if it's ever removed, the incident returns via "
     %+% "a different mechanism this test can't see")

.chk(length(grep("\\.synced_s3_keys <-", app_txt)) == 1L, TRUE,
     "exactly one .synced_s3_keys <- assignment exists (no leftover duplicate from editing)")
.chk(length(grep("SKIP_PRELOAD <-", app_txt)) == 1L, TRUE,
     "exactly one SKIP_PRELOAD <- assignment exists in app.R")
.chk(length(clear_loop_ln) == 1L, TRUE,
     "the cache-clear loop appears exactly once (not duplicated)")

# Scoping regression guard: .synced_s3_keys MUST be defined before the
# local({}) block starts, and the clear-loop MUST run after that block's own
# local({}) closes -- this exact bug (defining it INSIDE local({}) so it's
# invisible afterward) was caught and fixed during this stage's own
# development, before ever being committed.
if (length(start_ln) && length(local_start_ln) && length(clear_loop_ln)) {
  first_local_after_def <- local_start_ln[local_start_ln > start_ln[1]][1]
  .chk_true(!is.na(first_local_after_def) && start_ln[1] < first_local_after_def,
            ".synced_s3_keys is computed BEFORE the batch-prefetch local({}) block starts (scoping regression guard)")
  .chk_true(clear_loop_ln[1] > first_local_after_def,
            "the cache-clear loop runs AFTER the batch-prefetch local({}) block (ordering guard)")
}

if (length(clear_loop_ln)) {
  block <- paste(app_txt[clear_loop_ln[1]:min(clear_loop_ln[1] + 4, length(app_txt))], collapse = "\n")
  .chk_true(grepl("rm\\(list = intersect\\(\\.rk_full, ls\\(\\.s3_preload_cache\\)\\), envir = \\.s3_preload_cache\\)", block),
            "the clear-loop's rm() call exactly mirrors sync_bus.R's own version-triggered reload cache-clear")
}

.chk_true({p <- tryCatch({parse(app_r_path); TRUE}, error = function(e) FALSE); p},
          "app.R still parses without syntax errors after the edit")

# A second, independent copy of the same buggy "exists-in-cache means safe"
# pattern elsewhere in app.R would mean this fix didn't actually close the
# bug class -- confirm the pattern that matters (checking .s3_preload_cache
# existence to decide keys_needed) appears exactly at this one Phase 2 site.
exists_pattern_hits <- grep("vapply\\(paste0\\(cid_lo,.*unname\\(S3_KEYS\\)\\), exists,", app_txt)
.chk(length(exists_pattern_hits), 1L,
     "the 'cache-hit means safe to skip' pattern exists at exactly one site in app.R (this Phase 2 block) -- no second, unfixed copy elsewhere")

register_synced_hits <- grep("register_synced\\(", app_txt)
.chk_true(length(register_synced_hits) >= 25,
          sprintf("found a plausible number of register_synced() call sites (%d) -- sanity check that the registry scan has real data to work with",
                  length(register_synced_hits)))
for (ln in register_synced_hits) {
  # register_synced() calls are sometimes single-line, sometimes wrap the
  # name/s3_key/loader across several lines (e.g. "pagar_hoy_db"'s dispatch
  # loader) -- look at a small window after the call, not just the one line.
  window <- paste(app_txt[ln:min(ln + 3, length(app_txt))], collapse = " ")
  .chk_true(grepl('register_synced\\(\\s*"[^"]+"', window),
            sprintf("register_synced() call at app.R:%d has a real, non-empty first argument (registered name)", ln))
}

# =============================================================================
# Part B -- functional tests: eval the REAL extracted app.R block, not a
# hand-copied reimplementation
# =============================================================================
cat("\n-- Part B: executing the real extracted Phase 2 block --\n")

extract_block <- function(start_pattern, end_pattern, txt = app_txt) {
  s <- grep(start_pattern, txt)[1]
  e <- grep(end_pattern, txt)[1]
  stopifnot(!is.na(s), !is.na(e), e > s)
  paste(txt[s:(e - 1)], collapse = "\n")
}

real_block_text <- extract_block(
  "\\.synced_s3_keys <- unique\\(vapply\\(\\.SYNC_REGISTRY",
  "# Load aux objects with per-item timing"
)
.chk_true(nchar(real_block_text) > 200, "extracted the real Phase 2 block text from app.R (non-trivial length)")
.chk_true(grepl("\\.synced_s3_keys", real_block_text) && grepl("for \\(\\.rk in intersect", real_block_text),
          "extracted block contains both the .synced_s3_keys computation and the clear-loop")

# Hand-written OLD (pre-fix) equivalent, for the "would the bug have been
# caught" control below. This is the ONLY hand-written block in this file --
# used purely as a control, never as the thing being verified as correct.
old_block_text <- '
SKIP_PRELOAD <- c("sync_versions", "notes")
keys_needed <- names(S3_KEYS)[
  !vapply(paste0(cid_lo, "/", unname(S3_KEYS)), exists,
          logical(1), envir = .s3_preload_cache, inherits = FALSE) &
  !names(S3_KEYS) %in% SKIP_PRELOAD
]
'

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x

# Build a fresh sandbox with a realistic registry + key set + pre-populated
# (stale) process cache, then eval the REAL block inside it.
make_sandbox <- function(registry, s3_keys, precache_all = TRUE, cid_lo = "networks") {
  env <- new.env(parent = globalenv())
  env$.SYNC_REGISTRY <- registry
  env$S3_KEYS <- s3_keys
  env$cid_lo <- cid_lo
  env$`%||%` <- `%||%`
  env$.s3_preload_cache <- new.env(parent = emptyenv())
  if (precache_all) {
    for (suffix in unname(s3_keys)) {
      full_key <- paste0(cid_lo, "/", suffix)
      assign(full_key, list(stale_marker = TRUE, key = suffix), envir = env$.s3_preload_cache)
    }
  }
  env
}

cached_keys <- function(env) ls(env$.s3_preload_cache)
is_cached <- function(env, cid_lo, suffix) exists(paste0(cid_lo, "/", suffix), envir = env$.s3_preload_cache, inherits = FALSE)

# ── Scenario 1: the actual incident, reproduced and fixed ──────────────────
{
  registry <- list(
    moves_db = list(s3_key = "invoice_moves.rds"),
    manual_inv = list(s3_key = "manual_invoices.rds"),
    tags_db = list(s3_key = "invoice_tags.rds")
  )
  s3_keys <- list(
    moves = "invoice_moves.rds",
    manual = "manual_invoices.rds",
    tags = "invoice_tags.rds",
    notes = "calendar_notes.rds",
    sync_versions = "sync_versions.rds",
    erp_connections = "erp_connections.rds",
    group_config = "group_config.rds"
  )
  sbox <- make_sandbox(registry, s3_keys)

  eval(parse(text = real_block_text), envir = sbox)

  .chk(sort(sbox$.synced_s3_keys), sort(c("invoice_moves.rds", "manual_invoices.rds", "invoice_tags.rds")),
       "Scenario 1: .synced_s3_keys correctly derived from the registry (3 entries)")

  .chk(is_cached(sbox, "networks", "invoice_moves.rds"), FALSE,
       "Scenario 1: invoice_moves.rds cache entry CLEARED after the fix (the exact incident's key)")
  .chk(is_cached(sbox, "networks", "manual_invoices.rds"), FALSE,
       "Scenario 1: manual_invoices.rds cache entry CLEARED (another registered key)")
  .chk(is_cached(sbox, "networks", "invoice_tags.rds"), FALSE,
       "Scenario 1: invoice_tags.rds cache entry CLEARED (another registered key)")

  .chk(is_cached(sbox, "networks", "erp_connections.rds"), TRUE,
       "Scenario 1: erp_connections.rds (NOT registered) cache entry PRESERVED -- fix doesn't overreach")
  .chk(is_cached(sbox, "networks", "group_config.rds"), TRUE,
       "Scenario 1: group_config.rds (NOT registered) cache entry PRESERVED -- fix doesn't overreach")
  .chk(is_cached(sbox, "networks", "calendar_notes.rds"), TRUE,
       "Scenario 1: calendar_notes.rds untouched by the NEW clear-loop specifically (already handled by the separate hardcoded exclusion, unrelated to this fix's scope)")
  .chk(is_cached(sbox, "networks", "sync_versions.rds"), TRUE,
       "Scenario 1: sync_versions.rds untouched by the NEW clear-loop specifically")
}

# ── Scenario 2: empty registry -- mechanism must not hardcode "moves" ──────
{
  s3_keys <- list(moves = "invoice_moves.rds", notes = "calendar_notes.rds",
                   sync_versions = "sync_versions.rds")
  sbox <- make_sandbox(list(), s3_keys)
  eval(parse(text = real_block_text), envir = sbox)

  .chk(sbox$.synced_s3_keys, character(0),
       "Scenario 2: .synced_s3_keys is empty when the registry is empty")
  .chk(is_cached(sbox, "networks", "invoice_moves.rds"), TRUE,
       "Scenario 2: with an EMPTY registry, invoice_moves.rds is NOT forcibly cleared -- "
       %+% "proves the fix is driven by the actual registry, not a hardcoded 'moves' string")
}

# ── Scenario 3: malformed registry entry (NA s3_key) doesn't crash ─────────
{
  registry <- list(
    moves_db = list(s3_key = "invoice_moves.rds"),
    broken_entry = list(s3_key = NA_character_),
    also_broken = list()  # no s3_key field at all -> NULL -> %||% NA
  )
  s3_keys <- list(moves = "invoice_moves.rds", notes = "calendar_notes.rds",
                   sync_versions = "sync_versions.rds")
  sbox <- make_sandbox(registry, s3_keys)

  ran_ok <- tryCatch({ eval(parse(text = real_block_text), envir = sbox); TRUE },
                      error = function(e) { cat("    (error was:", conditionMessage(e), ")\n"); FALSE })
  .chk(ran_ok, TRUE, "Scenario 3: a registry entry with NA/missing s3_key does not crash the block")
  if (ran_ok) {
    # The real code does not explicitly filter NA out of .synced_s3_keys --
    # an NA entry there is harmless as long as it never causes a real S3_KEYS
    # suffix to be wrongly matched (R's %in%/match() only matches NA against
    # an explicit NA in the table, never against a real string), which is
    # the actual property that matters here, not the vector's literal shape.
    .chk(is_cached(sbox, "networks", "invoice_moves.rds"), FALSE,
         "Scenario 3: the one well-formed registry entry (moves_db) is still correctly cleared despite the malformed siblings")
    .chk(is_cached(sbox, "networks", "calendar_notes.rds"), TRUE,
         "Scenario 3: a real S3_KEYS suffix ('calendar_notes.rds') is never wrongly matched by the NA/missing s3_key entries")
    .chk(is_cached(sbox, "networks", "sync_versions.rds"), TRUE,
         "Scenario 3: another real S3_KEYS suffix ('sync_versions.rds') is also never wrongly matched by the NA entries")
  }
}

# ── Scenario 4: duplicate s3_key across two registry names ────────────────
{
  registry <- list(
    moves_db = list(s3_key = "invoice_moves.rds"),
    moves_db_alias = list(s3_key = "invoice_moves.rds")  # hypothetical duplicate
  )
  s3_keys <- list(moves = "invoice_moves.rds", notes = "calendar_notes.rds",
                   sync_versions = "sync_versions.rds")
  sbox <- make_sandbox(registry, s3_keys)
  ran_ok <- tryCatch({ eval(parse(text = real_block_text), envir = sbox); TRUE },
                      error = function(e) FALSE)
  .chk(ran_ok, TRUE, "Scenario 4: duplicate s3_key values across two registry names does not error")
  .chk(length(sbox$.synced_s3_keys), 1L,
       "Scenario 4: duplicate s3_key values are collapsed via unique(), not double-counted")
  .chk(is_cached(sbox, "networks", "invoice_moves.rds"), FALSE,
       "Scenario 4: the duplicated key is still correctly cleared exactly once")
}

# ── Scenario 5: OLD (pre-fix) code reproduces the bug -- proves the test
#    would have caught this before it shipped ───────────────────────────────
{
  registry <- list(moves_db = list(s3_key = "invoice_moves.rds"))
  s3_keys <- list(moves = "invoice_moves.rds", notes = "calendar_notes.rds",
                   sync_versions = "sync_versions.rds")
  sbox <- make_sandbox(registry, s3_keys)
  eval(parse(text = old_block_text), envir = sbox)

  .chk(is_cached(sbox, "networks", "invoice_moves.rds"), TRUE,
       "Scenario 5 (control): the OLD pre-fix logic leaves invoice_moves.rds stale-cached -- "
       %+% "confirms this test suite would have failed against the code that actually shipped the incident")
}

# ── Scenario 6: full realistic registry (mirrors app.R's real registrations
#    as of this stage) -- broader coverage than the 3-key toy example above ─
{
  real_registry <- list(
    bancos_confirmados_db = list(s3_key = "bancos_confirmados.rds"),
    bancos_movimientos_db = list(s3_key = "bancos_movimientos.rds"),
    ctas_cuentas           = list(s3_key = "ctas_cuentas.rds"),
    proveedores_db         = list(s3_key = "proveedores.rds"),
    empresas_db            = list(s3_key = "empresas.rds"),
    grupos_db              = list(s3_key = "grupos.rds"),
    pasivos_provisions_db  = list(s3_key = "pasivos_provisions.rds"),
    pasivos_liabilities_db = list(s3_key = "pasivos_liabilities.rds"),
    papelera_rv            = list(s3_key = "papelera.rds"),
    manual_inv             = list(s3_key = "manual_invoices.rds"),
    tags_db                = list(s3_key = "invoice_tags.rds"),
    moves_db               = list(s3_key = "invoice_moves.rds"),
    policy_moves_db        = list(s3_key = "policy_moves.rds"),
    abonos_db              = list(s3_key = "abonos.rds"),
    interco_v2             = list(s3_key = "interco_v2.rds"),
    sap_ov_db              = list(s3_key = "sap_overrides.rds")
  )
  # S3_KEYS deliberately includes a few NOT present in the registry above,
  # to keep proving the negative case across a wider, more realistic set.
  real_s3_keys <- c(
    lapply(real_registry, function(e) e$s3_key),
    list(notes = "calendar_notes.rds", sync_versions = "sync_versions.rds",
         erp_connections = "erp_connections.rds", group_config = "group_config.rds",
         tiers_config = "tiers_config.rds", agenda_sync_config = "agenda_sync_config.rds")
  )
  names(real_s3_keys)[seq_along(real_registry)] <- names(real_registry)
  sbox <- make_sandbox(real_registry, real_s3_keys)
  eval(parse(text = real_block_text), envir = sbox)

  registered_suffixes <- unname(unlist(lapply(real_registry, function(e) e$s3_key)))
  for (suffix in registered_suffixes) {
    .chk(is_cached(sbox, "networks", suffix), FALSE,
         sprintf("Scenario 6: registered key '%s' is cleared from the preload cache", suffix))
  }
  never_registered <- c("erp_connections.rds", "group_config.rds", "tiers_config.rds", "agenda_sync_config.rds")
  for (suffix in never_registered) {
    .chk(is_cached(sbox, "networks", suffix), TRUE,
         sprintf("Scenario 6: non-registered key '%s' is preserved in the preload cache", suffix))
  }
}

# ── Scenario 7: prefix construction matches the app's own convention ───────
{
  registry <- list(moves_db = list(s3_key = "invoice_moves.rds"))
  s3_keys <- list(moves = "invoice_moves.rds", notes = "calendar_notes.rds",
                   sync_versions = "sync_versions.rds")
  sbox <- make_sandbox(registry, s3_keys, cid_lo = "networks")
  eval(parse(text = real_block_text), envir = sbox)
  .chk(is_cached(sbox, "networks", "invoice_moves.rds"), FALSE,
       "Scenario 7: full-prefix key construction (\"networks/invoice_moves.rds\") matches .s3_key()'s own convention")

  # A different client prefix in the same run must not bleed across clients.
  sbox2 <- make_sandbox(registry, s3_keys, cid_lo = "hopdesk")
  assign("networks/invoice_moves.rds", list(stale = TRUE), envir = sbox2$.s3_preload_cache)
  eval(parse(text = real_block_text), envir = sbox2)
  .chk(is_cached(sbox2, "hopdesk", "invoice_moves.rds"), FALSE,
       "Scenario 7: this session's own client prefix (hopdesk) is correctly cleared")
  .chk(exists("networks/invoice_moves.rds", envir = sbox2$.s3_preload_cache, inherits = FALSE), TRUE,
       "Scenario 7: a DIFFERENT client's cache entry (networks) is left untouched -- no cross-client bleed")
}

# ── Scenario 8: re-running the block twice in the same process (two
#    sessions connecting back-to-back) stays correct and doesn't error ─────
{
  registry <- list(moves_db = list(s3_key = "invoice_moves.rds"))
  s3_keys <- list(moves = "invoice_moves.rds", notes = "calendar_notes.rds",
                   sync_versions = "sync_versions.rds")
  sbox <- make_sandbox(registry, s3_keys)
  eval(parse(text = real_block_text), envir = sbox)
  # Simulate a write landing in between: repopulate the cache (as .s3_write()'s
  # own restore-cache step would for the writing process), then a second
  # session's Phase 2 must clear it again.
  assign("networks/invoice_moves.rds", list(stale_marker = TRUE), envir = sbox$.s3_preload_cache)
  ran_ok <- tryCatch({ eval(parse(text = real_block_text), envir = sbox); TRUE },
                      error = function(e) FALSE)
  .chk(ran_ok, TRUE, "Scenario 8: running the block a second time in the same process does not error")
  .chk(is_cached(sbox, "networks", "invoice_moves.rds"), FALSE,
       "Scenario 8: a second run still correctly clears a re-populated stale entry")
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
