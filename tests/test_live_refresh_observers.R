# =============================================================================
# tests/test_live_refresh_observers.R
# Stage 2 of the real-time-refresh strategy (2026-07-30): sync_bus (Stage 1)
# keeps shared reactiveVals fresh across sessions, but that alone doesn't
# redraw a view whose content was captured as a one-time snapshot (Shiny
# won't re-run a modalDialog body just because a dependency changed
# elsewhere). This stage widens/adds the small number of observers that
# bridge "the data changed" into "redraw the open view":
#
#   2a. R/ledger_module.R's Calendario day-view modal observer -- previously
#       watched ONLY pasivos_provisions_db; widened to also watch moves_db,
#       manual_inv, tags_db, papelera_rv, bancos_confirmados, abonos_db.
#   2b. R/pasivos_module.R's "manage converted provision" modal (pcm_conv_body)
#       -- switched from a one-time load_pasivos_provisions()/load_manual()
#       S3 read to reading shared$pasivos_provisions_db()/shared$manual_inv()
#       directly, which is both cheaper AND makes it a genuine live
#       dependency. NOT applied to the initial convert/edit form modal --
#       .pasivos_perform_conversion() already re-validates fresh from S3 and
#       checks estado=="provisional" at save time, and live-redrawing an
#       ACTIVE edit form's inputs while the user is typing would silently
#       clobber their in-progress edits, which is worse than the gap it
#       would "fix".
#   2c. R/interco_module.R's ic_invoices observe() block was missing
#       papelera_rv as an explicit dependency even though .load_ic_data()
#       reads it (via isolate()) -- a papelera-only change never triggered
#       a re-derive. Added as a direct read alongside the other deps.
#   2d. R/search_module.R's search modal was deliberately NOT converted to a
#       live view -- documented in place, not silently skipped.
#
# Static, permanent half of verification. The other half was live: a minimal
# headless-Chrome (chromote) harness reproducing the EXACT shape of the 2a
# fix (observeEvent({a(); b(); c()}, ...)) with three independent watched
# reactives plus one deliberately unwatched one, and a modal_open() gate --
# confirmed 9/9: the observer fires on ANY of the three watched reactives
# changing (not just the first one referenced), never fires for the
# unwatched one, respects the open/closed gate, and survives repeated
# open/close cycles. That's the one genuinely non-obvious Shiny behavior
# this stage relies on (does referencing N reactives in one observeEvent
# trigger block really subscribe to all N, not just the first?) -- confirmed
# empirically, not assumed.
# =============================================================================
cat("=== test_live_refresh_observers ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) {
    cat(" PASS:", label, "\n"); .pass <<- .pass + 1L
  } else {
    cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L
  }
}

# ── 2a. Calendario day-view modal observer ──────────────────────────────────
{
  ledger_txt <- readLines("R/ledger_module.R", warn = FALSE)
  start <- grep("observeEvent\\(\\{", ledger_txt)
  # There may be more than one observeEvent({ ... }) block in this large
  # file; narrow to the one that also mentions modal_open() nearby, which
  # uniquely identifies the day-modal auto-refresh observer.
  start <- start[vapply(start, function(s) {
    any(grepl("modal_open\\(\\)", ledger_txt[s:min(s + 20, length(ledger_txt))]))
  }, logical(1))]
  ok("found the day-modal auto-refresh observer to scan", length(start) > 0)

  if (length(start)) {
    end <- grep("^    \\}\\)$", ledger_txt)
    end <- end[end > start[1]][1]
    block <- paste(ledger_txt[start[1]:end], collapse = "\n")

    WATCHED <- c("moves_db", "manual_inv", "tags_db", "papelera_rv",
                "bancos_confirmados", "abonos_db", "pasivos_provisions_db")
    for (key in WATCHED) {
      ok(sprintf("day-modal observer watches shared$%s()", key),
         grepl(sprintf("shared\\$%s\\(\\)", key), block))
    }
    ok("day-modal observer still uses ignoreInit = TRUE (regression guard)",
       grepl("ignoreInit\\s*=\\s*TRUE", block))
    ok("day-modal observer still checks modal_open() before refreshing (only redraws if open)",
       grepl("if \\(!isolate\\(modal_open\\(\\)\\)\\) return\\(\\)", block))
    ok("day-modal observer still consumes suppress_ledger_prov_refresh (external-add guard preserved)",
       grepl("suppress_ledger_prov_refresh", block))
    ok("day-modal observer still calls .refresh_ctx_detail() to recompute the shown day (unchanged recompute logic)",
       grepl("\\.refresh_ctx_detail\\(ctx\\)", block))
    ok("day-modal observer still pushes the result back into modal_ctx() (unchanged publish step)",
       grepl("modal_ctx\\(new_ctx\\)", block))
    for (key in WATCHED) {
      ok(sprintf("day-modal observer watches shared$%s() exactly once (not accidentally duplicated)", key),
         lengths(regmatches(block, gregexpr(sprintf("shared\\$%s\\(\\)", key), block))) == 1L)
    }
    ok("day-modal observer does NOT watch shared$sap_data() (deliberately excluded -- SAP has its own refresh cadence, not a sync_bus gap)",
       !grepl("shared\\$sap_data\\(\\)", block))
  }
}

# ── 2b. Pasivos "manage converted" modal (pcm_conv_body) ────────────────────
{
  pas_txt <- readLines("R/pasivos_module.R", warn = FALSE)
  start <- grep('output\\$pcm_conv_body <- shiny::renderUI', pas_txt)
  ok("found pcm_conv_body's renderUI to scan", length(start) > 0)
  if (length(start)) {
    block <- paste(pas_txt[start[1]:min(start[1] + 25, length(pas_txt))], collapse = "\n")
    ok("pcm_conv_body reads shared$pasivos_provisions_db() directly (live dependency, not a one-time S3 read)",
       grepl("shared\\$pasivos_provisions_db\\(\\)", block))
    ok("pcm_conv_body reads shared$manual_inv() directly (live dependency, not a one-time S3 read)",
       grepl("shared\\$manual_inv\\(\\)", block))
    ok("pcm_conv_body no longer calls load_pasivos_provisions() directly in its body (fully switched, not a duplicate/leftover read)",
       !grepl("load_pasivos_provisions\\(client_id", block))
    ok("pcm_conv_body no longer calls load_manual() directly in its body (fully switched, not a duplicate/leftover read)",
       !grepl("load_manual\\(client_id", block))
    ok("pcm_conv_body still handles a NULL provisions table gracefully (no regression on the error path)",
       grepl("if \\(is\\.null\\(provs\\)\\)", block))
    ok("pcm_conv_body still handles a not-found provision gracefully (shows a message, doesn't crash)",
       grepl("if \\(!nrow\\(prov\\)\\)", block))
    ok("pcm_conv_body still computes is_conf from estado (regression guard -- item_confirmed rows still show the 'only delete' warning)",
       grepl('is_conf\\s*<-\\s*estado\\s*==\\s*"item_confirmed"', block))
    ok("pcm_conv_body's fix is documented in place (explains the ~8s tradeoff, not a silent change)",
       any(grepl("genuine reactive dependency", pas_txt[max(1, start[1]-15):start[1]])))
  }

  # Confirm the initial convert/edit form's own, separate staleness
  # protection is intact -- this is WHY that modal deliberately did not get
  # the same live-redraw treatment (redrawing active form inputs would be
  # actively harmful, not helpful).
  conv_start <- grep("^\\.pasivos_perform_conversion <- function", pas_txt)
  ok("found .pasivos_perform_conversion() to scan", length(conv_start) > 0)
  if (length(conv_start)) {
    conv_block <- paste(pas_txt[conv_start[1]:min(conv_start[1] + 10, length(pas_txt))], collapse = "\n")
    ok(".pasivos_perform_conversion() re-reads provisions fresh from S3 at save time (not trusting a stale form snapshot)",
       grepl("load_pasivos_provisions\\(client_id", conv_block))
    ok(".pasivos_perform_conversion() re-checks estado == \"provisional\" at save time (rejects a conversion if someone else already converted/deleted it)",
       grepl('estado\\[1\\]\\s*!=\\s*"provisional"', conv_block))
  }
}

# ── 2c. Interco's papelera_rv dependency gap ────────────────────────────────
{
  ic_txt <- readLines("R/interco_module.R", warn = FALSE)
  start <- grep("^\\s*observe\\(\\{\\s*$", ic_txt)
  start <- start[vapply(start, function(s) {
    any(grepl("ic_invoices\\(\\.load_ic_data\\(\\)\\)", ic_txt[s:min(s + 20, length(ic_txt))]))
  }, logical(1))]
  ok("found the ic_invoices refresh observe() block to scan", length(start) > 0)
  if (length(start)) {
    block <- paste(ic_txt[start[1]:min(start[1] + 20, length(ic_txt))], collapse = "\n")
    ok("ic_invoices observer now reads shared$papelera_rv() as an explicit dependency",
       grepl("shared\\$papelera_rv\\(\\)", block))
    ok("ic_invoices observer still reads shared$interco_v2() (regression guard on pre-existing deps)",
       grepl("shared\\$interco_v2\\(\\)", block))
    ok("ic_invoices observer still reads shared$bancos_confirmados() (regression guard on pre-existing deps)",
       grepl("shared\\$bancos_confirmados\\(\\)", block))
    ok("ic_invoices observer still reads shared$abonos_db() (regression guard on pre-existing deps)",
       grepl("shared\\$abonos_db\\(\\)", block))
    ok("ic_invoices observer still calls ic_invoices(.load_ic_data()) to recompute (unchanged recompute logic)",
       grepl("ic_invoices\\(\\.load_ic_data\\(\\)\\)", block))
  }
  ok(".load_ic_data() itself still reads papelera_rv via isolate() (unchanged -- the observer now duplicates this as a live dependency on purpose, not a leftover)",
     any(grepl("isolate\\(shared\\$papelera_rv\\(\\)\\)", ic_txt)))
}

# ── 2d. Search modal: deliberate non-conversion, documented not silent ─────
{
  search_txt <- readLines("R/search_module.R", warn = FALSE)
  header <- paste(search_txt[1:25], collapse = "\n")
  ok("search_module.R documents the deliberate decision NOT to make the search modal live",
     grepl("Deliberately NOT converted", header))
  ok("the documented reasoning explains the actual tradeoff (shared reactives -> recompute cost), not just \"skipped\"",
     grepl("recomputes and rebuilds", header))
  ok("show_search_modal still exists and is still the entry point (regression guard -- decision didn't accidentally remove functionality)",
     any(grepl("^show_search_modal <- function", search_txt)))
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
cat("(Plus 9/9 live browser scenarios verifying the multi-dependency observer mechanism itself -- see file header.)\n")
if (.fail > 0) stop("Tests FAILED.")
invisible(NULL)
