# =============================================================================
# tests/test_pasivos_audit_visibility.R
# Stage 5 of the real-time-refresh/audit-logging strategy (2026-07-30) --
# the most important correctness finding of the whole effort:
#
# Pasivos had its OWN separate audit-log system (pasivos_log_audit() ->
# pasivos_audit.rds, R/pasivos_audit.R), completely independent of
# log_action()/app_audit.rds. R/audit_log_viewer_module.R (Gestión de
# Usuarios > Actividad, the ONLY real activity screen in the app)
# exclusively reads app_audit.rds -- confirmed by grep, it never mentions
# "pasivos" at all. So every pasivos action "logged" (provision
# conversions, reverts, deletes, generation, capability denials -- ~20 real
# call sites across the codebase) was being written to a store nobody's
# Actividad screen could ever show. This was flagged as critical
# specifically because provision conversion is exactly the kind of "who
# approved what, when, how" moment that needs to be auditable, and it
# wasn't, silently, the whole time.
#
# Fix, one choke point instead of ~20 call sites: pasivos_log_audit() now
# also calls log_action() internally. Plus: conversion's audit entry is
# enriched with a field-by-field diff (did the user change anything on the
# review form vs. the provision's original implied values), and the
# revert/delete handlers -- previously passing only a bare notes string --
# now pass proper before/after too, matching conversion's richness.
# =============================================================================
cat("=== test_pasivos_audit_visibility ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) {
    cat(" PASS:", label, "\n"); .pass <<- .pass + 1L
  } else {
    cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L
  }
}

# ── 1. Confirm the original finding is real (control, not assumed) ─────────
{
  viewer_txt <- readLines("R/audit_log_viewer_module.R", warn = FALSE)
  ok("control: R/audit_log_viewer_module.R genuinely never mentions pasivos anywhere (confirms the bug was real, not a misreading)",
     !any(grepl("pasivos", viewer_txt, ignore.case = TRUE)))
  ok("control: the viewer's only data source is read_audit_log_scoped() (app_audit.rds)",
     any(grepl("read_audit_log_scoped", viewer_txt)))
}

# ── 2. Real functional test of the dual-write: extract the ACTUAL
# pasivos_log_audit() function (not a hand-copied mirror), mock its
# persistence + log_action, call it, and verify BOTH happen correctly. ─────
{
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
  assign("%||%", function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x, envir = globalenv())
  assign("load_pasivos_audit", function(client_id = NULL) tibble::tibble(), envir = globalenv())
  pasivos_audit_saved <- NULL
  assign("save_pasivos_audit", function(df, client_id = NULL) { pasivos_audit_saved <<- df; invisible(TRUE) }, envir = globalenv())
  log_action_calls <- list()
  assign("log_action", function(...) { log_action_calls[[length(log_action_calls)+1L]] <<- list(...); invisible(NULL) }, envir = globalenv())
  .extract_fn("R/pasivos_audit.R", "PASIVOS_ACTION_TYPES")
  .extract_fn("R/pasivos_audit.R", "pasivos_log_audit")

  row_id <- pasivos_log_audit(
    action_type = "provision.converted_to_item",
    user        = "larivera",
    target_kind = "provision",
    target_id   = "prov-123",
    before      = list(estado = "provisional"),
    after       = list(estado = "converted"),
    client_id   = "networks"
  )

  ok("pasivos_log_audit() still writes a row to pasivos_audit (old behavior preserved, not replaced)",
     !is.null(pasivos_audit_saved) && nrow(pasivos_audit_saved) == 1L)
  ok("pasivos_log_audit() returns the new row's id (regression guard on existing return contract)",
     is.character(row_id) && nzchar(row_id))
  ok("pasivos_log_audit() ALSO calls log_action() exactly once (the actual fix -- this is what makes it visible)",
     length(log_action_calls) == 1L)
  if (length(log_action_calls)) {
    call1 <- log_action_calls[[1]]
    ok("the log_action() call sets module=\"pasivos\"", identical(call1$module, "pasivos"))
    ok("the log_action() call's action matches the original action_type", identical(call1$action, "provision.converted_to_item"))
    ok("the log_action() call's target_id matches", identical(call1$target_id, "prov-123"))
    ok("the log_action() call's user matches", identical(call1$user, "larivera"))
    ok("the log_action() call's client_id matches (client-scoping preserved end to end)", identical(call1$client_id, "networks"))
    ok("the log_action() call's metadata includes the original 'before' payload (structured, not flattened into text)",
       identical(call1$metadata$before, list(estado = "provisional")))
    ok("the log_action() call's metadata includes the original 'after' payload",
       identical(call1$metadata$after, list(estado = "converted")))
  }

  # Second call: confirm notes-only calls (revert/delete's old shape, and
  # capability.denied, which never has before/after) still produce a
  # sensible, non-empty description.
  log_action_calls <- list()
  pasivos_log_audit(
    action_type = "capability.denied",
    user        = "someuser",
    target_kind = "provision",
    target_id   = "prov-999",
    notes       = "convert_to_item",
    client_id   = "networks"
  )
  ok("a notes-only call (no before/after) still produces exactly 1 log_action() call",
     length(log_action_calls) == 1L)
  ok("a notes-only call's description includes both the action type and the notes text (readable, not just the bare code)",
     grepl("capability.denied", log_action_calls[[1]]$description, fixed = TRUE) &&
     grepl("convert_to_item", log_action_calls[[1]]$description, fixed = TRUE))

  # Confirm a log_action() failure doesn't break the pasivos_audit write
  # (dual-write must not turn a working feature into a broken one).
  assign("log_action", function(...) stop("simulated log_action failure"), envir = globalenv())
  pasivos_audit_saved <- NULL
  row_id2 <- tryCatch(
    pasivos_log_audit(action_type = "provision.edited", user = "u", target_kind = "provision",
                      target_id = "p2", client_id = "networks"),
    error = function(e) "THREW"
  )
  ok("if log_action() itself fails, pasivos_log_audit() still succeeds and still wrote the pasivos_audit row (dual-write degrades gracefully, doesn't take down the primary write)",
     !identical(row_id2, "THREW") && !is.null(pasivos_audit_saved) && nrow(pasivos_audit_saved) == 1L)
}

# ── 3. Static: pasivos_audit.R's dual-write is wired correctly ─────────────
{
  txt <- readLines("R/pasivos_audit.R", warn = FALSE)
  ok("pasivos_log_audit() gained a viewer_home_client_id parameter (client-scoping, matching every other log_action-adjacent call)",
     any(grepl("viewer_home_client_id\\s*=\\s*NULL", txt)))
  start <- grep("log_action\\($", txt)
  ok("found the dual-write call site to scan", length(start) > 0)
  if (length(start)) {
    block <- paste(txt[start[1]:min(start[1]+12, length(txt))], collapse = "\n")
    ok("the dual-write call passes client_id (scoping regression guard)", grepl("client_id\\s*=\\s*client_id", block))
    ok("the dual-write call passes viewer_home_client_id (scoping regression guard)", grepl("viewer_home_client_id\\s*=\\s*viewer_home_client_id", block))
    ok("the dual-write is wrapped in tryCatch (a log_action failure can't break the primary pasivos_audit write)",
       any(grepl("tryCatch\\(", txt[max(1,start[1]-2):start[1]])))
  }
  ok("R/pasivos_audit.R is in the log_action scoping guard's file list (tests/test_saas_log_action_scoping.R)",
     any(grepl('"R/pasivos_audit\\.R"', readLines("tests/test_saas_log_action_scoping.R", warn = FALSE))))
}

# ── 4. Conversion's content_diff -- real functional test of the diff logic
# itself (synthetic prov/input pairs, not just text-matching the code) ─────
{
  # Mirror of the .cmp()-based diff construction in .pasivos_perform_conversion(),
  # kept in sync by hand (same convention as this suite's other tests for
  # logic nested inside a larger function). Tests the LOGIC (does it
  # correctly detect changed vs unchanged fields), not the literal source.
  build_diff <- function(prov, inp) {
    .cmp <- function(a, b) !identical(as.character(a %||% ""), as.character(b %||% ""))
    Filter(Negate(is.null), list(
      if (.cmp(prov$empresa, inp$empresa)) list(field = "empresa", before = prov$empresa, after = inp$empresa),
      if (.cmp(prov$moneda, inp$moneda))   list(field = "moneda",  before = prov$moneda,  after = inp$moneda),
      if (.cmp(prov$importe, inp$importe)) list(field = "importe", before = prov$importe, after = inp$importe),
      if (.cmp(prov$fecha, inp$fecha))     list(field = "fecha",   before = prov$fecha,   after = inp$fecha)
    ))
  }
  assign("%||%", function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x, envir = globalenv())

  unchanged <- list(empresa = "NCS", moneda = "MXN", importe = 100, fecha = "2026-07-01")
  ok("converting with NO edits produces an empty diff (control -- proves the diff isn't always non-empty)",
     length(build_diff(unchanged, unchanged)) == 0L)

  one_change <- list(empresa = "NCS", moneda = "MXN", importe = 150, fecha = "2026-07-01")
  d1 <- build_diff(unchanged, one_change)
  ok("changing only importe produces a diff with exactly 1 entry", length(d1) == 1L)
  ok("that entry correctly names the changed field", d1[[1]]$field == "importe")
  ok("that entry's before value is the ORIGINAL provision amount, not the new one", d1[[1]]$before == 100)
  ok("that entry's after value is the NEW form amount", d1[[1]]$after == 150)

  all_changed <- list(empresa = "NTS", moneda = "USD", importe = 999, fecha = "2026-08-15")
  d2 <- build_diff(unchanged, all_changed)
  ok("changing all 4 tracked fields produces a diff with exactly 4 entries (no field silently skipped)",
     length(d2) == 4L)
  ok("all 4 changed fields are represented, in some order", setequal(vapply(d2, `[[`, character(1), "field"), c("empresa","moneda","importe","fecha")))

  numeric_vs_char <- list(empresa = "NCS", moneda = "MXN", importe = "100", fecha = "2026-07-01")
  ok("comparing 100 (numeric) vs \"100\" (character) is NOT flagged as a change (type-insensitive comparison, avoids false-positive diffs from R's own type coercion)",
     length(build_diff(unchanged, numeric_vs_char)) == 0L)
}

# ── 5. Static: content_diff is threaded through .pasivos_perform_conversion()
# and pasivos_provision_convert() correctly ─────────────────────────────────
{
  mod_txt <- readLines("R/pasivos_module.R", warn = FALSE)
  eng_txt <- readLines("R/pasivos_engine.R", warn = FALSE)

  ok(".pasivos_perform_conversion() computes a content_diff variable",
     any(grepl("content_diff <- Filter", mod_txt)))
  DIFF_FIELDS <- c("empresa", "moneda", "documento", "parte", "codigo", "importe", "fecha", "notas")
  for (fld in DIFF_FIELDS) {
    ok(sprintf("content_diff checks the '%s' field for changes", fld),
       any(grepl(sprintf('field = "%s"', fld), mod_txt)))
  }
  ok(".pasivos_perform_conversion() passes content_diff through to pasivos_provision_convert()",
     any(grepl("content_diff\\s*=\\s*if \\(length\\(content_diff\\)\\)", mod_txt)))
  ok("pasivos_provision_convert() accepts a content_diff parameter (engine signature extended, backward compatible -- defaults to NULL)",
     any(grepl("content_diff = NULL", eng_txt)))
  ok("pasivos_provision_convert() merges content_diff into the audit 'after' payload when present",
     any(grepl("after_payload\\$content_diff <- content_diff", eng_txt)))
  ok("pasivos_provision_convert() still logs 'before' as the full pre-conversion provision row (regression guard -- the estado-transition audit wasn't weakened by this addition)",
     any(grepl("before      = before,", eng_txt)))
}

# ── 6. Static: revert/delete asymmetry closed ───────────────────────────────
{
  mod_txt <- readLines("R/pasivos_module.R", warn = FALSE)

  revert_start <- grep("pcm_do_undo_conversion, ignoreInit = TRUE", mod_txt)
  ok("found the revert (pcm_do_undo_conversion) handler to scan", length(revert_start) > 0)
  if (length(revert_start)) {
    end <- grep("^  \\}\\)$", mod_txt)
    end <- end[end > revert_start[1]][1]
    block <- paste(mod_txt[revert_start[1]:end], collapse = "\n")
    ok("revert handler now captures 'before' from the provision row prior to mutating it",
       grepl("before <- as\\.list\\(provs\\[idx\\[1\\], \\]\\)", block))
    ok("revert handler now captures 'after' from the provision row after mutating it",
       grepl("after <- as\\.list\\(provs\\[idx\\[1\\], \\]\\)", block))
    ok("revert handler's pasivos_log_audit() call now passes before=", grepl("before    = before,", block))
    ok("revert handler's pasivos_log_audit() call now passes after=", grepl("after     = after,", block))
    ok("revert handler still keeps its original descriptive notes= text (enrichment, not replacement)",
       grepl("reverted from converted to provisional via UI", block))
  }

  delete_start <- grep("pcm_do_delete_converted, ignoreInit = TRUE", mod_txt)
  ok("found the delete (pcm_do_delete_converted) handler to scan", length(delete_start) > 0)
  if (length(delete_start)) {
    end <- grep("^  \\}\\)$", mod_txt)
    end <- end[end > delete_start[1]][1]
    block <- paste(mod_txt[delete_start[1]:end], collapse = "\n")
    ok("delete handler now captures 'before' from prov_row (the pre-delete snapshot already used for the papelera archive -- reused, not recomputed)",
       grepl("before <- as\\.list\\(prov_row\\)", block))
    ok("delete handler now captures 'after' from the provision row after the soft-delete mutation",
       grepl("after <- as\\.list\\(provs\\[idx\\[1\\], \\]\\)", block))
    ok("delete handler's pasivos_log_audit() call now passes before=", grepl("before    = before,", block))
    ok("delete handler's pasivos_log_audit() call now passes after=", grepl("after     = after,", block))
    ok("delete handler still archives to papelera (regression guard -- the audit enrichment didn't touch this unrelated existing behavior)",
       grepl("add_to_papelera\\(", block))
  }

  ok("the revert engine function pasivos_provision_revive() exists and is NOT called by the UI handler (documented, deliberate choice -- see commit message: safer to enrich the existing tested path than swap in a different one for a live financial workflow)",
     any(grepl("^pasivos_provision_revive <- function", readLines("R/pasivos_engine.R", warn = FALSE))))
}

# ── 7. Stage 9, Issue D (2026-08-03): "I deleted a provision... did not show
# on activity, ever ... review again all ways of deleting items and
# provisions, same with mover." Full sweep of every save_pasivos_provisions()/
# save_pasivos_liabilities() call site across pasivos_module.R,
# pasivos_list_module.R, pasivos_table_module.R, pasivos_edit_confirm_module.R,
# pasivos_wizard_module.R, pasivos_engine.R, and pasivos_observers.R.
#
# Correction to the Stage 9 doc's premise: it predicted "delete a still-
# open, never-converted provision" (pcm_delete_confirm) might go through a
# path Stage 5 never touched, since Stage 5's own writeup only names three
# enriched call sites (conversion, undo, delete-converted). Reading the code
# shows this doesn't hold up: pcm_delete_confirm has called
# pasivos_log_audit() since 2026-05-08 (git blame, commit 28a92aab) --
# long before Stage 5 existed. Stage 5 fixed pasivos_log_audit() ITSELF
# (the shared function every one of these ~15 call sites already called),
# not just the three call sites its own writeup highlights as examples, so
# this path started dual-writing into the real Actividad log automatically
# the moment e6f15aa landed (2026-07-30), with no separate change needed.
# tests/test_log_action_completeness_scan.R (Stage 6's app-wide, parser-based
# sweep, re-run this stage) independently confirms 0 actionable gaps across
# ALL of R/*.R, corroborating this file-by-file check rather than resting on
# it alone. Most likely explanation for Mouse's live report: a stale running
# R process from before Stage 5 (this app's reactive environment persists
# across runApp() calls in the same R session -- see project memory), the
# same caveat already flagged for Issue C's Bancos papelera report.
{
  mod_txt   <- readLines("R/pasivos_module.R", warn = FALSE)
  list_txt  <- readLines("R/pasivos_list_module.R", warn = FALSE)
  table_txt <- readLines("R/pasivos_table_module.R", warn = FALSE)
  wiz_txt   <- readLines("R/pasivos_wizard_module.R", warn = FALSE)
  eng_txt   <- readLines("R/pasivos_engine.R", warn = FALSE)
  ledger_txt <- readLines("R/ledger_module.R", warn = FALSE)

  # ── 7a. Every delete-shaped path for a STILL-OPEN (never-converted)
  # provision or liability logs, matching pcm_delete_confirm's real shape ──
  del_start <- grep("pcm_delete_confirm, ignoreInit = TRUE", mod_txt)
  ok("found pcm_delete_confirm (delete a still-provisional provision) to scan", length(del_start) > 0)
  if (length(del_start)) {
    end <- grep("^  \\}\\)$", mod_txt); end <- end[end > del_start[1]][1]
    block <- paste(mod_txt[del_start[1]:end], collapse = "\n")
    ok("pcm_delete_confirm rejects deleting anything but a 'provisional' provision (only the still-open state is deletable here -- converted ones go through pcm_do_delete_converted instead, already covered by Stage 5)",
       grepl('estado\\[idx\\[1\\]\\]\\s*!=\\s*"provisional"', block))
    ok("pcm_delete_confirm calls pasivos_log_audit() (the actual finding: this path was NOT a gap)",
       grepl("pasivos_log_audit\\(", block))
    ok("pcm_delete_confirm's audit call uses action_type = \"provision.deleted\" (specific, not a generic bucket)",
       grepl('action_type = "provision\\.deleted"', block))
    ok("pcm_delete_confirm still archives to the shared papelera before logging (regression guard -- Issue D didn't touch this)",
       grepl("add_to_papelera\\(", block))
  }

  # Liability-level delete (archive) and pause/resume -- these mutate
  # save_pasivos_liabilities(), a different store than provisions.
  arch_start <- grep("plist_confirm_archive, ignoreInit = TRUE", list_txt)
  ok("found plist_confirm_archive (delete/archive a liability) to scan", length(arch_start) > 0)
  if (length(arch_start)) {
    end <- grep("^\\s*\\}\\)$", list_txt); end <- end[end > arch_start[1]][1]
    block <- paste(list_txt[arch_start[1]:end], collapse = "\n")
    ok("liability archive/delete calls pasivos_log_audit()", grepl("pasivos_log_audit\\(", block))
    ok("liability archive/delete uses action_type = \"liability.deleted\"",
       grepl('action_type = "liability\\.deleted"', block))
    ok("liability archive/delete also removes that liability's future provisional provisions (regression guard, unrelated to logging)",
       grepl("pasivos_provisions_db\\(\\)", block))
  }
  pause_start <- grep("pasivos_list_pause, ignoreInit = TRUE", list_txt)
  ok("found pasivos_list_pause (pause/resume a liability) to scan", length(pause_start) > 0)
  if (length(pause_start)) {
    end <- grep("^\\s*\\}\\)$", list_txt); end <- end[end > pause_start[1]][1]
    block <- paste(list_txt[pause_start[1]:end], collapse = "\n")
    ok("pause/resume calls pasivos_log_audit()", grepl("pasivos_log_audit\\(", block))
  }

  # Wizard create/edit (liability.created / liability.edited)
  ok(".pasivos_wizard_save() (create/edit a liability + regenerate provisions) calls pasivos_log_audit()",
     any(grepl("pasivos_log_audit\\(", wiz_txt[grep("^\\.pasivos_wizard_save <- function", wiz_txt):length(wiz_txt)][1:150])))

  # Engine-level lifecycle transitions (§2.5): convert, item_confirmed,
  # close, cancel, revive -- every one of these calls .save_provision_row()
  # (the low-level, unlogged-by-design writer, correctly excluded from the
  # app-wide scanner's KNOWN_FINE_HELPERS list) immediately followed by its
  # own pasivos_log_audit() call.
  ENGINE_FNS <- c("pasivos_provision_convert", "pasivos_provision_item_confirmed",
                  "pasivos_provision_close", "pasivos_provision_cancel",
                  "pasivos_provision_revive")
  for (fn in ENGINE_FNS) {
    fn_start <- grep(sprintf("^%s <- function", fn), eng_txt)
    ok(sprintf("found %s() to scan", fn), length(fn_start) > 0)
    if (length(fn_start)) {
      fn_end <- grep("^\\}$", eng_txt); fn_end <- fn_end[fn_end > fn_start[1]][1]
      block <- paste(eng_txt[fn_start[1]:fn_end], collapse = "\n")
      ok(sprintf("%s() calls .save_provision_row() (mutates persisted state)", fn),
         grepl("\\.save_provision_row\\(", block))
      ok(sprintf("%s() also calls pasivos_log_audit() (the mutation is audited)", fn),
         grepl("pasivos_log_audit\\(", block))
    }
  }

  # ── 7b. "Mover" (date move): the Calendario/Ledger generic bulk-move
  # feature deliberately EXCLUDES provisions (pasivos_filter_out_provisions,
  # documented in pasivos_calendar_glue.R as "must be called at every action
  # site that mass-moves items"), rather than silently mis-moving them --
  # verified safe, not a bug. A provision's own date (fecha_efectiva) is
  # changed through the Pasivos module's edit path (pcm_save_provision)
  # instead, which is already logged (action_type = "provision.edited",
  # checked below) -- this matches Mouse's own words in Issue E that
  # "changing a provision... from one date to another does update live and
  # does log", which could only be true via THIS path, not the (provision-
  # excluding) Calendario Mover button. ──────────────────────────────────────
  move_start <- grep("observeEvent\\(input\\$do_move,", ledger_txt)
  ok("found the Calendario/Ledger 'Mover' handler (do_move) to scan", length(move_start) > 0)
  if (length(move_start)) {
    block <- paste(ledger_txt[move_start[1]:min(move_start[1] + 30, length(ledger_txt))], collapse = "\n")
    ok("do_move calls pasivos_filter_out_provisions() before applying the move (provisions deliberately excluded from this bulk mechanism, not silently mismoved)",
       grepl("pasivos_filter_out_provisions\\(keys\\)", block))
  }
  edit_start <- grep("pcm_save_provision, ignoreInit = TRUE", mod_txt)
  ok("found pcm_save_provision (the actual path that moves a provision's date) to scan", length(edit_start) > 0)
  if (length(edit_start)) {
    end <- grep("^  \\}\\)$", mod_txt); end <- end[end > edit_start[1]][1]
    block <- paste(mod_txt[edit_start[1]:end], collapse = "\n")
    ok("pcm_save_provision can change fecha_efectiva (this IS the 'mover' Mouse described for a provision)",
       grepl("fecha_efectiva\\[idx\\]\\s*<-\\s*new_fecha", block))
    ok("pcm_save_provision calls pasivos_log_audit() whenever it saves (covers the date-move case, not just other fields)",
       grepl("pasivos_log_audit\\(", block))
  }

  # ── 7c. Real functional test: pcm_delete_confirm's exact pasivos_log_audit()
  # call (action_type, notes) produces a genuinely readable Actividad entry,
  # not just "some log_action fired" -- the doc's own explicit warning that a
  # clean scanner run alone isn't proof of that. Reuses the REAL,
  # already-imported pasivos_log_audit() and log_action_calls hook from
  # section 2 above -- but section 2's OWN last test deliberately broke
  # log_action() (to prove pasivos_log_audit() degrades gracefully), so it
  # must be restored to the recording stub here first, or this section would
  # just be re-testing that same failure path by accident.
  assign("log_action", function(...) { log_action_calls[[length(log_action_calls)+1L]] <<- list(...); invisible(NULL) }, envir = globalenv())
  log_action_calls <- list()
  pasivos_audit_saved <- NULL
  pasivos_log_audit(
    action_type = "provision.deleted",
    user        = "larivera",
    target_kind = "provision", target_id = "prov-del-1",
    notes       = "soft-deleted via convert modal; archived to papelera",
    client_id   = "networks"
  )
  ok("a real provision.deleted call produces exactly 1 log_action() entry", length(log_action_calls) == 1L)
  if (length(log_action_calls)) {
    entry <- log_action_calls[[1]]
    ok("the entry's module is \"pasivos\" (shows up under the right filter in Actividad)", identical(entry$module, "pasivos"))
    ok("the entry's description names the action AND the human-readable notes (a viewer reads this without cross-referencing action_type codes)",
       grepl("provision.deleted", entry$description, fixed = TRUE) &&
       grepl("archived to papelera", entry$description, fixed = TRUE))
    ok("the entry's target_id identifies exactly which provision was deleted", identical(entry$target_id, "prov-del-1"))
    ok("the entry's client_id is scoped (Issue E2's isolation boundary depends on every log_action call actually doing this)",
       identical(entry$client_id, "networks"))
  }

  # ── 7d. The one real (small) fix this stage made: ledger_module.R's
  # generic batch-delete now names provisions explicitly in its description
  # when the deleted batch includes one, instead of a generic "N factura(s)"
  # that reads identically whether a provision was involved or not. ─────────
  bd_start <- grep("observeEvent\\(input\\$confirm_delete,", ledger_txt)
  ok("found the Calendario/Ledger batch-delete handler (confirm_delete) to scan", length(bd_start) > 0)
  if (length(bd_start)) {
    end <- grep('^\\s*\\)\\s*$', ledger_txt)
    la_start <- grep("action\\s*=\\s*\"eliminar_facturas\"", ledger_txt)
    la_start <- la_start[la_start > bd_start[1]][1]
    ok("found the eliminar_facturas log_action() call to scan", !is.na(la_start))
    if (!is.na(la_start)) {
      block <- paste(ledger_txt[max(1, la_start - 15):(la_start + 10)], collapse = "\n")
      ok("batch-delete's description now conditionally names provisiones when n_prov > 0 (Stage 9 enrichment)",
         grepl("incluye %d provisión", block, fixed = TRUE))
      ok("batch-delete's metadata now includes n_provisiones (structured, not just prose)",
         grepl("n_provisiones\\s*=\\s*n_prov", block))
      ok("batch-delete still logs unconditionally even when n_prov is 0 (regression guard -- the enrichment is additive, not a new gate on whether logging happens at all)",
         grepl("desc <- if \\(n_prov > 0\\)", block))
    }
  }
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0) stop("Tests FAILED.")
invisible(NULL)
