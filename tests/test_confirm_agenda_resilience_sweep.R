# =============================================================================
# tests/test_confirm_agenda_resilience_sweep.R
# Follow-up to tests/test_confirm_agenda_resilience.R (2026-08-05): a second
# pair of eyes on that fix pointed out the exact same bug shape -- a bare
# save_pagar_hoy() call sitting unguarded between an optimistic in-memory
# update and a log_action() call -- also exists at 9 other call sites, not
# just the two confirm handlers. Same root mechanism every time: a network
# hiccup during that specific S3 write leaves the acting user's own screen
# looking fine (the reactiveVal already updated in memory one line earlier)
# while nothing persists and nothing gets logged.
#
# Sites fixed this sweep (see save_pagar_hoy_with_retry()'s own header
# comment in R/persistence.R for the shared root-cause writeup):
#   R/pagar_hoy_module.R "Remove AP"  (quitar_pagos)  -- ALSO reverts a
#     linked Pasivos provision right after the save; a failure here must
#     stop BEFORE that revert too, or the provision would flip back to
#     "provisional" (editable) while its pagar_hoy row is still sitting in
#     S3, unremoved -- worse than a plain no-op.
#   R/pagar_hoy_module.R "Remove AR"  (quitar_cobros)  -- no linked-provision
#     side effect (provisions are AP-only) -- simpler fix.
#   R/ledger_module.R stage_all, stage_sel, the cart_<i> group-toggle
#     (both its stage and unstage branches), and cart_inv_click (both its
#     stage and unstage branches) -- all the same shape as the original
#     confirm handlers: guard, on failure show a clear error + log the
#     incompleteness + stop before any success notification/log.
#   R/ledger_module.R's .sync_staged() -- a DIFFERENT, deliberately
#     different fix: this is a shared, best-effort "keep an already-staged
#     Agenda row's displayed date/amount in sync with an edit made
#     elsewhere" helper (called from do_move/do_restore/sap_edit_save in
#     this file, and app.R's manual-entry edit-mode save) -- not a primary
#     user-facing action; every caller already does its OWN primary save +
#     its OWN log_action for the actual action, entirely separately. Two of
#     its four callers already fully contain a failure here (do_move/
#     do_restore wrap their whole handler in a tryCatch; sap_edit_save wraps
#     this specific call in its own), but app.R's manual-entry edit-mode
#     call had NO containment at all -- an uncaught failure there would have
#     skipped removeModal()/the success notification even though the
#     primary edit had already saved successfully. Rather than bolt a
#     bespoke tryCatch onto that one call site, the fix is inside the shared
#     helper itself: it now retries internally and can never throw, so
#     every caller is safe by construction, with no per-site changes needed.
#     No NEW log_action() call was added for its failure path -- there was
#     never one for its success path either, at any of the 4 call sites
#     (it's cosmetic display sync, not a business action) -- adding one only
#     for failure would be new, disproportionate audit noise. A warning()
#     (matching the pattern sap_edit_save's own pre-existing tryCatch
#     already uses for this exact call) is the right visibility level.
# =============================================================================
cat("=== test_confirm_agenda_resilience_sweep ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) {
    cat(" PASS:", label, "\n"); .pass <<- .pass + 1L
  } else {
    cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L
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
assign("%||%", function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x, envir = globalenv())
.extract_fn("R/persistence.R", "save_pagar_hoy_with_retry")
.extract_fn("R/ledger_module.R", ".sync_staged")

# ── 1. Functional: the real .sync_staged(), synthetic failures ─────────────
# .sync_staged is a real, top-level, extractable function -- unlike the
# per-empresa observeEvent handlers (deeply nested inside moduleServer +
# lapply, not mechanically extractable), so this section drives the ACTUAL
# production function, not a mirror of it.
{
  keys <- data.frame(Empresa = "NCS", Moneda = "MXN", Documento = "SAP-100", stringsAsFactors = FALSE)
  ph_current <- data.frame(
    ledger = "AP", status = "pending", Empresa = "NCS", Moneda = "MXN",
    Documento = "SAP-100", FechaVenc = as.Date("2026-08-01"), Importe = 1000,
    stringsAsFactors = FALSE
  )
  ph_rv_val <- NULL
  ph_rv <- function(x) { if (missing(x)) ph_rv_val else { ph_rv_val <<- x; invisible(x) } }

  # No matching staged row at all: no-op, reports changed=FALSE, never calls save.
  save_calls <- 0L
  assign("save_pagar_hoy", function(...) { save_calls <<- save_calls + 1L; invisible(TRUE) }, envir = globalenv())
  ph_rv_val <- NULL
  res_noop <- .sync_staged(
    data.frame(Empresa = "NCS", Moneda = "MXN", Documento = "NO-SUCH-DOC", stringsAsFactors = FALSE),
    "AP", ph_rv, new_date = as.Date("2026-08-05"), username = "u", client_id = "networks")
  ok(".sync_staged(): no matching staged row -> reports changed=FALSE, ok=TRUE, never calls save",
     isTRUE(res_noop$changed == FALSE) && isTRUE(res_noop$ok) && save_calls == 0L)

  # A matching row, save succeeds: reports changed=TRUE, ok=TRUE, and the row's
  # FechaVenc is actually updated.
  save_calls <- 0L
  assign("save_pagar_hoy", function(df, username = NULL, client_id = NULL) {
    save_calls <<- save_calls + 1L; invisible(TRUE)
  }, envir = globalenv())
  ph_rv_val <- NULL
  res_ok <- .sync_staged(keys, "AP", function(v) {
    if (missing(v)) ph_current else { ph_current <<- v; invisible(v) }
  }, new_date = as.Date("2026-08-05"), username = "u", client_id = "networks")
  ok(".sync_staged(): matching row + successful save -> changed=TRUE, ok=TRUE",
     isTRUE(res_ok$changed) && isTRUE(res_ok$ok))
  ok(".sync_staged(): the staged row's FechaVenc is actually updated to the new date",
     as.character(ph_current$FechaVenc[1]) == "2026-08-05")
  ok(".sync_staged(): calls the real save exactly once on the happy path", save_calls == 1L)

  # A matching row, save fails every attempt: NEVER throws (the actual fix --
  # this is what makes app.R's previously-bare call site safe), reports
  # ok=FALSE with the real error, and still warns (not silent).
  ph_current$FechaVenc <- as.Date("2026-08-01")  # reset
  assign("save_pagar_hoy", function(...) stop("simulated persistent network error"), envir = globalenv())
  threw <- FALSE
  warned <- FALSE
  res_fail <- withCallingHandlers(
    tryCatch(
      .sync_staged(keys, "AP", function(v) {
        if (missing(v)) ph_current else { ph_current <<- v; invisible(v) }
      }, new_date = as.Date("2026-08-06"), username = "u", client_id = "networks"),
      error = function(e) { threw <<- TRUE; NULL }
    ),
    warning = function(w) { warned <<- TRUE; invokeRestart("muffleWarning") }
  )
  ok(".sync_staged(): a total save failure NEVER throws uncaught (the actual fix -- this is what makes every caller, including app.R's previously-bare one, safe by construction)",
     !threw)
  ok(".sync_staged(): a total save failure still emits a warning (visible to a developer, not fully silent)",
     warned)
  ok(".sync_staged(): a total save failure is reported as ok=FALSE with the real underlying error",
     !is.null(res_fail) && !isTRUE(res_fail$ok) && !is.null(res_fail$error) &&
     grepl("simulated persistent network error", conditionMessage(res_fail$error)))
  ok(".sync_staged(): even on total failure, it still reports changed=TRUE (a real matching row WAS found, just not durably saved -- the caller can tell 'we found something to sync' apart from 'nothing needed syncing')",
     isTRUE(res_fail$changed))
}

# ── 2. Static: R/pagar_hoy_module.R's "Remove AP" -- the provision-revert
# ordering is the one genuinely site-specific behavior here, so it gets its
# own dedicated check (not just "same as everywhere else"): a failed save
# must stop BEFORE the provision-revert block runs. ─────────────────────────
{
  txt <- readLines("R/pagar_hoy_module.R", warn = FALSE)
  rm_ap_start <- grep('observeEvent\\(input\\[\\[paste0\\("remove_ap_", emp\\)\\]\\],', txt)
  ok("found the 'Remove AP' handler (quitar_pagos) to scan", length(rm_ap_start) > 0)
  if (length(rm_ap_start)) {
    rm_ar_start <- grep('observeEvent\\(input\\[\\[paste0\\("remove_ar_", emp\\)\\]\\],', txt)
    block <- paste(txt[rm_ap_start[1]:(rm_ar_start[1] - 1)], collapse = "\n")

    ok("Remove AP calls save_pagar_hoy_with_retry() (the fix)", grepl("save_pagar_hoy_with_retry\\(", block))
    ok("Remove AP checks remove_result$ok and returns before the provision-revert block on failure",
       grepl("isTRUE\\(remove_result\\$ok\\)", block) && grepl("return\\(\\)", block))
    ok("Remove AP logs the failure itself (quitar_pagos_incompleto)",
       grepl('action = "quitar_pagos_incompleto"', block))
    # found 2026-09-05: the revert itself used to be an inline
    # provs$estado[idx_p] <- "provisional" mutation, saved once for the
    # whole batch, with NO audit call at all -- a real blind spot (see
    # tests/test_pasivos_activity_richness.R Part [pagar_hoy revive] for the
    # audit-completeness side of this fix). Now routed through the same
    # pasivos_provision_revive() the explicit "Deshacer conversión" button
    # already uses, which logs provision.revived internally.
    ok("Remove AP's provision-revert block still exists and is now wrapped in its own outer tryCatch (secondary_err_rmap)",
       grepl("secondary_err_rmap <- tryCatch\\(\\{", block) && grepl("pasivos_provision_revive\\(", block))
    ok("Remove AP still logs the normal success action (quitar_pagos) on the happy path (regression guard)",
       grepl('"pagar_hoy", "quitar_pagos"', block))

    # The actual ordering claim: in the source text, the failure-path
    # return() must appear BEFORE the provision-revert loop
    # (pasivos_provision_revive( ... )), so a failed save can never reach
    # the revert.
    return_pos  <- regexpr("if \\(!isTRUE\\(remove_result\\$ok\\)\\)", block)
    revert_pos  <- regexpr("pasivos_provision_revive\\(", block)
    ok("Remove AP's failure-check appears BEFORE the provision-revert code in source order (the actual safety property)",
       return_pos > 0 && revert_pos > 0 && return_pos < revert_pos)
  }

  # "Remove AR" -- simpler, no provision revert.
  rm_ar_start <- grep('observeEvent\\(input\\[\\[paste0\\("remove_ar_", emp\\)\\]\\],', txt)
  ok("found the 'Remove AR' handler (quitar_cobros) to scan", length(rm_ar_start) > 0)
  if (length(rm_ar_start)) {
    conf_ap_start <- grep("Confirm AP", txt)
    conf_ap_start <- conf_ap_start[conf_ap_start > rm_ar_start[1]][1]
    block <- paste(txt[rm_ar_start[1]:(conf_ap_start %||% (rm_ar_start[1] + 40))], collapse = "\n")
    ok("Remove AR calls save_pagar_hoy_with_retry() (the fix)", grepl("save_pagar_hoy_with_retry\\(", block))
    ok("Remove AR logs the failure itself (quitar_cobros_incompleto)",
       grepl('action = "quitar_cobros_incompleto"', block))
    ok("Remove AR still logs the normal success action (quitar_cobros) on the happy path (regression guard)",
       grepl('"pagar_hoy", "quitar_cobros"', block))
  }
}

# ── 3. Static: R/ledger_module.R's 6 remaining sites (stage_all, stage_sel,
# cart_<i> x2 branches, cart_inv_click x2 branches) all use the fix ────────
{
  txt <- readLines("R/ledger_module.R", warn = FALSE)
  sites <- list(
    list(anchor = "observeEvent\\(input\\$stage_all,",          action = "enviar_a_agenda_dia_incompleto",  label = "stage_all"),
    list(anchor = "observeEvent\\(input\\$stage_sel,",           action = "enviar_a_agenda_dia_incompleto",  label = "stage_sel"),
    list(anchor = 'observeEvent\\(input\\[\\[paste0\\("cart_", i\\)\\]\\],', action = "quitar_de_agenda_dia_incompleto", label = "cart_<i> (unstage branch)"),
    list(anchor = "observeEvent\\(input\\$cart_inv_click,",      action = "quitar_de_agenda_dia_incompleto", label = "cart_inv_click (unstage branch)")
  )
  n_site_result <- length(gregexpr("site_result <- save_pagar_hoy_with_retry\\(", paste(txt, collapse = "\n"))[[1]])
  ok("exactly 4 site_result <- save_pagar_hoy_with_retry(...) call sites exist (cart_<i> stage+unstage, cart_inv_click stage+unstage)",
     n_site_result == 4L)
  n_stage_result <- length(gregexpr("stage_result <- save_pagar_hoy_with_retry\\(", paste(txt, collapse = "\n"))[[1]])
  ok("exactly 2 stage_result <- save_pagar_hoy_with_retry(...) call sites exist (stage_all, stage_sel)",
     n_stage_result == 2L)

  for (s in sites) {
    start <- grep(s$anchor, txt)
    ok(sprintf("found %s to scan", s$label), length(start) > 0)
    if (length(start)) {
      block <- paste(txt[start[1]:min(start[1] + 150, length(txt))], collapse = "\n")
      ok(sprintf("%s logs its own failure action (%s) on total save failure", s$label, s$action),
         grepl(sprintf('action = "%s"', s$action), block) || grepl(sprintf('action\\s*=\\s*"%s"', s$action), block))
    }
  }

  ok("stage_all's failure path stops (return()) strictly BEFORE its own success log_action (enviar_a_agenda_dia, not the _incompleto one) in source order",
     {
       start <- grep("observeEvent\\(input\\$stage_all,", txt)
       block <- paste(txt[start[1]:min(start[1] + 150, length(txt))], collapse = "\n")
       incompleto_pos <- regexpr('action = "enviar_a_agenda_dia_incompleto"', block)
       success_pos    <- regexpr('action      = "enviar_a_agenda_dia"', block)
       # The block's very first return() is an EARLIER, unrelated early-exit
       # (the "no hay items para enviar" empty-selection guard) -- search for
       # the return() that specifically follows the _incompleto action, not
       # just the first return() anywhere in the whole handler.
       after_incompleto <- substring(block, incompleto_pos)
       return_pos_rel   <- regexpr("\\breturn\\(\\)", after_incompleto)
       incompleto_pos > 0 && success_pos > 0 && return_pos_rel > 0 &&
         (incompleto_pos + return_pos_rel) < success_pos
     })

  ok(".sync_staged() is still referenced from exactly 3 call sites in ledger_module.R (do_move/do_restore/sap_edit_save) -- regression guard against this sweep's own comments accidentally adding a phantom 4th textual match",
     length(grep("\\.sync_staged\\(", txt)) == 3L)
}

# ── 4. Static: app.R's previously-bare .sync_staged() call site is now safe
# by construction (the shared-helper fix), no bespoke wrapper needed there ─
{
  txt <- readLines("app.R", warn = FALSE)
  start <- grep("\\.sync_staged\\(", txt)
  ok("found app.R's .sync_staged() call site (manual-entry edit-mode save)", length(start) > 0)
  if (length(start)) {
    ok("app.R's call site is unchanged (no bespoke tryCatch added -- the fix lives inside the shared helper, per the sweep's own design note)",
       !grepl("tryCatch\\(\\s*\\n?\\s*\\.sync_staged", paste(txt[max(1,start[1]-2):start[1]], collapse = "\n")))
  }
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0) stop("Tests FAILED.")
invisible(NULL)
