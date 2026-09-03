# =============================================================================
# tests/test_pasivos_activity_richness.R
#
# Stage 3 of the Bancos/Pasivos activity-logging extension (2026-09-03).
# tests/test_pasivos_audit_visibility.R (Stage 5 of the earlier real-time-
# refresh/audit-logging effort, 2026-07-30) already proved every pasivos
# action is VISIBLE in Actividad (the dual-write into log_action() exists
# and fires). This stage is about two further, narrower things it didn't
# cover:
#   1. Several pasivos_log_audit() call sites were visible but not RICH --
#      before/after payloads that were missing entirely, partial, or
#      notes-only despite real structured data being in scope. Fixed at
#      6 call sites (see Part C).
#   2. Two action_type strings used at real call sites
#      ("provision.cancelled", "bulk.purge_closed_provisions") were never
#      added to PASIVOS_ACTION_TYPES -- every real call silently fired
#      `warning("[pasivos] unknown action_type: ...")`. Fixed in Part A.
#   3. R/audit_log_viewer_module.R's Actividad popup had no bespoke pasivos
#      view -- before/after rendered as an unlabeled unlist()-flattened
#      blob via the generic fallback. Fixed with ONE generic before/after
#      diff renderer (unlike Bancos' per-action bespoke tables) since every
#      pasivos_log_audit() call shares the same metadata shape
#      (target_kind/empresa/before/after) even though the field sets inside
#      before/after vary wildly by action type. See Part B.
#
# Sourced by _run_confirmed_logic.R (NOT _run_saas.R -- matches where its
# sibling test_pasivos_audit_visibility.R already lives; this runner has no
# global shiny/AWS setup, so this file loads what it needs itself).
# =============================================================================

cat("=== test_pasivos_activity_richness ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

suppressPackageStartupMessages(library(shiny))
.html <- function(tag) as.character(htmltools::doRenderTags(tag))

# =============================================================================
# Part A -- PASIVOS_ACTION_TYPES: the 2 missing entries
# =============================================================================
{
  .extract_fn("R/pasivos_audit.R", "PASIVOS_ACTION_TYPES")
  ok("PASIVOS_ACTION_TYPES now includes \"provision.cancelled\" (pasivos_provision_cancel(), R/pasivos_engine.R, previously fired unknown-action-type on every real call)",
     "provision.cancelled" %in% PASIVOS_ACTION_TYPES)
  ok("PASIVOS_ACTION_TYPES now includes \"bulk.purge_closed_provisions\" (pasivos_admin_purge_closed(), R/pasivos_module.R, same previously-unrecognized-on-every-call bug)",
     "bulk.purge_closed_provisions" %in% PASIVOS_ACTION_TYPES)
}

# =============================================================================
# Part B -- .audit_detail_pasivos_diff() -- the new bespoke renderer
# =============================================================================
{
  source("R/audit_log_viewer_module.R", local = FALSE)

  .chk_kind <- function(module, action, expected)
    ok(sprintf(".audit_detail_kind(\"%s\", \"%s\") -> \"%s\"", module, action, expected),
       identical(.audit_detail_kind(module, action), expected))
  .chk_kind("pasivos", "liability.created", "pasivos_diff")
  .chk_kind("pasivos", "provision.converted_to_item", "pasivos_diff")
  .chk_kind("pasivos", "capability.denied", "pasivos_diff")
  .chk_kind("bancos", "importar_movimientos", "bancos_movimientos")  # unaffected by this stage

  # No before/after at all (e.g. capability.denied, notes-only) -- must not
  # crash, must say something rather than render an empty shell.
  html_empty <- .html(.audit_detail_pasivos_diff(list(target_kind = "provision", empresa = "NCS")))
  ok("no before/after: shows the 'no detail' fallback message",
     grepl("Sin detalle adicional", html_empty, fixed = TRUE))
  ok("no before/after: still shows the target_kind/empresa header",
     grepl("provision", html_empty, fixed = TRUE) && grepl("NCS", html_empty, fixed = TRUE))

  # after only (creation, e.g. provision.generated / liability.created) --
  # flat field dump, not a diff table (nothing to diff against).
  html_create <- .html(.audit_detail_pasivos_diff(list(
    target_kind = "liability", empresa = "NTS",
    after = list(nombre = "Renta oficina", categoria = "regular", amount_default = 15000))))
  ok("after-only (creation): the new value renders", grepl("Renta oficina", html_create, fixed = TRUE))
  ok("after-only (creation): the field name renders as a label", grepl("nombre", html_create, fixed = TRUE))

  # before+after with a real change -- diff table, changed fields only.
  html_diff <- .html(.audit_detail_pasivos_diff(list(
    target_kind = "provision", empresa = "NCS",
    before = list(estado = "provisional", amount_pago = 1000, parte = "Proveedor A"),
    after  = list(estado = "converted",   amount_pago = 1000, parte = "Proveedor A"))))
  ok("before+after diff: the changed field's BEFORE value renders", grepl("provisional", html_diff, fixed = TRUE))
  ok("before+after diff: the changed field's AFTER value renders", grepl("converted", html_diff, fixed = TRUE))
  ok("before+after diff: an UNCHANGED field (amount_pago, identical before/after) does not render as a diff row",
     !grepl(">1000<.*>1000<", gsub("\n", " ", html_diff)))
  ok("before+after diff: an unchanged text field (parte) does not appear duplicated as a before/after pair",
     lengths(regmatches(html_diff, gregexpr("Proveedor A", html_diff))) <= 1)

  # before+after with NO actual change -- e.g. a no-op save.
  html_nochange <- .html(.audit_detail_pasivos_diff(list(
    target_kind = "provision", empresa = "NCS",
    before = list(estado = "provisional"), after = list(estado = "provisional"))))
  ok("before+after with identical values: shows a 'no changes' message, not an empty table",
     grepl("Sin cambios de valor", html_nochange, fixed = TRUE))

  # A list-column value (e.g. recurrence_params, or the bulk decisions array
  # from .pec_apply_decisions()) must render as SOMETHING, not crash and not
  # print an R-internal representation like "list(a = 1)".
  html_listval <- .html(.audit_detail_pasivos_diff(list(
    target_kind = "bulk", empresa = NA_character_,
    after = list(n_conflicts = 2L,
                decisions = list(list(provision_id = "p1", decision = "aplicar"),
                                 list(provision_id = "p2", decision = "mantener"))))))
  ok("a nested list-valued field renders without crashing (flattened, not literal R syntax)",
     grepl("p1", html_listval, fixed = TRUE) && !grepl("list(", html_listval, fixed = TRUE))

  # NA-safety: %||% only substitutes on NULL, not NA -- a field genuinely
  # holding NA_character_ (very common in the liability schema: subcategoria,
  # flavor, cotizado_en are usually NA) must render as blank, not literal "NA".
  html_na <- .html(.audit_detail_pasivos_diff(list(
    target_kind = "liability", empresa = "NCS",
    before = list(flavor = NA_character_), after = list(flavor = "credito_simple"))))
  ok("an NA before-value renders as blank, not the literal string 'NA'",
     !grepl("<td>NA</td>", html_na, fixed = TRUE))
}

# =============================================================================
# Part C -- static scan: the 6 enriched call sites really pass before=/after=
# with real captured variables now, not the old gaps
# =============================================================================
{
  # Grabs from the handler's start line through ~15 lines past the first
  # pasivos_log_audit() call that follows it -- covers both the pre-call
  # `before <-`/snapshot lines AND the call's own parameter list, without
  # the fragility of trying to brace-match the handler's true closing line
  # (a naive "first bare '})'" search hits an unrelated inner tryCatch's
  # close well before the real end in more than one of these handlers).
  .scan_block <- function(txt, start_pattern) {
    s <- grep(start_pattern, txt)
    if (!length(s)) return(NA_character_)
    call_start <- grep("pasivos_log_audit\\(", txt)
    call_start <- call_start[call_start > s[1]][1]
    if (is.na(call_start)) return(NA_character_)
    paste(txt[s[1]:min(call_start + 15, length(txt))], collapse = "\n")
  }

  # C1: pcm_save_provision (provision.edited) -- was after-only, 3 fields.
  mod_txt <- readLines("R/pasivos_module.R", warn = FALSE)
  b1 <- .scan_block(mod_txt, "pcm_save_provision, ignoreInit = TRUE")
  ok("C1 provision.edited: found pcm_save_provision to scan", !is.na(b1))
  if (!is.na(b1)) {
    ok("C1: now captures `before` from the provision row prior to mutation",
       grepl("before <- as\\.list\\(provs\\[idx, \\]\\)", b1))
    ok("C1: pasivos_log_audit() now receives before=before", grepl("before      = before,", b1))
    ok("C1: `after` is now the full post-mutation row, not a 3-field hand-picked list",
       grepl("after       = as\\.list\\(provs\\[idx, \\]\\)", b1))
  }

  # C2: liability wizard save (liability.created/liability.edited).
  wiz_txt <- readLines("R/pasivos_wizard_module.R", warn = FALSE)
  b2 <- .scan_block(wiz_txt, "^\\.pasivos_wizard_save <- function")
  ok("C2 liability.created/edited: found .pasivos_wizard_save() to scan", !is.na(b2))
  if (!is.na(b2)) {
    ok("C2: captures before_row from the pre-replace liabs snapshot on edit",
       grepl('before_row <- if \\(wiz\\$mode == "edit"\\)', b2))
    ok("C2: pasivos_log_audit() now receives a real before= (not omitted)",
       grepl("before      = if \\(!is\\.null\\(before_row\\)", b2))
    ok("C2: `after` is the FULL liability row (all payment-terms columns), not 5 hand-picked identity fields",
       grepl("after       = as\\.list\\(liab_row\\)", b2))
  }

  # C3: liability pause/resume (liability.edited) -- was notes-only.
  list_txt <- readLines("R/pasivos_list_module.R", warn = FALSE)
  b3 <- .scan_block(list_txt, "pasivos_list_pause, ignoreInit = TRUE")
  ok("C3 liability pause/resume: found the handler to scan", !is.na(b3))
  if (!is.na(b3)) {
    ok("C3: captures old_estado before flipping it", grepl("old_estado <- liabs\\$estado\\[idx\\[1\\]\\]", b3))
    ok("C3: pasivos_log_audit() now receives before=/after= (was notes-only)",
       grepl("before      = list\\(nombre = nombre, estado = old_estado\\)", b3) &&
       grepl("after       = list\\(nombre = nombre, estado = new_estado\\)", b3))
  }

  # C4: liability archive/delete (liability.deleted) -- had no metadata at all.
  b4 <- .scan_block(list_txt, "plist_confirm_archive, ignoreInit = TRUE")
  ok("C4 liability archive/delete: found the handler to scan", !is.na(b4))
  if (!is.na(b4)) {
    ok("C4: captures old_estado before the soft-delete mutation", grepl("old_estado <- liabs\\$estado\\[idx\\[1\\]\\]", b4))
    ok("C4: pasivos_log_audit() now receives before=/after= (was metadata-free)",
       grepl('after       = list\\(nombre = nombre, estado = "deleted"\\)', b4))
  }

  # C5: .pec_apply_decisions (bulk.aplicar_pagos_futuros) -- was count-only.
  pec_txt <- readLines("R/pasivos_edit_confirm_module.R", warn = FALSE)
  b5 <- .scan_block(pec_txt, "^\\.pec_apply_decisions <- function")
  ok("C5 bulk.aplicar_pagos_futuros: found .pec_apply_decisions() to scan", !is.na(b5))
  if (!is.na(b5)) {
    ok("C5: builds a per-provision decision_log inside the loop",
       grepl("decision_log\\[\\[length\\(decision_log\\) \\+ 1L\\]\\]", b5))
    ok("C5: pasivos_log_audit() now receives the decisions list as after= (was a bare conflict count)",
       grepl("after       = list\\(n_conflicts = nrow\\(conflicts\\), decisions = decision_log\\)", b5))
  }

  # C6: pasivos_admin_purge_closed (bulk.purge_closed_provisions) -- notes-only.
  b6 <- .scan_block(mod_txt, "^pasivos_admin_purge_closed <- function")
  ok("C6 bulk.purge_closed_provisions: found pasivos_admin_purge_closed() to scan", !is.na(b6))
  if (!is.na(b6)) {
    ok("C6: pasivos_log_audit() now receives before= (the purged row ids) and after= (the backup key)",
       grepl("before      = list\\(ids = ids_capped", b6) &&
       grepl("after       = list\\(backup_key = backup_key\\)", b6))
    ok("C6: the id list is capped (this scans the whole client's history, unlike Bancos' per-session bulk actions -- audit found this was unbounded and asked for the same discipline as .bancos_audit_rows_summary())",
       grepl("ids_capped\\s*<-\\s*utils::head\\(ids_all,", b6))
  }

  # C7: forecasting_set_estimate (estimate.changed) -- overwrite lost the old value.
  fc_txt <- readLines("R/forecasting_service.R", warn = FALSE)
  b7 <- .scan_block(fc_txt, "^forecasting_set_estimate <- function")
  ok("C7 estimate.changed: found forecasting_set_estimate() to scan", !is.na(b7))
  if (!is.na(b7)) {
    ok("C7: captures before_est from the existing row before an overwrite",
       grepl("before_est <- if \\(length\\(existing_idx\\)\\)", b7))
    ok("C7: pasivos_log_audit() now receives before=before_est", grepl("before      = before_est,", b7))
  }
}

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0) stop("Tests FAILED.")
