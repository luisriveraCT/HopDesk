# =============================================================================
# tests/test_log_action_completeness_scan.R
# Stage 6 of the real-time-refresh/audit-logging strategy (2026-07-30):
# systematic sweep for "every mutating button produces a log entry",
# per the explicit mandate to be absolutely sure -- not another manual pass
# (this session already found gaps by hand in Vencidos/Search, Bancos
# manual movements, and Pasivos; manual passes miss things).
#
# Uses R's own parser (not line-window heuristics, which proved fragile
# elsewhere in this suite -- several existing tests needed their windows
# widened this session when unrelated code shifted their target lines) to
# find the EXACT source boundaries of every observeEvent(...) block and
# named handler function in R/*.R, then checks whether each block that
# calls a save_*()/bump_sync_version() (i.e. mutates persisted state) also
# calls log_action()/pasivos_log_audit() (i.e. is auditable).
#
# This file has two parts:
#   1. The scanner itself, verified against synthetic fixture files first
#      (a fake logged handler and a fake unlogged one) so its own output is
#      trustworthy before using it for anything real.
#   2. Running it for real across R/*.R and reporting the punch list.
# =============================================================================
cat("=== test_log_action_completeness_scan ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) {
    cat(" PASS:", label, "\n"); .pass <<- .pass + 1L
  } else {
    cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L
  }
}

# ── The scanner ──────────────────────────────────────────────────────────────
# Returns a data.frame: file, line, label, kind ("observeEvent" | "function"),
# mutates (logical), logs (logical). Callers filter for mutates & !logs.
scan_log_action_completeness <- function(file) {
  exprs <- tryCatch(parse(file, keep.source = TRUE), error = function(e) NULL)
  if (is.null(exprs)) return(data.frame())

  results <- list()
  MUTATE_PAT <- "\\bsave_[A-Za-z0-9_.]*\\s*\\(|\\bbump_sync_version\\s*\\("
  # T72-E: a write that passes a real event (not NULL, not evento_parte_de())
  # is logged by the write layer once it worked.
  # A wrapper's pass-through (`evento = evento`) is not a log: its callers are.
  LOG_PAT    <- "\\blog_action\\s*\\(|\\bpasivos_log_audit\\s*\\(|\\bevento\\s*=(?!=|\\s*(NULL\\b|evento_parte_de\\b|evento\\b))"

  .record <- function(call_expr, kind, label) {
    sref <- attr(call_expr, "srcref")
    txt  <- if (!is.null(sref)) paste(as.character(sref), collapse = "\n")
            else paste(deparse(call_expr), collapse = "\n")
    mutates <- grepl(MUTATE_PAT, txt)
    logs    <- grepl(LOG_PAT, txt, perl = TRUE)
    line    <- if (!is.null(sref)) sref[1] else NA_integer_
    results[[length(results) + 1L]] <<- data.frame(
      file = file, line = line, label = label, kind = kind,
      mutates = mutates, logs = logs, stringsAsFactors = FALSE
    )
  }

  .walk <- function(e) {
    tryCatch({
    if (is.call(e)) {
      fn_name <- tryCatch(as.character(e[[1]]), error = function(e) "")
      # Dead code (if (FALSE) { ... }) still parses as valid R -- this is
      # exactly the CONCILIAR_REMOVED/VINCULAR_RETIRED convention used
      # elsewhere in this codebase to retire a feature while keeping its
      # code. R's parser has no concept of "this branch never runs", so
      # without this check the scanner would flag genuinely inert code as
      # a live gap. Skip recursing into a literal if(FALSE)'s body.
      if (length(fn_name) == 1L && fn_name == "if" && length(e) >= 2 &&
          identical(e[[2]], FALSE)) {
        return(invisible(NULL))
      }
      if (length(fn_name) == 1L && fn_name == "observeEvent" && length(e) >= 3) {
        ev_label <- tryCatch(paste(deparse(e[[2]]), collapse = " "), error = function(e) "?")
        .record(e, "observeEvent", ev_label)
      }
      # Recurse into all sub-expressions (handles nesting inside
      # moduleServer()/function()/if()/etc. at any depth). Guard against R's
      # "missing argument" placeholder (the empty slot in df[idx, ], used
      # throughout this codebase) -- indexing into it or passing it on
      # throws "argument is missing, with no default", which a plain
      # tryCatch around the EXTRACTION doesn't catch (the error happens when
      # the missing value is later used, not when e[[i]] itself resolves).
      n <- tryCatch(length(e), error = function(e) 0L)
      for (i in seq_len(n)) {
        has_item <- tryCatch({ item <- e[[i]]; TRUE }, error = function(e) FALSE)
        if (has_item && !is.null(item) && !identical(item, quote(expr = ))) .walk(item)
      }
    }
    }, error = function(e) NULL)
  }

  for (top in exprs) {
    # Top-level named handler functions (e.g. handle_invoice_action <- function...)
    # -- but ONLY when they're a leaf mutating unit, not a module-server-style
    # wrapper whose body just contains observeEvent() blocks. Those get
    # walked and recorded individually below; recording the wrapper TOO
    # would be pure noise (it always "mutates" and "isn't logged" simply
    # because some child inside it does/doesn't, duplicating that child's
    # own, more specific finding).
    if (is.call(top) && identical(top[[1]], as.name("<-")) &&
        is.symbol(top[[2]]) && is.call(top[[3]]) &&
        identical(top[[3]][[1]], as.name("function"))) {
      body_txt <- tryCatch(paste(deparse(top[[3]][[3]]), collapse = "\n"), error = function(e) "")
      if (!grepl("observeEvent\\s*\\(", body_txt)) {
        .record(top, "function", as.character(top[[2]]))
      }
    }
    .walk(top)
  }

  if (!length(results)) return(data.frame())
  do.call(rbind, results)
}

# ── Part 1: verify the scanner on synthetic fixtures before trusting it ─────
{
  tmp_logged   <- tempfile(fileext = ".R")
  tmp_unlogged <- tempfile(fileext = ".R")
  writeLines(c(
    'server_fixture_logged <- function(input, output, session, shared) {',
    '  observeEvent(input$do_thing, {',
    '    save_something(x, client_id = shared$effective_client_id())',
    '    bump_sync_version("something")',
    '    log_action(user = "u", module = "m", action = "a", description = "d")',
    '  })',
    '}'
  ), tmp_logged)
  writeLines(c(
    'server_fixture_unlogged <- function(input, output, session, shared) {',
    '  observeEvent(input$do_other_thing, {',
    '    save_something_else(y, client_id = shared$effective_client_id())',
    '    bump_sync_version("something_else")',
    '  })',
    '  observeEvent(input$just_toggle_ui, {',
    '    shinyjs::toggle("panel")',
    '  })',
    '}'
  ), tmp_unlogged)
  on.exit(unlink(c(tmp_logged, tmp_unlogged)), add = TRUE)

  res_logged   <- scan_log_action_completeness(tmp_logged)
  res_unlogged <- scan_log_action_completeness(tmp_unlogged)

  ok("scanner finds the observeEvent block in the logged fixture",
     any(res_logged$kind == "observeEvent"))
  ok("scanner correctly identifies the logged fixture's handler as mutating",
     any(res_logged$mutates))
  ok("scanner correctly identifies the logged fixture's handler as ALREADY logged (no false positive)",
     all(res_logged$logs[res_logged$mutates]))

  ok("scanner finds BOTH observeEvent blocks in the unlogged fixture",
     sum(res_unlogged$kind == "observeEvent") == 2L)
  mutating_unlogged <- res_unlogged[res_unlogged$mutates, ]
  ok("scanner flags exactly 1 mutating-but-unlogged handler in the fixture (do_other_thing, not do_thing)",
     nrow(mutating_unlogged) == 1L && !mutating_unlogged$logs[1])
  ok("scanner correctly does NOT flag the UI-only toggle handler (it never mutates persisted state)",
     !any(res_unlogged$mutates[res_unlogged$label == "input$just_toggle_ui"]))

  # Dead code (if (FALSE) { ... }) still parses as valid R -- exactly the
  # CONCILIAR_REMOVED/VINCULAR_RETIRED convention this codebase already
  # uses to retire a feature while keeping its code. Confirmed live this
  # stage: the first version of this scanner flagged vin_keep_b/sug_keep_a/
  # sug_keep_b (genuinely inert, inside if(FALSE) blocks from this same
  # session's Stage 3) as real gaps -- false positives, not real work.
  tmp_dead <- tempfile(fileext = ".R")
  writeLines(c(
    'server_fixture_dead <- function(input, output, session, shared) {',
    '  if (FALSE) {',
    '  observeEvent(input$retired_action, {',
    '    save_something(x, client_id = shared$effective_client_id())',
    '  })',
    '  } # END RETIRED',
    '}'
  ), tmp_dead)
  on.exit(unlink(tmp_dead), add = TRUE)
  res_dead <- scan_log_action_completeness(tmp_dead)
  ok("scanner does NOT flag an observeEvent block sitting inside if(FALSE) -- it's inert, not a real gap",
     !any(res_dead$label == "input$retired_action"))

  # Also verify against a top-level handler FUNCTION (not observeEvent-wrapped),
  # matching handle_invoice_action()'s real shape in R/search_module.R.
  tmp_fn <- tempfile(fileext = ".R")
  writeLines(c(
    'handle_thing <- function(payload, shared) {',
    '  save_something(payload, client_id = shared$effective_client_id())',
    '}'
  ), tmp_fn)
  on.exit(unlink(tmp_fn), add = TRUE)
  res_fn <- scan_log_action_completeness(tmp_fn)
  ok("scanner also catches a top-level handler FUNCTION (not just observeEvent), matching handle_invoice_action()'s real shape",
     any(res_fn$kind == "function" & res_fn$mutates & !res_fn$logs))
}

cat("\n  Scanner verified against synthetic fixtures -- ", .pass, "passed,", .fail, "failed so far.\n\n")

# ── Part 2: run it for real, report the punch list ──────────────────────────
{
  r_files <- list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  all_results <- do.call(rbind, lapply(r_files, function(f) {
    res <- tryCatch(scan_log_action_completeness(f), error = function(e) data.frame())
    if (nrow(res)) res else NULL
  }))

  ok("the real scan across R/*.R produces results (guard against a broken scanner silently reporting nothing)",
     !is.null(all_results) && nrow(all_results) > 0)

  gaps <- all_results[all_results$mutates & !all_results$logs, ]
  gaps <- gaps[order(gaps$file, gaps$line), ]

  # Known-fine exclusions: generic, low-level persistence/engine helpers
  # called from MANY different contexts (e.g. save_manual() is called from
  # ledger_module.R, pasivos_module.R, search_module.R, and more, each with
  # its own user-facing action and its own log_action() call already at
  # THAT call site). Flagging the generic helper itself would be actively
  # wrong advice -- logging belongs where the user's intent is known, not
  # inside a shared low-level writer. This scanner can't trace call graphs
  # (it checks textual containment only), so these are confirmed by hand,
  # not detected automatically -- keep this list short and re-verify each
  # entry stays true whenever it's touched.
  KNOWN_FINE_HELPERS <- c(
    "save_manual", "save_papelera", "save_pagar_hoy", "save_grupos",
    ".save_provision_row", "fetch_all_companies",
    # .interco_v2_migrar() (CM32, R/persistence.R): load_interco_v2()'s
    # one-time v1 -> v2 migration, moved out of it (load_interco_v2 itself no
    # longer writes and left this list) -- a system-side write with no user
    # action behind it.
    ".interco_v2_migrar",
    ".save_moves_deferred", ".sync_staged", ".pasivos_perform_conversion",
    "rename_empresa_initials",
    # .rni_avisar() (CM29, R/persistence.R): bumps the sync bus for each
    # table rename_empresa_initials() changed; it saves nothing, and the
    # rename is logged by its caller (the Empresas edit handler).
    ".rni_avisar",
    # avisar_senal() (T83, R/auth.R): bumps the sync bus's signal for
    # Usuarios / Invitaciones after a save; it saves nothing, and the save is
    # logged by its caller -- same shape as .rni_avisar above.
    "avisar_senal",
    # modificar_bancos_confirmados() (2026-11-01, R/bancos_persistence.R):
    # the fresh-read-then-write wrapper around save_bancos_confirmados(); its
    # callers (the Agenda AP/AR confirm handlers) log their own action.
    "modificar_bancos_confirmados",
    # modificar_pasivos_liabilities() (T22, R/pasivos_persistence.R): the
    # fresh-read-then-write wrapper around save_pasivos_liabilities(), and
    # .pasivos_cambiar_estado() (R/pasivos_list_module.R), which changes one
    # liability's estado through it. Their callers (pasivos_cambiar_pausa,
    # pasivos_borrar, pasivos_recuperar, .pasivos_wizard_save) log their own
    # action (liability.edited / .deleted / .restored / .created).
    "modificar_pasivos_liabilities", ".pasivos_cambiar_estado",
    # modificar_forecasting_metrics() (T63, R/forecasting_persistence.R): the
    # fresh-read-then-write wrapper around the metrics table; its one caller
    # is the additive seeding in R/forecasting_metric_registry.R, a system-side
    # write with no user action behind it (same shape as .interco_v2_migrar).
    "modificar_forecasting_metrics",
    # save_pagar_hoy_with_retry() (found 2026-08-05, R/persistence.R) is a
    # generic retry wrapper around save_pagar_hoy() -- same shape as
    # .sync_staged above: called from multiple contexts (the AP and AR
    # confirm handlers in R/pagar_hoy_module.R), each of which already logs
    # with its own user-facing action (action="confirmar_pago"/
    # "confirmar_cobro" on success, "confirmar_pago_incompleto"/
    # "confirmar_cobro_incompleto" on a retry-exhausted failure -- see
    # tests/test_confirm_agenda_resilience.R). Logging belongs at those call
    # sites, not inside the generic retry mechanism itself.
    "save_pagar_hoy_with_retry",
    # generate_cashflow_html_doc's "mutation" is htmltools::save_html() writing
    # a throwaway temp file for Chrome to print, not app data -- a false
    # positive from the scanner's textual save_*( match. The real export
    # action is logged at its one call site (cashflow_export_module.R's
    # downloadHandler, action="exportar_cashflow_pdf").
    "generate_cashflow_html_doc",
    # Same false-positive shape as generate_cashflow_html_doc above (found
    # 2026-09-01): both also call htmltools::save_html() to write a
    # throwaway temp file for Chrome/pagedown to print into a PDF, not app
    # data. generate_audit_log_html_doc (R/audit_log_viewer_module.R) and
    # generate_pagos_audit_html_doc (R/pagos_audit_viewer_module.R) are the
    # Actividad/payment-audit PDF-export equivalents of the same pattern --
    # the real export action is logged at each one's own downloadHandler
    # call site, not inside the pure HTML-building function itself.
    "generate_audit_log_html_doc", "generate_pagos_audit_html_doc",
    # Generic forecasting persistence writers (forecasting_persistence.R) --
    # each caller already logs with its own user-facing context:
    #   save_forecasting_metrics/save_forecasting_subscriptions are called
    #   from forecasting_seed_if_empty (T72-C: each write is an evento_sistema("forecasting", "sembrar_catalogo"))
    #   and forecasting_module.R's sub_save/global_sub_save handlers
    #   (action="editar_suscripcion"/"crear_suscripcion_global").
    #   save_forecasting_series_observations is called from
    #   fcs_fetch_and_store (logged at ITS OWN call sites below).
    #   save_forecasting_manual_curves has no live call site yet (manual-curve
    #   editor UI is "Etapa 6.2" -- not built); log at that call site when it
    #   ships, not inside this generic writer.
    "save_forecasting_series_observations", "save_forecasting_metrics",
    "save_forecasting_subscriptions", "save_forecasting_manual_curves",
    # fcs_fetch_and_store is called once per metric from forecasting_module.R's
    # btn_refresh_all loop (up to ~13x per click) -- logging inside it would
    # emit one row per metric instead of one summary row per click. Both its
    # UI call sites (btn_refresh, btn_refresh_all) already log a single
    # summary entry (action="refrescar_metrica"/"refrescar_todas_metricas").
    "fcs_fetch_and_store",
    # modificar_forecasting_subscriptions() (T26, forecasting_persistence.R)
    # and modificar_pasivos_estimates() (T26, pasivos_persistence.R): the
    # fresh-read-then-write wrappers around save_forecasting_subscriptions()
    # and save_pasivos_estimates(). Their callers log their own action:
    # forecasting_seed_if_empty (evento_sistema, T72-C), forecasting_module.R's
    # .guardar_subs callers (sub_save, global_sub_save, plus the *_fallido
    # row on a failed write), and forecasting_set_estimate /
    # forecasting_freeze_metric / forecasting_unfreeze_metric
    # (pasivos_log_audit).
    "modificar_forecasting_subscriptions", "modificar_pasivos_estimates",
    # Found 2026-09-03: .apply_move/.clear_moves (R/ledger_module.R) no
    # longer call log_action() directly in their own body -- they now pass a
    # log_info list into .save_moves_deferred() (already a KNOWN_FINE_HELPERS
    # entry above), which logs the real outcome ("mover_fecha"/
    # "restaurar_fecha" on success, with a definitive failure path) only
    # after the retried S3 write actually settles, instead of unconditionally
    # at click time. Same silent-write-failure fix class as
    # save_pagar_hoy_with_retry above, just one call-site indirection deeper.
    ".apply_move", ".clear_moves",
    # s3_save_with_retry (R/persistence.R, added 2026-09-03) is the generic
    # retry wrapper behind that same fix for the Calendario/Search tag and
    # delete handlers -- identical shape and rationale to
    # save_pagar_hoy_with_retry: it has no idea what it's saving or how to
    # describe it, so every caller logs its own outcome
    # ("etiquetar"/"etiquetar_fallo_guardado",
    # "eliminar_facturas"/"eliminar_facturas_fallo_guardado") at its own
    # call site instead.
    "s3_save_with_retry"
  )
  gaps_actionable <- gaps[!gaps$label %in% KNOWN_FINE_HELPERS, ]

  cat("\n=== PUNCH LIST: mutating handlers with no log_action()/pasivos_log_audit() call ===\n")
  cat("Total mutating handlers found:      ", sum(all_results$mutates), "\n")
  cat("Already logged:                     ", sum(all_results$mutates & all_results$logs), "\n")
  cat("Unlogged, but a known-fine helper:  ", nrow(gaps) - nrow(gaps_actionable),
      "(logging belongs at each call site, not here -- see KNOWN_FINE_HELPERS)\n")
  cat("Unlogged and ACTIONABLE (this list):", nrow(gaps_actionable), "\n\n")
  if (nrow(gaps_actionable)) {
    for (i in seq_len(nrow(gaps_actionable))) {
      cat(sprintf("  %-45s %-15s %5s  %s\n",
                  gaps_actionable$file[i], gaps_actionable$kind[i],
                  gaps_actionable$line[i], gaps_actionable$label[i]))
    }
  }
  cat("\n")

  ok("every entry in KNOWN_FINE_HELPERS is still actually found by the scanner (guards against this exclusion list going stale -- e.g. if one of these is deleted or renamed, this catches it rather than silently hiding a typo forever)",
     all(KNOWN_FINE_HELPERS %in% gaps$label))

  # The full 55-item punch list this scanner originally produced (Tiers
  # hop_grants, every Settings sub-tab, Bancos reasignar/import, Ledger
  # stage/cart/delete/sap-edit, Pagar Hoy balances/remove/link/clear-all/
  # load-saldos, Forecasting, notes/staging/treasury-map/cashflow-export) has
  # now all been closed -- either with a real log_action() call at the
  # right choke point, or (for genuine false positives / generic multi-caller
  # helpers) an explicit, justified KNOWN_FINE_HELPERS entry. This is now a
  # real regression gate, not just a visibility report: a future mutating
  # handler that ships without logging must fail the suite, the same way any
  # other correctness regression would.
  ok("zero actionable (unlogged, non-helper) mutating handlers remain -- every gap from the original completeness sweep is either logged or a justified KNOWN_FINE_HELPERS exclusion",
     nrow(gaps_actionable) == 0L)
}

# ═════════════════════════════════════════════════════════════════════════════
# T72 (A5): the path. The scan above only counted save_*()/bump_sync_version()
# as writes and only asked whether a log call appeared ANYWHERE in the block:
# Requisiciones (.req_modificar -> modificar_tabla), Panorama (prefs_guardar)
# and every modificar_*() went unseen, and a block that logged in one branch
# but returned early in another after saving (the Pasivos wizard, 0
# provisions) passed. This part walks each handler's statements in order,
# each if-branch on its own (a function definition inside is not entered):
# after a write, a return() -- or the end of the block -- reached without a
# log call is a hole.
#   - writes: save_*, modificar_*, modificar_tabla, .req_modificar, .s3_write,
#     .s3_modificar, prefs_guardar, bump_sync_version
#   - logs:   log_action, pasivos_log_audit, .req_log
#   - base writers (their callers log; pasivos_log_audit is the logger
#     itself, its log_action goes first since A1): save_*, modificar_*, .s3_*, load_*,
#     .audit_*, .req_modificar, .req_historial, upsert_*, add_to_*, .save_*,
#     leer_*, prefs_guardar, prefs_reemplazar (T68; it logs its own change,
#     like prefs_guardar) -- not checked themselves.
# It cannot tell a write that FAILED from one that worked (an early return
# after a failed save is fine, though a *_fallido entry is better), so the
# holes found on 2026-11-20 are a frozen list, DEUDA_CAMINO: a hole not in it
# fails (new code that writes and leaves without a log), and an entry that is
# no longer a hole fails too (take it out: the list only goes down).
# ═════════════════════════════════════════════════════════════════════════════
`%||%` <- function(a, b) if (is.null(a) || !length(a) || is.na(a[1])) b else a
.ESCRITURA_T72 <- "^(save_[A-Za-z0-9_.]+|modificar_[A-Za-z0-9_.]+|modificar_tabla|\\.req_modificar|\\.s3_write|\\.s3_modificar|prefs_guardar|bump_sync_version)$"
.REGISTRO_T72  <- c("log_action", "pasivos_log_audit", ".req_log")
.BASE_T72      <- "^(save_|modificar_|\\.s3_|\\.req_modificar$|\\.req_historial$|\\.audit_|leer_|upsert_|add_to_|load_|prefs_guardar$|prefs_reemplazar$|\\.save_|pasivos_log_audit$)"
# Named writers whose ONE caller logs the action, checked by hand (keep short):
# reservar_renombre_empresa() and confirmar_renombre_empresa() (CM40,
# R/empresas_module.R) -- the Empresas rename handler logs "reservar_renombre"
# and "confirmar_renombre" right after each one succeeds.
.HELPERS_T72   <- c("reservar_renombre_empresa", "confirmar_renombre_empresa")

.t72_nombre <- function(e) {
  f <- e[[1]]
  if (is.name(f)) return(as.character(f))
  if (is.call(f) && identical(f[[1]], as.name("::")) && length(f) == 3L) return(as.character(f[[3]]))
  ""
}
.t72_contiene <- function(e, pat = NULL, nombres = NULL) {
  if (!is.call(e) || identical(e[[1]], as.name("function"))) return(FALSE)
  n <- .t72_nombre(e)
  if ((!is.null(pat) && grepl(pat, n)) || (!is.null(nombres) && n %in% nombres)) return(TRUE)
  for (i in seq_along(as.list(e))) if (is.call(e[[i]]) && .t72_contiene(e[[i]], pat, nombres)) return(TRUE)
  FALSE
}
# T72-E: a call with `evento = <a real event>` (not NULL, not
# evento_parte_de(...)) writes AND logs in one step (the write layer logs it
# once the save worked). Looked for inside function literals too: Bancos
# passes its save as function() save_...(..., evento = ev) to .bnc_guardar().
.t72_env <- new.env()   # formal: TRUE while scanning a function whose formals include `evento`
# partes: the names a scanned body binds to evento_parte_de(...) (Confirmar's
# .parte_ap <- evento_parte_de(...), T72-F): passing one is a /parte, not a log.
.t72_partes <- function(e) {
  out <- character()
  if (!is.call(e)) return(out)
  if (as.character(e[[1]])[1] %in% c("<-", "=") && length(e) == 3L && is.name(e[[2]]) && is.call(e[[3]]) &&
      identical(.t72_nombre(e[[3]]), "evento_parte_de")) out <- as.character(e[[2]])
  for (i in seq_along(as.list(e))[-1]) if (is.call(e[[i]])) out <- c(out, .t72_partes(e[[i]]))
  unique(out)
}
.t72_con_evento <- function(e) {
  if (!is.call(e)) return(FALSE)
  a <- as.list(e)
  if (!is.null(names(a)) && "evento" %in% names(a)) {
    v <- a[["evento"]]
    real <- !(is.null(v) || identical(v, quote(NULL)) ||
              (identical(v, quote(evento)) && isTRUE(.t72_env$formal)) ||   # a wrapper's pass-through of its own formal
              (is.name(v) && as.character(v) %in% .t72_env$partes) ||          # a name bound to evento_parte_de() (T72-F)
              (is.call(v) && identical(.t72_nombre(v), "evento_parte_de")))
    if (real) return(TRUE)
  }
  for (i in seq_along(a)[-1]) if (is.call(e[[i]]) && .t72_con_evento(e[[i]])) return(TRUE)
  FALSE
}
# bump_sync_version alone (a cache counter, not the user's data) after the
# action was logged does not make it "written" again.
.t72_solo_bump <- function(st) .t72_contiene(st, nombres = "bump_sync_version") &&
  !.t72_contiene(st, "^(save_[A-Za-z0-9_.]+|modificar_[A-Za-z0-9_.]+|modificar_tabla|\\.req_modificar|\\.s3_write|\\.s3_modificar|prefs_guardar)$")
.t72_bloque <- function(x) {
  if (is.call(x) && identical(x[[1]], as.name("{"))) {
    b <- as.list(x)[-1]; sr <- attr(x, "srcref"); if (!is.null(sr)) attr(b, "srcref") <- sr[-1]; b
  } else list(x)
}
# estado: "limpio" | "escrito" | "registrado" | "evento" (a write that passed a
# real event: written and logged at once, T72-E) | "fin"; huecos: lines.
.t72_camino <- function(sts, estado, ln0) {
  huecos <- integer()
  for (k in seq_along(sts)) {
    st <- sts[[k]]
    ln <- tryCatch(attr(sts, "srcref")[[k]][1], error = function(e) NULL) %||% ln0
    if (!is.call(st)) next
    n <- .t72_nombre(st)
    if (n == "{") { r <- .t72_camino(.t72_bloque(st), estado, ln); huecos <- c(huecos, r$huecos); estado <- r$estado; if (estado == "fin") break; next }
    if (n == "if") {
      if (.t72_con_evento(st[[2]])) estado <- "evento"
      else if (.t72_contiene(st[[2]], .ESCRITURA_T72)) estado <- "escrito"
      fin <- character()
      for (rama in list(st[[3]], if (length(st) >= 4) st[[4]] else NULL)) {
        if (is.null(rama)) { fin <- c(fin, estado); next }
        r <- .t72_camino(.t72_bloque(rama), estado, ln); huecos <- c(huecos, r$huecos); fin <- c(fin, r$estado)
      }
      vivos <- fin[fin != "fin"]
      estado <- if (!length(vivos)) "fin" else if (any(vivos == "escrito")) "escrito" else if (any(vivos == "registrado")) "registrado" else if (any(vivos == "evento")) "evento" else vivos[1]
      if (estado == "fin") break
      next
    }
    if (n == "return") {
      if (length(st) >= 2 && .t72_contiene(st[[2]], .ESCRITURA_T72) && !.t72_contiene(st[[2]], nombres = .REGISTRO_T72)) estado <- "escrito"
      if (estado == "escrito") huecos <- c(huecos, ln)
      estado <- "fin"; break
    }
    if (.t72_contiene(st, nombres = .REGISTRO_T72)) estado <- "registrado"
    else if (.t72_con_evento(st)) estado <- "evento"
    else if (estado == "evento" && .t72_solo_bump(st)) estado <- "evento"
    else if (.t72_contiene(st, .ESCRITURA_T72)) estado <- "escrito"
  }
  list(estado = estado, huecos = huecos)
}
.t72_cuerpo <- function(cuerpo, ln) {
  if (!.t72_contiene(cuerpo, .ESCRITURA_T72)) return(integer())
  r <- .t72_camino(.t72_bloque(cuerpo), "limpio", ln)
  c(r$huecos, if (identical(r$estado, "escrito")) -1L)        # -1: the block ends without a log
}
escanear_camino <- function(f) {
  ex <- tryCatch(parse(f, keep.source = TRUE), error = function(e) NULL)
  if (is.null(ex)) return(data.frame())
  out <- list()
  rec <- function(e, ln) {
    if (!is.call(e)) return()
    if (.t72_nombre(e) == "observeEvent" && length(e) >= 3) {
      .t72_env$formal <- FALSE   # a handler has no formals: `evento = evento` there is a local event
      .t72_env$partes <- .t72_partes(e[[3]])
      h <- .t72_cuerpo(e[[3]], ln)
      if (length(h)) out[[length(out) + 1L]] <<- data.frame(file = f, label = paste(deparse(e[[2]])[1]),
                                                            huecos = paste(h, collapse = ","), stringsAsFactors = FALSE)
    }
    for (i in seq_along(as.list(e))) if (is.call(e[[i]])) rec(e[[i]], ln)
  }
  sr <- attr(ex, "srcref")
  for (i in seq_along(ex)) {
    e <- ex[[i]]; ln <- sr[[i]][1]
    if (is.call(e) && as.character(e[[1]])[1] %in% c("<-", "=") && is.name(e[[2]]) && is.call(e[[3]]) &&
        identical(e[[3]][[1]], as.name("function"))) {
      nm <- as.character(e[[2]])
      if (!grepl(.BASE_T72, nm) && !nm %in% .HELPERS_T72 && !.t72_contiene(e[[3]][[3]], nombres = "observeEvent")) {
        .t72_env$formal <- "evento" %in% names(e[[3]][[2]])
        .t72_env$partes <- .t72_partes(e[[3]][[3]])
        h <- .t72_cuerpo(e[[3]][[3]], ln)
        if (length(h)) out[[length(out) + 1L]] <- data.frame(file = f, label = nm, huecos = paste(h, collapse = ","),
                                                             stringsAsFactors = FALSE)
      }
    }
    rec(e, ln)
  }
  if (length(out)) do.call(rbind, out) else data.frame()
}

# The scanner on synthetic handlers first.
{
  fx <- function(lineas) { f <- tempfile(fileext = ".R"); writeLines(lineas, f); r <- escanear_camino(f); unlink(f); r }
  r <- fx(c('h <- function(x) {', '  modificar_tabla("k", cid, s, function(df) df)', '  log_action(user = "u", module = "m", action = "a", description = "d")', '}'))
  ok("T72 camino: write then log -> no hole", !nrow(r))
  r <- fx(c('h <- function(x) {', '  .req_modificar("carpetas", cid, function(df) df)', '}'))
  ok("T72 camino: .req_modificar with no log -> a hole (the old scan never saw it)", nrow(r) == 1L)
  r <- fx(c('h <- function(x) {', '  prefs_guardar(cid, u, list(a = 1))', '  invisible(TRUE)', '}'))
  ok("T72 camino: prefs_guardar with no log -> a hole", nrow(r) == 1L)
  r <- fx(c('h <- function(x) {', '  save_algo(x)', '  if (!nrow(x)) {', '    showNotification("x")', '    return(list(ok = TRUE))', '  }',
            '  if (x$a) {', '    pasivos_log_audit("liability.edited")', '    return(list(ok = TRUE))', '  }', '  log_action(user = "u")', '}'))
  ok("T72 camino: the wizard's shape -- a log in one branch, an early return after the save in another -> a hole at that return",
     nrow(r) == 1L && grepl("^[0-9]+$", r$huecos) )
  r <- fx(c('h <- function(x) {', '  save_algo(x)', '  if (x$a) log_action(user = "u") else log_action(user = "v")', '}'))
  ok("T72 camino: a log in both branches -> no hole", !nrow(r))
  r <- fx(c('h <- function(x) {', '  if (x) return(NULL)', '  tryCatch({ save_algo(x); log_action(user = "u") }, error = function(e) NULL)', '}'))
  ok("T72 camino: a return before any write is fine", !nrow(r))
  r <- fx(c('h <- function(x) {', '  tryCatch({', '    g <- modificar_algo(function(f) f, client_id = cid, evento = ev)', '    showNotification("ok")', '  }, error = function(e) NULL)', '}'))
  ok("T72 camino: a write that passes evento= logs itself (the write layer, T72-B) -> no hole", !nrow(r))
  r <- fx(c('h <- function(x) {', '  modificar_algo(function(f) f, client_id = cid)', '}'))
  ok("T72 camino: the same write without evento= and no log -> a hole", nrow(r) == 1L)
  r <- fx(c('save_algo <- function(x) .s3_write(x, "k", client_id = "c")'))
  ok("T72 camino: a base writer itself is not checked (its callers log)", !nrow(r))
  # T72-E: a write that passes a real event is written AND logged at once.
  r <- fx(c('h <- function(x) {', '  ev <- list(accion = "a")',
            '  if (!guardar("a", "d", function() save_algo(x, evento = ev))) return()', '  bump_sync_version("t")', '}'))
  ok("T72-E camino: a save with evento= (even inside the function a guard runs), then bump_sync_version -> no hole", !nrow(r))
  r <- fx(c('h <- function(x) {', '  save_algo(x, evento = evento_parte_de("a"))', '  bump_sync_version("t")', '}'))
  ok("T72-E camino: evento_parte_de() is not the action's log -> a hole", nrow(r) == 1L)
  r <- fx(c('h <- function(x, evento = NULL) {', '  save_otra(x, evento = evento)', '}'))
  ok("T72-E camino: a pass-through (evento = evento) is not a log -> a hole", nrow(r) == 1L)
  r <- fx(c('h <- function(x) {', '  evento <- list(accion = "a")', '  modificar_algo(function(f) f, client_id = cid, evento = evento)', '}'))
  ok("T72-E camino: evento = evento where `evento` is a local event (not a formal) logs -> no hole (Empresas' Guardar)", !nrow(r))
  r <- fx(c('h <- function(input) {', '  observeEvent(input$b, {', '    evento <- list(accion = "a")', '    modificar_algo(function(f) f, client_id = cid, evento = evento)', '  })', '}'))
  ok("T72-E camino: the same inside a handler -> no hole", !nrow(r))
  r <- fx(c('h <- function(input) {', '  observeEvent(input$b, {', '    .p <- evento_parte_de("a")',
            '    save_algo(x, evento = .p)', '    save_otra(x, evento = .p)', '  })', '}'))
  ok("T72-F camino: a name bound to evento_parte_de() (Confirmar's .parte_ap) is not a log -> a hole", nrow(r) == 1L)
  r <- fx(c('h <- function(x) {', '  save_algo(x)', '  if (x$a) log_action(user = "u")', '  bump_sync_version("t")', '}'))
  ok("T72-E camino: a log in ONE branch, then bump_sync_version -> still a hole (only a real event survives a bump)", nrow(r) == 1L)
  lg <- function(lineas) { f <- tempfile(fileext = ".R"); writeLines(lineas, f); r <- scan_log_action_completeness(f); unlink(f); r }
  r <- lg(c('srv <- function(input, output, session) {', '  observeEvent(input$a, save_algo(1, evento = ev))',
            '  observeEvent(input$b, save_algo(1, evento = NULL))', '  observeEvent(input$c, save_algo(1, evento = evento))', '}'))
  ok("T72-E text scan: evento = <a real event> logs; evento = NULL and evento = evento do not",
     identical(r$logs[r$kind == "observeEvent"], c(TRUE, FALSE, FALSE)))
}

DEUDA_CAMINO <- c(
    "R/audit_log_viewer_module.R :: generate_audit_log_html_doc",
    "R/auth_reset.R :: .reset_modificar",
    "R/avisos.R :: avisos_agregar",
    "R/avisos.R :: avisos_retirar",
    "R/avisos.R :: avisos_marcar_hecho",
    "R/avisos_alertas_module.R :: alertas_guardar",
    "R/avisos_archivo.R :: avisos_archivar",
    "R/avisos_datos.R :: correo_semana_guardar",
    "R/avisos_datos.R :: correo_historial_guardar",
    "R/avisos_datos.R :: correo_partidas_guardar",
    "R/avisos_umbrales.R :: umbral_serie_guardar",
    "R/bancos_persistence.R :: enlazar_archivo_confirmaciones",
    "R/cashflow_export_module.R :: generate_cashflow_html_doc",
    "R/email_service.R :: .email_queue_marcar",
    "R/email_service.R :: .enqueue_email",
    "R/forecasting_persistence.R :: append_forecasting_fetch_log",
    "R/forecasting_sources.R :: fcs_fetch_and_store",
    "R/fx_banxico.R :: .fx_divisas_bajar",
    "R/fx_tipos.R :: fx_escrito_guardar",
    "R/ledger_module.R :: .sap_ov_guardar",
    "R/ledger_module.R :: .sync_staged",
    "R/notes_handlers.R :: input$note_delete_confirm",
    "R/offline_queue.R :: .queue_replay_entry",
    "R/pagar_hoy_module.R :: input[[paste0(\"do_confirm_ap_\", emp)]]",
    "R/pagar_hoy_module.R :: input[[paste0(\"do_confirm_ar_\", emp)]]",
    "R/pagar_hoy_module.R :: input$do_clear_all",
    # Entered master with the TANDA51 (T66's Saldo apertura / Bancos saldos,
    # CM34c's supplier links): a refused or failed save is told on screen but
    # not logged. Phase B's fallido events close them.
    "R/pagar_hoy_module.R :: input[[paste0(\"bal_\", emp)]]",
    "R/pagar_hoy_module.R :: input$ph_link_supplier",
    "R/pagar_hoy_module.R :: input$ph_unlink_supplier",
    "R/pagar_hoy_module.R :: input$load_bancos_saldos",
    "R/pagos_audit_viewer_module.R :: generate_pagos_audit_html_doc",
    "R/panorama_publicar_module.R :: input$publicar",
    "R/pasivos_list_module.R :: .pasivos_cambiar_estado",
    "R/pasivos_module.R :: .pasivos_perform_conversion",
    "R/pasivos_wizard_module.R :: .pasivos_wizard_save",
    "R/persistence.R :: register_username",
    "R/persistence.R :: unregister_username",
    "R/persistence.R :: .interco_v2_migrar",
    "R/persistence.R :: s3_save_with_retry",
    "R/persistence.R :: .proveedores_guardar",
    "R/persistence.R :: .rni_avisar",
    "R/auth.R :: avisar_senal",   # T83: a signal only, like .rni_avisar (no data saved, the caller logs)
    "R/persistence.R :: update_client_user_count",
    "R/persistence.R :: append_hd_notification",
    "R/persistence.R :: create_invite",
    "R/persistence.R :: purgar_invitaciones_vencidas",
    "R/persistence.R :: resolve_invite",
    "R/persistence.R :: accept_invite",
    "R/persistence.R :: add_contact",
    "R/persistence.R :: deactivate_contact",
    "R/requisiciones_persistence.R :: requisicion_editar",
    "R/requisiciones_persistence.R :: requisiciones_marcar_leido",
    "R/sap_api.R :: fetch_all_companies",
    "R/search_module.R :: handle_invoice_action",
    "R/settings/settings_cuentas.R :: input$banco_save",
    "R/settings/settings_cuentas.R :: input$cuenta_save",
    "R/settings/settings_cuentas.R :: input$cuenta_delete",
    "R/settings/settings_helpers.R :: borrar_banco_si_libre",
    "R/settings/settings_hub.R :: input$save_interco_v2",
    "R/settings/settings_hub.R :: input$ic_scan_apply",
    "R/settings/settings_hub.R :: input$bp_apply_ic_btn",
    "R/settings/settings_sincro.R :: input$do_sincro_activar",
    "R/settings/settings_sincro.R :: input$do_sincro_desactivar",
    "R/sync_bus.R :: bump_sync_version",
    "R/tiers_module.R :: input$btn_submit_client_req",
    "R/tiers_module.R :: input$btn_approve_req",
    "R/tiers_module.R :: input$btn_reject_req",
    "R/tiers_module.R :: input$btn_confirm_limit_change",
    "R/tiers_module.R :: invitaciones_t$revocar()",
    "R/tiers_module.R :: input$mark_notif_read",
    "R/tiers_module.R :: input$btn_mark_all_read",
    "R/treasury_map_module.R :: input$send_to_agenda",
    "R/usuarios_cuentas.R :: reservar_usuario",
    "R/usuarios_cuentas.R :: .tomar_invitacion",
    "R/usuarios_cuentas.R :: .devolver_invitacion"
)

{
  r_files <- list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  cam <- do.call(rbind, lapply(r_files, escanear_camino))
  hoy <- if (nrow(cam)) unique(paste0(cam$file, " :: ", cam$label)) else character()
  nuevos <- setdiff(hoy, DEUDA_CAMINO)
  viejos <- setdiff(DEUDA_CAMINO, hoy)
  if (length(nuevos)) cat("  NEW holes (write, then leave without a log):\n", paste0("    ", nuevos, "\n"), sep = "")
  if (length(viejos)) cat("  no longer holes (take them out of DEUDA_CAMINO):\n", paste0("    ", viejos, "\n"), sep = "")
  ok(sprintf("T72 camino: no new handler writes and leaves without a log (%d new)", length(nuevos)), !length(nuevos))
  ok(sprintf("T72 camino: DEUDA_CAMINO only goes down -- every entry is still a hole (%d to take out)", length(viejos)), !length(viejos))
  ok("T72 camino: the real scan found what it should (guard against a broken scanner)", length(hoy) > 0L)
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0) stop("Tests FAILED.")
invisible(NULL)
