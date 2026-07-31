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
  LOG_PAT    <- "\\blog_action\\s*\\(|\\bpasivos_log_audit\\s*\\("

  .record <- function(call_expr, kind, label) {
    sref <- attr(call_expr, "srcref")
    txt  <- if (!is.null(sref)) paste(as.character(sref), collapse = "\n")
            else paste(deparse(call_expr), collapse = "\n")
    mutates <- grepl(MUTATE_PAT, txt)
    logs    <- grepl(LOG_PAT, txt)
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
    "load_interco_v2", ".save_provision_row", "fetch_all_companies",
    ".save_moves_deferred", ".sync_staged", ".pasivos_perform_conversion",
    "rename_empresa_initials",
    # generate_cashflow_html_doc's "mutation" is htmltools::save_html() writing
    # a throwaway temp file for Chrome to print, not app data -- a false
    # positive from the scanner's textual save_*( match. The real export
    # action is logged at its one call site (cashflow_export_module.R's
    # downloadHandler, action="exportar_cashflow_pdf").
    "generate_cashflow_html_doc",
    # Generic forecasting persistence writers (forecasting_persistence.R) --
    # each caller already logs with its own user-facing context:
    #   save_forecasting_metrics/save_forecasting_subscriptions are called
    #   from forecasting_seed_if_empty (action="seed_catalog", user="system")
    #   and forecasting_module.R's sub_save/global_sub_save handlers
    #   (action="editar_suscripcion"/"crear_suscripcion_global").
    #   save_forecasting_series_observations is called from
    #   forecasting_set_estimate (action="set_estimate"/"congelar_metrica")
    #   and fcs_fetch_and_store (logged at ITS OWN call sites below).
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
    "fcs_fetch_and_store"
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

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0) stop("Tests FAILED.")
invisible(NULL)
