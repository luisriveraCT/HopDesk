# =============================================================================
# tests/test_vencidos_search_audit_logging.R
# Live gap found 2026-07-29: deleting a manual invoice from Vencidos left no
# entry in Gestión de Usuarios > Actividad at all. Root cause:
# handle_invoice_action() (R/search_module.R) -- the single shared handler
# behind every Vencidos and Search edit action (tag/move/restore/delete/
# stage) -- never called log_action() for any of them, unlike
# R/ledger_module.R's own move/restore/delete handlers, which already did.
#
# Fix mirrors Calendario exactly, nothing more: log_action() is now called
# for the same three actions Calendario logs (eliminar_facturas,
# mover_fecha, restaurar_fecha) via a small per-ledger helper,
# .log_ledger_action(), since a single Vencidos/Search action can span both
# AR and AP at once (Calendario's own handlers never could). Tag actions and
# stage_all/stage_selected are deliberately NOT logged here either, because
# Calendario's own handlers don't log those -- this is a like-for-like
# replication, not a broader audit-coverage change.
# =============================================================================
cat("=== test_vencidos_search_audit_logging ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) {
    cat(" PASS:", label, "\n"); .pass <<- .pass + 1L
  } else {
    cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L
  }
}

# ── Mirror of the .log_ledger_action() helper added to handle_invoice_action ─
# (kept in sync by hand, same convention as this suite's other tests for
# logic nested inside a larger function -- see test_cart_inv_sel.R)
make_log_ledger_action <- function(current_user, shared, log_action_fn) {
  function(keys_df, action, describe) {
    cid      <- tryCatch(shared$effective_client_id(), error = function(e) NULL)
    home_cid <- tryCatch(shared$home_client_id(),      error = function(e) NULL)
    for (lg in unique(keys_df$ledger)) {
      sub <- keys_df[keys_df$ledger == lg, , drop = FALSE]
      tryCatch(
        log_action_fn(
          user        = current_user,
          module      = paste0("ledger_", lg),
          action      = action,
          description = describe(sub),
          target_id   = paste(sub[["Documento"]][seq_len(min(5, nrow(sub)))], collapse = ", "),
          metadata    = list(n = nrow(sub)),
          client_id             = cid,
          viewer_home_client_id = home_cid
        ),
        error = function(e) message("log_action failed: ", e$message)
      )
    }
  }
}

mock_shared <- list(
  effective_client_id = function() "networks",
  home_client_id       = function() NULL
)

# ── 1. Single-ledger delete logs exactly one entry with the right shape ────
{
  calls <- list()
  mock_log <- function(...) calls[[length(calls) + 1L]] <<- list(...)
  log_ledger_action <- make_log_ledger_action("larivera", mock_shared, mock_log)

  keys_df <- data.frame(ledger = c("AP","AP"), Documento = c("D1","D2"),
                        stringsAsFactors = FALSE)
  log_ledger_action(keys_df, "eliminar_facturas", function(sub)
    sprintf("%d factura(s) enviada(s) a papelera", nrow(sub)))

  ok("single-ledger delete produces exactly 1 log_action call", length(calls) == 1L)
  ok("module is ledger_AP, matching Calendario's own naming", calls[[1]]$module == "ledger_AP")
  ok("action is eliminar_facturas, matching Calendario exactly", calls[[1]]$action == "eliminar_facturas")
  ok("description mentions the correct count", grepl("^2 factura", calls[[1]]$description))
  ok("target_id lists the affected documents", calls[[1]]$target_id == "D1, D2")
  ok("metadata$n matches the row count", calls[[1]]$metadata$n == 2L)
  ok("user is passed through", calls[[1]]$user == "larivera")
}

# ── 2. Mixed AR+AP selection logs ONE entry per ledger, not one combined ────
# (Vencidos/Search can select across both ledgers at once; Calendario's own
# handlers never could, since they always act within a single ledger.)
{
  calls <- list()
  mock_log <- function(...) calls[[length(calls) + 1L]] <<- list(...)
  log_ledger_action <- make_log_ledger_action("larivera", mock_shared, mock_log)

  keys_df <- data.frame(ledger = c("AR","AP","AP"), Documento = c("R1","P1","P2"),
                        stringsAsFactors = FALSE)
  log_ledger_action(keys_df, "mover_fecha", function(sub)
    sprintf("%d factura(s) movida(s) a 01/08/2026", nrow(sub)))

  ok("mixed AR+AP selection produces exactly 2 log_action calls (one per ledger)",
     length(calls) == 2L)
  modules <- vapply(calls, function(c) c$module, character(1))
  ok("one call is for ledger_AR", "ledger_AR" %in% modules)
  ok("one call is for ledger_AP", "ledger_AP" %in% modules)
  ar_call <- calls[[which(modules == "ledger_AR")]]
  ap_call <- calls[[which(modules == "ledger_AP")]]
  ok("the AR call's count reflects only the 1 AR row, not all 3", ar_call$metadata$n == 1L)
  ok("the AP call's count reflects only the 2 AP rows, not all 3", ap_call$metadata$n == 2L)
}

# ── 3. Static scan: the real handler in R/search_module.R wires all three ──
{
  txt   <- readLines("R/search_module.R", warn = FALSE)
  block <- paste(txt, collapse = "\n")

  ok("handle_invoice_action defines the .log_ledger_action helper",
     grepl("\\.log_ledger_action\\s*<-\\s*function", block))

  start <- grep('} else if \\(action == "move"\\)', txt)
  end   <- grep('} else if \\(action == "restore"\\)', txt)
  ok("found the move branch to scan", length(start) > 0 && length(end) > 0)
  if (length(start) && length(end)) {
    move_block <- paste(txt[start[1]:end[1]], collapse = "\n")
    ok("the move branch calls .log_ledger_action(..., \"mover_fecha\", ...)",
       grepl('\\.log_ledger_action\\(keys_df,\\s*"mover_fecha"', move_block))
  }

  start2 <- grep('} else if \\(action == "restore"\\)', txt)
  end2   <- grep('} else if \\(action == "delete"\\)', txt)
  ok("found the restore branch to scan", length(start2) > 0 && length(end2) > 0)
  if (length(start2) && length(end2)) {
    restore_block <- paste(txt[start2[1]:end2[1]], collapse = "\n")
    ok("the restore branch calls .log_ledger_action(..., \"restaurar_fecha\", ...)",
       grepl('\\.log_ledger_action\\(keys_df,\\s*"restaurar_fecha"', restore_block))
  }

  start3 <- grep('} else if \\(action == "delete"\\)', txt)
  end3   <- grep('} else if \\(action %in% c\\("stage_all", "stage_selected"\\)\\)', txt)
  ok("found the delete branch to scan", length(start3) > 0 && length(end3) > 0)
  if (length(start3) && length(end3)) {
    delete_block <- paste(txt[start3[1]:end3[1]], collapse = "\n")
    ok("the delete branch calls .log_ledger_action(..., \"eliminar_facturas\", ...)",
       grepl('\\.log_ledger_action\\(keys_df,\\s*"eliminar_facturas"', delete_block))
    ok("the delete branch's log call covers the FULL selection (keys_df), not just item_rows",
       grepl('\\.log_ledger_action\\(keys_df,', delete_block) &&
       !grepl('\\.log_ledger_action\\(item_rows,', delete_block))
  }
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0) stop("Tests FAILED.")
invisible(NULL)
