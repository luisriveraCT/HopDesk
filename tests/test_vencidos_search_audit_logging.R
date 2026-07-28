# =============================================================================
# tests/test_vencidos_search_audit_logging.R
# Live gap found 2026-07-29: deleting a manual invoice from Vencidos left no
# entry in Gestión de Usuarios > Actividad at all. Root cause:
# handle_invoice_action() (R/search_module.R) -- the single shared handler
# behind every Vencidos and Search edit action (tag/move/restore/delete/
# stage) -- never called log_action() for any of them, unlike
# R/ledger_module.R's own move/restore/delete handlers, which already did.
#
# Fix mirrors Calendario exactly for eliminar_facturas/mover_fecha/
# restaurar_fecha via a small per-ledger helper, .log_ledger_action(), since
# a single Vencidos/Search action can span both AR and AP at once
# (Calendario's own handlers never could).
#
# Follow-up (same day): tagging (Urgente/Importante/Ambas/Quitar etiqueta)
# had NO log_action() call anywhere -- not in Calendario's day-view modal
# (R/ledger_module.R's .handle_tags_once(), used identically in and out of
# "Modo auditoría" since it's the same handler/table selection either way)
# nor in the Vencidos/Search shared handler. Added a matching "etiquetar"
# log_action() call in both places, with an identical .tag_label_es() text
# helper in both files so the same tag action logs the same Spanish
# description regardless of which UI surface it came from.
#
# stage_all/stage_selected remain deliberately NOT logged, because
# Calendario's own equivalent stage-to-agenda handlers don't log those
# either.
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

# ── Mirror of .tag_label_es(), kept identical by hand in both R/ledger_
# module.R and R/search_module.R -- see those files for the real copies.
tag_label_es <- function(new_tags) {
  if (!length(new_tags)) "sin etiqueta"
  else if (length(new_tags) == 2L) "urgente e importante"
  else if (identical(new_tags, "urgent")) "urgente"
  else "importante"
}

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

# ── 3. tag_label_es() covers all four tag actions correctly ────────────────
{
  ok("tag_urgent -> 'urgente'", tag_label_es("urgent") == "urgente")
  ok("tag_important -> 'importante'", tag_label_es("important") == "importante")
  ok("tag_both -> 'urgente e importante'", tag_label_es(c("urgent","important")) == "urgente e importante")
  ok("tag_clear -> 'sin etiqueta'", tag_label_es(character(0)) == "sin etiqueta")
}

# ── 4. A tag action logs one entry per ledger, same shape as delete/move ───
{
  calls <- list()
  mock_log <- function(...) calls[[length(calls) + 1L]] <<- list(...)
  log_ledger_action <- make_log_ledger_action("larivera", mock_shared, mock_log)

  keys_df <- data.frame(ledger = c("AR","AR"), Documento = c("R1","R2"),
                        stringsAsFactors = FALSE)
  new_tags <- c("urgent", "important")
  log_ledger_action(keys_df, "etiquetar", function(sub)
    sprintf("%d factura(s) etiquetada(s) como %s", nrow(sub), tag_label_es(new_tags)))

  ok("tag action produces exactly 1 log_action call for a single-ledger selection",
     length(calls) == 1L)
  ok("action is etiquetar", calls[[1]]$action == "etiquetar")
  ok("description reflects the 'both' tag combination", grepl("urgente e importante", calls[[1]]$description))
}

# ── 5. Static scan: R/ledger_module.R's .handle_tags_once() logs too ───────
# (single handler, fires identically in and out of "Modo auditoría")
{
  txt   <- readLines("R/ledger_module.R", warn = FALSE)
  start <- grep("\\.handle_tags_once <- function", txt)
  ok("found .handle_tags_once() to scan", length(start) > 0)
  if (length(start)) {
    end   <- grep("^    observeEvent\\(input\\$tag_urgent,", txt)
    end   <- end[end > start[1]][1]
    block <- paste(txt[start[1]:end], collapse = "\n")
    ok("ledger_module.R defines the .tag_label_es helper",
       grepl("\\.tag_label_es\\s*<-\\s*function", paste(txt, collapse = "\n")))
    ok(".handle_tags_once() calls log_action with action = \"etiquetar\"",
       grepl('action\\s*=\\s*"etiquetar"', block))
    ok(".handle_tags_once() logs against the SAME rows that were just tagged (nrow(rows))",
       grepl("nrow\\(rows\\).*\\.tag_label_es\\(new_tags\\)", block))
  }
}

# ── 6. Static scan: the real handler in R/search_module.R wires all three ──
{
  txt   <- readLines("R/search_module.R", warn = FALSE)
  block <- paste(txt, collapse = "\n")

  ok("handle_invoice_action defines the .log_ledger_action helper",
     grepl("\\.log_ledger_action\\s*<-\\s*function", block))
  ok("handle_invoice_action defines the .tag_label_es helper",
     grepl("\\.tag_label_es\\s*<-\\s*function", block))

  tag_start <- grep('if \\(action %in% c\\("tag_urgent","tag_important","tag_both","tag_clear"\\)\\)', txt)
  tag_end   <- grep('} else if \\(action == "move"\\)', txt)
  ok("found the tag branch to scan", length(tag_start) > 0 && length(tag_end) > 0)
  if (length(tag_start) && length(tag_end)) {
    tag_block <- paste(txt[tag_start[1]:tag_end[1]], collapse = "\n")
    ok("the tag branch calls .log_ledger_action(..., \"etiquetar\", ...)",
       grepl('\\.log_ledger_action\\(keys_df,\\s*"etiquetar"', tag_block))
  }

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
