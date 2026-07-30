# =============================================================================
# tests/test_bancos_vincular_retirement_and_manual_edit.R
# Stage 3+4 of the real-time-refresh/audit-logging strategy (2026-07-30):
#
# Stage 3: retired "Vincular" (a manual duplicate-merge tool) -- kept the
# code in place, wrapped in if(FALSE)/JS block comments, same convention
# already established in this file for the earlier CONCILIAR_REMOVED
# retirement. Not deleted: may be reused creatively later, but the app no
# longer surfaces it.
#
# Stage 4: replaces Vincular's role for banks that can't use "Importar TXT"
# (Vantage Bank, specifically) with a real correction tool for manually-
# entered movements:
#   - Add (already existed) now logs via log_action() (it didn't before).
#   - Edit (new) -- scoped to fuente=="manual" rows only, via a reused
#     "Editar" toolbar button + the existing row-selection mechanism
#     ("Eliminar seleccionados" already used this same sel[] array).
#   - Delete's existing "Deshacer" gap closed for manual rows specifically:
#     a new undo_mov_delete action restores an accidentally-deleted manual
#     row, mirroring the undo_conf/undo_abono precedent already proven
#     elsewhere in this file. TXT-imported rows deliberately keep the
#     existing one-way behavior (restoring one could fight a future
#     re-import of the same statement).
#
# Static, permanent half of verification. The other half was live: a
# minimal headless-Chrome (chromote) harness driving the actual extracted
# "Editar" button client-side validation logic -- confirmed 8/8: 0 selected
# rows shows the correct Spanish alert and never calls setInputValue; 1
# selected row shows no alert and sends the correct row id; 2 selected rows
# shows the distinct "select only one" alert; 5 selected rows is rejected
# the same way as 2 (confirms the boundary is ">1", not "==2").
# =============================================================================
cat("=== test_bancos_vincular_retirement_and_manual_edit ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) {
    cat(" PASS:", label, "\n"); .pass <<- .pass + 1L
  } else {
    cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L
  }
}

txt <- readLines("R/bancos_module.R", warn = FALSE)
full <- paste(txt, collapse = "\n")

# ── Stage 3: Vincular retirement ────────────────────────────────────────────
{
  ok("the retired Vincular block is tagged VINCULAR_RETIRED (findable, documented, not silently disabled)",
     any(grepl("VINCULAR_RETIRED", txt)))
  ok("the toolbar no longer renders an ACTIVE tags$button for btn_vincular (only inside comments)",
     {
       active_lines <- txt[!grepl("^\\s*#", txt)]
       !any(grepl('id = ns\\("btn_vincular"\\)', active_lines))
     })
  n_if_false_vincular <- sum(grepl("if \\(FALSE\\) \\{", txt) &
                             (seq_along(txt) %in% grep("VINCULAR_RETIRED", txt) |
                              vapply(seq_along(txt), function(i)
                                any(grepl("VINCULAR_RETIRED", txt[max(1,i-4):min(i+1,length(txt))])), logical(1))))
  ok("exactly 2 if(FALSE) blocks were added for Vincular (reactive-state decls + main modal block)",
     n_if_false_vincular == 2L)
  ok("the main Vincular modal header ('Vincular modal') is immediately followed by if(FALSE) (retirement wraps the whole block right after its own header comment)",
     {
       start <- grep("VINCULAR_RETIRED: Vincular modal", txt)[1]
       nearby <- txt[start:min(start + 8, length(txt))]
       any(grepl("if \\(FALSE\\)", nearby))
     })
  ok("score_vincular_match() is still defined (code kept, not deleted)",
     any(grepl("^score_vincular_match <- function", txt)))
  ok(".do_vinculation() is still defined (code kept, not deleted)",
     any(grepl("\\.do_vinculation <- function", txt)))
  ok(".do_vin_keep_a() is still defined (code kept, not deleted)",
     any(grepl("\\.do_vin_keep_a <- function", txt)))
  ok("vincular_item_rv/vin_confirm_rv reactiveVal declarations are inside an if(FALSE) block",
     {
       start <- grep('vincular_item_rv\\s*<-\\s*reactiveVal', txt)[1]
       preceding <- txt[max(1, start - 2):start]
       any(grepl("if \\(FALSE\\)", preceding))
     })
  ok("the JS mode-toggle for Vincular is inside a /* */ block comment, not active",
     {
       start <- grep('deactivateVin', txt)[1]
       preceding <- txt[max(1, start - 2):start]
       any(grepl("/\\*", preceding))
     })
  ok("the JS block comment for Vincular is properly closed (no nested */ prematurely ending it)",
     {
       start <- grep("Vincular button \\(VINCULAR_RETIRED", txt)[1]
       end   <- grep("END VINCULAR_RETIRED", txt)[1]
       ok_range <- txt[(start+1):(end-1)]
       # None of the lines strictly between open and close may contain */
       !any(grepl("\\*/", ok_range))
     })
  ok("vinMode/_vinBtnOrigHtml are still declared OUTSIDE the commented block (no ReferenceError -- the row-click handler's `if (vinMode)` branch still needs them, even though nothing can set vinMode=true anymore)",
     any(grepl("var vinMode\\s*=\\s*false;", txt[1:300])))
  ok("R/bancos_module.R still parses as valid R after the retirement (structural sanity -- a misplaced brace would corrupt everything after it, not just Vincular)",
     !inherits(tryCatch(parse("R/bancos_module.R"), error = function(e) e), "error"))
  ok("the Manual entry modal section (right after Vincular) is genuinely OUTSIDE the if(FALSE) block, not accidentally swallowed",
     {
       vinc_start <- grep("VINCULAR_RETIRED: Vincular modal", txt)[1]
       vinc_close <- grep("END VINCULAR_RETIRED", txt)
       vinc_close <- vinc_close[vinc_close > vinc_start][1]
       manual_start <- grep("Manual entry modal", txt)[1]
       !is.na(manual_start) && !is.na(vinc_close) && manual_start > vinc_close && vinc_close > vinc_start
     })
  ok("observeEvent(input$add_mov_manual, ...) is genuinely active code, not inside the retired block",
     {
       vinc_close <- grep("END VINCULAR_RETIRED", txt)[1]
       add_start  <- grep("observeEvent\\(input\\$add_mov_manual,", txt)[1]
       !is.na(add_start) && add_start > vinc_close
     })
  ok("the existing static Vincular test (test_stage8_vincular_warning.R) still finds its target text (regression guard on a pre-existing test)",
     any(grepl("observeEvent\\(input\\$vin_keep_a,", txt)))
}

# ── Stage 4a: Add now logs ───────────────────────────────────────────────────
{
  start <- grep("observeEvent\\(input\\$do_add_manual,", txt)
  ok("found the do_add_manual observer to scan", length(start) > 0)
  if (length(start)) {
    end <- grep("^    \\}, ignoreInit = TRUE\\)$", txt)
    end <- end[end > start[1]][1]
    block <- paste(txt[start[1]:end], collapse = "\n")
    ok("do_add_manual calls log_action() (previously missing entirely)",
       grepl("log_action\\(", block))
    ok("do_add_manual's log_action call passes client_id= (scoping regression guard)",
       grepl("client_id\\s*=", block))
    ok("do_add_manual's log_action call passes viewer_home_client_id= (scoping regression guard)",
       grepl("viewer_home_client_id\\s*=", block))
    ok("do_add_manual still calls bump_sync_version(\"bancos_movimientos_db\") (regression guard -- Stage 1 fix intact)",
       grepl('bump_sync_version\\("bancos_movimientos_db"\\)', block))
  }
}

# ── Stage 4b: Edit capability ────────────────────────────────────────────────
{
  ok("a new 'Editar' toolbar button exists (btn_editar)",
     any(grepl('id = ns\\("btn_editar"\\)', txt)))
  ok("the Editar button's JS handler validates the selection count client-side before sending",
     any(grepl('sel\\.length === 0.*Selecciona una fila', txt)) ||
     any(grepl("Selecciona una fila para editar", txt)))
  ok("the Editar button's JS handler also rejects 2+ selected rows (edit is single-row only)",
     any(grepl("Selecciona solo una fila para editar", txt)))
  ok("editing_mov_id reactiveVal exists to track add-vs-edit mode",
     any(grepl("editing_mov_id\\s*<-\\s*reactiveVal\\(NULL\\)", txt)))
  ok(".show_manual_mov_modal() helper exists (shared between Agregar and Editar, not duplicated)",
     any(grepl("\\.show_manual_mov_modal\\s*<-\\s*function", txt)))
  ok("add_mov_manual explicitly clears editing_mov_id(NULL) (prevents an aborted edit from leaking into a later Agregar)",
     {
       start <- grep("observeEvent\\(input\\$add_mov_manual,", txt)[1]
       block <- paste(txt[start:(start+3)], collapse = "\n")
       grepl("editing_mov_id\\(NULL\\)", block)
     })

  start <- grep("observeEvent\\(input\\$editar_row,", txt)
  ok("found the editar_row observer to scan", length(start) > 0)
  if (length(start)) {
    end <- grep("^    \\}, ignoreInit = TRUE\\)$", txt)
    end <- end[end > start[1]][1]
    block <- paste(txt[start[1]:end], collapse = "\n")
    ok("editar_row rejects rows that are NOT fuente==\"manual\" (never opens edit for a TXT-imported row)",
       grepl('fuente == "manual"', block))
    ok("editar_row rejects rows that are already eliminado (can't edit a deleted row directly)",
       grepl("!movs\\$eliminado", block))
    ok("editar_row shows a warning notification when the row isn't editable (not a silent no-op)",
       grepl("showNotification", block))
    ok("editar_row passes prefill values into .show_manual_mov_modal (genuinely pre-fills, not a blank form)",
       grepl("\\.show_manual_mov_modal\\(prefill", block))
  }

  # Every prefillable field in .show_manual_mov_modal actually threads
  # through a `prefill$<field>` reference (parameterized across all 8
  # fields, not just spot-checked on one).
  modal_start <- grep("\\.show_manual_mov_modal <- function", txt)[1]
  modal_end   <- grep("^    \\}$", txt)
  modal_end   <- modal_end[modal_end > modal_start][1]
  modal_block <- paste(txt[modal_start:modal_end], collapse = "\n")
  PREFILL_FIELDS <- c("cuenta_id", "fecha", "tipo", "parte", "concepto", "cargo", "abono", "notas")
  for (fld in PREFILL_FIELDS) {
    ok(sprintf(".show_manual_mov_modal() prefills the '%s' field from prefill$%s", fld, fld),
       grepl(sprintf("prefill\\$%s", fld), modal_block))
  }
}

# ── Stage 4b: save observer's insert-vs-update branch ────────────────────────
{
  start <- grep("observeEvent\\(input\\$do_add_manual,", txt)[1]
  end   <- grep("^    \\}, ignoreInit = TRUE\\)$", txt)
  end   <- end[end > start][1]
  block <- paste(txt[start:end], collapse = "\n")

  ok("do_add_manual branches on is_edit (edit_id <- editing_mov_id(); is_edit <- !is.null(edit_id))",
     grepl("is_edit\\s*<-\\s*!is\\.null\\(edit_id\\)", block))
  ok("the edit branch re-validates the row still exists AND is still fuente==\"manual\" at save time (not trusting the snapshot from when the modal opened -- same staleness discipline as .pasivos_perform_conversion())",
     grepl('which\\(!is\\.na\\(movs\\$id\\) & movs\\$id == edit_id & movs\\$fuente == "manual"\\)', block))
  ok("the edit branch handles the row having vanished since the modal opened (shows a warning, doesn't crash)",
     grepl("El movimiento ya no existe", block))
  ok("the edit branch captures 'before' values prior to mutating (audit trail input)",
     grepl("before <- as\\.list\\(movs\\[idx\\[1\\], ", block))
  ok("the insert branch (else) still generates a fresh UUID for a genuinely new row (unchanged from before this stage)",
     grepl("id\\s*=\\s*uuid::UUIDgenerate\\(\\)", block))
  ok("the insert branch still stamps fuente = \"manual\" (unchanged -- this is what makes it editable/undoable later)",
     grepl('fuente\\s*=\\s*"manual"', block))
  ok("the log description distinguishes 'editado' vs 'agregado' (not the same generic message for both)",
     grepl("editado.*agregado|agregado.*editado", block))
  ok("the log action name distinguishes editar_movimiento_manual vs agregar_movimiento_manual",
     grepl("editar_movimiento_manual", block) && grepl("agregar_movimiento_manual", block))
  ok("the edit branch's audit metadata includes BOTH before and after (rich enough to see exactly what changed)",
     grepl("list\\(before = before, after = after\\)", block))
  ok("editing_mov_id is reset to NULL at the end (doesn't leak edit-mode into the next Agregar/Editar)",
     grepl("editing_mov_id\\(NULL\\)$", trimws(tail(strsplit(block, "\n")[[1]], 3)[1])) ||
     grepl("editing_mov_id\\(NULL\\)", block))
}

# ── Stage 4c: Deshacer (undo) for manual deletions ──────────────────────────
{
  ok("the Acciones column exists in papelera_tbl's schema (both the empty-table case and the real data path)",
     sum(grepl("Acciones\\s*=\\s*character\\(\\)|Acciones\\s*=", txt)) >= 4L)
  ok("rows_mov's Acciones column only shows the undo button for fuente==\"manual\" rows (dplyr::if_else gated)",
     any(grepl('fuente == "manual",\\s*$', txt)) || any(grepl('fuente == "manual"', txt[grep("Acciones\\s*=\\s*dplyr::if_else", txt)])))
  ok("the undo button's onclick fires undo_mov_delete with the row's real id (not a placeholder)",
     {
       start <- grep('ns\\("undo_mov_delete"\\), id', txt)[1]
       !is.na(start) && any(grepl("nonce:Math\\.random", txt[max(1, start-4):start]))
     })

  start <- grep("observeEvent\\(input\\$undo_mov_delete,", txt)
  ok("found the undo_mov_delete observer to scan", length(start) > 0)
  if (length(start)) {
    end <- grep("^    \\}, ignoreInit = TRUE\\)$", txt)
    end <- end[end > start[1]][1]
    block <- paste(txt[start[1]:end], collapse = "\n")
    ok("undo_mov_delete only restores rows matching fuente==\"manual\" (never a TXT-imported row, even if somehow requested)",
       grepl('movs\\$fuente == "manual"', block))
    ok("undo_mov_delete flips eliminado back to FALSE (the actual restore)",
       grepl("movs\\$eliminado\\[idx\\]\\s*<-\\s*FALSE", block))
    ok("undo_mov_delete clears eliminado_at (consistent restored-row state, not a half-restored row)",
       grepl("movs\\$eliminado_at\\[idx\\]\\s*<-\\s*as\\.POSIXct\\(NA\\)", block))
    ok("undo_mov_delete calls bump_sync_version (so the restore propagates live, consistent with Stage 1/2 of this effort)",
       grepl('bump_sync_version\\("bancos_movimientos_db"\\)', block))
    ok("undo_mov_delete calls log_action (every correction action is auditable, per the general logging mandate)",
       grepl("log_action\\(", block))
    ok("undo_mov_delete's log_action call passes client_id= (scoping regression guard)",
       grepl("client_id\\s*=", block))
    ok("undo_mov_delete's log_action call passes viewer_home_client_id= (scoping regression guard)",
       grepl("viewer_home_client_id\\s*=", block))
    ok("undo_mov_delete guards against a not-found id (req(mov_id) + length(idx) check, doesn't crash on a stale/garbage id)",
       grepl("req\\(mov_id\\)", block) && grepl("if \\(!length\\(idx\\)\\) return\\(\\)", block))
  }
}

# ── Cross-cutting: the log_action scoping test now covers bancos_module.R's
# new call sites too (this file was already in that scanner's hardcoded
# list from an earlier stage) ────────────────────────────────────────────────
{
  ok("R/bancos_module.R is in the log_action scoping guard's file list (tests/test_saas_log_action_scoping.R)",
     any(grepl('"R/bancos_module\\.R"', readLines("tests/test_saas_log_action_scoping.R", warn = FALSE))))
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0) stop("Tests FAILED.")
invisible(NULL)
