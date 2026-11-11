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
# T12b removed the retired Vincular and Conciliar code, kept inert in
# if (FALSE) blocks until then (it is in git history).
{
  ok("the toolbar no longer renders an ACTIVE tags$button for btn_vincular (only inside comments)",
     {
       active_lines <- txt[!grepl("^\\s*#", txt)]
       !any(grepl('id = ns\\("btn_vincular"\\)', active_lines))
     })
  ok("no if (FALSE) block is left in R/bancos_module.R (T12b: the retired code is gone)",
     !any(grepl("if \\(FALSE\\) \\{", txt)))
  ok("none of the retired Vincular / Conciliar handlers or state is left",
     !any(grepl("vin_keep_|sug_keep_|\\.do_vinculation|\\.do_vin_keep_a|vincular_item_rv|sug_state_rv|\\.compute_sug_pairs", txt)))
  # T12a: the Libro's selection JS went to the table engine, and the retired
  # Vincular / Conciliar JS (kept in /* */ until then) went with it.
  ok("the Libro's JS keeps no Vincular mode at all (T12a: the retired JS is gone, not just commented)",
     {
       i <- grep("^bancos_libro_js <- function", txt)[1]
       j <- grep("^# ── Match scoring", txt)[1]
       !is.na(i) && !is.na(j) && !any(grepl("vinMode|deactivateVin|vincular_row_id", txt[i:j]))
     })
  ok("R/bancos_module.R still parses as valid R after the removal (a misplaced brace would corrupt everything after it)",
     !inherits(tryCatch(parse("R/bancos_module.R"), error = function(e) e), "error"))
  ok("the Manual entry modal and observeEvent(input$add_mov_manual, ...) are still there",
     length(grep("Manual entry modal", txt)) >= 1L && length(grep("observeEvent\\(input\\$add_mov_manual,", txt)) == 1L)
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
       grepl('bump_sync_version\\("bancos_movimientos_db"[,)]', block))   # CM16: now with client_id
  }
}

# ── Stage 4b: Edit capability ────────────────────────────────────────────────
{
  # T12a: Editar is the row's ✎ (hd_table acc_fila), shown only on manual
  # entries (.acciones); the toolbar button and its alert() went.
  ok("Editar is the Libro's ✎ row action (acciones_fila = \"editar\")",
     any(grepl('acciones_fila = "editar"', txt, fixed = TRUE)))
  ok("the ✎ shows only on manual entries (.acciones), to who can change Bancos",
     any(grepl('.acciones   = ifelse(!is.na(movs$fuente) & movs$fuente == "manual" & .bnc_puede_ver_escritura(), "editar", "")', txt, fixed = TRUE)))
  ok("the table's ✎ calls the edit function with the row's id",
     any(grepl('identical(a$accion, "editar") && length(a$ids)) .bnc_editar(a$ids[1])', txt, fixed = TRUE)))
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

  start <- grep("\\.bnc_editar <- function\\(mov_id\\)", txt)
  ok("found the edit function (.bnc_editar) to scan", length(start) > 0)
  if (length(start)) {
    end <- grep("^    \\}$", txt)
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
     grepl("tablas.bancos.aviso.ya_no_existe", block, fixed = TRUE))   # "El movimiento ya no existe ..." (bancos-avisos)
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
  # Since T12b the Papelera is hd_table "papelera": a deleted movement is a
  # "mov:" row whose ↶ (.acciones "deshacer") calls the handler with its id.
  ok("the Papelera table has the deshacer row action",
     any(grepl('hd_table_server\\("papelera"', txt)) && any(grepl('acciones_fila = "deshacer"', txt)))
  ok("a deleted movement's ↶ only shows for fuente==\"manual\" rows",
     any(grepl('ifelse\\(movs_del\\$fuente %in% "manual" & escribe, "deshacer", ""\\)', txt)))
  ok("the ↶ calls the undo handler with the row's real id (not a placeholder)",
     any(grepl('startsWith\\(id, "mov:"\\)\\) \\.bnc_deshacer_eliminacion\\(sub\\("\\^mov:", "", id\\)\\)', txt)))

  # The handler is a function of the row's id since T12b (the Papelera's
  # engine table calls it with its id).
  start <- grep("\\.bnc_deshacer_eliminacion <- function\\(mov_id\\)", txt)
  ok("found the undo_mov_delete observer to scan", length(start) > 0)
  if (length(start)) {
    end <- grep("^    \\}$", txt)
    end <- end[end > start[1]][1]
    block <- paste(txt[start[1]:end], collapse = "\n")
    ok("undo_mov_delete only restores rows matching fuente==\"manual\" (never a TXT-imported row, even if somehow requested)",
       grepl('movs\\$fuente == "manual"', block))
    ok("undo_mov_delete flips eliminado back to FALSE (the actual restore)",
       grepl("movs\\$eliminado\\[idx\\]\\s*<-\\s*FALSE", block))
    ok("undo_mov_delete clears eliminado_at (consistent restored-row state, not a half-restored row)",
       grepl("movs\\$eliminado_at\\[idx\\]\\s*<-\\s*as\\.POSIXct\\(NA\\)", block))
    ok("undo_mov_delete calls bump_sync_version (so the restore propagates live, consistent with Stage 1/2 of this effort)",
       grepl('bump_sync_version\\("bancos_movimientos_db"[,)]', block))   # CM16: now with client_id
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
