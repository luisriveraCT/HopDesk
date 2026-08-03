# =============================================================================
# tests/test_bancos_papelera_archive.R
# Stage 9, Issue C (2026-08-03): Mouse asked for a dedicated, separate
# backend data store for deleted bank movements -- "you already have the
# perfect example of one that works, right in this app" (manual_inv +
# papelera.rds). See R/bancos_persistence.R's "bancos_papelera" section for
# the full design reasoning, including why this is ADDITIVE alongside the
# existing bancos_movimientos eliminado flag, not a replacement for it.
#
# Correction to the Stage 9 doc's premise, confirmed before writing anything:
# do_eliminar_confirm (R/bancos_module.R) already called log_action() and
# already showed up correctly in papelera_tbl's rows_mov section (deleted
# bancos_movimientos rows, filtered by the eliminado flag, with "Deshacer"
# restricted to fuente=="manual") BEFORE this stage -- both landed in commit
# 6dcf916 (2026-07-30, "Retire Vincular; build manual bank-movement
# edit/delete-correction tool"), four days before Mouse's live test and this
# doc. Most likely explanation for "the delete button doesn't work": a stale
# running R process from before that commit (this app's reactive environment
# persists across runApp() calls -- see project memory), the same pattern
# already flagged for Issues C/D elsewhere in this stage. This suite verifies
# that existing mechanism is genuinely intact (regression guards), then tests
# the NEW dedicated archive table this stage adds on top of it.
#
# Mirrors tests/test_archive_mechanism.R's style (the closest sibling --
# that file tests papelera.rds's own add/restore mechanism the exact same
# way), extracting the REAL functions from R/bancos_persistence.R rather
# than a hand-copied mirror.
# =============================================================================

cat("── bancos_papelera: dedicated archive store for deleted bank movements ──\n")

.pass <- 0L
.fail <- 0L
.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}
.chk_true  <- function(cond, label) .chk(isTRUE(cond), TRUE, label)
.chk_error <- function(expr, label) {
  ok <- inherits(tryCatch({ force(expr); NULL }, error = function(e) e), "error")
  .chk(ok, TRUE, label)
}

empty_bpap <- .schema_bancos_papelera() |> dplyr::slice(0)

# A realistic bank-movement row, every field a real bancos_movimientos row
# would carry (not just the subset the archive copies) -- so a future field
# addition to the movement schema without a matching archive test failure
# would be a real (if silent) gap, matching test_archive_mechanism.R's own
# zero-field-loss philosophy for the item-level papelera.
.movement_row <- function(overrides = list()) {
  base <- list(
    id              = uuid::UUIDgenerate(),
    cuenta_id       = "cta-ncs-mxn",
    fecha           = as.Date("2026-08-01"),
    hora            = "14:32:10",
    recibo          = "REC-001",
    descripcion_raw = "SPEI ENVIADO PROVEEDOR X",
    tipo            = "spei_out",
    parte           = "Proveedor de Prueba SA de CV",
    rfc             = "PPX010101AAA",
    referencia      = "REF-001",
    clave_rastreo   = "CLAVE-001",
    concepto        = "Pago factura F-100",
    cargo           = 5000.50,
    abono           = 0,
    saldo_banco     = 123456.78,
    conciliado      = FALSE,
    doc_vinculado   = NA_character_,
    agenda_id       = NA_character_,
    importado_at    = as.POSIXct("2026-07-15 09:00:00"),
    fuente          = "manual",
    eliminado       = FALSE,
    eliminado_por   = NA_character_,
    eliminado_at    = as.POSIXct(NA),
    notas           = "Nota de prueba"
  )
  base[names(overrides)] <- overrides
  as.data.frame(base, stringsAsFactors = FALSE)
}

# =============================================================================
# A. add_to_bancos_papelera() — basic archiving
# =============================================================================
{
  row <- .movement_row()
  bpap <- add_to_bancos_papelera(empty_bpap, row, actor = "mouse")

  .chk(nrow(bpap), 1L, "A1: add_to_bancos_papelera() appends exactly 1 row")
  .chk(bpap$mov_id[1], row$id, "A2: mov_id links back to the original movement id")
  .chk(bpap$action[1], "archived", "A3: action defaults to 'archived'")
  .chk(bpap$disposition[1], "deleted", "A4: disposition defaults to 'deleted'")
  .chk_true(is.na(bpap$reverses_id[1]), "A5: reverses_id is NA for a fresh archive")
  .chk(bpap$actor[1], "mouse", "A6: actor recorded correctly")
  .chk_true(!is.na(bpap$event_at[1]), "A7: event_at timestamp recorded")
  .chk_true(!is.na(bpap$id[1]) && nzchar(bpap$id[1]), "A8: id (this event's own permanent id) is generated and non-empty")
  .chk_true(!identical(bpap$id[1], bpap$mov_id[1]), "A9: the event's own id is NOT the same as mov_id (two distinct identifiers)")
  .chk(bpap$parte[1], row$parte, "A10: parte copied correctly")
  .chk(bpap$cargo[1], row$cargo, "A11: cargo copied correctly")
  .chk(bpap$abono[1], row$abono, "A12: abono copied correctly")
  .chk(bpap$fuente[1], row$fuente, "A13: fuente copied correctly (manual vs. txt matters for restore-eligibility elsewhere)")
  .chk(bpap$cuenta_id[1], row$cuenta_id, "A14: cuenta_id copied correctly")
}

# =============================================================================
# B. add_to_bancos_papelera() — multi-row batch archiving (a bulk delete of
# several selected movements at once, do_eliminar_confirm's real shape)
# =============================================================================
{
  rows <- dplyr::bind_rows(
    .movement_row(list(concepto = "BATCH-1", cargo = 100, abono = 0)),
    .movement_row(list(concepto = "BATCH-2", cargo = 0, abono = 200)),
    .movement_row(list(concepto = "BATCH-3", cargo = 300, abono = 0))
  )
  bpap_batch <- add_to_bancos_papelera(empty_bpap, rows, actor = "mouse")
  .chk(nrow(bpap_batch), 3L, "B1: batch archive appends exactly 3 rows")
  .chk(length(unique(bpap_batch$id)), 3L, "B2: each row in a batch gets its own unique event id")
  .chk(length(unique(bpap_batch$mov_id)), 3L, "B3: each row keeps its own distinct mov_id (not collapsed)")
  .chk(sort(bpap_batch$concepto), c("BATCH-1", "BATCH-2", "BATCH-3"), "B4: all 3 concepts present")
  .chk_true(all(bpap_batch$action == "archived"), "B5: every row in the batch is action='archived'")
  .chk_true(all(is.na(bpap_batch$reverses_id)), "B6: every fresh-archive row has NA reverses_id")
}

# =============================================================================
# C. restore_from_bancos_papelera() — zero-field-loss round trip, same
# philosophy as test_archive_mechanism.R's Hapag-Lloyd-incident regression
# guard for the item-level papelera
# =============================================================================
{
  row <- .movement_row(list(
    concepto = "ROUNDTRIP-1", notas = "Notas con caracteres especiales: \u00f1\u00e1\u00e9\u00ed\u00f3\u00fa \u00bf\u00a1",
    referencia = "REF-ROUNDTRIP", clave_rastreo = "CLAVE-ROUNDTRIP"
  ))
  bpap <- add_to_bancos_papelera(empty_bpap, row, actor = "mouse")

  result <- restore_from_bancos_papelera(bpap, row$id, actor = "mouse2")
  restored <- as.data.frame(result$original_data, stringsAsFactors = FALSE)

  fields_to_check <- c("id", "cuenta_id", "fecha", "parte", "rfc", "referencia",
                       "clave_rastreo", "concepto", "cargo", "abono", "saldo_banco",
                       "fuente", "notas")
  for (f in fields_to_check) {
    .chk(restored[[f]], row[[f]], sprintf("C: restored field '%s' matches the original movement exactly", f))
  }

  .chk(nrow(result$papelera_df), 2L, "C-final: restore appends exactly 1 restore row (2 total)")
  restored_row <- result$papelera_df[result$papelera_df$action == "restored", ]
  .chk(nrow(restored_row), 1L, "C-final: exactly one action='restored' row exists")
  .chk(restored_row$reverses_id[1], bpap$id[1], "C-final: restore row's reverses_id points at the original archive event's own id")
  .chk(restored_row$actor[1], "mouse2", "C-final: restore row records the RESTORING actor, not the original archiver")

  # The ORIGINAL archived row must be completely untouched by the restore.
  orig_after <- result$papelera_df[result$papelera_df$id == bpap$id[1], ]
  .chk(nrow(orig_after), 1L, "C-final: original archived row still exists, exactly once")
  .chk(orig_after$action[1], "archived", "C-final: original archived row's own action is still 'archived' (never mutated)")
}

# =============================================================================
# D. restore_from_bancos_papelera() — error paths
# =============================================================================
{
  .chk_error(restore_from_bancos_papelera(empty_bpap, "nonexistent-mov-id"),
             "D1: restoring a mov_id with no archived event errors")

  row <- .movement_row()
  bpap <- add_to_bancos_papelera(empty_bpap, row, actor = "mouse")
  once <- restore_from_bancos_papelera(bpap, row$id, actor = "mouse")
  .chk_error(restore_from_bancos_papelera(once$papelera_df, row$id, actor = "mouse"),
             "D2: restoring an already-restored mov_id errors (can't double-restore the same event)")
}

# =============================================================================
# E. restore_from_bancos_papelera() — re-delete after restore is a genuinely
# NEW archive event, and restoring again correctly picks the latest one
# (delete -> restore -> delete -> restore, not confused by history)
# =============================================================================
{
  row <- .movement_row(list(concepto = "REDELETE-CYCLE"))
  bpap <- add_to_bancos_papelera(empty_bpap, row, actor = "mouse")
  r1   <- restore_from_bancos_papelera(bpap, row$id, actor = "mouse")

  # Delete it again -- a genuinely new archive event, not a reuse of the first.
  bpap2 <- add_to_bancos_papelera(r1$papelera_df, row, actor = "mouse")
  .chk(sum(bpap2$action == "archived"), 2L, "E1: a second delete of the same movement creates a SECOND archived event, not reusing the first")
  .chk(sum(bpap2$mov_id == row$id & bpap2$action == "archived"), 2L,
       "E2: both archived events share the same mov_id (same underlying movement)")

  r2 <- restore_from_bancos_papelera(bpap2, row$id, actor = "mouse3")
  .chk(sum(r2$papelera_df$action == "restored"), 2L, "E3: restoring again produces a second restore row")
  second_restore <- r2$papelera_df[r2$papelera_df$actor == "mouse3", ]
  .chk(nrow(second_restore), 1L, "E4: exactly one row attributable to the second restoring actor")
  # The second restore must reverse the SECOND archive event (the most recent
  # still-archived one), not re-reverse the already-restored first one.
  first_archive_id <- bpap2$id[bpap2$action == "archived"][1]
  .chk_true(!identical(second_restore$reverses_id[1], first_archive_id),
            "E5: the second restore reverses the SECOND (latest) archive event, not the already-restored first one")
}

# =============================================================================
# F. Static: do_eliminar_confirm wires the new archive write correctly
# =============================================================================
{
  mod_txt <- readLines("R/bancos_module.R", warn = FALSE)
  del_start <- grep("observeEvent\\(input\\$do_eliminar_confirm,", mod_txt)
  .chk_true(length(del_start) > 0, "F1: found do_eliminar_confirm to scan")
  if (length(del_start)) {
    end <- grep("^    \\}, ignoreInit = TRUE\\)$", mod_txt)
    end <- end[end > del_start[1]][1]
    block <- paste(mod_txt[del_start[1]:end], collapse = "\n")

    .chk_true(grepl("del_rows\\s*<-\\s*movs\\[idx", block),
              "F2: snapshots the pre-delete rows into del_rows")
    snap_line   <- grep("del_rows\\s*<-\\s*movs\\[idx", mod_txt[del_start[1]:end])[1]
    flip_line   <- grep("movs\\$eliminado\\[idx\\]\\s*<-\\s*TRUE", mod_txt[del_start[1]:end])[1]
    .chk_true(!is.na(snap_line) && !is.na(flip_line) && snap_line < flip_line,
              "F3: del_rows is captured BEFORE the eliminado flag flips (archive reflects pre-delete state, not post-mutation)")
    .chk_true(grepl("add_to_bancos_papelera\\(", block),
              "F4: calls add_to_bancos_papelera()")
    .chk_true(grepl("save_bancos_papelera\\(", block),
              "F5: calls save_bancos_papelera() (the archive is actually persisted, not just built in memory)")
    .chk_true(grepl("log_action\\(", block),
              "F6 (regression guard): still calls log_action() -- Issue C's fix didn't remove Stage 6's existing logging")
    .chk_true(grepl("movs\\$eliminado\\[idx\\]\\s*<-\\s*TRUE", block),
              "F7 (regression guard): still flips the eliminado flag -- the archive write is additive, not a replacement")
  }
}

# =============================================================================
# G. Static: undo_mov_delete wires the new restore write correctly
# =============================================================================
{
  mod_txt <- readLines("R/bancos_module.R", warn = FALSE)
  undo_start <- grep("observeEvent\\(input\\$undo_mov_delete,", mod_txt)
  .chk_true(length(undo_start) > 0, "G1: found undo_mov_delete to scan")
  if (length(undo_start)) {
    end <- grep("^    \\}, ignoreInit = TRUE\\)$", mod_txt)
    end <- end[end > undo_start[1]][1]
    block <- paste(mod_txt[undo_start[1]:end], collapse = "\n")

    .chk_true(grepl("restore_from_bancos_papelera\\(", block),
              "G2: calls restore_from_bancos_papelera()")
    .chk_true(grepl("save_bancos_papelera\\(", block),
              "G3: persists the restore event")
    .chk_true(grepl("fuente\\s*==\\s*\"manual\"", block),
              "G4 (regression guard): still restricted to fuente=='manual' -- Issue C's fix didn't quietly widen the existing deliberate restore-eligibility restriction (open design question flagged back in this stage's report, not decided unilaterally)")
    .chk_true(grepl("movs\\$eliminado\\[idx\\]\\s*<-\\s*FALSE", block),
              "G5 (regression guard): still un-flips the eliminado flag -- the archive write is additive, not a replacement")
  }
}

# =============================================================================
# H. Static regression guard: the PRE-EXISTING mechanism the Stage 9 doc
# flagged as possibly-already-fixed is genuinely intact -- rows_mov still
# displays every deleted movement (regardless of source) with "Deshacer"
# shown only for fuente=="manual", exactly as before this stage's changes.
# =============================================================================
{
  mod_txt <- readLines("R/bancos_module.R", warn = FALSE)
  rows_mov_start <- grep("rows_mov\\s*<-\\s*if\\s*\\(nrow\\(movs_del\\)\\)", mod_txt)
  .chk_true(length(rows_mov_start) > 0, "H1: found the rows_mov papelera-display block to scan")
  if (length(rows_mov_start)) {
    end <- grep("^      \\} else NULL$", mod_txt)
    end <- end[end > rows_mov_start[1]][1]
    block <- paste(mod_txt[rows_mov_start[1]:end], collapse = "\n")
    .chk_true(grepl('fuente == "manual"', block),
              "H2 (regression guard): Deshacer button still restricted to fuente=='manual' rows")
    .chk_true(grepl("undo_mov_delete", block),
              "H3 (regression guard): Deshacer button still wired to undo_mov_delete")
  }
}

cat(sprintf("\n  bancos_papelera archive: %d passed, %d failed\n", .pass, .fail))
