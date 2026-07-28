# =============================================================================
# tests/test_archive_mechanism.R
# Stage 3 of the ledger-integrity master plan (docs/LEDGER_INTEGRITY_MASTER_PLAN.md):
# a permanent, append-only event log for both dispositions ("deleted" and
# "confirmed") of a manual_inv row, generalizing papelera's existing
# archive-forever mechanism — plus the parallel upgrade to bancos_confirmados
# so recovering a confirmation writes a new permanent row instead of only
# flipping a flag in place. Nothing calls these functions from production
# code yet (that's Stages 4-5) — this is the mechanism itself, tested in
# isolation, thoroughly, before anything is wired into it.
#
# Mouse's explicit requirements this suite is built against:
#   - Both histories are permanent in S3 forever (never physically deleted).
#   - Recovering something writes its OWN new line, not a flag flip.
#   - Every recovery carries a permanent link to exactly which prior event
#     it reverses, so later audits can trace the full chain.
#   - Zero field loss on restore (the root cause of the Hapag Lloyd incident).
# =============================================================================

cat("── Stage 3: permanent archive/event-log mechanism ──────────────────────\n")

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

empty_papelera <- .schema_papelera() |> dplyr::slice(0)
empty_conf     <- .schema_bancos_confirmados() |> dplyr::slice(0)

# A realistic full manual-invoice row, with every field a real manual_inv row
# would carry -- not just the subset papelera's own display columns cover.
# This is exactly the shape that must round-trip with ZERO loss.
.manual_invoice_row <- function(overrides = list()) {
  base <- list(
    id                       = uuid::UUIDgenerate(),
    ledger                   = "AP",
    source                   = "manual",
    Empresa                  = "Networks & Logistics",
    Moneda                   = "MXN",
    Documento                = "TEST-001",
    Factura                  = "FAC-9988",
    Parte                    = "Proveedor de Prueba SA de CV",
    Codigo                   = "P0001",
    Importe                  = 12345.67,
    `Abono futuro`           = 500,
    `Fecha de vencimiento`   = as.Date("2026-08-15"),
    FechaEff                 = as.Date("2026-08-15"),
    Notas                    = "Nota original con ñ y acentos éíóú",
    created_by               = "mouse",
    created_at               = as.POSIXct("2026-07-01 10:00:00"),
    updated_at               = as.POSIXct("2026-07-02 11:30:00"),
    provision_id             = NA_character_,
    liability_id             = NA_character_,
    referencia               = NA_character_
  )
  base[names(overrides)] <- overrides
  as.data.frame(base, stringsAsFactors = FALSE)
}

# =============================================================================
# A. add_to_papelera() — basic archiving, both dispositions
# =============================================================================
{
  row <- .manual_invoice_row()

  pap_del <- add_to_papelera(empty_papelera, row, ledger = "AP", deleted_by = "mouse")
  .chk(nrow(pap_del), 1L, "A1: add_to_papelera() default call appends exactly 1 row")
  .chk(pap_del$disposition[1], "deleted", "A2: default disposition is 'deleted' (backward compatible)")
  .chk(pap_del$action[1], "archived", "A3: default action is 'archived'")
  .chk_true(!is.na(pap_del$event_id[1]) && nzchar(pap_del$event_id[1]), "A4: event_id is generated and non-empty")
  .chk_true(is.na(pap_del$reverses_event_id[1]), "A5: reverses_event_id is NA for a fresh archive")
  .chk(pap_del$deleted_by[1], "mouse", "A6: deleted_by (actor) recorded correctly")
  .chk_true(!is.na(pap_del$deleted_at[1]), "A7: deleted_at timestamp recorded")

  pap_conf <- add_to_papelera(empty_papelera, row, ledger = "AP", deleted_by = "mouse", disposition = "confirmed")
  .chk(pap_conf$disposition[1], "confirmed", "A8: explicit disposition='confirmed' is honored")
  .chk(pap_conf$action[1], "archived", "A9: action is still 'archived' regardless of disposition")

  ar_row <- .manual_invoice_row(list(ledger = "AR", Empresa = "Networks Crossdocking Services"))
  pap_ar <- add_to_papelera(empty_papelera, ar_row, ledger = "AR", deleted_by = "mouse")
  .chk(pap_ar$ledger[1], "AR", "A10: AR ledger recorded correctly, independent of AP")
}

# =============================================================================
# B. add_to_papelera() — multi-row batch archiving
# =============================================================================
{
  rows <- dplyr::bind_rows(
    .manual_invoice_row(list(Documento = "BATCH-1", Importe = 100)),
    .manual_invoice_row(list(Documento = "BATCH-2", Importe = 200)),
    .manual_invoice_row(list(Documento = "BATCH-3", Importe = 300))
  )
  pap_batch <- add_to_papelera(empty_papelera, rows, ledger = "AP", deleted_by = "mouse")
  .chk(nrow(pap_batch), 3L, "B1: batch archive appends exactly 3 rows")
  .chk(length(unique(pap_batch$event_id)), 3L, "B2: each row in a batch gets its own unique event_id")
  .chk(sort(pap_batch$Documento), c("BATCH-1", "BATCH-2", "BATCH-3"), "B3: all 3 documentos present")
  .chk_true(all(pap_batch$action == "archived"), "B4: every row in the batch is action='archived'")
  .chk_true(all(is.na(pap_batch$reverses_event_id)), "B5: every fresh-archive row has NA reverses_event_id")
}

# =============================================================================
# C. restore_from_papelera() — zero-field-loss round trip (the core
#    regression test for the Hapag Lloyd FechaVenc-substitution incident)
# =============================================================================
{
  row <- .manual_invoice_row(list(
    Documento = "ROUNDTRIP-1", Notas = "Notas con caracteres especiales: ñáéíóú ¿¡",
    provision_id = "prov-abc-123", liability_id = "liab-xyz-789", referencia = "REF-001"
  ))
  pap <- add_to_papelera(empty_papelera, row, ledger = "AP", deleted_by = "mouse", disposition = "confirmed")
  event_id <- pap$event_id[1]

  result <- restore_from_papelera(pap, event_id, actor = "mouse2")
  restored <- as.data.frame(result$restored_data, stringsAsFactors = FALSE)

  # Field-by-field zero-loss check across EVERY field a real manual invoice carries.
  fields_to_check <- c("id", "ledger", "Empresa", "Moneda", "Documento", "Factura",
                       "Parte", "Codigo", "Importe", "Abono futuro",
                       "Fecha de vencimiento", "Notas", "created_by", "created_at",
                       "updated_at", "provision_id", "liability_id", "referencia")
  for (f in fields_to_check) {
    .chk(restored[[f]], row[[f]], sprintf("C: restored field '%s' matches original exactly", f))
  }

  .chk(nrow(result$papelera_df), 2L, "C-final: restore appends exactly 1 recovery row (2 total)")
  recov_row <- result$papelera_df[result$papelera_df$action == "recovered", ]
  .chk(nrow(recov_row), 1L, "C-final: exactly one action='recovered' row exists")
  .chk(recov_row$reverses_event_id[1], event_id, "C-final: recovery row's reverses_event_id points at the original archive event")
  .chk(recov_row$deleted_by[1], "mouse2", "C-final: recovery row records the RECOVERING actor, not the original archiver")

  # The ORIGINAL archived row must be completely untouched by the recovery.
  orig_after <- result$papelera_df[result$papelera_df$event_id == event_id, ]
  .chk(nrow(orig_after), 1L, "C-final: original archived row still exists, exactly once")
  .chk(orig_after$action[1], "archived", "C-final: original archived row's own action is still 'archived' (never mutated)")
  .chk(orig_after$disposition[1], "confirmed", "C-final: original archived row's disposition is untouched")
}

# =============================================================================
# D. restore_from_papelera() — error conditions
# =============================================================================
{
  row <- .manual_invoice_row(list(Documento = "ERR-1"))
  pap <- add_to_papelera(empty_papelera, row, ledger = "AP", deleted_by = "mouse")
  event_id <- pap$event_id[1]

  .chk_error(restore_from_papelera(pap, "nonexistent-event-id", actor = "mouse"),
             "D1: recovering a nonexistent event_id errors")

  once <- restore_from_papelera(pap, event_id, actor = "mouse")
  .chk_error(restore_from_papelera(once$papelera_df, event_id, actor = "mouse"),
             "D2: recovering an already-recovered event_id errors (no silent double-processing)")

  recov_event_id <- once$papelera_df[once$papelera_df$action == "recovered", "event_id"][[1]]
  .chk_error(restore_from_papelera(once$papelera_df, recov_event_id, actor = "mouse"),
             "D3: attempting to 'recover' a recovery row itself errors (only archived events are recoverable)")
}

# =============================================================================
# E. Multiple archive/recover cycles for the "same" real-world invoice —
#    each cycle is an independent event with its own event_id (the same
#    business invoice can legitimately be archived and recovered more than
#    once over its lifetime), and the chain stays traceable throughout.
# =============================================================================
{
  key <- list(Documento = "CYCLE-1", Empresa = "Networks & Logistics", Moneda = "MXN")
  pap <- empty_papelera

  event_ids <- character(0)
  for (cycle in 1:3) {
    row <- .manual_invoice_row(c(key, list(Importe = cycle * 111)))
    pap <- add_to_papelera(pap, row, ledger = "AP", deleted_by = "mouse", disposition = "confirmed")
    this_event <- pap$event_id[pap$action == "archived" & pap$Documento == "CYCLE-1"]
    this_event <- setdiff(this_event, event_ids)
    .chk(length(this_event), 1L, sprintf("E%d: cycle %d produces exactly one new archive event", cycle, cycle))
    event_ids <- c(event_ids, this_event)
    res <- restore_from_papelera(pap, this_event, actor = paste0("mouse-cycle-", cycle))
    pap <- res$papelera_df
  }

  .chk(length(unique(event_ids)), 3L, "E-final: all 3 archive cycles produced 3 distinct event_ids")
  archived_rows  <- pap[pap$Documento == "CYCLE-1" & pap$action == "archived", ]
  recovered_rows <- pap[pap$Documento == "CYCLE-1" & pap$action == "recovered", ]
  .chk(nrow(archived_rows), 3L, "E-final: 3 archived events total for this business key")
  .chk(nrow(recovered_rows), 3L, "E-final: 3 recovery events total for this business key")
  .chk(sort(recovered_rows$reverses_event_id), sort(event_ids),
       "E-final: the 3 recovery events' reverses_event_id exactly cover the 3 archive events, one each")
  # "How many times was this recovered" -- answerable by a plain filter+count,
  # no new schema field needed beyond what's already here.
  .chk(sum(pap$Documento == "CYCLE-1" & pap$action == "recovered"), 3L,
       "E-final: recovery COUNT for this business key is queryable directly (3)")
}

# =============================================================================
# F. Backward compatibility — old-shape data (written before Stage 3, with
#    none of the 4 new columns) must still normalize and behave correctly.
# =============================================================================
{
  old_shape <- data.frame(
    id = "old-1", ledger = "AP", source = "manual", Empresa = "Old Co",
    Moneda = "MXN", Documento = "OLD-DOC", Parte = "Old Parte", Importe = 50,
    FechaEff = as.Date("2026-01-01"), deleted_by = "past_user",
    deleted_at = as.POSIXct("2026-01-01 09:00:00"),
    stringsAsFactors = FALSE
  )
  old_shape$original_data <- list(as.list(old_shape[1, c("id","Empresa","Documento","Importe")]))

  normalized <- .normalize(old_shape, .schema_papelera)
  .chk(nrow(normalized), 1L, "F1: old-shape row survives .normalize() without dropping")
  .chk_true(is.na(normalized$disposition[1]), "F2: old-shape row's disposition normalizes to NA (not a crash, not a wrong default)")
  .chk_true(is.na(normalized$action[1]), "F3: old-shape row's action normalizes to NA")
  .chk_true(is.na(normalized$event_id[1]), "F4: old-shape row's event_id normalizes to NA")
  .chk(normalized$deleted_by[1], "past_user", "F5: old-shape row's pre-existing fields are untouched by normalization")

  # A caller written before Stage 3 (no disposition= argument at all) must
  # still get IDENTICAL behavior to before -- this is the actual backward-
  # compatibility contract, not just "doesn't crash."
  row <- .manual_invoice_row(list(Documento = "LEGACY-CALLER"))
  legacy_call <- add_to_papelera(empty_papelera, row, ledger = "AP", deleted_by = "legacy")
  .chk(legacy_call$disposition[1], "deleted", "F6: a pre-Stage-3-style call (no disposition arg) still defaults to 'deleted'")
}

# =============================================================================
# G. recover_confirmacion() — basic behavior and field preservation
# =============================================================================
{
  conf <- tibble::tibble(
    confirmacion_id = uuid::UUIDgenerate(), agenda_item_id = uuid::UUIDgenerate(),
    empresa = "Networks & Logistics", parte = "prueba", documento = "prueba",
    codigo = "P0001", importe = 0.03, moneda = "MXN",
    cuenta_id = uuid::UUIDgenerate(), fecha = as.Date("2026-07-23"),
    tipo = "pago", mov_id = NA_character_, confirmado_at = Sys.time(),
    eliminado = FALSE, eliminado_at = as.POSIXct(NA), provision_id = NA_character_
  )
  conf <- .normalize(conf, .schema_bancos_confirmados)
  orig_id <- conf$confirmacion_id[1]

  result <- recover_confirmacion(conf, orig_id, actor = "mouse")
  .chk(nrow(result), 2L, "G1: recover_confirmacion() appends exactly 1 row (2 total)")

  orig_after <- result[result$confirmacion_id == orig_id, ]
  .chk(orig_after$eliminado[1], TRUE, "G2: original row's eliminado flips to TRUE (existing Historial filter still works)")
  .chk_true(!is.na(orig_after$eliminado_at[1]), "G3: original row's eliminado_at is set")
  .chk(orig_after$importe[1], 0.03, "G4: original row's own fields (importe) are otherwise untouched")
  .chk(orig_after$parte[1], "prueba", "G5: original row's own fields (parte) are otherwise untouched")

  recov <- result[result$action == "recovered" & !is.na(result$action), ]
  .chk(nrow(recov), 1L, "G6: exactly one action='recovered' row exists")
  .chk(recov$reverses_confirmacion_id[1], orig_id, "G7: recovery row's reverses_confirmacion_id points at the original")
  .chk_true(!is.na(recov$recovered_at[1]), "G8: recovery row's recovered_at timestamp is set")
  .chk(recov$eliminado[1], FALSE, "G9: the recovery row ITSELF is never eliminado")
  .chk(recov$empresa[1], "Networks & Logistics", "G10: recovery row copies empresa for a self-contained display record")
  .chk(recov$parte[1], "prueba", "G11: recovery row copies parte for a self-contained display record")
  .chk(recov$importe[1], 0.03, "G12: recovery row copies importe for a self-contained display record")
  .chk_true(recov$confirmacion_id[1] != orig_id, "G13: recovery row has its OWN fresh confirmacion_id, not reused")
}

# =============================================================================
# H. recover_confirmacion() — error conditions and repeated cycles
# =============================================================================
{
  conf <- tibble::tibble(
    confirmacion_id = "conf-h1", agenda_item_id = NA_character_,
    empresa = "E", parte = "P", documento = "D", codigo = NA_character_,
    importe = 1, moneda = "MXN", cuenta_id = NA_character_, fecha = as.Date("2026-01-01"),
    tipo = "pago", mov_id = NA_character_, confirmado_at = Sys.time(),
    eliminado = FALSE, eliminado_at = as.POSIXct(NA), provision_id = NA_character_
  )
  conf <- .normalize(conf, .schema_bancos_confirmados)

  .chk_error(recover_confirmacion(conf, "nonexistent-id", actor = "mouse"),
             "H1: recovering a nonexistent confirmacion_id errors")

  once <- recover_confirmacion(conf, "conf-h1", actor = "mouse")
  .chk_error(recover_confirmacion(once, "conf-h1", actor = "mouse"),
             "H2: recovering an already-recovered confirmacion_id errors")

  # Re-confirm the same business invoice (a fresh confirmacion_id, as the
  # real confirm handler always generates), then recover THAT one too --
  # the two recovery events must not be conflated.
  reconf <- tibble::tibble(
    confirmacion_id = "conf-h2", agenda_item_id = NA_character_,
    empresa = "E", parte = "P", documento = "D", codigo = NA_character_,
    importe = 1, moneda = "MXN", cuenta_id = NA_character_, fecha = as.Date("2026-01-02"),
    tipo = "pago", mov_id = NA_character_, confirmado_at = Sys.time(),
    eliminado = FALSE, eliminado_at = as.POSIXct(NA), provision_id = NA_character_
  )
  reconf <- .normalize(reconf, .schema_bancos_confirmados)
  combined <- dplyr::bind_rows(once, reconf)
  twice <- recover_confirmacion(combined, "conf-h2", actor = "mouse")

  recov_rows <- twice[twice$action == "recovered" & !is.na(twice$action), ]
  .chk(nrow(recov_rows), 2L, "H3: two independent confirm-then-recover cycles produce 2 distinct recovery rows")
  .chk(sort(recov_rows$reverses_confirmacion_id), sort(c("conf-h1", "conf-h2")),
       "H4: the 2 recovery rows correctly reverse conf-h1 and conf-h2 respectively, not conflated")
  .chk(sum(twice$documento == "D" & twice$action == "recovered", na.rm = TRUE), 2L,
       "H5: recovery COUNT for business key 'D' is queryable directly (2)")
}

# =============================================================================
# I. Backward compatibility for bancos_confirmados
# =============================================================================
{
  old_shape_conf <- data.frame(
    confirmacion_id = "old-conf-1", agenda_item_id = NA_character_,
    empresa = "E", parte = "P", documento = "D", codigo = NA_character_,
    importe = 1, moneda = "MXN", cuenta_id = NA_character_, fecha = as.Date("2026-01-01"),
    tipo = "pago", mov_id = NA_character_, confirmado_at = Sys.time(),
    eliminado = FALSE, eliminado_at = as.POSIXct(NA), provision_id = NA_character_,
    stringsAsFactors = FALSE
  )
  normalized <- .normalize(old_shape_conf, .schema_bancos_confirmados)
  .chk_true(is.na(normalized$action[1]), "I1: old-shape confirmacion row's action normalizes to NA")
  .chk_true(is.na(normalized$reverses_confirmacion_id[1]), "I2: old-shape row's reverses_confirmacion_id normalizes to NA")
  .chk_true(is.na(normalized$recovered_at[1]), "I3: old-shape row's recovered_at normalizes to NA")

  # recover_confirmacion() must work correctly on an old-shape row that has
  # never seen the new columns before -- the function only depends on
  # eliminado, not on action/reverses_confirmacion_id already being set.
  recovered <- recover_confirmacion(normalized, "old-conf-1", actor = "mouse")
  .chk(sum(recovered$action == "recovered", na.rm = TRUE), 1L,
       "I4: recovering an old-shape row (pre-existing NA action) works correctly")
}

cat(sprintf("\n=== Archive-mechanism results: %d passed, %d failed ===\n", .pass, .fail))
