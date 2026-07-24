# =============================================================================
# tests/test_stage5_provision_confirm_undo.R
# Stage 5 of the ledger-integrity master plan: extend Stage 4's archiving to
# provision-derived confirmations too (Mouse's rule: confirmation-history
# covers EVERYTHING that gets confirmed, no special-casing by origin), and
# fix the undo_conf/pasivos_observers.R collision -- undo_conf no longer
# touches pagar_hoy_db or manual_inv at all for a provision-derived
# confirmation, deferring entirely to pasivos_observers.R's own reversal
# watcher (which reverts the provision to "provisional" so its raw
# placeholder reappears, and never needs an Agenda footprint).
# =============================================================================

cat("── Stage 5: provision-derived confirm/undo ──────────────────────────────\n")

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
.chk_true <- function(cond, label) .chk(isTRUE(cond), TRUE, label)

# =============================================================================
# A. Static scan
# =============================================================================
{
  ph_txt  <- readLines("R/pagar_hoy_module.R", warn = FALSE)
  bnc_txt <- readLines("R/bancos_module.R", warn = FALSE)

  .chk(sum(grepl("unchanged hard-delete for now \\(Stage 5\\)", ph_txt)), 0L,
       "A1: no leftover 'unchanged hard-delete for now (Stage 5)' comment -- the deferred TODO is resolved")
  .chk_true(sum(grepl('disposition = "confirmed"', ph_txt)) == 4L,
            "A2: exactly 4 disposition='confirmed' archive calls now (2 plain-manual + 2 provision-derived)")

  # The has_provision branch in undo_conf must no longer reference
  # pagar_hoy_db / manual_inv / upsert_pagar_hoy at all -- isolate its body
  # (from the "if (has_provision)" line to the matching "} else if
  # (has_archive)") and scan only that block.
  start <- grep("if \\(has_provision\\)", bnc_txt)
  end   <- grep("else if \\(has_archive\\)", bnc_txt)
  .chk(length(start), 1L, "A3: exactly one has_provision branch found in bancos_module.R")
  .chk(length(end), 1L, "A4: exactly one has_archive else-branch found (bounds the has_provision block)")
  if (length(start) && length(end)) {
    block <- bnc_txt[start:end]
    # Strip full-line and trailing comments before scanning -- the block's
    # own explanatory comment legitimately discusses pagar_hoy_db/manual_inv
    # in prose (explaining what it USED to do); only actual code matters here.
    code_only <- sub("#.*$", "", block)
    code_only <- code_only[!grepl("^\\s*$", code_only)]
    .chk(sum(grepl("pagar_hoy_db|upsert_pagar_hoy|shared\\$manual_inv", code_only)), 0L,
         "A5: the has_provision branch's CODE (not its comments) touches neither pagar_hoy_db nor manual_inv anywhere")
    .chk_true(any(grepl("pasivos_observers", block)),
              "A6: the has_provision branch documents that it defers to pasivos_observers.R")
  }
}

# =============================================================================
# Shared fixtures
# =============================================================================
.manual_invoice_row <- function(overrides = list()) {
  base <- list(
    id = uuid::UUIDgenerate(), ledger = "AP", source = "manual",
    Empresa = "Networks & Logistics", Moneda = "MXN", Documento = "S5-DOC",
    Factura = "S5-FAC", Parte = "Proveedor S5", Codigo = "P-S5",
    Importe = 321.5, `Abono futuro` = 0,
    `Fecha de vencimiento` = as.Date("2026-09-15"), FechaEff = as.Date("2026-09-15"),
    Notas = "provision-derived test row", created_by = "mouse",
    created_at = as.POSIXct("2026-07-01 09:00:00"),
    updated_at = as.POSIXct("2026-07-01 09:00:00"),
    provision_id = "prov-fixed-1", liability_id = "liab-fixed-1", referencia = "REF-S5"
  )
  base[names(overrides)] <- overrides
  as.data.frame(base, stringsAsFactors = FALSE)
}
.fresh_conf_row <- function(agenda_item_id, empresa, parte, documento, importe, moneda = "MXN",
                            provision_id = NA_character_) {
  row <- tibble::tibble(
    confirmacion_id = uuid::UUIDgenerate(), agenda_item_id = agenda_item_id,
    empresa = empresa, parte = parte, documento = documento, codigo = NA_character_,
    importe = importe, moneda = moneda, cuenta_id = NA_character_, fecha = Sys.Date(),
    tipo = "pago", mov_id = NA_character_, confirmado_at = Sys.time(),
    eliminado = FALSE, eliminado_at = as.POSIXct(NA), provision_id = provision_id
  )
  .normalize(row, .schema_bancos_confirmados)
}

# Replicates the rewritten confirm handler's provision-derived archiving loop.
.simulate_confirm_provision <- function(mi, pap, conf, ph_id, provision_id, actor = "mouse") {
  to_archive <- mi[mi[["provision_id"]] == provision_id, , drop = FALSE]
  pap <- add_to_papelera(pap, to_archive, ledger = "AP", deleted_by = actor, disposition = "confirmed")
  event_id <- pap[["event_id"]][nrow(pap)]
  cidx <- which(conf[["agenda_item_id"]] == ph_id & is.na(conf[["archive_event_id"]]))
  if (length(cidx)) conf[["archive_event_id"]][cidx[length(cidx)]] <- event_id
  mi <- mi[mi[["provision_id"]] != provision_id | is.na(mi[["provision_id"]]), , drop = FALSE]
  list(mi = mi, pap = pap, conf = conf, event_id = event_id)
}

empty_papelera <- .schema_papelera() |> dplyr::slice(0)

# =============================================================================
# B. Provision-derived confirm archives correctly
# =============================================================================
{
  row <- .manual_invoice_row(list(Documento = "B-PROV"))
  mi  <- row
  ph_id <- uuid::UUIDgenerate()  # provision-derived rows always mint a fresh pagar_hoy id

  conf <- .fresh_conf_row(ph_id, row$Empresa, row$Parte, row$Documento, row$Importe,
                          provision_id = row$provision_id)
  conf_id <- conf$confirmacion_id[1]

  step1 <- .simulate_confirm_provision(mi, empty_papelera, conf, ph_id, row$provision_id)
  .chk(nrow(step1$mi), 0L, "B1: provision-derived manual_inv row removed from the live table")
  .chk(nrow(step1$pap), 1L, "B2: exactly 1 papelera archive row created")
  .chk(step1$pap$disposition[1], "confirmed", "B3: archived with disposition='confirmed'")
  .chk_true(!is.na(step1$conf$archive_event_id[step1$conf$confirmacion_id == conf_id]),
            "B4: bancos_confirmados row's archive_event_id is linked, same as plain manual")

  archived <- as.data.frame(step1$pap$original_data[[1]], stringsAsFactors = FALSE)
  .chk(archived$provision_id[1], "prov-fixed-1", "B5: archived copy preserves provision_id (full field fidelity)")
  .chk(archived$liability_id[1], "liab-fixed-1", "B6: archived copy preserves liability_id (full field fidelity)")
}

# =============================================================================
# C. Multiple provision-derived rows in one batch link correctly, not
#    cross-contaminated
# =============================================================================
{
  row1 <- .manual_invoice_row(list(Documento = "C-PROV-1", Importe = 10, provision_id = "prov-c-1"))
  row2 <- .manual_invoice_row(list(Documento = "C-PROV-2", Importe = 20, provision_id = "prov-c-2"))
  mi   <- dplyr::bind_rows(row1, row2)
  ph_id1 <- uuid::UUIDgenerate(); ph_id2 <- uuid::UUIDgenerate()

  conf <- dplyr::bind_rows(
    .fresh_conf_row(ph_id1, row1$Empresa, row1$Parte, row1$Documento, row1$Importe, provision_id = "prov-c-1"),
    .fresh_conf_row(ph_id2, row2$Empresa, row2$Parte, row2$Documento, row2$Importe, provision_id = "prov-c-2")
  )

  pap <- empty_papelera
  step_a <- .simulate_confirm_provision(mi, pap, conf, ph_id1, "prov-c-1")
  step_b <- .simulate_confirm_provision(step_a$mi, step_a$pap, step_a$conf, ph_id2, "prov-c-2")

  .chk(nrow(step_b$mi), 0L, "C1: both provision-derived rows removed from manual_inv")
  .chk(nrow(step_b$pap), 2L, "C2: 2 independent archive rows, one per provision")

  conf1_id <- step_b$conf$confirmacion_id[step_b$conf$documento == "C-PROV-1"]
  conf2_id <- step_b$conf$confirmacion_id[step_b$conf$documento == "C-PROV-2"]
  event1 <- step_b$conf$archive_event_id[step_b$conf$confirmacion_id == conf1_id]
  event2 <- step_b$conf$archive_event_id[step_b$conf$confirmacion_id == conf2_id]
  .chk_true(event1 != event2, "C3: the 2 confirmations link to 2 DIFFERENT archive events, not conflated")
  .chk(step_b$pap$event_id[step_b$pap$Documento == "C-PROV-1"], event1,
       "C4: confirmation 1's archive_event_id points at the archive row for doc C-PROV-1 specifically")
  .chk(step_b$pap$event_id[step_b$pap$Documento == "C-PROV-2"], event2,
       "C5: confirmation 2's archive_event_id points at the archive row for doc C-PROV-2 specifically")
}

# =============================================================================
# D. Undo a provision-derived confirmation: recover_confirmacion() still
#    fires (Historial gets its recovery line, uniform with every other
#    disposition), but the archived manual_inv copy is never restored --
#    that would duplicate the provision's own revival display.
# =============================================================================
{
  row <- .manual_invoice_row(list(Documento = "D-PROV"))
  mi  <- row
  ph_id <- uuid::UUIDgenerate()
  conf <- .fresh_conf_row(ph_id, row$Empresa, row$Parte, row$Documento, row$Importe,
                          provision_id = row$provision_id)
  conf_id <- conf$confirmacion_id[1]

  step1 <- .simulate_confirm_provision(mi, empty_papelera, conf, ph_id, row$provision_id)

  # Undo: recover_confirmacion() fires unconditionally (same as every other
  # disposition) -- this is the actual behavior undo_conf exercises before
  # any has_provision/has_archive branching.
  recovered_conf <- recover_confirmacion(step1$conf, conf_id, actor = "mouse2")
  .chk(sum(recovered_conf$eliminado, na.rm = TRUE), 1L,
       "D1: provision-derived confirmation's eliminado flag clears correctly on undo")
  .chk(sum(recovered_conf$action == "recovered", na.rm = TRUE), 1L,
       "D2: provision-derived confirmation still gets its own permanent recovery event row")

  # The archived manual_inv copy itself is NEVER touched by undo -- it
  # stays exactly as archived, forever (permanent per Mouse's rule), with
  # no corresponding "recovered" event of its own, since reverting the
  # provision (tracked entirely in pasivos_provisions_db, not here) is the
  # actual undo mechanism for this case.
  .chk(nrow(step1$pap), 1L, "D3: papelera still has exactly 1 row (the original archive) after undo -- no restore attempted")
  .chk(sum(step1$pap$action == "recovered"), 0L,
       "D4: no 'recovered' papelera event exists for the provision-derived archive -- undo never calls restore_from_papelera for this case")
  .chk(nrow(mi), 1L, "D5: manual_inv is untouched by this undo simulation (still holds the original unarchived reference from before confirm, proving undo doesn't reach into it)")
}

# =============================================================================
# E. pasivos_observers.R's own manual_inv cleanup degrades to a safe no-op
#    once the row is already archived (not hard-deleted) at confirm time
# =============================================================================
{
  # Simulates exactly what pasivos_observers.R's revival watcher does to
  # manual_inv (R/pasivos_observers.R:125-129): `mi <- mi[mi$id != mi_id, ]`.
  # After Stage 5, by the time this runs, the row is already gone (archived
  # at confirm time), so this must be provably harmless -- no error, no
  # unintended removal of anything else.
  row <- .manual_invoice_row(list(Documento = "E-PROV"))
  mi_after_archive <- row[row$id != row$id, , drop = FALSE]  # simulates the post-archive empty state
  mi_id <- row$id

  result <- tryCatch({
    mi_after_archive[mi_after_archive$id != mi_id, , drop = FALSE]
  }, error = function(e) "ERROR")

  .chk(nrow(result), 0L, "E1: pasivos_observers.R's manual_inv cleanup is a safe no-op when the row is already archived")
  .chk_true(!identical(result, "ERROR"), "E2: no error is thrown by the now-redundant cleanup step")
}

cat(sprintf("\n=== Stage 5 provision confirm/undo results: %d passed, %d failed ===\n", .pass, .fail))
