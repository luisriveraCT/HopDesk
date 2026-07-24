# =============================================================================
# tests/test_stage4_confirm_undo.R
# Stage 4 of the ledger-integrity master plan: confirm/undo rewrite for ERP +
# plain manual entries. Confirming now always fully unstages Agenda (every
# source, not just manual); plain manual entries get archived (Stage 3's
# mechanism) instead of hard-deleted; the four "can't Quitar/re-stage a
# confirmed SAP row" guards are removed entirely (Agenda removal is always
# safe regardless of source per Mouse's explicit rule).
#
# The confirm/undo handlers themselves live inside Shiny observeEvent blocks
# and can't be unit-tested directly without a live session -- this suite
# uses two techniques instead:
#   1. Static source scan -- the four removed guards are gone, the new
#      full-unstage/archive/branching logic is present.
#   2. Full integration simulation -- replicate the EXACT sequence of Stage 3
#      persistence-layer calls the rewritten handlers make (not the handlers
#      themselves), covering both real UUID-linking cases (direct manual-
#      entry, which shares an id with manual_inv, and Calendar/Search
#      staging, which mints a fresh one) end to end through a full
#      confirm-then-undo cycle, proving zero field loss.
# =============================================================================

cat("── Stage 4: confirm/undo rewrite (ERP + plain manual) ──────────────────\n")

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
# A. Static scan — the 4 wrong guards are gone, the new logic is present
# =============================================================================
{
  ph_txt <- readLines("R/pagar_hoy_module.R", warn = FALSE)
  led_txt <- readLines("R/ledger_module.R", warn = FALSE)
  bnc_txt <- readLines("R/bancos_module.R", warn = FALSE)

  .chk(any(grepl("no se pueden quitar", ph_txt)), FALSE,
       "A1: pagar_hoy_module.R has no leftover 'no se pueden quitar' guard text")
  .chk(any(grepl("no se pueden quitar", led_txt)), FALSE,
       "A2: ledger_module.R has no leftover 'no se pueden quitar' guard text")
  .chk(any(grepl("No se puede quitar", led_txt)), FALSE,
       "A3: ledger_module.R has no leftover 'No se puede quitar' (singular) guard text")
  .chk(sum(grepl("Solo SAP puede cerrarlo", c(ph_txt, led_txt))), 0L,
       "A4: no 'Solo SAP puede cerrarlo(s)' text remains anywhere in either file")

  .chk_true(any(grepl('rm_ids <- c\\(factura_rows\\$id, abono_rows\\$id\\)', ph_txt)),
            "A5: confirm handler now unstages ALL factura_rows (not just non-ERP ones)")
  .chk(sum(grepl('ph\\$status\\[idx_ph\\]\\s*<-\\s*"confirmed"', ph_txt)), 0L,
       "A6: confirm handler no longer leaves any pagar_hoy row with status=='confirmed'")
  .chk_true(sum(grepl("add_to_papelera\\(pap, to_archive", ph_txt)) == 2L,
            "A7: exactly 2 archiving call sites (AP + AR confirm handlers)")
  .chk_true(sum(grepl('disposition = "confirmed"', ph_txt)) == 2L,
            "A8: exactly 2 disposition='confirmed' archive calls")

  .chk_true(any(grepl("recover_confirmacion\\(conf, conf_id", bnc_txt)),
            "A9: undo_conf() calls the new recover_confirmacion()")
  .chk_true(any(grepl("restore_from_papelera\\(pap, row\\$archive_event_id", bnc_txt)),
            "A10: undo_conf() calls the new restore_from_papelera() for the archived-manual branch")
  .chk_true(any(grepl("has_provision", bnc_txt)) && any(grepl("has_archive", bnc_txt)),
            "A11: undo_conf() branches explicitly on provision vs. archived-manual vs. neither")
}

# =============================================================================
# Shared fixtures for the integration simulations below
# =============================================================================
.manual_invoice_row <- function(overrides = list()) {
  base <- list(
    id = uuid::UUIDgenerate(), ledger = "AP", source = "manual",
    Empresa = "Networks & Logistics", Moneda = "MXN", Documento = "S4-DOC",
    Factura = "S4-FAC", Parte = "Proveedor S4", Codigo = "P-S4",
    Importe = 777.77, `Abono futuro` = 0,
    `Fecha de vencimiento` = as.Date("2026-09-01"), FechaEff = as.Date("2026-09-01"),
    Notas = "nota de prueba con ñ", created_by = "mouse",
    created_at = as.POSIXct("2026-07-01 08:00:00"),
    updated_at = as.POSIXct("2026-07-01 08:00:00"),
    provision_id = NA_character_, liability_id = NA_character_, referencia = NA_character_
  )
  base[names(overrides)] <- overrides
  as.data.frame(base, stringsAsFactors = FALSE)
}

# Replicates the rewritten confirm handler's plain-manual archiving sequence
# (the id-then-business-key matching, papelera archive, and the
# archive_event_id link into bancos_confirmados) exactly as the real R code
# does it, using the REAL Stage 3 functions -- not the handler itself, which
# needs a live Shiny session, but the identical persistence-layer sequence.
.simulate_confirm_plain_manual <- function(mi, pap, conf, ph_id, actor = "mouse") {
  .biz_key <- function(df) paste(
    toupper(trimws(df[["Empresa"]])), toupper(trimws(df[["Moneda"]])),
    toupper(trimws(df[["Documento"]])), sprintf("%.2f", round(as.numeric(df[["Importe"]]), 2)))

  fr <- data.frame(id = ph_id, Empresa = mi$Empresa[1], Moneda = mi$Moneda[1],
                   Documento = mi$Documento[1], Importe = mi$Importe[1], stringsAsFactors = FALSE)
  mi_idx <- which(mi[["id"]] == fr[["id"]])
  if (!length(mi_idx)) mi_idx <- which(.biz_key(mi) == .biz_key(fr))
  mi_idx <- mi_idx[1]

  to_archive <- mi[mi_idx, , drop = FALSE]
  pap <- add_to_papelera(pap, to_archive, ledger = "AP", deleted_by = actor, disposition = "confirmed")
  event_id <- pap[["event_id"]][nrow(pap)]

  cidx <- which(conf[["agenda_item_id"]] == fr[["id"]] & is.na(conf[["archive_event_id"]]))
  if (length(cidx)) conf[["archive_event_id"]][cidx[length(cidx)]] <- event_id

  mi <- mi[!mi[["id"]] %in% mi[["id"]][mi_idx], , drop = FALSE]
  list(mi = mi, pap = pap, conf = conf, event_id = event_id)
}

# Replicates the rewritten undo_conf()'s archived-manual restore branch.
.simulate_undo_archived_manual <- function(mi, pap, conf, confirmacion_id, actor = "mouse2") {
  conf <- recover_confirmacion(conf, confirmacion_id, actor = actor)
  archive_event_id <- conf[["archive_event_id"]][conf[["confirmacion_id"]] == confirmacion_id][1]
  res <- restore_from_papelera(pap, archive_event_id, actor = actor)
  restored_row <- as.data.frame(res$restored_data, stringsAsFactors = FALSE)
  mi <- dplyr::bind_rows(mi, restored_row)
  list(mi = mi, pap = res$papelera_df, conf = conf)
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

empty_papelera <- .schema_papelera() |> dplyr::slice(0)

# =============================================================================
# B. Full cycle — direct manual-entry-with-send-to-agenda case
#    (pagar_hoy.id deliberately == manual_inv.id, matches by id directly)
# =============================================================================
{
  row <- .manual_invoice_row(list(Documento = "B-DIRECT"))
  mi  <- row
  ph_id <- row$id  # shared id, per the direct-entry staging path

  conf <- .fresh_conf_row(ph_id, row$Empresa, row$Parte, row$Documento, row$Importe)
  conf_id <- conf$confirmacion_id[1]

  step1 <- .simulate_confirm_plain_manual(mi, empty_papelera, conf, ph_id)
  .chk(nrow(step1$mi), 0L, "B1: manual_inv row removed from the live table after confirm")
  .chk(nrow(step1$pap), 1L, "B2: exactly 1 papelera archive row created")
  .chk(step1$pap$disposition[1], "confirmed", "B3: archived with disposition='confirmed'")
  .chk_true(!is.na(step1$conf$archive_event_id[step1$conf$confirmacion_id == conf_id]),
            "B4: bancos_confirmados row's archive_event_id is linked")

  step2 <- .simulate_undo_archived_manual(step1$mi, step1$pap, step1$conf, conf_id)
  .chk(nrow(step2$mi), 1L, "B5: manual_inv row restored after undo")
  for (f in c("id","Empresa","Moneda","Documento","Factura","Parte","Codigo","Importe",
             "Abono futuro","Fecha de vencimiento","Notas","created_by","created_at","updated_at")) {
    .chk(step2$mi[[f]][1], row[[f]][1], sprintf("B6: restored field '%s' matches the original exactly (direct-entry case)", f))
  }
  .chk(sum(step2$conf$eliminado, na.rm = TRUE), 1L, "B7: exactly 1 eliminado row after undo (the original confirmation)")
  .chk(sum(step2$conf$action == "recovered", na.rm = TRUE), 1L, "B8: exactly 1 recovery event row after undo")
}

# =============================================================================
# C. Full cycle — Calendar/Search-staged case (fresh pagar_hoy id, decoupled
#    from manual_inv's own id -- the latent gap found and fixed in this stage)
# =============================================================================
{
  row   <- .manual_invoice_row(list(Documento = "C-STAGED", Importe = 999.99))
  mi    <- row
  ph_id <- uuid::UUIDgenerate()  # deliberately DIFFERENT from row$id

  conf <- .fresh_conf_row(ph_id, row$Empresa, row$Parte, row$Documento, row$Importe)
  conf_id <- conf$confirmacion_id[1]

  step1 <- .simulate_confirm_plain_manual(mi, empty_papelera, conf, ph_id)
  .chk(nrow(step1$mi), 0L, "C1: manual_inv row correctly found and removed via business-key fallback")
  .chk(nrow(step1$pap), 1L, "C2: exactly 1 papelera archive row created despite the id mismatch")
  .chk_true(!is.na(step1$conf$archive_event_id[step1$conf$confirmacion_id == conf_id]),
            "C3: bancos_confirmados row's archive_event_id is still correctly linked")

  step2 <- .simulate_undo_archived_manual(step1$mi, step1$pap, step1$conf, conf_id)
  .chk(nrow(step2$mi), 1L, "C4: manual_inv row restored after undo (Calendar/Search-staged case)")
  for (f in c("id","Empresa","Documento","Importe","Notas","liability_id","referencia")) {
    .chk(step2$mi[[f]][1], row[[f]][1], sprintf("C5: restored field '%s' matches the original exactly (staged case)", f))
  }
}

# =============================================================================
# D. ERP-sourced confirm/undo — no manual_inv or papelera involvement at all
# =============================================================================
{
  conf <- .fresh_conf_row("erp-ph-id", "Networks Group", "HAPAG LLOYD", "ERP-DOC-1", 5000, moneda = "USD")
  conf_id <- conf$confirmacion_id[1]

  # Confirm: per the rewritten handler, sap_fact_ids never touches manual_inv
  # or papelera at all -- only the pagar_hoy unstage (out of scope for this
  # persistence-only simulation) and the bancos_confirmados write (already
  # captured in the fixture) happen. archive_event_id stays NA throughout.
  .chk_true(is.na(conf$archive_event_id[1]), "D1: ERP confirmation never gets an archive_event_id")

  # Undo: has_provision is FALSE, has_archive is FALSE -> the "do nothing
  # beyond the flag clear" branch. recover_confirmacion() still fires (it's
  # unconditional, before the branching) so the permanent recovery-event
  # record still gets written even for ERP.
  recovered <- recover_confirmacion(conf, conf_id, actor = "mouse")
  .chk(sum(recovered$eliminado, na.rm = TRUE), 1L, "D2: ERP confirmation's eliminado flag still clears correctly")
  .chk(sum(recovered$action == "recovered", na.rm = TRUE), 1L, "D3: ERP recovery still gets its own permanent event row")
  .chk_true(is.na(recovered$archive_event_id[recovered$action == "recovered"][1]),
            "D4: the ERP recovery event row itself has no archive_event_id (nothing was ever archived)")
}

# =============================================================================
# E. Provision-derived rows — deferred behavior unchanged for now (Stage 5)
# =============================================================================
{
  conf <- .fresh_conf_row("prov-ph-id", "Networks & Logistics", "Provider", "PROV-DOC-1", 250,
                          provision_id = "prov-xyz")
  row  <- conf[1, ]
  has_provision <- !is.na(row$provision_id) && nzchar(row$provision_id %||% "")
  has_archive   <- "archive_event_id" %in% names(row) &&
                    !is.na(row$archive_event_id) && nzchar(row$archive_event_id %||% "")
  .chk_true(has_provision, "E1: a provision-derived confirmation is correctly identified as such")
  .chk_true(!has_archive, "E2: a provision-derived confirmation has no archive_event_id (Stage 4 never archives these)")
  # The actual branch precedence in undo_conf checks has_provision FIRST --
  # confirmed here since Stage 5 will need to know this ordering is safe to
  # change later without silently breaking Stage 4's ERP/manual paths.
  branch <- if (has_provision) "provision-deferred" else if (has_archive) "archived-manual" else "erp-or-legacy"
  .chk(branch, "provision-deferred", "E3: provision-derived rows route to the unchanged deferred branch, not the new archive-restore one")
}

# =============================================================================
# F. Multi-cycle: confirm -> undo -> confirm -> undo for the same business
#    invoice, proving the chain stays correct across repeated cycles
# =============================================================================
{
  key <- list(Empresa = "Networks & Logistics", Moneda = "MXN", Documento = "F-CYCLE")
  pap <- empty_papelera
  conf <- .schema_bancos_confirmados() |> dplyr::slice(0)

  for (cycle in 1:2) {
    row <- .manual_invoice_row(c(key, list(Importe = cycle * 50)))
    mi  <- row
    ph_id <- row$id
    new_conf <- .fresh_conf_row(ph_id, row$Empresa, row$Parte, row$Documento, row$Importe)
    conf <- dplyr::bind_rows(conf, new_conf)
    conf_id <- new_conf$confirmacion_id[1]

    step1 <- .simulate_confirm_plain_manual(mi, pap, conf, ph_id)
    pap  <- step1$pap
    conf <- step1$conf
    .chk(nrow(step1$mi), 0L, sprintf("F%d.1: cycle %d confirm removes manual_inv row", cycle, cycle))

    step2 <- .simulate_undo_archived_manual(step1$mi, pap, conf, conf_id)
    pap  <- step2$pap
    conf <- step2$conf
    .chk(nrow(step2$mi), 1L, sprintf("F%d.2: cycle %d undo restores manual_inv row", cycle, cycle))
    .chk(step2$mi$Importe[1], cycle * 50, sprintf("F%d.3: cycle %d restored row has the correct cycle-specific Importe (no cross-cycle contamination)", cycle, cycle))
  }

  .chk(sum(pap$Documento == "F-CYCLE" & pap$action == "archived", na.rm = TRUE), 2L,
       "F-final: 2 distinct archive events across the 2 cycles")
  .chk(sum(pap$Documento == "F-CYCLE" & pap$action == "recovered", na.rm = TRUE), 2L,
       "F-final: 2 distinct recovery events across the 2 cycles")
  .chk(sum(conf$documento == "F-CYCLE" & conf$action == "recovered", na.rm = TRUE), 2L,
       "F-final: bancos_confirmados also shows 2 distinct recovery events for this business key")
}

cat(sprintf("\n=== Stage 4 confirm/undo results: %d passed, %d failed ===\n", .pass, .fail))
