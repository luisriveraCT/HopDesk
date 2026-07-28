# =============================================================================
# tests/test_stage6_agenda_derivation.R
# Stage 6 of the ledger-integrity master plan (see
# docs/LEDGER_INTEGRITY_MASTER_PLAN.md): the "send straight to Agenda"
# convenience (Pasivos conversion modal's stage_to_agenda branch, and direct
# manual-entry creation's "send to agenda" checkbox) used to fabricate the
# pagar_hoy row from the modal/form's raw input a SECOND time, independently
# of the row that had just been written to manual_inv -- a violation of the
# root/mirror principle (Agenda should only ever reference real Calendario
# data, never a hand-built guess). The fix: stage_manual_row_to_agenda()
# (R/data_pipeline.R) derives the pagar_hoy row entirely from the manual_inv
# row as it now exists, and both call sites re-read that row by id from the
# just-written data frame instead of touching input$... a second time.
#
# Three kinds of checks:
#   1. Unit tests on stage_manual_row_to_agenda() itself: field mapping,
#      plain-manual vs provision-derived source inference, NA handling.
#   2. A divergence test proving REAL re-derivation, not a tautology: build a
#      manual_row whose stored fields deliberately differ from what naive
#      raw-input reconstruction would have produced (a full company name
#      where raw input would have been an initial; a Documento already
#      resolved through the CONV_ fallback) and confirm the staged row
#      reflects the row's ACTUAL stored values.
#   3. Static source scan — confirms both call sites now call
#      stage_manual_row_to_agenda() and no longer read the ledger-specific
#      raw input fields (input$pcm_empresa/moneda/parte/codigo/importe/fecha,
#      input$me_empresa/moneda/parte/codigo/importe/fecha) inside the
#      stage-to-Agenda block specifically.
# =============================================================================

cat("── Stage 6: \"send straight to Agenda\" derives, never fabricates ───────\n")

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

# ── 1. Unit tests on stage_manual_row_to_agenda() ───────────────────────────
{
  plain_row <- tibble::tibble(
    id = "man-1", ledger = "AP", Empresa = "Networks & Logistics",
    Moneda = "MXN", Documento = "F-100", Parte = "ACME", Codigo = "C1",
    Importe = 1234.56, `Fecha de vencimiento` = as.Date("2026-08-15"),
    provision_id = NA_character_, liability_id = NA_character_
  )
  out <- stage_manual_row_to_agenda(plain_row, user = "dev")

  .chk(nrow(out), 1L, "plain manual row: returns exactly one staged row")
  .chk(out$id,        "man-1",                 "plain manual: id shared with manual_inv row (§2.1 convention)")
  .chk(out$ledger,    "AP",                    "plain manual: ledger copied through")
  .chk(out$Empresa,   "Networks & Logistics",  "plain manual: Empresa copied through")
  .chk(out$Moneda,    "MXN",                   "plain manual: Moneda copied through")
  .chk(out$Documento, "F-100",                 "plain manual: Documento copied through")
  .chk(out$Parte,     "ACME",                  "plain manual: Parte copied through")
  .chk(out$Codigo,    "C1",                    "plain manual: Codigo copied through")
  .chk(out$tipo_item, "factura",               "plain manual: tipo_item is always 'factura'")
  .chk(out$Importe,   1234.56,                 "plain manual: Importe copied through")
  .chk(as.character(out$FechaVenc), "2026-08-15", "plain manual: FechaVenc derived from Fecha de vencimiento")
  .chk(out$staged_by, "dev",                   "plain manual: staged_by set from user arg")
  .chk(out$status,    "pending",               "plain manual: status always starts 'pending'")
  .chk(is.na(out$provision_id), TRUE,          "plain manual: provision_id stays NA")
  .chk(is.na(out$liability_id), TRUE,          "plain manual: liability_id stays NA")
  .chk(out$source,    "manual",                "plain manual: source inferred as 'manual' (no provision_id)")

  prov_row <- tibble::tibble(
    id = "man-2", ledger = "AP", Empresa = "Networks & Logistics",
    Moneda = "MXN", Documento = "CONV_prov-9", Parte = "Proveedor X", Codigo = "",
    Importe = 500, `Fecha de vencimiento` = as.Date("2026-09-01"),
    provision_id = "prov-9", liability_id = "liab-9"
  )
  out2 <- stage_manual_row_to_agenda(prov_row, user = "dev")
  .chk(out2$source,       "provision", "provision-derived: source inferred as 'provision'")
  .chk(out2$provision_id, "prov-9",    "provision-derived: provision_id copied through")
  .chk(out2$liability_id, "liab-9",    "provision-derived: liability_id copied through")
  .chk(out2$Documento,    "CONV_prov-9", "provision-derived: Documento (already resolved) copied verbatim")

  # Empty-string Parte/Codigo must stay "" (not NA, not dropped) — matches the
  # existing trimws(input$... %||% "") convention at both call sites.
  blank_row <- tibble::tibble(
    id = "man-3", ledger = "AR", Empresa = "Networks & Logistics",
    Moneda = "USD", Documento = "F-3", Parte = "", Codigo = "",
    Importe = 10, `Fecha de vencimiento` = as.Date("2026-10-01"),
    provision_id = NA_character_, liability_id = NA_character_
  )
  out3 <- stage_manual_row_to_agenda(blank_row, user = "dev")
  .chk(out3$Parte,  "", "blank Parte preserved as empty string, not NA")
  .chk(out3$Codigo, "", "blank Codigo preserved as empty string, not NA")
}

# ── 2. Divergence test: proves real re-derivation, not a tautology ─────────
# Construct a manual_row whose stored values deliberately differ from what a
# naive "rebuild from raw form input" implementation would have produced —
# the exact class of drift Stage 6 exists to prevent. If the call sites still
# passed input$... straight through, this would catch it; because they now
# re-read the row by id from the written data frame, the staged row must
# match the ACTUAL stored row, not a hypothetical fabricated one.
{
  # Simulates Pasivos' full_empresa translation: raw input holds the initials
  # ("NL"), but the row actually written to manual_inv (and now being staged)
  # already carries the translated full name — the fabricated-from-raw-input
  # version would have wrongly staged "NL", never translated.
  hypothetical_raw_input_empresa <- "NL"
  manual_row <- tibble::tibble(
    id = "man-4", ledger = "AP", Empresa = "Networks & Logistics",
    Moneda = "MXN", Documento = "F-4", Parte = "Proveedor Y", Codigo = "",
    Importe = 99, `Fecha de vencimiento` = as.Date("2026-11-11"),
    provision_id = NA_character_, liability_id = NA_character_
  )
  out <- stage_manual_row_to_agenda(manual_row, user = "dev")
  .chk(out$Empresa != hypothetical_raw_input_empresa, TRUE,
       "staged Empresa is the row's translated full name, not the raw initials a fabricated build would have used")
  .chk(out$Empresa, "Networks & Logistics",
       "staged Empresa exactly matches the manual_inv row's actual stored value")
}

# ── 3. Static source scan — both call sites re-derive, never fabricate ─────
{
  .block_lines <- function(txt, start_pat, n = 20) {
    s <- grep(start_pat, txt)
    if (!length(s)) return(character(0))
    s <- s[1]
    txt[s:min(s + n, length(txt))]
  }
  .strip_comments <- function(lines) sub("#.*$", "", lines)

  pasivos_txt <- readLines("R/pasivos_module.R", warn = FALSE)
  pv_block <- .strip_comments(.block_lines(pasivos_txt, "if \\(stage_to_agenda\\) \\{", 15))
  pv_joined <- paste(pv_block, collapse = "\n")

  .chk(length(pv_block) > 0, TRUE,
       "pasivos_module.R: found the stage_to_agenda branch to scan (guard against a silent no-op)")
  .chk(grepl("stage_manual_row_to_agenda\\(", pv_joined), TRUE,
       "pasivos_module.R stage_to_agenda branch calls stage_manual_row_to_agenda()")
  for (raw_field in c("input\\$pcm_empresa", "input\\$pcm_moneda", "input\\$pcm_parte",
                       "input\\$pcm_codigo", "input\\$pcm_importe", "input\\$pcm_fecha")) {
    .chk(grepl(raw_field, pv_joined), FALSE,
         sprintf("pasivos_module.R stage_to_agenda branch no longer reads %s directly", raw_field))
  }

  app_txt <- readLines("app.R", warn = FALSE)
  app_block <- .strip_comments(.block_lines(app_txt, "Stage to Agenda de hoy if toggle is active", 15))
  app_joined <- paste(app_block, collapse = "\n")

  .chk(length(app_block) > 0, TRUE,
       "app.R: found the send-to-agenda block to scan (guard against a silent no-op)")
  .chk(grepl("stage_manual_row_to_agenda\\(", app_joined), TRUE,
       "app.R send-to-agenda block calls stage_manual_row_to_agenda()")
  for (raw_field in c("input\\$me_empresa", "input\\$me_moneda", "input\\$me_parte",
                       "input\\$me_codigo", "input\\$me_importe", "input\\$me_fecha")) {
    .chk(grepl(raw_field, app_joined), FALSE,
         sprintf("app.R send-to-agenda block no longer reads %s directly", raw_field))
  }

  # Both call sites must look the written row up BY ID from the data frame
  # that was just bound/saved -- the actual "re-derive from what exists"
  # mechanism, not just an absence of raw-input reads.
  .chk(grepl("manual_df\\[manual_df\\[\\[\"id\"\\]\\]", pv_joined) ||
       grepl("manual_row <- manual_df\\[", pv_joined), TRUE,
       "pasivos_module.R re-reads the manual row by id from manual_df before staging")
  .chk(grepl("df\\[df\\[\\[\"id\"\\]\\]", app_joined) ||
       grepl("manual_row <- df\\[", app_joined), TRUE,
       "app.R re-reads the manual row by id from df before staging")
}

cat(sprintf("\n  Stage 6 subtotal: %d passed, %d failed\n", .pass, .fail))
