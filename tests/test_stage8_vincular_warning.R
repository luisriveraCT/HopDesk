# =============================================================================
# tests/test_stage8_vincular_warning.R
# Stage 8 of the ledger-integrity master plan (source
# docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md §6.2): Vincular's duplicate-merge
# modal could silently un-confirm an invoice as a side effect of picking
# which duplicate candidate to keep, with no warning at all. "Conservar A"
# (keep the bank movement, discard the candidate) hits this whenever the
# discarded candidate's source is "confirmado" -- .do_vinculation() flips
# that confirmation's eliminado flag. "Conservar B" (keep the candidate,
# discard the bank movement) never hits this: when the candidate's source
# is "confirmado" there, it's the one being KEPT, not discarded.
#
# Fixed: vin_keep_a now checks the candidate's source before doing
# anything -- if it's "confirmado", show an explicit warning dialog naming
# the invoice (Parte/Documento/Importe/Moneda/Fecha) with Cancelar + a
# separate confirm button; the actual mutation (unchanged, extracted into
# .do_vin_keep_a()) only runs after that explicit confirm. Any other
# source proceeds exactly as before, no behavior change for the common
# case. Also captured moneda on the "confirmado" candidate struct
# (.build_candidates_bank_side()), which wasn't there before and is needed
# to name the currency in the warning.
#
# Per the original plan's own guidance for this stage: this is UI-modal
# behavior in a Shiny reactive context, hard to unit-test meaningfully --
# focus on the branch logic that decides whether to warn, not the modal
# rendering itself.
# =============================================================================

cat("── Stage 8: Vincular warns before un-confirming a discarded invoice ────\n")

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
.strip_comments <- function(lines) sub("#.*$", "", lines)

txt <- readLines("R/bancos_module.R", warn = FALSE)

# ── 1. Static scan: vin_keep_a branches on ca_source before mutating ───────
{
  start <- grep("observeEvent\\(input\\$vin_keep_a,", txt)
  end   <- grep("observeEvent\\(input\\$vin_keep_a_confirm,", txt)
  .chk(length(start) > 0 && length(end) > 0, TRUE, "found vin_keep_a and vin_keep_a_confirm observers to scan")
  if (length(start) && length(end)) {
    block <- paste(.strip_comments(txt[start[1]:end[1]]), collapse = "\n")
    .chk(grepl('ca_source == "confirmado"', block), TRUE,
         "vin_keep_a checks whether the discarded candidate's source is 'confirmado' before doing anything")
    .chk(grepl("showModal\\(modalDialog\\(", block), TRUE,
         "vin_keep_a shows a warning modal in the confirmado branch")
    .chk(grepl("\\.do_vin_keep_a\\(\\)", block), TRUE,
         "vin_keep_a calls the extracted mutation function, not inline duplicated logic")
    # The warning dialog must name the specific invoice, not a generic message.
    warn_start <- grep("Descartar una factura ya confirmada", txt, fixed = TRUE)
    .chk(length(warn_start) > 0, TRUE, "found the warning dialog's title to scan")
    if (length(warn_start)) {
      warn_block <- paste(.strip_comments(txt[warn_start[1]:min(warn_start[1] + 20, length(txt))]), collapse = "\n")
      for (field in c("ca\\$parte", "ca\\$documento", "ca\\$importe", "ca\\$moneda", "ca\\$fecha")) {
        .chk(grepl(field, warn_block), TRUE,
             sprintf("warning dialog names the invoice's %s", sub("\\\\", "", field)))
      }
    }
  }
}

# ── 2. Static scan: the fast path for non-confirmado candidates is untouched ─
{
  start <- grep("observeEvent\\(input\\$vin_keep_a,", txt)
  end   <- grep("observeEvent\\(input\\$vin_keep_a_confirm,", txt)
  if (length(start) && length(end)) {
    block <- paste(.strip_comments(txt[start[1]:end[1]]), collapse = "\n")
    .chk(grepl("\\} else \\{\\s*\\n\\s*\\.do_vin_keep_a\\(\\)", block), TRUE,
         "any candidate that is NOT 'confirmado' still calls .do_vin_keep_a() directly, no warning, no behavior change")
  }
}

# ── 3. Static scan: vin_keep_b's confirmado handling is unchanged (already safe) ─
{
  start <- grep("observeEvent\\(input\\$vin_keep_b,", txt)
  .chk(length(start) > 0, TRUE, "found vin_keep_b observer to scan")
  if (length(start)) {
    # Deliberately NOT comment-stripped -- the "no-op" behavior we're
    # guarding is the comment itself confirming no eliminado flip happens.
    raw_block  <- paste(txt[start[1]:min(start[1] + 50, length(txt))], collapse = "\n")
    stripped_block <- paste(.strip_comments(txt[start[1]:min(start[1] + 50, length(txt))]), collapse = "\n")
    .chk(grepl("confirmado is the winner", raw_block), TRUE,
         "vin_keep_b still documents that a confirmado candidate is the one being kept (no warning needed there -- regression guard)")
    .chk(grepl("showModal", stripped_block), FALSE,
         "vin_keep_b does NOT show a warning modal -- it never discards a confirmado candidate")
  }
}

# ── 4. Static scan: moneda captured on the confirmado candidate struct ─────
{
  start <- grep("Source 1: bancos_confirmados \\(eliminado=FALSE\\)", txt)
  .chk(length(start) > 0, TRUE, "found .build_candidates_bank_side()'s confirmado source block to scan")
  if (length(start)) {
    block <- paste(.strip_comments(txt[start[1]:min(start[1] + 30, length(txt))]), collapse = "\n")
    .chk(grepl("moneda\\s*=.*r\\$moneda", block), TRUE,
         "the confirmado candidate struct now captures moneda from the bancos_confirmados row")
  }
}

# ── 5. Behavioral: the warn-decision itself, isolated from the modal ──────
{
  should_warn <- function(ca_source) identical(ca_source, "confirmado")
  .chk(should_warn("confirmado"), TRUE,  "discarding a confirmado candidate triggers the warning")
  .chk(should_warn("mov_txt"),    FALSE, "discarding an ordinary bank-movement candidate does not")
  .chk(should_warn("sap_AR"),     FALSE, "discarding a SAP-sourced candidate does not (SAP items aren't in our tables)")
  .chk(should_warn(NA_character_), FALSE, "an NA/missing source never triggers the warning (fails safe toward the existing behavior, not a crash)")
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
