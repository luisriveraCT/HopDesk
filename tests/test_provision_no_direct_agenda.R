# =============================================================================
# tests/test_provision_no_direct_agenda.R
# Real bug found 2026-07-24 while live-testing this session's other work:
# the "+ Provisión manual" creation modal (R/pasivos_table_module.R) had a
# second save button, "Agregar a calendario y Agenda de hoy" (ppm_save_and_
# stage), that staged the brand-new provision directly into pagar_hoy_db --
# a direct violation of Mouse's core rule that a provision may NEVER enter
# Agenda on its own; only its converted derivative manual_inv item can be
# staged, and only via the separate convert-to-comprobante modal. Confirmed
# live: clicking it created a real pagar_hoy row (source="provision") with
# no corresponding manual_inv row at all -- exactly the state the rule
# exists to prevent.
#
# Fix: removed the button and its entire observer. The modal now has exactly
# one save action ("Agregar a calendario"), which only ever writes to
# pasivos_provisions_db -- never pagar_hoy_db.
#
# This is a static source scan, not a Shiny testServer() integration test:
# the removed observer's own absence is what's being asserted, and the
# remaining .ppm_build_new_prov()/ppm_save observer read from input$...
# reactive values that require a live session to exercise meaningfully.
# =============================================================================

cat("── Provision creation never stages directly to Agenda ──────────────────\n")

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

txt <- readLines("R/pasivos_table_module.R", warn = FALSE)
stripped <- sub("#.*$", "", txt)
joined <- paste(stripped, collapse = "\n")

.chk(grepl("ppm_save_and_stage", joined), FALSE,
     "ppm_save_and_stage (button id, observer, disable/enable calls) is fully removed, not just hidden")
.chk(grepl("\\bagregar a calendario y agenda\\b", tolower(joined)), FALSE,
     "no button text offering to stage a fresh provision straight to Agenda remains")
.chk(grepl("save_pagar_hoy\\(", joined), FALSE,
     "R/pasivos_table_module.R never writes to pagar_hoy_db anywhere (provision creation is calendar-only)")

# The modal footer must have exactly one actionButton besides Cancelar.
modal_start <- grep("\\.ppm_modal_ui <- function", txt)
.chk(length(modal_start) > 0, TRUE, "found .ppm_modal_ui() to scan")
if (length(modal_start)) {
  footer_block <- paste(stripped[modal_start[1]:min(modal_start[1] + 45, length(stripped))], collapse = "\n")
  n_buttons <- length(gregexpr("actionButton\\(", footer_block)[[1]])
  n_buttons <- if (n_buttons == -1) 0L else n_buttons
  .chk(n_buttons, 1L, "the create-provision modal has exactly one actionButton (ppm_save) besides Cancelar")
}

# .ppm_build_new_prov() no longer takes a pagar_hoy_id parameter -- the field
# is hardcoded NA_character_ in the tibble now, not a caller-supplied value.
sig_line <- grep("\\.ppm_build_new_prov <- function", txt)
.chk(length(sig_line) > 0, TRUE, "found .ppm_build_new_prov() to scan")
if (length(sig_line)) {
  sig_block <- paste(stripped[sig_line[1]:min(sig_line[1] + 3, length(stripped))], collapse = " ")
  .chk(grepl("pagar_hoy_id", sig_block), FALSE,
       ".ppm_build_new_prov()'s signature no longer accepts a pagar_hoy_id argument")
}
.chk(grepl("pagar_hoy_id\\s*=\\s*NA_character_", joined), TRUE,
     "the tibble's pagar_hoy_id field is hardcoded NA_character_ (a freshly-created provision never has one)")

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
