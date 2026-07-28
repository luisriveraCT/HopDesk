# =============================================================================
# tests/test_vencidos_confirm_bar_fixed_position.R
# Real bug found 2026-07-24: Vencidos' "Agregar todo" / "Agregar selección" /
# "Eliminar" buttons all route through a confirm bar (#ven_stage_confirm_bar
# or #ven_delete_confirm_bar) before actually calling Shiny.setInputValue.
# Both bars rendered in normal document flow, right after the sticky header
# -- so once the invoice list was scrolled down (as it always is for a list
# with 1000+ rows), the bar's normal-flow position scrolled up along with
# the page and ended up hidden above the viewport or behind the sticky
# header. Clicking the button looked like it did nothing, because the
# confirmation step it needed was invisible, not because the click failed.
#
# Fixed by giving both bars the same `position: fixed` treatment
# #ven_sel_bubble already uses for exactly this reason.
#
# Static source scan -- this is a CSS/scroll-behavior bug, not something a
# unit test can exercise without a real browser viewport.
# =============================================================================

cat("── Vencidos confirm bars stay visible regardless of scroll position ────\n")

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

txt <- readLines("R/vencidos_module.R", warn = FALSE)
joined <- paste(txt, collapse = "\n")

rule_line <- grep("#ven_stage_confirm_bar,\\s*#ven_delete_confirm_bar", txt)
.chk(length(rule_line) > 0, TRUE,
     "found the shared CSS rule targeting both confirm bars")
if (length(rule_line)) {
  block <- paste(txt[rule_line[1]:min(rule_line[1] + 6, length(txt))], collapse = "\n")
  .chk(grepl("position:\\s*fixed", block), TRUE,
       "both confirm bars are taken out of normal document flow (position: fixed)")
  .chk(grepl("z-index:\\s*9999", block), TRUE,
       "both confirm bars sit above the sticky header's own z-index (200)")
}

# Both bar ids must still exist in the UI (guard against a copy-paste error
# that renamed or removed one while adding the CSS rule).
for (bar_id in c("ven_stage_confirm_bar", "ven_delete_confirm_bar")) {
  .chk(grepl(sprintf('id\\s*=\\s*"%s"', bar_id), joined), TRUE,
       sprintf("%s is still rendered in the UI", bar_id))
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
