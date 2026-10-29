# =============================================================================
# tests/test_vencidos_select_all_ghosts_chromote.R
# Vencidos shows already-paid SAP invoices as greyed "ghost" rows
# (tr.ven-row.ven-confirmed-ghost, R/vencidos_module.R). "Seleccionar todos",
# "Agregar todo" and a group-header click used to select / stage them too, so
# paid invoices could be sent to Agenda and were counted in the selection
# bubble. This drives the REAL www/vencidos.js in headless Chrome against a
# fixture table with 2 open invoices + 1 ghost (all in one group) and checks
# that ghosts are never selected, counted or staged -- while a group-header
# click still selects every (open) invoice of the group.
#
# Run from project root:  Rscript tests/test_vencidos_select_all_ghosts_chromote.R
# To see the old behaviour, point VEN_JS at an older copy of vencidos.js:
#   git show d527db2:www/vencidos.js > old_ven.js
#   VEN_JS=old_ven.js Rscript tests/test_vencidos_select_all_ghosts_chromote.R
# =============================================================================
cat("=== test_vencidos_select_all_ghosts_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

source("tests/helpers/live_app_harness.R")

js_file <- normalizePath(Sys.getenv("VEN_JS", "www/vencidos.js"), winslash = "/")
cat("  using", js_file, "\n")

app_code <- sprintf('
library(shiny)
js_dir <- dirname("%s")
addResourcePath("venjs", js_dir)
tr <- function(doc, imp, ghost = FALSE)
  sprintf(paste0(
    "<tr class=\\"ven-row ven-subrow%%s\\" data-group-id=\\"g1\\" data-tipo=\\"Pagos\\" ",
    "data-ledger=\\"AP\\" data-moneda=\\"MXN\\" data-tag=\\"\\" data-importe=\\"%%s\\" ",
    "data-fecha=\\"2026-10-01\\" data-parte=\\"acme\\" data-parteraw=\\"ACME\\" ",
    "data-doc=\\"%%s\\" data-ref=\\"\\" data-empresa=\\"E1\\" data-documento=\\"%%s\\" ",
    "data-source=\\"sap\\" data-invid=\\"\\" data-provisionid=\\"\\" data-codigo=\\"\\">",
    "<td>%%s</td></tr>"),
    if (ghost) " ven-confirmed-ghost" else "", imp, tolower(doc), doc, doc)
ui <- fluidPage(
  tags$input(id = "ven_search_text", value = ""),
  tags$button(id = "ven_sel_all_btn"), tags$button(id = "ven_sel_none_btn"),
  div(id = "ven_sel_bubble", style = "display:none"),
  div(id = "ven_stage_confirm_bar", style = "display:none",
      tags$span(id = "ven_stage_confirm_msg")),
  tags$table(tags$tbody(id = "ven_tbody_AP_MXN", HTML(paste0(
    "<tr class=\\"ven-group-row\\" data-group-id=\\"g1\\"><td>Grupo ACME</td></tr>",
    tr("DOC-OPEN-1", 100), tr("DOC-PAID", 999, ghost = TRUE), tr("DOC-OPEN-2", 50))))),
  tags$script(src = "venjs/%s")
)
shinyApp(ui, function(input, output, session) {})
', js_file, basename(js_file))

PORT <- 38933L
app <- NULL; s1 <- NULL
on.exit({ try(if (!is.null(s1)) s1$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)

app <- tryCatch(start_bg_app(app_code, PORT), error = function(e) { message(e$message); NULL })
ok("fixture app started", !is.null(app))

if (!is.null(app)) {
  s1 <- new_chromote_session()
  s1$Page$navigate(sprintf("http://127.0.0.1:%d/", PORT), wait_ = TRUE)
  js <- function(code) s1$Runtime$evaluate(code)$result$value
  ok("vencidos.js loaded", poll_until(function() isTRUE(js("typeof window.venSelectAll === 'function'"))))

  sel_docs <- function() js("Array.from(document.querySelectorAll('.ven-row.ven-row-selected')).map(function(r){return r.dataset.documento;}).sort().join(',')")

  # Select all
  js("venSelectAll()")
  cat("  after venSelectAll, selected:", sel_docs(), "\n")
  ok("Seleccionar todos selects both open invoices and not the paid one",
     identical(sel_docs(), "DOC-OPEN-1,DOC-OPEN-2"))
  bubble <- js("document.getElementById('ven_sel_bubble').innerText")
  cat("  bubble:", gsub("\n", " | ", bubble), "\n")
  ok("selection bubble counts 2 invoices", grepl("2 facturas seleccionadas", bubble, fixed = TRUE))
  ok("selection bubble sums 150, not 1,149",
     grepl("150.00", bubble, fixed = TRUE) && !grepl("1,149", bubble, fixed = TRUE))
  js("venSelectNone()")

  # Agregar todo
  js("venStageAll()")
  staged <- js("JSON.parse(document.getElementById('ven_stage_confirm_bar').dataset.pendingRows || '[]').map(function(r){return r.documento;}).sort().join(',')")
  cat("  venStageAll pending rows:", staged, "\n")
  ok("Agregar todo stages only the open invoices", identical(staged, "DOC-OPEN-1,DOC-OPEN-2"))
  ok("Agregar todo message says 2", grepl("Agregar 2 ", js("document.getElementById('ven_stage_confirm_msg').textContent"), fixed = TRUE))

  # Group-header click (decided behaviour: selects every invoice of the group)
  js("document.querySelector('.ven-group-row td').click()")
  cat("  after group click, selected:", sel_docs(), "\n")
  ok("group-header click still selects every open invoice of the group, not the paid one",
     identical(sel_docs(), "DOC-OPEN-1,DOC-OPEN-2"))
  ok("group header shows as fully selected",
     isTRUE(js("document.querySelector('.ven-group-row').classList.contains('ven-group-all-selected')")))
  js("document.querySelector('.ven-group-row td').click()")
  ok("second group-header click deselects them", identical(sel_docs(), ""))
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
