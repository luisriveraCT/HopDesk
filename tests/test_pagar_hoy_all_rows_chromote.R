# =============================================================================
# tests/test_pagar_hoy_all_rows_chromote.R
# Agenda de Hoy's AP (pagos) and AR (cobros) tables use dom = "t", which
# hides DataTables' pager -- but they also set pageLength = 50, so with more
# than 50 queued payments rows 51+ were silently never drawn and could not be
# selected. They now use paging = FALSE.
#
# Two halves:
#   1. Static: the two data-bearing tables in R/pagar_hoy_module.R turn
#      paging off (the empty-state placeholders are irrelevant: 0 rows).
#   2. Live (chromote): a fixture DT with 60 rows and the same options as the
#      AP table draws all 60 rows, and clicking row 55 selects row 55. The
#      same fixture with the old options is drawn too, to show the bug: 50.
#
# Run from project root:  Rscript tests/test_pagar_hoy_all_rows_chromote.R
# =============================================================================
cat("=== test_pagar_hoy_all_rows_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# ── 1. Static ───────────────────────────────────────────────────────────────
src <- readLines("R/pagar_hoy_module.R", warn = FALSE)
ap_line <- grep('scrollY = "200px"', src, fixed = TRUE)
ar_line <- grep('language = list(emptyTable = "Sin cobros esperados"),', src, fixed = TRUE)
ok("AP table options turn paging off",
   length(ap_line) == 1L && grepl("paging = FALSE", src[ap_line]))
ok("AR table options turn paging off",
   length(ar_line) == 1L && grepl("paging = FALSE", src[ar_line - 2L]))

# ── 2. Live ─────────────────────────────────────────────────────────────────
source("tests/helpers/live_app_harness.R")

app_code <- '
library(shiny)
df <- data.frame(Proveedor = sprintf("PROV-%02d", 1:60), Importe = 1:60)
opts_new <- list(paging = FALSE,   dom = "t", scrollX = TRUE, scrollY = "200px")
opts_old <- list(pageLength = 50,  dom = "t", scrollX = TRUE, scrollY = "200px")
ui <- fluidPage(DT::DTOutput("tbl_new"), DT::DTOutput("tbl_old"),
                verbatimTextOutput("sel"))
server <- function(input, output, session) {
  output$tbl_new <- DT::renderDataTable(DT::datatable(df, rownames = FALSE,
    selection = list(mode = "multiple"), options = opts_new), server = FALSE)
  output$tbl_old <- DT::renderDataTable(DT::datatable(df, rownames = FALSE,
    selection = list(mode = "multiple"), options = opts_old), server = FALSE)
  output$sel <- renderText(paste0("SEL=", paste(input$tbl_new_rows_selected, collapse = ",")))
}
shinyApp(ui, server)
'

PORT <- 38931L
app <- NULL; s1 <- NULL
on.exit({ try(if (!is.null(s1)) s1$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)

app <- tryCatch(start_bg_app(app_code, PORT), error = function(e) { message(e$message); NULL })
ok("fixture app started", !is.null(app))

if (!is.null(app)) {
  s1 <- new_chromote_session()
  s1$Page$navigate(sprintf("http://127.0.0.1:%d/", PORT), wait_ = TRUE)
  n_rows <- function(id) s1$Runtime$evaluate(sprintf(
    "document.querySelectorAll('#%s tbody tr').length", id))$result$value
  poll_until(function() isTRUE(n_rows("tbl_new") > 0) && isTRUE(n_rows("tbl_old") > 0))

  cat(sprintf("  rows drawn -- old options: %s, new options: %s\n",
              n_rows("tbl_old"), n_rows("tbl_new")))
  ok("old options (pageLength = 50, dom = 't') draw only 50 of 60 rows (the bug)",
     identical(n_rows("tbl_old"), 50L))
  ok("new options (paging = FALSE) draw all 60 rows", identical(n_rows("tbl_new"), 60L))

  # DT selects rows on mousedown (not click), see DT's datatables.js.
  s1$Runtime$evaluate("document.querySelectorAll('#tbl_new tbody tr')[54].querySelector('td')
    .dispatchEvent(new MouseEvent('mousedown', {bubbles: true, button: 0}))")
  ok("clicking the 55th row selects row 55",
     poll_until(function() identical(trimws(s1$Runtime$evaluate(
       "document.getElementById('sel').innerText")$result$value %||% ""), "SEL=55")))
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
