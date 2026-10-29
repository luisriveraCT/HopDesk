# =============================================================================
# tests/test_ledger_cart_group_click_chromote.R
# Calendario day modal: the group-level ＋ (groups with 2+ invoices) used to
# fire ns("cart_<i>"), and ledgerModuleServer registered observers for
# cart_1..cart_50 only, once at init -- on a day with more than 50 groups the
# ＋ of group 51+ had no observer and silently did nothing. It now fires one
# delegated input, ns("cart_group_click") with {i, nonce}, served by a single
# observer.
#
# Two halves:
#   1. Static: the real R/ledger_module.R has no per-index cart_<i> observers
#      or MAX_CART_ROWS left, has exactly one cart_group_click observer, and
#      the group button emits cart_group_click with its index.
#   2. Live (chromote): a module fixture (namespaced, like ledgerModuleServer
#      "ar"/"ap") renders 60 group buttons using the onclick template
#      EXTRACTED from the real source, and listens on the input name the real
#      observer uses. Clicking the ＋ of group 55 must deliver i = 55, exactly
#      once per click.
#
# Run from project root:  Rscript tests/test_ledger_cart_group_click_chromote.R
# =============================================================================
cat("=== test_ledger_cart_group_click_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# ── 1. Static ───────────────────────────────────────────────────────────────
src <- readLines("R/ledger_module.R", warn = FALSE)
ok("no per-index cart_<i> observers left",
   !any(grepl('input\\[\\[paste0\\("cart_", i\\)\\]\\]', src)))
ok("no MAX_CART_ROWS cap left", !any(grepl("MAX_CART_ROWS", src, fixed = TRUE)))
ok("exactly one observeEvent(input$cart_group_click, ...)",
   sum(grepl("observeEvent(input$cart_group_click,", src, fixed = TRUE)) == 1L)

# The group ＋ button: the sprintf() whose id argument is ns("cart_group_click").
id_line <- grep('ns("cart_group_click"), i', src, fixed = TRUE)
ok("found the group button's onclick in the real source", length(id_line) == 1L)
tmpl <- if (length(id_line) == 1L)
  eval(parse(text = trimws(sub(",\\s*$", "", src[id_line - 1L])))) else NA_character_
cat("  extracted onclick template:", tmpl, "\n")
ok("onclick template sends {i:%d, ...} with priority 'event'",
   grepl("{i:%d", tmpl, fixed = TRUE) && grepl("priority:'event'", tmpl, fixed = TRUE))

# ── 2. Live ─────────────────────────────────────────────────────────────────
source("tests/helpers/live_app_harness.R")

app_code <- sprintf('
library(shiny)
tmpl <- %s
cartUI <- function(id) {
  ns <- NS(id)
  tagList(
    lapply(1:60, function(i) tags$button(
      id = paste0("grp_btn_", i), class = "btn btn-sm",
      onclick = sprintf(tmpl, ns("cart_group_click"), i), "\\uff0b")),
    verbatimTextOutput(ns("log"))
  )
}
cartServer <- function(id) moduleServer(id, function(input, output, session) {
  hits <- reactiveVal(integer(0))
  observeEvent(input$cart_group_click, {
    hits(c(hits(), as.integer(input$cart_group_click$i)))
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  output$log <- renderText(paste0("HITS=", paste(hits(), collapse = ",")))
})
ui <- fluidPage(cartUI("ap"))
server <- function(input, output, session) cartServer("ap")
shinyApp(ui, server)
', deparse(tmpl))

PORT <- 38935L
app <- NULL; s1 <- NULL
on.exit({ try(if (!is.null(s1)) s1$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)

app <- tryCatch(start_bg_app(app_code, PORT), error = function(e) { message(e$message); NULL })
ok("fixture app started", !is.null(app))

if (!is.null(app)) {
  s1 <- new_chromote_session()
  s1$Page$navigate(sprintf("http://127.0.0.1:%d/", PORT), wait_ = TRUE)
  log_txt <- function() {
    v <- s1$Runtime$evaluate("(document.getElementById('ap-log')||{}).innerText || ''")$result$value
    if (is.null(v)) "" else v
  }
  poll_until(function() grepl("HITS=", log_txt(), fixed = TRUE))

  s1$Runtime$evaluate("document.getElementById('grp_btn_55').click()")
  ok("clicking the ＋ of group 55 delivers i = 55, once",
     poll_until(function() identical(trimws(log_txt()), "HITS=55")))
  Sys.sleep(0.5)
  ok("no duplicate delivery after settling", identical(trimws(log_txt()), "HITS=55"))

  s1$Runtime$evaluate("document.getElementById('grp_btn_55').click()")
  ok("a second click on the same ＋ is delivered again (toggle), not swallowed",
     poll_until(function() identical(trimws(log_txt()), "HITS=55,55")))

  s1$Runtime$evaluate("document.getElementById('grp_btn_3').click()")
  ok("group 3 still works and delivers i = 3",
     poll_until(function() identical(trimws(log_txt()), "HITS=55,55,3")))
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
