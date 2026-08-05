# =============================================================================
# tests/test_calendario_stage_sel_chromote.R
# Stage 9 follow-up (2026-08-04): live-browser proof for the "Agregar
# selección" fix (see tests/test_calendario_stage_sel_provision_guard.R for
# the full incident writeup and the static/functional half of verification).
#
# This drives a real headless Chrome (chromote) against a small, isolated
# fixture app (never the full app.R -- see tests/helpers/live_app_harness.R's
# header for why) that reproduces the day-view modal's audit-mode table +
# "Agregar selección" button, wired to the REAL, unmodified, extracted
# production functions that make up the actual fix:
#   pasivos_exclude_provision_keys() and pasivos_filter_out_provisions()
#   (R/pasivos_calendar_glue.R), upsert_pagar_hoy() (R/persistence.R).
# The DT-selection-to-dataframe glue around them is simple, obvious fixture
# code (not itself part of the bug or the fix); the security-relevant
# provision-exclusion logic under test is the real production code.
#
# Proves, live in a browser: (a) selecting a real invoice AND a provision
# together and clicking "Agregar selección" does not crash the app, (b) only
# the real invoice ends up in the resulting Agenda (pagar_hoy-equivalent)
# table, (c) the day's hourglass-equivalent badge count reflects only the
# real invoice, not the excluded provision.
# =============================================================================
cat("=== test_calendario_stage_sel_chromote ===\n\n")

.pass <- 0L; .fail <- 0L; .skip <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) {
    cat(" PASS:", label, "\n"); .pass <<- .pass + 1L
  } else {
    cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L
  }
}

.deps_ok <- all(vapply(
  c("chromote", "callr", "curl", "jsonlite", "shiny", "DT", "dplyr"),
  requireNamespace, logical(1), quietly = TRUE
))
.chrome_ok <- .deps_ok && !inherits(tryCatch(chromote::find_chrome(), error = function(e) e), "error")

if (!.chrome_ok) {
  cat(" SKIP: chromote/Chrome not available in this environment -- skipping live browser test.\n")
  .skip <- 1L
  cat("\n=== results:", .pass, "passed,", .fail, "failed,", .skip, "skipped ===\n")
  invisible(NULL)
} else {

source("tests/helpers/live_app_harness.R")

# ── Extract the REAL production functions (not hand-copied mirrors) ────────
glue_txt  <- readLines("R/pasivos_calendar_glue.R", warn = FALSE)
pers_txt  <- readLines("R/persistence.R", warn = FALSE)

.extract_block <- function(txt, fn_name) {
  start <- grep(sprintf("^%s <- function", fn_name), txt)
  if (!length(start)) stop(sprintf("%s not found", fn_name))
  # naive brace-matching from the start line
  depth <- 0L; end <- start[1]
  for (i in start[1]:length(txt)) {
    depth <- depth + lengths(regmatches(txt[i], gregexpr("\\{", txt[i]))) -
                     lengths(regmatches(txt[i], gregexpr("\\}", txt[i])))
    if (i > start[1] && depth <= 0) { end <- i; break }
  }
  paste(txt[start[1]:end], collapse = "\n")
}

pasivos_filter_fn  <- .extract_block(glue_txt, "pasivos_filter_out_provisions")
pasivos_exclude_fn <- .extract_block(glue_txt, "pasivos_exclude_provision_keys")
upsert_fn          <- .extract_block(pers_txt, "upsert_pagar_hoy")

ok("extracted pasivos_filter_out_provisions() from the real source", nzchar(pasivos_filter_fn))
ok("extracted pasivos_exclude_provision_keys() from the real source", nzchar(pasivos_exclude_fn))
ok("extracted upsert_pagar_hoy() from the real source", nzchar(upsert_fn))

app_code <- sprintf('
library(shiny)
library(DT)
library(dplyr)

`%%||%%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x

%s

%s

%s

detail <- data.frame(
  Empresa = c("NCS", "NCS"), Moneda = c("MXN", "MXN"),
  Documento = c("SAP-100", "PROV-1"),
  Parte = c("Proveedor A", "Proveedor B"),
  Codigo = c("C001", NA_character_),
  Importe = c(1000, 500),
  source = c("sap", "provision"),
  provision_id = c(NA_character_, "prov-uuid-1"),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  DT::dataTableOutput("modal_tbl"),
  actionButton("stage_sel", "Agregar selección"),
  hr(),
  uiOutput("hourglass"),
  DT::dataTableOutput("agenda_tbl")
)

server <- function(input, output, session) {
  ph <- reactiveVal(data.frame(
    id = character(), ledger = character(), Empresa = character(),
    Moneda = character(), Documento = character(), Importe = numeric(),
    Parte = character(), Codigo = character(), status = character(),
    stringsAsFactors = FALSE
  ))

  output$modal_tbl <- DT::renderDataTable({
    DT::datatable(detail[, c("Empresa","Documento","Parte","Importe")], selection = "multiple", rownames = FALSE)
  })

  observeEvent(input$stage_sel, {
    sel <- input$modal_tbl_rows_selected %%||%% integer(0)
    if (!length(sel)) return()
    # DT selection -> keys, same SHAPE .audit_sel_to_keys() returns (this
    # glue is simple fixture code; the security-relevant logic under test
    # is pasivos_exclude_provision_keys() itself, extracted verbatim above).
    keys <- unique(detail[sel, c("Empresa","Moneda","Documento","Importe","provision_id"), drop = FALSE])
    keys <- pasivos_exclude_provision_keys(keys, detail)
    if (is.null(keys) || !nrow(keys)) return()
    new_rows <- merge(keys[, c("Empresa","Moneda","Documento","Importe"), drop = FALSE],
                      detail[, c("Empresa","Moneda","Documento","Importe","Parte","Codigo"), drop = FALSE],
                      by = c("Empresa","Moneda","Documento","Importe"))
    new_rows$id <- paste0("row-", seq_len(nrow(new_rows)))
    new_rows$ledger <- "AP"
    new_rows$status <- "pending"
    updated <- upsert_pagar_hoy(ph(), new_rows, keys = c("ledger","Empresa","Moneda","Documento","Importe"))
    ph(updated)
  }, ignoreInit = TRUE)

  output$agenda_tbl <- DT::renderDataTable({
    DT::datatable(ph()[, c("Documento","Parte","Importe")], rownames = FALSE)
  })

  output$hourglass <- renderUI({
    n <- nrow(ph())
    if (n > 0) tags$span(id = "hourglass_badge", class = "badge", paste0("\\u23f3 ", n))
    else tags$span(id = "hourglass_badge", "no-hourglass")
  })
}

shinyApp(ui, server)
', pasivos_filter_fn, pasivos_exclude_fn, upsert_fn)

PORT <- 38929L
app <- NULL
s1 <- NULL
on.exit({
  try(if (!is.null(s1)) s1$close(), silent = TRUE)
  stop_bg_app(app)
}, add = TRUE)

app <- tryCatch(start_bg_app(app_code, PORT), error = function(e) { message("startup error: ", e$message); NULL })
ok("fixture app started and became reachable", !is.null(app))

if (!is.null(app)) {
  base_url <- sprintf("http://127.0.0.1:%d/", PORT)
  s1 <- new_chromote_session()
  s1$Page$navigate(base_url, wait_ = TRUE)
  ok("modal table renders with both rows (real invoice + provision)",
     poll_until(function() grepl("SAP-100", body_text(s1)) && grepl("PROV-1", body_text(s1))))

  # Select BOTH DT rows (real invoice row 1 + provision row 2) via Shiny's
  # own input channel -- what's under test here is the SERVER-side provision
  # exclusion logic, not DT's client-side row-click UX, so setting the
  # selection input directly is more robust than simulating DOM clicks on
  # DT's internally-rendered rows. Then click "Agregar selección", the exact
  # scenario Mouse reported crashing.
  ok("selecting both rows and clicking Agregar selección does not crash the app", {
    set_input_value(s1, "modal_tbl_rows_selected", c(1, 2))
    Sys.sleep(0.3)
    click_selector(s1, "#stage_sel")
    poll_until(function() {
      v <- s1$Runtime$evaluate("document.querySelector('#agenda_tbl') ? document.querySelector('#agenda_tbl').innerText : null")$result$value
      !is.null(v) && nzchar(v)
    }, timeout_s = 8)
  })

  ok("the app is still alive after clicking (no red Shiny error overlay)",
     !grepl("An error has occurred", body_text(s1)))

  # Scope these checks to #agenda_tbl specifically -- #modal_tbl (the
  # SELECTION table) always shows PROV-1 regardless of the fix, since it
  # displays the unfiltered `detail`; checking the whole page body would
  # give a false failure there, not a real one.
  agenda_tbl_text <- function(session) {
    v <- session$Runtime$evaluate("document.querySelector('#agenda_tbl').innerText")$result$value
    if (is.null(v)) "" else v
  }
  ok("the resulting Agenda table contains the real invoice (SAP-100)",
     poll_until(function() grepl("SAP-100", agenda_tbl_text(s1))))
  ok("the resulting Agenda table does NOT contain the provision (PROV-1) -- the actual fix",
     !grepl("PROV-1", agenda_tbl_text(s1)))

  hourglass_text <- function(session) {
    v <- session$Runtime$evaluate("document.getElementById('hourglass_badge') ? document.getElementById('hourglass_badge').innerText : ''")$result$value
    if (is.null(v)) "" else v
  }
  ok("the hourglass-equivalent badge shows exactly 1 (only the real invoice, not the provision)",
     poll_until(function() grepl("1", hourglass_text(s1)) && !grepl("2", hourglass_text(s1))))
}

cat("\n=== results:", .pass, "passed,", .fail, "failed,", .skip, "skipped ===\n")
if (.fail > 0) stop("Tests FAILED.")
invisible(NULL)
}
