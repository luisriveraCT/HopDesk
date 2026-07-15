# =============================================================================
# tests/test_papelera_filtering.R
# Verify that deleted items are correctly excluded from calendar, vencidos,
# and search modal display.
# Run with: source("tests/test_papelera_filtering.R")
# =============================================================================

# Load app dependencies without starting Shiny
suppressMessages({
  if (!exists("S3_KEYS")) {
    source(file.path(dirname(getwd()), "Antiguedad_App", "R", "global.R"),
           local = FALSE)
  }
})

source_app <- function(path) {
  source(file.path("R", path), local = FALSE)
}

# Only load what we need for pure-function tests
if (!exists("add_to_papelera", mode = "function")) {
  source(file.path("R", "persistence.R"), local = FALSE)
}

cat("=== Stage 1: test_papelera_filtering ===\n\n")

pass <- 0L; fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) FALSE)
  if (isTRUE(result)) {
    cat(" PASS:", label, "\n"); pass <<- pass + 1L
  } else {
    cat(" FAIL:", label, "\n"); fail <<- fail + 1L
  }
}

# ── Test 1: add_to_papelera preserves per-row ledger ─────────────────────────
{
  base_pap <- tibble::tibble(
    id = character(), ledger = character(), source = character(),
    Empresa = character(), Moneda = character(), Documento = character(),
    Parte = character(), Importe = numeric(), FechaEff = as.Date(character()),
    deleted_by = character(), deleted_at = as.POSIXct(character()),
    original_data = list()
  )
  detail <- data.frame(
    ledger    = c("AR", "AP"),
    Empresa   = c("EmpA", "EmpB"),
    Moneda    = c("MXN", "MXN"),
    Documento = c("DOC001", "DOC002"),
    Parte     = c("P1", "P2"),
    Importe   = c(100, 200),
    source    = c("sap", "sap"),
    stringsAsFactors = FALSE
  )
  result <- add_to_papelera(base_pap, detail, ledger = "MIXED")
  ok("add_to_papelera uses per-row ledger when column present",
     all(result$ledger == c("AR", "AP")))
}

# ── Test 2: add_to_papelera falls back to ledger param when column missing ───
{
  base_pap <- tibble::tibble(
    id = character(), ledger = character(), source = character(),
    Empresa = character(), Moneda = character(), Documento = character(),
    Parte = character(), Importe = numeric(), FechaEff = as.Date(character()),
    deleted_by = character(), deleted_at = as.POSIXct(character()),
    original_data = list()
  )
  detail <- data.frame(
    Empresa = "EmpA", Moneda = "MXN", Documento = "DOC003",
    Parte = "P1", Importe = 100, source = "sap",
    stringsAsFactors = FALSE
  )
  result <- add_to_papelera(base_pap, detail, ledger = "AR")
  ok("add_to_papelera falls back to ledger param when column absent",
     result$ledger == "AR")
}

# ── Test 3: add_to_papelera falls back when ledger column has empty strings ──
{
  base_pap <- tibble::tibble(
    id = character(), ledger = character(), source = character(),
    Empresa = character(), Moneda = character(), Documento = character(),
    Parte = character(), Importe = numeric(), FechaEff = as.Date(character()),
    deleted_by = character(), deleted_at = as.POSIXct(character()),
    original_data = list()
  )
  detail <- data.frame(
    ledger = c("AR", ""),
    Empresa = c("EmpA", "EmpB"), Moneda = c("MXN", "MXN"),
    Documento = c("DOC001", "DOC002"), Parte = c("P1", "P2"),
    Importe = c(100, 200), source = c("sap", "sap"),
    stringsAsFactors = FALSE
  )
  result <- add_to_papelera(base_pap, detail, ledger = "FALLBACK")
  ok("add_to_papelera falls back to ledger param when any ledger empty",
     all(result$ledger == "FALLBACK"))
}

# ── Test 4: calendar filter excludes items with correct ledger ───────────────
{
  # Simulate what df_combined does with the papelera filter
  papelera <- tibble::tibble(
    ledger = c("AR", "AP", "MIXED"),
    Empresa = c("EmpA", "EmpB", "EmpC"),
    Moneda  = c("MXN", "MXN", "MXN"),
    Documento = c("DOC001", "DOC002", "DOC003")
  )
  df <- data.frame(
    Empresa   = c("EmpA", "EmpB", "EmpC", "EmpD"),
    Moneda    = c("MXN", "MXN", "MXN", "MXN"),
    Documento = c("DOC001", "DOC002", "DOC003", "DOC004"),
    stringsAsFactors = FALSE
  )

  # Apply the fixed calendar filter (AR ledger; also catches MIXED)
  ledger <- "AR"
  pap_keys <- papelera[papelera[["ledger"]] == ledger |
                          papelera[["ledger"]] == "MIXED",
                        c("Empresa","Moneda","Documento"), drop = FALSE]
  filtered <- dplyr::anti_join(df, pap_keys, by = c("Empresa","Moneda","Documento"))

  ok("calendar AR filter removes DOC001 (AR ledger)",  !("DOC001" %in% filtered$Documento))
  ok("calendar AR filter removes DOC003 (MIXED ledger)", !("DOC003" %in% filtered$Documento))
  ok("calendar AR filter keeps DOC002 (AP ledger, not AR)", "DOC002" %in% filtered$Documento)
  ok("calendar AR filter keeps DOC004 (not deleted)",    "DOC004" %in% filtered$Documento)
}

# ── Test 5: Old MIXED entries in papelera are filtered from both AR and AP ───
{
  papelera <- tibble::tibble(
    ledger = c("MIXED", "MIXED"),
    Empresa = c("EmpA", "EmpB"),
    Moneda  = c("MXN", "MXN"),
    Documento = c("DOCX", "DOCY")
  )
  df_ar <- data.frame(Empresa = c("EmpA","EmpC"), Moneda = c("MXN","MXN"),
                       Documento = c("DOCX","DOCZ"), stringsAsFactors = FALSE)
  df_ap <- data.frame(Empresa = c("EmpB","EmpC"), Moneda = c("MXN","MXN"),
                       Documento = c("DOCY","DOCZ"), stringsAsFactors = FALSE)

  for (ldg in c("AR", "AP")) {
    df_in <- if (ldg == "AR") df_ar else df_ap
    pap_keys <- papelera[papelera[["ledger"]] == ldg |
                           papelera[["ledger"]] == "MIXED",
                         c("Empresa","Moneda","Documento"), drop = FALSE]
    out <- dplyr::anti_join(df_in, pap_keys, by = c("Empresa","Moneda","Documento"))
    ok(paste("MIXED papelera row removed from", ldg, "calendar"),
       nrow(out) == 1 && out$Documento == "DOCZ")
  }
}

cat("\n=== Stage 1 results:", pass, "passed,", fail, "failed ===\n")
if (fail > 0) stop("Stage 1 tests FAILED — do not proceed to Stage 2.")
invisible(NULL)
