# =============================================================================
# tests/test_vencidos_papelera.R
# Verify that vencidos_df papelera filter behaves correctly.
# Run with: source("tests/test_vencidos_papelera.R")
# =============================================================================
cat("=== Stage 2: test_vencidos_papelera ===\n\n")

pass <- 0L; fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) {
    cat(" PASS:", label, "\n"); pass <<- pass + 1L
  } else {
    cat(" FAIL:", label, "\n"); fail <<- fail + 1L
  }
}

# Simulate the papelera filter logic extracted from vencidos_module.R
apply_vencidos_papelera_filter <- function(combined, papelera) {
  if (is.null(papelera) || !nrow(papelera)) return(combined)
  pap_keys <- papelera[papelera[["ledger"]] %in% c("AR", "AP", "MIXED"),
                       c("Empresa", "Moneda", "Documento"), drop = FALSE]
  if (!nrow(pap_keys) || is.null(combined) || !nrow(combined)) return(combined)
  prov_mask <- "source" %in% names(combined) &
    !is.na(combined[["source"]]) & combined[["source"]] == "provision"
  dplyr::bind_rows(
    dplyr::anti_join(combined[!prov_mask, , drop = FALSE], pap_keys,
                     by = c("Empresa", "Moneda", "Documento")),
    combined[prov_mask, , drop = FALSE]
  )
}

# ── Test 1: Regular SAP item deleted (AR) is hidden from vencidos ─────────────
{
  combined <- data.frame(
    Empresa = c("EmpA", "EmpB"), Moneda = c("MXN", "MXN"),
    Documento = c("DOC001", "DOC002"), source = c("sap", "sap"),
    stringsAsFactors = FALSE
  )
  papelera <- tibble::tibble(
    ledger = "AR", Empresa = "EmpA", Moneda = "MXN", Documento = "DOC001"
  )
  out <- apply_vencidos_papelera_filter(combined, papelera)
  ok("AR SAP item deleted is hidden from vencidos",
     !("DOC001" %in% out$Documento) && "DOC002" %in% out$Documento)
}

# ── Test 2: MIXED-ledger deleted item is hidden from vencidos ─────────────────
{
  combined <- data.frame(
    Empresa = c("EmpA", "EmpB"), Moneda = c("MXN", "MXN"),
    Documento = c("DOCX", "DOCY"), source = c("sap", "sap"),
    stringsAsFactors = FALSE
  )
  papelera <- tibble::tibble(
    ledger = "MIXED", Empresa = "EmpA", Moneda = "MXN", Documento = "DOCX"
  )
  out <- apply_vencidos_papelera_filter(combined, papelera)
  ok("MIXED papelera item hidden from vencidos",
     !("DOCX" %in% out$Documento) && "DOCY" %in% out$Documento)
}

# ── Test 3: Provision rows are never filtered via papelera anti_join ──────────
{
  combined <- data.frame(
    Empresa = c("EmpA", "EmpA"), Moneda = c("MXN", "MXN"),
    Documento = c("DOC001", "DOC001"),
    source = c("sap", "provision"),
    stringsAsFactors = FALSE
  )
  papelera <- tibble::tibble(
    ledger = "AP", Empresa = "EmpA", Moneda = "MXN", Documento = "DOC001"
  )
  out <- apply_vencidos_papelera_filter(combined, papelera)
  ok("Provision row survives papelera anti_join (self-hides via estado)",
     sum(out$source == "provision") == 1L)
  ok("SAP row with same Documento as provision is correctly removed",
     sum(out$source == "sap") == 0L)
}

# ── Test 4: Empty papelera returns combined unchanged ─────────────────────────
{
  combined <- data.frame(
    Empresa = "EmpA", Moneda = "MXN", Documento = "DOC001",
    source = "sap", stringsAsFactors = FALSE
  )
  papelera <- tibble::tibble(
    ledger = character(), Empresa = character(), Moneda = character(),
    Documento = character()
  )
  out <- apply_vencidos_papelera_filter(combined, papelera)
  ok("Empty papelera leaves vencidos data unchanged", nrow(out) == 1L)
}

# ── Test 5: NULL papelera returns combined unchanged ─────────────────────────
{
  combined <- data.frame(
    Empresa = "EmpA", Moneda = "MXN", Documento = "DOC001",
    source = "sap", stringsAsFactors = FALSE
  )
  out <- apply_vencidos_papelera_filter(combined, NULL)
  ok("NULL papelera leaves vencidos data unchanged", nrow(out) == 1L)
}

# ── Test 6: AP item deleted is hidden from vencidos ───────────────────────────
{
  combined <- data.frame(
    Empresa = c("EmpA", "EmpB"), Moneda = c("USD", "USD"),
    Documento = c("FAC100", "FAC200"), source = c("sap", "sap"),
    stringsAsFactors = FALSE
  )
  papelera <- tibble::tibble(
    ledger = "AP", Empresa = "EmpA", Moneda = "USD", Documento = "FAC100"
  )
  out <- apply_vencidos_papelera_filter(combined, papelera)
  ok("AP SAP item deleted is hidden from vencidos",
     !("FAC100" %in% out$Documento) && "FAC200" %in% out$Documento)
}

cat("\n=== Stage 2 results:", pass, "passed,", fail, "failed ===\n")
if (fail > 0) stop("Stage 2 tests FAILED — do not proceed to Stage 3.")
invisible(NULL)
