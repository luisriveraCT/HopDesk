# =============================================================================
# tests/test_delete_consistency.R
# End-to-end: verifies that a delete from Search/Vencidos produces papelera
# entries with correct per-row ledgers, and that all three views (Calendar,
# Vencidos, Search) consistently exclude those rows.
# Run with: source("tests/test_delete_consistency.R")
# =============================================================================
cat("=== Stage 3: test_delete_consistency ===\n\n")

pass <- 0L; fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) {
    cat(" PASS:", label, "\n"); pass <<- pass + 1L
  } else {
    cat(" FAIL:", label, "\n"); fail <<- fail + 1L
  }
}

# ── Helper: simulate handle_invoice_action delete path ───────────────────────
simulate_delete <- function(keys_df, papelera_df, current_user = "testuser") {
  item_rows <- keys_df[!(keys_df$source == "provision" & nzchar(keys_df$provision_id %||% "")), ]
  if (!nrow(item_rows)) return(papelera_df)
  archive <- item_rows
  archive$FechaEff   <- as.Date(NA)
  archive$deleted_by <- current_user
  archive$deleted_at <- Sys.time()
  add_to_papelera(papelera_df, archive, ledger = "MIXED", deleted_by = current_user)
}

# ── Helper: apply calendar filter (df_combined logic) ────────────────────────
apply_calendar_filter <- function(df, papelera, ledger) {
  if (is.null(papelera) || !nrow(papelera)) return(df)
  pap_keys <- papelera[papelera[["ledger"]] == ledger |
                          papelera[["ledger"]] == "MIXED",
                        c("Empresa","Moneda","Documento"), drop = FALSE]
  if (!nrow(pap_keys)) return(df)
  if ("source" %in% names(df)) {
    prov_mask <- !is.na(df[["source"]]) & df[["source"]] == "provision"
    dplyr::bind_rows(
      dplyr::anti_join(df[!prov_mask, , drop = FALSE], pap_keys,
                       by = c("Empresa","Moneda","Documento")),
      df[prov_mask, , drop = FALSE]
    )
  } else {
    dplyr::anti_join(df, pap_keys, by = c("Empresa","Moneda","Documento"))
  }
}

# ── Helper: apply vencidos filter ─────────────────────────────────────────────
apply_vencidos_filter <- function(combined, papelera) {
  if (is.null(papelera) || !nrow(papelera)) return(combined)
  pap_keys <- papelera[papelera[["ledger"]] %in% c("AR", "AP", "MIXED"),
                       c("Empresa","Moneda","Documento"), drop = FALSE]
  if (!nrow(pap_keys) || is.null(combined) || !nrow(combined)) return(combined)
  prov_mask <- "source" %in% names(combined) &
    !is.na(combined[["source"]]) & combined[["source"]] == "provision"
  dplyr::bind_rows(
    dplyr::anti_join(combined[!prov_mask, , drop = FALSE], pap_keys,
                     by = c("Empresa","Moneda","Documento")),
    combined[prov_mask, , drop = FALSE]
  )
}

# ── Helper: apply search modal filter ────────────────────────────────────────
apply_search_filter <- function(all_df, papelera) {
  if (is.null(papelera) || !nrow(papelera) || is.null(all_df) || !nrow(all_df))
    return(all_df)
  pap_keys <- papelera[papelera[["ledger"]] %in% c("AR", "AP", "MIXED"),
                       c("Empresa","Moneda","Documento"), drop = FALSE]
  if (!nrow(pap_keys)) return(all_df)
  prov_mask <- "source" %in% names(all_df) &
    !is.na(all_df[["source"]]) & all_df[["source"]] == "provision"
  dplyr::bind_rows(
    dplyr::anti_join(all_df[!prov_mask, , drop = FALSE], pap_keys,
                     by = c("Empresa","Moneda","Documento")),
    all_df[prov_mask, , drop = FALSE]
  )
}

# ── Empty papelera to start ───────────────────────────────────────────────────
empty_papelera <- tibble::tibble(
  id = character(), ledger = character(), source = character(),
  Empresa = character(), Moneda = character(), Documento = character(),
  Parte = character(), Importe = numeric(), FechaEff = as.Date(character()),
  deleted_by = character(), deleted_at = as.POSIXct(character()),
  original_data = list()
)

# ── Dataset: one AR and one AP item ──────────────────────────────────────────
full_ar <- data.frame(
  Empresa = c("EmpA","EmpB"), Moneda = c("MXN","MXN"),
  Documento = c("FAC001","FAC002"), source = c("sap","sap"),
  stringsAsFactors = FALSE
)
full_ap <- data.frame(
  Empresa = c("EmpA","EmpC"), Moneda = c("MXN","MXN"),
  Documento = c("FAC003","FAC004"), source = c("sap","sap"),
  stringsAsFactors = FALSE
)
full_all <- dplyr::bind_rows(
  dplyr::mutate(full_ar, Ledger = "AR"),
  dplyr::mutate(full_ap, Ledger = "AP")
)

# ── Simulate a multi-ledger delete from Search (AR + AP rows at once) ─────────
keys_df <- data.frame(
  ledger = c("AR", "AP"),
  Empresa = c("EmpA", "EmpA"), Moneda = c("MXN", "MXN"),
  Documento = c("FAC001", "FAC003"), source = c("sap", "sap"),
  provision_id = c("", ""), Parte = c("P1","P2"), Importe = c(100,200),
  Fecha = c("2026-06-01","2026-06-01"), tipo = c("",""),
  inv_id = c("",""), Codigo = c("",""),
  stringsAsFactors = FALSE
)
papelera_after <- simulate_delete(keys_df, empty_papelera)

ok("papelera has 2 rows after delete", nrow(papelera_after) == 2L)
ok("AR row stored with ledger=AR (not MIXED)", papelera_after$ledger[papelera_after$Documento == "FAC001"] == "AR")
ok("AP row stored with ledger=AP (not MIXED)", papelera_after$ledger[papelera_after$Documento == "FAC003"] == "AP")

# ── Calendar: each ledger correctly excludes its own deleted item ─────────────
cal_ar_out <- apply_calendar_filter(full_ar, papelera_after, "AR")
ok("Calendar AR excludes FAC001 (deleted AR)", !("FAC001" %in% cal_ar_out$Documento))
ok("Calendar AR keeps FAC002 (not deleted)",    "FAC002" %in% cal_ar_out$Documento)

cal_ap_out <- apply_calendar_filter(full_ap, papelera_after, "AP")
ok("Calendar AP excludes FAC003 (deleted AP)", !("FAC003" %in% cal_ap_out$Documento))
ok("Calendar AP keeps FAC004 (not deleted)",    "FAC004" %in% cal_ap_out$Documento)

# ── Vencidos: excludes both deleted items ─────────────────────────────────────
ven_out <- apply_vencidos_filter(full_all, papelera_after)
ok("Vencidos excludes FAC001 (deleted AR)", !("FAC001" %in% ven_out$Documento))
ok("Vencidos excludes FAC003 (deleted AP)", !("FAC003" %in% ven_out$Documento))
ok("Vencidos keeps FAC002 and FAC004",
   "FAC002" %in% ven_out$Documento && "FAC004" %in% ven_out$Documento)

# ── Search modal: excludes both deleted items ────────────────────────────────
srch_out <- apply_search_filter(full_all, papelera_after)
ok("Search modal excludes FAC001 (deleted AR)", !("FAC001" %in% srch_out$Documento))
ok("Search modal excludes FAC003 (deleted AP)", !("FAC003" %in% srch_out$Documento))
ok("Search modal keeps FAC002 and FAC004",
   "FAC002" %in% srch_out$Documento && "FAC004" %in% srch_out$Documento)

# ── Legacy MIXED: simulate old S3 data, all three views must still filter ─────
legacy_papelera <- tibble::tibble(
  id = "old-uuid", ledger = "MIXED", source = "sap",
  Empresa = "EmpB", Moneda = "MXN", Documento = "FAC002",
  Parte = "", Importe = 50, FechaEff = as.Date(NA),
  deleted_by = "olduser", deleted_at = as.POSIXct(NA),
  original_data = list(list())
)
ok("Legacy MIXED: calendar AR excludes FAC002",
   !("FAC002" %in% apply_calendar_filter(full_ar, legacy_papelera, "AR")$Documento))
ok("Legacy MIXED: vencidos excludes FAC002",
   !("FAC002" %in% apply_vencidos_filter(full_all, legacy_papelera)$Documento))
ok("Legacy MIXED: search modal excludes FAC002",
   !("FAC002" %in% apply_search_filter(full_all, legacy_papelera)$Documento))

# ── Provision rows are never filtered via papelera anti_join ─────────────────
mixed_data <- data.frame(
  Empresa = c("EmpA","EmpA"), Moneda = c("MXN","MXN"),
  Documento = c("FAC001","FAC001"), source = c("sap","provision"),
  Ledger = c("AP","AP"), stringsAsFactors = FALSE
)
pap_with_provision_doc <- tibble::tibble(
  id = "x", ledger = "AP", source = "sap",
  Empresa = "EmpA", Moneda = "MXN", Documento = "FAC001",
  Parte = "", Importe = 100, FechaEff = as.Date(NA),
  deleted_by = "u", deleted_at = as.POSIXct(NA),
  original_data = list(list())
)
filtered <- apply_vencidos_filter(mixed_data, pap_with_provision_doc)
ok("Provision row never removed by papelera anti_join",
   sum(filtered$source == "provision") == 1L)

cat("\n=== Stage 3 results:", pass, "passed,", fail, "failed ===\n")
if (fail > 0) stop("Stage 3 tests FAILED.")
invisible(NULL)
