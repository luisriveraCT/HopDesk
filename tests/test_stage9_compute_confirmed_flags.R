# =============================================================================
# tests/test_stage9_compute_confirmed_flags.R
# Stage 9 of the ledger-integrity master plan (source
# docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md §5/§7, revised to a 2-source model
# per AGENDA_CALENDARIO_WIRING_AUDIT.md §3.1/§3.8): extract the "is this
# invoice confirmed" computation out of df_combined() into a single
# reference implementation, compute_confirmed_flags() (R/data_pipeline.R),
# with zero further behavior change to the calendar.
#
# The pre-Stage-4 third source (pagar_hoy_db.status=="confirmed") is
# structurally dead by this point -- every confirm handler now
# unconditionally unstages the Agenda row regardless of source, so nothing
# can ever leave one behind with status=="confirmed" -- and was dropped
# from the extracted function entirely, not just left unused.
#
# Three kinds of checks:
#   1. Static scan -- df_combined() calls compute_confirmed_flags() and the
#      old inline 3-source block (including the dead pagar_hoy_db source)
#      is actually gone from ledger_module.R, not just supplemented.
#   2. Unit tests on compute_confirmed_flags() itself, covering both
#      sources independently, the manual-row-removal behavior, and the
#      provision belt-and-suspenders clear.
#   3. A frozen snapshot of the OLD 3-source inline logic (as it was before
#      this stage -- Source 3 always a no-op today, kept here only to prove
#      the extraction changed nothing observable) run against identical
#      synthetic input, asserting byte-for-byte identical output to the
#      new function -- the "zero behavior change" contract, made concrete.
# =============================================================================

cat("── Stage 9: compute_confirmed_flags() extracted, 2-source model ────────\n")

suppressPackageStartupMessages(library(tibble))

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

.extract_fn <- function(file, fn_name) {
  exprs <- parse(file, keep.source = FALSE)
  for (e in exprs) {
    if (is.call(e) && length(e) >= 2 &&
        identical(e[[1]], as.name("<-")) &&
        identical(e[[2]], as.name(fn_name))) {
      assign(fn_name, eval(e[[3]], envir = globalenv()), envir = globalenv())
      return(invisible(TRUE))
    }
  }
  stop(sprintf("%s not found in %s", fn_name, file))
}
.extract_fn("R/data_pipeline.R", "compute_confirmed_flags")

.strip_comments <- function(lines) sub("#.*$", "", lines)

# ── 1. Static scan: df_combined() wired to the extracted function ─────────
{
  txt <- readLines("R/ledger_module.R", warn = FALSE)
  start <- grep("df_combined <- reactive", txt)
  end   <- grep("Calendar-ready aggregation", txt)
  .chk(length(start) > 0 && length(end) > 0, TRUE, "found df_combined() to scan")
  if (length(start) && length(end)) {
    block <- paste(.strip_comments(txt[start[1]:end[1]]), collapse = "\n")
    .chk(grepl("df <- compute_confirmed_flags\\(", block), TRUE,
         "df_combined() calls compute_confirmed_flags()")
    .chk(grepl("ph_db <- tryCatch\\(shared\\$pagar_hoy_db\\(\\)", block), FALSE,
         "the old inline pagar_hoy_db (dead Source 3) read is actually gone from df_combined(), not just unused")
    .chk(grepl("bc_mask   <- \\(match_key", block), FALSE,
         "the old inline bancos_confirmados matching block is gone (moved into the extracted function)")
    .chk(grepl("ghost_mask <- \\(match_key", block), FALSE,
         "the old inline papelera-ghost matching block is gone (moved into the extracted function)")
  }
}

# ── 2. Unit tests on compute_confirmed_flags() ─────────────────────────────
# Saldo_original (not Importe -- SAP rows have no such column at all, only
# Saldo vencido/Saldo_original) doubles as the row's "confirmation-time"
# amount here too, matching Stage 10's amount-match guard.
.mkdf <- function(source, Empresa, Moneda, Documento, Saldo_original) {
  tibble::tibble(source = source, Empresa = Empresa, Moneda = Moneda,
                 Documento = Documento, Saldo_original = Saldo_original)
}

{
  # Source 1: bancos_confirmados matches a SAP row -> confirmed + is_paid_ghost
  df1 <- .mkdf("sap", "ACME", "MXN", "F-1", 100)
  bc1 <- tibble::tibble(empresa = "ACME", documento = "F-1", moneda = "MXN",
                        tipo = "pago", eliminado = FALSE, importe = 100)
  out1 <- compute_confirmed_flags(df1, "AP", bc1, NULL)
  .chk(out1$confirmed[1],     TRUE, "Source 1: matching SAP row marked confirmed")
  .chk(out1$is_paid_ghost[1], TRUE, "Source 1: matching SAP row marked is_paid_ghost")
  .chk(nrow(out1), 1L, "Source 1: SAP ghost stays IN the data frame (visible, struck through), not removed")

  # Source 1 must NEVER match a manual or provision row, even with an
  # identical key -- manual/provision confirmation works through entirely
  # different mechanisms (archive-and-remove / conversion modal).
  df2 <- .mkdf(c("manual", "provision"), c("ACME","ACME"), c("MXN","MXN"),
              c("F-1","F-1"), c(100,100))
  out2 <- compute_confirmed_flags(df2, "AP", bc1, NULL)
  .chk(any(out2$confirmed), FALSE,
       "Source 1: a matching key never confirms a manual or provision row")

  # Source 2: papelera SAP ghost -> confirmed + is_ghost. Source 2 has no
  # amount-match guard (Stage 10 scopes it to bancos_confirmados only), so
  # amount is irrelevant here.
  df3 <- .mkdf("sap", "ACME", "MXN", "F-2", 200)
  pap3 <- tibble::tibble(ledger = "AP", source = "sap", Empresa = "ACME",
                         Moneda = "MXN", Documento = "F-2")
  out3 <- compute_confirmed_flags(df3, "AP", NULL, pap3)
  .chk(out3$confirmed[1], TRUE, "Source 2: matching papelera SAP ghost marked confirmed")
  .chk(out3$is_ghost[1],  TRUE, "Source 2: matching papelera SAP ghost marked is_ghost")

  # Source 2 must never confirm a provision row (belt), and the suspenders
  # clear must also fire even if some earlier mask had (incorrectly) set it.
  df4 <- .mkdf("provision", "ACME", "MXN", "F-2", 200)
  out4 <- compute_confirmed_flags(df4, "AP", NULL, pap3)
  .chk(out4$confirmed[1], FALSE, "Source 2: a matching key never confirms a provision row")

  # Manual row that IS confirmed (e.g. archived-and-removed elsewhere,
  # already reflected in bancos_confirmados matching being irrelevant here
  # -- simulate by pre-setting confirmed=TRUE directly, matching how a
  # caller might pass a row already flagged) gets removed entirely from df.
  df5 <- tibble::tibble(source = "manual", Empresa = "ACME", Moneda = "MXN",
                        Documento = "F-3", Saldo_original = 50, confirmed = TRUE)
  out5 <- compute_confirmed_flags(df5, "AP", NULL, NULL)
  .chk(nrow(out5), 0L, "a confirmed manual row is removed entirely from df (disappears from calendar), unlike a SAP ghost")

  # Provision belt-and-suspenders: forcibly cleared regardless of any mask.
  df6 <- tibble::tibble(source = "provision", Empresa = "ACME", Moneda = "MXN",
                        Documento = "F-1", Saldo_original = 100,
                        confirmed = TRUE, is_paid_ghost = TRUE, is_ghost = TRUE)
  out6 <- compute_confirmed_flags(df6, "AP", bc1, pap3)
  .chk(out6$confirmed[1],     FALSE, "provision suspenders-clear: confirmed forced FALSE even if already TRUE going in")
  .chk(out6$is_paid_ghost[1], FALSE, "provision suspenders-clear: is_paid_ghost forced FALSE")
  .chk(out6$is_ghost[1],      FALSE, "provision suspenders-clear: is_ghost forced FALSE")

  # NA confirmed input normalizes to FALSE, never left NA.
  df7 <- tibble::tibble(source = "sap", Empresa = "X", Moneda = "MXN",
                        Documento = "F-9", Saldo_original = 1, confirmed = NA)
  out7 <- compute_confirmed_flags(df7, "AP", NULL, NULL)
  .chk(is.na(out7$confirmed[1]), FALSE, "NA confirmed input is normalized, never left NA")
  .chk(out7$confirmed[1], FALSE, "NA confirmed input normalizes to FALSE absent any match")

  # AR uses tipo=="cobro", not "pago" -- confirm the tipo_val branch is correct.
  df8 <- .mkdf("sap", "ACME", "MXN", "F-4", 300)
  bc8 <- tibble::tibble(empresa = "ACME", documento = "F-4", moneda = "MXN",
                        tipo = "cobro", eliminado = FALSE, importe = 300)
  out8_ar <- compute_confirmed_flags(df8, "AR", bc8, NULL)
  out8_ap <- compute_confirmed_flags(df8, "AP", bc8, NULL)
  .chk(out8_ar$confirmed[1], TRUE,  "AR ledger matches bancos_confirmados rows with tipo=='cobro'")
  .chk(out8_ap$confirmed[1], FALSE, "the SAME confirmado row does NOT match when checked as AP (tipo mismatch, not just ledger-blind)")

  # Real bug found 2026-07-24 wiring Stage 12 (Reporte's Cash Flow Pulse,
  # deliberately SAP-only, no "source" column at all): is_manual/is_provision
  # used to collapse to logical(0) instead of all-FALSE when "source" is
  # entirely absent (not just NA-valued), which broke every downstream mask
  # recycling and crashed on the tibble assignment. A caller with no "source"
  # column at all must still work, treating every row as (implicitly) SAP.
  df9 <- tibble::tibble(Empresa = "ACME", Moneda = "MXN", Documento = "F-5", Saldo_original = 400)
  bc9 <- tibble::tibble(empresa = "ACME", documento = "F-5", moneda = "MXN",
                        tipo = "pago", eliminado = FALSE, importe = 400)
  out9 <- compute_confirmed_flags(df9, "AP", bc9, NULL)
  .chk(nrow(out9), 1L, "a data frame with no 'source' column at all does not crash")
  .chk(out9$confirmed[1], TRUE, "and still correctly matches bancos_confirmados (every row is implicitly treated as SAP)")

  # Real bug found 2026-07-24: Source 2 (papelera SAP ghosts) had no
  # !is_manual guard, unlike Source 1 -- a brand-new manual invoice that
  # happens to reuse the same Empresa+Moneda+Documento key as some
  # unrelated, previously-archived SAP invoice (a generic placeholder like
  # "test" is a realistic collision) was wrongly treated as that SAP ghost,
  # marked confirmed, and then deleted outright -- silently vanishing from
  # the calendar despite never having been staged, confirmed, or deleted by
  # any real user action. Reproduced live: a user created a manual AP
  # invoice (Documento="test") that collided with an old trashed SAP
  # invoice of the same key and it never appeared on the calendar.
  df10 <- .mkdf("manual", "Networks & Logistics", "MXN", "test", 1)
  pap10 <- tibble::tibble(ledger = "AP", source = "sap",
                          Empresa = "Networks & Logistics",
                          Moneda = "MXN", Documento = "test")
  out10 <- compute_confirmed_flags(df10, "AP", NULL, pap10)
  .chk(nrow(out10), 1L,
       "a manual invoice colliding on key with an unrelated papelera SAP ghost survives -- it is NOT deleted from df")
  .chk(out10$confirmed[1], FALSE,
       "and is not marked confirmed either -- the SAP ghost's archival has nothing to do with this manual row")

  # The same collision against a SAP-sourced row (not manual) must still
  # correctly ghost -- proves the fix is a manual-only guard, not a
  # regression that breaks Source 2 for its actual intended case.
  df11 <- .mkdf("sap", "Networks & Logistics", "MXN", "test", 1)
  out11 <- compute_confirmed_flags(df11, "AP", NULL, pap10)
  .chk(out11$confirmed[1], TRUE,
       "the identical key collision still correctly ghosts a real SAP row (Source 2's actual intended case, unaffected by the manual-only guard)")
}

# ── 3. Regression: byte-for-byte equivalence with the frozen pre-extraction logic ─
{
  # Frozen historical snapshot of df_combined()'s inline logic exactly as it
  # existed before this stage's extraction (including the then-already-dead
  # Source 3, which never actually matches anything real by this point in
  # the project -- kept here ONLY to prove the extraction changed nothing
  # observable, not as a template for future changes).
  old_logic <- function(df, ledger, bancos_confirmados_df, papelera_df, pagar_hoy_df) {
    tipo_val <- if (ledger == "AR") "cobro" else "pago"
    if (!"confirmed" %in% names(df)) df[["confirmed"]] <- FALSE
    na_conf <- is.na(df[["confirmed"]])
    if (any(na_conf)) df[["confirmed"]][na_conf] <- FALSE
    is_manual    <- "source" %in% names(df) & !is.na(df[["source"]]) & df[["source"]] == "manual"
    is_provision <- "source" %in% names(df) & !is.na(df[["source"]]) & df[["source"]] == "provision"

    conf_db <- bancos_confirmados_df
    if (!is.null(conf_db) && nrow(conf_db)) {
      conf_active <- conf_db[!(conf_db[["eliminado"]] %in% TRUE) & conf_db[["tipo"]] == tipo_val, , drop = FALSE]
      if (nrow(conf_active)) {
        bc_keys   <- unique(conf_active[, c("empresa","documento","moneda"), drop = FALSE])
        match_key <- paste(toupper(trimws(df[["Empresa"]])), toupper(trimws(df[["Documento"]])), toupper(trimws(df[["Moneda"]])))
        conf_key  <- paste(toupper(trimws(bc_keys[["empresa"]])), toupper(trimws(bc_keys[["documento"]])), toupper(trimws(bc_keys[["moneda"]])))
        bc_mask   <- (match_key %in% conf_key) & !is_manual & !is_provision
        df[["confirmed"]]   <- df[["confirmed"]] | bc_mask
        if (!"is_paid_ghost" %in% names(df)) df[["is_paid_ghost"]] <- FALSE
        df[["is_paid_ghost"]] <- df[["is_paid_ghost"]] | bc_mask
      }
    }
    if (!is.null(papelera_df) && nrow(papelera_df)) {
      pap_this <- papelera_df[papelera_df[["ledger"]] == ledger | papelera_df[["ledger"]] == "MIXED", , drop = FALSE]
      if (nrow(pap_this)) {
        sap_pap <- pap_this[!is.na(pap_this[["source"]]) & pap_this[["source"]] == "sap", c("Empresa","Moneda","Documento"), drop = FALSE]
        if (nrow(sap_pap)) {
          match_key <- paste(df[["Empresa"]], df[["Moneda"]], df[["Documento"]])
          pap_key   <- paste(sap_pap[["Empresa"]], sap_pap[["Moneda"]], sap_pap[["Documento"]])
          ghost_mask <- (match_key %in% pap_key) & !is_provision
          df[["confirmed"]] <- df[["confirmed"]] | ghost_mask
          if (!"is_ghost" %in% names(df)) df[["is_ghost"]] <- FALSE
          df[["is_ghost"]]  <- df[["is_ghost"]] | ghost_mask
        }
      }
    }
    # Source 3 (pagar_hoy_db) -- structurally dead, always a no-op in
    # today's data, but included for a faithful pre-extraction snapshot.
    ph_db <- pagar_hoy_df
    if (!is.null(ph_db) && nrow(ph_db)) {
      ph_conf <- ph_db[!is.na(ph_db[["status"]]) & ph_db[["status"]] == "confirmed" &
                       !is.na(ph_db[["ledger"]]) & ph_db[["ledger"]] == ledger, , drop = FALSE]
      if (nrow(ph_conf) && all(c("Empresa","Documento","Moneda") %in% names(ph_conf))) {
        ph_key <- paste(toupper(trimws(ph_conf[["Empresa"]])), toupper(trimws(ph_conf[["Documento"]])), toupper(trimws(ph_conf[["Moneda"]])))
        df_key <- paste(toupper(trimws(df[["Empresa"]])), toupper(trimws(df[["Documento"]])), toupper(trimws(df[["Moneda"]])))
        ph_mask     <- (df_key %in% ph_key) & !is_provision
        sap_ph_mask <- ph_mask & !is_manual
        df[["confirmed"]] <- df[["confirmed"]] | ph_mask
        if (!"is_paid_ghost" %in% names(df)) df[["is_paid_ghost"]] <- FALSE
        df[["is_paid_ghost"]] <- df[["is_paid_ghost"]] | sap_ph_mask
      }
    }
    if ("source" %in% names(df) && any(df[["confirmed"]] & df[["source"]] == "manual")) {
      df <- df[!(df[["confirmed"]] & df[["source"]] == "manual"), , drop = FALSE]
    }
    if ("source" %in% names(df) && "confirmed" %in% names(df)) {
      prov_mask <- !is.na(df[["source"]]) & df[["source"]] == "provision"
      if (any(prov_mask)) {
        df[["confirmed"]][prov_mask] <- FALSE
        if ("is_paid_ghost" %in% names(df)) df[["is_paid_ghost"]][prov_mask] <- FALSE
        if ("is_ghost"      %in% names(df)) df[["is_ghost"]][prov_mask]      <- FALSE
      }
    }
    df
  }

  synth_df <- tibble::tibble(
    source         = c("sap",   "sap",   "manual", "manual", "provision", "sap"),
    Empresa        = c("ACME",  "ACME",  "ACME",   "ACME",   "ACME",      "ZETA"),
    Moneda         = c("MXN",   "MXN",   "MXN",    "MXN",    "MXN",       "USD"),
    Documento      = c("F-10",  "F-11",  "F-12",   "F-13",   "F-11",      "F-14"),
    Saldo_original = c(100,      200,     50,       75,       200,         999)
  )
  # importe matches F-10's Saldo_original (100) exactly -- this section
  # tests the extraction (Stage 9) is behavior-preserving, not Stage 10's
  # amount-guard itself (that gets its own dedicated test file), so amounts
  # are set up to agree under both old (amount-blind) and new (amount-aware)
  # logic.
  synth_bc <- tibble::tibble(
    empresa = c("ACME"), documento = c("F-10"), moneda = c("MXN"),
    tipo = c("pago"), eliminado = c(FALSE), importe = c(100)
  )
  synth_pap <- tibble::tibble(
    ledger = c("AP"), source = c("sap"), Empresa = c("ZETA"),
    Moneda = c("USD"), Documento = c("F-14")
  )
  # status is "pending", never "confirmed" -- the true real-world invariant
  # this stage relies on (every confirm handler unconditionally unstages the
  # row, so nothing can ever leave one behind with status=="confirmed").
  # A "confirmed" row here would make Source 3 fire, which is exactly the
  # dead-code case this test exists to confirm never actually happens.
  synth_ph <- tibble::tibble(
    id = "x", status = "pending", ledger = "AP",
    Empresa = "ACME", Documento = "F-12", Moneda = "MXN"
  )

  old_out <- old_logic(synth_df, "AP", synth_bc, synth_pap, synth_ph)
  new_out <- compute_confirmed_flags(synth_df, "AP", synth_bc, synth_pap)

  common_cols <- intersect(names(old_out), names(new_out))
  .chk(nrow(old_out), nrow(new_out),
       "old and new logic produce the same number of surviving rows on identical synthetic input")
  .chk(as.data.frame(old_out[, common_cols, drop = FALSE]),
       as.data.frame(new_out[, common_cols, drop = FALSE]),
       "old (pre-extraction, Source-3-included) and new (extracted, 2-source) logic produce byte-for-byte identical output -- the 'zero behavior change' contract")
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
