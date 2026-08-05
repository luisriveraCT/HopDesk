# =============================================================================
# tests/test_calendario_stage_sel_provision_guard.R
# Found live 2026-08-04: Mouse reported "Agregar selección" (the per-selection
# stage button on Calendario's day-view modal) crashes the app, and asked
# for a review of every button that sends items to Agenda: it must always
# log, always register the real ITEM (never a provision) into pagar_hoy_db,
# and the calendar's hourglass badge (⏳, staged_count_by_day in R/global.R)
# must reflect that correctly.
#
# Root cause, confirmed by reading R/ledger_module.R's stage_sel/do_move/
# do_restore observers and reproducing the exact call chain with synthetic
# data (see the diagnostic run during investigation): all three called
# pasivos_filter_out_provisions(keys) -- but `keys` is the output of
# .audit_sel_to_keys()/.resolve_summary_sel(), which never carries a
# `source` column (audit mode keeps `provision_id` instead; summary mode
# keeps neither). pasivos_filter_out_provisions()'s own guard
# (!"source" %in% names(rows) -> return rows unchanged) fired silently
# every time, so a selected provision sailed straight through into
# new_rows -> upsert_pagar_hoy() -> pagar_hoy_db, the one thing this
# mechanism exists to prevent. The ALREADY-CORRECT buttons (stage_all,
# the per-group cart_<i> handler, cart_inv_click) never had this bug:
# they all call pasivos_filter_out_provisions() on `detail` (or a
# detail-derived frame that still has `source`) BEFORE deriving keys, not
# after.
#
# Fix: a new pasivos_exclude_provision_keys(keys, detail) in
# R/pasivos_calendar_glue.R restricts a post-selection `keys` frame to just
# the (Empresa,Moneda,Documento,Importe) combinations that also survive
# pasivos_filter_out_provisions(detail) -- effective regardless of whether
# `keys` carries `source`, `provision_id`, both, or neither. stage_sel,
# do_move, and do_restore now call this instead of the no-op.
#
# do_delete is DELIBERATELY untouched -- it already handles provisions
# correctly by a different route (keeping them in `keys` via provision_id so
# they can be soft-deleted/papelera'd like Issue D's Stage 9 work confirmed),
# not by excluding them.
# =============================================================================
cat("=== test_calendario_stage_sel_provision_guard ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) {
    cat(" PASS:", label, "\n"); .pass <<- .pass + 1L
  } else {
    cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L
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
assign("%||%", function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x, envir = globalenv())
.extract_fn("R/pasivos_calendar_glue.R", "pasivos_filter_out_provisions")
.extract_fn("R/pasivos_calendar_glue.R", "pasivos_exclude_provision_keys")
.extract_fn("R/persistence.R", "upsert_pagar_hoy")

# ── 1. Functional: the real pasivos_exclude_provision_keys(), synthetic data ─
{
  detail <- data.frame(
    Empresa = c("NCS", "NCS", "NTS"), Moneda = c("MXN", "MXN", "MXN"),
    Documento = c("SAP-100", "PROV-1", "SAP-200"),
    Parte = c("Proveedor A", "Proveedor B", "Proveedor C"),
    Codigo = c("C001", NA_character_, "C002"),
    Importe = c(1000, 500, 250),
    FechaEff = as.Date(c("2026-08-10", "2026-08-10", "2026-08-10")),
    source = c("sap", "provision", "manual"),
    provision_id = c(NA_character_, "prov-uuid-1", NA_character_),
    stringsAsFactors = FALSE
  )

  # Shape returned by .audit_sel_to_keys() when both real rows + the
  # provision are selected together.
  keys_audit_mixed <- data.frame(
    Empresa = c("NCS", "NCS"), Moneda = c("MXN", "MXN"),
    Documento = c("SAP-100", "PROV-1"), Importe = c(1000, 500),
    provision_id = c(NA_character_, "prov-uuid-1"),
    stringsAsFactors = FALSE
  )
  fixed <- pasivos_exclude_provision_keys(keys_audit_mixed, detail)
  ok("mixed audit-mode selection: the provision (PROV-1) is excluded", !("PROV-1" %in% (fixed$Documento %||% character(0))))
  ok("mixed audit-mode selection: the real SAP invoice (SAP-100) survives", "SAP-100" %in% fixed$Documento)
  ok("mixed audit-mode selection: exactly 1 row survives (not 2, not 0)", nrow(fixed) == 1L)
  ok("extra columns already on keys (provision_id) pass through the merge untouched",
     "provision_id" %in% names(fixed) && is.na(fixed$provision_id[1]))

  # Shape returned by .resolve_summary_sel() -- no provision_id column at all,
  # but Parte is present instead.
  keys_summary_mixed <- data.frame(
    Empresa = c("NCS", "NCS"), Moneda = c("MXN", "MXN"),
    Documento = c("SAP-100", "PROV-1"), Importe = c(1000, 500),
    Parte = c("Proveedor A", "Proveedor B"),
    stringsAsFactors = FALSE
  )
  fixed2 <- pasivos_exclude_provision_keys(keys_summary_mixed, detail)
  ok("summary-mode selection (no provision_id column at all): provision still excluded",
     !("PROV-1" %in% (fixed2$Documento %||% character(0))))
  ok("summary-mode selection: real invoice survives with its Parte column intact",
     nrow(fixed2) == 1L && fixed2$Parte[1] == "Proveedor A")

  # Selecting ONLY the provision: nothing should be staged, not an error.
  keys_prov_only <- keys_audit_mixed[keys_audit_mixed$Documento == "PROV-1", ]
  fixed3 <- pasivos_exclude_provision_keys(keys_prov_only, detail)
  ok("provision-only selection: result has 0 rows (nothing to stage, no error)", nrow(fixed3) == 0L)

  # Selecting only real invoices: nothing should be dropped.
  keys_real_only <- data.frame(
    Empresa = c("NCS", "NTS"), Moneda = c("MXN", "MXN"),
    Documento = c("SAP-100", "SAP-200"), Importe = c(1000, 250),
    stringsAsFactors = FALSE
  )
  fixed4 <- pasivos_exclude_provision_keys(keys_real_only, detail)
  ok("real-only selection: both real invoices survive unchanged", nrow(fixed4) == 2L)

  # Empty selection: no crash.
  empty_keys <- data.frame(Empresa = character(), Moneda = character(),
                           Documento = character(), Importe = numeric())
  fixed5 <- tryCatch(pasivos_exclude_provision_keys(empty_keys, detail), error = function(e) "THREW")
  ok("empty keys input does not throw", !identical(fixed5, "THREW"))

  # Control: confirm the OLD, broken call really is a no-op on this shape --
  # guards against this test suite's own understanding of the bug being wrong.
  ok("control: pasivos_filter_out_provisions(keys) alone (the OLD call) is confirmed still a no-op on a keys-shaped frame (no 'source' column) -- this is exactly why the fix couldn't just tweak that function",
     nrow(pasivos_filter_out_provisions(keys_audit_mixed)) == nrow(keys_audit_mixed))
}

# ── 2. Functional: end-to-end into upsert_pagar_hoy() -- confirm a provision
# genuinely never reaches pagar_hoy_db via this path, not just that the
# intermediate `keys` frame looks right. ────────────────────────────────────
{
  detail <- data.frame(
    Empresa = "NCS", Moneda = "MXN", Documento = c("SAP-100", "PROV-1"),
    Parte = c("Proveedor A", "Proveedor B"), Codigo = c("C001", NA_character_),
    Importe = c(1000, 500), FechaEff = as.Date("2026-08-10"),
    source = c("sap", "provision"), provision_id = c(NA_character_, "prov-uuid-1"),
    stringsAsFactors = FALSE
  )
  keys <- data.frame(
    Empresa = "NCS", Moneda = "MXN", Documento = c("SAP-100", "PROV-1"),
    Importe = c(1000, 500), provision_id = c(NA_character_, "prov-uuid-1"),
    stringsAsFactors = FALSE
  )
  keys_fixed <- pasivos_exclude_provision_keys(keys, detail)
  new_rows <- merge(keys_fixed[, c("Empresa","Moneda","Documento","Importe"), drop = FALSE],
                    detail[, c("Empresa","Moneda","Documento","Importe","Parte","Codigo"), drop = FALSE],
                    by = c("Empresa","Moneda","Documento","Importe"))
  new_rows$id <- "row-1"; new_rows$ledger <- "AP"; new_rows$status <- "pending"

  ph_empty <- data.frame(id = character(), ledger = character(), Empresa = character(),
                        Moneda = character(), Documento = character(), Importe = numeric(),
                        Parte = character(), Codigo = character(), status = character(),
                        stringsAsFactors = FALSE)
  updated <- upsert_pagar_hoy(ph_empty, new_rows, keys = c("ledger","Empresa","Moneda","Documento","Importe"))
  ok("end-to-end: upsert_pagar_hoy() result contains exactly 1 row (the real invoice)", nrow(updated) == 1L)
  ok("end-to-end: the surviving pagar_hoy_db row is the real SAP invoice, not the provision",
     nrow(updated) == 1L && updated$Documento[1] == "SAP-100")
  ok("end-to-end: the provision (PROV-1) never appears anywhere in the resulting pagar_hoy_db",
     !("PROV-1" %in% updated$Documento))
}

# ── 3. Static: the three fixed handlers now call the real helper ───────────
{
  txt <- readLines("R/ledger_module.R", warn = FALSE)
  for (handler in c('observeEvent\\(input\\$stage_sel,', 'observeEvent\\(input\\$do_move,', 'observeEvent\\(input\\$do_restore,')) {
    start <- grep(handler, txt)
    ok(sprintf("found %s to scan", handler), length(start) > 0)
    if (length(start)) {
      end <- grep("^    \\}, ignoreInit = TRUE\\)$|^      \\}, ignoreInit = TRUE\\)$", txt)
      end <- end[end > start[1]][1]
      block <- paste(txt[start[1]:min(end %||% (start[1] + 60), start[1] + 60)], collapse = "\n")
      ok(sprintf("%s calls pasivos_exclude_provision_keys(keys, detail) (the fix)", handler),
         grepl("pasivos_exclude_provision_keys\\(keys,\\s*detail\\)", block))
      ok(sprintf("%s no longer calls the broken pasivos_filter_out_provisions(keys) on a bare keys object", handler),
         !grepl("keys\\s*<-\\s*pasivos_filter_out_provisions\\(keys\\)", block))
    }
  }

  # ── Regression guard: the already-correct buttons are untouched ──────────
  ok("stage_all still filters on `detail` (or a detail-derived frame with `source`) BEFORE deriving keys -- unaffected regression guard",
     any(grepl("pasivos_filter_out_provisions\\(detail\\[mask,", txt)))
  ok("cart_<i> group-click handler still filters BEFORE stripping down to key columns -- unaffected regression guard",
     any(grepl("inv_keys <- pasivos_filter_out_provisions\\(", txt)))
  ok("cart_inv_click still filters on inv_rows_raw (has `source`) BEFORE deriving inv_key -- unaffected regression guard",
     any(grepl("inv_rows <- pasivos_filter_out_provisions\\(inv_rows_raw\\)", txt)))

  # ── do_delete is deliberately untouched (handles provisions differently) ─
  del_start <- grep("observeEvent\\(input\\$do_delete,", txt)
  ok("found do_delete to scan", length(del_start) > 0)
  if (length(del_start)) {
    block <- paste(txt[del_start[1]:min(del_start[1] + 40, length(txt))], collapse = "\n")
    ok("do_delete still keeps provisions in `keys` via provision_id (deliberately different handling, not this bug's fix)",
       grepl("has_pid_col", block))
  }
}

# ── 4. Static: new helper's contract documented in place ───────────────────
{
  txt <- readLines("R/pasivos_calendar_glue.R", warn = FALSE)
  ok("pasivos_exclude_provision_keys() is defined", any(grepl("^pasivos_exclude_provision_keys <- function", txt)))
  ok("the incident is documented in place (grep-able tone matching this codebase's own convention)",
     any(grepl("found 2026-08-04", txt)))
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0) stop("Tests FAILED.")
invisible(NULL)
