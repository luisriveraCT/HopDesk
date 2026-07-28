# =============================================================================
# tests/test_stage20_abono_followups.R
# Stage 20 of docs/LEDGER_INTEGRITY_MASTER_PLAN.md: two of the three Abono
# Parcial follow-ups flagged by Stage 15's audit.
#
# 1. The staged-abono row's Vencimiento always showed "today" (the staging
#    date), not the real invoice due date -- misleading next to real
#    factura rows in the same Agenda table. Fixed in
#    R/staging_browse_module.R's ab_rows observer to parse the real due
#    date the client already sends (data-fecha-venc), falling back to
#    Sys.Date() only if that's genuinely unparseable.
#
# 2. rename_empresa_initials() (R/persistence.R) -- triggered when editing
#    a company's "Iniciales" in Settings > Empresas -- was missing two
#    tables that also store Empresa as initials: pasivos_provisions and
#    pasivos_liabilities. Both use full display names in manual_inv/
#    pagar_hoy/abonos_db/sap_overrides instead, so those four were never
#    actually in scope for this function (a separate, unbuilt full-name
#    rename cascade would be needed for them -- deliberately not built
#    this stage, deprioritized as low-frequency).
# =============================================================================

cat("── Stage 20: staged-abono due date + initials-rename cascade gaps ──────\n")

suppressPackageStartupMessages({ library(dplyr); library(tibble) })

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
.strip_comments <- function(lines) sub("#.*$", "", lines)
"%||%" <- function(a, b) if (!is.null(a)) a else b

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

# ── 1a. Static scan: ab_rows uses the real due date, not Sys.Date() alone ──
{
  txt <- readLines("R/staging_browse_module.R", warn = FALSE)
  start <- grep("observeEvent\\(input\\$ab_rows,", txt)
  .chk(length(start) > 0, TRUE, "found the ab_rows observer to scan")
  if (length(start)) {
    block <- paste(.strip_comments(txt[start[1]:min(start[1] + 90, length(txt))]), collapse = "\n")
    .chk(grepl("FechaVenc\\s*=\\s*tryCatch\\(as\\.Date\\(r_raw\\$fecha_venc\\)", block), TRUE,
         "the staged abono row's FechaVenc parses the real invoice due date (r_raw$fecha_venc)")
    .chk(grepl("error = function\\(e\\) Sys\\.Date\\(\\)", block), TRUE,
         "falls back to Sys.Date() only if the real date is unparseable, never left NA")
  }
}

# ── 1b. Behavioral: the parse-with-fallback logic itself ───────────────────
{
  parse_due <- function(fecha_venc) tryCatch(as.Date(fecha_venc), error = function(e) Sys.Date())
  .chk(parse_due("2026-08-15"), as.Date("2026-08-15"),
       "a valid ISO date string parses to the real due date")
  .chk(parse_due("garbage"), Sys.Date(),
       "an unparseable value falls back to today, never errors out to the caller")
}

# ── 2a. Static scan: rename_empresa_initials() now covers both new tables ──
{
  .extract_fn("R/persistence.R", "rename_empresa_initials")
  txt <- readLines("R/persistence.R", warn = FALSE)
  start <- grep("^rename_empresa_initials <- function", txt)
  end   <- grep("^\\}", txt)
  end   <- end[end > start[1]][1] %||% (start[1] + 90)
  block <- paste(.strip_comments(txt[start[1]:end]), collapse = "\n")

  .chk(grepl("load_pasivos_provisions\\(client_id = client_id\\)", block), TRUE,
       "rename_empresa_initials() now reads pasivos_provisions")
  .chk(grepl("save_pasivos_provisions\\(df, client_id = client_id\\)", block), TRUE,
       "rename_empresa_initials() now writes pasivos_provisions back")
  .chk(grepl("load_pasivos_liabilities\\(client_id = client_id\\)", block), TRUE,
       "rename_empresa_initials() now reads pasivos_liabilities")
  .chk(grepl("save_pasivos_liabilities\\(df, client_id = client_id\\)", block), TRUE,
       "rename_empresa_initials() now writes pasivos_liabilities back")
  .chk(grepl('touched, "pasivos_provisions"', block), TRUE,
       "pasivos_provisions is recorded in the touched-tables report")
  .chk(grepl('touched, "pasivos_liabilities"', block), TRUE,
       "pasivos_liabilities is recorded in the touched-tables report")
}

# ── 2b. Behavioral: run the REAL function end-to-end with mocked S3 I/O ───
# Mocks every load_*/save_* dependency so the real rename_empresa_initials()
# logic (not a reimplementation) can be exercised without real S3 access.
# Only pasivos_provisions/pasivos_liabilities carry a matching row here --
# proves the new blocks actually fire and actually rename in place.
{
  # Mocks must NOT leak into the shared global environment other test files
  # in the same _run_confirmed_logic.R process rely on -- .run_module()
  # sources each file into its own env(parent = globalenv()), so a bare `<<-`
  # at top level here would create/overwrite these names in globalenv()
  # itself, silently breaking any later test file that calls the real
  # versions (found while writing this test: it broke
  # test_stage11_cashflow_export_confirmed.R's provisions handling). Run
  # inside a function so on.exit() has a real frame to attach to; save
  # whatever was there before (if anything) and restore it no matter how
  # the test body exits.
  .run_mocked <- function() {
    .written <- new.env()
    .mock_names <- c("load_ctas_cuentas", "save_ctas_cuentas", "load_bancos_cuentas",
                      "save_bancos_cuentas", "load_proveedores", "save_proveedores",
                      ".s3_read_with", ".s3_write", ".normalize",
                      "load_interco_v2", "save_interco_v2",
                      "load_pasivos_provisions", "save_pasivos_provisions",
                      "load_pasivos_liabilities", "save_pasivos_liabilities")
    .had_prior <- vapply(.mock_names, exists, logical(1), envir = globalenv(), inherits = FALSE)
    .prior <- lapply(.mock_names, function(nm) {
      if (exists(nm, envir = globalenv(), inherits = FALSE)) get(nm, envir = globalenv()) else NULL
    })
    names(.prior) <- .mock_names
    on.exit({
      for (nm in .mock_names) {
        if (isTRUE(.had_prior[[nm]])) assign(nm, .prior[[nm]], envir = globalenv())
        else if (exists(nm, envir = globalenv(), inherits = FALSE)) rm(list = nm, envir = globalenv())
      }
    }, add = TRUE)

    assign("load_ctas_cuentas",   function(client_id = NULL) tibble(Empresa = character()), envir = globalenv())
    assign("save_ctas_cuentas",   function(df, client_id = NULL) invisible(TRUE), envir = globalenv())
    assign("load_bancos_cuentas", function(client_id = NULL) tibble(empresa = character()), envir = globalenv())
    assign("save_bancos_cuentas", function(df, client_id = NULL) invisible(TRUE), envir = globalenv())
    assign("load_proveedores",    function(client_id = NULL) tibble(Empresa = character()), envir = globalenv())
    assign("save_proveedores",    function(df, client_id = NULL) invisible(TRUE), envir = globalenv())
    assign(".s3_read_with",       function(key, client_id = NULL) NULL, envir = globalenv())
    assign(".s3_write",           function(obj, key, client_id = NULL) invisible(TRUE), envir = globalenv())
    assign(".normalize",          function(df, schema_fn) df, envir = globalenv())
    assign("load_interco_v2",     function(client_id = NULL) list(companies = list()), envir = globalenv())
    assign("save_interco_v2",     function(reg, client_id = NULL) invisible(TRUE), envir = globalenv())

    assign("load_pasivos_provisions", function(client_id = NULL)
      tibble(id = c("p1", "p2"), empresa = c("NCS", "OTHER")), envir = globalenv())
    assign("save_pasivos_provisions", function(df, client_id = NULL) {
      assign("provisions", df, envir = .written); invisible(TRUE)
    }, envir = globalenv())
    assign("load_pasivos_liabilities", function(client_id = NULL)
      tibble(id = c("l1"), empresa = c("NCS")), envir = globalenv())
    assign("save_pasivos_liabilities", function(df, client_id = NULL) {
      assign("liabilities", df, envir = .written); invisible(TRUE)
    }, envir = globalenv())

    touched <- rename_empresa_initials("NCS", "NG", client_id = "test")

    .chk("pasivos_provisions" %in% touched, TRUE,
         "the real function reports pasivos_provisions as touched when a matching row exists")
    .chk("pasivos_liabilities" %in% touched, TRUE,
         "the real function reports pasivos_liabilities as touched when a matching row exists")
    .chk(.written$provisions$empresa, c("NG", "OTHER"),
         "pasivos_provisions: only the NCS row is renamed to NG, the unrelated row is untouched")
    .chk(.written$liabilities$empresa, "NG",
         "pasivos_liabilities: the NCS row is renamed to NG")
  }
  .run_mocked()
}

# ── 3. void_abono() finally wired to a real UI (Historial de confirmaciones) ─
# void_abono() itself already existed and is tested elsewhere by nothing --
# verify it works as the new UI depends on, plus static-scan the new
# Deshacer button and its observer in R/bancos_module.R.
{
  .extract_fn("R/persistence.R", "void_abono")
  ab <- tibble(id = c("a1", "a2", "a3"), status = c("active", "active", "voided"))
  out <- void_abono(ab, "a1")
  .chk(out$status, c("voided", "active", "voided"),
       "void_abono() flips only the targeted id to voided, leaves the rest untouched")
  out2 <- void_abono(ab, character(0))
  .chk(identical(out2, ab), TRUE,
       "void_abono() with no ids is a true no-op (returns the input unchanged)")

  txt <- readLines("R/bancos_module.R", warn = FALSE)
  .chk(any(grepl('ns\\("undo_abono"\\)', txt)), TRUE,
       "historial_tbl now renders a Deshacer button wired to undo_abono for active abono rows")
  .chk(any(grepl('Tipo\\s*=\\s*.*badge bg-warning text-dark.*Abono', txt)), TRUE,
       "active abonos are shown with a distinct 'Abono' badge, not confused with Pago/Cobro")

  start <- grep("observeEvent\\(input\\$undo_abono,", txt)
  .chk(length(start) > 0, TRUE, "found the undo_abono observer to scan")
  if (length(start)) {
    block <- paste(.strip_comments(txt[start[1]:min(start[1] + 20, length(txt))]), collapse = "\n")
    .chk(grepl("void_abono\\(ab, ab_id\\)", block), TRUE,
         "the undo_abono observer calls the real void_abono(), not a reimplementation")
    .chk(grepl("save_abonos\\(updated", block), TRUE,
         "the undo_abono observer persists the voided status via save_abonos()")
  }
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
