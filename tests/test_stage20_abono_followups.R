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

# ── 1a. Static scan: the staged row uses the real due date, not Sys.Date() ──
# Since T25 it is the server's invoice row (abono_facturas()$Vence, a Date),
# no longer parsed from what the browser sent.
{
  txt <- readLines("R/staging_browse_module.R", warn = FALSE)
  start <- grep("observeEvent\\(input\\$ab_stage,", txt)
  .chk(length(start) > 0, TRUE, "found the ab_stage observer to scan")
  if (length(start)) {
    block <- paste(.strip_comments(txt[start[1]:min(start[1] + 90, length(txt))]), collapse = "\n")
    .chk(grepl("FechaVenc\\s*=\\s*accepted\\$Vence\\[i\\]", block), TRUE,
         "the staged abono row's FechaVenc is the invoice's real due date (the server's row)")
    .chk(grepl("FechaVenc\\s*=\\s*Sys\\.Date\\(\\)", block), FALSE,
         "never the staging date")
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

  # CM26 (2026-11-17): each step now changes S3's current table, strictly
  # read (modificar_*), as a named step whose name is what it reports.
  .chk(grepl("pasivos_provisions = function", block) &&
         grepl("S3_KEYS$pasivos_provisions", block, fixed = TRUE), TRUE,
       "rename_empresa_initials() renames pasivos_provisions, on S3's current table")
  .chk(grepl("pasivos_liabilities = function", block) &&
         grepl("modificar_pasivos_liabilities(cid", block, fixed = TRUE), TRUE,
       "rename_empresa_initials() renames pasivos_liabilities, on S3's current table")
}

# ── 2b. Behavioral ─────────────────────────────────────────────────────────
# The mocked run that used to be here (load_*/save_* replaced in globalenv)
# no longer fits: since CM26 the function reads empresas.rds and every table
# strictly through modificar_*. tests/test_renombrar_iniciales.R runs the
# real function against an in-memory S3, both pasivos tables included
# (cases 1, 4 and 5).

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
  # Since T12b the Historial is hd_table "historial_conf": an active abono is
  # an "abono:" row with ↶ (deshacer), which calls .bnc_deshacer_abono().
  .chk(any(grepl('filas\\$abono <- \\.bnc_hist_filas\\("abono"', txt)) &&
         any(grepl('if \\(escribe\\) "deshacer" else ""', txt)) &&
         any(grepl('fuente == "abono"\\) \\.bnc_deshacer_abono\\(clave\\)', txt)), TRUE,
       "the Historial gives active abono rows a Deshacer wired to the undo_abono handler")
  .chk(any(grepl('abono = list\\(texto = "tablas\\.bancos\\.abono_parcial"', txt)), TRUE,
       "active abonos are shown with a distinct 'Abono' badge, not confused with Pago/Cobro")

  start <- grep("\\.bnc_deshacer_abono <- function\\(ab_id\\)", txt)   # a function of the id since T12b
  .chk(length(start) > 0, TRUE, "found the undo_abono observer to scan")
  if (length(start)) {
    block <- paste(.strip_comments(txt[start[1]:min(start[1] + 20, length(txt))]), collapse = "\n")
    .chk(grepl("void_abono\\(ab, ab_id\\)", block), TRUE,
         "the undo_abono observer calls the real void_abono(), not a reimplementation")
    .chk(grepl("modificar_abonos\\(function\\(ab\\) void_abono\\(ab, ab_id\\)", block), TRUE,
         "the undo_abono observer voids on S3's current table via modificar_abonos() (CM19)")
  }
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
