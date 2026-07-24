# =============================================================================
# tests/test_stage13_intercompany_confirmed.R
# Stage 13 of the ledger-integrity master plan (source
# docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md §5/§7, Stage G) -- the final stage:
# Intercompany's .filter_ic() (R/interco_module.R) previously carried its own
# standalone 5th independent "is this invoice confirmed" implementation
# (a hand-rolled .ckey() over bancos_confirmados + the dead
# pagar_hoy_db.status=="confirmed" source), missing papelera SAP ghosts
# entirely and matching against the abono-NETTED balance rather than the
# confirmation-time amount.
#
# Now calls the canonical compute_confirmed_flags() (R/data_pipeline.R)
# directly against the raw SAP snapshot data .filter_ic() already works
# with (no "source" column at all there -- same shape as Reporte's Pulse,
# Stage 12) -- inheriting the amount-match guard automatically (Stage 10)
# and picking up papelera ghosts, the source this module was previously
# missing. The pre-netting amount is captured into Saldo_original before
# the abono-netting mutation runs, so the amount-match guard is never
# stale relative to a later abono the way the old .ckey() implementation
# risked being.
#
# .filter_ic() is a closure inside a moduleServer-scoped reactive, not
# directly unit-testable -- static scans for the wiring, plus a behavioral
# simulation of the exact capture -> net -> compute_confirmed_flags ->
# filter sequence against the real compute_confirmed_flags().
# =============================================================================

cat("── Stage 13: Intercompany migrated onto the canonical function ─────────\n")

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
.extract_fn("R/persistence.R", "active_abonos_summary")
.strip_comments <- function(lines) sub("#.*$", "", lines)

# ── 1. Static scan: .filter_ic() wired to the canonical function ──────────
{
  txt <- readLines("R/interco_module.R", warn = FALSE)
  start <- grep("\\.filter_ic <- function", txt)
  end   <- grep("^      list\\(", txt)
  end   <- end[end > start[1]][1]
  .chk(length(start) > 0 && length(end) > 0, TRUE, "found .filter_ic() to scan")
  if (length(start) && length(end)) {
    block <- paste(.strip_comments(txt[start[1]:end]), collapse = "\n")
    .chk(grepl("compute_confirmed_flags\\(", block), TRUE,
         ".filter_ic() calls the canonical compute_confirmed_flags()")
    .chk(grepl("\\.ckey\\s*<-\\s*function", block), FALSE,
         "the old standalone .ckey() implementation is actually gone, not just unused")
    .chk(grepl("ph_db", block), FALSE,
         "the dead pagar_hoy_db.status=='confirmed' source (ph_db) is gone too")
    .chk(grepl('Saldo_original.*<-.*df\\[\\[amt_col\\]\\]', block), TRUE,
         "the pre-netting amount is captured into Saldo_original")
    # Order matters: the capture must happen BEFORE the abono-netting
    # mutation, or the amount-match guard would match the netted balance.
    capture_pos <- regexpr("Saldo_original.*<-.*df\\[\\[amt_col\\]\\]", block)
    net_pos     <- regexpr("df\\[\\[amt_col\\]\\] <- pmax\\(0", block)
    .chk(capture_pos > 0 && net_pos > 0 && capture_pos < net_pos, TRUE,
         "Saldo_original is captured BEFORE the abono-netting mutation runs, not after")
  }

  .chk(any(grepl("papelera_raw", txt)), TRUE,
       "papelera data is now fetched at all (previously missing entirely)")
}

# ── 2. Behavioral: the exact capture -> net -> compute -> filter sequence ──
{
  .sim_filter_ic <- function(df, ledger, conf_db, papelera_df, abonos_df) {
    amt_col <- "Saldo vencido"
    df[["Saldo_original"]] <- df[[amt_col]]
    ab_sum <- active_abonos_summary(abonos_df) |> dplyr::filter(.data$ledger == !!ledger)
    if (nrow(ab_sum) > 0) {
      ab_key <- paste(ab_sum$Empresa, ab_sum$Moneda, ab_sum$Documento)
      ab_lookup <- setNames(ab_sum$abono_total, ab_key)
      df_key <- paste(df$Empresa, df$Moneda, df$Documento)
      abono_total <- dplyr::coalesce(unname(ab_lookup[df_key]), 0)
      df[[amt_col]] <- pmax(0, df[[amt_col]] - abono_total)
    }
    df <- compute_confirmed_flags(df, ledger, conf_db, papelera_df)
    df[!(df[["confirmed"]] %in% TRUE), , drop = FALSE]
  }

  df <- tibble::tibble(
    Empresa   = c("ACME", "ACME", "ACME", "ACME"),
    Moneda    = c("MXN", "MXN", "MXN", "MXN"),
    Documento = c("F-1", "F-2", "F-3", "F-4"),
    `Saldo vencido` = c(500, 300, 200, 1000),
    check.names = FALSE
  )
  conf_db <- tibble::tibble(
    empresa = "ACME", documento = "F-1", moneda = "MXN",
    tipo = "pago", eliminado = FALSE, importe = 500
  )
  # F-2 was NEVER in bancos_confirmados/pagar_hoy_db -- only ever became a
  # papelera SAP ghost. This is the exact source Intercompany was missing
  # entirely before this stage.
  pap_db <- tibble::tibble(
    ledger = "AP", source = "sap", Empresa = "ACME", Moneda = "MXN", Documento = "F-2"
  )
  abonos_df <- tibble::tibble(
    ledger = character(), Empresa = character(), Moneda = character(),
    Documento = character(), importe = numeric(), fecha_abono = as.Date(character()),
    status = character()
  )

  out <- .sim_filter_ic(df, "AP", conf_db, pap_db, abonos_df)
  .chk(nrow(out), 2L, "2 of 4 rows survive: F-3 (open) and F-4 (open, no confirmation of any kind)")
  .chk(sort(out$Documento), c("F-3", "F-4"), "F-1 (bancos_confirmados) and F-2 (papelera ghost -- the previously-missed source) are both excluded")
  .chk("F-2" %in% out$Documento, FALSE,
       "specifically: F-2, confirmed ONLY via a papelera ghost, is now correctly excluded -- the concrete regression this stage fixes")
}

# ── 3. Behavioral: abono applied around confirmation time still matches ───
{
  .sim_filter_ic <- function(df, ledger, conf_db, papelera_df, abonos_df) {
    amt_col <- "Saldo vencido"
    df[["Saldo_original"]] <- df[[amt_col]]
    ab_sum <- active_abonos_summary(abonos_df) |> dplyr::filter(.data$ledger == !!ledger)
    if (nrow(ab_sum) > 0) {
      ab_key <- paste(ab_sum$Empresa, ab_sum$Moneda, ab_sum$Documento)
      ab_lookup <- setNames(ab_sum$abono_total, ab_key)
      df_key <- paste(df$Empresa, df$Moneda, df$Documento)
      abono_total <- dplyr::coalesce(unname(ab_lookup[df_key]), 0)
      df[[amt_col]] <- pmax(0, df[[amt_col]] - abono_total)
    }
    df <- compute_confirmed_flags(df, ledger, conf_db, papelera_df)
    list(kept = df[!(df[["confirmed"]] %in% TRUE), , drop = FALSE], all = df)
  }

  df <- tibble::tibble(Empresa = "ACME", Moneda = "MXN", Documento = "F-5",
                       `Saldo vencido` = 100, check.names = FALSE)
  # Confirmed at the FULL original amount (100), before any abono existed.
  conf_db <- tibble::tibble(empresa = "ACME", documento = "F-5", moneda = "MXN",
                            tipo = "pago", eliminado = FALSE, importe = 100)
  # A $40 abono applied AFTER confirmation -- Saldo vencido now reads 60.
  abonos_df <- tibble::tibble(
    ledger = "AP", Empresa = "ACME", Moneda = "MXN", Documento = "F-5",
    importe = 40, fecha_abono = Sys.Date(), status = "active"
  )
  res <- .sim_filter_ic(df, "AP", conf_db, NULL, abonos_df)
  .chk(res$all$`Saldo vencido`[1], 60,
       "the abono still nets the DISPLAYED balance down to 60 -- netting itself is untouched")
  .chk(nrow(res$kept), 0L,
       "but the confirmation still correctly matches (against Saldo_original=100, not the netted 60) and the row stays excluded -- the abono does not reopen it")
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
