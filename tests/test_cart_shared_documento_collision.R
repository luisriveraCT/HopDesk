# =============================================================================
# tests/test_cart_shared_documento_collision.R
# Live bug found 2026-07-25: two different manual invoices under the same
# Empresa, sharing the same free-text Documento ("test") but with different
# Parte ("test" vs "test1") and different Importe (1.00 vs 0.50), appeared
# linked in the calendar day-modal's cart -- staging one made the OTHER's
# checkmark button turn green too, and vice versa on removal. The user's own
# description: "as if there was a ghost group that controlled both items
# (spoiler; there's clearly not)".
#
# Root cause: R/ledger_module.R's cart rendering computed "is this invoice
# currently staged in Agenda" using only (Empresa, Documento) as the lookup
# key, in three places:
#   1. The "EnProceso" join (grp-level, currently unused for display but
#      still computed and carried through the pipeline).
#   2. output$cart_table's per-group `is_staged`/`n_in_cart` check (drives
#      the single-invoice group's checkmark button).
#   3. The expanded-subrow `in_cart_ii` check (drives each subrow's own
#      checkmark button within a multi-invoice Parte group).
# Documento alone is not a unique invoice identifier -- it's a free-text
# field a user can reuse across entries, exactly as happened here. The
# actual stage/unstage operation itself (cart_inv_click's own `already`
# check, and the group-level cart_<i> toggle's `already` check) already
# correctly used the full (Empresa, Moneda, Documento, Importe) key -- only
# the DISPLAY layer had the narrower, colliding key. Fixed by widening all
# three display-layer checks to the same four-column key the real
# stage/unstage logic already trusted.
# =============================================================================

cat("── Cart display: shared-Documento invoices no longer appear linked ─────\n")

suppressPackageStartupMessages(library(dplyr))

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

# ── 1. Behavioral: replicate the exact bug and the exact fix ───────────────
{
  # Two manual invoices, same Empresa/Documento, different Parte/Importe --
  # exactly the real-world collision reported live.
  detail <- tibble::tibble(
    Empresa   = c("Networks & Logistics", "Networks & Logistics"),
    Moneda    = c("MXN", "MXN"),
    Documento = c("test", "test"),
    Parte     = c("test", "test1"),
    Importe   = c(1.00, 0.50)
  )
  # Only the FIRST invoice ("test"/Importe=1.00) is actually staged.
  staged_now <- tibble::tibble(
    Empresa = "Networks & Logistics", Moneda = "MXN",
    Documento = "test", Importe = 1.00
  )

  test1_row <- detail[detail[["Parte"]] == "test1", , drop = FALSE]

  # Buggy key (Empresa+Documento only): falsely matches test1 against test's
  # staged row, since both share the same Documento.
  buggy_keys    <- test1_row[, c("Empresa","Documento"), drop = FALSE]
  buggy_staged  <- staged_now[, c("Empresa","Documento"), drop = FALSE]
  buggy_match   <- nrow(merge(buggy_keys, buggy_staged, by = c("Empresa","Documento")))
  .chk(buggy_match > 0, TRUE,
       "control: the OLD (Empresa+Documento only) key DOES falsely match test1 against test's staged row -- reproduces the reported bug")

  # Fixed key (Empresa+Moneda+Documento+Importe): test1's own Importe (0.50)
  # never matches the staged row's Importe (1.00), so no false link.
  fixed_keys   <- test1_row[, c("Empresa","Moneda","Documento","Importe"), drop = FALSE]
  fixed_staged <- staged_now[, c("Empresa","Moneda","Documento","Importe"), drop = FALSE]
  fixed_match  <- nrow(merge(fixed_keys, fixed_staged, by = c("Empresa","Moneda","Documento","Importe")))
  .chk(fixed_match, 0L,
       "fix: the NEW (Empresa+Moneda+Documento+Importe) key correctly does NOT match test1 against test's staged row")

  # Control: the actually-staged invoice ("test"/Importe=1.00) still
  # correctly matches itself under the fixed key -- the fix doesn't just
  # make everything never match.
  test_row    <- detail[detail[["Parte"]] == "test", , drop = FALSE]
  self_keys   <- test_row[, c("Empresa","Moneda","Documento","Importe"), drop = FALSE]
  self_match  <- nrow(merge(self_keys, fixed_staged, by = c("Empresa","Moneda","Documento","Importe")))
  .chk(self_match > 0, TRUE,
       "the fix still correctly matches the ACTUALLY staged invoice against itself -- not a blanket never-match")
}

# ── 2. The day modal on the table engine (T05) ─────────────────────────────
# The three display sites (staged_now, n_in_cart, in_cart_ii) and the unused
# EnProceso join are gone: whether a row is already in the Agenda is now
# dia_datos()'s .agregado (R/dia_module.R), with the same four-column key.
# The real function, with the incident's two invoices.
{
  for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
              "R/vencidos_module.R", "R/dia_module.R")) source(f)
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
  SAP_B1_ERP_TYPE <- "sap_b1_service_layer"
  # is_erp_sourced(): the views lock a confirmed ERP invoice in $0 (only ERP ones).
  for (e in parse("R/global.R", keep.source = FALSE)) if (is.call(e) && length(e) > 1L && identical(e[[2]], as.name("is_erp_sourced"))) eval(e)
  dia <- as.data.frame(detail)
  dia$source <- "manual"; dia$id <- c("MAN-1", "MAN-2")
  agenda <- data.frame(ledger = "AP", Empresa = "Networks & Logistics", Moneda = "MXN", Documento = "test",
                       Importe = 1.00, status = "pending", stringsAsFactors = FALSE)
  d <- dia_datos(dia, "AP", NULL, agenda)
  .chk(d$.agregado[d$Parte == "test"], TRUE, "dia_datos: the staged invoice (test, 1.00) shows as in the Agenda")
  .chk(d$.agregado[d$Parte == "test1"], FALSE,
       "dia_datos: the other invoice sharing its Documento (test1, 0.50) does NOT -- no false link")
  src <- paste(.strip_comments(readLines("R/dia_module.R", warn = FALSE)), collapse = "\n")
  .chk(grepl('llave(col("Empresa"), col("Moneda"), documento, importe)', src, fixed = TRUE) &&
         grepl("llave(pend$Empresa, pend$Moneda, pend$Documento, pend$Importe)", src, fixed = TRUE), TRUE,
       "dia_datos's Agenda key is Empresa+Moneda+Documento+Importe on both sides")
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
