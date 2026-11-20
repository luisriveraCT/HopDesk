# =============================================================================
# tests/test_t71_notas_conservadas.R
# Data loss found in the T70/T71 paso 0: editing a liability ran
# pasivos_reconcile_provisions(mode = "regenerate"), which copied the
# engine's notas (always NA) over every pending provision, so a note written
# by hand on a provision ("Pagar con cheque") vanished.
# Now a provision's own note is kept; it is replaced only when it has none or
# still has the liability's previous note (`nota_anterior`, T71).
# Against the base (1fc83b5) the first block fails.
# Run from the repo root:  Rscript tests/test_t71_notas_conservadas.R
# =============================================================================
library(testthat)
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L || (length(a) == 1L && is.na(a))) b else a
for (f in c("R/pasivos_schemas.R", "R/pasivos_engine.R")) suppressMessages(source(f))

existentes <- tibble(id = c("p1", "p2", "p3", "p4"), liability_id = "L1", occurrence_index = 1:4,
                     estado = c("provisional", "provisional", "provisional", "converted"),
                     fecha_efectiva = as.Date("2026-12-01") + 0:3 * 30, amount_pago = 100,
                     amount_pago_override = NA_real_, notas = c("Pagar con cheque", NA, "Nota del pasivo", "Ya pagada así"))
generadas <- tibble(id = paste0("g", 1:4), liability_id = "L1", occurrence_index = 1:4, estado = "provisional",
                    fecha_efectiva = as.Date("2026-12-01") + 0:3 * 30, amount_pago = 120,
                    amount_pago_override = NA_real_, notas = NA_character_)
nota_de <- function(r, id) { d <- dplyr::bind_rows(r$keep, r$update, r$conflicts); d$notas[d$id == id] }

test_that("editar el pasivo no borra la nota escrita a mano en una provisión", {
  r <- pasivos_reconcile_provisions(existentes, generadas, mode = "regenerate")
  expect_identical(nota_de(r, "p1"), "Pagar con cheque")
  expect_identical(r$update$amount_pago[r$update$id == "p1"], 120)   # the rest is updated as before
})

test_that("una provisión sin nota o con la nota anterior del pasivo toma la del motor", {
  g <- dplyr::mutate(generadas, notas = "Nota nueva")
  r <- pasivos_reconcile_provisions(existentes, g, mode = "regenerate", nota_anterior = "Nota del pasivo")
  expect_identical(nota_de(r, "p2"), "Nota nueva")                  # had none
  expect_identical(nota_de(r, "p3"), "Nota nueva")                  # had the liability's previous one
  expect_identical(nota_de(r, "p1"), "Pagar con cheque")            # its own: kept
  expect_identical(nota_de(r, "p4"), "Ya pagada así")               # converted: never touched
})

test_that("sin nota anterior, una nota no vacía siempre es propia", {
  expect_true(.nota_propia("Pagar con cheque"))
  expect_false(.nota_propia(NA_character_))
  expect_false(.nota_propia("  "))
  expect_false(.nota_propia("Nota del pasivo ", "Nota del pasivo"))
  expect_true(.nota_propia("Otra", "Nota del pasivo"))
})
