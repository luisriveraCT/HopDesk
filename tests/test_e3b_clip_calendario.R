# E3b-C: the requisition's evidence clip in the day's detail (Calendario)
# and in the confirmations' history (Bancos).
# Run from the repo root: Rscript --vanilla tests/test_e3b_clip_calendario.R
suppressPackageStartupMessages({ library(testthat); library(shiny) })
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R")) source(f)

test_that("the history of confirmations has the clip piece (class C only has it if the registry adds it)", {
  expect_false("adjuntos" %in% c(hd_clases$C$siempre, hd_clases$C$opcional))
  expect_true("adjuntos" %in% hd_piezas_de("historial_conf"))
  # Nothing else in the history's pieces changes.
  expect_identical(setdiff(hd_piezas_de("historial_conf", c("acc_fila")), "adjuntos"),
                   HD_PIEZAS_VALIDAS[HD_PIEZAS_VALIDAS %in% union(hd_clases$C$siempre, "acc_fila")])
})
