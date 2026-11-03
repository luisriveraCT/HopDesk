# Sending invoices to the Agenda needs can_register_payments on every path
# (Luis, 2026-11-03): Calendario's day, Vencidos and the lupa, Intercompany,
# the Mapa IC, the manual entry's "a la Agenda" and the partial abono.
# Static: every upsert_pagar_hoy() outside the Agenda itself has the check
# between the start of its handler/function and the call.
# Known exceptions, each with its reason:
#   - pasivos_module.R: converting a provision (can_convert_provisiones);
#   - staging_browse_module.R: the partial abono, checked by its caller
#     (app.R: autorizado = puede_o_avisa(shared, "can_register_payments")).
# PENDIENTES is a ratchet: sites still without the check, owned by other
# fronts while they convert them. It only goes down.
# Run from the repo root:  Rscript tests/test_agenda_permiso.R
library(testthat)

EXCEPCIONES <- c("R/pasivos_module.R", "R/staging_browse_module.R")
PENDIENTES  <- 0L   # every path checks it now

archivos <- setdiff(c(list.files("R", pattern = "\\.R$", full.names = TRUE, recursive = TRUE), "app.R"),
                    c("R/persistence.R", "R/pagar_hoy_module.R", EXCEPCIONES))

sin_permiso <- unlist(lapply(archivos, function(f) {
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")
  x[grepl("^\\s*#", x)] <- ""
  inicio <- grepl("^\\s*(observeEvent\\(|observe\\(\\{|[.[:alnum:]_]+ <- function\\()", x)
  sitios <- which(grepl("upsert_pagar_hoy(", x, fixed = TRUE) & !grepl("<- function", x, fixed = TRUE))
  malos <- Filter(function(k) {
    j <- max(c(1L, which(inicio[seq_len(k)])))
    !any(grepl("can_register_payments", x[j:k], fixed = TRUE))
  }, sitios)
  if (length(malos)) sprintf("%s:%d", f, malos) else NULL
}))

test_that("mandar a la Agenda revisa can_register_payments (trinquete de pendientes)", {
  for (s in sin_permiso) message("  sin permiso: ", s)
  expect_lte(length(sin_permiso), PENDIENTES)
})

test_that("la captura manual solo manda a la Agenda con el permiso", {
  x <- readLines("app.R", warn = FALSE, encoding = "UTF-8")
  i <- grep("send_to_agenda <- isTRUE(input$me_agenda_active)", x, fixed = TRUE)
  expect_length(i, 1)
  expect_true(grepl('puede_o_avisa(shared, "can_register_payments")', x[i + 1L], fixed = TRUE))
})

test_that("el abono parcial sigue pidiendo el permiso", {
  x <- readLines("app.R", warn = FALSE, encoding = "UTF-8")
  expect_true(any(grepl('autorizado = function() puede_o_avisa(shared, "can_register_payments")', x, fixed = TRUE)))
})
