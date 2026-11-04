# Every Bancos action that writes checks can_manage_bancos on the server
# (Luis, 2026-11-02): hiding the tab is not a permission. Static: each write
# handler starts with the check, and every save in bancos_module.R sits inside
# one of them -- except the one-time cleanup on load, a system action.
# Run from the repo root:  Rscript tests/test_bancos_permiso.R
library(testthat)

src <- readLines("R/bancos_module.R", encoding = "UTF-8")
CHECK <- 'if (!puede_o_avisa(shared, "can_manage_bancos")) return()'

HANDLERS <- c("observeEvent(input$do_eliminar_confirm, {", ".do_vin_keep_a <- function() {",
              "observeEvent(input$vin_keep_b, {", "observeEvent(input$do_add_manual, {",
              "observeEvent(input$sug_keep_a, {", "observeEvent(input$sug_keep_b, {",
              "observeEvent(input$do_reasignar_confirm, {", "observeEvent(input$do_eliminar_sesion_confirm, {",
              "observeEvent(input$do_import, {", ".bnc_deshacer_eliminacion <- function(mov_id) {",
              ".bnc_deshacer_abono <- function(ab_id) {", "observeEvent(input$do_delete_conf_confirm, {",
              ".bnc_deshacer_confirmacion <- function(conf_id) {")

test_that("cada handler de Bancos que escribe empieza revisando el permiso", {
  for (h in HANDLERS) {
    i <- which(trimws(src) == h)
    expect_length(i, 1)
    expect_identical(trimws(src[i + 1L]), CHECK, info = h)
  }
})

test_that("ningún guardado de Bancos queda fuera de un handler con el permiso", {
  inicio <- grepl("^\\s*(observeEvent\\(|observe\\(\\{|[.[:alnum:]_]+ <- function\\()", src)
  guarda <- grepl("save_[a-z_]+\\(|modificar_[a-z_]+\\(|\\.s3_write\\(", src) &
            !grepl("<- function", src) & !grepl("^\\s*#", src)
  for (k in which(guarda)) {
    j <- max(which(inicio[seq_len(k)]))
    cabeza <- trimws(src[j])
    limpieza <- identical(cabeza, "observe({") &&
                any(grepl("One-time cleanup", src[max(1L, j - 8L):j], fixed = TRUE))
    expect_true(cabeza %in% HANDLERS || limpieza,
                info = sprintf("bancos_module.R:%d (%s) dentro de: %s", k, trimws(src[k]), cabeza))
  }
})

test_that("valores por nivel (Luis, 2026-11-02): admin, dev, finance y hopdesk sí; analysis y compras no", {
  `%||%` <- function(a, b) if (is.null(a)) b else a
  source("R/i18n.R"); source("R/tier_registry.R"); source("R/permisos.R")
  u <- function(t) list(tier = t, permisos = permisos_resueltos(t))
  for (t in c("admin", nivel_ti(), "finance", "hopdesk", "principal"))
    expect_true(puede(u(t), "can_manage_bancos"), info = t)
  for (t in c("analysis", "compras")) expect_false(puede(u(t), "can_manage_bancos"), info = t)
  expect_false(permiso_interno("can_manage_bancos"))
})
