# Server-side locks: every write the plan names checks its permission inside
# the observer itself, so firing the event by hand (bypassing a hidden
# button) is refused too. Static scan: the check must be the first statement.
# Run from the repo root: Rscript tests/test_candados_servidor.R
library(testthat)

primera_linea <- function(archivo, apertura) {
  l <- readLines(archivo, warn = FALSE, encoding = "UTF-8")
  i <- grep(apertura, l, fixed = TRUE)
  if (length(i) != 1) return(NA_character_)
  trimws(l[i + 1])
}

CANDADOS <- list(
  list("app.R", "observeEvent(input$btn_add_entry, {", "can_register_payments"),
  list("app.R", "observeEvent(input$pick_ar, {", "can_register_payments"),
  list("app.R", "observeEvent(input$pick_ap, {", "can_register_payments"),
  list("app.R", "observeEvent(input$pick_abono, {", "can_register_payments"),
  list("app.R", "observeEvent(input$me_save, {", "can_register_payments"),
  list("R/pagar_hoy_module.R", 'observeEvent(input[[paste0("confirm_ap_", emp)]], {', "can_confirm_payments"),
  list("R/pagar_hoy_module.R", 'observeEvent(input[[paste0("do_confirm_ap_", emp)]], {', "can_confirm_payments"),
  list("R/pagar_hoy_module.R", 'observeEvent(input[[paste0("confirm_ar_", emp)]], {', "can_confirm_payments"),
  list("R/pagar_hoy_module.R", 'observeEvent(input[[paste0("do_confirm_ar_", emp)]], {', "can_confirm_payments"),
  list("R/pasivos_module.R", "shiny::observeEvent(input$pcm_save_provision, ignoreInit = TRUE, {", "pasivos.edit_provision"),
  list("R/pasivos_module.R", "shiny::observeEvent(input$pcm_delete_provision, ignoreInit = TRUE, {", "pasivos.edit_provision"),
  list("R/pasivos_module.R", "shiny::observeEvent(input$pcm_delete_confirm, ignoreInit = TRUE, {", "pasivos.edit_provision"),
  list("R/pasivos_module.R", "shiny::observeEvent(input$pcm_do_undo_conversion, ignoreInit = TRUE, {", "pasivos.convert_to_item"),
  list("R/pasivos_module.R", "shiny::observeEvent(input$pcm_do_delete_converted, ignoreInit = TRUE, {", "pasivos.convert_to_item"),
  list("R/pasivos_list_module.R", "shiny::observeEvent(input$plist_confirm_archive, ignoreInit = TRUE, {", "pasivos.delete_liability"),
  list("R/pasivos_wizard_module.R", "shiny::observeEvent(input$pwiz_save, ignoreInit = TRUE, {", "pasivos.edit_liability"),
  list("R/pasivos_edit_confirm_module.R", "shiny::observeEvent(input$pec_save, ignoreInit = TRUE, {", "pasivos.edit_liability"),
  list("R/staging_browse_module.R", "observeEvent(input$ab_rows, {", "autorizado()")
)

test_that("cada escritura revisa su permiso antes de hacer nada", {
  for (c in CANDADOS) {
    l <- primera_linea(c[[1]], c[[2]])
    expect_true(!is.na(l) && grepl(c[[3]], l, fixed = TRUE),
                info = sprintf("%s: %s -> %s", c[[1]], c[[2]], l))
  }
})

test_that("el abono parcial falla cerrado si nadie le pasa el permiso", {
  src <- paste(readLines("R/staging_browse_module.R", warn = FALSE), collapse = "\n")
  expect_true(grepl("autorizado = function() FALSE", src, fixed = TRUE))
})
