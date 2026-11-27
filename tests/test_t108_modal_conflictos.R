# =============================================================================
# tests/test_t108_modal_conflictos.R
# T108: the edit-confirm panel ("Confirmación de cambios") is "xl". At "l"
# (800 px) two "fecha · monto" columns with amounts in the millions overflowed
# the table at Luis' 1293 px: horizontal scroll and "Otro monto" cut (measured
# in Chrome on the fixture, 2026-11-27). The wizard that opens it is "xl" too
# (T109), so the two modals of an edit are the same width.
# Run from project root:  Rscript tests/test_t108_modal_conflictos.R
# =============================================================================
suppressPackageStartupMessages(library(shiny))
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/pasivos_edit_confirm_module.R"))
  suppressMessages(source(f))
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}

html <- as.character(.pec_modal_ui())
ok(grepl("modal-dialog modal-xl", html, fixed = TRUE), "el panel de conflictos se dibuja con modal-xl")
ok(!grepl("modal-lg", html, fixed = TRUE), "y ya no con modal-lg (800 px)")
ok(grepl("pec_tabla", html, fixed = TRUE), "con su tabla dentro")

# The size each modalDialog of the edit flow declares, read from the code.
tamano <- function(archivo, funcion) {
  ex <- parse(archivo, keep.source = FALSE)
  out <- character(0)
  buscar <- function(e) {
    if (is.call(e)) {
      fn <- e[[1]]
      es_modal <- (is.name(fn) && identical(as.character(fn), "modalDialog")) ||
        (is.call(fn) && identical(as.character(fn[[1]]), "::") && identical(as.character(fn[[3]]), "modalDialog"))
      if (es_modal && !is.null(e$size)) out <<- c(out, as.character(e$size))
      for (k in as.list(e)[-1]) if (!missing(k)) buscar(k)
    }
  }
  for (e in ex)
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && identical(as.character(e[[2]]), funcion)) buscar(e[[3]])
  out
}
ok(identical(tamano("R/pasivos_edit_confirm_module.R", ".pec_modal_ui"), "xl"),
   "el código declara size = \"xl\" en .pec_modal_ui()")
ok("xl" %in% tamano("R/pasivos_wizard_module.R", "setup_pasivos_wizard") ||
     any(grepl('size *= *"xl"', readLines("R/pasivos_wizard_module.R", warn = FALSE))),
   "el asistente que lo abre también es xl (T109)")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
