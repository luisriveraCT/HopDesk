# =============================================================================
# tests/test_pasivos_list_escape.R
# Lista de pasivos renders with escape = FALSE (for its button columns), so
# every free-text column must be HTML-escaped in R. A liability named
# `<b>Renta</b> & "Bodega"` must show literally, not as bold text.
# Run from project root:  Rscript tests/test_pasivos_list_escape.R
# =============================================================================

suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}
strip_accents <- function(x) stringi::stri_trans_general(x %||% "", "Latin-ASCII")
fmt_money <- function(x) format(x, nsmall = 2)
source("R/pasivos_schemas.R")
source("R/pasivos_list_module.R")

.pass <- 0L; .fail <- 0L
.chk_true <- function(ok, label) {
  if (isTRUE(ok)) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else            { cat(sprintf("  FAIL  %s\n", label)); .fail <<- .fail + 1L }
}

evil <- '<b>Renta</b> & "Bodega"'
liabs <- tibble::tibble(
  id = "L1", categoria = "financiero", flavor = "<i>raro</i>", subcategoria = "x",
  nombre = evil, empresa = "<script>e</script>", parte = "<u>Parte</u>",
  moneda_pago = "MXN", cotizado_en = "<s>USD</s>", estado = "active"
)
shared <- list(
  pasivos_liabilities_db = function() liabs,
  pasivos_provisions_db  = function() stop("none"),
  visible_initials       = function() NULL
)

shiny::testServer(pasivos_list_module_server, args = list(shared = shared), {
  session$setInputs(categoria_sel = c("regular", "financiero", "tarjeta"))
  df <- display_df()
  .chk_true(identical(df$Nombre[1], htmltools::htmlEscape(evil)), "Nombre escaped")
  .chk_true(!grepl("<b>", df$Nombre[1], fixed = TRUE), "no raw <b> in Nombre")
  for (col in c("Categoría", "Empresa", "Parte", "Moneda", "Estado", "Próxima provisión"))
    .chk_true(!grepl("<", df[[col]][1], fixed = TRUE), sprintf("no raw '<' in %s", col))
  .chk_true(grepl("<button", df$Acciones[1], fixed = TRUE), "Acciones still HTML buttons")
})

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
