# =============================================================================
# tests/test_pasivos_list_escape.R
# A liability named `<b>Renta</b> & "Bodega"` must show literally, never as
# bold text. Since T18 the Lista is on hd_table, which paints every cell with
# textContent (R/hd_columnas.R, www/hdtable.js): the rows carry the text as
# it is -- escaping it in R would show "&lt;b&gt;" -- and no cell holds a
# button's HTML (the row's buttons are the engine's, by .acciones).
# Run from project root:  Rscript tests/test_pasivos_list_escape.R
# =============================================================================

suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}
fmt_money <- function(x) format(x, nsmall = 2)
has_capability <- function(user, cap) TRUE
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/pasivos_schemas.R", "R/pasivos_list_module.R"))
  suppressMessages(source(f))

.pass <- 0L; .fail <- 0L
.chk_true <- function(ok, label) {
  if (isTRUE(ok)) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else            { cat(sprintf("  FAIL  %s\n", label)); .fail <<- .fail + 1L }
}

evil <- '<b>Renta</b> & "Bodega"'
liabs <- tibble::tibble(
  id = "L1", categoria = "financiero", flavor = "<i>raro</i>", subcategoria = "x",
  nombre = evil, empresa = "<script>e</script>", parte = "<u>Parte</u>",
  moneda_pago = "MXN", cotizado_en = "<s>USD</s>", estado = "active",
  updated_at = Sys.time(), updated_by = "ana"
)
shared <- list(
  pasivos_liabilities_db = function() liabs,
  pasivos_provisions_db  = function() stop("none"),
  visible_initials       = function() NULL,
  current_user           = function() "ana"
)

shiny::testServer(pasivos_list_module_server, args = list(shared = shared), {
  session$flushReact()
  df <- lista_datos()
  .chk_true(identical(df$nombre[1], evil), "Nombre travels as written (painted as text by the engine)")
  .chk_true(identical(df$empresa[1], "<script>e</script>") && identical(df$parte[1], "<u>Parte</u>"),
            "Empresa and Parte too: never pre-escaped into &lt;...&gt;")
  .chk_true(identical(df$tipo[1], "<i>raro</i>"), "an unknown flavor shows as written")
  .chk_true(!any(grepl("<button", unlist(df), fixed = TRUE)), "no cell holds a button's HTML")
  .chk_true(identical(df$.acciones[1], "editar,inactivar,eliminar"), "the row's buttons are the engine's (.acciones)")
})
# Every visible column is a typed hd column (text cells painted with textContent)
cols_txt <- readLines("R/pasivos_list_module.R", warn = FALSE)
.chk_true(!any(grepl("escape\\s*=\\s*FALSE|DT::", cols_txt)), "no DT table with escape = FALSE is left in the module")

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
