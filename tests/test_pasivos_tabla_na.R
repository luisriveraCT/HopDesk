# =============================================================================
# tests/test_pasivos_tabla_na.R
# CM21, 2026-11-10. Pasivos > Tabla was empty in prod (networks) while Lista
# de pasivos loaded. One row did it: a manual AP item with no due date
# ("Fecha de vencimiento" NA), converted from an orphan manual provision
# (estado item_confirmed) whose confirmation was deleted -- so it is unpaid
# and reaches `if (item_date < today)`, which stops on NA. long_df()'s
# tryCatch then replaced the whole table with an empty one.
# The row below is that one, anonymised. Now an undated item is shown on its
# provision's date, a row that cannot be built is left out alone (logged by
# id), and every other row is still shown.
#
# Run from project root:  Rscript tests/test_pasivos_tabla_na.R
# =============================================================================
cat("=== test_pasivos_tabla_na ===\n\n")
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/pasivos_schemas.R")
source("R/pasivos_table_pivot.R")
for (e in parse("R/global.R", keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && identical(e[[2]], as.name("strip_accents"))) eval(e)
# confirmaciones_vigentes() as long_df() applies it (R/persistence.R), by name.
for (e in parse("R/persistence.R", keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && identical(e[[2]], as.name("confirmaciones_vigentes"))) eval(e)

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
hoy <- Sys.Date()

prov <- function(id, fecha, estado = "provisional", liability_id = NA_character_, origin = "manual") {
  p <- .schema_pasivos_provision()[NA_integer_, ]
  p$id <- id; p$estado <- estado; p$origin <- origin; p$liability_id <- liability_id
  p$empresa <- "Empresa A"; p$parte <- "Proveedor"; p$moneda_pago <- "MXN"
  p$fecha_calculada <- fecha; p$fecha_efectiva <- fecha; p$amount_pago <- 100
  p
}
lib <- .schema_pasivos_liability()[NA_integer_, ]
lib$id <- "lib1"; lib$nombre <- "Arrendamiento"; lib$empresa <- "Empresa A"
lib$categoria <- "regular"; lib$subcategoria <- ""; lib$estado <- "active"

# The real row: orphan manual provision, item_confirmed, converted into a
# manual item with no due date; its confirmation deleted, then "recovered".
provs <- dplyr::bind_rows(
  prov("p_buena_1", hoy + 3, liability_id = "lib1", origin = "schedule"),
  prov("p_buena_2", hoy + 20, liability_id = "lib1", origin = "schedule"),
  prov("p_test", hoy - 9, estado = "item_confirmed"))
mi <- tibble::tibble(id = "mi_test", ledger = "AP", Empresa = "Empresa A", Moneda = "MXN",
                     Documento = "test", Importe = 1, `Fecha de vencimiento` = as.Date(NA),
                     provision_id = "p_test")
bc <- confirmaciones_vigentes(tibble::tibble(
  confirmacion_id = c("c1", "c2"), provision_id = "p_test",
  eliminado = c(TRUE, FALSE), action = c(NA, "recovered")))
filtros <- list(empresa = character(0), categorias = c("regular", "financiero", "tarjeta"),
                subcategorias = character(0), currency = "All", search = "",
                granularity = "day", provision_type = "todas")
correr <- function(m = mi, p = provs) pasivos_table_build_long(p, m, bc, lib, filtros)

# 1. The undated item no longer empties the table
msgs <- character()
out <- tryCatch(withCallingHandlers(correr(), message = function(m) {
  msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") }), error = function(e) e)
ok("build_long does not stop on an item with no due date", is.data.frame(out))
ok("the two healthy provisions are still shown",
   is.data.frame(out) && all(c("p_buena_1", "p_buena_2") %in% out$cell_provision_id))
ok("the undated item is shown on its provision's date, overdue",
   is.data.frame(out) && any(out$cell_manual_id %in% "mi_test" & out$fecha == hoy - 9 &
                               out$cell_kind == "overdue_manual"))
ok("the log names the undated item by id", any(grepl("mi_test", msgs, fixed = TRUE)))

# 2. Neither a due date nor a provision date: that item alone is left out
p_sin <- provs; p_sin$fecha_efectiva[p_sin$id == "p_test"] <- as.Date(NA)
msgs <- character()
out2 <- tryCatch(withCallingHandlers(correr(p = p_sin), message = function(m) {
  msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") }), error = function(e) e)
ok("no date at all: build_long still returns", is.data.frame(out2))
ok("no date at all: the item is left out, the rest shown",
   is.data.frame(out2) && !any(out2$cell_manual_id %in% "mi_test") &&
     all(c("p_buena_1", "p_buena_2") %in% out2$cell_provision_id))
ok("no date at all: the log says which item was left out",
   any(grepl("mi_test", msgs, fixed = TRUE) & grepl("left out", msgs, fixed = TRUE)))

# 3. A paid item (current confirmation) still leaves the table (rule 22)
bc_pagada <- tibble::tibble(confirmacion_id = "c3", provision_id = "p_test", eliminado = FALSE, action = NA)
out3 <- suppressMessages(pasivos_table_build_long(provs, mi, bc_pagada, lib, filtros))
ok("paid undated item: not in the table", !any(out3$cell_manual_id %in% "mi_test"))

# 4. A dated item is unchanged
mi_ok <- mi; mi_ok$`Fecha de vencimiento` <- hoy + 5
out4 <- suppressMessages(correr(m = mi_ok))
ok("dated item: shown on its own due date", any(out4$cell_manual_id %in% "mi_test" & out4$fecha == hoy + 5 &
                                                   out4$cell_kind == "manual_item"))

# 5. Every row goes through the per-row guard
src <- paste(readLines("R/pasivos_table_pivot.R", warn = FALSE), collapse = "\n")
ok("provision rows and manual-item rows are each built under fila_segura()",
   grepl("function(k) fila_segura(\"provision\"", src, fixed = TRUE) &&
     grepl("function(k) fila_segura(\"manual item\"", src, fixed = TRUE))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
