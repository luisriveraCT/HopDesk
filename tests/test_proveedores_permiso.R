# Every write path of the vendor catalog checks can_manage_proveedores on the
# server (the catalog's CLABE is where the PPL sends money). Static: each
# observer that edits or saves starts with the check, and the two write
# helpers refuse as a last line. Run from the repo root:
#   Rscript tests/test_proveedores_permiso.R
library(testthat)

src  <- readLines("R/proveedores_module.R", encoding = "UTF-8")
leg  <- readLines("R/settings/settings_proveedores.R", encoding = "UTF-8")

primera_linea <- function(lines, input) {
  i <- grep(sprintf("observeEvent(input$%s, {", input), lines, fixed = TRUE)
  expect_length(i, 1)
  trimws(lines[i + 1L])
}

test_that("cada observador que edita el catálogo empieza revisando el permiso", {
  escritores <- c("cat_new", "prov_edit_id", "prov_inactivar_id", "prov_confirm_inactivar",
                  "prov_delete_id", "cat_inactivar_bulk", "bulk_inactivar_confirm",
                  "prov_modal_save", "excel_file", "confirmar_upload", "prov_reactivar_id",
                  "prov_confirm_reactivar", "prov_del_inac_id")
  for (n in escritores)
    expect_identical(primera_linea(src, n), "if (!.puede_editar()) return()", info = n)
})

test_that("set_provs() y set_inac() exigen el permiso antes de guardar", {
  for (f in c("set_provs", "set_inac")) {
    i <- grep(sprintf("%s <- function(df) {", f), src, fixed = TRUE)
    expect_length(i, 1)
    expect_identical(trimws(src[i + 1L]), ".exigir_permiso()", info = f)
  }
  expect_true(any(grepl('puede(isolate(shared$usuario()), "can_manage_proveedores")', src, fixed = TRUE)))
})

test_that("ningún guardado del catálogo queda fuera de set_provs()/set_inac()", {
  llamadas <- grep("save_proveedores(_inactivos)?[(]", src)
  dentro   <- grep("save_proveedores(_inactivos)?[(]df, client_id = shared[$]effective_client_id[(][)](, base = get_(provs|inac)[(][)])?[)]", src)   # CM09: base
  expect_setequal(llamadas, dentro)
  expect_length(dentro, 2)
})

test_that("el editor viejo de Ajustes también revisa el permiso y usa el cliente de la sesión", {
  for (n in c("cat_save_row", "cat_delete_row"))
    expect_identical(primera_linea(leg, n),
                     'if (!puede_o_avisa(shared, "can_manage_proveedores", session)) return()', info = n)
  expect_false(any(grepl("load_proveedores()", leg, fixed = TRUE)))
})
