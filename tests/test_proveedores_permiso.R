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

# The row buttons and the bar of the catalog and Inactivos are the table
# engine's (T07): they send an id to a function of the module instead of an
# input to an observer. Same rule for both: the first line checks.
primera_linea_fun <- function(lines, fun) {
  i <- grep(sprintf("%s <- function(", fun), lines, fixed = TRUE)
  expect_length(i, 1)
  trimws(lines[i + 1L])
}

test_that("cada observador que edita el catálogo empieza revisando el permiso", {
  escritores <- c("cat_new", "prov_confirm_inactivar", "bulk_inactivar_confirm",
                  "prov_modal_save", "excel_file", "confirmar_upload", "prov_confirm_reactivar")
  for (n in escritores)
    expect_identical(primera_linea(src, n), "if (!.puede_editar()) return()", info = n)
})

test_that("cada acción de las tablas que edita el catálogo empieza revisando el permiso", {
  acciones <- c(".editar", ".pedir_inactivar", ".eliminar", ".pedir_inactivar_bloque",
                ".pedir_reactivar", ".eliminar_inactivo")
  for (f in acciones)
    expect_identical(primera_linea_fun(src, f), "if (!.puede_editar()) return()", info = f)
  # ... and every action the two tables send reaches one of them.
  expect_true(all(vapply(c("detalle       = .editar(id)", "inactivar     = .pedir_inactivar(id)",
                    "eliminar      = .eliminar(id)", "inactivar_sel = .pedir_inactivar_bloque(a$ids)",
                    "reactivar = .pedir_reactivar(id)", "eliminar  = .eliminar_inactivo(id)"),
                  function(x) any(grepl(x, src, fixed = TRUE)), logical(1))))
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
