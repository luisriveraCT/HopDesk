# Permission registry + puede(). Run from the repo root: Rscript tests/test_permisos.R
library(testthat)
`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/i18n.R")
source("R/tier_registry.R")
source("R/permisos.R")

u <- function(tier, json = "{}", cfg = NULL) list(tier = tier, permisos = permisos_resueltos(tier, json, cfg))
TI <- nivel_ti()

test_that("Compras: requisiciones y provisiones sí; convertir, pagos, confirmar y pasivos no", {
  c <- u("compras")
  for (k in c("can_view_requisiciones", "can_create_requisiciones", "can_edit_requisiciones",
              "can_trash_own_requisiciones", "can_trash_others_requisiciones", "can_edit_provisiones"))
    expect_true(puede(c, k), info = k)
  for (k in c("can_convert_provisiones", "can_register_payments", "can_confirm_payments",
              "can_manage_pasivos", "can_publish_panorama", "can_view_tiers"))
    expect_false(puede(c, k), info = k)
  expect_identical(nivel_pestanas("compras"), c("CAL", "PSV"))
})

test_that("Análisis queda de solo lectura en provisiones, pagos y pasivos", {
  a <- u("analysis")
  for (k in c("can_edit_provisiones", "can_convert_provisiones", "can_register_payments",
              "can_confirm_payments", "can_manage_pasivos"))
    expect_false(puede(a, k), info = k)
})

test_that("Finanzas conserva lo que tenía", {
  f <- u("finance")
  for (k in c("can_edit_provisiones", "can_convert_provisiones", "can_register_payments",
              "can_confirm_payments", "can_manage_pasivos"))
    expect_true(puede(f, k), info = k)
  expect_false(puede(f, "can_view_requisiciones"))
  expect_false(puede(f, "can_publish_panorama"))
})

test_that("Dev y Admin tienen todo lo de cliente, incluido publicar", {
  for (t in c(TI, "admin")) {
    x <- u(t)
    for (k in c(permisos_de_grupo("requisiciones"), permisos_de_grupo("tesoreria"), "can_publish_panorama"))
      expect_true(puede(x, k), info = paste(t, k))
  }
})

test_that("los permisos internos nunca llegan a un nivel de cliente", {
  for (t in niveles_cliente()) {
    x <- u(t, jsonlite::toJSON(as.list(setNames(rep(TRUE, length(permisos_internos())), permisos_internos())),
                               auto_unbox = TRUE))
    for (k in permisos_internos()) expect_false(puede(x, k), info = paste(t, k))
  }
  cfg <- setNames(list(as.list(setNames(rep(TRUE, length(permisos_internos())), permisos_internos()))), TI)
  for (k in permisos_internos()) expect_false(puede(u(TI, "{}", cfg), k), info = k)
  expect_false(any(permisos_internos() %in% perm_visible_keys(FALSE)))
  expect_true(all(permisos_internos() %in% perm_visible_keys(TRUE)))
})

test_that("orden: nivel < configuración del cliente < usuario < bloqueados", {
  cfg <- list(compras = list(can_edit_provisiones = FALSE))
  expect_false(puede(u("compras", "{}", cfg), "can_edit_provisiones"))
  expect_true(puede(u("compras", '{"can_edit_provisiones": true}', cfg), "can_edit_provisiones"))
  # locked keys win over everything
  expect_true(puede(u(TI, '{"can_view_tiers": false}', list(dev = list(can_view_tiers = FALSE))),
                    "can_view_tiers"))
  expect_true(puede(u(nivel_tope(), '{"can_approve_clients": false}'), "can_approve_clients"))
})

test_that("falla cerrado", {
  expect_false(puede(NULL, "can_confirm_payments"))
  expect_false(puede(list(), "can_confirm_payments"))
  expect_false(puede(u(""), "can_confirm_payments"))
  expect_false(puede(u("no_existe"), "can_confirm_payments"))
  expect_warning(expect_false(puede(u(TI), "can_hacer_magia")), "desconocido")
  expect_false(puede(u("compras", "{esto no es json"), "can_convert_provisiones"))
})

test_that("auth_resolve_perms conserva su forma para el código viejo", {
  p <- auth_resolve_perms("finance", "{}")
  expect_type(p, "list")
  expect_setequal(names(p), permisos_claves())
})

test_that("cada permiso y grupo tiene nombre en los dos idiomas", {
  for (idioma in c("es", "en")) {
    dic <- jsonlite::fromJSON(file.path("www/i18n", paste0(idioma, ".json")))
    expect_length(setdiff(paste0("permisos.", permisos_claves()), names(dic)), 0)
    expect_length(setdiff(paste0("permisos.grupo.", PERMISOS_GRUPOS), names(dic)), 0)
  }
  expect_true(all(vapply(PERMISOS, function(p) p$grupo %in% PERMISOS_GRUPOS, logical(1))))
})

test_that("las listas de permisos del registro de niveles existen", {
  for (t in names(TIER_REGISTRY)) {
    on <- TIER_REGISTRY[[t]]$permisos
    if (!identical(on, "*")) expect_length(setdiff(on, permisos_claves()), 0)
    expect_length(setdiff(TIER_REGISTRY[[t]]$bloqueados, permisos_claves()), 0)
  }
})
