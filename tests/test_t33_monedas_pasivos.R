# =============================================================================
# tests/test_t33_monedas_pasivos.R
# T33 (monedas fase 3, Pasivos): no currency written by hand. The client's
# base currency (shared$moneda_base_cliente()) and the data decide; a client
# in MXN sees exactly what it saw before.
#   1. Order: rows, sections and badges -- the base first, then the data's.
#   2. Badge colors by position in that order.
# Pure, with tests/helpers/pasivos_datos_prueba.R.
# Run from the repo root: Rscript tests/test_t33_monedas_pasivos.R
# PIVOT_R / STYLES_R / TMOD_R = other copies (contraprueba).
# =============================================================================
suppressPackageStartupMessages({ library(testthat); library(dplyr); library(tibble) })
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
    eval(e, envir = globalenv())
cargar("R/global.R", "strip_accents")
cargar("R/persistence.R", "confirmaciones_vigentes")
F_PIVOT  <- Sys.getenv("PIVOT_R",  "R/pasivos_table_pivot.R")
F_STYLES <- Sys.getenv("STYLES_R", "R/pasivos_table_styles.R")
F_TMOD   <- Sys.getenv("TMOD_R",   "R/pasivos_table_module.R")
for (f in c("R/monedas.R", "R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/pasivos_schemas.R", F_PIVOT, F_STYLES)) source(f)
source("tests/helpers/pasivos_datos_prueba.R")

D <- pasivos_datos_prueba()
HOY <- D$hoy
LIABS <- D$liabilities[D$liabilities$empresa %in% D$visibles, ]
PROVS <- D$provisions[D$provisions$empresa %in% D$visibles, ]
FILTROS <- list(empresa = character(0), categorias = c("regular", "financiero", "tarjeta"), subcategorias = character(0),
                currency = "All", search = "", granularity = "day", provision_type = "todas")
largo <- function(base, provs = PROVS) suppressWarnings(pasivos_table_build_long(
  provs, D$manual_inv, confirmaciones_vigentes(D$bancos_confirmados), LIABS, FILTROS, base = base))
ancho <- function(base, provs = PROVS) pasivos_table_pivot_wide(largo(base, provs), "day", HOY - 7L, HOY + 53L, base = base)
monedas_en_orden <- function(x) unique(x)

# ── 1. Order ─────────────────────────────────────────────────────────────────
test_that("a client in MXN: rows in today's order (MXN, USD, EUR, CAD)", {
  w <- ancho("MXN")
  expect_identical(w$metadata_cols$row_id, c("orphan_p0", "lib1", "lib2", "lib2_fee_1", "lib2_fee_2", "lib3", "lib'4"))
  expect_identical(monedas_en_orden(largo("MXN")$row_moneda), c("MXN", "USD", "EUR", "CAD"))
  expect_identical(.pasivos_cur_order(largo("MXN")$row_moneda, "MXN"), c("MXN", "USD", "EUR", "CAD"))
})
test_that("a client in USD: its rows go first, in the long and the wide form", {
  expect_identical(monedas_en_orden(largo("USD")$row_moneda)[1], "USD")
  expect_identical(ancho("USD")$metadata_cols$row_id[1:3], c("lib2", "lib2_fee_1", "lib2_fee_2"))
  expect_identical(monedas_en_orden(ancho("EUR")$metadata_cols$row_moneda)[1], "EUR")
})
test_that("a provision whose currency is missing falls in the base", {
  p <- PROVS; p$moneda_pago <- NULL
  expect_true(all(largo("USD", p)$row_moneda[largo("USD", p)$cell_kind == "provision"] == "USD"))
  expect_true(all(largo("MXN", p)$row_moneda[largo("MXN", p)$cell_kind == "provision"] == "MXN"))
})
test_that(".pasivos_cur_order: base first (present or not), then by rows, ties alphabetical, data as is", {
  expect_identical(.pasivos_cur_order(c("USD", "EUR", "EUR", "MXN"), "MXN"), c("MXN", "EUR", "USD"))
  expect_identical(.pasivos_cur_order(c("USD", "EUR", "EUR"), "MXN"), c("EUR", "USD"))
  expect_identical(.pasivos_cur_order(c("USD", "EUR", "MXN"), "USD"), c("USD", "EUR", "MXN"))
  expect_identical(.pasivos_cur_order(c("usd", "MXN", NA, ""), "USD"), c("usd", "MXN", ""))
  expect_identical(.pasivos_cur_order(character(), "MXN"), character())
})
test_that("the table's sections take their order from a function of the base, not a fixed list", {
  x <- readLines(F_TMOD, warn = FALSE, encoding = "UTF-8")
  sec <- grep("secciones = list(list(campo = \"Moneda\"", x, fixed = TRUE, value = TRUE)
  expect_length(sec, 1L)
  expect_match(sec, "orden = function() .pasivos_cur_order(long_df()$row_moneda, base())", fixed = TRUE)
  s <- hd_secciones_validar(list(list(campo = "Moneda", orden = function() .pasivos_cur_order(largo("USD")$row_moneda, "USD"))))
  d <- data.frame(Moneda = ancho("USD")$metadata_cols$row_moneda, stringsAsFactors = FALSE)
  expect_identical(vapply(hd_secciones_payload(s, d, list())[[1]]$opciones, `[[`, character(1), "valor"),
                   c("USD", "MXN", "EUR", "CAD"))
})

# ── 2. Badge colors ──────────────────────────────────────────────────────────
test_that("colors by position: a client in MXN keeps MXN blue, USD green, EUR purple, the rest gray", {
  o <- c("MXN", "USD", "EUR", "CAD")
  expect_identical(vapply(o, .pasivos_cur_color, character(1), orden = o, USE.NAMES = FALSE),
                   c("#2563eb", "#16a34a", "#7c3aed", "#6b7280"))
  expect_identical(.pasivos_cur_color("USD", c("USD", "MXN")), "#2563eb")   # a client in USD: its base is blue
  expect_identical(.pasivos_cur_color("JPY", o), "#6b7280")
  expect_identical(.pasivos_cur_color(NA_character_, o), "#6b7280")
})

# ── 3. The table's server: base from shared, and only a real change rebuilds ─
suppressPackageStartupMessages(library(shiny))
cargar("R/global.R", c("fmt_money", "CURRENCIES"))
cargar("R/pasivos_observers.R", "isTRUE_safe")
cargar("R/pasivos_capabilities.R", c("PASIVOS_CAPABILITIES", "pasivos_autoriza"))
for (f in c(F_TMOD, "R/pasivos_list_module.R")) suppressMessages(source(f))
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())
has_capability <- function(user, cap, empresa = NULL) TRUE
permiso_nombre <- function(p) p
.build_long_real <- pasivos_table_build_long
G <- new.env(); G$construcciones <- 0L; G$bases <- character()
pasivos_table_build_long <- function(..., base = MONEDA_BASE_OMISION) {
  G$construcciones <- G$construcciones + 1L; G$bases <- c(G$bases, base)
  .build_long_real(..., base = base)
}
CMAP <- c(NTS = "Networks Transport", NRS = "Networks Resources", NG = "Networks Group")
config <- reactiveVal(list(moneda_base = NA_character_))
SHARED <- list(
  pasivos_provisions_db = reactiveVal(D$provisions), pasivos_liabilities_db = reactiveVal(D$liabilities),
  manual_inv = reactiveVal(D$manual_inv), bancos_confirmados = reactiveVal(D$bancos_confirmados),
  visible_initials = reactiveVal(D$visibles), empresa_sel = reactiveVal(D$visibles),
  proveedores_db = reactiveVal(NULL), current_user = reactiveVal("tester"), effective_client_id = function() "prueba",
  suppress_ledger_prov_refresh = reactiveVal(FALSE), company_map = reactive(CMAP), req_version = reactiveVal(0L),
  moneda_base_cliente = reactive(moneda_base_resolver(config()$moneda_base)))
test_that("the table's server: base not chosen = MXN as today; a same-value save rebuilds nothing; USD goes first", {
  suppressWarnings(suppressMessages(testServer(pasivos_table_module_server, args = list(shared = SHARED), {
    session$flushReact()
    expect_identical(utils::tail(G$bases, 1), "MXN")
    n <- G$construcciones
    config(list(moneda_base = NA_character_, logo = "otro")); session$flushReact()    # saved, base unchanged
    expect_identical(G$construcciones, n)
    config(list(moneda_base = "USD")); session$flushReact()
    expect_identical(utils::tail(G$bases, 1), "USD")
    expect_gt(G$construcciones, n)
  })))
})
test_that("the table's server without moneda_base_cliente (an old shared): MXN, no error", {
  s <- SHARED; s$moneda_base_cliente <- NULL; G$bases <- character()
  suppressWarnings(suppressMessages(testServer(pasivos_table_module_server, args = list(shared = s), {
    session$flushReact()
    expect_identical(utils::tail(G$bases, 1), "MXN")
  })))
})
