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
for (f in c("R/monedas.R", "R/zona.R", "R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
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
cargar("R/global.R", c("fmt_money"))
cargar("R/pasivos_observers.R", "isTRUE_safe")
cargar("R/pasivos_capabilities.R", c("PASIVOS_CAPABILITIES", "pasivos_autoriza"))
for (f in c(F_TMOD, "R/pasivos_list_module.R", "R/pasivos_deuda.R")) suppressMessages(source(f))
source("tests/helpers/hd_numero.R")   # T66: hd_numero_input()
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

# ── 4. Selectors and new rows start on the client's base (C1, C2) ────────────
F_WMOD  <- Sys.getenv("WMOD_R",  "R/pasivos_wizard_module.R")
F_STEPS <- Sys.getenv("STEPS_R", "R/pasivos_wizard_steps.R")
F_PMOD  <- Sys.getenv("PMOD_R",  "R/pasivos_module.R")
for (f in c("R/pasivos_engine.R", F_STEPS, F_WMOD)) suppressMessages(source(f))
cargar(F_PMOD, "pasivos_convert_modal_ui")
elegida <- function(ui, id) {
  h <- paste(as.character(ui), collapse = "")
  s <- regmatches(h, regexpr(sprintf('(?s)id="%s"[^>]*>.*?</select>', id), h, perl = TRUE))
  sub('(?s).*<option value="([^"]*)" selected>.*', "\\1", s, perl = TRUE)
}
test_that("the wizard's steps and the modals: a draft with no currency starts on the base", {
  expect_identical(elegida(pasivos_step_ui_detalles(list(categoria = "regular"), base = "USD"), "pwiz_moneda_pago"), "USD")
  expect_identical(elegida(pasivos_step_ui_detalles(list(categoria = "regular")), "pwiz_moneda_pago"), "MXN")
  expect_identical(elegida(pasivos_step_ui_detalles(list(categoria = "regular", moneda_pago = "EUR"), base = "USD"),
                           "pwiz_moneda_pago"), "EUR")
  expect_identical(elegida(pasivos_cargo_row_ui(1L, list(), base = "USD"), "pwiz_cargo_moneda_1"), "USD")
  expect_identical(elegida(pasivos_step_ui_cargos(list(categoria = "financiero"), base = "USD"), "pwiz_residual_moneda"), "USD")
  expect_identical(elegida(.ppm_modal_ui(c(NTS = "NTS"), base = "USD"), "ppm_moneda"), "USD")
  expect_identical(elegida(.ppm_modal_ui(c(NTS = "NTS"), prefill = list(moneda = "EUR"), base = "USD"), "ppm_moneda"), "EUR")
  p <- PROVS[1, ]; p$moneda_pago <- NA_character_
  expect_identical(elegida(pasivos_convert_modal_ui(p, LIABS, "NTS", base = "USD"), "pcm_moneda"), "USD")
})
nuevo_cargo <- function(moneda_base) {
  s <- list(moneda_base_cliente = reactive(moneda_base_resolver(moneda_base)), effective_client_id = function() "prueba",
            company_map = reactive(CMAP))
  out <- NULL
  suppressWarnings(suppressMessages(testServer(function(input, output, session) setup_pasivos_wizard(input, output, session, s), {
    session$flushReact(); session$setInputs(pwiz_add_cargo = 1); session$flushReact()
    out <<- elegida(paste(unlist(output$pwiz_cargos_table), collapse = ""), "pwiz_cargo_moneda_1")
  })))
  out
}
test_that("the wizard's server: a new initial fee starts on the base (MXN until one is chosen)", {
  expect_identical(nuevo_cargo("USD"), "USD")
  expect_identical(nuevo_cargo(NA_character_), "MXN")
})

# ── 5. A stored value with no currency (C3) and the engine ───────────────────
suppressPackageStartupMessages(library(lubridate))   # the engine uses Date(), as the app
F_ECM <- Sys.getenv("ECM_R", "R/pasivos_edit_confirm_module.R")
F_ENG <- Sys.getenv("ENG_R", "R/pasivos_engine.R")
for (f in c("R/policy_engine.R", F_ENG, F_ECM)) suppressMessages(source(f))
if (!exists("isTRUE_safe")) cargar("R/pasivos_observers.R", "isTRUE_safe")
credito <- list(categoria = "financiero", flavor = "credito_simple", nombre = "Crédito", empresa = "NTS",
                parte = "Banco", moneda_pago = "USD", principal_original = 120000, saldo_capital = 120000,
                tasa_tipo = "fija", tasa_actual = 12, fecha_inicio = Sys.Date(), plazo_meses = 12L,
                frecuencia_pago = "mensual", dia_pago = 15L, periodo_gracia = 0L, metodo_amortizacion = "francesa",
                cargos_iniciales = list(list(desc = "Apertura", monto = 100, fecha = Sys.Date() + 3L)))
test_that("an initial fee with no currency takes its liability's (USD), not a fixed one", {
  s <- pasivos_generate_schedule(pasivos_wizard_draft_to_liability(credito, user = "x"), today = Sys.Date())
  fee <- s[s$origin == "initial_fee", , drop = FALSE]
  expect_identical(nrow(fee), 1L)
  expect_identical(fee$moneda, "USD")
  c2 <- credito; c2$cargos_iniciales[[1]]$moneda <- "EUR"   # one with its own currency keeps it
  s2 <- pasivos_generate_schedule(pasivos_wizard_draft_to_liability(c2, user = "x"), today = Sys.Date())
  expect_identical(s2$moneda[s2$origin == "initial_fee"], "EUR")
})
test_that("the wizard: a liability with no currency opens with none and is saved in the base", {
  r <- pasivos_wizard_draft_to_liability(credito, user = "x"); r$moneda_pago <- NA_character_
  expect_true(is.na(pasivos_liability_to_draft(r)$moneda_pago))
  d <- credito; d$moneda_pago <- NULL
  expect_identical(pasivos_wizard_draft_to_liability(d, user = "x", base = "USD")$moneda_pago, "USD")
  expect_identical(pasivos_wizard_draft_to_liability(d, user = "x")$moneda_pago, "MXN")   # base not chosen: as today
  expect_identical(pasivos_wizard_draft_to_liability(credito, user = "x", base = "EUR")$moneda_pago, "USD")
})
test_that("the conflicts table: a provision with no currency shows the base", {
  conf <- PROVS[1:2, ]; conf$moneda_pago <- NULL
  expect_identical(unique(pasivos_conflictos_filas(conf, NULL, base = "USD")$Moneda), "USD")
  expect_identical(unique(pasivos_conflictos_filas(PROVS[1:2, ], NULL, base = "USD")$Moneda), unique(PROVS$moneda_pago[1:2]))
})

# ── 6. The FIX: Banxico's pair, in R/fx_banxico.R ────────────────────────────
F_FXB <- Sys.getenv("FXB_R", "R/fx_banxico.R")
suppressMessages(source(F_FXB))
test_that("fx_fix_aplica(): only an amount quoted in dollars and paid in pesos", {
  expect_true(fx_fix_aplica("USD", "MXN"))
  expect_true(fx_fix_aplica(" usd ", "mxn"))
  expect_false(fx_fix_aplica("EUR", "MXN"))
  expect_false(fx_fix_aplica("USD", "USD"))
  expect_false(fx_fix_aplica(NA, "MXN"))
  expect_false(fx_fix_aplica(NULL, NULL))
  expect_false(fx_fix_aplica("", "MXN"))
})
test_that("the wizard asks for the FIX only for that pair", {
  G$fix <- 0L
  fx_fix <- function(...) { G$fix <- G$fix + 1L; list(valor = 18.5, fecha = Sys.Date()) }
  assign("fx_fix", fx_fix, envir = globalenv())
  s <- list(moneda_base_cliente = reactive(moneda_base_resolver(NA)), effective_client_id = function() "prueba",
            company_map = reactive(CMAP))
  suppressWarnings(suppressMessages(testServer(function(input, output, session) setup_pasivos_wizard(input, output, session, s), {
    session$setInputs(pwiz_cotizado_en = "USD", pwiz_moneda_pago = "MXN"); session$flushReact()
    invisible(output$pwiz_fx_info)
    expect_identical(G$fix, 1L)
    session$setInputs(pwiz_cotizado_en = "EUR"); session$flushReact()
    invisible(output$pwiz_fx_info)
    expect_identical(G$fix, 1L)
  })))
})

# ── 7. The selectors offer the whole catalog: base, then the data's, then A-Z ─
F_VAL <- Sys.getenv("VAL_R", "R/pasivos_wizard_validate.R")
suppressMessages(source(F_VAL))
opciones <- function(ui, id) {
  h <- paste(as.character(ui), collapse = "")
  s <- regmatches(h, regexpr(sprintf('(?s)id="%s"[^>]*>.*?</select>', id), h, perl = TRUE))
  m <- regmatches(s, gregexpr('<option value="[^"]*"', s))[[1]]
  sub('<option value="([^"]*)"', "\\1", m)
}
test_that("each Pasivos selector: base, then the data's currencies, then the rest of the catalog", {
  pr <- c("USD", "EUR", "MXN")
  todos <- c(pr, setdiff(monedas_catalogo(), pr))
  expect_identical(opciones(pasivos_step_ui_detalles(list(categoria = "regular"), base = "USD", primero = pr), "pwiz_moneda_pago"), todos)
  expect_identical(opciones(pasivos_step_ui_detalles(list(categoria = "regular"), base = "USD", primero = pr), "pwiz_cotizado_en"),
                   c("", todos))
  expect_identical(opciones(pasivos_cargo_row_ui(1L, list(), base = "USD", primero = pr), "pwiz_cargo_moneda_1"), todos)
  expect_identical(opciones(pasivos_step_ui_cargos(list(categoria = "financiero"), base = "USD", primero = pr), "pwiz_residual_moneda"), todos)
  expect_identical(opciones(.ppm_modal_ui(c(NTS = "NTS"), base = "USD", primero = pr), "ppm_moneda"), todos)
  expect_identical(opciones(pasivos_convert_modal_ui(PROVS[1, ], LIABS, "NTS", base = "USD", primero = pr), "pcm_moneda"), todos)
  expect_gt(length(todos), 100L)   # the ISO catalog, not the old list of ten
})
test_that("a currency outside the old list of ten is valid; a made-up one is not", {
  expect_null(pasivos_wizard_validate_final(list(categoria = "regular", nombre = "x", empresa = "NTS", parte = "p",
                                                 moneda_pago = "ZAR"), "NTS")$errors$moneda_pago)
  expect_false(is.null(pasivos_wizard_validate_final(list(categoria = "regular", nombre = "x", empresa = "NTS", parte = "p",
                                                          moneda_pago = "XYZ"), "NTS")$errors$moneda_pago))
  expect_identical(elegida(.ppm_modal_ui(c(NTS = "NTS"), prefill = list(moneda = "XYZ"), base = "USD"), "ppm_moneda"), "USD")
})
