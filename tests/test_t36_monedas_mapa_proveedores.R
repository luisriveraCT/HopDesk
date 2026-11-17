# =============================================================================
# tests/test_t36_monedas_mapa_proveedores.R
# T36 · monedas fase 3: Mapa de tesorería and Proveedores without currency
# codes written by hand.
#   1. The bank's words: ppl_moneda_palabra(), one list for bank accounts and
#      the providers' Excel. ppl_moneda_cuenta == T34 on its whole table;
#      prov_moneda / prov_medio_pago == before, but for the cases listed.
#   2. Mapa de tesorería (testServer): selector = base + the data's currencies.
#   3. Proveedores (testServer): defaults on the base, full-catalog selectors.
# Run from project root:  Rscript tests/test_t36_monedas_mapa_proveedores.R
# Contrapruebas: PPL_R / PEXC_R / TMAP_R / PMOD_R = an older file must fail.
# =============================================================================
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x

.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}
seccion <- function(t) cat("\n--", t, "--\n")

source("R/monedas.R")
source(Sys.getenv("PPL_R", "R/ppl_generator.R"))
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE))
  if (is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% nombres) eval(e, envir = globalenv())
cargar("R/global.R", c("strip_accents", "CURRENCIES"))
source(Sys.getenv("PEXC_R", "R/proveedores_excel.R"))

# ── 1. The bank's words ──────────────────────────────────────────────────────
seccion("1. ppl_moneda_palabra, ppl_moneda_cuenta, prov_moneda, prov_medio_pago")

# T34's ppl_moneda_cuenta (a5c696a), verbatim.
cuenta_t34 <- function(x, base) {
  mon <- toupper(trimws(x %||% base))
  dplyr::case_when(
    grepl("peso",  mon, ignore.case = TRUE)              ~ "MXN",
    grepl("d.lar", mon, ignore.case = TRUE, perl = TRUE) ~ "USD",
    moneda_valida(mon)                                    ~ mon,
    TRUE                                                  ~ base
  )
}
# T34's table (tests/test_t34_monedas_pagar_hoy.R) and more of the same kind.
CTAS_T34 <- c("MXN", "USD", "EUR", "Pesos", "Dólares", "dolares", "pesos mexicanos", "", NA, "xyz")
CTAS <- c(CTAS_T34, "COP", "cad", " usd ", "PESOS", "DÓLARES", "Dolar", "dólar americano", "Peso MXN",
          "Moneda Nacional MXN", "Dolares USD", "mx", "EURO", "ABC", "  ")
igual <- TRUE
for (b in c("MXN", "USD", "EUR")) {
  igual <- igual && identical(ppl_moneda_cuenta(CTAS, b), cuenta_t34(CTAS, b))
  for (x in as.list(CTAS)) igual <- igual && identical(ppl_moneda_cuenta(x, b), cuenta_t34(x, b))
  igual <- igual && identical(ppl_moneda_cuenta(NULL, b), cuenta_t34(NULL, b))
}
ok("ppl_moneda_cuenta == T34 on T34's table + 14 more, bases MXN/USD/EUR, vector and one by one, NULL (identical)", igual)
ok("... the T34 table itself, base MXN", identical(ppl_moneda_cuenta(CTAS_T34, "MXN"), cuenta_t34(CTAS_T34, "MXN")))
# What the account now understands that T34 sent to the base: the Excel's words.
NUEVAS <- c("M.N.", "MN", "US", "DLL", "DLLS")
ok("bank words T34 did not know (M.N., MN, US, DLL, DLLS): before, the base",
   identical(cuenta_t34(NUEVAS, "EUR"), rep("EUR", 5)))
ok("... now MXN, MXN, USD, USD, USD whatever the base",
   identical(ppl_moneda_cuenta(NUEVAS, "EUR"), c("MXN", "MXN", "USD", "USD", "USD")))
ok("ppl_moneda_palabra: NA for codes and words it doesn't know",
   identical(ppl_moneda_palabra(c("EUR", "Euros", "", NA, "xyz")), rep(NA_character_, 5)))

# prov_moneda before T36 (a5c696a), verbatim.
prov_moneda_antes <- function(x) {
  t <- prov_normalizar_encabezado(x)
  out <- prov_limpiar_texto(x)
  out[!is.na(t) & grepl("^(mxn|mn|m n|peso|pesos|moneda nacional)$", t)] <- "MXN"
  out[!is.na(t) & grepl("^(usd|us|dll|dlls|dolar|dolares|dolar americano)$", t)] <- "USD"
  out
}
EXC <- c("Pesos", "M.N.", "mxn", "Dólar", "DLLS", "USD", NA, "EUR", "pesos", "PESO", "m n", "Moneda Nacional",
         "us", "dll", "dolares", "Dólar Americano", " MXN ", "N/A", "-", "", "Euros", "eur", "COP", "xyz")
ok("prov_moneda == before on 24 values of an Excel's Moneda column (identical)",
   identical(prov_moneda(EXC), prov_moneda_antes(EXC)))
DISTINTAS <- c("Pesos mexicanos", "Dólares americanos", "MXN pesos", "USD dólares")
ok("before, a longer phrase stayed as written (status incomplete)", identical(prov_moneda_antes(DISTINTAS), DISTINTAS))
ok("... now it is the account's rule too: MXN, USD, MXN, USD",
   identical(prov_moneda(DISTINTAS), c("MXN", "USD", "MXN", "USD")))

prov_medio_pago_antes <- function(moneda, banco)
  ifelse(!is.na(moneda) & moneda == "USD", "SPD", ifelse(!is.na(banco) & banco == "030", "BCO", "SPI"))
MON <- c("MXN", "USD", NA, "EUR"); BAN <- c("030", "012", NA, "")
g <- expand.grid(m = MON, b = BAN, stringsAsFactors = FALSE)
antes <- prov_medio_pago_antes(g$m, g$b); ahora <- prov_medio_pago(g$m, g$b)
cambia <- antes != ahora
ok("prov_medio_pago == before on {MXN,USD,NA,EUR} x {030,012,NA,\"\"} but where noted",
   identical(antes[!cambia], ahora[!cambia]))
ok("the only change: NA or EUR to BanBajío (030) was BCO, now SPI (as the modal and the PPL)",
   identical(paste(g$m[cambia], g$b[cambia]), c("NA 030", "EUR 030")) && all(ahora[cambia] == "SPI") &&
     identical(ahora[cambia], ppl_medio_pago(g$m[cambia], g$b[cambia])))
ok("prov_medio_pago on one currency for many rows", identical(prov_medio_pago("MXN", c("030", "012")), c("BCO", "SPI")))

# ── 2. Mapa de tesorería ─────────────────────────────────────────────────────
seccion("2. Mapa de tesorería: selector, balances, base")
cargar("R/erp_connector_registry.R", "SAP_B1_ERP_TYPE")
cargar("R/vencidos_module.R", "facturas_ids")
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R")) source(f)
.i18n_dir <- file.path(getwd(), "www", "i18n")
puede <- function(usuario, permiso) TRUE
source(Sys.getenv("TMAP_R", "R/treasury_map_module.R"))

# updateSelectInput() as the module calls it: keep what it sent.
.upd <- new.env(); .upd$x <- list()
updateSelectInput <- function(session, inputId, label = NULL, choices = NULL, selected = NULL)
  .upd$x[[inputId]] <- list(choices = unname(as.character(choices)), selected = selected)

ar_map <- data.frame(.ini = "BBB", Parte = "Empresa A", Documento = paste0("D", 1:4),
                     `Fecha de vencimiento` = as.Date("2026-10-05"), `Saldo vencido` = c(100, 200, 300, 50),
                     Moneda = c("MXN", "MXN", "MXN", "USD"), check.names = FALSE, stringsAsFactors = FALSE)
CTAS_MAP <- data.frame(id = c("c1", "c2", "c3", "c4"), Empresa = c("AAA", "AAA", "BBB", "BBB"),
                       Moneda = c("Pesos", "EUR", "Dólares", "CAD"), activa = c(TRUE, TRUE, TRUE, FALSE),
                       saldo_inicial = c(1000, 70, 40, 9), stringsAsFactors = FALSE)
mapa_shared <- function(base_cfg) {
  n <- new.env(); n$ctas <- 0L
  cfg <- reactiveVal(base_cfg)
  list(company_map = function() c(AAA = "Empresa A", BBB = "Empresa B"), current_user = function() "tester",
       ctas_cuentas = function() { n$ctas <- n$ctas + 1L; CTAS_MAP },
       bancos_movimientos = function() NULL,
       moneda_base_cliente = reactive(moneda_base_resolver(cfg())), cfg = cfg, n = n)
}
ar_rv <- reactiveVal(ar_map)

sh <- mapa_shared(NA)
shiny::testServer(treasuryMapServer, args = list(shared = sh, ic_invoices_rv = reactive(list(ar = ar_rv()))), {
  session$flushReact()
  ok("base not chosen (MXN): the selector offers MXN, USD (invoices) and EUR (an active account); not CAD (inactive)",
     identical(.upd$x$map_cur$choices, c("MXN", "USD", "EUR")) && identical(.upd$x$map_cur$selected, "MXN"))
  ok("before the selector reports a value the map shows the base", identical(moneda_sel(), "MXN"))
  b <- bank_balances()
  ok("the EUR account has its own balance (it used to be added to the pesos)",
     identical(b$AAA$MXN, 1000) && identical(b$AAA$EUR, 70) && identical(b$BBB$USD, 40))
  ok("a currency with no account has no balance (the map says 'sin datos')", is.null(b$AAA$USD) && is.null(b$BBB$CAD))
  nodos <- map_data()$nodes
  ok("the nodes carry the MXN balance label", nrow(nodos) == 2L)

  # The user picks USD; new data brings a GBP invoice: USD stays selected.
  session$setInputs(map_cur = "USD")
  ar_rv(rbind(ar_map, transform(ar_map[4, ], Documento = "D5", Moneda = "GBP")))
  session$flushReact()
  ok("new currency in the data: the selector adds it and keeps USD",
     identical(.upd$x$map_cur$choices, c("MXN", "USD", "EUR", "GBP")) && identical(.upd$x$map_cur$selected, "USD"))
  ok("the edges are the USD ones", identical(edges_summary()$Total, 50))
  ar_rv(ar_map); session$flushReact()

  # Saving Ajustes with the same base recomputes nothing.
  antes <- sh$n$ctas
  sh$cfg("MXN"); session$flushReact()
  ok("saving the same base (NA -> MXN) does not recompute balances or the selector", identical(sh$n$ctas, antes))
  # A new base reorders the selector.
  sh$cfg("USD"); session$flushReact()
  ok("base USD: USD goes first", identical(.upd$x$map_cur$choices[1], "USD"))
})

.upd$x <- list()
sh <- mapa_shared("USD")
shiny::testServer(treasuryMapServer, args = list(shared = sh, ic_invoices_rv = reactive(list(ar = ar_map))), {
  session$flushReact()
  ok("client with base USD: the map opens on USD", identical(moneda_sel(), "USD") &&
       identical(.upd$x$map_cur$choices, c("USD", "MXN", "EUR")) && identical(.upd$x$map_cur$selected, "USD"))
  ok("... and its edges are the USD invoices", identical(edges_summary()$Total, 50))
})
sh_sin <- list(company_map = function() c(AAA = "Empresa A", BBB = "Empresa B"), current_user = function() "tester")
.upd$x <- list()
shiny::testServer(treasuryMapServer, args = list(shared = sh_sin, ic_invoices_rv = reactive(NULL)), {
  session$flushReact()
  ok("no base, no accounts, no invoices: only the default base", identical(.upd$x$map_cur$choices, "MXN"))
})
ok(".tm_fmt has no default currency", !"cur" %in% names(Filter(Negate(is.symbol), formals(.tm_fmt))))

# ── 3. Proveedores ───────────────────────────────────────────────────────────
seccion("3. Proveedores: the base by default, the whole catalog in the selectors")
source("tests/helpers/proveedores_testserver.R")            # the real module, S3 replaced in memory
if (nzchar(Sys.getenv("PMOD_R"))) sys.source(Sys.getenv("PMOD_R"), envir = globalenv())
.modal <- new.env(); .modal$html <- NULL
showModal <- function(ui, session = NULL) .modal$html <- paste(as.character(ui), collapse = "")
showNotification <- function(...) invisible(NULL)
# The <select id="...-prov_moneda">: its options and the one selected.
select_de <- function(html, id) {
  s <- regmatches(html, regexpr(paste0('<select id="[^"]*', id, '"[\\s\\S]*?</select>'), html, perl = TRUE))
  list(n = lengths(regmatches(s, gregexpr("<option ", s))),
       sel = sub('.*<option value="([^"]*)" selected>.*', "\\1", s),
       primeros = head(regmatches(s, gregexpr('(?<=<option value=")[^"]*', s, perl = TRUE))[[1]], 3))
}
N_CAT <- length(monedas_catalogo())
prov_shared <- function(base_cfg, activos) list(
  proveedores_db = reactiveVal(activos), proveedores_inactivos_db = reactiveVal(activos[0, ]),
  company_map = reactive(c(NTS = "Networks Trucking")), effective_client_id = function() "prueba",
  home_client_id = function() "prueba", current_user = function() "prueba", usuario = function() "prueba",
  parte_alias_map_db = reactiveVal(NULL), moneda_base_cliente = reactive(moneda_base_resolver(base_cfg)))
ACT <- rbind(prov_fila("p-1", "UNO SA", "012180001010101010"), prov_fila("p-2", "DOS SA", "012180001010101011"),
             prov_fila("p-3", "TRES SA", "012180001010101012", moneda = "EUR"),
             prov_fila("p-4", "CUATRO SA", "012180001010101013", moneda = "Dólares"),
             prov_fila("p-5", "CINCO SA", "012180001010101014", moneda = "COP"))
xlsx <- "tests/fixtures/proveedores_excel/01_reemplazo.xlsx"

.upd$x <- list()
shiny::testServer(proveedoresServer, args = list(shared = prov_shared(NA, ACT)), {
  session$flushReact()
  e <- .upd$x$excel_moneda
  ok("base not chosen: the Excel's currency selector offers the whole catalog, MXN selected",
     length(e$choices) == N_CAT && identical(e$selected, "MXN"))
  ok("... MXN first, then the providers' currencies ('Dólares' counts as USD)",
     identical(e$choices[1:4], c("MXN", "COP", "EUR", "USD")))
  .show_edit_modal(NULL)
  s <- select_de(.modal$html, "prov_moneda")
  ok("new provider: the modal opens on MXN with the whole catalog (it had 10)", s$n == N_CAT && identical(s$sel, "MXN"))
  .editar("p-4"); s <- select_de(.modal$html, "prov_moneda")
  ok("a provider saved as 'Dólares' opens on USD (as before)", identical(s$sel, "USD"))
  .editar("p-5"); s <- select_de(.modal$html, "prov_moneda")
  ok("a provider in COP opens on COP (it used to become MXN)", identical(s$sel, "COP"))
  .editar("p-3"); s <- select_de(.modal$html, "prov_moneda")
  ok("a provider in EUR opens on EUR", identical(s$sel, "EUR"))
})

shiny::testServer(proveedoresServer, args = list(shared = prov_shared("USD", ACT)), {
  session$flushReact()
  ok("base USD: the Excel's selector starts on USD", identical(.upd$x$excel_moneda$selected, "USD") &&
       identical(.upd$x$excel_moneda$choices[1], "USD"))
  .show_edit_modal(NULL); s <- select_de(.modal$html, "prov_moneda")
  ok("base USD: a new provider's modal opens on USD", identical(s$sel, "USD") && identical(s$primeros[1], "USD"))
  # Saved without touching the currency (input not reported): the base, and
  # BanBajío's rule for USD.
  rm(list = ls(.prov_guardado), envir = .prov_guardado)
  session$setInputs(prov_nombre = "NUEVO SA", prov_no_cuenta = "030180001010101019", prov_banco = "BANBAJIO|030",
                    prov_modal_save = 1)
  n <- .prov_guardado$activos; n <- n[n$nombre == "NUEVO SA", ]
  ok("base USD, saved without a currency: USD and SPD (it was MXN and BCO)",
     identical(c(n$moneda, n$medio_pago), c("USD", "SPD")))
  # The Excel without a Moneda column, the selector untouched: the base.
  rm(list = ls(.prov_guardado), envir = .prov_guardado)
  session$setInputs(excel_tipo = "activos", excel_empresa = "TODOS")
  session$setInputs(excel_file = data.frame(name = basename(xlsx), size = file.size(xlsx), type = "",
                                            datapath = normalizePath(xlsx), stringsAsFactors = FALSE))
  session$setInputs(confirmar_upload = 1)
  m <- .prov_guardado$activos$moneda
  m <- m[!.prov_guardado$activos$id %in% ACT$id]
  ok("base USD: an Excel without a Moneda column gives its new providers USD", length(m) > 0 && all(m == "USD"))
})

rm(list = ls(.prov_guardado), envir = .prov_guardado)
shiny::testServer(proveedoresServer, args = list(shared = prov_shared(NA, ACT)), {
  session$flushReact()
  session$setInputs(prov_nombre = "OTRO SA", prov_no_cuenta = "030180001010101019", prov_banco = "BANBAJIO|030",
                    prov_moneda = "MXN", prov_modal_save = 1)
  n <- .prov_guardado$activos; n <- n[n$nombre == "OTRO SA", ]
  ok("MXN to BanBajío from the modal: BCO (as before)", identical(c(n$moneda, n$medio_pago), c("MXN", "BCO")))
})

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
