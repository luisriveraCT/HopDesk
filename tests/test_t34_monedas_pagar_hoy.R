# =============================================================================
# tests/test_t34_monedas_pagar_hoy.R
# Monedas fase 3, Pagar hoy (T34): no currency code written by hand in
# R/pagar_hoy_module.R; BanBajío's rules live in R/ppl_generator.R.
#   1. ppl_medio_pago is identical() to the three expressions of 6368b5d, on
#      the whole table moneda x banco_dest (integrator, condition 2).
#   2. ppl_moneda_soportada / ppl_requiere_clasificacion / ppl_moneda_cuenta
#      against today's expressions (COP is now kept, not added to MXN).
#   3. The module's own code (taken from its AST, not copied): moneda_base,
#      moneda_sel, monedas_cuentas, the selector's list, the badge map and the
#      opening-balance loop -- NG (MXN/USD) identical to today.
#   4. Static: 0 codes in the module, moneda_base never global, the two texts
#      in the dictionary (es, en).
#
# Run from project root:  Rscript tests/test_t34_monedas_pagar_hoy.R
# =============================================================================
cat("=== test_t34_monedas_pagar_hoy ===\n\n")
suppressPackageStartupMessages(library(dplyr))
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
source("R/monedas.R")
source("R/ppl_generator.R")
MOD <- "R/pagar_hoy_module.R"
BASE_COMMIT <- "6368b5d"
antes <- tryCatch(system2("git", c("show", paste0(BASE_COMMIT, ":", MOD)), stdout = TRUE, stderr = FALSE),
                  error = function(e) character())

# ── 1. medio_pago: the three expressions of today, verbatim ─────────────────
# (1298: PPL preview/download; 2976: the ZIP; 3058: "incluir sin cuenta")
hoy_1298 <- function(cur_sel, banco_dest) {
  is_bajio <- grepl("^030$", trimws(banco_dest))
  dplyr::case_when(
    toupper(cur_sel) == "USD"            ~ "SPD",
    toupper(cur_sel) == "MXN" & is_bajio ~ "BCO",
    TRUE                                 ~ "SPI"
  )
}
hoy_2976 <- function(cur, banco_dest) {
  is_bajio <- grepl("^030$", trimws(banco_dest))
  dplyr::case_when(
    toupper(cur) == "USD"            ~ "SPD",
    toupper(cur) == "MXN" & is_bajio ~ "BCO",
    TRUE                             ~ "SPI"
  )
}
hoy_3058 <- function(cur) if (toupper(cur) == "USD") "SPD" else "SPI"
ok("the three expressions copied here are the ones in 6368b5d",
   length(antes) > 0L &&
     sum(grepl('toupper(cur_sel) == "MXN" & is_bajio ~ "BCO"', antes, fixed = TRUE)) == 1L &&
     sum(grepl('toupper(cur) == "MXN" & is_bajio ~ "BCO"', antes, fixed = TRUE)) == 1L &&
     sum(grepl('medio_pago = if (toupper(cur) == "USD") "SPD" else "SPI"', antes, fixed = TRUE)) == 1L)

MONEDAS <- list("MXN", "USD", "EUR", "usd", NA_character_, "")
BANCOS  <- c("030", "012", "", NA, " 030 ")
igual_1 <- igual_2 <- igual_3 <- TRUE; na_3 <- NULL
for (m in MONEDAS) {
  igual_1 <- igual_1 && identical(hoy_1298(m, BANCOS), ppl_medio_pago(m, BANCOS))
  igual_2 <- igual_2 && identical(hoy_2976(m, BANCOS), ppl_medio_pago(m, BANCOS))
  for (b in BANCOS) {   # one row at a time too (a single payment)
    igual_1 <- igual_1 && identical(hoy_1298(m, b), ppl_medio_pago(m, b))
    igual_2 <- igual_2 && identical(hoy_2976(m, b), ppl_medio_pago(m, b))
  }
  v3 <- tryCatch(hoy_3058(m), error = function(e) "ERROR")
  if (identical(v3, "ERROR")) na_3 <- c(na_3, ppl_medio_pago(m, ""))
  else igual_3 <- igual_3 && identical(v3, ppl_medio_pago(m, ""))
}
ok("ppl_medio_pago == 1298 on {MXN,USD,EUR,usd,NA,\"\"} x {030,012,\"\",NA,\" 030 \"} (identical)", igual_1)
ok("ppl_medio_pago == 2976 on the same table (identical)", igual_2)
ok("ppl_medio_pago(cur, \"\") == 3058 for every currency it answers (identical)", igual_3)
ok("3058 with NA stopped with an error; now it is SPI, like 1298 and 2976", identical(na_3, "SPI"))
ok("on 0 rows: character(0), as today", identical(ppl_medio_pago("MXN", character()), hoy_1298("MXN", character())))

# ── 2. The other rules ───────────────────────────────────────────────────────
VALS <- c("MXN", "USD", "EUR", "mxn", "usd", NA, "")
ok("ppl_moneda_soportada == !(1095's condition)",
   identical(vapply(VALS, ppl_moneda_soportada, logical(1)),
             vapply(VALS, function(x) toupper(x) %in% c("MXN", "USD"), logical(1))))
ok("ppl_requiere_clasificacion == 1180's condition",
   identical(vapply(VALS, ppl_requiere_clasificacion, logical(1)),
             vapply(VALS, function(x) toupper(x) != "MXN", logical(1))))
LISTA_CORTA_6368B5D <- c("MXN","USD","EUR","GBP","CAD","JPY","CHF","AUD","CNY","BRL")   # global.R's old short list, 6368b5d
cuenta_hoy <- function(x) {
  mon <- toupper(trimws(x %||% "MXN"))
  dplyr::case_when(
    grepl("peso",  mon, ignore.case = TRUE)              ~ "MXN",
    grepl("d.lar", mon, ignore.case = TRUE, perl = TRUE) ~ "USD",
    mon %in% LISTA_CORTA_6368B5D                          ~ mon,
    TRUE                                                  ~ "MXN"
  )
}
CTAS <- c("MXN", "USD", "EUR", "Pesos", "Dólares", "dolares", "pesos mexicanos", "", NA, "xyz")
ok("ppl_moneda_cuenta with base MXN == today for MXN/USD/EUR/Pesos/Dólares/''/NA/xyz",
   identical(ppl_moneda_cuenta(CTAS, "MXN"), cuenta_hoy(CTAS)))
ok("ppl_moneda_cuenta: one account at a time too, NULL included",
   all(vapply(as.list(CTAS), function(x) identical(ppl_moneda_cuenta(x, "MXN"), cuenta_hoy(x)), logical(1))) &&
     identical(ppl_moneda_cuenta(NULL, "MXN"), cuenta_hoy(NULL)))
ok("COP: today it became MXN; now it stays COP", identical(cuenta_hoy("COP"), "MXN") && identical(ppl_moneda_cuenta("COP", "MXN"), "COP"))
ok("base EUR: what is not a currency goes to the base (EUR), not to MXN",
   identical(ppl_moneda_cuenta(c("", NA, "xyz", "Pesos"), "EUR"), c("EUR", "EUR", "EUR", "MXN")))

# ── 3. The module's own code, from its AST ───────────────────────────────────
mod <- parse(MOD, keep.source = FALSE)
buscar <- function(e, pred, acc = list()) {
  if (pred(e)) acc[[length(acc) + 1L]] <- e
  if (is.call(e) || is.expression(e) || is.pairlist(e))
    for (s in as.list(e)) if (!missing(s) && !is.null(s)) acc <- buscar(s, pred, acc)
  acc
}
asig <- function(nombre) function(e) is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(nombre))
uno  <- function(nombre) { r <- buscar(mod, asig(nombre)); if (length(r) == 1L) r[[1]] else stop(nombre, ": ", length(r), " definitions") }

# A fake module environment: shared, input, reactive() as a plain function.
entorno <- function(base = NULL, ctas = NULL, cmap = c(NTS = "Networks Trucking", NCS = "Networks Cross"), input = list()) {
  env <- new.env(parent = globalenv())
  env$reactive <- function(x) { f <- function() NULL; body(f) <- substitute(x); environment(f) <- parent.frame(); f }
  env$input <- input
  env$shared <- list(company_map = function() cmap)
  if (!is.null(base)) env$shared$moneda_base_cliente <- function() list(moneda = base, origen = "ajuste")
  if (!is.null(ctas)) env$shared$ctas_cuentas <- function() ctas
  for (n in c("moneda_base", "moneda_sel", "monedas_cuentas")) eval(uno(n), env)
  env
}
e0 <- entorno()
ok("moneda_base: a shared without moneda_base_cliente (test apps) -> MXN_BASE_OMISION", identical(e0$moneda_base(), MONEDA_BASE_OMISION))
ok("moneda_base: the client's setting when there is one", identical(entorno(base = "EUR")$moneda_base(), "EUR"))
eerr <- entorno(); eerr$shared$moneda_base_cliente <- function() stop("boom")
ok("moneda_base: an error reading the setting -> the default, the module keeps going", identical(eerr$moneda_base(), MONEDA_BASE_OMISION))
ok("moneda_sel: the selector's value when drawn, the base before",
   identical(entorno(input = list(bal_cur_NTS = "USD"))$moneda_sel("NTS"), "USD") &&
     identical(entorno()$moneda_sel("NTS"), "MXN") && identical(entorno(base = "EUR")$moneda_sel("NTS"), "EUR"))

CTAS_NG <- data.frame(Empresa = c("NTS", "NTS", "NCS", "NCS"), Moneda = c("MXN", "USD", "MXN", "USD"),
                      activa = c(TRUE, TRUE, TRUE, FALSE), stringsAsFactors = FALSE)
eng <- entorno(ctas = CTAS_NG)
ok("monedas_cuentas: the company's active accounts, by its initials", identical(eng$monedas_cuentas("Networks Trucking"), c("MXN", "USD")))
ok("monedas_cuentas: an inactive account does not count", identical(eng$monedas_cuentas("Networks Cross"), "MXN"))
ok("monedas_cuentas: no ctas_cuentas in shared -> nothing (never an S3 read)", identical(e0$monedas_cuentas("Networks Trucking"), character()))

# The selector's list: the module's own `curs <- ...`, evaluated.
curs_expr <- uno("curs")
lista <- function(env, docs, emp = "Networks Trucking") {
  env$s <- data.frame(Empresa = rep(emp, length(docs)), Moneda = docs, stringsAsFactors = FALSE)
  env$emp <- emp; env$staged <- function() env$s
  eval(curs_expr, env); env$curs
}
hoy_lista <- function(docs) { curs <- sort(unique(docs)); if (!length(curs)) curs <- c("MXN", "USD"); curs }
ok("C, NG company with no documents (accounts MXN+USD): MXN, USD -- identical to today",
   identical(lista(eng, character()), hoy_lista(character())))
ok("C, NG company with MXN and USD documents: MXN, USD -- identical to today",
   identical(lista(eng, c("USD", "MXN", "USD", "USD")), hoy_lista(c("USD", "MXN", "USD", "USD"))))
ok("C, it opens on the base: EUR+MXN documents -> MXN first (today EUR)", identical(lista(eng, c("EUR", "MXN"))[1], "MXN"))
ok("C, a company with nothing (no documents, no accounts): only the base",
   identical(lista(entorno(base = "EUR"), character(), emp = "Otra"), "EUR"))
ok("C, a USD account without documents still offers USD", "USD" %in% lista(eng, "MXN"))

# The badge map (D): the module's own mapa expression with the base given.
mapa_expr <- buscar(mod, function(e) is.call(e) && identical(e[[1]], as.name("col_insignia")) && identical(e[[2]], "Moneda"))
ok("D: one col_insignia(\"Moneda\") in the module", length(mapa_expr) == 1L)
mapa_de <- function(base) {
  env <- new.env(parent = globalenv()); env$.descargar_base_mon <- function() base
  env$col_insignia <- function(campo, titulo, mapa) mapa
  eval(mapa_expr[[1]], env)
}
m_mxn <- mapa_de("MXN")
ok("D, base MXN: MXN info and USD exito -- identical to today",
   identical(m_mxn[c("MXN", "USD")], list(MXN = list(texto = "MXN", tono = "info"), USD = list(texto = "USD", tono = "exito"))))
ok("D: every catalog currency has its badge; the base is the only info",
   identical(names(m_mxn), monedas_catalogo()) && sum(vapply(m_mxn, function(x) x$tono == "info", logical(1))) == 1L)
ok("D, base EUR: EUR info, MXN exito", identical(mapa_de("EUR")$EUR$tono, "info") && identical(mapa_de("EUR")$MXN$tono, "exito"))
ok("D: the base is held in a reactiveVal (no redraw when the config is saved with the same base)",
   length(buscar(mod, function(e) is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(".descargar_base_mon")) &&
                   is.call(e[[3]]) && identical(e[[3]][[1]], as.name("reactiveVal")))) == 1L)

# The opening-balance loop (3271): the module's own monedas_saldos + for.
ms_expr <- uno("monedas_saldos")
bucle <- buscar(mod, function(e) is.call(e) && identical(e[[1]], as.name("for")) && identical(e[[2]], as.name("mon")) &&
                  identical(e[[3]], as.name("monedas_saldos")))
ok("3271: one loop over monedas_saldos", length(bucle) == 1L)
escribir_hoy <- function(sa, spc, emps) {
  for (emp in emps) for (mon in LISTA_CORTA_6368B5D) { key <- paste0(emp, "_", mon)
    if (!is.null(spc[[key]])) sa[[emp]][[mon]] <- spc[[key]] }
  sa
}
escribir_ahora <- function(sa, spc, emps) {
  env <- new.env(parent = globalenv()); env$sa <- sa; env$saldos_por_cuenta <- spc; env$n_updated <- 0L
  eval(ms_expr, env)
  for (emp in emps) { env$emp <- emp; eval(bucle[[1]], env) }
  env$sa
}
EMPS <- c("Networks Cross", "Networks Trucking")
SA0 <- list(`Networks Cross` = list(MXN = 1), `Networks Trucking` = list(USD = 5))
SPC <- list(`Networks Trucking_MXN` = 100, `Networks Trucking_USD` = 7, `Networks Cross_MXN` = 3)
ok("3271, NG (MXN/USD): sa identical() to today", identical(escribir_ahora(SA0, SPC, EMPS), escribir_hoy(SA0, SPC, EMPS)))
ok("3271, NG with no saldos: sa identical() to today", identical(escribir_ahora(SA0, list(), EMPS), escribir_hoy(SA0, list(), EMPS)))
# COP: today the account's "COP" became MXN (added to MXN); now it is its own.
spc_hoy   <- list(`Networks Trucking_MXN` = 100 + 40)
spc_ahora <- list(`Networks Trucking_MXN` = 100, `Networks Trucking_COP` = 40)
sa_cop <- escribir_ahora(SA0, spc_ahora, EMPS)
ok("3271, COP account: COP 40 in its own currency, MXN stays 100 (today MXN 140)",
   identical(sa_cop$`Networks Trucking`$COP, 40) && identical(sa_cop$`Networks Trucking`$MXN, 100) &&
     identical(escribir_hoy(SA0, spc_hoy, EMPS)$`Networks Trucking`$MXN, 140))

# ── 4. Static ────────────────────────────────────────────────────────────────
src <- sub("#.*$", "", readLines(MOD, warn = FALSE, encoding = "UTF-8"))
lit <- unlist(regmatches(src, gregexpr("[\"'][A-Z]{3}[\"']", src)))
ok("no currency code written by hand in pagar_hoy_module.R", sum(substr(lit, 2, 4) %in% monedas_catalogo()) == 0L)
# (The old short list's name is built, so a search for it across tests/ is 0, T39.)
ok("no old short list left in pagar_hoy_module.R", !any(grepl(paste0("\\b", "CURREN", "CIES\\b"), src)))
ok("medio_pago: only ppl_medio_pago, three times, no copy of the rule left",
   sum(grepl("ppl_medio_pago(", src, fixed = TRUE)) == 3L && !any(grepl("\"SPD\"|\"BCO\"", src)))
glob <- unlist(lapply(c("app.R", list.files("R", "\\.R$", full.names = TRUE, recursive = TRUE)), function(f)
  vapply(parse(f, keep.source = FALSE), function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
           as.character(e[[2]])[1] %in% c("moneda_base", "moneda_sel", "monedas_cuentas"), logical(1))))
ok("moneda_base / moneda_sel / monedas_cuentas are never global (integrator, condition 4)", !any(glob))
es <- jsonlite::fromJSON("www/i18n/es.json"); en <- jsonlite::fromJSON("www/i18n/en.json")
K <- c("requisiciones.ppl.agregar_al_catalogo", "requisiciones.ppl.banbajio_clasificacion")
ok("the two PPL texts are dictionary keys, es and en", all(K %in% names(es)) && all(K %in% names(en)) &&
     all(nzchar(unlist(es[K]))) && all(nzchar(unlist(en[K]))))
ok("the module asks the dictionary for them, and the old texts are gone",
   all(vapply(K, function(k) any(grepl(k, src, fixed = TRUE)), logical(1))) &&
     !any(grepl("Agrega estos proveedores", src, fixed = TRUE)) && !any(grepl("requiere introducir", src, fixed = TRUE)))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
