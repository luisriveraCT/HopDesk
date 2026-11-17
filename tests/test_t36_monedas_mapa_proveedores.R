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

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
