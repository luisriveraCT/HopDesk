# =============================================================================
# tests/test_monedas_cierre_sap.R
# Monedas fase 3, cierre (integrador, requisiciones/monedas_fase3_cierre_paso0.md):
#   - moneda_normalizar() / MONEDAS_ALIAS_LEGADO in R/monedas.R replace the
#     MXP/MXV maps written in R/sap_api.R and R/data_pipeline.R;
#   - .parse_invoices() compares against the SAP database's local currency
#     (moneda_local, from the ERP connection; MONEDA_BASE_OMISION by default),
#     not against c("MXN","MXP") written by hand;
#   - ensure_moneda(df, base) puts the base on a file with no currency column.
# With the default (MXN) everything is identical() to 23ffc1f, except MXV
# (UDIS), which now reads the local amount in pesos (it took the UDIS amount
# and called it MXN).
#
# Run from project root:  Rscript tests/test_monedas_cierre_sap.R
# =============================================================================
cat("=== test_monedas_cierre_sap ===\n\n")
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a

.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

BASE_COMMIT <- "23ffc1f"
# The code of 23ffc1f, in its own environment (needs git).
viejo <- new.env()
for (f in c("R/data_pipeline.R", "R/sap_api.R")) {
  src <- system2("git", c("show", paste0(BASE_COMMIT, ":", f)), stdout = TRUE)
  eval(parse(text = src), envir = viejo)
}

source("R/monedas.R")
source("R/data_pipeline.R")
source("R/sap_api.R")

# ── 1. moneda_normalizar ─────────────────────────────────────────────────────
ok("moneda_normalizar: legacy codes, case, spaces, NA and others kept",
   identical(moneda_normalizar(c("MXN", "mxp", " MXV ", "usd", "", NA, "XYZ")),
             c("MXN", "MXN", "MXN", "USD", "", NA, "XYZ")))
ok("moneda_normalizar: 0 values -> character(0)", identical(moneda_normalizar(character()), character()))
ok("moneda_normalizar: no names on the result", is.null(names(moneda_normalizar(c("MXP", "USD")))))
ok("every alias maps to a catalog code", all(moneda_valida(MONEDAS_ALIAS_LEGADO)))
ok("no alias is itself a catalog code", !any(moneda_valida(names(MONEDAS_ALIAS_LEGADO))))
ok("the old maps are gone from sap_api.R and data_pipeline.R",
   !exists(".SAP_CURRENCY_MAP") && !exists(".normalize_currency") &&
     !any(grepl("\"MXP\"|\"MXV\"", c(readLines("R/sap_api.R", warn = FALSE), readLines("R/data_pipeline.R", warn = FALSE)))))

# ── 2. ensure_moneda ─────────────────────────────────────────────────────────
casos <- list(
  con_moneda  = data.frame(Moneda = c("MXN", "mxp", " MXV", "USD", "", NA, "eur"), x = 1:7),
  con_divisa  = data.frame(Divisa = c("MXP", "USD", NA, "MXN"), x = 1:4),
  sin_columna = data.frame(Documento = c("1", "2"), x = 1:2),
  vacio       = data.frame(Moneda = character(), x = integer())
)
for (n in names(casos))
  ok(sprintf("ensure_moneda (%s): identical() to %s with base MXN", n, BASE_COMMIT),
     identical(ensure_moneda(casos[[n]]), viejo$ensure_moneda(casos[[n]])))
ok("ensure_moneda: a file with no currency column takes the base given",
   identical(unique(ensure_moneda(casos$sin_columna, base = "EUR")$Moneda), "EUR"))

# ── 3. .parse_invoices ───────────────────────────────────────────────────────
raw <- tibble::tibble(
  DocNum      = 1:8,
  NumAtCard   = paste0("F", 1:8),
  CardCode    = paste0("C", 1:8),
  CardName    = paste0("Parte ", 1:8),
  FederalTaxID = "XAXX010101000",
  DocDate     = "2026-11-01",
  DocDueDate  = "2026-11-30",
  DocCurrency = c("MXN", "MXP", "USD", "EUR", "usd", "", NA, "MXN"),
  DocTotal    = c(1000, 2000, 18552.5, 20508.5, 9276.25, 500, 700, 300),
  DocTotalFc  = c(0, 0, 1000, 1000, 500, 0, 0, 0),
  PaidToDate  = c(100, 0, 1855.25, 0, 0, 0, 0, 300)
)
for (led in c("AR", "AP"))
  ok(sprintf(".parse_invoices %s: identical() to %s with the default local currency", led, BASE_COMMIT),
     identical(.parse_invoices(raw, led), viejo$.parse_invoices(raw, led)))
ok(".parse_invoices: 0 rows as before",
   identical(.parse_invoices(raw[0, ], "AR"), viejo$.parse_invoices(raw[0, ], "AR")))

# MXV (UDIS): local amount in pesos now, not the UDIS amount labelled MXN.
udis <- raw[1, ]; udis$DocCurrency <- "MXV"; udis$DocTotal <- 8500; udis$DocTotalFc <- 1000; udis$PaidToDate <- 0
r_new <- .parse_invoices(udis, "AR"); r_old <- viejo$.parse_invoices(udis, "AR")
ok("MXV: before, 1,000 (UDIS) labelled MXN", identical(r_old$Moneda, "MXN") && isTRUE(all.equal(r_old$`Saldo vencido`, 1000)))
ok("MXV: now 8,500 pesos, labelled MXN", identical(r_new$Moneda, "MXN") && isTRUE(all.equal(r_new$`Saldo vencido`, 8500)))

# A database kept in USD: USD documents are domestic (DocTotal), pesos foreign.
usd_local <- .parse_invoices(raw[c(1, 3), ], "AR", moneda_local = "USD")
ok("moneda_local USD: a USD invoice reads DocTotal - PaidToDate",
   isTRUE(all.equal(usd_local$`Saldo vencido`[usd_local$Moneda == "USD"], 18552.5 - 1855.25)))

# ── 4. .sap_moneda_local ─────────────────────────────────────────────────────
conexiones <- NULL
load_erp_connections <- function(cid) if (is.function(conexiones)) conexiones() else conexiones
fila <- function(config) data.frame(
  company_initials = '["NTS"]', erp_type = "sap_b1_service_layer", active = TRUE,
  config = config, secrets_encrypted = "x", stringsAsFactors = FALSE)

conexiones <- fila('{"url":"u","company":"c"}')
ok(".sap_moneda_local: no moneda_local in the connection -> MONEDA_BASE_OMISION",
   identical(.sap_moneda_local("NTS", "cli"), MONEDA_BASE_OMISION))
conexiones <- fila('{"url":"u","company":"c","moneda_local":"usd"}')
ok(".sap_moneda_local: the connection's moneda_local (normalized)", identical(.sap_moneda_local("nts", "cli"), "USD"))
conexiones <- fila('{"moneda_local":"MXP"}')
ok(".sap_moneda_local: a legacy code is mapped", identical(.sap_moneda_local("NTS", "cli"), "MXN"))
conexiones <- fila('{"moneda_local":"pesos"}')
ok(".sap_moneda_local: a value outside the catalog -> the default", identical(.sap_moneda_local("NTS", "cli"), MONEDA_BASE_OMISION))
conexiones <- fila("{no es json")
ok(".sap_moneda_local: unreadable config -> the default", identical(.sap_moneda_local("NTS", "cli"), MONEDA_BASE_OMISION))
conexiones <- function() stop("S3 caído")
ok(".sap_moneda_local: unreadable store -> the default", identical(.sap_moneda_local("NTS", "cli"), MONEDA_BASE_OMISION))
conexiones <- fila('{"moneda_local":"USD"}')
ok(".sap_moneda_local: another company's connection is not used", identical(.sap_moneda_local("NCR", "cli"), MONEDA_BASE_OMISION))
old_cid <- Sys.getenv("CLIENT_ID"); Sys.setenv(CLIENT_ID = "")
ok(".sap_moneda_local: no client at all -> the default, no store read",
   identical(.sap_moneda_local("NTS", NULL), MONEDA_BASE_OMISION))
Sys.setenv(CLIENT_ID = old_cid)

# .sap_creds_from_store still reads the same row (refactor to .sap_fila_conexion).
decrypt_secret <- function(x) list(user = "u", password = "p")
conexiones <- fila('{"url":"https://sap","company":"DB","ssl_verify":false}')
cr <- suppressMessages(.sap_creds_from_store("NTS", "cli"))
ok(".sap_creds_from_store: same creds after the refactor",
   identical(cr, suppressMessages(viejo$.sap_creds_from_store("NTS", "cli"))) &&
     identical(cr$url, "https://sap") && identical(cr$ssl_verify, FALSE))
conexiones <- NULL
ok(".sap_creds_from_store: no store -> NULL", is.null(.sap_creds_from_store("NTS", "cli")))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
