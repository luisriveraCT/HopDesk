# =============================================================================
# tests/test_bancos_detect_cuenta.R
# detectar_cuenta_banbajio(): which registered account a BanBajío TXT is for.
#
# Regression origin (2026-10-26): an NCS MXN file was suggested as "NL — BanBajío
# MXN" because the old banner scored names from the global COMPANY_MAP (bare
# initials -> every score 0 -> first account wins) and ignored currency.
#
# Headers below copy the exact SHAPE of 10 real ConsultaMovimientos files
# (6 companies, MXN + USD, including the mojibaked "Dólar"); totals and
# movements are synthetic so no real bank statement lives in the repo.
# The Settings accounts mirror Configuración → Cuentas as of that date
# (numbers only, no CLABE).
#
# Run: Rscript tests/test_bancos_detect_cuenta.R
# =============================================================================

cat("── detectar_cuenta_banbajio: empresa + moneda + subcuenta del TXT ──\n")

suppressPackageStartupMessages({ library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/bancos_parser.R")
source("R/bancos_detect.R")

.pass <- 0L
.fail <- 0L
.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

DOLAR <- "D��lar"   # how BanBajío's "Dólar" actually arrives

mk_txt <- function(razon, base, producto, sub, movs) {
  cg <- sum(vapply(movs, `[[`, numeric(1), "cargo"))
  ab <- sum(vapply(movs, `[[`, numeric(1), "abono"))
  hdr <- sprintf("%s,%s,%s,%s,26-Oct-2026,12:16:39,Mes Actual,TODOS,01-Oct-2026 al 31-Oct-2026,,%d,%.2f,%.2f",
                 razon, base, producto, sub, length(movs), cg, ab)
  rows <- vapply(seq_along(movs), function(i) {
    m <- movs[[i]]
    sprintf("%d,2%d-Oct-2026,12:00:%02d,%d,%s,%s,%s,%.2f", i, i %% 9, i, 1000 + i,
            m$desc, if (m$cargo > 0) sprintf("%.2f", m$cargo) else "",
            if (m$abono > 0) sprintf("%.2f", m$abono) else "", 1000)
  }, character(1))
  c(hdr, "#,Fecha Movimiento,Hora,Recibo,Descripción,Cargos,Abonos,Saldo", rows)
}
mxn_movs <- list(
  list(desc = "SPEI Enviado: | Beneficiario: PROV SA DE CV por (840.00) mxn | Recibo # 1", cargo = 840, abono = 0),
  list(desc = "Retiro de Nomina empresas por (510621.02) mxn | Recibo # 2", cargo = 510621.02, abono = 0),
  list(desc = "SPEI Recibido: | Ordenante: CLIENTE SA DE CV | Recibo # 3", cargo = 0, abono = 1624860))
usd_movs <- list(
  list(desc = "Envio SPID:  | Beneficiario: PROV SA DE CV | Importe en dolar: $ 5788.96", cargo = 5788.96, abono = 0),
  list(desc = "Retiro por operacion cambios por (42423.30) usd | Recibo # 5", cargo = 42423.30, abono = 0),
  list(desc = "Depósito de transferencia del extranjero por 29294.48 usd", cargo = 0, abono = 29294.48))

cuentas <- tibble(
  cuenta_id = c("ncs_mxn", "ncs_usd", "ng_mxn", "nl_mxn", "nl_usd", "nrs_mxn",
                "nrs_usd", "nts_mxn", "nts_usd", "pl_mxn"),
  empresa   = c("NCS", "NCS", "NG", "NL", "NL", "NRS", "NRS", "NTS", "NTS", "PL"),
  moneda    = c("MXN", "USD", "MXN", "MXN", "USD", "MXN", "USD", "MXN", "USD", "MXN"),
  numero_cuenta = c("073543010201", "073543010402", "334332850201", "133852570201",
                    "133852570401", "133865030201", "133865030402", "133861560201",
                    "0133861560401", "501848780201"),
  clabe     = c(rep(NA, 9), "030680900047301333"))
# Deliberately put NL first: the old algorithm's failure mode was "first wins".
cuentas <- cuentas[c(4, 5, 1, 2, 3, 6:10), ]

empresas <- tibble(
  initials     = c("NCS", "NG", "NL", "NRS", "NTS", "PL"),
  razon_social = c("Networks Crossdocking Services S.A. de C.V.", "Networks Group LCT SC",
                   "Networks & Logistics SA de CV", "Networks Realtors SA de CV",
                   "Networks Trucking Services SA de CV", "Paragon Logistics S de RL de CV"),
  nombre_corto = c("Networks Crossdocking", "Networks Group", "Networks Logistics",
                   "Networks Realtors", "Networks Trucking", "Paragon"),
  nombres_alt  = "[]")

run <- function(lines, cts = cuentas, emp = empresas)
  detectar_cuenta_banbajio(lines, cts, emp, parsed = parse_banbajio_txt(lines, "x"))

cat("\n[1] Los 10 encabezados reales -> cuenta exacta, confianza alta\n")
casos <- list(
  list("NETWORKS CROSSDOCKING SERVICES S.A. DE C.V.", "7354301", "Conecta BanBajio", "01", mxn_movs, "ncs_mxn"),
  list("NETWORKS CROSSDOCKING SERVICES S.A. DE C.V.", "7354301", DOLAR,              "02", usd_movs, "ncs_usd"),
  list("NETWORKS & LOGISTICS SA DE CV",               "13385257", "Conecta BanBajio GPO", "01", mxn_movs, "nl_mxn"),
  list("NETWORKS & LOGISTICS SA DE CV",               "13385257", paste(DOLAR, "GPO"),    "01", usd_movs, "nl_usd"),
  list("NETWORKS TRUCKING SERVICES SA DE CV",         "13386156", "Conecta BanBajio GPO", "01", mxn_movs, "nts_mxn"),
  list("NETWORKS TRUCKING SERVICES SA DE CV",         "13386156", paste(DOLAR, "GPO"),    "01", usd_movs, "nts_usd"),
  list("NETWORKS REALTORS SA DE CV",                  "13386503", "Conecta BanBajio GPO", "01", mxn_movs, "nrs_mxn"),
  list("NETWORKS GROUP LCT SC",                       "33433285", "Conecta BanBajio GPO", "01", mxn_movs, "ng_mxn"))
for (c_ in casos) {
  r <- run(mk_txt(c_[[1]], c_[[2]], c_[[3]], c_[[4]], c_[[5]]))
  .chk(r$sugerida, c_[[6]], sprintf("%s %s -> %s", c_[[2]], substr(c_[[3]], 1, 7), c_[[6]]))
  .chk(r$confianza, "alta", sprintf("  confianza alta (%s)", c_[[6]]))
  .chk(length(r$avisos), 0L, sprintf("  sin avisos (%s)", c_[[6]]))
}

cat("\n[2] Moneda: encabezado mojibake y voto por descripciones\n")
.chk(.moneda_txt(DOLAR, character(0)), "USD", "\"D��lar\" -> USD")
.chk(.moneda_txt("Conecta BanBajio", vapply(mxn_movs, `[[`, "", "desc")), "MXN",
     "\"Conecta\" sin pista -> voto por descripciones = MXN")
.chk(.moneda_txt("Producto X", vapply(usd_movs, `[[`, "", "desc")), "USD",
     "producto desconocido + SPID/usd -> USD")
.chk(.moneda_txt("", character(0)), NA_character_, "sin evidencia -> NA")

cat("\n[3] Encabezado robusto: razón social con comas\n")
h <- leer_encabezado_banbajio("COCA COLA, S.A. DE C.V.,9876543,Conecta BanBajio,01,26-Oct-2026,12:00:00,Mes Actual,TODOS,x,,3,10.00,20.00")
.chk(h$razon_social, "COCA COLA, S.A. DE C.V.", "razón social con coma intacta")
.chk(h$numero, "9876543", "número de cuenta localizado")
.chk(c(h$total_cargos, h$total_abonos), c(10, 20), "totales desde el final")

cat("\n[4] Sin número registrado -> sugiere por nombre + moneda, confianza media\n")
sin_num <- cuentas; sin_num$numero_cuenta <- NA; sin_num$clabe <- NA
r <- run(mk_txt("NETWORKS CROSSDOCKING SERVICES S.A. DE C.V.", "7354301", DOLAR, "02", usd_movs), sin_num)
.chk(r$sugerida, "ncs_usd", "nombre + moneda -> NCS USD")
.chk(r$confianza, "media", "confianza media")
.chk(any(grepl("Ninguna cuenta registrada", r$avisos)), TRUE, "avisa registrar el número")

cat("\n[5] Número coincide pero la moneda registrada no -> no es alta\n")
mal <- cuentas; mal$moneda[mal$cuenta_id == "ncs_usd"] <- "MXN"
mal <- mal[mal$cuenta_id != "ncs_mxn", ]
r <- run(mk_txt("NETWORKS CROSSDOCKING SERVICES S.A. DE C.V.", "7354301", DOLAR, "02", usd_movs), mal)
.chk(r$confianza != "alta", TRUE, "confianza no alta")
.chk(any(grepl("parece estar en USD", r$avisos)), TRUE, "avisa la diferencia de moneda")

cat("\n[6] Totales que no cuadran -> aviso, sin bajar la confianza de identidad\n")
ln <- mk_txt("NETWORKS REALTORS SA DE CV", "13386503", "Conecta BanBajio GPO", "01", mxn_movs)
ln <- ln[-length(ln)]                   # archivo truncado: falta un movimiento
r <- run(ln)
.chk(r$sugerida, "nrs_mxn", "sigue identificando NRS MXN")
.chk(r$confianza, "alta", "identidad alta")
.chk(any(grepl("no cuadran", r$avisos)), TRUE, "avisa totales")

cat("\n[7] Conglomerado: 60 empresas casi iguales, mismo banco\n")
set.seed(1)
n <- 60L
bases <- as.character(sample(10000000:99999999, n))
cong_cts <- tibble(
  cuenta_id = c(paste0("c", seq_len(n), "_mxn"), paste0("c", seq_len(n), "_usd")),
  empresa   = rep(paste0("KO", seq_len(n)), 2),
  moneda    = rep(c("MXN", "USD"), each = n),
  numero_cuenta = c(paste0(bases, "0201"), paste0(bases, "0401")),
  clabe = NA)
cong_cts <- cong_cts[sample(nrow(cong_cts)), ]
cong_emp <- tibble(initials = paste0("KO", seq_len(n)),
                   razon_social = sprintf("Embotelladora Regional %s S.A. de C.V.",
                                          c("Norte", "Sur", "Centro")[(seq_len(n) %% 3) + 1]),
                   nombre_corto = "", nombres_alt = "[]")   # names deliberately collide
ok <- 0L
for (j in c(7L, 23L, 41L, 60L)) {
  rm <- run(mk_txt(cong_emp$razon_social[j], bases[j], "Conecta BanBajio", "01", mxn_movs), cong_cts, cong_emp)
  ru <- run(mk_txt(cong_emp$razon_social[j], bases[j], DOLAR, "01", usd_movs), cong_cts, cong_emp)
  ok <- ok + (rm$sugerida == sprintf("c%d_mxn", j) && rm$confianza == "alta") +
             (ru$sugerida == sprintf("c%d_usd", j) && ru$confianza == "alta")
}
.chk(ok, 8L, "8/8 archivos -> cuenta exacta aunque los nombres choquen")
r <- run(mk_txt(cong_emp$razon_social[5], "11111111", "Conecta BanBajio", "01", mxn_movs),
         cong_cts, cong_emp)
.chk(r$confianza, "baja", "número desconocido + nombres repetidos -> baja (no adivina)")

cat("\n[8] Base corta no debe prefijar un número largo ajeno\n")
.chk(.cuenta_vs_base("7354301999990201", NA, "7354301", "01")$match, FALSE,
     "resto > 4 dígitos -> no coincide")
.chk(.cuenta_vs_base(NA, "030680900047301333", "5018487", "01")$match, FALSE,
     "CLABE de otra cuenta -> no coincide")
.chk(.cuenta_vs_base("7354301", NA, "7354301", "02")$match, TRUE,
     "Settings con base corta -> coincide (sin subcuenta)")

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
