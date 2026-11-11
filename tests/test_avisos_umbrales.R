# =============================================================================
# tests/test_avisos_umbrales.R
# Frente D: the weekly CxC past-due rule (R/avisos_umbrales.R) with invented
# series -- steady, volatile, cold start, small empresa, strong rise and
# strong fall -- plus the series write (strict, idempotent) and the avisos it
# publishes (CxC tiers only, one per clave, expiring on Sunday).
# S3 is the in-memory store of tests/helpers/s3_memoria.R; nothing reaches
# the network.
#
# Run from project root:  Rscript tests/test_avisos_umbrales.R
# =============================================================================
cat("=== test_avisos_umbrales ===\n\n")
source("tests/helpers/s3_memoria.R")
suppressPackageStartupMessages(library(shiny))
source("R/i18n.R")
source("R/tier_registry.R")
source("R/permisos.R")
source("R/avisos.R")
suppressMessages(source("R/data_pipeline.R"))   # apply_ic_filter, build_ic_fullcodes
source("R/avisos_umbrales.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
falla <- function(expr) inherits(tryCatch({ force(expr); NULL }, error = function(e) e), "error")

CID   <- "acme"
LUNES <- as.Date("2026-11-16")   # a Monday

# A series for one empresa: the % past due on consecutive Mondays ending on
# LUNES, over an open balance of 1,000 and `facturas` open invoices.
serie <- function(pcts, empresa = "NL", moneda = "MXN", facturas = 20L, abierto = 1000) {
  lunes <- LUNES - 7L * rev(seq_along(pcts) - 1L)
  tibble::tibble(semana = umbral_semana(lunes), lunes = lunes, empresa = empresa, moneda = moneda,
                 saldo_abierto = abierto, saldo_vencido = abierto * pcts / 100,
                 facturas = as.integer(facturas))
}
# pcts from a start value and the weekly changes
desde_deltas <- function(deltas, inicio = 10) inicio + c(0, cumsum(deltas))
ESTABLE <- c(0.3, -0.2, 0.4, -0.1, 0.2, 0.0, -0.3, 0.1)
VOLATIL <- c(2.1, -1.8, 2.5, -2.2, 1.9, -2.4, 2.0, -1.7)
alertas <- function(s) umbral_evaluar(s, LUNES)

cat("-- la regla, con los números del paso 0 --\n")
ok("estable: +1.8 pts alerta, y dice que subieron", {
  a <- alertas(serie(desde_deltas(c(ESTABLE, 1.8))))
  nrow(a) == 1 && a$direccion == "sube" && abs(a$delta - 1.8) < 1e-9 && abs(a$umbral - 1.0) < 1e-9
})
ok("estable: +0.9 pts no alerta (bajo el piso de 1.0)",
   nrow(alertas(serie(desde_deltas(c(ESTABLE, 0.9))))) == 0)
ok("volátil: +1.8 pts es su variación habitual, no alerta", {
  a <- alertas(serie(desde_deltas(c(VOLATIL, 1.8))))
  nrow(a) == 0 && max(3 * stats::mad(VOLATIL), 1) > 8
})
ok("bajada fuerte: -1.8 pts también alerta, como bajada", {
  a <- alertas(serie(desde_deltas(c(ESTABLE, -1.8))))
  nrow(a) == 1 && a$direccion == "baja"
})
ok("arranque en frío: +6 pts con 2 semanas de historia no alerta",
   nrow(alertas(serie(desde_deltas(c(0.2, -0.1, 6))))) == 0)
ok("con 4 Δ de historia ya alerta", {
  nrow(alertas(serie(desde_deltas(c(0.2, -0.1, 0.1, 0.0, 6))))) == 1 &&
  nrow(alertas(serie(desde_deltas(c(0.2, -0.1, 0.1, 6))))) == 0
})
ok("empresa chica (6 facturas): +16.7 pts no alerta",
   nrow(alertas(serie(desde_deltas(c(ESTABLE, 16.7)), facturas = 6L))) == 0)
ok("empresa con menos del 5% del saldo de su moneda no alerta, la grande sí", {
  s <- dplyr::bind_rows(serie(desde_deltas(c(ESTABLE, 3))),
                        serie(desde_deltas(c(ESTABLE, 3)), empresa = "NG", abierto = 1000 * 0.04 / 0.96 - 1),
                        serie(desde_deltas(c(ESTABLE, 3)), empresa = "NL", moneda = "USD", abierto = 10))
  a <- alertas(s)
  nrow(a) == 2 && setequal(paste(a$empresa, a$moneda), c("NL MXN", "NL USD"))
})
ok("sin el lunes anterior no hay Δ: un hueco no es un cambio semanal", {
  s <- serie(desde_deltas(c(ESTABLE, 0, 5)))
  nrow(alertas(s)) == 1 && nrow(alertas(s[-(nrow(s) - 1L), ])) == 0
})
ok("si el % bajó, nunca dice que subió (aunque se aleje de su mediana)", {
  # usually falls 2 pts a week; this week fell only 0.1: unusual, but not a rise
  nrow(alertas(serie(desde_deltas(c(-2, -2.1, -1.9, -2, -2.05, -0.1), inicio = 30)))) == 0
})
ok("una semana sin filas no da alertas", nrow(umbral_evaluar(serie(desde_deltas(ESTABLE)), LUNES + 7)) == 0)

cat("\n-- la métrica desde el ledger de CxC --\n")
ok("% vencido por empresa y moneda, con saldo después de abonos y fecha original", {
  ar <- tibble::tibble(
    Empresa = c("NL", "NL", "NL", "NL", "NG"), Moneda = c("MXN", "MXN", "MXN", "USD", "MXN"),
    `Saldo vencido` = c(300, 700, 0, 50, 80),
    FechaVenc_Original = as.Date(c("2026-11-10", "2026-11-20", "2026-11-01", "2026-11-15", NA)))
  m <- umbral_metricas(ar, LUNES, interco = NULL, cmap = character())
  nl <- m[m$empresa == "NL" & m$moneda == "MXN", ]
  nrow(m) == 3 && nl$saldo_abierto == 1000 && nl$saldo_vencido == 300 && nl$facturas == 2L &&
    m$saldo_vencido[m$moneda == "USD"] == 50 && m$saldo_vencido[m$empresa == "NG"] == 0 &&
    all(m$semana == "2026-W47") && all(m$lunes == LUNES)
})
ok("un ledger vacío da cero filas", nrow(umbral_metricas(NULL, LUNES, NULL, character())) == 0)

# Intercompany: a past-due IC invoice must not move the %.
ar_ic <- tibble::tibble(
  Empresa = "NL", Moneda = "MXN", Parte = c("Cliente A", "Cliente B", "NG Logística", "Cliente C"),
  `Código de cliente` = c("C100", "C200", "C1027", "C300"), RFC = c("AAA010101AA1", "", "", ""),
  `Saldo vencido` = c(300, 700, 5000, 100),
  FechaVenc_Original = as.Date(c("2026-11-10", "2026-11-20", "2026-10-01", "2026-11-01")))
pct_nl <- function(m) 100 * m$saldo_vencido / m$saldo_abierto
ok("una factura IC vencida (código del registro interco) no mueve el %", {
  reg <- list(ar_prefix = "C", ap_prefix = "P", companies = list(NG = list(ar = "1027", ap = character())))
  m <- umbral_metricas(ar_ic, LUNES, interco = reg, cmap = c(NL = "NL Transportes", NG = "NG Logística"))
  nrow(m) == 1 && m$saldo_abierto == 1100 && m$facturas == 3L && abs(pct_nl(m) - 400 / 11) < 1e-9
})
ok("un RFC del registro también la saca, aunque su código no esté", {
  reg <- list(ar_prefix = "C", companies = list(NG = list(ar = "1027")), rfcs = c(XX = "AAA010101AA1"))
  m <- umbral_metricas(ar_ic, LUNES, interco = reg, cmap = character())
  m$saldo_abierto == 800 && m$facturas == 2L
})
ok("sin registro, la saca por el nombre de la empresa del grupo (el mismo respaldo de la app)", {
  m <- umbral_metricas(ar_ic, LUNES, interco = NULL, cmap = c(NG = "NG Logística"))
  m$saldo_abierto == 1100 && m$facturas == 3L
})
ok("umbral_metricas exige interco y cmap (nadie se salta el filtro IC)",
   falla(umbral_metricas(ar_ic, LUNES)))

cat("\n-- la serie en S3 --\n")
k_met <- paste0(CID, "/avisos/metricas.rds")
ok("guardar dos veces la misma semana no duplica filas", {
  s3_memoria_reset()
  base <- serie(desde_deltas(ESTABLE))
  umbral_serie_guardar(CID, base[-nrow(base), ])
  umbral_serie_guardar(CID, base[nrow(base), ])
  umbral_serie_guardar(CID, base[nrow(base), ])
  g <- .mem[[k_met]]
  nrow(g) == nrow(base) && !anyDuplicated(paste(g$semana, g$empresa, g$moneda))
})
ok("una re-corrida del lunes reemplaza la fila de esa semana", {
  nueva <- serie(desde_deltas(ESTABLE))[9, ]; nueva$saldo_vencido <- 999
  umbral_serie_guardar(CID, nueva)
  g <- .mem[[k_met]]
  sum(g$lunes == LUNES) == 1 && g$saldo_vencido[g$lunes == LUNES] == 999
})
ok("si leer la serie falla, no se escribe nada (nunca una serie vacía)", {
  antes <- .mem[[k_met]]; esc <- .mem_escrituras
  .mem_falla <<- k_met
  f <- falla(umbral_serie_guardar(CID, serie(10, empresa = "NG")))
  .mem_falla <<- character()
  f && identical(.mem[[k_met]], antes) && .mem_escrituras == esc
})

cat("\n-- el aviso --\n")
ok("el aviso es solo para los niveles que ven CxC (nunca Compras ni staff)", {
  niv <- umbral_niveles_cxc()
  length(niv) > 0 && all(niv %in% niveles_cliente()) &&
    !any(vapply(niv, function(t) identical(nivel_calendario(t), "AP"), logical(1))) &&
    any(vapply(niveles_cliente(), function(t) identical(nivel_calendario(t), "AP"), logical(1)))
})
ok("publica uno por alerta; una segunda corrida del mismo lunes no duplica", {
  s3_memoria_reset()
  a <- alertas(dplyr::bind_rows(serie(desde_deltas(c(ESTABLE, 1.8))),
                                serie(desde_deltas(c(ESTABLE, -2)), empresa = "NG")))
  n1 <- umbral_publicar(CID, a, LUNES)
  n2 <- umbral_publicar(CID, a, LUNES)
  av <- avisos_leer(CID)
  n1 == 2 && n2 == 2 && nrow(av) == 2 && all(av$contador == 2L) &&
    setequal(av$clave, c("umbral:cxc_vencidos:sube:NL:MXN:2026-W47", "umbral:cxc_vencidos:baja:NG:MXN:2026-W47"))
})
ok("el texto sale del diccionario, en español, con puntos y %", {
  av <- avisos_leer(CID)
  sube <- av[grepl(":sube:", av$clave), ]; baja <- av[grepl(":baja:", av$clave), ]
  sube$titulo == "Los vencidos de CxC de NL (MXN) subieron 1.8 pts" &&
    grepl("^Ahora están en 12\\.2%", sube$cuerpo) &&
    baja$titulo == "Los vencidos de CxC de NG (MXN) bajaron 2.0 pts" &&
    sube$severidad == "atencion" && baja$severidad == "info"
})
ok("vence el domingo de esa semana a las 23:59 de México", {
  v <- avisos_leer(CID)$vence[1]
  format(v, "%Y-%m-%d %H:%M:%S", tz = UMBRAL_TZ) == "2026-11-22 23:59:59"
})
ok("Compras no lo ve; finanzas sí", {
  ve <- function(t) nrow(avisos_visibles(CID, list(user = "x", tier = t, permisos = permisos_resueltos(t))))
  compras <- Filter(function(t) identical(nivel_calendario(t), "AP"), niveles_cliente())
  all(vapply(compras, ve, integer(1)) == 0L) && ve("finance") == 2L
})

s3_memoria_restaurar()
cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
