# =============================================================================
# tests/test_avisos_logros.R
# Frente D: the Friday "Logros de la semana" and "En el radar" lines
# (R/avisos_logros.R), from invented series and rows: each one appears only
# when the data prove it, never with an amount, sorted by priority, capped,
# in the recipient's language, and without CxC lines for Compras.
# The in-memory S3 helper is only sourced for R/persistence.R; nothing is
# read or written.
#
# Run from project root:  Rscript tests/test_avisos_logros.R
# =============================================================================
cat("=== test_avisos_logros ===\n\n")
source("tests/helpers/s3_memoria.R")
suppressPackageStartupMessages(library(shiny))
source("R/i18n.R")
source("R/tier_registry.R")
suppressMessages(source("R/data_pipeline.R"))
source("R/panorama_datos.R")
source("R/avisos_umbrales.R")
source("R/avisos_correos.R")
source("R/avisos_datos.R")
source("R/avisos_logros.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

LUNES <- as.Date("2026-11-16"); VIERNES <- LUNES + 4L
# A series: % past due per Monday ending on LUNES (open 1,000, 20 invoices).
serie <- function(pcts, empresa = "NL", moneda = "MXN") {
  lunes <- LUNES - 7L * rev(seq_along(pcts) - 1L)
  tibble::tibble(semana = umbral_semana(lunes), lunes = lunes, empresa = empresa, moneda = moneda,
                 saldo_abierto = 1000, saldo_vencido = 10 * pcts, facturas = 20L)
}
dv0 <- list(viernes = VIERNES, pagos_conf = 3L, pagos_total = 5L, cobros_pct = 80,
            datos_sap = as.POSIXct("2026-11-20 06:00:00", tz = "UTC"))
vacio_df <- data.frame(Tipo = character(), Moneda = character(), `Saldo vencido` = numeric(),
                       FechaEff = as.Date(character()), Parte = character(), Documento = character(),
                       check.names = FALSE)
items <- function(s = serie(c(10, 10.2, 9.9, 10.1, 10)), m = NULL, dv = dv0, df = vacio_df)
  correo_items_viernes(s, m, dv, df)
num <- function(it, tipo) vapply(Filter(function(x) x$tipo == tipo, it), function(x) x$n, integer(1))
uno <- function(it, tipo, n) Filter(function(x) x$tipo == tipo && x$n == n, it)
fin <- list(idioma = "es", ver_cxc = TRUE); com <- list(idioma = "es", ver_cxc = FALSE)
eng <- list(idioma = "en", ver_cxc = TRUE)

cat("-- logros --\n")
ok("una semana normal: ningún logro (pagos y cobros incompletos, sin bajadas)", !length(num(items(), "logro")))
ok("1: bajada fuerte con la regla del lunes", {
  it <- items(serie(c(10, 10.3, 10.1, 10.4, 10.3, 10.2, 8.2)))
  1L %in% num(it, "logro") && !1L %in% num(it, "radar")
})
ok("1 en el radar: la subida fuerte", 1L %in% num(items(serie(c(10, 10.3, 10.1, 10.4, 10.3, 10.2, 12.5))), "radar"))
ok("2: el más bajo en 12 semanas, solo con 8 guardadas y sin empate", {
  con8   <- items(serie(c(12, 13, 12, 14, 13, 12, 13, 11.5)))
  con7   <- items(serie(c(12, 13, 12, 14, 13, 12, 11.5)))
  empate <- items(serie(c(12, 13, 11.5, 14, 13, 12, 13, 11.5)))
  2L %in% num(con8, "logro") && !2L %in% num(con7, "logro") && !2L %in% num(empate, "logro")
})
ok("3: una empresa con vencidos el lunes y sin vencidos el viernes", {
  m  <- tibble::tibble(empresa = "NL", moneda = "MXN", saldo_abierto = 900, saldo_vencido = 0, facturas = 18L)
  m2 <- m; m2$saldo_vencido <- 5
  3L %in% num(items(m = m), "logro") && !3L %in% num(items(m = m2), "logro")
})
ok("4: todos los pagos confirmados; con dos sin confirmar, radar 7 con el conteo", {
  a <- items(dv = modifyList(dv0, list(pagos_conf = 5L)))
  b <- items()
  4L %in% num(a, "logro") && !7L %in% num(a, "radar") && !4L %in% num(b, "logro") &&
    uno(b, "radar", 7L)[[1]]$params$pagos == 2L
})
ok("4 no sale en una semana sin pagos programados",
   !4L %in% num(items(dv = modifyList(dv0, list(pagos_conf = 0L, pagos_total = 0L))), "logro"))
ok("6: el 100% de los cobros esperados", 6L %in% num(items(dv = modifyList(dv0, list(cobros_pct = 100))), "logro") &&
     !6L %in% num(items(dv = modifyList(dv0, list(cobros_pct = NA))), "logro"))
ok("14: tres bajadas seguidas a nivel cliente, aunque ninguna sea fuerte", {
  14L %in% num(items(serie(c(10, 10.2, 9.9, 9.8, 9.6))), "logro") &&
    !14L %in% num(items(serie(c(10, 10.2, 9.9, 10.0, 9.6))), "logro")
})

cat("\n-- en el radar --\n")
sig <- LUNES + 8L   # Tuesday of next week
df_sig <- data.frame(Tipo = c("AR", "AP", "AP", "AP", "AP"), Moneda = c("MXN", "MXN", "MXN", "MXN", "USD"),
                     `Saldo vencido` = c(100, 90, 40, 30, 1e6), FechaEff = sig,
                     Parte = c("Cliente", "Prov A", "Prov B", "Prov C", "Prov USD"), Documento = paste0("D", 1:5),
                     check.names = FALSE)
ok("3: pagos de la semana próxima 1.6 a 1 sobre cobros (solo moneda base; el USD no se suma)", {
  r3 <- uno(items(df = df_sig), "radar", 3L)
  length(r3) == 1L && r3[[1]]$params$razon == "1.6"
})
ok("3 no sale bajo 1.3 a 1", {
  d2 <- df_sig; d2[["Saldo vencido"]][1] <- 200
  !3L %in% num(items(df = d2), "radar")
})
ok("6: un proveedor con 35% o más de los pagos de la semana próxima (Prov A, 56%)", {
  r6 <- uno(items(df = df_sig), "radar", 6L)
  length(r6) == 1L && r6[[1]]$params$pct == 56
})
ok("9: SAP con más de 2 días hábiles; el fin de semana no cuenta", {
  viejo <- items(dv = modifyList(dv0, list(datos_sap = as.POSIXct("2026-11-17 15:00:00", tz = "UTC"))))
  lunes_sap <- items(dv = modifyList(dv0, list(viernes = LUNES + 7L,
                                               datos_sap = as.POSIXct("2026-11-20 18:00:00", tz = "UTC"))))
  r9 <- uno(viejo, "radar", 9L)
  dos <- items(dv = modifyList(dv0, list(datos_sap = as.POSIXct("2026-11-18 15:00:00", tz = "UTC"))))
  length(r9) == 1L && r9[[1]]$params$dias == 3L && !9L %in% num(items(), "radar") && !9L %in% num(dos, "radar") &&
    !9L %in% num(lunes_sap, "radar")
})

cat("\n-- textos por destinatario --\n")
todo <- items(serie(c(10, 10.3, 10.1, 10.4, 10.3, 10.2, 8.2)),
              dv = modifyList(dv0, list(pagos_conf = 5L, cobros_pct = 100)), df = df_sig)
ok("hasta 2 logros, los de prioridad más alta (alta antes que media)", {
  t <- correo_items_texto(todo, "logro", fin, CORREO_MAX_LOGROS)
  length(t) == 2L && grepl("bajaron 2.0 pts", t[1], fixed = TRUE)
})
ok("el orden es por prioridad, no por cómo llegaron (normal y media antes que alta, al revés)", {
  t <- correo_items_texto(rev(todo), "logro", fin, CORREO_MAX_LOGROS)
  grepl("bajaron 2.0 pts", t[1], fixed = TRUE) && identical(t[2], "Se confirmaron todos los pagos de la semana.")
})
ok("Compras: sin logros ni radar de CxC", {
  t <- correo_items_texto(todo, "logro", com, 9L); r <- correo_items_texto(todo, "radar", com, 9L)
  identical(t, "Se confirmaron todos los pagos de la semana.") && length(r) >= 1L && !any(grepl("CxC|cobros", r))
})
ok("en inglés, en inglés; sin claves crudas ni marcadores", {
  t <- c(correo_items_texto(todo, "logro", eng, 9L), correo_items_texto(todo, "radar", eng, 9L))
  length(t) >= 4L && !any(grepl("avisos\\.correo|\\{|vencidos", t))
})
ok("ningún renglón lleva montos", {
  t <- c(correo_items_texto(todo, "logro", fin, 9L), correo_items_texto(todo, "radar", fin, 9L))
  !any(grepl("$", t, fixed = TRUE))
})
ok("el correo del viernes los toma de los ítems, en el idioma de quien lo recibe", {
  d <- modifyList(dv0, list(pagos_conf = 5L, cobros_pct = 100, url = "https://x"))
  d$items <- todo
  h <- correo_viernes_html(d, fin); he <- correo_viernes_html(d, eng)
  grepl("LOGROS DE LA SEMANA", h, fixed = TRUE) && grepl("bajaron 2.0 pts", h, fixed = TRUE) &&
    grepl("fell 2.0 pts", he, fixed = TRUE) && grepl("On the radar", he, fixed = TRUE)
})

s3_memoria_restaurar()
cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
