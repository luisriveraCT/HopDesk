# =============================================================================
# tests/test_t45_correos_semana.R
# T45: the redesigned weekly emails (R/avisos_datos.R, R/avisos_correos.R).
#   Monday  every figure with its code, top 10, cards that explain themselves,
#           day by day and the 4+4 weeks context, the client's base currency.
#   Friday  each committed item of Monday's plan classified by its state at
#           the close -- hecho (HopDesk), liquidado (the ERP: no longer open,
#           or its balance went down), reprogramado, por_vencer, atrasado,
#           cambio -- no close without SAP data newer than Monday's, a
#           mid-week close says so; the real context weeks only from the
#           coverage threshold (T45 v3, P3 measured in prod); plus what
#           is apart from the plan (outside it, intercompany, plans not
#           committed), concentration, "moved N weeks in a row" and the
#           history-less wording; a base other than MXN (EUR) everywhere.
# Invented data; S3 is the in-memory store (tests/helpers/s3_memoria.R).
# Nothing reaches the network.
#
# Run from project root:  Rscript tests/test_t45_correos_semana.R
# =============================================================================
cat("=== test_t45_correos_semana ===\n\n")
source("tests/helpers/s3_memoria.R")
for (e in parse("R/global.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "S3_KEYS_CRITICAL"))
    eval(e, globalenv())
suppressPackageStartupMessages({ library(shiny); library(tidyr); library(stringr); library(lubridate) })
source("R/i18n.R")
source("R/monedas.R")
source("R/tier_registry.R")
source("R/permisos.R")
source("R/avisos.R")
source("R/erp_connector_registry.R")
suppressMessages({
  source("R/data_pipeline.R")
  source("R/pasivos_engine.R")
  source("R/pasivos_calendar_glue.R")
  source("R/pasivos_schemas.R")
  source("R/pasivos_persistence.R")
  source("R/bancos_persistence.R")
  source("R/cashflow_export_module.R")
  source("R/fx_banxico.R")
  source("R/fx_tipos.R")
})
source("R/panorama_datos.R")
source("R/ui_components.R")
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
tiene <- function(h, ...) all(vapply(c(...), function(s) grepl(s, h, fixed = TRUE), logical(1)))
limpio <- function(x) !grepl("avisos\\.correo\\.|semana\\.tarjeta\\.|\\{[a-z_0-9]+\\}", x)

LUNES <- as.Date("2026-11-16"); VIERNES <- LUNES + 4L
FC <- as.Date("2026-10-30")   # every invoice's posting date
CMAP <- c(NL = "NL Transportes", NG = "NG Logística")
fin <- list(idioma = "es", ver_cxc = TRUE)
com <- list(idioma = "es", ver_cxc = FALSE)
eng <- list(idioma = "en", ver_cxc = TRUE)

fila <- function(tipo, doc, parte, monto, fecha, moneda = "MXN", source = "sap", pid = NA_character_)
  tibble::tibble(Tipo = tipo, Empresa = "NL Transportes", Documento = doc, Parte = parte, Moneda = moneda,
                 `Fecha de contabilización` = FC, Saldo_original = monto, `Saldo vencido` = monto,
                 source = source, id = NA_character_, provision_id = pid, FechaEff = as.Date(fecha),
                 FechaVenc_Original = as.Date(fecha), `Código de cliente` = NA_character_)
conf <- function(tipo, doc, parte, importe, fecha, codigo = NA_character_, moneda = "MXN", pid = NA_character_)
  tibble::tibble(confirmacion_id = paste0("c", doc), empresa = "NL Transportes", documento = doc, parte = parte,
                 codigo = codigo, importe = importe, moneda = moneda, fecha = as.Date(fecha), tipo = tipo,
                 eliminado = FALSE, action = NA_character_, fecha_factura = FC, provision_id = pid)

# ── Friday fixture: Monday's plan and Friday's state ─────────────────────────
plan <- dplyr::bind_rows(
  fila("AR", "A1", "Cliente Uno", 400, LUNES + 1),     # collected
  fila("AR", "A2", "Cliente Dos", 300, LUNES + 2),     # moved to Dec 2
  fila("AR", "A3", "Cliente Tres", 200, LUNES + 2),    # still open, date passed: atrasado
  fila("AR", "A4", "Cliente Cuatro", 150, LUNES + 3),  # amount went up to 180: cambio
  fila("AR", "A5", "Cliente Cinco", 120, LUNES + 3),   # no longer open in the ERP, not confirmed: liquidado
  fila("AR", "A7", "Cliente Siete", 200, LUNES + 2),   # balance down to 150: 50 liquidado, 150 atrasado
  fila("AR", "M1", "Cliente Manual", 90, LUNES + 2, source = "manual"),  # a HopDesk row deleted: cambio
  fila("AR", "A6", "Cliente Seis", 60, LUNES + 5),     # due on Saturday, unchanged: not late yet
  fila("AP", "P1", "Proveedor Uno", 500, LUNES + 1),   # paid (83% of what was paid)
  fila("AP", "P2", "Proveedor Dos", 100, LUNES + 2),   # paid
  fila("AP", "PROV_pv1", "Banco Uno", 50, LUNES + 3, source = "provision", pid = "pv1"),  # liability: committed, paid
  fila("AP", "PROV_pv2", "Cotización A", 70, LUNES + 3, source = "provision", pid = "pv2"),  # requisition: plan
  fila("AP", "PROV_pv3", "Manual suelta", 30, LUNES + 3, source = "provision", pid = "pv3"))  # manual: plan
semana <- list(lunes = LUNES, moneda_base = "MXN", vencidos_pct = 10, filas = plan)
hoy_df <- dplyr::bind_rows(
  fila("AR", "A2", "Cliente Dos", 300, "2026-12-02"),
  fila("AR", "A3", "Cliente Tres", 200, LUNES + 2),
  fila("AR", "A4", "Cliente Cuatro", 180, LUNES + 3),
  fila("AR", "A7", "Cliente Siete", 150, LUNES + 2),
  fila("AR", "A6", "Cliente Seis", 60, LUNES + 5),
  fila("AR", "N1", "Cliente Sig", 900, LUNES + 8),
  fila("AP", "N2", "Proveedor Sig", 700, LUNES + 9))
hoy_df$`Saldo_original`[hoy_df$Documento == "A4"] <- 150
bancos <- dplyr::bind_rows(
  conf("cobro", "A1", "Cliente Uno", 400, LUNES + 1),
  conf("pago", "P1", "Proveedor Uno", 500, LUNES + 1),
  conf("pago", "P2", "Proveedor Dos", 100, LUNES + 2),
  conf("cobro", "X9", "Cliente Nuevo", 80, LUNES + 3, codigo = "C999"),     # outside the plan
  conf("pago", "X8", "NG Logística", 1000, LUNES + 3, codigo = "P777"))     # intercompany (by name)
provs <- tibble::tibble(id = c("pv1", "pv2", "pv3"), estado = c("item_confirmed", "provisional", "provisional"),
                        liability_id = c("L1", NA, NA), origin = c("rule", "manual", "manual"))
sh <- list(company_map = function() CMAP, bancos_confirmados = function() bancos, papelera_rv = function() NULL,
           pasivos_provisions_db = function() provs,
           interco_v2 = function() list(ar_prefix = "C", ap_prefix = "P", companies = list(), rfcs = character()))
SAP <- as.POSIXct("2026-11-20 14:12:00", tz = "UTC")

cat("-- viernes: clasificación --\n")
dv <- correo_datos_viernes(semana, hoy_df, sh, VIERNES, SAP, url = "https://x", req_ids = "pv2")
comp <- function(lado, k, campo = "monto") { x <- dv[[lado]][["MXN"]]; x[[campo]][x$clase == k] }
ok("cobrado: la confirmada por la regla de la app (A1, 400)", comp("cobros", "hecho") == 400 && comp("cobros", "hecho", "n") == 1)
ok("reprogramado: con fecha nueva después del viernes, y a cuándo (A2 → 2 dic)",
   comp("cobros", "reprogramado") == 300 && identical(dv$reprogramados$fecha_nueva[dv$reprogramados$contraparte == "Cliente Dos"], as.Date("2026-12-02")))
ok("vence el fin de semana: sigue abierta, en la semana, después del viernes; nunca 'reprogramada' a la misma fecha (A6)",
   comp("cobros", "por_vencer") == 60 && !"Cliente Seis" %in% dv$reprogramados$contraparte &&
     !"Cliente Seis" %in% dv$atrasados_cobros$contraparte)
ok("atrasado: sigue abierta, su fecha pasó, sin fecha nueva (A3 200 y el resto de A7, 150)",
   comp("cobros", "atrasado") == 350 && identical(dv$atrasados_cobros$contraparte, c("Cliente Tres", "Cliente Siete")))
ok("cambió de monto o se canceló: el monto subió (A4) o una fila de HopDesk ya no está (M1)",
   comp("cobros", "cambio") == 240 && comp("cobros", "cambio", "n") == 2)
ok("liquidado en el ERP = cobrado: ya no está abierta (A5, 120) o su saldo bajó (A7, por la diferencia: 50)",
   comp("cobros", "liquidado") == 170 && comp("cobros", "liquidado", "n") == 2)
ok("pagado: las confirmadas y la provisión de un pasivo ya item_confirmed (500 + 100 + 50)",
   comp("pagos", "hecho") == 650 && dv$pagos_conf == 3L && dv$pagos_total == 3L)
ok("planes no comprometidos fuera del cierre: 1 de requisición y 1 manual sin pasivo (100 MXN)",
   dv$planes$n == 2 && dv$planes$requisicion == 1 && dv$planes$manual == 1 && dv$planes$montos[["MXN"]] == 100 &&
     sum(dv$pagos$MXN$monto) == 650)
ok("fuera del plan: el cobro que no estaba el lunes (80); la IC va aparte (1000), nunca en pagos",
   dv$fuera$cobros[["MXN"]] == 80 && dv$fuera$cobros_n == 1 && !length(dv$fuera$pagos) &&
     dv$fuera$ic_n == 1 && dv$fuera$ic[["MXN"]] == 1000)
ok("concentración: Proveedor Uno es más del 40% de lo pagado; Cliente Uno de lo cobrado (HopDesk + ERP)", {
  x <- Filter(function(z) z$tipo == "AP", dv$concentracion); y <- Filter(function(z) z$tipo == "AR", dv$concentracion)
  length(x) == 1 && x[[1]]$contraparte == "Proveedor Uno" && abs(x[[1]]$pct - 100 * 500 / 650) < 1e-9 &&
    length(y) == 1 && y[[1]]$contraparte == "Cliente Uno" && abs(y[[1]]$pct - 100 * 400 / 570) < 1e-9
})
ok("una sola partida cobrada nunca dispara el aviso (sería 100% trivialmente)",
   is.null(.avisos_concentracion(data.frame(clase = "hecho", Tipo = "AR", fila = 1L, Moneda = "MXN", Parte = "X",
                                            `Saldo vencido` = 10, check.names = FALSE), "AR")))
ok("el titular dice los hechos en la base: cobrado (HopDesk + ERP), movido y atrasado",
   dv$titular$cobrado == 570 && dv$titular$cobros_reprog == 300 && dv$titular$cobros_atrasado == 350)
ok("con datos de SAP del viernes, el corte es el viernes", isFALSE(dv$sin_cierre) && dv$corte == VIERNES)

cat("\n-- viernes: corte --\n")
sc <- correo_datos_viernes(modifyList(semana, list(sap_fecha = SAP)), hoy_df, sh, VIERNES, SAP, req_ids = "pv2")
ok("SAP no se refrescó desde el plan del lunes: sin cierre, solo la semana que entra",
   isTRUE(sc$sin_cierre) && is.null(sc$cobros) && sc$siguiente$cobros[["MXN"]] == 900)
ok("sin la fecha del lunes en semana.rds: datos del mismo lunes = sin cierre",
   isTRUE(correo_datos_viernes(semana, hoy_df, sh, VIERNES, as.POSIXct("2026-11-16 20:00:00", tz = "UTC"))$sin_cierre))
hsc <- correo_viernes_html(sc, fin)
ok("el correo sin cierre lo dice y muestra solo la semana que entra",
   tiene(hsc, "SAP no se actualizó desde el lunes", "sin cierre esta semana", "La semana que entra") &&
     !grepl("PLAN DEL LUNES|Lo que hay que ver|Desempeño", hsc))
mie <- correo_datos_viernes(semana, hoy_df, sh, VIERNES, as.POSIXct("2026-11-18 20:00:00", tz = "UTC"), req_ids = "pv2")
ok("SAP refrescado a media semana: el corte es ese día; lo que vence después todavía no vence",
   mie$corte == as.Date("2026-11-18") && sum(mie$cobros$MXN$monto[mie$cobros$MXN$clase == "por_vencer"]) == 60 &&
     mie$titular$cobros_atrasado == 350)
ok("y el titular dice a qué día", tiene(correo_viernes_html(mie, fin), "Al miércoles 18, se cobraron"))
ok("la semana que entra: lo programado hoy", dv$siguiente$cobros[["MXN"]] == 900 && dv$siguiente$pagos[["MXN"]] == 700)

cat("\n-- viernes: historia --\n")
ok("sin historia: la racha y el desempeño lo dicen",
   isTRUE(dv$racha$sin_historia) && isTRUE(dv$desempeno$sin_historia) && is.na(dv$desempeno$cobros$semana_pasada))
pasadas <- dplyr::bind_rows(lapply(1:2, function(k) {
  x <- dv$partidas_fila[dv$partidas_fila$clase == "reprogramado", ]; x$lunes <- LUNES - 7L * k; x }))
hist <- tibble::tibble(lunes = LUNES - 7L * 1:4, moneda = "MXN", atrasado_cobros = c(500, 100, 300, 700),
                       atrasado_cobros_n = c(5L, 1L, 3L, 7L), atrasado_pagos = 0, atrasado_pagos_n = 0L, vencidos_pct = NA_real_)
dh <- correo_datos_viernes(semana, hoy_df, sh, VIERNES, SAP, req_ids = "pv2", historial = hist, partidas = pasadas)
ok("se ha movido 3 semanas seguidas: A2 (reprogramada esta y las 2 anteriores)",
   !dh$racha$sin_historia && identical(dh$racha$filas$contraparte, "Cliente Dos") && dh$racha$filas$semanas == 3L)
ok("desempeño: lo atrasado contra la semana pasada (500) y la mediana de 4 (400), sin %",
   dh$desempeno$cobros$hoy == 350 && dh$desempeno$cobros$semana_pasada == 500 && dh$desempeno$cobros$mediana == 400)
ok("con historia de 2 semanas sin la racha, no hay lista ni 'sin historia'", {
  x <- correo_datos_viernes(semana, hoy_df, sh, VIERNES, SAP, req_ids = "pv2", partidas = pasadas[pasadas$lunes == LUNES - 14L, ])
  !x$racha$sin_historia && is.null(x$racha$filas)
})
s3_memoria_reset()
correo_historial_guardar("acme", dv); correo_partidas_guardar("acme", dv$partidas_fila, dv$lunes)
ok("los dos historiales se escriben bajo avisos/ y solo ahí",
   all(c("acme/avisos/semana_historial.rds", "acme/avisos/semana_partidas.rds") %in% ls(.mem)) && length(ls(.mem)) == 2)
ok("semana_partidas guarda 8 semanas como máximo", {
  vieja <- dv$partidas_fila; vieja$lunes <- LUNES - 7L * 9
  correo_partidas_guardar("acme", vieja, vieja$lunes[1])
  correo_partidas_guardar("acme", dv$partidas_fila, dv$lunes)
  p <- get("acme/avisos/semana_partidas.rds", envir = .mem)
  !any(p$lunes == LUNES - 63L) && all(p$lunes == LUNES)
})

cat("\n-- viernes: HTML --\n")
dv$logros <- c("Logro uno", "Logro dos", "Logro tres"); dv$radar <- c("R1", "R2", "R3", "R4")
v <- correo_viernes_html(dv, fin); vc <- correo_viernes_html(dv, com); ve <- correo_viernes_html(dv, eng)
ok("titular con los hechos y montos con código, sin $",
   tiene(v, "Se cobraron 570.00 MXN; 300.00 MXN se movieron a otra semana y 350.00 MXN quedaron atrasados") && !grepl("$", v, fixed = TRUE))
ok("barra apilada por moneda: cobrado en dos tonos (HopDesk y ERP) y un renglón por clase presente",
   tiene(v, "PLAN DEL LUNES EN MXN", "Cobrado según HopDesk", "Cobrado según el ERP (pago o cancelación)", "#E6D6AC",
         "Reprogramado", "Todavía no vence", "Atrasado sin fecha nueva", "Cambió de monto o se canceló") &&
     !grepl("Salió del ERP", v, fixed = TRUE))
ok("lo que hay que ver: lo atrasado", tiene(v, "Lo que hay que ver", "Cliente Tres", "Cliente Siete", "sin fecha nueva"))
ok("reprogramados con a cuándo", tiene(v, "Reprogramados", "&rarr; <b>Mié 2</b>") || tiene(v, "Reprogramados", "&rarr; <b>"))
ok("aparte del plan: fuera del plan, planes no comprometidos e intercompañía",
   tiene(v, "Fuera del plan", "+80.00 MXN", "Planes y provisiones no comprometidos", "1 de requisiciones", "1 manuales sin pasivo",
         "Intercompañía", "1000.00 MXN") || tiene(v, "Fuera del plan", "+80.00 MXN", "Intercompañía", "1,000.00 MXN"))
ok("concentración avisada", tiene(v, "Concentración:", "Proveedor Uno es el 77% de lo pagado", "Cliente Uno es el 70% de lo cobrado"))
ok("sin historia: lo dice en la racha y en el desempeño",
   tiene(v, "Todavía sin historia", "todavía sin semanas anteriores para comparar"))
ok("logros (2) y radar (3) en una línea cada uno", tiene(v, "Logro dos", "R3") && !grepl("Logro tres|R4", v))
ok("Compras: sin cobros en ningún lado", !grepl("Cliente Tres|Cobros que salieron|PLAN DEL LUNES EN MXN</td><td align=\"right\" style=\"font-size:12px;color:#5A6474\"><span style=\"white-space:nowrap\">1,170", vc) &&
     !grepl(">Cobros<", vc) && tiene(vc, "Pagos", "Se pagaron 650.00 MXN"))
ok("solo tablas y estilos en línea, sin claves crudas (es, en)",
   !grepl("<style|display:\\s*flex|display:\\s*grid|<svg", v) && limpio(v) && limpio(vc) && limpio(ve) &&
     tiene(ve, "HOW THE WEEK CLOSED", "Overdue, no new date"))

cat("\n-- lunes --\n")
s3_memoria_reset()
ar_l <- dplyr::bind_rows(lapply(1:12, function(i) fila("AR", paste0("C", i), paste("Cliente", i), 100 * i, LUNES + (i %% 5))))
ap_l <- dplyr::bind_rows(fila("AP", "Q1", "Proveedor USD", 50, LUNES + 1, moneda = "USD"),
                         fila("AP", "Q2", "Proveedor Local", 800, LUNES + 2),
                         fila("AP", "PROV_x", "Crédito", 300, LUNES + 3, source = "provision", pid = "x"),
                         fila("AR", "OLD", "Atrasado viejo", 999, LUNES - 10))
df_l <- dplyr::bind_rows(ar_l, ap_l)
serie <- tibble::tibble(semana = umbral_semana(LUNES), lunes = LUNES, empresa = "NL", moneda = c("MXN", "USD"),
                        saldo_abierto = c(1000, 500), saldo_vencido = c(150, 40), facturas = 5L)
bancos_ant <- bancos; bancos_ant$fecha <- bancos_ant$fecha - 7L   # last week: the context's real weeks
sh_l <- modifyList(sh, list(bancos_confirmados = function() bancos_ant))
banco <- function(cargo) data.frame(fecha = LUNES - 6L, tipo = c("spei_out", "traspaso"), cargo = c(cargo, 5000),
                                     abono = 0, moneda = "MXN")
dl <- correo_datos_lunes(df_l, sh_l, LUNES, serie, NULL, SAP, url = "https://x", moneda_base = "MXN",
                         tipos = c(MXN = 1, USD = 20), banco = banco(600))
ok("top 10 de cobros", nrow(dl$top_cobros) == 10 && dl$top_cobros$monto[1] == 1200)
ok("el top ordena por el equivalente en la base: 50 USD (1,000 MXN) va antes que 800 MXN",
   identical(dl$top_pagos$contraparte[1:2], c("Proveedor USD", "Proveedor Local")))
ok("pasivos: las provisiones de las filas AP de la semana (parte de 'por pagar')",
   dl$pasivos_n == 1 && dl$pasivos[["MXN"]] == 300 && dl$pagos[["MXN"]] == 1100)
ok("cobros vencidos sin fecha nueva, aparte", dl$cobros_atrasados[["MXN"]] == 999)
ok("cartera vencida: % de la base y monto por moneda", abs(dl$vencidos_pct - 15) < 1e-9 && dl$vencido[["USD"]] == 40)
ok("por día: lunes a viernes en la base", nrow(dl$por_dia) == 5 && sum(dl$por_dia$cobros) == sum(ar_l$`Saldo vencido`))
ok("contexto: semanas reales (confirmadas, sin la IC de 1000) + esta + 4 programadas",
   identical(dl$contexto$clase, c(rep("real", 4), "esta", rep("programado", 4))) && dl$contexto$cobros[4] == 480 && dl$contexto$pagos[4] == 600 && dl$contexto$cobros[3] == 0)
ok("cobertura: 600 confirmados (sin la IC) de 600 pagados en el banco (sin traspasos) = 100%", dl$cobertura == 1)
dl_baja <- correo_datos_lunes(df_l, sh_l, LUNES, serie, NULL, SAP, banco = banco(10000))
dl_sin <- correo_datos_lunes(df_l, sh_l, LUNES, serie, NULL, SAP)
ok("cobertura bajo el umbral (6%) o sin estado de cuenta: sin semanas reales",
   abs(dl_baja$cobertura - 0.06) < 1e-9 && identical(dl_baja$contexto$clase, c("esta", rep("programado", 4))) &&
     is.na(dl_sin$cobertura) && identical(dl_sin$contexto$clase, c("esta", rep("programado", 4))))
ok("y la línea dice por qué y cuándo aparecerá lo real",
   tiene(correo_lunes_html(dl_baja, fin), "Lo real aparecerá cuando las confirmaciones en HopDesk cubran los pagos", "hoy cubren el 6%") &&
     tiene(correo_lunes_html(dl_sin, fin), "no hay estado de cuenta"))
ok("sin confirmaciones en las 4 semanas anteriores: solo lo programado",
   identical(correo_datos_lunes(df_l, modifyList(sh, list(bancos_confirmados = function() NULL)), LUNES, serie, NULL, SAP)$contexto$clase,
             c("esta", rep("programado", 4))))
h <- correo_lunes_html(dl, fin)
ok("toda cifra con su código, también la base; sin $",
   tiene(h, "1,200.00 MXN", "50.00 USD", "Esta semana se cobra más de lo que se paga") && !grepl("$", h, fixed = TRUE))
ok("cada tarjeta con su línea", tiene(h, "Por cobrar esta semana", "Facturas de clientes que vencen del", "Incluye 1 pasivo",
                                      "Neto de la semana", "Cartera vencida", "% del saldo por cobrar en MXN", "No incluye 999.00 MXN"))
ok("día por día y contexto", tiene(h, "Día por día", "Esta semana en contexto", "la columna más alta es"))
ok("la caja del correo es fluida (no width=568 fijo fuera de Outlook)",
   tiene(h, "max-width:568px", "<!--[if mso]>") && !grepl('width="568" cellpadding="0" cellspacing="0" border="0" style="width:568px', h, fixed = TRUE))
ok("nota del orden por equivalente cuando hay varias monedas", tiene(h, "su equivalente en MXN"))

cat("\n-- base EUR --\n")
df_e <- df_l; df_e$Moneda[df_e$Moneda == "MXN"] <- "EUR"
serie_e <- serie; serie_e$moneda <- c("EUR", "USD")
de <- correo_datos_lunes(df_e, sh_l, LUNES, serie_e, NULL, SAP, moneda_base = "EUR", tipos = c(EUR = 1, USD = 0.9))
he <- correo_lunes_html(de, fin)
ok("lunes con base EUR: titular, tarjetas y gráficas en EUR; ninguna MXN",
   tiene(he, "EUR", "% del saldo por cobrar en EUR", "Lo que vence cada día de esta semana, en EUR") && !grepl("MXN", he, fixed = TRUE) &&
     names(de$cobros)[1] == "EUR")
plan_e <- plan; plan_e$Moneda <- "EUR"; hoy_e <- hoy_df; hoy_e$Moneda <- "EUR"
bancos_e <- bancos; bancos_e$moneda <- "EUR"
sh_e <- modifyList(sh, list(bancos_confirmados = function() bancos_e))
dve <- correo_datos_viernes(list(lunes = LUNES, moneda_base = "EUR", vencidos_pct = NA, filas = plan_e), hoy_e, sh_e, VIERNES, SAP,
                            req_ids = "pv2")
ve2 <- correo_viernes_html(dve, fin)
ok("viernes con base EUR: el cierre y el titular en EUR; ninguna MXN",
   tiene(ve2, "PLAN DEL LUNES EN EUR", "Se cobraron 570.00 EUR") && !grepl("MXN", ve2, fixed = TRUE) &&
     dve$titular$cobrado == 570)
ok("la base del cliente sale de su configuración; sin ella, la de omisión", {
  s3_memoria_reset()
  assign("acme/group_config.rds", list(moneda_base = "EUR"), envir = .mem)
  a <- avisos_moneda_base("acme")
  .mem_prohibido <<- "acme/group_config.rds"; b <- avisos_moneda_base("acme"); .mem_prohibido <<- character()
  identical(a, list(moneda = "EUR", leida = TRUE)) && identical(b, list(moneda = MONEDA_BASE_OMISION, leida = FALSE))
})

s3_memoria_restaurar()
cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
