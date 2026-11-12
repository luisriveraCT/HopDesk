# =============================================================================
# tests/test_avisos_datos.R
# Frente D: how the job gets one client's numbers (R/avisos_datos.R): the
# strict read of every key, the job's own `shared` for the app's pipeline
# (build_export_combined_df), the clients it leaves out (unreadable key, no
# or stale SAP snapshot, a pipeline failure) and the Monday email's numbers.
# S3 is the in-memory store of tests/helpers/s3_memoria.R; nothing reaches
# the network, and the test checks the job wrote nothing.
#
# Run from project root:  Rscript tests/test_avisos_datos.R
# =============================================================================
cat("=== test_avisos_datos ===\n\n")
source("tests/helpers/s3_memoria.R")
for (e in parse("R/global.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "S3_KEYS_CRITICAL"))
    eval(e, globalenv())
suppressPackageStartupMessages({ library(shiny); library(tidyr); library(stringr); library(lubridate) })
source("R/i18n.R")
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
})
source("R/panorama_datos.R")
source("R/avisos_umbrales.R")
source("R/avisos_correos.R")
source("R/avisos_datos.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

CID   <- "acme"
LUNES <- as.Date("2026-11-16")
AHORA <- as.POSIXct("2026-11-16 14:00:00", tz = "UTC")
pon   <- function(k, x) assign(.s3_key(k, client_id = CID), x, envir = .mem)
llave <- function(k) .s3_key(k, client_id = CID)

ar <- tibble::tibble(
  Documento = paste0("A", 1:10),
  Empresa   = "NL Transportes",
  Parte     = c("Cliente Uno", "Cliente Dos", "Cliente Tres", "NG Logística", paste("Menor", 1:6)),
  Moneda    = c("MXN", "USD", "MXN", "MXN", rep("MXN", 6)),
  `Saldo vencido` = c(400, 40, 999, 5000, 10, 20, 30, 35, 45, 50),
  `Fecha de vencimiento` = as.Date(c("2026-11-17", "2026-11-18", "2026-11-30", "2026-11-19",
                                     rep("2026-11-20", 6))),
  `Código de cliente` = paste0("C", 101:110),
  check.names = FALSE)
ap <- tibble::tibble(
  Documento = c("P1", "P2"), Empresa = "NL Transportes", Parte = c("Proveedor Uno", "Proveedor Dos"),
  Moneda = "MXN", `Saldo vencido` = c(300, 700),
  `Fecha de vencimiento` = as.Date(c("2026-11-18", "2026-11-19")),
  `Código de proveedor` = c("P201", "P202"), check.names = FALSE)

cargar <- function(guardado_ar = AHORA - 2 * 86400, guardado_ap = AHORA - 2 * 86400) {
  s3_memoria_reset()
  if (!is.null(guardado_ar)) pon(S3_KEYS_CRITICAL$sap_snap_ar, list(data = ar, saved_at = guardado_ar))
  if (!is.null(guardado_ap)) pon(S3_KEYS_CRITICAL$sap_snap_ap, list(data = ap, saved_at = guardado_ap))
  pon(S3_KEYS$empresas, tibble::tibble(initials = c("NL", "NG"), nombre_corto = c("NL Transportes", "NG Logística"),
                                       activa = TRUE, deleted = FALSE))
  pon(S3_KEYS$bancos_confirmados, tibble::tibble(empresa = "NL Transportes", documento = "P2", moneda = "MXN",
                                                 tipo = "pago", eliminado = FALSE, importe = 700))
  pon(S3_KEYS$interco_v2, list(ar_prefix = "C", ap_prefix = "P", companies = list(), rfcs = character()))
  .mem_escrituras <<- 0L
}

cat("-- lectura y shared --\n")
cargar()
r <- avisos_cliente_datos(CID, AHORA)
ok("con todo legible: los datos salen, con la fecha del snapshot", r$ok && is.na(r$motivo) &&
     abs(as.numeric(difftime(r$sap_fecha, AHORA - 2 * 86400, units = "secs"))) < 1)
ok("las filas son las del Calendario: sin la IC (por nombre) ni el pago confirmado", {
  docs <- r$df$Documento
  all(c("A1", "A2", "A3", "P1") %in% docs) && !"A4" %in% docs && !"P2" %in% docs
})
ok("una llave que no existe (manual, abonos...) no es falla: su tabla va vacía",
   !exists(llave(S3_KEYS$manual), envir = .mem) && r$ok)
ok("el job no escribió nada (ni empresas.rds, que load_empresas rellenaría)",
   .mem_escrituras == 0L && .mem_subidas == 0L)
ok("al terminar suelta lo precargado del cliente",
   !any(startsWith(ls(.s3_preload_cache), paste0(CID, "/"))))
ok("el mapa de empresas sale de la tabla leída: iniciales -> nombre",
   identical(r$shared$company_map()[["NL"]], "NL Transportes"))

cat("\n-- clientes que se omiten --\n")
ok("una llave con 403 (sin permiso de lectura): omitido_lectura, nunca tabla vacía", {
  cargar(); .mem_prohibido <<- llave(S3_KEYS$bancos_confirmados)
  x <- avisos_cliente_datos(CID, AHORA); !x$ok && x$motivo == "omitido_lectura" && is.null(x$df)
})
ok("una lectura que falla: omitido_lectura", {
  cargar(); pon(S3_KEYS$moves, tibble::tibble()); .mem_falla <<- llave(S3_KEYS$moves)
  x <- avisos_cliente_datos(CID, AHORA); !x$ok && x$motivo == "omitido_lectura"
})
ok("falta el snapshot de CxP: omitido_sin_sap", {
  cargar(guardado_ap = NULL); x <- avisos_cliente_datos(CID, AHORA); !x$ok && x$motivo == "omitido_sin_sap"
})
ok("un snapshot de hace más de 3 días: omitido_sap_viejo (cuenta el más viejo)", {
  cargar(guardado_ap = AHORA - 3.2 * 86400); x <- avisos_cliente_datos(CID, AHORA)
  !x$ok && x$motivo == "omitido_sap_viejo"
})
ok("de 2.9 días todavía sale (el fin de semana)", {
  cargar(guardado_ar = AHORA - 2.9 * 86400); avisos_cliente_datos(CID, AHORA)$ok
})
ok("si la tubería truena (su warning), el cliente se omite: omitido_calculo, nunca cobros en $0", {
  cargar(); tipo <- SAP_B1_ERP_TYPE; rm(SAP_B1_ERP_TYPE, envir = globalenv())
  x <- tryCatch(avisos_cliente_datos(CID, AHORA), finally = assign("SAP_B1_ERP_TYPE", tipo, envir = globalenv()))
  !x$ok && x$motivo == "omitido_calculo" && is.null(x$df)
})
ok("otros warnings y mensajes no se imprimen ni omiten al cliente", {
  sh <- list(company_map = function() c(NL = "NL Transportes"))
  salida <- utils::capture.output(type = "message", {
    df <- with_mocked <- NULL
    f <- build_export_combined_df
    assign("build_export_combined_df", function(shared, ic) { message("[IC] cliente acme"); warning("otra cosa"); ar },
           envir = globalenv())
    df <- tryCatch(avisos_ledger(sh), finally = assign("build_export_combined_df", f, envir = globalenv()))
  })
  !length(salida) && is.data.frame(df) && nrow(df) == nrow(ar)
})

cat("\n-- números del lunes --\n")
cargar()
pon(S3_KEYS$pasivos_provisions, tibble::tibble(
  id = c("pv1", "pv2"), estado = "provisional", moneda_pago = "MXN", amount_pago = c(45000, 9000),
  fecha_efectiva = as.Date(c("2026-11-20", "2026-11-27"))))
r <- avisos_cliente_datos(CID, AHORA)
serie <- tibble::tibble(semana = umbral_semana(c(LUNES - 7, LUNES, LUNES)), lunes = c(LUNES - 7, LUNES, LUNES),
                        empresa = c("NL", "NL", "NG"), moneda = "MXN",
                        saldo_abierto = c(1000, 1000, 1000), saldo_vencido = c(100, 150, 50), facturas = 20L)
alertas <- tibble::tibble(semana = "2026-W47", empresa = "NL", moneda = "MXN", pct = 15, delta = 5,
                          umbral = 1, direccion = "sube")
d <- correo_datos_lunes(r$df, r$shared, LUNES, serie, alertas, r$sap_fecha, url = "https://x")
ok("cobros por moneda, solo de la semana y sin IC; nunca sumados entre monedas", {
  d$cobros[["MXN"]] == 400 + 10 + 20 + 30 + 35 + 45 + 50 && d$cobros[["USD"]] == 40 && length(d$cobros) == 2
})
ok("pagos: los de SAP sin el confirmado, más la provisión de la semana (como el Calendario)",
   identical(names(d$pagos), "MXN") && d$pagos[["MXN"]] == 300 + 45000 &&
     !"Proveedor Dos" %in% d$top_pagos$contraparte)
ok("una fila sin contraparte (provisión manual) no dice NA en el correo", {
  h <- correo_lunes_html(d, list(idioma = "es", ver_cxc = TRUE))
  d$top_pagos$contraparte[1] == "—" && !grepl(">NA<", h, fixed = TRUE)
})
ok("top 5 de cobros: el mayor primero, con las iniciales de la empresa", {
  nrow(d$top_cobros) == 5 && d$top_cobros$contraparte[1] == "Cliente Uno" && all(d$top_cobros$empresa == "NL") &&
    !"Cliente Tres" %in% d$top_cobros$contraparte
})
ok("pasivos de la semana con la función de B (panorama_pasivos_pronto), no una copia",
   d$pasivos_n == 1L && d$pasivos[["MXN"]] == 45000 &&
     any(grepl("panorama_pasivos_pronto", deparse(body(correo_datos_lunes)), fixed = TRUE)))
ok("vencidos CxC del cliente en moneda base, con su Δ desde el lunes anterior", {
  abs(d$vencidos_pct - 10) < 1e-9 && abs(d$vencidos_delta - 0) < 1e-9
})
ok("sin el lunes anterior, el Δ es NA (sin flecha)",
   is.na(correo_vencidos(serie[serie$lunes == LUNES, ], LUNES)$delta))
ok("la caja Para revisión sale de las alertas, en el idioma de quien lo recibe", {
  h  <- correo_lunes_html(d, list(idioma = "es", ver_cxc = TRUE))
  he <- correo_lunes_html(d, list(idioma = "en", ver_cxc = TRUE))
  grepl("Los vencidos de CxC de NL (MXN) subieron 5.0 pts", h, fixed = TRUE) &&
    !grepl("subieron", he, fixed = TRUE) && grepl(correo_revision(alertas, "en"), he, fixed = TRUE)
})
ok("un monto aproximado (provisión en USD al FIX) va con ≈, en el mosaico y en la tabla", {
  df2 <- r$df; df2$aprox <- df2$Documento == "P1"
  d2 <- correo_datos_lunes(df2, r$shared, LUNES, serie, alertas, r$sap_fecha)
  h <- correo_lunes_html(d2, list(idioma = "es", ver_cxc = TRUE))
  # the MXN total holds it (≈ $45,300.00); in the table only its row (≈ $300.00), not the exact provision
  identical(d2$pagos_aprox, "MXN") && !length(d2$cobros_aprox) &&
    grepl("&#8776; $45,300.00", h, fixed = TRUE) && grepl("&#8776; $300.00", h, fixed = TRUE) &&
    !grepl("&#8776; $45,000.00", h, fixed = TRUE)
})

cat("
-- comunicados por destinatario --
")
cargar()
usuarios <- data.frame(username = c("ti", "jefa"), tier = c(nivel_ti(), "admin"),
                       email = c("ti@x.mx", "jefa@x.mx"), permisos = "{}", stringsAsFactors = FALSE)
dest <- correo_destinatarios(usuarios)
VENCE <- Sys.time() + 7 * 86400
avisos_agregar(CID, list(tipo = "publicado", titulo = "Solo para dev", niveles = nivel_ti(), vence = VENCE), por = "x")
avisos_agregar(CID, list(tipo = "publicado", titulo = "Para todos", niveles = niveles_cliente(), vence = VENCE), por = "x")
avisos_agregar(CID, list(tipo = "personal", clase = "solicitud", titulo = "Firma la requisición",
                         usuarios = "jefa", vence = VENCE), por = "x")
av <- avisos_leer(CID)
tam <- function(u) vapply(correo_tambien(av, dest[dest$usuario == u, ], CID), function(x) paste(x$tipo, x$texto), "")
ok("cada correo lleva solo los avisos que su destinatario ve: el de Dev no le llega a Admin", {
  ti <- tam("ti"); jefa <- tam("jefa")
  "comunicado Solo para dev" %in% ti && !"comunicado Solo para dev" %in% jefa &&
    "comunicado Para todos" %in% jefa && "comunicado Para todos" %in% ti
})
ok("la solicitud personal va solo a quien se le pidió", {
  "solicitud Firma la requisición" %in% tam("jefa") && !any(grepl("Firma", tam("ti")))
})
ok("los destinatarios traen sus permisos resueltos (para avisos_visibles)",
   is.list(dest$permisos) && isTRUE(dest$permisos[[1]][["can_receive_weekly_email"]]))

cat("
-- semana.rds y viernes --
")
VIERNES <- LUNES + 4L
cargar()
pon(S3_KEYS$pasivos_provisions, tibble::tibble(id = "pv1", estado = "provisional", moneda_pago = "MXN",
                                               amount_pago = 45000, fecha_efectiva = as.Date("2026-11-20")))
r <- avisos_cliente_datos(CID, AHORA)
dl <- correo_datos_lunes(r$df, r$shared, LUNES, serie, alertas, r$sap_fecha)
correo_semana_guardar(CID, r$df, dl)
ok("el lunes guarda sus filas de la semana en <c>/avisos/semana.rds, y solo ahí escribe", {
  x <- get(llave("avisos/semana.rds"), envir = .mem)
  identical(ls(.mem)[!ls(.mem) %in% llave(c(.avisos_llaves_ledger(), "avisos/avisos.rds"))], llave("avisos/semana.rds")) &&
    x$lunes == LUNES && nrow(x$filas) == 10 && "provision" %in% x$filas$source && abs(x$vencidos_pct - 10) < 1e-9
})
ok("si semana.rds no se puede leer, el lunes no la pisa", {
  .mem_prohibido <<- llave("avisos/semana.rds"); n0 <- .mem_escrituras
  f <- inherits(tryCatch(correo_semana_guardar(CID, r$df, dl), error = function(e) e), "error")
  .mem_prohibido <<- character(); f && .mem_escrituras == n0
})
# By Friday: P1 paid and A1 (400 of the 590 MXN expected) collected.
b <- get(llave(S3_KEYS$bancos_confirmados), envir = .mem)
pon(S3_KEYS$bancos_confirmados, dplyr::bind_rows(b, tibble::tibble(
  empresa = "NL Transportes", documento = c("P1", "A1"), moneda = "MXN", tipo = c("pago", "cobro"),
  eliminado = FALSE, importe = c(300, 400))))
VIE_AHORA <- AHORA + 4 * 86400
pon(S3_KEYS_CRITICAL$sap_snap_ar, list(data = ar, saved_at = VIE_AHORA - 3600))
pon(S3_KEYS_CRITICAL$sap_snap_ap, list(data = ap, saved_at = VIE_AHORA - 3600))
sl <- correo_semana_leer(CID, VIERNES)
rv <- avisos_cliente_datos(CID, VIE_AHORA)
dv <- correo_datos_viernes(sl$semana, rv$df, rv$shared, VIERNES, rv$sap_fecha)
ok("una fila que la regla quita en vez de marcar (manual confirmada) cuenta como confirmada", {
  f <- compute_confirmed_flags
  assign("compute_confirmed_flags", function(df, ledger, b, p) { df$confirmed <- FALSE; df[-1, , drop = FALSE] },
         envir = globalenv())
  x <- tryCatch(.avisos_confirmadas(data.frame(Documento = c("M1", "S1")), "AP", NULL, NULL),
                finally = assign("compute_confirmed_flags", f, envir = globalenv()))
  identical(x, c(TRUE, FALSE))
})
ok("el viernes lee la semana del lunes", sl$ok && is.na(sl$motivo))
ok("pagos confirmados contra los del lunes, por la regla de la app; sin provisiones", {
  dv$pagos_conf == 1L && dv$pagos_total == 1L
})
ok("cobros recibidos: % de lo que el lunes esperaba en moneda base (400 de 590)",
   abs(dv$cobros_pct - 100 * 400 / 590) < 1e-9)
ok("vencidos del viernes contra los del lunes", !is.na(dv$vencidos_pct) && !is.na(dv$vencidos_delta) &&
     abs(dv$vencidos_delta - (dv$vencidos_pct - 10)) < 1e-9)
ok("el correo del viernes con esos datos no lleva montos", {
  h <- correo_viernes_html(dv, list(idioma = "es", ver_cxc = TRUE))
  !grepl("$", h, fixed = TRUE) && grepl("100%", h, fixed = TRUE)
})
ok("sin la semana de este lunes: omitido_sin_lunes; ilegible: omitido_lectura", {
  a <- correo_semana_leer(CID, VIERNES + 7L)
  .mem_prohibido <<- llave("avisos/semana.rds"); b2 <- correo_semana_leer(CID, VIERNES); .mem_prohibido <<- character()
  !a$ok && a$motivo == "omitido_sin_lunes" && !b2$ok && b2$motivo == "omitido_lectura"
})

s3_memoria_restaurar()
cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
