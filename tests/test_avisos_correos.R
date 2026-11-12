# =============================================================================
# tests/test_avisos_correos.R
# Frente D: who gets the weekly emails (R/avisos_correos.R) and the HTML of
# the Monday and Friday emails, from invented data. Pure functions: no S3,
# no network, nothing is sent.
#
# Run from project root:  Rscript tests/test_avisos_correos.R
# =============================================================================
cat("=== test_avisos_correos ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/i18n.R")
source("R/tier_registry.R")
source("R/permisos.R")
source("R/avisos_umbrales.R")
source("R/avisos_correos.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

AP_TIER <- Filter(function(t) identical(nivel_calendario(t), "AP"), niveles_cliente())[1]
STAFF   <- niveles_staff()[1]
on      <- function() '{"can_receive_weekly_email": true}'
off     <- function() '{"can_receive_weekly_email": false}'
usuarios <- data.frame(
  username = c("ti", "jefa", "fin", "fin_si", "compra_si", "borrado", "inactivo", "sin_correo", "staff", "jefa_no", "apagado", "ingles"),
  tier     = c(nivel_ti(), "admin", "finance", "finance", AP_TIER, "admin", "admin", "admin", STAFF, "admin", "admin", "admin"),
  email    = c("ti@x.mx", "Jefa@X.mx ", "fin@x.mx", "fin2@x.mx", "c@x.mx", "b@x.mx", "i@x.mx", "no-es-correo", "s@x.mx",
               "jn@x.mx", "a@x.mx", "en@x.mx"),
  permisos = c("{}", "{}", "{}", on(), on(), "{}", "{}", "{}", on(), off(), "{}", "{}"),
  deleted  = c(rep(FALSE, 5), TRUE, rep(FALSE, 6)),
  activo   = c(rep(TRUE, 6), FALSE, rep(TRUE, 5)),
  stringsAsFactors = FALSE)
prefs <- data.frame(usuario = c("apagado", "ingles", "fin_si"),
                    datos = c('{"correos_semana": false}', '{"idioma": "en"}', '{"idioma": "fr"}'),
                    stringsAsFactors = FALSE)

cat("-- destinatarios --\n")
d <- correo_destinatarios(usuarios, NULL, prefs)
ok("dev y admin lo reciben por defecto; finanzas solo con el permiso encendido",
   setequal(d$usuario, c("ti", "jefa", "fin_si", "compra_si", "ingles")))
ok("nunca: borrados, inactivos, sin correo válido, staff, permiso apagado ni preferencia apagada",
   !any(c("borrado", "inactivo", "sin_correo", "staff", "jefa_no", "apagado", "fin") %in% d$usuario))
ok("el correo se normaliza (minúsculas, sin espacios)", d$email[d$usuario == "jefa"] == "jefa@x.mx")
ok("idioma de la preferencia; uno desconocido o ausente es español",
   d$idioma[d$usuario == "ingles"] == "en" && d$idioma[d$usuario == "fin_si"] == "es" && d$idioma[d$usuario == "jefa"] == "es")
ok("Compras (calendario AP) va sin CxC; los demás con CxC",
   !d$ver_cxc[d$usuario == "compra_si"] && all(d$ver_cxc[d$usuario != "compra_si"]))
ok("la configuración de niveles del cliente cuenta: si apaga admin, admin ya no lo recibe", {
  d2 <- correo_destinatarios(usuarios, list(admin = list(can_receive_weekly_email = FALSE)), prefs)
  !"jefa" %in% d2$usuario && "ti" %in% d2$usuario
})
ok("sin usuarios, cero destinatarios", nrow(correo_destinatarios(NULL)) == 0)

cat("\n-- lunes --\n")
lunes <- list(
  lunes = as.Date("2026-11-16"),
  cobros = c(MXN = 1120000, USD = 40000), pagos = c(MXN = 842300),
  pasivos_n = 2L, pasivos = c(MXN = 96500),
  vencidos_pct = 12.4, vencidos_delta = 1.8,
  revision = "Los vencidos de CxC de NL (MXN) subieron 1.8 pts",
  top_cobros = data.frame(contraparte = c("Cliente <script>alert(1)</script>", "Cliente 2"), empresa = c("NL", "NTS"),
                          fecha = as.Date(c("2026-11-17", "2026-11-18")), monto = c(420000, 310000),
                          moneda = c("MXN", "USD"), stringsAsFactors = FALSE),
  top_pagos = data.frame(contraparte = "Proveedor 1", empresa = "NL", fecha = as.Date("2026-11-18"),
                         monto = 312000, moneda = "MXN", stringsAsFactors = FALSE),
  tambien = list(list(tipo = "pasivos", texto = "Arrendamiento oficinas $45,000.00 (viernes 20)"),
                 list(tipo = "comunicado", texto = "Nuevo proceso para alta de proveedores")),
  datos_sap = as.POSIXct("2026-11-14 00:40:00", tz = "UTC"),
  url = "https://hopdesk.example/app")
fin <- list(idioma = "es", ver_cxc = TRUE)
com <- list(idioma = "es", ver_cxc = FALSE)
eng <- list(idioma = "en", ver_cxc = TRUE)
h  <- correo_lunes_html(lunes, fin)
hc <- correo_lunes_html(lunes, com)
he <- correo_lunes_html(lunes, eng)
limpio <- function(x) !grepl("avisos\\.correo\\.|\\{[a-z_0-9]+\\}", x)
ok("asunto con el rango de la semana", correo_lunes_asunto(lunes, "es") == "Panorama de la semana · 16 al 20 de noviembre")
ok("trae lo del diseño: encabezado, 4 mosaicos, revisión, 2 tablas, también, botón y pie", all(vapply(c(
  "PANORAMA DE LA SEMANA", "Lunes 16 de noviembre", "Cobros superan a pagos <i>esta semana</i>", "COBROS ESPERADOS",
  "$1,120,000.00", "$40,000.00 USD", "PAGOS PROGRAMADOS", "$842,300.00", "2 · $96,500.00", "VENCIDOS CXC", "12.4%",
  "&#9650; 1.8 pts", "Para revisión:", "Los 5 cobros más grandes", "Los 5 pagos más grandes", "Mar 17", "También esta semana",
  "Pasivos:", "Abrir Panorama", "https://hopdesk.example/app", "Datos de SAP al viernes 13, 18:40",
  "Puedes dejar de recibirlo en Configuración"), function(s) grepl(s, h, fixed = TRUE), logical(1))))
ok("sin claves crudas ni marcadores sin llenar (es y en)", limpio(h) && limpio(hc) && limpio(he))
ok("el texto de los datos va escapado: una contraparte no mete HTML",
   !grepl("<script>", h, fixed = TRUE) && grepl("&lt;script&gt;", h, fixed = TRUE))
ok("solo tablas y estilos en línea: sin <style>, flex, grid ni SVG",
   !grepl("<style|display:\\s*flex|display:\\s*grid|<svg", h))
ok("Compras: sin cobros, sin vencidos CxC y sin la caja de revisión; con pagos", {
  !grepl("COBROS ESPERADOS|VENCIDOS CXC|Para revisión|Los 5 cobros", hc) &&
    grepl("PAGOS PROGRAMADOS", hc, fixed = TRUE) && grepl("Tus pagos", hc, fixed = TRUE)
})
ok("en inglés, en inglés", grepl("THE WEEK AHEAD", he, fixed = TRUE) && grepl("Monday, November 16", he, fixed = TRUE) &&
     grepl("Open Panorama", he, fixed = TRUE) && !grepl("Abrir Panorama", he, fixed = TRUE))
ok("si los pagos superan a los cobros, el título lo dice",
   grepl("Pagos superan a cobros", correo_lunes_html(modifyList(lunes, list(pagos = c(MXN = 5e6))), fin), fixed = TRUE))
ok("una semana que cruza de mes", correo_rango(as.Date("2026-11-30"), as.Date("2026-12-04"), "es") == "30 de noviembre al 4 de diciembre")
ok("sin historia de vencidos, el mosaico va sin flecha",
   !grepl("&#9650;|&#9660;", correo_lunes_html(modifyList(lunes, list(vencidos_delta = NA)), fin)))

cat("\n-- viernes --\n")
viernes <- list(viernes = as.Date("2026-11-20"), pagos_conf = 23L, pagos_total = 25L, cobros_pct = 81,
                vencidos_pct = 11.2, vencidos_delta = -1.2,
                logros = c("Logro uno", "Logro dos", "Logro tres"),
                radar = c("Radar uno", "Radar dos", "Radar tres", "Radar cuatro"),
                datos_sap = as.POSIXct("2026-11-20 22:10:00", tz = "UTC"), url = "https://hopdesk.example/app")
v  <- correo_viernes_html(viernes, fin)
vc <- correo_viernes_html(viernes, com)
ve <- correo_viernes_html(viernes, eng)
ok("asunto y título", correo_viernes_asunto(viernes, "es") == "Cierre de semana · 16 al 20 de noviembre" &&
     grepl("Se confirmó el 92% <i>de los pagos</i>", v, fixed = TRUE))
ok("3 mosaicos: pagos 92% (23 de 25), cobros 81%, vencidos 11.2% con ▼ en verde", all(vapply(c(
  "PAGOS CONFIRMADOS", "92%", "23 de 25", "COBROS RECIBIDOS", "81%", "de lo esperado", "11.2%", "&#9660; 1.2 pts en la semana",
  "#2F6B4F"), function(s) grepl(s, v, fixed = TRUE), logical(1))))
ok("ningún monto: el viernes no lleva $", !grepl("$", v, fixed = TRUE) && !grepl("$", vc, fixed = TRUE))
ok("hasta 2 logros y 3 renglones de En el radar", grepl("Logro dos", v) && !grepl("Logro tres", v) &&
     grepl("Radar tres", v) && !grepl("Radar cuatro", v) && grepl("En el radar", v, fixed = TRUE) &&
     grepl("LOGROS DE LA SEMANA", v, fixed = TRUE))
ok("sin logros ni radar, esas secciones no salen", {
  x <- correo_viernes_html(modifyList(viernes, list(logros = character(), radar = character())), fin)
  !grepl("LOGROS DE LA SEMANA|En el radar", x)
})
ok("Compras: sin cobros ni vencidos CxC", !grepl("COBROS RECIBIDOS|VENCIDOS CXC", vc) && grepl("PAGOS CONFIRMADOS", vc, fixed = TRUE))
ok("sin pagos programados, otro título y sin mosaico de pagos", {
  x <- correo_viernes_html(modifyList(viernes, list(pagos_total = 0L, pagos_conf = 0L)), fin)
  grepl("sin pagos programados", x, fixed = TRUE) && !grepl("PAGOS CONFIRMADOS", x, fixed = TRUE)
})
ok("en inglés, sin claves crudas", limpio(v) && limpio(vc) && limpio(ve) && grepl("WEEK IN REVIEW", ve, fixed = TRUE) &&
     grepl("On the radar", ve, fixed = TRUE))

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
