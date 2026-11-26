# =============================================================================
# tests/test_t116_f2_mercado.R
# T116 F2: the market countries and the staff's official calendar
# (R/mercado.R), and how get_holidays() takes it. In-memory S3; invented data.
#   1  MERCADO_PAISES: valid codes, the country of a currency, a client's
#      countries (empty = its base currency's; a USD client never gets MX)
#   2  TARGET2 (EU) 2026 and 2027
#   3  the official calendar in get_holidays(): nothing loaded = the rule; a
#      year half captured changes nothing; a CONFIRMED year replaces the
#      rule's dates for that year only; voiding the confirmation = the rule
#      again; the client's overrides still last
#   4  the refresh: a failed read keeps what was there and says "falla" once;
#      within the hour "cache"; silent (the job's log is public)
#   5  the writers on S3: add, the same day twice refused, confirm empty
#      refused, confirm, void; nothing is ever removed
#   6  group_config$paises: the schema's default and the CM36 rebase (two
#      sessions, one changes the countries, the other the base currency)
#   7  the screen's comparison: coincide / solo_oficial / solo_regla
#
# Run from project root:  Rscript tests/test_t116_f2_mercado.R
# =============================================================================
cat("=== test_t116_f2_mercado ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr, detalle = NULL) {
  r <- tryCatch(isTRUE(expr), error = function(e) { message("  error: ", conditionMessage(e)); FALSE })
  if (r) { .pass <<- .pass + 1L; cat(" PASS:", label, "\n") }
  else   { .fail <<- .fail + 1L; cat(" FAIL:", label, "\n"); if (!is.null(detalle)) cat("       ", detalle, "\n") }
}
source("tests/helpers/s3_memoria.R")
for (f in c("R/i18n.R", "R/monedas.R", "R/policy_engine.R", "R/mercado.R"))
  suppressMessages(sys.source(f, envir = globalenv()))
q <- function(expr) invisible(suppressWarnings(suppressMessages(expr)))
err <- function(expr) tryCatch({ q(expr); NULL }, error = function(e) e)
D <- function(...) as.Date(c(...))

cat("== 1. El registro ==\n")
ok("1.1 códigos válidos, con minúsculas y espacios; FR no es país de cliente",
   identical(mercado_pais_valido(c("MX", "us ", "eu", "FR", NA, "")), c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)))
ok("1.2 el país de cada moneda", identical(vapply(c("MXN", "usd", "EUR", "GBP"), mercado_pais_de_moneda, ""),
                                           c(MXN = "MX", usd = "US", EUR = "EU", GBP = NA_character_)))
ok("1.3 sin países elegidos: el de la moneda base (MXN -> MX, USD -> US)",
   identical(mercado_paises_cliente(list(moneda_base = "MXN", paises = character())), "MX") &&
     identical(mercado_paises_cliente(list(moneda_base = "USD")), "US"))
ok("1.4 un cliente con base USD sin países NO recibe México", !"MX" %in% mercado_paises_cliente(list(moneda_base = "USD")))
ok("1.5 sin moneda base: la de omisión", identical(mercado_paises_cliente(list(moneda_base = NA_character_)),
                                                   mercado_pais_de_moneda(MONEDA_BASE_OMISION)))
ok("1.6 elegidos: en el orden del registro, sin códigos inválidos", identical(mercado_paises_cliente(list(paises = c("eu", "ZZ", "MX"))), c("MX", "EU")))
ok("1.7 una moneda sin país (GBP) sin países elegidos: ninguno", identical(mercado_paises_cliente(list(moneda_base = "GBP")), character()))

cat("== 2. TARGET2 ==\n")
ok("2.1 EU 2026: 1 ene, Viernes Santo 3 abr, Lunes de Pascua 6 abr, 1 may, 25 y 26 dic",
   identical(get_holidays("EU", 2026), D("2026-01-01", "2026-04-03", "2026-04-06", "2026-05-01", "2026-12-25", "2026-12-26")))
ok("2.2 EU 2027: Pascua el 28 mar", identical(get_holidays("EU", 2027),
   D("2027-01-01", "2027-03-26", "2027-03-29", "2027-05-01", "2027-12-25", "2027-12-26")))
ok("2.3 cada día de la regla trae su nombre del diccionario (MX, US, EU)",
   all(vapply(c("MX", "US", "EU"), function(p) { r <- .festivos_regla(p, 2030)
     all(!is.na(r$clave)) && all(vapply(r$clave, function(k) !identical(tr(k), k), logical(1))) }, logical(1))))

cat("== 3. El calendario oficial en get_holidays() ==\n")
fila <- function(tipo, fecha, anio, motivo = "Ficticio", pais = "MX", anulado = FALSE) tibble::tibble(
  id = paste0("r", sample.int(1e6, 1)), tipo = tipo, pais = pais, fecha = as.Date(fecha), anio = as.integer(anio),
  motivo = motivo, fuente = "prueba", capturado_por = "staff.ficticio", capturado_en = Sys.time(),
  anulado_por = if (anulado) "staff.ficticio" else NA_character_, anulado_en = if (anulado) Sys.time() else as.POSIXct(NA))
regla27 <- get_holidays("MX", 2027)
mercado_inhabiles_poner(NULL)
ok("3.1 sin nada cargado: la regla", identical(get_holidays("MX", 2027), regla27))
oficial <- dplyr::bind_rows(fila("dia", "2027-01-01", 2027), fila("dia", "2027-10-15", 2027, "Día ficticio"))
mercado_inhabiles_poner(oficial)
ok("3.2 un año a medias (sin confirmar): no cambia nada", identical(get_holidays("MX", 2027), regla27))
conf <- fila("anio", NA, 2027)
mercado_inhabiles_poner(dplyr::bind_rows(oficial, conf))
ok("3.3 año confirmado: la lista oficial reemplaza a la regla en ese año",
   identical(get_holidays("MX", 2027), D("2027-01-01", "2027-10-15")))
ok("3.4 ...solo en ese año y ese país", identical(get_holidays("MX", 2026), {mercado_inhabiles_poner(NULL); x <- get_holidays("MX", 2026); x}))
mercado_inhabiles_poner(dplyr::bind_rows(oficial, conf))
ok("3.5 ...y no en EU", identical(get_holidays("EU", 2027), D("2027-01-01", "2027-03-26", "2027-03-29", "2027-05-01", "2027-12-25", "2027-12-26")))
ov <- tibble::tibble(id = "o1", country = "MX", date = D("2027-10-15"), action = "remove", description = "x",
                     created_by = "x", created_at = Sys.time())
ok("3.6 los ajustes del cliente siguen al final", identical(get_holidays("MX", 2027, overrides = ov), D("2027-01-01")))
mercado_inhabiles_poner(dplyr::bind_rows(oficial, fila("anio", NA, 2027, anulado = TRUE)))
ok("3.7 confirmación anulada: la regla otra vez", identical(get_holidays("MX", 2027), regla27))
mercado_inhabiles_poner(dplyr::bind_rows(oficial, conf))
ok("3.8 el motivo oficial de un día; NA si no está capturado",
   identical(mercado_inhabil_motivo("MX", D("2027-10-15")), "Día ficticio") && is.na(mercado_inhabil_motivo("MX", D("2027-11-02"))))

cat("== 4. El refresco ==\n")
.MERCADO_INHABILES$intento_en <- NULL
salida <- utils::capture.output(r1 <- mercado_inhabiles_refrescar(forzar = TRUE, leer = function() {
  cat("ruido de aws.s3\n"); message("Client error: (403)"); stop("Forbidden") }), type = "output")
salida_msg <- utils::capture.output(r1b <- mercado_inhabiles_refrescar(forzar = TRUE, leer = function() {
  message("Client error: (403)"); stop("Forbidden") }), type = "message")
ok("4.1 una lectura fallida dice «falla», sin imprimir nada", identical(r1, "falla") && identical(r1b, "falla") &&
     !length(salida) && !length(salida_msg))
ok("4.2 ...y deja lo que ya había en memoria", identical(nrow(mercado_inhabiles_tabla()), 3L))
ok("4.3 dentro de la hora: «cache», sin leer", identical(mercado_inhabiles_refrescar(leer = function() stop("no debía leer")), "cache"))
ok("4.4 una lectura buena reemplaza la tabla", identical(mercado_inhabiles_refrescar(forzar = TRUE, leer = function() oficial), "ok") &&
     nrow(mercado_inhabiles_tabla()) == 2L)
s3_memoria_reset()
.MERCADO_INHABILES$intento_en <- NULL
ok("4.5 el archivo aún no existe: «ok» con la tabla vacía (la regla)",
   identical(q(mercado_inhabiles_refrescar()), "ok") && nrow(mercado_inhabiles_tabla()) == 0L &&
     identical(get_holidays("MX", 2027), regla27))

cat("== 5. Los escritores, sobre S3 ==\n")
llave <- grep("mercado/inhabiles", ls(.mem), value = TRUE)
q(modificar_mercado_agregar_dia("mx", "2027-11-02", "Día de Muertos", "CNBV prueba", por = "staff.ficticio"))
llave <- grep("mercado/inhabiles", ls(.mem), value = TRUE)
ok("5.1 queda en hd-admin/mercado/inhabiles.rds, también en memoria",
   identical(llave, "hd-admin/mercado/inhabiles.rds") && nrow(.mem[[llave]]) == 1L && nrow(mercado_inhabiles_tabla()) == 1L)
e <- err(modificar_mercado_agregar_dia("MX", "2027-11-02", "Otra vez", "", por = "staff.ficticio"))
ok("5.2 el mismo día dos veces: rechazado con su motivo", inherits(e, "mercado_error") && identical(e$clave, "tablas.ajustes.mercado.error_repetido"))
ok("5.3 fecha absurda, motivo vacío, país desconocido: rechazados",
   identical(err(modificar_mercado_agregar_dia("MX", "9999-01-01", "x", "", "s"))$clave, "tablas.ajustes.mercado.error_fecha") &&
     identical(err(modificar_mercado_agregar_dia("MX", "2027-12-12", "  ", "", "s"))$clave, "tablas.ajustes.mercado.error_motivo") &&
     identical(err(modificar_mercado_agregar_dia("FR", "2027-12-12", "x", "", "s"))$clave, "tablas.ajustes.mercado.error_pais"))
ok("5.4 confirmar un año sin días: rechazado",
   identical(err(modificar_mercado_confirmar_anio("MX", 2028, "", "s"))$clave, "tablas.ajustes.mercado.error_anio_vacio"))
q(modificar_mercado_confirmar_anio("MX", 2027, "CNBV prueba", por = "staff.ficticio"))
ok("5.5 confirmado: get_holidays(MX, 2027) es la lista oficial", identical(get_holidays("MX", 2027), D("2027-11-02")))
ok("5.6 confirmar dos veces: rechazado", identical(err(modificar_mercado_confirmar_anio("MX", 2027, "", "s"))$clave,
                                                   "tablas.ajustes.mercado.error_ya_confirmado"))
id_conf <- mercado_anio_confirmado("MX", 2027)$id
q(modificar_mercado_anular(id_conf, por = "staff.ficticio"))
ok("5.7 anular la confirmación: la regla otra vez, y la fila sigue (anulada)",
   identical(get_holidays("MX", 2027), regla27) && nrow(.mem[[llave]]) == 2L && !is.na(.mem[[llave]]$anulado_en[.mem[[llave]]$id == id_conf]))
ok("5.8 anular lo ya anulado: rechazado", identical(err(modificar_mercado_anular(id_conf, "s"))$clave, "tablas.ajustes.mercado.error_no_existe"))
ok("5.9 la tabla está en la red de «solo crece»", "mercado/inhabiles.rds" %in% .S3_SOLO_CRECEN())

cat("== 6. group_config$paises ==\n")
ok("6.1 el esquema trae paises vacío", identical(.schema_group_config()$paises, character(0)))
viejo <- list(group_name = "Grupo Ficticio", moneda_base = "MXN")
ok("6.2 un archivo viejo sin paises se completa vacío", identical(.group_config_completar(viejo)$paises, character(0)))
base <- .group_config_completar(viejo)
a <- base; a$paises <- c("MX", "US")                       # session A: the countries
fresco <- base; fresco$moneda_base <- "USD"                # session B saved first: the base
r6 <- .group_config_rebase(a, base, fresco)
ok("6.3 rebase: los países de una sesión y la moneda base de la otra sobreviven",
   identical(r6$paises, c("MX", "US")) && identical(r6$moneda_base, "USD"))
ok("6.4 la diferencia del evento nombra paises", "paises" %in% names(.group_config_diferencia(base, r6)))

cat("== 7. La comparación de la pantalla ==\n")
mercado_inhabiles_poner(dplyr::bind_rows(fila("dia", "2027-11-02", 2027, "Día de Muertos"), fila("dia", "2027-10-15", 2027, "Día ficticio")))
cmp <- mercado_comparar("MX", 2027)
ok("7.1 coincide / solo_oficial / solo_regla, con su motivo",
   identical(cmp$estado[cmp$fecha == D("2027-11-02")], "coincide") &&
     identical(cmp$estado[cmp$fecha == D("2027-10-15")], "solo_oficial") &&
     identical(cmp$estado[cmp$fecha == D("2027-12-25")], "solo_regla") &&
     identical(cmp$motivo[cmp$fecha == D("2027-12-25")], tr("comun.mercado.inhabil.navidad")))
ok("7.2 solo las filas capturadas llevan fila_id (lo único que se puede anular)",
   all(is.na(cmp$fila_id[cmp$estado == "solo_regla"])) && all(!is.na(cmp$fila_id[cmp$estado != "solo_regla"])))

mercado_inhabiles_poner(NULL)
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
