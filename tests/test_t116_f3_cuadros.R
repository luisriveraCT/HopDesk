# =============================================================================
# tests/test_t116_f3_cuadros.R
# T116 F3: Panorama's «Tipos» and «Tasas» (R/mercado_datos.R). In-memory S3;
# the sources are a fake server on 127.0.0.1 (tests/helpers/mercado_falso.R).
#   1  «Tipos» by base: MXN (Banxico: FIX and its currencies), USD (FIX and
#      ECB), EUR (all from the ECB), GBP (crossed); empty without markets
#   2  «Sin dato reciente»: by business days of the figure's own calendar (a
#      holiday in between counts as not working); CETES 28 is weekly
#   3  «Tasas»: each market's rates, their labels from the dictionary
#   4  the card: date and source; an old figure is never shown; the New York
#      Fed's attribution and the ECB's note when a figure comes from them
#   5  reading never asks a source; the 10-minute memo
#   6  the refresh (in line): fills the caches from the fake server; once an
#      hour; only new rows, an unchanged cache is not written; all down:
#      nothing written, "falla"
#   7  the refresh in the background (the app's way): returns at once, a
#      child fetches, the caches are written when it ends
#   8  Panorama only reads: no source, no refresh in panorama_module.R
#   9  the T74 lessons: the child's errors reach the app's log (never its
#      text, a URL or the token); absolute paths, any working directory;
#      a hung child killed; the token by environment, never in its command
#      line (7.2, 7.5); the child never touches S3
#
# Run from project root:  Rscript tests/test_t116_f3_cuadros.R
# =============================================================================
cat("=== test_t116_f3_cuadros ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr, detalle = NULL) {
  r <- tryCatch(isTRUE(expr), error = function(e) { message("  error: ", conditionMessage(e)); FALSE })
  if (r) { .pass <<- .pass + 1L; cat(" PASS:", label, "\n") }
  else   { .fail <<- .fail + 1L; cat(" FAIL:", label, "\n"); if (!is.null(detalle)) cat("       ", detalle, "\n") }
}
suppressPackageStartupMessages({ library(shiny); library(httr); library(jsonlite) })
source("tests/helpers/s3_memoria.R")
source("tests/helpers/mercado_falso.R")
for (f in c("R/i18n.R", "R/monedas.R", "R/zona.R", "R/policy_engine.R", "R/mercado.R", "R/fx_banxico.R",
            "R/forecasting_schemas.R", "R/forecasting_persistence.R", "R/forecasting_metric_registry.R",
            "R/forecasting_sources.R", "R/forecasting_source_banxico.R", "R/forecasting_source_fred.R",
            "R/forecasting_source_ecb.R", "R/forecasting_source_nyfed.R", "R/mercado_datos.R"))
  suppressMessages(sys.source(f, envir = globalenv()))
mercado_inhabiles_poner(NULL)
q <- function(expr) suppressWarnings(suppressMessages(expr))
D <- function(...) as.Date(c(...))
A <- function(dia, hora = "18:00") as.POSIXct(paste(dia, hora), tz = "UTC")   # 12:00 in Mexico City
serie <- function(fechas, valor) data.frame(fecha = D(fechas), valor = valor)
filas_por <- function(df, k) df[df$clave == k, , drop = FALSE]
FX <- c("2026-11-17", "2026-11-18", "2026-11-19")
CACHE <- list(
  fix     = serie(FX, c(18.30, 18.35, 18.40)),
  divisas = rbind(data.frame(fecha = D(FX), moneda = "EUR", valor = c(20.1, 20.2, 20.3)),
                  data.frame(fecha = D(FX), moneda = "GBP", valor = c(23.1, 23.2, 23.3))),
  bce     = rbind(data.frame(fecha = D(FX), moneda = "USD", valor = 1.1),
                  data.frame(fecha = D(FX), moneda = "MXN", valor = 20.2),
                  data.frame(fecha = D(FX), moneda = "GBP", valor = 0.8)),
  tasas   = rbind(data.frame(fecha = D(FX), metric_id = "tiie28", valor = 7.1, fuente = "banxico"),
                  data.frame(fecha = D("2026-11-12"), metric_id = "cetes28", valor = 6.2, fuente = "banxico"),
                  data.frame(fecha = D(FX), metric_id = "sofr", valor = 3.94, fuente = "nyfed"),
                  data.frame(fecha = D(FX), metric_id = "fed_funds", valor = 3.88, fuente = "nyfed"),
                  data.frame(fecha = D(FX), metric_id = "prime", valor = 7.5, fuente = "fred"),
                  data.frame(fecha = D(FX), metric_id = "estr", valor = 2.445, fuente = "ecb")))
HOY <- A("2026-11-19")
cfg <- function(base, paises = character()) list(moneda_base = base, paises = paises)

cat("== 1. «Tipos» por moneda base ==\n")
t <- mercado_tipos_cliente(cfg("MXN", c("MX", "US", "EU")), CACHE, HOY)
ok("1.1 base MXN: USD/MXN del FIX y EUR/MXN de Banxico, los más recientes",
   identical(t$clave, c("USD/MXN", "EUR/MXN")) && identical(t$valor, c(18.40, 20.3)) &&
     identical(t$fuente, c("banxico_fix", "banxico")) && all(t$fecha == D("2026-11-19")) && all(t$reciente))
t <- mercado_tipos_cliente(cfg("USD", c("US", "MX", "EU")), CACHE, HOY)
ok("1.2 base USD: USD/MXN del FIX (no cruzado) y EUR/USD del BCE",
   identical(sort(t$clave), c("EUR/USD", "USD/MXN")) && identical(filas_por(t, "USD/MXN")$fuente, "banxico_fix") &&
     identical(filas_por(t, "EUR/USD")$valor, 1.1) && identical(filas_por(t, "EUR/USD")$fuente, "ecb"))
t <- mercado_tipos_cliente(cfg("EUR", c("EU", "US", "MX")), CACHE, HOY)
ok("1.3 base EUR: todo del BCE (EUR/USD 1.1, EUR/MXN 20.2), nada de Banxico",
   identical(filas_por(t, "EUR/USD")$valor, 1.1) && identical(filas_por(t, "EUR/MXN")$valor, 20.2) && all(t$fuente == "ecb") &&
     !any(t$cruzado))
t <- mercado_tipos_cliente(cfg("GBP", "US"), CACHE, HOY)
ok("1.4 base GBP con EUA: GBP/USD cruzado en el BCE (1.1 / 0.8)", identical(t$clave, "GBP/USD") &&
     abs(t$valor - 1.375) < 1e-12 && isTRUE(t$cruzado))
ok("1.5 sin mercados (moneda sin país, sin países elegidos): sin filas",
   !nrow(mercado_tipos_cliente(cfg("ZAR"), CACHE, HOY)) && !nrow(mercado_tasas_cliente(cfg("ZAR"), CACHE, HOY)))
ok("1.6 cliente solo MX con base MXN: ningún par (su única moneda es la base)",
   !nrow(mercado_tipos_cliente(cfg("MXN", "MX"), CACHE, HOY)))

cat("== 2. «Sin dato reciente» ==\n")
viejo <- CACHE; viejo$fix <- serie("2026-11-13", 18.2)
t <- mercado_tipos_cliente(cfg("MXN", "US"), viejo, A("2026-11-19"))
ok("2.1 FIX del viernes 13, hoy jueves 19: el lunes 16 es inhábil en México -> 3 días hábiles, se muestra",
   isTRUE(t$reciente) && identical(t$valor, 18.2))
t <- mercado_tipos_cliente(cfg("MXN", "US"), viejo, A("2026-11-20"))
ok("2.2 el viernes 20 ya son 4: no se muestra la cifra, pero sí su fecha", !t$reciente && is.na(t$valor) &&
     identical(t$fecha, D("2026-11-13")))
v2 <- CACHE; v2$tasas <- data.frame(fecha = D("2026-11-23"), metric_id = "sofr", valor = 3.9, fuente = "nyfed")
ok("2.3 SOFR del lunes 23, hoy viernes 27: el 26 (Acción de Gracias) no cuenta -> 3, se muestra",
   isTRUE(filas_por(mercado_tasas_cliente(cfg("USD", "US"), v2, A("2026-11-27")), "sofr")$reciente))
t <- mercado_tasas_cliente(cfg("MXN", "MX"), CACHE, A("2026-11-20"))
ok("2.4 CETES 28 (semanal) del 12, hoy 20: 5 días hábiles (el 16 no cuenta) y sigue (tope 7); TIIE del 19 también",
   isTRUE(filas_por(t, "cetes28")$reciente) && isTRUE(filas_por(t, "tiie28")$reciente))
t <- mercado_tasas_cliente(cfg("MXN", "MX"), CACHE, A("2026-11-24"))
ok("2.5a el 24: CETES lleva justo 7 (se muestra)", isTRUE(filas_por(t, "cetes28")$reciente))
t <- mercado_tasas_cliente(cfg("MXN", "MX"), CACHE, A("2026-11-25"))
ok("2.5 ...el 25 CETES ya pasa de 7 (no se muestra), y TIIE del 19 pasa de 3", !filas_por(t, "cetes28")$reciente &&
     !filas_por(t, "tiie28")$reciente)
sin <- CACHE; sin$fix <- NULL
t <- mercado_tipos_cliente(cfg("MXN", "US"), sin, HOY)
ok("2.6 sin ningún dato: fila sin valor ni fecha", nrow(t) == 1L && is.na(t$valor) && is.na(t$fecha) && !t$reciente)
futuro <- CACHE; futuro$fix <- serie(c("2026-11-19", "2026-11-20"), c(18.4, 99))
ok("2.7 una fecha posterior a hoy no se toma", identical(mercado_tipos_cliente(cfg("MXN", "US"), futuro, HOY)$valor, 18.4))

cat("== 3. «Tasas» ==\n")
t <- mercado_tasas_cliente(cfg("MXN", c("MX", "US", "EU")), CACHE, HOY)
ok("3.1 en el orden de los países: MX (TIIE, CETES), EUA (SOFR, EFFR, Prime), zona euro (€STR)",
   identical(t$clave, c("tiie28", "cetes28", "sofr", "fed_funds", "prime", "estr")))
ok("3.2 los nombres del diccionario", identical(t$etiqueta[1], tr("comun.mercado.tasa.tiie28")) &&
     !any(grepl("^comun\\.", t$etiqueta)))
ok("3.3 cada una con su fuente", identical(t$fuente, c("banxico", "banxico", "nyfed", "nyfed", "fred", "ecb")))

cat("== 4. El cuadro ==\n")
html <- function(x) paste(as.character(x), collapse = "")
h <- html(mercado_cuadro_ui(mercado_tipos_cliente(cfg("MXN", c("US", "EU")), CACHE, HOY), "panorama.mercado.tipos"))
ok("4.1 la cifra con su fuente y fecha («FIX Banxico 19/11»)", grepl("18.4000", h, fixed = TRUE) &&
     grepl(tr("panorama.mercado.fuente.banxico_fix", fecha = "19/11"), h, fixed = TRUE))
hv <- html(mercado_cuadro_ui(mercado_tipos_cliente(cfg("MXN", "US"), viejo, A("2026-11-20")), "panorama.mercado.tipos"))
ok("4.2 vieja: «Sin dato reciente (último: 13/11)» y nunca la cifra",
   grepl(tr("panorama.mercado.sin_dato_reciente", fecha = "13/11"), hv, fixed = TRUE) && !grepl("18.2", hv, fixed = TRUE))
ht <- html(mercado_cuadro_ui(mercado_tasas_cliente(cfg("USD", c("US", "EU")), CACHE, HOY), "panorama.mercado.tasas"))
ok("4.3 una cifra de la Fed de NY lleva su línea de atribución; una del BCE, su nota",
   grepl("Federal Reserve Bank of New York", ht, fixed = TRUE) && grepl(tr("comun.mercado.atribucion.bce"), ht, fixed = TRUE))
hm <- html(mercado_cuadro_ui(mercado_tasas_cliente(cfg("MXN", "MX"), CACHE, HOY), "panorama.mercado.tasas"))
ok("4.4 solo Banxico: ni la atribución de la Fed ni la nota del BCE", !grepl("Federal Reserve", hm, fixed = TRUE) &&
     !grepl(tr("comun.mercado.atribucion.bce"), hm, fixed = TRUE))
ok("4.5 sin filas: nada", is.null(mercado_cuadro_ui(mercado_tipos_cliente(cfg("ZAR"), CACHE, HOY), "panorama.mercado.tipos")))
ok("4.6 tasas con 2 decimales y «%»", grepl("3.94 %", ht, fixed = TRUE))

cat("== 5. Leer nunca llama a una fuente ==\n")
s3_memoria_reset()
poner <- function(llave, schema, df) q(modificar_hd_admin(llave, schema, function(x) df[, names(schema()), drop = FALSE]))
poner(MERCADO_BCE_LLAVE, .schema_mercado_bce, transform(CACHE$bce, obtenido_en = "x"))
poner(MERCADO_TASAS_LLAVE, .schema_mercado_tasas, transform(CACHE$tasas, obtenido_en = "x"))
poner(FX_FIX_LLAVE, .schema_fx_fix, transform(CACHE$fix, serie = FX_FIX_SERIE, obtenido_en = "x"))
espias <- c("fcs_fetch_banxico", "fcs_fetch_fred", "fcs_fetch_ecb", "fcs_fetch_nyfed", ".fcs_ecb_serie", "mercado_datos_refrescar")
originales <- mget(espias, envir = globalenv())
for (e in espias) assign(e, function(...) stop("una fuente desde la lectura"), envir = globalenv())
.MERCADO_DATOS$memo <- NULL
c1 <- tryCatch(mercado_cache(HOY), error = function(e) e)
ok("5.1 mercado_cache() y los cuadros leen solo las cachés", is.list(c1) && !inherits(c1, "error") &&
     nrow(mercado_tipos_cliente(cfg("MXN", c("US", "EU")), c1, HOY)) == 2L &&
     identical(filas_por(mercado_tasas_cliente(cfg("USD", "US"), c1, HOY), "sofr")$valor, 3.94))
for (e in espias) assign(e, originales[[e]], envir = globalenv())
extra <- data.frame(fecha = D("2026-11-20"), metric_id = "tiie28", valor = 7.2, fuente = "banxico", obtenido_en = "x")
poner(MERCADO_TASAS_LLAVE, .schema_mercado_tasas, rbind(transform(CACHE$tasas, obtenido_en = "x"), extra))   # the cache only grows
ok("5.2 dentro de 10 minutos, el memo (no relee S3)", nrow(mercado_cache(HOY + 300)$tasas) == nrow(CACHE$tasas))
ok("5.3 pasados 10 minutos, relee", nrow(mercado_cache(HOY + 700)$tasas) == nrow(CACHE$tasas) + 1L)
ok("5.4 caché ilegible: NULL esa parte, nunca un error", {
  .MERCADO_DATOS$memo <- NULL
  k <- .s3_key(MERCADO_BCE_LLAVE, client_id = .HD_ADMIN_CID)
  assign(".mem_prohibido", c(get0(".mem_prohibido", ifnotfound = character()), k), envir = globalenv())
  r <- mercado_cache(HOY + 2000)
  assign(".mem_prohibido", setdiff(.mem_prohibido, k), envir = globalenv())
  is.null(r$bce) && !is.null(r$fix)
})

cat("== 6. El refresco (en línea) ==\n")
FECHAS <- c("2026-11-17", "2026-11-18", "2026-11-19")
TOKEN <- "tok-de-prueba-123"
VALORES_BX <- list(SF43718 = 18.4, SF46405 = 18.41, SF46410 = 20.3, SF46407 = 23.3, SF46406 = 0.12,
                   SF290383 = 2.5, SF60632 = 13.1, SF43783 = 7.1, SF43936 = 6.2)
CONF <- list(ecb = list(modo = "ok", fechas = FECHAS,
                        valores = list(USD = 1.1, MXN = 20.2, GBP = 0.8, JPY = 170, CAD = 1.5, CNY = 7.9,
                                       "B.EU000A2X2A25.WT" = 2.445)),
             nyfed = list(modo = "ok", fechas = FECHAS, valores = list(sofr = 3.94, effr = 3.88)),
             banxico = list(modo = "ok", token_esperado = TOKEN, fechas = FECHAS, valores = VALORES_BX))
srv <- mercado_falso_iniciar(CONF)
on.exit(srv$parar(), add = TRUE)
llaves <- Sys.getenv(c("FRED_API_KEY", "BANXICO_TOKEN"))
Sys.setenv(HD_ECB_URL = srv$url, HD_NYFED_URL = srv$url, HD_BANXICO_URL = srv$url,
           FRED_API_KEY = "", BANXICO_TOKEN = TOKEN)          # FRED: no key, no network
lineas_log <- character()
con_log <- function(expr) withCallingHandlers(expr, message = function(m) {
  if (startsWith(conditionMessage(m), "mercado:")) lineas_log <<- c(lineas_log, trimws(conditionMessage(m)))
  invokeRestart("muffleMessage") })
s3_memoria_reset()
.MERCADO_DATOS$intento <- NULL; .MERCADO_DATOS$memo <- NULL
r <- con_log(mercado_datos_refrescar(HOY, en_fondo = FALSE))
c6 <- mercado_cache(HOY)
ok("6.1 llena el FIX, las divisas de Banxico, el BCE (6 × 3) y las tasas que respondieron",
   nrow(c6$fix) == 3L && nrow(c6$divisas) == 18L && nrow(c6$bce) == 18L &&
     setequal(unique(c6$tasas$metric_id), c("tiie28", "cetes28", "sofr", "fed_funds", "estr")) &&
     all(c6$tasas$fuente[c6$tasas$metric_id == "sofr"] == "nyfed"), paste(r, nrow(c6$bce)))
ok("6.2 Prime sin llave de FRED NO es una falla: «ok», y una sola línea «fuente no configurada (fred: falta FRED_API_KEY)», sin valores",
   identical(r, "ok") && length(lineas_log) == 1L &&
     identical(lineas_log, "mercado: fuente no configurada (fred: falta FRED_API_KEY)"), paste(r, paste(lineas_log, collapse = " | ")))
ok("6.3 otra vez dentro de la hora: «espera», sin pedir nada", { srv$log_borrar(); identical(mercado_datos_refrescar(HOY + 60, en_fondo = FALSE), "espera") && !nrow(srv$log()) })
n <- .mem_escrituras
q(mercado_datos_refrescar(HOY + 120, forzar = TRUE, en_fondo = FALSE))
ok("6.4 forzado con las mismas cifras: no escribe", .mem_escrituras == n)
c2 <- CONF; for (k in c("ecb", "nyfed", "banxico")) c2[[k]]$fechas <- c(FECHAS, "2026-11-20"); srv$config(c2)
q(mercado_datos_refrescar(A("2026-11-20"), forzar = TRUE, en_fondo = FALSE))
c7 <- mercado_cache(A("2026-11-20"))
ok("6.5 un día nuevo: solo se agrega (6 filas más en el BCE, 1 en el FIX), nada se repite",
   nrow(c7$bce) == 24L && nrow(c7$fix) == 4L && !anyDuplicated(paste(c7$bce$fecha, c7$bce$moneda)))
s3_memoria_reset()
srv$config(list(ecb = list(modo = "falla500"), nyfed = list(modo = "falla500"), banxico = list(modo = "falla500", token_esperado = TOKEN)))
n <- .mem_escrituras; lineas_log <- character()
r <- con_log(mercado_datos_refrescar(HOY, forzar = TRUE, en_fondo = FALSE))
ok("6.6 todo caído: «falla», nada escrito, una sola línea que nombra lo que falló",
   identical(r, "falla") && .mem_escrituras == n && length(lineas_log) == 1L && grepl("bce:USD", lineas_log) &&
     grepl("banxico:SF43718", lineas_log) && grepl("tasa:sofr", lineas_log))
Sys.setenv(BANXICO_TOKEN = ""); lineas_log <- character()
srv$config(CONF); srv$log_borrar()
q0 <- con_log(mercado_datos_refrescar(HOY, forzar = TRUE, en_fondo = FALSE))
ok("6.7 sin token de Banxico: no se le pide, no es una falla, y se dice una vez («banxico: falta BANXICO_TOKEN»)",
   !any(grepl("SieAPIRest", srv$log()$ruta)) && identical(q0, "ok") &&
     identical(lineas_log, "mercado: fuente no configurada (banxico: falta BANXICO_TOKEN)"), paste(q0, paste(lineas_log, collapse = " | ")))
lineas_log <- character()
q9 <- con_log(mercado_datos_refrescar(HOY, forzar = TRUE, en_fondo = FALSE))
ok("6.9 la siguiente hora: ni falla ni otra vez «no configurada» (una línea por proceso, no cada hora)",
   identical(q9, "ok") && !length(lineas_log), paste(lineas_log, collapse = " | "))
sin_prime <- CACHE; sin_prime$tasas <- sin_prime$tasas[sin_prime$tasas$metric_id != "prime", ]
ok("6.10 una tasa sin fuente configurada y sin datos no se muestra (nunca un «Sin dato» permanente); con datos, sí",
   !"prime" %in% mercado_tasas_cliente(cfg("USD", "US"), sin_prime, HOY)$clave &&
     "prime" %in% mercado_tasas_cliente(cfg("USD", "US"), CACHE, HOY)$clave &&
     "sofr" %in% mercado_tasas_cliente(cfg("USD", "US"), sin_prime, HOY)$clave)
Sys.setenv(BANXICO_TOKEN = TOKEN)
# 6.8 everything fetched and saved: "ok" and NO log line (paste0 with an empty
# vector used to log «refresco falló (guardar:)» on every good refresh).
s3_memoria_reset(); lineas_log <- character()
bien <- list(fix = NULL, divisas = NULL, bce = transform(CACHE$bce, obtenido_en = "x"), tasas = NULL, fallas = character())
r8 <- con_log(.mercado_guardar(bien))
ok("6.8 todo bajado y guardado: «ok» y ninguna línea de falla en el log", identical(r8, "ok") && !length(lineas_log),
   paste(r8, paste(lineas_log, collapse = " | ")))

cat("== 7. En segundo plano (como en la app) ==\n")
esperar_hijo <- function(seg = 90) {
  fin <- Sys.time() + seg
  con_log(while (!is.null(.MERCADO_DATOS$job) && Sys.time() < fin) { later::run_now(1); Sys.sleep(0.2) })
}
srv$config(CONF); srv$log_borrar()
s3_memoria_reset(); lineas_log <- character()
.MERCADO_DATOS$intento <- NULL; .MERCADO_DATOS$memo <- NULL
t0 <- Sys.time()
r <- mercado_datos_refrescar(HOY)
ok("7.1 vuelve enseguida con «lanzado»", identical(r, "lanzado") && as.numeric(difftime(Sys.time(), t0, units = "secs")) < 5)
job <- .MERCADO_DATOS$job
linea_cmd <- tryCatch(paste(job$get_cmdline(), collapse = " "), error = function(e) "")
ok("7.2 el token nunca va en la línea de comandos del hijo (la lista de procesos)", nzchar(linea_cmd) && !grepl(TOKEN, linea_cmd, fixed = TRUE))
ok("7.3 mientras corre: «corriendo» (nunca dos a la vez)", identical(mercado_datos_refrescar(HOY, forzar = TRUE), "corriendo"))
esperar_hijo()
c8 <- mercado_cache(HOY + 3600)
lg <- srv$log()
ok("7.4 al terminar, el proceso principal escribió las cachés (FIX, BCE y tasas)",
   is.null(.MERCADO_DATOS$job) && nrow(c8$fix) == 3L && nrow(c8$bce) == 18L && "sofr" %in% c8$tasas$metric_id)
ok("7.5 el token llegó al hijo por su entorno: Banxico lo recibió en el encabezado", any(lg$consulta == "token=ok") && !any(lg$consulta == "token=no"))
ok("7.6 el hijo lo trajo todo: ninguna línea de falla en el log de la app", !any(grepl("^mercado: refresco falló", lineas_log)),
   paste(lineas_log, collapse = " | "))

cat("== 9. Las lecciones de la T74 ==\n")
# 9.1 the child's errors are not hidden: a file it cannot load.
s3_memoria_reset(); lineas_log <- character(); .MERCADO_DATOS$intento <- NULL
n <- .mem_escrituras
mercado_datos_refrescar(HOY, archivos = c(.mercado_archivos_hijo(), file.path(normalizePath("."), "R/no_existe.R")))
esperar_hijo()
ok("9.1 un hijo que truena (archivo que no existe): «mercado: refresco falló (proceso hijo)», nada escrito",
   any(lineas_log == "mercado: refresco falló (proceso hijo)") && .mem_escrituras == n, paste(lineas_log, collapse = " | "))
# 9.2 the child does not depend on the working directory.
s3_memoria_reset(); lineas_log <- character(); .MERCADO_DATOS$intento <- NULL
assign("HD_APP_DIR", normalizePath(".", winslash = "/"), envir = globalenv())
antes <- setwd(tempdir())
r <- mercado_datos_refrescar(HOY)
setwd(antes)
esperar_hijo()
ok("9.2 lanzado desde otro directorio (como Rtmp*): carga por rutas absolutas y llena las cachés",
   identical(r, "lanzado") && nrow(mercado_cache(HOY + 7200)$bce) == 18L)
ok("9.3 el hijo recibe rutas absolutas, ninguna relativa", all(grepl("^([A-Za-z]:)?/", .mercado_archivos_hijo())))
# 9.4 a hung child is killed past its limit.
srv$config(modifyList(CONF, list(banxico = list(modo = "lento", segundos = 20))))
s3_memoria_reset(); lineas_log <- character(); .MERCADO_DATOS$intento <- NULL
tope <- MERCADO_HIJO_TOPE_SEG; assign("MERCADO_HIJO_TOPE_SEG", 3, envir = globalenv())
n <- .mem_escrituras
mercado_datos_refrescar(HOY); job <- .MERCADO_DATOS$job
esperar_hijo(30)
assign("MERCADO_HIJO_TOPE_SEG", tope, envir = globalenv())
ok("9.4 un hijo colgado: se mata al pasar el tope, «tiempo agotado», nada escrito, y se puede lanzar otro",
   !isTRUE(job$is_alive()) && any(lineas_log == "mercado: refresco falló (tiempo agotado)") && .mem_escrituras == n &&
     is.null(.MERCADO_DATOS$job))
srv$config(CONF)
# 9.6 a child that LOOKS fine (a result) but failed: T74's case.
falso <- function(r, err = character(), estado = 0L) list(get_result = function() r,
  read_all_error_lines = function() err, get_exit_status = function() estado)
bueno <- list(fix = NULL, divisas = NULL, bce = NULL, tasas = NULL, fallas = character())
ok("9.6 un resultado con «Error» en el stderr del hijo, o con salida distinta de 0: se toma como falla (NULL)",
   is.null(.mercado_resultado_hijo(falso(bueno, err = c("algo", "Error in f(): x")))) &&
     is.null(.mercado_resultado_hijo(falso(bueno, estado = 1L))) &&
     is.null(.mercado_resultado_hijo(falso("no es lista"))) &&
     identical(.mercado_resultado_hijo(falso(bueno, err = "aviso sin importancia")), bueno))
# 9.5 the child never touches S3 (statically: its function and what it loads write nothing).
hijo_txt <- paste(deparse(.mercado_hijo), collapse = "\n")
ok("9.5 el hijo solo baja: ni modificar_hd_admin, ni .s3_, ni aws.s3 en .mercado_hijo ni en .mercado_bajar_*",
   !grepl("modificar_|\\.s3_|aws\\.s3|s3save", paste(hijo_txt, paste(deparse(.mercado_bajar_todo), collapse = ""),
                                                     paste(deparse(.mercado_bajar_banxico), collapse = ""),
                                                     paste(deparse(.mercado_bajar_bce), collapse = ""),
                                                     paste(deparse(.mercado_bajar_tasas), collapse = ""))))
Sys.setenv(FRED_API_KEY = llaves[[1]], BANXICO_TOKEN = llaves[[2]], HD_ECB_URL = "", HD_NYFED_URL = "", HD_BANXICO_URL = "")
srv$parar()

cat("== 8. Panorama solo lee ==\n")
pm <- readLines("R/panorama_module.R", warn = FALSE, encoding = "UTF-8")
pm <- pm[!grepl("^\\s*#", pm)]
ok("8.1 panorama_module.R no llama a ninguna fuente ni al refresco",
   !any(grepl("mercado_datos_refrescar|fcs_fetch_|\\.fcs_ecb_serie|fx_divisas\\(", pm)))
ok("8.2 ...y sí dibuja los dos cuadros desde mercado_cache()",
   any(grepl("mercado_cache()", pm, fixed = TRUE)) && any(grepl("panorama.mercado.tipos", pm, fixed = TRUE)) &&
     any(grepl("panorama.mercado.tasas", pm, fixed = TRUE)))
app <- readLines("app.R", warn = FALSE, encoding = "UTF-8")
ok("8.3 app.R refresca al primer dibujo de la sesión y con «Actualizar»", sum(grepl("mercado_datos_refrescar()", app, fixed = TRUE)) == 2L)

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
