# =============================================================================
# tests/test_t103_actualizador.R
# T103 F1: the ERP refresh without the app (scripts/actualizar_erp.R) and the
# changes it needed in the one path it shares with the app's Refrescar
# (fetch_all_companies() / save_sap_snapshot()). Against a FAKE Service Layer
# on 127.0.0.1 (tests/helpers/sap_falso.R, invented data) and the in-memory S3
# (tests/helpers/s3_memoria.R): no real ERP, no real bucket.
#   1  C1  pages of 500 (Prefer: odata.maxpagesize); 20 without the header
#   2  the app's defaults: a failed company is left out and the rest saved
#   3  C3  exigir_todas: one failed company = nothing saved
#   4  C4  the floor against the current snapshot; piso = FALSE skips it
#   5  guardar = FALSE (--prueba) writes nothing
#   6  C6  a failed save is never "saved"
#   7  C5  credentials resolved from the store count, not only SAP_<INI>_URL
#   8  C2  page timeout and retries from options
#   9  hopdesk.sin_cola keeps the offline queue off
#  10  the script's arguments and environment guards
#  11  the script end to end: saves, signs the log (T87), releases the lock;
#      lock held = 3; --prueba writes nothing; no VPN = 2; incomplete = 4;
#      no secret and no URL on the console
#  12  the script's file list loads in a clean R process
#
# Run from project root:  Rscript tests/test_t103_actualizador.R
# =============================================================================
cat("=== test_t103_actualizador ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr, detalle = NULL) {
  r <- tryCatch(isTRUE(expr), error = function(e) { message("  error: ", conditionMessage(e)); FALSE })
  if (r) { .pass <<- .pass + 1L; cat(" PASS:", label, "\n") }
  else   { .fail <<- .fail + 1L; cat(" FAIL:", label, "\n"); if (!is.null(detalle)) cat("       ", detalle, "\n") }
}
source("tests/helpers/s3_memoria.R")
source("tests/helpers/sap_falso.R")
sys.source("scripts/actualizar_erp.R", envir = globalenv())   # defines erp_*(); does not run
suppressMessages(erp_cargar())                                 # the script's own file list
s3_memoria_reinstalar()

CID <- "demo"; CLAVE <- "CLAVE-FICTICIA-123"
sap <- sap_falso_iniciar(list(NG = list(AR = 0, AP = 0, modo = "ok")))
on.exit(sap$parar(), add = TRUE)
for (ini in c("NG", "NT", "FA", "LA", "LB"))
  do.call(Sys.setenv, stats::setNames(list(sap$url_de(ini), "BD_FICTICIA", "usuario.ficticio", CLAVE),
                                      paste0("SAP_", ini, c("_URL", "_COMPANY", "_USER", "_PASSWORD"))))
NOMBRES <- c(NG = "Ficticia Norte", NT = "Ficticia Centro", FA = "Ficticia Falla", LA = "Ficticia Lenta A",
             LB = "Ficticia Lenta B", ST = "Ficticia Almacen")
cmap_de <- function(...) NOMBRES[c(...)]
snap <- function(l) .mem[[paste0(CID, "/sap_snapshot_", l, ".rds")]]
configurar <- function(...) { sap$config(list(...)); sap$log_borrar() }
reiniciar <- function() {
  s3_memoria_reset(); rm(list = ls(.sap_sessions), envir = .sap_sessions)
  options(hopdesk.sap_timeout_pagina = NULL, hopdesk.sap_reintentos = NULL, hopdesk.sap_timeout_login = NULL,
          hopdesk.sap_progreso = NULL, hopdesk.sin_cola = NULL)
}
q <- function(expr) suppressWarnings(suppressMessages(expr))
error_de <- function(expr) tryCatch({ q(expr); NULL }, error = function(e) e)

cat("== 1. C1: páginas de 500 ==\n")
reiniciar(); configurar(NG = list(AR = 1200, AP = 0, modo = "ok"))
r <- q(fetch_ar("NG", client_id = CID))
lg <- sap$log()
ok("1.1 1,200 facturas en 3 páginas, no en 60", nrow(r) == 1200L && sum(lg$empresa == "NG") == 3L,
   paste("páginas:", sum(lg$empresa == "NG")))
ok("1.2 cada página pidió Prefer: odata.maxpagesize=500", all(lg$prefer == "odata.maxpagesize=500"))
sin <- httr::GET(paste0(sap$url_de("NG"), "/Invoices"), query = list(`$top` = 500, `$skip` = 0))
ok("1.3 sin el encabezado el servidor da 20 (el PageSize que la app sufría)",
   length(jsonlite::fromJSON(httr::content(sin, "text", encoding = "UTF-8"))$value$DocNum) == 20L)

cat("== 2. La app, como hoy ==\n")
reiniciar(); configurar(NG = list(AR = 30, modo = "ok"), FA = list(AR = 10, modo = "falla500"))
r <- q(fetch_all_companies("AR", cmap_de("NG", "FA"), client_id = CID))
ok("2.1 sin salvaguardas: la empresa que falla queda fuera y se guarda el resto",
   nrow(r) == 30L && nrow(snap("AR")$data) == 30L && identical(unique(snap("AR")$data$Empresa), "Ficticia Norte"))
res <- sap_ultimo_resumen("AR")
ok("2.2 el resumen lo cuenta: guardado, y FA en failed",
   isTRUE(res$guardado) && identical(res$empresas$FA$status, "failed") && identical(res$empresas$NG$status, "ok"))

cat("== 3. C3: todas o nada ==\n")
antes <- snap("AR")
e <- error_de(fetch_all_companies("AR", cmap_de("NG", "FA"), client_id = CID, exigir_todas = TRUE))
ok("3.1 una empresa fallida = error erp_incompleto que la nombra",
   inherits(e, "erp_incompleto") && identical(e$fallidas, "FA"))
ok("3.2 y la foto no se tocó", identical(snap("AR"), antes))

cat("== 4. C4: el piso ==\n")
reiniciar(); configurar(NG = list(AR = 100, modo = "ok"), NT = list(AR = 8, modo = "ok"))
invisible(q(fetch_all_companies("AR", cmap_de("NG", "NT"), client_id = CID)))
antes <- snap("AR")
configurar(NG = list(AR = 10, modo = "ok"), NT = list(AR = 8, modo = "ok"))
e <- error_de(fetch_all_companies("AR", cmap_de("NG", "NT"), client_id = CID, piso = TRUE))
ok("4.1 de 100 a 10 filas: erp_piso, con la empresa y los números", inherits(e, "erp_piso") &&
     identical(e$violaciones, "Ficticia Norte: 100 -> 10"))
ok("4.2 la foto buena sigue intacta", identical(snap("AR"), antes))
configurar(NG = list(AR = 50, modo = "ok"), NT = list(AR = 8, modo = "ok"))
ok("4.3 la mitad exacta pasa", is.null(error_de(fetch_all_companies("AR", cmap_de("NG", "NT"), client_id = CID, piso = TRUE))) &&
     nrow(snap("AR")$data) == 58L)
configurar(NG = list(AR = 0, modo = "ok"), NT = list(AR = 8, modo = "ok"))
e <- error_de(fetch_all_companies("AR", cmap_de("NG", "NT"), client_id = CID, piso = TRUE))
ok("4.4 una empresa que tenía filas y ahora 0 también se detiene", inherits(e, "erp_piso") &&
     identical(e$violaciones, "Ficticia Norte: 50 -> 0"))
ok("4.5 piso = FALSE (--forzar) sí guarda", is.null(error_de(fetch_all_companies("AR", cmap_de("NG", "NT"), client_id = CID))) &&
     nrow(snap("AR")$data) == 8L)
.mem_falla <- paste0(CID, "/sap_snapshot_AR.rds")
configurar(NG = list(AR = 40, modo = "ok"), NT = list(AR = 8, modo = "ok"))
e <- error_de(fetch_all_companies("AR", cmap_de("NG", "NT"), client_id = CID, piso = TRUE))
ok("4.6 si S3 no deja leer la foto actual, no se guarda nada", !is.null(e) && nrow(.mem[[paste0(CID, "/sap_snapshot_AR.rds")]]$data) == 8L)
.mem_falla <- character()

cat("== 5. guardar = FALSE (--prueba) ==\n")
reiniciar(); configurar(NG = list(AR = 25, modo = "ok"))
n0 <- .mem_escrituras
r <- q(fetch_all_companies("AR", cmap_de("NG"), client_id = CID, guardar = FALSE))
ok("5.1 devuelve las filas y no escribe nada", nrow(r) == 25L && .mem_escrituras == n0 && is.null(snap("AR")))
ok("5.2 el resumen dice prueba", identical(sap_ultimo_resumen("AR")$motivo, "prueba"))

cat("== 6. C6: un guardado fallido nunca dice guardado ==\n")
reiniciar(); configurar(NG = list(AR = 12, modo = "ok"))
.mem_falla_escritura <- paste0(CID, "/sap_snapshot_AR.rds")
e <- error_de(save_sap_snapshot(data.frame(Empresa = "X", n = 1), "AR", client_id = CID))
ok("6.1 save_sap_snapshot() se detiene con foto_no_guardada", inherits(e, "foto_no_guardada"))
ok("6.2 y la caché no guarda lo que S3 no tiene",
   !exists(paste0(CID, "/sap_snapshot_AR.rds"), envir = .s3_preload_cache, inherits = FALSE))
r <- q(fetch_all_companies("AR", cmap_de("NG"), client_id = CID))
ok("6.3 la app recibe las filas en vivo y el resumen dice no_guardado",
   nrow(r) == 12L && identical(sap_ultimo_resumen("AR")$motivo, "no_guardado") && !isTRUE(sap_ultimo_resumen("AR")$guardado))
e <- error_de(fetch_all_companies("AR", cmap_de("NG"), client_id = CID, exigir_guardado = TRUE))
ok("6.4 el script (exigir_guardado) recibe el error", inherits(e, "foto_no_guardada"))
.mem_falla_escritura <- character()
app <- readLines("app.R", warn = FALSE, encoding = "UTF-8")
ok("6.5 la app muestra el aviso de guardado fallido (error, sin cerrarse solo)",
   any(grepl('identical(sap_ultimo_resumen(ledger_name)$motivo, "no_guardado")', app, fixed = TRUE)) &&
     any(grepl('tr("tablas.datos_sap.aviso.no_guardado", ledger = ledger_name)', app, fixed = TRUE)))
ok("6.6 el texto existe en los dos idiomas", nzchar(tr("tablas.datos_sap.aviso.no_guardado", ledger = "AR")) &&
     "tablas.datos_sap.aviso.no_guardado" %in% names(jsonlite::fromJSON("www/i18n/en.json")))

cat("== 7. C5: credenciales resueltas, no solo SAP_<INI>_URL ==\n")
reiniciar()
ok("7.1 sin ninguna credencial la URL es \"\" (la empresa se omite, como hoy)", identical(.sap_url_configurada("ZZ", CID), ""))
Sys.setenv(SAP_PA_URL = sap$url_de("PA"))
ok("7.2 credenciales a medias son un error, no un \"no configurada\"", !is.null(error_de(.sap_url_configurada("PA", CID))))
Sys.unsetenv("SAP_PA_URL")
configurar(ST = list(AR = 5, modo = "ok"))
creds_orig <- .sap_creds
.sap_creds <- function(initials, client_id = NULL)   # the store's answer, with NO SAP_ST_* in the environment
  if (toupper(initials) == "ST") list(url = sap$url_de("ST"), company = "BD_FICTICIA", user = "u", password = CLAVE, ssl_verify = FALSE) else creds_orig(initials, client_id)
r <- q(fetch_all_companies("AR", cmap_de("ST"), client_id = CID))
.sap_creds <- creds_orig
ok("7.3 una empresa con credenciales solo en el almacén se lee", !is.null(r) && nrow(r) == 5L &&
     identical(sap_ultimo_resumen("AR")$empresas$ST$status, "ok"))

cat("== 8. C2: tiempo por página y reintentos ==\n")
# One fake server per case: a slow page of one must not shift the other's clock.
reiniciar()
sap_la <- sap_falso_iniciar(list(LA = list(AR = 600, modo = "lento_siempre")))
sap_lb <- sap_falso_iniciar(list(LB = list(AR = 600, modo = "lento_una_vez")))
Sys.setenv(SAP_LA_URL = sap_la$url_de("LA"), SAP_LB_URL = sap_lb$url_de("LB"))
options(hopdesk.sap_timeout_pagina = 1, hopdesk.sap_reintentos = 0L, hopdesk.sap_timeout_login = 10)
e <- error_de(fetch_all_companies("AR", cmap_de("LA"), client_id = CID))
ok("8.1 sin reintentos (la app), una página lenta corta el libro y no guarda", inherits(e, "sap_no_connection") && is.null(snap("AR")))
options(hopdesk.sap_reintentos = 3L)
r <- q(fetch_all_companies("AR", cmap_de("LB"), client_id = CID))
ok("8.2 con reintentos (el script) la misma página lenta se recupera y se guarda", !is.null(r) && nrow(r) == 600L &&
     nrow(snap("AR")$data) == 600L)
sap_la$parar(); sap_lb$parar()
src <- paste(readLines("R/sap_api.R", warn = FALSE), collapse = "\n")
ok("8.3 los valores de la app no cambian: 8 s por página, 0 reintentos, 2 s de login",
   grepl('getOption("hopdesk.sap_timeout_pagina", 8)', src, fixed = TRUE) &&
     grepl('getOption("hopdesk.sap_reintentos", 0L)', src, fixed = TRUE) &&
     grepl('getOption("hopdesk.sap_timeout_login", 2)', src, fixed = TRUE))

cat("== 9. hopdesk.sin_cola ==\n")
reiniciar(); options(hopdesk.sin_cola = TRUE)
tf <- tempfile(fileext = ".rds"); saveRDS(1, tf)
ok("9.1 con la opción, .queue_stage_entry() no encola", identical(q(.queue_stage_entry(tf, paste0(CID, "/moves.rds"), "moves.rds", CID)), FALSE))
options(hopdesk.sin_cola = NULL)

cat("== 10. Argumentos y entorno del script ==\n")
ok("10.1 sin --cliente: error", !is.null(error_de(erp_args(c("--usuario", "x")))))
ok("10.2 hd-admin no es un cliente", !is.null(error_de(erp_args(c("--cliente", "hd-admin", "--usuario", "x")))))
ok("10.3 --libros solo AR/AP", !is.null(error_de(erp_args(c("--cliente", "demo", "--usuario", "x", "--libros", "XX")))))
ok("10.4 sin --usuario solo con --prueba", !is.null(error_de(erp_args(c("--cliente", "demo")))) &&
     is.null(error_de(erp_args(c("--cliente", "demo", "--prueba")))))
ok("10.5 una opción desconocida es error", !is.null(error_de(erp_args(c("--cliente", "demo", "--prueba", "--borrar")))))
ent <- Sys.getenv(c("S3_BUCKET", "AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_DEFAULT_REGION", "HOPDESK_PRODUCTION"))
Sys.setenv(AWS_ACCESS_KEY_ID = "ficticia", AWS_SECRET_ACCESS_KEY = "ficticia", AWS_DEFAULT_REGION = "us-east-1")
wd <- getwd(); tmp <- tempfile("t103_env_"); dir.create(tmp); setwd(tmp)
a_prod <- erp_args(c("--cliente", "demo", "--prueba", "--produccion")); a_dev <- erp_args(c("--cliente", "demo", "--prueba"))
Sys.unsetenv("HOPDESK_PRODUCTION")
ok("10.6 --produccion sin HOPDESK_PRODUCTION=1: error", !is.null(error_de(erp_entorno(a_prod))))
Sys.setenv(HOPDESK_PRODUCTION = "1")
ok("10.7 HOPDESK_PRODUCTION=1 sin --produccion: error", !is.null(error_de(erp_entorno(a_dev))))
Sys.unsetenv("HOPDESK_PRODUCTION")
ok("10.8 dev sin .Renviron.local: error", !is.null(error_de(erp_entorno(a_dev))))
writeLines("S3_BUCKET=fake-bucket", ".Renviron.local")
ok("10.9 un .Renviron.local que no cambia el bucket: error", !is.null(error_de(erp_entorno(a_dev))))
writeLines("S3_BUCKET=fake-bucket-dev", ".Renviron.local"); Sys.setenv(S3_BUCKET = "fake-bucket")
b <- error_de(erp_entorno(a_dev))
ok("10.10 dev con su propio bucket: pasa, con la cola apagada y los tiempos de VPN",
   is.null(b) && identical(Sys.getenv("S3_BUCKET"), "fake-bucket-dev") && isTRUE(getOption("hopdesk.sin_cola")) &&
     getOption("hopdesk.sap_timeout_pagina") > 8)
setwd(wd); do.call(Sys.setenv, as.list(ent)); if (!nzchar(ent[["HOPDESK_PRODUCTION"]])) Sys.unsetenv("HOPDESK_PRODUCTION")
Sys.setenv(S3_BUCKET = "fake-bucket")

cat("== 11. El script de punta a punta ==\n")
reiniciar(); s3_memoria_reinstalar()
Sys.setenv(HOPDESK_PRODUCTION = "1", AWS_ACCESS_KEY_ID = "ficticia", AWS_SECRET_ACCESS_KEY = "ficticia", AWS_DEFAULT_REGION = "us-east-1")
options(hopdesk.erp_logs_dir = file.path(tempdir(), "t103_logs"))
.mem[["hd-admin/username_index.rds"]] <- data.frame(username = "operador.ficticio", home_folder = "hd-admin",
  account_code = "S9999", created_at = "2026-11-25", active = TRUE, stringsAsFactors = FALSE)
emp <- .schema_empresas()[0, ]
emp <- dplyr::bind_rows(emp, tibble::tibble(id = c("e1", "e2"), account_code = c("E0001", "E0002"), initials = c("NG", "NT"),
                                            razon_social = c("EMPRESA FICTICIA NORTE SA", "EMPRESA FICTICIA CENTRO SA"),
                                            nombre_corto = unname(NOMBRES[c("NG", "NT")]), activa = TRUE, deleted = FALSE))
.mem[[paste0(CID, "/empresas.rds")]] <- emp
configurar(NG = list(AR = 700, AP = 30, modo = "ok"), NT = list(AR = 40, AP = 0, modo = "ok"))
correr <- function(...) { salida <- utils::capture.output(codigo <- erp_main(c(...))); s3_memoria_reinstalar(); list(codigo = codigo, salida = salida) }
x <- correr("--cliente", CID, "--usuario", "operador.ficticio", "--produccion")
ok("11.1 termina con 0 y guarda AR (740) y AP (30)", identical(as.integer(x$codigo), 0L) &&
     nrow(snap("AR")$data) == 740L && nrow(snap("AP")$data) == 30L, paste(x$salida, collapse = " | "))
ok("11.2 la consola muestra el avance por página y el cierre", any(grepl("AR · NG", x$salida, fixed = TRUE)) &&
     any(grepl("^Listo", x$salida)))
lock <- .mem[[paste0(CID, "/erp_actualizacion_lock.rds")]]
ok("11.3 el candado quedó liberado", isTRUE(lock$liberado))
aud <- tryCatch(.mem[[.audit_current_key(CID)]], error = function(e) NULL)
filas_erp <- if (is.data.frame(aud)) aud[aud$action == "erp_actualizado", , drop = FALSE] else data.frame()
ok("11.4 el registro tiene un erp_actualizado por libro", nrow(filas_erp) == 2L)
ok("11.5 firmado como staff (T87: hd-staff en la carpeta del cliente)", nrow(filas_erp) == 2L && all(startsWith(filas_erp$user, "hd-staff")))
ok("11.6 erp_actualizado es C en el catálogo (T87)", identical(t87_clase_accion("erp_actualizado"), "C") &&
     any(grepl('"erp_actualizado"', readLines("tests/test_t87_catalogo.R", warn = FALSE), fixed = TRUE)))
bit <- unlist(lapply(list.files(getOption("hopdesk.erp_logs_dir"), full.names = TRUE), readLines, warn = FALSE))
ok("11.7 ni la consola ni la bitácora muestran la contraseña ni la URL del ERP",
   !any(grepl(CLAVE, c(x$salida, bit), fixed = TRUE)) && !any(grepl("127.0.0.1", c(x$salida, bit), fixed = TRUE)))

.mem[[paste0(CID, "/erp_actualizacion_lock.rds")]] <- list(token = "otro", usuario = "otra.persona", inicio = Sys.time())
antes <- list(snap("AR"), snap("AP"))
x <- correr("--cliente", CID, "--usuario", "operador.ficticio", "--produccion")
ok("11.8 con el candado tomado por otra corrida: código 3 y nada escrito", identical(as.integer(x$codigo), 3L) &&
     identical(list(snap("AR"), snap("AP")), antes) && any(grepl("otra.persona", x$salida, fixed = TRUE)))
.mem[[paste0(CID, "/erp_actualizacion_lock.rds")]] <- list(token = "otro", usuario = "otra.persona", inicio = Sys.time() - 3 * 3600)
configurar(NG = list(AR = 720, AP = 30, modo = "ok"), NT = list(AR = 40, AP = 0, modo = "ok"))
n0 <- .mem_escrituras
x <- correr("--cliente", CID, "--produccion", "--prueba")
ok("11.9 --prueba: código 0 y no escribe nada (ni candado ni registro)", identical(as.integer(x$codigo), 0L) &&
     .mem_escrituras == n0 && identical(list(snap("AR"), snap("AP")), antes))
ok("11.10 --prueba compara la foto actual con el ERP de hoy", any(grepl("foto actual", x$salida, fixed = TRUE)) &&
     any(grepl("720", x$salida, fixed = TRUE)))
url_ng <- Sys.getenv("SAP_NG_URL"); Sys.setenv(SAP_NG_URL = sprintf("http://127.0.0.1:%d/NG/b1s/v1", httpuv::randomPort()))
x <- correr("--cliente", CID, "--usuario", "operador.ficticio", "--produccion")
ok("11.11 sin conexión (VPN abajo): código 2, mensaje claro y nada escrito", identical(as.integer(x$codigo), 2L) &&
     any(grepl("VPN", x$salida, fixed = TRUE)) && identical(list(snap("AR"), snap("AP")), antes))
Sys.setenv(SAP_NG_URL = url_ng)
configurar(NG = list(AR = 700, AP = 30, modo = "ok"), NT = list(AR = 40, AP = 5, modo = "falla500"))
x <- correr("--cliente", CID, "--usuario", "operador.ficticio", "--produccion")
ok("11.12 una empresa que falla: código 4 y las fotos intactas", identical(as.integer(x$codigo), 4L) &&
     identical(list(snap("AR"), snap("AP")), antes))
ok("11.13 y el candado se soltó igual", isTRUE(.mem[[paste0(CID, "/erp_actualizacion_lock.rds")]]$liberado))
Sys.unsetenv("HOPDESK_PRODUCTION")

cat("== 12. Carga limpia ==\n")
limpio <- callr::r(function(wd) {
  setwd(wd); options(hopdesk.sin_s3 = TRUE)
  sys.source("scripts/actualizar_erp.R", envir = globalenv())
  suppressMessages(erp_cargar())
  all(vapply(c("fetch_all_companies", "save_sap_snapshot", "log_action", "t87_staff_firma", ".read_username_index",
               ".company_map_from_empresas", ".queue_stage_entry", "decrypt_secret", "parse_sap_date", "tr"),
             exists, logical(1)))
}, args = list(wd = getwd()))
ok("12.1 la lista de archivos del script carga sola en un R limpio, con todo lo que usa", isTRUE(limpio))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
sap$parar()
if (.fail > 0L) quit(status = 1L)
