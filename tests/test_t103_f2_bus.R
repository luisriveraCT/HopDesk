# =============================================================================
# tests/test_t103_f2_bus.R
# T103 F2: an ERP snapshot saved anywhere reaches the sessions already open.
#   1  bump_sync_version() merges S3's stamp first: two processes bumping
#      different tables lose nothing and nothing goes back; a fresh process
#      keeps counting up; a stamp that cannot be read is never read as empty
#      (T72) -- nothing is uploaded and the bump stays in memory
#   2  avisar_foto_erp(): registers the signal outside the app and bumps it
#   3  fetch_all_companies() signals only after a snapshot that WAS saved
#   4  foto_erp_releer(): new data -> change; the same -> nothing; a failed
#      read -> keep everything; a ledger with no file -> keep the session's
#   5  app.R: the signal is registered and the observer's gates (T69b/M6,
#      staff at home, a fetch running) come before any read
# In-memory S3 (tests/helpers/s3_memoria.R) and a fake Service Layer
# (tests/helpers/sap_falso.R): invented data only.
#
# Run from project root:  Rscript tests/test_t103_f2_bus.R
# =============================================================================
cat("=== test_t103_f2_bus ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr, detalle = NULL) {
  r <- tryCatch(isTRUE(expr), error = function(e) { message("  error: ", conditionMessage(e)); FALSE })
  if (r) { .pass <<- .pass + 1L; cat(" PASS:", label, "\n") }
  else   { .fail <<- .fail + 1L; cat(" FAIL:", label, "\n"); if (!is.null(detalle)) cat("       ", detalle, "\n") }
}
source("tests/helpers/s3_memoria.R")
source("tests/helpers/sap_falso.R")
sys.source("scripts/actualizar_erp.R", envir = globalenv())
suppressMessages(erp_cargar())          # S3_KEYS(_CRITICAL), persistence, sync_bus, sap_api...
s3_memoria_reinstalar()
q <- function(expr) invisible(suppressWarnings(suppressMessages(expr)))

CID   <- "demo"
SELLO <- paste0(CID, "/sync_versions.rds")
sello <- function() .mem[[SELLO]]
proceso <- function(versiones) assign(".sync_versions", versiones, envir = .GlobalEnv)   # "another R process"
for (n in c("tabla_a", "tabla_b")) register_synced(n, paste0(n, ".rds"), function(client_id = NULL) NULL)

cat("== 1. El sello se une antes de subir ==\n")
s3_memoria_reset()
proceso(list(tabla_a = 0L, tabla_b = 0L))
for (i in 1:3) q(bump_sync_version("tabla_a", client_id = CID))
memoria_a <- .GlobalEnv$.sync_versions
ok("1.1 el proceso A sube tabla_a a 3", identical(sello()$tabla_a, 3L))
proceso(list())                                  # B: a fresh process (a script, a job)
q(bump_sync_version("tabla_b", client_id = CID))
ok("1.2 un proceso nuevo que avisa tabla_b no borra tabla_a", identical(sello()$tabla_a, 3L) && identical(sello()$tabla_b, 1L))
proceso(memoria_a)                               # back to A, which never saw B's bump
q(bump_sync_version("tabla_a", client_id = CID))
ok("1.3 A no regresa tabla_b a 0 aunque su memoria diga 0", identical(sello()$tabla_b, 1L) && identical(sello()$tabla_a, 4L))
proceso(list())                                  # C: fresh again
q(bump_sync_version("tabla_a", client_id = CID))
ok("1.4 desde un proceso nuevo la versión sigue subiendo (5), no vuelve a 1", identical(sello()$tabla_a, 5L))
ok("1.5 y la memoria del proceso queda unida con S3", identical(.GlobalEnv$.sync_versions$tabla_b, 1L))
antes <- sello(); .mem_falla <- SELLO
r <- q(bump_sync_version("tabla_b", client_id = CID))
ok("1.6 sello ilegible (T72): devuelve FALSE y no sube nada", identical(r, FALSE) && identical(sello(), antes))
ok("1.7 ...pero el aviso queda en la memoria del proceso (sus sesiones lo ven)",
   identical(.GlobalEnv$.sync_versions$tabla_b, 2L))
.mem_falla <- character()
s3_memoria_reset(); proceso(list())
ok("1.8 sin sello todavía (404) se crea normalmente", isTRUE(q(bump_sync_version("tabla_a", client_id = CID))) &&
     identical(sello()$tabla_a, 1L))
.mem[[SELLO]] <- list(tabla_a = 9L, raro = "no es un número")
ok("1.9 una entrada rara en el sello no rompe el bump", isTRUE(q(bump_sync_version("tabla_a", client_id = CID))) &&
     identical(sello()$tabla_a, 10L))

cat("== 2. avisar_foto_erp() ==\n")
s3_memoria_reset(); proceso(list())
.SYNC_REGISTRY[["erp_foto_db"]] <- NULL
.mem[[SELLO]] <- list(moves_db = 7L)
ok("2.1 fuera de la app registra la señal y avisa", isTRUE(q(avisar_foto_erp(CID))) &&
     "erp_foto_db" %in% names(.SYNC_REGISTRY) && identical(sello()$erp_foto_db, 1L))
ok("2.2 sin tocar las demás versiones", identical(sello()$moves_db, 7L))
proceso(list())
q(avisar_foto_erp(CID))
ok("2.3 la segunda corrida (otro proceso) sube a 2", identical(sello()$erp_foto_db, 2L))

cat("== 3. fetch_all_companies() avisa solo si guardó ==\n")
sap <- sap_falso_iniciar(list(NG = list(AR = 12, modo = "ok")))
do.call(Sys.setenv, stats::setNames(list(sap$url_de("NG"), "BD_FICTICIA", "u", "p"),
                                    paste0("SAP_NG", c("_URL", "_COMPANY", "_USER", "_PASSWORD"))))
cmap <- c(NG = "Ficticia Norte")
s3_memoria_reset(); proceso(list())
q(fetch_all_companies("AR", cmap, client_id = CID, guardar = FALSE))
ok("3.1 --prueba (guardar = FALSE): sin señal", is.null(sello()$erp_foto_db))
.mem_falla_escritura <- paste0(CID, "/sap_snapshot_AR.rds")
q(fetch_all_companies("AR", cmap, client_id = CID))
ok("3.2 guardado fallido: sin señal", is.null(sello()$erp_foto_db) && !isTRUE(sap_ultimo_resumen("AR")$avisado))
.mem_falla_escritura <- character()
q(fetch_all_companies("AR", cmap, client_id = CID))
ok("3.3 guardado bueno: señal arriba y el resumen lo dice", identical(sello()$erp_foto_db, 1L) &&
     isTRUE(sap_ultimo_resumen("AR")$avisado))
sap$parar()

cat("== 4. foto_erp_releer() ==\n")
s3_memoria_reset()
ar <- data.frame(Empresa = "Ficticia Norte", Documento = c("1", "2"))
ap <- data.frame(Empresa = "Ficticia Norte", Documento = "9")
t_ar <- as.POSIXct("2026-11-25 10:00:00", tz = "UTC"); t_ap <- as.POSIXct("2026-11-25 10:05:00", tz = "UTC")
.mem[[paste0(CID, "/sap_snapshot_AR.rds")]] <- list(data = ar, saved_at = t_ar)
.mem[[paste0(CID, "/sap_snapshot_AP.rds")]] <- list(data = ap, saved_at = t_ap)
viejo <- list(AR = ar[1, , drop = FALSE], AP = ap)
r <- q(foto_erp_releer(CID, viejo))
ok("4.1 foto nueva: cambio, con los datos y la hora de cada libro", isTRUE(r$leida) && isTRUE(r$cambio) &&
     identical(r$datos$AR, ar) && identical(r$info$AR$ts, t_ar) && identical(r$info$AP$ts, t_ap) && isFALSE(r$info$AR$is_live))
r <- q(foto_erp_releer(CID, list(AR = ar, AP = ap)))
ok("4.2 lo mismo que ya tiene (su propio eco): sin cambio", isTRUE(r$leida) && isFALSE(r$cambio))
.mem_falla <- paste0(CID, "/sap_snapshot_AP.rds")
r <- q(foto_erp_releer(CID, viejo))
ok("4.3 una lectura fallida: no se toca nada (nunca una copia vacía)", isFALSE(r$leida) && isFALSE(r$cambio) && is.null(r$datos))
.mem_falla <- character()
rm(list = paste0(CID, "/sap_snapshot_AP.rds"), envir = .mem)
r <- q(foto_erp_releer(CID, viejo))
ok("4.4 un libro sin archivo conserva el de la sesión", isTRUE(r$leida) && identical(r$datos$AP, ap) &&
     identical(r$datos$AR, ar) && is.null(r$info$AP))

cat("== 5. app.R ==\n")
app <- readLines("app.R", warn = FALSE, encoding = "UTF-8")
ok("5.1 la señal está en shared y registrada en el bus",
   any(grepl("erp_foto_db       = reactiveVal(0)", app, fixed = TRUE)) &&
     any(grepl('register_synced("erp_foto_db",     SENAL_FOTO_ERP', app, fixed = TRUE)))
i0 <- grep("observeEvent(shared$erp_foto_db(), {", app, fixed = TRUE)
fin <- i0 + grep("}, ignoreInit = TRUE)", app[i0:length(app)], fixed = TRUE)[1] - 1L
cuerpo <- app[i0:fin]
pos <- function(txt) grep(txt, cuerpo, fixed = TRUE)[1]
ok("5.2 un solo observer de la señal", length(i0) == 1L)
ok("5.3 la puerta T69b/M6 va antes de cualquier lectura",
   !is.na(pos("nivel_reactivo_permitido(tier_efectivo(), \"sap_data\")")) &&
     pos("nivel_reactivo_permitido(tier_efectivo(), \"sap_data\")") < pos("foto_erp_releer("))
ok("5.4 staff en casa y una carga en curso salen antes de leer",
   pos("is.null(jump_client_id())") < pos("foto_erp_releer(") && pos(".sap_running") < pos("foto_erp_releer("))
ok("5.5 sin cambio no escribe sap_data", pos("if (!isTRUE(r$cambio)) return()") < pos("sap_data(r$datos)"))
ok("5.6 ignoreInit: la sesión no relee al arrancar (la Fase 1 ya sembró)", any(grepl("}, ignoreInit = TRUE)", cuerpo, fixed = TRUE)))
ok("5.7 el aviso existe en los dos idiomas", nzchar(tr("tablas.datos_sap.aviso.foto_nueva", hora = "10:00")) &&
     "tablas.datos_sap.aviso.foto_nueva" %in% names(jsonlite::fromJSON("www/i18n/en.json")))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
