# =============================================================================
# tests/test_t80b_correo_vigia.R
# T80b: "the weekly email has not gone out", inside HopDesk (no email).
#   - the rule (correo_vigia): windows Monday 17:00 / Friday 17:50 Mexico to
#     Sunday; a missing row alerts; a skip by design never does; failures do;
#   - who sees it: dev and admin by default, HopDesk staff; finance and
#     compras not; never another client's;
#   - the client never sees a technical detail; staff see the reason;
#   - Panorama passes it through avisos_visibles().
# The job's side (corridas.rds written per real run) is in
# tests/test_t80b_job_corridas.R.
#
# Run from project root:  Rscript tests/test_t80b_correo_vigia.R
# =============================================================================
cat("=== test_t80b_correo_vigia ===\n\n")
source("tests/helpers/s3_memoria.R")
suppressPackageStartupMessages(library(shiny))
source("R/i18n.R")
source("R/tier_registry.R")
source("R/permisos.R")
source("R/zona.R")
source("R/avisos.R")
source("R/avisos_correo_vigia.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
mx <- function(s) as.POSIXct(s, tz = .correo_vigia_tz())
SEM <- "2026-W48"                       # Monday 2026-11-23 .. Sunday 2026-11-29
LUN_1659 <- mx("2026-11-23 16:59"); LUN_1700 <- mx("2026-11-23 17:00")
VIE_1749 <- mx("2026-11-27 17:49"); VIE_1750 <- mx("2026-11-27 17:50")
DOM_2359 <- mx("2026-11-29 23:59"); LUN_SIG <- mx("2026-11-30 17:30")
fila <- function(modo, resultado, semana = SEM, hora = LUN_1700)
  tibble::tibble(semana = semana, modo = modo, resultado = resultado, destinatarios = 2L,
                 enviados = 2L, fallidos = 0L, hora = as.POSIXct(hora, tz = "UTC"))
nada <- .schema_correo_corridas()
usuario <- function(tier) list(user = paste0("u_", tier), tier = tier, permisos = permisos_resueltos(tier))
staff_tier <- niveles_staff()[1]

cat("-- la regla --\n")
ok("la semana es la ISO del día de México (la misma que el job)",
   identical(correo_vigia_semana(as.Date("2026-11-23")), SEM) && identical(correo_vigia_semana(as.Date("2026-11-29")), SEM))
ok("lunes antes de las 17:00: nada; desde las 17:00 sin fila: 'no salió' del lunes",
   !nrow(correo_vigia(nada, LUN_1659)) && {
     a <- correo_vigia(nada, LUN_1700); nrow(a) == 1L && a$modo == "lunes" && a$estado == "no_salio" })
ok("la ventana sigue hasta el domingo 23:59 y se cierra con la semana",
   nrow(correo_vigia(nada, DOM_2359)) == 2L &&
     !any(correo_vigia(fila("lunes", "enviado", semana = SEM), LUN_SIG)$semana %in% SEM))
ok("viernes: antes de las 17:50 solo el lunes; desde las 17:50 también el viernes",
   identical(correo_vigia(nada, VIE_1749)$modo, "lunes") && setequal(correo_vigia(nada, VIE_1750)$modo, c("lunes", "viernes")))
ok("enviado no alerta; la fila de otra semana no cuenta",
   !nrow(correo_vigia(fila("lunes", "enviado"), LUN_1700)) &&
     nrow(correo_vigia(fila("lunes", "enviado", semana = "2026-W47"), LUN_1700)) == 1L)
ok("omitido por diseño no alerta: el cliente sin SAP, sin lunes, sin destinatarios",
   all(vapply(c("omitido_sin_sap", "omitido_sin_lunes", "sin_destinatarios"),
              function(r) !nrow(correo_vigia(fila("lunes", r), LUN_1700)), logical(1))))
ok("con_fallas = 'salió incompleto'; sin_envio, error, lectura fallida y SAP viejo = 'no salió'", {
  e <- function(r) correo_vigia(fila("lunes", r), LUN_1700)$estado
  identical(e("con_fallas"), "incompleto") &&
    all(vapply(c("sin_envio", "error", "omitido_lectura", "omitido_calculo", "omitido_destinatarios", "omitido_sap_viejo"),
               function(r) identical(e(r), "no_salio"), logical(1)))
})
ok("decide la corrida más reciente de la semana (una tardía que sí salió apaga el aviso)", {
  dos <- dplyr::bind_rows(fila("lunes", "error", hora = mx("2026-11-23 08:00")),
                          fila("lunes", "enviado", hora = mx("2026-11-23 19:30")))
  !nrow(correo_vigia(dos, LUN_1700 + 3 * 3600))
})
ok("el archivo del sábado no se vigila", !("archivo" %in% names(CORREO_VIGIA_MODOS)))

cat("\n-- quién lo ve --\n")
ok("de serie: dev y admin sí; finanzas y compras no; el staff sí", {
  t <- function(x) puede(usuario(x), "can_receive_email_job_notices")
  compras <- Filter(function(x) identical(nivel_calendario(x), "AP"), niveles_cliente())
  t(niveles_cliente()[1]) && t(niveles_cliente()[2]) &&
    !any(vapply(setdiff(niveles_cliente(), niveles_cliente()[1:2]), t, logical(1))) &&
    length(compras) > 0 && !any(vapply(compras, t, logical(1))) &&
    all(vapply(niveles_staff(), t, logical(1)))
})
ok("el permiso es de cliente (no interno) y tiene nombre en los dos idiomas", {
  !isTRUE(PERMISOS$can_receive_email_job_notices$interno) &&
    !identical(tr("permisos.can_receive_email_job_notices", idioma = "es"), "permisos.can_receive_email_job_notices") &&
    !identical(tr("permisos.can_receive_email_job_notices", idioma = "en"), "permisos.can_receive_email_job_notices")
})

s3_memoria_reset()
assign(.s3_key(CORREO_VIGIA_KEY, client_id = "beta"), fila("lunes", "enviado"), envir = .mem)
assign(.s3_key(CORREO_VIGIA_KEY, client_id = "gama"), fila("lunes", "error"), envir = .mem)
# acme: no corridas.rds at all (the job never got to it).
ve <- function(cid, tier, staff = FALSE, ahora = LUN_1700) {
  u <- usuario(tier)
  avisos_visibles(cid, u, df = correo_vigia_avisos(cid, u, staff = staff, ahora = ahora))
}
DEV <- niveles_cliente()[1]; ADMIN <- niveles_cliente()[2]
ok("acme sin fila: su dev y su admin ven 'El correo semanal del lunes no ha salido'", {
  a <- ve("acme", DEV); b <- ve("acme", ADMIN)
  nrow(a) == 1L && nrow(b) == 1L && identical(a$titulo, tr("avisos.correo_vigia.no_salio", modo = tr("avisos.correo_vigia.modo.lunes")))
})
ok("finanzas de acme no lo ve", !nrow(ve("acme", setdiff(niveles_cliente(), c(DEV, ADMIN))[1])))
ok("beta (sí salió) no ve nada: ningún cliente ve el aviso de otro", !nrow(ve("beta", DEV)) && !nrow(ve("beta", ADMIN)))
ok("fuera de la ventana no se muestra nada (lunes 16:59, sin fila)",
   !nrow(correo_vigia_avisos("acme", usuario(DEV), ahora = LUN_1659)))
ok("sin el permiso no se lee nada (ni dentro de la ventana)", {
  .mem_falla <<- .s3_key(CORREO_VIGIA_KEY, client_id = "acme")
  r <- !nrow(correo_vigia_avisos("acme", usuario(setdiff(niveles_cliente(), c(DEV, ADMIN))[1]), ahora = LUN_1700))
  .mem_falla <<- character(); r
})
ok("una lectura fallida no muestra nada (no puede saber)", {
  .mem_falla <<- .s3_key(CORREO_VIGIA_KEY, client_id = "gama")
  r <- !nrow(ve("gama", DEV)); .mem_falla <<- character(); r
})

cat("\n-- el cliente nunca ve un detalle técnico (T72-D) --\n")
ok("gama (error): el cliente ve el título y el cuerpo, sin motivo, sin 'error', GitHub, Lambda ni archivos", {
  a <- ve("gama", ADMIN); txt <- paste(a$titulo, a$cuerpo)
  nrow(a) == 1L && !grepl("error|omitido|github|lambda|corridas|rds|staff|motivo|reason", txt, ignore.case = TRUE)
})
ok("el staff dentro del cliente sí ve el motivo", {
  a <- ve("gama", staff_tier, staff = TRUE)
  nrow(a) == 1L && grepl("error", a$cuerpo, fixed = TRUE)
})
ok("un dev del cliente con staff = FALSE nunca recibe el motivo, aunque tenga el permiso",
   !grepl("error", ve("gama", DEV, staff = FALSE)$cuerpo, fixed = TRUE))

cat("\n-- Panorama --\n")
ok("Panorama une el aviso calculado pasando por avisos_visibles() y le dice si es staff", {
  x <- sub("#.*$", "", readLines("R/panorama_module.R", warn = FALSE, encoding = "UTF-8"))
  any(grepl("avisos_visibles(c, u, df = correo_vigia_avisos(c, u, staff = staff))", x, fixed = TRUE)) &&
    any(grepl("shared$current_user_info()$is_staff", x, fixed = TRUE))
})
ok("nada se escribe desde la lectura (el aviso no se guarda)", {
  esc <- .mem_escrituras; invisible(ve("acme", DEV)); invisible(ve("gama", staff_tier, staff = TRUE))
  .mem_escrituras == esc
})

s3_memoria_restaurar()
cat(sprintf("\nTOTAL: %d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
