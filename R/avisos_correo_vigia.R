# =============================================================================
# R/avisos_correo_vigia.R -- T80b: "the weekly email has not gone out"
#
# The weekly email job (scripts/avisos_job.R) leaves one row per real run,
# client, week and mode in <cliente>/avisos/corridas.rds: what came of it.
# Panorama reads it and, from Monday 17:00 / Friday 17:50 Mexico until that
# week's Sunday, shows an aviso when the row is MISSING (the job never ran
# for the client: the alert cannot depend on the job running), when the run
# had failed sends, or when it skipped the client for an unexpected reason
# (an error, a failed read, stale SAP data: Luis wants to know that same
# day). A skip by design never alerts: the client has no SAP snapshot at
# all (AR or AP missing -- a client without an ERP would carry an aviso
# forever), no Monday for Friday, nobody subscribed. The job skips whole
# clients, never one empresa: an empresa without SAP data inside a client
# (PL in networks) or one left in SAP (NOS) does not skip the client.
#
# The aviso is computed when read and never stored: nothing is written from
# a screen, and it disappears on its own once the run arrives, late or not.
# Who sees it: whoever holds can_receive_email_job_notices (dev and admin by
# default, HopDesk staff). The client sees only "has not gone out" / "went
# out incomplete" (T72-D: never a technical detail); the reason
# (omitido_<x>, error) is added for staff only.
#
# The Saturday archive (modo "archivo") sends no email and is not watched.
# Used by the job (AVISOS_JOB_ARCHIVOS: the key and the schema) and by
# Panorama (correo_vigia_avisos()).
# =============================================================================

CORREO_VIGIA_KEY <- "avisos/corridas.rds"
# Mexico time: the job's (UMBRAL_TZ) and the app's default zone (R/zona.R).
.correo_vigia_tz <- function() ZONA_OMISION
CORREO_VIGIA_PERMISO <- "can_receive_email_job_notices"

# When each mode's alert window opens (Mexico time; ISO weekday 1 = Monday).
# It closes with the week, on Sunday 23:59:59.
CORREO_VIGIA_MODOS <- list(lunes   = list(dia = 1L, hora = 17L, minuto = 0L),
                           viernes = list(dia = 5L, hora = 17L, minuto = 50L))

# Results that never alert: the email went out, or the job skipped the
# client by design.
CORREO_VIGIA_SIN_ALERTA <- c("enviado", "sin_destinatarios",
                             "omitido_sin_sap", "omitido_sin_lunes")

.schema_correo_corridas <- function() tibble::tibble(
  semana        = character(),   # ISO week of the run's Mexico day, "2026-W48"
  modo          = character(),   # "lunes" | "viernes"
  resultado     = character(),   # enviado | con_fallas | sin_envio | sin_destinatarios | omitido_<x> | error
  destinatarios = integer(),
  enviados      = integer(),
  fallidos      = integer(),
  hora          = as.POSIXct(character(), tz = "UTC")
)

# The job's week (umbral_semana() in R/avisos_umbrales.R, the same format;
# that file is not loaded by the app).
correo_vigia_semana <- function(fecha) format(as.Date(fecha), "%G-W%V")

# Pure: which modes alert at `ahora`, from the client's run rows.
# A tibble (modo, estado = "no_salio" | "incompleto", motivo, semana).
correo_vigia <- function(corridas, ahora = Sys.time()) {
  vacio <- tibble::tibble(modo = character(), estado = character(), motivo = character(), semana = character())
  lt  <- as.POSIXlt(ahora, tz = .correo_vigia_tz())
  dia <- as.integer(format(lt, "%u"))
  min_dia <- lt$hour * 60L + lt$min
  hoy <- as.Date(format(lt, "%Y-%m-%d"))
  semana <- correo_vigia_semana(hoy)
  corridas <- if (is.data.frame(corridas)) corridas else .schema_correo_corridas()
  out <- vacio
  for (m in names(CORREO_VIGIA_MODOS)) {
    v <- CORREO_VIGIA_MODOS[[m]]
    abierta <- dia > v$dia || (dia == v$dia && min_dia >= v$hora * 60L + v$minuto)
    if (!abierta) next
    f <- corridas[corridas$semana %in% semana & corridas$modo %in% m, , drop = FALSE]
    if (!nrow(f)) {
      out <- dplyr::bind_rows(out, tibble::tibble(modo = m, estado = "no_salio", motivo = "sin_corrida", semana = semana))
      next
    }
    r <- f$resultado[order(f$hora, na.last = FALSE)][nrow(f)]   # the latest run decides
    if (is.na(r) || r %in% CORREO_VIGIA_SIN_ALERTA) next
    estado <- if (identical(r, "con_fallas")) "incompleto" else "no_salio"
    out <- dplyr::bind_rows(out, tibble::tibble(modo = m, estado = estado, motivo = r, semana = semana))
  }
  out
}

# Is any mode's window open at `ahora`? (Outside them nothing is read.)
correo_vigia_abierta <- function(ahora = Sys.time()) {
  lt <- as.POSIXlt(ahora, tz = .correo_vigia_tz())
  dia <- as.integer(format(lt, "%u")); min_dia <- lt$hour * 60L + lt$min
  any(vapply(CORREO_VIGIA_MODOS, function(v)
    dia > v$dia || (dia == v$dia && min_dia >= v$hora * 60L + v$minuto), logical(1)))
}

# Sunday 23:59:59 Mexico of the week of `ahora`, in UTC: when the aviso ends.
.correo_vigia_vence <- function(ahora) {
  hoy <- as.Date(format(as.POSIXlt(ahora, tz = .correo_vigia_tz()), "%Y-%m-%d"))
  domingo <- hoy + (7L - as.integer(format(hoy, "%u")))
  as.POSIXct(paste(format(domingo), "23:59:59"), tz = .correo_vigia_tz())
}

# The alerts as avisos rows (the shape of R/avisos.R's .schema_avisos), for
# Panorama to pass through avisos_visibles(client_id, usuario, df = <these>):
# the permission, the tiers and the expiry are checked there, as for any
# aviso. `staff` adds the reason to the body. Without the permission, or
# outside the windows, it reads nothing; a failed read shows nothing (it
# cannot tell) and says so in the log.
correo_vigia_avisos <- function(client_id, usuario, staff = FALSE, ahora = Sys.time()) {
  vacio <- .schema_avisos()
  if (is.null(usuario) || !puede(usuario, "can_receive_email_job_notices")) return(vacio)
  if (!correo_vigia_abierta(ahora)) return(vacio)
  corridas <- tryCatch(leer_tabla_para_escribir(CORREO_VIGIA_KEY, client_id, .schema_correo_corridas),
                       error = function(e) {
                         message("[correo_vigia] corridas.rds unreadable: no aviso this time")
                         NULL
                       })
  if (is.null(corridas)) return(vacio)
  al <- correo_vigia(corridas, ahora)
  if (!nrow(al)) return(vacio)
  ahora_utc <- as.POSIXct(ahora, tz = "UTC")
  niveles <- .json(c(niveles_cliente(), niveles_staff()))
  filas <- lapply(seq_len(nrow(al)), function(i) {
    modo <- tr(paste0("avisos.correo_vigia.modo.", al$modo[i]))
    cuerpo <- tr("avisos.correo_vigia.cuerpo")
    if (isTRUE(staff)) cuerpo <- paste(cuerpo, tr("avisos.correo_vigia.motivo_staff", motivo = al$motivo[i]))
    tibble::tibble(
      id = paste("correo", al$semana[i], al$modo[i], sep = "|"), tipo = "automatico", clase = NA_character_,
      titulo = tr(paste0("avisos.correo_vigia.", al$estado[i]), modo = modo),
      cuerpo = cuerpo, severidad = "atencion", niveles = niveles, usuarios = NA_character_,
      permiso = CORREO_VIGIA_PERMISO, clave = paste("correo", al$semana[i], al$modo[i], sep = "|"),
      contador = 1L, enlace = NA_character_, creado_por = "sistema",
      creado = ahora_utc, actualizado = ahora_utc, vence = as.POSIXct(.correo_vigia_vence(ahora), tz = "UTC"),
      retirado = FALSE, retirado_por = NA_character_, retirado_en = as.POSIXct(NA, tz = "UTC"),
      hechos = .json(character()))
  })
  dplyr::bind_rows(vacio, dplyr::bind_rows(filas))
}
