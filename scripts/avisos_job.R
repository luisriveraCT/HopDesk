# =============================================================================
# scripts/avisos_job.R -- the weekly Panorama emails (frente D)
#
#   Rscript --vanilla scripts/avisos_job.R            (from the repo root)
#
# Environment (the workflow, .github/workflows/avisos.yml, sets it):
#   AVISOS_MODO     lunes | viernes | archivo
#   AVISOS_PRUEBA   "true" (the default; anything but "false" counts as true):
#                   nothing is sent; each email's HTML goes to
#                   <cliente>/avisos/prueba/<fecha>_<modo>_<n>.html instead
#   AVISOS_FECHA    optional AAAA-MM-DD, the day to run as (tests, re-runs)
#   S3_BUCKET, AWS_REGION and the role's credentials; RESEND_* to send;
#   HOPDESK_APP_URL for the button.
#
# For every active client in hd-admin/client_registry.rds:
#   lunes    the week's rows (R/avisos_datos.R) -> metricas.rds and the umbral
#            avisos (R/avisos_umbrales.R) -> semana.rds -> one email per
#            recipient (R/avisos_correos.R)
#   viernes  semana.rds + the rows now -> logros and radar (R/avisos_logros.R)
#            -> one email per recipient
#   archivo  avisos expired over 30 days -> <cliente>/avisos/archivo/AAAA-MM.rds
#            (R/avisos_archivo.R); no email, AVISOS_PRUEBA does not apply
# A client that cannot be read whole is left out (avisos_cliente_datos()).
# Writes only under <cliente>/avisos/ (the role allows nothing else).
#
# Never resends: <cliente>/avisos/correos_enviados.rds holds one row per
# week, mode and recipient, written right after each send, so a re-run (or
# a run that died halfway) only sends what is missing.
#
# THE LOG IS PUBLIC (a public repo's Actions log). It prints only
# "evento=<count>" lines from JOB_EVENTOS: never a client, a name, an email,
# an amount or an error message. Everything else (messages, warnings, the
# app's own prints) is silenced. A client's error goes, with its text, to
# <cliente>/avisos/job_errores.rds (private bucket) and only counts in the
# log. tests/test_avisos_job_logs.R fails if this file prints anything else.
# =============================================================================

AVISOS_JOB_ARCHIVOS <- c(
  "R/i18n.R", "R/tier_registry.R", "R/permisos.R", "R/persistence.R", "R/erp_connector_registry.R",
  "R/data_pipeline.R", "R/pasivos_engine.R", "R/pasivos_schemas.R", "R/pasivos_calendar_glue.R",
  "R/pasivos_persistence.R", "R/bancos_persistence.R", "R/cashflow_export_module.R", "R/fx_banxico.R",
  "R/email_service.R", "R/avisos.R", "R/panorama_datos.R",
  "R/avisos_umbrales.R", "R/avisos_correos.R", "R/avisos_datos.R", "R/avisos_logros.R", "R/avisos_archivo.R")

# The only things the log may count.
JOB_EVENTOS <- c(
  "clientes", "clientes_ok", "omitido_lectura", "omitido_sin_sap", "omitido_sap_viejo",
  "omitido_calculo", "omitido_sin_lunes", "omitido_destinatarios", "error_cliente",
  "destinatarios", "enviado", "fallido", "ya_enviado", "prueba_html", "alertas_umbral",
  "archivados", "sin_llave_nueva")

.job <- new.env(parent = emptyenv())
job_reiniciar <- function() .job$cuenta <- stats::setNames(integer(length(JOB_EVENTOS)), JOB_EVENTOS)
job_contar <- function(evento, n = 1L) {
  if (!evento %in% JOB_EVENTOS) stop("evento de log desconocido", call. = FALSE)
  .job$cuenta[[evento]] <- .job$cuenta[[evento]] + as.integer(n)
  invisible(NULL)
}
# The whole log: one "evento=<integer>" line per event, fixed order.
job_log <- function() sprintf("%s=%d", names(.job$cuenta), as.integer(.job$cuenta))

# ── Loading the app's code without the app ───────────────────────────────────
# Only pure files plus the S3 key lists (R/global.R is not sourced: it takes
# a lock, syncs users and starts the app's state).
job_cargar <- function() {
  suppressPackageStartupMessages({
    for (p in c("dplyr", "tibble", "tidyr", "stringr", "lubridate", "jsonlite", "aws.s3", "httr", "uuid", "htmltools"))
      library(p, character.only = TRUE)
  })
  assign("%||%", function(a, b) if (is.null(a)) b else a, envir = globalenv())
  for (e in parse("R/global.R", keep.source = FALSE))
    if (is.call(e) && identical(e[[1]], as.name("<-")) &&
        as.character(e[[2]])[1] %in% c("S3_KEYS", "S3_KEYS_CRITICAL"))
      eval(e, globalenv())
  for (f in AVISOS_JOB_ARCHIVOS) sys.source(f, envir = globalenv())
  invisible(TRUE)
}

# ── Small pieces ─────────────────────────────────────────────────────────────

.ENVIADOS_KEY <- "avisos/correos_enviados.rds"
.ERRORES_KEY  <- "avisos/job_errores.rds"

.schema_enviados <- function() tibble::tibble(
  clave = character(), semana = character(), modo = character(), usuario = character(),
  enviado = as.POSIXct(character(), tz = "UTC"), resend_id = character())
.schema_errores <- function() tibble::tibble(
  cuando = as.POSIXct(character(), tz = "UTC"), modo = character(), mensaje = character())

job_clave <- function(fecha, modo, usuario) paste(umbral_semana(fecha), modo, usuario, sep = ":")

job_clientes <- function() {
  reg <- .s3_leer_para_escribir("client_registry.rds", "hd-admin")
  if (!is.data.frame(reg) || !nrow(reg)) stop("client registry unreadable or empty", call. = FALSE)
  cid <- tolower(trimws(reg$client_id[reg$status %in% "active"]))
  sort(unique(cid[!is.na(cid) & nzchar(cid) & cid != "hd-admin"]))
}

job_error <- function(cid, modo, e) {
  job_contar("error_cliente")
  tryCatch(modificar_tabla(.ERRORES_KEY, cid, .schema_errores, function(df) dplyr::bind_rows(df,
             tibble::tibble(cuando = as.POSIXct(Sys.time(), tz = "UTC"), modo = modo,
                            mensaje = substr(conditionMessage(e), 1L, 500L)))),
           error = function(e2) NULL)
}

job_guardar_prueba <- function(cid, nombre, html) {
  aws.s3::put_object(file = charToRaw(enc2utf8(html)), object = .s3_key(paste0("avisos/prueba/", nombre), cid),
                     bucket = .s3_bucket(), headers = list(`Content-Type` = "text/html; charset=utf-8"))
}

# Who gets the email, read strictly: users, the client's tier settings and
# preferences. NULL when any of them cannot be read. usuarios.rds is read
# raw, not with auth_load_usuarios(): that one backfills account_code and
# client_id and WRITES usuarios.rds, outside avisos/ (correo_destinatarios()
# fills the columns it needs).
job_destinatarios <- function(cid) {
  tryCatch({
    u    <- .s3_leer_para_escribir(S3_KEYS$usuarios, cid)
    if (!is.null(u) && !is.data.frame(u)) stop("usuarios.rds is not a table", call. = FALSE)
    conf <- .s3_leer_para_escribir(S3_KEYS$tiers_config, cid)
    pref <- .s3_leer_para_escribir(.PREFERENCIAS_KEY, cid)
    correo_destinatarios(u, if (is.list(conf)) conf else NULL, if (is.data.frame(pref)) pref else NULL)
  }, error = function(e) NULL)
}

# One email per recipient: `construir(dest)` -> list(asunto, html).
job_enviar <- function(cid, modo, fecha, dests, construir, prueba) {
  enviados <- leer_tabla_para_escribir(.ENVIADOS_KEY, cid, .schema_enviados)
  for (i in seq_len(nrow(dests))) {
    dest <- dests[i, ]
    clave <- job_clave(fecha, modo, dest$usuario)
    if (clave %in% enviados$clave) { job_contar("ya_enviado"); next }
    correo <- construir(dest)
    if (prueba) {
      job_guardar_prueba(cid, sprintf("%s_%s_%d.html", format(fecha), modo, i), correo$html)
      job_contar("prueba_html")
      next
    }
    r <- send_email(dest$email, correo$asunto, correo$html, queue_on_fail = FALSE)
    if (!isTRUE(r)) { job_contar("fallido"); next }
    modificar_tabla(.ENVIADOS_KEY, cid, .schema_enviados, function(df) dplyr::bind_rows(df, tibble::tibble(
      clave = clave, semana = umbral_semana(fecha), modo = modo, usuario = dest$usuario,
      enviado = as.POSIXct(Sys.time(), tz = "UTC"), resend_id = as.character(attr(r, "resend_id") %||% NA))))
    job_contar("enviado")
  }
}

# ── Per client ───────────────────────────────────────────────────────────────

job_lunes <- function(cid, hoy, ahora, prueba, url) {
  x <- avisos_cliente_datos(cid, ahora)
  if (!x$ok) return(job_contar(x$motivo))
  sh <- x$shared
  ar <- x$df[x$df$Tipo %in% "AR", , drop = FALSE]
  serie <- umbral_serie_guardar(cid, umbral_metricas(ar, hoy, sh$interco_v2(), sh$company_map()))
  alertas <- umbral_evaluar(serie, hoy)
  job_contar("alertas_umbral", umbral_publicar(cid, alertas, hoy))
  d <- correo_datos_lunes(x$df, sh, umbral_lunes(hoy), serie, alertas, x$sap_fecha, url = url)
  correo_semana_guardar(cid, x$df, d)
  dests <- job_destinatarios(cid)
  if (is.null(dests)) return(job_contar("omitido_destinatarios"))
  job_contar("destinatarios", nrow(dests))
  avisos_df <- .normalizar(.avisos_leer_para_escribir(cid), .schema_avisos)
  job_enviar(cid, "lunes", hoy, dests, function(dest) {
    di <- d; di$tambien <- correo_tambien(avisos_df, dest, cid)
    list(asunto = correo_lunes_asunto(di, dest$idioma), html = correo_lunes_html(di, dest))
  }, prueba)
  job_contar("clientes_ok")
}

job_viernes <- function(cid, hoy, ahora, prueba, url) {
  x <- avisos_cliente_datos(cid, ahora)
  if (!x$ok) return(job_contar(x$motivo))
  s <- correo_semana_leer(cid, hoy)
  if (!s$ok) return(job_contar(s$motivo))
  serie <- leer_tabla_para_escribir(.METRICAS_KEY, cid, .schema_metricas)
  dv <- correo_datos_viernes(s$semana, x$df, x$shared, hoy, x$sap_fecha, url = url)
  dv$items <- correo_items_viernes(serie, dv$metricas, dv, x$df, s$semana$moneda_base %||% "MXN")
  dests <- job_destinatarios(cid)
  if (is.null(dests)) return(job_contar("omitido_destinatarios"))
  job_contar("destinatarios", nrow(dests))
  job_enviar(cid, "viernes", hoy, dests, function(dest)
    list(asunto = correo_viernes_asunto(dv, dest$idioma), html = correo_viernes_html(dv, dest)), prueba)
  job_contar("clientes_ok")
}

job_archivo <- function(cid, ahora) {
  r <- avisos_archivar(cid, ahora)
  if (!is.na(r$motivo)) return(job_contar(r$motivo))
  job_contar("archivados", r$archivados)
  job_contar("clientes_ok")
}

# ── Main ─────────────────────────────────────────────────────────────────────

# Runs the job silently and returns the log lines. stop()s only when the run
# cannot start (bad mode, unreadable client registry).
job_correr <- function(modo = Sys.getenv("AVISOS_MODO"), prueba = !identical(tolower(Sys.getenv("AVISOS_PRUEBA")), "false"),
                       hoy = NULL, ahora = Sys.time()) {
  if (!modo %in% c("lunes", "viernes", "archivo")) stop("AVISOS_MODO must be lunes, viernes or archivo", call. = FALSE)
  hoy <- as.Date(hoy %||% (if (nzchar(Sys.getenv("AVISOS_FECHA"))) Sys.getenv("AVISOS_FECHA")
                           else format(ahora, tz = UMBRAL_TZ, "%Y-%m-%d")))
  url <- tryCatch(app_public_url(), error = function(e) "")
  job_reiniciar()
  callar <- function(expr) withCallingHandlers(expr,
    message = function(m) invokeRestart("muffleMessage"),
    warning = function(w) invokeRestart("muffleWarning"))
  utils::capture.output(callar({
    clientes <- job_clientes()
    job_contar("clientes", length(clientes))
    for (cid in clientes)
      tryCatch(switch(modo, lunes = job_lunes(cid, hoy, ahora, prueba, url),
                            viernes = job_viernes(cid, hoy, ahora, prueba, url),
                            archivo = job_archivo(cid, ahora)),
               error = function(e) job_error(cid, modo, e))
  }), type = "output")
  job_log()
}

# Loading is inside the same guard: a failed library() or sys.source() would
# otherwise print its error (and a package's startup warnings) to the log.
if (sys.nframe() == 0L) {
  lineas <- tryCatch(suppressWarnings(suppressMessages({ job_cargar(); job_correr() })),
                     error = function(e) { cat("job_no_arranco=1\n"); quit(status = 1L) })
  cat(lineas, sep = "\n")
  if (.job$cuenta[["error_cliente"]] > 0L || .job$cuenta[["fallido"]] > 0L) quit(status = 1L)
}
