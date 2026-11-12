# =============================================================================
# R/avisos_archivo.R
# Frente D: the monthly archive of avisos. Avisos that expired more than
# AVISOS_ARCHIVO_DIAS ago leave <cliente>/avisos/avisos.rds for
# <cliente>/avisos/archivo/AAAA-MM.rds (the month they expired, Mexico time),
# so the live file the app reads on every screen stays small.
#
# Order, so nothing is lost and nothing is duplicated:
#   1. add them to the month's archive (strict read, only ids not there yet);
#   2. read the archive back (strict): only ids found there count as archived;
#   3. remove exactly those from avisos.rds, on S3's current copy, and only
#      if they are still expired by then (one renewed meanwhile stays).
# A failure at 1 or 2 stops before 3: avisos.rds keeps everything.
# Step 3 shrinks avisos.rds on purpose, so it passes permitir_reduccion.
#
# Only once the new key exists (R/avisos.R, D llave): the old key is never
# written. Never prints (the job's log is public).
# =============================================================================

AVISOS_ARCHIVO_DIAS <- 30L

.avisos_archivo_key <- function(mes) paste0("avisos/archivo/", mes, ".rds")

# list(archivados = <int>, motivo = NA | "sin_llave_nueva").
avisos_archivar <- function(client_id, ahora = Sys.time()) {
  cid <- .avisos_cid(client_id)
  if (!identical(.s3_existe(.AVISOS_KEY, cid), TRUE))
    return(list(archivados = 0L, motivo = "sin_llave_nueva"))
  limite  <- as.POSIXct(ahora, tz = "UTC") - AVISOS_ARCHIVO_DIAS * 86400
  vencido <- function(d) !is.na(d$vence) & d$vence < limite
  df <- .normalizar(.s3_leer_para_escribir(.AVISOS_KEY, cid), .schema_avisos)
  viejos <- df[vencido(df), , drop = FALSE]
  if (!nrow(viejos)) return(list(archivados = 0L, motivo = NA_character_))
  mes <- format(viejos$vence, "%Y-%m", tz = UMBRAL_TZ)
  hechos <- character()
  for (m in sort(unique(mes))) {
    filas <- viejos[mes == m, , drop = FALSE]
    key <- .avisos_archivo_key(m)
    modificar_tabla(key, cid, .schema_avisos, function(a) dplyr::bind_rows(a, filas[!filas$id %in% a$id, , drop = FALSE]))
    leido <- .normalizar(.s3_leer_para_escribir(key, cid), .schema_avisos)
    hechos <- c(hechos, intersect(filas$id, leido$id))
  }
  if (!length(hechos)) return(list(archivados = 0L, motivo = NA_character_))
  quitados <- 0L
  modificar_tabla(.AVISOS_KEY, cid, .schema_avisos, function(d) {
    fuera <- d$id %in% hechos & vencido(d)
    quitados <<- sum(fuera)
    d[!fuera, , drop = FALSE]
  }, permitir_reduccion = TRUE)
  list(archivados = as.integer(quitados), motivo = NA_character_)
}
