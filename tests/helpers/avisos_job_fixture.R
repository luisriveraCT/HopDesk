# =============================================================================
# tests/helpers/avisos_job_fixture.R
# For the job's tests (test_avisos_job.R, test_avisos_job_logs.R): the
# in-memory S3 with two active clients and one archived, their SAP
# snapshots, companies, users and preferences, and a fake send_email() that
# records what it would send. Loads scripts/avisos_job.R without running it.
# Nothing reaches the network or Resend.
# =============================================================================
source("tests/helpers/s3_memoria.R")
source("scripts/avisos_job.R")      # defines; runs nothing (sourced, not Rscript)
suppressMessages(job_cargar())
s3_memoria_reinstalar()             # job_cargar() re-sourced persistence; the fakes stay

FX_LUNES   <- as.Date("2026-11-16")
FX_VIERNES <- FX_LUNES + 4L
FX_AHORA_L <- as.POSIXct("2026-11-16 14:00:00", tz = "UTC")
FX_AHORA_V <- as.POSIXct("2026-11-20 23:00:00", tz = "UTC")

fx_pon <- function(cid, k, x) assign(.s3_key(k, client_id = cid), x, envir = .mem)

fx_ar <- tibble::tibble(
  Documento = paste0("A", 1:4), Empresa = "NL Transportes",
  Parte = c("Cliente Secreto Uno", "Cliente Dos", "Cliente Tres", "Cliente Cuatro"),
  Moneda = c("MXN", "USD", "MXN", "MXN"), `Saldo vencido` = c(400123.45, 40, 999, 77),
  `Fecha de vencimiento` = as.Date(c("2026-11-17", "2026-11-18", "2026-11-30", "2026-11-10")),
  `Código de cliente` = paste0("C", 101:104), check.names = FALSE)
fx_ap <- tibble::tibble(
  Documento = "P1", Empresa = "NL Transportes", Parte = "Proveedor Secreto", Moneda = "MXN",
  `Saldo vencido` = 300, `Fecha de vencimiento` = as.Date("2026-11-18"),
  `Código de proveedor` = "P201", check.names = FALSE)

fx_cliente <- function(cid, guardado) {
  fx_pon(cid, S3_KEYS_CRITICAL$sap_snap_ar, list(data = fx_ar, saved_at = guardado))
  fx_pon(cid, S3_KEYS_CRITICAL$sap_snap_ap, list(data = fx_ap, saved_at = guardado))
  fx_pon(cid, S3_KEYS$empresas, tibble::tibble(initials = "NL", nombre_corto = "NL Transportes",
                                               activa = TRUE, deleted = FALSE))
  fx_pon(cid, S3_KEYS$interco_v2, list(ar_prefix = "C", ap_prefix = "P", companies = list(), rfcs = character()))
  fx_pon(cid, S3_KEYS$usuarios, data.frame(
    username = c("jefa", "ti", "fin"), tier = c("admin", nivel_ti(), "finance"),
    email = paste0(c("jefa", "ti", "fin"), "@", cid, "-secreto.mx"), permisos = "{}",
    deleted = FALSE, activo = TRUE, stringsAsFactors = FALSE))
  fx_pon(cid, .PREFERENCIAS_KEY, tibble::tibble(usuario = "ti", datos = '{"idioma": "en"}',
                                                actualizado = as.POSIXct(FX_AHORA_L, tz = "UTC")))
}

# A fresh store: acme and beta active, zeta archived.
fx_reset <- function(guardado = FX_AHORA_L - 86400) {
  s3_memoria_reset()
  assign("hd-admin/client_registry.rds", data.frame(
    client_id = c("acme", "beta", "zeta"), status = c("active", "active", "archived"),
    stringsAsFactors = FALSE), envir = .mem)
  for (cid in c("acme", "beta", "zeta")) fx_cliente(cid, guardado)
  fx_enviados <<- list()
  fx_envio_ok <<- TRUE
}

# The fake Resend: records (to, subject), answers like send_email().
fx_enviados <- list(); fx_envio_ok <- TRUE
send_email <- function(to, subject, html, queue_on_fail = TRUE) {
  fx_enviados[[length(fx_enviados) + 1L]] <<- list(to = to, subject = subject, cola = queue_on_fail)
  if (fx_envio_ok) structure(TRUE, resend_id = paste0("re_", length(fx_enviados)))
  else structure(FALSE, error = "simulated")
}

fx_cuenta <- function(lineas, evento) {
  l <- lineas[startsWith(lineas, paste0(evento, "="))]
  if (!length(l)) NA_integer_ else as.integer(sub("^[^=]+=", "", l))
}
fx_llaves <- function() ls(.mem, all.names = TRUE)
fx_firma <- function() vapply(fx_llaves(), function(k) digest_txt(get(k, envir = .mem)), character(1))
digest_txt <- function(x) paste(utils::capture.output(utils::str(x, give.attr = FALSE, vec.len = 50)), collapse = "")
