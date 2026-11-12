# =============================================================================
# tests/test_avisos_job_logs.R
# Guardian for frente D: the job runs in a PUBLIC repo's Actions log, so it
# may print only "evento=<count>" lines. Fails when
#   1. scripts/avisos_job.R prints anything but job_log()'s lines and the
#      fixed "job_no_arranco=1", or the D files it runs (R/avisos_umbrales.R,
#      R/avisos_correos.R, R/avisos_datos.R, R/avisos_logros.R) print at all;
#   2. a run (test and real mode, Monday and Friday, with a client left out
#      and one that errors) lets any message, warning or print through, or
#      its log carries anything but a known event and an integer -- a client
#      id, a name, an email, an amount;
#   3. the script run as Rscript, failing to start (no S3), prints more than
#      "job_no_arranco=1". That run has no network (a dead proxy) and no
#      credentials.
#
# Run from project root:  Rscript tests/test_avisos_job_logs.R
# =============================================================================
cat("=== test_avisos_job_logs ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

cat("-- 1. el código --\n")
codigo <- function(f) { x <- readLines(f, warn = FALSE, encoding = "UTF-8"); x[!grepl("^\\s*#", x)] }
IMPRIME <- "\\b(cat|print|message|writeLines|warning|printf|str|show)\\s*\\("
job <- codigo("scripts/avisos_job.R")
salidas <- trimws(job[grepl(IMPRIME, job)])
ok(sprintf("avisos_job.R solo imprime el log (%s)", paste(salidas, collapse = " | ")),
   setequal(salidas, c('error = function(e) { cat("job_no_arranco=1\\n"); quit(status = 1L) })',
                       'cat(lineas, sep = "\\n")')))
for (f in c("R/avisos_umbrales.R", "R/avisos_correos.R", "R/avisos_datos.R", "R/avisos_logros.R")) {
  x <- codigo(f)
  ok(sprintf("%s no imprime nada", f), !any(grepl(IMPRIME, x)))
}

cat("\n-- 2. una corrida completa --\n")
source("tests/helpers/avisos_job_fixture.R")
fuga <- character(); log_total <- character()
corre <- function(...) {
  out <- character(); err <- textConnection("err_txt", "w", local = TRUE)
  sink(err, type = "message")
  out <- utils::capture.output(l <- tryCatch(job_correr(...), error = function(e) character()))
  sink(type = "message"); close(err)
  fuga <<- c(fuga, out, err_txt)
  log_total <<- c(log_total, l)
  l
}
fx_reset()
invisible(corre("lunes", TRUE, hoy = FX_LUNES, ahora = FX_AHORA_L))
invisible(corre("lunes", FALSE, hoy = FX_LUNES, ahora = FX_AHORA_L))
for (cid in c("acme", "beta")) fx_cliente(cid, FX_AHORA_V - 3600)   # SAP refreshed by Friday
.mem_prohibido <<- "beta/bancos_confirmados.rds"; assign("beta/bancos_confirmados.rds", tibble::tibble(), envir = .mem)
f <- correo_datos_viernes; correo_datos_viernes <- function(...) stop("Cliente Secreto Uno debe $400,123.45")
invisible(corre("viernes", FALSE, hoy = FX_VIERNES, ahora = FX_AHORA_V))
correo_datos_viernes <- f; .mem_prohibido <<- character()
invisible(corre("viernes", FALSE, hoy = FX_VIERNES, ahora = FX_AHORA_V))
ok(sprintf("nada se escapa por fuera del log: ni mensajes, ni warnings, ni prints (%d líneas)", length(fuga)),
   !length(fuga))
ok("el log son solo líneas evento=<entero>, de eventos conocidos", {
  length(log_total) > 0 && all(grepl("^[a-z_]+=[0-9]+$", log_total)) &&
    all(sub("=.*$", "", log_total) %in% JOB_EVENTOS)
})
ok("en el log no hay clientes, nombres, correos ni montos", {
  txt <- paste(log_total, collapse = "\n")
  !grepl("acme|beta|zeta|Secreto|Cliente|Proveedor|@|\\$|jefa|NL", txt)
})
ok("las corridas sí pasaron por todo (envíos, omisiones y un error)", {
  n <- function(e) sum(as.integer(sub("^.*=", "", log_total[startsWith(log_total, paste0(e, "="))])))
  n("enviado") > 0 && n("prueba_html") > 0 && n("omitido_lectura") > 0 && n("error_cliente") > 0
})
ok("job_contar() rechaza un evento que no está en la lista (nada libre llega al log)",
   inherits(tryCatch(job_contar("acme"), error = function(e) e), "error"))
s3_memoria_restaurar()

cat("\n-- 3. como Rscript, sin poder arrancar --\n")
rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
muerto <- "http://127.0.0.1:9"
env <- c(AVISOS_MODO = "lunes", S3_BUCKET = "hopdesk-prueba-no-existe", AWS_ACCESS_KEY_ID = "",
         AWS_SECRET_ACCESS_KEY = "", AWS_SESSION_TOKEN = "", AWS_DEFAULT_REGION = "us-east-1",
         http_proxy = muerto, https_proxy = muerto, HTTP_PROXY = muerto, HTTPS_PROXY = muerto,
         HOPDESK_APP_URL = "https://ejemplo.invalid")
viejo <- Sys.getenv(names(env), unset = NA); do.call(Sys.setenv, as.list(env))
salida <- suppressWarnings(system2(rscript, c("--vanilla", "scripts/avisos_job.R"), stdout = TRUE, stderr = TRUE))
# Loading fails: the same script run from an empty folder, where R/global.R
# and the app's files do not exist (as a missing file or a failed library()).
vacia <- tempfile("job_sin_codigo"); dir.create(vacia)
script <- normalizePath("scripts/avisos_job.R")
salida2 <- local({ antes <- setwd(vacia); on.exit(setwd(antes))
  suppressWarnings(system2(rscript, c("--vanilla", shQuote(script)), stdout = TRUE, stderr = TRUE)) })
for (k in names(viejo)) if (is.na(viejo[[k]])) Sys.unsetenv(k) else do.call(Sys.setenv, stats::setNames(list(viejo[[k]]), k))
ok(sprintf("sin S3, imprime solo job_no_arranco=1 y sale con error (%s)", paste(salida, collapse = " | ")),
   identical(as.character(salida), "job_no_arranco=1") && identical(attr(salida, "status"), 1L))
ok(sprintf("si la carga falla (falta un archivo), imprime solo job_no_arranco=1 (%s)", paste(salida2, collapse = " | ")),
   identical(as.character(salida2), "job_no_arranco=1") && identical(attr(salida2, "status"), 1L))

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
