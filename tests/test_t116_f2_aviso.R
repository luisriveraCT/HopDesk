# =============================================================================
# tests/test_t116_f2_aviso.R
# T116 F2: the «día inhábil» aviso the avisos job publishes (R/avisos_inhabiles.R).
# In-memory S3; invented clients.
#   1  the horizon: weekdays from today to today + 10 natural days; weekends
#      out; the client's countries only
#   2  a client with base USD and no countries gets no Mexican aviso
#   3  the aviso: Spanish text, its reason, clave inhabil:<pais>:<fecha>,
#      every client tier, expires at the end of that day (Mexico)
#   4  two runs (Monday and Friday): one aviso, never "new" again
#   5  a failed read of the client's config: nothing published, no error
#   6  the staff's calendar unreadable: the rule, and the job counts
#      inhabiles_regla_calculada ONCE per run (not per client); readable: the
#      official reason in the text
#   7  the job: the step runs first in job_lunes and job_viernes, and both
#      events are in JOB_EVENTOS (the log may only count known events)
#
# Run from project root:  Rscript tests/test_t116_f2_aviso.R
# =============================================================================
cat("=== test_t116_f2_aviso ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr, detalle = NULL) {
  r <- tryCatch(isTRUE(expr), error = function(e) { message("  error: ", conditionMessage(e)); FALSE })
  if (r) { .pass <<- .pass + 1L; cat(" PASS:", label, "\n") }
  else   { .fail <<- .fail + 1L; cat(" FAIL:", label, "\n"); if (!is.null(detalle)) cat("       ", detalle, "\n") }
}
source("tests/helpers/s3_memoria.R")
for (f in c("R/i18n.R", "R/monedas.R", "R/zona.R", "R/tier_registry.R", "R/permisos.R", "R/avisos.R",
            "R/policy_engine.R", "R/mercado.R", "R/avisos_inhabiles.R"))
  suppressMessages(sys.source(f, envir = globalenv()))
q <- function(expr) invisible(suppressWarnings(suppressMessages(expr)))
D <- function(...) as.Date(c(...))
cuenta <- character()
contar <- function(evento, n = 1L) cuenta <<- c(cuenta, rep(evento, n))
cfg <- function(...) function() .group_config_completar(list(...))
avisos_de <- function(cid) { a <- q(avisos_leer(cid)); a[!is.na(a$clave) & startsWith(a$clave, "inhabil:"), , drop = FALSE] }
mercado_inhabiles_poner(NULL)

cat("== 1. El horizonte ==\n")
p1 <- inhabiles_proximos("MX", D("2027-10-29"))            # a Friday
ok("1.1 viernes 29 oct 2027: el martes 2 nov sí; el lunes 15 nov (a 17 días) no",
   identical(p1$fecha, D("2027-11-02")) && identical(p1$pais, "MX"))
ok("1.2 un festivo en fin de semana no se avisa (12 dic 2027, domingo)",
   !D("2027-12-12") %in% inhabiles_proximos("MX", D("2027-12-06"))$fecha)
ok("1.3 el mismo día cuenta (si el job corre un festivo)", D("2027-11-02") %in% inhabiles_proximos("MX", D("2027-11-02"))$fecha)
p14 <- inhabiles_proximos(c("MX", "US"), D("2027-11-15"))   # Monday: Revolución today, Thanksgiving in 10 days
ok("1.4 varios países: cada uno con lo suyo (MX y US, semana de Acción de Gracias 2027)",
   setequal(paste(p14$pais, p14$fecha),
            c("MX 2027-11-15", "US 2027-11-25")))

cat("== 2. Un cliente con base USD sin países ==\n")
s3_memoria_reset(); cuenta <- character()
n <- inhabiles_job("cli-usd", D("2027-10-29"), contar, leer_config = cfg(moneda_base = "USD"))
ok("2.1 en la semana del 2 nov no recibe el aviso de México", n == 0L && !nrow(avisos_de("cli-usd")))
n <- inhabiles_job("cli-usd", D("2027-11-05"), contar, leer_config = cfg(moneda_base = "USD"))
ok("2.2 ...y sí el de su país (Veterans Day, jueves 11 nov 2027)",
   n == 1L && identical(avisos_de("cli-usd")$clave, "inhabil:US:2027-11-11"))

cat("== 3. El aviso ==\n")
q(inhabiles_job("cli-mx", D("2027-10-29"), contar, leer_config = cfg(moneda_base = "MXN")))
a <- avisos_de("cli-mx")
ok("3.1 uno, con su clave", nrow(a) == 1L && identical(a$clave, "inhabil:MX:2027-11-02"))
ok("3.2 el texto en español con el día y la fecha; el motivo en el cuerpo",
   identical(a$titulo, "Día inhábil bancario en México: martes 02/11/2027") && grepl("Día de Muertos", a$cuerpo, fixed = TRUE))
ok("3.3 automático, info, para todos los niveles cliente",
   a$tipo == "automatico" && a$severidad == "info" && setequal(jsonlite::fromJSON(a$niveles), niveles_cliente()))
ok("3.4 vence al final de ese día en México (06:00 UTC del día siguiente)",
   identical(format(a$vence, "%Y-%m-%d %H:%M", tz = "UTC"), "2027-11-03 05:59"))

cat("== 4. Dos corridas ==\n")
q(inhabiles_job("cli-mx", D("2027-11-01"), contar, leer_config = cfg(moneda_base = "MXN")))
b <- avisos_de("cli-mx")
ok("4.1 el lunes no lo duplica ni lo vuelve «nuevo» (contador 1, mismo id)",
   nrow(b) == 1L && identical(b$id, a$id) && identical(b$contador, 1L) && identical(b$actualizado, a$actualizado))

cat("== 5. La configuración no se lee ==\n")
n <- inhabiles_job("cli-x", D("2027-10-29"), contar, leer_config = function() stop("S3 caído"))
ok("5.1 no publica nada y no truena", identical(n, 0L) && !nrow(avisos_de("cli-x")))

cat("== 6. El calendario del staff ==\n")
s3_memoria_reset(); cuenta <- character(); mercado_inhabiles_poner(NULL); .MERCADO_INHABILES$intento_en <- NULL
assign(".mem_prohibido", c(.mem_prohibido, "hd-admin/mercado/inhabiles.rds"), envir = globalenv())   # 403: a role without access
salida <- utils::capture.output({
  n1 <- inhabiles_job("cli-1", D("2027-10-29"), contar, leer_config = cfg(moneda_base = "MXN"))
  n2 <- inhabiles_job("cli-2", D("2027-10-29"), contar, leer_config = cfg(moneda_base = "MXN"))
}, type = "message")
ok("6.1 ilegible: los avisos salen con la regla", n1 == 1L && n2 == 1L)
ok("6.2 ...y el job cuenta inhabiles_regla_calculada UNA vez por corrida, sin imprimir nada",
   sum(cuenta == "inhabiles_regla_calculada") == 1L && !length(salida), paste(salida, collapse = " | "))
assign(".mem_prohibido", setdiff(.mem_prohibido, "hd-admin/mercado/inhabiles.rds"), envir = globalenv())
s3_memoria_reset(); .MERCADO_INHABILES$intento_en <- NULL
.mem[["hd-admin/mercado/inhabiles.rds"]] <- tibble::tibble(
  id = c("d1", "a1"), tipo = c("dia", "anio"), pais = "MX", fecha = c(D("2027-11-02"), NA), anio = 2027L,
  motivo = c("Día de Muertos (DOF ficticio)", NA), fuente = "prueba", capturado_por = "staff.ficticio",
  capturado_en = Sys.time(), anulado_por = NA_character_, anulado_en = as.POSIXct(NA))
q(inhabiles_job("cli-3", D("2027-10-29"), contar, leer_config = cfg(moneda_base = "MXN")))
ok("6.3 legible: el aviso lleva el motivo oficial", grepl("DOF ficticio", avisos_de("cli-3")$cuerpo, fixed = TRUE))

cat("== 7. El job ==\n")
job <- readLines("scripts/avisos_job.R", warn = FALSE, encoding = "UTF-8")
primera <- function(fn) { i <- grep(paste0("^", fn, " <- function"), job); trimws(job[i + 1L]) }
ok("7.1 job_lunes y job_viernes publican primero, antes de cualquier salida temprana",
   startsWith(primera("job_lunes"), "inhabiles_job(cid, hoy, job_contar)") &&
     startsWith(primera("job_viernes"), "inhabiles_job(cid, hoy, job_contar)"))
ok("7.2 los dos eventos están en JOB_EVENTOS y los archivos en AVISOS_JOB_ARCHIVOS", {
  env <- new.env(); for (e in parse("scripts/avisos_job.R", keep.source = FALSE))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% c("JOB_EVENTOS", "AVISOS_JOB_ARCHIVOS")) eval(e, env)
  all(c("avisos_inhabiles", "inhabiles_regla_calculada") %in% env$JOB_EVENTOS) &&
    all(c("R/policy_engine.R", "R/mercado.R", "R/avisos_inhabiles.R") %in% env$AVISOS_JOB_ARCHIVOS) })
x <- readLines("R/avisos_inhabiles.R", warn = FALSE, encoding = "UTF-8"); x <- x[!grepl("^\\s*#", x)]
ok("7.3 R/avisos_inhabiles.R no imprime nada (el log del job es público)",
   !any(grepl("\\b(cat|print|message|writeLines|warning|printf|str|show)\\s*\\(", x)))

mercado_inhabiles_poner(NULL)
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
