# =============================================================================
# tests/test_avisos_job_carga_monedas.R  (monedas fase 3, T39)
# The emails job (scripts/avisos_job.R, GitHub) does not run the app: it loads
# AVISOS_JOB_ARCHIVOS in order with job_cargar(). R/avisos_*.R now default to
# MONEDA_BASE_OMISION (R/monedas.R) where they said "MXN", and
# R/data_pipeline.R is to use moneda_normalizar() from the same file -- so
# R/monedas.R must be in that list, before data_pipeline.
#
# A clean R (Rscript --vanilla, its own process), from the project root,
# takes AVISOS_JOB_ARCHIVOS and job_cargar() from the real script, runs the
# job's loading and checks what the emails need. No network: nothing is read
# from S3, only the files are sourced.
#   1. the load ends without error;
#   2. MONEDA_BASE_OMISION exists, with the catalog (monedas_catalogo());
#   3. moneda_normalizar() exists once R/data_pipeline.R calls it;
#   4. monto_moneda() (R/monedas.R since T55) gives "1,234.50 MXN" for the
#      default base: the emails use it since T45 (correo_monto() is gone);
#   5. sap_al_texto() works in the job (it needs sap_viejo(), R/panorama_datos.R);
#   6. the companies' identity for the IC filter (R/empresas_identidad.R, T47):
#      the job's apply_ic_filter() calls it when a client has no IC registry;
#   7. panorama_datos.R's Agenda figures: agenda_pendientes() (R/agenda_filas.R, T60).
# Contraprueba: the same list without "R/monedas.R" fails 2 (and 4); without
# "R/ui_components.R" fails 5 (T45: the job must load it); without
# "R/empresas_identidad.R" fails 6 (T47).
# Run from the project root:  Rscript tests/test_avisos_job_carga_monedas.R
# =============================================================================
cat("=== test_avisos_job_carga_monedas ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# The child: the job's own loading, nothing else. `quitar` drops one file
# from the list (the contraprueba).
hijo <- function(quitar = "") {
  f <- tempfile(fileext = ".R")
  writeLines(c(
    'for (e in parse("scripts/avisos_job.R", keep.source = FALSE))',
    '  if (is.call(e) && identical(e[[1]], as.name("<-")) &&',
    '      as.character(e[[2]])[1] %in% c("AVISOS_JOB_ARCHIVOS", "job_cargar")) eval(e, globalenv())',
    sprintf('AVISOS_JOB_ARCHIVOS <- setdiff(AVISOS_JOB_ARCHIVOS, "%s")', quitar),
    'r <- tryCatch({ job_cargar(); "ok" }, error = function(e) paste("error:", conditionMessage(e)))',
    'cat("CARGA=", r, "\\n", sep = "")',
    'cat("OMISION=", exists("MONEDA_BASE_OMISION") && exists("monedas_catalogo") && "MXN" %in% monedas_catalogo(), "\\n", sep = "")',
    'usa <- any(grepl("moneda_normalizar(", readLines("R/data_pipeline.R", warn = FALSE), fixed = TRUE))',
    'cat("NORMALIZAR=", !usa || exists("moneda_normalizar"), "\\n", sep = "")',
    'cat("MONTO=", tryCatch(monto_moneda(1234.5, MONEDA_BASE_OMISION), error = function(e) "error"), "\\n", sep = "")',
    'cat("IDENT=", tryCatch(isTRUE(ic_por_identidad("NETWORKS LOGISTICS SA DE CV", NA, empresas_identidad(data.frame(initials = "NL", razon_social = "Networks Logistics", nombre_corto = "NL", rfc = NA, activa = TRUE, deleted = FALSE)))), error = function(e) "error"), "\\n", sep = "")',
    'cat("AGENDA=", tryCatch(identical(panorama_pagos_hoy(data.frame(ledger = "AP", status = "pending", Moneda = "MXN", Importe = 5, FechaVenc = Sys.Date() + 3), base = "MXN")$total, 5), error = function(e) "error"), "\\n", sep = "")',
    'cat("SAP=", tryCatch(startsWith(sap_al_texto(as.POSIXct("2026-11-16 14:54", tz = "UTC"), tz = UMBRAL_TZ, idioma = "es", ahora = as.POSIXct("2026-11-17 14:00", tz = "UTC")), "Datos de SAP al"), error = function(e) "error"), "\\n", sep = "")'
  ), f)
  rs <- file.path(R.home("bin"), "Rscript")
  out <- suppressWarnings(system2(rs, c("--vanilla", shQuote(f)), stdout = TRUE, stderr = TRUE))
  val <- function(k) sub(paste0("^", k, "="), "", grep(paste0("^", k, "="), out, value = TRUE)[1])
  list(carga = val("CARGA"), omision = val("OMISION"), normalizar = val("NORMALIZAR"), monto = val("MONTO"),
       sap = val("SAP"), ident = val("IDENT"), agenda = val("AGENDA"))
}

r <- hijo()
ok("1: the job's load (AVISOS_JOB_ARCHIVOS in order, clean R, from the root) ends without error",
   identical(r$carga, "ok"))
ok("2: MONEDA_BASE_OMISION exists, with the catalog", identical(r$omision, "TRUE"))
ok("3: moneda_normalizar() exists if R/data_pipeline.R calls it", identical(r$normalizar, "TRUE"))
ok("4: monto_moneda() for the default base: \"1,234.50 MXN\" (the code always, no $)", identical(r$monto, "1,234.50 MXN"))
ok("6: the IC filter's identity of the companies works in the job (T47)", identical(r$ident, "TRUE"))
ok("5: sap_al_texto() works in the job: \"Datos de SAP al ...\"", identical(r$sap, "TRUE"))
ok("7: panorama_datos.R's Agenda figures work in the job: agenda_pendientes() (R/agenda_filas.R, T60) is loaded", identical(r$agenda, "TRUE"))

sin <- hijo(quitar = "R/monedas.R")
ok("contraprueba: without R/monedas.R the emails have no default currency",
   identical(sin$omision, "FALSE") && identical(sin$monto, "error"))
sin_id <- hijo(quitar = "R/empresas_identidad.R")
ok("contraprueba: without R/empresas_identidad.R the job cannot tell its own companies apart (T47)",
   identical(sin_id$ident, "error"))
sin_ui <- hijo(quitar = "R/ui_components.R")
# Since T55 monto_moneda() lives in R/monedas.R: without R/ui_components.R
# the amount still works and SAP's date does not.
ok("contraprueba: without R/ui_components.R the emails cannot write SAP's date (the amount comes from R/monedas.R, T55)",
   identical(sin_ui$sap, "error") && identical(sin_ui$monto, "1,234.50 MXN"))

sin_ag <- hijo(quitar = "R/agenda_filas.R")
ok("contraprueba: without R/agenda_filas.R panorama_pagos_hoy() cannot read the Agenda's rows (T60)", identical(sin_ag$agenda, "error"))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
