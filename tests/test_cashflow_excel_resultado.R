# =============================================================================
# tests/test_cashflow_excel_resultado.R
#
# T74 F1 (2026-11-22): on Connect Cloud the Flujo Excel failed with "Error
# desconocido al generar el archivo." and nothing in any log. Cause of the
# blindness: a callr::r_bg() child whose function throws EXITS WITH STATUS 0
# and leaves stderr empty -- the error travels in job$get_result(). The poll
# observer read only stderr. .cf_excel_resultado() reads get_result(); the
# user gets plain words (tr()), the technical detail goes to the log only.
#   A. a child that throws: status 0 (the trap), "error", its message as the
#      detail, the plain "excel_fallo" notice;
#   B. the worker's "[SIN_DATOS]" stop: the "excel_sin_datos" notice;
#   C. a child that writes the file: "done";
#   D. a killed child: "error" with a non-empty detail;
#   E. the observer uses it: logs "[EXCEL]" + the detail, shows tr(res$aviso);
#   F. both notices exist in es and en, and say nothing technical.
# Real child processes (callr), nothing mocked. Run from project root.
# =============================================================================
cat("=== test_cashflow_excel_resultado ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
src <- "R/cashflow_export_module.R"
for (e in parse(src, keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(".cf_excel_resultado")))
    assign(".cf_excel_resultado", eval(e[[3]]), envir = globalenv())
if (!exists(".cf_excel_resultado")) stop(".cf_excel_resultado not found in ", src)

hijo <- function(f, args = list()) { j <- callr::r_bg(f, args = args, supervise = TRUE); j$wait(60000); j }
out <- tempfile(fileext = ".xlsx")

cat("A. a child that throws\n")
jA <- hijo(function() stop("boom: openxlsx missing font"))
ok("A0 the trap: a child that throws still exits with status 0", identical(jA$get_exit_status(), 0L))
ok("A0 the trap: and stderr gives nothing (no pipe: reading it throws, or it is empty)",
   !nzchar(tryCatch(trimws(jA$read_all_error_text()), error = function(e) "")))
rA <- .cf_excel_resultado(jA, out)
ok("A1 estado 'error'", identical(rA$estado, "error"))
ok("A2 the detail is the child's real message", grepl("boom: openxlsx missing font", rA$detalle, fixed = TRUE))
ok("A3 the user gets the plain 'excel_fallo' notice", identical(rA$aviso, "tablas.flujo.aviso.excel_fallo"))

cat("\nB. no data for the period\n")
rB <- .cf_excel_resultado(hijo(function() stop("[SIN_DATOS] Sin datos disponibles para el período seleccionado.")), out)
ok("B1 the 'excel_sin_datos' notice", identical(rB$estado, "error") && identical(rB$aviso, "tablas.flujo.aviso.excel_sin_datos"))
src_txt <- paste(readLines(src, warn = FALSE), collapse = "\n")
ok("B2 the worker's no-data stop carries the [SIN_DATOS] marker",
   grepl('stop("[SIN_DATOS] ', src_txt, fixed = TRUE))

cat("\nC. a child that writes the file\n")
rC <- .cf_excel_resultado(hijo(function(p) { writeLines("x", p); p }, list(p = out)), out)
ok("C1 estado 'done', no notice", identical(rC$estado, "done") && is.null(rC$aviso))
unlink(out)

cat("\nD. a killed child\n")
jD <- callr::r_bg(function() Sys.sleep(60), supervise = TRUE); Sys.sleep(1); jD$kill()
rD <- .cf_excel_resultado(jD, out)
ok("D1 estado 'error' with a non-empty detail", identical(rD$estado, "error") && nzchar(rD$detalle))

cat("\nE. the poll observer uses it\n")
ok("E1 the observer calls .cf_excel_resultado(job, info$out_path)",
   grepl(".cf_excel_resultado(job, info$out_path)", src_txt, fixed = TRUE))
ok("E2 the technical detail goes to the log as [EXCEL]",
   grepl('message("[EXCEL] the Flujo Excel export failed: ", res$detalle)', src_txt, fixed = TRUE))
ok("E3 the user sees tr(res$aviso), not the raw error",
   grepl("showNotification(tr(res$aviso)", src_txt, fixed = TRUE) &&
     !grepl('tr("tablas.flujo.aviso.error_excel", error = err)', src_txt, fixed = TRUE))

cat("\nF. the notices\n")
for (idioma in c("es", "en")) {
  d <- jsonlite::fromJSON(file.path("www", "i18n", paste0(idioma, ".json")))
  for (k in c("tablas.flujo.aviso.excel_fallo", "tablas.flujo.aviso.excel_sin_datos"))
    ok(sprintf("F %s: %s exists and says nothing technical", idioma, k),
       is.character(d[[k]]) && nzchar(d[[k]]) && !grepl("\\{error\\}|callr|stderr|status", d[[k]]))
}

cat("\nG. the child finds the app's files even when the session's working directory moved\n")
# The real cause on 2026-11-22: inside the app getwd() was the R temp folder,
# so app_dir = getwd() sent the child there ("cannot source .../Rtmp*/R/i18n.R").
raiz <- normalizePath(getwd(), winslash = "/")
for (x in parse(src, keep.source = FALSE))
  if (is.call(x) && identical(x[[1]], as.name("<-")) && identical(x[[2]], as.name(".cf_excel_worker")))
    assign(".cf_excel_worker", eval(x[[3]]), envir = globalenv())
correr_worker <- function(app_dir, wd) {
  j <- callr::r_bg(.cf_excel_worker, args = list(snapshot_path = tempfile(), output_path = tempfile(), app_dir = app_dir),
                   wd = wd, supervise = TRUE)
  j$wait(120000); .cf_excel_resultado(j, tempfile())$detalle
}
viejo <- setwd(tempdir())   # the session's working directory, moved
d_bien <- correr_worker(raiz, raiz)
d_mal  <- correr_worker(normalizePath(getwd(), winslash = "/"), getwd())
setwd(viejo)
ok("G1 with the app root it sources every file and stops only at the (missing) snapshot",
   !grepl("cannot source", d_bien, fixed = TRUE) && grepl("cannot open", d_bien, fixed = TRUE))
ok("G2 counter-case: with getwd() moved to the temp folder it cannot source R/i18n.R (the bug)",
   grepl("cannot source", d_mal, fixed = TRUE) && grepl("i18n.R", d_mal, fixed = TRUE))
ok("G3 the launcher sends HD_APP_DIR (not getwd()) as app_dir and as the child's wd",
   grepl('get0("HD_APP_DIR"', src_txt, fixed = TRUE) && grepl("wd       = app_dir,", src_txt, fixed = TRUE) &&
     !grepl("app_dir <- normalizePath(getwd())", src_txt, fixed = TRUE))
g <- readLines("R/global.R", warn = FALSE)
ok("G4 global.R takes HD_APP_DIR before its first source(\"R/...\")",
   length(grep("^HD_APP_DIR <- normalizePath\\(getwd\\(\\)", g)) == 1L &&
     grep("^HD_APP_DIR <- ", g)[1] < grep('^\\s*source\\("R/', g)[1])

cat("\nH. every function the report reaches is loaded in the child\n")
# The second cause (2026-11-22): 'could not find function "etiquetas_por_factura"'
# -- R/data_pipeline.R was not among the worker's files. The worker's own list
# (its fuente("...") calls) is loaded as the child does, and every function
# reachable from the two entry points must exist there or in a package.
cuerpo <- deparse(body(.cf_excel_worker), width.cutoff = 500L)
archivos <- regmatches(cuerpo, regexpr('(?<=fuente\\(")[^"]+(?="\\))', cuerpo, perl = TRUE))
pk <- c("dplyr", "lubridate", "openxlsx", "stringr", "purrr", "scales", "tidyr")
faltan <- callr::r(function(raiz, archivos, pk) {
  suppressPackageStartupMessages(for (p in pk) library(p, character.only = TRUE, warn.conflicts = FALSE))
  e <- new.env(parent = globalenv())
  e$`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
  for (f in archivos) { source(file.path(raiz, "R", f), local = e); if (f == "i18n.R") e$.i18n_dir <- file.path(raiz, "www", "i18n") }
  visto <- character(); faltan <- character(); cola <- c("build_cashflow_export_data", "generate_cashflow_excel_wb")
  while (length(cola)) {
    fn <- cola[1]; cola <- cola[-1]; if (fn %in% visto) next; visto <- c(visto, fn)
    f <- get0(fn, envir = e, mode = "function")
    if (is.null(f)) { faltan <- c(faltan, fn); next }
    if (!identical(environment(f), e)) next
    cola <- c(cola, setdiff(codetools::findGlobals(f, merge = FALSE)$functions, visto))
  }
  sort(unique(faltan))
}, args = list(raiz = raiz, archivos = archivos, pk = pk))
ok(sprintf("H1 the worker loads %d files (%s)", length(archivos), paste(archivos, collapse = ", ")),
   all(c("data_pipeline.R", "zona.R", "cashflow_export_module.R") %in% archivos))
ok(sprintf("H2 no function the report reaches is missing in the child (%s)",
           if (length(faltan)) paste(faltan, collapse = ", ") else "none"), !length(faltan))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
