# =============================================================================
# tests/test_avisos_job_tema.R  (TEMA stage 4b, correos)
# The emails job (scripts/avisos_job.R, GitHub) loads AVISOS_JOB_ARCHIVOS in
# order with job_cargar(), without the app. The mails resolve their colours
# with the theme registry (R/email_service.R: .email_pintar() -> tema_color();
# avisos_correos.R next: tema_tokens("marca")), so R/tema_registro.R must be
# in that list, before them, and the job's path through the registry must
# never need bslib: only hd_bs_theme() (the app's Bootstrap theme) uses it.
#
# A clean R (Rscript --vanilla, its own process), from the project root, takes
# AVISOS_JOB_ARCHIVOS and job_cargar() from the real script, runs the job's
# loading, builds a mail, and:
#   1. the load ends without error;
#   2. tema_tokens("marca") resolves (Marca's primario);
#   3. a mail that paints with tema_color() builds, with nothing unpainted;
#   4. bslib's namespace was never loaded (loadedNamespaces()). A failing
#      onLoad hook would not do: R reports the hook's error and loads anyway.
#      In GitHub bslib IS installed (shiny imports it), so this check is the
#      guard: it fails the moment anything on the job's path touches bslib.
#   5. CORREO_COLORES (the weekly mails, R/avisos_correos.R) is built from the
#      registry, in Marca;
#   6. the same in a CI-like library (only the job's packages, and a bslib
#      that cannot load): the job loads, builds a painted mail and
#      CORREO_COLORES. The library is links to the real packages, removed
#      one by one; the real packages are checked untouched afterwards.
# Contraprueba: without "R/tema_registro.R" the mail cannot be built; and
# calling hd_bs_theme() (it does need bslib) in the same child is caught by 4.
# No network: nothing is read from S3, only the files are sourced.
# Run from the project root:  Rscript tests/test_avisos_job_tema.R
# =============================================================================
cat("=== test_avisos_job_tema ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

hijo <- function(quitar = "", bs_theme = FALSE, lib = NULL) {
  f <- tempfile(fileext = ".R")
  writeLines(c(
    # A CI-like library: only the job's packages (and their dependencies).
    if (!is.null(lib)) sprintf('.libPaths("%s")', lib) else '',
    'for (e in parse("scripts/avisos_job.R", keep.source = FALSE))',
    '  if (is.call(e) && identical(e[[1]], as.name("<-")) &&',
    '      as.character(e[[2]])[1] %in% c("AVISOS_JOB_ARCHIVOS", "job_cargar")) eval(e, globalenv())',
    sprintf('AVISOS_JOB_ARCHIVOS <- setdiff(AVISOS_JOB_ARCHIVOS, "%s")', quitar),
    'r <- tryCatch({ job_cargar(); "ok" }, error = function(e) paste("error:", conditionMessage(e)))',
    'cat("CARGA=", r, "\\n", sep = "")',
    'cat("MARCA=", tryCatch(tema_tokens("marca")[["primario"]], error = function(e) "error"), "\\n", sep = "")',
    'm <- tryCatch(email_invite_html("Ana", "Acme", "https://x/?invite=t"), error = function(e) "error")',
    'cat("CORREO=", !identical(m, "error") && !grepl("{{", m, fixed = TRUE) && grepl(tema_color("acento"), m, fixed = TRUE), "\\n", sep = "")',
    # The weekly mails' colours (R/avisos_correos.R): built from the registry on first use.
    'cc <- tryCatch(CORREO_COLORES, error = function(e) NULL)',
    'cat("COLORES=", is.list(cc) && identical(cc$marino, tema_color("primario", "marca")) && identical(cc$fondo, tema_color("lino", "marca")), "\\n", sep = "")',
    if (bs_theme) 'invisible(tryCatch(hd_bs_theme(), error = function(e) NULL))' else '',
    'cat("BSLIB=", "bslib" %in% loadedNamespaces(), "\\n", sep = "")',
    # Last: asking for bslib loads it where it is installed.
    if (!is.null(lib)) 'cat("BSLIB_INSTALADO=", requireNamespace("bslib", quietly = TRUE), "\\n", sep = "")' else ''
  ), f)
  rs <- file.path(R.home("bin"), "Rscript")
  out <- suppressWarnings(system2(rs, c("--vanilla", shQuote(f)), stdout = TRUE, stderr = TRUE))
  val <- function(k) sub(paste0("^", k, "="), "", grep(paste0("^", k, "="), out, value = TRUE)[1])
  list(carga = val("CARGA"), marca = val("MARCA"), correo = val("CORREO"), bslib = val("BSLIB"),
       colores = val("COLORES"), instalado = val("BSLIB_INSTALADO"))
}

# The CI-like library (the integrator's condition): a folder with a link to
# each package job_cargar() attaches and everything they need (the closure of
# avisos.yml's job packages, minus base and recommended), and in front of
# them a stand-in "bslib" whose .onLoad fails. R's own library (.Library)
# cannot leave .libPaths() and here it also holds bslib and shiny, so the
# stand-in is what makes bslib absent: loadNamespace("bslib") finds it first
# and fails, as it would where bslib is not installed. The links are removed
# one by one with rmdir on their own path (never normalizePath(): it resolves
# a junction to the real package), and the folder only if nothing is left.
JOB_PAQUETES <- c("dplyr", "tibble", "tidyr", "stringr", "lubridate", "jsonlite", "aws.s3", "httr", "uuid",
                  "htmltools", "digest", "openssl")
lib_ci <- function() {
  ip <- installed.packages()
  base <- rownames(installed.packages(lib.loc = .Library, priority = c("base", "recommended")))
  pk <- setdiff(unique(c(JOB_PAQUETES, unlist(tools::package_dependencies(JOB_PAQUETES, db = ip, recursive = TRUE,
                which = c("Depends", "Imports", "LinkingTo"))))), c(base, "bslib", "shiny"))
  dir <- gsub("\\", "/", file.path(tempdir(), paste0("lib_ci_", Sys.getpid())), fixed = TRUE); dir.create(dir)
  for (p in pk) {
    hacia <- file.path(dir, p)
    if (.Platform$OS.type == "windows") Sys.junction(find.package(p), hacia) else file.symlink(find.package(p), hacia)
  }
  # The stand-in bslib: installed into the folder, its load always fails.
  src <- file.path(tempdir(), "bslib_ausente"); dir.create(file.path(src, "R"), recursive = TRUE)
  writeLines(c("Package: bslib", "Version: 0.0.0.9000", "Title: Absent", "Description: Stands for an absent bslib.",
               "License: MIT", "Authors@R: person('t', 't', role = c('aut', 'cre'), email = 't@t.t')"),
             file.path(src, "DESCRIPTION"))
  writeLines("", file.path(src, "NAMESPACE"))
  writeLines('.onLoad <- function(libname, pkgname) stop("bslib is not installed in the CI-like library")',
             file.path(src, "R", "zzz.R"))
  system2(file.path(R.home("bin"), "R"), c("CMD", "INSTALL", "--no-test-load", "--no-docs", "--no-multiarch",
          paste0("--library=", shQuote(dir)), shQuote(src)), stdout = FALSE, stderr = FALSE)
  list(dir = dir, paquetes = pk, falso = file.exists(file.path(dir, "bslib", "DESCRIPTION")))
}
lib_ci_borrar <- function(l) {
  for (p in l$paquetes) {
    x <- file.path(l$dir, p)
    if (.Platform$OS.type == "windows")
      system2("cmd", c("/c", "rmdir", shQuote(gsub("/", "\\\\", x))), stdout = FALSE, stderr = FALSE)
    else file.remove(x)
  }
  unlink(file.path(l$dir, "bslib"), recursive = TRUE)   # the stand-in: a real folder of its own
  quedan <- list.files(l$dir, all.files = TRUE, no.. = TRUE)
  if (!length(quedan)) unlink(l$dir, recursive = TRUE)
  quedan
}

reg <- new.env(); sys.source("R/tema_registro.R", envir = reg)
r <- hijo()
ok("1: the job's load (AVISOS_JOB_ARCHIVOS in order, clean R, from the root) ends without error",
   identical(r$carga, "ok"))
ok("2: tema_tokens(\"marca\") resolves in the job (Marca's primario)",
   identical(r$marca, reg$tema_color("primario", "marca")))
ok("3: email_invite_html() builds in the job, painted (no {{rol}} left, Clásico's acento)", identical(r$correo, "TRUE"))
ok("4: bslib is never loaded on the job's path", identical(r$bslib, "FALSE"))
ok("5: CORREO_COLORES (the weekly mails) is built in the job, from the registry (Marca)", identical(r$colores, "TRUE"))

n_dplyr <- length(list.files(find.package("dplyr"), recursive = TRUE))
l <- lib_ci()
quedan <- character()
ci <- tryCatch(hijo(lib = l$dir), finally = quedan <- lib_ci_borrar(l))
ok(sprintf("6: in a CI-like library (%d job packages + an absent bslib) bslib cannot be loaded", length(l$paquetes)),
   isTRUE(l$falso) && identical(ci$instalado, "FALSE"))
ok("6: ...and there the job loads, builds a painted mail and CORREO_COLORES",
   identical(ci$carga, "ok") && identical(ci$correo, "TRUE") && identical(ci$colores, "TRUE"))
ok("6: the CI-like library is gone and the real packages are untouched (only the links were removed)",
   !length(quedan) && !dir.exists(l$dir) && length(list.files(find.package("dplyr"), recursive = TRUE)) == n_dplyr)

sin <- hijo(quitar = "R/tema_registro.R")
ok("contraprueba: without R/tema_registro.R the job's mail cannot be built",
   identical(sin$correo, "FALSE") && identical(sin$marca, "error"))
bs <- hijo(bs_theme = TRUE)
ok("contraprueba: hd_bs_theme() (it needs bslib) in the same child is caught by check 4",
   identical(bs$bslib, "TRUE"))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
