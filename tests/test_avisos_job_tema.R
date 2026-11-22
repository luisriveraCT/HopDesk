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

hijo <- function(quitar = "", bs_theme = FALSE) {
  f <- tempfile(fileext = ".R")
  writeLines(c(
    'for (e in parse("scripts/avisos_job.R", keep.source = FALSE))',
    '  if (is.call(e) && identical(e[[1]], as.name("<-")) &&',
    '      as.character(e[[2]])[1] %in% c("AVISOS_JOB_ARCHIVOS", "job_cargar")) eval(e, globalenv())',
    sprintf('AVISOS_JOB_ARCHIVOS <- setdiff(AVISOS_JOB_ARCHIVOS, "%s")', quitar),
    'r <- tryCatch({ job_cargar(); "ok" }, error = function(e) paste("error:", conditionMessage(e)))',
    'cat("CARGA=", r, "\\n", sep = "")',
    'cat("MARCA=", tryCatch(tema_tokens("marca")[["primario"]], error = function(e) "error"), "\\n", sep = "")',
    'm <- tryCatch(email_invite_html("Ana", "Acme", "https://x/?invite=t"), error = function(e) "error")',
    'cat("CORREO=", !identical(m, "error") && !grepl("{{", m, fixed = TRUE) && grepl(tema_color("acento"), m, fixed = TRUE), "\\n", sep = "")',
    if (bs_theme) 'invisible(tryCatch(hd_bs_theme(), error = function(e) NULL))' else '',
    'cat("BSLIB=", "bslib" %in% loadedNamespaces(), "\\n", sep = "")'
  ), f)
  rs <- file.path(R.home("bin"), "Rscript")
  out <- suppressWarnings(system2(rs, c("--vanilla", shQuote(f)), stdout = TRUE, stderr = TRUE))
  val <- function(k) sub(paste0("^", k, "="), "", grep(paste0("^", k, "="), out, value = TRUE)[1])
  list(carga = val("CARGA"), marca = val("MARCA"), correo = val("CORREO"), bslib = val("BSLIB"))
}

reg <- new.env(); sys.source("R/tema_registro.R", envir = reg)
r <- hijo()
ok("1: the job's load (AVISOS_JOB_ARCHIVOS in order, clean R, from the root) ends without error",
   identical(r$carga, "ok"))
ok("2: tema_tokens(\"marca\") resolves in the job (Marca's primario)",
   identical(r$marca, reg$tema_color("primario", "marca")))
ok("3: email_invite_html() builds in the job, painted (no {{rol}} left, Clásico's acento)", identical(r$correo, "TRUE"))
ok("4: bslib is never loaded on the job's path", identical(r$bslib, "FALSE"))

sin <- hijo(quitar = "R/tema_registro.R")
ok("contraprueba: without R/tema_registro.R the job's mail cannot be built",
   identical(sin$correo, "FALSE") && identical(sin$marca, "error"))
bs <- hijo(bs_theme = TRUE)
ok("contraprueba: hd_bs_theme() (it needs bslib) in the same child is caught by check 4",
   identical(bs$bslib, "TRUE"))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
