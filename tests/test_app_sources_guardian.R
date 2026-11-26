# =============================================================================
# tests/test_app_sources_guardian.R
# Every R/*.R the app uses is source()d from app.R -- directly or through a
# file app.R sources (global.R, ledger_module.R...). Never left to shinyapps'
# autoload of R/: tests/helpers/arranque_app.R (T15) and every test that
# sources app.R have no autoload, so a file missing there is a function
# missing there. Found in the T109: R/pasivos_engine.R had no source; in
# arranque_app the Pasivos wizard died on `.periods_per_year`.
#   1. every R/*.R is loaded from app.R or is in EXCEPCIONES with its reason;
#   2. the list stays honest: each exception exists, is not loaded by the app,
#      and no file the app loads calls one of its functions;
#   3. R/pasivos_engine.R is loaded (the T109 case), and the guardian sees it
#      missing when its line is taken out (counter-proof in memory).
# Run from project root:  Rscript --vanilla tests/test_app_sources_guardian.R
# =============================================================================
cat("=== test_app_sources_guardian ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, cond, detalle = NULL) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else {
    cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L
    if (length(detalle)) cat("       ", paste(detalle, collapse = "\n        "), "\n")
  }
}

# Files the app does not load, on purpose -- and why.
EXCEPCIONES <- c(
  "avisos_archivo.R"          = "only the weekly emails job (scripts/avisos_job.R); the app never calls it",
  "avisos_correos.R"          = "only the weekly emails job (scripts/avisos_job.R)",
  "avisos_datos.R"            = "only the weekly emails job (scripts/avisos_job.R)",
  "avisos_logros.R"           = "only the weekly emails job (scripts/avisos_job.R)",
  "avisos_umbrales.R"         = "only the weekly emails job (scripts/avisos_job.R); Panorama's vigia keeps its own copy of the week",
  "hopdesk_ops_placeholder.R" = "a design marker (Stage 5), no code",
  "tema_contraste.R"          = "only the theme tests (test_temas_guardian, test_tema_contraste)"
)

codigo <- function(f) {   # the file's lines without whole-line comments
  l <- readLines(f, warn = FALSE, encoding = "UTF-8")
  l[!grepl("^\\s*#", l)]
}
# The R/ files loaded from `inicio`, following source("R/...") in each one.
cargados <- function(inicio = "app.R", leer = codigo) {
  vistos <- character(); cola <- inicio
  while (length(cola)) {
    f <- cola[1]; cola <- cola[-1]
    if (f %in% vistos || !file.exists(f)) next
    vistos <- c(vistos, f)
    l <- leer(f)
    m <- regmatches(l, regexpr('source\\(\\s*"R/[^"]+\\.R"', l))
    cola <- c(cola, setdiff(sub('"$', "", sub('^source\\(\\s*"', "", m)), vistos))
  }
  vistos
}
todos <- basename(list.files("R", "[.]R$", full.names = TRUE))
app <- cargados()
en_app <- basename(app[startsWith(app, "R/") & !grepl("^R/.+/", app)])

cat("== 1. Every R/*.R is loaded or excepted ==\n")
faltan <- setdiff(todos, c(en_app, names(EXCEPCIONES)))
ok(sprintf("all %d R/*.R files: %d loaded from app.R, %d excepted", length(todos), length(en_app), length(EXCEPCIONES)),
   !length(faltan), paste0("R/", faltan, ": add source(\"R/", faltan, "\") to app.R, or an exception with its reason"))

cat("== 2. The exceptions are honest ==\n")
ok("each exception exists and has a reason", all(names(EXCEPCIONES) %in% todos) && all(nzchar(EXCEPCIONES)),
   setdiff(names(EXCEPCIONES), todos))
ok("no exception is loaded by the app (then it is not one)", !length(intersect(names(EXCEPCIONES), en_app)),
   intersect(names(EXCEPCIONES), en_app))
cod_app <- unlist(lapply(app, function(f) sub("#.*$", "", codigo(f))))
llamadas <- unlist(lapply(names(EXCEPCIONES), function(b) {
  fns <- sub("\\s*<-.*$", "", grep("^[A-Za-z_.][A-Za-z0-9_.]*\\s*<-\\s*function", readLines(file.path("R", b), warn = FALSE, encoding = "UTF-8"), value = TRUE))
  usadas <- fns[vapply(fns, function(fn) any(grepl(paste0("(^|[^A-Za-z0-9_.])", gsub(".", "\\.", fn, fixed = TRUE), "\\("), cod_app)), logical(1))]
  if (length(usadas)) paste0("R/", b, ": ", paste(usadas, collapse = ", ")) else character()
}))
ok("no file the app loads calls a function of an exception", !length(llamadas), llamadas)
trabajo <- if (file.exists("scripts/avisos_job.R")) paste(readLines("scripts/avisos_job.R", warn = FALSE), collapse = "\n") else ""
del_job <- names(EXCEPCIONES)[grepl("avisos_job", EXCEPCIONES, fixed = TRUE)]
ok("the job's exceptions are loaded by the job", all(vapply(del_job, function(b) grepl(paste0("R/", b), trabajo, fixed = TRUE), logical(1))),
   del_job[!vapply(del_job, function(b) grepl(paste0("R/", b), trabajo, fixed = TRUE), logical(1))])

cat("== 3. The T109 case ==\n")
ok("R/pasivos_engine.R is loaded from app.R", "pasivos_engine.R" %in% en_app)
sin_motor <- function(f) { l <- codigo(f); if (identical(f, "app.R")) l[!grepl('source("R/pasivos_engine.R")', l, fixed = TRUE)] else l }
app_sin <- cargados(leer = sin_motor)
ok("counter-proof: without its line, the guardian finds it missing",
   "pasivos_engine.R" %in% setdiff(todos, c(basename(app_sin[startsWith(app_sin, "R/")]), names(EXCEPCIONES))))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
