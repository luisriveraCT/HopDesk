# =============================================================================
# tests/test_t100_server_usuarios_guardiana.R
# T100 guardian: the files a test loads to run the real tiersServer (and the
# Archivos de pago viewer it starts) come from ONE list,
# tests/helpers/server_usuarios_archivos.R, and that list keeps up with the
# server. Static: no Shiny, nothing runs.
#   G1  every R/ file whose top-level CONSTANTS the server reaches (from
#       tiersServer and pagosAuditViewerServer, through every R/ function they
#       call, transitively) is in ARCHIVOS_SERVER_USUARIOS or in
#       NO_CARGADOS_A_PROPOSITO. (Functions need no list: the helper defines
#       every function in R/ first.) T93's R/bancos_respuesta.R is the case
#       this would have caught: its BTR_ESTADOS was missing in 3 tests that
#       still passed.
#   G2  every test that runs testServer(tiersServer, ...) or
#       testServer(pagosAuditViewerServer, ...) loads the helper -- no test
#       keeps its own copy of the list -- except a test that only runs inside
#       a runner, named in EN_RUNNER with its reason.
#   G3  the lists themselves: every file exists, none is in both, every
#       exclusion has a reason, every EN_RUNNER test exists and its runner
#       really runs it.
# Run from project root:  Rscript --vanilla tests/test_t100_server_usuarios_guardiana.R
# =============================================================================
cat("=== test_t100_server_usuarios_guardiana ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(cond, label, detalle = character()) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else { cat(" FAIL:", label, "\n"); for (d in detalle) cat("       ", d, "\n"); .fail <<- .fail + 1L }
}
HELPER <- "tests/helpers/server_usuarios_archivos.R"
sys.source(HELPER, envir = (h <- new.env()))   # only the lists and the loader; nothing is loaded
LISTA <- h$ARCHIVOS_SERVER_USUARIOS; FUERA <- h$NO_CARGADOS_A_PROPOSITO

# A test that runs the server only inside a runner, which builds its own
# environment (not the helper's): moving it to the helper would change what
# the runner's other modules see.
EN_RUNNER <- c(
  "tests/test_payment_audit_stage.R" = "tests/_run_saas.R"
)

# ── Every top-level name of R/, and its body ─────────────────────────────────
def <- list(); cuerpo <- list()
for (f in sort(list.files("R", pattern = "[.]R$", recursive = TRUE, full.names = TRUE))) {
  ex <- tryCatch(parse(f, keep.source = FALSE, encoding = "UTF-8"), error = function(e) expression())
  for (e in ex) if (is.call(e) && as.character(e[[1]])[1] %in% c("<-", "=") && is.name(e[[2]])) {
    n <- as.character(e[[2]])
    if (is.null(def[[n]])) { def[[n]] <- f; cuerpo[[n]] <- e[[3]] }
  }
}
es_funcion <- function(x) is.call(x) && identical(x[[1]], as.name("function"))

# What the two servers reach, transitively (every name of R/ written in a body).
RAICES <- c("tiersServer", "pagosAuditViewerServer")
ok(all(RAICES %in% names(def)), "G0: tiersServer and pagosAuditViewerServer are defined in R/",
   setdiff(RAICES, names(def)))
vistos <- character(); cola <- RAICES
while (length(cola)) {
  n <- cola[1]; cola <- cola[-1]
  if (n %in% vistos || is.null(cuerpo[[n]])) next
  vistos <- c(vistos, n)
  cola <- c(cola, setdiff(intersect(unique(all.names(cuerpo[[n]])), names(def)), vistos))
}
constantes <- vistos[!vapply(vistos, function(n) es_funcion(cuerpo[[n]]), NA)]
de_archivo <- split(constantes, vapply(constantes, function(n) def[[n]], ""))

# ── G1 ───────────────────────────────────────────────────────────────────────
cat("== G1: the list keeps up with the server ==\n")
ok("BTR_ESTADOS" %in% constantes && "R/bancos_respuesta.R" %in% names(de_archivo),
   "G1.0: the walk reaches T93's BTR_ESTADOS (R/bancos_respuesta.R): it sees what it must see")
falta <- setdiff(names(de_archivo), c(LISTA, names(FUERA)))
ok(!length(falta),
   sprintf("G1: every R/ file whose constants the server uses is listed (%d files)", length(de_archivo)),
   vapply(falta, function(f) sprintf(
     "%s is missing: the server uses its %s. Add it to ARCHIVOS_SERVER_USUARIOS in %s (or, if a test must not load it, to NO_CARGADOS_A_PROPOSITO there, with the reason).",
     f, paste(utils::head(de_archivo[[f]], 4), collapse = ", "), HELPER), ""))

# ── G2 ───────────────────────────────────────────────────────────────────────
cat("\n== G2: every test that runs the server uses the helper ==\n")
pruebas <- setdiff(list.files("tests", pattern = "^test_.*[.]R$", full.names = TRUE),
                   "tests/test_t100_server_usuarios_guardiana.R")   # this file names the pattern itself
corre <- Filter(function(f) {
  tx <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("testServer\\(\\s*(tiersServer|pagosAuditViewerServer)\\b", tx, perl = TRUE)
}, pruebas)
ok(length(corre) >= 4L, sprintf("G2.0: the scan finds the tests that run the server (%d)", length(corre)))
sin_helper <- Filter(function(f) {
  tx <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  !(grepl(HELPER, tx, fixed = TRUE) && grepl("cargar_server_usuarios(", tx, fixed = TRUE))
}, setdiff(corre, names(EN_RUNNER)))
ok(!length(sin_helper), "G2: each one loads tests/helpers/server_usuarios_archivos.R and calls cargar_server_usuarios()",
   vapply(sin_helper, function(f) sprintf(
     "%s runs the server with its own list of files: replace it with source(\"%s\") + cargar_server_usuarios() (if it only runs inside a runner, add it to EN_RUNNER in this test, with the runner).",
     f, HELPER), ""))

# ── G3 ───────────────────────────────────────────────────────────────────────
cat("\n== G3: the lists ==\n")
todos <- c(LISTA, names(FUERA))
ok(all(file.exists(todos)), "G3.1: every listed file exists",
   sprintf("%s does not exist: fix it in %s", todos[!file.exists(todos)], HELPER))
ok(!anyDuplicated(todos), "G3.2: no file is in both lists, nor twice in one",
   sprintf("%s is listed twice in %s", unique(todos[duplicated(todos)]), HELPER))
ok(length(FUERA) == 0L || all(nzchar(trimws(FUERA))) && !is.null(names(FUERA)) && all(nzchar(names(FUERA))),
   "G3.3: every file in NO_CARGADOS_A_PROPOSITO says why")
runner_ok <- vapply(names(EN_RUNNER), function(t) file.exists(t) && file.exists(EN_RUNNER[[t]]) &&
  grepl(basename(t), paste(readLines(EN_RUNNER[[t]], warn = FALSE, encoding = "UTF-8"), collapse = "\n"), fixed = TRUE), NA)
ok(all(runner_ok), "G3.4: every EN_RUNNER test exists and its runner names it",
   sprintf("%s: not found, or %s does not run it", names(EN_RUNNER)[!runner_ok], EN_RUNNER[!runner_ok]))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
