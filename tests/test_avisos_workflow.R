# =============================================================================
# tests/test_avisos_workflow.R
# Guardian for .github/workflows/avisos.yml (frente D). Its log is public and
# it reaches production, so it fails when the workflow
#   - starts by anything but workflow_dispatch and the backup crons of its
#     header (T80: the main start is the AWS trigger, by workflow_dispatch);
#     no push, no pull_request; a cron on the busy :00/:30 minutes;
#   - a cron does not map to its own mode, or a scheduled run is not a real
#     send (it has no inputs: an empty prueba would mean "test" forever);
#   - the run's name does not say its mode and real/prueba the same way the
#     job's env does (the AWS trigger reads it);
#   - a manual run does not default to test mode;
#   - asks for more permissions than id-token: write and contents: read;
#   - reaches AWS by anything but the OIDC role (stored AWS keys);
#   - echoes or prints a variable, or pastes an input or secret into a
#     command line (they must arrive through env);
#   - runs without concurrency "avisos" or outside the produccion environment;
#   - runs anything but scripts/avisos_job.R.
#
# Run from project root:  Rscript tests/test_avisos_workflow.R
# =============================================================================
cat("=== test_avisos_workflow ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

F_WF <- ".github/workflows/avisos.yml"
txt <- readLines(F_WF, warn = FALSE, encoding = "UTF-8")
codigo <- txt[!grepl("^\\s*#", txt)]
wf <- yaml::read_yaml(F_WF)
`%||%` <- function(a, b) if (is.null(a)) b else a
# yaml reads the key "on" as TRUE (YAML 1.1); take it by either name.
on <- wf[["on"]] %||% wf[["TRUE"]]
pasos <- wf$jobs$avisos$steps

# T80: the backup crons of each mode (UTC), as the header lists them.
CRONS <- list(lunes   = c("17 14 * * 1", "47 15 * * 1", "23 18 * * 1"),
              viernes = c("17 23 * * 5", "41 23 * * 5"),
              archivo = c("37 5 * * 6", "13 9 * * 6"))
lista_json <- function(m) sprintf("contains(fromJSON('[%s]'), github.event.schedule) && '%s'",
                                  paste0('"', CRONS[[m]], '"', collapse = ","), m)
ok("se arranca por horario (los crons del encabezado) o a mano; sin push ni pull_request",
   setequal(names(on), c("schedule", "workflow_dispatch")) &&
     setequal(vapply(on$schedule, function(x) x$cron, ""), unlist(CRONS)))
ok("los crons del encabezado son los mismos que los del horario",
   all(vapply(unlist(CRONS), function(c) any(grepl(paste0('"', c, '"'), txt[grepl("^#", txt)], fixed = TRUE)), logical(1))))
ok("ningún cron en el minuto 0 o 30 (las horas más cargadas de GitHub)",
   !any(sub(" .*", "", unlist(CRONS)) %in% c("0", "30")))
ok("cada cron cae en un solo modo", !anyDuplicated(unlist(CRONS)))
env_job <- pasos[[length(pasos)]]$env
ok("cada cron da su propio modo (lunes, viernes, archivo); uno desconocido no corre ningún modo",
   all(vapply(names(CRONS), function(m) grepl(lista_json(m), env_job$AVISOS_MODO, fixed = TRUE), logical(1))) &&
     grepl("'desconocido'", env_job$AVISOS_MODO, fixed = TRUE) && grepl("|| inputs.modo", env_job$AVISOS_MODO, fixed = TRUE))
ok("el nombre de la corrida lleva el mismo modo que el env, y 'real' o 'prueba'", {
  modo_env <- sub("^\\$\\{\\{ (.*) \\}\\}$", "\\1", env_job$AVISOS_MODO)
  identical(wf[["run-name"]], paste0("avisos ${{ ", modo_env, " }} ",
            "${{ (github.event_name == 'schedule' || inputs.prueba == false) && 'real' || 'prueba' }}"))
})
ok("por horario envía de verdad (prueba false); a mano, lo que diga el input",
   identical(env_job$AVISOS_PRUEBA, "${{ github.event_name == 'schedule' && 'false' || inputs.prueba }}"))
ok("a mano, modo prueba por defecto", isTRUE(on$workflow_dispatch$inputs$prueba$default) &&
     identical(on$workflow_dispatch$inputs$prueba$type, "boolean"))
ok("los modos son lunes, viernes y archivo",
   setequal(unlist(on$workflow_dispatch$inputs$modo$options), c("lunes", "viernes", "archivo")))
ok("permisos: id-token write y contents read, nada más", {
  p <- wf$permissions
  setequal(names(p), c("id-token", "contents")) && identical(p[["id-token"]], "write") && identical(p$contents, "read") &&
    is.null(wf$jobs$avisos$permissions)
})
ok("una corrida a la vez (concurrency avisos, sin cancelar la que va)",
   identical(wf$concurrency$group, "avisos") && identical(wf$concurrency$`cancel-in-progress`, FALSE))
ok("corre en el environment produccion", identical(wf$jobs$avisos$environment, "produccion"))
ok("AWS solo por el rol OIDC de avisos, sin llaves guardadas", {
  aws <- Filter(function(s) grepl("configure-aws-credentials", s$uses %||% ""), pasos)
  length(aws) == 1L && identical(aws[[1]]$with$`role-to-assume`, "arn:aws:iam::496973234684:role/hopdesk-avisos-github") &&
    !any(grepl("aws-access-key-id|aws-secret-access-key|AWS_ACCESS_KEY_ID|AWS_SECRET_ACCESS_KEY|AWS_SESSION_TOKEN", codigo))
})
ok("ningún echo, printf, set -x ni impresión de variables", !any(grepl("\\b(echo|printf|set -x|env\\b|printenv|cat )",
   unlist(lapply(pasos, function(s) s$run %||% character()))) ))
ok("ningún input ni secreto pegado en una línea de comando (llegan por env)", {
  runs <- unlist(lapply(pasos, function(s) s$run %||% character()))
  !any(grepl("\\$\\{\\{", runs))
})
ok("los secretos solo en el env del paso del job", {
  con_secretos <- Filter(function(s) any(grepl("secrets\\.", unlist(s$env %||% list()))) ||
                           any(grepl("secrets\\.", unlist(s$with %||% list()))), pasos)
  length(con_secretos) == 1L && identical(con_secretos[[1]]$run, "Rscript --vanilla scripts/avisos_job.R")
})
ok("lo único que corre es scripts/avisos_job.R", {
  runs <- unlist(lapply(pasos, function(s) s$run %||% character()))
  identical(runs, "Rscript --vanilla scripts/avisos_job.R")
})
ok("los inputs llegan al script por env con los nombres que lee",
   all(c("AVISOS_MODO", "AVISOS_PRUEBA", "AVISOS_FECHA", "S3_BUCKET") %in% names(pasos[[length(pasos)]]$env)))

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
