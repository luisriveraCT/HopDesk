# =============================================================================
# tests/test_avisos_workflow.R
# Guardian for .github/workflows/avisos.yml (frente D). Its log is public and
# it reaches production, so it fails when the workflow
#   - starts by anything but workflow_dispatch (no schedule until the
#     integrador reviews a real run; no push, no pull_request);
#   - does not default to test mode;
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

ok("solo se arranca a mano: workflow_dispatch, sin schedule, push ni pull_request",
   identical(names(on), "workflow_dispatch"))
ok("modo prueba por defecto", isTRUE(on$workflow_dispatch$inputs$prueba$default) &&
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
