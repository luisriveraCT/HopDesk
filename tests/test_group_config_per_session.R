# =============================================================================
# tests/test_group_config_per_session.R
# group_config_rv (group name + logo, used by the cash-flow export and the
# financial report brand) started each session with load_group_config()
# WITHOUT a client_id -- the process's own config, hd-admin's in the shared
# production deployment. The context-switch observer only reloads it on a
# staff jump (ignoreInit = TRUE), so a native client kept that config for the
# whole session.
#
# It now starts empty (.schema_group_config()) and Phase 2 loads the
# session's own client, like empresas.
#
# S3 is never touched: .s3_read_with() is a fake in the test's own environment.
#
# Run from project root:  Rscript tests/test_group_config_per_session.R
# =============================================================================
cat("=== test_group_config_per_session ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# ── The loader reads the client it is given ───────────────────────────────────
env <- new.env()
for (e in parse("R/persistence.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c("load_group_config", ".schema_group_config"))
    eval(e, env)
env$S3_KEYS <- list(group_config = "group_config.rds")
configs <- list(acme = list(group_name = "Grupo Acme"), `hd-admin` = list(group_name = "HopDesk Staff"))
env$.s3_read_with <- function(key, client_id = NULL) configs[[client_id %||% "hd-admin"]]
`%||%` <- function(a, b) if (is.null(a)) b else a

ok("load_group_config(client_id) reads that client's group",
   identical(env$load_group_config(client_id = "acme")$group_name, "Grupo Acme"))
ok("the empty schema has no group name",
   identical(env$.schema_group_config()$group_name, ""))

# ── app.R: starts empty, Phase 2 loads the session's client ───────────────────
exprs <- parse("app.R", keep.source = FALSE)
calls <- list(); inits <- list()
walk <- function(x) {
  if (is.call(x)) {
    if (identical(x[[1]], as.name("load_group_config"))) calls[[length(calls) + 1L]] <<- x
    if (identical(x[[1]], as.name("<-")) && identical(x[[2]], as.name("group_config_rv")))
      inits[[length(inits) + 1L]] <<- x[[3]]
    args <- as.list(x)[-1]
    for (i in seq_along(args)) if (!identical(args[[i]], quote(expr = ))) walk(args[[i]])
  }
}
for (e in exprs) walk(e)

ok("group_config_rv is created once", length(inits) == 1L)
ok("group_config_rv starts as the empty schema, not another client's config",
   length(inits) == 1L && identical(inits[[1]], quote(reactiveVal(.schema_group_config()))))
ok("app.R never calls load_group_config() without a client_id",
   length(calls) >= 1L && all(vapply(calls, function(cl) "client_id" %in% names(cl), logical(1))))
ok("Phase 2 loads the session's own client's group config",
   any(vapply(calls, function(cl) identical(cl$client_id, as.name("cid_lo")), logical(1))))
ok("the context-switch observer still reloads it on a jump",
   any(grepl(".ctx_load(load_group_config,", readLines("app.R", warn = FALSE), fixed = TRUE)))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
