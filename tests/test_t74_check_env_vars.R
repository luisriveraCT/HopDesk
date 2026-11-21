# =============================================================================
# tests/test_t74_check_env_vars.R
# T74: R/global.R's .check_env_vars() fails CLOSED. With any of CLIENT_ID,
# S3_BUCKET, AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, AWS_DEFAULT_REGION
# missing, the app refuses to start (stop), naming what is missing and never
# a value. Only options(hopdesk.sin_s3 = TRUE) turns it back into a warning.
#   1. All five set: TRUE, no warning, no error.
#   2. Each one missing (empty): an error naming exactly that variable, with
#      none of the other variables' values in the message.
#   3. Several missing: all named.
#   4. options(hopdesk.sin_s3 = TRUE): a warning, FALSE, no error.
#   5. global.R's wiring: the call runs at top level, before s3_init().
# Runs .check_env_vars() taken from global.R itself (no S3, no network); the
# environment is put back afterwards.
# Run from project root:  Rscript tests/test_t74_check_env_vars.R
# Counter-proof: on 29d829e (warning only) 2, 3 and the list check fail.
# =============================================================================
cat("=== test_t74_check_env_vars ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
VARS <- c("CLIENT_ID", "S3_BUCKET", "AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_DEFAULT_REGION")
antes <- Sys.getenv(VARS, unset = NA)
op_antes <- getOption("hopdesk.sin_s3")
restaurar <- function() {
  for (v in VARS) if (is.na(antes[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, setNames(list(antes[[v]]), v))
  options(hopdesk.sin_s3 = op_antes)
}

exprs <- parse("R/global.R", keep.source = FALSE, encoding = "UTF-8")
for (e in exprs)
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c(".check_env_vars", "HD_ENV_REQUERIDAS"))
    eval(e, globalenv())
ok("global.R defines .check_env_vars() and HD_ENV_REQUERIDAS",
   exists(".check_env_vars", mode = "function") && exists("HD_ENV_REQUERIDAS"))
ok("the required list is exactly the five", exists("HD_ENV_REQUERIDAS") && setequal(HD_ENV_REQUERIDAS, VARS))

# Fake values that look like secrets, so a leak into the message is caught.
FALSOS <- setNames(paste0("VALOR_PRUEBA_", seq_along(VARS), "_x9"), VARS)
poner <- function(sin = character()) {
  for (v in VARS) do.call(Sys.setenv, setNames(list(if (v %in% sin) "" else FALSOS[[v]]), v))
}
correr <- function() {
  w <- character(); err <- NULL
  val <- withCallingHandlers(
    tryCatch(.check_env_vars(), error = function(e) { err <<- conditionMessage(e); NULL }),
    warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
  list(val = val, err = err, w = w)
}

if (exists(".check_env_vars", mode = "function")) {
  options(hopdesk.sin_s3 = NULL)
  # ── 1. All set ─────────────────────────────────────────────────────────────
  poner(); r <- correr()
  ok("all five set: TRUE, no error, no warning", isTRUE(r$val) && is.null(r$err) && !length(r$w))

  # ── 2. Each one missing ────────────────────────────────────────────────────
  for (v in VARS) {
    poner(sin = v); r <- correr()
    ok(sprintf("%s missing: it stops, naming it", v), !is.null(r$err) && grepl(v, r$err, fixed = TRUE))
    ok(sprintf("%s missing: the message carries no value", v),
       !is.null(r$err) && !any(vapply(FALSOS, function(x) grepl(x, r$err, fixed = TRUE), logical(1))))
  }

  # ── 3. Several missing ─────────────────────────────────────────────────────
  poner(sin = c("CLIENT_ID", "AWS_DEFAULT_REGION")); r <- correr()
  ok("two missing: both named in one error",
     !is.null(r$err) && grepl("CLIENT_ID", r$err, fixed = TRUE) && grepl("AWS_DEFAULT_REGION", r$err, fixed = TRUE))

  # ── 4. The explicit opt-out ────────────────────────────────────────────────
  options(hopdesk.sin_s3 = TRUE); poner(sin = "S3_BUCKET"); r <- correr()
  ok("options(hopdesk.sin_s3 = TRUE): only a warning naming it, FALSE, no error",
     is.null(r$err) && identical(r$val, FALSE) && any(grepl("S3_BUCKET", r$w, fixed = TRUE)))
  options(hopdesk.sin_s3 = "yes"); r <- correr()
  ok("the opt-out is exactly TRUE: anything else still stops", !is.null(r$err))
  restaurar()
}

# ── 5. Wiring ────────────────────────────────────────────────────────────────
nivel_sup <- vapply(exprs, function(e) paste(deparse(e, width.cutoff = 500L)[1], collapse = ""), character(1))
i_check <- which(nivel_sup == ".check_env_vars()")
i_init <- which(nivel_sup == "s3_init()")
ok("global.R calls .check_env_vars() once at top level, before s3_init()",
   length(i_check) == 1L && length(i_init) >= 1L && i_check < min(i_init))
ais <- sub("#.*$", "", readLines("tests/helpers/aislar_global.R", warn = FALSE))
ok("tests/helpers/aislar_global.R declares options(hopdesk.sin_s3 = TRUE) (tests load global.R with no S3 on purpose)",
   any(grepl("options(hopdesk.sin_s3 = TRUE)", ais, fixed = TRUE)))
glob <- sub("#.*$", "", readLines("R/global.R", warn = FALSE, encoding = "UTF-8"))
ok("global.R takes the opt-out only from an R option, never from an env var",
   any(grepl('getOption("hopdesk.sin_s3")', glob, fixed = TRUE)) && !any(grepl("Sys.getenv(\"HOPDESK_SIN_S3", glob, fixed = TRUE)))
ok("the environment is back as it was",
   identical(Sys.getenv(VARS, unset = NA), antes) && identical(getOption("hopdesk.sin_s3"), op_antes))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
