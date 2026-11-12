# =============================================================================
# tests/test_pronosticos_una_version.R
# pron-duplicadas (2026-11-10). The five public forecasting functions
# (forecasting_get_estimate / set_estimate / freeze_metric / unfreeze_metric /
# list_metrics) were defined twice, in forecasting_service.R and
# forecasting_resolve.R, with different behavior. Which one ran depended on
# how the app was started:
#   - shinyapps.io (shiny::shinyAppDir: loadSupport() sources R/ into a
#     shared env, alphabetically) ran service.R's -- what prod ran;
#   - source("app.R") (T15, the dev workflow) ran resolve.R's.
# Now they live only in forecasting_resolve.R, with service.R's bodies.
#   1. both ways of starting the app reach the same definition, the one in
#      forecasting_resolve.R, from every caller;
#   2. forecasting_get_estimate reads the Pasivos estimates as prod did:
#      frozen > exact date > latest before > earliest; names as given; NA
#      when there is nothing or the read fails.
# Writers (set / freeze / unfreeze): tests/test_estimaciones_escritura_segura.R.
#
# Run from project root:  Rscript tests/test_pronosticos_una_version.R
# =============================================================================
cat("=== test_pronosticos_una_version ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
FN <- c("forecasting_get_estimate", "forecasting_set_estimate", "forecasting_freeze_metric",
        "forecasting_unfreeze_metric", "forecasting_list_metrics")

# ── 1. One definition, whatever the start ─────────────────────────────────────
# Where each of the five is defined in R/ (parsed, not run).
donde <- unlist(lapply(list.files("R", "\\.R$", full.names = TRUE, recursive = TRUE), function(f) {
  ex <- tryCatch(parse(f, keep.source = FALSE), error = function(e) NULL)
  n <- vapply(as.list(ex), function(e) if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]))
    as.character(e[[2]]) else "", "")
  rep(f, sum(n %in% FN))
}))
ok("the five are defined once each, all in R/forecasting_resolve.R",
   length(donde) == 5L && all(donde == "R/forecasting_resolve.R"))
ok("R/forecasting_service.R is gone", !file.exists("R/forecasting_service.R"))

# Boot the app in a fresh R and report, for every function that calls one of
# the five, the body it reaches. The child inherits aislar_global.R's
# environment: its own instance lock and no network (candado, 2026-11-11).
source("tests/helpers/aislar_global.R")
arrancar <- function(como) callr::r(function(raiz, como, FN, sin_red) {
  setwd(raiz); options(keep.source = FALSE)
  Sys.unsetenv("HOPDESK_PRODUCTION")
  if (como == "shinyapps") {
    # shiny:::shinyAppDir_appR, as shinyapps.io (and runApp) starts it.
    shared <- new.env(parent = globalenv())
    suppressMessages(suppressWarnings(shiny::loadSupport(raiz, renv = shared, globalrenv = NULL)))
    app_env <- new.env(parent = shared)
    suppressMessages(suppressWarnings(shiny:::sourceUTF8(file.path(raiz, "app.R"), envir = app_env)))
  } else {
    # T15 and the dev workflow.
    suppressMessages(suppressWarnings(source("app.R")))
    app_env <- globalenv(); shared <- globalenv()
  }
  # Checked after global.R ran its readRenviron(): still no way out.
  if (!sin_red()) stop("global.R se cargó con red hacia AWS")
  out <- list()
  for (env in unique(list(shared, globalenv()))) for (n in ls(env, all.names = TRUE)) {
    f <- get(n, env)
    if (!is.function(f) || n %in% FN || !identical(get(n, envir = app_env), f)) next
    for (u in intersect(all.names(body(f)), FN))
      out[[paste(n, u, sep = " -> ")]] <- deparse(body(get(u, envir = environment(f))))
  }
  out[["app.R -> forecasting_set_estimate"]] <- deparse(body(get("forecasting_set_estimate", envir = app_env)))
  out
}, args = list(raiz = normalizePath(".", winslash = "/"), como = como, FN = FN, sin_red = aislar_global_sin_red))

ref <- new.env()
for (e in parse("R/forecasting_resolve.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% FN) eval(e, ref)
es_la_de_resolve <- function(llegan) all(vapply(names(llegan), function(k) {
  u <- sub(".* -> ", "", k); identical(llegan[[k]], deparse(body(ref[[u]])))
}, TRUE))

for (como in c("shinyapps", "source_app")) {
  llegan <- tryCatch(arrancar(como), error = function(e) { message("  ERROR: ", conditionMessage(e)); NULL })
  ok(sprintf("%s: the app starts and its forecasting callers are found (%d)", como, length(llegan)),
     length(llegan) >= 4L)
  ok(sprintf("%s: every caller reaches forecasting_resolve.R's definition", como),
     !is.null(llegan) && es_la_de_resolve(llegan))
  if (como == "shinyapps")
    ok("shinyapps: Pasivos' engine and wizard, Flujo's export and app.R are among the callers",
       all(c("pasivos_generate_schedule -> forecasting_get_estimate", ".pasivos_wizard_save -> forecasting_set_estimate",
             ".suggest_fx_rate -> forecasting_get_estimate", "app.R -> forecasting_set_estimate") %in% names(llegan)))
}

# ── 2. forecasting_get_estimate reads as prod did ─────────────────────────────
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
source("tests/helpers/s3_memoria.R")
for (f in c("R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R", "R/forecasting_resolve.R"))
  suppressMessages(source(f))
CID <- "acme"; KEY <- paste0(CID, "/", S3_KEYS$pasivos_estimates)
est <- function(metric, fecha, value, frozen = FALSE)
  tibble::tibble(metric = metric, fecha = as.Date(fecha), value = value, source_method = "manual",
                 is_frozen = frozen, updated_by = "t", updated_at = as.POSIXct("2026-11-01 10:00:00"))
poner <- function(df) { s3_memoria_reset(); assign(KEY, df, envir = .mem) }
g <- function(metric, fecha) forecasting_get_estimate(metric, as.Date(fecha), client_id = CID)

poner(dplyr::bind_rows(est("sofr", "2026-11-01", 4.1), est("sofr", "2026-11-05", 4.5), est("sofr", "2026-11-09", 4.9)))
ok("get: the exact date's value", identical(g("sofr", "2026-11-05"), 4.5))
ok("get: no exact date -> the latest one before", identical(g("sofr", "2026-11-07"), 4.5))
ok("get: before every date -> the earliest known", identical(g("sofr", "2026-10-01"), 4.1))
poner(dplyr::bind_rows(est("sofr", "2026-11-05", 4.5), est("sofr", "2026-11-01", 3.0, frozen = TRUE)))
ok("get: a frozen value wins over every date", identical(g("sofr", "2026-11-05"), 3.0))
poner(est("fx_usd_mxn", "2026-11-01", 18.2))
ok("get: the name is taken as given (usd_mxn is not fx_usd_mxn)",
   identical(g("fx_usd_mxn", "2026-11-02"), 18.2) && is.na(g("usd_mxn", "2026-11-02")))
ok("get: an unknown metric is NA", is.na(g("tiie28", "2026-11-02")))
.mem_falla <- KEY
ok("get: a failed read is NA, not an error", is.na(g("fx_usd_mxn", "2026-11-02")))
.mem_falla <- character()
ok("list_metrics: the metrics in the Pasivos estimates",
   identical(forecasting_list_metrics(client_id = CID), "fx_usd_mxn"))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
