# =============================================================================
# tests/test_ic_filter_company_map_session.R
# apply_ic_filter()'s name fallback (Parte = one of our own companies -> IC)
# read the GLOBAL COMPANY_MAP, as did Vencidos' own copy of that layer. Every
# session rewrites that global and production is one shared process, so the
# filter could treat another client's company names as intercompany and miss
# the session's own. The cash-flow export also fell back to the global for its
# file-name initials whenever the session's map was empty (c() is NULL).
#
# apply_ic_filter() now takes the session's map as a required `cmap`, and
# every caller passes shared$company_map().
#
# Run from project root:  Rscript tests/test_ic_filter_company_map_session.R
# =============================================================================
cat("=== test_ic_filter_company_map_session ===\n\n")
`%||%` <- function(a, b) if (is.null(a)) b else a

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# Extract the real function (no S3, no app).
env <- new.env()
for (e in parse("R/data_pipeline.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c("apply_ic_filter", "normalize_code"))
    eval(e, env)
env$`%||%` <- `%||%`

session_map <- c(NRS = "Networks Realtors", NTS = "Networks Trucking Services")
run <- function(...) tryCatch(env$apply_ic_filter(...), error = function(e) { message("  ERROR: ", conditionMessage(e)); NULL })
df <- data.frame(Parte = c("Networks Realtors", "Otra Empresa", "Cliente Externo"),
                 Saldo = c(100, 200, 300), stringsAsFactors = FALSE)

for (global in list(c(), c(HDX = "Otra Empresa"))) {
  assign("COMPANY_MAP", global, envir = .GlobalEnv)   # the old code read it from here
  etiqueta <- if (length(global)) "another client's map" else "an empty map"

  ex <- run(df, "exclude", NULL, character(), cmap = session_map)
  ok(sprintf("exclude drops the session's own company (global holds %s)", etiqueta),
     identical(ex$Parte, c("Otra Empresa", "Cliente Externo")))
  only <- run(df, "only", NULL, character(), cmap = session_map)
  ok(sprintf("only keeps just the session's own company (global holds %s)", etiqueta),
     identical(only$Parte, "Networks Realtors"))
  inc <- run(df, "include", NULL, character(), cmap = session_map)
  ok(sprintf("include is a no-op (global holds %s)", etiqueta), identical(inc, df))
  emp <- run(df, "exclude", NULL, character(), cmap = c())
  ok(sprintf("an empty session map filters nothing by name (global holds %s)", etiqueta),
     identical(emp, df))
}
rm("COMPANY_MAP", envir = .GlobalEnv)

ok("a call without cmap fails loudly instead of using a global",
   tryCatch({ env$apply_ic_filter(df, "include", NULL, character()); FALSE },
            error = function(e) TRUE))

# Every caller passes the session's map.
calls <- list()
walk <- function(x, file) {
  if (is.call(x)) {
    if (identical(x[[1]], as.name("apply_ic_filter")))
      calls[[length(calls) + 1L]] <<- list(file = file, call = x)
    args <- as.list(x)[-1]
    for (i in seq_along(args)) if (!identical(args[[i]], quote(expr = ))) walk(args[[i]], file)
  }
}
for (f in c("R/ledger_module.R", "R/cashflow_export_module.R",
            "R/search_module.R", "R/vencidos_module.R"))
  for (e in parse(f)) walk(e, f)
ok("found the 6 apply_ic_filter() calls", length(calls) == 6L)
for (cl in calls)
  ok(sprintf("%s: apply_ic_filter() passes cmap = shared$company_map()", cl$file),
     "cmap" %in% names(cl$call) &&
       identical(deparse(cl$call$cmap), "shared$company_map()"))

# No global reads left in these files.
for (f in c("R/vencidos_module.R", "R/cashflow_export_module.R",
            "R/ledger_module.R", "R/search_module.R")) {
  src <- readLines(f, warn = FALSE)
  src <- src[!grepl("^\\s*#", src)]
  ok(sprintf("%s does not read COMPANY_MAP", f), !any(grepl("COMPANY_MAP", src, fixed = TRUE)))
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
