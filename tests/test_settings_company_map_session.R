# =============================================================================
# tests/test_settings_company_map_session.R
# The Settings panels and forms took the company list from a `cmap` argument
# that defaulted to the GLOBAL COMPANY_MAP. Every session rewrites that global
# when its empresas load, and production is one shared process, so the
# default could be another client's companies. One caller (the BP table's
# "Actualizar" button, settings_hub.R) did not pass the session's map at all.
# `cmap` is now required, and every caller passes the session's map.
#
# Run from project root:  Rscript tests/test_settings_company_map_session.R
# =============================================================================
cat("=== test_settings_company_map_session ===\n\n")
suppressMessages({ library(dplyr); library(shiny) })
`%||%` <- function(a, b) if (is.null(a)) b else a

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# Extract the real functions (no S3, no app).
extract <- function(file, names, env) {
  for (e in parse(file))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% names)
      eval(e, env)
}
env <- new.env()
extract("R/settings/settings_cuentas.R",     c(".cuentas_panel_ui", ".cuenta_form_ui"), env)
extract("R/settings/settings_proveedores.R", c(".catalogo_panel_ui", ".catalogo_form_ui"), env)
extract("R/settings/settings_bp.R",          ".bp_datatable", env)
extract("R/data_pipeline.R",                 "scan_ic_candidates", env)
env$`%||%` <- `%||%`

session_map <- c(NRS = "Networks Realtors", NTS = "Networks Trucking Services")

for (global in list(c(), c(HDX = "Otra Empresa"))) {
  env$COMPANY_MAP <- global
  etiqueta <- if (length(global)) "another client's map" else "an empty map"

  for (fn in c(".cuentas_panel_ui", ".cuenta_form_ui", ".catalogo_panel_ui",
               ".catalogo_form_ui", ".bp_datatable", "scan_ic_candidates")) {
    f <- get(fn, env)
    ok(sprintf("%s: cmap has no default (global holds %s)", fn, etiqueta),
       "cmap" %in% names(formals(f)) && identical(formals(f)$cmap, quote(expr = )))
  }

  # Forms built with the session's map list its companies and never the global's.
  html <- tryCatch(as.character(env$.cuentas_panel_ui(cmap = session_map)), error = function(e) "")
  ok(sprintf("accounts panel lists the session's companies, not %s", etiqueta),
     grepl("Networks Realtors", html) && !grepl("Otra Empresa", html))
  html <- tryCatch(as.character(env$.catalogo_form_ui(NULL, cmap = session_map)), error = function(e) "")
  ok(sprintf("supplier form lists the session's companies, not %s", etiqueta),
     grepl("Networks Trucking Services", html) && !grepl("Otra Empresa", html))
  # Without a map, the forms do not fall back to the global.
  html <- tryCatch(as.character(env$.catalogo_form_ui(NULL)), error = function(e) "")
  ok(sprintf("supplier form without a map never shows %s", etiqueta), !grepl("Otra Empresa", html))
  html <- tryCatch(as.character(env$.cuentas_panel_ui()), error = function(e) "")
  ok(sprintf("accounts panel without a map never shows %s", etiqueta), !grepl("Otra Empresa", html))
}

# Every scan_ic_candidates() call in Settings passes the session's map.
calls <- list()
walk <- function(x) {
  if (is.call(x)) {
    if (identical(x[[1]], as.name("scan_ic_candidates"))) calls[[length(calls) + 1L]] <<- x
    args <- as.list(x)[-1]
    for (i in seq_along(args)) if (!identical(args[[i]], quote(expr = ))) walk(args[[i]])
  }
}
for (e in parse("R/settings/settings_hub.R")) walk(e)
ok("settings_hub.R calls scan_ic_candidates at least 4 times", length(calls) >= 4L)
ok("every scan_ic_candidates() call passes shared$company_map()",
   all(vapply(calls, function(cl) length(cl) == 5L && grepl("company_map()", deparse(cl[[5]]), fixed = TRUE),
              logical(1))))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
