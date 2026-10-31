# =============================================================================
# tests/test_treasury_map_company_map_session.R
# The treasury map read the GLOBAL COMPANY_MAP in three places: as the default
# of .tm_parte_to_ini() (and the edge invoice table called it without a map),
# as the error fallback of .tm_bank_balances(), and as the "Empresa objetivo"
# choices of treasuryMapUI(), which is built once at startup inside the static
# ui. Every session rewrites that global and production is one shared
# process, so all three could use another client's companies.
#
# Run from project root:  Rscript tests/test_treasury_map_company_map_session.R
# =============================================================================
cat("=== test_treasury_map_company_map_session ===\n\n")
suppressMessages({ library(dplyr); library(shiny) })
`%||%` <- function(a, b) if (is.null(a)) b else a

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# Extract the real functions (no S3, no app).
env <- new.env()
for (e in parse("R/treasury_map_module.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c(".tm_parte_to_ini", ".tm_bank_balances", "treasuryMapUI"))
    eval(e, env)
env$`%||%` <- `%||%`

session_map <- c(NRS = "Networks Realtors", NTS = "Networks Trucking Services")
shared_err  <- list(company_map = function() stop("not ready"),
                    ctas_cuentas = function() NULL)

for (global in list(c(), c(HDX = "Otra Empresa", NRS = "Otra Realtors"))) {
  env$COMPANY_MAP <- global
  etiqueta <- if (length(global)) "another client's map" else "an empty map"

  ok(sprintf(".tm_parte_to_ini: cmap has no default (global holds %s)", etiqueta),
     identical(formals(env$.tm_parte_to_ini)$cmap, quote(expr = )))
  ok(sprintf(".tm_parte_to_ini uses the session's map (global holds %s)", etiqueta),
     identical(unname(env$.tm_parte_to_ini(c("Networks Realtors", "Otra Realtors"), session_map)),
               c("NRS", NA_character_)))

  bal <- env$.tm_bank_balances(shared_err)
  ok(sprintf(".tm_bank_balances with a failing session map does not use %s", etiqueta),
     length(bal) == 0L)

  html <- tryCatch(as.character(env$treasuryMapUI("icmap")), error = function(e) {
    message("  treasuryMapUI: ", conditionMessage(e)); "" })
  ok(sprintf("treasuryMapUI builds (global holds %s)", etiqueta), nzchar(html))
  ok(sprintf("treasuryMapUI's static choices never carry %s", etiqueta),
     !grepl("Otra Empresa", html) && !grepl("Otra Realtors", html))
}

# Every .tm_parte_to_ini() call in the module passes a map.
calls <- list()
walk <- function(x) {
  if (is.call(x)) {
    if (identical(x[[1]], as.name(".tm_parte_to_ini"))) calls[[length(calls) + 1L]] <<- x
    args <- as.list(x)[-1]
    for (i in seq_along(args)) if (!identical(args[[i]], quote(expr = ))) walk(args[[i]])
  }
}
for (e in parse("R/treasury_map_module.R")) walk(e)
# 3 since the edge table and "Agregar al plan" share edge_invoices() (ff90084).
ok("the module calls .tm_parte_to_ini at least 3 times", length(calls) >= 3L)
ok("every .tm_parte_to_ini() call passes a map",
   all(vapply(calls, function(cl) length(cl) == 3L, logical(1))))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
