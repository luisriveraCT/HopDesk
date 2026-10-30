# =============================================================================
# tests/test_pasivos_company_map_session.R
# Pasivos took its list of valid companies from shared$company_map() but, if
# that call failed, fell back to the GLOBAL COMPANY_MAP. Every session
# rewrites that global and production is one shared process, so the wizard
# could offer -- and validate against -- another client's companies. The
# fallback is now an empty list: nothing valid rather than someone else's.
#
# Run from project root:  Rscript tests/test_pasivos_company_map_session.R
# =============================================================================
cat("=== test_pasivos_company_map_session ===\n\n")
`%||%` <- function(a, b) if (is.null(a)) b else a

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# Extract the real save handler (no S3, no app) and stop it at validation.
env <- new.env()
for (e in parse("R/pasivos_wizard_module.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      identical(as.character(e[[2]]), ".pasivos_wizard_save"))
    eval(e, env)
seen <- NULL
env$pasivos_wizard_validate_final <- function(draft, empresa_choices) {
  seen <<- empresa_choices
  list(ok = FALSE, errors = list(empresa = "x"))
}

session_map  <- c(NRS = "Networks Realtors", NTS = "Networks Trucking Services")
shared_ok    <- list(company_map = function() session_map)
shared_error <- list(company_map = function() stop("not ready"))

for (global in list(c(), c(HDX = "Otra Empresa"))) {
  env$COMPANY_MAP <- global
  etiqueta <- if (length(global)) "another client's map" else "an empty map"

  seen <- NULL
  env$.pasivos_wizard_save(new.env(), shared_ok, NULL, "dev")
  ok(sprintf("the wizard validates against the session's companies (global holds %s)", etiqueta),
     identical(seen, c("NRS", "NTS")))

  seen <- NULL
  env$.pasivos_wizard_save(new.env(), shared_error, NULL, "dev")
  ok(sprintf("if the session's map fails, the valid list is empty, not %s", etiqueta),
     identical(seen, character(0)))
}

# The other five fallbacks live inside server closures: check the source.
for (f in c("R/pasivos_module.R", "R/pasivos_wizard_module.R", "R/pasivos_table_module.R")) {
  src <- readLines(f, warn = FALSE)
  src <- src[!grepl("^\\s*#", src)]
  ok(sprintf("%s does not read COMPANY_MAP", f), !any(grepl("COMPANY_MAP", src, fixed = TRUE)))
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
