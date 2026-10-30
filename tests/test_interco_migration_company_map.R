# =============================================================================
# tests/test_interco_migration_company_map.R
# load_interco_v2()'s one-time migration from the legacy flat intercompany
# file built the per-company registry from the GLOBAL COMPANY_MAP and SAVED it
# to S3. Every session rewrites that global and production is one shared
# process, so the saved registry could be keyed by another client's companies.
# The migration now uses the migrated client's own empresas.rds.
#
# Also: fetch_all_companies() and sap_companies_available() defaulted `cmap`
# to the global; it is now required.
#
# S3 is never touched: every loader/saver below is a fake defined in the
# test's own environment (no package or global function is patched, so there
# is nothing to restore), and aws.s3 is never loaded.
#
# Run from project root:  Rscript tests/test_interco_migration_company_map.R
# =============================================================================
cat("=== test_interco_migration_company_map ===\n\n")
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
extract("R/persistence.R", c("load_interco_v2", ".migrate_interco_v1_to_v2", ".empty_interco_v2",
                             ".company_map_from_empresas"), env)
extract("R/sap_api.R", c("fetch_all_companies", "sap_companies_available"), env)
env$`%||%` <- `%||%`

# Fakes: no v2 registry yet, a legacy v1 file with codes, and this client's empresas.
saved <- NULL
env$S3_KEYS        <- list(interco_v2 = "interco_v2.rds")
env$.s3_read_with  <- function(key, client_id = NULL) NULL
env$load_interco   <- function(client_id = NULL) list(ar_clients = c("C100"), ap_suppliers = c("P200"))
env$save_interco_v2 <- function(registry, client_id = NULL) { saved <<- registry; invisible(registry) }
own_empresas <- data.frame(initials = c("NRS", "NTS", "OLD"),
                           nombre_corto = c("Networks Realtors", "Networks Trucking Services", "Vieja"),
                           activa = c(TRUE, TRUE, TRUE), deleted = c(FALSE, FALSE, TRUE),
                           stringsAsFactors = FALSE)
env$load_empresas <- function(client_id = NULL) own_empresas
stopifnot(!"aws.s3" %in% loadedNamespaces())

for (global in list(c(), c(HDX = "Otra Empresa"))) {
  assign("COMPANY_MAP", global, envir = .GlobalEnv)   # the old code read it from here
  etiqueta <- if (length(global)) "another client's map" else "an empty map"

  saved <- NULL
  reg <- tryCatch(env$load_interco_v2(), error = function(e) { message("  ERROR: ", conditionMessage(e)); NULL })
  ok(sprintf("the migration saves a registry (global holds %s)", etiqueta), is.list(saved))
  ok(sprintf("the saved registry is keyed by the client's own active companies, not %s", etiqueta),
     identical(sort(names(saved$companies)), c("NRS", "NTS")))
  ok(sprintf("the legacy codes are carried over (global holds %s)", etiqueta),
     identical(saved$companies$NRS$ar, "100") && identical(saved$companies$NRS$ap, "200"))
}
rm("COMPANY_MAP", envir = .GlobalEnv)

env$load_empresas <- function(client_id = NULL) own_empresas[0, ]
saved <- NULL
invisible(tryCatch(env$load_interco_v2(), error = function(e) NULL))
ok("a client with no companies migrates to the ALL bucket, not someone else's companies",
   identical(names(saved$companies), "ALL"))

ok(".company_map_from_empresas keeps active, non-deleted companies",
   identical(env$.company_map_from_empresas(own_empresas),
             c(NRS = "Networks Realtors", NTS = "Networks Trucking Services")))
ok(".company_map_from_empresas gives an empty map for a missing table",
   is.null(env$.company_map_from_empresas(NULL)))

for (fn in c("fetch_all_companies", "sap_companies_available"))
  ok(sprintf("%s: cmap has no default", fn),
     identical(formals(get(fn, env))$cmap, quote(expr = )))

for (f in c("R/persistence.R", "R/sap_api.R")) {
  src <- readLines(f, warn = FALSE)
  src <- src[!grepl("^\\s*#", src)]
  ok(sprintf("%s does not read COMPANY_MAP", f), !any(grepl("COMPANY_MAP", src, fixed = TRUE)))
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
