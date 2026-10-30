# =============================================================================
# tests/test_bancos_staging_company_map_session.R
# Several screens took company names from the GLOBAL COMPANY_MAP, which every
# session rewrites; production is one shared process, so they could show
# another client's companies:
#   - Bancos: company names in the balance ticker, the accounts and balances
#     tables, and the TXT import account picker;
#   - the "Abono parcial" modal: its Empresa dropdown WAS the global map;
#   - the manual-entry edit modal: its Empresa fallback added the global map;
#   - Proveedores' "Importar Excel" tab: its initial Empresa choices.
# They now use the session's map (or start empty and let the server fill it).
#
# Run from project root:  Rscript tests/test_bancos_staging_company_map_session.R
# =============================================================================
cat("=== test_bancos_staging_company_map_session ===\n\n")
suppressMessages({ library(shiny) })
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
extract("R/bancos_module.R",         c(".cmap_lookup", ".company_name"), env)
# The staging file holds only definitions; load them all (CSS/JS constants included).
for (e in parse("R/staging_browse_module.R")) eval(e, env)
extract("R/proveedores_module.R",    ".prov_importar_tab", env)
env$`%||%` <- `%||%`
env$CURRENCIES <- c("MXN", "USD")
shown <- NULL
env$showModal <- function(ui, ...) shown <<- as.character(ui)

session_map <- c(NRS = "Networks Realtors", NTS = "Networks Trucking Services")

for (global in list(c(), c(HDX = "Otra Empresa", NRS = "Otra Realtors"))) {
  env$COMPANY_MAP <- global
  etiqueta <- if (length(global)) "another client's map" else "an empty map"

  ok(sprintf("Bancos names a company from the session's map (global holds %s)", etiqueta),
     identical(env$.company_name("NRS", session_map), "Networks Realtors"))
  ok(sprintf("Bancos keeps the initials with an empty session map (global holds %s)", etiqueta),
     identical(env$.company_name("NRS", c()), "NRS"))

  shown <- NULL
  tryCatch(env$show_abono_modal(function() list(), NULL, empresa_vals = unname(session_map)),
           error = function(e) message("  show_abono_modal: ", conditionMessage(e)))
  ok(sprintf("the Abono modal lists the session's companies (global holds %s)", etiqueta),
     !is.null(shown) && grepl("Networks Realtors", shown, fixed = TRUE))
  ok(sprintf("the Abono modal never lists %s", etiqueta),
     !is.null(shown) && !grepl("Otra ", shown, fixed = TRUE))

  html <- tryCatch(as.character(env$.prov_importar_tab(NS("prov_mod"))), error = function(e) "")
  ok(sprintf("Proveedores' import tab builds and never lists %s", etiqueta),
     nzchar(html) && !grepl("Otra ", html, fixed = TRUE))
}

ok("an empty map gives one NA per key",
   identical(env$.cmap_lookup(c(), c("A", "B")), c(NA_character_, NA_character_)))
ok("show_combined_entry_modal's default company list is empty, not the global",
   identical(formals(env$show_combined_entry_modal)$empresa_vals, quote(character(0))))

app <- readLines("app.R", warn = FALSE)
ok("app.R passes the session's companies to show_abono_modal()",
   any(grepl("show_abono_modal(sap_data, session, empresa_vals = unname(company_map_rv()))",
             app, fixed = TRUE)))

for (f in c("R/bancos_module.R", "R/staging_browse_module.R",
            "R/manual_entry_handlers.R", "R/proveedores_module.R")) {
  src <- readLines(f, warn = FALSE)
  src <- src[!grepl("^\\s*#", src)]
  ok(sprintf("%s does not read COMPANY_MAP", f), !any(grepl("COMPANY_MAP", src, fixed = TRUE)))
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
