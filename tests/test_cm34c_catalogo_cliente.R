# =============================================================================
# tests/test_cm34c_catalogo_cliente.R
# CM34c, first commit. Ajustes › Políticas read the catalog (and the partner
# assignments) from the session's copy, and when that copy was empty -- a
# client without policies -- fell back to load_policy_catalog() WITHOUT
# client_id: the CLIENT_ID prefix's catalog. Creating the first policy of a
# client then saved that other catalog plus the new policy into the client
# being viewed. Now every fallback names the client being viewed.
#   1  staff viewing client "bbb" (no policies) under a deployment whose
#      CLIENT_ID is "home" (with two policies) creates bbb's first policy:
#      bbb ends with that one only; home is untouched;
#   2  no loader of the catalog or the assignments is ever called without
#      client_id, in Políticas or in Socios (settings_companies.R);
#   3  every call in R/settings/ of these loaders names a client.
# The real observers (testServer) over the in-memory S3. On master, 1 and 2 fail.
# Run from project root:  Rscript tests/test_cm34c_catalogo_cliente.R
# =============================================================================
cat("=== test_cm34c_catalogo_cliente ===\n\n")
source("tests/helpers/s3_memoria.R")
suppressPackageStartupMessages(library(shiny))
Sys.setenv(CLIENT_ID = "home")                 # after the helper, which sets its own
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R", "R/policy_engine.R",
            "R/hd_registro.R", "R/hd_piezas.R", "R/monedas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/settings/settings_policies.R", "R/settings/settings_companies.R"))
  suppressMessages(source(f, encoding = "UTF-8"))
.i18n_dir <- file.path(getwd(), "www", "i18n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

log_action <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
showNotification <- function(...) invisible("id")
showModal <- function(...) invisible(NULL)
removeModal <- function(...) invisible(NULL)
show_settings_modal <- function(...) invisible(NULL)

# Every read of the catalog and the assignments, with the client it names.
LECTURAS <- character()
local({
  orig_cat <- load_policy_catalog; orig_pp <- load_partner_policies
  assign("load_policy_catalog", function(client_id = NULL) {
    LECTURAS <<- c(LECTURAS, paste0("catalogo:", client_id %||% "<sin client_id>")); orig_cat(client_id = client_id)
  }, envir = globalenv())
  assign("load_partner_policies", function(client_id = NULL) {
    LECTURAS <<- c(LECTURAS, paste0("socios:", client_id %||% "<sin client_id>")); orig_pp(client_id = client_id)
  }, envir = globalenv())
})

pol <- function(id, nombre) tibble::tibble(id = id, name = nombre, type = "weekdays", params = list(list()),
                                           roll_direction = "forward", created_by = "x",
                                           created_at = Sys.time(), updated_at = Sys.time())
k <- function(cid, key) paste0(cid, "/", key)
s3_memoria_reset()
assign(k("home", S3_KEYS$policy_catalog), dplyr::bind_rows(pol("h1", "Viernes home"), pol("h2", "Quincena home")), envir = .mem)
assign(k("home", S3_KEYS$partner_policies),
       tibble::tibble(parte = "PROV HOME", policy_id = "h1", policy_order = 1L, ledger = "AP",
                      is_interco = FALSE, is_manual = FALSE, linked_by = "x", linked_at = Sys.time()), envir = .mem)
home_antes <- get(k("home", S3_KEYS$policy_catalog), envir = .mem)

staff <- list(user = "staff", tier = "admin", permisos = permisos_resueltos("admin"))
shared <- list(usuario = reactiveVal(staff), current_user = function() "staff",
               current_user_info = reactive(list(username = "staff", tier = "admin")),
               effective_client_id = function() "bbb", home_client_id = function() "home",
               ctx_no_cargadas = reactiveVal(character()),
               company_map = reactive(c(BB = "Bbb SA")), sap_data = reactiveVal(NULL),
               policy_catalog_db = reactiveVal(.schema_policy_catalog()),   # bbb has no policies
               partner_policies_db = reactiveVal(NULL), policy_moves_db = reactiveVal(NULL),
               holiday_overrides_db = reactiveVal(NULL), moves_db = reactiveVal(NULL))

res <- tryCatch(testServer(function(input, output, session) {
  settings_policies_observer(input, output, session, shared)
  settings_companies_observer(input, output, session, shared)
}, {
  session$flushReact()
  session$setInputs(pol_btn_new = 1)
  session$setInputs(pol_name = "Lunes bbb", pol_type = "weekdays", pol_btn_save = 1)
  session$setInputs(pol_action = list(action = "assign", id = "x", name = "x", nonce = 1))
  session$flushReact()
  TRUE
}), error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })

bbb <- tryCatch(get(k("bbb", S3_KEYS$policy_catalog), envir = .mem), error = function(e) NULL)
ok("1a bbb's first policy is saved in bbb", is.data.frame(bbb) && "Lunes bbb" %in% bbb$name)
ok("1b ...and it is the only one: none of home's policies came along",
   is.data.frame(bbb) && nrow(bbb) == 1L && !any(c("h1", "h2") %in% bbb$id))
ok("1c home's catalog is untouched", identical(get(k("home", S3_KEYS$policy_catalog), envir = .mem), home_antes))

for (l in unique(LECTURAS)) cat("   lectura:", l, "\n")
ok("2 no read of the catalog or the assignments without client_id (Políticas and Socios)",
   !any(grepl("<sin client_id>", LECTURAS, fixed = TRUE)))

codigo <- unlist(lapply(list.files("R/settings", pattern = "\\.R$", full.names = TRUE), function(f)
  sub("#.*$", "", readLines(f, warn = FALSE, encoding = "UTF-8"))))
ok("3 every call in R/settings/ of these loaders names a client",
   !any(grepl("load_(policy_catalog|partner_policies|parte_alias_map)\\(\\s*\\)", codigo)))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
