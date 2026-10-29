# =============================================================================
# tests/test_proveedores_bulk_inactivar.R
# Catálogo de proveedores: the search box filters the table in R, so DT's
# _rows_selected are positions in the FILTERED table -- but "Inactivar
# selección" applied them to the unfiltered active list, inactivating other
# providers than the ones marked. Its confirm also rewrote the catalog with
# active rows only, dropping any row whose activo flag was not TRUE.
#
# Drives the real proveedoresServer with shiny::testServer. Runs in its own
# Rscript process: the S3 save functions are replaced with in-memory fakes
# here, and nothing else runs in this process.
# Run from project root:  Rscript tests/test_proveedores_bulk_inactivar.R
# =============================================================================

suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}
strip_accents <- function(x) stringi::stri_trans_general(x %||% "", "Latin-ASCII")
source("R/persistence.R")          # .schema_proveedores
source("R/proveedores_module.R")

# In-memory fakes for the writes this module does (this process only).
.saved <- new.env()
save_proveedores           <- function(df, client_id = NULL) { .saved$provs <- df; invisible(TRUE) }
save_proveedores_inactivos <- function(df, client_id = NULL) {
  if (isTRUE(.saved$fail_inac)) stop("simulated S3 outage")
  .saved$inac <- df; invisible(TRUE)
}
bump_sync_version <- function(...) invisible(NULL)
.logged <- list()
log_action <- function(...) { .logged[[length(.logged) + 1L]] <<- list(...); invisible(NULL) }

.pass <- 0L; .fail <- 0L
.chk <- function(ok, label) {
  if (isTRUE(ok)) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else            { cat(sprintf("  FAIL  %s\n", label)); .fail <<- .fail + 1L }
}

# Old code has neither reactiveVal; report instead of aborting.
.try <- function(x) tryCatch(x, error = function(e) "(missing)")

mk <- function(id, nombre, activo = TRUE) {
  r <- .schema_proveedores()[NA_integer_, ]
  r$id <- id; r$nombre <- nombre; r$activo <- activo; r$status <- "completo"
  r
}
base_provs <- dplyr::bind_rows(
  mk("P1", "Alfa Servicios"), mk("P2", "Beta Logística"), mk("P3", "Gamma Fletes"),
  mk("P4", "Delta Sin Bandera", activo = NA)
)

run_case <- function(label, fail_inac = FALSE) {
  .saved$provs <- NULL; .saved$inac <- NULL; .saved$fail_inac <- fail_inac
  provs_rv <- reactiveVal(base_provs)
  inac_rv  <- reactiveVal(.schema_proveedores())
  shared <- list(
    company_map              = function() c(AAA = "Empresa A"),
    current_user             = function() "tester",
    effective_client_id      = function() "test-client",
    home_client_id           = function() "test-client",
    proveedores_db           = provs_rv,
    proveedores_inactivos_db = inac_rv
  )
  cat("\n--", label, "\n")
  shiny::testServer(proveedoresServer, args = list(shared = shared), {
    # Search "gamma": the table shows only Gamma Fletes, as row 1.
    session$setInputs(cat_search = "gamma")
    session$flushReact()
    invisible(tryCatch(output$tbl_catalogo_prov, error = function(e) NULL))
    cat("  table shows:", paste(.try(catalogo_ids()), collapse = ", "), "\n")
    .chk(identical(.try(catalogo_ids()), "P3"), "search 'gamma' shows only P3")

    session$setInputs(tbl_catalogo_prov_rows_selected = 1L, cat_inactivar_bulk = 1L)
    cat("  modal will inactivate:", paste(.try(bulk_inactivar_ids()), collapse = ", "), "\n")
    .chk(identical(.try(bulk_inactivar_ids()), "P3"), "modal targets the marked provider (P3), not P1")

    session$setInputs(bulk_inactivar_confirm = 1L)
    if (!fail_inac) {
      cat("  catalog after:", paste(provs_rv()$id, collapse = ", "),
          "| inactivos:", paste(inac_rv()$id, collapse = ", "), "\n")
      .chk(identical(sort(inac_rv()$id), "P3"), "only P3 moved to inactivos")
      .chk(identical(sort(provs_rv()$id), c("P1", "P2", "P4")),
           "catalog keeps P1, P2 and the non-TRUE-activo P4")
    } else {
      .chk(identical(provs_rv()$id, base_provs$id), "failed save leaves the catalog untouched")
      .chk(length(.logged) == 1L && identical(.logged[[1]]$action, "inactivar_proveedores_fallido"),
           "failed save is logged with log_action")
    }
  })
}

run_case("search + mark row 1 + confirm")
run_case("same, but the S3 write fails", fail_inac = TRUE)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
