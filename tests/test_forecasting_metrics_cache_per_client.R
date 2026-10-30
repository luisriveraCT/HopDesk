# =============================================================================
# tests/test_forecasting_metrics_cache_per_client.R
# R/forecasting_sources.R kept ONE process-wide metrics cache, filled with the
# catalog of whichever client called .lookup_metric() first; later calls for
# any other client_id were answered from it. Production is one shared
# process, so a client could get another client's forecasting metrics. The
# cache is now keyed by client.
#
# S3 is never touched: load_forecasting_metrics() is a fake defined in the
# test's own environment (no package or global function is patched).
#
# Run from project root:  Rscript tests/test_forecasting_metrics_cache_per_client.R
# =============================================================================
cat("=== test_forecasting_metrics_cache_per_client ===\n\n")
`%||%` <- function(a, b) if (is.null(a)) b else a

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# Load the real cache and lookup code into its own environment.
env <- new.env()
for (e in parse("R/forecasting_sources.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c(".fcs_metrics_cache", ".fcs_sources_cache", ".fcs_client_key",
                                  ".lookup_metric", "fcs_flush_caches"))
    eval(e, env)

catalogs <- list(
  acme  = data.frame(metric_id = "ventas", label = "Ventas de Acme",  stringsAsFactors = FALSE),
  otros = data.frame(metric_id = "ventas", label = "Ventas de Otros", stringsAsFactors = FALSE))
loads <- character()
env$load_forecasting_metrics <- function(client_id = NULL) {
  loads <<- c(loads, client_id %||% "<null>")
  catalogs[[client_id %||% "acme"]]
}
label_for <- function(client_id) {
  m <- tryCatch(env$.lookup_metric("ventas", client_id = client_id), error = function(e) {
    message("  ERROR: ", conditionMessage(e)); NULL })
  m$label %||% NA_character_
}

ok("the first client gets its own metric", identical(label_for("acme"), "Ventas de Acme"))
ok("the second client gets ITS metric, not the first client's",
   identical(label_for("otros"), "Ventas de Otros"))
ok("asking the first client again still gives its own", identical(label_for("acme"), "Ventas de Acme"))
ok("each client's catalog is loaded once", identical(loads, c("acme", "otros")))

invisible(env$fcs_flush_caches())
loads <- character()
ok("after a flush the second client reloads its own catalog",
   identical(label_for("otros"), "Ventas de Otros") && identical(loads, "otros"))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
