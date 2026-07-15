# =============================================================================
# R/sap_cache.R
# Process-wide SAP AR/AP snapshot cache, keyed by client id — lets a second
# session on the same R process (e.g. shinymanager's auth-gate + real-app
# session pair) skip a redundant fetch, WITHOUT ever serving one client's
# data to a different client's session. Every reader/writer of
# .GlobalEnv$.sap_global_cache must go through these functions.
# =============================================================================

.sap_cache_get <- function(client_id) {
  store <- .GlobalEnv$.sap_global_cache
  if (is.null(store)) return(NULL)
  store[[tolower(client_id %||% "")]]
}

.sap_cache_set <- function(client_id, ar, ap) {
  if (is.null(.GlobalEnv$.sap_global_cache)) .GlobalEnv$.sap_global_cache <- list()
  .GlobalEnv$.sap_global_cache[[tolower(client_id %||% "")]] <- list(
    AR = ar, AP = ap, fetched_at = Sys.time()
  )
  invisible(TRUE)
}

.sap_cache_fresh <- function(entry, max_age_secs = 300) {
  if (is.null(entry) || is.null(entry$fetched_at)) return(FALSE)
  as.numeric(difftime(Sys.time(), entry$fetched_at, units = "secs")) < max_age_secs
}
