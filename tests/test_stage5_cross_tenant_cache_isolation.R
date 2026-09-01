# =============================================================================
# tests/test_stage5_cross_tenant_cache_isolation.R
#
# CRITICAL fix (2026-08-27), found while investigating a report that a
# treasurer's ("tesoreria") manually-moved invoice dates appeared to revert
# to raw SAP dates in Calendario: R/global.R's .app_data_cache (.cache_get/
# .cache_set), used by load_moves()/save_moves() and load_interco_v2()/
# save_interco_v2() (and, defensively, every other load_*/save_* pair that
# writes into it) keyed its entries with BARE strings ("moves", "interco_v2",
# ...) -- no client_id at all. In this app's single-shared-deployment model
# (docs/saas_rebuild/ARCHITECTURE.md), one running R process legitimately
# serves multiple different clients' sessions, so whichever client's session
# populated the cache FIRST in a process silently served ITS data (or, if
# that first client had none, an incorrectly EMPTY result) to every other
# client's load_moves()/load_interco_v2() call afterward in that same
# process -- a live cross-tenant data exposure, and the most likely actual
# cause of the reported symptom. Fixed by scoping every cache key with
# client_id, exactly like R/persistence.R's .s3_key() already does.
#
# This file: (A) unit-tests .cache_scoped_key()/.cache_get()/.cache_set()
# directly for cross-client isolation, (B) end-to-end tests load_moves()/
# save_moves() and load_interco_v2()/save_interco_v2() for the same, in the
# EXACT scenario that reproduced the bug (an emptier client's session
# populating the cache first), and (C) a static regression guard asserting
# every .cache_get()/.cache_set() call site in R/persistence.R and app.R
# passes a client_id argument, so a future call site can never reintroduce
# an unscoped entry.
#
# Sourced by _run_saas.R, which does NOT source R/global.R (it builds its
# own minimal S3_KEYS/env setup to avoid global.R's other side effects --
# readRenviron, COMPANY_MAP, live S3_KEYS -- clobbering the mock harness).
# The three cache functions are therefore redefined here, kept byte-for-byte
# in sync with R/global.R's real definitions -- Part C's static check
# guards against that copy silently drifting from the real implementation.
# =============================================================================

cat("── Cross-tenant .app_data_cache isolation (CRITICAL) ──────────────────────\n")

.pass <- 0L
.fail <- 0L
.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}
.chk_true  <- function(cond, label) .chk(isTRUE(cond), TRUE, label)
.chk_null  <- function(x, label) .chk(is.null(x), TRUE, label)

# ── Local copy of R/global.R's cache functions (see file header for why) ────
.app_data_cache <- list()
.cache_scoped_key <- function(key, client_id = NULL) {
  prefix <- if (!is.null(client_id) && nzchar(client_id))
              tolower(trimws(client_id))
            else
              tolower(.client_id())
  paste0(prefix, "/", key)
}
.cache_get <- function(key, client_id = NULL) .app_data_cache[[.cache_scoped_key(key, client_id)]]
.cache_set <- function(key, value, client_id = NULL) {
  .app_data_cache[[.cache_scoped_key(key, client_id)]] <<- value
}

# =============================================================================
# Part A -- .cache_scoped_key() / .cache_get() / .cache_set() direct tests
# =============================================================================
cat("\n-- Part A: cache key scoping --\n")

.chk(.cache_scoped_key("moves", "networks"), "networks/moves", "explicit client_id builds a prefixed key")
.chk(.cache_scoped_key("moves", "NETWORKS"), "networks/moves",
     "client_id is lowercased -- matches .s3_key()'s own convention exactly")
.chk(.cache_scoped_key("moves", "  networks  "), "networks/moves", "client_id is trimmed")

Sys.setenv(CLIENT_ID = "hd-admin")
.chk(.cache_scoped_key("moves", NULL), "hd-admin/moves",
     "NULL client_id falls back to the CLIENT_ID env var, same as .s3_key()")
.chk(.cache_scoped_key("moves"), "hd-admin/moves", "client_id can be omitted entirely with the same fallback")

.cache_set("moves", data.frame(x = 1), "networks")
.chk_true(!is.null(.cache_get("moves", "networks")), "set/get round-trip for the SAME client_id works")
.chk_null(.cache_get("moves", "hd-admin"),
     "get for a DIFFERENT client_id returns NULL -- the actual isolation guarantee")
.chk_null(.cache_get("moves", "beta"),
     "get for a THIRD, unrelated client_id also returns NULL")

.cache_set("moves", data.frame(x = 2), "hd-admin")
val_net <- .cache_get("moves", "networks")
val_hd  <- .cache_get("moves", "hd-admin")
.chk(val_net$x, 1, "networks' own cached value is untouched by hd-admin's later, separate write")
.chk(val_hd$x, 2, "hd-admin's write lands under its own key, not overwriting networks'")

# =============================================================================
# Part B -- end-to-end: load_moves()/save_moves(), the exact reported bug
# =============================================================================
cat("\n-- Part B: load_moves()/save_moves() cross-tenant isolation --\n")

.app_data_cache <<- list()  # reset between parts

mv <- .schema_moves()
mv <- dplyr::bind_rows(mv, tibble::tibble(
  ledger = "AP", Empresa = "NCS", Moneda = "MXN", Documento = "DOC-1",
  FechaVenc_Proyectada = as.Date("2026-08-20"), notas = "",
  moved_by = "tesoreria", last_updated = Sys.time()
))
mock_s3saveRDS(mv, "networks/invoice_moves.rds", "mock-bucket")
mock_s3saveRDS(.schema_moves(), "hd-admin/invoice_moves.rds", "mock-bucket")

# Reproduce the exact bug scenario: the EMPTIER client (hd-admin, staff home,
# un-jumped) loads first in this "process" -- this is what used to poison the
# bare "moves" cache key for every other client's load_moves() call after it.
empty_first <- load_moves(client_id = "hd-admin")
.chk(nrow(empty_first), 0L, "hd-admin (empty) loads first, as in the reported scenario")

# tesoreria's own client (networks) loads next, in the SAME process/cache --
# this must see HER OWN moved invoice, not hd-admin's empty cached result.
tesoreria_moves <- load_moves(client_id = "networks")
.chk_true(nrow(tesoreria_moves) >= 1L,
     "CRITICAL: networks' load_moves() is NOT poisoned by hd-admin's earlier empty cache entry")
.chk_true(any(tesoreria_moves$Documento == "DOC-1" & tesoreria_moves$moved_by == "tesoreria"),
     "CRITICAL: tesoreria's actual moved invoice (DOC-1) is visible, reproducing the fix for the real report")

# And the reverse direction: hd-admin's later load must not pick up networks' data either.
hd_again <- load_moves(client_id = "hd-admin")
.chk(nrow(hd_again), 0L, "hd-admin's cache entry is unaffected by networks' subsequent load (no cross-bleed either way)")

# save_moves() also writes under the correct scoped key
save_moves(mv, client_id = "beta")
beta_moves <- load_moves(client_id = "beta")
.chk_true(any(beta_moves$Documento == "DOC-1"), "save_moves(client_id='beta') is readable back under its own scope")
.chk(nrow(load_moves(client_id = "hd-admin")), 0L,
     "a THIRD client's (beta) save_moves() still does not bleed into hd-admin's cache entry")

# =============================================================================
# Part C -- load_interco_v2()/save_interco_v2() cross-tenant isolation
# =============================================================================
cat("\n-- Part C: load_interco_v2()/save_interco_v2() cross-tenant isolation --\n")

# _run_saas.R's global S3_KEYS (built for the harness, not a full global.R
# source -- see file header) doesn't include interco_v2; add it here. Must
# be <<- (super-assignment), not <-: save_interco_v2()/load_interco_v2() are
# defined in persistence.R (sourced into globalenv()), so they resolve
# S3_KEYS from THEIR OWN defining environment -- a plain `S3_KEYS$x <- y`
# here would only shadow S3_KEYS inside this file's own sourcing
# environment, invisible to those functions.
S3_KEYS[["interco_v2"]] <<- "interco_v2.rds"

.app_data_cache <<- list()

reg_networks <- list(ar_prefix = "C", ap_prefix = "P", rfcs = character(),
                     companies = list(NCS = list(ar = "1027", ap = "2001")))
mock_s3saveRDS(reg_networks, "networks/interco_v2.rds", "mock-bucket")
mock_s3saveRDS(list(ar_prefix = "C", ap_prefix = "P", rfcs = character(), companies = list()),
               "hd-admin/interco_v2.rds", "mock-bucket")

save_interco_v2(reg_networks, client_id = "networks")
hd_ic <- load_interco_v2(client_id = "hd-admin")
.chk_true(!length(hd_ic$companies),
     "hd-admin's interco_v2 has no companies -- unaffected by networks' just-saved registry")

net_ic <- load_interco_v2(client_id = "networks")
.chk_true("NCS" %in% names(net_ic$companies),
     "CRITICAL: networks' own interco_v2 companies are visible, not hd-admin's empty cached entry")

# =============================================================================
# Part D -- static regression guard: every cache call site passes client_id
# =============================================================================
cat("\n-- Part D: static regression guard (every call site scoped) --\n")

.scan_unscoped_cache_calls <- function(file) {
  txt <- readLines(file, warn = FALSE)
  get_lines <- grep('\\.cache_get\\(', txt, value = TRUE)
  set_lines <- grep('\\.cache_set\\(', txt, value = TRUE)
  # Exclude the function DEFINITIONS themselves (R/global.R), only checking
  # call SITES.
  get_lines <- get_lines[!grepl('^\\s*\\.cache_get\\s*<-', get_lines)]
  set_lines <- set_lines[!grepl('^\\s*\\.cache_set\\s*<-', set_lines)]
  # "Scoped" = the line, somewhere after the call opens, has a trailing
  # `, client_id)` or `, cid_lo)` argument -- checked as two independent
  # patterns (call opens; a client_id/cid_lo arg closes it) rather than one
  # bridging regex, since earlier arguments legitimately contain their own
  # parens (e.g. `moves_db()`) that a single [^)]*-style regex can't span.
  has_call  <- function(lines, fn) grepl(fn, lines, fixed = TRUE)
  has_scope <- function(lines) grepl(',\\s*(client_id|cid_lo)\\s*\\)', lines)
  unscoped_get <- get_lines[has_call(get_lines, ".cache_get(") & !has_scope(get_lines)]
  unscoped_set <- set_lines[has_call(set_lines, ".cache_set(") & !has_scope(set_lines)]
  list(get = unscoped_get, set = unscoped_set)
}

for (f in c("R/persistence.R", "app.R")) {
  res <- .scan_unscoped_cache_calls(f)
  .chk(length(res$get), 0L, sprintf("%s: no unscoped .cache_get() call sites remain", f))
  .chk(length(res$set), 0L, sprintf("%s: no unscoped .cache_set() call sites remain", f))
}

# The real R/global.R definitions must actually perform client-scoping --
# guards against this test file's local copy silently drifting from
# production (see file header).
global_txt <- paste(readLines("R/global.R", warn = FALSE), collapse = "\n")
.chk_true(grepl("\\.cache_scoped_key", global_txt),
     "R/global.R defines .cache_scoped_key() (the fix is actually present in production, not just this test)")
.chk_true(grepl("\\.cache_get\\s*<-\\s*function\\(key, client_id = NULL\\)", global_txt),
     "R/global.R's .cache_get() signature accepts client_id")
.chk_true(grepl("\\.cache_set\\s*<-\\s*function\\(key, value, client_id = NULL\\)", global_txt),
     "R/global.R's .cache_set() signature accepts client_id")

cat(sprintf("\n  Stage 5 subtotal: %d passed, %d failed\n", .pass, .fail))
