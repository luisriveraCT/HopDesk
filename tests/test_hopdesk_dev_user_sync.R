# =============================================================================
# tests/test_hopdesk_dev_user_sync.R
#
# .hopdesk_sync_dev_users_from_prod() (R/global.R, added 2026-09-04): when
# the dev-bucket override is active, copies real user-account/auth data
# (usuarios.rds per client, client_registry.rds, username_index.rds) AND
# each active client's own empresas.rds from the production bucket into
# the dev bucket, so the dev sandbox always has working login credentials
# and a real company list instead of starting empty (which cost real
# debugging time three times in one day: "Usuario o contraseña incorrecto"
# against an unseeded dev bucket twice, then the Calendario's company-
# toggle "bubbles" silently disappearing because company_map_rv() had
# nothing to read).
#
# STAKES: this reads from production and writes to the dev bucket. A
# reversed direction, or a write that ever reaches the PRODUCTION bucket,
# would be a real, serious bug -- the opposite of what the dev/staging
# override exists to prevent. Every check below that matters most is about
# that direction, not just "does it run".
#
# aws.s3::s3readRDS/s3saveRDS are mocked via assignInNamespace with a
# GUARANTEED restore (tryCatch/finally, never a bare on.exit inside a
# sourced-not-called script -- see tests/_run_saas.R's own comments on why,
# and the real incident that taught this: mock_s3_leak_incident, fixed
# 4d4431d, where an unrestored mock leaked into a live session and appeared
# to delete real IC/Pasivos data). No real network calls, no real S3.
#
# Sourced by _run_saas.R (already has aws.s3 loaded).
# =============================================================================

cat("── Dev-bucket user-account sync (2026-09-04) ────────────────────────────\n")

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

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
.chk_false <- function(cond, label) .chk(isTRUE(cond), FALSE, label)

global_src <- readLines("R/global.R", warn = FALSE)
fn_start <- grep("^\\.hopdesk_sync_dev_users_from_prod <- function", global_src)
fn_end   <- grep("^if \\(Sys\\.getenv\\(\"HOPDESK_PRODUCTION\"\\) == \"1\"\\) \\{", global_src)
.chk_true(length(fn_start) == 1 && length(fn_end) == 1 && fn_start < fn_end,
          "located exactly one .hopdesk_sync_dev_users_from_prod() block in R/global.R")
fn_txt <- paste(global_src[fn_start:(fn_end - 1)], collapse = "\n")
eval(parse(text = fn_txt), envir = environment())
.chk_true(exists(".hopdesk_sync_dev_users_from_prod", mode = "function"),
          ".hopdesk_sync_dev_users_from_prod() extracted and evaluated successfully from the real source file")

# ── Fake in-memory "S3", keyed by "bucket/key" ──────────────────────────────
.fake_store <- new.env(parent = emptyenv())
.read_log   <- list()   # list of list(bucket=, key=)
.write_log  <- list()   # list of list(bucket=, key=)
.fail_write_keys <- character(0)  # keys that should throw on write, for the failure test

mock_read <- function(object, bucket, ...) {
  extra <- list(...)
  .read_log[[length(.read_log) + 1L]] <<- list(bucket = bucket, key = object,
                                               key_arg = extra$key, secret_arg = extra$secret,
                                               region_arg = extra$region)
  slot <- paste0(bucket, "/", object)
  if (!exists(slot, envir = .fake_store, inherits = FALSE))
    stop("NoSuchKey: ", slot)  # mirrors the real aws.s3 behavior tested against elsewhere in this suite
  get(slot, envir = .fake_store, inherits = FALSE)
}
mock_write <- function(x, object, bucket, ...) {
  .write_log[[length(.write_log) + 1L]] <<- list(bucket = bucket, key = object)
  slot <- paste0(bucket, "/", object)
  if (object %in% .fail_write_keys) stop("simulated write failure for ", slot)
  assign(slot, x, envir = .fake_store)
  invisible(TRUE)
}

.orig_s3readRDS <- aws.s3::s3readRDS
.orig_s3saveRDS <- aws.s3::s3saveRDS

tryCatch({
  utils::assignInNamespace("s3readRDS", mock_read,  "aws.s3")
  utils::assignInNamespace("s3saveRDS", mock_write, "aws.s3")

  # =============================================================================
  # Part A -- safety guards: no-op cases must make ZERO S3 calls at all
  # =============================================================================
  cat("\n-- Part A: no-op safety guards --\n")

  .read_log <- list(); .write_log <- list()
  r1 <- .hopdesk_sync_dev_users_from_prod("same-bucket", "same-bucket")
  .chk_false(r1, "identical prod/dev bucket returns FALSE")
  .chk(length(.read_log),  0L, "identical prod/dev bucket: zero S3 reads attempted")
  .chk(length(.write_log), 0L, "identical prod/dev bucket: zero S3 writes attempted (the critical safety property -- never even risk touching prod as a 'write target' here)")

  .read_log <- list(); .write_log <- list()
  r2 <- .hopdesk_sync_dev_users_from_prod("", "dev-bucket")
  .chk_false(r2, "empty prod_bucket returns FALSE")
  .chk(length(.read_log) + length(.write_log), 0L, "empty prod_bucket: zero S3 calls attempted")

  .read_log <- list(); .write_log <- list()
  r3 <- .hopdesk_sync_dev_users_from_prod("prod-bucket", NULL)
  .chk_false(r3, "NULL dev_bucket returns FALSE")
  .chk(length(.read_log) + length(.write_log), 0L, "NULL dev_bucket: zero S3 calls attempted")

  # =============================================================================
  # Part B -- normal case: real data flows prod -> dev, correctly filtered
  # =============================================================================
  cat("\n-- Part B: normal sync, correct data, correct direction --\n")

  rm(list = ls(envir = .fake_store), envir = .fake_store)
  usuarios_hd <- data.frame(username = "mouse", tier = "principal", stringsAsFactors = FALSE)
  idx         <- data.frame(username = "mouse", client_id = "hd-admin", stringsAsFactors = FALSE)
  registry    <- data.frame(
    client_id = c("networks", "hopdesk", "inactive-client"),
    status    = c("active",   "active",  "suspended"),
    stringsAsFactors = FALSE
  )
  usuarios_networks <- data.frame(username = "dev", tier = "dev", client_id = "networks", stringsAsFactors = FALSE)
  usuarios_hopdesk  <- data.frame(username = "bunny", tier = "hopdesk", client_id = "hopdesk", stringsAsFactors = FALSE)
  empresas_networks <- data.frame(initials = "NCS", nombre_corto = "Networks CS", activa = TRUE, stringsAsFactors = FALSE)

  assign("prod-bucket/hd-admin/usuarios.rds",        usuarios_hd,       envir = .fake_store)
  assign("prod-bucket/hd-admin/username_index.rds",  idx,               envir = .fake_store)
  assign("prod-bucket/hd-admin/client_registry.rds", registry,          envir = .fake_store)
  assign("prod-bucket/networks/usuarios.rds",        usuarios_networks, envir = .fake_store)
  assign("prod-bucket/networks/empresas.rds",        empresas_networks, envir = .fake_store)
  assign("prod-bucket/hopdesk/usuarios.rds",         usuarios_hopdesk,  envir = .fake_store)
  # Deliberately NO "prod-bucket/inactive-client/usuarios.rds" at all --
  # proves the active-only filter, not just a lucky absence. Also
  # deliberately NO "prod-bucket/hopdesk/empresas.rds" -- proves a missing
  # empresas.rds for one client doesn't block anything else.

  .read_log <- list(); .write_log <- list()
  result <- .hopdesk_sync_dev_users_from_prod("prod-bucket", "dev-bucket")
  .chk_true(result, "normal sync returns TRUE")

  read_buckets  <- vapply(.read_log,  function(x) x$bucket, character(1))
  write_buckets <- vapply(.write_log, function(x) x$bucket, character(1))
  .chk_true(all(read_buckets == "prod-bucket"),
            "EVERY read targets the production bucket, never the dev bucket (wrong direction would silently sync nothing, or worse)")
  .chk_true(all(write_buckets == "dev-bucket"),
            "EVERY write targets the dev bucket -- NEVER the production bucket (the single most important property this function has)")

  write_keys <- vapply(.write_log, function(x) x$key, character(1))
  .chk_true("hd-admin/usuarios.rds" %in% write_keys, "hd-admin/usuarios.rds was copied")
  .chk_true("hd-admin/username_index.rds" %in% write_keys, "hd-admin/username_index.rds was copied")
  .chk_true("hd-admin/client_registry.rds" %in% write_keys, "hd-admin/client_registry.rds was copied")
  .chk_true("networks/usuarios.rds" %in% write_keys, "the active 'networks' client's usuarios.rds was copied")
  .chk_true("hopdesk/usuarios.rds" %in% write_keys, "the active 'hopdesk' client's usuarios.rds was copied")
  .chk_true("networks/empresas.rds" %in% write_keys,
            "the active 'networks' client's empresas.rds was copied (this is what company_map_rv() needs to return anything at all)")
  .chk_false("hopdesk/empresas.rds" %in% write_keys,
             "hopdesk's empresas.rds, which doesn't exist in production, was correctly skipped rather than erroring the whole sync")
  .chk_false("inactive-client/usuarios.rds" %in% write_keys,
             "the SUSPENDED client's usuarios.rds was NOT copied -- proves the active-only filter is real, not incidental")
  .chk(sum(write_keys == "hd-admin/usuarios.rds"), 1L,
       "hd-admin is not ALSO swept up by the per-active-client loop (would double-write it)")

  # Content actually landed correctly, not just a key with placeholder data
  .chk(get("dev-bucket/networks/usuarios.rds", envir = .fake_store)$username, "dev",
       "the copied networks/usuarios.rds has the REAL content from production, not a stand-in")
  .chk(get("dev-bucket/networks/empresas.rds", envir = .fake_store)$initials, "NCS",
       "the copied networks/empresas.rds has the REAL content from production, not a stand-in")

  # =============================================================================
  # Part C -- graceful degradation: a missing key in prod, and a write
  # failure, are both non-fatal and don't stop the rest of the sync
  # =============================================================================
  cat("\n-- Part C: graceful degradation --\n")

  rm(list = ls(envir = .fake_store), envir = .fake_store)
  assign("prod-bucket/hd-admin/usuarios.rds", usuarios_hd, envir = .fake_store)
  # username_index.rds and client_registry.rds deliberately absent from prod.

  .read_log <- list(); .write_log <- list()
  result_missing <- tryCatch(.hopdesk_sync_dev_users_from_prod("prod-bucket", "dev-bucket"),
                             error = function(e) paste("THREW:", conditionMessage(e)))
  .chk_true(!identical(result_missing, FALSE) && !grepl("^THREW:", result_missing),
            "a missing key in production (username_index.rds, client_registry.rds) does not crash the whole sync")
  .chk_true("hd-admin/usuarios.rds" %in% vapply(.write_log, function(x) x$key, character(1)),
            "the key that DID exist (usuarios.rds) still got copied despite the others being missing")

  rm(list = ls(envir = .fake_store), envir = .fake_store)
  assign("prod-bucket/hd-admin/usuarios.rds",       usuarios_hd, envir = .fake_store)
  assign("prod-bucket/hd-admin/username_index.rds", idx,         envir = .fake_store)
  .fail_write_keys <- "hd-admin/usuarios.rds"  # simulate THIS ONE write failing

  .write_log <- list()
  result_partial_fail <- tryCatch(.hopdesk_sync_dev_users_from_prod("prod-bucket", "dev-bucket"),
                                  error = function(e) paste("THREW:", conditionMessage(e)))
  .chk_true(!grepl("^THREW:", result_partial_fail),
            "one key's write failure does not crash the sync or stop the remaining keys")
  .chk_true("hd-admin/username_index.rds" %in% vapply(.write_log, function(x) x$key, character(1)),
            "the key AFTER the failing one still got attempted and copied (the loop continues past one failure)")
  .fail_write_keys <- character(0)

  # =============================================================================
  # Part D -- REAL BUG FOUND 2026-09-04 via an actual end-to-end run against
  # live S3 (not caught by any mocked test until this one was added): by the
  # time this function runs for real, .Renviron.local has ALREADY overwritten
  # the ambient AWS credentials to the dev bucket's own IAM user, which has
  # NO read access to production at all. Every prod read must explicitly
  # pass production's OWN credentials -- this verifies they actually reach
  # the read call, not just that the function still runs without them.
  # =============================================================================
  cat("\n-- Part D: production credentials are explicitly threaded into every read --\n")

  rm(list = ls(envir = .fake_store), envir = .fake_store)
  assign("prod-bucket/hd-admin/usuarios.rds",       usuarios_hd, envir = .fake_store)
  assign("prod-bucket/hd-admin/username_index.rds", idx,         envir = .fake_store)
  assign("prod-bucket/hd-admin/client_registry.rds", registry,   envir = .fake_store)

  .read_log <- list()
  .hopdesk_sync_dev_users_from_prod("prod-bucket", "dev-bucket",
                                    prod_aws_key = "PRODKEY123", prod_aws_secret = "PRODSECRET456",
                                    prod_aws_region = "us-east-1")
  key_args    <- vapply(.read_log, function(x) x$key_arg %||% "MISSING", character(1))
  secret_args <- vapply(.read_log, function(x) x$secret_arg %||% "MISSING", character(1))
  region_args <- vapply(.read_log, function(x) x$region_arg %||% "MISSING", character(1))
  .chk_true(all(key_args == "PRODKEY123"),
            "EVERY read call receives the explicit production AWS key -- this is the actual fix; without it, every real prod read gets silently AccessDenied and the whole sync is a no-op")
  .chk_true(all(secret_args == "PRODSECRET456"), "every read call receives the explicit production AWS secret")
  .chk_true(all(region_args == "us-east-1"), "every read call receives the explicit production AWS region")

  # Control: when prod_aws_* are left NULL (as every other Part in this file
  # does), the read call must NOT be passed a "key"/"secret"/"region" arg at
  # all -- confirms this is a pure additive override, not something that
  # could accidentally pass literal NULLs through to aws.s3's own args and
  # break its normal ambient-credential fallback.
  .read_log <- list()
  .hopdesk_sync_dev_users_from_prod("prod-bucket", "dev-bucket")
  .chk_true(all(vapply(.read_log, function(x) is.null(x$key_arg), logical(1))),
            "control: with prod_aws_key left NULL, no key= argument is passed through at all (not even a literal NULL)")

}, finally = {
  utils::assignInNamespace("s3readRDS", .orig_s3readRDS, "aws.s3")
  utils::assignInNamespace("s3saveRDS", .orig_s3saveRDS, "aws.s3")
})

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
