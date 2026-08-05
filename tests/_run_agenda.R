# =============================================================================
# tests/_run_agenda.R
# Runner for Agenda de Hoy persistence tests.
# Run from project root:  source("tests/_run_agenda.R")
# No live S3 credentials needed — all aws.s3 I/O is patched in-memory.
#
# IMPORTANT: this patches the REAL aws.s3 package's namespace
# (utils::assignInNamespace), which is process-global and OUTLIVES this
# script's own execution unless explicitly restored — see the matching
# comment in tests/_run_saas.R for the full incident this was found from
# (2026-08-04). Restored unconditionally via tryCatch/finally below. Safe
# habit regardless: only ever run this file as a fresh `Rscript` subprocess,
# never source()'d into a terminal you plan to keep using afterward.
# =============================================================================

options(warn = 1)
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(uuid)
  library(aws.s3)    # must load before patching its namespace
})

# Capture the REAL functions before patching, so they can be restored no
# matter how this script exits.
.orig_s3readRDS  <- aws.s3::s3readRDS
.orig_s3saveRDS  <- aws.s3::s3saveRDS
.orig_put_object <- aws.s3::put_object
.orig_get_bucket <- aws.s3::get_bucket

.restore_aws_s3_namespace <- function() {
  suppressWarnings({
    utils::assignInNamespace("s3readRDS",  .orig_s3readRDS,  "aws.s3")
    utils::assignInNamespace("s3saveRDS",  .orig_s3saveRDS,  "aws.s3")
    utils::assignInNamespace("put_object", .orig_put_object, "aws.s3")
    utils::assignInNamespace("get_bucket", .orig_get_bucket, "aws.s3")
  })
}

.total_pass <- 0L
.total_fail <- 0L

tryCatch({

  # ── In-memory S3 mock ───────────────────────────────────────────────────────
  # Keyed by full S3 path  e.g. "networks/pagar_hoy_sync.rds"
  .mock_s3_store <- new.env(parent = emptyenv())

  mock_s3readRDS <- function(object, bucket, ...) {
    obj <- tryCatch(get(object, envir = .mock_s3_store, inherits = FALSE),
                    error = function(e) NULL)
    if (is.null(obj)) stop("NoSuchKey: ", object)
    obj
  }
  mock_s3saveRDS <- function(x, object, bucket, ...) {
    assign(object, x, envir = .mock_s3_store)
    invisible(x)
  }
  mock_put_object <- function(file, object, bucket, ...) {
    assign(object, readRDS(file), envir = .mock_s3_store)
    invisible(TRUE)
  }
  mock_s3getbucket <- function(bucket, prefix = "", max = 200L, ...) {
    all_keys <- ls(.mock_s3_store)
    matched  <- all_keys[startsWith(all_keys, prefix)]
    lapply(matched, function(k) list(Key = k))
  }

  suppressWarnings({
    utils::assignInNamespace("s3readRDS",  mock_s3readRDS,   "aws.s3")
    utils::assignInNamespace("s3saveRDS",  mock_s3saveRDS,   "aws.s3")
    utils::assignInNamespace("put_object", mock_put_object,  "aws.s3")
    utils::assignInNamespace("get_bucket", mock_s3getbucket, "aws.s3")
  })

  # ── Env vars ─────────────────────────────────────────────────────────────────
  Sys.setenv(
    CLIENT_ID              = "networks",
    S3_BUCKET              = "mock-bucket",
    AWS_ACCESS_KEY_ID      = "mock",
    AWS_SECRET_ACCESS_KEY  = "mock",
    AWS_DEFAULT_REGION     = "us-east-1"
  )

  # ── S3_KEYS (superset of all keys touched by these tests) ─────────────────
  S3_KEYS <- list(
    pagar_hoy          = "pagar_hoy.rds",
    pagar_hoy_sync     = "pagar_hoy_sync.rds",
    agenda_sync_config = "agenda_sync_config.rds",
    sync_versions      = "sync_versions.rds",
    usuarios           = "usuarios.rds"
  )

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # Source order matters: sync_bus first (defines bump_sync_version), then persistence
  source("R/sync_bus.R",   local = FALSE)
  source("R/persistence.R", local = FALSE)

  # ── Run test module ─────────────────────────────────────────────────────────
  .run_module <- function(file) {
    e <- new.env(parent = globalenv())
    e$.pass <- 0L
    e$.fail <- 0L
    tryCatch(source(file, local = e), error = function(err) {
      cat(sprintf("  ERROR loading %s: %s\n", basename(file), err$message))
      e$.fail <- e$.fail + 1L
    })
    .total_pass <<- .total_pass + e$.pass
    .total_fail <<- .total_fail + e$.fail
  }

  cat("\n====================================================\n")
  cat("  Agenda de Hoy Persistence Test Suite\n")
  cat("====================================================\n\n")

  .run_module("tests/test_agenda_persistence.R")

  cat("\n====================================================\n")
  cat(sprintf("  TOTAL: %d passed, %d failed\n", .total_pass, .total_fail))
  cat("====================================================\n\n")

  if (.total_fail > 0L) stop(sprintf("%d test(s) failed", .total_fail))

}, finally = {
  .restore_aws_s3_namespace()
})

invisible(.total_pass)
