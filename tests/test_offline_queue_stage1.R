# =============================================================================
# tests/test_offline_queue_stage1.R
#
# Stage 1 of the durable offline write queue (see
# OFFLINE_WRITE_QUEUE_DESIGN.md and R/offline_queue.R). This stage only
# covers: durably staging a payload locally when .s3_write()'s upload is
# fully exhausted, partitioned per client_id, gated to sync_bus-registered
# tables, with .s3_write()'s own caller-visible behavior left unchanged.
# No flush loop, no replay, no conflict handling yet -- those are later
# stages and are NOT tested here.
#
# Convention followed throughout (matches test_s3_read_retry_hardening.R,
# test_hopdesk_single_instance_lock.R): real function bodies are extracted
# via readLines()/parse()/eval() rather than reimplemented, and any mocked
# aws.s3 function is restored via tryCatch(..., finally = ...) even if an
# assertion inside the mocked block throws (regression guard for the
# documented mock-S3-leak incident).
# =============================================================================
cat("=== test_offline_queue_stage1 ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) {
    cat(" PASS:", label, "\n"); .pass <<- .pass + 1L
  } else {
    cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L
  }
}

.extract_fn <- function(file, fn_name) {
  exprs <- parse(file, keep.source = FALSE)
  for (e in exprs) {
    if (is.call(e) && length(e) >= 2 &&
        identical(e[[1]], as.name("<-")) &&
        identical(e[[2]], as.name(fn_name))) {
      assign(fn_name, eval(e[[3]], envir = globalenv()), envir = globalenv())
      return(invisible(TRUE))
    }
  }
  stop(sprintf("%s not found in %s", fn_name, file))
}

assign("%||%", function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x, envir = globalenv())

# ── Extract the real offline-queue functions ────────────────────────────────
for (fn in c(".queue_root_dir", ".queue_slug", ".queue_valid_client_id",
             ".queue_client_dir", ".queue_entries_dir", ".queue_manifest_path",
             ".queue_pending_index_path", ".queue_manifest_schema",
             ".queue_read_manifest", ".queue_write_manifest", ".queue_next_seq",
             # Added 2026-09-09 alongside the .queue_with_client_lock() fix
             # for the seq-number race (see tests/test_offline_queue_review_
             # findings.R Part R1): .queue_stage_entry() now calls these.
             ".QUEUE_LOCK_STALE_SECS", ".queue_lock_dir", ".queue_with_client_lock",
             ".queue_mark_client_pending", ".queue_table_name_for_key",
             ".queue_stage_entry"))
  .extract_fn("R/offline_queue.R", fn)

# ── Extract the real sync_bus registry pieces (offline_queue depends on them) ─
assign(".SYNC_REGISTRY", list(), envir = globalenv())
.extract_fn("R/sync_bus.R", "register_synced")

# ── Extract the real .s3_write() + its dependencies for the integration part ──
.extract_fn("R/persistence.R", ".client_id")
.extract_fn("R/persistence.R", ".s3_key")
.extract_fn("R/persistence.R", ".s3_bucket")
.extract_fn("R/persistence.R", ".s3_write")
assign(".s3_missing_cache", new.env(parent = emptyenv()), envir = globalenv())
assign(".s3_preload_cache", new.env(parent = emptyenv()), envir = globalenv())
assign(".sync_versions", list(), envir = globalenv())

# ── Point the queue at a throwaway directory for the whole run ──────────────
.TEST_QUEUE_ROOT <- file.path(tempdir(), paste0("hopdesk_queue_test_", as.integer(Sys.time())))
dir.create(.TEST_QUEUE_ROOT, recursive = TRUE, showWarnings = FALSE)
options(hopdesk.queue_dir = .TEST_QUEUE_ROOT)

.with_mock_s3write <- function(fake_save, fake_put, expr) {
  orig_save <- aws.s3::s3saveRDS
  orig_put  <- aws.s3::put_object
  utils::assignInNamespace("s3saveRDS", fake_save, "aws.s3")
  utils::assignInNamespace("put_object", fake_put, "aws.s3")
  tryCatch(expr, finally = {
    utils::assignInNamespace("s3saveRDS", orig_save, "aws.s3")
    utils::assignInNamespace("put_object", orig_put, "aws.s3")
  })
}
.orig_s3saveRDS  <- aws.s3::s3saveRDS
.orig_put_object <- aws.s3::put_object

# A couple of registered tables to stage against, mirroring app.R's real
# register_synced() calls in shape (name, bare S3_KEYS-style key, loader).
register_synced("moves_db",  "invoice_moves.rds",  function(client_id = NULL) NULL)
register_synced("manual_inv", "manual_invoices.rds", function(client_id = NULL) NULL)

# =============================================================================
# Part A -- .queue_valid_client_id(): valid/invalid client_id shapes.
# =============================================================================
cat("\n-- Part A: client_id validity (path-safety) --\n")

.valid_ids <- c("networks", "NETWORKS", "acme-corp", "client_007", "a.b.c",
                 "x", "client123", "hop-desk", "a_b_c", "UPPER-lower_1.2")
for (id in .valid_ids)
  ok(sprintf("A: '%s' is accepted as a valid client_id", id), isTRUE(.queue_valid_client_id(id)))

.invalid_ids <- list(
  "" , "   ", NA_character_, "a/b", "../evil", "..", ".", "a\\b",
  "client id with spaces", "semi;colon", "a,b", "a:b", "a*b", "a?b",
  "a<b>", "a|b", "\"quoted\"", "a\nb", "a\tb", c("a", "b"), 123, NULL, list("x")
)
for (id in .invalid_ids) {
  label <- if (is.null(id)) "NULL" else if (length(id) == 0) "character(0)" else paste0("'", paste(id, collapse=","), "'")
  ok(sprintf("A: %s is rejected as an invalid client_id", label), !isTRUE(.queue_valid_client_id(id)))
}

ok("A: '..' resolves to the parent dir via file.path() if it were allowed (regression guard for why '..' must be rejected explicitly)",
   identical(normalizePath(file.path(tempdir(), "clients", ".."), mustWork = FALSE),
             normalizePath(tempdir(), mustWork = FALSE)))

# =============================================================================
# Part B -- manifest schema, read/write round trip, atomic-write crash safety.
# =============================================================================
cat("\n-- Part B: manifest read/write --\n")

ok("B1: reading a manifest for a client with no prior activity returns the empty schema",
   nrow(.queue_read_manifest("brand_new_client_b1")) == 0L)
ok("B1: empty schema has the expected columns",
   identical(names(.queue_manifest_schema()),
             c("seq","s3_key","table_name","client_id","expected_sync_version",
               "acting_user","acting_home_client_id","status","staged_at","synced_at")))

{
  df <- .queue_manifest_schema()
  df <- rbind(df, data.frame(seq = 1L, s3_key = "b2client/invoice_moves.rds",
                              table_name = "moves_db", client_id = "b2client",
                              expected_sync_version = 3L, acting_user = NA_character_,
                              acting_home_client_id = NA_character_, status = "pending",
                              staged_at = Sys.time(), synced_at = as.POSIXct(NA),
                              stringsAsFactors = FALSE))
  .queue_write_manifest("b2client", df)
  back <- .queue_read_manifest("b2client")
  ok("B2: manifest round-trips through write/read with 1 row", nrow(back) == 1L)
  ok("B2: round-tripped row has the correct seq", identical(back$seq[1], 1L))
  ok("B2: round-tripped row has the correct table_name", identical(back$table_name[1], "moves_db"))
  ok("B2: round-tripped row has the correct expected_sync_version", identical(back$expected_sync_version[1], 3L))

  # Simulate a crash mid-write of a LATER manifest update: a stray .rds.tmp
  # file left behind (saveRDS succeeded, file.rename() never ran/crashed).
  stray <- tempfile(tmpdir = .queue_client_dir("b2client"), fileext = ".rds.tmp")
  writeLines("garbage, not a real manifest", stray)
  ok("B3: a stray leftover .tmp file does not corrupt reading the real manifest",
     nrow(.queue_read_manifest("b2client")) == 1L)

  # And a subsequent real write must still succeed despite the leftover file
  # (tempfile() picks a fresh random name, never collides with the stray one).
  df2 <- rbind(back, data.frame(seq = 2L, s3_key = "b2client/manual_invoices.rds",
                                 table_name = "manual_inv", client_id = "b2client",
                                 expected_sync_version = NA_integer_, acting_user = NA_character_,
                                 acting_home_client_id = NA_character_, status = "pending",
                                 staged_at = Sys.time(), synced_at = as.POSIXct(NA),
                                 stringsAsFactors = FALSE))
  .queue_write_manifest("b2client", df2)
  ok("B4: a write after a stray .tmp file still succeeds and is durable",
     nrow(.queue_read_manifest("b2client")) == 2L)
  unlink(stray)
}

# =============================================================================
# Part C -- .queue_stage_entry(): sequence monotonicity + per-entry payload
# correctness across 100 consecutive stagings for one client/table.
# =============================================================================
cat("\n-- Part C: sequence monotonicity + per-entry payload correctness (100 entries) --\n")

.make_payload <- function(marker) {
  tf <- tempfile(fileext = ".rds")
  saveRDS(list(marker = marker, n = nchar(marker)), tf, compress = TRUE)
  tf
}

.CID_C <- "seqclient"
.N_C <- 100L
for (i in seq_len(.N_C)) {
  pf <- .make_payload(sprintf("seq-entry-%03d", i))
  staged <- .queue_stage_entry(payload_path = pf, s3_key = paste0(.CID_C, "/invoice_moves.rds"),
                                key = "invoice_moves.rds", client_id = .CID_C)
  if (!isTRUE(staged)) stop(sprintf("setup failure: entry %d did not stage", i))
  unlink(pf)
}
.mf_c <- .queue_read_manifest(.CID_C)
ok("C1: exactly 100 rows were staged", nrow(.mf_c) == .N_C)
ok("C2: seq values are exactly 1..100, no gaps, no duplicates",
   identical(sort(.mf_c$seq), seq_len(.N_C)))
ok("C3: seq values are strictly increasing in manifest insertion order",
   all(diff(.mf_c$seq) == 1L))

for (i in seq_len(.N_C)) {
  row <- .mf_c[.mf_c$seq == i, ]
  dest <- file.path(.queue_entries_dir(.CID_C), sprintf("%012d.rds", i))
  payload_ok <- file.exists(dest) && identical(readRDS(dest)$marker, sprintf("seq-entry-%03d", i))
  ok(sprintf("C4[%03d]: staged entry file exists with the exact original payload content", i), payload_ok)
}

# =============================================================================
# Part D -- client partition isolation under interleaved multi-client,
# multi-table staging (20 clients x 5 "tables" each, stage order shuffled).
# =============================================================================
cat("\n-- Part D: client partition isolation (20 clients x 5 tables, interleaved) --\n")

register_synced("t1_db", "t1.rds", function(client_id = NULL) NULL)
register_synced("t2_db", "t2.rds", function(client_id = NULL) NULL)
register_synced("t3_db", "t3.rds", function(client_id = NULL) NULL)
register_synced("t4_db", "t4.rds", function(client_id = NULL) NULL)
register_synced("t5_db", "t5.rds", function(client_id = NULL) NULL)
.D_TABLES <- c("t1.rds", "t2.rds", "t3.rds", "t4.rds", "t5.rds")
.D_CLIENTS <- sprintf("dclient%02d", 1:20)

set.seed(42)
.d_jobs <- expand.grid(client = .D_CLIENTS, key = .D_TABLES, stringsAsFactors = FALSE)
.d_jobs <- .d_jobs[sample(nrow(.d_jobs)), ]

for (i in seq_len(nrow(.d_jobs))) {
  cid <- .d_jobs$client[i]; key <- .d_jobs$key[i]
  marker <- sprintf("%s::%s", cid, key)
  pf <- .make_payload(marker)
  staged <- .queue_stage_entry(payload_path = pf, s3_key = paste0(cid, "/", key),
                                key = key, client_id = cid)
  if (!isTRUE(staged)) stop(sprintf("setup failure: %s/%s did not stage", cid, key))
  unlink(pf)
}

for (cid in .D_CLIENTS) {
  mf <- .queue_read_manifest(cid)
  ok(sprintf("D1[%s]: manifest has exactly 5 rows (one per table), none belonging to another client",
             cid),
     nrow(mf) == 5L && all(mf$client_id == cid))

  entries_here <- list.files(.queue_entries_dir(cid))
  ok(sprintf("D2[%s]: entries directory has exactly 5 files", cid), length(entries_here) == 5L)

  for (key in .D_TABLES) {
    row <- mf[mf$s3_key == paste0(cid, "/", key), ]
    dest <- file.path(.queue_entries_dir(cid), sprintf("%012d.rds", row$seq[1]))
    expected_marker <- sprintf("%s::%s", cid, key)
    ok(sprintf("D3[%s/%s]: staged payload content matches this exact client+table, not a neighbor's",
               cid, key),
       nrow(row) == 1L && file.exists(dest) && identical(readRDS(dest)$marker, expected_marker))
  }
}

# Cross-contamination sweep: no client directory anywhere on disk contains a
# manifest row or payload marker that names a DIFFERENT client.
.all_leaked <- FALSE
for (cid in .D_CLIENTS) {
  mf <- .queue_read_manifest(cid)
  if (any(mf$client_id != cid)) .all_leaked <- TRUE
  for (f in list.files(.queue_entries_dir(cid), full.names = TRUE)) {
    m <- tryCatch(readRDS(f)$marker, error = function(e) "")
    if (!startsWith(m, paste0(cid, "::"))) .all_leaked <- TRUE
  }
}
ok("D4: sweep across all 20 client partitions finds zero cross-client leakage", !.all_leaked)

# =============================================================================
# Part E -- defense-in-depth: tenant-prefix mismatch is refused outright,
# and refusal never touches the local filesystem at all.
# =============================================================================
cat("\n-- Part E: tenant-prefix mismatch is refused, not silently written --\n")

{
  fresh_cid <- "mismatch_client_e1"
  ok("E1: fresh test client_id has no directory yet (sanity check before the refusal test)",
     !dir.exists(.queue_client_dir(fresh_cid)))

  pf <- .make_payload("should-never-be-written")
  staged <- .queue_stage_entry(payload_path = pf, s3_key = "someone_else/invoice_moves.rds",
                                key = "invoice_moves.rds", client_id = fresh_cid)
  ok("E2: staging with a client_id/s3_key prefix mismatch returns FALSE", identical(staged, FALSE))
  ok("E3: the mismatch refusal creates NO directory for that client_id at all",
     !dir.exists(.queue_client_dir(fresh_cid)))
  unlink(pf)
}

# =============================================================================
# Part F -- eligibility gate: unregistered tables are never staged (Phase 1
# only covers sync_bus-registered tables).
# =============================================================================
cat("\n-- Part F: unregistered tables are not eligible for Stage 1 queuing --\n")

{
  fresh_cid <- "ineligible_client_f1"
  pf <- .make_payload("irrelevant")
  staged <- .queue_stage_entry(payload_path = pf, s3_key = paste0(fresh_cid, "/not_registered.rds"),
                                key = "not_registered_anywhere.rds", client_id = fresh_cid)
  ok("F1: staging for a key with no register_synced() entry returns FALSE", identical(staged, FALSE))
  ok("F2: no directory is created for an ineligible-table staging attempt",
     !dir.exists(.queue_client_dir(fresh_cid)))
  unlink(pf)
}

# =============================================================================
# Part G -- acting_user / acting_home_client_id: default to NA when the
# caller doesn't supply them (today's .s3_write() doesn't yet), but are
# stored correctly when a future caller does supply them.
# =============================================================================
cat("\n-- Part G: acting_user / acting_home_client_id metadata --\n")

{
  cid <- "gclient_default"
  pf <- .make_payload("g1")
  .queue_stage_entry(payload_path = pf, s3_key = paste0(cid, "/invoice_moves.rds"),
                      key = "invoice_moves.rds", client_id = cid)
  row <- .queue_read_manifest(cid)
  ok("G1: acting_user defaults to NA when not supplied", is.na(row$acting_user[1]))
  ok("G2: acting_home_client_id defaults to NA when not supplied", is.na(row$acting_home_client_id[1]))
  unlink(pf)

  cid2 <- "gclient_staff_jump"
  pf2 <- .make_payload("g2")
  .queue_stage_entry(payload_path = pf2, s3_key = paste0(cid2, "/invoice_moves.rds"),
                      key = "invoice_moves.rds", client_id = cid2,
                      acting_user = "mouse", acting_home_client_id = "hopdesk")
  row2 <- .queue_read_manifest(cid2)
  ok("G3: acting_user is stored correctly when supplied (staff-jump scenario)",
     identical(row2$acting_user[1], "mouse"))
  ok("G4: acting_home_client_id is stored correctly when supplied (staff's own tenant, not the target client)",
     identical(row2$acting_home_client_id[1], "hopdesk"))
  ok("G5: the target client_id is still the client being helped, not the staff's home tenant",
     identical(row2$client_id[1], cid2))
  unlink(pf2)
}

# =============================================================================
# Part H -- pending_client_ids top-level index.
# =============================================================================
cat("\n-- Part H: pending_client_ids index --\n")

{
  idx_before <- if (file.exists(.queue_pending_index_path())) readRDS(.queue_pending_index_path()) else character(0)
  for (n in 1:3) {
    pf <- .make_payload(paste0("h", n))
    .queue_stage_entry(payload_path = pf, s3_key = "hclient_a/invoice_moves.rds",
                        key = "invoice_moves.rds", client_id = "hclient_a")
    unlink(pf)
  }
  pf <- .make_payload("h_b")
  .queue_stage_entry(payload_path = pf, s3_key = "hclient_b/invoice_moves.rds",
                      key = "invoice_moves.rds", client_id = "hclient_b")
  unlink(pf)
  idx_after <- readRDS(.queue_pending_index_path())
  ok("H1: pending index contains hclient_a exactly once despite 3 staging calls",
     sum(idx_after == "hclient_a") == 1L)
  ok("H2: pending index contains hclient_b", "hclient_b" %in% idx_after)
  ok("H3: pending index has no duplicate entries at all", !any(duplicated(idx_after)))
}

# =============================================================================
# Part I -- .s3_write() integration: caller-visible behavior is unchanged on
# both success and failure; queuing only happens on genuine, fully-exhausted
# failure, for eligible tables only.
# =============================================================================
cat("\n-- Part I: .s3_write() integration (success path, failure path, ineligible path) --\n")

Sys.setenv(CLIENT_ID = "iclient", S3_BUCKET = "mock-bucket")

# I1: success path -- no queue entry created, tempfile cleaned up, no error.
{
  fake_save <- function(...) invisible(TRUE)
  fake_put  <- function(...) stop("should never be called on the success path")
  ok("I1: .s3_write() succeeds without error when s3saveRDS succeeds",
     isTRUE(.with_mock_s3write(fake_save, fake_put,
       .s3_write(list(hello = "world"), "invoice_moves.rds", client_id = "iclient"))))
  ok("I1: a successful write creates NO queue entry",
     nrow(.queue_read_manifest("iclient")) == 0L)
}

# I2: failure path (both s3saveRDS and put_object fail) -- .s3_write() still
# raises the same error as before offline queuing existed, AND a queue entry
# is durably staged with the correct client/table/payload.
{
  fake_save <- function(...) stop("simulated S3 outage (s3saveRDS)")
  fake_put  <- function(...) stop("simulated S3 outage (put_object fallback)")
  err <- NULL
  res <- tryCatch(
    .with_mock_s3write(fake_save, fake_put,
      .s3_write(list(payload = "durable-data", n = 42), "invoice_moves.rds", client_id = "iclient")),
    error = function(e) { err <<- conditionMessage(e); NULL }
  )
  ok("I2: .s3_write() still raises an error when both upload attempts fail (unchanged caller-visible behavior)",
     !is.null(err) && grepl("put_object fallback", err, fixed = TRUE))

  mf <- .queue_read_manifest("iclient")
  ok("I2: exactly one queue entry now exists for this client (the earlier success added none)",
     nrow(mf) == 1L)
  ok("I2: the queued entry is for the right table_name", identical(mf$table_name[1], "moves_db"))
  ok("I2: the queued entry is for the right s3_key", identical(mf$s3_key[1], "iclient/invoice_moves.rds"))
  ok("I2: the queued entry's status is 'pending'", identical(mf$status[1], "pending"))
  dest <- file.path(.queue_entries_dir("iclient"), sprintf("%012d.rds", mf$seq[1]))
  staged_obj <- readRDS(dest)
  ok("I2: the queued payload's actual content matches the object .s3_write() was given",
     identical(staged_obj$payload, "durable-data") && identical(staged_obj$n, 42))
}

# I3: failure path for a table that was never register_synced()'d -- error
# still raised as before, but NOTHING is queued (Phase 1 scope boundary).
{
  fake_save <- function(...) stop("simulated outage")
  fake_put  <- function(...) stop("simulated outage (fallback)")
  err <- NULL
  tryCatch(
    .with_mock_s3write(fake_save, fake_put,
      .s3_write(list(x = 1), "totally_unregistered_key.rds", client_id = "iclient")),
    error = function(e) { err <<- conditionMessage(e) }
  )
  ok("I3: .s3_write() still raises an error for an ineligible/unregistered key", !is.null(err))
  ok("I3: no NEW queue entry was added for the ineligible key (still exactly 1 from I2)",
     nrow(.queue_read_manifest("iclient")) == 1L)
}

# I4: expected_sync_version is captured from whatever .sync_versions holds
# at staging time.
{
  .GlobalEnv$.sync_versions[["moves_db"]] <- 7L
  fake_save <- function(...) stop("simulated outage")
  fake_put  <- function(...) stop("simulated outage (fallback)")
  tryCatch(
    .with_mock_s3write(fake_save, fake_put,
      .s3_write(list(v = 1), "invoice_moves.rds", client_id = "iclient_v7")),
    error = function(e) NULL
  )
  mf <- .queue_read_manifest("iclient_v7")
  ok("I4: expected_sync_version on the staged entry reflects the in-memory sync_versions value at stage time",
     identical(mf$expected_sync_version[1], 7L))
}

ok("I5: aws.s3::s3saveRDS is restored to the real function after every mocked block above (no leaked mock)",
   identical(aws.s3::s3saveRDS, .orig_s3saveRDS))
ok("I6: aws.s3::put_object is restored to the real function after every mocked block above (no leaked mock)",
   identical(aws.s3::put_object, .orig_put_object))

# =============================================================================
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
unlink(.TEST_QUEUE_ROOT, recursive = TRUE, force = TRUE)
if (.fail > 0) stop("Tests FAILED.")
invisible(NULL)
