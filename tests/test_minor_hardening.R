# =============================================================================
# tests/test_minor_hardening.R
# Regression tests for the "minor" tier of the same multi-pass code review:
# a lost-update race in auth_load_usuarios()/.migrate_legacy_passwords()
# (writing a stale full-row snapshot back to S3 can silently revert a
# concurrent password reset/tier change), plus a batch of Bancos
# double-click/idempotency gaps and two data-display bugs
# (.filter_periodo("mes_anterior")'s off-by-one, .tipo_badge()'s NA badge).
#
# bancos_module.R is NOT sourced into this harness (too large/UI-coupled --
# same reason test_fix_soon_hardening.R verified its Bancos fixes via static
# source checks rather than calling the real observers). This file follows
# the same convention for every bancos_module.R item below.
#
# Sourced by _run_saas.R.
# =============================================================================

cat("── Minor hardening (post-review) ───────────────────────────────────────\n")

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

.new_user_row <- function(overrides = list()) {
  row <- data.frame(
    id = "u1", account_code = NA_character_, username = "raceuser",
    password_hash = hash_password("OldPass1!"), display_name = "Race User",
    tier = "finance", client_id = "networks", permisos = "{}", group_ids = "[]",
    allowed_clients = "[]", email = NA_character_, requires_password_change = FALSE,
    activo = TRUE, created_at = as.character(Sys.time()), last_login = NA_character_,
    deleted = FALSE, deleted_at = NA_character_, stringsAsFactors = FALSE
  )
  for (k in names(overrides)) row[[k]] <- overrides[[k]]
  row
}

# ── A. auth_load_usuarios(): account_code backfill no longer clobbers a
# concurrently-written password_hash with the stale copy read at function
# entry (the lost-update race a deep cross-file trace found: this function's
# own internal read happens again, right before writing, via s3readRDS --
# simulate a concurrent write landing between those two reads). ────────────
{
  Sys.setenv(CLIENT_ID = "networks")
  key_full <- "networks/usuarios.rds"

  old_row <- .new_user_row()                                    # account_code NA -> needs backfill
  new_row <- .new_user_row(list(password_hash = hash_password("NewPass2!")))

  mock_s3saveRDS(old_row, key_full, "mock-bucket")

  call_n <- 0L
  orig_read <- aws.s3::s3readRDS
  fake_read <- function(object, bucket, ...) {
    if (!identical(object, key_full)) stop("NoSuchKey: ", object)
    call_n <<- call_n + 1L
    if (call_n == 1L) old_row else new_row
  }
  utils::assignInNamespace("s3readRDS", fake_read, "aws.s3")
  # .real_auth_load_usuarios(): _run_saas.R stubs the plain auth_load_usuarios
  # binding to a trivial mock_s3readRDS wrapper for the rest of the suite
  # (so other tests aren't surprised by real backfill/migration side
  # effects) -- this test specifically needs the REAL function, stashed by
  # the harness before that override.
  result <- tryCatch(
    .real_auth_load_usuarios(client_id = "networks"),
    finally = utils::assignInNamespace("s3readRDS", orig_read, "aws.s3")
  )

  written <- .mock_s3_store[[key_full]]
  .chk(call_n >= 2L, TRUE,
       "auth_load_usuarios(): the account_code backfill re-reads S3 before writing (not reusing the entry-point snapshot)")
  .chk(identical(written$password_hash, new_row$password_hash), TRUE,
       "auth_load_usuarios(): the backfill write preserves a concurrently-written password_hash instead of reverting it to the stale value read at function-entry")
  .chk(!is.na(written$account_code) && nzchar(written$account_code), TRUE,
       "auth_load_usuarios(): the account_code backfill still actually happens")
}

# ── B. .migrate_legacy_passwords(): re-reads fresh before writing, so
# migrating a legacy password doesn't revert an unrelated field (tier) that
# changed on S3 after the caller's `raw` was read but before this write. ───
{
  Sys.setenv(CLIENT_ID = "networks")
  key_full <- "networks/usuarios.rds"

  stale_raw <- .new_user_row(list(password_hash = "plaintext_legacy_pw", tier = "finance"))
  mock_s3saveRDS(stale_raw, key_full, "mock-bucket")

  # `raw` here stands in for a caller's already-in-hand, now-stale copy.
  raw <- stale_raw

  # Simulate a concurrent tier change landing on S3 in the meantime.
  mock_s3saveRDS(.new_user_row(list(password_hash = "plaintext_legacy_pw", tier = "admin")),
                 key_full, "mock-bucket")

  .migrate_legacy_passwords(raw, "networks")

  written <- .mock_s3_store[[key_full]]
  .chk(.is_scrypt_hash(written$password_hash), TRUE,
       ".migrate_legacy_passwords(): the legacy password is still migrated to a real hash")
  .chk(identical(written$tier, "admin"), TRUE,
       ".migrate_legacy_passwords(): a concurrently-changed tier is preserved, not reverted to the caller's stale copy")
}

# ── C. bancos_module.R: debounce/idempotency guards on the confirm-modal and
# per-row undo buttons flagged by the review (static source checks -- this
# file isn't sourced into the harness; see file header). ───────────────────
{
  txt <- readLines("R/bancos_module.R", warn = FALSE)
  block_after <- function(pattern, n = 8) {
    start <- grep(pattern, txt)[1]
    if (is.na(start)) return("")
    paste(txt[start:min(start + n, length(txt))], collapse = "\n")
  }

  # Real actionButton()-backed modal-confirm handlers -- shinyjs::disable()
  # actually works on these (unlike the per-row buttons checked below).
  for (btn in c("do_eliminar_confirm", "do_reasignar_confirm",
                "do_eliminar_sesion_confirm", "do_import")) {
    blk <- block_after(sprintf("observeEvent\\(input\\$%s,", btn))
    .chk(grepl(sprintf('shinyjs::disable\\("%s"\\)', btn), blk), TRUE,
         sprintf("R/bancos_module.R: %s debounces itself against a double-click", btn))
  }

  # do_eliminar_confirm must also clear elim_ids_rv after reading it, mirroring
  # the sibling confirm handlers -- otherwise a double-fire (even past the
  # shinyjs guard) would still re-process the same ids.
  blk <- block_after("observeEvent\\(input\\$do_eliminar_confirm,")
  .chk(grepl("elim_ids_rv\\(NULL\\)", blk), TRUE,
       "R/bancos_module.R: do_eliminar_confirm clears elim_ids_rv() after consuming it")

  # Per-row dynamic buttons (onclick + nonce, no persistent DOM id) --
  # protected via an idempotency check instead of shinyjs::disable().
  blk <- block_after("observeEvent\\(input\\$undo_mov_delete,", n = 15)
  .chk(grepl('if \\(!isTRUE\\(movs\\$eliminado\\[idx\\[1\\]\\]\\)\\) return\\(\\)', blk), TRUE,
       "R/bancos_module.R: undo_mov_delete guards against re-processing an already-restored row")

  blk <- block_after("observeEvent\\(input\\$undo_abono,")
  .chk(grepl('ab\\$status\\[ab\\$id == ab_id\\]\\[1\\], "voided"', blk), TRUE,
       "R/bancos_module.R: undo_abono guards against re-voiding an already-voided abono")

  # confirmacion_id duplicate-match guard: all 3 live call sites collapse to
  # idx_c[1] with a loud message instead of letting a length>1 vector reach
  # a `&&` downstream (a hard error in R >= 4.3).
  n_guards <- length(grep('idx_c <- idx_c\\[1\\]', txt))
  .chk(n_guards, 3L,
       "R/bancos_module.R: all 3 live confirmacion_id lookups (delete_conf, do_delete_conf_confirm, undo_conf) guard against a duplicate-id match")
}

# ── D. .filter_periodo("mes_anterior"): derives inicio from fin's own
# month, not a fixed "-32 days" offset that under-shoots after a 31-day
# previous month (verified analytically, not via Sys.Date() injection --
# .filter_periodo() isn't parameterized by "today"). ────────────────────────
{
  txt <- readLines("R/bancos_module.R", warn = FALSE)
  start <- grep("mes_anterior = \\{", txt)[1]
  blk   <- paste(txt[start:min(start + 6, length(txt))], collapse = "\n")
  .chk(grepl("inicio <- ms\\(fin\\)", blk), TRUE,
       "R/bancos_module.R: mes_anterior derives inicio from fin's own month (correct for any month length)")
  .chk(grepl("hoy - 32", blk), FALSE,
       "R/bancos_module.R: the old '-32 days' heuristic (wrong after a 31-day previous month) is gone")

  # Same property, checked directly with the same `ms()` truncation the real
  # code uses, for the exact edge case the review found (today = day 1,
  # previous month has 31 days): fin = day-0 truncation of today minus 1 =
  # last day of previous month; inicio must be THAT month's 1st, not two
  # months back.
  ms   <- function(d) as.Date(format(d, "%Y-%m-01"))
  hoy  <- as.Date("2026-08-01")
  fin  <- ms(hoy) - 1
  .chk(fin, as.Date("2026-07-31"), "sanity: fin is the last day of July for hoy=Aug 1")
  .chk(ms(fin), as.Date("2026-07-01"), "the fixed formula (inicio <- ms(fin)) gives July 1st")
  .chk(ms(hoy - 32), as.Date("2026-06-01"),
       "the old formula would have wrongly given June 1st (the bug this replaces)")
}

# ── E. .tipo_badge(): NA tipo falls back to the existing "otro" bucket
# instead of rendering a literal "NA" badge (static check). ─────────────────
{
  txt   <- readLines("R/bancos_module.R", warn = FALSE)
  start <- grep("^\\.tipo_badge <- function", txt)[1]
  blk   <- paste(txt[start:min(start + 6, length(txt))], collapse = "\n")
  .chk(grepl('if \\(is\\.na\\(tipo\\)\\) tipo <- "otro"', blk), TRUE,
       "R/bancos_module.R: .tipo_badge() maps NA to the existing 'otro' bucket before rendering")
}

cat("\n")
