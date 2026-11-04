# =============================================================================
# tests/test_ultimo_dev_vivo.R
# Every client keeps at least one live account of its IT tier (dev) -- any
# of them, not "the first" -- and the count is per client (Luis, 2026-11-03,
# when principal got to invite with the dev tier).
#   - "live" = not deleted AND not deactivated: an inactive dev cannot log in.
#   - auth_save_usuarios() refuses any write that leaves a client with no
#     live account of a unico_vivo tier it had: demote, deactivate, delete,
#     drop the row, or the user-limit change. Every path ends there.
#   - the four UI paths (delete pre-check, delete confirm, edit, limit
#     change) warn before saving, with the same helper.
# Before this, deactivating the last dev from the editor's "Activo" box and
# the limit change were not checked, and inactive devs counted as live.
#
# S3 is an in-memory store: aws.s3's s3readRDS / s3saveRDS / object_exists
# are swapped for this test only and ALWAYS restored (on.exit).
#
# Run from project root:  Rscript tests/test_ultimo_dev_vivo.R
# =============================================================================
cat("=== test_ultimo_dev_vivo ===\n\n")
`%||%` <- function(a, b) if (is.null(a)) b else a

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# ── In-memory S3, swapped in and always restored ──────────────────────────────
store <- new.env()
fake_read   <- function(object, bucket, ...) {
  if (!exists(object, envir = store, inherits = FALSE)) stop("NoSuchKey")
  get(object, envir = store)
}
fake_save   <- function(x, object, bucket, ...) { assign(object, x, envir = store); invisible(TRUE) }
fake_exists <- function(object, bucket, ...) exists(object, envir = store, inherits = FALSE)
ns <- asNamespace("aws.s3")
orig <- list(s3readRDS = get("s3readRDS", ns), s3saveRDS = get("s3saveRDS", ns),
             object_exists = get("object_exists", ns))
restore <- function() for (nm in names(orig)) utils::assignInNamespace(nm, orig[[nm]], "aws.s3")
on.exit(restore(), add = TRUE)
utils::assignInNamespace("s3readRDS", fake_read, "aws.s3")
utils::assignInNamespace("s3saveRDS", fake_save, "aws.s3")
utils::assignInNamespace("object_exists", fake_exists, "aws.s3")

# ── The real functions (no app) ───────────────────────────────────────────────
source("R/tier_registry.R")
env <- new.env()
for (e in parse("R/auth.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c("auth_save_usuarios", ".usuarios_leer_estricto", ".usuarios_existe",
                                  "usuarios_vivos_nivel", "usuarios_niveles_perdidos"))
    eval(e, env)
env$.s3_bucket <- function() "fake-bucket"
env$auth_invalidate_credentials <- function() invisible(NULL)

DEV <- nivel_ti()
OTRO <- niveles_invitables()[1]
ok("the IT tier is a unico_vivo tier", nivel_unico_vivo(DEV))

tabla <- function(cid, users, tiers, activo = TRUE, deleted = FALSE)
  data.frame(username = users, tier = tiers, client_id = cid, activo = activo, deleted = deleted,
             stringsAsFactors = FALSE)
put  <- function(cid, df) assign(paste0(cid, "/usuarios.rds"), df, envir = store)
get_ <- function(cid) get(paste0(cid, "/usuarios.rds"), envir = store)
guardar <- function(df, cid) tryCatch({ env$auth_save_usuarios(df, client_id = cid); TRUE },
                                     error = function(e) FALSE)
reset <- function() {
  rm(list = ls(store), envir = store)
  # acme: two devs and two others; beta: one dev; nuevo: no dev yet.
  put("acme", tabla("acme", c("d1", "d2", "a1", "a2"), c(DEV, DEV, OTRO, OTRO)))
  put("beta", tabla("beta", c("bd", "b1", "b2"), c(DEV, OTRO, OTRO)))
  put("nuevo", tabla("nuevo", c("n1", "n2"), c(OTRO, OTRO)))
}
cambia <- function(df, user, col, val) { df[df$username == user, col] <- val; df }

# ── 1. What counts as live ────────────────────────────────────────────────────
x <- tabla("acme", c("d1", "d2", "d3", "d4"), DEV, activo = c(TRUE, FALSE, TRUE, NA),
           deleted = c(FALSE, FALSE, TRUE, NA))
ok("live = not deleted and not deactivated (NA counts as live)", env$usuarios_vivos_nivel(x, DEV) == 2L)
ok("no table, no live accounts", env$usuarios_vivos_nivel(NULL, DEV) == 0L)

# ── 2. Each path, on the LAST dev ─────────────────────────────────────────────
for (quien in c("d1", "d2")) {        # any dev, not the first one
  otro_dev <- setdiff(c("d1", "d2"), quien)
  reset()
  ok(sprintf("acme: with two devs, demoting %s is allowed", otro_dev),
     guardar(cambia(get_("acme"), otro_dev, "tier", OTRO), "acme"))
  base <- get_("acme")
  ok(sprintf("acme: demoting the last dev (%s) is refused", quien),
     !guardar(cambia(base, quien, "tier", OTRO), "acme"))
  ok(sprintf("acme: deactivating the last dev (%s) is refused", quien),
     !guardar(cambia(base, quien, "activo", FALSE), "acme"))
  ok(sprintf("acme: deleting the last dev (%s) is refused", quien),
     !guardar(cambia(cambia(base, quien, "deleted", TRUE), quien, "activo", FALSE), "acme"))
  ok(sprintf("acme: dropping the last dev's row (%s) is refused", quien),
     !guardar(base[base$username != quien, ], "acme"))
  ok("... and S3 still has that dev live", env$usuarios_vivos_nivel(get_("acme"), DEV) == 1L)
}

reset()
ok("acme: deactivating one of two devs is allowed",
   guardar(cambia(get_("acme"), "d1", "activo", FALSE), "acme"))
ok("acme: then deleting the other (the only LIVE one) is refused",
   !guardar(cambia(get_("acme"), "d2", "deleted", TRUE), "acme"))
ok("acme: reactivating d1 lets d2 go",
   guardar(cambia(cambia(get_("acme"), "d1", "activo", TRUE), "d2", "deleted", TRUE), "acme"))

# ── 3. Per client ─────────────────────────────────────────────────────────────
reset()
ok("beta: its only dev cannot be demoted, though acme has two",
   !guardar(cambia(get_("beta"), "bd", "tier", OTRO), "beta"))
ok("beta: editing a non-dev account still saves", guardar(cambia(get_("beta"), "b1", "tier", OTRO), "beta"))
ok("nuevo: a client that never had a dev can save (nothing to lose)",
   guardar(cambia(get_("nuevo"), "n1", "activo", FALSE), "nuevo"))
ok("nuevo: and gets its first dev (e.g. an invitation from principal)",
   guardar(rbind(get_("nuevo"), tabla("nuevo", "nd", DEV)), "nuevo"))
ok("nuevo: from then on that dev is locked too",
   !guardar(cambia(get_("nuevo"), "nd", "tier", OTRO), "nuevo"))

# ── 4. A stale table from another session cannot remove the last dev ─────────
reset()
viejo <- get_("acme")                                   # session A reads: d1, d2
ok("session B demotes d1", guardar(cambia(get_("acme"), "d1", "tier", OTRO), "acme"))
ok("session A, on its stale copy, demotes d2 -- S3 has only d2 as dev now; the result keeps d1 as dev, so it saves",
   guardar(cambia(viejo, "d2", "tier", OTRO), "acme") && env$usuarios_vivos_nivel(get_("acme"), DEV) == 1L)

# ── 5. The UI paths warn with the same helper ────────────────────────────────
src <- readLines("R/tiers_module.R", warn = FALSE, encoding = "UTF-8")
ok("tiers_module: the four paths (delete x2, edit, limit change) check usuarios_niveles_perdidos()",
   sum(grepl("usuarios_niveles_perdidos(", src, fixed = TRUE)) == 4L)
ok("tiers_module: no hand-made dev count left (deleted-only, ignoring activo)",
   !any(grepl("all_u$deleted != TRUE))", src, fixed = TRUE)) &&
     !any(grepl("all_u_check$deleted", src, fixed = TRUE)))
lim <- grep("perdidos <- usuarios_niveles_perdidos(usuarios_orig, usuarios_new)", src, fixed = TRUE)
ok("the limit change checks before saving",
   length(lim) == 1L && lim < grep("auth_save_usuarios(usuarios_new, client_id = cid", src, fixed = TRUE))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
