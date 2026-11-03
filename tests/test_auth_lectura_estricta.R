# =============================================================================
# tests/test_auth_lectura_estricta.R
# auth_load_usuarios() returned an EMPTY table whenever reading
# <cid>/usuarios.rds failed. Callers took that for "this client has no users"
# and saved it plus their change: accepting an invitation left the client
# with the invitee as its only account; creating a user or lowering the user
# limit did the same (found 2026-11-01, CM07/CM08).
#   - auth_load_usuarios(): retries, then stop()s on a failed read or an
#     empty read of an object that exists; an empty table only for a client
#     with no file yet.
#   - auth_save_usuarios(): refuses an empty table, or one keeping fewer than
#     half of the accounts active in S3, unless alta_inicial = TRUE.
#   - the three write paths (invitation, create user, user limit) stop and
#     warn when the read fails.
#
# S3 is an in-memory store: aws.s3's s3readRDS / s3saveRDS / object_exists
# are swapped for this test only and ALWAYS restored (on.exit).
#
# Run from project root:  Rscript tests/test_auth_lectura_estricta.R
# =============================================================================
cat("=== test_auth_lectura_estricta ===\n\n")
`%||%` <- function(a, b) if (is.null(a)) b else a

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# ── In-memory S3, swapped in and always restored ──────────────────────────────
store <- new.env(); writes <- 0L; failing <- FALSE
fake_read   <- function(object, bucket, ...) {
  if (failing) stop("simulated S3 timeout")
  if (!exists(object, envir = store, inherits = FALSE)) stop("NoSuchKey")
  get(object, envir = store)
}
fake_save   <- function(x, object, bucket, ...) { writes <<- writes + 1L; assign(object, x, envir = store); invisible(TRUE) }
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
source("R/tier_registry.R")   # niveles_unico_vivo(), for the last-live-account lock
env <- new.env()
for (e in parse("R/auth.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c("auth_load_usuarios", "auth_save_usuarios",
                                  ".usuarios_leer_estricto", ".usuarios_existe",
                                  "usuarios_vivos_nivel", "usuarios_niveles_perdidos"))
    eval(e, env)
env$`%||%` <- `%||%`
env$.s3_bucket <- function() "fake-bucket"
env$auth_invalidate_credentials <- function() invisible(NULL)
env$.migrate_legacy_passwords <- function(raw, cid) raw   # password hashing is not under test

usuarios <- data.frame(
  id = paste0("u", 1:4), account_code = sprintf("U%04d", 1:4),
  username = c("ana", "beto", "caro", "dani"), password_hash = "x", display_name = "",
  tier = "admin", client_id = "acme", permisos = "{}", group_ids = "[]", allowed_clients = "[]",
  email = NA_character_, requires_password_change = FALSE, activo = c(TRUE, TRUE, TRUE, FALSE),
  created_at = "", last_login = NA_character_, deleted = FALSE, deleted_at = NA_character_,
  stringsAsFactors = FALSE)
reset <- function() { rm(list = ls(store), envir = store); assign("acme/usuarios.rds", usuarios, envir = store)
                      writes <<- 0L; failing <<- FALSE }

# 1. Reading
reset(); failing <- TRUE
r <- tryCatch(env$auth_load_usuarios("acme"), error = function(e) e)
ok("a failed read stop()s instead of returning an empty table", inherits(r, "error"))

reset()
ok("a good read returns the 4 accounts", nrow(env$auth_load_usuarios("acme")) == 4L)
ok("a client with no file yet gets an empty table (not an error)",
   is.data.frame(r2 <- env$auth_load_usuarios("nuevo")) && nrow(r2) == 0L)

reset(); assign("acme/usuarios.rds", usuarios[0, ], envir = store)
ok("an EMPTY read of a file that exists stop()s",
   inherits(tryCatch(env$auth_load_usuarios("acme"), error = function(e) e), "error"))

# 2. The invitation's shape: read, then save the table plus the new account
invitacion <- function() {
  u <- tryCatch(env$auth_load_usuarios("acme"), error = function(e) NULL)
  if (is.null(u)) return("aviso")
  nuevo <- u[1, ]; nuevo$username <- "invitado"; nuevo$id <- "u9"
  env$auth_save_usuarios(rbind(u, nuevo), client_id = "acme"); "guardado"
}
reset(); failing <- TRUE
res <- tryCatch(invitacion(), error = function(e) "error")
failing <- FALSE
ok("invitation with a failed read: nothing is written", writes == 0L)
ok("... and the 4 accounts are still there", nrow(get("acme/usuarios.rds", envir = store)) == 4L)

# 3. The save's safety net
reset()
ok("saving an empty table is refused",
   inherits(tryCatch(env$auth_save_usuarios(usuarios[0, ], "acme"), error = function(e) e), "error") && writes == 0L)
reset()
ok("saving a table that keeps 1 of 3 active accounts is refused",
   inherits(tryCatch(env$auth_save_usuarios(usuarios[c(1, 4), ], "acme"), error = function(e) e), "error") && writes == 0L)
reset(); desact <- usuarios; desact$activo[2:3] <- FALSE; desact$deleted[2:3] <- TRUE
ok("deactivating accounts (rows kept) is allowed", isTRUE(env$auth_save_usuarios(desact, "acme")) && writes == 1L)
reset()
ok("adding one account is allowed", isTRUE(env$auth_save_usuarios(rbind(usuarios, usuarios[1, ]), "acme")) && writes == 1L)
reset()
ok("alta_inicial = TRUE lets an empty table through on purpose",
   isTRUE(tryCatch(env$auth_save_usuarios(usuarios[0, ], "acme", alta_inicial = TRUE), error = function(e) FALSE)))

# 4. The three write paths in the app
app   <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
tiers <- paste(readLines("R/tiers_module.R", warn = FALSE), collapse = "\n")
ok("invitation: the read is caught and returns before saving",
   grepl("usuarios <- tryCatch(auth_load_usuarios(client_id = invite$client_id),", app, fixed = TRUE) &&
     grepl("if (is.null(usuarios)) return()", app, fixed = TRUE))
ok("invitation: the invitee only sees the generic message, no internal detail",
   !grepl('paste("Error:", e$message)', app, fixed = TRUE) && grepl('tr("comun.reintentar")', app, fixed = TRUE))
ok("create user: all_usuarios_rv() cancels its consumers when the read fails",
   grepl("req(!is.null(u))", tiers, fixed = TRUE))
ok("user limit: the read is caught and stops with a notice",
   grepl("auth_load_usuarios(client_id = cid), error = function(e) NULL)", tiers, fixed = TRUE) &&
     grepl("No se pudo leer usuarios del cliente.", tiers, fixed = TRUE))

restore()
ok("aws.s3 is restored", identical(get("s3readRDS", ns), orig$s3readRDS) &&
     identical(get("s3saveRDS", ns), orig$s3saveRDS))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
