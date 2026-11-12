# =============================================================================
# tests/test_t31_esquema.R
# T31, the account table:
#   1. auth_load_usuarios() adds `puesto` and `password_changed_at` as NA to
#      a table saved before they existed -- in memory only: nothing is
#      written to S3 to add them (no migration of production);
#   2. a new client's empty table, and .schema_usuarios_init(), carry both;
#   3. create_invite(): a 64-hex token from openssl, not from sample() (the
#      same seed must not give the same token);
#   4. the activation builds the account with a UUID and .next_account_code()
#      (a deleted account's code is never handed out twice).
# S3 is the in-memory store of tests/helpers/s3_memoria.R (no network).
# Run from project root:  Rscript tests/test_t31_esquema.R
# AUTH_R / APP_R / PERSIST_R = other copies to check (contraprueba).
# =============================================================================
cat("=== test_t31_esquema ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("tests/helpers/s3_memoria.R")
F_AUTH <- Sys.getenv("AUTH_R", "R/auth.R"); F_APP <- Sys.getenv("APP_R", "app.R")
F_PER  <- Sys.getenv("PERSIST_R", "R/persistence.R")
if (!identical(F_PER, "R/persistence.R")) source(F_PER)
for (e in parse(F_AUTH, keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c("auth_load_usuarios", ".usuarios_leer_estricto", ".usuarios_existe"))
    eval(e, globalenv())
auth_invalidate_credentials <- function() invisible(NULL)
.migrate_legacy_passwords <- function(raw, cid) raw

# ── 1. Old table: NA columns, no write ──────────────────────────────────────
viejos <- data.frame(
  id = c("u_20260101000000", "a1b2"), account_code = c("U0001", "U0002"), username = c("ana", "beto"),
  password_hash = "x", display_name = c("Ana", "Beto"), tier = "admin", client_id = "acme",
  permisos = "{}", group_ids = "[]", allowed_clients = "[]", email = NA_character_,
  requires_password_change = FALSE, activo = TRUE, created_at = "", last_login = NA_character_,
  deleted = FALSE, deleted_at = NA_character_, stringsAsFactors = FALSE)
assign("acme/usuarios.rds", viejos, envir = .mem)
antes <- .mem_escrituras
u <- auth_load_usuarios("acme")
ok("una tabla vieja trae puesto y password_changed_at en NA",
   all(c("puesto", "password_changed_at") %in% names(u)) && all(is.na(u$puesto)) &&
     all(is.na(u$password_changed_at)))
ok("... sin escribir nada en S3", .mem_escrituras == antes &&
     !"puesto" %in% names(get("acme/usuarios.rds", envir = .mem)))
ok("un puesto ya guardado se conserva",
   { v <- viejos; v$puesto <- c("Tesorería", NA); assign("acme/usuarios.rds", v, envir = .mem)
     identical(auth_load_usuarios("acme")$puesto, c("Tesorería", NA)) })

# ── 2. Empty schemas ─────────────────────────────────────────────────────────
ok("un cliente sin archivo: la tabla vacía trae las dos columnas",
   all(c("puesto", "password_changed_at") %in% names(auth_load_usuarios("nuevo"))))
ok(".schema_usuarios_init() trae las dos columnas",
   all(c("puesto", "password_changed_at") %in% names(.schema_usuarios_init())))

# ── 3. Invitation token ──────────────────────────────────────────────────────
.mem_set_invites <- function() assign("hd-admin/pending_invites.rds", .schema_pending_invites(), envir = .mem)
.mem_set_invites()
set.seed(1); t1 <- create_invite("a@x.mx", "A", "acme", "admin", "luis")
set.seed(1); t2 <- create_invite("b@x.mx", "B", "acme", "admin", "luis")
ok("el token de invitación son 64 caracteres hex", grepl("^[0-9a-f]{64}$", t1) && grepl("^[0-9a-f]{64}$", t2))
ok("con la misma semilla, tokens distintos (no sale de sample())", !identical(t1, t2))
ok("resolve_invite() encuentra la invitación con ese token",
   identical(resolve_invite(t1)$email, "a@x.mx"))

# ── 4. The activation's new account ─────────────────────────────────────────
# The activation builds the account in activar_invitacion() (R/usuarios_cuentas.R;
# its behaviour is in tests/test_t31_activacion.R). app.R keeps no copy.
app <- paste(readLines(F_APP, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
act <- paste(deparse(body(local({ source(Sys.getenv("CUENTAS_R", "R/usuarios_cuentas.R"), local = TRUE)
                                  activar_invitacion }))), collapse = "\n")
ok("la activación crea el id con uuid::UUIDgenerate()",
   grepl("id = uuid::UUIDgenerate()", act, fixed = TRUE) && !grepl('paste0("u_", format(Sys.time()', app, fixed = TRUE))
ok("la activación toma el código con .next_account_code(usuarios)",
   grepl("account_code = .next_account_code(usuarios)", act, fixed = TRUE) &&
     !grepl("nrow(usuarios) + 1", app, fixed = TRUE))
ok("la cuenta nueva trae las dos columnas", grepl("puesto = ", act, fixed = TRUE) &&
     grepl("password_changed_at = ", act, fixed = TRUE))
tiers <- parse("R/tiers_module.R", keep.source = FALSE)
for (e in tiers) if (is.call(e) && identical(e[[1]], as.name("<-")) &&
                     identical(as.character(e[[2]]), ".next_account_code")) eval(e, globalenv())
ok(".next_account_code(): tras borrar U0002 de U0001..U0003 da U0004, no U0003",
   identical(.next_account_code(data.frame(account_code = c("U0001", "U0003"))), "U0004"))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
