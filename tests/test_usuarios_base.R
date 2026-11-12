# =============================================================================
# tests/test_usuarios_base.R
# CM15 (2026-11-04): auth_save_usuarios() wrote the whole table the caller
# had, so a session working on a stale copy undid accounts another session
# had created or changed since (lost update). Now callers pass base = the
# table they started from, and only their own changes (by username) are
# applied on S3's current table; the last-live-Dev lock and the shrink check
# then run on that result, never on the stale copy.
#
# Run from project root:  Rscript tests/test_usuarios_base.R
# =============================================================================
cat("=== test_usuarios_base ===\n\n")
source("tests/helpers/s3_memoria.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
falla <- function(expr) inherits(tryCatch({ force(expr); NULL }, error = function(e) e), "error")

source("R/tier_registry.R")
for (e in parse("R/auth.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c("auth_save_usuarios", ".usuarios_leer_estricto", ".usuarios_existe",
                                  "usuarios_vivos_nivel", "usuarios_niveles_perdidos"))
    eval(e, globalenv())
auth_invalidate_credentials <- function() invisible(NULL)

k <- "acme/usuarios.rds"
u <- function(name, tier = "admin", activo = TRUE)
  data.frame(id = paste0("id_", name), account_code = paste0("U_", name), username = name,
             password_hash = "x", display_name = name, tier = tier, client_id = "acme",
             permisos = "{}", group_ids = "[]", allowed_clients = "[]", email = NA_character_,
             requires_password_change = FALSE, activo = activo, created_at = "",
             last_login = NA_character_, deleted = FALSE, deleted_at = NA_character_,
             stringsAsFactors = FALSE)
tabla <- function(...) do.call(rbind, list(...))
s3 <- function() get(k, envir = .mem)
dev <- niveles_unico_vivo()[1]

# 1. Two sessions: A opened [dev1, ana, beto]; B created caro since.
#    A changes ana's tier with base: caro stays, ana changes, beto stays.
s3_memoria_reset()
base_a <- tabla(u("dev1", dev), u("ana"), u("beto"))
assign(k, tabla(u("dev1", dev), u("ana"), u("beto"), u("caro")), envir = .mem)   # B's save
mia <- base_a; mia$tier[mia$username == "ana"] <- "finance"
auth_save_usuarios(mia, client_id = "acme", base = base_a)
ok("two sessions: B's new account survives A's save", "caro" %in% s3()$username)
ok("two sessions: A's change lands", identical(s3()$tier[s3()$username == "ana"], "finance"))
ok("two sessions: 4 accounts, none lost", nrow(s3()) == 4L)

# 2. B changed beto meanwhile; A (stale) only touched ana: B's change kept
s3_memoria_reset()
base_a <- tabla(u("dev1", dev), u("ana"), u("beto"))
otra <- base_a; otra$display_name[otra$username == "beto"] <- "Beto B"
assign(k, otra, envir = .mem)
mia <- base_a; mia$activo[mia$username == "ana"] <- FALSE
auth_save_usuarios(mia, client_id = "acme", base = base_a)
ok("B's edit of another account is not undone",
   identical(s3()$display_name[s3()$username == "beto"], "Beto B") &&
   identical(s3()$activo[s3()$username == "ana"], FALSE))

# 3. The last-live-Dev lock runs on the rebased table, both ways:
# 3a. A (stale: dev1 only Dev) deactivates dev1; B had added dev2 -> allowed
s3_memoria_reset()
base_a <- tabla(u("dev1", dev), u("ana"))
assign(k, tabla(u("dev1", dev), u("ana"), u("dev2", dev)), envir = .mem)
mia <- base_a; mia$activo[mia$username == "dev1"] <- FALSE
ok("lock on the rebased table: dev2 (B's) keeps a live Dev, so A may deactivate dev1",
   !falla(auth_save_usuarios(mia, client_id = "acme", base = base_a)) &&
   identical(s3()$activo[s3()$username == "dev1"], FALSE) && isTRUE(s3()$activo[s3()$username == "dev2"]))
# 3b. A (stale: dev1 and dev2) deactivates dev1; B had deactivated dev2 -> refused
s3_memoria_reset()
base_a <- tabla(u("dev1", dev), u("dev2", dev), u("ana"))
otra <- base_a; otra$activo[otra$username == "dev2"] <- FALSE
assign(k, otra, envir = .mem)
mia <- base_a; mia$activo[mia$username == "dev1"] <- FALSE
n0 <- .mem_escrituras
ok("lock on the rebased table: no live Dev would remain -> refused",
   falla(auth_save_usuarios(mia, client_id = "acme", base = base_a)) && .mem_escrituras == n0)

# 4. Rollback (limit change): base = what was saved, df = the original:
#    only my rows go back; an account another session added in between stays
s3_memoria_reset()
orig <- tabla(u("dev1", dev), u("ana"), u("beto"))
nuevo <- orig; nuevo$activo[nuevo$username == "beto"] <- FALSE
assign(k, rbind(nuevo, u("caro")), envir = .mem)       # my save + B's caro
auth_save_usuarios(orig, client_id = "acme", base = nuevo)
ok("rollback: beto active again, caro (B's) kept",
   isTRUE(s3()$activo[s3()$username == "beto"]) && "caro" %in% s3()$username)

# 5. Without base the old behaviour stays (callers that pass none)
s3_memoria_reset()
assign(k, tabla(u("dev1", dev), u("ana"), u("caro")), envir = .mem)
auth_save_usuarios(tabla(u("dev1", dev), u("ana"), u("beto")), client_id = "acme")
ok("without base: the table is written as given", identical(sort(s3()$username), c("ana", "beto", "dev1")))

# 6. Every caller passes base
# T31 moved two callers out of app.R and tiers_module.R: the activation and
# Usuarios' "Nueva cuenta" save through guardar_cuenta_nueva()
# (R/usuarios_cuentas.R), and the reset link saves in R/auth_reset.R.
# USUARIOS_BASE_ARCHIVOS="a.R;b.R;..." reads other copies (contraprueba).
fuente <- function(f) { x <- readLines(f, warn = FALSE); x[!grepl("^\\s*#", x)] }
archivos <- c("app.R", "R/tiers_module.R", "R/usuarios_cuentas.R", "R/auth_reset.R")
otros <- strsplit(Sys.getenv("USUARIOS_BASE_ARCHIVOS", ""), ";", fixed = TRUE)[[1]]
if (length(otros)) archivos <- otros
src <- paste(unlist(lapply(archivos, fuente)), collapse = "\n")
llamadas <- regmatches(src, gregexpr("auth_save_usuarios\\((?:[^()]|\\((?:[^()]|\\([^()]*\\))*\\))*\\)", src, perl = TRUE))[[1]]
ok(sprintf("all %d auth_save_usuarios() calls in app.R, tiers_module.R, usuarios_cuentas.R and auth_reset.R pass base",
           length(llamadas)),
   length(llamadas) >= 8L && all(grepl("base = ", llamadas, fixed = TRUE)))

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
