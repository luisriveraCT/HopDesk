# =============================================================================
# tests/test_t82_aviso_cuenta.R
# T82: someone joining the client is seen in Panorama.
#   1. A new account leaves one aviso; dev and admin see it, finance,
#      analysis and compras do not; anyone given the permission by hand does.
#   2. The same account never tells twice; an inactive one tells nobody.
#   3. With nobody holding the permission, nothing is published.
#   4. An aviso that fails never fails the account (no error).
#   5. The activation hands over what the aviso says, never the hash; its
#      aviso says "accepted their invitation" and is not new to the invitee.
#   6. Both call sites are there (Usuarios and the activation, after its log).
# S3 is an in-memory store local to this test; aws.s3 is never touched.
# Contraprueba: on e4f77ff (T81, before T82) avisar_cuenta_nueva does not exist.
# Run: Rscript --vanilla tests/test_t82_aviso_cuenta.R (repo root)
# =============================================================================
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
source("R/i18n.R")
source("R/tier_registry.R")
source("R/permisos.R")
source("R/avisos.R")
source("R/panorama_datos.R")
source("R/usuarios_cuentas.R")
S3_KEYS <- list(usuarios = "usuarios.rds", tiers_config = "tiers_config.rds")

pass <- 0L; fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("  PASS ", label, "\n") }
  else { fail <<- fail + 1L; cat("  FAIL ", label, "\n") }
}

.mem <- new.env()
.s3_preload_cache <- new.env(); .s3_missing_cache <- new.env()
.s3_read_with <- function(key, client_id = NULL) .mem[[paste0(client_id, "/", key)]]
.s3_leer_para_escribir <- function(key, client_id) .mem[[paste0(client_id, "/", key)]]
.s3_existe <- function(key, client_id) !is.null(.mem[[paste0(client_id, "/", key)]])
.fallar_escrituras <- 0L
.s3_write <- function(obj, key, client_id = NULL) {
  if (.fallar_escrituras > 0L) { .fallar_escrituras <<- .fallar_escrituras - 1L; stop("simulada") }
  assign(paste0(client_id, "/", key), obj, envir = .mem)
}

CID <- "cliente_prueba"
usuarios <- data.frame(
  username = c("dora", "adan", "fede", "ali", "coco", "fina"),
  tier     = c(nivel_ti(), "admin", "finance", "analysis", "compras", "finance"),
  permisos = c("{}", "{}", "{}", "{}", "{}", '{"can_receive_account_notices": true}'),
  account_code = sprintf("U%04d", 1:6), deleted = FALSE, activo = TRUE, stringsAsFactors = FALSE)
assign(paste0(CID, "/usuarios.rds"), usuarios, envir = .mem)

usr <- function(i) list(user = usuarios$username[i], tier = usuarios$tier[i],
                        permisos = permisos_resueltos(usuarios$tier[i], usuarios$permisos[i]))
ve <- function(i, clave) any(avisos_visibles(CID, usr(i))$clave %in% clave)
fila <- function(user, code, activo = TRUE) data.frame(username = user, display_name = "Nora Nueva",
  tier = "finance", puesto = "Tesorera", account_code = code, activo = activo, stringsAsFactors = FALSE)

cat("\n== 1. Quién lo ve ==\n")
avisar_cuenta_nueva(CID, fila("nora", "U0007"), por = "adan", via = "alta")
a <- avisos_leer(CID)
ok("1.1 una cuenta nueva deja un aviso", sum(a$clave %in% "cuenta|U0007") == 1L)
ok("1.2 lo ve dev", ve(1, "cuenta|U0007"))
ok("1.3 lo ve admin", ve(2, "cuenta|U0007"))
ok("1.4 no lo ven finance, analysis ni compras", !ve(3, "cuenta|U0007") && !ve(4, "cuenta|U0007") && !ve(5, "cuenta|U0007"))
ok("1.5 sí lo ve quien tiene el permiso a mano", ve(6, "cuenta|U0007"))
ok("1.6 nombra a la persona y su nivel", grepl("Nora Nueva", a$titulo[a$clave %in% "cuenta|U0007"]) &&
     grepl(nivel_nombre("finance"), a$titulo[a$clave %in% "cuenta|U0007"], fixed = TRUE))
ok("1.7 dice puesto y quién la creó", grepl("Tesorera", a$cuerpo[a$clave %in% "cuenta|U0007"]) &&
     grepl("adan", a$cuerpo[a$clave %in% "cuenta|U0007"]))

cat("\n== 2. Sin repetir; inactiva no avisa ==\n")
avisar_cuenta_nueva(CID, fila("nora", "U0007"), por = "adan", via = "alta")
a <- avisos_leer(CID)
ok("2.1 la misma cuenta no avisa dos veces", sum(a$clave %in% "cuenta|U0007") == 1L &&
     identical(a$contador[a$clave %in% "cuenta|U0007"], 1L))
avisar_cuenta_nueva(CID, fila("ines", "U0008", activo = FALSE), por = "adan", via = "alta")
ok("2.2 una cuenta inactiva no avisa", !any(avisos_leer(CID)$clave %in% "cuenta|U0008"))

cat("\n== 3. Nadie con el permiso: nada ==\n")
CID2 <- "sin_lectores"
assign(paste0(CID2, "/usuarios.rds"), usuarios[3:5, ], envir = .mem)
avisar_cuenta_nueva(CID2, fila("nora", "U0009"), por = "fede", via = "alta")
ok("3.1 sin lectores no se publica", !nrow(avisos_leer(CID2)))
assign("solo_dora/usuarios.rds", usuarios[1, ], envir = .mem)
ok("3.2 la nueva no cuenta como lectora", !cuentas_alguien_recibe_aviso("solo_dora", "dora") &&
     cuentas_alguien_recibe_aviso("solo_dora", "otra"))

cat("\n== 4. Un aviso que truena no tumba la cuenta ==\n")
.fallar_escrituras <- 2L
r <- tryCatch({ avisar_cuenta_nueva(CID, fila("olga", "U0010"), por = "adan", via = "alta"); TRUE },
              error = function(e) FALSE)
.fallar_escrituras <- 0L
ok("4.1 no lanza error", r)

cat("\n== 5. La activación ==\n")
# The real activar_invitacion; only its S3, the hash and the name index are local.
auth_load_usuarios <- function(client_id) .mem[[paste0(client_id, "/usuarios.rds")]]
.next_account_code <- function(u) "U0100"
hash_password <- function(x) "HASH-SECRETO"
.inv <- data.frame(token = "tk", status = "pending", accepted_at = NA_character_, stringsAsFactors = FALSE)
modificar_pending_invites <- function(cambio) { .inv <<- cambio(.inv); invisible(.inv) }
reservar_usuario <- function(username, ...) username
.hd_admin_agregar <- function(df, fila) dplyr::bind_rows(df, fila)
auth_save_usuarios <- function(df, client_id, base = NULL) assign(paste0(client_id, "/usuarios.rds"), df, envir = .mem)
.usuarios_en_credenciales <- function() character()
res <- activar_invitacion(list(client_id = CID, tier = "analysis", email = "x@y.mx", token = "tk"),
  list(nombre = "Pau Pérez", puesto = "Analista", usuario = "paup", contrasena = "Una-clave-larga-9", confirmar = "Una-clave-larga-9"))
ok("5.1 la activación funciona", isTRUE(res$ok))
ok("5.2 entrega la cuenta sin el hash", is.data.frame(res$cuenta) && !"password_hash" %in% names(res$cuenta) &&
     !any(grepl("HASH-SECRETO", unlist(res$cuenta), fixed = TRUE)))
avisar_cuenta_nueva(CID, res$cuenta, por = res$username, via = "invitacion")
a <- avisos_leer(CID); i <- which(a$clave %in% paste0("cuenta|", res$account_code))
ok("5.3 la activación deja su aviso", length(i) == 1L && grepl(tr("avisos.cuenta.via_invitacion"), a$cuerpo[i], fixed = TRUE))
v <- avisos_visibles(CID, usr(1))
f <- panorama_feed(v, as.POSIXct("2000-01-01", tz = "UTC"), usuario = res$username)
ok("5.4 no es nuevo para quien se acaba de unir", !any(f$nuevo[f$clave %in% paste0("cuenta|", res$account_code)]))

cat("\n== 6. Los dos caminos lo llaman ==\n")
app  <- readLines("app.R", warn = FALSE, encoding = "UTF-8")
tm   <- readLines("R/tiers_module.R", warn = FALSE, encoding = "UTF-8")
i_log <- grep('action = "activacion_usuario"', app, fixed = TRUE)
i_av  <- grep("avisar_cuenta_nueva(invite$client_id, res$cuenta", app, fixed = TRUE)
ok("6.1 la activación lo llama, después de su log", length(i_log) == 1L && length(i_av) == 1L && i_av > i_log && i_av - i_log < 8L)
j_log <- grep('action      = "crear_usuario"', tm, fixed = TRUE)
j_av  <- grep('avisar_cuenta_nueva(current_client_id(), new_row', tm, fixed = TRUE)
ok("6.2 Usuarios lo llama, después de su log", length(j_log) == 1L && length(j_av) == 1L && j_av > j_log && j_av - j_log < 12L)

cat(sprintf("\nT82 aviso de cuenta nueva: %d PASS / %d FAIL\n", pass, fail))
if (fail > 0L) quit(status = 1L)
