# tests/test_t95b_vencida.R
# T95b (the Coordinador's call, 2026-11-28): an overdue solicitud stays
# visible to each recipient who has not marked it done; for one who did, for
# the author, and for anything that is not a solicitud (an informative
# personal aviso, a publication), the vence still takes it away, as before.
# Run from the repo root: Rscript --vanilla tests/test_t95b_vencida.R
cat("=== test_t95b_vencida ===\n\n")
`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/i18n.R")
source("R/tier_registry.R")
source("R/permisos.R")
source("R/avisos.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
  invisible(isTRUE(result))
}

# In-memory S3, local to this test (never aws.s3).
.mem <- new.env()
.s3_preload_cache <- new.env(); .s3_missing_cache <- new.env()
.s3_read_with <- function(key, client_id = NULL) .mem[[paste0(client_id, "/", key)]]
.s3_leer_para_escribir <- function(key, client_id) .mem[[paste0(client_id, "/", key)]]
.s3_existe <- function(key, client_id) !is.null(.mem[[paste0(client_id, "/", key)]])
.s3_write <- function(obj, key, client_id = NULL, evento = NULL) assign(paste0(client_id, "/", key), obj, envir = .mem)
CID <- "cliente_prueba"
TIER <- niveles_cliente()[1]
usr <- function(user) list(user = user, tier = TIER, permisos = permisos_resueltos(TIER))
AHORA <- Sys.time()
# Move a row's vence (the API takes a vence; this sets it in the past).
poner_vence <- function(ids, cuando) {
  k <- paste0(CID, "/", .AVISOS_KEY); df <- .mem[[k]]
  df$vence[df$id %in% ids] <- as.POSIXct(cuando, tz = "UTC"); assign(k, df, envir = .mem)
}
titulos <- function(user, propios = FALSE) avisos_visibles(CID, usr(user), propios = propios)$titulo

# A solicitud from ana to dani and eli (one personal row each), already overdue.
sol <- avisos_personales(CID, c("dani", "eli"), list(tipo = "personal", clase = "solicitud", titulo = "Firma vencida",
                                                     cuerpo = "", clave = "sol:v1", autor = "ana"))
avisos_marcar_hecho(CID, sol[2], "eli")
# Not a solicitud: an informative personal aviso (a recordatorio) for dani, and a comunicado.
info <- avisos_personales(CID, "dani", list(tipo = "personal", clase = "recordatorio", titulo = "Aviso informativo vencido",
                                            cuerpo = "", autor = "ana"))
pub <- avisos_agregar(CID, list(tipo = "publicado", clase = "comunicado", titulo = "Comunicado vencido",
                                niveles = niveles_cliente()), por = "ana")
# An overdue solicitud that was withdrawn.
ret <- avisos_personales(CID, "dani", list(tipo = "personal", clase = "solicitud", titulo = "Firma retirada",
                                           cuerpo = "", clave = "sol:v2", autor = "ana"))

cat("-- before its vence (the counter-check: these are visible today) --\n")
ok("0a the author sees her solicitud (Que pedí), not overdue yet", "Firma vencida" %in% titulos("ana", propios = TRUE))
ok("0b eli, who did it, still sees it before its vence", "Firma vencida" %in% titulos("eli"))
ok("0c the informative one and the comunicado are visible before their vence",
   all(c("Aviso informativo vencido", "Comunicado vencido") %in% titulos("dani")))

poner_vence(c(sol, info, pub, ret), AHORA - 3600)
df <- avisos_leer(CID); df$retirado[df$id %in% ret] <- TRUE; assign(paste0(CID, "/", .AVISOS_KEY), df, envir = .mem)

cat("-- after its vence --\n")
ok("1 overdue, for the recipient who has not done it: STILL visible", "Firma vencida" %in% titulos("dani"))
ok("2 overdue, for the recipient who already did it: gone (as before)", !("Firma vencida" %in% titulos("eli")))
ok("3 overdue, for the author: gone from her view (nothing changes for the author)",
   !("Firma vencida" %in% titulos("ana", propios = TRUE)) && !("Firma vencida" %in% titulos("ana")))
ok("4 an overdue informative aviso (not a solicitud) for dani: gone", !("Aviso informativo vencido" %in% titulos("dani")))
ok("5 an overdue comunicado: gone", !("Comunicado vencido" %in% titulos("dani")))
ok("6 an overdue solicitud that was withdrawn: gone", !("Firma retirada" %in% titulos("dani")))
ok("7 one visible row only (dani's own), never eli's", sum(titulos("dani") %in% "Firma vencida") == 1L)
ok("8 a row without clase is never a solicitud", {
  df <- avisos_leer(CID); df$clase[df$id %in% sol[1]] <- NA_character_
  !any(.solicitud_vencida_pendiente(df, "dani")) })
ok("9 no user: nothing extra", !any(.solicitud_vencida_pendiente(avisos_leer(CID), "")))
ok("10 a frame that is not avisos.rds (no clase / hechos / retirado columns, as the vigía's): its rows stay", {
  av <- data.frame(id = c("a", "b"), tipo = "automatico", titulo = c("uno", "dos"), niveles = as.character(jsonlite::toJSON(niveles_cliente())),
                   usuarios = "[]", permiso = NA_character_, vence = as.POSIXct(NA), retirado = FALSE, stringsAsFactors = FALSE)
  av$retirado <- NULL
  identical(.solicitud_vencida_pendiente(av, "dani"), c(FALSE, FALSE)) &&
    nrow(avisos_visibles(CID, usr("dani"), cbind(av, retirado = FALSE))) == 2L })

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
