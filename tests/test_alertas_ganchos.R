# =============================================================================
# tests/test_alertas_ganchos.R
# Alertas del equipo (C): the one-line hooks in the action handlers.
#   S  structure: each hook is in its handler once, with its accion, AFTER
#      the action succeeded (after the file is written, after the ZIP, after
#      the success log), never inside a shared per-file helper and never
#      on a failure path; nobody calls avisos_agregar() directly for them.
#   V  a hook never makes the action fail: avisos_actividad() swallows a
#      failing shared$effective_client_id()/current_user(), a failing
#      alertas_preparar() and an unreadable S3.
# Run from project root:  Rscript tests/test_alertas_ganchos.R
# =============================================================================
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
source("tests/helpers/s3_memoria.R")
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R", "R/avisos_alertas_module.R", "R/avisos.R"))
  suppressMessages(source(f))

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}

# Hook lines of a file: line number and accion.
ganchos <- function(f) {
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")
  i <- grep("avisos_actividad(", x, fixed = TRUE)
  i <- i[!grepl("^\\s*#", x[i])]
  data.frame(linea = i, accion = sub('^.*avisos_actividad\\([^,]+, "([a-z_]+)".*$', "\\1", x[i]),
             stringsAsFactors = FALSE)
}
# First line at or after `desde` matching `patron`.
siguiente <- function(x, patron, desde, fixed = TRUE) { j <- grep(patron, x, fixed = fixed); j[j >= desde][1] }

# ── S: pagar_hoy_module.R ────────────────────────────────────────────────────
F_PH <- "R/pagar_hoy_module.R"
ph   <- readLines(F_PH, warn = FALSE, encoding = "UTF-8")
g    <- ganchos(F_PH)
ok(nrow(g) == 3L && sum(g$accion == "generar_archivo_pago") == 2L && sum(g$accion == "confirmar_pago") == 1L,
   "S pagar_hoy: 3 ganchos (2 de archivo de pago, 1 de confirmar pago)")

# Never inside .log_pago_generado(): one aviso per click, not per file.
ini <- grep("^\\.log_pago_generado <- function", ph)
fin <- siguiente(ph, "^}", ini, fixed = FALSE)
ok(length(ini) == 1L && !any(g$linea > ini & g$linea < fin), "S archivo de pago: nada dentro de .log_pago_generado()")

# Per company: right after the file is written.
w <- grep("write_ppl(lines, file)", ph, fixed = TRUE)
ok(length(w) == 1L && any(g$accion == "generar_archivo_pago" & g$linea == w + 1L),
   "S archivo de pago por empresa: justo después de write_ppl()")

# ZIP: once, after zip(), only when files were made.
z <- grep('zip(file, fnames, flags = "-j")', ph, fixed = TRUE)
gz <- g$linea[g$accion == "generar_archivo_pago" & g$linea > z[1] & g$linea <= z[1] + 3L]
ok(length(z) == 1L && length(gz) == 1L && grepl("if (length(fnames))", ph[gz], fixed = TRUE),
   "S archivo de pago ZIP: uno por clic, después del zip() y solo si hubo archivos")

# Confirm AP: after the success log, after the incomplete branch's return,
# and not in Confirm AR.
lc  <- grep('"pagar_hoy", "confirmar_pago",', ph, fixed = TRUE)
inc <- grep('action = "confirmar_pago_incompleto"', ph, fixed = TRUE)
ret <- siguiente(ph, "return()", inc)
gc  <- g$linea[g$accion == "confirmar_pago"]
ar  <- grep("# ── Confirm AR", ph, fixed = TRUE)
ok(length(lc) == 1L && length(gc) == 1L && gc > lc && gc - lc <= 15L && gc < ar[1],
   "S confirmar pago: después de su log_action de éxito, antes de Confirm AR")
ok(length(inc) == 1L && ret < lc, "S confirmar pago: la rama incompleta sale (return) antes del gancho")

# Hooks only through avisos_actividad().
directos <- unlist(lapply(setdiff(list.files("R", "\\.R$", full.names = TRUE, recursive = TRUE),
                                  c("R/avisos.R", "R/avisos_alertas_module.R")), function(f) {
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")
  if (any(grepl("avisos_agregar(", x[!grepl("^\\s*#", x)], fixed = TRUE))) f
}))
ok(!length(setdiff(directos, c("R/requisiciones_persistence.R", "R/requisiciones_module.R"))),
   sprintf("S nadie llama avisos_agregar() directo para la actividad (%s)", paste(directos, collapse = ", ")))

# ── V: a hook never makes the action fail ────────────────────────────────────
CID <- "acme"
auth_load_usuarios <- function(client_id = NULL) data.frame(
  username = "ana", display_name = "Ana", tier = "admin", deleted = FALSE, activo = TRUE, stringsAsFactors = FALSE)
s3_memoria_reset()
alertas_guardar(CID, data.frame(usuario = "ana", crear = TRUE, archivo_pago = TRUE,
                                confirmar_pago = TRUE, bancos = TRUE), por = "ana")
roto <- list(effective_client_id = function() stop("sin sesión"), current_user = function() stop("sin usuario"))
sano <- list(effective_client_id = function() CID, current_user = function() "ana")
corre <- function(expr) tryCatch({ suppressMessages(expr); "ok" }, error = function(e) conditionMessage(e))
ok(identical(corre(avisos_actividad(roto$effective_client_id(), "confirmar_pago", sano$current_user(), list(cuerpo = "x"))), "ok"),
   "V client_id que truena: no lanza error")
ok(identical(corre(avisos_actividad(sano$effective_client_id(), "confirmar_pago", roto$current_user(), list(cuerpo = "x"))), "ok"),
   "V usuario que truena: no lanza error")
ok(identical(corre(avisos_actividad(CID, "confirmar_pago", "ana", list(cuerpo = stop("cuerpo roto")))), "ok"),
   "V parámetros que truenan: no lanza error")
.mem_falla <<- paste0(CID, "/avisos.rds"); assign(paste0(CID, "/avisos.rds"), tibble::tibble(), envir = .mem)
ok(identical(corre(avisos_actividad(CID, "confirmar_pago", "ana")), "ok"), "V avisos.rds ilegible: no lanza error")
.mem_falla <<- character()
orig <- alertas_preparar; alertas_preparar <- function(...) stop("boom")
ok(identical(corre(avisos_actividad(CID, "confirmar_pago", "ana")), "ok"), "V alertas_preparar que truena: no lanza error")
alertas_preparar <- orig
rm(list = paste0(CID, "/avisos.rds"), envir = .mem)
avisos_actividad(CID, "confirmar_pago", "ana", list(cuerpo = "1 pago(s)"))
ok(nrow(get(paste0(CID, "/avisos.rds"), envir = .mem)) == 1L, "V con todo sano, el gancho sí publica")

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
