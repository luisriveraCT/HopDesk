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

# ── S: bancos_module.R ───────────────────────────────────────────────────────
F_BN <- Sys.getenv("ALERTAS_BANCOS_R", "R/bancos_module.R")
bn   <- readLines(F_BN, warn = FALSE, encoding = "UTF-8")
gb   <- ganchos(F_BN)
ok(nrow(gb) == 2L && setequal(gb$accion, c("importar_bancos", "movimiento_manual")),
   "S bancos: 2 ganchos (importar y movimiento manual)")
# Import: after its success log, inside the same observer; the save comes
# first and returns on failure (.bnc_guardar).
li <- grep('action      = "importar_movimientos",', bn, fixed = TRUE)
gi <- gb$linea[gb$accion == "importar_bancos"]
gu <- grep('if (!.bnc_guardar("importar_movimientos"', bn, fixed = TRUE)
ok(length(li) == 1L && length(gi) == 1L && gi > li && gi - li <= 12L && grepl("}, ignoreInit = TRUE)", bn[gi + 1L], fixed = TRUE),
   "S importar: después de su log, al final del observer: un aviso por clic")
ok(length(gu) == 1L && gu < li && grepl("return()", paste(bn[gu:(gu + 1L)], collapse = " "), fixed = TRUE),
   "S importar: si guardar falla, sale antes del gancho")
# Manual: only a new movement (an edit does not alert), after the success log.
lm <- grep('action      = if (is_edit) "editar_movimiento_manual" else "agregar_movimiento_manual",', bn, fixed = TRUE)
gm <- gb$linea[gb$accion == "movimiento_manual"]
gg <- grep('if (!.bnc_guardar("guardar_movimiento_manual"', bn, fixed = TRUE)
ok(length(lm) == 1L && length(gm) == 1L && gm > lm && gm - lm <= 12L && grepl("if (!is_edit) avisos_actividad(", bn[gm], fixed = TRUE),
   "S movimiento manual: después de su log y solo si es nuevo (editar no avisa)")
ok(length(gg) == 1L && gg < lm && grepl("return()", paste(bn[gg:(gg + 1L)], collapse = " "), fixed = TRUE),
   "S movimiento manual: si guardar falla, sale antes del gancho")

# ── S: app.R, Agregar Item (me_save, insert mode) ────────────────────────────
F_APP <- Sys.getenv("ALERTAS_APP_R", "app.R")
ap  <- readLines(F_APP, warn = FALSE, encoding = "UTF-8")
ga  <- ganchos(F_APP)
ins <- grep("# ── Insert mode", ap, fixed = TRUE)
age <- siguiente(ap, "# ── Stage to Agenda de hoy if toggle is active", ins)
sv  <- siguiente(ap, "save_manual(df, client_id = effective_client_id(), base = mi_base)", ins)
fal <- siguiente(ap, "if (!isTRUE(guardado)) {", ins)
rt  <- siguiente(ap, "return()", fal)
mi  <- siguiente(ap, "manual_inv(df)", ins)
lg  <- siguiente(ap, '"agregar_item",', ins)
gi  <- ga$linea[ga$accion == "crear_item"]
ok(nrow(ga) == 1L && length(gi) == 1L, "S app.R: un gancho, crear_item")
ok(length(ins) == 1L && !is.na(sv) && !is.na(fal) && sv < fal && fal < rt && rt < mi,
   "S Agregar Item: guarda primero; si falla, sale (return) antes de tocar la sesión")
ok(!grepl("tryCatch(save_manual", ap[sv], fixed = TRUE) || grepl("guardado <-", ap[sv], fixed = TRUE),
   "S Agregar Item: el resultado de save_manual se revisa (bandera), no solo se avisa")
ok(!is.na(lg) && mi < lg && lg < gi && gi < age, "S Agregar Item: log agregar_item y gancho después del éxito, antes de la Agenda")
ok(any(grepl('"agregar_item_fallido"', ap[fal:rt], fixed = TRUE)) && any(grepl('tr("comun.error_guardar")', ap[fal:rt], fixed = TRUE)),
   "S Agregar Item: la falla se dice y queda en Actividad (agregar_item_fallido)")

# Hooks only through avisos_actividad().
directos <- unlist(lapply(setdiff(list.files("R", "\\.R$", full.names = TRUE, recursive = TRUE),
                                  c("R/avisos.R", "R/avisos_alertas_module.R")), function(f) {
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")
  if (any(grepl("avisos_agregar(", x[!grepl("^\\s*#", x)], fixed = TRUE))) f
}))
# Files that write avisos that are NOT team activity, with why. Team activity
# always goes through avisos_actividad().
# (Requisiciones was listed here for its comments and mentions, but it no
# longer calls avisos_agregar(): the list must stay honest, checked below.)
NO_ACTIVIDAD <- c(
  "R/panorama_publicar_module.R"  = "what a person publishes in Panorama › Publicar (publicado / solicitud)",
  "R/auth_reset.R"                = "security alert on password resets (T31): staff in hd-admin, a client's Dev; never switched off",
  "R/avisos_umbrales.R" = "system alert when a company's past-due CxC moves more than usual (D, umbrales)")
ok(!length(setdiff(directos, names(NO_ACTIVIDAD))),
   sprintf("S nadie llama avisos_agregar() directo para la actividad (%s)",
           paste(setdiff(directos, names(NO_ACTIVIDAD)), collapse = ", ")))
ok(all(names(NO_ACTIVIDAD) %in% directos),
   sprintf("S la lista de quien escribe avisos que no son actividad está al día (%s)",
           paste(setdiff(names(NO_ACTIVIDAD), directos), collapse = ", ")))
# Publicar writes only what a person publishes: never an automatic aviso.
pub <- readLines("R/panorama_publicar_module.R", warn = FALSE, encoding = "UTF-8")
ok(!any(grepl('tipo *= *"automatico"', pub)),
   "S Publicar nunca escribe un aviso automático (eso es avisos_actividad())")

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
