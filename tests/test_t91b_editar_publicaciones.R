# =============================================================================
# tests/test_t91b_editar_publicaciones.R
# T91b: «Mis publicaciones» with Editar and Borrar; an edit shows as such.
#   A. avisos_editar(): in place (same id), editado_en/por set, creado and
#      actualizado never move; nothing changed = nothing written; only the
#      author (a moderator neither); a deleted one is not edited; a
#      solicitud edits every recipient's row and keeps their hechos.
#   B. The audience (Coordinador's (c)): widened -> the new tier sees it as
#      NEW and not "edited"; the old tiers see it "edited", not new; narrowed
#      -> it leaves the dropped tier.
#   C. An edited public note updates its aviso in place (contraprueba: the
#      T81 path, notas_aviso_publicar, kept the old text).
#   D. The server: Editar one's own in Mis publicaciones saves in place and
#      logs before/after with client_id and viewer_home_client_id; someone
#      else's is refused even to a moderator; Borrar someone else's needs
#      can_moderate_panorama; a note is managed in Notas del mes.
# Run from project root:  Rscript --vanilla tests/test_t91b_editar_publicaciones.R
# =============================================================================
cat("=== test_t91b_editar_publicaciones ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a
.i18n_dir <- file.path(getwd(), "www", "i18n")
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R", "R/avisos.R", "R/hd_registro.R", "R/hd_piezas.R",
            "R/monedas.R", "R/hd_columnas.R", "R/hd_table.R", "R/avisos_alertas_module.R", "R/panorama_datos.R",
            "R/panorama_publicar_module.R"))
  suppressMessages(source(f, encoding = "UTF-8"))
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L } else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
# In-memory S3, local to this test (aws.s3 is never patched).
.mem <- new.env(); .escrituras <- 0L
.s3_read_with <- function(key, client_id = NULL) .mem[[paste0(client_id, "/", key)]]
.s3_leer_para_escribir <- function(key, client_id) .mem[[paste0(client_id, "/", key)]]
.s3_existe <- function(key, client_id) !is.null(.mem[[paste0(client_id, "/", key)]])
.s3_write <- function(obj, key, client_id = NULL, ...) { .escrituras <<- .escrituras + 1L; assign(paste0(client_id, "/", key), obj, envir = .mem) }
leer_tabla_para_escribir <- function(key, client_id, schema) .mem[[paste0(client_id, "/", key)]] %||% schema()
NIV <- niveles_cliente(); TI <- nivel_ti(); N1 <- NIV[1]; N2 <- NIV[2]; BAJO <- NIV[length(NIV)]
auth_load_usuarios <- function(client_id = NULL) data.frame(
  username = c("ana", "beto", "caro"), display_name = c("Ana", "Beto", "Caro"),
  tier = c(TI, N2, BAJO), deleted = FALSE, activo = TRUE, stringsAsFactors = FALSE)
LOGS <- list()
log_action <- function(user, module, action, ...) { LOGS[[length(LOGS) + 1L]] <<- c(list(action = action), list(...)); invisible(NULL) }
CID <- "acme"
reiniciar <- function() { rm(list = ls(.mem), envir = .mem); .escrituras <<- 0L; LOGS <<- list() }
cuenta <- function(expr) { n0 <- .escrituras; force(expr); .escrituras - n0 }
leer <- function() avisos_leer(CID)
T0 <- as.POSIXct("2026-11-20 10:00:00", tz = "UTC")

cat("== A. avisos_editar ==\n")
reiniciar()
id <- avisos_agregar(CID, list(tipo = "publicado", clase = "comunicado", titulo = "Cierre", cuerpo = "Jueves",
                                niveles = N1, vence = T0 + 30 * 86400), por = "ana")
a0 <- leer()
r <- avisos_editar(CID, id, list(titulo = "Cierre de mes", cuerpo = "Viernes"), por = "ana", ahora = T0 + 3600)
a1 <- leer()
ok("A.1 in place: the same id, the new text", nrow(a1) == 1L && identical(a1$id, id) && identical(a1$titulo, "Cierre de mes"))
ok("A.2 editado_en / editado_por set; creado and actualizado never move (an edit is not news)",
   identical(as.numeric(a1$editado_en), as.numeric(T0 + 3600)) && identical(a1$editado_por, "ana") &&
     identical(a1$creado, a0$creado) && identical(a1$actualizado, a0$actualizado))
ok("A.3 the log gets the text before and after", identical(r$antes$titulo, "Cierre") && identical(r$despues$titulo, "Cierre de mes") && isTRUE(r$cambiado))
n <- cuenta(r2 <- avisos_editar(CID, id, list(titulo = "Cierre de mes", cuerpo = "Viernes"), por = "ana"))
ok("A.4 nothing changed: nothing written", n == 0L && !isTRUE(r2$cambiado))
n <- cuenta(e <- tryCatch(avisos_editar(CID, id, list(titulo = "Otro"), por = "beto"), error = function(e) e))
ok("A.5 someone else (a moderator too) never edits it: refused, nothing written", inherits(e, "error") && n == 0L && identical(leer()$titulo, "Cierre de mes"))
avisos_retirar(CID, id, "ana")
ok("A.6 a deleted one is not edited", inherits(tryCatch(avisos_editar(CID, id, list(titulo = "X"), por = "ana"), error = function(e) e), "error"))
reiniciar()
avisos_personales(CID, c("beto", "caro"), list(tipo = "personal", clase = "solicitud", titulo = "Firma", cuerpo = "",
                                              clave = "sol:g1", autor = "ana", vence = T0 + 86400 * 5))
s0 <- leer(); avisos_marcar_hecho(CID, s0$id[1], "beto"); s0 <- leer()
avisos_editar(CID, "sol:g1", list(titulo = "Firma del contrato"), por = "ana")
s1 <- leer()
ok("A.7 a solicitud: every recipient's row edited, each keeping its hechos and recipients",
   all(s1$titulo == "Firma del contrato") && identical(s1$hechos, s0$hechos) && identical(s1$usuarios, s0$usuarios) && all(!is.na(s1$editado_en)))

cat("== B. the audience ==\n")
reiniciar()
id <- avisos_agregar(CID, list(tipo = "publicado", clase = "comunicado", titulo = "Aviso", cuerpo = "", niveles = N1), por = "ana")
creado <- leer()$creado
visita <- creado + 60                                   # both readers visited after it was published
avisos_editar(CID, id, list(niveles = c(N1, N2)), por = "ana", ahora = creado + 3600)
v <- leer()
f_n2 <- panorama_feed(v, visita, usuario = "x", nivel = N2)
f_n1 <- panorama_feed(v, visita, usuario = "y", nivel = N1)
ok("B.1 widened: the tier added sees it as NEW, and not as edited", isTRUE(f_n2$nuevo) && !isTRUE(f_n2$editado))
ok("B.2 ...the tier that had it sees it edited, not new", !isTRUE(f_n1$nuevo) && isTRUE(f_n1$editado))
u2 <- list(user = "beto", tier = N2, permisos = permisos_resueltos(N2))
ok("B.3 the added tier's reader does see it (avisos_visibles)", id %in% avisos_visibles(CID, u2)$id)
avisos_editar(CID, id, list(niveles = N1), por = "ana", ahora = creado + 7200)
ok("B.4 narrowed: it leaves the dropped tier, and its arrival time goes with it",
   !(id %in% avisos_visibles(CID, u2)$id) && is.na(leer()$niveles_alta))
avisos_editar(CID, id, list(titulo = "Aviso 2"), por = "ana", ahora = creado + 9000)
avisos_editar(CID, id, list(niveles = c(N1, N2)), por = "ana", ahora = creado + 9500)
avisos_editar(CID, id, list(titulo = "Aviso 3"), por = "ana", ahora = creado + 9900)
ok("B.5 an edit AFTER the tier arrived shows to it as edited", isTRUE(panorama_feed(leer(), creado + 9600, usuario = "x", nivel = N2)$editado))

cat("== C. the edited public note ==\n")
for (f in c("R/notes_handlers.R")) suppressMessages(source(f, encoding = "UTF-8"))
notas_enlace <- function(...) NA_character_
reiniciar()
nota <- list(id = "n1", title = "Inventario", body = "El lunes", author = "Ana", year = 2026, month = 11)
notas_aviso_publicar(CID, nota, por = "ana")
id_av <- leer()$id
nota$body <- "El martes"
notas_aviso_publicar(CID, nota, por = "ana")             # the T81 path on an edit
ok("C.1 contraprueba: the T81 path (repetir = FALSE) kept the old text", identical(leer()$cuerpo, "El lunes"))
notas_aviso_editar(CID, nota, por = "ana")
x <- leer()
ok("C.2 notas_aviso_editar: the same aviso takes the new text, marked edited", identical(x$id, id_av) && identical(x$cuerpo, "El martes") && !is.na(x$editado_en))
notas_aviso_editar(CID, modifyList(nota, list(body = "Otro")), por = "beto")
ok("C.3 ...and only its author (someone else changes nothing)", identical(leer()$cuerpo, "El martes"))

cat("== D. the server (Mis publicaciones) ==\n")
sesion <- function(nivel, user, guion) {
  usr <- list(user = user, tier = nivel, permisos = permisos_resueltos(nivel))
  shared <- list(effective_client_id = function() CID, home_client_id = function() "casa-de-prueba",
                 current_user = function() user, usuario = reactiveVal(usr),
                 current_user_info = function() list(user = user, is_staff = FALSE))
  suppressWarnings(suppressMessages(testServer(panoramaPublicarServer, args = list(shared = shared), {
    session$flushReact(); guion(session, function() publicadas())
  })))
}
reiniciar()
id <- avisos_agregar(CID, list(tipo = "publicado", clase = "comunicado", titulo = "Junta", cuerpo = "10 am",
                                niveles = NIV, vence = Sys.time() + 5 * 86400), por = "ana")
sesion(TI, "ana", function(session, pub) {
  session$setInputs(`tabla-accion` = list(accion = "editar", ids = list(id), nonce = 1))
  session$setInputs(titulo = "Junta de cierre", mensaje = "11 am", vence = Sys.Date() + 6, enlace = "")
  session$setInputs(publicar = 1)
})
x <- leer(); lg <- Filter(function(l) identical(l$action, "editar_publicacion"), LOGS)
ok("D.1 Editar one's own: saved in place (same id, new text)", nrow(x) == 1L && identical(x$id, id) && identical(x$titulo, "Junta de cierre") && identical(x$cuerpo, "11 am"))
ok("D.2 logged editar_publicacion with before/after, client_id and viewer_home_client_id",
   length(lg) == 1L && identical(lg[[1]]$client_id, CID) && identical(lg[[1]]$viewer_home_client_id, "casa-de-prueba") &&
     identical(lg[[1]]$metadata$antes$titulo, "Junta") && identical(lg[[1]]$metadata$despues$titulo, "Junta de cierre"))
mod <- Filter(function(t) puede(list(tier = t, permisos = permisos_resueltos(t)), "can_moderate_panorama"), niveles_todos())[1]
n <- cuenta(sesion(mod, "moder", function(session, pub) {
  session$setInputs(`tabla-accion` = list(accion = "editar", ids = list(id), nonce = 1))
  session$setInputs(titulo = "Cambiado por otro", mensaje = "", vence = Sys.Date() + 6, enlace = "")
  session$setInputs(publicar = 1)
}))
ok("D.3 a moderator does NOT edit someone else's (refused on the server; the click then only publishes a new one)",
   identical(leer()$titulo[leer()$id == id], "Junta de cierre") && identical(leer()$editado_por[leer()$id == id], "ana"))
NOMOD <- Filter(function(t) !puede(list(tier = t, permisos = permisos_resueltos(t)), "can_moderate_panorama"), NIV)[1]
sesion(NOMOD, "beto", function(session, pub) {
  session$setInputs(`tabla-accion` = list(accion = "eliminar", ids = list(id), nonce = 1))
})
ok("D.4 Borrar someone else's without can_moderate_panorama: nothing", !isTRUE(leer()$retirado[leer()$id == id]))
sesion(mod, "moder", function(session, pub) {
  session$setInputs(`tabla-accion` = list(accion = "eliminar", ids = list(id), nonce = 1))
})
ok("D.5 a moderator deletes it -- a mark, the row stays", isTRUE(leer()$retirado[leer()$id == id]) && id %in% leer()$id)
reiniciar()
notas_aviso_publicar(CID, nota, por = "ana"); id_n <- leer()$id
sesion(TI, "ana", function(session, pub) {
  ok("D.6 a public note of mine is listed in Mis publicaciones", "nota" %in% pub()$tipo)
  session$setInputs(`tabla-accion` = list(accion = "eliminar", ids = list(id_n), nonce = 1))
})
ok("D.7 ...but deleted only in Notas del mes (here nothing)", !isTRUE(leer()$retirado))
ok("D.8 publicadas_editables: own yes; someone else's and notes no",
   identical(publicadas_editables(data.frame(por = c("ana", "beto", "ana"), tipo = c("comunicado", "comunicado", "nota")),
                                  list(user = "ana"))$rechazadas, 2L))

cat("== E. Publicar for everyone; Nueva only with can_publish_panorama ==\n")
SIN <- Filter(function(t) !puede(list(tier = t, permisos = permisos_resueltos(t)), "can_publish_panorama"), NIV)[1]
if (is.na(SIN)) cat("  (every client tier may publish: E.1-E.2 skipped)\n") else {
  reiniciar()
  id <- avisos_agregar(CID, list(tipo = "publicado", clase = "comunicado", titulo = "Mía de antes", cuerpo = "",
                                  niveles = NIV, vence = Sys.time() + 5 * 86400), por = "caro")
  vistas <- NULL
  n <- cuenta(sesion(SIN, "caro", function(session, pub) {
    vistas <<- pub()
    session$setInputs(titulo = "Nueva sin permiso", mensaje = "", vence = Sys.Date() + 3, enlace = "")
    session$setInputs(publicar = 1)
  }))
  ok("E.1 without can_publish_panorama: Mis publicaciones lists one's own", identical(vistas$id, id))
  ok("E.2 ...and Nueva cannot be used, even forcing the button (nothing written)", n == 0L && nrow(leer()) == 1L)
}
pp <- paste(readLines("R/panorama_publicar_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
pm <- paste(readLines("R/panorama_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("E.3 the Nueva tab shows only with can_publish_panorama (server side), and the publish observer still checks it",
   grepl('shinyjs::toggle("tab_nueva", condition = p)', pp, fixed = TRUE) &&
     grepl('if (!puede_o_avisa(shared, "can_publish_panorama")) return()', pp, fixed = TRUE))
ok("E.4 Panorama opens Publicar for everyone (the button says Mis publicaciones without the permission)",
   grepl('shiny::observeEvent(input$abrir_publicar, modo("publicar"), ignoreInit = TRUE)', pm, fixed = TRUE) &&
     grepl('"panorama.publicar.publicadas"', pm, fixed = TRUE))
ok("E.5 (the Chrome finding, T106 pattern) the form's outputs are not suspended while their section is hidden",
   grepl('for (o in c("edicion", "vista", "tipos", "para", "alcance", "vence_ui"))', pp, fixed = TRUE) &&
     grepl("shiny::outputOptions(output, o, suspendWhenHidden = FALSE)", pp, fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
