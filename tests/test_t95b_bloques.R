# =============================================================================
# tests/test_t95b_bloques.R
# T95b: "Lo que ha pasado" in three blocks, and its cleaning actions.
#   A. panorama_bloques() (pure): Pendiente (a solicitud for me not done, the
#      overdue first; a live condition alert), never discardable; Nuevo;
#      Antes inside its window (3/7/14 days from seen; never seen never
#      leaves); discarded ones (stored or this session's) not drawn.
#   B. The Clima counts only Pendiente and Nuevo.
#   C. The real panoramaServer (in-memory S3; aws.s3 is never patched):
#      the x discards (one write, logged), Deshacer restores, Limpiar clears
#      every seen discardable one in ONE write and writes NOTHING when there
#      is nothing; a solicitud pending for me is refused on the server;
#      Marcar todo moves the visit; staff inside a client only in the
#      session (never a write).
# Run from project root:  Rscript --vanilla tests/test_t95b_bloques.R
# =============================================================================
cat("=== test_t95b_bloques ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, r) {
  r <- tryCatch(r, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L } else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
invisible(source("tests/helpers/vencidos_fixture_app.R", local = FALSE))
.i18n_dir <- file.path(getwd(), "www", "i18n")
for (f in c("R/tier_registry.R", "R/permisos.R", "R/agenda_filas.R", "R/ui_components.R", "R/avisos.R",
            "R/avisos_alertas_module.R", "R/panorama_datos.R", "R/panorama_publicar_module.R", "R/panorama_module.R"))
  suppressMessages(source(f, encoding = "UTF-8"))
source("tests/helpers/ic_filtro_piezas.R")
for (e in parse("R/data_pipeline.R", keep.source = FALSE))
  if (is.call(e) && identical(as.character(e[[1]]), "<-") && identical(as.character(e[[2]]), "to_calendar_data"))
    eval(e, globalenv())
# In-memory S3, local to this test.
.mem <- new.env(); ESCRITURAS <- character(); EVENTOS <- list()
.s3_read_with <- function(key, client_id = NULL) .mem[[paste0(client_id, "/", key)]]
.s3_leer_para_escribir <- function(key, client_id) .mem[[paste0(client_id, "/", key)]]
.s3_existe <- function(key, client_id) !is.null(.mem[[paste0(client_id, "/", key)]])
.s3_write <- function(obj, key, client_id = NULL, ..., evento = NULL) {
  ESCRITURAS <<- c(ESCRITURAS, key); if (!is.null(evento)) EVENTOS[[length(EVENTOS) + 1L]] <<- evento
  assign(paste0(client_id, "/", key), obj, envir = .mem)
}
leer_tabla_para_escribir <- function(key, client_id, schema) .mem[[paste0(client_id, "/", key)]] %||% schema()
correo_vigia_avisos <- function(...) NULL
LOGS <- list(); log_action <- function(...) { LOGS[[length(LOGS) + 1L]] <<- list(...); invisible(NULL) }
TIER <- niveles_cliente()[1]
CID <- "cli"
reiniciar <- function() { rm(list = ls(.mem), envir = .mem); ESCRITURAS <<- character(); EVENTOS <<- list(); LOGS <<- list() }
leer <- function() avisos_leer(CID)
AHORA <- Sys.time()
d <- function(dias) AHORA - dias * 86400

cat("== A. panorama_bloques ==\n")
fila <- function(id, tipo_feed = "com", tipo = "publicado", clase = "comunicado", cuando = AHORA, vence = as.POSIXct(NA),
                 nuevo = FALSE, usuarios = NA_character_, hechos = NA_character_, descartados = NA_character_,
                 clave = NA_character_, propia = FALSE)
  data.frame(id = id, tipo_feed = tipo_feed, tipo = tipo, clase = clase, clase_ok = clase, cuando = cuando, actualizado = cuando,
             creado = cuando, vence = vence, nuevo = nuevo, usuarios = usuarios, hechos = hechos, descartados = descartados,
             clave = clave, propia = propia, severidad = "info", titulo = id, cuerpo = "", contador = 1L, creado_por = "otro",
             stringsAsFactors = FALSE)
visita <- format(d(1), tz = "UTC", usetz = TRUE)
f <- rbind(
  fila("sol_mia", "sol", "personal", "solicitud", cuando = d(0.5), vence = AHORA + 86400, nuevo = TRUE, usuarios = '["ana"]'),
  fila("sol_vencida", "sol", "personal", "solicitud", cuando = d(5), vence = d(1), usuarios = '["ana"]'),
  fila("sol_hecha", "sol", "personal", "solicitud", cuando = d(2), usuarios = '["ana"]', hechos = '["ana"]'),
  fila("alerta", "ale", "automatico", NA_character_, cuando = d(9), clave = "correo|2026-W48|x"),
  fila("nuevo_com", cuando = d(0.2), nuevo = TRUE),
  fila("visto_com", cuando = d(3)),
  fila("viejo_com", cuando = d(20)),
  fila("act_2", "act", "automatico", NA_character_, cuando = d(2)),
  fila("act_4", "act", "automatico", NA_character_, cuando = d(4)),
  fila("desc", cuando = d(2), descartados = '["ana"]'),
  fila("desc_sesion", cuando = d(2)))
b <- panorama_bloques(f, "ana", visita, AHORA, descartados_sesion = "desc_sesion")
ok("A.1 Pendiente: the solicitud for me not done and the condition alert; the overdue one first",
   identical(b$pendiente$id[1], "sol_vencida") && setequal(b$pendiente$id, c("sol_vencida", "sol_mia", "alerta")))
ok("A.2 ...none of Pendiente has the x", !any(b$pendiente$descartable))
ok("A.3 Nuevo: what is new and not discarded", identical(b$nuevo$id, "nuevo_com") && isTRUE(b$nuevo$descartable))
ok("A.4 Antes: seen inside the window (com 3 d, act 2 d, the done solicitud); out: com 20 d, act 4 d",
   setequal(b$antes$id, c("visto_com", "act_2", "sol_hecha")) && !any(c("viejo_com", "act_4") %in% b$antes$id))
ok("A.5 discarded (stored, or in this session) is not drawn", !any(c("desc", "desc_sesion") %in% unlist(lapply(b, `[[`, "id"))))
b0 <- panorama_bloques(f, "ana", NULL, AHORA)
ok("A.6 never seen (first visit) never leaves by itself", "viejo_com" %in% c(b0$nuevo$id, b0$antes$id) && "act_4" %in% c(b0$nuevo$id, b0$antes$id))

cat("== B. the Clima ==\n")
fu <- fila("urg_vista", cuando = d(2)); fu$severidad <- "urgente"
bb <- panorama_bloques(fu, "ana", visita, AHORA)
ok("B.1 an urgent one already seen (in Antes) does not cloud the Clima",
   nrow(bb$antes) == 1L && panorama_clima(dplyr::bind_rows(bb$pendiente, bb$nuevo))$clima != "nublado" &&
     panorama_clima(fu)$clima == "nublado")

pm <- paste(readLines("R/panorama_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("B.2 the module's Clima is fed Pendiente + Nuevo, not the whole feed",
   grepl("b <- bloques(); f <- dplyr::bind_rows(b$pendiente, b$nuevo)", pm, fixed = TRUE) &&
     !grepl("f <- feed(); ms <- mis_sol(); p <- pagos()", pm, fixed = TRUE))

cat("== C. the real panoramaServer ==\n")
sembrar <- function() {
  reiniciar()
  avisos_agregar(CID, list(tipo = "publicado", clase = "comunicado", titulo = "Viejo 1", cuerpo = "", niveles = niveles_cliente()), por = "otro")
  avisos_agregar(CID, list(tipo = "publicado", clase = "comunicado", titulo = "Viejo 2", cuerpo = "", niveles = niveles_cliente()), por = "otro")
  avisos_personales(CID, "ana", list(tipo = "personal", clase = "solicitud", titulo = "Firma", cuerpo = "",
                                    clave = "sol:g1", autor = "otro", vence = AHORA + 3 * 86400))
  prefs_guardar(CID, "ana", list(ultima_visita = format(AHORA + 60, tz = "UTC", usetz = TRUE)))   # all seen
  ESCRITURAS <<- character(); EVENTOS <<- list(); LOGS <<- list()
}
correr <- function(staff, pasos) {
  shared <- list(interco_v2 = function() list(ar_prefix = "C", ap_prefix = "P", companies = list()),
    company_map = function() CMAP, ic_mode = list(AR = function() "exclude", AP = function() "exclude"),
    df_combined_AR = function() NULL, df_combined_AP = function() NULL, pagar_hoy_db = function() NULL,
    pasivos_provisions_db = function() NULL, sap_snapshot_info = function() list(),
    effective_client_id = function() CID, home_client_id = function() if (staff) "hd-casa" else CID,
    usuario = function() list(user = "ana", tier = TIER, permisos = permisos_resueltos(TIER)),
    current_user_info = function() list(user = "ana", name = "Ana", is_staff = staff),
    tier_efectivo = function() TIER, fx_fix = function() NULL, moneda_base_cliente = function() list(moneda = "MXN"))
  out <- list()
  suppressWarnings(suppressMessages(shiny::testServer(panoramaServer,
    args = list(shared = shared, activa = shiny::reactive(TRUE), tz = shiny::reactive("UTC")), {
      session$flushReact()
      out <<- pasos(session, function() bloques(), output)
    })))
  out
}
sembrar()
ids_viejos <- leer()$id[leer()$titulo %in% c("Viejo 1", "Viejo 2")]
r <- correr(FALSE, function(session, bl, output) {
  a0 <- bl()$antes$id
  n0 <- length(ESCRITURAS)
  session$setInputs(descartar = ids_viejos[1]); session$flushReact()
  n1 <- length(ESCRITURAS); a1 <- bl()$antes$id
  session$setInputs(deshacer = 1); session$flushReact()
  a2 <- bl()$antes$id
  sol <- leer()$id[leer()$clase %in% "solicitud"]
  session$setInputs(descartar = sol); session$flushReact()
  n_sol <- length(ESCRITURAS)
  session$setInputs(limpiar = 1); session$flushReact()
  n_limpiar <- length(ESCRITURAS); a3 <- bl()$antes$id
  session$setInputs(limpiar = 2); session$flushReact()
  n_limpiar2 <- length(ESCRITURAS)
  list(a0 = a0, n0 = n0, n1 = n1, a1 = a1, a2 = a2, n_sol = n_sol, n_limpiar = n_limpiar, a3 = a3, n_limpiar2 = n_limpiar2,
       pend = bl()$pendiente$titulo)
})
ok("C.1 both old ones are in Antes; the solicitud for me is in Pendiente", setequal(r$a0, ids_viejos) && "Firma" %in% r$pend)
ok("C.2 the x: one write, logged descartar_aviso; it leaves Antes",
   r$n1 - r$n0 == 1L && identical(EVENTOS[[1]]$accion, "descartar_aviso") && !(ids_viejos[1] %in% r$a1))
ok("C.3 Deshacer puts it back (restaurar_aviso)", ids_viejos[1] %in% r$a2 && identical(EVENTOS[[2]]$accion, "restaurar_aviso"))
ok("C.4 the x forced on a solicitud pending for me: refused on the server, nothing written", r$n_sol == length(EVENTOS) + 0L && r$n_sol == r$n1 + 1L)
ok("C.5 Limpiar lo visto: ONE write (limpiar_avisos) with every seen discardable one; Antes empty",
   r$n_limpiar - r$n_sol == 1L && identical(EVENTOS[[length(EVENTOS)]]$accion, "limpiar_avisos") && !length(r$a3))
ok("C.6 Limpiar again with nothing to clear: NOTHING written", r$n_limpiar2 == r$n_limpiar)
sembrar()
r2 <- correr(TRUE, function(session, bl, output) {
  session$setInputs(descartar = ids_viejos[1]); session$flushReact()
  a1 <- bl()$antes$id
  session$setInputs(limpiar = 1); session$flushReact()
  session$setInputs(marcar_visto = 1); session$flushReact()
  list(a1 = a1, a2 = bl()$antes$id)
})
ok("C.7 staff inside a client: the x and Limpiar act only in the session (gone from their view)...",
   !(ids_viejos[1] %in% r2$a1) && !length(r2$a2))
ok("C.8 ...and NEVER write the client's data (no write at all, Marcar todo neither; nothing logged)",
   !length(ESCRITURAS) && !length(LOGS))
sembrar()
r3 <- correr(FALSE, function(session, bl, output) {
  avisos_agregar(CID, list(tipo = "publicado", clase = "comunicado", titulo = "Nuevo", cuerpo = "", niveles = niveles_cliente()), por = "otro")
  session$setInputs(marcar_visto = 1); session$flushReact()
  list(nuevo = bl()$nuevo$titulo)
})
p <- prefs_leer(CID, "ana")
ok("C.9 Marcar todo como visto: the visit moves (a preference, written for a client user)",
   !is.null(p$ultima_visita) && as.POSIXct(p$ultima_visita, tz = "UTC") >= AHORA)
ok("C.9b ...and it is logged (a decision, unlike opening Panorama), scoped to the client",
   length(LOGS) == 1L && identical(LOGS[[1]]$action, "guardar_preferencias") &&
     identical(LOGS[[1]]$client_id, CID) && identical(LOGS[[1]]$viewer_home_client_id, CID))

# A solicitud addressed to its own author, not done: not Pendiente (it is
# hers: T91a), not discardable (a solicitud open for her) -- the one kind
# that sits in Antes without an x. avisos_personales() no longer sends one to
# its author, so it is seeded as an older row / another path would have
# written it. Limpiar must not take it, nor write.
reiniciar()
avisos_agregar(CID, list(tipo = "personal", clase = "solicitud", titulo = "Recordatorio propio", cuerpo = "",
                         clave = "sol:p1", autor = "ana", usuarios = "ana"), por = "ana")
prefs_guardar(CID, "ana", list(ultima_visita = format(AHORA + 60, tz = "UTC", usetz = TRUE)))
ESCRITURAS <- character(); EVENTOS <- list()
r4 <- correr(FALSE, function(session, bl, output) {
  a <- bl()$antes
  html <- as.character(output$feed$html)
  n0 <- length(ESCRITURAS)   # opening Panorama writes the visit: not Limpiar's
  session$setInputs(limpiar = 1); session$flushReact()
  list(antes = a$titulo, desc = a$descartable, html = html, despues = bl()$antes$titulo,
       escritas = length(ESCRITURAS) - n0)
})
ok("C.10 her own open solicitud is in Antes, without the x and without 'Limpiar lo visto'",
   identical(r4$antes, "Recordatorio propio") && identical(r4$desc, FALSE) &&
     !grepl("pan-item-x", r4$html, fixed = TRUE) && !grepl(tr("panorama.bloques.limpiar"), r4$html, fixed = TRUE))
ok("C.11 Limpiar forced anyway: nothing to clear, so NOTHING written, and it stays",
   r4$escritas == 0L && !length(EVENTOS) && identical(r4$despues, "Recordatorio propio"))

# The whole chain (avisos_visibles -> feed -> blocks -> HTML): a solicitud
# for ana already past its vence reaches Panorama, first in Pendiente and in
# red (Chrome showed the old vigencia dropping it before Panorama).
reiniciar()
avisos_personales(CID, "ana", list(tipo = "personal", clase = "solicitud", titulo = "Firma atrasada", cuerpo = "",
                                  clave = "sol:v9", autor = "otro", vence = AHORA - 3600))
avisos_personales(CID, "ana", list(tipo = "personal", clase = "solicitud", titulo = "Firma a tiempo", cuerpo = "",
                                  clave = "sol:v10", autor = "otro", vence = AHORA + 86400))
r5 <- correr(FALSE, function(session, bl, output) list(pend = bl()$pendiente$titulo, html = as.character(output$feed$html)))
ok("C.12 an overdue solicitud for me reaches Panorama: first in Pendiente, marked Vencida (red)",
   identical(r5$pend[1], "Firma atrasada") && "Firma a tiempo" %in% r5$pend &&
     grepl("pan-item--vencida", r5$html, fixed = TRUE) && grepl(tr("panorama.bloques.vencida"), r5$html, fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
