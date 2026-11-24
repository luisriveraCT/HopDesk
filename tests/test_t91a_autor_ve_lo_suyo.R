# =============================================================================
# tests/test_t91a_autor_ve_lo_suyo.R
# T91a: every user always sees what they published, even outside its
# audience (Luis asked "tesoreria" for something and could not see it), and
# only moderators take down someone else's.
#   - avisos_visibles(propios = TRUE): own publication, solicitud (one row
#     per solicitud, with how many recipients marked it done) and note
#     aviso, marked propia; never the automatic ones in the user's name;
#     only this client's; propios = FALSE (the weekly email) unchanged;
#   - Panorama: "Para mí" / "Que pedí"; the Clima does not count one's own;
#   - can_moderate_panorama: dev, admin and staff by default, not finance;
#     publicadas_retirables(): finance takes down its own, never another's;
#   - notes: nota_es_autor() without the "both without a code" case, and
#     the edit/delete observers check it on the server.
#
# Run from project root:  Rscript tests/test_t91a_autor_ve_lo_suyo.R
# =============================================================================
cat("=== test_t91a_autor_ve_lo_suyo ===\n\n")
source("tests/helpers/s3_memoria.R")
suppressPackageStartupMessages(library(shiny))
.i18n_dir <- file.path(getwd(), "www", "i18n")
for (f in c("R/i18n.R", "R/monedas.R", "R/tier_registry.R", "R/permisos.R", "R/avisos.R", "R/agenda_filas.R",
            "R/panorama_datos.R", "R/panorama_publicar_module.R"))
  suppressMessages(source(f))
for (e in parse("R/notes_handlers.R", keep.source = FALSE))
  if (is.call(e) && identical(as.character(e[[1]]), "<-") && identical(as.character(e[[2]]), "nota_es_autor"))
    eval(e, globalenv())

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
CID <- "acme"
DEV <- niveles_cliente()[1]; ADMIN <- niveles_cliente()[2]
FIN <- setdiff(niveles_cliente(), c(DEV, ADMIN))[1]
STAFF <- niveles_staff()[length(niveles_staff())]
usr <- function(user, tier) list(user = user, tier = tier, permisos = permisos_resueltos(tier))
LUIS <- usr("luis", DEV); TESO <- usr("tesoreria", FIN); OTRO <- usr("otro", DEV)

cat("-- el caso de Luis: una solicitud solo a tesoreria --\n")
s3_memoria_reset()
avisos_personales(CID, "tesoreria", list(tipo = "personal", clase = "solicitud", titulo = "Mándame el estado de cuenta",
                                         cuerpo = "", severidad = "info", clave = "sol:abc", autor = "luis"))
v <- avisos_visibles(CID, LUIS, propios = TRUE)
ok("Luis ve su solicitud: una fila, marcada propia, 'Pendiente' (0 de 1)",
   nrow(v) == 1L && isTRUE(v$propia) && identical(v$sol_atendidas, 0L) && identical(v$sol_total, 1L))
ok("no es un pendiente suyo: no entra en 'Para mí'; sí en 'Que pedí'",
   !nrow(panorama_mis_solicitudes(v, "luis")) && nrow(panorama_solicitudes_pedidas(v)) == 1L)
ok("tesoreria la ve en 'Para mí', como siempre",
   nrow(panorama_mis_solicitudes(avisos_visibles(CID, TESO, propios = TRUE), "tesoreria")) == 1L)
avisos_marcar_hecho(CID, avisos_leer(CID)$id[1], "tesoreria")
ok("cuando tesoreria la marca hecha, Luis la ve 'Atendida por 1 de 1'",
   identical(avisos_visibles(CID, LUIS, propios = TRUE)$sol_atendidas, 1L))
ok("el correo semanal (propios = FALSE) no cambia: Luis no recibe su propia solicitud",
   !nrow(avisos_visibles(CID, LUIS)))
ok("otro usuario del mismo nivel no la ve (no es suya ni va para él)",
   !nrow(avisos_visibles(CID, OTRO, propios = TRUE)))

cat("\n-- publicación, nota, automático, otro cliente --\n")
s3_memoria_reset()
avisos_agregar(CID, list(tipo = "publicado", clase = "comunicado", titulo = "Cierre de mes", cuerpo = "",
                         severidad = "info", niveles = c(FIN)), por = "luis")
ok("una publicación para niveles que no incluyen el suyo: el autor la ve, marcada propia",
   { x <- avisos_visibles(CID, LUIS, propios = TRUE); nrow(x) == 1L && isTRUE(x$propia) })
ok("…y quien sí está en la audiencia la ve sin marca", {
  x <- avisos_visibles(CID, TESO, propios = TRUE); nrow(x) == 1L && !isTRUE(x$propia) })
avisos_agregar(CID, list(tipo = "automatico", titulo = "luis confirmó un pago", cuerpo = "", severidad = "info",
                         niveles = c(FIN)), por = "luis")
ok("un automático escrito en su nombre (actividad) NO cuenta como suyo",
   nrow(avisos_visibles(CID, LUIS, propios = TRUE)) == 1L)
avisos_agregar(CID, list(tipo = "automatico", titulo = "Nota de staff", cuerpo = "", severidad = "info",
                         niveles = niveles_cliente(), clave = "nota|n1"), por = "mouse")
ok("la nota pública de un staff (audiencia: niveles de cliente): su autor staff la ve, propia",
   { x <- avisos_visibles(CID, usr("mouse", STAFF), propios = TRUE)
     any(x$clave %in% "nota|n1" & x$propia %in% TRUE) })
ok("…y Luis (dev, en su audiencia) la ve, pero NO marcada como suya: solo lo de su autor es 'Tuya'",
   { x <- avisos_visibles(CID, LUIS, propios = TRUE)
     any(x$clave %in% "nota|n1") && !any(x$clave %in% "nota|n1" & x$propia %in% TRUE) })
avisos_agregar("beta", list(tipo = "publicado", clase = "comunicado", titulo = "En beta", cuerpo = "",
                            severidad = "info", niveles = c(FIN)), por = "luis")
ok("lo que publicó en otro cliente no aparece en este (cada cliente su avisos.rds)",
   !any(avisos_visibles(CID, LUIS, propios = TRUE)$titulo %in% "En beta"))
ok("una publicación retirada ya no se ve, ni para su autor", {
  id <- avisos_leer(CID)$id[avisos_leer(CID)$titulo %in% "Cierre de mes"]
  avisos_retirar(CID, id, "luis"); !any(avisos_visibles(CID, LUIS, propios = TRUE)$titulo %in% "Cierre de mes")
})

cat("\n-- moderar: retirar lo de otros --\n")
ok("can_moderate_panorama de serie: dev, admin y staff sí; finanzas no",
   puede(usr("a", DEV), "can_moderate_panorama") && puede(usr("a", ADMIN), "can_moderate_panorama") &&
     all(vapply(niveles_staff(), function(t) puede(usr("a", t), "can_moderate_panorama"), logical(1))) &&
     !puede(usr("a", FIN), "can_moderate_panorama"))
filas <- data.frame(id = c("p1", "p2"), titulo = c("mía", "ajena"), por = c("tesoreria", "luis"),
                    ids = c("p1", "p2"), stringsAsFactors = FALSE)
r <- publicadas_retirables(filas, TESO)
ok("finanzas pide retirar la suya y una ajena: solo pasa la suya, 1 rechazada",
   identical(r$filas$id, "p1") && identical(r$rechazadas, 1L))
ok("un moderador (dev) retira ambas", nrow(publicadas_retirables(filas, LUIS)$filas) == 2L)
ok("el observer de Desactivar usa publicadas_retirables() (el servidor revisa, no la pantalla)", {
  x <- sub("#.*$", "", readLines("R/panorama_publicar_module.R", warn = FALSE, encoding = "UTF-8"))
  any(grepl("r <- publicadas_retirables(filas, u)", x, fixed = TRUE))
})

cat("\n-- notas: autoría en el servidor --\n")
nota <- function(code, name) data.frame(author_code = code, author = name, stringsAsFactors = FALSE)
ok("con código, manda el código", nota_es_autor(nota("U1", "Ana"), "U1", "Otra") && !nota_es_autor(nota("U1", "Ana"), "U2", "Ana"))
ok("sin código en alguno, el nombre", nota_es_autor(nota("", "Ana"), "U9", "Ana") && !nota_es_autor(nota("", "Ana"), "", "Beto"))
ok("los dos sin código y sin nombre: NO es autor (el hueco de la pantalla)",
   !nota_es_autor(nota("", ""), "", "") && !nota_es_autor(nota(NA, NA), "", "unknown"))
ok("editar, borrar y confirmar revisan la autoría en el servidor", {
  x <- readLines("R/notes_handlers.R", warn = FALSE, encoding = "UTF-8")
  sum(grepl(".nota_puede(", x, fixed = TRUE)) >= 4L &&
    any(grepl('puede(shared$usuario(), "can_moderate_panorama")', x, fixed = TRUE))
})

cat("\n-- Panorama de Luis (servidor real) --\n")
for (f in c("R/ui_components.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/avisos_alertas_module.R", "R/panorama_module.R"))
  suppressMessages(source(f))
source("tests/helpers/ic_filtro_piezas.R")
s3_memoria_reset()
avisos_personales(CID, "tesoreria", list(tipo = "personal", clase = "solicitud", titulo = "Mándame el estado de cuenta",
                                         cuerpo = "", severidad = "urgente", clave = "sol:abc", autor = "luis"))
pintar <- function(u) {
  shared <- list(effective_client_id = function() CID, usuario = function() u,
                 current_user_info = function() list(user = u$user, name = u$user, is_staff = FALSE),
                 tier_efectivo = function() u$tier, pagar_hoy_db = function() NULL, pasivos_provisions_db = function() NULL,
                 sap_snapshot_info = function() list(), fx_fix = function() NULL,
                 moneda_base_cliente = function() list(moneda = "MXN"))
  out <- NULL
  testServer(panoramaServer, args = list(shared = shared, activa = shiny::reactive(TRUE)), {
    session$flushReact()
    out <<- list(sol = as.character(output$solicitudes$html), feed = as.character(output$feed$html),
                 clima = as.character(output$clima$html))
  })
  out
}
pl <- pintar(LUIS)
tiene <- function(x, s) grepl(s, x %||% "", fixed = TRUE)
ok("la tarjeta: 'Solicitudes' con 'Que pedí', 'Para tesoreria · Pendiente', sin botón Hecho ni 'Para mí'",
   tiene(pl$sol, "Solicitudes") && tiene(pl$sol, "Que pedí") && tiene(pl$sol, "Para tesoreria · Pendiente") &&
     !tiene(pl$sol, "Para mí") && !tiene(pl$sol, ">Hecho<"))
ok("el feed la muestra marcada 'Tuya'", tiene(pl$feed, "Mándame el estado de cuenta") && tiene(pl$feed, "Tuya"))
ok("el Clima no la cuenta: ni urgente ni solicitud por atender (es suya, aunque sea urgente)",
   !tiene(pl$clima, "1 aviso urgente") && !tiene(pl$clima, "1 solicitud"))
pt <- pintar(TESO)
ok("tesoreria: 'Para mí' con su botón Hecho, sin 'Que pedí'",
   tiene(pt$sol, "Para mí") && tiene(pt$sol, "Hecho") && !tiene(pt$sol, "Que pedí"))

s3_memoria_restaurar()
cat(sprintf("\nTOTAL: %d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
