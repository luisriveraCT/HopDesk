# =============================================================================
# tests/test_t87_catalogo.R
# T87 paso 2: the wall between HopDesk staff and a client, in what the log
# WRITES (R/app_audit.R: ACCIONES_STAFF and log_action()). The integrator's
# conditions (2026-11-24), one section each:
#   1. hd-admin keeps every staff row whole -- real user, full action and
#      metadata (+ home_client, actor_code) -- for every class S, N, A, C.
#   2. An S row never reaches the client's folder; a C row reaches it signed
#      "hd-staff:<code>" with the first name, and the staff's user name (and
#      full name) is in no field of anything written to the client.
#   3. A jump's exit is logged for the client LEFT, not the new one (X -> Y,
#      and back home); nothing on a jump to the same client.
#   4. The scanner: every action written anywhere (log_action, the T72
#      events' accion, evento_sistema, "<accion>/parte", sin_evento) is
#      classified -- S/N/A in the catalog or a known C.
#   5. The staff directory: read with a short TTL and invalidated; a failed
#      read leaves "hd-staff" and never stops the log.
#   6. log_action()'s signature is unchanged; a client user's own row and a
#      staff row at home are exactly as before.
#   7. Rows already in a client's log are never rewritten.
#   8. A write by event without a home, in a staff session, is staff's too.
# S3 is tests/helpers/s3_memoria.R (restored at the end).
# Run from project root:  Rscript --vanilla tests/test_t87_catalogo.R
# =============================================================================
cat("=== test_t87_catalogo ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(tibble) })
source("tests/helpers/s3_memoria.R")
`%||%` <- function(a, b) if (is.null(a)) b else a
suppressMessages({ source("R/i18n.R"); source("R/app_audit.R") })
source("tests/helpers/acciones_escaner.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, cond, detalle = NULL) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else {
    cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L
    if (length(detalle)) cat("       ", paste(utils::head(detalle, 5), collapse = "\n        "), "\n")
  }
}

STAFF <- list(user = "zz_canario_staff", nombre = "Zelda Kanaryx Qwertzil", code = "S9999", primer = "Zelda")
.directorio_falla <- FALSE
.directorio_lecturas <- 0L
auth_load_usuarios <- function(client_id = NULL) {
  .directorio_lecturas <<- .directorio_lecturas + 1L
  if (.directorio_falla) stop("simulada")
  stopifnot(identical(client_id, "hd-admin"))
  data.frame(username = c(STAFF$user, "mouse"), display_name = c(STAFF$nombre, "Mouse Rivera"),
             account_code = c(STAFF$code, "S0001"), tier = c("hopdesk", "principal"), stringsAsFactors = FALSE)
}
log_de <- function(cid) {
  .audit_flush_ahora()
  x <- tryCatch(read_audit_log(cid), error = function(e) NULL)
  if (is.null(x)) .APP_AUDIT_SCHEMA() else x
}
texto <- function(df) paste(unlist(lapply(df, as.character)), collapse = " | ")
staff_en <- function(cid, accion, ...) log_action(STAFF$user, "pruebas", accion, ..., client_id = cid,
                                                  viewer_home_client_id = "hd-admin")

# Something already in acme's log before T87 (condition 7).
log_action(STAFF$user, "notes", "eliminar_nota", "vieja, de antes", client_id = "acme", viewer_home_client_id = "acme")
viejo <- log_de("acme")

cat("== 1/2. Cada clase ==\n")
staff_en("acme", "eliminar_nota", sprintf("Nota eliminada por %s (%s)", STAFF$user, STAFF$nombre),
         target_id = "nota-1", metadata = list(ledger = "AR", quien = STAFF$user))
staff_en("acme", "ver_como", sprintf("%s (como Admin)", STAFF$user), metadata = list(nivel = "admin"))
staff_en("acme", "cambio_limite_usuarios", sprintf("Límite cambiado de 5 a 4 por %s", STAFF$user),
         metadata = list(old_max_users = 5L, new_max_users = 4L, changed_by = STAFF$user, deactivated_count = 1L))
staff_en("acme", "user_deactivated_by_limit_reduction", "Cuenta 'caro' desactivada por reducción de límite en 'acme'",
         target_id = "U-caro", metadata = list(nombre = "Caro Díaz"))
staff_en("acme", "confirmar_pago_fallido", "fallo", metadata = list(x = 1))
acme  <- log_de("acme")
casa  <- log_de("hd-admin")
nuevo <- acme[!acme$id %in% viejo$id, , drop = FALSE]
de_casa <- function(a) casa[casa$action == a, , drop = FALSE]
de_acme <- function(a) nuevo[nuevo$action == a, , drop = FALSE]

ok("1.1 hd-admin guarda todas, con el usuario real",
   all(vapply(c("eliminar_nota", "ver_como", "cambio_limite_usuarios", "user_deactivated_by_limit_reduction",
                "confirmar_pago_fallido"), function(a) any(de_casa(a)$user == STAFF$user), logical(1))))
mc <- jsonlite::fromJSON(de_casa("cambio_limite_usuarios")$metadata[1])
ok("1.2 hd-admin conserva la metadata completa, más home_client y actor_code",
   identical(mc$changed_by, STAFF$user) && identical(mc$home_client, "hd-admin") && identical(mc$actor_code, STAFF$code))
ok("1.3 hd-admin conserva la descripción original", grepl(STAFF$user, de_casa("eliminar_nota")$description[1], fixed = TRUE))

ok("2.1 S: ver_como no se escribe en el cliente", !nrow(de_acme("ver_como")))
c1 <- de_acme("eliminar_nota")
ok("2.2 C: llega firmada hd-staff:<código>", nrow(c1) == 1L && identical(c1$user, paste0("hd-staff:", STAFF$code)))
ok("2.3 C: con el primer nombre en la metadata", identical(jsonlite::fromJSON(c1$metadata)$actor_primer, STAFF$primer))
ok("2.4 C: sin home_client", !grepl("home_client", c1$metadata, fixed = TRUE))
ok("2.5 el usuario y el nombre completo del staff no están en NADA de lo escrito al cliente",
   !grepl(STAFF$user, texto(nuevo), fixed = TRUE) && !grepl(STAFF$nombre, texto(nuevo), fixed = TRUE) &&
     !grepl("hd-admin", texto(nuevo), fixed = TRUE))
n1 <- de_acme("cambio_limite_usuarios")
ok("2.6 N: la suscripción llega como 'hopdesk', en palabras neutras",
   nrow(n1) == 1L && identical(n1$user, "hopdesk") &&
     identical(n1$description, "El límite de usuarios cambió de 5 a 4"))
ok("2.7 N: sin quién lo hizo ni la razón interna",
   !grepl("changed_by|deactivated_count", n1$metadata))
n2 <- de_acme("user_deactivated_by_limit_reduction")
ok("2.8 N: la cuenta desactivada, por su nombre",
   identical(n2$description, "La cuenta de Caro Díaz se desactivó por ajuste del plan de usuarios") &&
     identical(n2$user, "hopdesk"))
ok("2.9 '<accion>_fallido' toma la clase de su acción (C, firmada)",
   identical(de_acme("confirmar_pago_fallido")$user, paste0("hd-staff:", STAFF$code)))

cat("== 3. Entrada y salida ==\n")
t87_registrar_salto(NULL, "acme", STAFF$user, "hd-admin")
t87_registrar_salto("acme", "beta", STAFF$user, "hd-admin")
t87_registrar_salto("beta", NULL, STAFF$user, "hd-admin")
t87_registrar_salto("beta", "beta", STAFF$user, "hd-admin")
acme <- log_de("acme"); beta <- log_de("beta"); casa <- log_de("hd-admin")
ok("3.1 entrar a acme: su línea en acme, firmada y neutral",
   any(acme$action == "acceso_staff_entrada" & acme$user == paste0("hd-staff:", STAFF$code) &
         acme$description == "Entró a los datos de la empresa"))
sal_a <- acme[acme$action == "acceso_staff_salida", , drop = FALSE]
ok("3.2 X -> Y: la salida va al cliente que se DEJA (acme), con motivo 'cambio'",
   nrow(sal_a) == 1L && identical(jsonlite::fromJSON(sal_a$metadata)$motivo, "cambio"))
ok("3.3 ...y la entrada al nuevo (beta)", sum(beta$action == "acceso_staff_entrada") == 1L)
sal_b <- beta[beta$action == "acceso_staff_salida", , drop = FALSE]
ok("3.4 regreso a casa: la salida va a beta, con motivo 'volver'",
   nrow(sal_b) == 1L && identical(jsonlite::fromJSON(sal_b$metadata)$motivo, "volver"))
ok("3.5 un salto al mismo cliente no deja nada",
   sum(beta$action == "acceso_staff_entrada") == 1L && sum(beta$action == "acceso_staff_salida") == 1L)
ok("3.6 hd-admin tiene cada entrada y salida, con el usuario real",
   sum(casa$action == "acceso_staff_entrada" & casa$user == STAFF$user) == 2L &&
     sum(casa$action == "acceso_staff_salida" & casa$user == STAFF$user) == 2L)
app <- readLines("app.R", warn = FALSE, encoding = "UTF-8")
ok("3.7 app.R le pasa el valor ANTERIOR del salto y lo actualiza después",
   any(grepl("t87_registrar_salto(.salto_anterior, jcid", app, fixed = TRUE)) &&
     which(grepl("t87_registrar_salto(.salto_anterior, jcid", app, fixed = TRUE))[1] <
       which(grepl(".salto_anterior <<- jcid", app, fixed = TRUE))[1])
ok("3.8 cerrar sesión dentro de un cliente registra la salida ('cierre'), solo si estaba dentro",
   any(grepl('if (!is.null(dentro)) t87_registrar_salto(dentro, NULL', app, fixed = TRUE)))

cat("== 4. El escáner ==\n")
C_CONOCIDAS <- c(
  "_conflict_replay", "_queue_bucket_mismatch", "_queue_corrupt_payload", "_queue_red_refused",
  "_queue_stale_broadcast", "_queue_superseded", "_queue_tenant_mismatch", "activacion_usuario",
  "agregar_item", "agregar_item_fallido", "agregar_seleccion", "agregar_todo", "aplicar_filtros",
  "archivar_manual_fallo_guardado", "cargar_saldos_bancos", "confirmar_cobro", "confirmar_cobro_incompleto",
  "confirmar_pago", "confirmar_pago_incompleto", "confirmar_renombre", "convertir", "crear_empresa",
  "crear_suscripcion_global", "crear_usuario", "crudos", "datos_fallido", "desactivar_empresa",
  "deshacer_abono", "desvincular_proveedor", "editar_empresa", "editar_factura_sap",
  "editar_factura_sap_fallido", "editar_item_fallido", "editar_saldo_apertura", "editar_suscripcion",
  "editar_usuario", "eliminar", "eliminar_nota", "eliminar_usuario", "enviar_a_agenda_dia",
  "enviar_a_agenda_dia_incompleto", "enviar_a_agenda_ic_incompleto", "enviar_abonos_a_agenda",
  "enviar_invitacion", "enviar_ruta_a_agenda", "escritura_no_permitida", "etiqueta_a", "etiqueta_b",
  "etiqueta_c", "exportar_actividad_pdf", "exportar_cashflow_excel", "exportar_cashflow_pdf",
  "exportar_cashflow_word", "exportar_pagos_pdf", "fx_fix", "generar_archivo_pago", "generar_archivo_pago_zip",
  "guardar_config_grupo", "guardar_preferencias", "guardar_tier_config", "importar_proveedores",
  "importar_proveedores_fallido", "inactivar_proveedores_fallido", "inactivar_sel", "incluir_sin_cuenta",
  "invitacion_vencida_eliminada", "login", "logout", "mover", "papelera", "provisiones_refrescadas",
  "quitar_agenda_manual_fallo_guardado", "quitar_cobros", "quitar_cobros_incompleto", "quitar_de_agenda_dia",
  "quitar_de_agenda_dia_incompleto", "quitar_etiqueta", "quitar_pagos", "quitar_pagos_incompleto",
  "recuperar_papelera_fallo_guardado", "refrescar_metrica", "refrescar_todas_metricas", "reintentar_todo",
  "renombrar_iniciales", "reservar_renombre", "restaurar", "revertir_confirmacion", "revocar_invitacion",
  "sembrar_catalogo", "solicitar_usuarios", "vaciar_agenda", "vincular_proveedor")
todas <- acciones_en_codigo(".")
ok("4.0 el escáner ve las acciones de todas las fuentes (log_action, eventos, /parte, sin_evento)",
   all(c("eliminar_nota", "provisiones_refrescadas", "confirmar_pago/parte", "sin_evento", "ver_como") %in% todas))
sin_clase <- todas[vapply(todas, t87_clase_accion, "") == "C" & !todas %in% C_CONOCIDAS]
ok("4.1 ninguna acción sin clasificar (S/N/A en ACCIONES_STAFF, o C conocida)", !length(sin_clase),
   paste("clasifícala en ACCIONES_STAFF o, si un usuario del cliente también la hace, en C_CONOCIDAS:", sin_clase))
ok("4.2 '/parte' y sin_evento son S", t87_clase_accion("confirmar_pago/parte") == "S" && t87_clase_accion("sin_evento") == "S")
ok("4.3 C_CONOCIDAS no esconde una acción que el catálogo ya clasifica",
   !length(intersect(C_CONOCIDAS, names(ACCIONES_STAFF))))

cat("== 5. El directorio staff ==\n")
t87_directorio_invalidar(); n0 <- .directorio_lecturas
invisible(t87_staff_firma(STAFF$user)); invisible(t87_staff_firma(STAFF$user))
ok("5.1 se lee una vez dentro del TTL", .directorio_lecturas - n0 == 1L)
t87_directorio_invalidar(); invisible(t87_staff_firma(STAFF$user))
ok("5.2 invalidarlo hace leer de nuevo", .directorio_lecturas - n0 == 2L)
ok("5.3 el TTL es corto (minutos)", .T87_DIRECTORIO_TTL <= 300)
t87_directorio_invalidar(); .directorio_falla <- TRUE
r <- tryCatch({ staff_en("gamma", "eliminar_nota", sprintf("de %s", STAFF$user)); TRUE }, error = function(e) FALSE)
.directorio_falla <- FALSE
g <- log_de("gamma")
ok("5.4 una lectura fallida no para el log", r && nrow(g) == 1L)
ok("5.5 ...y deja 'hd-staff', nunca el usuario", identical(g$user, "hd-staff") && !grepl(STAFF$user, texto(g), fixed = TRUE))
t87_directorio_invalidar()
log_action("quien_sabe", "notes", "eliminar_nota", "de quien_sabe", client_id = "delta", viewer_home_client_id = "hd-admin")
d <- log_de("delta")
ok("5.6 un staff que no está en el directorio sale 'hd-staff', sin su usuario",
   any(d$user == "hd-staff") && !grepl("quien_sabe", texto(d), fixed = TRUE))

cat("== 6. Lo demás, igual que antes ==\n")
ok("6.1 la firma de log_action no cambia",
   identical(names(formals(log_action)), c("user", "module", "action", "description", "target_id", "s3_key",
                                           "metadata", "client_id", "viewer_home_client_id")))
casa0 <- nrow(log_de("hd-admin"))
log_action("ana", "notes", "eliminar_nota", "de ana", metadata = list(a = 1), client_id = "acme", viewer_home_client_id = "acme")
acme <- log_de("acme"); fila <- acme[acme$description == "de ana", , drop = FALSE]
ok("6.2 la línea de un usuario del cliente queda igual (su usuario, su metadata)",
   nrow(fila) == 1L && identical(fila$user, "ana") && identical(fila$metadata, '{"a":1}'))
ok("6.3 ...y no llega a hd-admin", nrow(log_de("hd-admin")) == casa0)
log_action(STAFF$user, "tiers", "ver_como", "en casa", client_id = "hd-admin", viewer_home_client_id = "hd-admin")
casa <- log_de("hd-admin")
ok("6.4 el staff en su casa escribe como siempre", any(casa$description == "en casa" & casa$user == STAFF$user))

cat("== 7. Lo ya escrito no se toca ==\n")
acme <- log_de("acme")
ok("7.1 la fila vieja del cliente sigue idéntica", identical(acme[acme$id %in% viejo$id, names(viejo)], viejo[, names(viejo)]))

cat("== 8. Escritura por evento sin casa, en una sesión de staff ==\n")
s <- MockShinySession$new(); s$userData$hd_cliente_casa <- function() "hd-admin"
withReactiveDomain(s, log_action(STAFF$user, "bancos", "confirmar_pago", "por evento", client_id = "omega",
                                 viewer_home_client_id = "omega"))
o <- log_de("omega"); casa <- log_de("hd-admin")
ok("8.1 llega a hd-admin aunque el evento no trajera casa", any(casa$description == "por evento" & casa$user == STAFF$user))
ok("8.2 y al cliente, firmada", identical(o$user, paste0("hd-staff:", STAFF$code)))
s2 <- MockShinySession$new(); s2$userData$hd_cliente_casa <- function() "omega"
withReactiveDomain(s2, log_action("ana", "bancos", "confirmar_pago", "nativa", client_id = "omega", viewer_home_client_id = "omega"))
o <- log_de("omega")
ok("8.3 una sesión del cliente sigue siendo del cliente", any(o$description == "nativa" & o$user == "ana"))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
