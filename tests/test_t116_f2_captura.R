# =============================================================================
# tests/test_t116_f2_captura.R
# T116 F2: «Calendario de mercado» is HopDesk staff only.
#   1  can_capture_market_calendar is internal: every client tier, with EVERY
#      key switched on (its own config and the user's), still does not hold
#      it, nor sees it among the keys of Usuarios (perm_visible_keys);
#      principal and hopdesk do
#   2  the section is NOT in a client's DOM: the Configuración window a
#      client gets has no button and no text of it; staff's has it
#   3  on the real observers (testServer, in-memory S3): a client sending the
#      inputs by hand writes nothing and gets no panel; staff adds a day,
#      confirms the year and voids it, each write with its T72 event
#  4  nothing of it travels to a client's browser: no hdt-datos of the table
#     (HD_ARCHIVOS_PERMISO), and a client pressing the buttons by hand gets
#     no notification at all (never the permission's name)
#
# Run from project root:  Rscript tests/test_t116_f2_captura.R
# =============================================================================
cat("=== test_t116_f2_captura ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr, detalle = NULL) {
  r <- tryCatch(isTRUE(expr), error = function(e) { message("  error: ", conditionMessage(e)); FALSE })
  if (r) { .pass <<- .pass + 1L; cat(" PASS:", label, "\n") }
  else   { .fail <<- .fail + 1L; cat(" FAIL:", label, "\n"); if (!is.null(detalle)) cat("       ", detalle, "\n") }
}
suppressPackageStartupMessages(library(shiny))
source("tests/helpers/s3_memoria.R")
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R", "R/policy_engine.R", "R/mercado.R", "R/ui_components.R",
            "R/hd_registro.R", "R/hd_piezas.R", "R/monedas.R", "R/zona.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/app_audit.R", "R/settings/settings_mercado.R", "R/settings/settings_hub.R"))
  suppressMessages(sys.source(f, envir = globalenv()))
q <- function(expr) invisible(suppressWarnings(suppressMessages(expr)))
K <- "can_capture_market_calendar"
todo <- as.list(stats::setNames(rep(TRUE, length(permisos_claves())), permisos_claves()))
usuario_de <- function(t, todos = FALSE) {
  cfg <- if (todos) stats::setNames(list(todo), t) else NULL
  list(user = paste0("u.", t), tier = t, is_staff = is_staff_tier(t),
       permisos = permisos_resueltos(t, if (todos) jsonlite::toJSON(todo, auto_unbox = TRUE) else "{}", cfg))
}

cat("== 1. El permiso es interno ==\n")
clientes <- niveles_cliente()
ok("1.1 ningún nivel cliente lo tiene, ni con TODAS las llaves prendidas (config y usuario)",
   length(clientes) > 0L && !any(vapply(clientes, function(t) puede(usuario_de(t, todos = TRUE), K), logical(1))))
ok("1.2 un cliente no ve la llave entre las de Usuarios (perm_visible_keys fail-closed)",
   !K %in% perm_visible_keys(FALSE) && K %in% perm_visible_keys(TRUE) && permiso_interno(K))
ok("1.3 principal y hopdesk sí", puede(usuario_de("principal"), K) && puede(usuario_de("hopdesk"), K))

cat("== 2. No está en el DOM de un cliente ==\n")
modal_html <- function(u) {
  capturado <- NULL
  assign("showModal", function(ui, ...) capturado <<- ui, envir = globalenv())
  shared <- list(usuario = function() u)
  out <- list()
  show_settings_modal(list(), out, NULL, shared)
  paste(as.character(capturado), collapse = "")
}
txt_boton <- tr("tablas.ajustes.mercado.boton")
for (t in clientes) {
  h <- modal_html(usuario_de(t, todos = TRUE))
  ok(sprintf("2.1 %s con todo: la ventana no tiene ni el botón ni su texto", t),
     nzchar(h) && !grepl("stg_btn_mercado", h, fixed = TRUE) && !grepl(txt_boton, h, fixed = TRUE))
}
hs <- modal_html(usuario_de("hopdesk"))
ok("2.2 staff (hopdesk): sí está, bajo Admin", grepl("stg_btn_mercado", hs, fixed = TRUE) && grepl(txt_boton, hs, fixed = TRUE))
ok("2.3 mercado_boton_hub() de un cliente es NULL (no se esconde: no existe)",
   is.null(mercado_boton_hub(usuario_de("admin", todos = TRUE))))

cat("== 3. Los observers reales ==\n")
assign("showNotification", function(...) invisible("id"), envir = globalenv())
correr <- function(u, clics) {
  shared <- list(usuario = reactiveVal(u), current_user = function() u$user,
                 home_client_id = function() "hd-admin", effective_client_id = function() "hd-admin")
  testServer(function(input, output, session) settings_mercado_observer(input, output, session, shared),
             { session$flushReact(); clics(session, output) })
}
s3_memoria_reset(); mercado_inhabiles_poner(NULL); .MERCADO_INHABILES$intento_en <- NULL
n0 <- .mem_escrituras
panel <- NULL
q(correr(usuario_de("admin", todos = TRUE), function(session, output) {
  session$setInputs(stg_btn_mercado = 1)
  panel <<- tryCatch(output$settings_panel, error = function(e) NULL)
  session$setInputs(mer_pais = "MX", mer_anio = 2027, mer_fecha = as.Date("2027-11-02"),
                    mer_motivo = "Día de Muertos", mer_fuente = "prueba")
  session$setInputs(mer_agregar = 1); session$setInputs(mer_confirmar = 1); session$setInputs(mer_desconfirmar = 1)
}))
ok("3.1 un cliente con todo, mandando los inputs a mano: no escribe nada", .mem_escrituras == n0 &&
     !length(grep("mercado", ls(.mem))))
ok("3.2 ...y no recibe el panel", is.null(panel) || !grepl("mercado_t", paste(unlist(panel), collapse = ""), fixed = TRUE))
ok("3.2b ...y ninguna tabla falló (datos_fallido)", !"datos_fallido" %in% c(
   unlist(lapply(ls(.audit_pending), function(k) .audit_pending[[k]]$action)),
   unlist(lapply(grep("app_audit", ls(.mem), value = TRUE), function(k) .mem[[k]]$action))))

eventos <- function() { k <- grep("app_audit|audit", ls(.mem), value = TRUE); k }
q(correr(usuario_de("hopdesk"), function(session, output) {
  session$setInputs(stg_btn_mercado = 1)
  panel <<- output$settings_panel
  session$setInputs(mer_pais = "MX", mer_anio = 2027, mer_fecha = as.Date("2027-11-02"),
                    mer_motivo = "Día de Muertos", mer_fuente = "prueba")
  session$setInputs(mer_agregar = 1)
  session$setInputs(mer_confirmar = 1)
}))
llave <- "hd-admin/mercado/inhabiles.rds"
t1 <- .mem[[llave]]
ok("3.3 staff: el panel se dibuja", grepl("mercado_t", paste(unlist(panel), collapse = ""), fixed = TRUE))
ok("3.4 staff: agrega el día y confirma el año (dos filas, quién las capturó)",
   is.data.frame(t1) && nrow(t1) == 2L && setequal(t1$tipo, c("dia", "anio")) && all(t1$capturado_por == "u.hopdesk"))
ok("3.5 ...y get_holidays(MX, 2027) ya es la lista oficial", identical(get_holidays("MX", 2027), as.Date("2027-11-02")))
q(correr(usuario_de("hopdesk"), function(session, output) {
  session$setInputs(mer_pais = "MX", mer_anio = 2027)
  session$setInputs(mer_desconfirmar = 1)
}))
t2 <- .mem[[llave]]
ok("3.6 quitar la confirmación: la fila sigue, anulada por quien lo hizo; rige la regla",
   nrow(t2) == 2L && identical(t2$anulado_por[t2$tipo == "anio"], "u.hopdesk") &&
     length(get_holidays("MX", 2027)) > 1L)
# The rows may still be queued, or already flushed (later::later, ~2 s): both.
log_claves <- grep("app_audit", ls(.mem), value = TRUE)
acciones <- c(unlist(lapply(ls(.audit_pending), function(k) .audit_pending[[k]]$action)),
              unlist(lapply(log_claves, function(k) { x <- .mem[[k]]; if (is.data.frame(x)) x$action })))
ok("3.7 cada escritura dejó su evento T72 (agregar, confirmar, anular)",
   all(c("mercado_inhabil_agregar", "mercado_anio_confirmar", "mercado_anular") %in% acciones),
   paste("claves:", paste(log_claves, collapse = ", "), "| acciones:", paste(unique(acciones), collapse = ", ")))

cat("== 4. Al navegador de un cliente no viaja nada de la captura ==\n")
# What the session sends: hdt-* messages (rootScope's sendCustomMessage) and
# notifications. The table gate is app.R's: nivel_tabla_permitida_usuario()
# on the session's user (HD_ARCHIVOS_PERMISO).
viaja <- function(u, clics = function(session) NULL) {
  mensajes <- list(); avisos <- list()
  assign("showNotification", function(ui, ...) { avisos[[length(avisos) + 1L]] <<- paste(as.character(ui), collapse = ""); invisible("id") },
         envir = globalenv())
  shared <- list(usuario = reactiveVal(u), current_user = function() u$user,
                 home_client_id = function() "hd-admin", effective_client_id = function() "hd-admin")
  q(testServer(function(input, output, session) {
    rs <- session$rootScope()
    unlockBinding("sendCustomMessage", rs)
    rs$sendCustomMessage <- function(type, message) mensajes[[length(mensajes) + 1L]] <<- list(type = type, msg = message)
    # shiny::showNotification() (namespaced, as puede_o_avisa calls it) goes
    # around the global stub above: catch it where every notification leaves.
    for (s in unique(list(session, rs))) {
      unlockBinding("sendNotification", s)
      s$sendNotification <- function(type, message) avisos[[length(avisos) + 1L]] <<- paste(type, utils::capture.output(utils::str(message)), collapse = "")
    }
    session$userData$hd_tabla_permitida <- function(tabla) nivel_tabla_permitida_usuario(u, tabla)
    settings_mercado_observer(input, output, session, shared)
  }, { session$setInputs(mer_pais = "MX", mer_anio = 2027); session$flushReact(); clics(session) }))
  list(datos = Filter(function(m) identical(m$type, "hdt-datos") && identical(m$msg$tabla, "mercado_inhabiles"), mensajes),
       todo = paste(vapply(mensajes, function(m) paste(utils::capture.output(utils::str(m)), collapse = ""), ""), collapse = ""),
       avisos = unlist(avisos))
}
for (t in clientes) {
  v <- viaja(usuario_de(t, todos = TRUE))
  ok(sprintf("4.1 %s con todo: ningún mensaje de la tabla mercado_inhabiles", t),
     !length(v$datos) && !grepl("mercado_inhabiles|solo_oficial|solo_regla", v$todo))
}
vs <- viaja(usuario_de("hopdesk"))
ok("4.2 staff sí la recibe (la puerta no la apaga de más)", length(vs$datos) >= 1L)
ok("4.3 la puerta de la tabla: settings_mercado.R pide can_capture_market_calendar",
   identical(HD_ARCHIVOS_PERMISO[["settings_mercado.R"]], K) && identical(hd_registro$mercado_inhabiles$archivo, "settings_mercado.R"))
s3_memoria_reset()
vc <- viaja(usuario_de("dev", todos = TRUE), function(session) {
  session$setInputs(mer_fecha = as.Date("2027-11-02"), mer_motivo = "x")
  session$setInputs(mer_agregar = 1); session$setInputs(mer_anular = 1)
  session$setInputs(mer_confirmar = 1); session$setInputs(mer_desconfirmar = 1)
})
ok("4.4 un cliente que manda los botones a mano: ninguna notificación (ni el nombre del permiso)",
   !length(vc$avisos) && !length(grep("mercado", ls(.mem))), paste(vc$avisos, collapse = " | "))

mercado_inhabiles_poner(NULL)
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
