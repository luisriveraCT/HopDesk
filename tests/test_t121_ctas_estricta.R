# =============================================================================
# tests/test_t121_ctas_estricta.R
# T121 (A2's rule, found by ISO in T84): load_ctas_cuentas() swallowed a
# failed read and returned the empty catalog -- "no accounts" for a session
# whose sync reload failed, until the next change. Now:
#   1  load_ctas_cuentas(): a good read is the table; a missing file (S3
#      confirms it, or the 404 is already cached -- no HEAD then) is the empty
#      catalog; a failed read stop()s; something that is not a table stop()s
#   2  the sync bus's reload: a failed read leaves the session's last good
#      catalog (its loader's error is NULL, and NULL is never applied)
#   3  T104's .ph_cuenta_confirmacion(): an unreadable catalog is "lectura",
#      not "sin_cuenta"
#   4  Bancos (real bancosServer, testServer): the accounts not read -> the
#      Libro / Cuentas notice, the import's banner says so, cuentas() is an
#      empty table (no error); do_import with them unread writes nothing and
#      says to retry (never stops the session); S3 back -> no notice, imports
#   5  Ajustes > Cuentas (real settings_cuentas_observer): the notice above the
#      table, and opening an account says to retry instead of failing
# In-memory S3 (tests/helpers/s3_memoria.R): no real bucket.
# Run from project root:  Rscript tests/test_t121_ctas_estricta.R
# =============================================================================
cat("=== test_t121_ctas_estricta ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("tests/helpers/s3_memoria.R")
cargar <- function(archivo, nombres) {
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
}
cargar("R/erp_connector_registry.R", "SAP_B1_ERP_TYPE")
cargar("R/global.R", c("fmt_money", "is_erp_sourced"))
cargar("R/settings/settings_helpers.R", c(".schema_ctas_bancos", ".schema_ctas_cuentas", "load_ctas_bancos",
                                          "load_ctas_cuentas", "modificar_ctas_bancos", "modificar_ctas_cuentas",
                                          ".BANCOS_MX_CHOICES", "borrar_banco_si_libre",
                                          ".banco_en_uso_filas", ".banco_en_uso_texto"))
cargar("R/adjuntos_s3.R", c(".NUMERO_PURO", ".CELDA_PELIGRO", "neutralizar_celdas", "tabla_a_texto_copiable"))
cargar("R/ui_components.R", c("HD_NUMERO_FORMATOS", ".hd_numero_decimales", "hd_numero_texto", "hd_numero_deps",
                              "hd_numero_input", "hd_numero_update", "hd_numero_revisar"))
cargar("R/pagar_hoy_module.R", ".ph_cuenta_confirmacion")
cargar("R/pagos_audit_viewer_module.R", c(".cuenta_digitos", "cuenta_foto"))
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R", "R/hd_registro.R", "R/hd_piezas.R", "R/monedas.R",
            "R/hd_columnas.R", "R/hd_table.R", "R/settings/settings_cuentas.R",
            "R/bancos_parser.R", "R/empresas_identidad.R", "R/bancos_detect.R", "R/bancos_importadores.R",
            "R/bancos_persistence.R", "R/bancos_formatos.R", "R/bancos_module.R"))
  suppressMessages(source(f))

LOG <- character(0); AVISOS <- character(0)
log_action <- function(user, module, action, description = "", ...) LOG <<- c(LOG, action)
avisos_actividad <- function(...) invisible(NULL)
showNotification <- function(ui, ..., type = "default") { AVISOS <<- c(AVISOS, paste0(type, ": ", as.character(ui))); invisible("id") }
removeModal <- function(...) invisible(NULL)
showModal <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
puede <- function(...) TRUE
puede_o_avisa <- function(...) TRUE
q <- function(expr) suppressWarnings(suppressMessages(expr))
error_de <- function(expr) tryCatch({ q(expr); NULL }, error = function(e) e)

CID <- "acme"
KC  <- paste0(CID, "/", S3_KEYS$ctas_cuentas)
KB  <- paste0(CID, "/", S3_KEYS$ctas_bancos)
AVISO <- tr("comun.cuentas_no_leidas")
CTAS <- tibble::tibble(id = c("c1", "c2"), banco_id = "b1", Empresa = "NTS", razon_social = "NT SA", rfc = "NTS010101AA1",
                       Moneda = "MXN", alias = c("A1", "A2"), cuenta = c("73543010201", "73543010202"),
                       clabe_interbancaria = "", is_ppl_default = FALSE, saldo_inicial = 0, activa = TRUE)
BANCOS <- tibble::tibble(id = "b1", nombre = "BanBajío", clave = "030")
limpiar_caches <- function() for (env in list(.s3_preload_cache, .s3_missing_cache))
  rm(list = ls(env), envir = env)
sembrar <- function() { s3_memoria_reset(); limpiar_caches(); .mem_falla <<- character(); .mem_falla_cabeza <<- character()
  assign(KC, CTAS, envir = .mem); assign(KB, BANCOS, envir = .mem) }
falla <- function() { limpiar_caches(); .mem_falla <<- KC }

cat("== 1. load_ctas_cuentas() ==\n")
sembrar()
x <- q(load_ctas_cuentas(client_id = CID))
ok("1.1 lectura buena: la tabla, con sus filas", is.data.frame(x) && identical(sort(x$id), c("c1", "c2")))
s3_memoria_reset(); limpiar_caches()
x <- q(load_ctas_cuentas(client_id = CID))
ok("1.2 sin archivo (S3 dice 404): el catálogo vacío, con sus columnas",
   is.data.frame(x) && !nrow(x) && all(names(.schema_ctas_cuentas()) %in% names(x)))
.mem_falla_cabeza <- KC   # the 404 is cached by 1.2: a HEAD now would fail and stop()
x <- error_de(load_ctas_cuentas(client_id = CID))
ok("1.3 404 ya en caché: vacío sin preguntar otra vez a S3", is.null(x))
sembrar(); falla()
e <- error_de(load_ctas_cuentas(client_id = CID))
ok("1.4 lectura fallida: se detiene (nunca 'sin cuentas')", inherits(e, "error") && grepl("could not read", conditionMessage(e)))
.mem_falla_cabeza <- KC
e <- error_de(load_ctas_cuentas(client_id = CID))
ok("1.5 lectura y HEAD fallidos: se detiene", inherits(e, "error"))
sembrar(); assign(KC, list(no = "tabla"), envir = .mem)
ok("1.6 algo que no es tabla: se detiene", inherits(error_de(load_ctas_cuentas(client_id = CID)), "error"))

cat("== 2. La recarga del bus de sincronización ==\n")
# What sync_bus's poll does with a loader (R/sync_bus.R): tryCatch(loader,
# error = NULL), and only a non-NULL value is applied to the reactive.
sembrar()
rv <- reactiveVal(q(load_ctas_cuentas(client_id = CID)))
recargar <- function() { v <- tryCatch(q(load_ctas_cuentas(client_id = CID)), error = function(e) NULL)
  if (!is.null(v)) rv(v) }
falla(); isolate(recargar())
ok("2.1 recarga con la lectura fallida: la sesión conserva sus cuentas", identical(nrow(isolate(rv())), 2L))
sembrar(); assign(KC, CTAS[1, ], envir = .mem); isolate(recargar())
ok("2.2 recarga buena: aplica el cambio de la otra sesión", identical(nrow(isolate(rv())), 1L))

cat("== 3. T104: la cuenta de la confirmación ==\n")
sembrar(); falla()
sh <- list(effective_client_id = function() CID)   # no session catalog: reads S3
r <- q(.ph_cuenta_confirmacion(sh, "c1", "MXN"))
ok("3.1 catálogo ilegible: 'lectura' (reintentar), no 'sin_cuenta'", identical(r$error, "lectura"))
sembrar()
r <- q(.ph_cuenta_confirmacion(sh, "c1", "MXN"))
ok("3.2 catálogo legible: la foto de la cuenta (control)", !is.null(r$foto) && is.null(r$error))

cat("== 4. Bancos ==\n")
SES <- as.POSIXct("2026-11-20 10:00:00")
mov <- function(id, cargo = 100)
  tibble::tibble(id = id, cuenta_id = "c1", fecha = as.Date("2026-11-20"), hora = "10:00:00", recibo = id,
                 descripcion_raw = paste("Mov", id), tipo = "spei_out", parte = "PROV", rfc = NA_character_,
                 referencia = NA_character_, clave_rastreo = NA_character_, concepto = "c", cargo = cargo, abono = 0,
                 saldo_banco = 1000, conciliado = FALSE, doc_vinculado = NA_character_, agenda_id = NA_character_,
                 importado_at = SES, fuente = "txt", eliminado = FALSE, eliminado_por = NA_character_,
                 eliminado_at = as.POSIXct(NA), notas = NA_character_)
MK <- paste0(CID, "/", S3_KEYS$bancos_movimientos)
movs0 <- .normalize(mov("m1"), .schema_bancos_movimientos)
# A confirmation the imported movement would match (same amount and date, MXN):
# with the accounts read, it reconciles; unread, it must not.
CONF <- .normalize(tibble::tibble(confirmacion_id = "k1", agenda_item_id = "A1", documento = "F-1", moneda = "MXN",
                                  tipo = "pago", importe = 4321, fecha = as.Date("2026-11-20"), cuenta_id = "c1",
                                  eliminado = FALSE, confirmado_at = SES, parte = "PROV", empresa = "NTS"),
                   .schema_bancos_confirmados)
MOVS <- reactiveVal(movs0); CTAS_RV <- reactiveVal(NULL)
shared <- list(
  effective_client_id = function() CID, home_client_id = function() CID, current_user = function() "luis",
  usuario = reactive("luis"), company_map = reactiveVal(c(NTS = "Networks Trucking")),
  visible_initials = reactiveVal(NULL), bancos_movimientos = MOVS,
  bancos_confirmados = reactiveVal(CONF), conciliacion_rv = reactiveVal(tibble::tibble()),
  ctas_cuentas = CTAS_RV, papelera_rv = reactiveVal(NULL), abonos_db = reactiveVal(tibble::tibble()),
  manual_inv = reactiveVal(NULL), pasivos_provisions_db = reactiveVal(NULL), sap_data = reactiveVal(list()),
  empresas_db = reactiveVal(NULL), suppress_ledger_prov_refresh = reactiveVal(FALSE))
importar <- function(session, parsed_preview, recibo = "999001") {
  nuevo <- mov("TMP", cargo = 4321); nuevo$recibo <- recibo; nuevo$descripcion_raw <- paste("Mov", recibo); nuevo$id <- NULL
  parsed_preview(nuevo); session$setInputs(import_cuenta = "c1", do_import = runif(1)); session$flushReact()
}
sembrar(); assign(MK, movs0, envir = .mem); falla()
testServer(bancosServer, args = list(shared = shared), {
  session$flushReact()
  ok("4.1 cuentas sin leer (Fase 1 falló y S3 sigue fallando): marcadas como no leídas", isTRUE(cuentas_no_leidas()))
  ok("4.2 …el Libro y Cuentas llevan el aviso", identical(aviso_no_leidas(), AVISO))
  ok("4.3 …y cuentas() es una tabla vacía, no un error", is.data.frame(cuentas()) && !nrow(cuentas()))
  n0 <- .mem_escrituras; AVISOS <<- character()
  vivo <- tryCatch({ q(importar(session, parsed_preview)); TRUE }, error = function(e) { message("  do_import: ", conditionMessage(e)); FALSE })
  ok("4.4 do_import con las cuentas sin leer: no truena", vivo)
  # T121 (Coordinador): a cuenta_id nobody could validate is never saved.
  ok("4.5 …no importa nada: 0 escrituras, la sesión y S3 igual que antes",
     .mem_escrituras == n0 && identical(MOVS(), movs0) && identical(get(MK, envir = .mem), movs0))
  ok("4.5b …y dice que reintente antes de importar",
     any(grepl(tr("tablas.bancos.aviso.importar_sin_cuentas"), AVISOS, fixed = TRUE)))
  .mem_falla <<- character(); limpiar_caches()
  CTAS_RV(CTAS); session$flushReact()
  ok("4.6 con las cuentas leídas: sin aviso", !isTRUE(cuentas_no_leidas()) && is.null(aviso_no_leidas()) &&
       identical(sort(cuentas()$cuenta_id), c("c1", "c2")))
  q(importar(session, parsed_preview, recibo = "999002"))
  otro <- MOVS()[MOVS()$recibo %in% "999002", , drop = FALSE]
  ok("4.7 control: con las cuentas leídas, el mismo movimiento sí concilia (la guarda de 4.5 es la que detuvo la otra)",
     nrow(otro) == 1L && isTRUE(otro$conciliado) && identical(otro$agenda_id, "A1"))
})

cat("== 5. Ajustes > Cuentas ==\n")
ADMIN <- list(user = "admin", tier = nivel_ti(), permisos = permisos_resueltos(nivel_ti()))
sembrar(); falla(); AVISOS <- character()
sh5 <- list(usuario = reactiveVal(ADMIN), current_user = function() "admin",
            effective_client_id = function() CID, home_client_id = function() CID,
            company_map = reactive(c(NTS = "Networks Trucking")), ctas_cuentas = reactiveVal(NULL))
# The observer is not a module: its table's datos/aviso are taken from the
# hd_table_server() call it makes (the real one still runs).
.hts <- hd_table_server; TABLAS <- list()
hd_table_server <- function(id, ...) { a <- list(...); TABLAS[[id]] <<- a; .hts(id, ...) }
testServer(function(input, output, session) settings_cuentas_observer(input, output, session, sh5), {
  session$flushReact()
  t <- TABLAS[["cuentas_t"]]
  ok("5.1 cuentas ilegibles: el aviso sobre la tabla", is.function(t$aviso) && identical(isolate(t$aviso()), AVISO))
  ok("5.1b …y la tabla queda vacía, sin error", identical(nrow(isolate(t$datos())), 0L))
  vivo <- tryCatch({ session$setInputs(`cuentas_t-accion` = list(accion = "editar", ids = "c1", nonce = 1)); TRUE },
                   error = function(e) FALSE)
  ok("5.2 abrir una cuenta: dice que no se pudieron leer, sin tronar", vivo && any(grepl(AVISO, AVISOS, fixed = TRUE)))
  .mem_falla <<- character(); limpiar_caches()
  sh5$ctas_cuentas(CTAS); session$flushReact()
  ok("5.3 con las cuentas leídas: la tabla, sin aviso", is.null(isolate(t$aviso())) && identical(nrow(isolate(t$datos())), 2L))
})
hd_table_server <- .hts

cat("== 6. Guardián: nadie llama a load_ctas_cuentas() sin envolver su stop() ==\n")
# A stop() at a session's start or in a job kills it (the 14 % incident).
# Every CALL in app.R, R/ and scripts/ must sit inside a tryCatch()/try();
# a bare REFERENCE (the function passed as a value) only to register_synced()
# (sync_bus's poll wraps the loader) or .ctx_load() (wraps it). One known
# exception: the Activity viewer's .catalogo_cuentas(), which
# .audit_con_cuentas() always calls inside tryCatch() (checked below).
# wipe_scripts/ are manual one-offs: a stop() there is what we want.
.llamadas_ctas <- function(archivo) {
  out <- list()
  paseo <- function(e, envuelto, padre) {
    if (is.call(e)) {
      f <- if (is.name(e[[1]])) as.character(e[[1]]) else ""
      if (identical(f, "load_ctas_cuentas")) out[[length(out) + 1L]] <<- list(tipo = "llamada", envuelto = envuelto, padre = padre)
      dentro <- envuelto || f %in% c("tryCatch", "try")
      for (i in seq_along(e)[-1]) if (!identical(e[[i]], quote(expr = )) && !is.null(e[[i]])) paseo(e[[i]], dentro, f)
      if (is.call(e[[1]])) paseo(e[[1]], dentro, f)
    } else if (is.name(e) && identical(as.character(e), "load_ctas_cuentas") && !identical(padre, "<-")) {
      out[[length(out) + 1L]] <<- list(tipo = "referencia", envuelto = envuelto, padre = padre)
    } else if (is.expression(e)) for (k in seq_along(e)) paseo(e[[k]], envuelto, padre)
  }
  for (x in parse(archivo, keep.source = FALSE, encoding = "UTF-8")) paseo(x, FALSE, "")
  out
}
archivos <- c("app.R", list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE),
              list.files("scripts", pattern = "\\.R$", full.names = TRUE))
EXCEPCION <- "R/audit_log_viewer_module.R"
sueltas <- character(); n_llamadas <- 0L; n_ref <- 0L
for (a in archivos) {
  for (u in .llamadas_ctas(a)) {
    if (u$tipo == "llamada") { n_llamadas <- n_llamadas + 1L
      if (!u$envuelto && a != EXCEPCION) sueltas <- c(sueltas, sprintf("%s (llamada sin tryCatch)", a)) }
    else { n_ref <- n_ref + 1L
      if (!u$padre %in% c("register_synced", ".ctx_load")) sueltas <- c(sueltas, sprintf("%s (pasada a %s)", a, u$padre)) }
  }
}
ok(sprintf("6.1 las %d llamadas y %d referencias de app.R, R/ y scripts/ están envueltas", n_llamadas, n_ref),
   n_llamadas >= 10L && n_ref == 2L && !length(sueltas))
if (length(sueltas)) cat("       sueltas:", paste(sueltas, collapse = "; "), "\n")
aud <- paste(readLines(EXCEPCION, encoding = "UTF-8", warn = FALSE), collapse = " ")
ok("6.2 la excepción: .audit_con_cuentas() llama a su catálogo dentro de tryCatch()", grepl("tryCatch(catalogo(cid[i]), error", aud, fixed = TRUE))
ok("6.3 el job de avisos no carga settings_helpers.R (no puede llamarla)",
   !any(grepl("settings_helpers", readLines("scripts/avisos_job.R", warn = FALSE), fixed = TRUE)))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
