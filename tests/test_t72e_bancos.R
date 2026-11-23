# =============================================================================
# tests/test_t72e_bancos.R
# T72-E: Bancos logs its saves with an event (the write layer writes the entry
# once the save worked), instead of a hand log_action after them.
#   A  evento$metadata (R/persistence.R): the module's own keys ride with the
#      event; the write layer's keys (tabla, filas_antes, filas_despues,
#      diferencia, origen) always win and a module key with one of those
#      names is dropped, said in the log; without evento$metadata the entry
#      is as before;
#   B  evento_parte_de(accion): the second write of one action leaves a
#      staff-only "<accion>/parte" entry with the table and its counts, and
#      never alarms;
#   C  the real bancosServer (testServer, in-memory S3): Eliminar writes the
#      movements and the papelera -> ONE "eliminar_movimientos" row with the
#      metadata of always (n + the rows summary) plus the table and the
#      difference, and its "/parte"; Agregar manual -> one row with its
#      metadata and target_id; a refused save -> only its _fallido;
#   D  the viewer's bespoke detail (bancos_movimientos / bancos_manual_edit)
#      draws the same from the new entry, its T72 keys taken out (what a
#      client gets, T72-D), as from the old hand-written one;
#   E  static guards: every evento_parte_de("X") in R/ sits in the same named
#      function as the action's event (.bnc_evento("X"), accion = "X" or a
#      log_action with action = "X"); no Bancos event metadata uses a T72 key.
# Run from project root:  Rscript tests/test_t72e_bancos.R
# Contraprueba: on 159e2ff (before T72-E) A, B, C and E fail.
# =============================================================================
cat("=== test_t72e_bancos ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
quieto <- function(expr) suppressWarnings(suppressMessages(expr))
T72 <- c("tabla", "filas_antes", "filas_despues", "diferencia", "origen")

source("tests/helpers/s3_memoria.R")
cargar <- function(archivo, nombres) {
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
}
cargar("R/erp_connector_registry.R", "SAP_B1_ERP_TYPE")
cargar("R/global.R", c("fmt_money", "is_erp_sourced"))
cargar("R/settings/settings_helpers.R", c(".schema_ctas_bancos", "load_ctas_bancos"))
cargar("R/ui_components.R", c("hd_numero_revisar"))
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/monedas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/bancos_parser.R", "R/empresas_identidad.R", "R/bancos_detect.R", "R/bancos_persistence.R", "R/bancos_formatos.R",
            "R/bancos_module.R", "R/audit_log_viewer_module.R"))
  quieto(source(f))

L <- new.env(); L$x <- list(); L$al <- 0L; AVISOS <- character(0)
log_action <- function(user, module, action, description = "", target_id = NA_character_, s3_key = NA_character_,
                       metadata = NULL, client_id = NULL, viewer_home_client_id = NULL) {
  L$x[[length(L$x) + 1L]] <- list(user = user, module = module, action = action, description = description,
                                  target_id = target_id, metadata = metadata, client_id = client_id, home = viewer_home_client_id)
  invisible(TRUE)
}
avisos_agregar <- function(...) { L$al <- L$al + 1L; invisible("id") }
niveles_staff <- function() "dev"
avisos_actividad <- function(...) invisible(NULL)
showNotification <- function(ui, ..., type = "default") { AVISOS <<- c(AVISOS, paste0(type, ": ", as.character(ui))); invisible("id") }
removeModal <- function(...) invisible(NULL); showModal <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
puede <- function(...) TRUE; puede_o_avisa <- function(...) TRUE
limpiar <- function() { L$x <- list(); L$al <- 0L; AVISOS <<- character(0) }
acciones <- function() vapply(L$x, `[[`, "", "action")
CID <- "acme"

# ── A. evento$metadata ─────────────────────────────────────────────────────
s3_memoria_reset(); limpiar()
quieto(.s3_write(tibble::tibble(id = "a"), "t72e.rds", client_id = CID,
                 evento = list(usuario = "ana", modulo = "bancos", accion = "x", descripcion = "d",
                               metadata = list(n = 3L, rows = list(list(id = "a"))))))
m <- L$x[[1]]$metadata
ok("A the module's own keys ride with the event, beside the write layer's", identical(m$n, 3L) && "rows" %in% names(m) && identical(m$tabla, "t72e.rds"))
limpiar()
msgs <- character(0)
withCallingHandlers(
  .s3_write(tibble::tibble(id = "a"), "t72e.rds", client_id = CID,
            evento = list(usuario = "ana", modulo = "bancos", accion = "x", metadata = list(tabla = "MIA", n = 1L))),
  message = function(c) { msgs <<- c(msgs, conditionMessage(c)); invokeRestart("muffleMessage") })
ok("A a module key with a T72 name is dropped (the write layer's wins), said in the log",
   identical(L$x[[1]]$metadata$tabla, "t72e.rds") && identical(L$x[[1]]$metadata$n, 1L) && any(grepl("descartadas: tabla", msgs, fixed = TRUE)))
limpiar(); quieto(.s3_write(tibble::tibble(id = "a"), "t72e.rds", client_id = CID, evento = list(usuario = "ana", modulo = "m", accion = "x")))
ok("A without evento$metadata the entry is as before (only the write layer's keys)",
   setequal(names(L$x[[1]]$metadata), c("tabla", "filas_antes", "filas_despues")))

# ── B. evento_parte_de ─────────────────────────────────────────────────────
limpiar()
quieto(.s3_write(tibble::tibble(id = c("a", "b")), "t72e_b.rds", client_id = CID,
                 evento = evento_parte_de("eliminar_movimientos", "bancos", "ana", "casa")))
e <- if (length(L$x)) L$x[[1]] else NULL
ok("B the second write: one \"eliminar_movimientos/parte\" entry, module bancos, the user, no words, the table and its counts",
   length(L$x) == 1L && identical(e$action, "eliminar_movimientos/parte") && identical(e$module, "bancos") && identical(e$user, "ana") &&
     identical(e$description, "") && identical(e$metadata$tabla, "t72e_b.rds") && identical(e$metadata$filas_despues, 2L) && identical(e$home, "casa"))
ok("B ... and it never alarms", L$al == 0L)

# ── C. The real bancosServer ───────────────────────────────────────────────
MK <- paste0(CID, "/", S3_KEYS$bancos_movimientos); PK <- paste0(CID, "/", S3_KEYS$bancos_papelera)
ctas <- tibble::tibble(id = c("c1", "c2"), Empresa = "NTS", banco_id = "bb", Moneda = "MXN", alias = c("A1", "A2"),
                       cuenta = c("01", "02"), clabe_interbancaria = c("0301", "0302"), saldo_inicial = 0, activa = TRUE)
SES <- as.POSIXct("2026-10-01 10:00:00")
mov <- function(id, fuente = "txt", eliminado = FALSE, cargo = 100)
  tibble::tibble(id = id, cuenta_id = "c1", fecha = as.Date("2026-10-30"), hora = "10:00:00", recibo = id,
                 descripcion_raw = paste("Mov", id), tipo = "spei_out", parte = "PROV", rfc = NA_character_,
                 referencia = NA_character_, clave_rastreo = NA_character_, concepto = "c", cargo = cargo, abono = 0,
                 saldo_banco = 1000, conciliado = FALSE, doc_vinculado = NA_character_, agenda_id = NA_character_,
                 importado_at = SES, fuente = fuente, eliminado = eliminado, eliminado_por = NA_character_,
                 eliminado_at = if (eliminado) Sys.time() else as.POSIXct(NA), notas = NA_character_)
movs0 <- .normalize(dplyr::bind_rows(mov("m1"), mov("m2"), mov("m3", fuente = "manual", eliminado = TRUE)), .schema_bancos_movimientos)
sembrar <- function() { s3_memoria_reset(); assign(MK, movs0, envir = .mem); assign(PK, .schema_bancos_papelera(), envir = .mem) }
MOVS <- reactiveVal(movs0)
shared <- list(
  effective_client_id = function() CID, home_client_id = function() "casa", current_user = function() "luis",
  usuario = reactive("luis"), company_map = reactiveVal(c(NTS = "Networks Trucking")),
  visible_initials = reactiveVal(NULL), bancos_movimientos = MOVS,
  bancos_confirmados = reactiveVal(.schema_bancos_confirmados()), conciliacion_rv = reactiveVal(tibble::tibble()),
  ctas_cuentas = reactiveVal(ctas), papelera_rv = reactiveVal(NULL), abonos_db = reactiveVal(tibble::tibble()),
  manual_inv = reactiveVal(NULL), pasivos_provisions_db = reactiveVal(NULL), sap_data = reactiveVal(list()),
  empresas_db = reactiveVal(NULL), suppress_ledger_prov_refresh = reactiveVal(FALSE))
G <- new.env()
quieto(testServer(bancosServer, args = list(shared = shared), {
  session$flushReact()
  # Eliminar m1 (the movements, then the papelera)
  sembrar(); MOVS(movs0); limpiar()
  elim_ids_rv("m1"); session$setInputs(do_eliminar_confirm = runif(1))
  G$elim <- L$x
  # Agregar manual
  sembrar(); MOVS(movs0); limpiar(); editing_mov_id(NULL)
  session$setInputs(man_cuenta = "c1", man_tipo = "manual_in", man_cargo = 0, man_abono = 777, man_fecha = as.Date("2026-10-31"),
                    man_parte = "AJUSTE", man_concepto = "x", man_notas = "", do_add_manual = runif(1))
  G$man <- L$x
  # A refused save
  sembrar(); MOVS(movs0); limpiar(); .mem_falla_escritura <<- MK
  elim_ids_rv("m1"); session$setInputs(do_eliminar_confirm = runif(1))
  .mem_falla_escritura <<- character(0)
  G$falla <- L$x
}))
ac <- function(x) vapply(x, `[[`, "", "action")
cliente <- function(x) Filter(function(e) !grepl("/parte$", e$action) && !identical(e$action, "sin_evento"), x)
ok("C Eliminar: ONE row a client sees, \"eliminar_movimientos\", and its \"/parte\" (the papelera)",
   identical(ac(cliente(G$elim)), "eliminar_movimientos") && "eliminar_movimientos/parte" %in% ac(G$elim) && !"sin_evento" %in% ac(G$elim))
ee <- Filter(function(e) identical(e$action, "eliminar_movimientos"), G$elim)[[1]]
ok("C ... with the metadata of always (n and the rows summary), the words of always, and the table and the difference (staff)",
   identical(ee$metadata$n, 1L) && all(c("n_total", "rows") %in% names(ee$metadata)) &&
     identical(ee$description, "1 movimiento(s) enviado(s) a papelera") && identical(ee$user, "luis") && identical(ee$home, "casa") &&
     identical(ee$metadata$tabla, S3_KEYS$bancos_movimientos) && !is.null(ee$metadata$diferencia))
pp <- Filter(function(e) identical(e$action, "eliminar_movimientos/parte"), G$elim)[[1]]
ok("C ... its /parte is the papelera's write", identical(pp$metadata$tabla, S3_KEYS$bancos_papelera) && identical(pp$module, "bancos"))
em <- Filter(function(e) identical(e$action, "agregar_movimiento_manual"), G$man)
ok("C Agregar manual: one row, with its metadata (cuenta, tipo, cargo, abono) and the new movement's id as target",
   length(cliente(G$man)) == 1L && length(em) == 1L && identical(em[[1]]$metadata$abono, 777) && identical(em[[1]]$metadata$cuenta_id, "c1") &&
     !is.na(em[[1]]$target_id) && nzchar(em[[1]]$target_id) && em[[1]]$target_id != S3_KEYS$bancos_movimientos)
ok("C a refused save: only its _fallido, no event", identical(ac(G$falla), "eliminar_movimientos_fallido"))

# ── D. The viewer draws the same ───────────────────────────────────────────
sin_t72 <- function(m) m[setdiff(names(m), T72)]
viejo <- c(list(n = 1L), .bancos_audit_rows_summary(movs0[movs0$id == "m1", ]))   # what the hand log wrote
json <- function(m) jsonlite::fromJSON(as.character(jsonlite::toJSON(m, auto_unbox = TRUE, null = "null")), simplifyVector = TRUE)
html <- function(kind, m) as.character(.audit_detail_body_by_kind(kind, json(m)))
ok("D bancos_movimientos: the client's detail from the new entry (T72 keys out) is the old one's",
   identical(html("bancos_movimientos", sin_t72(ee$metadata)), html("bancos_movimientos", viejo)))
ok("D bancos_manual_edit: the same for an edit's before/after",
   identical(html("bancos_manual_edit", sin_t72(c(list(before = list(cargo = 1), after = list(cargo = 2)), list(tabla = "x", filas_antes = 1L)))),
             html("bancos_manual_edit", list(before = list(cargo = 1), after = list(cargo = 2)))))

# ── E. Static guards ───────────────────────────────────────────────────────
filas <- list()
andar <- function(e, archivo, funcion) {
  if (!is.call(e)) return(invisible())
  if ((identical(e[[1]], as.name("<-")) || identical(e[[1]], as.name("="))) && length(e) == 3L && is.name(e[[2]]) &&
      is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) { andar(e[[3]][[3]], archivo, as.character(e[[2]])); return(invisible()) }
  f <- if (is.name(e[[1]])) as.character(e[[1]]) else ""
  a <- as.list(e)
  if (f == "evento_parte_de" && length(a) >= 2L && is.character(a[[2]])) filas[[length(filas) + 1L]] <<- c("parte", archivo, funcion, a[[2]])
  if (f == ".bnc_evento" && length(a) >= 2L) {
    x <- a[[2]]; for (s in unlist(Filter(is.character, if (is.call(x)) as.list(x) else list(x)))) filas[[length(filas) + 1L]] <<- c("evento", archivo, funcion, s)
  }
  if (!is.null(names(a)) && "accion" %in% names(a) && is.character(a[["accion"]])) filas[[length(filas) + 1L]] <<- c("evento", archivo, funcion, a[["accion"]])
  if (f == "log_action" && !is.null(names(a)) && "action" %in% names(a) && is.character(a[["action"]]))
    filas[[length(filas) + 1L]] <<- c("evento", archivo, funcion, a[["action"]])
  # log_action(user, module, action, ...) by position (the Agenda's Confirmar, T72-F)
  if (f == "log_action" && length(a) >= 4L && (is.null(names(a)) || !nzchar(names(a)[4])) && is.character(a[[4]]))
    filas[[length(filas) + 1L]] <<- c("evento", archivo, funcion, a[[4]])
  for (k in seq_along(e)[-1]) { b <- e[[k]]; if (!missing(b) && is.call(b)) andar(b, archivo, funcion) }
}
for (f in list.files("R", "\\.R$", full.names = TRUE, recursive = TRUE)) for (e in parse(f, keep.source = FALSE, encoding = "UTF-8")) andar(e, f, "<top>")
F <- as.data.frame(do.call(rbind, filas), stringsAsFactors = FALSE); names(F) <- c("tipo", "archivo", "funcion", "accion")
partes <- F[F$tipo == "parte", ]
huerfanas <- partes[!vapply(seq_len(nrow(partes)), function(i)
  any(F$tipo == "evento" & F$archivo == partes$archivo[i] & F$funcion == partes$funcion[i] & F$accion == partes$accion[i]), TRUE), ]
ok(sprintf("E every evento_parte_de(\"X\") sits beside the action's event (%d found, %d orphan: %s)", nrow(partes), nrow(huerfanas),
           paste(huerfanas$accion, collapse = ", ")), nrow(partes) >= 4L && !nrow(huerfanas))
ok("E no Bancos event metadata uses a T72 key (the rows summary's names included)",
   !any(T72 %in% names(.bancos_audit_rows_summary(movs0))) && !any(T72 %in% names(ee$metadata[setdiff(names(ee$metadata), T72)])))
plantado <- parse(text = c('f1 <- function() { save_x(1, evento = evento_parte_de("sola")) }'), keep.source = FALSE)
filas <- list(); for (e in plantado) andar(e, "R/plantado.R", "<top>")
P <- as.data.frame(do.call(rbind, filas), stringsAsFactors = FALSE)
ok("E the guard is not blind: an evento_parte_de with no event beside it is found", nrow(P) == 1L && P[[1]][1] == "parte")

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
