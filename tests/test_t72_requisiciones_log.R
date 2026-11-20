# =============================================================================
# tests/test_t72_requisiciones_log.R
# T72 (A4): every Requisiciones write reaches the Actividad log (app_audit,
# module "requisiciones"), with what changed row by row (antes/despues), right
# after the write succeeds. Before, only requisiciones_historial.rds (which no
# Actividad screen reads) and nothing at all for folders.
# Harness: the one of tests/test_requisiciones_datos.R (in-memory S3 local to
# this test, aws.s3 never patched); log_action is a recorder.
# Run from the repo root: Rscript tests/test_t72_requisiciones_log.R
# Contraprueba: on 867d251 every block but the last fails.
# =============================================================================
library(testthat)
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a

# Only these definitions from files this test does not own.
.tomar <- function(archivo, nombres) {
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]])[1] %in% nombres)
      eval(e, globalenv())
}
.tomar("R/global.R", "S3_KEYS")
.tomar("R/persistence.R", c(".normalize", "leer_tabla_para_escribir", ".s3_leer_para_escribir",
                            "modificar_tabla", "rebase_por_id", "confirmaciones_vigentes", ".schema_manual",
                            ".schema_empresas",
                            # CM35: the view reads the Agenda strictly (en_agenda)
                            "safe_load_pagar_hoy", "agenda_sync_activa", ".pagar_hoy_vivo",
                            "s3_key_pagar_hoy_user", ".schema_agenda_sync_config", ".schema_pagar_hoy"))
.tomar("R/erp_connector_registry.R", "SAP_B1_ERP_TYPE")
.tomar("R/bancos_persistence.R", ".schema_bancos_confirmados")
.tomar("R/pasivos_persistence.R", c("etapa_de_provision_en", "PROVISION_PAGADA"))
source("R/pasivos_schemas.R")
source("R/i18n.R")
source("R/tier_registry.R")
source("R/permisos.R")
source("R/avisos.R")
source("R/adjuntos_s3.R")
source("R/empresas_identidad.R")
source("R/agenda_filas.R")   # CM35 A2: agenda_pendientes(), the Agenda's own "pending"
source("R/requisiciones_persistence.R")

# ── In-memory S3 ─────────────────────────────────────────────────────────────
.mem  <- new.env()     # tables, by full key
.objs <- new.env()     # raw objects (attachments, pasted tables), by full key
.s3_preload_cache <- new.env(); .s3_missing_cache <- new.env()
.fallar_lecturas <- FALSE
.fallar_escrituras <- 0L
.escrituras <- 0L
.s3_key <- function(key, client_id = NULL) {
  if (is.null(client_id) || !nzchar(client_id)) stop("sin client_id en .s3_key")
  paste0(tolower(client_id), "/", key)
}
.s3_read_with <- function(key, client_id = NULL) {
  fk <- .s3_key(key, client_id)
  if (exists(fk, envir = .s3_preload_cache, inherits = FALSE)) return(.s3_preload_cache[[fk]])
  if (.fallar_lecturas) return(NULL)
  .mem[[fk]]
}
.s3_existe <- function(key, client_id) {
  if (.fallar_lecturas) return(NA)
  exists(.s3_key(key, client_id), envir = .mem, inherits = FALSE)
}
.s3_write <- function(obj, key, client_id = NULL, permitir_reduccion = FALSE) {
  if (.fallar_escrituras > 0L) { .fallar_escrituras <<- .fallar_escrituras - 1L; stop("escritura simulada falló") }
  .escrituras <<- .escrituras + 1L
  assign(.s3_key(key, client_id), obj, envir = .mem)
}
.adj_s3_poner <- function(bytes, llave, mime) assign(llave, bytes, envir = .objs)
.adj_s3_traer <- function(llave) {
  if (!exists(llave, envir = .objs, inherits = FALSE)) stop("no existe ", llave)
  get(llave, envir = .objs)
}
reiniciar <- function() {
  rm(list = ls(.mem), envir = .mem); rm(list = ls(.objs), envir = .objs)
  rm(list = ls(.s3_preload_cache), envir = .s3_preload_cache)
  .fallar_lecturas <<- FALSE; .fallar_escrituras <<- 0L
}
tabla <- function(clave, cid = CID) .mem[[paste0(cid, "/", REQ_KEYS[[clave]])]]
foto <- function() as.list(.mem)

# ── T72: the log recorder (after the sources, so it wins) ───────────────────
LOG <- list()
log_action <- function(user, module, action, description, target_id = NA_character_, s3_key = NA_character_,
                       metadata = NULL, client_id = NULL, viewer_home_client_id = NULL) {
  LOG[[length(LOG) + 1L]] <<- list(user = user, module = module, action = action, description = description,
                                   target_id = target_id, metadata = metadata,
                                   client_id = client_id, home = viewer_home_client_id)
  invisible(NULL)
}
limpiar <- function() LOG <<- list()
acciones <- function() vapply(LOG, `[[`, "", "action")
ultima <- function(a) { x <- Filter(function(e) identical(e$action, a), LOG); if (length(x)) x[[length(x)]] else NULL }

CID <- "cliente_prueba"
base_req <- list(titulo = "Cemento para la bodega 3", empresa = "NCS", proveedor = "Cemex",
                 monto = 15000, moneda = "MXN", notas = "Urgente")

test_that("crear: una entrada 'requisiciones' en el log del cliente, con la fila nueva", {
  reiniciar(); limpiar()
  r <- requisicion_crear(CID, base_req, "dani")
  e <- ultima("crear_requisicion")
  expect_false(is.null(e))
  expect_identical(e$module, "requisiciones")
  expect_identical(e$client_id, CID)
  expect_identical(e$home, CID)                                   # a client's own user: home = the client
  expect_identical(e$user, "dani")
  expect_identical(e$target_id, r$id)
  expect_match(e$description, r$folio, fixed = TRUE)
  expect_identical(unname(e$metadata$cambio$n["nuevas"]), 1L)
  expect_identical(e$metadata$cambio$nuevas[[1]]$titulo, "Cemento para la bodega 3")
  expect_identical(e$metadata$tabla, "requisiciones")
})

test_that("staff saltado: home = su cliente de origen (la copia en hd-admin)", {
  reiniciar(); limpiar()
  requisicion_crear(CID, base_req, "luis", home = "hd-admin")
  e <- ultima("crear_requisicion")
  expect_identical(e$client_id, CID)
  expect_identical(e$home, "hd-admin")
})

test_that("editar: el antes y el después de lo que cambió; sin cambios, nada en el log", {
  reiniciar(); limpiar()
  r <- requisicion_crear(CID, base_req, "dani"); limpiar()
  requisicion_editar(CID, r$id, list(monto = 20000), "eli")
  e <- ultima("editar_requisicion")
  expect_false(is.null(e))
  expect_equal(e$metadata$cambio$cambiadas[[r$id]]$monto$antes, 15000)
  expect_equal(e$metadata$cambio$cambiadas[[r$id]]$monto$despues, 20000)
  expect_identical(e$metadata$cambio$cambiadas[[r$id]]$actualizado_por$despues, "eli")
  limpiar()
  requisicion_editar(CID, r$id, list(monto = 20000), "eli")      # the same value: nothing written
  expect_length(LOG, 0L)
})

test_that("mover, papelera y restaurar quedan en el log", {
  reiniciar(); limpiar()
  carp <- requisiciones_carpeta_crear(CID, list(nombre = "Obra norte"), "eli")
  r <- requisicion_crear(CID, base_req, "dani"); limpiar()
  requisiciones_mover(CID, r$id, carp, "eli")
  requisiciones_papelera(CID, r$id, "eli")
  requisiciones_restaurar(CID, r$id, "eli")
  expect_identical(acciones(), c("mover_requisiciones", "papelera_requisiciones", "restaurar_requisiciones"))
  expect_identical(ultima("mover_requisiciones")$metadata$cambio$cambiadas[[r$id]]$carpeta_id$despues, carp)
  limpiar()
  requisiciones_papelera(CID, "no-existe", "eli")                 # nothing changes: nothing logged
  expect_length(LOG, 0L)
})

test_that("las carpetas, que no dejaban rastro en ningún lado, quedan en el log", {
  reiniciar(); limpiar()
  carp <- requisiciones_carpeta_crear(CID, list(nombre = "Obra norte"), "eli")
  requisiciones_carpeta_editar(CID, carp, list(nombre = "Obra sur"), "eli")
  requisiciones_carpeta_papelera(CID, carp, "eli")
  requisiciones_carpeta_restaurar(CID, carp, "eli")
  expect_identical(acciones(), c("crear_carpeta", "editar_carpeta", "papelera_carpeta", "restaurar_carpeta"))
  expect_identical(ultima("editar_carpeta")$metadata$cambio$cambiadas[[carp]]$nombre,
                   list(antes = "Obra norte", despues = "Obra sur"))
  expect_identical(ultima("crear_carpeta")$metadata$tabla, "carpetas")
})

test_that("comentar queda; la marca de 'leído' sola no (excepción documentada)", {
  reiniciar()
  r <- requisicion_crear(CID, base_req, "dani"); limpiar()
  requisicion_comentar(CID, r$id, "¿ya llegó?", "eli")
  expect_identical(acciones(), "comentar_requisicion")
  limpiar()
  requisiciones_marcar_leido(CID, r$id, "dani")
  expect_length(LOG, 0L)
})

test_that("evidencia: tabla pegada, papelera y restaurar", {
  reiniciar()
  r <- requisicion_crear(CID, base_req, "dani"); limpiar()
  t <- tabla_pegada_preparar("Concepto\tMonto\nCemento\t15000")
  ev <- evidencia_tabla_agregar(CID, r$id, t, "Cotización", TRUE, "dani")
  evidencia_papelera(CID, ev, "dani")
  evidencia_restaurar(CID, ev, "dani")
  expect_identical(acciones(), c("agregar_tabla_evidencia", "papelera_evidencia", "restaurar_evidencia"))
  expect_identical(ultima("agregar_tabla_evidencia")$target_id, r$id)
})

test_that("un log que falla nunca deshace ni rompe la acción", {
  reiniciar(); limpiar()
  log_action <<- function(...) stop("S3 caído")
  r <- requisicion_crear(CID, base_req, "dani")
  expect_identical(nrow(tabla("requisiciones")), 1L)
  expect_true(nzchar(r$folio))
})

test_that("una escritura que falla no deja entrada de log", {
  reiniciar()
  log_action <<- function(...) { LOG[[length(LOG) + 1L]] <<- list(action = list(...)$action); invisible(NULL) }
  limpiar(); .fallar_escrituras <<- 1L
  expect_error(requisicion_crear(CID, base_req, "dani"))
  expect_length(LOG, 0L)
})

test_that("requisicion_historial(): su tabla y el log, unidos y sin duplicados", {
  reiniciar(); limpiar()
  log_action <<- function(user, module, action, description, target_id = NA_character_, s3_key = NA_character_,
                          metadata = NULL, client_id = NULL, viewer_home_client_id = NULL) {
    LOG[[length(LOG) + 1L]] <<- list(user = user, module = module, action = action, target_id = target_id,
                                     metadata = as.character(jsonlite::toJSON(metadata, auto_unbox = TRUE, null = "null")),
                                     ts = Sys.time())
    invisible(NULL)
  }
  r <- requisicion_crear(CID, base_req, "dani")
  requisicion_editar(CID, r$id, list(monto = 20000), "eli")
  como_log <- function() dplyr::bind_rows(lapply(seq_along(LOG), function(i) tibble::tibble(
    id = paste0("e", i), ts = LOG[[i]]$ts, user = LOG[[i]]$user, module = LOG[[i]]$module,
    action = LOG[[i]]$action, target_id = LOG[[i]]$target_id, metadata = LOG[[i]]$metadata)))
  h <- requisicion_historial(CID, r$id, log = como_log())
  expect_identical(h$accion, c("creada", "editada"))            # both in the table and the log: once each
  # The table loses its "editada" row (its write failed): the log puts it back.
  k <- paste0(CID, "/", REQ_KEYS$historial)
  .mem[[k]] <- .mem[[k]][.mem[[k]]$accion != "editada", ]
  h <- requisicion_historial(CID, r$id, log = como_log())
  expect_identical(h$accion, c("creada", "editada"))
  expect_identical(h$detalle[2], "monto")
  expect_match(h$id[2], "^log:")
  # Another module, or another requisition, never comes in.
  otro <- dplyr::bind_rows(como_log(), tibble::tibble(id = "z", ts = Sys.time(), user = "x", module = "bancos",
                                                      action = "editar_requisicion", target_id = r$id, metadata = "{}"))
  expect_identical(nrow(requisicion_historial(CID, r$id, log = otro)), 2L)
  expect_identical(nrow(requisicion_historial(CID, "otra", log = como_log())), 0L)
})

test_that("las claves requisiciones.log.* existen en es y en", {
  es <- jsonlite::fromJSON("www/i18n/es.json"); en <- jsonlite::fromJSON("www/i18n/en.json")
  cl <- grep("^requisiciones\\.log\\.", names(es), value = TRUE)
  expect_gte(length(cl), 15L)
  expect_true(all(cl %in% names(en)))
})
