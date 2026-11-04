# Requisiciones, stage E1 (data and evidence): R/requisiciones_persistence.R
# and R/adjuntos_s3.R.
# S3 is an in-memory store local to this test: only the persistence helpers
# those files call are redefined, in this script's own global env. aws.s3
# itself is never patched. The real write helpers (modificar_tabla,
# leer_tabla_para_escribir, .s3_leer_para_escribir, rebase_por_id,
# .normalize) are taken from R/persistence.R unchanged.
# Run from the repo root: Rscript tests/test_requisiciones_datos.R
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
                            "modificar_tabla", "rebase_por_id", "confirmaciones_vigentes", ".schema_manual"))
.tomar("R/bancos_persistence.R", ".schema_bancos_confirmados")
.tomar("R/pasivos_persistence.R", c("etapa_de_provision_en", "PROVISION_PAGADA"))
source("R/pasivos_schemas.R")
source("R/i18n.R")
source("R/tier_registry.R")
source("R/permisos.R")
source("R/avisos.R")
source("R/adjuntos_s3.R")
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

CID <- "cliente_prueba"
base_req <- list(titulo = "Cemento para la bodega 3", empresa = "NCS", proveedor = "Cemex",
                 monto = 15000, moneda = "MXN", notas = "Urgente porque la obra empieza el lunes")

poner_provision <- function(id, estado = "provisional", monto = 20000, cid = CID) {
  k <- paste0(cid, "/", S3_KEYS$pasivos_provisions)
  p <- .normalize(.mem[[k]], .schema_pasivos_provision)
  p <- dplyr::bind_rows(p[p$id != id, ], tibble::tibble(id = id, estado = estado, amount_pago = monto,
                                                         moneda_pago = "MXN", fecha_efectiva = as.Date("2026-12-15")))
  assign(k, p, envir = .mem)
}
poner_confirmacion <- function(provision_id, importe = 19500, eliminado = FALSE, action = "confirmed", cid = CID) {
  k <- paste0(cid, "/", S3_KEYS$bancos_confirmados)
  c <- .normalize(.mem[[k]], .schema_bancos_confirmados)
  c <- dplyr::bind_rows(c, tibble::tibble(confirmacion_id = uuid::UUIDgenerate(), provision_id = provision_id,
                                          importe = importe, moneda = "MXN", eliminado = eliminado,
                                          action = action, confirmado_at = Sys.time()))
  assign(k, c, envir = .mem)
}
archivo_temp <- function(bytes, ext = "bin") { f <- tempfile(fileext = paste0(".", ext)); writeBin(bytes, f); f }
png_real <- function(w = 40, h = 20) magick::image_write(magick::image_blank(w, h, "steelblue"), format = "png")

# ── Fail closed ──────────────────────────────────────────────────────────────

test_that("sin client_id falla cerrado y no escribe nada", {
  reiniciar()
  for (cid in list(NULL, "", NA_character_, "  ")) {
    expect_error(requisicion_crear(cid, base_req, "ana"), "client_id")
    expect_error(requisiciones_vista(cid), "client_id")
    expect_error(requisicion_de_provision(cid, "p1"), "client_id")
    expect_error(requisicion_comentar(cid, "x", "hola", "ana"), "client_id")
    expect_error(requisiciones_carpeta_crear(cid, list(nombre = "Obra"), "ana"), "client_id")
  }
  expect_error(.adj_llave("", "12345678-aaaa", "12345678-bbbb", "png"), "client_id")
  expect_length(ls(.mem), 0)
})

# ── Create, folio, validation ────────────────────────────────────────────────

test_that("crear asigna folio REQ-AAAA-0001 consecutivo sobre la tabla fresca", {
  reiniciar()
  a <- requisicion_crear(CID, base_req, "dani")
  b <- requisicion_crear(CID, modifyList(base_req, list(titulo = "Varilla")), "dani")
  anio <- format(Sys.time(), "%Y", tz = "America/Mexico_City")
  expect_identical(a$folio, sprintf("REQ-%s-0001", anio))
  expect_identical(b$folio, sprintf("REQ-%s-0002", anio))
  r <- tabla("requisiciones")
  expect_equal(nrow(r), 2)
  expect_true(all(is.na(r$provision_id)))
  expect_true(all(c("creada") %in% tabla("historial")$accion))
})

test_that("prioridad opcional, entero 1 a 10", {
  reiniciar()
  r1 <- requisicion_crear(CID, modifyList(base_req, list(prioridad = 10)), "dani")
  r2 <- requisicion_crear(CID, base_req, "dani")
  expect_identical(tabla("requisiciones")$prioridad, c(10L, NA_integer_))
  for (p in list(0, 11, 2.5, "alta")) {
    e <- tryCatch(requisicion_crear(CID, modifyList(base_req, list(prioridad = p)), "dani"), error = identity)
    expect_s3_class(e, "req_error")
  }
  requisicion_editar(CID, r2$id, list(prioridad = 3), "caro")
  expect_identical(tabla("requisiciones")$prioridad, c(10L, 3L))
  requisicion_editar(CID, r2$id, list(prioridad = NA), "caro")
  expect_identical(tabla("requisiciones")$prioridad, c(10L, NA_integer_))
})

test_that("validaciones: título, empresa, notas <= 2000, monto con moneda", {
  reiniciar()
  expect_s3_class(tryCatch(requisicion_crear(CID, modifyList(base_req, list(titulo = "  ")), "d"), error = identity), "req_error")
  expect_s3_class(tryCatch(requisicion_crear(CID, modifyList(base_req, list(empresa = NULL)), "d"), error = identity), "req_error")
  expect_s3_class(tryCatch(requisicion_crear(CID, modifyList(base_req, list(notas = strrep("x", 2001))), "d"), error = identity), "req_error")
  expect_s3_class(tryCatch(requisicion_crear(CID, modifyList(base_req, list(moneda = NULL)), "d"), error = identity), "req_error")
  expect_s3_class(tryCatch(requisicion_crear(CID, modifyList(base_req, list(monto = -1)), "d"), error = identity), "req_error")
  ok <- requisicion_crear(CID, list(titulo = "Sin monto", empresa = "NCS"), "d")
  expect_true(is.na(tabla("requisiciones")$monto))
  expect_true(nchar(requisicion_crear(CID, modifyList(base_req, list(notas = strrep("x", 2000))), "d")$id) > 0)
})

# ── Concurrency ──────────────────────────────────────────────────────────────

test_that("dos sesiones escribiendo a la vez no pierden nada", {
  reiniciar()
  a1 <- requisicion_crear(CID, base_req, "dani")
  # Session A cached the table when it had one row (stale preload cache).
  vieja <- tabla("requisiciones")
  # Session B adds a requisition, a folder and a comment meanwhile.
  b1 <- requisicion_crear(CID, modifyList(base_req, list(titulo = "De B")), "eli")
  carp <- requisiciones_carpeta_crear(CID, list(nombre = "Obra norte", presupuesto = 1e6, moneda = "MXN"), "eli")
  requisicion_comentar(CID, a1$id, "Lo reviso", "eli")
  assign(paste0(CID, "/", REQ_KEYS$requisiciones), vieja, envir = .s3_preload_cache)
  # Session A edits its row and creates another one, from its stale view.
  requisicion_editar(CID, a1$id, list(titulo = "Cemento (editado)"), "dani")
  a2 <- requisicion_crear(CID, modifyList(base_req, list(titulo = "Otra de A")), "dani")
  requisicion_comentar(CID, a1$id, "Gracias", "dani")
  requisiciones_mover(CID, c(a1$id, b1$id), carp, "dani")
  r <- tabla("requisiciones")
  expect_setequal(r$id, c(a1$id, b1$id, a2$id))
  expect_identical(r$titulo[r$id == a1$id], "Cemento (editado)")
  expect_identical(r$titulo[r$id == b1$id], "De B")
  expect_false(anyDuplicated(r$folio) > 0)
  expect_true(all(r$carpeta_id[r$id %in% c(a1$id, b1$id)] == carp))
  expect_equal(nrow(tabla("comentarios")), 2)
})

test_that("una lectura fallida no escribe nada ni se vuelve tabla vacía", {
  reiniciar()
  r <- requisicion_crear(CID, base_req, "dani")
  antes <- foto(); n_obj <- length(ls(.objs))
  .fallar_lecturas <<- TRUE
  expect_error(requisicion_crear(CID, base_req, "dani"), "could not read")
  expect_error(requisicion_editar(CID, r$id, list(titulo = "x"), "dani"), "could not read")
  expect_error(requisiciones_papelera(CID, r$id, "dani"), "could not read")
  expect_error(requisiciones_vista(CID), "could not read")          # never an empty list
  expect_error(requisicion_comentar(CID, r$id, "hola", "dani"), "could not read")
  f <- archivo_temp(png_real(), "png")
  expect_error(evidencia_adjuntar(CID, r$id, adjunto_preparar(f, "c.png"), "dani"), "could not read")
  .fallar_lecturas <<- FALSE
  expect_identical(foto(), antes)
  expect_equal(length(ls(.objs)), n_obj)
})

test_that("un guardado fallido se reporta como error y el estado queda igual", {
  reiniciar()
  r <- requisicion_crear(CID, base_req, "dani")
  antes <- tabla("requisiciones")
  .fallar_escrituras <<- 1L
  expect_error(requisicion_editar(CID, r$id, list(titulo = "Nuevo"), "dani"), "simulada")
  expect_identical(tabla("requisiciones"), antes)
})

# ── Stage ────────────────────────────────────────────────────────────────────

test_that("Pagada solo con una confirmación vigente con su provision_id", {
  reiniciar()
  r <- requisicion_crear(CID, base_req, "dani")
  etapa <- function() requisiciones_vista(CID, incluir_pagadas = TRUE)$etapa
  expect_identical(etapa(), "sin_provision")
  poner_provision("p1")
  requisicion_ligar_provision(CID, r$id, "p1", "caro")
  expect_identical(etapa(), "provisionada")
  # estado says paid, but there is no confirmation: not Pagada
  poner_provision("p1", estado = "item_confirmed")
  expect_identical(etapa(), "en_pago")
  # a deleted confirmation and an undo row are not payments
  poner_confirmacion("p1", eliminado = TRUE)
  poner_confirmacion("p1", action = "recovered")
  expect_identical(etapa(), "en_pago")
  expect_equal(nrow(requisiciones_vista(CID)), 1)
  poner_confirmacion("p1")
  expect_identical(etapa(), "pagada")
  # paid: out of every view, but never deleted from S3
  expect_equal(nrow(requisiciones_vista(CID)), 0)
  expect_equal(nrow(tabla("requisiciones")), 1)
  # still found from the confirmation history, read-only
  d <- requisicion_de_provision(CID, "p1")
  expect_equal(nrow(d), 1); expect_identical(d$etapa, "pagada")
  e <- tryCatch(requisicion_editar(CID, r$id, list(titulo = "x"), "dani"), error = identity)
  expect_s3_class(e, "req_error")
  expect_identical(e$clave, "requisiciones.error.pagada")
  expect_s3_class(tryCatch(requisicion_comentar(CID, r$id, "x", "dani"), error = identity), "req_error")
})

test_that("monto mostrado: la cifra más firme, nunca dos sumadas", {
  reiniciar()
  r <- requisicion_crear(CID, base_req, "dani")
  v <- function() requisiciones_vista(CID, incluir_pagadas = TRUE)
  expect_equal(v()$monto_mostrado, 15000); expect_identical(v()$monto_fuente, "estimado")
  poner_provision("p1", monto = 20000)
  requisicion_ligar_provision(CID, r$id, "p1", "caro")
  expect_equal(v()$monto_mostrado, 20000); expect_identical(v()$monto_fuente, "provision")
  assign(paste0(CID, "/", S3_KEYS$manual),
         tibble::tibble(id = "m1", Importe = 19800, Moneda = "MXN", provision_id = "p1"), envir = .mem)
  poner_provision("p1", estado = "converted", monto = 20000)
  expect_equal(v()$monto_mostrado, 19800); expect_identical(v()$monto_fuente, "item")
  poner_confirmacion("p1", importe = 19500)
  expect_equal(v()$monto_mostrado, 19500); expect_identical(v()$monto_fuente, "pago")
})

test_that("una provisión borrada devuelve la requisición a Sin provisión, marcada", {
  reiniciar()
  r <- requisicion_crear(CID, base_req, "dani")
  poner_provision("p1")
  requisicion_ligar_provision(CID, r$id, "p1", "caro")
  poner_provision("p1", estado = "deleted")
  v <- requisiciones_vista(CID)
  expect_identical(v$etapa, "sin_provision"); expect_true(v$provision_perdida)
  expect_equal(v$monto_mostrado, 15000)
  # it can take a new provision
  poner_provision("p2")
  requisicion_ligar_provision(CID, r$id, "p2", "caro")
  expect_identical(requisiciones_vista(CID)$etapa, "provisionada")
})

# Luis, 2026-11-04: a deleted OR cancelled provision sends the requisition
# back to Sin provisión, flagged, and another provision can be linked.
test_that("una provisión cancelada (cerrada sin pago vigente) también la devuelve a Sin provisión", {
  reiniciar()
  r <- requisicion_crear(CID, base_req, "dani")
  poner_provision("p1")
  requisicion_ligar_provision(CID, r$id, "p1", "caro")
  poner_confirmacion("p1", eliminado = TRUE)          # its payment was undone
  poner_provision("p1", estado = "closed")
  v <- requisiciones_vista(CID)
  expect_identical(v$etapa, "sin_provision"); expect_true(v$provision_perdida)
  expect_identical(v$monto_fuente, "estimado")
  expect_equal(nrow(requisiciones_vista(CID)), 1)       # still listed, not hidden as paid
  poner_provision("p2")
  requisicion_ligar_provision(CID, r$id, "p2", "caro")
  v <- requisiciones_vista(CID)
  expect_identical(v$etapa, "provisionada"); expect_false(v$provision_perdida)
  expect_identical(v$provision_id, "p2")
})

test_that("una provisión tiene a lo más una requisición", {
  reiniciar()
  poner_provision("p1")
  a <- requisicion_crear(CID, modifyList(base_req, list(provision_id = "p1")), "dani")
  expect_identical(tabla("requisiciones")$provision_id, "p1")
  e <- tryCatch(requisicion_crear(CID, modifyList(base_req, list(provision_id = "p1")), "dani"), error = identity)
  expect_identical(e$clave, "requisiciones.error.provision_ocupada")
  b <- requisicion_crear(CID, base_req, "dani")
  e <- tryCatch(requisicion_ligar_provision(CID, b$id, "p1", "caro"), error = identity)
  expect_identical(e$clave, "requisiciones.error.provision_ocupada")
  e <- tryCatch(requisicion_ligar_provision(CID, b$id, "no_existe", "caro"), error = identity)
  expect_identical(e$clave, "requisiciones.error.provision_no_existe")
})

test_that("papelera es una marca: sale de la lista, se restaura, nada se borra", {
  reiniciar()
  a <- requisicion_crear(CID, base_req, "dani")
  b <- requisicion_crear(CID, base_req, "eli")
  requisiciones_papelera(CID, a$id, "dani")
  v <- requisiciones_vista(CID)
  expect_identical(v$etapa[v$id == a$id], "papelera")
  expect_equal(nrow(tabla("requisiciones")), 2)
  expect_s3_class(tryCatch(requisicion_editar(CID, a$id, list(titulo = "x"), "dani"), error = identity), "req_error")
  requisiciones_restaurar(CID, a$id, "dani")
  expect_identical(requisiciones_vista(CID)$etapa, c("sin_provision", "sin_provision"))
  # permission: own vs others
  usr <- function(user, tier) list(user = user, tier = tier, permisos = permisos_resueltos(tier))
  filas <- tabla("requisiciones")
  expect_length(requisiciones_puede_papelera(usr("dani", "compras"), filas), 2)
})

# ── Folders ──────────────────────────────────────────────────────────────────

test_that("carpetas: nombre único, color, presupuesto con moneda, papelera", {
  reiniciar()
  c1 <- requisiciones_carpeta_crear(CID, list(nombre = "Obra norte", color = "#AA3300", presupuesto = 500000, moneda = "mxn"), "dani")
  cf <- tabla("carpetas")
  expect_identical(cf$color, "#aa3300"); expect_identical(cf$moneda, "MXN")
  expect_identical(tryCatch(requisiciones_carpeta_crear(CID, list(nombre = "obra NORTE"), "d"), error = identity)$clave,
                   "requisiciones.error.carpeta_repetida")
  expect_s3_class(tryCatch(requisiciones_carpeta_crear(CID, list(nombre = "X", color = "red"), "d"), error = identity), "req_error")
  expect_s3_class(tryCatch(requisiciones_carpeta_crear(CID, list(nombre = "Y", presupuesto = 10), "d"), error = identity), "req_error")
  r <- requisicion_crear(CID, modifyList(base_req, list(carpeta_id = c1)), "dani")
  res <- requisiciones_carpetas_resumen(tabla("carpetas"), requisiciones_vista(CID))
  expect_equal(res$n_requisiciones, 1); expect_equal(res$comprometido, 15000)
  # Luis, 2026-11-04: a folder with live requisitions cannot go to the trash.
  antes <- tabla("carpetas")
  e <- tryCatch(requisiciones_carpeta_papelera(CID, c1, "dani"), error = identity)
  expect_s3_class(e, "req_error")
  expect_identical(e$clave, "requisiciones.error.carpeta_con_requisiciones")
  expect_identical(tabla("carpetas"), antes)
  # once its requisition is in the trash, it can
  requisiciones_papelera(CID, r$id, "dani")
  requisiciones_carpeta_papelera(CID, c1, "dani")
  expect_false(is.na(tabla("carpetas")$papelera_en))
  expect_true(is.na(requisiciones_vista(CID)$carpeta_nombre))
  expect_s3_class(tryCatch(requisicion_crear(CID, modifyList(base_req, list(carpeta_id = c1)), "d"), error = identity), "req_error")
  requisiciones_carpeta_restaurar(CID, c1, "dani")
  expect_identical(requisiciones_vista(CID)$carpeta_nombre, "Obra norte")
})

test_that("las requisiciones pagadas no cuentan como vivas al mandar una carpeta a la papelera", {
  reiniciar()
  c1 <- requisiciones_carpeta_crear(CID, list(nombre = "Obra sur"), "dani")
  poner_provision("p1"); poner_provision("p2")
  pagada <- requisicion_crear(CID, modifyList(base_req, list(carpeta_id = c1, provision_id = "p1")), "dani")
  en_pago <- requisicion_crear(CID, modifyList(base_req, list(carpeta_id = c1, provision_id = "p2")), "dani")
  poner_confirmacion("p1")
  poner_provision("p2", estado = "converted")
  # the one In payment is still live
  expect_identical(tryCatch(requisiciones_carpeta_papelera(CID, c1, "dani"), error = identity)$clave,
                   "requisiciones.error.carpeta_con_requisiciones")
  requisiciones_mover(CID, en_pago$id, NA, "dani")
  requisiciones_carpeta_papelera(CID, c1, "dani")      # only the paid one is left
  expect_false(is.na(tabla("carpetas")$papelera_en))
  expect_identical(tabla("requisiciones")$carpeta_id[tabla("requisiciones")$id == pagada$id], c1)
})

# ── Comments ─────────────────────────────────────────────────────────────────

test_that("comentarios: avisan en Panorama al creador y a quienes comentaron, nunca al autor; no leídos", {
  reiniciar()
  r <- requisicion_crear(CID, base_req, "dani")
  x <- requisicion_comentar(CID, r$id, "¿Ya tiene fecha?", "caro")
  expect_true(x$aviso_ok)
  av <- avisos_leer(CID)
  expect_equal(nrow(av), 1)
  expect_true(grepl("dani", av$usuarios)); expect_false(grepl("caro", av$usuarios))
  expect_true(grepl(r$folio, av$titulo, fixed = TRUE))
  v <- requisiciones_vista(CID, usuario = "dani")
  expect_equal(v$n_comentarios, 1); expect_equal(v$comentarios_sin_leer, 1)
  expect_equal(requisiciones_vista(CID, usuario = "caro")$comentarios_sin_leer, 0)
  requisiciones_marcar_leido(CID, r$id, "dani")
  expect_equal(requisiciones_vista(CID, usuario = "dani")$comentarios_sin_leer, 0)
  requisicion_comentar(CID, r$id, "Todavía no", "dani")
  av <- avisos_leer(CID)
  expect_true(any(grepl("caro", av$usuarios)))
  expect_equal(nrow(requisicion_comentarios(CID, r$id)), 2)
  expect_s3_class(tryCatch(requisicion_comentar(CID, r$id, strrep("x", 2001), "d"), error = identity), "req_error")
})

test_that("si el aviso falla, el comentario queda guardado", {
  reiniciar()
  r <- requisicion_crear(CID, base_req, "dani")
  orig <- avisos_personales
  avisos_personales <<- function(...) stop("avisos caído")
  on.exit(avisos_personales <<- orig)
  x <- requisicion_comentar(CID, r$id, "hola", "caro")
  expect_false(x$aviso_ok)
  expect_equal(nrow(tabla("comentarios")), 1)
})

# ── Evidence: files ──────────────────────────────────────────────────────────

test_that("un .exe renombrado a .png se rechaza por sus bytes", {
  exe <- c(charToRaw("MZ"), as.raw(0x90), as.raw(rep(0, 60)), charToRaw("This program cannot be run in DOS mode"))
  f <- archivo_temp(exe, "png")
  e <- tryCatch(adjunto_preparar(f, "captura.png", "pegado"), error = identity)
  expect_s3_class(e, "req_error")
  expect_identical(e$clave, "requisiciones.evidencia.error_tipo")
  expect_true(is.na(adjunto_tipo_por_bytes(exe)))
  # an HTML page renamed .pdf, too
  expect_true(is.na(adjunto_tipo_por_bytes(charToRaw("<html><script>alert(1)</script></html>"))))
})

test_that("las imágenes se recodifican en el servidor: lo que sobra en el archivo no llega a S3", {
  carga <- charToRaw("<script>alert('x')</script>")
  b <- c(png_real(), carga)                                # a polyglot: PNG + trailing payload
  f <- archivo_temp(b, "png")
  p <- adjunto_preparar(f, "C:\\Users\\x\\captura\u0007 <b>.png", "pegado")
  expect_identical(p$ext, "png"); expect_true(p$es_imagen)
  expect_equal(c(p$ancho, p$alto), c(40L, 20L))
  expect_length(grepRaw(charToRaw("script"), b, fixed = TRUE), 1)   # it was there...
  expect_length(grepRaw(charToRaw("script"), p$bytes, fixed = TRUE), 0)   # ...and is gone
  expect_identical(p$nombre, "captura <b>.png")            # no path, no control chars; shown escaped
  # JPEG stays JPEG, WEBP becomes PNG
  j <- magick::image_write(magick::image_blank(30, 30, "red"), format = "jpeg")
  expect_identical(adjunto_preparar(archivo_temp(j), "a.jpg")$ext, "jpg")
  w <- magick::image_write(magick::image_blank(30, 30, "red"), format = "webp")
  expect_identical(adjunto_tipo_por_bytes(w), "webp")
  pw <- adjunto_preparar(archivo_temp(w), "a.webp")
  expect_identical(pw$ext, "png"); expect_equal(c(pw$ancho, pw$alto), c(30L, 30L))
})

test_that("una imagen que dice medir 100,000 x 100,000 se rechaza antes de decodificarla", {
  b <- png_real()
  b[17:20] <- as.raw(c(0x00, 0x01, 0x86, 0xA0)); b[21:24] <- as.raw(c(0x00, 0x01, 0x86, 0xA0))
  e <- tryCatch(adjunto_preparar(archivo_temp(b), "bomba.png"), error = identity)
  expect_identical(e$clave, "requisiciones.evidencia.error_imagen")
})

test_that("límites de tamaño: 4 MB pegado, 10 MB archivo; el pegado solo acepta imágenes", {
  grande <- c(charToRaw("%PDF-1.4\n"), as.raw(rep(32, 5 * 1024^2)))
  f <- archivo_temp(grande, "pdf")
  expect_identical(tryCatch(adjunto_preparar(f, "a.pdf", "pegado"), error = identity)$clave,
                   "requisiciones.evidencia.error_tamano")
  expect_identical(adjunto_preparar(f, "a.pdf", "archivo")$ext, "pdf")
  pdf_chico <- archivo_temp(charToRaw("%PDF-1.4\n%%EOF\n"))
  expect_identical(tryCatch(adjunto_preparar(pdf_chico, "a.pdf", "pegado"), error = identity)$clave,
                   "requisiciones.evidencia.error_tipo")
  enorme <- archivo_temp(c(charToRaw("%PDF-1.4\n"), as.raw(rep(32, 10 * 1024^2 + 1))))
  expect_identical(tryCatch(adjunto_preparar(enorme, "a.pdf"), error = identity)$clave,
                   "requisiciones.evidencia.error_tamano")
})

test_that("xlsx se reconoce por dentro; con macros (xlsm renombrado) se rechaza", {
  skip_if_not_installed("openxlsx")
  f <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(data.frame(a = 1:3), f)
  expect_identical(adjunto_tipo_por_bytes(readBin(f, "raw", file.info(f)$size)), "xlsx")
  # same zip plus a macro project
  d <- tempfile(); dir.create(d); utils::unzip(f, exdir = d)
  writeBin(as.raw(1:10), file.path(d, "xl", "vbaProject.bin"))
  m <- tempfile(fileext = ".xlsx")
  ok <- tryCatch({ zip::zipr(m, list.files(d, full.names = TRUE)); TRUE }, error = function(e) FALSE)
  if (ok) expect_true(is.na(adjunto_tipo_por_bytes(readBin(m, "raw", file.info(m)$size))))
  # a plain zip that is not a workbook
  z <- tempfile(fileext = ".zip"); writeLines("hola", file.path(d, "x.txt"))
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zipr(z, file.path(d, "x.txt"))
    expect_true(is.na(adjunto_tipo_por_bytes(readBin(z, "raw", file.info(z)$size))))
  }
})

test_that("adjuntar: llave del servidor, máximo 20 vivos, quitar va a papelera y el archivo se conserva", {
  reiniciar()
  r <- requisicion_crear(CID, base_req, "dani")
  p <- adjunto_preparar(archivo_temp(png_real()), "../../etc/passwd.png", "pegado")
  id <- evidencia_adjuntar(CID, r$id, p, "dani")
  ev <- tabla("evidencia")
  expect_identical(ev$llave, paste0(CID, "/adjuntos/", r$id, "/", id, ".png"))
  expect_identical(ev$nombre, "passwd.png")
  expect_true(exists(ev$llave, envir = .objs))
  expect_identical(evidencia_bytes(CID, id)$bytes, p$bytes)
  pdf <- adjunto_preparar(archivo_temp(charToRaw("%PDF-1.4\n%%EOF\n")), "f.pdf")
  for (i in 2:20) evidencia_adjuntar(CID, r$id, pdf, "dani")
  e <- tryCatch(evidencia_adjuntar(CID, r$id, pdf, "dani"), error = identity)
  expect_identical(e$clave, "requisiciones.evidencia.error_limite_adjuntos")
  evidencia_papelera(CID, id, "dani")
  expect_true(exists(ev$llave, envir = .objs))             # the file stays in S3
  expect_equal(nrow(tabla("evidencia")), 20)              # the row stays too
  expect_equal(nrow(requisicion_evidencia(CID, r$id)), 19)
  evidencia_adjuntar(CID, r$id, pdf, "dani")               # room again
  expect_identical(tryCatch(evidencia_restaurar(CID, id, "dani"), error = identity)$clave,
                   "requisiciones.evidencia.error_limite_adjuntos")
  res <- adjuntos_resumen(requisiciones_vista(CID))
  expect_equal(res$adjuntos, 20); expect_equal(res$imagenes, 0); expect_equal(res$total, 20)
  expect_error(.adj_llave(CID, "../otro", id, "png"), "inválido")
  expect_error(.adj_llave(CID, r$id, id, "exe"), "inválida")
})

# ── Evidence: pasted Excel ───────────────────────────────────────────────────

test_that("celdas pegadas: solo texto, nada se reinterpreta, comillas de Excel", {
  txt <- paste0("Clave\tFecha\tDescripción\tImporte\r\n",
                "007\t01/02\t\"Tubo\nlargo\"\t1,200.50\r\n",
                "010\t2026-03-04\t5\" pulgada\t-3\r\n",
                "011\t\t\"con \"\"comillas\"\"\"\t\r\n")
  t <- tabla_pegada_preparar(txt)
  expect_equal(dim(t$celdas), c(4L, 4L))
  expect_identical(t$celdas[2, ], c("007", "01/02", "Tubo\nlargo", "1,200.50"))
  expect_identical(t$celdas[3, 3], "5\" pulgada")
  expect_identical(t$celdas[4, ], c("011", "", "con \"comillas\"", ""))
  expect_true(t$encabezado_sugerido)
  expect_identical(t$numericas, c(TRUE, FALSE, FALSE, TRUE))
  expect_false(t$recortada); expect_true(is.na(t$aviso))
  expect_false(tabla_pegada_preparar("1\t2\n3\t4")$encabezado_sugerido)
  expect_identical(tryCatch(tabla_pegada_preparar(" \t \n"), error = identity)$clave, "requisiciones.tabla.error_vacia")
})

test_that("una tabla de 340 filas se recorta a 200 y lo dice", {
  filas <- vapply(1:340, function(i) paste(i, "x", sep = "\t"), character(1))
  t <- tabla_pegada_preparar(paste(filas, collapse = "\n"))
  expect_equal(t$filas, 200); expect_equal(t$filas_pegadas, 340); expect_true(t$recortada)
  expect_identical(t$aviso, "Se pegaron 340 filas; se guardaron 200.")
  ancha <- tabla_pegada_preparar(paste(rep("c", 25), collapse = "\t"))
  expect_equal(ancha$columnas, 20); expect_equal(ancha$columnas_pegadas, 25)
  expect_identical(ancha$aviso, "Se pegaron 25 columnas; se guardaron 20.")
})

test_that("=HYPERLINK(...) y compañía se neutralizan al exportar o copiar, sin tocar lo guardado", {
  x <- c("=HYPERLINK(\"http://malo\",\"clic\")", "+1+1", "-2+3", "@SUM(A1)", "normal", "a=b", NA)
  expect_identical(neutralizar_celdas(x),
                   c("'=HYPERLINK(\"http://malo\",\"clic\")", "'+1+1", "'-2+3", "'@SUM(A1)", "normal", "a=b", NA))
  # Luis, 2026-11-04: pure numbers stay numbers; anything else with = + - @ does not
  expect_identical(neutralizar_celdas(c("-150.00", "-1,234.5", "+7", "-.5", "-3")),
                   c("-150.00", "-1,234.5", "+7", "-.5", "-3"))
  expect_identical(neutralizar_celdas(c("-1+2", "-1e5", "- 3", "-", "+A1", "-150.00 MXN", "@1", "=150")),
                   c("'-1+2", "'-1e5", "'- 3", "'-", "'+A1", "'-150.00 MXN", "'@1", "'=150"))
  expect_identical(tabla_a_texto_copiable(matrix(c("-150.00", "-1+2"), 1)), "-150.00\t'-1+2")
  df <- data.frame(t = c("=1", "ok"), n = 1:2, stringsAsFactors = FALSE)
  expect_identical(neutralizar_celdas(df)$t, c("'=1", "ok"))
  t <- tabla_pegada_preparar("=HYPERLINK(\"http://malo\")\tb")
  expect_identical(t$celdas[1, 1], "=HYPERLINK(\"http://malo\")")   # stored as pasted
  expect_identical(tabla_a_texto_copiable(t$celdas), "'=HYPERLINK(\"http://malo\")\tb")
})

test_that("guardar tablas pegadas: ida y vuelta exacta, máximo 10 por requisición", {
  reiniciar()
  r <- requisicion_crear(CID, base_req, "dani")
  t <- tabla_pegada_preparar("Clave\tNombre\n007\tÑandú\n=1\t\"a\tb\"")
  id <- evidencia_tabla_agregar(CID, r$id, t, "Cotización", TRUE, "dani")
  back <- evidencia_tabla(CID, id)
  expect_identical(back$celdas, t$celdas); expect_true(back$encabezado)
  uno <- tabla_pegada_preparar("solo")
  id1 <- evidencia_tabla_agregar(CID, r$id, uno, NA, FALSE, "dani")
  expect_identical(evidencia_tabla(CID, id1)$celdas, matrix("solo", 1, 1))
  for (i in 3:10) evidencia_tabla_agregar(CID, r$id, uno, paste("t", i), FALSE, "dani")
  expect_identical(tryCatch(evidencia_tabla_agregar(CID, r$id, uno, "11", FALSE, "dani"), error = identity)$clave,
                   "requisiciones.evidencia.error_limite_tablas")
  ev <- tabla("evidencia")
  expect_identical(ev$filas_pegadas[ev$id == id], 3L)
  expect_equal(adjuntos_resumen(requisiciones_vista(CID))$tablas, 10)
})

# ── For other fronts ─────────────────────────────────────────────────────────

test_that("requisicion_de_provision es vectorizada y adjuntos_resumen cuenta para el clip", {
  reiniciar()
  poner_provision("p1"); poner_provision("p2")
  a <- requisicion_crear(CID, modifyList(base_req, list(provision_id = "p1")), "dani")
  b <- requisicion_crear(CID, modifyList(base_req, list(provision_id = "p2")), "dani")
  evidencia_adjuntar(CID, a$id, adjunto_preparar(archivo_temp(png_real()), "x.png"), "dani")
  d <- requisicion_de_provision(CID, c("p1", "p2", "p9"))
  expect_setequal(d$provision_id, c("p1", "p2"))
  res <- adjuntos_resumen(d[d$provision_id == "p1", ])
  expect_equal(res$imagenes, 1); expect_false(is.na(res$primera_imagen_id))
  expect_equal(nrow(requisicion_de_provision(CID, "p9")), 0)
  expect_equal(nrow(adjuntos_resumen(d[0, ])), 0)
  expect_identical(unname(requisiciones_etapa_nombre("en_pago")), "En pago")
})

test_that("las tablas de Requisiciones no pierden filas y cada escritura es por cliente", {
  reiniciar()
  requisicion_crear(CID, base_req, "dani")
  requisicion_crear("otro_cliente", base_req, "zoe")
  expect_equal(nrow(tabla("requisiciones")), 1)
  expect_equal(nrow(tabla("requisiciones", "otro_cliente")), 1)
  expect_true(all(grepl(paste0("^(", CID, "|otro_cliente)/requisiciones"), ls(.mem))))
})
