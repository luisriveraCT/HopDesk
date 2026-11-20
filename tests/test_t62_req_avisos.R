# =============================================================================
# tests/test_t62_req_avisos.R
# T62: a requisition's events (creada, cancelada, restaurada) reach, as
# avisos personales, the people who chose them (req_avisos: ninguna | mias |
# mias_y | todas, people by account_code), and Panorama shows them under the
# "Requisiciones" chip. In-memory S3 as in tests/test_requisiciones_*.R
# (aws.s3 is never patched). The integrator's conditions:
#   1 avisos_agregar() without `repetir` is master's; repetir = FALSE leaves a
#     clave already there (live, retired, expired) as it is;
#   2 published after the write is in S3; a failing notice never changes what
#     the action returns, in the three actions;
#   3 unreadable preferences or users: nothing is published (never "everyone");
#   4 recipients by puede() and account_code; never the author; nobody
#     deactivated or deleted;
#   5 the clave is the CM35 event id, built in one place;
#   6 crear_requisicion goes to avisos_actividad() after the write.
# Run from the repo root:  Rscript tests/test_t62_req_avisos.R
# =============================================================================
library(testthat)
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a
.tomar <- function(archivo, nombres, env = globalenv()) {
  ex <- if (is.character(archivo) && length(archivo) == 1L && file.exists(archivo)) parse(archivo, keep.source = FALSE)
        else parse(text = archivo, keep.source = FALSE)
  for (e in ex)
    if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]])[1] %in% nombres) eval(e, env)
}
.tomar("R/global.R", "S3_KEYS")
.tomar("R/persistence.R", c(".normalize", "leer_tabla_para_escribir", ".s3_leer_para_escribir",
                            "modificar_tabla", "rebase_por_id", "confirmaciones_vigentes", ".schema_manual",
                            ".schema_empresas"))
.tomar("R/bancos_persistence.R", ".schema_bancos_confirmados")
.tomar("R/pasivos_persistence.R", c("etapa_de_provision_en", "PROVISION_PAGADA"))
source("R/pasivos_schemas.R"); source("R/i18n.R"); source("R/tier_registry.R"); source("R/permisos.R")
source("R/avisos.R"); source("R/adjuntos_s3.R"); source("R/empresas_identidad.R")
.tomar("R/panorama_datos.R", c("REQ_AVISOS_MODOS", "req_avisos_pref", "req_avisos_personas", "PANORAMA_TIPOS_FEED",
                               "avisos_clase", "panorama_tipo_feed"))
source("R/agenda_filas.R")   # requisiciones_eventos_en() reads the Agenda through agenda_pendientes() (CM35)
source("R/requisiciones_persistence.R")
.tomar("R/avisos_alertas_module.R", c(".ALERTAS_KEY", "ALERTAS_COLUMNAS", "ALERTAS_VENTANA_MIN", "ALERTAS_ACCIONES",
                                      ".schema_avisos_alertas", ".alertas_cid", "alertas_leer", "alertas_preparar"))

# ── In-memory S3 ─────────────────────────────────────────────────────────────
.mem <- new.env(); .s3_preload_cache <- new.env(); .s3_missing_cache <- new.env()
.no_leer <- character()            # keys whose read fails (strict: an error, never "empty")
.s3_key <- function(key, client_id = NULL) {
  if (is.null(client_id) || !nzchar(client_id)) stop("sin client_id en .s3_key")
  paste0(tolower(client_id), "/", key)
}
.s3_read_with <- function(key, client_id = NULL) {
  if (key %in% .no_leer) return(NULL)
  .mem[[.s3_key(key, client_id)]]
}
.s3_existe <- function(key, client_id) {
  if (key %in% .no_leer) return(NA)
  exists(.s3_key(key, client_id), envir = .mem, inherits = FALSE)
}
.s3_write <- function(obj, key, client_id = NULL, permitir_reduccion = FALSE) assign(.s3_key(key, client_id), obj, envir = .mem)
CID <- "cliente_t62"
reiniciar <- function() { rm(list = ls(.mem), envir = .mem); .no_leer <<- character() }
tabla <- function(key, cid = CID) .mem[[paste0(cid, "/", key)]]
avisos <- function() .normalizar(tabla(.AVISOS_KEY), .schema_avisos)

TOP <- niveles_cliente()[1]
usuario <- function(user, code, tier = TOP, ver = TRUE, deleted = FALSE, activo = TRUE)
  data.frame(username = user, account_code = code, tier = tier, deleted = deleted, activo = activo,
             permisos = sprintf('{"can_view_requisiciones": %s}', tolower(ver)), stringsAsFactors = FALSE)
poner_usuarios <- function(...) assign(paste0(CID, "/", S3_KEYS$usuarios), rbind(...), envir = .mem)
poner_pref <- function(user, modo, personas = character())
  prefs_guardar(CID, user, list(req_avisos = list(modo = modo, personas = personas)))
gente <- function() poner_usuarios(usuario("ana", "U0001"), usuario("beto", "U0002"), usuario("caro", "U0003"),
                                   usuario("dani", "U0004"), usuario("eli", "U0005", ver = FALSE),
                                   usuario("fer", "U0006", activo = FALSE), usuario("gabo", "U0007", deleted = TRUE))
base_req <- list(titulo = "Cable para la bodega", empresa = "NCS", proveedor = "Amazon",
                 monto = 2000, moneda = "MXN", notas = "")
para <- function(user) { a <- avisos(); a[vapply(a$usuarios, function(u) user %in% .de_json(u), logical(1)), , drop = FALSE] }

# ── 1. avisos_agregar(): repetir ─────────────────────────────────────────────
test_that("1: sin repetir (y con repetir = TRUE) avisos_agregar es el de master; con FALSE no refresca ni revive", {
  maestro <- new.env()
  src <- tryCatch(system2("git", c("show", "777c655:R/avisos.R"), stdout = TRUE, stderr = FALSE), error = function(e) NULL)
  expect_true(length(src) > 0)
  .tomar(paste(src, collapse = "\n"), "avisos_agregar", maestro)
  environment(maestro$avisos_agregar) <- globalenv()
  sin_tiempo <- function(df) df[, setdiff(names(df), c("id", "creado", "actualizado"))]
  correr <- function(f, extra) { reiniciar()
    a <- c(list(tipo = "automatico", titulo = "x", clave = "k1", niveles = niveles_cliente()), extra)
    f(CID, a); f(CID, a); sin_tiempo(avisos()) }
  m <- correr(maestro$avisos_agregar, list())
  expect_identical(correr(avisos_agregar, list()), m)
  expect_identical(correr(avisos_agregar, list(repetir = TRUE)), m)
  expect_identical(m$contador, 2L)

  reiniciar()
  a <- list(tipo = "personal", usuarios = "ana", titulo = "t", clave = "k2", repetir = FALSE)
  id1 <- avisos_agregar(CID, a); avisos_marcar_hecho(CID, id1, "ana"); antes <- avisos()
  id2 <- avisos_agregar(CID, a)
  expect_identical(id2, id1); expect_identical(nrow(avisos()), 1L)
  expect_identical(avisos()$contador, 1L); expect_identical(avisos()$hechos, antes$hechos)

  avisos_retirar(CID, id1, "ana")
  avisos_agregar(CID, a)
  expect_identical(nrow(avisos()), 1L); expect_true(isTRUE(avisos()$retirado))
  reiniciar()
  avisos_agregar(CID, c(a, list(vence = Sys.time() - 3600)))
  avisos_agregar(CID, a)
  expect_identical(nrow(avisos()), 1L)
})

# ── 4. Destinatarios ─────────────────────────────────────────────────────────
test_that("4: destinatarios por modo, con puede() y por account_code; nunca el autor, ni desactivados ni borrados", {
  reiniciar(); gente()
  poner_pref("ana", "ninguna"); poner_pref("beto", "todas"); poner_pref("caro", "mias_y", c("U0004"))
  # dani: no preference -> mias.
  u <- tabla(S3_KEYS$usuarios); pr <- tabla(.PREFERENCIAS_KEY)
  d <- function(interesados, autor, de_quien) sort(req_avisos_destinatarios(u, NULL, pr, interesados, autor, de_quien))
  expect_identical(d(c("ana", "dani"), "ana", c("ana")), c("beto", "dani"))          # ana: autor and ninguna
  expect_identical(d(character(), "dani", c("dani")), c("beto", "caro"))            # caro follows U0004 = dani
  expect_identical(d(character(), "beto", c("beto")), character())                   # beto is the author; nobody else
  expect_false("eli" %in% d(c("eli"), "ana", "ana"))                                  # no can_view_requisiciones
  poner_pref("fer", "todas"); poner_pref("gabo", "todas")
  pr <- tabla(.PREFERENCIAS_KEY)
  expect_false(any(c("fer", "gabo") %in% req_avisos_destinatarios(u, NULL, pr, character(), "ana", "ana")))
  # mias_y is by account_code: a username in `personas` matches nobody.
  poner_pref("caro", "mias_y", c("dani")); pr <- tabla(.PREFERENCIAS_KEY)
  expect_false("caro" %in% req_avisos_destinatarios(u, NULL, pr, character(), "dani", "dani"))
})

test_that("req_avisos_pref: un modo raro es 'mias'; personas solo con mias_y, únicas y sin vacíos", {
  expect_identical(req_avisos_pref(NULL), list(modo = "mias", personas = character()))
  expect_identical(req_avisos_pref(list(modo = "otro", personas = "U1")), list(modo = "mias", personas = character()))
  expect_identical(req_avisos_pref(list(modo = "mias_y", personas = c("U1", "U1", "", NA))),
                   list(modo = "mias_y", personas = "U1"))
  expect_identical(req_avisos_pref(list(modo = "todas", personas = "U1"))$personas, character())
})

# ── 2, 5, 6. Publicar: después de escribir, clave = id del evento ────────────
test_that("2/5/6: crear, cancelar y restaurar publican después de escribir, con la clave del evento", {
  reiniciar(); gente(); poner_pref("beto", "todas")
  en_s3 <- NULL; act <- NULL
  orig_p <- avisos_personales
  assign("avisos_personales", function(cid, usuarios, aviso) {
    en_s3 <<- c(en_s3, nrow(.normalize(tabla(REQ_KEYS$requisiciones), .REQ_SCHEMAS$requisiciones)) > 0)
    orig_p(cid, usuarios, aviso) }, envir = globalenv())
  orig_a <- avisos_actividad
  assign("avisos_actividad", function(cid, accion, usuario, params = list()) {
    act <<- c(act, paste(accion, nrow(.normalize(tabla(REQ_KEYS$requisiciones), .REQ_SCHEMAS$requisiciones))))
    invisible(NULL) }, envir = globalenv())
  on.exit({ assign("avisos_personales", orig_p, envir = globalenv()); assign("avisos_actividad", orig_a, envir = globalenv()) })
  r <- requisicion_crear(CID, base_req, "ana")
  expect_true(all(en_s3))                                   # the requisition was already in S3
  expect_identical(act, "crear_requisicion 1")              # 6: after the write
  h <- .normalize(tabla(REQ_KEYS$historial), .REQ_SCHEMAS$historial)
  hid <- h$id[h$accion == "creada"]
  a <- para("beto")
  expect_identical(a$clave, paste0(req_evento_clave(hid, "creada"), "|beto"))   # 5: the CM35 event id (+ |user)
  expect_identical(a$enlace, "REQ"); expect_true(grepl(r$folio, a$titulo, fixed = TRUE))
  expect_false(grepl("2000|2,000", a$clave))                # no amounts in the clave
  requisiciones_papelera(CID, r$id, "ana"); requisiciones_restaurar(CID, r$id, "ana")
  expect_identical(nrow(para("beto")), 3L)
  # R1: each aviso's clave IS the id requisiciones_eventos_en() (CM35) gives the
  # same history row: creada -> sin_provision, papelera, restaurada.
  h  <- .normalize(tabla(REQ_KEYS$historial), .REQ_SCHEMAS$historial)
  ev <- requisiciones_eventos_en(.normalize(tabla(REQ_KEYS$requisiciones), .REQ_SCHEMAS$requisiciones), h,
                                 audit = NULL, conf = NULL, agenda = NULL)
  for (acc in c("creada", "papelera", "restaurada")) {
    hid <- h$id[h$accion == acc]
    id_cm35 <- ev$id[ev$fuente == "requisiciones" & grepl(hid, ev$id, fixed = TRUE)]
    expect_identical(req_evento_clave(hid, acc), id_cm35)
    expect_true(paste0(id_cm35, "|beto") %in% para("beto")$clave)
  }
  expect_identical(req_evento_clave("H1", "creada"), "requisiciones|H1|sin_provision")
  expect_identical(nrow(para("ana")), 0L)                   # the author never
  expect_identical(panorama_tipo_feed(para("beto")), rep("req", 3L))
})

test_that("2: si el aviso falla, las tres acciones devuelven lo mismo que hoy y no hay error", {
  for (falla in c("avisos", "personales")) {
    reiniciar(); gente(); poner_pref("beto", "todas")
    if (falla == "avisos") .no_leer <<- .AVISOS_KEY
    orig <- avisos_personales
    if (falla == "personales") assign("avisos_personales", function(...) stop("S3 caído"), envir = globalenv())
    r <- expect_no_error(requisicion_crear(CID, base_req, "ana"))
    expect_named(r, c("id", "folio"))
    expect_identical(withVisible(requisiciones_papelera(CID, r$id, "ana"))$value, r$id)
    expect_identical(withVisible(requisiciones_restaurar(CID, r$id, "ana"))$value, r$id)
    assign("avisos_personales", orig, envir = globalenv())
    expect_true(is.na(.normalize(tabla(REQ_KEYS$requisiciones), .REQ_SCHEMAS$requisiciones)$papelera_en))
  }
})

test_that("3: preferencias o usuarios ilegibles: no se publica (nunca 'a todos')", {
  for (k in c(.PREFERENCIAS_KEY, S3_KEYS$usuarios)) {
    reiniciar(); gente(); poner_pref("beto", "todas")
    .no_leer <<- k
    expect_message(requisicion_crear(CID, base_req, "ana"), "no se publicó")
    expect_identical(nrow(avisos()), 0L)
  }
})

test_that("reintentar no duplica: el mismo evento publicado dos veces deja un aviso por persona", {
  reiniciar(); gente(); poner_pref("beto", "todas")
  r <- requisicion_crear(CID, base_req, "ana")
  h <- .normalize(tabla(REQ_KEYS$historial), .REQ_SCHEMAS$historial)
  .req_publicar_evento(CID, r$id, h$id[h$accion == "creada"], "creada", "ana")
  expect_identical(nrow(para("beto")), 1L); expect_identical(para("beto")$contador, 1L)
})

# ── CM35: each event's id ends in ITS own stage ──────────────────────────────
test_that("CM35: en un lote mezclado, cada id de evento termina en su propia etapa (no en la del primero)", {
  reqs <- .normalize(tibble::tibble(id = "R1", folio = "REQ-1", titulo = "t", empresa = "NCS",
                                    creado_por = "ana", creado = as.POSIXct("2026-11-19 10:00", tz = "UTC")),
                     .REQ_SCHEMAS$requisiciones)
  h <- .normalize(tibble::tibble(id = c("H1", "H2", "H3"), requisicion_id = "R1",
                                 accion = c("creada", "papelera", "restaurada"), detalle = NA_character_, por = "ana",
                                 en = as.POSIXct(c("2026-11-19 10:00", "2026-11-19 11:00", "2026-11-19 12:00"), tz = "UTC")),
                  .REQ_SCHEMAS$historial)
  ev <- requisiciones_eventos_en(reqs, h, audit = NULL, conf = NULL, agenda = NULL)
  expect_identical(sort(ev$id), sort(c("requisiciones|H1|sin_provision", "requisiciones|H2|papelera",
                                       "requisiciones|H3|restaurada")))
  expect_identical(sort(ev$id), sort(req_evento_clave(h$id, h$accion)))
})

# ── Pagada y pago anulado (con los eventos de la CM35) ──────────────────────
poner_provision <- function(id) {
  k <- paste0(CID, "/", S3_KEYS$pasivos_provisions)
  p <- .normalize(.mem[[k]], .schema_pasivos_provision)
  assign(k, dplyr::bind_rows(p, tibble::tibble(id = id, estado = "provisional", amount_pago = 2000, empresa = "NCS",
                                               parte = "AMAZON", moneda_pago = "MXN",
                                               fecha_efectiva = as.Date("2026-11-21"))), envir = .mem)
}
poner_confirmacion <- function(conf_id, provision_id, por, anulada_por = NA_character_) {
  k <- paste0(CID, "/", S3_KEYS$bancos_confirmados)
  c <- .normalize(.mem[[k]], .schema_bancos_confirmados)
  c <- c[!c$confirmacion_id %in% conf_id, , drop = FALSE]
  assign(k, dplyr::bind_rows(c, tibble::tibble(
    confirmacion_id = conf_id, provision_id = provision_id, importe = 2000, moneda = "MXN", action = "confirmed",
    confirmado_at = as.POSIXct("2026-11-19 18:00", tz = "UTC"), confirmado_por = por,
    eliminado = !is.na(anulada_por), eliminado_por = anulada_por,
    eliminado_at = if (is.na(anulada_por)) as.POSIXct(NA, tz = "UTC") else as.POSIXct("2026-11-19 19:00", tz = "UTC"))),
    envir = .mem)
}
safe_load_pagar_hoy <- function(...) NULL   # requisiciones_eventos() reads the Agenda: none here

test_that("pagada y anulada: a quien eligió la requisición, con la clave = id del evento de la CM35", {
  reiniciar(); gente(); poner_provision("P1")
  r <- requisicion_crear(CID, c(base_req, list(provision_id = "P1")), "ana")
  poner_confirmacion("C1", "P1", "beto")
  expect_true(req_publicar_pagos(CID, "C1", "beto"))
  ev <- requisiciones_eventos(CID)
  id_pago <- ev$id[ev$fuente == "bancos" & ev$detalle == "confirmada"]
  expect_identical(length(id_pago), 1L)
  a <- para("ana")                                   # ana created it: "mias"
  expect_true(paste0(id_pago, "|ana") %in% a$clave)
  expect_true(any(grepl("beto pagó", a$titulo, fixed = TRUE) & grepl(r$folio, a$titulo, fixed = TRUE)))
  expect_identical(nrow(para("beto")[grepl("^bancos", para("beto")$clave), ]), 0L)   # the author never

  poner_confirmacion("C1", "P1", "beto", anulada_por = "beto")
  req_publicar_pagos(CID, "C1", "beto")
  ev <- requisiciones_eventos(CID)
  id_anul <- ev$id[ev$fuente == "bancos" & ev$detalle == "anulada"]
  expect_true(paste0(id_anul, "|ana") %in% para("ana")$clave)
  n <- nrow(avisos()); req_publicar_pagos(CID, "C1", "beto")
  expect_identical(nrow(avisos()), n)                # published again: nothing new
})

test_that("pagos: una confirmación que no es de una requisición no avisa; una lectura fallida tampoco", {
  reiniciar(); gente(); poner_pref("beto", "todas"); poner_provision("P9")
  poner_confirmacion("C9", "P9", "ana")              # P9 belongs to no requisition
  req_publicar_pagos(CID, "C9", "ana")
  expect_identical(nrow(avisos()), 0L)
  poner_provision("P1"); requisicion_crear(CID, c(base_req, list(provision_id = "P1")), "ana")
  n <- nrow(avisos()); poner_confirmacion("C1", "P1", "ana")
  .no_leer <<- .PREFERENCIAS_KEY
  expect_message(req_publicar_pagos(CID, "C1", "ana"), "no se publicó")
  expect_identical(nrow(avisos()), n)
})

test_that("el aviso de un comentario enlaza la pestaña Requisiciones (REQ): el feed solo enlaza valores de pestaña", {
  reiniciar(); gente()
  r <- requisicion_crear(CID, base_req, "ana")
  requisicion_comentar(CID, r$id, "¿Ya llegó?", "beto")
  a <- para("ana")
  a <- a[grepl("^req:", a$clave), , drop = FALSE]
  expect_identical(nrow(a), 1L)
  expect_identical(a$enlace, "REQ")
  expect_true(grepl("^[A-Z]{2,6}$", a$enlace))        # what panorama_module.R's feed links
})

test_that("Panorama: el chip Requisiciones existe y un comentario de requisición también cae ahí", {
  expect_true("req" %in% PANORAMA_TIPOS_FEED)
  df <- data.frame(tipo = c("personal", "personal", "automatico"), clase = NA_character_, severidad = "info",
                   clave = c("requisiciones|h|estatus|beto", "req:abc|beto", "act|confirmar_pago|ana"),
                   stringsAsFactors = FALSE)
  expect_identical(panorama_tipo_feed(df), c("req", "req", "act"))
  u <- rbind(usuario("ana", "U0001"), usuario("x", NA), usuario("fer", "U0006", activo = FALSE))
  expect_identical(req_avisos_personas(u)$id, "U0001")
})
