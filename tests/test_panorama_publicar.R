# Panorama › Publicar (front B2): the audience bar, the form's checks, the
# Publicadas list, and the server (who may publish, what gets written, and
# Desactivar). S3 is an in-memory store local to this test (as in
# tests/test_avisos.R); aws.s3 is never patched.
# Run from the repo root: Rscript tests/test_panorama_publicar.R
library(testthat)
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a
.i18n_dir <- file.path(getwd(), "www", "i18n")
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R", "R/avisos.R", "R/hd_registro.R", "R/hd_piezas.R",
            "R/hd_columnas.R", "R/hd_table.R", "R/avisos_alertas_module.R", "R/panorama_datos.R",
            "R/panorama_publicar_module.R"))
  suppressMessages(source(f))

.mem <- new.env()
.s3_read_with <- function(key, client_id = NULL) .mem[[paste0(client_id, "/", key)]]
.s3_leer_para_escribir <- function(key, client_id) .mem[[paste0(client_id, "/", key)]]
.s3_write <- function(obj, key, client_id = NULL) assign(paste0(client_id, "/", key), obj, envir = .mem)
leer_tabla_para_escribir <- function(key, client_id, schema) .mem[[paste0(client_id, "/", key)]] %||% schema()
NIV <- niveles_cliente(); TI <- nivel_ti(); BAJO <- NIV[length(NIV)]
STAFF <- setdiff(niveles_todos(), NIV)[1]
auth_load_usuarios <- function(client_id = NULL) data.frame(
  username = c("ana", "beto", "caro", "soporte"), display_name = c("Ana", "Beto", "Caro", "Soporte"),
  tier = c(TI, NIV[2], BAJO, STAFF), deleted = FALSE, activo = TRUE, stringsAsFactors = FALSE)
registro <- list()
log_action <- function(user, module, action, ...) { registro[[length(registro) + 1L]] <<- action; invisible(NULL) }
CID <- "acme"

# ── The audience bar ─────────────────────────────────────────────────────────
test_that("barra: ampliar, recortar por un extremo, omitir en medio; nunca queda vacía", {
  e <- list(desde = 2L, hasta = 3L, omitidos = integer())
  e <- audiencia_click(e, 5L)                       # outside, below: widens down to it
  expect_equal(c(e$desde, e$hasta), c(2L, 5L))
  e <- audiencia_click(e, 1L)                       # outside, above
  expect_equal(c(e$desde, e$hasta), c(1L, 5L))
  e <- audiencia_click(e, 3L)                       # middle: skipped
  expect_equal(e$omitidos, 3L)
  e <- audiencia_click(e, 4L)                       # another skip: no limit (plan)
  expect_equal(e$omitidos, c(3L, 4L))
  e <- audiencia_click(e, 3L)                       # again: back in
  expect_equal(e$omitidos, 4L)
  e <- audiencia_click(e, 5L)                       # an end: trimmed, and it walks past the skipped 4
  expect_equal(c(e$desde, e$hasta), c(1L, 3L)); expect_length(e$omitidos, 0)
  uno <- list(desde = 2L, hasta = 2L, omitidos = integer())
  expect_identical(audiencia_click(uno, 2L), uno)   # the only one stays
  expect_identical(audiencia_click(uno, NA), uno)
})

test_that("barra: de la preferencia guardada a la selección y de vuelta", {
  sel <- NIV[c(1, 2, 4)]
  e <- audiencia_estado(sel, NIV)
  expect_equal(c(e$desde, e$hasta), c(1L, 4L)); expect_equal(e$omitidos, 3L)
  expect_identical(audiencia_seleccion(e, NIV), sel)
  todos <- audiencia_estado(NULL, NIV)               # first time: every tier
  expect_identical(audiencia_seleccion(todos, NIV), NIV)
  expect_identical(audiencia_estado("no_existe", NIV)$hasta, length(NIV))
})

test_that("barra: el resumen se arma con los nombres del registro", {
  e <- list(desde = 2L, hasta = length(NIV), omitidos = 3L)
  r <- audiencia_resumen(e, NIV, "es")
  expect_identical(r, sprintf("%s a %s, sin %s", nivel_nombre(NIV[2]), nivel_nombre(NIV[length(NIV)]),
                              nivel_nombre(NIV[3])))
  expect_identical(audiencia_resumen(list(desde = 1L, hasta = 1L, omitidos = integer()), NIV), nivel_nombre(NIV[1]))
})

# ── The form's checks ────────────────────────────────────────────────────────
test_that("publicación: lo que no se puede publicar se dice con una clave del diccionario", {
  hoy <- as.Date(Sys.time(), tz = "America/Mexico_City")
  err <- function(...) panorama_publicacion(...)$error
  expect_identical(err("chisme", "t", "", NIV), "panorama.publicar.error.tipo")
  expect_identical(err("comunicado", "  ", "", NIV), "panorama.publicar.error.titulo")
  expect_identical(err("comunicado", strrep("a", 81), "", NIV), "panorama.publicar.error.titulo_largo")
  expect_identical(err("comunicado", "t", strrep("a", 501), NIV), "panorama.publicar.error.mensaje_largo")
  expect_identical(err("recordatorio", "t", "", NIV), "panorama.publicar.error.fecha")
  expect_identical(err("comunicado", "t", "", NIV, vence = hoy - 1), "panorama.publicar.error.fecha_pasada")
  expect_identical(err("comunicado", "t", "", character()), "panorama.publicar.error.niveles")
  # a staff tier is never an audience, even if the browser sends it
  expect_identical(err("comunicado", "t", "", STAFF), "panorama.publicar.error.niveles")
  expect_identical(err("solicitud", "t", "", NIV, responsables = "soporte", vence = hoy, vivos = c("ana", "beto")),
                   "panorama.publicar.error.responsables")
  dic <- jsonlite::fromJSON("www/i18n/es.json")
  claves <- c("tipo", "titulo", "titulo_largo", "mensaje_largo", "fecha", "fecha_pasada", "niveles", "responsables")
  expect_true(all(paste0("panorama.publicar.error.", claves) %in% names(dic)))
})

test_that("publicación: urgente es un comunicado urgente; la solicitud va a personas vivas", {
  hoy <- as.Date(Sys.time(), tz = "America/Mexico_City")
  u <- panorama_publicacion("urgente", "Corte", "", NIV[1:2], enlace = "BNC")
  expect_true(u$ok)
  expect_identical(u$aviso[c("tipo", "clase", "severidad", "enlace")],
                   list(tipo = "publicado", clase = "comunicado", severidad = "urgente", enlace = "BNC"))
  expect_identical(u$aviso$niveles, NIV[1:2])
  x <- panorama_publicacion("comunicado", "t", "", NIV, enlace = "javascript:alert(1)")
  expect_true(is.na(x$aviso$enlace))                 # only known tabs
  s <- panorama_publicacion("solicitud", "Soporte", "", NIV, responsables = c("Beto", "soporte", "beto"),
                            vence = hoy, vivos = c("ana", "beto"))
  expect_true(s$ok); expect_identical(s$responsables, "beto"); expect_identical(s$aviso$tipo, "personal")
})

# ── The server ───────────────────────────────────────────────────────────────
sesion <- function(nivel, user, guion) {
  usr <- list(user = user, tier = nivel, permisos = permisos_resueltos(nivel))
  shared <- list(effective_client_id = function() CID, home_client_id = function() CID,
                 current_user = function() user, usuario = reactiveVal(usr),
                 current_user_info = function() list(user = user, is_staff = FALSE))
  out <- NULL
  suppressWarnings(suppressMessages(testServer(panoramaPublicarServer, args = list(shared = shared), {
    session$flushReact()
    out <- guion(session, function() publicadas())
  })))
  invisible(out)
}

test_that("sin can_publish_panorama no se escribe nada, aunque se fuerce el botón", {
  rm(list = ls(.mem), envir = .mem); registro <<- list()
  sin <- Filter(function(t) !puede(list(tier = t, permisos = permisos_resueltos(t)), "can_publish_panorama"), NIV)[1]
  skip_if(is.na(sin), "every client tier may publish")
  sesion(sin, "caro", function(session, pub) {
    session$setInputs(titulo = "Hola", mensaje = "", vence = Sys.Date() + 3, enlace = "")
    session$setInputs(publicar = 1)
  })
  expect_null(.mem[[paste0(CID, "/", .AVISOS_KEY)]])
  expect_length(registro, 0)
})

test_that("con permiso: publica para los niveles de la barra, guarda la elección y la Desactiva", {
  rm(list = ls(.mem), envir = .mem); registro <<- list()
  sesion(TI, "ana", function(session, pub) {
    session$flushReact()
    session$setInputs(nivel = 3L)                     # skips tier 3 (a middle one)
    session$setInputs(titulo = "Cierre de mes", mensaje = "Conciliar antes del jueves",
                      vence = Sys.Date() + 3, enlace = "BNC")
    session$setInputs(publicar = 1)
    session$flushReact()
    expect_equal(nrow(pub()), 1)
    tabla_ids <- pub()$id
    # Desactivar through the table's action
    session$setInputs(`tabla-accion` = list(accion = "desactivar", ids = as.list(tabla_ids), nonce = 1))
    session$flushReact()
  })
  df <- avisos_leer(CID)
  expect_equal(nrow(df), 1)
  expect_identical(.pan_hechos_raw(df$niveles), NIV[-3])
  expect_identical(df$clase, "comunicado"); expect_identical(df$enlace, "BNC")
  expect_identical(prefs_leer(CID, "ana")$audiencia, NIV[-3])
  expect_true("publicar" %in% unlist(registro))
  expect_true(df$retirado)                             # Desactivar ended it, the row stays
  expect_true("desactivar_aviso" %in% unlist(registro))
})

test_that("una solicitud: una fila por persona, una fila en Publicadas, y Desactivar las cierra todas", {
  rm(list = ls(.mem), envir = .mem)
  avisos_personales(CID, c("beto", "caro"), list(tipo = "personal", clase = "solicitud", titulo = "Soporte",
                                                 clave = "sol:abc", autor = "ana",
                                                 vence = format(Sys.time() + 86400, tz = "UTC", usetz = TRUE)))
  p <- panorama_publicadas(avisos_leer(CID))
  expect_equal(nrow(p), 1); expect_identical(p$id, "sol:abc"); expect_identical(p$para, "beto, caro")
  expect_length(strsplit(p$ids, ",")[[1]], 2)
  for (i in strsplit(p$ids, ",")[[1]]) avisos_retirar(CID, i, "ana")
  expect_equal(nrow(panorama_publicadas(avisos_leer(CID))), 0)
})
