# Avisos and preferences contracts (R/avisos.R).
# S3 is replaced by an in-memory store local to this test: only the two
# persistence helpers avisos.R calls are redefined, in this script's own
# global env. aws.s3 itself is never patched.
# Run from the repo root: Rscript tests/test_avisos.R
library(testthat)
suppressPackageStartupMessages(library(shiny))
`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/i18n.R")
source("R/tier_registry.R")
source("R/permisos.R")
source("R/avisos.R")

.mem <- new.env()
.s3_preload_cache <- new.env(); .s3_missing_cache <- new.env()
.s3_key <- function(key, client_id = NULL) paste0(client_id, "/", key)
.s3_read_with <- function(key, client_id = NULL) .mem[[paste0(client_id, "/", key)]]
.fallar_escrituras <- 0L
.s3_write <- function(obj, key, client_id = NULL) {
  if (.fallar_escrituras > 0L) { .fallar_escrituras <<- .fallar_escrituras - 1L; stop("simulada") }
  assign(paste0(client_id, "/", key), obj, envir = .mem)
}
auth_load_usuarios <- function(client_id = NULL) data.frame(
  username = c("ana", "beto", "caro", "dani", "eli"),
  tier = c(nivel_ti(), "admin", "finance", "compras", "compras"),
  deleted = c(FALSE, FALSE, FALSE, FALSE, TRUE), activo = TRUE, stringsAsFactors = FALSE)

usr <- function(user, tier) list(user = user, tier = tier, permisos = permisos_resueltos(tier))
CID <- "cliente_prueba"

test_that("sin client_id falla cerrado", {
  expect_error(avisos_leer(NULL), "client_id")
  expect_error(avisos_agregar("", list(titulo = "x")), "client_id")
  expect_error(prefs_guardar(NULL, "ana", list(idioma = "en")), "client_id")
})

test_that("un aviso publicado llega solo a los niveles elegidos", {
  avisos_agregar(CID, list(tipo = "publicado", titulo = "Cierre de mes",
                           niveles = c("admin", "compras")), por = "ana")
  expect_equal(nrow(avisos_visibles(CID, usr("beto", "admin"))), 1)
  expect_equal(nrow(avisos_visibles(CID, usr("dani", "compras"))), 1)
  expect_equal(nrow(avisos_visibles(CID, usr("caro", "finance"))), 0)
  expect_error(avisos_agregar(CID, list(tipo = "publicado", titulo = "x", niveles = character())),
               "al menos un nivel")
  expect_error(avisos_agregar(CID, list(tipo = "otro")), "tipo desconocido")
})

test_that("un aviso automático con permiso solo lo ve quien lo tiene", {
  avisos_agregar(CID, list(tipo = "automatico", titulo = "Pago confirmado",
                           permiso = "can_confirm_payments"))
  vis <- function(u) any(avisos_visibles(CID, u)$titulo == "Pago confirmado")
  expect_true(vis(usr("caro", "finance")))
  expect_false(vis(usr("dani", "compras")))
})

test_that("los avisos personales van a personas, nunca al autor, y se agrupan", {
  avisos_personales(CID, c("ana", "dani", "beto"),
                    list(titulo = "Comentario nuevo en REQ-2026-0014", clave = "req:14", autor = "beto"))
  avisos_personales(CID, c("ana", "dani"),
                    list(titulo = "3 comentarios nuevos en REQ-2026-0014", clave = "req:14", autor = "caro"))
  df <- avisos_leer(CID)
  pers <- df[df$tipo == "personal", ]
  expect_equal(nrow(pers), 2)                                 # ana y dani, una vez cada uno
  expect_false(any(grepl("beto", pers$usuarios)))            # nunca al autor
  expect_equal(pers$contador, c(2L, 2L))
  expect_true(all(pers$titulo == "3 comentarios nuevos en REQ-2026-0014"))
  # a personal aviso is invisible to someone else of the same tier
  expect_false(any(avisos_visibles(CID, usr("eli", "compras"))$tipo == "personal"))
})

test_that("retirar y marcar hecho", {
  id <- avisos_agregar(CID, list(tipo = "publicado", titulo = "Temporal", niveles = "finance"))
  avisos_marcar_hecho(CID, id, "caro")
  expect_true("caro" %in% .de_json(avisos_leer(CID)$hechos[avisos_leer(CID)$id == id]))
  avisos_retirar(CID, id, por = "ana")
  expect_false(id %in% avisos_visibles(CID, usr("caro", "finance"))$id)
  expect_true(id %in% avisos_leer(CID)$id)                   # nada se borra
})

test_that("cada escritura parte de lo que hay en S3 (no pisa a otra sesión)", {
  antes <- nrow(avisos_leer(CID))
  # Another session writes directly between our read and our write.
  otra <- avisos_leer(CID)
  otra <- dplyr::bind_rows(otra, tibble::tibble(id = "de-otra-sesion", tipo = "automatico",
                                               titulo = "otra", niveles = .json(niveles_cliente()),
                                               retirado = FALSE))
  assign(paste0(CID, "/", .AVISOS_KEY), otra, envir = .mem)
  avisos_agregar(CID, list(titulo = "mía"))
  df <- avisos_leer(CID)
  expect_true("de-otra-sesion" %in% df$id)
  expect_equal(nrow(df), antes + 2)
})

test_that("una escritura que falla una vez se reintenta sobre lo fresco", {
  .fallar_escrituras <<- 1L
  id <- avisos_agregar(CID, list(titulo = "tras reintento"))
  expect_true(id %in% avisos_leer(CID)$id)
  .fallar_escrituras <<- 2L
  expect_error(avisos_agregar(CID, list(titulo = "nunca")), "no se pudo guardar")
  .fallar_escrituras <<- 0L
})

test_that("avisos_actividad nunca lanza error", {
  .fallar_escrituras <<- 5L
  expect_silent(suppressMessages(avisos_actividad(CID, "confirmar_pago", "caro")))
  .fallar_escrituras <<- 0L
  expect_null(suppressMessages(avisos_actividad(NULL, "confirmar_pago", "caro")))
})

test_that("audiencia: niveles del registro con su gente viva", {
  a <- audiencia_niveles(CID)
  expect_identical(a$nivel, niveles_cliente())
  expect_equal(a$personas[a$nivel == "compras"], 1L)         # eli está borrada
  expect_equal(a$personas[a$nivel == nivel_ti()], 1L)
})

test_that("preferencias por usuario, sin el usuario en la ruta", {
  expect_identical(prefs_leer(CID, "ana")$idioma, "es")
  prefs_guardar(CID, "ana", list(idioma = "en", audiencia = c("admin", "compras")))
  prefs_guardar(CID, "beto", list(inicio = "PSV"))
  expect_identical(prefs_leer(CID, "ana")$idioma, "en")
  expect_identical(prefs_leer(CID, "ANA")$audiencia, c("admin", "compras"))
  expect_identical(prefs_leer(CID, "beto")$inicio, "PSV")
  expect_identical(prefs_leer(CID, "beto")$idioma, "es")
  expect_false(any(grepl("ana|beto", ls(.mem))))
})
