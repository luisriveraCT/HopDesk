# =============================================================================
# tests/test_init_cliente_no_pisa.R
# CM25 (PENDIENTES §2). initialize_client_folder() (R/persistence.R) wrote
# the five starting files of a new client -- usuarios, empresas, grupos,
# contacts, app_audit -- with s3saveRDS, without looking first. The approval
# (tiers_module.R) only checks the client registry: a folder that exists
# without a registry row (a registry row removed, a client from before the
# registry, an id used again) had its accounts and companies replaced by
# empty tables. Now (the integrador's "A with a safe retry"):
#   1  a new client: the five files are created, empty;
#   2  a folder with data (any of the five with rows): it stops
#      (hd_init_con_datos), names them, and writes NOTHING;
#   3  a file that can't be checked -- 403, HEAD failing like the network, or
#      there but unreadable: it stops (hd_init_no_comprobada), writes NOTHING;
#   4  what a failed approval leaves (some files there, all empty): those are
#      not rewritten, only the missing ones are created (the approval can be
#      tried again);
#   5  the approval shows the notice of each class (tiers_module.R), and both
#      keys exist in es and en.
# Run from project root:  Rscript tests/test_init_cliente_no_pisa.R
# =============================================================================
suppressPackageStartupMessages(library(testthat))
source("tests/helpers/s3_memoria.R")

ARCHIVOS <- c("usuarios.rds", "empresas.rds", "grupos.rds", "contacts.rds", "app_audit.rds")
llave <- function(cid, a) paste0(cid, "/", a)
en_mem <- function(k) if (exists(k, envir = .mem, inherits = FALSE)) get(k, envir = .mem) else NULL
limpiar <- function() {
  rm(list = ls(.mem, all.names = TRUE), envir = .mem)
  .mem_prohibido <<- character(); .mem_falla_cabeza <<- character(); .mem_falla <<- character()
  .mem_escrituras <<- 0L
}
iniciar <- function(cid) suppressMessages(initialize_client_folder(cid, created_by = "prueba"))
# The condition it stops with (NULL if it did not stop).
condicion <- function(cid) tryCatch({ iniciar(cid); NULL }, error = function(e) e)

test_that("1 a new client: the five files, empty", {
  limpiar()
  iniciar("nuevo")
  for (a in ARCHIVOS) {
    x <- en_mem(llave("nuevo", a))
    expect_false(is.null(x), info = a)
    expect_identical(nrow(as.data.frame(x)), 0L, info = a)
  }
  expect_identical(.mem_escrituras, 5L)
})

test_that("2 a folder with data: it stops, names what has data, and touches nothing", {
  limpiar()
  usuarios <- data.frame(username = "ana", deleted = FALSE, stringsAsFactors = FALSE)
  empresas <- data.frame(iniciales = c("NL", "NG"), stringsAsFactors = FALSE)
  assign(llave("viejo", "usuarios.rds"), usuarios, envir = .mem)
  assign(llave("viejo", "empresas.rds"), empresas, envir = .mem)
  e <- condicion("viejo")
  expect_s3_class(e, "hd_init_con_datos")
  expect_match(conditionMessage(e), "usuarios.rds", fixed = TRUE)
  expect_identical(sort(e$archivos), c("empresas.rds", "usuarios.rds"))
  expect_identical(en_mem(llave("viejo", "usuarios.rds")), usuarios)
  expect_identical(en_mem(llave("viejo", "empresas.rds")), empresas)
  expect_identical(.mem_escrituras, 0L)
  expect_identical(sort(ls(.mem)), sort(llave("viejo", c("usuarios.rds", "empresas.rds"))))
})

test_that("2b one file with data among empty ones: still nothing written", {
  limpiar()
  for (a in c("grupos.rds", "contacts.rds")) assign(llave("mixto", a), data.frame(), envir = .mem)
  assign(llave("mixto", "app_audit.rds"), data.frame(id = "x"), envir = .mem)
  e <- condicion("mixto")
  expect_s3_class(e, "hd_init_con_datos")
  expect_identical(e$archivos, "app_audit.rds")
  expect_identical(.mem_escrituras, 0L)
})

test_that("3a a file that answers 403: it stops, names it, and writes nothing", {
  limpiar()
  .mem_prohibido <<- llave("vedado", "empresas.rds")
  e <- condicion("vedado")
  expect_s3_class(e, "hd_init_no_comprobada")
  expect_match(conditionMessage(e), "empresas.rds", fixed = TRUE)
  expect_identical(.mem_escrituras, 0L)
  expect_identical(ls(.mem), character(0))
})

test_that("3b a file whose HEAD fails like the network: same, nothing written", {
  limpiar()
  .mem_falla_cabeza <<- llave("sinred", "contacts.rds")
  e <- condicion("sinred")
  expect_s3_class(e, "hd_init_no_comprobada")
  expect_identical(e$archivos, "contacts.rds")
  expect_identical(.mem_escrituras, 0L)
})

test_that("3c a file that is there but can't be read: same, nothing written", {
  limpiar()
  assign(llave("ilegible", "usuarios.rds"), data.frame(username = "ana"), envir = .mem)
  .mem_falla <<- llave("ilegible", "usuarios.rds")
  e <- condicion("ilegible")
  expect_s3_class(e, "hd_init_no_comprobada")
  expect_identical(e$archivos, "usuarios.rds")
  expect_identical(.mem_escrituras, 0L)
})

test_that("4 a failed approval's folder (3 there, empty; 2 missing): only the 2 are written", {
  limpiar()
  previas <- c("usuarios.rds", "empresas.rds", "grupos.rds")
  for (a in previas) assign(llave("amedias", a), structure(data.frame(), marca = "previa"), envir = .mem)
  iniciar("amedias")
  expect_identical(.mem_escrituras, 2L)
  for (a in previas) expect_identical(attr(en_mem(llave("amedias", a)), "marca"), "previa", info = a)
  for (a in setdiff(ARCHIVOS, previas)) expect_false(is.null(en_mem(llave("amedias", a))), info = a)
})

test_that("5 the approval's notice for each class, and both keys in es and en", {
  if (!exists("init_cliente_aviso", mode = "function")) fail("init_cliente_aviso() does not exist")
  con_datos <- structure(class = c("hd_init_con_datos", "error", "condition"), list(message = "", call = NULL))
  no_comp   <- structure(class = c("hd_init_no_comprobada", "error", "condition"), list(message = "", call = NULL))
  expect_identical(init_cliente_aviso(con_datos), "niveles.solicitudes.aviso.carpeta_con_datos")
  expect_identical(init_cliente_aviso(no_comp), "niveles.solicitudes.aviso.carpeta_no_comprobada")
  expect_identical(init_cliente_aviso(simpleError("otro")), "niveles.solicitudes.aviso.error_folder")
  for (idioma in c("es", "en")) {
    d <- jsonlite::fromJSON(sprintf("www/i18n/%s.json", idioma))
    for (k in c("carpeta_con_datos", "carpeta_no_comprobada")) {
      txt <- d[[paste0("niveles.solicitudes.aviso.", k)]]
      expect_true(is.character(txt) && grepl("{cliente}", txt, fixed = TRUE), info = paste(idioma, k))
    }
  }
  # The approval (tiers_module.R) turns initialize_client_folder()'s error into
  # init_cliente_aviso(e), shown with the client's id.
  src <- paste(deparse(parse("R/tiers_module.R", keep.source = FALSE)), collapse = "\n")
  expect_match(src, "initialize_client_folder(row$client_id_proposed", fixed = TRUE)
  expect_match(src, "init_cliente_aviso(e)", fixed = TRUE)
  expect_match(src, "tr(init_aviso, cliente = row$client_id_proposed)", fixed = TRUE)
})

s3_memoria_restaurar()
