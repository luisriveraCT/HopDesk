# =============================================================================
# tests/test_init_cliente_no_pisa.R
# CM25 (PENDIENTES §2). initialize_client_folder() (R/persistence.R) writes
# the five starting files of a new client -- usuarios, empresas, grupos,
# contacts, app_audit -- with s3saveRDS, without looking first. The approval
# (tiers_module.R) only checks the client registry: a folder that exists
# without a registry row (a registry row removed, a client from before the
# registry, a second approval of the same id) had its accounts and companies
# replaced by empty tables.
#   1  a new client: the five files are created, empty, with their schema;
#   2  an existing client (any of the five already there): re-initializing
#      stops, says which exist, and writes nothing -- not even what is
#      missing (a new client's folder never mixes with an old one's);
#   3  a file that can't be checked (403, or HEAD failing like the network):
#      it stops, says which, and writes NOTHING -- not even the files that
#      could be checked.
# Run from project root:  Rscript tests/test_init_cliente_no_pisa.R
# =============================================================================
suppressPackageStartupMessages(library(testthat))
source("tests/helpers/s3_memoria.R")

ARCHIVOS <- c("usuarios.rds", "empresas.rds", "grupos.rds", "contacts.rds", "app_audit.rds")
llave <- function(cid, a) paste0(cid, "/", a)
en_mem <- function(k) if (exists(k, envir = .mem, inherits = FALSE)) get(k, envir = .mem) else NULL
limpiar <- function() {
  rm(list = ls(.mem, all.names = TRUE), envir = .mem)
  .mem_prohibido <<- character(); .mem_falla_cabeza <<- character(); .mem_escrituras <<- 0L
}
iniciar <- function(cid) suppressMessages(initialize_client_folder(cid, created_by = "prueba"))

test_that("1 a new client: the five files, empty", {
  limpiar()
  iniciar("nuevo")
  for (a in ARCHIVOS) {
    x <- en_mem(llave("nuevo", a))
    expect_false(is.null(x), info = a)
    expect_identical(nrow(as.data.frame(x)), 0L, info = a)
  }
})

test_that("2 an existing client: it stops, names what exists, and touches nothing", {
  limpiar()
  usuarios <- data.frame(username = "ana", deleted = FALSE, stringsAsFactors = FALSE)
  empresas <- data.frame(iniciales = c("NL", "NG"), stringsAsFactors = FALSE)
  assign(llave("viejo", "usuarios.rds"), usuarios, envir = .mem)
  assign(llave("viejo", "empresas.rds"), empresas, envir = .mem)
  antes <- .mem_escrituras
  e <- tryCatch({ iniciar("viejo"); NULL }, error = function(e) conditionMessage(e))
  expect_false(is.null(e))
  expect_match(e %||% "", "usuarios.rds", fixed = TRUE)
  expect_identical(en_mem(llave("viejo", "usuarios.rds")), usuarios)
  expect_identical(en_mem(llave("viejo", "empresas.rds")), empresas)
  expect_identical(.mem_escrituras - antes, 0L)
  expect_identical(sort(ls(.mem)), sort(llave("viejo", c("usuarios.rds", "empresas.rds"))))
})

test_that("3a a file that answers 403: it stops, names it, and writes nothing", {
  limpiar()
  .mem_prohibido <<- llave("vedado", "empresas.rds")
  antes <- .mem_escrituras
  e <- tryCatch({ iniciar("vedado"); NULL }, error = function(e) conditionMessage(e))
  expect_false(is.null(e))
  expect_match(e %||% "", "empresas.rds", fixed = TRUE)
  expect_identical(.mem_escrituras - antes, 0L)
  expect_identical(ls(.mem), character(0))
})

test_that("3b a file whose HEAD fails like the network: same, nothing written", {
  limpiar()
  .mem_falla_cabeza <<- llave("sinred", "contacts.rds")
  antes <- .mem_escrituras
  e <- tryCatch({ iniciar("sinred"); NULL }, error = function(e) conditionMessage(e))
  expect_false(is.null(e))
  expect_identical(.mem_escrituras - antes, 0L)
})

s3_memoria_restaurar()
