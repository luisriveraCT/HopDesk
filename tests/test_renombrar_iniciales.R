# =============================================================================
# tests/test_renombrar_iniciales.R
# CM26 (PENDIENTES Â§2 l.51). rename_empresa_initials() (R/persistence.R)
# renames a company's initials in every table that keys on them. Steps 1
# (ctas_cuentas) and 5 (interco_v2) write on S3's current table; steps 2
# (bancos_cuentas), 3 (proveedores), 4 (parte_alias_map), 6
# (pasivos_provisions) and 7 (pasivos_liabilities) read the ordinary way --
# the preload cache first -- and save the whole table. A step that fails
# only leaves a message(): the caller (empresas_module.R) is told nothing,
# saves the new initials in empresas.rds, and the table that failed keeps
# the old ones for good.
#   1  a stale session copy (preload cache) does not erase what another
#      session saved: every step works on S3's current table;
#   2  proveedores: a legacy activo = FALSE row is kept and renamed, and the
#      active ones are renamed (load_proveedores() is active-only);
#   3  a step that fails is reported to the caller, saying which steps were
#      done and which failed;
#   4  running it again after such a failure finishes the job (the steps
#      already done have nothing left to rename).
# Run from project root:  Rscript tests/test_renombrar_iniciales.R
# =============================================================================
suppressPackageStartupMessages(library(testthat))
source("tests/helpers/s3_memoria.R")
for (f in c("R/bancos_persistence.R", "R/pasivos_schemas.R", "R/pasivos_persistence.R", "R/settings/settings_helpers.R"))
  suppressMessages(source(f))

CID <- "acme"
k <- function(key) paste0(CID, "/", key)
poner <- function(key, df) assign(k(key), df, envir = .mem)
leer  <- function(key) get(k(key), envir = .mem)
limpiar <- function() {
  rm(list = ls(.mem, all.names = TRUE), envir = .mem)
  .mem_falla <<- character(); .mem_prohibido <<- character(); .mem_escrituras <<- 0L
  for (env in Filter(Negate(is.null), list(get0(".s3_preload_cache"), get0(".s3_missing_cache"), get0(".s3_filas"))))
    rm(list = ls(env, all.names = TRUE), envir = env)
}
fila <- function(schema, ...) {
  x <- tibble::as_tibble(list(...))
  .normalize(x, schema)
}
renombrar <- function() suppressMessages(rename_empresa_initials("OLD", "NEW", client_id = CID))

# Every table of the cascade, each with one row of OLD and one of OTRA.
sembrar <- function() {
  poner(S3_KEYS$ctas_cuentas,   fila(.schema_ctas_cuentas,  id = c("c1", "c2"), Empresa = c("OLD", "OTRA")))
  poner(S3_KEYS$bancos_cuentas, fila(.schema_bancos_cuentas, cuenta_id = c("b1", "b2"), empresa = c("OLD", "OTRA")))
  poner(S3_KEYS$proveedores,    fila(.schema_proveedores, id = c("p1", "p2"), nombre = c("UNO", "DOS"),
                                     Empresa = c("OLD", "OTRA"), activo = c(TRUE, TRUE)))
  poner(S3_KEYS$parte_alias_map, fila(.schema_parte_alias_map, Empresa = c("OLD", "OTRA")))
  poner(S3_KEYS$pasivos_provisions, fila(.schema_pasivos_provision, id = c("v1", "v2"), empresa = c("OLD", "OTRA")))
  poner(S3_KEYS$pasivos_liabilities, fila(.schema_pasivos_liability, id = c("l1", "l2"), empresa = c("OLD", "OTRA")))
}

ID_COL <- c(bancos_cuentas = "cuenta_id", proveedores = "id", pasivos_provisions = "id", pasivos_liabilities = "id")
test_that("1 a stale session copy does not erase another session's rows", {
  limpiar(); sembrar()
  for (t in names(ID_COL)) {
    key <- S3_KEYS[[t]]
    # What this session loaded earlier (the preload cache) ...
    assign(k(key), leer(key), envir = .s3_preload_cache)
    # ... and a row another session added since.
    col <- if (t == "proveedores") "Empresa" else "empresa"
    actual <- leer(key)
    nueva <- actual[2, , drop = FALSE]; nueva[[ID_COL[[t]]]] <- paste0(t, "-otra-sesion"); nueva[[col]] <- "OLD"
    if (t == "proveedores") nueva$nombre <- "TRES"
    poner(key, dplyr::bind_rows(actual, nueva))
  }
  renombrar()
  for (t in names(ID_COL)) {
    df <- leer(S3_KEYS[[t]])
    col <- if (t == "proveedores") "Empresa" else "empresa"
    expect_true(paste0(t, "-otra-sesion") %in% df[[ID_COL[[t]]]], info = t)
    expect_false("OLD" %in% df[[col]], info = t)
  }
})

test_that("2 proveedores: a legacy inactive row is kept, and everything is renamed", {
  limpiar(); sembrar()
  prov <- leer(S3_KEYS$proveedores)
  vieja <- prov[1, , drop = FALSE]; vieja$id <- "p-legado"; vieja$nombre <- "LEGADO"; vieja$activo <- FALSE
  poner(S3_KEYS$proveedores, dplyr::bind_rows(prov, vieja))
  renombrar()
  df <- leer(S3_KEYS$proveedores)
  expect_true("p-legado" %in% df$id)
  expect_identical(df$Empresa[df$id %in% c("p1", "p-legado")], c("NEW", "NEW"))
  expect_identical(df$Empresa[df$id == "p2"], "OTRA")
})

test_that("3 a step that fails is reported: which were done, which failed", {
  limpiar(); sembrar()
  .mem_falla <<- k(S3_KEYS$proveedores)
  e <- tryCatch({ renombrar(); NULL }, error = function(e) e)
  expect_false(is.null(e))
  expect_s3_class(e, "hd_renombrar_incompleto")
  expect_true("proveedores" %in% e$fallidos)
  expect_true(all(c("ctas_cuentas", "bancos_cuentas", "pasivos_liabilities") %in% e$hechos))
})

test_that("4 running it again after a failure finishes the job", {
  limpiar(); sembrar()
  .mem_falla <<- k(S3_KEYS$proveedores)
  tryCatch(renombrar(), error = function(e) NULL)
  .mem_falla <<- character()
  r <- renombrar()
  expect_identical(leer(S3_KEYS$proveedores)$Empresa, c("NEW", "OTRA"))
  expect_true("proveedores" %in% r)
  for (t in c("ctas_cuentas", "bancos_cuentas", "pasivos_provisions", "pasivos_liabilities")) {
    df <- leer(S3_KEYS[[t]])
    col <- if (t == "ctas_cuentas") "Empresa" else "empresa"
    expect_identical(df[[col]], c("NEW", "OTRA"), info = t)
  }
})

s3_memoria_restaurar()
