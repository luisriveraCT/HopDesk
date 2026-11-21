# =============================================================================
# tests/test_renombrar_iniciales.R
# CM26 (PENDIENTES §2 l.51). rename_empresa_initials() (R/persistence.R)
# renames a company's initials in every table that keys on them. Before
# CM26, five of its seven steps read the ordinary way -- the session's
# preload cache first -- and saved the whole table; proveedores read active
# rows only; a step that failed only left a message(), so the caller saved
# the new initials in empresas.rds and that table kept the old ones for
# good; and erp_connections, proveedores' `empresas` list,
# proveedores_inactivos and requisiciones were never renamed (a renamed
# company lost its stored SAP connection).
#   1  a stale session copy (preload cache) does not erase what another
#      session saved: every step works on S3's current table;
#   2  proveedores: a legacy activo = FALSE row is kept and renamed;
#   3  a step that fails is reported (hd_renombrar_incompleto: done, failed);
#      ctas_cuentas (step 1) counts too;
#   4  a failure at an early step and at a late one, then the same rename
#      again: everything renamed, no duplicates, nothing rewritten twice;
#   5  the new tables: erp_connections changes only the element of its JSON
#      array (config and secrets identical); proveedores' comma list changes
#      whole elements only ("NTSX" is not "NTS"); proveedores_inactivos and
#      requisiciones are renamed; pasivos_audit is not touched;
#   6  empresas.rds is read from S3 first: NEW taken by another company, a
#      rename never reserved (CM40), or neither OLD nor NEW there, stops it
#      with nothing written;
#   8  a retry whose row already says NEW (reservation NEW kept) goes on
#      (CM29b), and NEW as another row's initials still stops it.
# Every rename here reserves first, the way the Empresas edit does
# (reservar_renombre_empresa(), CM40).
# Run from project root:  Rscript tests/test_renombrar_iniciales.R
# =============================================================================
suppressPackageStartupMessages(library(testthat))
source("tests/helpers/s3_memoria.R")
for (f in c("R/bancos_persistence.R", "R/pasivos_schemas.R", "R/pasivos_persistence.R",
            "R/settings/settings_helpers.R", "R/requisiciones_persistence.R"))
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
fila <- function(schema, ...) .normalize(tibble::as_tibble(list(...)), schema)
source("R/empresas_module.R", encoding = "UTF-8")   # reservar_renombre_empresa() (CM40)
# The company's row is reserved first, as the Empresas edit does; a rename of
# initials no company has goes to the cascade unreserved (case 6).
renombrar <- function(old = "OLD", new = "NEW") {
  e  <- get0(k(S3_KEYS$empresas), envir = .mem)
  id <- if (is.data.frame(e)) as.character(e$id[toupper(e$initials) %in% c(old, new)])[1] else NA
  if (!is.na(id)) suppressMessages(reservar_renombre_empresa(id, old, new, CID))
  suppressMessages(rename_empresa_initials(old, new, client_id = CID))
}
falla_de <- function() tryCatch({ renombrar(); NULL }, error = function(e) e)

SECRETO <- "c2VjcmV0by1jaWZyYWRv"
# Every table the rename handles, each with one row of OLD and one of OTRA.
sembrar <- function() {
  poner(S3_KEYS$empresas, fila(.schema_empresas, id = c("e1", "e2"), initials = c("OLD", "OTRA"),
                               activa = c(TRUE, TRUE), deleted = c(FALSE, FALSE)))
  poner(S3_KEYS$ctas_cuentas,   fila(.schema_ctas_cuentas,  id = c("c1", "c2"), Empresa = c("OLD", "OTRA")))
  poner(S3_KEYS$bancos_cuentas, fila(.schema_bancos_cuentas, cuenta_id = c("b1", "b2"), empresa = c("OLD", "OTRA")))
  poner(S3_KEYS$proveedores,    fila(.schema_proveedores, id = c("p1", "p2"), nombre = c("UNO", "DOS"),
                                     Empresa = c("OLD", "OTRA"), empresas = c("OLD,OTRA", "OLDX, OTRA"),
                                     activo = c(TRUE, TRUE)))
  poner(S3_KEYS$proveedores_inactivos, fila(.schema_proveedores, id = "pi1", nombre = "INACTIVO",
                                            Empresa = "OLD", empresas = "OTRA, OLD", activo = FALSE))
  poner(S3_KEYS$parte_alias_map, fila(.schema_parte_alias_map, Empresa = c("OLD", "OTRA")))
  poner(S3_KEYS$interco_v2, list(ar_prefix = "C", ap_prefix = "P", rfcs = character(),
                                 companies = list(OLD = list(ar = "C1"), OTRA = list(ar = "C2"))))
  poner(S3_KEYS$pasivos_provisions, fila(.schema_pasivos_provision, id = c("v1", "v2"), empresa = c("OLD", "OTRA")))
  poner(S3_KEYS$pasivos_liabilities, fila(.schema_pasivos_liability, id = c("l1", "l2"), empresa = c("OLD", "OTRA")))
  poner(S3_KEYS$erp_connections, fila(.schema_erp_connections, id = c("x1", "x2"), client_id = CID,
                                      company_initials = c('["OTRA","OLD"]', '["OTRA"]'),
                                      config = c('{"db":"SBO_UNO"}', '{"db":"SBO_DOS"}'),
                                      secrets_encrypted = c(SECRETO, SECRETO), active = TRUE, deleted = FALSE))
  poner(REQ_KEYS$requisiciones, fila(.REQ_SCHEMAS$requisiciones, id = c("r1", "r2"), empresa = c("OLD", "OTRA")))
  poner(S3_KEYS$pasivos_audit, fila(.schema_pasivos_audit, empresa = "OLD"))
}
# Every renamed table: no OLD left, OTRA as it was.
todo_renombrado <- function() {
  cols <- list(ctas_cuentas = "Empresa", bancos_cuentas = "empresa", proveedores = "Empresa",
               proveedores_inactivos = "Empresa", parte_alias_map = "Empresa",
               pasivos_provisions = "empresa", pasivos_liabilities = "empresa")
  for (t in names(cols)) {
    v <- leer(S3_KEYS[[t]])[[cols[[t]]]]
    expect_false("OLD" %in% v, info = t)
    expect_true("NEW" %in% v, info = t)
  }
  expect_identical(leer(REQ_KEYS$requisiciones)$empresa, c("NEW", "OTRA"))
  expect_setequal(names(leer(S3_KEYS$interco_v2)$companies), c("NEW", "OTRA"))
  expect_identical(leer(S3_KEYS$erp_connections)$company_initials, c('["OTRA","NEW"]', '["OTRA"]'))
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
  e <- falla_de()
  expect_false(is.null(e))
  expect_s3_class(e, "hd_renombrar_incompleto")
  expect_true("proveedores" %in% e$fallidos)
  expect_true(all(c("ctas_cuentas", "bancos_cuentas", "pasivos_liabilities") %in% e$hechos))
})

test_that("3b ctas_cuentas (step 1) failing counts as failed too", {
  limpiar(); sembrar()
  .mem_falla <<- k(S3_KEYS$ctas_cuentas)
  e <- falla_de()
  expect_s3_class(e, "hd_renombrar_incompleto")
  expect_identical(e$fallidos, "ctas_cuentas")
})

for (k_falla in c("ctas_cuentas", "erp_connections")) {
  test_that(sprintf("4 a failure at %s, then the same rename again: all renamed, no duplicates", k_falla), {
    limpiar(); sembrar()
    llave_falla <- if (k_falla %in% names(REQ_KEYS)) REQ_KEYS[[k_falla]] else S3_KEYS[[k_falla]]
    .mem_falla <<- k(llave_falla)
    e <- falla_de()
    expect_identical(e$fallidos, k_falla)
    escritas <- .mem_escrituras
    .mem_falla <<- character()
    r <- renombrar()
    # Only the step that failed writes on the retry.
    expect_identical(r, k_falla)
    expect_identical(.mem_escrituras - escritas, 1L)
    todo_renombrado()
    for (t in c("ctas_cuentas", "bancos_cuentas", "pasivos_provisions", "pasivos_liabilities", "erp_connections"))
      expect_identical(nrow(leer(S3_KEYS[[t]])), 2L, info = t)
    # A third run finds nothing to rename.
    escritas <- .mem_escrituras
    expect_identical(renombrar(), character(0))
    expect_identical(.mem_escrituras, escritas)
  })
}

test_that("5 the new tables: erp_connections, the comma list, inactive suppliers, requisiciones", {
  limpiar(); sembrar()
  erp_antes <- leer(S3_KEYS$erp_connections)
  audit_antes <- leer(S3_KEYS$pasivos_audit)
  renombrar()
  todo_renombrado()
  erp <- leer(S3_KEYS$erp_connections)
  for (col in setdiff(names(erp_antes), "company_initials"))
    expect_identical(erp[[col]], erp_antes[[col]], info = col)
  prov <- leer(S3_KEYS$proveedores)
  expect_identical(prov$empresas, c("NEW,OTRA", "OLDX, OTRA"))
  expect_identical(leer(S3_KEYS$proveedores_inactivos)$empresas, "OTRA, NEW")
  expect_identical(leer(S3_KEYS$pasivos_audit), audit_antes)
})

test_that("6 empresas.rds first: NEW taken, or neither there, stops with nothing written", {
  limpiar(); sembrar()
  e <- tryCatch({ renombrar("OLD", "OTRA"); NULL }, error = function(e) e)
  expect_false(is.null(e))
  expect_identical(.mem_escrituras, 0L)
  expect_identical(leer(S3_KEYS$ctas_cuentas)$Empresa, c("OLD", "OTRA"))
  e <- tryCatch({ renombrar("NADA", "NUEVA"); NULL }, error = function(e) e)
  expect_false(is.null(e))
  expect_identical(.mem_escrituras, 0L)
  # A rename never reserved (CM40): the cascade refuses it, nothing written.
  e <- tryCatch({ suppressMessages(rename_empresa_initials("OLD", "NEW", client_id = CID)); NULL },
                error = function(e) e)
  expect_match(conditionMessage(e), "was not reserved", fixed = TRUE)
  expect_identical(.mem_escrituras, 0L)
  expect_identical(leer(S3_KEYS$ctas_cuentas)$Empresa, c("OLD", "OTRA"))
  # empresas.rds unreadable: nothing either.
  .mem_falla <<- k(S3_KEYS$empresas)
  e <- falla_de()
  expect_false(is.null(e))
  expect_identical(.mem_escrituras, 0L)
})

test_that("8 a retry whose row already says NEW goes on; NEW as another row's initials stops it", {
  limpiar(); sembrar()
  .mem_falla <<- k(S3_KEYS$pasivos_provisions)
  e <- falla_de()                                       # reserved, a step failed
  expect_identical(e$fallidos, "pasivos_provisions")
  .mem_falla <<- character()
  emp <- leer(S3_KEYS$empresas)                         # the row says NEW, its reservation kept
  emp$initials[emp$id == "e1"] <- "NEW"; poner(S3_KEYS$empresas, emp)
  expect_identical(suppressMessages(rename_empresa_initials("OLD", "NEW", client_id = CID)), "pasivos_provisions")
  todo_renombrado()
  # Another row holding NEW stops it, even with this row's reservation.
  limpiar(); sembrar()
  suppressMessages(reservar_renombre_empresa("e1", "OLD", "NEW", CID))
  emp <- leer(S3_KEYS$empresas); emp$initials[emp$id == "e2"] <- "NEW"; poner(S3_KEYS$empresas, emp)
  n0 <- .mem_escrituras
  e <- tryCatch({ suppressMessages(rename_empresa_initials("OLD", "NEW", client_id = CID)); NULL },
                error = function(e) e)
  expect_match(conditionMessage(e), "already another company's", fixed = TRUE)
  expect_identical(.mem_escrituras, n0)
})

test_that("7 the Empresas edit maps hd_renombrar_incompleto to its notice, and the key is in es and en", {
  for (idioma in c("es", "en")) {
    txt <- jsonlite::fromJSON(sprintf("www/i18n/%s.json", idioma))[["tablas.empresas.aviso.renombrar_incompleto"]]
    expect_true(is.character(txt) && all(vapply(c("{antes}", "{despues}", "{tablas}"), grepl, logical(1), txt,
                                               fixed = TRUE)), info = idioma)
  }
  src <- paste(deparse(parse("R/empresas_module.R", keep.source = FALSE)), collapse = "\n")
  expect_match(src, "inherits(cascade, \"hd_renombrar_incompleto\")", fixed = TRUE)
  expect_match(src, "tr(\"tablas.empresas.aviso.renombrar_incompleto\"", fixed = TRUE)
  expect_match(src, "action = \"renombrar_iniciales\"", fixed = TRUE)
})

s3_memoria_restaurar()
