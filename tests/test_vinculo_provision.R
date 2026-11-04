# Provision -> item -> payment link: an item converted from a provision must
# keep its provision_id through every edit.
# Run from the repo root: Rscript tests/test_vinculo_provision.R
library(testthat)
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/persistence.R")

guardado <- tibble(
  id = "m1", ledger = "AP", Empresa = "NCS", Moneda = "MXN", Documento = "PROV-7",
  Parte = "Renta", Importe = 1000, `Abono futuro` = 200,
  created_by = "ana", created_at = as.POSIXct("2026-10-01", tz = "UTC"),
  provision_id = "p-7", liability_id = "l-3", referencia = "Contrato 12")

formulario <- tibble(
  id = "m1", ledger = "AP", Empresa = "NCS", Moneda = "MXN", Documento = "PROV-7",
  Parte = "Renta oficina", Importe = 1200, created_by = "ana",
  created_at = as.POSIXct("2026-10-01", tz = "UTC"), updated_at = Sys.time())

test_that("editar un item conserva su vínculo con la provisión", {
  out <- editar_manual(guardado, formulario)
  expect_equal(out$provision_id, "p-7")
  expect_equal(out$liability_id, "l-3")
  expect_equal(out$referencia, "Contrato 12")
  expect_equal(out$`Abono futuro`, 200)
  # what the form sends wins
  expect_equal(out$Parte, "Renta oficina")
  expect_equal(out$Importe, 1200)
})

test_that("tras upsert la fila guardada sigue vinculada", {
  df <- upsert_manual(guardado, editar_manual(guardado, formulario))
  expect_equal(nrow(df), 1)
  expect_equal(df$provision_id, "p-7")
})

test_that("sin fila previa, la edición es la fila del formulario", {
  expect_identical(editar_manual(guardado[0, ], formulario), formulario)
  expect_identical(editar_manual(NULL, formulario), formulario)
})

source("R/pasivos_persistence.R")
provs <- tibble(id = c("p1", "p2", "p3", "p4", NA),
                estado = c("provisional", "converted", "item_confirmed", "closed", "item_confirmed"))

test_that("una provisión pagada se reconoce como pagada", {
  expect_setequal(provisiones_pagadas(provs, c("p1", "p2", "p3", "p4")), c("p3", "p4"))
  expect_length(provisiones_pagadas(provs, c("p1", "p2")), 0)
  expect_length(provisiones_pagadas(provs, NA), 0)
  expect_length(provisiones_pagadas(NULL, "p3"), 0)
  expect_length(provisiones_pagadas(provs, character()), 0)
})

lineas <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

test_that("borrar desde Calendario deja fuera las pagadas y relee S3", {
  src <- lineas("R/ledger_module.R")
  expect_match(src, "provisiones_pagadas(provs_db", fixed = TRUE)
  expect_match(src, "provs_db <- if (\"provision_id\" %in% names(rows_to_delete))\n        tryCatch(load_pasivos_provisions(",
               fixed = TRUE)
  # the in-memory copy is no longer the source for the estado write
  expect_false(grepl("provs_db <- tryCatch(shared$pasivos_provisions_db()", src, fixed = TRUE))
})

test_that("deshacer conversión rechaza en el servidor una provisión pagada", {
  src <- lineas("R/pasivos_module.R")
  undo <- regmatches(src, regexpr("observeEvent\\(input\\$pcm_do_undo_conversion[\\s\\S]*?provs\\$estado\\[idx\\]\\s+<- \"provisional\"", src, perl = TRUE))
  expect_length(undo, 1)
  expect_match(undo, "provisiones_pagadas(", fixed = TRUE)
  expect_lt(regexpr("provisiones_pagadas(", undo, fixed = TRUE),
            regexpr("save_manual(", undo, fixed = TRUE))   # checked before anything is removed
})

# ── Undoing a confirmation in Bancos ──────────────────────────────────────────
source("R/bancos_persistence.R")
source("R/data_pipeline.R")
conf0 <- .normalize(tibble(confirmacion_id = c("c1", "c2"), empresa = "NCS",
                           documento = c("F100", "F200"), moneda = "MXN", importe = c(500, 80),
                           tipo = "pago", eliminado = FALSE, provision_id = c("p3", NA),
                           action = "confirmed"), .schema_bancos_confirmados)
deshecha <- recover_confirmacion(conf0, "c1")

test_that("la fila de recuperación no es una confirmación vigente", {
  expect_equal(nrow(deshecha), 3)
  expect_setequal(confirmaciones_vigentes(deshecha)$confirmacion_id, "c2")
  expect_null(confirmaciones_vigentes(NULL))
  # rows from before Stage 3 have no action column: still live
  viejas <- conf0[, setdiff(names(conf0), "action")]
  expect_equal(nrow(confirmaciones_vigentes(viejas)), 2)
})

test_that("una factura cuya confirmación se deshizo vuelve a estar abierta", {
  df <- tibble(Empresa = c("NCS", "NCS"), Documento = c("F100", "F200"), Moneda = "MXN",
               Saldo_original = c(500, 80), `Saldo vencido` = c(500, 80),
               source = "sap", confirmed = FALSE)
  out <- compute_confirmed_flags(df, "AP", deshecha, NULL)
  expect_equal(out$confirmed, c(FALSE, TRUE))
})

test_that("etapa: pagada solo con una confirmación vigente", {
  pv <- tibble(id = c("p1", "p2", "p3", "p4", "p5", "p6"),
               estado = c("provisional", "converted", "item_confirmed", "closed", "deleted", "closed"))
  cf <- .normalize(tibble(confirmacion_id = c("a", "b"), provision_id = c("p3", "p6"),
                          eliminado = FALSE, action = "confirmed"), .schema_bancos_confirmados)
  expect_equal(etapa_de_provision_en(pv, cf, c("p1", "p2", "p3", "p4", "p5", "p6", "zz")),
               c("provisionada", "en_pago", "pagada", "borrada", "borrada", "pagada", NA))
  # item_confirmed whose confirmation was undone: back to en_pago, not pagada
  expect_equal(etapa_de_provision_en(pv, recover_confirmacion(cf, "a"), "p3"), "en_pago")
  expect_error(etapa_de_provision(NULL, "p1"), "client_id")
})

test_that("deshacer la confirmación regresa la provisión a convertida", {
  .mem <- new.env()
  .s3_key       <<- function(key, client_id = NULL) paste0(client_id, "/", key)
  .s3_read_with <<- function(key, client_id = NULL) .mem[[paste0(client_id, "/", key)]]
  .s3_write     <<- function(obj, key, client_id = NULL) assign(paste0(client_id, "/", key), obj, envir = .mem)
  bump_sync_version <<- function(...) invisible(TRUE)
  pasivos_log_audit <<- function(...) invisible("audit")
  S3_KEYS <<- list(pasivos_provisions = "pasivos_provisions.rds")
  if (!exists(".schema_pasivos_provision")) source("R/pasivos_schemas.R")
  source("R/pasivos_engine.R")
  save_pasivos_provisions(tibble(id = c("p3", "p1"), estado = c("item_confirmed", "provisional"),
                                 bancos_conf_id = c("c1", NA), manual_inv_id = c("m3", NA)),
                          client_id = "cli")
  pasivos_provision_item_reversed("p3", "ana", client_id = "cli")
  pasivos_provision_item_reversed("p1", "ana", client_id = "cli")      # not paid: no-op
  p <- load_pasivos_provisions(client_id = "cli")
  expect_equal(p$estado, c("converted", "provisional"))
  expect_true(is.na(p$bancos_conf_id[1]))
  expect_equal(p$manual_inv_id[1], "m3")                               # the item is still linked
})

test_that("regenerar o extender un pasivo no vuelve a crear ocurrencias convertidas, pagadas o borradas", {
  existentes <- tibble(id = c("a", "b", "c", "d"), liability_id = "L1", occurrence_index = 1:4,
                       estado = c("converted", "item_confirmed", "deleted", "provisional"),
                       fecha_efectiva = as.Date("2026-01-01") + 0:3 * 30)
  generadas <- tibble(id = paste0("g", 1:5), liability_id = "L1", occurrence_index = 1:5,
                      estado = "provisional", fecha_efectiva = as.Date("2026-01-01") + 0:4 * 30)
  r <- pasivos_reconcile_provisions(existentes, generadas, mode = "regenerate")
  expect_equal(r$insert$occurrence_index, 5L)
  e <- pasivos_reconcile_provisions(existentes, generadas, mode = "extend")
  expect_equal(e$insert$occurrence_index, 5L)
  # all occurrences converted: extend only adds what is new
  todas <- dplyr::mutate(existentes, estado = "converted")
  expect_equal(pasivos_reconcile_provisions(todas, generadas, mode = "extend")$insert$occurrence_index, 5L)
  # same index in another liability is not "already there"
  otra <- dplyr::mutate(generadas[5, ], liability_id = "L2", occurrence_index = 1L)
  expect_equal(nrow(pasivos_reconcile_provisions(existentes, otra, mode = "regenerate")$insert), 1L)
})

test_that("confirmar parte de lo que hay en S3 y no escribe si la lectura falla", {
  S3_KEYS$bancos_confirmados <<- "bancos_confirmados.rds"
  leer <- .s3_read_with   # the in-memory store set up in the test above
  .s3_existe <<- function(key, client_id) !is.null(leer(key, client_id))
  # another session already confirmed c2 after this session loaded c1 only
  save_bancos_confirmados(conf0, client_id = "cli")
  mia <- .normalize(tibble(confirmacion_id = "c9", agenda_item_id = "ag9", tipo = "pago",
                           eliminado = FALSE), .schema_bancos_confirmados)
  out <- modificar_bancos_confirmados("cli", function(c) dplyr::bind_rows(c, mia))
  expect_setequal(out$confirmacion_id, c("c1", "c2", "c9"))
  # archive link applied on the fresh table
  enlazar_archivo_confirmaciones("cli", list(list(ids = "ag9", event = "ev9")))
  guardado <- .s3_read_with("bancos_confirmados.rds", "cli")
  expect_equal(guardado$archive_event_id[guardado$confirmacion_id == "c9"], "ev9")
  # a failed read stops before writing
  .s3_read_with <<- function(key, client_id = NULL) NULL
  expect_error(modificar_bancos_confirmados("cli", function(c) mia), "could not read")
  .s3_read_with <<- leer
  expect_equal(nrow(.s3_read_with("bancos_confirmados.rds", "cli")), 3)
  expect_error(modificar_bancos_confirmados(NULL, identity), "client_id")
})

test_that("importar TXT no empata contra confirmaciones eliminadas ni deshechas (CM10/C6)", {
  source("R/bancos_parser.R")
  conf <- .normalize(tibble(confirmacion_id = c("v", "x"), agenda_item_id = c("ag-v", "ag-x"),
                            documento = c("F1", "F2"), importe = c(300, 777),
                            fecha = as.Date("2026-10-30"), tipo = "pago",
                            eliminado = c(FALSE, TRUE)), .schema_bancos_confirmados)
  conf <- recover_confirmacion(dplyr::mutate(conf, eliminado = FALSE)[1, ], "v") |>
    dplyr::bind_rows(conf[2, ])                       # v undone, x deleted: nothing live
  movs <- tibble(id = c("m1", "m2"), cargo = c(300, 777), abono = 0,
                 fecha = as.Date("2026-10-30"))
  out <- auto_match_agenda(movs, conf)
  expect_false(any(out$conciliado))
  vivo <- .normalize(tibble(confirmacion_id = "w", agenda_item_id = "ag-w", documento = "F3",
                            importe = 300, fecha = as.Date("2026-10-30"), tipo = "pago",
                            eliminado = FALSE), .schema_bancos_confirmados)
  expect_true(auto_match_agenda(movs[1, ], vivo)$conciliado)
})

test_that("confirmar en Agenda ya no guarda la copia en memoria", {
  ph <- lineas("R/pagar_hoy_module.R")
  expect_false(grepl("conf_db <- shared$bancos_confirmados()", ph, fixed = TRUE))
  expect_false(grepl("save_bancos_confirmados(conf_now", ph, fixed = TRUE))
  expect_equal(lengths(regmatches(ph, gregexpr("modificar_bancos_confirmados(", ph, fixed = TRUE))), 2L)
})

test_that("Bancos y Pasivos usan confirmaciones vigentes al deshacer y eliminar", {
  obs <- lineas("R/pasivos_observers.R")
  expect_match(obs, "bc <- confirmaciones_vigentes(bc)", fixed = TRUE)
  ban <- lineas("R/bancos_module.R")
  undo <- regmatches(ban, regexpr("\\.bnc_deshacer_confirmacion <- function\\(conf_id\\)[\\s\\S]*?\\n    \\}\\n", ban, perl = TRUE))
  expect_match(undo, "pasivos_provision_item_reversed(", fixed = TRUE)
  del <- regmatches(ban, regexpr("observeEvent\\(input\\$do_delete_conf_confirm,[\\s\\S]*?ignoreInit = TRUE\\)", ban, perl = TRUE))
  expect_match(del, "pasivos_provision_revive(", fixed = TRUE)
  expect_false(grepl("filter(confirmados(), !is.na(eliminado) & !eliminado)", ban, fixed = TRUE))
  pas <- lineas("R/pasivos_module.R")
  borrar <- regmatches(pas, regexpr("observeEvent\\(input\\$pcm_do_delete_converted[\\s\\S]*?provs\\$estado\\[idx\\]\\s+<- \"deleted\"", pas, perl = TRUE))
  expect_match(borrar, "modificar_bancos_confirmados(cid", fixed = TRUE)
  # confirmation first, movement after; a failed read stops before the provision
  expect_lt(regexpr("modificar_bancos_confirmados(cid", borrar, fixed = TRUE),
            regexpr("save_bancos_movimientos(", borrar, fixed = TRUE))
  expect_match(borrar, "if (is.null(conf)) {", fixed = TRUE)
})
