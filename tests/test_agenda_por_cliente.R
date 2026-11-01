# Agenda de Hoy: sync state per client, fresh reads, and the rebase merge.
# Regression for 2026-10-31: the shared-agenda flag and a copy of the agenda
# were one process-global slot for every client, so a "Vaciar agenda" could
# land in the wrong file and stale copies were written back.
# S3 is an in-memory store local to this test (aws.s3 is never patched).
# Run from the repo root: Rscript tests/test_agenda_por_cliente.R
library(testthat)
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/persistence.R")
if (!exists("SAP_B1_ERP_TYPE")) SAP_B1_ERP_TYPE <- "sap_b1_service_layer"
S3_KEYS <- list(pagar_hoy = "pagar_hoy.rds", pagar_hoy_sync = "pagar_hoy_sync.rds",
                agenda_sync_config = "agenda_sync_config.rds", sync_versions = "sync_versions.rds")

.mem <- new.env()
.s3_key <- function(key, client_id = NULL) {
  if (is.null(client_id) || !nzchar(client_id)) stop("sin client_id")
  paste0(client_id, "/", key)
}
.s3_read_with <- function(key, client_id = NULL) .mem[[.s3_key(key, client_id)]]
.s3_write <- function(obj, key, client_id = NULL) assign(.s3_key(key, client_id), obj, envir = .mem)
.s3_existe <- function(key, client_id) !is.null(.mem[[.s3_key(key, client_id)]])
bump_sync_version <- function(...) invisible(TRUE)

fila <- function(id, doc, status = "pending", importe = 100) tibble(
  id = id, ledger = "AP", Empresa = "NCS", Moneda = "MXN", Documento = doc, Parte = "P",
  tipo_item = "factura", Importe = importe, status = status)

test_that("el estado de sincronización es por cliente", {
  save_agenda_sync_config(TRUE,  "x", client_id = "networks")
  save_agenda_sync_config(FALSE, "x", client_id = "otro")
  expect_true(agenda_sync_activa("networks"))
  expect_false(agenda_sync_activa("otro"))
  expect_false(agenda_sync_activa(NULL))
  # Writing for "otro" (sync off) never flips networks.
  save_pagar_hoy(fila("o1", "D-O"), "dev", client_id = "otro")
  expect_true(agenda_sync_activa("networks"))
  expect_false(exists(".agenda_sync", envir = .GlobalEnv))
})

test_that("cada cliente escribe en su propio archivo", {
  save_pagar_hoy(fila("n1", "D1"), "dev", client_id = "networks")
  expect_equal(.mem[["networks/pagar_hoy_sync.rds"]]$id, "n1")
  expect_equal(.mem[["otro/pagar_hoy_user_dev.rds"]]$id, "o1")
  expect_null(.mem[["networks/pagar_hoy_user_dev.rds"]])
})

test_that("Vaciar agenda con la sincronización activa vacía el archivo compartido", {
  .mem[["networks/pagar_hoy_sync.rds"]] <- bind_rows(fila("k1", "FP311907"), fila("k2", "FP314176"))
  ph <- safe_load_pagar_hoy("dev", client_id = "networks") |> filter(status != "pending")
  save_pagar_hoy(ph, "dev", client_id = "networks")
  expect_equal(nrow(.mem[["networks/pagar_hoy_sync.rds"]]), 0)
})

test_that("safe_load lee S3, no una copia vieja", {
  .mem[["networks/pagar_hoy_sync.rds"]] <- fila("a", "A")
  expect_equal(safe_load_pagar_hoy("dev", "networks")$id, "a")
  .mem[["networks/pagar_hoy_sync.rds"]] <- fila("b", "B")       # another process wrote
  expect_equal(safe_load_pagar_hoy("dev", "networks")$id, "b")
})

test_that("rebase: lo mío gana, lo ajeno se queda, lo borrado no regresa", {
  base   <- bind_rows(fila("1", "D1"), fila("2", "D2"), fila("3", "D3"))
  # this session: confirms 1, removes 2, adds 4
  nuevo  <- bind_rows(fila("1", "D1", "confirmed"), fila("3", "D3"), fila("4", "D4"))
  # meanwhile in S3: someone added 5 and removed 3
  fresco <- bind_rows(fila("1", "D1"), fila("2", "D2"), fila("5", "D5"))
  out <- rebase_pagar_hoy(nuevo, base, fresco)
  expect_setequal(out$id, c("1", "4", "5"))
  expect_equal(out$status[out$id == "1"], "confirmed")
})

test_that("rebase: dos sesiones agregan la misma factura y queda una", {
  base   <- fila("1", "D1")
  nuevo  <- bind_rows(fila("1", "D1"), fila("mio", "D9"))
  fresco <- bind_rows(fila("1", "D1"), fila("suyo", "D9"))
  out <- rebase_pagar_hoy(nuevo, base, fresco)
  expect_equal(sum(out$Documento == "D9"), 1)
  expect_true("mio" %in% out$id)
})

test_that("save con base no pisa lo que otra sesión agregó", {
  .mem[["networks/pagar_hoy_sync.rds"]] <- fila("x", "DX")
  base <- safe_load_pagar_hoy("dev", "networks")
  .mem[["networks/pagar_hoy_sync.rds"]] <- bind_rows(fila("x", "DX"), fila("y", "DY"))  # other process
  escrito <- save_pagar_hoy(bind_rows(base, fila("z", "DZ")), "dev", client_id = "networks", base = base)
  expect_setequal(.mem[["networks/pagar_hoy_sync.rds"]]$id, c("x", "y", "z"))
  expect_setequal(escrito$id, c("x", "y", "z"))
})

test_that("si falla la lectura, guardar con base no escribe nada", {
  .mem[["networks/pagar_hoy_sync.rds"]] <- bind_rows(fila("x", "DX"), fila("y", "DY"))
  base <- safe_load_pagar_hoy("dev", "networks")
  leer_bien <- .s3_read_with
  .s3_read_with <<- function(key, client_id = NULL)
    if (grepl("pagar_hoy_sync", key)) NULL else leer_bien(key, client_id)   # read fails; object exists
  expect_error(save_pagar_hoy(bind_rows(base, fila("z", "DZ")), "dev", client_id = "networks", base = base),
               "could not read")
  .s3_read_with <<- leer_bien
  expect_setequal(.mem[["networks/pagar_hoy_sync.rds"]]$id, c("x", "y"))   # untouched
})

test_that("no queda estado de agenda global en el código", {
  archivos <- c("app.R", list.files("R", "[.]R$", recursive = TRUE, full.names = TRUE))
  hits <- unlist(lapply(archivos, function(f) {
    l <- readLines(f, warn = FALSE, encoding = "UTF-8")
    l <- l[!grepl("^[[:space:]]*#", l)]
    if (any(grepl(".agenda_sync$|.agenda_user_cache", l, fixed = TRUE) |
            grepl("[.]agenda_sync[$]", l))) f
  }))
  expect_length(hits, 0)
})
