# The day modal on the engine (T05), R side: the tables' data, ids <-> the
# handlers' keys, the policy hint, and the refresh that used to fail.
# Run from the repo root: Rscript tests/test_dia_r.R
suppressPackageStartupMessages({ library(testthat); library(shiny) })
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/vencidos_module.R", "R/dia_module.R", "R/policy_engine.R")) source(f)
SAP_B1_ERP_TYPE <- "sap_b1_service_layer"

DIA <- as.Date("2026-11-03")
detalle <- data.frame(
  Empresa = c("NTS", "NTS", "NTS", "NCS", "NCS", "NTS"), Moneda = "MXN",
  Documento = c("3001", "3002", "3006", "PROV_1", "3003", "3001"),
  Factura = c("P-1", "P-2", "P-6", "", "", "P-1b"),
  Parte = c("ACEROS", "ACEROS", "ACEROS", "RENTA", "PAPELERIA", "OTRA PARTE"),
  Importe = c(22000, 11000, 4000, 18000, 640, 500),
  FechaVenc_Original = DIA, Movida = c("Falso", "Manual", "Falso", "Falso", "Políticas", "Falso"),
  source = c(SAP_B1_ERP_TYPE, SAP_B1_ERP_TYPE, SAP_B1_ERP_TYPE, "provision", "manual", SAP_B1_ERP_TYPE),
  confirmed = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE), has_sap_override = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
  provision_id = c(NA, NA, NA, "PRV-1", NA, NA), id = c(NA, NA, NA, NA, "MAN-1", NA),
  stringsAsFactors = FALSE)
tags <- data.frame(ledger = "AP", Empresa = "NTS", Moneda = "MXN", Documento = "3001", tag = "urgent", stringsAsFactors = FALSE)
agenda <- data.frame(ledger = "AP", Empresa = "NTS", Moneda = "MXN", Documento = "3002", Importe = 11000,
                     status = "pending", stringsAsFactors = FALSE)

test_that("dia_datos: one row per invoice, largest first, with its control columns", {
  d <- dia_datos(detalle, "AP", tags, agenda, puede_convertir = TRUE)
  expect_identical(d$Importe, sort(detalle$Importe, decreasing = TRUE))
  expect_identical(d$Etiqueta[d$Documento == "3001" & d$Parte == "ACEROS"], "urgent")
  expect_identical(d$.agregado, d$Documento == "3002")                 # the Agenda's key has Importe
  expect_identical(d$.bloqueada, d$Documento == "3006")
  expect_identical(d$Documento[d$.provision], "")                      # PROV_ keys show as nothing
  expect_identical(d$.acciones[d$.provision], "provision")
  expect_identical(d$.acciones[d$.bloqueada], "")                      # a paid row takes nothing
  expect_true(all(d$.acciones[!d$.provision & !d$.bloqueada] == "agregar,editar"))
  expect_identical(d$Origen[d$.provision], "provision")
  expect_identical(d$Origen[d$Documento == "3001" & d$Parte == "ACEROS"], "sap_editada")
  expect_identical(d$Origen[d$Documento == "3003"], "manual")
  expect_identical(d$Movida[d$Documento == "3002"], "manual")
  expect_identical(d$Movida[d$Documento == "3003"], "politicas")
  expect_identical(dia_datos(detalle, "AP", tags, agenda)$.acciones[d$.provision], "")   # ⚡ only for who may convert
  expect_null(dia_datos(detalle[0, ], "AP", tags))
})

test_that("ids -> the handlers' keys, by row, never by position (same document, two counterparts)", {
  ids <- dia_ids(detalle)
  expect_false(anyDuplicated(ids) > 0)
  k <- dia_llaves(detalle, ids[c(6, 1)])
  expect_identical(k$Parte, c("OTRA PARTE", "ACEROS"))
  expect_identical(k$Importe, c(500, 22000))
  expect_identical(dia_llaves(detalle, ids[4])$provision_id, "PRV-1")
  expect_identical(nrow(dia_llaves(detalle, "no-existe")), 0L)
  # The order the table shows doesn't matter: ids are the data's.
  d <- dia_datos(detalle, "AP", tags, agenda)
  expect_setequal(d$id, ids)
})

test_that("the policy hint: only counterparts whose policy moves the date", {
  pp <- data.frame(parte = c("ACEROS", "RENTA"), is_interco = FALSE, policy_order = 1L, policy_id = c("VIE", "VIE"))
  cat <- data.frame(id = "VIE", type = "weekdays", roll_direction = "forward", stringsAsFactors = FALSE)
  cat$params <- list(list(days = 5L))                                  # fridays
  expect_identical(dia_aviso_politicas(as.Date("2026-11-04"), c("ACEROS", "PAPELERIA"), pp, cat),
                   "La fecha elegida no cumple la política de pago de: ACEROS → 06/11/2026")
  expect_null(dia_aviso_politicas(as.Date("2026-11-06"), "ACEROS", pp, cat))   # a friday
  expect_null(dia_aviso_politicas(as.Date("2026-11-04"), "PAPELERIA", pp, cat))
  expect_null(dia_aviso_politicas(NA, "ACEROS", pp, cat))
  expect_null(dia_aviso_politicas(as.Date("2026-11-04"), "ACEROS", NULL, cat))
})

test_that("dia_acciones: the invoices' bar, with Mover opening on the day after", {
  dia <- reactiveVal(DIA + 1)
  a <- dia_acciones(dia)
  mover <- Find(function(x) identical(x$accion, "mover"), a)
  expect_true(is.function(mover$campo_inicial))
  meta <- isolate(hd_acciones_barra_meta(hd_acciones_barra_validar(a)))   # evaluated where the data goes out
  expect_identical(Find(function(x) identical(x$accion, "mover"), meta)$campo_inicial, "2026-11-04")
  expect_setequal(vapply(a, `[[`, "", "accion"), vapply(facturas_acciones(), `[[`, "", "accion"))
})

test_that("the day's refresh no longer uses modifyList on ctx (it failed when the row count changed)", {
  src <- paste(readLines("R/ledger_module.R", warn = FALSE), collapse = "\n")
  i <- regexpr(".refresh_ctx_detail <- function", src, fixed = TRUE)
  cuerpo <- substr(src, i, i + 3000)
  expect_false(grepl("modifyList(ctx, list(detail", cuerpo, fixed = TRUE))
  expect_true(grepl("ctx$detail <- new_detail", cuerpo, fixed = TRUE))
})
