# E3b-C: the requisition's evidence clip in the day's detail (Calendario)
# and in the confirmations' history (Bancos).
# Run from the repo root: Rscript --vanilla tests/test_e3b_clip_calendario.R
suppressPackageStartupMessages({ library(testthat); library(shiny) })
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R")) source(f)

test_that("the history of confirmations has the clip piece (class C only has it if the registry adds it)", {
  expect_false("adjuntos" %in% c(hd_clases$C$siempre, hd_clases$C$opcional))
  expect_true("adjuntos" %in% hd_piezas_de("historial_conf"))
  # Nothing else in the history's pieces changes.
  expect_identical(setdiff(hd_piezas_de("historial_conf", c("acc_fila")), "adjuntos"),
                   HD_PIEZAS_VALIDAS[HD_PIEZAS_VALIDAS %in% union(hd_clases$C$siempre, "acc_fila")])
})

# ── The day's detail (R/dia_module.R) ────────────────────────────────────────
for (f in c("R/hd_columnas.R", "R/hd_table.R", "R/vencidos_module.R", "R/dia_module.R", "R/policy_engine.R")) source(f)
SAP_B1_ERP_TYPE <- "sap_b1_service_layer"
for (e in parse("R/global.R", keep.source = FALSE))
  if (is.call(e) && length(e) > 1L && identical(e[[2]], as.name("is_erp_sourced"))) eval(e)
has_capability <- function(user, cap, empresa = NULL) TRUE
puede <- function(usuario, clave) TRUE

DIA <- as.Date("2026-11-03")
# A provision with evidence, an item converted from another provision, a
# provision whose requisition has no evidence yet, and an ERP invoice.
detalle <- data.frame(
  Empresa = c("NCS", "NTS", "NCS", "NTS"), Moneda = "MXN",
  Documento = c("PROV_1", "4001", "PROV_3", "3001"), Factura = "", Parte = c("RENTA", "CFE", "TELMEX", "ACEROS"),
  Importe = c(18000, 5000, 700, 22000), FechaVenc_Original = DIA, Movida = "Falso",
  source = c("provision", "manual", "provision", SAP_B1_ERP_TYPE), confirmed = FALSE, has_sap_override = FALSE,
  provision_id = c("PRV-1", "PRV-2", "PRV-3", NA), id = c(NA, "MAN-2", NA, NA), stringsAsFactors = FALSE)
RESP <- data.frame(provision_id = c("PRV-1", "PRV-2", "PRV-3"), requisicion_id = c("R1", "R2", "R3"),
                   total = c(2L, 1L, 0L), stringsAsFactors = FALSE)
id_de <- function(pid, det = detalle) dia_ids(det)[match(pid, det$provision_id)]

test_that("dia_datos: each row with a requisition carries its count; the rest, no clip", {
  d <- dia_datos(detalle, "AP", NULL, NULL, clip = RESP)
  expect_identical(d$.adjuntos[match(id_de(c("PRV-1", "PRV-2", "PRV-3")), d$id)], c(2L, 1L, 0L))
  expect_identical(d$.adjuntos[d$Documento == "4001"], 1L)                       # the converted item too
  expect_identical(d$.adjuntos[d$Documento == "3001"], 0L)
  expect_identical(dia_datos(detalle, "AP", NULL, NULL)$.adjuntos, rep(0L, 4))   # no answer: no clip
  expect_identical(dia_datos(detalle, "AP", NULL, NULL, clip = RESP[0, ])$.adjuntos, rep(0L, 4))   # no permission
})

# The module, with a fake `shared` that records the contract's calls.
G <- new.env()
compartido <- function(permiso = TRUE, contrato = TRUE) {
  G$clip <- list(); G$abre <- character(0); G$version <- reactiveVal(0L); G$nonce <- 0L
  base <- list(current_user = function() "u", usuario = function() list(user = "u"), tags_db = function() NULL,
               pagar_hoy_db = function() NULL, company_map = function() NULL,
               partner_policies_db = function() NULL, policy_catalog_db = function() NULL)
  if (!contrato) return(base)
  c(base, list(
    req_version = G$version,
    req_clip = function(provision_ids) {
      G$clip[[length(G$clip) + 1L]] <- provision_ids
      if (permiso) RESP[RESP$provision_id %in% provision_ids, , drop = FALSE] else RESP[0, ]
    },
    req_abrir_evidencia = function(provision_id) { G$abre <- c(G$abre, provision_id); invisible(TRUE) }))
}
nada <- function(...) invisible(NULL)
manejadores <- list(agregar = nada, quitar = nada, etiquetar = nada, mover = nada, restaurar = nada,
                    eliminar = nada, editar = nada, crudos = nada)
modulo <- function(id, ctx, shared) shiny::moduleServer(id, function(input, output, session)
  dia_server(input, output, session, ctx, "AP", shared, manejadores))
clic <- function(session, id, tabla = "detalle") {
  G$nonce <- G$nonce + 1L
  args <- setNames(list(list(accion = "adjuntos", ids = id, nonce = G$nonce)), paste0(tabla, "-accion"))
  do.call(session$setInputs, args)
  session$flushReact()
}
datos_de <- function(session) attr(session$returned, "datos")()

test_that("the day: one req_clip per set of provisions; a refresh with the same ids doesn't ask again", {
  ctx <- reactiveVal(list(detail = detalle, sel_date = DIA))
  testServer(modulo, args = list(ctx = ctx, shared = compartido()), {
    session$flushReact()
    d <- datos_de(session)
    expect_identical(length(G$clip), 1L)
    expect_identical(G$clip[[1]], c("PRV-1", "PRV-2", "PRV-3"))
    expect_identical(d$.adjuntos[match(id_de(c("PRV-1", "PRV-2", "PRV-3")), d$id)], c(2L, 1L, 0L))
    # The day refreshes (another save, same rows in another order): no read.
    ctx(list(detail = detalle[4:1, ], sel_date = DIA, prov_refresh = TRUE)); session$flushReact()
    expect_identical(nrow(datos_de(session)), 4L)
    expect_identical(length(G$clip), 1L)
    # req_version() moves (a requisition or its evidence changed): asked again.
    G$version(1L); session$flushReact(); datos_de(session)
    expect_identical(length(G$clip), 2L)
    # Another provision comes into the day: asked again, with the new set.
    det2 <- rbind(detalle, transform(detalle[1, ], Documento = "PROV_4", provision_id = "PRV-4", Importe = 90000))
    ctx(list(detail = det2, sel_date = DIA)); session$flushReact(); datos_de(session)
    expect_identical(length(G$clip), 3L)
    expect_identical(G$clip[[3]], c("PRV-1", "PRV-2", "PRV-3", "PRV-4"))
  })
})

test_that("the clip opens the row's provision by id, in both views; rows without a clip open nothing", {
  ctx <- reactiveVal(list(detail = detalle, sel_date = DIA))
  testServer(modulo, args = list(ctx = ctx, shared = compartido()), {
    session$flushReact()
    clic(session, id_de("PRV-2"))
    expect_identical(G$abre, "PRV-2")
    # Another session adds a larger row on top: the same id is the same row.
    det2 <- rbind(transform(detalle[1, ], Documento = "PROV_9", provision_id = "PRV-9", Importe = 99000), detalle)
    ctx(list(detail = det2, sel_date = DIA)); session$flushReact()
    clic(session, id_de("PRV-2", det2), "resumen")
    clic(session, id_de("PRV-1", det2), "resumen")
    expect_identical(G$abre, c("PRV-2", "PRV-2", "PRV-1"))
    # No clip: a requisition without evidence, PRV-9 (no requisition), an
    # ERP invoice, a forged id, no id.
    clic(session, id_de("PRV-3", det2)); clic(session, id_de("PRV-9", det2))
    clic(session, dia_ids(det2)[det2$Documento == "3001"]); clic(session, "no-existe"); clic(session, character(0))
    expect_identical(G$abre, c("PRV-2", "PRV-2", "PRV-1"))
  })
})

test_that("without can_view_requisiciones (req_clip answers 0 rows): no clip, and a forged clip opens nothing", {
  ctx <- reactiveVal(list(detail = detalle, sel_date = DIA))
  testServer(modulo, args = list(ctx = ctx, shared = compartido(permiso = FALSE)), {
    session$flushReact()
    expect_identical(datos_de(session)$.adjuntos, rep(0L, 4))
    clic(session, id_de("PRV-1")); clic(session, id_de("PRV-2"), "resumen")
    expect_identical(G$abre, character(0))
  })
})

test_that("a shared without the contract: no clip and no error", {
  ctx <- reactiveVal(list(detail = detalle, sel_date = DIA))
  testServer(modulo, args = list(ctx = ctx, shared = compartido(contrato = FALSE)), {
    session$flushReact()
    expect_identical(datos_de(session)$.adjuntos, rep(0L, 4))
    expect_no_error(clic(session, id_de("PRV-1")))
  })
})

test_that("Resumen shows the clip too (class B, where it is optional)", {
  expect_false("adjuntos" %in% hd_piezas_de("detalle_dia_resumen"))
  expect_true("adjuntos" %in% hd_piezas_de("detalle_dia_detalle"))
  expect_match(paste(deparse(body(dia_server)), collapse = " "),
               'tabla = "detalle_dia_resumen", piezas = "adjuntos"', fixed = TRUE)
})

# ── The confirmations' history (R/bancos_module.R, "historial_conf") ─────────
# The real bancosServer in testServer over the in-memory S3 (s3_memoria,
# restored at the end), as test_bancos_doble_aviso.R does.
source("tests/helpers/s3_memoria.R")
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
    eval(e, envir = globalenv())
cargar("R/global.R", "fmt_money")
cargar("R/settings/settings_helpers.R", c(".schema_ctas_bancos", "load_ctas_bancos"))
for (f in c("R/bancos_parser.R", "R/bancos_detect.R", "R/bancos_persistence.R", "R/bancos_formatos.R",
            "R/bancos_module.R")) suppressMessages(source(f))
log_action <- function(...) invisible(NULL)
showNotification <- function(...) invisible("id")
puede_o_avisa <- function(...) TRUE
bump_sync_version <- function(...) invisible(NULL)

CID_B <- "acme"
confirmacion <- function(id, pid, cuando, tipo = "pago", eliminado = FALSE)
  tibble::tibble(confirmacion_id = id, agenda_item_id = paste0("ag-", id), empresa = "NTS", parte = "PROV",
                 documento = paste0("D-", id), importe = 100, moneda = "MXN", cuenta_id = "c1",
                 fecha = as.Date("2026-10-30"), tipo = tipo, mov_id = NA_character_, provision_id = pid,
                 confirmado_at = as.POSIXct("2026-11-01 10:00:00") + cuando, eliminado = eliminado)
# k1: a paid provision with evidence; k2: one whose requisition has none;
# k3: a cobro (no provision); k4: deleted (not in the history).
conf_h <- .normalize(dplyr::bind_rows(confirmacion("k1", "P1", 3), confirmacion("k2", "P2", 2),
                                      confirmacion("k3", NA, 1, "cobro"), confirmacion("k4", "P4", 0, eliminado = TRUE)),
                     .schema_bancos_confirmados)
legado_h <- tibble::tibble(id = "L1", created_at = as.POSIXct("2026-10-01"), FechaPago = as.Date("2026-10-01"),
                           Empresa = "NTS", Parte = "OTRO", Documento = "LEG-1", Importe = 50, Moneda = "MXN", tipo = "pago")
abono_h <- .normalize(tibble::tibble(id = "A1", ledger = "AP", Empresa = "NTS", Moneda = "MXN", Documento = "D-9",
                                     Parte = "PROV", importe = 50, fecha_abono = as.Date("2026-10-30"), notas = "",
                                     created_by = "t", created_at = as.POSIXct("2026-10-15"), status = "active"),
                      .schema_abonos)
RESP_H <- data.frame(provision_id = c("P1", "P2", "P4"), requisicion_id = c("R1", "R2", "R4"),
                     total = c(2L, 0L, 5L), stringsAsFactors = FALSE)
compartido_bancos <- function(conf, permiso = TRUE, contrato = TRUE) {
  G$clip <- list(); G$abre <- character(0); G$version <- reactiveVal(0L); G$nonce <- 0L
  G$conf <- reactiveVal(conf)
  base <- list(
    effective_client_id = function() CID_B, home_client_id = function() CID_B, current_user = function() "luis",
    usuario = reactive("luis"), company_map = reactiveVal(c(NTS = "Networks Trucking")),
    visible_initials = reactiveVal(NULL), bancos_movimientos = reactiveVal(.schema_bancos_movimientos()),
    bancos_confirmados = G$conf, conciliacion_rv = reactiveVal(legado_h), ctas_cuentas = reactiveVal(tibble::tibble()),
    papelera_rv = reactiveVal(NULL), abonos_db = reactiveVal(abono_h), manual_inv = reactiveVal(NULL),
    pasivos_provisions_db = reactiveVal(NULL), sap_data = reactiveVal(list()), empresas_db = reactiveVal(NULL),
    suppress_ledger_prov_refresh = reactiveVal(FALSE))
  if (!contrato) return(base)
  c(base, list(
    req_version = G$version,
    req_clip = function(provision_ids) {
      G$clip[[length(G$clip) + 1L]] <- provision_ids
      if (permiso) RESP_H[RESP_H$provision_id %in% provision_ids, , drop = FALSE] else RESP_H[0, ]
    },
    req_abrir_evidencia = function(provision_id) { G$abre <- c(G$abre, provision_id); invisible(TRUE) }))
}
clic_h <- function(session, id) {
  G$nonce <- G$nonce + 1L
  session$setInputs(`historial-accion` = list(accion = "adjuntos", ids = id, nonce = G$nonce))
  session$flushReact()
}
clip_de <- function(h) setNames(h$.adjuntos, h$id)

test_that("history: only a live confirmation with a requisition carries the clip; legado and abono, never", {
  testServer(bancosServer, args = list(shared = compartido_bancos(conf_h)), {
    session$flushReact()
    h <- historial_datos()
    expect_setequal(h$id, c("conf:k1", "conf:k2", "conf:k3", "legado:L1", "abono:A1"))
    expect_identical(clip_de(h)[c("conf:k1", "conf:k2", "conf:k3", "legado:L1", "abono:A1")],
                     c(`conf:k1` = 2L, `conf:k2` = 0L, `conf:k3` = 0L, `legado:L1` = 0L, `abono:A1` = 0L))
    expect_identical(length(G$clip), 1L)
    expect_identical(G$clip[[1]], c("P1", "P2"))                       # k4 is deleted; k3 has none
    expect_true(any(grepl('tabla = "historial_conf"', readLines("R/bancos_module.R", encoding = "UTF-8"), fixed = TRUE)) &&
                any(grepl('piezas = c("acc_fila", "adjuntos")', readLines("R/bancos_module.R", encoding = "UTF-8"), fixed = TRUE)))
  })
})

test_that("history: a refresh with the same provisions doesn't ask again; req_version() or a new one does", {
  testServer(bancosServer, args = list(shared = compartido_bancos(conf_h)), {
    session$flushReact(); historial_datos()
    G$conf(conf_h[4:1, ]); session$flushReact(); historial_datos()     # same set, another order
    expect_identical(length(G$clip), 1L)
    G$version(1L); session$flushReact(); historial_datos()
    expect_identical(length(G$clip), 2L)
    G$conf(.normalize(dplyr::bind_rows(conf_h, confirmacion("k5", "P5", 9)), .schema_bancos_confirmados))
    session$flushReact(); historial_datos()
    expect_identical(length(G$clip), 3L)
    expect_identical(G$clip[[3]], c("P1", "P2", "P5"))
  })
})

test_that("history: the clip opens the confirmation's provision by id; other rows and forged ids open nothing", {
  testServer(bancosServer, args = list(shared = compartido_bancos(conf_h)), {
    session$flushReact(); historial_datos()
    clic_h(session, "conf:k1")
    expect_identical(G$abre, "P1")
    # A newer confirmation comes on top: the same id is still k1's.
    G$conf(.normalize(dplyr::bind_rows(confirmacion("k6", "P2", 99), conf_h), .schema_bancos_confirmados))
    session$flushReact(); historial_datos()
    clic_h(session, "conf:k1")
    expect_identical(G$abre, c("P1", "P1"))
    # No clip: k2 (no evidence), k3 (no provision), k4 (deleted, even if its
    # provision has a clip), legado and abono with a forged clip, unknown ids.
    for (id in c("conf:k2", "conf:k3", "conf:k4", "legado:L1", "abono:A1", "conf:nada", "P1", "")) clic_h(session, id)
    expect_identical(G$abre, c("P1", "P1"))
  })
})

test_that("history without can_view_requisiciones (0 rows) or without the contract: no clip, nothing opens", {
  testServer(bancosServer, args = list(shared = compartido_bancos(conf_h, permiso = FALSE)), {
    session$flushReact()
    expect_true(all(historial_datos()$.adjuntos == 0L))
    clic_h(session, "conf:k1")
    expect_identical(G$abre, character(0))
  })
  testServer(bancosServer, args = list(shared = compartido_bancos(conf_h, contrato = FALSE)), {
    session$flushReact()
    expect_true(all(historial_datos()$.adjuntos == 0L))
    expect_no_error(clic_h(session, "conf:k1"))
  })
  # An empty history still builds.
  testServer(bancosServer, args = list(shared = compartido_bancos(.schema_bancos_confirmados())), {
    session$flushReact()
    expect_no_error(historial_datos())
  })
})
s3_memoria_restaurar()

# ── The real contract: requisiciones_clip() over the Requisiciones fixture ───
# (in-memory S3; p-luz and p-pagada have requisitions, p-libre1 has none).
Sys.setenv(REQ_NIVEL = "compras", REQ_USUARIO = "dani", HD_ARRANQUE_NULL = "", REQ_FALLA_LECTURA = "",
           REQ_OVERRIDES = "", REQ_ABRIR_PROVISION = "")
invisible(suppressMessages(source("tests/helpers/requisiciones_fixture_app.R")))
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
u_de <- function(nivel) list(user = "x", tier = nivel, permisos = permisos_resueltos(nivel))
local({
  f <- tempfile(fileext = ".png")
  writeBin(magick::image_write(magick::image_blank(30, 20, "red"), format = "png"), f)
  r <- .req_mem[["fixture/requisiciones.rds"]]
  evidencia_adjuntar(CID, r$id[r$titulo == "Luz bodega 3"], adjunto_preparar(f, "luz.png", "archivo"), "eli")
})
real <- transform(detalle[1:3, ], provision_id = c("p-luz", "p-pagada", "p-libre1"))
con_contrato <- function(nivel) c(compartido(contrato = FALSE), list(
  req_version = G$version,
  req_clip = function(provision_ids) { G$clip[[length(G$clip) + 1L]] <- provision_ids
                                       requisiciones_clip(CID, u_de(nivel), provision_ids) },
  req_abrir_evidencia = function(provision_id) { G$abre <- c(G$abre, provision_id); invisible(TRUE) }))

test_that("real contract: who sees Requisiciones gets the count; whoever doesn't, no clip and nothing opens", {
  ctx <- reactiveVal(list(detail = real, sel_date = DIA))
  testServer(modulo, args = list(ctx = ctx, shared = con_contrato("compras")), {
    session$flushReact()
    d <- datos_de(session)
    expect_identical(d$.adjuntos[match(id_de(c("p-luz", "p-pagada", "p-libre1"), real), d$id)], c(1L, 0L, 0L))
    clic(session, id_de("p-luz", real)); clic(session, id_de("p-pagada", real)); clic(session, id_de("p-libre1", real))
    expect_identical(G$abre, "p-luz")
  })
  testServer(modulo, args = list(ctx = ctx, shared = con_contrato("finance")), {
    session$flushReact()
    expect_identical(datos_de(session)$.adjuntos, rep(0L, 3))
    expect_identical(length(G$clip), 1L)                       # asked once, answered nothing
    clic(session, id_de("p-luz", real))
    expect_identical(G$abre, character(0))
  })
})
