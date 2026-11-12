# Panorama (front B1): the pure data of R/panorama_datos.R, the `clase`
# column of R/avisos.R, and the module's server (what each user receives).
# S3 is an in-memory store local to this test (as in tests/test_avisos.R);
# aws.s3 is never patched.
# Run from the repo root: Rscript tests/test_panorama_datos.R
library(testthat)
suppressPackageStartupMessages(library(shiny))
`%||%` <- function(a, b) if (is.null(a)) b else a
.i18n_dir <- file.path(getwd(), "www", "i18n")
source("R/i18n.R")
source("R/tier_registry.R")
source("R/permisos.R")
source("R/avisos.R")
source("R/panorama_datos.R")
source("R/panorama_module.R")

.mem <- new.env()
.s3_read_with <- function(key, client_id = NULL) .mem[[paste0(client_id, "/", key)]]
.s3_leer_para_escribir <- function(key, client_id) .mem[[paste0(client_id, "/", key)]]
.s3_write <- function(obj, key, client_id = NULL) assign(paste0(client_id, "/", key), obj, envir = .mem)
auth_load_usuarios <- function(client_id = NULL) data.frame(
  username = c("ana", "beto"), tier = c(nivel_ti(), niveles_cliente()[2]),
  deleted = FALSE, activo = TRUE, stringsAsFactors = FALSE)
usr <- function(user, tier) list(user = user, tier = tier, permisos = permisos_resueltos(tier))
CID <- "cliente_panorama"
HOY <- as.Date("2026-11-12")
NIV <- niveles_cliente()
COMPRAS <- NIV[length(NIV)]   # the lowest client tier

# ── Totals never mix currencies ──────────────────────────────────────────────
test_that("cada moneda es su propio renglón, MXN primero", {
  t <- .pan_por_moneda(c("USD", "MXN", "mxn", NA, "EUR"), c(10, 100, 50, 5, 1))
  expect_identical(t$moneda, c("MXN", "EUR", "USD"))
  expect_equal(t$total, c(155, 1, 10))   # NA currency counts as MXN, never as a sum of others
  expect_equal(t$n, c(3L, 1L, 1L))
  expect_equal(nrow(.pan_por_moneda(character(), numeric())), 0)
})

test_that("Pagos de hoy: solo CxP pendiente de hoy o antes, sin abonos", {
  ph <- data.frame(
    ledger = c("AP", "AP", "AR", "AP", "AP", "AP"),
    status = c("pending", "confirmed", "pending", "pending", "pending", "pending"),
    tipo_item = c("factura", "factura", "factura", "abono", NA, "factura"),
    Moneda = c("MXN", "MXN", "MXN", "MXN", "USD", "MXN"),
    Importe = c(100, 200, 300, 400, -50, 600),
    FechaVenc = as.Date(c("2026-11-12", "2026-11-12", "2026-11-12", "2026-11-12", "2026-11-10", "2026-11-13")),
    stringsAsFactors = FALSE)
  t <- panorama_pagos_hoy(ph, HOY)
  expect_identical(t$moneda, c("MXN", "USD"))
  expect_equal(t$total, c(100, 50))
  expect_equal(nrow(panorama_pagos_hoy(NULL, HOY)), 0)
})

test_that("Cobros de hoy: las filas del Calendario con fecha efectiva hoy", {
  df <- data.frame(FechaEff = as.Date(c("2026-11-12", "2026-11-12", "2026-11-11")),
                   Moneda = c("MXN", "USD", "MXN"), `Saldo vencido` = c(1000, 20, 999),
                   check.names = FALSE, stringsAsFactors = FALSE)
  t <- panorama_cobros_hoy(df, HOY)
  expect_equal(t$total, c(1000, 20)); expect_equal(sum(t$n), 2)
  expect_equal(nrow(panorama_cobros_hoy(data.frame(x = 1), HOY)), 0)
})

test_that("Pasivos en 3 días: provisiones abiertas, con la fecha y el monto que se corrigieron", {
  p <- data.frame(
    estado = c("provisional", "provisional", "converted", "provisional", "provisional"),
    fecha_efectiva = as.Date(c("2026-11-13", "2026-11-20", "2026-11-13", "2026-11-11", "2026-11-14")),
    fecha_efectiva_override = as.Date(c(NA, "2026-11-15", NA, NA, NA)),
    amount_pago = c(10, 20, 30, 40, 50), amount_pago_override = c(NA, NA, NA, NA, 55),
    moneda_pago = "MXN", stringsAsFactors = FALSE)
  r <- panorama_pasivos_pronto(p, HOY)
  expect_equal(r$totales$total, 10 + 20 + 55)   # override date 15 nov is inside; overdue and converted are not
  expect_equal(r$primera, as.Date("2026-11-13"))
  expect_true(is.na(panorama_pasivos_pronto(NULL, HOY)$primera))
})

# ── Avisos: clase, feed, solicitudes, destacados ─────────────────────────────
test_that("clase: un publicado sin clase es comunicado; una clase desconocida falla", {
  avisos_agregar(CID, list(tipo = "publicado", titulo = "Viejo estilo", niveles = NIV), por = "ana")
  avisos_agregar(CID, list(tipo = "publicado", clase = "recordatorio", titulo = "Conciliar",
                           niveles = NIV, severidad = "urgente"), por = "ana")
  expect_error(avisos_agregar(CID, list(tipo = "publicado", clase = "chisme", titulo = "x", niveles = NIV)),
               "clase desconocida")
  df <- avisos_leer(CID)
  expect_identical(df$clase, c("comunicado", "recordatorio"))
  # A row written before the column existed reads as NA and is a comunicado.
  viejo <- df[1, ]; viejo$clase <- NA_character_
  expect_identical(avisos_clase(viejo), "comunicado")
  auto <- df[1, ]; auto$tipo <- "automatico"; auto$clase <- NA_character_
  expect_true(is.na(avisos_clase(auto)))
})

aviso <- function(...) {
  a <- list(...)
  tibble::tibble(id = a$id %||% "x", tipo = a$tipo %||% "automatico", clase = a$clase %||% NA_character_,
                 titulo = a$titulo %||% "t", cuerpo = "", severidad = a$severidad %||% "info",
                 niveles = "[]", usuarios = a$usuarios %||% "[]", permiso = NA_character_,
                 clave = NA_character_, contador = 1L, enlace = a$enlace %||% NA_character_,
                 creado_por = a$por %||% "ana",
                 creado = a$creado %||% as.POSIXct("2026-11-12 10:00", tz = "UTC"),
                 actualizado = a$creado %||% as.POSIXct("2026-11-12 10:00", tz = "UTC"),
                 vence = a$vence %||% as.POSIXct(NA, tz = "UTC"), retirado = FALSE,
                 retirado_por = NA_character_, retirado_en = as.POSIXct(NA, tz = "UTC"),
                 hechos = a$hechos %||% "[]")
}

test_that("tipo del feed: solicitud, alerta, actividad, comunicado", {
  df <- rbind(aviso(tipo = "publicado", clase = "solicitud"), aviso(tipo = "personal", clase = "solicitud"),
              aviso(severidad = "atencion"), aviso(severidad = "urgente"), aviso(),
              aviso(tipo = "personal"), aviso(tipo = "publicado", clase = "reconocimiento"),
              aviso(tipo = "publicado"))
  expect_identical(panorama_tipo_feed(df), c("sol", "sol", "ale", "ale", "act", "act", "com", "com"))
})

test_that("nuevo = después de la última visita; primera visita: nada es nuevo", {
  df <- rbind(aviso(id = "a", creado = as.POSIXct("2026-11-12 09:00", tz = "UTC")),
              aviso(id = "b", creado = as.POSIXct("2026-11-12 11:00", tz = "UTC")))
  f <- panorama_feed(df, "2026-11-12 10:00:00 UTC")
  expect_identical(f$id, c("b", "a"))            # newest first
  expect_identical(f$nuevo, c(TRUE, FALSE))
  expect_false(any(panorama_feed(df, NULL)$nuevo))
  expect_equal(nrow(panorama_feed(NULL)), 0)
})

test_that("Mis solicitudes: solo las personales de este usuario que no ha marcado hechas", {
  df <- rbind(aviso(id = "s1", tipo = "personal", clase = "solicitud", usuarios = '["beto"]'),
              aviso(id = "s2", tipo = "personal", clase = "solicitud", usuarios = '["beto"]', hechos = '["Beto"]'),
              aviso(id = "c1", tipo = "personal", usuarios = '["beto"]'),
              aviso(id = "p1", tipo = "publicado", clase = "solicitud"))
  expect_identical(panorama_mis_solicitudes(df, "beto")$id, "s1")
  expect_equal(nrow(panorama_mis_solicitudes(df, "")), 0)
})

test_that("Destacados: urgentes primero, luego lo más nuevo; máximo 3; nunca solicitudes ni automáticos", {
  t <- function(h) as.POSIXct(sprintf("2026-11-12 %02d:00", h), tz = "UTC")
  df <- rbind(aviso(id = "viejo_urg", tipo = "publicado", clase = "recordatorio", severidad = "urgente", creado = t(1)),
              aviso(id = "nuevo", tipo = "publicado", creado = t(9)),
              aviso(id = "medio", tipo = "publicado", clase = "reconocimiento", creado = t(5)),
              aviso(id = "mas_viejo", tipo = "publicado", creado = t(2)),
              aviso(id = "sol", tipo = "publicado", clase = "solicitud", creado = t(10)),
              aviso(id = "auto", creado = t(11)))
  expect_identical(panorama_destacados(df)$id, c("viejo_urg", "nuevo", "medio"))
})

# ── SAP and Clima ────────────────────────────────────────────────────────────
test_that("SAP: un respaldo o datos de más de 24 h son viejos", {
  ahora <- as.POSIXct("2026-11-12 12:00", tz = "UTC")
  vivo <- list(ts = ahora - 3600, is_live = TRUE)
  expect_false(panorama_sap_estado(list(AR = vivo, AP = vivo), ahora)$viejo)
  expect_true(panorama_sap_estado(list(AR = vivo, AP = list(ts = ahora - 60, is_live = FALSE)), ahora)$viejo)
  expect_true(panorama_sap_estado(list(AR = list(ts = ahora - 30 * 3600, is_live = TRUE)), ahora)$viejo)
  expect_false(panorama_sap_estado(list(AR = NULL, AP = NULL), ahora)$cargado)
})

test_that("Clima: neblina manda, luego nublado, medio y despejado", {
  f0 <- panorama_feed(aviso())
  expect_identical(panorama_clima(f0)$clima, "despejado")
  expect_identical(panorama_clima(f0, n_solicitudes = 1)$clima, "medio")
  expect_identical(panorama_clima(panorama_feed(aviso(severidad = "atencion")))$clima, "medio")
  urg <- panorama_feed(aviso(tipo = "publicado", severidad = "urgente"))
  expect_identical(panorama_clima(urg)$clima, "nublado")
  expect_identical(panorama_clima(urg, sap = list(viejo = TRUE))$clima, "neblina")
  r <- panorama_clima(urg, n_solicitudes = 2, n_pagos_hoy = 3)
  expect_equal(r$pendientes, 5L); expect_equal(r$urgentes, 1L)
  expect_lte(length(r$razones), 3)
  # every reason is a dictionary key
  dic <- jsonlite::fromJSON("www/i18n/es.json")
  expect_true(all(vapply(r$razones, function(x) x$clave %in% names(dic), logical(1))))
})

test_that("formato: monto y fecha larga en los dos idiomas", {
  expect_identical(panorama_monto(842300), "$842,300.00")
  expect_identical(panorama_fecha_larga(HOY, "es"), "jueves 12 de noviembre")
  expect_identical(panorama_fecha_larga(HOY, "en"), "Thursday, November 12")
  expect_identical(panorama_saludo(9), "panorama.saludo.dias")
  expect_identical(panorama_saludo(20), "panorama.saludo.noches")
})

# ── The module: what each user receives ──────────────────────────────────────
test_that("el servidor solo manda los avisos del nivel del usuario, y Hecho solo cierra los suyos", {
  rm(list = ls(.mem), envir = .mem)
  avisos_agregar(CID, list(tipo = "publicado", titulo = "Solo para TI", niveles = nivel_ti()), por = "ana")
  avisos_agregar(CID, list(tipo = "publicado", titulo = "Para todos", niveles = NIV), por = "ana")
  sid <- avisos_personales(CID, "beto", list(tipo = "personal", clase = "solicitud",
                                             titulo = "Enviar soporte", autor = "ana"))
  avisos_personales(CID, "ana", list(tipo = "personal", clase = "solicitud", titulo = "De otro", autor = "beto"))
  html_de <- function(tier, user) {
    shared <- list(
      effective_client_id = function() CID, usuario = function() usr(user, tier),
      current_user_info = function() list(user = user, name = "Beto Ruiz", is_staff = FALSE),
      tier_efectivo = function() tier, pagar_hoy_db = function() NULL,
      pasivos_provisions_db = function() NULL, sap_snapshot_info = function() list(),
      df_combined_AR = function() NULL, fx_fix = function() NULL)
    out <- NULL
    testServer(panoramaServer, args = list(shared = shared), {
      session$flushReact()
      out <<- list(feed = output$feed$html, sol = output$solicitudes$html, cinta = output$cinta$html,
                   hoy = output$hoy$html, mis = mis_sol())
      # A forged id (another user's solicitud) changes nothing.
      todas <- avisos_leer(CID)
      ajena <- todas$id[todas$clase %in% "solicitud" &
                          !vapply(todas$usuarios, function(u) user %in% .pan_hechos(u), logical(1))]
      expect_length(ajena, 1L)
      session$setInputs(hecho = ajena)
      expect_false(user %in% unlist(lapply(avisos_leer(CID)$hechos, .pan_hechos)))
      propia <- setdiff(todas$id[todas$clase %in% "solicitud"], ajena)
      session$setInputs(hecho = propia)
      session$flushReact()
      out$mis_despues <<- mis_sol()
    })
    out
  }
  bajo <- html_de(COMPRAS, "beto")
  expect_false(grepl("Solo para TI", bajo$feed))
  expect_true(grepl("Para todos", bajo$feed))
  expect_true(grepl("Enviar soporte", bajo$sol))
  expect_false(grepl("De otro", paste(bajo$feed, bajo$sol)))
  expect_equal(nrow(bajo$mis), 1); expect_equal(nrow(bajo$mis_despues), 0)
  expect_false(grepl("panorama.", bajo$feed, fixed = TRUE))   # no raw keys on screen
  expect_false(grepl("Banxico", bajo$cinta %||% ""))           # no FIX: not shown
  # Compras' Calendar is fixed to CxP: no Cobros card.
  expect_false(grepl(tr("panorama.hoy.cobros"), bajo$hoy, fixed = TRUE))
  ti <- html_de(nivel_ti(), "ana")
  expect_true(grepl("Solo para TI", ti$feed))
  expect_true(grepl(tr("panorama.hoy.cobros"), ti$hoy, fixed = TRUE))
})

test_that("La Cinta muestra el FIX solo cuando hay dato", {
  shared <- list(
    effective_client_id = function() CID, usuario = function() usr("ana", nivel_ti()),
    current_user_info = function() list(user = "ana", is_staff = FALSE),
    tier_efectivo = function() nivel_ti(), pagar_hoy_db = function() NULL,
    pasivos_provisions_db = function() NULL, sap_snapshot_info = function() list(),
    df_combined_AR = function() NULL,
    fx_fix = function() list(valor = 18.4215, cambio_pct = -0.3, fecha = HOY))
  testServer(panoramaServer, args = list(shared = shared), {
    session$flushReact()
    expect_true(grepl("USD/MXN 18.4215", output$cinta$html, fixed = TRUE))
    expect_true(grepl("▼ 0.30%", output$cinta$html, fixed = TRUE))
  })
  shared$fx_fix <- function() list(valor = NA_real_)
  testServer(panoramaServer, args = list(shared = shared), {
    session$flushReact()
    expect_false(grepl("USD/MXN", output$cinta$html %||% "", fixed = TRUE))
  })
})

test_that("staff dentro de un cliente no escribe en sus preferencias", {
  rm(list = ls(.mem), envir = .mem)
  shared <- list(
    effective_client_id = function() CID, usuario = function() usr("soporte", nivel_ti()),
    current_user_info = function() list(user = "soporte", is_staff = TRUE),
    tier_efectivo = function() nivel_ti(), pagar_hoy_db = function() NULL,
    pasivos_provisions_db = function() NULL, sap_snapshot_info = function() list(),
    df_combined_AR = function() NULL, fx_fix = function() NULL)
  testServer(panoramaServer, args = list(shared = shared), { session$flushReact() })
  expect_null(.mem[[paste0(CID, "/", .PREFERENCIAS_KEY)]])
  shared$current_user_info <- function() list(user = "ana", is_staff = FALSE)
  shared$usuario <- function() usr("ana", nivel_ti())
  testServer(panoramaServer, args = list(shared = shared), { session$flushReact() })
  expect_false(is.null(prefs_leer(CID, "ana")$ultima_visita))
})
