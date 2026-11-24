# =============================================================================
# tests/test_t89_por_pagar.R
# T89: Panorama's "Por pagar hoy" (the ERP's supplier invoices due today, the
# twin of "Por cobrar hoy"), and both cards with the exclusions the Calendar
# and Vencidos apply: confirmed (already paid) and intercompany invoices out
# (panorama_filas_vigentes()). Before T89 "Por cobrar hoy" read
# df_combined_AR raw and added both.
#   - nivel_ve_libro(): who sees which ledger, fail-closed;
#   - the same rows, the same totals: Panorama = Calendario = Vencidos, with a
#     confirmed and an IC invoice in each ledger (Vencidos' real server);
#   - the cards: order, words, who gets which; the user's day (CM27).
#
# Run from project root:  Rscript tests/test_t89_por_pagar.R
# =============================================================================
cat("=== test_t89_por_pagar ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# Vencidos' own stack (its fixture loads the real module and its pieces).
invisible(source("tests/helpers/vencidos_fixture_app.R", local = FALSE))
.i18n_dir <- file.path(getwd(), "www", "i18n")
for (f in c("R/tier_registry.R", "R/permisos.R", "R/agenda_filas.R", "R/ui_components.R", "R/avisos.R",
            "R/avisos_alertas_module.R", "R/panorama_datos.R", "R/panorama_publicar_module.R", "R/panorama_module.R"))
  suppressMessages(source(f))
source("tests/helpers/ic_filtro_piezas.R")
for (e in parse("R/data_pipeline.R", keep.source = FALSE))
  if (is.call(e) && identical(as.character(e[[1]]), "<-") && identical(as.character(e[[2]]), "to_calendar_data"))
    eval(e, globalenv())

cat("-- nivel_ve_libro --\n")
COMPRAS <- Filter(function(t) identical(nivel_calendario(t), "AP"), niveles_cliente())
ok("compras ve CxP y no CxC", length(COMPRAS) > 0 &&
     all(vapply(COMPRAS, function(t) nivel_ve_libro(t, "AP") && !nivel_ve_libro(t, "AR"), logical(1))))
ok("los niveles sin calendario fijo (staff, dev, admin…) ven los dos libros (T69: NULL = los dos)", {
  libres <- Filter(function(t) is.null(nivel_calendario(t)), c(niveles_staff(), niveles_cliente()))
  length(libres) >= 3 && all(vapply(libres, function(t) nivel_ve_libro(t, "AR") && nivel_ve_libro(t, "AP"), logical(1)))
})
ok("fail-closed: un nivel desconocido, vacío, NA o NULL no ve ningún libro",
   !any(vapply(list("no_existe", "", NA_character_, NULL, c("a", "b")), function(t)
     nivel_ve_libro(t, "AR") || nivel_ve_libro(t, "AP"), logical(1))))

cat("\n-- las mismas filas: Panorama = Calendario = Vencidos --\n")
HOY <- hoy   # the fixture's day (Sys.Date()), the one Vencidos counts with
REG <- list(ar_prefix = "C", ap_prefix = "P", companies = list(list(ar = "66", ap = "77")))
f2 <- function(ledger, mon, doc, dias, saldo, codigo, confirmed = FALSE) {
  x <- fila(ledger, "NTS", mon, doc, paste("PARTE", doc), dias, saldo, confirmed = confirmed)
  x[[if (ledger == "AR") "Código de cliente" else "Código de proveedor"]] <- codigo
  x$RFC <- ""
  x
}
datos <- function(ledger) {
  p <- if (ledger == "AR") "C" else "P"
  rbind(f2(ledger, "MXN", "1", 0, 100, paste0(p, "10")),
        f2(ledger, "USD", "2", 0, 7, paste0(p, "11")),
        f2(ledger, "MXN", "3", 0, 50, paste0(p, "12"), confirmed = TRUE),      # paid: out
        f2(ledger, "MXN", "4", 0, 30, paste0(p, if (ledger == "AR") "66" else "77")),  # IC: out
        f2(ledger, "MXN", "5", 1, 500, paste0(p, "13")),                        # yesterday
        f2(ledger, "MXN", "6", 1, 40, paste0(p, "14"), confirmed = TRUE),       # yesterday, paid
        f2(ledger, "MXN", "7", 1, 9, paste0(p, if (ledger == "AR") "66" else "77")))   # yesterday, IC
}
AR <- datos("AR"); AP <- datos("AP")
sh <- list(interco_v2 = function() REG, company_map = function() CMAP,
           ic_mode = list(AR = function() "exclude", AP = function() "exclude"),
           df_combined_AR = function() AR, df_combined_AP = function() AP)
por_moneda <- function(d) if (is.null(d) || !nrow(d)) c() else { x <- tapply(d$total, d$moneda, sum); x[order(names(x))] }
# The Calendar: df_calendar's own steps (apply_ic_filter with the ledger's
# mode, codes and RFCs, then to_calendar_data()), the day's rows by currency.
calendario <- function(df, ledger, dia) {
  d <- apply_ic_filter(df, mode = "exclude", code_col = PANORAMA_CODIGO_COL[[ledger]],
                       ic_codes = build_ic_fullcodes(REG, ledger), ic_rfcs = character(), cmap = CMAP, empresas = NULL)
  cd <- to_calendar_data(d, "Saldo vencido")
  cd <- cd[as.Date(cd$Fecha) == dia, , drop = FALSE]
  x <- tapply(cd$Importe, cd$Moneda, sum); x[order(names(x))]
}
# Vencidos: its real server over the same shared.
ven <- NULL
shv <- c(sh, list(interco_v2 = reactive(REG), tags_db = reactive(tags0), pagar_hoy_db = reactive(agenda0),
                  ic_mode = list(AR = reactive("exclude"), AP = reactive("exclude")),
                  df_combined_AR = reactive(AR), df_combined_AP = reactive(AP),
                  empresa_sel = reactive(c("NTS", "NCS")), company_map = reactive(CMAP),
                  current_user = reactive("luis"), sap_data = reactive(crudos0), amount_col = reactive("Saldo vencido")))
shv <- shv[!duplicated(names(shv), fromLast = TRUE)]
testServer(vencidosServer, args = list(shared = shv, accion_factura = function(...) NULL), {
  session$flushReact(); ven <<- vencidos_df()
})
vencidos <- function(ledger, dia) {
  d <- ven[ven$Ledger == ledger & as.Date(ven$Fecha) == dia & !(ven$confirmed %in% TRUE), , drop = FALSE]
  x <- tapply(d$Importe, d$Moneda, sum); x[order(names(x))]
}
for (L in c("AR", "AP")) {
  pan <- por_moneda(panorama_cobros_hoy(panorama_filas_vigentes(get(L), L, sh), HOY))
  ok(sprintf("%s hoy: Panorama da 100 MXN y 7 USD (sin la confirmada ni la IC)", L),
     identical(as.numeric(pan[c("MXN", "USD")]), c(100, 7)))
  ok(sprintf("%s hoy: Panorama = Calendario = Vencidos", L),
     identical(pan, calendario(get(L), L, HOY)) && identical(pan, vencidos(L, HOY)))
  atr <- por_moneda(panorama_cobros_atrasados(panorama_filas_vigentes(get(L), L, sh), HOY))
  ok(sprintf("%s ya vencidos sin fecha nueva: 500 MXN (sin la confirmada ni la IC), igual que Calendario y Vencidos", L),
     identical(as.numeric(atr), 500) && identical(atr, calendario(get(L), L, HOY - 1)) && identical(atr, vencidos(L, HOY - 1)))
  ok(sprintf("%s contraprueba: el df crudo (lo de antes de la T89) sumaba la confirmada y la IC: 180 MXN", L),
     identical(unname(por_moneda(panorama_cobros_hoy(get(L), HOY))["MXN"]), 180))
}
ok("un registro IC que falla detiene el cálculo (nunca deja pasar la IC)",
   inherits(tryCatch(panorama_filas_vigentes(AP, "AP", modifyList(sh, list(interco_v2 = function() stop("s3")))),
                     error = function(e) e), "error"))

cat("\n-- las tarjetas --\n")
pintar <- function(tier, ar = AR, ap = AP, zona = "UTC") {
  shared <- c(sh, list(
    effective_client_id = function() "cli", usuario = function() list(user = "ana", tier = tier, permisos = list()),
    current_user_info = function() list(user = "ana", name = "Ana", is_staff = FALSE),
    tier_efectivo = function() tier, pagar_hoy_db = function() NULL, pasivos_provisions_db = function() NULL,
    sap_snapshot_info = function() list(), fx_fix = function() NULL, moneda_base_cliente = function() list(moneda = "MXN"),
    df_combined_AR = function() ar, df_combined_AP = function() ap))
  shared <- shared[!duplicated(names(shared), fromLast = TRUE)]
  out <- NULL
  testServer(panoramaServer, args = list(shared = shared, activa = shiny::reactive(FALSE), tz = shiny::reactive(zona)), {
    session$flushReact(); out <<- as.character(output$hoy$html)
  })
  out
}
tiene <- function(x, s) grepl(s, x, fixed = TRUE)
DEV <- niveles_cliente()[1]
h <- pintar(DEV)
ok("'Por pagar hoy' con 100.00 MXN · 7.00 USD, 2 facturas y su explicación",
   tiene(h, "Por pagar hoy") && tiene(h, "100.00 MXN") && tiene(h, "7.00 USD") &&
     tiene(h, "Facturas de proveedores que vencen hoy · 2 facturas"))
ok("…con su línea de ya vencidos y el enlace al Calendario",
   tiene(h, "No incluye 500.00 MXN ya vencidos sin fecha nueva · ver Vencidos") && tiene(h, "Ver en Calendario →"))
ok("el orden: Por cobrar hoy, Por pagar hoy, Programado para pagar hoy, Pasivos próximos", {
  pos <- vapply(c("Por cobrar hoy", "Por pagar hoy", "Programado para pagar hoy", "Pasivos próximos"),
                function(s) regexpr(s, h, fixed = TRUE)[1], numeric(1))
  all(pos > 0) && !is.unsorted(pos)
})
ok("ninguna confirmada ni IC en las tarjetas: no aparece 150.00 MXN ni 180.00 MXN",
   !tiene(h, "180.00 MXN") && !tiene(h, "150.00 MXN"))
hc <- pintar(COMPRAS[1])
ok("compras: ve Por pagar hoy, no Por cobrar hoy", tiene(hc, "Por pagar hoy") && !tiene(hc, "Por cobrar hoy"))
hx <- pintar("no_existe")
ok("un nivel desconocido no ve ninguna de las dos (fail-closed)", !tiene(hx, "Por pagar hoy") && !tiene(hx, "Por cobrar hoy"))

cat("\n-- el día del usuario (CM27) --\n")
# A zone whose date is not UTC's right now (+14 or -11: one of them always is).
ahora <- Sys.time(); dia_utc <- as.Date(format(ahora, tz = "UTC", "%Y-%m-%d"))
ZONA <- Filter(function(z) as.Date(format(ahora, tz = z, "%Y-%m-%d")) != dia_utc, c("Pacific/Kiritimati", "Pacific/Pago_Pago"))[1]
dia_zona <- as.Date(format(ahora, tz = ZONA, "%Y-%m-%d"))
ap_z <- rbind(f2("AP", "MXN", "8", 0, 321, "P20"), f2("AP", "MXN", "9", 0, 654, "P21"))
ap_z$FechaEff <- c(dia_zona, dia_utc)
hz <- pintar(DEV, ap = ap_z, zona = ZONA)
# The UTC-dated one is, for this user, yesterday (zone ahead: it shows as
# "ya vencido") or tomorrow (zone behind: not shown at all); never today.
ok(sprintf("con la zona %s (su día ya no es el de UTC), 'Por pagar hoy' cuenta la factura de SU día, no la del día UTC", ZONA),
   !is.na(ZONA) && tiene(hz, "321.00 MXN") && tiene(hz, "Facturas de proveedores que vencen hoy · 1 factura") &&
     if (dia_zona > dia_utc) tiene(hz, "No incluye 654.00 MXN ya vencidos") else !tiene(hz, "654.00 MXN"))

cat("\n-- el globo de Vencidos (y del ☰) --\n")
H <- as.Date("2026-11-24")
g <- data.frame(Ledger = c("AR", "AR", "AR", "AP", "AP", "AP", "AR"),
                Fecha = H - c(1, 7, 8, 0, 3, 3, 2), confirmed = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, NA),
                stringsAsFactors = FALSE)
ok("cuenta lo vencido de hace 1 a 7 días, sin confirmar: 4 (no hoy, no hace 8, no la confirmada)",
   identical(vencidos_globo(g, H)$n, 4L))
ok("solo los libros que ve el nivel (compras: CxP)", identical(vencidos_globo(g, H, "AP")$n, 1L))
ok("sin libros (nivel desconocido), nada", identical(vencidos_globo(g, H, character())$n, 0L))
ok("más de 99 dice 99+; 0 no muestra nada (texto '0', count 0)", {
  mucho <- data.frame(Ledger = "AR", Fecha = rep(H - 2, 150), confirmed = FALSE)
  identical(vencidos_globo(mucho, H)$texto, "99+") && identical(vencidos_globo(mucho, H)$n, 150L) &&
    identical(vencidos_globo(NULL, H)$n, 0L)
})
ok("el día es el del usuario: con su día un día adelante, lo de ayer UTC es su antier",
   identical(vencidos_globo(g, H + 1)$n, vencidos_globo(g[g$Fecha >= H - 6, ], H + 1)$n))
ok("Vencidos le pasa al globo el día del usuario (hoy_usuario) y los libros del nivel (nivel_ve_libro)", {
  x <- sub("#.*$", "", readLines("R/vencidos_module.R", warn = FALSE, encoding = "UTF-8"))
  any(grepl("vencidos_globo(vencidos_df(), hoy_usuario(session), libros)", x, fixed = TRUE)) &&
    any(grepl("nivel_ve_libro(tier, l)", x, fixed = TRUE))
})

cat(sprintf("\nTOTAL: %d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
