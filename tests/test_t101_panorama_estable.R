# =============================================================================
# tests/test_t101_panorama_estable.R
#
# T101 (2026-11-25): on sign-in, half of Panorama's redraw waves after the
# 100 % came from sources re-run with an IDENTICAL value: the browser's time
# zone arriving late (client_tz, the same zone as the fallback) and
# df_combined_AR/AP re-run with the same rows (T101_paso0.md). .pan_estable()
# (R/panorama_module.R) copies a source into a reactiveVal, which ignores an
# identical() value, so only a real change reaches the cards.
#   A. the sign-in sequence measured: Phase 1, Phase 2 (changes), the same
#      data re-run, the zone arriving late with the same value -- the repeats
#      recompute nothing;
#   B. the 9 drawn outputs after that sequence are the SAME HTML as a session
#      that got the complete data at once (no figure changes);
#   C. a real change still goes through (Phase 2 recomputes; a new day too);
#   D. .pan_estable() itself: an error and a req() travel as before.
# Real panoramaServer under shiny::testServer(), the fixture of
# test_t89_por_pagar (activa = FALSE: no S3). Run from project root.
# =============================================================================
cat("=== test_t101_panorama_estable ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, r) {
  r <- tryCatch(r, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

invisible(source("tests/helpers/vencidos_fixture_app.R", local = FALSE))
.i18n_dir <- file.path(getwd(), "www", "i18n")
for (f in c("R/tier_registry.R", "R/permisos.R", "R/agenda_filas.R", "R/ui_components.R", "R/avisos.R",
            "R/avisos_alertas_module.R", "R/panorama_datos.R", "R/panorama_publicar_module.R", "R/panorama_module.R"))
  suppressMessages(source(f))
source("tests/helpers/ic_filtro_piezas.R")
for (e in parse("R/data_pipeline.R", keep.source = FALSE))
  if (is.call(e) && identical(as.character(e[[1]]), "<-") && identical(as.character(e[[2]]), "to_calendar_data"))
    eval(e, globalenv())

# Counters on what the cards compute with (the module finds them in globalenv).
CUENTA <- new.env()
for (fn in c("panorama_filas_vigentes", "panorama_cobros_hoy", "panorama_cobros_atrasados", "panorama_pagos_hoy",
             "panorama_pasivos_pronto", "panorama_sap_estado")) local({
  orig <- get(fn, globalenv()); nombre <- fn; assign(nombre, 0L, envir = CUENTA)
  assign(fn, function(...) { assign(nombre, get(nombre, CUENTA) + 1L, envir = CUENTA); orig(...) }, envir = globalenv())
})
cuenta <- function() unlist(mget(ls(CUENTA), envir = CUENTA))

# Counter on every run of every output: MockShinySession$defineOutput wraps
# each output's function (restored at the end of the file).
SALIDA_N <- new.env()
.def_orig <- shiny:::MockShinySession$public_methods$defineOutput
.def_t101 <- .def_orig
body(.def_t101) <- as.call(c(as.name("{"),
  quote(if (is.function(func)) { .f0 <- func; .nm <- sub("^.*-", "", name)
    func <- function(...) { assign(.nm, (if (exists(.nm, SALIDA_N, inherits = FALSE)) get(.nm, SALIDA_N) else 0L) + 1L, envir = SALIDA_N)
                            .f0(...) } }),
  as.list(body(.def_orig))[-1]))
shiny:::MockShinySession$set("public", "defineOutput", .def_t101, overwrite = TRUE)
corridas <- function() { x <- unlist(mget(ls(SALIDA_N), envir = SALIDA_N)); if (is.null(x)) integer() else x }

# The fixture's data (as test_t89_por_pagar): today's and yesterday's invoices.
HOY <- hoy
REG <- list(ar_prefix = "C", ap_prefix = "P", companies = list(list(ar = "66", ap = "77")))
f2 <- function(ledger, mon, doc, dias, saldo, codigo) {
  x <- fila(ledger, "NTS", mon, doc, paste("PARTE", doc), dias, saldo)
  x[[if (ledger == "AR") "Código de cliente" else "Código de proveedor"]] <- codigo
  x$RFC <- ""
  x
}
datos <- function(ledger, fase) {
  p <- if (ledger == "AR") "C" else "P"
  d <- rbind(f2(ledger, "MXN", "1", 0, 100, paste0(p, "10")), f2(ledger, "MXN", "5", 1, 500, paste0(p, "13")))
  if (fase == 2) d <- rbind(d, f2(ledger, "USD", "2", 0, 7, paste0(p, "11")), f2(ledger, "MXN", "8", 0, 25, paste0(p, "15")))
  d
}
ZONA_HOY <- Filter(function(z) identical(as.Date(Sys.time(), tz = z), HOY), unique(c("UTC", Sys.timezone())))[1]
if (is.null(ZONA_HOY) || is.na(ZONA_HOY)) stop("no zone has the fixture's day: HOY = ", HOY)
SALIDAS <- c("encabezado", "sap_franja", "clima", "destacados", "hoy", "solicitudes", "feed", "equipo", "cinta")
TIER <- niveles_cliente()[1]

# A session whose sources are driven by the test:
#   fase()  -- 1 or 2, the data (a real change);
#   rep()   -- bumping it re-runs every source with the SAME value;
#   tz_in() -- the browser's zone: NULL until it arrives (fallback ZONA_HOY).
sesion <- function(fase0, tz0, pasos) {
  fase <- shiny::reactiveVal(fase0); rep <- shiny::reactiveVal(0L); tz_in <- shiny::reactiveVal(tz0)
  r <- function(f) shiny::reactive({ rep(); f() })   # re-run with the same value when rep() moves
  shared <- list(interco_v2 = function() REG, company_map = function() CMAP,
    ic_mode = list(AR = function() "exclude", AP = function() "exclude"),
    df_combined_AR = r(function() datos("AR", fase())), df_combined_AP = r(function() datos("AP", fase())),
    # pagar_hoy_db is a reactiveVal in app.R (it never moves with an identical
    # value; the T101 trace never saw it re-run), and output$hoy reads it
    # directly for its breakdown: it stays a plain source here.
    pagar_hoy_db = function() NULL, pasivos_provisions_db = r(function() NULL),
    sap_snapshot_info = r(function() list()),
    effective_client_id = function() "cli", usuario = function() list(user = "ana", tier = TIER, permisos = list()),
    current_user_info = function() list(user = "ana", name = "Ana", is_staff = FALSE),
    tier_efectivo = function() TIER, fx_fix = function() NULL, moneda_base_cliente = function() list(moneda = "MXN"))
  out <- NULL
  shiny::testServer(panoramaServer, args = list(shared = shared, activa = shiny::reactive(FALSE),
                                                tz = shiny::reactive(tz_in() %||% ZONA_HOY)), {
    pasos(session, fase, rep, tz_in)
    session$flushReact()
    out <<- vapply(SALIDAS, function(s) paste(as.character(output[[s]]$html), collapse = ""), "")
  })
  out
}
# a clock in the header ("hh:mm") can tick between two sessions: not a figure
sin_hora <- function(h) gsub("[0-9]{1,2}:[0-9]{2}", "hh:mm", h)

cat("A. the sign-in sequence\n")
pasos_c <- list(); pasos_s <- list(); rm(list = ls(SALIDA_N), envir = SALIDA_N)
final <- sesion(1L, NULL, function(session, fase, rep, tz_in) {
  session$flushReact(); pasos_c$fase1 <<- cuenta(); pasos_s$fase1 <<- corridas()
  fase(2L); session$flushReact(); pasos_c$fase2 <<- cuenta(); pasos_s$fase2 <<- corridas()
  rep(rep() + 1L); session$flushReact(); pasos_c$repetido <<- cuenta(); pasos_s$repetido <<- corridas()
  tz_in(ZONA_HOY); session$flushReact(); pasos_c$zona <<- cuenta(); pasos_s$zona <<- corridas()
})
dif <- function(a, b) b - a
dif_s <- function(a, b) { n <- union(names(a), names(b)); x <- setNames(as.integer(b[n]) - as.integer(ifelse(is.na(a[n]), 0L, a[n])), n); x[x != 0L] }
cat("   corridas de las salidas en la fase 1:", paste(names(pasos_s$fase1), pasos_s$fase1, collapse = " "), "\n")
ok(sprintf("A1 Phase 2 (a real change) recomputes the rows and the cards' figures (%d calls)",
           sum(dif(pasos_c$fase1, pasos_c$fase2))),
   dif(pasos_c$fase1, pasos_c$fase2)[["panorama_filas_vigentes"]] >= 2L &&
     dif(pasos_c$fase1, pasos_c$fase2)[["panorama_cobros_hoy"]] >= 1L)
ok(sprintf("A1b ...and redraws the cards that show them (%s)", paste(names(dif_s(pasos_s$fase1, pasos_s$fase2)), collapse = " ")),
   "hoy" %in% names(dif_s(pasos_s$fase1, pasos_s$fase2)))
ok(sprintf("A2 df_combined, the Agenda, Pasivos and SAP re-run with the same value: no output redraws (redrawn: %s)",
           paste(names(dif_s(pasos_s$fase2, pasos_s$repetido)), collapse = " ")),
   length(dif_s(pasos_s$fase2, pasos_s$repetido)) == 0L &&
     all(dif(pasos_c$fase2, pasos_c$repetido)[c("panorama_cobros_hoy", "panorama_cobros_atrasados")] == 0L))
ok(sprintf("A3 the browser's zone arrives late with the same zone: nothing recomputes, no output redraws (redrawn: %s)",
           paste(names(dif_s(pasos_s$repetido, pasos_s$zona)), collapse = " ")),
   sum(dif(pasos_c$repetido, pasos_c$zona)) == 0L && length(dif_s(pasos_s$repetido, pasos_s$zona)) == 0L)

cat("\nB. the same drawing\n")
de_una <- sesion(2L, ZONA_HOY, function(session, fase, rep, tz_in) session$flushReact())
# T101_GUARDAR=<file>: keep the drawn HTML, to compare this code against
# another (master) by hand -- the same sequence, the same HTML.
if (nzchar(Sys.getenv("T101_GUARDAR"))) saveRDS(list(final = sin_hora(final), de_una = sin_hora(de_una)), Sys.getenv("T101_GUARDAR"))
VACIAS <- SALIDAS[!nzchar(de_una)]
ok(sprintf("B1 the cards with data drew something (empty without S3 avisos, as before: %s)", paste(VACIAS, collapse = " ")),
   all(nzchar(final[c("encabezado", "clima", "hoy", "cinta")])) && identical(SALIDAS[!nzchar(final)], VACIAS))
ok(sprintf("B2 the 9 outputs after the sequence are the same HTML as a session that got the complete data at once (%s)",
           paste(SALIDAS[sin_hora(final) != sin_hora(de_una)], collapse = ", ")),
   identical(sin_hora(final), sin_hora(de_una)))
ok("B3 and the figures are the complete data's (Phase 2's 25.00 MXN and 7.00 USD are in Por cobrar hoy)",
   grepl("125.00 MXN", final[["hoy"]], fixed = TRUE) && grepl("7.00 USD", final[["hoy"]], fixed = TRUE))

cat("\nC. a real change still goes through\n")
c_pasos <- list()
invisible(sesion(2L, ZONA_HOY, function(session, fase, rep, tz_in) {
  session$flushReact(); c_pasos$a <<- corridas()
  tz_in(if (identical(ZONA_HOY, "Pacific/Kiritimati")) "UTC" else "Pacific/Kiritimati")   # another zone
  session$flushReact(); c_pasos$b <<- corridas()
}))
ok(sprintf("C1 a different zone does go through: the header redraws (redrawn: %s)", paste(names(dif_s(c_pasos$a, c_pasos$b)), collapse = " ")),
   "encabezado" %in% names(dif_s(c_pasos$a, c_pasos$b)))

cat("\nD. .pan_estable() itself\n")
d <- list()
shiny::testServer(function(input, output, session) {
  src <- shiny::reactiveVal("a")
  e <- .pan_estable(shiny::reactive({ x <- src(); if (identical(x, "boom")) stop("roto"); shiny::req(!identical(x, "nada")); x }))
  corridas <- 0L
  shiny::observe({ tryCatch(e(), error = function(err) NULL); corridas <<- corridas + 1L })
  session$flushReact(); d$c0 <<- corridas; d$v0 <<- shiny::isolate(e())
  src("a"); session$flushReact(); d$c1 <<- corridas
  src("boom"); session$flushReact()
  d$err <<- tryCatch({ shiny::isolate(e()); "sin error" }, error = function(err) conditionMessage(err))
  src("nada"); session$flushReact()
  d$req <<- tryCatch({ shiny::isolate(e()); "sin error" }, error = function(err) class(err)[1])
  src("b"); session$flushReact(); d$v2 <<- shiny::isolate(e())
  # the same name on both sides (as `pagos <- .pan_estable(pagos)`): it must
  # wrap the old reactive, not read itself (R's lazy arguments)
  mismo <- shiny::reactive(src()); mismo <- .pan_estable(mismo)
  session$flushReact(); d$mismo <<- tryCatch(shiny::isolate(mismo()), error = function(err) paste("error:", conditionMessage(err)))
}, {})
ok("D1 it gives the value, and an identical value does not wake its readers", identical(d$v0, "a") && d$c1 == d$c0)
ok("D2 an error in the source reaches the reader as an error", identical(d$err, "roto"))
ok("D3 a req() in the source stays a silent stop", identical(d$req, "shiny.silent.error"))
ok("D4 after an error, a new value goes through", identical(d$v2, "b"))
ok(sprintf("D5 `x <- .pan_estable(x)` wraps the old x (force: R's lazy argument) -- got %s", format(d$mismo)),
   identical(d$mismo, "b"))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
shiny:::MockShinySession$set("public", "defineOutput", .def_orig, overwrite = TRUE)   # restored
if (.fail > 0L) quit(status = 1L)
