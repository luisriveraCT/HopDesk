# =============================================================================
# tests/test_fix_aprox_pantallas_r.R
# fix-aprox-pantallas: a provision quoted in USD and paid in MXN shows its
# amount converted at today's FIX, marked "≈", in Vencidos, the Lupa, Día and
# the Calendar -- only for display. Importe (the Agenda's key) never changes.
#   1. fx_columna_ver(): the amount to show and its help; nothing without a FIX;
#   2. Vencidos / Lupa (facturas_datos) and Día (dia_datos): ImporteVer and
#      its .aprox / .aprox_total; Importe, the ids and Día's keys identical
#      with FIX1, FIX2 and no FIX;
#   3. the Calendar (to_calendar_data): the party counts the approximate
#      amount instead of 0 and says so (aprox); without a FIX, as today;
#   4. THE GUARD (coordinator): no action, key or Agenda path reads the
#      display column -- ImporteVer appears only where the tables are built;
#   5. the engine paints "≈" on any money column and on its totals.
#
# Run from project root:  Rscript tests/test_fix_aprox_pantallas_r.R
# =============================================================================
cat("=== test_fix_aprox_pantallas_r ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble); library(tidyr) })
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/monedas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/vencidos_module.R", "R/dia_module.R", "R/fx_banxico.R", "R/pasivos_schemas.R")) suppressMessages(source(f))
SAP_B1_ERP_TYPE <- "sap_b1_service_layer"
tomar <- function(f, nombres) for (e in parse(f, keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres) eval(e, globalenv())
tomar("R/global.R", "is_erp_sourced")
tomar("R/pasivos_engine.R", c("pasivos_pago_amount", "pasivos_pago_currency"))
tomar("R/data_pipeline.R", "to_calendar_data")
suppressMessages(source("R/pasivos_calendar_glue.R"))
has_capability <- function(...) TRUE

FIX1 <- list(valor = 18.00, fecha = as.Date("2026-11-12"))
FIX2 <- list(valor = 19.50, fecha = as.Date("2026-11-13"))
DIA  <- as.Date("2026-11-20")

# ── 1. fx_columna_ver ───────────────────────────────────────────────────────
v <- fx_columna_ver(c(NA, 500), c(TRUE, FALSE), c(18000, NA), FIX1)
ok("fx_columna_ver: el aproximado donde aprox, el real donde no",
   identical(v$ver, c(18000, 500)) && nzchar(v$aprox[1]) && identical(v$aprox[2], "") && grepl("12/11/2026", v$aprox_total))
v0 <- fx_columna_ver(c(NA, 500), c(TRUE, FALSE), c(18000, NA), NULL)
ok("fx_columna_ver sin FIX: el real, sin marca", identical(v0$ver, c(NA, 500)) && all(v0$aprox == "") && identical(v0$aprox_total, ""))

# The ledger rows of one day: an SAP invoice and a USD provision with no
# stored amount, through the real provision bridge with each FIX.
prov <- .schema_pasivos_provision()[NA_integer_, ]
prov$id <- "P1"; prov$liability_id <- NA_character_; prov$origin <- "manual"; prov$estado <- "provisional"
prov$empresa <- "NTS"; prov$parte <- "RENTA USD"; prov$documento <- NA_character_
prov$fecha_efectiva <- DIA; prov$fecha_calculada <- DIA
prov$moneda_pago <- "MXN"; prov$cotizado_en <- "USD"; prov$amount_cotizado <- 1000; prov$amount_pago <- NA_real_
filas <- function(fx) {
  p <- pasivos_provisions_as_ledger_rows(prov, NULL, fx = fx)
  sap <- tibble::tibble(Empresa = "NTS", Parte = "ACEROS", Moneda = "MXN", Documento = "3001", Factura = "F-1",
                        Importe = 22000, `Saldo vencido` = 22000, FechaEff = DIA, `Fecha de vencimiento` = DIA,
                        FechaVenc_Original = DIA, source = SAP_B1_ERP_TYPE, confirmed = FALSE,
                        has_sap_override = FALSE, Movida = "Falso", Tipo = "AP")
  as.data.frame(dplyr::bind_rows(sap, p))
}
f1 <- filas(FIX1); f2 <- filas(FIX2); f0 <- filas(NULL)
ok("las filas del puente: el Importe de la provisión es NA con FIX1, FIX2 y sin FIX (nunca cambia)",
   identical(f1$Importe, f0$Importe) && identical(f2$Importe, f0$Importe) && is.na(f0$Importe[2]))

# ── 2. Vencidos / Lupa and Día ──────────────────────────────────────────────
fact <- function(f) { f$Ledger <- "AP"; f$Fecha <- as.Date(f$FechaEff); f$FechaOriginal <- DIA; f$Referencia <- f$Factura; f }
con_fix <- function(fx, expr) { orig <- .fx_para_mostrar; on.exit(assign(".fx_para_mostrar", orig, envir = globalenv()))
  assign(".fx_para_mostrar", function() fx, envir = globalenv()); force(expr) }
v1 <- con_fix(FIX1, facturas_datos(fact(f1), NULL)); v2 <- con_fix(FIX2, facturas_datos(fact(f2), NULL))
v0 <- con_fix(NULL, facturas_datos(fact(f0), NULL))
pv <- function(d) d[d$.provision %in% TRUE | grepl("^PROV_", d$Documento) | d$Parte == "RENTA USD", , drop = FALSE]
ok("Vencidos/Lupa: la provisión se ve con el aproximado (1000 × 18) y su ayuda; la factura SAP igual que siempre",
   identical(pv(v1)$ImporteVer, 18000) && nzchar(pv(v1)[["ImporteVer.aprox"]]) &&
     identical(v1$ImporteVer[v1$Parte == "ACEROS"], 22000) && identical(v1[["ImporteVer.aprox"]][v1$Parte == "ACEROS"], "") &&
     grepl("12/11/2026", v1[["ImporteVer.aprox_total"]][1]))
ok("Vencidos/Lupa: con otro FIX, otro aproximado; sin FIX, sin marca y el real",
   identical(pv(v2)$ImporteVer, 19500) && all(v0[["ImporteVer.aprox"]] == "") && identical(v0$ImporteVer, v0$Importe))
ok("Vencidos/Lupa: Importe y los ids iguales con FIX1, FIX2 y sin FIX",
   identical(v1$Importe, v0$Importe) && identical(v2$Importe, v0$Importe) && identical(sort(v1$id), sort(v0$id)) &&
     identical(facturas_ids(fact(f1)), facturas_ids(fact(f0))))
ok("Vencidos/Lupa: la columna de dinero de la tabla es la de mostrar",
   any(vapply(facturas_columnas(), function(c) identical(c$tipo, "dinero") && identical(c$campo, "ImporteVer"), TRUE)))
d1 <- con_fix(FIX1, dia_datos(f1, "AP", NULL)); d0 <- con_fix(NULL, dia_datos(f0, "AP", NULL))
ok("Día: la provisión se ve con el aproximado y su ayuda; sin FIX como hoy",
   identical(d1$ImporteVer[d1$Parte == "RENTA USD"], 18000) && nzchar(d1[["ImporteVer.aprox"]][d1$Parte == "RENTA USD"]) &&
     all(d0[["ImporteVer.aprox"]] == ""))
ok("Día: dia_ids y dia_llaves idénticos con FIX1, FIX2 y sin FIX (las llaves usan Importe)",
   identical(dia_ids(f1), dia_ids(f0)) && identical(dia_ids(f2), dia_ids(f0)) &&
     identical(dia_llaves(f1, dia_ids(f1)), dia_llaves(f0, dia_ids(f0))))
ok("Día: Detalle y Resumen muestran la columna de mostrar",
   all(vapply(c("detalle", "resumen"), function(vs) any(vapply(dia_columnas(vs), function(c)
     identical(c$tipo, "dinero") && identical(c$campo, "ImporteVer"), TRUE)), TRUE)))

# ── 3. The Calendar ─────────────────────────────────────────────────────────
c1 <- con_fix(FIX1, suppressMessages(to_calendar_data(f1, "Saldo vencido")))
c0 <- con_fix(NULL, suppressMessages(to_calendar_data(f0, "Saldo vencido")))
ok("Calendario: la Parte de la provisión suma el aproximado y dice aprox",
   identical(c1$Importe[c1$Parte == "RENTA USD"], 18000) && isTRUE(c1$aprox[c1$Parte == "RENTA USD"]) &&
     identical(c1$aprox[c1$Parte == "ACEROS"], FALSE))
ok("Calendario sin FIX: como hoy (la provisión sin monto cuenta 0, sin marca)",
   identical(c0$Importe[c0$Parte == "RENTA USD"], 0) && !any(c0$aprox))
ok("Calendario: el frame de origen no cambia (Importe del puente sigue NA)", is.na(f1$Importe[f1$Parte == "RENTA USD"]))

# ── 3b. The Calendar's bubble (calendar_html, R/global.R, integrator's permission) ──
suppressPackageStartupMessages({ library(lubridate); library(stringr) })
# Every function global.R defines (calendar_html needs its helpers), only the definitions.
es_funcion <- function(e) is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
  is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))
for (e in parse("R/global.R", keep.source = FALSE, encoding = "UTF-8")) if (es_funcion(e)) eval(e, globalenv())
maestro <- new.env()
src_m <- tryCatch(system2("git", c("show", "74f10ee:R/global.R"), stdout = TRUE, stderr = FALSE), error = function(e) NULL)
if (length(src_m)) for (e in parse(text = enc2utf8(paste(src_m, collapse = "\n")), keep.source = FALSE))
  if (es_funcion(e)) eval(e, maestro)
burbuja <- function(fn, dd) suppressMessages(fn(dd, as.Date("2026-11-01"), "MXN", title_prefix = "Pagos", ledger = "AP"))
dd <- data.frame(Fecha = DIA, Moneda = "MXN", Parte = c("ACEROS", "RENTA USD"), Importe = c(22000, 18000), stringsAsFactors = FALSE)
ok("burbuja sin la columna aprox: el HTML completo es idéntico al de master (74f10ee)",
   is.function(maestro$calendar_html) && identical(burbuja(calendar_html, dd), burbuja(maestro$calendar_html, dd)))
dda <- dd; dda$aprox <- c(FALSE, FALSE)
ok("burbuja con aprox todo FALSE: idéntico a master", identical(burbuja(calendar_html, dda), burbuja(maestro$calendar_html, dd)))
dda$aprox <- c(FALSE, TRUE)
h1 <- con_fix(FIX1, burbuja(calendar_html, dda))
ok("burbuja con aprox: ≈ en la Parte y en el total, con la ayuda (12/11/2026) como title escapado",
   lengths(regmatches(h1, gregexpr("≈ \\$ ", h1))) == 2L && grepl('title="Incluye montos aproximados (FIX del 12/11/2026).">≈ $ 18,000', h1, fixed = TRUE) &&
     grepl('<span class="cal-amount">$ 22,000', h1, fixed = TRUE))
orig <- .fx_para_mostrar; assign(".fx_para_mostrar", function() stop("caído"), envir = globalenv())
h2 <- burbuja(calendar_html, dda)
assign(".fx_para_mostrar", orig, envir = globalenv())
ok("si la ayuda falla (.fx_para_mostrar con error): sale ≈ sin title",
   lengths(regmatches(h2, gregexpr("≈ \\$ ", h2))) == 2L && !grepl("Incluye montos aproximados", h2, fixed = TRUE) &&
     grepl('<span class="cal-amount">≈ $ 18,000', h2, fixed = TRUE))
gl <- paste(readLines("R/global.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("en el código, el ≈ de calendar_html va como \\u2248 (no el carácter literal)",
   grepl("'>\\u2248 '", gl, fixed = TRUE) && !grepl("'>≈ '", gl, fixed = TRUE))

# ── 4. The guard: no action reads the display column ────────────────────────
PERMITIDAS <- c("facturas_datos", "facturas_columnas", "dia_datos", "dia_columnas")
leen <- character(0)
for (f in c(list.files("R", "\\.R$", full.names = TRUE, recursive = TRUE), "app.R")) {
  ex <- tryCatch(parse(f, keep.source = FALSE, encoding = "UTF-8"), error = function(e) NULL)
  ver <- function(x, dentro) {
    if (is.call(x)) {
      if (identical(x[[1]], as.name("<-")) && is.name(x[[2]]) && is.call(x[[3]]) && identical(x[[3]][[1]], as.name("function")))
        dentro <- as.character(x[[2]])
      for (k in seq_along(x)) if (!is.null(x[[k]]) && !missing(x)) tryCatch(ver(x[[k]], dentro), error = function(e) NULL)
    } else if (is.character(x) && any(grepl("^ImporteVer", x)) && !dentro %in% PERMITIDAS) {
      leen <<- c(leen, paste0(f, ": ", dentro))
    }
  }
  for (e in ex) ver(e, "(arriba)")
}
ok("ninguna acción, llave ni camino a la Agenda lee la columna de mostrar (ImporteVer)",
   { if (length(leen)) message("  la leen: ", paste(unique(leen), collapse = " | ")); !length(leen) })
cuerpo <- function(fn) paste(deparse(get(fn)), collapse = "\n")
ok("dia_ids, dia_llaves, facturas_ids y facturas_payload usan Importe y no la columna de mostrar",
   all(vapply(c("dia_ids", "dia_llaves", "facturas_ids", "facturas_payload"), function(fn) !grepl("ImporteVer", cuerpo(fn)), TRUE)) &&
     grepl("Importe", cuerpo("dia_ids")) && grepl("Importe", cuerpo("facturas_payload")))

# ── 5. The engine ───────────────────────────────────────────────────────────
col <- paste(readLines("R/hd_columnas.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
js  <- paste(readLines("www/hdtable.js", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("motor: .aprox en cualquier col_dinero (no solo accion_celda) y su ayuda de total",
   grepl('identical(col$tipo, "dinero") && !isTRUE(col$accion)', col, fixed = TRUE) && grepl("v$aprox_total", col, fixed = TRUE))
ok("motor: la celda, el pie (sumador), el total de grupo y la banda pintan ≈ con la ayuda como texto",
   grepl("c.tipo === 'dinero' && !c.accion && st.celdas[j].aprox", js, fixed = TRUE) &&
     grepl("hdt-pie-aprox", js, fixed = TRUE) && grepl("marcarAprox(st, td, libres)", js, fixed = TRUE) &&
     grepl("marcarAprox(st, bs, filas)", js, fixed = TRUE) && grepl("el.title = c.aproxTotal", js, fixed = TRUE))
ok("la ayuda de los totales viene del diccionario (es y en)",
   !is.null(i18n_dict("es")[["tablas.fx.aprox_total"]]) && !is.null(i18n_dict("en")[["tablas.fx.aprox_total"]]))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
