# =============================================================================
# tests/test_fx_aprox_r.R
# banxico-fix 3/3 (Luis): a provision quoted in USD and paid in MXN shows its
# amount converted with TODAY's FIX, marked "≈", wherever the converted
# amount is displayed -- and nothing approximate is ever saved.
#   1. fx_monto_aprox(): which provisions are approximated (USD -> MXN, no
#      hand-set amount, a FIX); none without a FIX;
#   2. THE KEY (coordinator): the provision bridge never changes Importe or
#      Saldo vencido -- Importe is part of the Agenda's key. Sent to the
#      Agenda one day and removed another day with a different FIX: same
#      key, recognised (no duplicate), removed; the stored Importe identical.
#      Contraprueba: had the approximation gone into Importe, the second
#      day's removal would miss the row;
#   3. Pasivos' table: the cell shows the approximate amount with its "≈"
#      help (.aprox); without a FIX, as before;
#   4. Flujo (preview and Exportar): the provision counts with the
#      approximate amount and its row says "≈";
#   5. the detail and the engine paint it; the help comes from the dictionary.
#
# Run from project root:  Rscript tests/test_fx_aprox_r.R
# =============================================================================
cat("=== test_fx_aprox_r ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
suppressMessages({ source("R/i18n.R"); source("R/fx_banxico.R"); source("R/pasivos_schemas.R") })
tomar <- function(f, nombres) for (e in parse(f, keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% nombres) eval(e, globalenv())
tomar("R/pasivos_engine.R", c("pasivos_pago_amount", "pasivos_pago_currency"))
tomar("R/persistence.R", "unstage_pagar_hoy")
suppressMessages({ source("R/pasivos_calendar_glue.R"); source("R/pasivos_table_pivot.R") })
tomar("R/cashflow_export_module.R", c(".aggregate_to_wide", ".bucket_labels"))
has_capability <- function(...) TRUE
hd_idioma <- function() "es"

FIX1 <- list(valor = 18.00, fecha = as.Date("2026-11-12"))   # day 1
FIX2 <- list(valor = 19.50, fecha = as.Date("2026-11-13"))   # day 2

prov <- function(id, cot = "USD", pago = "MXN", amount_cot = 1000, amount_pago = NA_real_, override = NA_real_,
                 fecha = Sys.Date() + 10L) {
  p <- .schema_pasivos_provision()[NA_integer_, ]
  p$id <- id; p$liability_id <- NA_character_; p$origin <- "manual"; p$estado <- "provisional"
  p$empresa <- "NTS"; p$parte <- paste("Parte", id); p$documento <- paste0("DOC-", id)
  p$fecha_efectiva <- fecha; p$fecha_calculada <- fecha
  p$moneda_pago <- pago; p$cotizado_en <- cot
  p$amount_cotizado <- amount_cot; p$amount_pago <- amount_pago; p$amount_pago_override <- override
  p
}

# ── 1. Which provisions are approximated ────────────────────────────────────
a <- fx_monto_aprox(c(1000, 1000, 1000, 1000, NA), c("USD", "USD", "MXN", "USD", "USD"),
                    c("MXN", "MXN", "MXN", "USD", "MXN"), c(NA, 5000, NA, NA, NA), FIX1)
ok("USD -> MXN sin monto a mano: aproximado con el FIX (1000 × 18 = 18,000)",
   identical(a$aprox, c(TRUE, FALSE, FALSE, FALSE, FALSE)) && identical(a$importe_aprox[1], 18000))
ok("monto puesto a mano, misma moneda, otro par o sin monto cotizado: no se aproxima",
   all(is.na(a$importe_aprox[2:5])))
ok("sin FIX: nada se aproxima (se ve como hoy)",
   !any(fx_monto_aprox(1000, "USD", "MXN", NA, NULL)$aprox) && !any(fx_monto_aprox(1000, "USD", "MXN", NA, list(valor = NA))$aprox))

# ── 2. The key: Importe never changes; the Agenda across two FIX ───────────
provs <- dplyr::bind_rows(prov("p1"), prov("p2", amount_pago = 17000))   # p2: an amount stored at generation
d1 <- pasivos_provisions_as_ledger_rows(provs, NULL, fx = FIX1)
d2 <- pasivos_provisions_as_ledger_rows(provs, NULL, fx = FIX2)
d0 <- pasivos_provisions_as_ledger_rows(provs, NULL, fx = NULL)
ok("el puente no cambia Importe ni Saldo vencido: los mismos con FIX1, FIX2 y sin FIX",
   identical(d1$Importe, d0$Importe) && identical(d2$Importe, d0$Importe) &&
     identical(d1$`Saldo vencido`, d0$`Saldo vencido`) && identical(d0$Importe, c(NA_real_, 17000)))
ok("solo para mostrar: importe_aprox y aprox, con el FIX de cada día",
   identical(d1$importe_aprox, c(18000, 18000)) && identical(d2$importe_aprox, c(19500, 19500)) &&
     all(d1$aprox) && !any(d0$aprox))
llave <- c("ledger", "Empresa", "Moneda", "Documento", "Importe")
k1 <- transform(d1, ledger = "AP")[, llave]; k2 <- transform(d2, ledger = "AP")[, llave]
ok("la llave de la Agenda es idéntica el día 1 y el día 2", identical(k1, k2))
agenda <- transform(k1, id = c("a1", "a2"), stringsAsFactors = FALSE)                   # sent on day 1
ok("el día 2 la Agenda las reconoce como ya enviadas (no se duplican)",
   nrow(dplyr::semi_join(k2, agenda, by = llave)) == 2L && nrow(dplyr::anti_join(k2, agenda, by = llave)) == 0L)
quitada <- unstage_pagar_hoy(agenda, k2[1, , drop = FALSE], keys = llave)
ok("el día 2, quitar de la Agenda encuentra la fila y la quita (sin fallar)",
   nrow(quitada) == 1L && identical(quitada$id, "a2"))
ok("el Importe guardado en la Agenda es el de la provisión, igual antes y después", identical(agenda$Importe, d0$Importe))
mal1 <- transform(k1, Importe = d1$importe_aprox); mal2 <- transform(k2, Importe = d2$importe_aprox)
ok("contraprueba: si el aproximado entrara en Importe, el día 2 no encontraría la fila al quitar",
   nrow(unstage_pagar_hoy(transform(mal1, id = c("a1", "a2")), mal2[1, , drop = FALSE], keys = llave)) == 2L)

# ── 3. Pasivos' table ───────────────────────────────────────────────────────
filtros <- list(empresa = character(0), categorias = c("regular", "financiero", "tarjeta"), subcategorias = character(0),
                currency = "All", search = "", granularity = "day", provision_type = "todas")
celda <- function(fx) {
  long <- pasivos_table_build_long(prov("p1"), data.frame(), data.frame(), .schema_pasivos_liability(), filtros, fx = fx)
  long[long$cell_provision_id %in% "p1", , drop = FALSE]
}
c1 <- celda(FIX1); c0 <- celda(NULL)
ok("tabla: con FIX la celda muestra el aproximado y lo marca", isTRUE(c1$cell_amount[1] == 18000) && isTRUE(c1$cell_aprox[1]))
ok("tabla: sin FIX, como hoy (sin marca; el monto NA suma 0, como ya hacía la tabla)",
   identical(c0$cell_amount[1], sum(NA_real_, na.rm = TRUE)) && identical(c0$cell_aprox[1], FALSE))
wide <- pasivos_table_pivot_wide(pasivos_table_build_long(prov("p1"), data.frame(), data.frame(),
                                                          .schema_pasivos_liability(), filtros, fx = FIX1), "day",
                                 Sys.Date(), Sys.Date() + 30L)
td <- pasivos_tabla_datos(wide, NULL, fx = FIX1)
ca <- grep("\\.aprox$", names(td), value = TRUE)          # one per date column
vals <- unlist(lapply(ca, function(k) td[[k]]))
ok("tabla: solo la celda de la provisión lleva la ayuda del \"≈\" (<campo>.aprox)",
   length(ca) > 1L && sum(nzchar(vals)) == 1L && identical(vals[nzchar(vals)], fx_aprox_ayuda(FIX1)) &&
     grepl("18.0000", vals[nzchar(vals)], fixed = TRUE))
td0 <- pasivos_tabla_datos(wide, NULL, fx = NULL)
ok("tabla: sin FIX, .aprox vacío en todas", all(unlist(lapply(ca, function(k) td0[[k]])) == ""))

# ── 4. Flujo: preview and Exportar ──────────────────────────────────────────
df <- d1
df[["Saldo vencido"]][df$aprox] <- df$importe_aprox[df$aprox]   # what build_export_combined_df does
df$date_bucket <- as.Date("2026-11-16"); df$tag_urgent <- FALSE; df$tag_important <- FALSE
w <- .aggregate_to_wide(df, as.Date("2026-11-16"), "week", idioma = "es")
ok("Flujo: la provisión cuenta con el aproximado y su fila dice ≈",
   all(startsWith(w$Parte, "≈ ")) && isTRUE(all.equal(sum(w$Total), 36000)))
d0f <- d0; d0f$date_bucket <- as.Date("2026-11-16"); d0f$tag_urgent <- FALSE; d0f$tag_important <- FALSE
w0 <- .aggregate_to_wide(d0f, as.Date("2026-11-16"), "week", idioma = "es")
ok("Flujo sin FIX: sin ≈ y con el monto guardado, como hoy", !any(startsWith(w0$Parte, "≈")) && isTRUE(sum(w0$Total) == 17000))
exp_src <- paste(readLines("R/cashflow_export_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("Flujo: build_export_combined_df cambia Saldo vencido solo en su copia y nunca Importe",
   grepl('out[["Saldo vencido"]][ax] <- out$importe_aprox[ax]', exp_src, fixed = TRUE) &&
     !grepl("out$Importe[ax]", exp_src, fixed = TRUE))

# ── 5. The detail, the engine and the dictionary ────────────────────────────
mod <- paste(readLines("R/pasivos_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("detalle: muestra el aproximado con ≈ y su ayuda",
   grepl("fx_monto_aprox(prov$amount_cotizado[1]", mod, fixed = TRUE) && grepl("title = fx_aprox_ayuda(fx_hoy)", mod, fixed = TRUE))
ok("detalle: convertir la provisión sigue con el monto guardado (es un guardado)",
   grepl("pago_amount <- tryCatch(pasivos_pago_amount(p),", mod, fixed = TRUE))
col <- paste(readLines("R/hd_columnas.R", warn = FALSE), collapse = "\n"); js <- paste(readLines("www/hdtable.js", warn = FALSE), collapse = "\n")
ok("motor: <campo>.aprox viaja a la celda y el JS pinta ≈ con la ayuda como texto",
   grepl("v$aprox <- I(ax)", col, fixed = TRUE) && grepl("hdt-celda-aprox", js, fixed = TRUE) && grepl("a.title = String(ax)", js, fixed = TRUE))
ok("la ayuda viene del diccionario (es y en) y lleva fecha y valor",
   !is.null(i18n_dict("es")[["tablas.fx.aprox_ayuda"]]) && !is.null(i18n_dict("en")[["tablas.fx.aprox_ayuda"]]) &&
     grepl("12/11/2026", fx_aprox_ayuda(FIX1), fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
