# =============================================================================
# tests/test_t60_agenda_panorama.R
# T60: every figure Panorama shows from the Agenda equals its twin in the
# Agenda. "Programado para pagar hoy" (the Hoy card), the Clima's "N pagos" and
# the Cinta's "Hoy: pagos …" all read panorama_pagos_hoy(); its twin is the
# Agenda's "↓ Pagos" (pending AP rows, Importe summed by currency, partial
# payments included). Before T60 Panorama kept FechaVenc <= today: an item due
# after today (Luis's REQ-2026-0001, due 21 Nov) was in the Agenda and not in
# Panorama.
#
# Origin (ERP, manual, Pasivos, Requisiciones) x due date (overdue, today,
# ahead), two currencies, a partial payment, confirmed and cancelled rows.
# Run from project root:  Rscript tests/test_t60_agenda_panorama.R
# =============================================================================
cat("=== test_t60_agenda_panorama ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/monedas.R"); source("R/i18n.R")
cargar <- function(archivo, nombres) if (file.exists(archivo)) for (e in parse(archivo, keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres) eval(e, globalenv())
cargar("R/agenda_filas.R", "agenda_pendientes")
cargar("R/panorama_datos.R", c(".pan_num", ".pan_por_moneda", "panorama_pagos_hoy", "panorama_pagos_desglose"))
leer <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

HOY <- as.Date("2026-11-19")
fila <- function(id, source, venc, importe, moneda = "MXN", tipo = "factura", status = "pending", ledger = "AP",
                 provision_id = NA_character_)
  data.frame(id = id, ledger = ledger, Empresa = "NCS", Moneda = moneda, Documento = id, Parte = "AMAZON",
             Codigo = "", tipo_item = tipo, Importe = importe, FechaVenc = as.Date(venc), status = status,
             provision_id = provision_id, liability_id = NA_character_, source = source, stringsAsFactors = FALSE)
origenes <- list(ERP = list(source = "sap_b1_service_layer", prov = NA_character_),
                 manual = list(source = "manual", prov = NA_character_),
                 Pasivos = list(source = "provision", prov = "prov-pasivo"),
                 Requisiciones = list(source = "provision", prov = "prov-req-2026-0001"))
vencs <- c(vencido = "2026-11-10", hoy = "2026-11-19", anticipado = "2026-11-21")
filas <- list(); k <- 0L
for (o in names(origenes)) for (v in names(vencs)) {
  k <- k + 1L
  filas[[k]] <- fila(paste(o, v, sep = "-"), origenes[[o]]$source, vencs[[v]], 100 * k, provision_id = origenes[[o]]$prov)
}
ph <- do.call(rbind, c(filas, list(
  fila("usd-anticipado", "sap_b1_service_layer", "2026-11-30", 50, moneda = "USD"),
  fila("abono-parcial",  "manual", "2026-11-25", 30, tipo = "abono"),
  fila("confirmada",     "manual", "2026-11-19", 7000, status = "confirmed"),
  fila("cancelada",      "manual", "2026-11-19", 9000, status = "cancelled"),
  fila("cobro-AR",       "sap_b1_service_layer", "2026-11-19", 8000, ledger = "AR"))))

# The Agenda's twin: "↓ Pagos" of its Posición neta (pagar_hoy_module.R,
# output$global_total_ui): its staged() rows, ledger AP, Importe summed by currency.
agenda_pagos <- function(ph) {
  s <- ph[ph$status %in% "pending" & ph$ledger %in% "AP", , drop = FALSE]
  t <- tapply(s$Importe, s$Moneda, sum); list(total = t[order(names(t))], n = nrow(s))
}
ag <- agenda_pagos(ph)
p <- panorama_pagos_hoy(ph, hoy = HOY, base = "MXN")
pt <- setNames(p$total, p$moneda)[sort(p$moneda)]

ok("Panorama 'Programado para pagar hoy' = the Agenda's '↓ Pagos', currency by currency",
   identical(names(pt), names(ag$total)) && isTRUE(all.equal(unname(pt), unname(as.numeric(ag$total)))))
ok("…and the same number of rows (the Clima's 'N pagos')", sum(p$n) == ag$n)
for (o in names(origenes)) for (v in names(vencs)) {
  id <- paste(o, v, sep = "-")
  uno <- ph[ph$id == id, , drop = FALSE]
  ok(sprintf("%s, %s: in the Agenda, and in Panorama with its amount", o, v),
     identical(panorama_pagos_hoy(uno, hoy = HOY, base = "MXN")$total, uno$Importe))
}
ok("Luis's case: a Requisiciones item due in two days (21 Nov) counts today",
   sum(panorama_pagos_hoy(ph[ph$id == "Requisiciones-anticipado", ], hoy = HOY, base = "MXN")$n) == 1L)
ok("a partial payment staged in the Agenda counts, like its twin (rule 25: not built further, never hidden)",
   isTRUE(all.equal(panorama_pagos_hoy(ph[ph$id == "abono-parcial", ], hoy = HOY, base = "MXN")$total, 30)))
ok("confirmed and cancelled rows and the AR ledger stay out",
   !any(c(7000, 9000, 8000) %in% p$total) && sum(p$n) == 14L)
ok("currencies never add up: MXN and USD apart", setequal(p$moneda, c("MXN", "USD")) &&
     isTRUE(all.equal(p$total[p$moneda == "USD"], 50)))
neg <- fila("nota", "manual", "2026-11-19", -40)
ok("no abs(): an amount keeps its sign, as the Agenda sums it",
   isTRUE(all.equal(panorama_pagos_hoy(rbind(ph[ph$id == "ERP-hoy", ], neg), hoy = HOY, base = "MXN")$total, 200 - 40)))
ok("nothing pending: an empty result, no error", nrow(panorama_pagos_hoy(ph[ph$status != "pending", ], hoy = HOY, base = "MXN")) == 0L &&
     nrow(panorama_pagos_hoy(NULL, hoy = HOY, base = "MXN")) == 0L)

# The breakdown under the card: overdue and ahead, each by currency.
d <- tryCatch(panorama_pagos_desglose(ph, hoy = HOY, base = "MXN"), error = function(e) NULL)
ok("the breakdown: 4 overdue (one per origin) and 6 ahead (4 + the USD one + the partial payment)",
   is.list(d) && sum(d$vencidos$n) == 4L && sum(d$anticipados$n) == 6L)
ok("…each by currency, never added up: ahead = MXN and USD apart",
   is.list(d) && setequal(d$anticipados$moneda, c("MXN", "USD")) &&
     isTRUE(all.equal(d$vencidos$total, 100 + 400 + 700 + 1000)))

# The Agenda's staged() moves to agenda_pendientes(ph) with no change of
# behaviour: identical() to its old dplyr::filter(status == "pending"), on the
# tibble load_pagar_hoy() returns, with every status, NA and zero rows.
suppressPackageStartupMessages(library(dplyr))
tb <- tibble::as_tibble(rbind(ph, fila("sin-status", "manual", "2026-11-19", 5, status = NA)))
ok("staged(): identical to the old filter, with pending, confirmed, cancelled, NA and the AR ledger",
   identical(agenda_pendientes(tb), dplyr::filter(tb, status == "pending")))
ok("staged(): identical with zero rows (the empty tibble keeps its columns)",
   identical(agenda_pendientes(tb[0, ]), dplyr::filter(tb[0, ], status == "pending")))
ok("staged(): identical with nothing pending", identical(agenda_pendientes(tb[tb$status %in% "confirmed", ]),
                                                        dplyr::filter(tb[tb$status %in% "confirmed", ], status == "pending")))

# The code: one source of rows, no date filter, the three figures on it.
ok("agenda_pendientes() exists, in R/agenda_filas.R", is.function(get0("agenda_pendientes")))
pd <- leer("R/panorama_datos.R")
cuerpo <- regmatches(pd, regexpr("(?s)panorama_pagos_hoy <- function.*?\n}\n", pd, perl = TRUE))
ok("panorama_pagos_hoy() reads its rows through agenda_pendientes(ph, \"AP\") and filters no date",
   grepl('agenda_pendientes(ph, "AP")', cuerpo, fixed = TRUE) && !grepl("FechaVenc", cuerpo, fixed = TRUE) &&
     !grepl("abs(", cuerpo, fixed = TRUE))
ph_mod <- leer("R/pagar_hoy_module.R")
ok("the Agenda's staged() reads through the same agenda_pendientes(ph)",
   grepl("staged <- reactive\\(\\{[^}]*\\}\\)\\)\\s*\\n\\s*agenda_pendientes\\(ph\\)", ph_mod) ||
     grepl("agenda_pendientes(ph)   # the same rows Panorama counts", ph_mod, fixed = TRUE))
pm <- leer("R/panorama_module.R")
ok("the Hoy card, the Clima and the Cinta all read the same pagos()",
   grepl("pagos  <- shiny::reactive(panorama_pagos_hoy(", pm, fixed = TRUE) &&
     grepl("n_pagos_hoy = sum(p$n)", pm, fixed = TRUE) && grepl("tr(\"panorama.cinta.pagos\", monto = .pan_monto_corto(p))", pm, fixed = TRUE) &&
     grepl(".pan_tarjeta_hoy(tr(\"panorama.hoy.programado\"), pagos(),", pm, fixed = TRUE))
ok("…and the card shows the breakdown", grepl("panorama_pagos_desglose(", pm, fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
