# =============================================================================
# tests/test_t88_provisiones_vector.R
# T88-2. pasivos_provisions_as_ledger_rows() (R/pasivos_calendar_glue.R) is
# vectorized: it built one tibble per provision (900 provisions: 5.6 s, the
# R process frozen for every session). It is money, so the rule is hard:
# its result must be identical() -- columns, types, order, NA -- to the
# row-by-row version it replaces, kept below VERBATIM as the oracle
# (.provisiones_fila_por_fila, from master 7227425b), on:
#   - edge cases: zero provisions, none provisional, archived liabilities, no
#     liabilities, a liability not found / NA / "", an NA liability id before
#     and after the match, missing liability columns, overrides, every
#     currency case, quoted in USD with and without the FIX, synthetic
#     documents, NA fields, and without fx_monto_aprox loaded;
#   - 300 random scenarios mixing every field;
#   - real data, when T88_DATOS points to an .rds list of
#     list(nombre, provisions, liabilities) (never committed: client data);
# plus the benchmark, row by row against vectorized.
# T99: one rule changed on purpose -- a liability whose id is NA is nobody's
# liability (the oracle is fed the liabilities without those rows; see igual()).
# Run from project root:  Rscript --vanilla tests/test_t88_provisiones_vector.R
# =============================================================================
cat("=== test_t88_provisiones_vector ===\n\n")
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x   # R/global.R's
source("R/pasivos_schemas.R")
for (e in parse("R/pasivos_engine.R", keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c("pasivos_pago_amount", "pasivos_pago_currency"))
    eval(e, globalenv())
for (e in parse("R/fx_banxico.R", keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c(".fx_valido", "fx_monto_aprox"))
    eval(e, globalenv())
source("R/pasivos_calendar_glue.R")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# ── The oracle: the row-by-row version of master 7227425b, verbatim ─────────
.provisiones_fila_por_fila <- function(provisions, liabilities, ledger = "AP", user = NULL,
                                              fx = if (exists(".fx_para_mostrar", mode = "function")) .fx_para_mostrar() else NULL) {
  if (!is.null(user) && !has_capability(user, "pasivos.view")) {
    return(.empty_provision_ledger_rows())
  }

  if (is.null(provisions) || !nrow(provisions)) {
    return(.empty_provision_ledger_rows())
  }

  # Only provisional provisions appear in the ledger; converted/confirmed/closed
  # are represented by their linked manual_inv row (source = "manual") to avoid double-counting.
  prov <- provisions[
    !is.na(provisions$estado) & provisions$estado == "provisional",
    , drop = FALSE
  ]

  if (!nrow(prov)) return(.empty_provision_ledger_rows())

  liabs <- if (!is.null(liabilities) && nrow(liabilities)) liabilities else NULL

  # A provision of an archived liability (estado "deleted") is not a payment
  # anyone will make. Archiving only removes FUTURE provisional provisions, so
  # past ones used to stay in the Calendar and the cash flow as overdue
  # payments (found 2026-10-31, CM06). Manual provisions have no liability.
  if (!is.null(liabs) && "estado" %in% names(liabs)) {
    archivados <- liabs$id[!is.na(liabs$estado) & liabs$estado == "deleted"]
    prov <- prov[is.na(prov$liability_id) | !prov$liability_id %in% archivados, , drop = FALSE]
    if (!nrow(prov)) return(.empty_provision_ledger_rows())
  }

  rows <- lapply(seq_len(nrow(prov)), function(i) {
    p   <- prov[i, , drop = FALSE]
    lid <- p$liability_id[1]

    liability <- if (!is.na(lid) && nzchar(lid %||% "") && !is.null(liabs)) {
      liabs[liabs$id == lid, , drop = FALSE]
    } else {
      NULL
    }

    empresa              <- if (!is.null(liability) && nrow(liability)) liability$empresa[1]     else p$empresa[1]
    pasivos_categoria    <- if (!is.null(liability) && nrow(liability)) liability$categoria[1]   else NA_character_
    pasivos_subcategoria <- if (!is.null(liability) && nrow(liability)) liability$subcategoria[1] else NA_character_

    saldo     <- tryCatch(pasivos_pago_amount(p),    error = function(e) NA_real_)
    # Without R/fx_banxico.R loaded (some tests), nothing is approximated.
    ax        <- if (exists("fx_monto_aprox", mode = "function")) fx_monto_aprox(p$amount_cotizado[1], p$cotizado_en[1], p$moneda_pago[1],
                                p$amount_pago_override[1], fx) else list(aprox = FALSE, importe_aprox = NA_real_)
    mon       <- tryCatch(pasivos_pago_currency(p),  error = function(e) NA_character_)
    fecha_ef  <- as.Date(p$fecha_efectiva[1])
    fecha_ca  <- as.Date(p$fecha_calculada[1])

    is_fee  <- identical(as.character(p$origin[1]), "initial_fee")
    doc_val <- as.character(p$documento[1] %||% NA_character_)
    # Fee provisions and provisions without a real Documento get a synthetic key
    # so they are never aggregated with each other or with SAP items in the calendar.
    if (is_fee || is.na(doc_val) || !nzchar(doc_val)) {
      doc_val <- paste0("PROV_", p$id[1])
    }

    tibble::tibble(
      Empresa                = as.character(empresa     %||% NA_character_),
      Parte                  = as.character(p$parte[1]  %||% NA_character_),
      Codigo                 = as.character(p$codigo_parte[1] %||% NA_character_),
      `Saldo vencido`        = as.numeric(saldo),
      Importe                = as.numeric(saldo),
      `Fecha de vencimiento` = fecha_ef,
      FechaVenc_Original     = fecha_ca,
      FechaEff               = fecha_ef,
      # FechaVenc_Proyectada intentionally omitted — left_join(ledger_moves) adds it as NA
      Tipo                   = "AP",
      source                 = "provision",
      Documento              = doc_val,
      Factura                = as.character(p$referencia[1]  %||% NA_character_),
      Moneda                 = as.character(mon),
      notas                  = as.character(p$notas[1]       %||% NA_character_),
      confirmed              = FALSE,
      has_sap_override       = FALSE,
      Movida                 = "Falso",
      provision_id           = as.character(p$id[1]),
      liability_id           = as.character(p$liability_id[1] %||% NA_character_),
      pasivos_categoria      = as.character(pasivos_categoria    %||% NA_character_),
      pasivos_subcategoria   = as.character(pasivos_subcategoria %||% NA_character_),
      importe_aprox          = ax$importe_aprox,
      aprox                  = ax$aprox
    )
  })

  dplyr::bind_rows(rows)
}

# ── The cases ────────────────────────────────────────────────────────────────
nueva <- pasivos_provisions_as_ledger_rows
# T99: a liability whose id is NA is nobody's liability. The oracle's subset
# let that row win (an all-NA row); the rule now is the oracle's result on the
# liabilities WITHOUT those rows -- the same thing for every other case.
sin_id_na <- function(liabs) if (is.null(liabs)) NULL else liabs[!is.na(liabs$id), , drop = FALSE]
igual <- function(prov, liabs, fx = NULL) {
  a <- tryCatch(.provisiones_fila_por_fila(prov, sin_id_na(liabs), fx = fx), error = function(e) paste("ERROR", conditionMessage(e)))
  b <- tryCatch(nueva(prov, liabs, fx = fx), error = function(e) paste("ERROR", conditionMessage(e)))
  # Both failing the same way would compare equal and prove nothing: the
  # oracle must give a result, never an error.
  ok_ <- !is.character(a) && identical(a, b)
  if (!ok_) { message("  difference: "); print(all.equal(a, b)) }
  ok_
}
FX  <- list(valor = 18.25, fecha = as.Date("2026-11-20"))
liab <- function(id, empresa, categoria = "regular", sub = NA_character_, estado = "active")
  tibble::tibble(id = id, categoria = categoria, subcategoria = sub, empresa = empresa, estado = estado)
prov <- function(id, liability_id = NA_character_, estado = "provisional", origin = "rule",
                 empresa = "NTS", moneda_pago = "MXN", cotizado_en = NA_character_,
                 amount_pago = 100, amount_cotizado = NA_real_, override = NA_real_,
                 documento = NA_character_, referencia = NA_character_, notas = NA_character_,
                 parte = "ACME", codigo = "P001", fe = as.Date("2026-12-01"), fc = as.Date("2026-11-30"))
  .normalize_prov(tibble::tibble(id = id, liability_id = liability_id, origin = origin, estado = estado,
                 fecha_calculada = fc, fecha_efectiva = fe, empresa = empresa, parte = parte, codigo_parte = codigo,
                 moneda_pago = moneda_pago, cotizado_en = cotizado_en, amount_pago = amount_pago,
                 amount_cotizado = amount_cotizado, amount_pago_override = override, documento = documento,
                 referencia = referencia, notas = notas))
.normalize_prov <- function(x) {
  s <- .schema_pasivos_provision()
  for (nm in setdiff(names(s), names(x))) x[[nm]] <- rep(s[[nm]][NA_integer_], nrow(x))
  x[, names(s)]
}
L <- dplyr::bind_rows(liab("L1", "Empresa Uno", "financiero", "credito"), liab("L2", "Empresa Dos", "tarjeta", "amex"),
                      liab("L3", "Archivada", estado = "deleted"), liab("L4", NA_character_))

cat("== edge cases ==\n")
ok("zero provisions (NULL and 0 rows)", igual(NULL, L) && igual(.schema_pasivos_provision(), L))
ok("none provisional", igual(dplyr::bind_rows(prov("a", estado = "converted"), prov("b", estado = NA)), L))
ok("all of an archived liability", igual(prov("a", "L3"), L))
ok("no liabilities (NULL and 0 rows)", igual(prov("a", "L1"), NULL) && igual(prov("a", "L1"), L[0, ]))
ok("liability found / not found / NA / empty string",
   igual(dplyr::bind_rows(prov("a", "L1"), prov("b", "L9"), prov("c", NA), prov("d", "")), L))
ok("a liability whose empresa is NA (L4) and a tarjeta (L2)", igual(dplyr::bind_rows(prov("a", "L4"), prov("b", "L2")), L))
ok("an NA liability id BEFORE the match: the match still wins (T99)",
   igual(dplyr::bind_rows(prov("a", "L1"), prov("b", "L9")), dplyr::bind_rows(liab(NA, "X"), L)))
# T99, the values themselves (the oracle comparison alone would not show which rule):
# a provision after the NA row keeps its liability; an orphan keeps its own empresa.
t99 <- nueva(dplyr::bind_rows(prov("a", "L1"), prov("b", "L9", empresa = "NCS"), prov("c", "L2")),
             dplyr::bind_rows(liab(NA, "X"), L, liab(NA, NA_character_)), fx = NULL)
ok("T99: with NA-id rows before and after, the provision of L1 gets Empresa Uno / financiero / credito",
   identical(c(t99$Empresa[1], t99$pasivos_categoria[1], t99$pasivos_subcategoria[1]),
             c("Empresa Uno", "financiero", "credito")))
ok("T99: ...the orphan (L9) keeps its own empresa, NCS, with no categoria",
   identical(t99$Empresa[2], "NCS") && is.na(t99$pasivos_categoria[2]))
ok("T99: ...the provision of L2 gets Empresa Dos / tarjeta", identical(c(t99$Empresa[3], t99$pasivos_categoria[3]), c("Empresa Dos", "tarjeta")))
ok("T99: ...no row comes out with Empresa NA", !anyNA(t99$Empresa))
t99b <- nueva(prov("a", "L1"), dplyr::bind_rows(liab("", "Vacio"), liab(NA, "X"), L), fx = NULL)
ok("T99: a liability whose id is \"\" is not matched either", identical(t99b$Empresa, "Empresa Uno"))
ok("an NA liability id AFTER the match: the match wins",
   igual(dplyr::bind_rows(prov("a", "L1"), prov("b", "L9")), dplyr::bind_rows(L, liab(NA, "X"))))
ok("liabilities without categoria/subcategoria columns", igual(prov("a", "L1"), L[, c("id", "empresa", "estado")]))
ok("liabilities without an estado column (nothing archived)", igual(prov("a", "L3"), L[, c("id", "empresa", "categoria")]))
ok("overrides: NA, a value, 0", igual(dplyr::bind_rows(prov("a"), prov("b", override = 250.5), prov("c", override = 0)), L))
ok("currency: MXN, USD, NA, empty (pasivos_pago_currency stop()ed: NA)",
   igual(dplyr::bind_rows(prov("a"), prov("b", moneda_pago = "USD"), prov("c", moneda_pago = NA), prov("d", moneda_pago = "")), L))
quoted <- dplyr::bind_rows(prov("a", moneda_pago = "MXN", cotizado_en = "USD", amount_cotizado = 10),
                           prov("b", moneda_pago = "MXN", cotizado_en = "USD", amount_cotizado = 10, override = 190),
                           prov("c", moneda_pago = "MXN", cotizado_en = "USD", amount_cotizado = NA),
                           prov("d", moneda_pago = "USD", cotizado_en = "USD", amount_cotizado = 10),
                           prov("e", moneda_pago = "MXN", cotizado_en = "EUR", amount_cotizado = 10))
ok("quoted in USD: with the FIX, without it (NULL), with an invalid one",
   igual(quoted, L, FX) && igual(quoted, L, NULL) && igual(quoted, L, list(valor = NA_real_)))
ok("documento: value, NA, empty; initial_fee always synthetic; an NA id",
   igual(dplyr::bind_rows(prov("a", documento = "F-1"), prov("b"), prov("c", documento = ""),
                          prov("d", origin = "initial_fee", documento = "F-2"), prov(NA_character_)), L))
ok("text fields NA (parte, codigo, referencia, notas) and dates NA",
   igual(prov("a", parte = NA, codigo = NA, referencia = NA, notas = NA, fe = as.Date(NA), fc = as.Date(NA)), L))
fx_guardada <- fx_monto_aprox; rm(fx_monto_aprox)
ok("without R/fx_banxico.R loaded (fx_monto_aprox missing): nothing approximated", igual(quoted, L, FX))
fx_monto_aprox <- fx_guardada

cat("\n== random (300 scenarios, every field mixed) ==\n")
set.seed(88)
pick <- function(n, x) sample(x, n, replace = TRUE)
generar <- function(n) {
  lids <- c(paste0("L", 1:30), NA, "", "L999")
  dplyr::bind_rows(lapply(seq_len(n), function(i) prov(
    id = if (runif(1) < 0.01) NA_character_ else paste0("p", i),
    liability_id = pick(1, lids), estado = pick(1, c(rep("provisional", 8), "converted", NA)),
    origin = pick(1, c("rule", "rule", "manual", "initial_fee", "residual", NA)),
    empresa = pick(1, c("NTS", "NCS", NA)), moneda_pago = pick(1, c("MXN", "MXN", "USD", NA, "")),
    cotizado_en = pick(1, c(NA, NA, "USD", "MXN")), amount_pago = pick(1, c(round(runif(1, 1, 1e6), 2), NA)),
    amount_cotizado = pick(1, c(round(runif(1, 1, 1e4), 2), NA)), override = pick(1, c(NA, NA, NA, round(runif(1, 1, 1e5), 2))),
    documento = pick(1, c(paste0("D", i), NA, "")), referencia = pick(1, c("R", NA)), notas = pick(1, c("n", NA)),
    fe = as.Date("2026-01-01") + sample(0:700, 1), fc = as.Date("2026-01-01") + sample(0:700, 1))))
}
LL <- dplyr::bind_rows(lapply(1:30, function(i) liab(paste0("L", i), pick(1, c(paste0("E", i), NA)),
                                                     pick(1, c("regular", "financiero", "tarjeta")),
                                                     pick(1, c("x", NA)), pick(1, c("active", "active", "deleted", NA)))))
malos <- 0L
for (k in 1:300) {
  P <- generar(sample(1:40, 1)); f <- pick(1, list(FX, NULL))[[1]]
  LLk <- if (k %% 25 == 0) dplyr::bind_rows(liab(NA, "X"), LL) else LL
  if (!igual(P, LLk, f)) malos <- malos + 1L
}
ok("300 random scenarios: identical() every time", malos == 0L)

# ── Real data (optional: never committed) ────────────────────────────────────
datos <- Sys.getenv("T88_DATOS", "")
if (nzchar(datos) && file.exists(datos)) {
  cat("\n== real data ==\n")
  for (d in readRDS(datos)) {
    ok(sprintf("real data %s: %d provisions, %d liabilities -- identical (FIX and no FIX)", d$nombre,
               nrow(d$provisions), nrow(d$liabilities)),
       igual(d$provisions, d$liabilities, FX) && igual(d$provisions, d$liabilities, NULL))
  }
}

# ── Benchmark ────────────────────────────────────────────────────────────────
cat("\n== benchmark (seconds, median of 3) ==\n")
mide <- function(f, P, L) median(vapply(1:3, function(i) system.time(f(P, L, fx = FX))[["elapsed"]], 0))
for (n in c(100L, 450L, 900L)) {
  P <- generar(n); P$estado <- "provisional"
  v <- mide(.provisiones_fila_por_fila, P, LL); w <- mide(nueva, P, LL)
  cat(sprintf("  %4d provisions: row by row %.2f s · vectorized %.3f s\n", n, v, w))
  if (n == 900L) ok("900 provisions: vectorized under 0.5 s", w < 0.5)
}

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
