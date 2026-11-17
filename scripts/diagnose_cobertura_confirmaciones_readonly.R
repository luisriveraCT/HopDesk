# =============================================================================
# scripts/diagnose_cobertura_confirmaciones_readonly.R
# READ-ONLY. T45, question 3: how much of what really moved through the banks
# is confirmed in HopDesk? The Monday email's "real" weeks come from
# bancos_confirmados (confirmaciones_vigentes()); if a client confirms little,
# those bars understate what happened.
#
# For each of the last N weeks (Monday to Sunday) and each currency, it
# compares the confirmations against the bank statement (bancos_movimientos,
# not deleted, currency from the movement's account):
#   conf_pagos / conf_cobros   confirmations in the week (counts)
#   mov_cargos / mov_abonos    statement debits / credits in the week
#                              (counts), leaving out the internal ones:
#                              traspaso, cambio, plazo, comision
#   pct_pagos / pct_cobros     confirmed amount as % of the statement's amount
#   pct_lig_cargos / _abonos   % of those movements tied to a confirmation
#                              (bancos_confirmados$mov_id) or to the agenda
#   pct_nomina                 nómina as % of the debits (paid, never in the plan)
# and, first, the span of each table (first/last date, weeks with rows).
#
# Prints only counts, percentages and dates: no amounts, no names, no
# account numbers.
#
# Usage (from the repo root, never from a session):
#   Rscript --vanilla scripts/diagnose_cobertura_confirmaciones_readonly.R \
#           <path to .Renviron> [client_id] [weeks]
# Defaults: "networks", 12 weeks.
#
# Guarantees no writes: every S3 write path -- the app's and aws.s3's own --
# is replaced by stop() before anything is read. Prints no secret (only the
# bucket name).
# =============================================================================
args      <- commandArgs(trailingOnly = TRUE)
renv      <- args[1]
client_id <- if (length(args) >= 2) args[2] else "networks"
semanas_n <- if (length(args) >= 3) as.integer(args[3]) else 12L
if (is.na(renv) || !file.exists(renv)) stop("Pass the path to the .Renviron to read.")
readRenviron(renv)
cat("Bucket:", Sys.getenv("S3_BUCKET"), "| client:", client_id, "| today:", format(Sys.Date()),
    "| weeks:", semanas_n, "\n\n")

suppressPackageStartupMessages({ library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a

# ── Read-only guard ───────────────────────────────────────────────────────────
.ro <- function(...) stop("READ-ONLY diagnostic: a write was attempted.", call. = FALSE)
for (fn in c("s3saveRDS", "put_object", "delete_object", "s3write_using", "copy_object"))
  if (exists(fn, envir = asNamespace("aws.s3"), inherits = FALSE))
    utils::assignInNamespace(fn, .ro, "aws.s3")

for (e in parse("R/global.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]])[1] %in% c("S3_KEYS", "strip_accents"))
    eval(e, globalenv())
source("R/persistence.R")
source("R/bancos_persistence.R")
.s3_write <- .ro
for (fn in ls(globalenv(), pattern = "^(save_|modificar_)")) assign(fn, .ro, envir = globalenv())

# ── Data ──────────────────────────────────────────────────────────────────────
conf <- confirmaciones_vigentes(load_bancos_confirmados(client_id = client_id))
movs <- load_bancos_movimientos(client_id = client_id)   # not deleted
# Account -> currency: ctas_cuentas (today's table) and the older
# bancos_cuentas. Only the id and the currency are kept.
cta_moneda <- function(df, id, mon) {
  if (!is.data.frame(df) || !nrow(df) || !all(c(id, mon) %in% names(df))) return(character())
  stats::setNames(toupper(trimws(as.character(df[[mon]]))), as.character(df[[id]]))
}
ctas <- tryCatch(.s3_read_with(S3_KEYS$ctas_cuentas, client_id = client_id), error = function(e) NULL)
bc   <- tryCatch(load_bancos_cuentas(client_id = client_id), error = function(e) NULL)
mon_de <- c(cta_moneda(ctas, "id", "Moneda"), cta_moneda(bc, "cuenta_id", "moneda"))
mon_de <- mon_de[!duplicated(names(mon_de))]

lunes_de <- function(f) { f <- as.Date(f); f - (as.integer(format(f, "%u")) - 1L) }
tramo <- function(nombre, f) {
  f <- as.Date(f); f <- f[!is.na(f)]
  if (!length(f)) return(cat(sprintf("%-22s sin filas\n", nombre)))
  cat(sprintf("%-22s %6d filas · de %s a %s · %d semanas con filas\n", nombre, length(f),
              format(min(f)), format(max(f)), length(unique(lunes_de(f)))))
}

cat("── Tramos ──\n")
tramo("confirmaciones pago", conf$fecha[conf$tipo %in% "pago"])
tramo("confirmaciones cobro", conf$fecha[conf$tipo %in% "cobro"])
tramo("movimientos banco", movs$fecha)
cat(sprintf("movimientos sin moneda (cuenta desconocida): %d\n\n",
            sum(is.na(mon_de[as.character(movs$cuenta_id)]))))

# ── Per week and currency ─────────────────────────────────────────────────────
INTERNOS <- c("traspaso", "cambio", "plazo", "comision")
hoy_lunes <- lunes_de(Sys.Date())
semanas <- hoy_lunes - 7L * rev(seq_len(semanas_n))      # the N full weeks before this one

movs$moneda <- unname(mon_de[as.character(movs$cuenta_id)])
movs$lunes  <- lunes_de(movs$fecha)
movs$ligado <- movs$id %in% conf$mov_id | !is.na(movs$agenda_id) & nzchar(movs$agenda_id)
mv <- movs[!(movs$tipo %in% INTERNOS) & movs$lunes %in% semanas & !is.na(movs$moneda), , drop = FALSE]
conf$moneda <- toupper(trimws(conf$moneda)); conf$lunes <- lunes_de(conf$fecha)
cf <- conf[conf$lunes %in% semanas, , drop = FALSE]

pct <- function(a, b) if (is.na(b) || b <= 0) NA_real_ else round(100 * a / b)
filas <- list()
for (s in as.list(semanas)) for (m in sort(unique(c(mv$moneda, cf$moneda)))) {
  car <- mv[mv$lunes == s & mv$moneda == m & mv$cargo > 0, , drop = FALSE]
  abo <- mv[mv$lunes == s & mv$moneda == m & mv$abono > 0, , drop = FALSE]
  cp  <- cf[cf$lunes == s & cf$moneda == m & cf$tipo %in% "pago", , drop = FALSE]
  cc  <- cf[cf$lunes == s & cf$moneda == m & cf$tipo %in% "cobro", , drop = FALSE]
  if (!nrow(car) && !nrow(abo) && !nrow(cp) && !nrow(cc)) next
  filas[[length(filas) + 1]] <- tibble(
    semana = format(s), moneda = m,
    conf_pagos = nrow(cp), mov_cargos = nrow(car),
    pct_pagos = pct(sum(cp$importe, na.rm = TRUE), sum(car$cargo, na.rm = TRUE)),
    pct_lig_cargos = pct(sum(car$ligado), nrow(car)),
    pct_nomina = pct(sum(car$cargo[car$tipo %in% "nomina"], na.rm = TRUE), sum(car$cargo, na.rm = TRUE)),
    conf_cobros = nrow(cc), mov_abonos = nrow(abo),
    pct_cobros = pct(sum(cc$importe, na.rm = TRUE), sum(abo$abono, na.rm = TRUE)),
    pct_lig_abonos = pct(sum(abo$ligado), nrow(abo)))
}
cat("── Por semana y moneda (sin internos: ", paste(INTERNOS, collapse = ", "), ") ──\n", sep = "")
if (!length(filas)) cat("sin datos en esas semanas\n") else {
  t <- bind_rows(filas)
  print(as.data.frame(t), row.names = FALSE)
  cat("\n── Resumen (mediana de las semanas con movimientos) ──\n")
  r <- t |> group_by(moneda) |>
    summarise(semanas = n(), pct_pagos = stats::median(pct_pagos, na.rm = TRUE),
              pct_cobros = stats::median(pct_cobros, na.rm = TRUE),
              pct_lig_cargos = stats::median(pct_lig_cargos, na.rm = TRUE),
              pct_lig_abonos = stats::median(pct_lig_abonos, na.rm = TRUE), .groups = "drop")
  print(as.data.frame(r), row.names = FALSE)
}
cat("\nLectura: pct_pagos ~100 = el histórico \"real\" del correo es fiel; muy por debajo =",
    "se confirma poco en HopDesk (o el estado de cuenta trae pagos fuera del plan, como nómina).\n")
