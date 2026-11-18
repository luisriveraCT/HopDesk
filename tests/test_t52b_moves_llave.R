# =============================================================================
# tests/test_t52b_moves_llave.R
# T52b: the dates moved by hand (moves.rds) were joined to the ledger -- and
# upserted -- on ledger + Empresa + Moneda + Documento, without the invoice's
# posting date. The ERP reuses a DocNum in another year, so:
#   - a date (and its note) moved on the 2025 invoice also moved the 2026 one;
#   - moving the 2026 invoice DELETED the 2025 invoice's move (upsert on the
#     document key): user data lost.
# Now moves.rds carries fecha_factura, the posting date (as policy_moves since
# T52); one reader (fila_por_factura(), mudanza_por_factura()) serves
# build_ledger_df(), the ledger module's df_with_moves() and its provisions
# block; upsert_moves() keys on the invoice. Rows written before T52b (no
# date) still reach their document.
#
# Run from project root:  Rscript --vanilla tests/test_t52b_moves_llave.R
# =============================================================================
cat("=== test_t52b_moves_llave ===\n\n")
`%||%` <- function(a, b) if (is.null(a)) b else a
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
cargar <- function(archivo, nombres) {
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% nombres)
      eval(e, globalenv())
}

SAP_B1_ERP_TYPE <- "sap_b1_service_layer"
source("R/monedas.R")
source("R/empresas_identidad.R")
source("R/data_pipeline.R")
cargar("R/persistence.R", c("active_abonos_summary", ".normalize", ".schema_moves", "upsert_moves"))

FC <- "Fecha de contabilización"
# Two AP invoices of NCS in MXN with the same number 1001: one of 2025 (Dell),
# one of 2026 (HP).
raw_ap <- tibble::tibble(
  Documento = c("1001", "1001"), Empresa = "NCS", Parte = c("Dell", "HP"), Moneda = "MXN",
  `Saldo vencido` = c(100, 200),
  `Fecha de contabilización` = as.Date(c("2025-03-01", "2026-10-01")),
  `Fecha de vencimiento`     = as.Date(c("2025-04-01", "2026-11-01")),
  check.names = FALSE)
mv_fila <- function(fecha_mov, fecha_factura = NA, notas = NA_character_, ledger = "AP", doc = "1001")
  tibble::tibble(ledger = ledger, Empresa = "NCS", Moneda = "MXN", Documento = doc,
                 fecha_factura = as.Date(fecha_factura),
                 FechaVenc_Proyectada = as.Date(fecha_mov), notas = notas,
                 moved_by = "luis", last_updated = as.POSIXct("2026-11-18", tz = "UTC"))
mv_2025 <- mv_fila("2025-06-30", "2025-03-01", "pago en junio")
mv_2026 <- mv_fila("2026-12-15", "2026-10-01", "pago en diciembre")
de <- function(df, fc, col = "FechaEff") df[[col]][as.Date(df[[FC]]) == as.Date(fc)]

# ── 1. build_ledger_df(): each invoice its own moved date and note ───────────
cat("-- 1. build_ledger_df --\n")
{
  df <- build_ledger_df(raw_ap, "AP", empresa = NULL, moves_df = bind_rows(mv_2025, mv_2026))
  ok("the 2025 invoice takes its own move", identical(de(df, "2025-03-01"), as.Date("2025-06-30")))
  ok("the 2026 invoice takes its own move", identical(de(df, "2026-10-01"), as.Date("2026-12-15")))
  ok("... and each its own note",
     identical(de(df, "2025-03-01", "notas"), "pago en junio") &&
       identical(de(df, "2026-10-01", "notas"), "pago en diciembre"))

  df <- build_ledger_df(raw_ap, "AP", empresa = NULL, moves_df = mv_2025)
  ok("the 2025 invoice's move does not move the 2026 one (reused number)",
     identical(de(df, "2026-10-01"), as.Date("2026-11-01")) &&
       identical(de(df, "2026-10-01", "Movida"), "Falso"))
  ok("... nor gives it its note", is.na(de(df, "2026-10-01", "notas")))

  df <- build_ledger_df(raw_ap[2, ], "AP", empresa = NULL, moves_df = mv_fila("2026-12-15", NA, "vieja"))
  ok("an undated move (before T52b) still moves its invoice, with its note",
     identical(df$FechaEff, as.Date("2026-12-15")) && identical(df$Movida, "Manual") &&
       identical(df$notas, "vieja"))

  otra <- mv_2026; otra$ledger <- "AR"
  df <- build_ledger_df(raw_ap[2, ], "AP", empresa = NULL, moves_df = otra)
  ok("an AR move does not reach an AP invoice", identical(df$FechaEff, as.Date("2026-11-01")))

  # The integrator's condition 1: an undated row plus a dated one of the same
  # document never duplicate an invoice (one value per ledger row).
  mezcla <- bind_rows(mv_fila("2025-05-05", NA, "vieja"), mv_2025, mv_2026)
  df <- build_ledger_df(raw_ap, "AP", empresa = NULL, moves_df = mezcla)
  ok("undated + dated rows of one document: the ledger keeps its 2 rows, each its own move",
     nrow(df) == 2L && identical(de(df, "2025-03-01"), as.Date("2025-06-30")) &&
       identical(de(df, "2026-10-01"), as.Date("2026-12-15")))

  # Manual moves win over policies, as before.
  pm <- tibble::tibble(ledger = "AP", Empresa = "NCS", Moneda = "MXN", Documento = "1001",
                       Parte = "HP", fecha_factura = as.Date("2026-10-01"),
                       FechaVenc_Politica = as.Date("2026-11-20"), policy_ids = "p1",
                       applied_by = "auto", applied_at = as.POSIXct("2026-10-02", tz = "UTC"))
  df <- build_ledger_df(raw_ap[2, ], "AP", empresa = NULL, moves_df = mv_2026, policy_moves_df = pm)
  ok("a move by hand still wins over the policy date", identical(df$FechaEff, as.Date("2026-12-15")))
}

# ── 2. Nothing stops applying: an old moves.rds gives master's ledger ────────
# The integrator's condition (T52): a table stored before the change (no
# fecha_factura at all) yields exactly the ledger master built.
# Master = dc94065 (production when T52 started), read with git show.
cat("-- 2. old moves.rds == master --\n")
{
  maestro <- tryCatch(system2("git", c("show", "dc94065:R/data_pipeline.R"), stdout = TRUE, stderr = FALSE),
                      error = function(e) NULL, warning = function(w) NULL)
  ok("master's R/data_pipeline.R (dc94065) can be read with git show", length(maestro) > 100L)
  env_m <- new.env(parent = globalenv())
  tryCatch(eval(parse(text = maestro, keep.source = FALSE), env_m), error = function(e) message(e$message))

  raw <- tibble::tibble(
    Documento = c("1001", "1002", "1003", "1003", NA, "1004"),
    Empresa = c("NCS", "NCS", "NCS", "ABC", "NCS", "NCS"),
    Parte = c("Dell", "HP", "Dell", "X", "Y", "Z"), Moneda = c("MXN", "MXN", "USD", "MXN", "MXN", "MXN"),
    `Saldo vencido` = c(100, 200, 300, 400, 500, 600),
    `Fecha de contabilización` = as.Date(c("2026-01-05", "2026-02-01", "2026-03-01", "2026-03-02", "2026-04-01", NA)),
    `Fecha de vencimiento`     = as.Date(c("2026-02-05", "2026-03-01", "2026-04-01", "2026-04-02", "2026-05-01", "2026-06-01")),
    check.names = FALSE)
  viejo <- tibble::tibble(
    ledger = c("AP", "AP", "AP", "AP", "AR", "AP", "AP"),
    Empresa = c("NCS", "NCS", "NCS", "ABC", "NCS", "NCS", "NCS"),
    Moneda = c("MXN", "MXN", "USD", "MXN", "MXN", "MXN", "MXN"),
    Documento = c("1001", "1001", "1003", "1003", "1002", NA, "1004"),
    FechaVenc_Proyectada = as.Date(c("2026-02-10", "2026-02-20", "2026-04-03", "2026-04-09", "2026-03-09", "2026-05-09", "2026-06-15")),
    notas = c("a", "b", NA, "d", "e", "f", "g"),
    moved_by = "luis", last_updated = as.POSIXct("2026-04-02", tz = "UTC"))
  antes <- function(m) tryCatch(env_m$build_ledger_df(raw, "AP", empresa = NULL, moves_df = m),
                                error = function(e) { message(e$message); NULL })
  nuevo <- build_ledger_df(raw, "AP", empresa = NULL, moves_df = viejo)
  ok("an old moves.rds (no fecha_factura) gives a ledger identical() to master's",
     identical(nuevo, antes(viejo)))
  ok("... and it does move dates (the check is not empty)", sum(nuevo$Movida == "Manual") >= 3L)
  ok("... also after .normalize() adds fecha_factura = NA",
     identical(build_ledger_df(raw, "AP", empresa = NULL, moves_df = .normalize(viejo, .schema_moves)),
               antes(viejo)))
  sin_notas <- dplyr::select(viejo, -notas)
  ok("... and with a moves table without notas", identical(build_ledger_df(raw, "AP", empresa = NULL, moves_df = sin_notas),
                                                           antes(sin_notas)))
  ok("... and with no moves at all (NULL, empty)",
     identical(build_ledger_df(raw, "AP", empresa = NULL, moves_df = NULL), antes(NULL)) &&
       identical(build_ledger_df(raw, "AP", empresa = NULL, moves_df = viejo[0, ]), antes(viejo[0, ])))
}

# ── 3. The ledger module's df_with_moves() and provisions: the same reader ───
cat("-- 3. ledger_module.R --\n")
{
  buscar <- function(e, nombre) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(nombre)))
      return(e[[3]][[2]])
    if (is.call(e) || is.expression(e)) for (x in as.list(e)) { r <- buscar(x, nombre); if (!is.null(r)) return(r) }
    NULL
  }
  cuerpo <- NULL
  for (e in parse("R/ledger_module.R", keep.source = FALSE)) { cuerpo <- buscar(e, "df_with_moves"); if (!is.null(cuerpo)) break }
  ok("df_with_moves() is found in R/ledger_module.R", !is.null(cuerpo))
  base <- build_ledger_df(raw_ap, "AP", empresa = NULL, moves_df = NULL)
  con <- function(mv) {
    env <- new.env(parent = globalenv())
    env$ledger <- "AP"; env$df_base <- function() base
    env$shared <- list(moves_db = function() mv, policy_moves_db = function() NULL)
    eval(cuerpo, env)
  }
  df <- con(bind_rows(mv_2025, mv_2026))
  ok("df_with_moves(): each invoice its own move",
     identical(de(df, "2025-03-01"), as.Date("2025-06-30")) && identical(de(df, "2026-10-01"), as.Date("2026-12-15")))
  df <- con(bind_rows(mv_fila("2025-05-05", NA, "vieja"), mv_2025, mv_2026))
  ok("df_with_moves(): undated + dated rows of one document keep the 2 rows",
     nrow(df) == nrow(base) && identical(de(df, "2026-10-01"), as.Date("2026-12-15")))
  df <- con(mv_2025)
  ok("df_with_moves(): the 2025 move does not move the 2026 invoice",
     identical(de(df, "2026-10-01"), as.Date("2026-11-01")) && is.na(de(df, "2026-10-01", "notas")))

  src <- readLines("R/ledger_module.R", warn = FALSE)
  ok("ledger_module.R no longer joins moves on its own (no left_join of lm / ledger_moves)",
     !any(grepl("left_join(lm", src, fixed = TRUE)) && !any(grepl("left_join(ledger_moves", src, fixed = TRUE)))
  ok("the provisions block takes its moved date from mudanza_por_factura()",
     any(grepl("mudanza_por_factura(prov_rows, moves, \"AP\")", src, fixed = TRUE)))
  # A provision has no posting date: it matches on its document, Documento NA on nothing.
  prov <- data.frame(Empresa = c("NCS", "NCS"), Moneda = "MXN", Documento = c("P-9", NA), stringsAsFactors = FALSE)
  m <- mudanza_por_factura(prov, bind_rows(mv_fila("2026-12-01", NA, doc = "P-9"), mv_fila("2026-12-02", NA, doc = NA)), "AP")
  ok("a provision (no posting date) takes the move of its document; Documento NA none",
     identical(m$FechaVenc_Proyectada, as.Date(c("2026-12-01", NA))))
}

# ── 4. Storage: the schema keeps fecha_factura ───────────────────────────────
cat("-- 4. schema --\n")
{
  ok(".schema_moves() has fecha_factura (Date)", inherits(.schema_moves()$fecha_factura, "Date"))
  ok(".normalize() keeps fecha_factura",
     identical(.normalize(mv_2026, .schema_moves)$fecha_factura, as.Date("2026-10-01")))
  n <- .normalize(dplyr::select(mv_2026, -fecha_factura), .schema_moves)
  ok("a row stored before T52b reads back with fecha_factura NA, nothing else lost",
     nrow(n) == 1L && is.na(n$fecha_factura) && identical(n$FechaVenc_Proyectada, as.Date("2026-12-15")) &&
       identical(n$notas, "pago en diciembre"))
}

# ── 5. upsert_moves(): one row per invoice ───────────────────────────────────
cat("-- 5. upsert_moves --\n")
{
  db <- upsert_moves(mv_2025, mv_2026)
  ok("moving the 2026 invoice keeps the 2025 invoice's own move",
     nrow(db) == 2L && setequal(format(db$fecha_factura), c("2025-03-01", "2026-10-01")))
  db <- upsert_moves(bind_rows(mv_2025, mv_2026), mv_fila("2027-01-10", "2026-10-01", "otra vez"))
  ok("moving the same invoice again replaces its row",
     nrow(db) == 2L && identical(db$FechaVenc_Proyectada[format(db$fecha_factura) == "2026-10-01"], as.Date("2027-01-10")) &&
       identical(db$FechaVenc_Proyectada[format(db$fecha_factura) == "2025-03-01"], as.Date("2025-06-30")))
  # Rows without the date (manual entries, provisions, rows before T52b):
  # as before, an undated row replaces the undated row of its document.
  db <- upsert_moves(mv_fila("2026-12-01", NA, doc = "P-9"), mv_fila("2026-12-20", NA, doc = "P-9"))
  ok("an undated row replaces the undated row of its document (as before)",
     nrow(db) == 1L && identical(db$FechaVenc_Proyectada, as.Date("2026-12-20")))
  # Rows without the column at all (callers not changed yet): as before.
  viejo_db  <- dplyr::select(mv_fila("2026-12-01", NA), -fecha_factura)
  viejo_new <- dplyr::select(mv_fila("2026-12-20", NA), -fecha_factura)
  db <- upsert_moves(viejo_db, viejo_new)
  ok("without the column on either side: one row, the new date (as before)",
     nrow(db) == 1L && identical(db$FechaVenc_Proyectada, as.Date("2026-12-20")))
  # The same invoice twice in one call: the latest date wins (as before).
  db <- upsert_moves(mv_2025[0, ], bind_rows(mv_fila("2026-12-01", "2026-10-01"), mv_fila("2026-12-31", "2026-10-01")))
  ok("the same invoice twice in one call keeps the latest date",
     nrow(db) == 1L && identical(db$FechaVenc_Proyectada, as.Date("2026-12-31")))
}

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
