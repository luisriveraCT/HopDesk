# =============================================================================
# tests/test_t52_politica_llave.R
# T52: the policy date (policy_moves.rds) was joined to the ledger on
# Empresa + Moneda + Documento, without the invoice's posting date. The ERP
# reuses a DocNum in another year, so:
#   - two open invoices with the same number shared the latest policy date;
#   - worse, the auto-apply ("new" scope) saw a reused number as "already
#     computed": the new invoice inherited, for good, the policy date of the
#     old one (a 2026 invoice shown overdue since 2025).
# Now policy_moves carries fecha_factura (the posting date, the identity
# agenda_filas_factura() uses): compute_policy_moves() writes it, one join
# (politica_por_factura()) serves build_ledger_df() AND the ledger module's
# df_with_moves(), and the "new" merge adds an invoice whose number exists
# only with another posting date. Rows written before T52 (no date) still
# reach their document and are never recomputed in bulk.
#
# Run from project root:  Rscript --vanilla tests/test_t52_politica_llave.R
# =============================================================================
cat("=== test_t52_politica_llave ===\n\n")
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
source("R/policy_engine.R")
cargar("R/persistence.R", c("active_abonos_summary", ".normalize", ".schema_policy_moves"))
cargar("R/settings/settings_companies.R",
       c(".policy_move_doc", ".policy_move_llave", ".policy_moves_sin_fecha", ".policy_moves_doc_unica",
         ".policy_moves_cubiertos", ".policy_moves_sumar",
         ".policy_moves_faltan"))   # T54: .policy_moves_sumar() adds these rows

FC <- "Fecha de contabilización"
# Two AP invoices of NCS in MXN with the same number 1001: one of 2025 (Dell),
# one of 2026 (HP).
raw_ap <- tibble::tibble(
  Documento = c("1001", "1001"), Empresa = "NCS", Parte = c("Dell", "HP"), Moneda = "MXN",
  `Saldo vencido` = c(100, 200),
  `Fecha de contabilización` = as.Date(c("2025-03-01", "2026-10-01")),
  `Fecha de vencimiento`     = as.Date(c("2025-04-01", "2026-11-01")),
  check.names = FALSE)
pm_fila <- function(parte, venc, fecha_factura = NA, aplicada = "2025-03-02")
  tibble::tibble(ledger = "AP", Empresa = "NCS", Moneda = "MXN", Documento = "1001",
                 fecha_factura = as.Date(fecha_factura), Parte = parte,
                 FechaVenc_Politica = as.Date(venc), policy_ids = "p1", applied_by = "auto",
                 applied_at = as.POSIXct(aplicada, tz = "UTC"))
pm_2025 <- pm_fila("Dell", "2025-04-15", "2025-03-01")
pm_2026 <- pm_fila("HP",   "2026-11-20", "2026-10-01", "2026-10-02")
eff <- function(df, fc) df$FechaEff[as.Date(df[[FC]]) == as.Date(fc)]

# ── 1. build_ledger_df(): each invoice its own policy date ───────────────────
cat("-- 1. build_ledger_df --\n")
{
  df <- build_ledger_df(raw_ap, "AP", empresa = NULL, moves_df = NULL,
                        policy_moves_df = bind_rows(pm_2025, pm_2026))
  ok("the 2025 invoice keeps its own policy date", identical(eff(df, "2025-03-01"), as.Date("2025-04-15")))
  ok("the 2026 invoice keeps its own policy date", identical(eff(df, "2026-10-01"), as.Date("2026-11-20")))

  df <- build_ledger_df(raw_ap[2, ], "AP", empresa = NULL, moves_df = NULL, policy_moves_df = pm_2025)
  ok("a policy row of the 2025 invoice does not move the 2026 one (reused number)",
     identical(df$FechaEff, as.Date("2026-11-01")) && identical(df$Movida, "Falso"))

  # A row written before T52 (no fecha_factura) still reaches its document.
  df <- build_ledger_df(raw_ap[2, ], "AP", empresa = NULL, moves_df = NULL,
                        policy_moves_df = pm_fila("HP", "2026-11-20", NA, "2026-10-02"))
  ok("a row without fecha_factura (before T52) still gives its invoice the policy date",
     identical(df$FechaEff, as.Date("2026-11-20")) && identical(df$Movida, "Políticas"))

  # The other ledger's row and a row with Documento NA reach nothing.
  otra <- pm_2026; otra$ledger <- "AR"
  df <- build_ledger_df(raw_ap[2, ], "AP", empresa = NULL, moves_df = NULL, policy_moves_df = otra)
  ok("an AR policy row does not reach an AP invoice", identical(df$FechaEff, as.Date("2026-11-01")))
}

# ── 2. The ledger module's df_with_moves(): the same join ────────────────────
cat("-- 2. ledger_module.R df_with_moves() --\n")
{
  # The real reactive's body, taken from the module (no copy of its code here).
  buscar <- function(e) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("df_with_moves")))
      return(e[[3]][[2]])
    if (is.call(e) || is.expression(e)) for (x in as.list(e)) { r <- buscar(x); if (!is.null(r)) return(r) }
    NULL
  }
  cuerpo <- NULL
  for (e in parse("R/ledger_module.R", keep.source = FALSE)) { cuerpo <- buscar(e); if (!is.null(cuerpo)) break }
  ok("df_with_moves() is found in R/ledger_module.R", !is.null(cuerpo))

  base <- build_ledger_df(raw_ap, "AP", empresa = NULL, moves_df = NULL, policy_moves_df = NULL)
  con_moves <- function(pm) {
    env <- new.env(parent = globalenv())
    env$ledger  <- "AP"
    env$df_base <- function() base
    env$shared  <- list(moves_db = function() NULL, policy_moves_db = function() pm)
    eval(cuerpo, env)
  }
  df <- con_moves(bind_rows(pm_2025, pm_2026))
  ok("df_with_moves(): the 2025 invoice keeps its own policy date",
     identical(eff(df, "2025-03-01"), as.Date("2025-04-15")))
  ok("df_with_moves(): the 2026 invoice keeps its own policy date",
     identical(eff(df, "2026-10-01"), as.Date("2026-11-20")))
  df <- con_moves(pm_2025)
  ok("df_with_moves(): the 2025 row does not move the 2026 invoice",
     identical(eff(df, "2026-10-01"), as.Date("2026-11-01")))
  ok("ledger_module.R no longer joins policy_moves on its own",
     !any(grepl("left_join(lpm", readLines("R/ledger_module.R", warn = FALSE), fixed = TRUE)))
}

# ── 3. compute_policy_moves(): one row per invoice, with its posting date ────
cat("-- 3. compute_policy_moves --\n")
{
  pp  <- tibble::tibble(parte = c("Dell", "HP"), policy_id = "p1", policy_order = 1L, ledger = "AP")
  cat_df <- tibble::tibble(id = "p1", type = "offset_days", params = list(list(days = 0)),
                           roll_direction = "forward")
  r <- tryCatch(compute_policy_moves(raw_ap, pp, cat_df, ledger = "AP"), error = function(e) { message(e$message); NULL })
  ok("compute_policy_moves() returns fecha_factura", is.data.frame(r) && "fecha_factura" %in% names(r))
  ok("two invoices with the same number give two rows, each with its posting date",
     is.data.frame(r) && nrow(r) == 2L &&
       setequal(format(r$fecha_factura), c("2025-03-01", "2026-10-01")))
  ok("compute_policy_moves()'s columns are those of .schema_policy_moves()",
     is.data.frame(r) && identical(names(r), names(.schema_policy_moves())))
}

# ── 4. Storage: .normalize keeps fecha_factura; old rows get NA ──────────────
cat("-- 4. schema --\n")
{
  ok(".schema_policy_moves() has fecha_factura (Date)",
     inherits(.schema_policy_moves()$fecha_factura, "Date"))
  ok(".normalize() keeps fecha_factura",
     identical(.normalize(pm_2026, .schema_policy_moves)$fecha_factura, as.Date("2026-10-01")))
  vieja <- dplyr::select(pm_2026, -fecha_factura)
  n <- .normalize(vieja, .schema_policy_moves)
  ok("a row stored before T52 reads back with fecha_factura NA, nothing else lost",
     nrow(n) == 1L && is.na(n$fecha_factura) && identical(n$FechaVenc_Politica, as.Date("2026-11-20")))
}

# ── 5. The "new" merge (auto-apply and Aplicar · Solo nuevas) ────────────────
cat("-- 5. .policy_moves_sumar --\n")
{
  r <- .policy_moves_sumar(pm_2025, bind_rows(pm_2025, pm_2026))
  ok("an invoice whose number exists only with another posting date is added",
     nrow(r) == 2L && setequal(format(r$fecha_factura), c("2025-03-01", "2026-10-01")))
  ok("the existing row is kept as it was",
     identical(r$FechaVenc_Politica[format(r$fecha_factura) == "2025-03-01"], as.Date("2025-04-15")))

  r <- .policy_moves_sumar(pm_2026, pm_fila("HP", "2027-01-01", "2026-10-01", "2026-11-17"))
  ok("an invoice that already has its own row is not added again", nrow(r) == 1L)

  legado <- pm_fila("HP", "2026-11-20", NA, "2026-10-02")
  r <- .policy_moves_sumar(legado, pm_fila("HP", "2027-01-01", "2026-10-01", "2026-11-17"))
  ok("a row from before T52 (no date) is not recomputed in bulk",
     nrow(r) == 1L && identical(r$FechaVenc_Politica, as.Date("2026-11-20")))
}

# ── 6. Nothing stops applying: an old policy_moves gives master's ledger ─────
# The integrator's condition: a policy_moves stored before T52 (no
# fecha_factura column at all) yields exactly the ledger master built.
# Master = dc94065 (production when T52 started), read with git show.
cat("-- 6. old policy_moves == master --\n")
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
    Parte = c("Dell", "Dell", "Dell", "X", "HP", "Y", "Z"),
    FechaVenc_Politica = as.Date(c("2026-02-10", "2026-02-20", "2026-04-03", "2026-04-09", "2026-03-09", "2026-05-09", "2026-06-15")),
    policy_ids = "p1", applied_by = "auto", applied_at = as.POSIXct("2026-04-02", tz = "UTC"))
  nuevo <- build_ledger_df(raw, "AP", empresa = NULL, moves_df = NULL, policy_moves_df = viejo)
  antes <- tryCatch(env_m$build_ledger_df(raw, "AP", empresa = NULL, moves_df = NULL, policy_moves_df = viejo),
                    error = function(e) { message(e$message); NULL })
  ok("an old policy_moves (no fecha_factura) gives a ledger identical() to master's",
     !is.null(antes) && identical(nuevo, antes))
  ok("... and it does move dates (the check is not empty)", any(nuevo$Movida == "Políticas"))
  # The same table read through the schema (fecha_factura NA in every row).
  nuevo_n <- build_ledger_df(raw, "AP", empresa = NULL, moves_df = NULL,
                             policy_moves_df = .normalize(viejo, .schema_policy_moves))
  ok("... also after .normalize() adds fecha_factura = NA", !is.null(antes) && identical(nuevo_n, antes))
}

# ── 7. Rows from before T52: only while the document is one invoice ──────────
# The Coordinator's rule (P1, B): an undated row reaches its document only
# when that document has a single posting date among the open invoices.
# With a reused number nobody can tell whose it is: it reaches neither, and
# each invoice gets its own dated row on the next apply, which replaces it.
cat("-- 7. undated rows --\n")
{
  legado <- pm_fila("Dell", "2025-04-15", NA, "2025-03-02")
  df <- build_ledger_df(raw_ap, "AP", empresa = NULL, moves_df = NULL, policy_moves_df = legado)
  ok("an undated row reaches neither invoice of a reused number (build_ledger_df)",
     identical(eff(df, "2025-03-01"), as.Date("2025-04-01")) &&
       identical(eff(df, "2026-10-01"), as.Date("2026-11-01")))

  env <- new.env(parent = globalenv())
  env$ledger  <- "AP"
  env$df_base <- function() build_ledger_df(raw_ap, "AP", empresa = NULL, moves_df = NULL, policy_moves_df = NULL)
  env$shared  <- list(moves_db = function() NULL, policy_moves_db = function() legado)
  df <- eval(cuerpo, env)
  ok("... nor in df_with_moves()", identical(eff(df, "2026-10-01"), as.Date("2026-11-01")))

  # The snapshot's open invoices, as the merge sees them.
  fact <- politica_facturas(list(AP = raw_ap, AR = NULL))
  ok("politica_facturas() takes each invoice's posting date from the snapshot",
     nrow(fact) == 2L && setequal(format(fact$fecha_factura), c("2025-03-01", "2026-10-01")) &&
       all(fact$ledger == "AP"))

  # Merge: both invoices computed, the undated row replaced, not duplicated.
  r <- .policy_moves_sumar(legado, bind_rows(pm_2025, pm_2026), fact)
  ok("reused number: each invoice gets its own dated row and the undated one is replaced",
     nrow(r) == 2L && !any(is.na(r$fecha_factura)) &&
       setequal(format(r$fecha_factura), c("2025-03-01", "2026-10-01")))

  # Only one of the two has a partner with policies: it is added, the
  # undated row (whose owner nobody knows) is replaced by it.
  r <- .policy_moves_sumar(legado, pm_2026, fact)
  ok("reused number, only one invoice computed: it is added, the undated row replaced",
     nrow(r) == 1L && identical(format(r$fecha_factura), "2026-10-01"))

  # One invoice: the undated row stays and nothing is recomputed.
  r <- .policy_moves_sumar(legado, pm_2025, fact[1, ])
  ok("one invoice: the undated row is kept and nothing is added",
     nrow(r) == 1L && is.na(r$fecha_factura) && identical(r$FechaVenc_Politica, as.Date("2025-04-15")))

  # The replacement never touches another posting date, ledger or document.
  otra_fecha <- pm_fila("Dell", "2024-05-01", "2024-04-01", "2024-04-02")
  otro_ledger <- legado; otro_ledger$ledger <- "AR"
  otro_doc <- legado; otro_doc$Documento <- "1002"
  fresco <- bind_rows(legado, otra_fecha, otro_ledger, otro_doc)
  r <- .policy_moves_sumar(fresco, bind_rows(pm_2025, pm_2026), fact)
  ok("the replacement removes only the undated row of that ledger and document",
     nrow(r) == 5L &&
       sum(format(r$fecha_factura) %in% "2024-04-01") == 1L &&
       sum(r$ledger == "AR") == 1L && sum(r$Documento == "1002") == 1L &&
       !any(is.na(r$fecha_factura) & r$ledger == "AP" & r$Documento == "1001"))
}

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
