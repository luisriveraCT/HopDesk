# =============================================================================
# tests/test_pv2_refresco.R
# PV-2 (2026-11-22, paso 0 hopdesk_coord/tablas/reportes/PROVISIONES_VIVAS_paso0.md):
# pasivos_refrescar_provisiones() brings a client's stored provisions up to
# date once a day, over the real persistence on the in-memory S3.
#   1. Variable rate: the index moves -> the future pending provisions move;
#      the past one, the paid one and the hand-edited one do not. Under the
#      0.5 % / 1-unit threshold, no write.
#   2. Recurring with no end: extended to today + 2 years; the months an old
#      edit left empty come back; nothing duplicated.
#   3. Idempotent: a second refresh writes nothing; once a day without forzar.
#   4. Another session's provision written in between is kept.
#   5. Fixed-rate, paused liabilities and other liabilities' rows: identical.
#   6. A failed read of the liabilities stops it: nothing written.
#   7-8. The trigger: the session's client, and only a tier that may read
#      Pasivos (Compras reads it empty under T69b: never a start).
# Run: Rscript --vanilla tests/test_pv2_refresco.R (repo root)
# =============================================================================
source("tests/helpers/pasivos_arnes.R")
suppressPackageStartupMessages(library(shiny))
for (f in c("R/pasivos_wizard_validate.R", "R/pasivos_wizard_module.R")) suppressMessages(source(f))
iniciales_vigentes <- function(df, tabla, client_id) df   # CM29's translation: not under test here

pass <- 0L; fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("  PASS ", label, "\n") }
  else { fail <<- fail + 1L; cat("  FAIL ", label, "\n") }
}

# The index (TIIE28), in memory: the engine reads it with the client.
INDICE <- 9
load_pasivos_estimates <- function(client_id = NULL) {
  if (is.null(client_id)) stop("[.s3_key] refusing to guess a tenant")
  tibble::tibble(metric = "tiie28", fecha = as.Date("2025-01-01"), value = INDICE,
                 source_method = "manual", is_frozen = FALSE, updated_by = "t", updated_at = Sys.time())
}

credito <- function(id = "V1", ...) {
  d <- modifyList(list(categoria = "financiero", flavor = "credito_simple", nombre = "Crédito TIIE", empresa = "NG",
                       parte = "Banco", moneda_pago = "MXN", principal_original = 1e7, tasa_tipo = "variable_tiie28",
                       tasa_actual = 8, tasa_spread = 250, plazo_meses = 36L, frecuencia_pago = "mensual",
                       dia_pago = 5L, fecha_inicio = as.Date("2026-06-05"), metodo_amortizacion = "francesa",
                       periodo_gracia = 0L), list(...))
  l <- pasivos_wizard_draft_to_liability(d, mode = "create", user = "ana"); l$id <- id; l$estado <- "active"; l
}
renta <- function(id = "R1", estado = "active") {
  l <- .schema_pasivos_liability()[1, ]
  l$id <- id; l$categoria <- "regular"; l$estado <- estado; l$nombre <- paste("Renta", id)
  l$empresa <- "NG"; l$moneda_pago <- "MXN"; l$amount_default <- 1000
  l$recurrence_type <- "monthly_day"; l$recurrence_params <- list(list(day = 5L))
  l$fecha_inicio <- as.Date("2024-11-01"); l$fecha_vence <- as.Date(NA)
  l
}
provs <- function() .mock_store[["pasivos_provisions.rds"]]
sembrar <- function(liabs, p) {
  .reset_mock()
  .mock_store[["pasivos_liabilities.rds"]] <- .normalize(liabs, .schema_pasivos_liability)
  .mock_store[["pasivos_provisions.rds"]]  <- .normalize(p, .schema_pasivos_provision)
}
refrescar <- function(hoy, forzar = TRUE) pasivos_refrescar_provisiones(PASIVOS_CID, hoy = hoy, forzar = forzar)
vivas <- function(t) t[!t$estado %in% "deleted", , drop = FALSE]

cat("\n== 1. Variable rate ==\n")
HOY0 <- as.Date("2026-07-01"); HOY1 <- as.Date("2026-09-20")
v <- credito()
p <- pasivos_generate_provisions(v, window_start = HOY0, client_id = PASIVOS_CID)
p$estado[1] <- "converted"                                   # jul: paid
k_pasada <- which(p$fecha_efectiva >= HOY0 & p$fecha_efectiva < HOY1 & p$estado == "provisional")[1]   # aug/sep, before HOY1
k_mano <- which(p$fecha_efectiva > HOY1)[3]
p$amount_pago_override[k_mano] <- 123456
sembrar(v, p)
antes <- provs()
INDICE <- 9.0001                                             # under the threshold
r <- refrescar(HOY1)
ok("1.1 under the threshold: nothing written", identical(provs(), antes) && !nrow(r$cambios))
INDICE <- 10
r <- refrescar(HOY1)
desp <- provs()
fut <- desp$estado == "provisional" & desp$fecha_efectiva >= HOY1 & is.na(desp$amount_pago_override)
m <- match(desp$id, antes$id)
ok("1.2 the future pending ones moved (more interest)", any(fut) && all(desp$componente_interes[fut] > antes$componente_interes[m[fut]]))
ok("1.3 the past pending one did not", identical(desp[desp$id == antes$id[k_pasada], ], antes[k_pasada, ]))
ok("1.4 the paid one did not", identical(desp[desp$id == antes$id[1], ], antes[1, ]))
ok("1.5 the hand-edited one did not", identical(desp[desp$id == antes$id[k_mano], ], antes[k_mano, ]))
ok("1.6 the same rows (none added, none lost)", setequal(desp$id, antes$id))
ok("1.7 the change is reported as 'tasa'", nrow(r$cambios) == 1L && r$cambios$motivo == "tasa" && r$cambios$actualizadas > 0)

cat("\n== 2. Recurring with no end ==\n")
HOY <- as.Date("2026-11-22")
R1 <- renta()
viejo <- R1; viejo$fecha_inicio <- as.Date(NA)
p <- pasivos_generate_provisions(viejo, window_start = as.Date("2024-11-01"), client_id = PASIVOS_CID)   # as prod: 2 years
p$estado[p$fecha_efectiva < as.Date("2025-12-01")] <- "converted"
# the old wizard edit on 2025-12-10 (index from that day): the damage
p <- dplyr::bind_rows(lapply(pasivos_reconcile_provisions(p, pasivos_generate_provisions(viejo, window_start = as.Date("2025-12-10"),
                                                                                     client_id = PASIVOS_CID), mode = "regenerate"),
                             function(x) x))
sembrar(R1, p)
antes <- provs()
meses_fut <- function(desde, hasta) { m <- seq(as.Date("2026-12-05"), hasta, by = "month"); m[m >= desde] }
ok("2.0 before: months from today without a provision",
   length(setdiff(meses_fut(HOY, HOY + 365), vivas(antes)$fecha_calculada)) > 0)
r <- refrescar(HOY)
desp <- provs()
fut <- meses_fut(HOY, HOY + lubridate::years(2))
ok("2.1 every month to today + 2 years has a provision", all(fut %in% vivas(desp)$fecha_calculada))
ok("2.2 no date duplicated", !any(duplicated(vivas(desp)$fecha_calculada)))
viejas <- desp[desp$id %in% antes$id, ]
ok("2.3 the existing rows are identical (only inserts)",
   isTRUE(all.equal(as.data.frame(viejas[match(antes$id, viejas$id), ]), as.data.frame(antes), check.attributes = FALSE)))
ok("2.4 nothing inserted before today", all(desp$fecha_efectiva[!desp$id %in% antes$id] >= HOY))
ok("2.5 reported as 'horizonte'", nrow(r$cambios) == 1L && r$cambios$motivo == "horizonte" && r$cambios$nuevas > 0)

cat("\n== 3. Idempotent; once a day ==\n")
otra <- provs()
r2 <- refrescar(HOY)
ok("3.1 a second refresh writes nothing", identical(provs(), otra) && !nrow(r2$cambios))
r3 <- refrescar(HOY, forzar = FALSE)
r4 <- refrescar(HOY, forzar = FALSE)
ok("3.2 without forzar the second call that day does not run", isFALSE(r4$hecho))

cat("\n== 4. Another session's write in between ==\n")
sembrar(R1, p)
manual <- p[1, ]; manual$id <- "MANUAL"; manual$liability_id <- NA_character_; manual$origin <- "manual"
.mock_store[["pasivos_provisions.rds"]] <- dplyr::bind_rows(provs(), manual)   # written by another session
refrescar(HOY)
ok("4.1 the other session's provision is kept", "MANUAL" %in% provs()$id)

cat("\n== 5. What is never touched ==\n")
fija <- credito(id = "F1", tasa_tipo = "fija", tasa_actual = 12)
pausada <- renta(id = "R2", estado = "paused")
p_f <- pasivos_generate_provisions(fija, window_start = HOY0, client_id = PASIVOS_CID)
p_r2 <- pasivos_generate_provisions(pausada, window_start = as.Date("2024-11-01"), client_id = PASIVOS_CID)
sembrar(dplyr::bind_rows(fija, pausada, R1), dplyr::bind_rows(p_f, p_r2, p))
antes <- provs()
INDICE <- 12
refrescar(HOY)
desp <- provs()
for (lid in c("F1", "R2")) {
  a <- antes[antes$liability_id %in% lid, ]; d <- desp[desp$liability_id %in% lid, ]
  ok(sprintf("5.%s %s: identical", lid, if (lid == "F1") "fixed rate" else "paused"),
     nrow(a) == nrow(d) && isTRUE(all.equal(as.data.frame(d[match(a$id, d$id), ]), as.data.frame(a), check.attributes = FALSE)))
}

cat("\n== 6. A failed read stops it ==\n")
sembrar(R1, p)
antes <- provs()
orig <- .s3_leer_para_escribir
.s3_leer_para_escribir <- function(key, client_id) if (grepl("liabilities", key)) stop("sin red") else orig(key, client_id)
res <- tryCatch(refrescar(HOY), error = function(e) conditionMessage(e))
.s3_leer_para_escribir <- orig
ok("6.1 it stops with the error", is.character(res) && grepl("sin red", res))
ok("6.2 nothing written", identical(provs(), antes))

cat("\n== 7. The trigger ==\n")
src <- paste(readLines("R/pasivos_module.R", warn = FALSE), collapse = "\n")
ok("7.1 the module refreshes on the session's client and tier, with the client's day",
   grepl("observeEvent(list(cliente_sesion(), nivel_sesion())", src, fixed = TRUE) &&
     grepl("if (!pasivos_refresco_permitido(nivel_sesion())) return()", src, fixed = TRUE) &&
     grepl("pasivos_refrescar_provisiones(cid, hoy = hoy_cliente(shared))", src, fixed = TRUE))
mot <- paste(readLines("R/pasivos_engine.R", warn = FALSE), collapse = "\n")

cat("\n== 8. Only tiers that may read Pasivos (T69b) ==\n")
suppressMessages(source("R/tier_registry.R"))
.extract_fn("R/pasivos_module.R", "pasivos_refresco_permitido")
niveles <- niveles_cliente()
con_psv <- Filter(function(n) is.null(nivel_pestanas(n)) || "PSV" %in% nivel_pestanas(n), niveles)   # NULL = every tab
sin_psv <- Filter(function(n) !is.null(nivel_pestanas(n)) && !any(c("PSV", "CAL", "VEN", "PH") %in% nivel_pestanas(n)), niveles)
ok("8.0 there are tiers on both sides (Compras has neither)", length(con_psv) > 0 && length(sin_psv) > 0)
ok("8.1 a tier with Pasivos runs it", all(vapply(con_psv, pasivos_refresco_permitido, logical(1))))
ok("8.2 a tier without Pasivos (Compras) never does", !any(vapply(sin_psv, pasivos_refresco_permitido, logical(1))))
ok("8.3 an unknown, empty or missing tier never does",
   !pasivos_refresco_permitido("no_existe") && !pasivos_refresco_permitido("") &&
     !pasivos_refresco_permitido(NA_character_) && !pasivos_refresco_permitido(NULL))
ok("7.2 the write carries evento_sistema", grepl('evento_sistema("pasivos_refresco"', mot, fixed = TRUE))

s3_memoria_restaurar()
cat(sprintf("\nPV-2: %d PASS / %d FAIL\n", pass, fail))
if (fail > 0L) quit(status = 1L)
