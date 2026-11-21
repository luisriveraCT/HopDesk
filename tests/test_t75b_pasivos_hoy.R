# =============================================================================
# tests/test_t75b_pasivos_hoy.R
# T75b (CM27): the Pasivos engine runs on the CLIENT's day, not the server's.
# Prod runs in UTC: from 18:00 to 23:59 in Mexico Sys.Date() is already
# tomorrow, so a payment due TODAY counted as historico (financiero) or fell
# before the window (regular) -- the provision of today was not generated.
# The engine is shared by every user of the client and runs without a
# session, like pagar_hoy (CM27-ppl): hoy_cliente(shared) / hoy_en(), never
# the browser's zone.
#   1  hoy_cliente(): shared$zona_cliente() when it is there, ZONA_OMISION
#      when not (today: CM27 fase 1 adds the setting).
#   2  the engine's defaults: at 19:30 of Friday the 20th in Mexico (01:30 of
#      the 21st, UTC), a financiero payment due the 20th is NOT historico,
#      and a regular liability's provision of the 20th is generated.
#   3  the wizard's save (.pasivos_wizard_save, the real one): the liability
#      created at that moment has its provision of the 20th.
# The process runs in UTC like prod; the clock is fixed in globalenv.
# Run from project root:  Rscript tests/test_t75b_pasivos_hoy.R
# Contraprueba: on 148553d (before T75b), 2 and 3 fail (the 20th is missing).
# =============================================================================
Sys.setenv(TZ = "UTC")
cat("=== test_t75b_pasivos_hoy ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
source("tests/helpers/s3_memoria.R")
source("tests/helpers/aislar_global.R")
Sys.unsetenv("HOPDESK_PRODUCTION")
suppressWarnings(suppressMessages(shiny::loadSupport(".", renv = globalenv(), globalrenv = NULL)))
stopifnot("global.R se cargó con red hacia AWS" = aislar_global_sin_red())
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
AHORA <- as.POSIXct("2026-11-21 01:30:00", tz = "UTC")   # Friday the 20th, 19:30 in Mexico
EL_20 <- as.Date("2026-11-20")
assign("Sys.time", function() AHORA, envir = globalenv())
assign("Sys.Date", function() as.Date(format(AHORA, "%Y-%m-%d", tz = "UTC")), envir = globalenv())   # the server's: the 21st

# ── 1. hoy_cliente() ───────────────────────────────────────────────────────
ok("1 no client zone yet (CM27 fase 1): Mexico's day, the 20th -- not the server's 21st",
   exists("hoy_cliente") && identical(hoy_cliente(NULL), EL_20) && identical(hoy_cliente(list()), EL_20))
ok("1 with the client's zone: that zone's day (Madrid: the 21st)",
   exists("hoy_cliente") && identical(hoy_cliente(list(zona_cliente = function() list(zona = "Europe/Madrid"))), as.Date("2026-11-21")))
ok("1 a zone that does not exist: Mexico's day",
   exists("hoy_cliente") && identical(hoy_cliente(list(zona_cliente = function() list(zona = "Marte/Olympus"))), EL_20))

# ── 2. The engine's defaults ───────────────────────────────────────────────
liab <- function(...) {
  base <- tibble::tibble(
    id = "l1", categoria = "regular", subcategoria = "servicios", flavor = NA_character_, nombre = "Renta",
    empresa = "NTS", parte = "Arrendadora", codigo_parte = NA_character_, referencia_default = NA_character_,
    documento_template = NA_character_, moneda_pago = "MXN", cotizado_en = NA_character_,
    recurrence_type = "monthly_day", recurrence_params = list(list(day = 20L)),
    amount_default = 1000, amount_default_cot = NA_real_, tarjeta_provision_source = NA_character_,
    tarjeta_closing_day = NA_integer_, tarjeta_due_day = NA_integer_, tarjeta_credit_limit = NA_real_,
    principal_original = NA_real_, saldo_capital = NA_real_, tasa_actual = NA_real_, tasa_tipo = "fija",
    tasa_spread = NA_real_, tasa_estimate_method = NA_character_,
    fecha_inicio = as.Date("2026-10-01"), fecha_vence = as.Date(NA), plazo_meses = NA_integer_,
    dia_pago = NA_integer_, metodo_amortizacion = NA_character_, schedule_imported = list(NULL),
    cargos_iniciales = list(NULL), valor_residual = list(NULL), modifier_ids = list(character(0)),
    estado = "active", notas = NA_character_, created_by = "t", created_at = AHORA, updated_by = "t", updated_at = AHORA)
  o <- list(...); for (nm in names(o)) base[[nm]] <- o[[nm]]
  base
}
fin <- liab(categoria = "financiero", subcategoria = NA_character_, flavor = "credito_simple",
            recurrence_type = NA_character_, recurrence_params = list(NULL), amount_default = NA_real_,
            principal_original = 120000, tasa_actual = 12, tasa_spread = 0,
            fecha_inicio = as.Date("2026-06-20"), fecha_vence = as.Date("2027-06-20"),
            plazo_meses = 12L, dia_pago = 20L, metodo_amortizacion = "francesa")

sched <- tryCatch(suppressWarnings(pasivos_generate_schedule(fin)), error = function(e) NULL)
fila20 <- if (!is.null(sched)) sched[as.Date(sched$fecha) == EL_20, , drop = FALSE] else NULL
ok("2 financiero schedule, default today: the payment due the 20th is NOT historico (it is the client's today)",
   !is.null(fila20) && nrow(fila20) == 1L && identical(fila20$historico, FALSE))
ok("2 ... and the one due October 20th still is historico", !is.null(sched) && isTRUE(sched$historico[as.Date(sched$fecha) == as.Date("2026-10-20")]))

pf <- tryCatch(suppressWarnings(pasivos_generate_provisions(fin)), error = function(e) NULL)
ok("2 financiero provisions, default window: the first one is the 20th's",
   !is.null(pf) && nrow(pf) >= 1L && identical(min(as.Date(pf$fecha_calculada)), EL_20))
pr <- tryCatch(suppressWarnings(pasivos_generate_provisions(liab())), error = function(e) NULL)
ok("2 regular provisions (day 20), default window: the first one is the 20th's",
   !is.null(pr) && nrow(pr) >= 1L && identical(min(as.Date(pr$fecha_calculada)), EL_20))
pt <- tryCatch(suppressWarnings(pasivos_generate_provisions(fin, window_start = as.Date("2026-11-21"))), error = function(e) NULL)
ok("2 a caller's own window_start is still its own (the 21st: the 20th's is out)",
   !is.null(pt) && !EL_20 %in% as.Date(pt$fecha_calculada))

# ── 3. The wizard's save ───────────────────────────────────────────────────
avisos_actividad <- function(...) invisible(TRUE)
has_capability <- function(user, capability, empresa = NULL) TRUE
pasivos_autoriza <- function(capability) TRUE
CID <- "acme"
s3_memoria_reset()
assign(paste0(CID, "/", S3_KEYS$pasivos_provisions), .schema_pasivos_provision(), envir = .mem)
invisible(suppressMessages(save_pasivos_liabilities(.schema_pasivos_liability(), client_id = CID)))
sh <- list(
  effective_client_id = function() CID, current_user = reactiveVal("tester"),
  company_map = reactive(c(NTS = "Networks Transport")), empresa_sel = reactiveVal("Networks Transport"),
  pasivos_provisions_db = reactiveVal(.schema_pasivos_provision()), pasivos_liabilities_db = reactiveVal(.schema_pasivos_liability()),
  manual_inv = reactiveVal(.schema_manual()), pagar_hoy_db = reactiveVal(NULL), holiday_overrides_db = function() list(),
  bancos_confirmados = reactiveVal(NULL), visible_initials = reactiveVal(NULL),
  proveedores_db = reactiveVal(NULL), suppress_ledger_prov_refresh = function(...) invisible(NULL))
draft <- list(categoria = "regular", nombre = "Renta oficina", empresa = "NTS", parte = "Arrendadora",
              moneda_pago = "MXN", recurrence_type = "monthly_day", recurrence_params = list(day = 20L),
              fecha_inicio = as.Date("2026-10-01"), amount_default = 100)
G <- new.env()
suppressWarnings(suppressMessages(testServer(function(input, output, session)
  G$res <- .pasivos_wizard_save(list(mode = "create", edit_target_id = NULL, draft = draft), sh, session, "beto"), {})))
provs <- tryCatch(get(paste0(CID, "/", S3_KEYS$pasivos_provisions), envir = .mem), error = function(e) NULL)
ok("3 the wizard saves a liability (day 20) at 19:30 of the 20th: its first provision is the 20th's",
   isTRUE(G$res$ok) && !is.null(provs) && nrow(provs) >= 1L && identical(min(as.Date(provs$fecha_calculada)), EL_20))

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
