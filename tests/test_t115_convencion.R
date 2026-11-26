# =============================================================================
# tests/test_t115_convencion.R
# T115: the day-count convention of a financiero (PASIVOS_CONVENCIONES).
#   A  the figures of the paso 0's table: 100,000 at 8.75 %, the first
#      payment -- efectiva 701.46, 30/360 729.17, Act/360 753.47 (31 days),
#      Act/365 743.15 -- and Act/360 680.56 for a 28-day month (bullet: the
#      interest is on the whole principal).
#   B  the conventions at their edges: 30E/360 (31st, February), Act/Act
#      across a new year into a leap year; r for the level payment.
#   C  what is saved does not change: with no convention (NA) the schedule,
#      the provisions and the cost of a fixed and a variable liability are
#      exactly 8739e70f's; NA = efectiva (fixed) / Act/360 (variable).
#   D  every method takes the convention (francesa, alemana, americana,
#      grace); the schedule carries Días and the period's rate.
#   E  the client's default: copied into a NEW liability, never read live --
#      only the wizard's Términos and the Empresas form read it.
#   F  the wizard: draft <-> liability, the step's selector, Revisión's rate,
#      the table's Días; T78's EAR by convention; group_config keeps it.
#   G  editing a saved liability's convention: the pending provisions are
#      recomputed, a paid one is untouched.
# Run from project root:  Rscript --vanilla tests/test_t115_convencion.R
# =============================================================================
cat("=== test_t115_convencion ===\n\n")
suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid)
})
source("tests/helpers/s3_memoria.R")
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
source("R/monedas.R"); source("R/zona.R")
for (f in c("R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R",
            "R/pasivos_capabilities.R", "R/pasivos_audit.R", "R/forecasting_resolve.R",
            "R/policy_engine.R", "R/pasivos_engine.R", "R/pasivos_deuda.R", "R/pasivos_wizard_validate.R",
            "R/pasivos_wizard_module.R", "R/pasivos_wizard_steps.R", "R/pasivos_edit_confirm_module.R",
            "R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R", "R/ui_components.R"))
  suppressMessages(source(f))
isTRUE_safe <- function(x) !is.na(x) & x
HOY <- as.Date("2026-11-26")
hoy_cliente <- function(...) HOY
forecasting_get_estimate <- function(...) NA_real_
has_capability <- function(...) TRUE
bump_sync_version <- function(...) invisible(NULL)
AUDIT <- list()
pasivos_log_audit <- function(...) { AUDIT[[length(AUDIT) + 1L]] <<- list(...); invisible(NULL) }
load_papelera <- function(...) data.frame(); save_papelera <- function(df, ...) invisible(TRUE)
pasivos_edit_confirm_open <- function(...) invisible(NULL)

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}
casi <- function(a, b, tol = 0.005) length(a) == length(b) && all(abs(a - b) < tol)
prestamo <- function(...) modifyList(list(
  categoria = "financiero", flavor = "credito_simple", nombre = "Préstamo", empresa = "NG", parte = "Banco",
  moneda_pago = "USD", principal_original = 100000, tasa_tipo = "fija", tasa_actual = 8.75,
  plazo_meses = 14L, dia_pago = 1L, frecuencia_pago = "mensual", periodo_gracia = 0L,
  fecha_inicio = as.Date("2026-12-01"), metodo_amortizacion = "francesa"), list(...))
liab <- function(...) pasivos_wizard_draft_to_liability(prestamo(...), mode = "create", user = "x")
cal <- function(l, hoy = as.Date("2026-11-01")) { s <- pasivos_generate_schedule(l, today = hoy, usar_indice = FALSE); s[order(s$fecha), ] }

# ── A ────────────────────────────────────────────────────────────────────────
cat("-- A. the paso 0's figures --\n")
esperado <- c(efectiva = 701.46, `30_360` = 729.17, act_360 = 753.47, act_365 = 743.15)
for (cv in names(esperado)) {
  s <- cal(liab(convencion_dias = cv))
  ok(isTRUE(round(s$interes[1], 2) == esperado[[cv]]),
     sprintf("A: %s -- the first payment (December, 31 days) pays %.2f", cv, esperado[[cv]]))
}
bullet <- cal(liab(convencion_dias = "act_360", metodo_amortizacion = "americana"))
feb <- bullet[format(bullet$fecha, "%Y-%m") == "2027-03", ]   # the period Feb 1 -> Mar 1: 28 days
ok(nrow(feb) == 1L && isTRUE(round(feb$interes, 2) == 680.56) && feb$dias == 28,
   "A: Act/360, a 28-day month on the whole principal: 680.56")

# ── B ────────────────────────────────────────────────────────────────────────
cat("-- B. the conventions at their edges --\n")
d <- as.Date
ok(pasivos_dias_periodo("30_360", d("2027-01-31"), d("2027-02-28")) == 28 &&
     pasivos_dias_periodo("30_360", d("2027-01-15"), d("2027-02-15")) == 30 &&
     pasivos_dias_periodo("30_360", d("2026-12-31"), d("2027-01-31")) == 30,
   "B: 30E/360 -- a 31st counts as the 30th; a whole month is 30")
ok(pasivos_dias_periodo("act_360", d("2027-01-15"), d("2027-02-15")) == 31 && is.na(pasivos_dias_periodo("efectiva", d("2027-01-15"), d("2027-02-15"))),
   "B: Act counts the real days; efectiva has none")
f_aa <- pasivos_tasas_convencion("act_act", 0.0875, 12, d("2027-12-15"), d("2028-01-15"))$f
ok(casi(f_aa, 0.0875 * (17 / 365 + 14 / 366), 1e-12), "B: Act/Act -- 17 days of 2027 over 365 and 14 of leap 2028 over 366")
ok(casi(pasivos_tasas_convencion("act_365", 0.0875, 12, d("2028-02-01"), d("2028-03-01"))$f, 0.0875 * 29 / 365, 1e-12),
   "B: Act/365 -- February of a leap year, 29 days over 365")
ok(casi(pasivos_tasas_convencion("act_360", 0.0875, 12, d("2027-01-01"), d("2027-02-01"))$r, 0.0875 / 12, 1e-12) &&
     casi(pasivos_tasas_convencion("efectiva", 0.0875, 12, d("2027-01-01"), d("2027-02-01"))$r, 1.0875^(1/12) - 1, 1e-12),
   "B: the level payment's r: t/m for a nominal one, (1+t)^(1/m)-1 for efectiva")
ok(inherits(tryCatch(pasivos_tasas_convencion("no_existe", 0.1, 12, HOY, HOY + 30), error = function(e) e), "error"),
   "B: an unknown convention stops, never computes something else")

# ── C ────────────────────────────────────────────────────────────────────────
cat("-- C. what is saved does not change --\n")
base <- new.env()
for (e in parse(text = system2("git", c("show", "8739e70f:R/pasivos_engine.R"), stdout = TRUE), keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=")) eval(e, base)
for (tipo in c("fija", "variable_tiie28")) for (met in c("francesa", "alemana", "americana")) {
  l <- liab(tasa_tipo = tipo, tasa_spread = 150, metodo_amortizacion = met, periodo_gracia = 2L)
  l$convencion_dias <- NA_character_
  a <- base$pasivos_generate_schedule(l, today = as.Date("2027-03-01"), usar_indice = FALSE)
  b <- pasivos_generate_schedule(l, today = as.Date("2027-03-01"), usar_indice = FALSE)
  ok(identical(as.data.frame(a), as.data.frame(b[, names(a)])),
     sprintf("C: %s, %s, with grace, no convention: the schedule is exactly 8739e70f's", tipo, met))
}
l <- liab(tasa_tipo = "variable_tiie28", tasa_spread = 150); l$convencion_dias <- NA_character_
cols <- c("occurrence_index", "fecha_calculada", "amount_pago", "componente_capital", "componente_interes")
pa <- base$pasivos_generate_provisions(l, window_start = HOY); pb <- pasivos_generate_provisions(l, window_start = HOY)
ok(nrow(pa) > 0L && identical(as.data.frame(pa[, cols]), as.data.frame(pb[, cols])), "C: a variable one's provisions are exactly 8739e70f's")
lf <- liab(); lf$convencion_dias <- NA_character_
ok(identical(base$pasivos_costeo(lf, hoy = HOY, usar_indice = FALSE)$filas[, c("intereses", "impuestos", "total", "anual")],
             pasivos_costeo(lf, hoy = HOY, usar_indice = FALSE)$filas[, c("intereses", "impuestos", "total", "anual")]),
   "C: a fixed one's cost (T77/T109) is exactly 8739e70f's")
ok(identical(pasivos_convencion(list(tasa_tipo = "fija")), "efectiva") &&
     identical(pasivos_convencion(list(tasa_tipo = "variable_sofr", convencion_dias = NA)), "act_360") &&
     identical(pasivos_convencion(list(tasa_tipo = "fija", convencion_dias = "nada")), "efectiva"),
   "C: NA (or anything unknown) = the one it had before: efectiva fixed, Act/360 variable")
le <- liab(convencion_dias = "efectiva"); ln <- liab(); ln$convencion_dias <- NA_character_
ok(identical(cal(le)$interes, cal(ln)$interes), "C: writing the old one explicitly gives the same figures")

# ── D ────────────────────────────────────────────────────────────────────────
cat("-- D. every method takes the convention --\n")
for (met in c("francesa", "alemana", "americana")) {
  s <- cal(liab(convencion_dias = "act_365", metodo_amortizacion = met))
  saldo_antes <- c(100000, utils::head(s$saldo_post, -1))
  base_int <- if (met == "americana") rep(100000, nrow(s)) else saldo_antes
  ok(casi(s$interes, base_int * 0.0875 * s$dias / 365, 0.01), sprintf("D: %s -- each interest = balance × t × days / 365", met))
}
g <- cal(liab(convencion_dias = "act_360", periodo_gracia = 2L))
ok(casi(g$interes[1:2], 100000 * 0.0875 * g$dias[1:2] / 360, 0.01) && all(g$capital[1:2] == 0), "D: grace periods: interest by the real days")
ok(all(c("dias", "tasa_periodo") %in% names(g)) && casi(g$tasa_periodo[1], 8.75 * 31 / 360, 1e-9), "D: the schedule carries Días and the period's rate")
ok(sum(cal(liab(convencion_dias = "30_360"))$capital) - 100000 < 0.01, "D: the principal is still repaid in full")

# ── E ────────────────────────────────────────────────────────────────────────
cat("-- E. the client's default: copied at creation, never read live --\n")
ok(identical(pasivos_convencion_cliente(NA), "act_360") && identical(pasivos_convencion_cliente("x"), "act_360") &&
     identical(pasivos_convencion_cliente("30_360"), "30_360"), "E: the client's default, Act/360 when none")
lectores <- unlist(lapply(c("app.R", list.files("R", "[.]R$", full.names = TRUE)), function(f) {
  l <- sub("#.*$", "", readLines(f, warn = FALSE, encoding = "UTF-8"))
  i <- grep("convencion_dias", l, fixed = TRUE); i <- i[grepl("group_config|cfg\\$convencion_dias|base\\$convencion", l[i])]
  if (length(i)) paste0(f, ":", i) else character()
}))
ok(length(lectores) >= 2L && all(grepl("^R/(pasivos_wizard_module|empresas_module)\\.R:", lectores)),
   "E: only the wizard (a new liability's default) and the Empresas form read the client's convention")
ok(!grepl("group_config", paste(deparse(body(pasivos_convencion)), collapse = " "), fixed = TRUE) &&
     identical(names(formals(pasivos_convencion)), "liability"),
   "E: pasivos_convencion() reads the liability only")

# ── F ────────────────────────────────────────────────────────────────────────
cat("-- F. the wizard, the EAR, group_config --\n")
ok(identical(liab(convencion_dias = "act_365")$convencion_dias, "act_365") && is.na(liab()$convencion_dias) &&
     is.na(liab(convencion_dias = "nada")$convencion_dias), "F: draft -> liability keeps a valid one, NA otherwise")
ok(identical(pasivos_liability_to_draft(ln)$convencion_dias, "efectiva"), "F: liability -> draft shows the one it computes with")
nuevo <- paste(as.character(pasivos_step_ui_terminos(list(categoria = "financiero"), convencion_omision = "30_360")), collapse = "")
guard <- paste(as.character(pasivos_step_ui_terminos(prestamo(convencion_dias = "act_365"), convencion_omision = "30_360")), collapse = "")
sel <- function(h) sub('.*<option value="([^"]+)" selected>.*', "\\1", regmatches(h, regexpr('<select[^>]*id="pwiz_convencion_dias".*?</select>', h)))
ok(identical(sel(nuevo), "30_360") && identical(sel(guard), "act_365"), "F: Términos -- a new one starts on the client's, a saved one shows its own")
ok(grepl(tr("tablas.pasivos.convencion.titulo"), nuevo, fixed = TRUE), "F: the selector's label from the dictionary")
rv <- paste(as.character(.wiz_revision_summary_html(prestamo(convencion_dias = "act_360"))), collapse = "")
rv0 <- paste(as.character(.wiz_revision_summary_html(prestamo())), collapse = "")
ok(grepl("8.750 % anual nominal · Actual/360", rv, fixed = TRUE) && grepl("8.750% anual (0.701% mensual)", rv0, fixed = TRUE),
   "F: Revisión -- nominal with its convention; the old effective text for the old one")
v <- pasivos_vista_amortizacion(prestamo(convencion_dias = "act_360", fecha_inicio = as.Date("2027-01-01")), completa = TRUE)
ok(all(c("dias", "tasa_periodo") %in% names(v)) && identical(as.numeric(v$dias[1:3]), c(31, 28, 31)), "F: the table's rows carry Días (31, 28, 31)")
ok(casi(.wiz_tasa_periodo(8.75, 12, "act_360"), 8.75 / 12, 1e-12) && casi(.wiz_tasa_anual(8.75 / 12, 12, "act_360"), 8.75, 1e-12) &&
     casi(.wiz_tasa_periodo(8.75, 12, "efectiva"), (1.0875^(1/12) - 1) * 100, 1e-12), "F: annual <-> period by the convention")
ear <- function(cv) pasivos_deuda_pasivo(liab(convencion_dias = cv, fecha_inicio = as.Date("2026-06-01")), hoy = HOY, client_id = "a")$ear
ok(casi(ear(NA), 8.75, 1e-9) && casi(ear("act_365"), 100 * ((1 + 0.0875 / 12)^12 - 1), 1e-6) &&
     casi(ear("act_360"), pasivos_ear_act360(8.75, 12), 1e-9), "F: T78's EAR -- the captured rate when efectiva, derived when nominal")
src_p <- readLines("R/persistence.R", warn = FALSE, encoding = "UTF-8")
for (e in parse(text = src_p, keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% c(".schema_group_config", ".GROUP_CONFIG_UNIDADES")) eval(e)
ok("convencion_dias" %in% names(.schema_group_config()) && "convencion_dias" %in% unlist(.GROUP_CONFIG_UNIDADES),
   "F: group_config has it, and a save writes it as its own unit")
for (idioma in c("es", "en")) {
  dic <- jsonlite::fromJSON(sprintf("www/i18n/%s.json", idioma))
  k <- c(paste0("tablas.pasivos.convencion.opcion.", PASIVOS_CONVENCIONES), paste0("tablas.pasivos.convencion.corto.", PASIVOS_CONVENCIONES),
         "tablas.pasivos.convencion.titulo", "tablas.pasivos.convencion.ayuda", "tablas.pasivos.convencion.resumen",
         "tablas.pasivos.convencion.cliente_titulo", "tablas.pasivos.convencion.cliente_ayuda", "tablas.pasivos.amort.dias")
  ok(all(k %in% names(dic)) && !any(grepl("Actual/360", dic[c("tablas.pasivos.tasa_var.fila", "tablas.pasivos.tasa_var.revision")], fixed = TRUE)),
     sprintf("F: %s.json -- every convention named; the variable rate's basis is no longer written by hand", idioma))
}

# ── G ────────────────────────────────────────────────────────────────────────
cat("-- G. editing a saved liability's convention --\n")
CID <- "acme"
l0 <- liab(fecha_inicio = as.Date("2026-06-01")); l0$id <- "L1"; l0$convencion_dias <- NA_character_
provs <- pasivos_generate_provisions(l0, window_start = HOY)
provs$estado[1] <- "item_confirmed"                     # one already paid
s3_memoria_reset()
save_pasivos_liabilities(l0, client_id = CID); .s3_write(provs, S3_KEYS$pasivos_provisions, client_id = CID)
sh <- list(pasivos_liabilities_db = reactiveVal(l0), pasivos_provisions_db = reactiveVal(provs),
           company_map = function() list(NG = "Nuevo Grupo"), holiday_overrides_db = function() list(),
           effective_client_id = function() CID, current_user = function() "ana",
           suppress_ledger_prov_refresh = function(...) invisible(NULL))
wiz <- list(mode = "edit", edit_target_id = "L1",
            draft = modifyList(pasivos_liability_to_draft(l0), list(convencion_dias = "act_360")))
res <- NULL
invisible(capture.output(testServer(function(input, output, session) res <<- .pasivos_wizard_save(wiz, sh, session, "ana"), {})))
s3l <- load_pasivos_liabilities(client_id = CID); s3p <- load_pasivos_provisions(client_id = CID)
ok(isTRUE(res$ok) && identical(s3l$convencion_dias[s3l$id == "L1"], "act_360"), "G: the liability keeps its new convention")
pend <- s3p[s3p$estado == "provisional", ]; orig <- provs[provs$estado == "provisional", ]
m <- match(pend$occurrence_index, orig$occurrence_index)
ok(nrow(pend) > 0L && any(abs(pend$componente_interes - orig$componente_interes[m]) > 0.01), "G: the pending provisions are recomputed")
pag <- s3p[s3p$estado == "item_confirmed", ]
ok(nrow(pag) == 1L && identical(pag$componente_interes, provs$componente_interes[1]) && identical(pag$amount_pago, provs$amount_pago[1]),
   "G: the paid one is untouched")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
