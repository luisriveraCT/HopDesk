# =============================================================================
# tests/test_t120_politicas_pasivos.R
# T120: payment policies for Pasivos' provisions (Luis; Coordinador's design).
#   A. pasivos_politicas_de(): the liability's own policies, in its order,
#      from the catalog, with their holidays (and the staff's overrides).
#   B. The generator: the payment date moves (MX holiday, weekend), the
#      calculated date stays, policy_ids says which; the interest of a loan
#      is the same with or without policies (T115: days between calculated
#      dates).
#   C. The daily refresh re-places ONLY provisional, future, non-overridden
#      provisions of liabilities with policies (or that a policy moved):
#      a new holiday, an edited or a removed policy. Converted, hand-edited,
#      past and never-moved ones stay; a second run changes nothing; a
#      failed read of the catalog stops it (never "no policies").
#   D. The guardian: every pasivos_generate_provisions() call in R/ takes its
#      policies from pasivos_politicas_de().
# Run from project root:  Rscript --vanilla tests/test_t120_politicas_pasivos.R
# =============================================================================
cat("=== test_t120_politicas_pasivos ===\n\n")
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
HOY <- as.Date("2026-11-26")
hoy_cliente <- function(...) HOY
forecasting_get_estimate <- function(...) NA_real_
has_capability <- function(...) TRUE
bump_sync_version <- function(...) invisible(NULL)
pasivos_log_audit <- function(...) invisible(NULL)

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}
d <- function(x) as.Date(x)

catalogo <- tibble::tibble(
  id = c("p_hab", "p_fest", "p_fin"), name = c("Hábiles", "Festivos MX", "Fin de mes"),
  type = c("weekdays", "skip_holidays", "last_day"),
  params = list(list(days = 1:5), list(country = "MX"), list()),
  roll_direction = c("forward", "forward", "backward"),
  created_by = "x", created_at = as.POSIXct(NA), updated_at = as.POSIXct(NA))
renta <- function(politicas = NA_character_) {
  l <- pasivos_wizard_draft_to_liability(list(
    categoria = "regular", nombre = "Renta", empresa = "NTS", parte = "Inmobiliaria", moneda_pago = "MXN",
    recurrence_type = "monthly_day", recurrence_params = list(day = 1L), fecha_inicio = d("2027-01-01"),
    amount_default = 100), mode = "create", user = "t")
  l$policy_ids <- politicas
  l
}

cat("== A. pasivos_politicas_de ==\n")
l <- renta("p_fest,p_hab,no_existe")
pp <- pasivos_politicas_de(l, catalogo)
ok(identical(vapply(pp, `[[`, "", "id"), c("p_fest", "p_hab")), "A.1 the liability's policies in its order; an id the catalog lacks is left out")
ok(d("2027-01-01") %in% attr(pp, "festivos")[["MX"]], "A.2 the MX holidays travel with them")
ok(length(pasivos_politicas_de(renta(), catalogo)) == 0L && length(pasivos_politicas_de(l, NULL)) == 0L,
   "A.3 no policies, or no catalog: none")
ov <- data.frame(country = "MX", date = d("2027-03-02"), action = "add", description = "x",
                 created_by = "x", created_at = as.POSIXct(NA), stringsAsFactors = FALSE)
pp_ov <- pasivos_politicas_de(l, catalogo, ov)
ok(d("2027-03-02") %in% attr(pp_ov, "festivos")[["MX"]], "A.4 the staff's holiday overrides count (T116)")

cat("== B. the generator ==\n")
gen <- function(l, pols = list()) {
  g <- pasivos_generate_provisions(l, window_start = HOY, window_end = d("2027-06-30"), client_id = "c",
                                   policies_for_liability = pols)
  g[order(g$fecha_calculada), ]
}
g0 <- gen(renta()); g1 <- gen(l, pp)
f <- function(g, x) as.Date(g$fecha_efectiva[as.Date(g$fecha_calculada) == d(x)])
ok(identical(f(g0, "2027-01-01"), d("2027-01-01")) && identical(f(g1, "2027-01-01"), d("2027-01-04")),
   "B.1 1 Jan (holiday, Friday) -> Monday 4 Jan; without policies it stays")
ok(identical(f(g1, "2027-02-01"), d("2027-02-02")), "B.2 Constitution Day (first Monday of Feb) -> Tuesday 2 Feb")
ok(identical(f(g1, "2027-05-01"), d("2027-05-03")), "B.3 1 May (holiday and Saturday) -> Monday 3 May")
ok(identical(as.Date(g1$fecha_calculada), as.Date(g0$fecha_calculada)) &&
     all(g1$policy_ids[f(g1, "2027-01-01") == g1$fecha_efectiva] %in% "p_fest,p_hab"),
   "B.4 the calculated dates stay; policy_ids says which policies")
l3 <- renta("p_fest")
ov3 <- data.frame(country = "MX", date = d("2027-03-01"), action = "add", description = "", created_by = "", created_at = as.POSIXct(NA))
ok(identical(f(gen(l3, pasivos_politicas_de(l3, catalogo)), "2027-03-01"), d("2027-03-01")) &&
     identical(f(gen(l3, pasivos_politicas_de(l3, catalogo, ov3)), "2027-03-01"), d("2027-03-02")),
   "B.5 an added holiday (the staff's override) moves its date")
prestamo <- pasivos_wizard_draft_to_liability(list(
  categoria = "financiero", flavor = "credito_simple", nombre = "Préstamo", empresa = "NG", parte = "Banco",
  moneda_pago = "USD", principal_original = 100000, tasa_tipo = "fija", tasa_actual = 8.75, convencion_dias = "act_360",
  plazo_meses = 12L, dia_pago = 1L, frecuencia_pago = "mensual", periodo_gracia = 0L,
  fecha_inicio = d("2026-12-01"), metodo_amortizacion = "francesa"), mode = "create", user = "x")
pr1 <- prestamo; pr1$policy_ids <- "p_fest,p_hab"
a <- pasivos_generate_provisions(prestamo, window_start = HOY, client_id = "c", policies_for_liability = list())
b <- pasivos_generate_provisions(pr1, window_start = HOY, client_id = "c", policies_for_liability = pasivos_politicas_de(pr1, catalogo))
ok(identical(round(a$componente_interes, 6), round(b$componente_interes, 6)) && identical(round(a$amount_pago, 6), round(b$amount_pago, 6)) &&
     any(as.Date(a$fecha_efectiva) != as.Date(b$fecha_efectiva)),
   "B.6 T115: a loan's interest and payments are the same with or without policies; only the payment date moves")

cat("== C. the daily refresh ==\n")
L <- renta("p_fest,p_hab"); L$estado <- "active"
base <- gen(renta()); base$liability_id <- L$id      # provisions generated before T120 (no policy)
base$estado <- "provisional"
i_conv <- which(as.Date(base$fecha_calculada) == d("2027-02-01")); base$estado[i_conv] <- "converted"
i_mano <- which(as.Date(base$fecha_calculada) == d("2027-05-01")); base$fecha_efectiva_override[i_mano] <- d("2027-05-07")
pasado <- base[1, ]; pasado$id <- "pasada"; pasado$fecha_calculada <- d("2026-11-01"); pasado$fecha_efectiva <- d("2026-11-01")
provs <- dplyr::bind_rows(pasado, base)
ctx <- list(catalogo = catalogo, overrides = NULL)
r <- pasivos_refresco_calcular(L, provs, HOY, "c", ctx)
t <- r$tabla; fe <- function(x) as.Date(t$fecha_efectiva[!is.na(t$fecha_calculada) & as.Date(t$fecha_calculada) == d(x)])
ok(identical(fe("2027-01-01"), d("2027-01-04")) && t$policy_ids[as.Date(t$fecha_calculada) == d("2027-01-01")] == "p_fest,p_hab",
   "C.1 a provisional future provision is re-placed by the liability's policies")
ok(identical(fe("2027-02-01"), d("2027-02-01")), "C.2 a converted one never moves")
ok(identical(fe("2027-05-01"), d("2027-05-01")) && identical(as.Date(t$fecha_efectiva_override[as.Date(t$fecha_calculada) == d("2027-05-01")]), d("2027-05-07")),
   "C.3 a hand-edited one (override) never moves")
ok(identical(as.Date(t$fecha_efectiva[t$id == "pasada"]), d("2026-11-01")), "C.4 a past one never moves")
ok(nrow(r$cambios) == 1L && r$cambios$reubicadas > 0L && r$cambios$motivo %in% c("politica", "horizonte"),
   "C.5 one line for the liability, with how many were re-placed")
r2 <- pasivos_refresco_calcular(L, t, HOY, "c", ctx)
ok(!nrow(r2$cambios) && identical(r2$tabla, t), "C.6 a second run changes nothing (nothing to write)")
L0 <- L; L0$policy_ids <- NA_character_
r3 <- pasivos_refresco_calcular(L0, t, HOY, "c", ctx)
ok(identical(as.Date(r3$tabla$fecha_efectiva[as.Date(r3$tabla$fecha_calculada) == d("2027-01-01")]), d("2027-01-01")) &&
     r3$cambios$motivo == "politica",
   "C.7 the policy removed: what a policy moved goes back to its calculated date")
otro <- renta(); otro$estado <- "active"
mov <- gen(renta()); mov$liability_id <- otro$id; mov$estado <- "provisional"
mov$fecha_efectiva[1] <- mov$fecha_efectiva[1] + 3          # moved by something else, no policy, no override
r4 <- pasivos_refresco_calcular(otro, mov, HOY, "c", ctx)
ok(identical(as.Date(r4$tabla$fecha_efectiva[1]), as.Date(mov$fecha_efectiva[1])), "C.8 a provision no policy ever moved is left as it is")
ov2 <- data.frame(country = "MX", date = d("2027-01-04"), action = "add", description = "", created_by = "", created_at = as.POSIXct(NA))
r5 <- pasivos_refresco_calcular(L, t, HOY, "c", list(catalogo = catalogo, overrides = ov2))
ok(identical(as.Date(r5$tabla$fecha_efectiva[as.Date(r5$tabla$fecha_calculada) == d("2027-01-01")]), d("2027-01-05")),
   "C.9 a holiday added later re-places it again")
# strict: a failed read of the catalog stops, never "no policies"
viejo_leer <- .s3_leer_para_escribir
.s3_leer_para_escribir <- function(key, client_id) stop("[S3] could not read ", key)
e <- tryCatch(.pasivos_politicas_contexto("c"), error = function(e) e)
.s3_leer_para_escribir <- viejo_leer
ok(inherits(e, "error"), "C.10 the refresh's catalog read is strict: a failure stops it")

cat("== D. the guardian ==\n")
llamadas <- function(archivos) {
  out <- data.frame(donde = character(), arg = character(), stringsAsFactors = FALSE)
  for (f in archivos) {
    lin <- readLines(f, warn = FALSE, encoding = "UTF-8")
    lin <- lin[!grepl("^[[:space:]]*#", lin)]                       # comment lines out
    x <- paste(lin, collapse = "\n")
    pos <- gregexpr("pasivos_generate_provisions(", x, fixed = TRUE)[[1]]
    if (pos[1] < 0) next
    for (p in pos) {
      ch <- strsplit(substr(x, p + 28L, p + 1500L), "")[[1]]; dd <- 1L
      for (n in seq_along(ch)) { if (ch[n] == "(") dd <- dd + 1L; if (ch[n] == ")") { dd <- dd - 1L; if (!dd) break } }
      arg <- paste(ch[seq_len(n - 1L)], collapse = "")
      if (grepl("window_start = hoy_en()", arg, fixed = TRUE)) next   # the definition
      out <- rbind(out, data.frame(donde = paste0(f, ": ", gsub("[[:space:]]+", " ", substr(arg, 1, 90))),
                                   arg = arg, stringsAsFactors = FALSE))
    }
  }
  out
}
fs <- c(list.files("R", pattern = "[.]R$", full.names = TRUE, recursive = TRUE), "app.R")
ll <- llamadas(fs)
sin <- ll$donde[!grepl("policies_for_liability[[:space:]]*=[[:space:]]*pasivos_politicas_de[(]", ll$arg)]
ok(nrow(ll) == 4L, sprintf("D.1 the four generators are where they were (%d calls)", nrow(ll)))
ok(!length(sin), "D.2 every pasivos_generate_provisions() call takes its policies from pasivos_politicas_de()")
if (length(sin)) cat("      sin políticas:", sin, sep = "\n        ")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
