# =============================================================================
# tests/test_t116_festivos.R
# T116 fase 1: get_holidays() = the banks' calendar.
#   1  MX 2026: exactly the CNBV's non-working days (2 nov and 12 dic were
#      missing)
#   2  1 oct, transmisión del Poder Ejecutivo: every six years from 2024
#   3  US, the Federal Reserve's rule: Saturday is NOT moved to Friday;
#      Sunday -> Monday
#   4  a skip_holidays policy now moves a 2 nov 2027 due date
#   5  guardian: a policy date ALREADY saved is not rewritten by the
#      auto-apply when the calendar changes (.policy_moves_sumar)
#
# Counter-test: T116_MOTOR=<old policy_engine.R> makes 1-4 fail.
# Run from project root:  Rscript tests/test_t116_festivos.R
# =============================================================================
cat("=== test_t116_festivos ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr, detalle = NULL) {
  r <- tryCatch(isTRUE(expr), error = function(e) { message("  error: ", conditionMessage(e)); FALSE })
  if (r) { .pass <<- .pass + 1L; cat(" PASS:", label, "\n") }
  else   { .fail <<- .fail + 1L; cat(" FAIL:", label, "\n"); if (!is.null(detalle)) cat("       ", detalle, "\n") }
}
suppressPackageStartupMessages({ library(dplyr); library(shiny) })
`%||%` <- function(a, b) if (is.null(a)) b else a
source(Sys.getenv("T116_MOTOR", "R/policy_engine.R"))
suppressMessages(source("R/settings/settings_companies.R", local = FALSE))
for (e in parse("R/data_pipeline.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c(".politica_doc_unica", ".politica_doc"))
    eval(e, globalenv())
D <- function(...) as.Date(c(...))

cat("== 1. MX 2026 = calendario CNBV ==\n")
cnbv26 <- D("2026-01-01", "2026-02-02", "2026-03-16", "2026-04-02", "2026-04-03", "2026-05-01",
            "2026-09-16", "2026-11-02", "2026-11-16", "2026-12-12", "2026-12-25")
mx26 <- get_holidays("MX", 2026)
ok("1.1 los 11 días inhábiles de 2026, ni uno más", identical(sort(mx26), cnbv26),
   paste("sobran:", paste(format(mx26[!mx26 %in% cnbv26]), collapse = " "),
         "| faltan:", paste(format(cnbv26[!cnbv26 %in% mx26]), collapse = " ")))
ok("1.2 2 nov y 12 dic en 2025 y 2027 también",
   all(D("2025-11-02", "2025-12-12", "2027-11-02", "2027-12-12") %in% get_holidays("MX", 2025:2027)))

cat("== 2. 1 oct, cambio de Ejecutivo ==\n")
mx_oct <- function(y) as.Date(sprintf("%d-10-01", y)) %in% get_holidays("MX", y)
ok("2.1 2024, 2030 y 2036 sí", mx_oct(2024) && mx_oct(2030) && mx_oct(2036))
ok("2.2 2025, 2026, 2027 y 2018 no (antes de 2024 era 1 dic)", !mx_oct(2025) && !mx_oct(2026) && !mx_oct(2027) && !mx_oct(2018))

cat("== 3. US, regla de la Fed ==\n")
us <- get_holidays("US", 2022:2027)
ok("3.1 4 jul 2026 cae en sábado: el viernes 3 la Fed abre", !D("2026-07-03") %in% us)
ok("3.2 Juneteenth 2027 cae en sábado: el viernes 18 la Fed abre", !D("2027-06-18") %in% us)
ok("3.3 en domingo se observa el lunes (25 dic 2022 -> 26; 1 ene 2023 -> 2)", all(D("2022-12-26", "2023-01-02") %in% us))
ok("3.4 entre semana, el mismo día (Thanksgiving 2026, 4 jul 2025)", all(D("2026-11-26", "2025-07-04") %in% us))

cat("== 4. Una política de festivos ==\n")
p_sk <- list(type = "skip_holidays", params = list(country = "MX"), roll_direction = "forward")
hc <- list(MX = get_holidays("MX", 2027))
ok("4.1 un vencimiento el martes 2 nov 2027 se recorre al 3", identical(compose_policies(D("2027-11-02"), list(p_sk), hc), D("2027-11-03")))
ok("4.2 el lunes 13 dic 2027 no se mueve (el 12 es domingo)", identical(compose_policies(D("2027-12-13"), list(p_sk), hc), D("2027-12-13")))

cat("== 5. Lo guardado no se reescribe ==\n")
fact <- tibble::tibble(Empresa = "NTS", Moneda = "MXN", Documento = "9001", Parte = "PROVEEDOR FICTICIO",
                       FechaVenc_Original = D("2027-11-02"), fecha_factura = D("2027-10-03"))
pp  <- tibble::tibble(parte = "PROVEEDOR FICTICIO", policy_id = "pol-1", policy_order = 1L, ledger = "AP", is_interco = FALSE)
cat_df <- tibble::tibble(id = "pol-1", type = "skip_holidays", params = list(list(country = "MX")), roll_direction = "forward")
nuevo <- compute_policy_moves(fact, pp, cat_df, ledger = "AP", applied_by = "auto")
ok("5.1 calculada hoy: 3 nov 2027", identical(nuevo$FechaVenc_Politica, D("2027-11-03")))
guardada <- nuevo; guardada$FechaVenc_Politica <- D("2027-11-02"); guardada$applied_by <- "auto"
r <- .policy_moves_sumar(guardada, nuevo, fact |> dplyr::mutate(ledger = "AP"))
ok("5.2 la fila guardada antes (2 nov) se queda: una sola fila y su fecha",
   nrow(r) == 1L && identical(r$FechaVenc_Politica, D("2027-11-02")))

cat("== 6. festivos_cache: el tipo que espera compose_policies ==\n")
ov <- tibble::tibble(id = "o1", country = "MX", date = D("2027-11-02"), action = "remove",
                     description = "ajuste ficticio", created_by = "staff.ficticio", created_at = Sys.time())
hc6 <- festivos_cache(list(p_sk), ov, years = 2027)
ok("6.1 lista país -> fechas, con el ajuste manual aplicado",
   identical(names(hc6), "MX") && inherits(hc6$MX, "Date") && !D("2027-11-02") %in% hc6$MX && D("2027-12-25") %in% hc6$MX)
ok("6.2 compose_policies respeta el ajuste con la lista; con la tabla de ajustes como caché (lo de antes) lo ignoraba",
   identical(compose_policies(D("2027-11-02"), list(p_sk), hc6), D("2027-11-02")) &&
     identical(suppressWarnings(compose_policies(D("2027-11-02"), list(p_sk), ov)), D("2027-11-03")))
ok("6.3 sin políticas, sin países: lista vacía", identical(festivos_cache(list(), ov), stats::setNames(list(), character())))
asis <- paste(readLines(Sys.getenv("T116_ASISTENTE", "R/pasivos_wizard_module.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
# T120: the wizard passes the liability's own policies (pasivos_politicas_de()),
# whose holiday cache is festivos_cache() of those same policies + overrides.
motor <- paste(readLines("R/pasivos_engine.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("6.4 el asistente de Pasivos arma la caché con festivos_cache() de las mismas políticas que pasa",
   grepl("policies_for_liability = pasivos_politicas_de(liab_row,", asis, fixed = TRUE) &&
     grepl("structure(pols, festivos = festivos_cache(pols, overrides))", motor, fixed = TRUE) &&
     !grepl("holidays <- tryCatch(shared$holiday_overrides_db()", asis, fixed = TRUE))

cat("== 7. La ventana del día y la vista previa usan los ajustes manuales ==\n")
suppressMessages(source("R/i18n.R"))
DIA <- Sys.getenv("T116_DIA", "R/dia_module.R")
for (e in parse(DIA, keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("dia_aviso_politicas"))) eval(e, globalenv())
pp7  <- data.frame(parte = "ACEROS FICTICIOS", is_interco = FALSE, policy_order = 1L, policy_id = "FES", stringsAsFactors = FALSE)
cat7 <- data.frame(id = "FES", type = "skip_holidays", roll_direction = "forward", stringsAsFactors = FALSE)
cat7$params <- list(list(country = "MX"))
sin_aj <- dia_aviso_politicas(D("2027-11-02"), "ACEROS FICTICIOS", pp7, cat7)
ok("7.1 sin ajustes: el 2 nov 2027 rompe la política, que lo pone el 03/11/2027",
   length(sin_aj) == 1L && grepl("03/11/2027", sin_aj, fixed = TRUE))
ok("7.2 con el ajuste que quita el 2 nov: no hay nada que avisar (la fecha que guarda la política)",
   is.null(dia_aviso_politicas(D("2027-11-02"), "ACEROS FICTICIOS", pp7, cat7, overrides = ov)))
dia_src <- paste(readLines(DIA, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("7.3 la ventana del día le pasa holiday_overrides_db() al aviso",
   grepl("tryCatch\\(shared\\$policy_catalog_db\\(\\), error = function\\(e\\) NULL\\),\\s*tryCatch\\(shared\\$holiday_overrides_db\\(\\)",
         dia_src, perl = TRUE))
pol_src <- paste(readLines(Sys.getenv("T116_POLITICAS", "R/settings/settings_policies.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("7.4 la vista previa de una política arma su caché con festivos_cache() y los ajustes",
   grepl("festivos_cache(list(list(params = params)),", pol_src, fixed = TRUE) &&
     grepl("tryCatch(shared$holiday_overrides_db(), error = function(e) NULL))", pol_src, fixed = TRUE) &&
     !grepl("get_holidays(ctry), error", pol_src, fixed = TRUE))

cat("== 8. Los años de la caché salen de las facturas ==\n")
calc8 <- function(venc, pols = "pol-1", catalogo = cat_df) {
  f <- tibble::tibble(Empresa = "NTS", Moneda = "MXN", Documento = paste0("D", seq_along(venc)), Parte = "PROVEEDOR FICTICIO",
                      FechaVenc_Original = as.Date(venc), fecha_factura = as.Date(venc) - 30)
  p <- tibble::tibble(parte = "PROVEEDOR FICTICIO", policy_id = pols, policy_order = seq_along(pols), ledger = "AP", is_interco = FALSE)
  r <- compute_policy_moves(f, p, catalogo, ledger = "AP", applied_by = "auto")
  r$FechaVenc_Politica[match(f$Documento, r$Documento)]
}
ok("8.1 una factura que vence el lunes 1 may 2028 se recorre al 2 (el año no es el actual ±1)",
   identical(calc8("2028-05-01"), D("2028-05-02")))
cat_off <- dplyr::bind_rows(cat_df, tibble::tibble(id = "pol-0", type = "offset_days",
  params = list(list(n = 1L, unit = "naturales")), roll_direction = "forward"))
ok("8.2 un recorrido que cruza el año: vence 31 dic 2029 +1 día = 1 ene 2030 (festivo) -> 2 ene",
   identical(calc8("2029-12-31", c("pol-0", "pol-1"), cat_off), D("2030-01-02")))
ok("8.3 fechas mal capturadas (1900, 9999, NA) no rompen nada ni inflan la caché",
   identical(.festivos_anios(D("1900-05-01", "9999-12-31", NA, "2028-05-01")), 2027:2029) &&
     is.null(.festivos_anios(D("1900-05-01", NA))) &&
     identical(calc8(c("1900-05-01", "9999-12-31", "2028-05-01"))[3], D("2028-05-02")))

cat("== 9. El país de la política, normalizado como la caché ==\n")
ov9 <- tibble::tibble(id = "o9", country = "MX", date = D("2027-09-16"), action = "remove",
                      description = "ajuste ficticio", created_by = "staff.ficticio", created_at = Sys.time())
cat_mx <- tibble::tibble(id = c("pol-a", "pol-b"), type = "skip_holidays",
                         params = list(list(country = "mx "), list(country = "MX")), roll_direction = "forward")
f9 <- tibble::tibble(Empresa = "NTS", Moneda = "MXN", Documento = c("A", "B"), Parte = c("PARTE A", "PARTE B"),
                     FechaVenc_Original = D("2028-11-02", "2027-09-16"), fecha_factura = D("2028-10-01", "2027-08-16"))
pp9 <- tibble::tibble(parte = c("PARTE A", "PARTE B"), policy_id = c("pol-a", "pol-a"), policy_order = 1L,
                      ledger = "AP", is_interco = FALSE)
r9 <- compute_policy_moves(f9, pp9, cat_mx, overrides_df = ov9, ledger = "AP", applied_by = "auto")
r9 <- r9$FechaVenc_Politica[match(c("A", "B"), r9$Documento)]
ok("9.1 «mx »: una factura de 2028 encuentra los festivos de 2028 (2 nov 2028 -> 3)", identical(r9[1], D("2028-11-03")))
ok("9.2 «mx »: respeta el ajuste manual (16 sep 2027 quitado: no se mueve; sin el ajuste iría al 17)", identical(r9[2], D("2027-09-16")))
ok("9.3 un país vacío o NA no rompe: sin festivos",
   identical(.get_policy_holidays(list(params = list(country = NA)), list()), as.Date(character())) &&
     identical(.get_policy_holidays(list(params = list(country = "  ")), list()), as.Date(character())))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
