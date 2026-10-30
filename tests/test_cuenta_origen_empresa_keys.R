# =============================================================================
# tests/test_cuenta_origen_empresa_keys.R
# Agenda de Hoy said "Cuenta origen no configurada" for Networks Realtors MXN
# while the account was there, active and flagged PPL (2026-10-30). Accounts
# store the company's initials ("NRS"); the Agenda asks by full name
# ("Networks Realtors") and the helpers translated it with the GLOBAL
# COMPANY_MAP, which every session rewrites when its empresas load -- so it
# can hold another session's map, or an empty one. The helpers now use the
# session's own company map.
#
# Also: the account form's "usar como origen" checkbox rendered the word
# "checked" next to an always-empty box, so saving any edit cleared the flag.
#
# Run from project root:  Rscript tests/test_cuenta_origen_empresa_keys.R
# =============================================================================
cat("=== test_cuenta_origen_empresa_keys ===\n\n")
suppressMessages({ library(dplyr); library(htmltools) })
`%||%` <- function(a, b) if (is.null(a)) b else a

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# Extract the real functions (no S3, no app).
env <- new.env()
exprs <- parse("R/settings/settings_helpers.R")
for (e in exprs) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c(".empresa_keys", "get_cuenta_ppl", "get_cuentas_ppl_data"))
    eval(e, env)
}
env$`%||%` <- `%||%`
env$load_ctas_bancos <- function(...) data.frame(id = "b1", nombre = "BanBajío", clave = "030")
env$.get_cuenta_origen_legacy <- function(...) ""

ctas <- data.frame(
  Empresa = c("NRS", "NRS", "NTS"), Moneda = c("USD", "MXN", "MXN"),
  cuenta = c("111100000402", "133865030201", "222200000201"),
  banco_id = "b1", alias = c("NREBB", "NREBB", "NTSBB"),
  is_ppl_default = c(FALSE, TRUE, FALSE), activa = TRUE, stringsAsFactors = FALSE)
session_map <- c(NRS = "Networks Realtors", NTS = "Networks Trucking Services")
shared <- list(ctas_cuentas = function() ctas, company_map = function() session_map,
               effective_client_id = function() "networks")

for (global in list(c(), c(HDX = "Otra Empresa"))) {
  env$COMPANY_MAP <- global
  etiqueta <- if (length(global)) "another session's map" else "an empty map"
  d <- env$get_cuentas_ppl_data("Networks Realtors", "MXN", shared)
  ok(sprintf("with %s in COMPANY_MAP, the full name finds the NRS MXN account", etiqueta),
     !is.null(d) && identical(d$cuenta, "133865030201"))
  ok(sprintf("with %s, get_cuenta_ppl returns the PPL account", etiqueta),
     identical(env$get_cuenta_ppl("Networks Realtors", "MXN", shared), "133865030201"))
}
ok("initials still work", identical(env$get_cuentas_ppl_data("NRS", "MXN", shared)$cuenta, "133865030201"))
ok("another company's account is never returned",
   is.null(env$get_cuentas_ppl_data("Networks Realtors", "EUR", shared)))

# PPL-default priority is row-wise now (isTRUE() on the column was always FALSE with 2+ rows)
ctas2 <- rbind(ctas, data.frame(Empresa = "NRS", Moneda = "MXN", cuenta = "999900000201",
                                banco_id = "b1", alias = "OTRA", is_ppl_default = FALSE, activa = TRUE))
ctas2 <- ctas2[c(4, 1:3), ]
shared2 <- modifyList(shared, list(ctas_cuentas = function() ctas2))
ok("the PPL-flagged account wins over an earlier non-flagged one",
   identical(env$get_cuenta_ppl("Networks Realtors", "MXN", shared2), "133865030201"))

# Checkbox: an attribute, not the word "checked"
src  <- readLines("R/settings/settings_cuentas.R", warn = FALSE)
line <- grep('id    = "cta_is_ppl_default"', src, fixed = TRUE)
ok("the form passes checked as an attribute",
   length(line) == 1L && any(grepl('checked = if (default_val) "checked" else NULL', src[line + 0:4], fixed = TRUE)))
on  <- as.character(tags$input(type = "checkbox", id = "x", checked = if (TRUE)  "checked" else NULL))
off <- as.character(tags$input(type = "checkbox", id = "x", checked = if (FALSE) "checked" else NULL))
ok("checked box renders checked=\"checked\" and no stray text",
   grepl('checked="checked"', on) && !grepl(">checked<", on))
ok("unchecked box has no checked attribute", !grepl("checked", off))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
