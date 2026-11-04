# =============================================================================
# tests/test_politicas_auto.R
# CM12a (2026-11-04): the auto-apply of payment policies (on every SAP refresh
# and when partner policies load) saved "this session's policy_moves + the new
# documents" over the whole table. With the session copy NULL, empty from a
# failed read, or stale, every policy date applied by hand or by another
# session was lost, and a failed save was swallowed.
# Now the new documents are added on S3's current table through
# modificar_tabla (strict reread): existing rows are never removed or
# rewritten, a failed read writes nothing, and a failure is logged.
#
# Runs the REAL settings_companies_observer in shiny::testServer over the
# in-memory S3; compute_policy_moves is stubbed (one date per invoice).
# POL_ARCHIVO: the settings_companies.R to load (default: this checkout's).
#
# Run from project root:  Rscript tests/test_politicas_auto.R
# =============================================================================
cat("=== test_politicas_auto ===\n\n")
source("tests/helpers/s3_memoria.R")
suppressPackageStartupMessages(library(shiny))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

suppressMessages(source(Sys.getenv("POL_ARCHIVO", "R/settings/settings_companies.R"), local = FALSE))
tr <- function(k, ...) k

.logs <- list()
log_action <- function(user, module, action, description, ...)
  .logs[[length(.logs) + 1L]] <<- list(action = action, description = description)
bump_sync_version <- function(...) invisible(NULL)
compute_policy_moves <- function(invoices_df, partner_policies_df, policy_catalog_df,
                                 holidays_cache = list(), overrides_df = NULL,
                                 ledger = "AP", applied_by = "system", applied_at = NULL)
  tibble::tibble(ledger = ledger, Empresa = invoices_df$Empresa, Moneda = invoices_df$Moneda,
                 Documento = invoices_df$Documento, Parte = invoices_df$Parte,
                 FechaVenc_Politica = as.Date("2026-12-31"), policy_ids = "p1",
                 applied_by = applied_by, applied_at = Sys.time())

k_pm <- .s3_key(S3_KEYS$policy_moves, client_id = "acme")
move <- function(doc, fecha = "2026-11-20", por = "luis")
  .normalize(tibble::tibble(ledger = "AP", Empresa = "NCS", Moneda = "MXN", Documento = doc,
                            Parte = "Dell", FechaVenc_Politica = as.Date(fecha),
                            policy_ids = "manual", applied_by = por,
                            applied_at = as.POSIXct("2026-11-01", tz = "UTC")),
             .schema_policy_moves)
moves <- function(docs, ...) dplyr::bind_rows(lapply(docs, move, ...))
facturas <- function(docs) tibble::tibble(Empresa = "NCS", Moneda = "MXN", Documento = docs, Parte = "Dell")

# Every shared$x the observer touches, as a reactiveVal; ids as functions.
nombres <- unique(regmatches(
  readLines(Sys.getenv("POL_ARCHIVO", "R/settings/settings_companies.R"), warn = FALSE),
  gregexpr("shared\\$[A-Za-z_.]+", readLines(Sys.getenv("POL_ARCHIVO", "R/settings/settings_companies.R"),
                                             warn = FALSE))))
nombres <- unique(sub("^shared\\$", "", unlist(nombres)))

# Runs the observer with `copia` as this session's policy_moves_db, then
# delivers SAP data for `docs`. Returns what the session ended up showing.
correr <- function(copia, docs) {
  s3_memoria_reset(); .logs <<- list()
  shared <- lapply(setNames(nombres, nombres), function(n) reactiveVal(NULL))
  shared$effective_client_id <- function() "acme"
  shared$home_client_id      <- function() "acme"
  shared$current_user_info   <- reactiveVal(list(user = "tester"))
  shared$partner_policies_db(tibble::tibble(Parte = "Dell", is_interco = FALSE))
  shared$policy_catalog_db(tibble::tibble(id = "p1"))
  shared$policy_moves_db(copia)
  list(shared = shared)
}
disparar <- function(ctx, docs, preparar = function() NULL) {
  shared <- ctx$shared
  visto <- NULL
  testServer(function(input, output, session) settings_companies_observer(input, output, session, shared), {
    session$flushReact()   # the observers' init run (ignoreInit) happens here
    preparar()
    shared$sap_data(list(AP = facturas(docs)))
    session$flushReact()
    visto <<- shared$policy_moves_db()
  })
  visto
}

# 1. The session copy is NULL (not loaded yet / failed read) and S3 holds
#    three dates applied by hand. A refresh brings D1..D4.
ctx <- correr(NULL, c("D1", "D2", "D3", "D4"))
visto <- disparar(ctx, c("D1", "D2", "D3", "D4"),
                  function() assign(k_pm, moves(c("D1", "D2", "D3")), envir = .mem))
s3 <- get(k_pm, envir = .mem)
ok("NULL session copy: S3 keeps the 3 hand-applied rows", nrow(s3) == 4L &&
   identical(s3$applied_by[s3$Documento %in% c("D1", "D2", "D3")], rep("luis", 3)))
ok("... their dates are not rewritten",
   all(s3$FechaVenc_Politica[s3$Documento != "D4"] == as.Date("2026-11-20")))
ok("... only D4 is added, by auto", identical(s3$applied_by[s3$Documento == "D4"], "auto"))
ok("... and the session now shows S3's table", !is.null(visto) && nrow(visto) == 4L)

# 2. The session copy is EMPTY (a failed read became a 0-row table): same
ctx <- correr(.schema_policy_moves(), c("D1", "D4"))
visto <- disparar(ctx, c("D1", "D4"),
                  function() assign(k_pm, moves(c("D1", "D2", "D3")), envir = .mem))
s3 <- get(k_pm, envir = .mem)
ok("empty session copy: nothing on S3 is lost", all(c("D1", "D2", "D3", "D4") %in% s3$Documento))
ok("... D1 keeps its hand-applied date",
   identical(s3$FechaVenc_Politica[s3$Documento == "D1"], as.Date("2026-11-20")))

# 3. Two sessions: this one's copy is stale (D1); meanwhile another session
#    added D5 by hand on S3. The auto-apply adds D4 and keeps D5.
ctx <- correr(moves("D1"), c("D1", "D4"))
visto <- disparar(ctx, c("D1", "D4"),
                  function() assign(k_pm, moves(c("D1", "D5")), envir = .mem))
s3 <- get(k_pm, envir = .mem)
ok("two sessions: the other session's D5 survives", "D5" %in% s3$Documento)
ok("two sessions: D4 is added", "D4" %in% s3$Documento && nrow(s3) == 3L)

# 4. The strict reread fails: nothing written, and the failure is logged
ctx <- correr(NULL, c("D1"))
visto <- disparar(ctx, c("D1"), function() {
  assign(k_pm, moves(c("D2", "D3")), envir = .mem)
  .mem_falla <<- k_pm
})
ok("failed read: nothing written", .mem_escrituras == 0L &&
   identical(get(k_pm, envir = .mem)$Documento, c("D2", "D3")))
ok("failed read: logged as auto_aplicar_politicas_fallido",
   "auto_aplicar_politicas_fallido" %in% vapply(.logs, `[[`, "", "action"))

# 5. Nothing new against the session copy: no write
ctx <- correr(moves(c("D1", "D2")), c("D1", "D2"))
visto <- disparar(ctx, c("D1", "D2"),
                  function() assign(k_pm, moves(c("D1", "D2")), envir = .mem))
ok("nothing new: zero writes", .mem_escrituras == 0L)

# 6. Pure: .policy_moves_sumar never drops or rewrites a row
f <- moves(c("D1", "D2")); n <- moves(c("D2", "D3", "D3"), fecha = "2027-01-01", por = "auto")
ok("sumar: keeps D1, D2 as they were, adds D3 once", {
  r <- .policy_moves_sumar(f, n)
  identical(r$Documento, c("D1", "D2", "D3")) && identical(r[1:2, ], f)
})

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
