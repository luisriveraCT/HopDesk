# =============================================================================
# tests/test_pasivos_liabilities_seguro.R
# T22: pasivos_liabilities saved safely. On the in-memory S3 of
# tests/helpers/s3_memoria.R (never a real bucket).
#   A  load_pasivos_liabilities() keeps created_at / updated_at (they came back
#      NA, and each save of the session's copy wrote NA for every liability)
# Run from project root:  Rscript tests/test_pasivos_liabilities_seguro.R
# =============================================================================
suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid)
})
source("tests/helpers/s3_memoria.R")
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
if (!exists("CURRENCIES")) CURRENCIES <- c("MXN", "USD", "EUR")
for (f in c("R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R",
            "R/pasivos_capabilities.R", "R/pasivos_audit.R", "R/forecasting_service.R",
            "R/policy_engine.R", "R/pasivos_engine.R", "R/pasivos_wizard_validate.R",
            "R/pasivos_wizard_module.R", "R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R",
            "R/hd_columnas.R", "R/hd_table.R", "R/pasivos_list_module.R"))
  suppressMessages(source(f))

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}
callado <- function(expr) invisible(utils::capture.output(suppressMessages(expr)))

CID <- "acme"
CREADO   <- as.POSIXct("2026-08-01 10:00:00")
CAMBIADO <- as.POSIXct("2026-09-01 12:30:00")
pasivo <- function(id, nombre, estado = "active") {
  l <- pasivos_wizard_draft_to_liability(list(
    categoria = "regular", nombre = nombre, empresa = "NG", parte = paste("Parte", id),
    moneda_pago = "MXN", recurrence_type = "monthly_day", recurrence_params = list(day = 15L),
    fecha_inicio = as.Date("2026-08-01"), amount_default = 100), mode = "create", user = "ana")
  l$id <- id; l$estado <- estado; l$created_at <- CREADO; l$updated_at <- CAMBIADO; l
}
LIABS0 <- dplyr::bind_rows(pasivo("L1", "Renta"), pasivo("L2", "Licencias"), pasivo("L3", "Seguro", "paused"))
sembrar <- function(liabs = LIABS0) { s3_memoria_reset(); callado(save_pasivos_liabilities(liabs, client_id = CID)) }
en_s3 <- function() get(paste0(CID, "/", S3_KEYS$pasivos_liabilities), envir = .mem)   # raw, as stored

# ── A: the loader keeps the timestamps ────────────────────────────────────────
sembrar()
l <- load_pasivos_liabilities(client_id = CID)
ok(inherits(l$created_at, "POSIXct") && all(as.numeric(l$created_at) == as.numeric(CREADO)) &&
     all(as.numeric(l$updated_at) == as.numeric(CAMBIADO)),
   "A1: load_pasivos_liabilities() keeps created_at and updated_at")
callado(save_pasivos_liabilities(l, client_id = CID))
ok(all(as.numeric(en_s3()$created_at) == as.numeric(CREADO)),
   "A2: saving the loaded copy writes them back as they were (no NA on S3)")
ok(identical(l$fecha_inicio, rep(as.Date("2026-08-01"), 3L)) && is.list(l$recurrence_params),
   "A3: Date and list columns load as before")

# A4: a row whose timestamp is NA stays NA; the others keep theirs.
x <- LIABS0; x$updated_at[2] <- NA
sembrar(x); l <- load_pasivos_liabilities(client_id = CID)
ok(is.na(l$updated_at[2]) && all(as.numeric(l$updated_at[-2]) == as.numeric(CAMBIADO)),
   "A4: an NA timestamp stays NA, the others keep theirs")

# A5: an old table without the timestamp columns, and one where they are all NA
# (logical, as the bug wrote them), load as POSIXct NA.
x <- LIABS0; x$created_at <- NULL; x$updated_at <- NA
sembrar(x); l <- load_pasivos_liabilities(client_id = CID)
ok(inherits(l$created_at, "POSIXct") && all(is.na(l$created_at)) &&
     inherits(l$updated_at, "POSIXct") && all(is.na(l$updated_at)),
   "A5: missing or all-NA timestamp columns load as POSIXct NA")

# A6: a timestamp stored as text loads as that time.
x <- LIABS0; x$created_at <- format(CREADO, "%Y-%m-%d %H:%M:%S")
sembrar(x); l <- load_pasivos_liabilities(client_id = CID)
ok(all(as.numeric(l$created_at) == as.numeric(CREADO)), "A6: a timestamp stored as text loads as that time")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
