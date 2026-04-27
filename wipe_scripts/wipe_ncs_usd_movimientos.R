# =============================================================================
# wipe_ncs_usd_movimientos.R
# ONE-TIME USE: hard-deletes all bancos_movimientos rows for the NCS USD
# BanBajío account. Run from the R console. Delete this file after running.
# =============================================================================

readRenviron("C:/Users/luisr/Antiguedad_App/.Renviron")
source("C:/Users/luisr/Antiguedad_App/R/persistence.R")
s3_init()

movs <- load_bancos_movimientos(include_deleted = TRUE)
message("Total rows before: ", nrow(movs))

ctas <- load_ctas_cuentas()

ncs_usd <- ctas[
  toupper(trimws(ctas[["Empresa"]])) == "NCS" &
  toupper(trimws(ctas[["Moneda"]]))  == "USD",
, drop = FALSE]

message("Matching accounts:")
print(ncs_usd[, c("id", "Empresa", "Moneda", "alias"), drop = FALSE])

if (nrow(ncs_usd) == 0) stop("No matching account found.")

target_ids <- ncs_usd$id   # ctas uses 'id', movs links via 'cuenta_id'
message("Wiping cuenta_ids: ", paste(target_ids, collapse = ", "))

movs_clean <- movs[!movs$cuenta_id %in% target_ids, , drop = FALSE]
message("Rows removed: ", nrow(movs) - nrow(movs_clean))
message("Rows remaining: ", nrow(movs_clean))

save_bancos_movimientos(movs_clean)
message("Done. Re-import the NCS USD TXT file to rebuild clean history.")
