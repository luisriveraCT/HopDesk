# =============================================================================
# wipe_ng_mxn_movimientos.R
# ONE-TIME USE: hard-deletes all bancos_movimientos rows for the Networks Group
# MXN account(s). Run from the R console. Delete this file after running.
# =============================================================================

readRenviron("C:/Users/luisr/Antiguedad_App/.Renviron")
setwd("C:/Users/luisr/Antiguedad_App")
source("R/global.R")
s3_init()

movs <- load_bancos_movimientos(include_deleted = TRUE)
message("Total rows before: ", nrow(movs))

ctas <- load_ctas_cuentas()

target_ctas <- ctas[
  toupper(trimws(ctas[["Empresa"]])) == "NG" &
  toupper(trimws(ctas[["Moneda"]]))  == "MXN",
, drop = FALSE]

message("Matching accounts:")
print(target_ctas[, c("id", "Empresa", "Moneda", "alias"), drop = FALSE])

if (nrow(target_ctas) == 0) stop("No matching account found.")

target_ids <- target_ctas$id
message("Wiping cuenta_ids: ", paste(target_ids, collapse = ", "))

movs_clean <- movs[!movs$cuenta_id %in% target_ids, , drop = FALSE]
message("Rows removed: ", nrow(movs) - nrow(movs_clean))
message("Rows remaining: ", nrow(movs_clean))

save_bancos_movimientos(movs_clean)
message("Done. Re-import the NG MXN TXT file to rebuild clean history.")
