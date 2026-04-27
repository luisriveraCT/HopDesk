# =============================================================================
# remove_disabled_ctas_cuentas.R
# ONE-TIME USE: hard-deletes all rows where activa == FALSE from ctas_cuentas.
# Intended to clean up the duplicate NG BanBajío account created by a
# double-click on "Guardar cambios". Run from the R console. Delete after use.
# =============================================================================

readRenviron("C:/Users/luisr/Antiguedad_App/.Renviron")
setwd("C:/Users/luisr/Antiguedad_App")
source("R/global.R")
s3_init()

ctas <- load_ctas_cuentas()
message("Total accounts before: ", nrow(ctas))

disabled <- ctas[!isTRUE(ctas$activa) | is.na(ctas$activa), , drop = FALSE]
message("Disabled / NA-activa accounts to remove:")
print(disabled[, c("id", "Empresa", "Moneda", "alias", "activa"), drop = FALSE])

if (nrow(disabled) == 0) {
  message("Nothing to remove. Exiting.")
  quit(status = 0)
}

clean <- ctas[isTRUE(ctas$activa) | (!is.na(ctas$activa) & ctas$activa == TRUE), , drop = FALSE]

# Safer: use vectorised check
clean <- ctas[!is.na(ctas$activa) & ctas$activa == TRUE, , drop = FALSE]

message("Accounts remaining: ", nrow(clean))

save_ctas_cuentas(clean)
message("Done. Disabled/duplicate accounts removed from ctas_cuentas.")
