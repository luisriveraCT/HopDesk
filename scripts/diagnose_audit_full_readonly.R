if (file.exists(".Renviron")) readRenviron(".Renviron")
suppressPackageStartupMessages({ library(dplyr) })
source("R/persistence.R", local = FALSE)
source("R/app_audit.R", local = FALSE)
log <- read_audit_log(client_id = "networks")
cat("Total rows (no filter):", nrow(log), "\n")
if (nrow(log)) {
  cat("ts class:", class(log$ts), "\n")
  cat("min ts:", as.character(min(log$ts, na.rm = TRUE)), "\n")
  cat("max ts:", as.character(max(log$ts, na.rm = TRUE)), "\n")
  cat("rows with ts > 2026-08-24:", sum(log$ts > as.POSIXct("2026-08-24", tz = Sys.timezone()), na.rm = TRUE), "\n\n")
  ordered <- log[order(log$ts), c("ts", "user", "module", "action")]
  print(utils::tail(ordered, 25))
}
