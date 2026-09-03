# =============================================================================
# scripts/enable_production_mode.R
#
# Run this (source it, or paste its one line) in your R console BEFORE
# launching the app locally (however you normally do -- "Run App", plain
# shiny::runApp(), etc.) whenever you want that session to hit REAL
# PRODUCTION data instead of the safe dev bucket. This is how you trigger
# the app's own existing SAP/ERP refresh the normal way but have it land in
# production -- it does not touch, wrap, or replace any part of that
# mechanism. hopdesk_start.bat does exactly this same thing internally.
#
# Without this, any local run automatically uses the dev bucket
# (.Renviron.local) -- see R/global.R's single-instance-lock/dev-override
# comments for why that's the safe default.
#
# Only affects THIS R session/process. Does not persist -- run it again after
# restarting R.
# =============================================================================

Sys.setenv(HOPDESK_PRODUCTION = "1")
message("[ENV] Production mode armed for this R session -- the next app launch will use REAL production data. Restart R (or unset HOPDESK_PRODUCTION) to go back to the safe dev bucket.")
