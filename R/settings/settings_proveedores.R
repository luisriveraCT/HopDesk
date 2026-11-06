# =============================================================================
# R/settings/settings_proveedores.R
# The old Ajustes editor of the supplier catalog: gone (T23).
# =============================================================================
# (Originally part of R/settings_module.R; split in Stage 7.1 for session efficiency.)
#
# The catalog lives in proveedoresUI("prov_mod") (R/proveedores_module.R, on
# the table engine since T07). Nothing drew this file's DT table
# (.catalogo_panel_ui() had no caller), its form or its counter any more, so
# its observers only answered crafted inputs -- some of them writing the
# catalog, where the PPL's CLABEs live. They are gone; the three functions
# app.R still calls stay as no-ops until the integrator takes the calls out
# (app.R:3126-3128).

settings_catalogo_edit_observer <- function(input, output, session, shared) invisible(NULL)

settings_alias_counter <- function(input, output, session) invisible(NULL)

settings_cancel_edit_observer <- function(input, output, session) invisible(NULL)
