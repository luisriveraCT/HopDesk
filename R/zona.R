# =============================================================================
# R/zona.R
# Time zones (CM27, 2026-11-17). The app took "today" from Sys.Date(), the
# day on the SERVER's clock: nothing sets TZ, and on a UTC server, from 18:00
# to 23:59 in Mexico, that is already tomorrow. A business day -- the date of
# a bank payment file, the proposed payment date -- is the client's.
#
#   ZONA_OMISION   the zone while a client has none chosen (CM27 fase 1 adds
#                  the setting): Mexico, where every client is today. The one
#                  place this literal is written.
#   hoy_en(zona, ahora)   the calendar day it is in `zona` at `ahora`.
#
# as.Date(<POSIXct>) is NOT used: in R 4.5 its tz defaults to "UTC" whatever
# TZ says. `ahora` is an argument so a test can fix the clock.
# =============================================================================

ZONA_OMISION <- "America/Mexico_City"

hoy_en <- function(zona = ZONA_OMISION, ahora = Sys.time())
  as.Date(format(ahora, "%Y-%m-%d", tz = zona))
