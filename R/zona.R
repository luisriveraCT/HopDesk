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
#   zona_navegador(session)   this session's browser zone (T75).
#   hoy_usuario(session, ahora)   the day it is for this session's user: the
#                  value a date field starts at (T75).
#   hoy_cliente(shared, ahora)   the client's business day (T75b): what a
#                  calculation shared by every user of the client runs on.
#
# as.Date(<POSIXct>) is NOT used: in R 4.5 its tz defaults to "UTC" whatever
# TZ says. `ahora` is an argument so a test can fix the clock.
# =============================================================================

ZONA_OMISION <- "America/Mexico_City"

hoy_en <- function(zona = ZONA_OMISION, ahora = Sys.time())
  as.Date(format(ahora, "%Y-%m-%d", tz = zona))

# The zone of this session's browser: input$client_tz, which app_scripts()
# (R/ui_components.R) sends on shiny:connected. ZONA_OMISION with no session
# (tests, the email job), before it arrives, or when it is not a real zone --
# Mexico's day, never the server's. isolate(): it arrives before any form is
# drawn, and a whole renderUI form must not redraw (and lose what was typed)
# if it changed. Each user sees their own browser's day (CM27, Luis).
zona_navegador <- function(session = shiny::getDefaultReactiveDomain()) {
  tz <- tryCatch(shiny::isolate(session$rootScope()$input$client_tz), error = function(e) NULL)
  if (is.character(tz) && length(tz) == 1L && !is.na(tz) && tz %in% OlsonNames()) tz else ZONA_OMISION
}

# The day it is for this session's user: what a date field starts at (T75).
hoy_usuario <- function(session = shiny::getDefaultReactiveDomain(), ahora = Sys.time())
  hoy_en(zona_navegador(session), ahora)

# The client's day (T75b, CM27-ppl): shared$zona_cliente()$zona once the
# client setting exists (CM27 fase 1), ZONA_OMISION until then -- never the
# server's. For calculations every user of the client shares (the Pasivos
# engine, like pagar_hoy), not for what one person sees.
hoy_cliente <- function(shared = NULL, ahora = Sys.time()) {
  zona <- tryCatch(shared$zona_cliente()$zona, error = function(e) NULL)
  if (!(is.character(zona) && length(zona) == 1L && !is.na(zona) && zona %in% OlsonNames())) zona <- ZONA_OMISION
  hoy_en(zona, ahora)
}
