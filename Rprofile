# =============================================================================
# .Rprofile — place in project root (same folder as app.R)
# =============================================================================
#
# Binds Shiny to 127.0.0.1 instead of 0.0.0.0 (all interfaces).
# On Windows, binding to 0.0.0.0 triggers a Windows Defender firewall prompt.
# If that dialog appears in the background or times out, it delays the
# WebSocket session handshake by 60-70 seconds — visible as a long blank
# screen before anything loads.
#
# This must be in .Rprofile (not inside app.R) because shiny::runApp()
# reads shiny.host before sourcing app.R.

local({
  options(
    shiny.host           = "127.0.0.1",
    shiny.launch.browser = TRUE,
    shiny.reactlog       = FALSE
  )
})
