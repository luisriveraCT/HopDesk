# =============================================================================
# tests/test_t612_calendario_hoy.R
# T6-12 (Luis): Flujo › Calendario marks TODAY clearly, and today is the
# user's day (CM27, R/zona.R), never the server's Sys.Date(). On a UTC server
# from 18:00 to 23:59 in Mexico, Sys.Date() is already tomorrow.
#   1. calendar_html() with the clock at 23:30 in Mexico (05:30 UTC of the
#      next day): the tile marked today is Mexico's day, and only that one;
#      its number carries title "Hoy" (tr) and aria-current="date".
#   2. The real clock with no session: ZONA_OMISION's day.
#   3. calendar_html()'s source calls no Sys.Date().
# The look (ring and pill, theme tokens, contrast) is checked in
# tests/test_temas_guardian.R.
# Run from project root:  Rscript tests/test_t612_calendario_hoy.R
# =============================================================================
cat("=== test_t612_calendario_hoy ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(lubridate); library(stringr) })
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
for (f in c("R/i18n.R", "R/monedas.R")) suppressMessages(source(f))
# Every function global.R defines (calendar_html needs its helpers), only the definitions.
es_funcion <- function(e) is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
  is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))
for (e in parse("R/global.R", keep.source = FALSE, encoding = "UTF-8")) if (es_funcion(e)) eval(e, globalenv())

dd <- data.frame(Fecha = as.Date("2026-11-20"), Moneda = "MXN", Parte = "ACEROS", Importe = 22000, stringsAsFactors = FALSE)
pintar <- function() suppressMessages(as.character(calendar_html(dd, as.Date("2026-11-01"), "MXN", title_prefix = "Pagos", ledger = "AP")))
# The tile of a day: from its opening div to its number.
marcado_hoy <- function(h) {
  m <- regmatches(h, gregexpr('<div class="cal-tile[^"]*cal-tile--today[^"]*"[\\s\\S]*?<span class="cal-daynum"[^>]*>([0-9]+)</span>', h, perl = TRUE))[[1]]
  as.integer(sub('.*>([0-9]+)</span>$', "\\1", m))
}

# ── 1. The user's day, at 23:30 in Mexico ───────────────────────────────────
source("R/zona.R")
NOCHE <- as.POSIXct("2026-11-11 05:30:00", tz = "UTC")   # 23:30 on the 10th in Mexico City (not the real today: Sys.Date() would mark another day)
ok("the clock used: 05:30 UTC on the 11th is still the 10th in Mexico (the server's UTC day would say 11)",
   hoy_en(ZONA_OMISION, NOCHE) == as.Date("2026-11-10") && as.Date(format(NOCHE, "%Y-%m-%d", tz = "UTC")) == as.Date("2026-11-11"))
hoy_real <- hoy_usuario
hoy_usuario <- function(session = NULL, ahora = NOCHE) hoy_real(session, ahora)
h <- pintar()
ok("calendar_html(): the tile marked today is the 10th (Mexico's day), and only that one",
   identical(marcado_hoy(h), 10L))
ok("…its number says title=\"Hoy\" (comun.calendario.hoy) and aria-current=\"date\"; exactly one day does",
   grepl('<span class="cal-daynum" title="Hoy" aria-current="date">10</span>', h, fixed = TRUE) &&
     lengths(regmatches(h, gregexpr('aria-current="date"', h, fixed = TRUE))) == 1L)
ok("…the other days' numbers carry nothing", grepl('<span class="cal-daynum">11</span>', h, fixed = TRUE))
for (idioma in c("es", "en"))
  ok(sprintf("the key comun.calendario.hoy exists in %s.json", idioma),
     nzchar(jsonlite::fromJSON(file.path("www/i18n", paste0(idioma, ".json")))[["comun.calendario.hoy"]] %||% ""))

# ── 2. The real clock, no session: ZONA_OMISION's day ───────────────────────
hoy_usuario <- hoy_real; rm(hoy_real)
h2 <- pintar()
hoy_z <- hoy_en(ZONA_OMISION)
ok("real clock, no session: today is ZONA_OMISION's day (marked only when it falls in November 2026)",
   if (format(hoy_z, "%Y-%m") == "2026-11") identical(marcado_hoy(h2), as.integer(format(hoy_z, "%d")))
   else length(marcado_hoy(h2)) == 0L)

# ── 3. No server day in calendar_html ───────────────────────────────────────
g  <- readLines("R/global.R", warn = FALSE, encoding = "UTF-8")
ini <- grep("^calendar_html <- function", g); fin <- grep("^[A-Za-z_.][A-Za-z0-9_.]* <- function", g)
cal <- g[ini:(min(c(fin[fin > ini], length(g) + 1L)) - 1L)]
cal <- sub("#.*$", "", cal)
ok("calendar_html()'s code calls no Sys.Date() (the server's day); its today is hoy_usuario()",
   length(ini) == 1L && !any(grepl("Sys.Date()", cal, fixed = TRUE)) && any(grepl("^\\s*today <- hoy_usuario\\(\\)\\s*$", cal)))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
quit(status = if (.fail) 1L else 0L)
