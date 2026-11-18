# =============================================================================
# tests/test_zona_horaria.R
# CM27 fase 0 (2026-11-17). The time zone, before it becomes the user's.
#   1  the startup log line (.hopdesk_linea_tz(), R/global.R): the process's
#      zone, its clock and day, and Mexico's day next to them;
#   2  the guardian, in COUNTING mode: per file of R/ and app.R, how many
#        - "America/..." zone literals          (zona)
#        - Sys.Date() calls (the server's day)   (hoy)
#        - as.Date(Sys.time()) calls             (as_date_ahora)
#        - "%H:%M" shown without its zone        (hora)  -- formats with
#          "T%H:%M" (ISO, stored as UTC) do not count
#      It fails when a file goes ABOVE its ceiling (TOPE), and says when one
#      went below, so the ceiling is lowered in the same change. The phases
#      bring them to 0; what stays has its reason in EXCEPCIONES.
# Luis (2026-11-17): every user sees their own browser's zone, and every
# time shown or recorded carries its zone. Banxico's FIX is always Mexico's.
# Run from project root:  Rscript tests/test_zona_horaria.R
# =============================================================================
cat("=== test_zona_horaria ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# ── 1. The [ENV] line ────────────────────────────────────────────────────────
src <- parse("R/global.R", keep.source = FALSE, encoding = "UTF-8")
def <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                identical(e[[2]], as.name(".hopdesk_linea_tz")), as.list(src))
ok("global.R defines .hopdesk_linea_tz() once", length(def) == 1L)
eval(def[[1]])
llama <- Filter(function(e) identical(e, quote(message(.hopdesk_linea_tz()))), as.list(src))
ok("global.R writes it to the startup log", length(llama) == 1L)
# 2026-11-18 02:30 UTC = 2026-11-17 20:30 in Mexico (UTC-6, no DST since 2022).
ahora <- as.POSIXct("2026-11-18 02:30:00", tz = "UTC")
linea <- withr::with_envvar(c(TZ = "UTC"), .hopdesk_linea_tz(ahora))
ok("on a UTC server at 20:30 in Mexico, the line shows both days: 2026-11-18 and Mexico 2026-11-17 20:30",
   grepl("Sys.Date()=2026-11-18", linea, fixed = TRUE) && grepl("Mexico=2026-11-17 20:30", linea, fixed = TRUE) &&
     grepl("ahora=2026-11-18 02:30 UTC", linea, fixed = TRUE) && startsWith(linea, "[ENV] TZ:"))
ok("the line carries no client data (zone, clock and days only)",
   !grepl("CLIENT_ID|S3_BUCKET|networks|hd-admin", linea))

# ── 2. The guardian (counting) ──────────────────────────────────────────────
# Kept on purpose, with their reason:
EXCEPCIONES <- c(
  "R/fx_banxico.R"   = "FX_FIX_TZ: Banxico publishes the FIX on Mexico's clock, whoever the user is",
  "R/global.R"       = ".hopdesk_linea_tz(): the startup log shows Mexico's day next to the server's",
  "R/zona.R"         = "ZONA_OMISION: the one place the default zone is written (CM27-ppl)")
`%||0%` <- function(x, y) ifelse(is.na(x), y, x)
# as.Date(Sys.time(), ...) calls in an expression (as.Date of a POSIXct
# ignores TZ: its tz defaults to "UTC", R 4.5).
as_date_ahora <- function(x) {
  if (!is.call(x)) return(0L)
  aqui <- identical(x[[1]], as.name("as.Date")) && length(x) >= 2L && identical(x[[2]], quote(Sys.time()))
  as.integer(aqui) + sum(vapply(as.list(x)[-1], function(a) if (missing(a)) 0L else as_date_ahora(a), integer(1)))
}
contar <- function(f) {
  ex <- parse(f, keep.source = TRUE, encoding = "UTF-8")
  pd <- utils::getParseData(ex)
  s  <- pd$text[pd$token == "STR_CONST"]
  c(zona = sum(grepl("America/", s, fixed = TRUE)),
    hoy = sum(pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "Sys.Date"),
    as_date_ahora = sum(vapply(as.list(ex), as_date_ahora, integer(1))),
    hora = sum(grepl("%H:%M", s, fixed = TRUE) & !grepl("T%H:%M", s, fixed = TRUE)))
}
archivos <- c(list.files("R", "\\.R$", recursive = TRUE, full.names = TRUE), "app.R")
cuenta <- do.call(rbind, lapply(stats::setNames(archivos, archivos), contar))

# The ceilings, per file, on 4554de3 + this phase (CM27 fase 0).
TOPE <- source("tests/helpers/zona_tope.R", local = TRUE)$value

for (medida in colnames(cuenta)) {
  hoy_m  <- cuenta[, medida]; tope_m <- TOPE[[medida]]
  arriba <- names(hoy_m)[hoy_m > (tope_m[names(hoy_m)] %||0% 0L)]
  ok(sprintf("%s: ningún archivo sube de su tope (total hoy %d, tope %d)", medida, sum(hoy_m), sum(tope_m)),
     { for (a in arriba) message(sprintf("  %s: %d > tope %d", a, hoy_m[[a]], tope_m[a] %||0% 0L)); !length(arriba) })
  abajo <- names(tope_m)[tope_m > (hoy_m[names(tope_m)] %||0% 0L)]
  if (length(abajo))
    cat(sprintf("   (bajó %s en: %s -- baja su tope en tests/helpers/zona_tope.R)\n", medida,
                paste(sprintf("%s %d->%d", abajo, tope_m[abajo], hoy_m[abajo] %||0% 0L), collapse = ", ")))
}
ok("las excepciones siguen teniendo su literal (si no, sácalas de la lista)",
   all(cuenta[intersect(names(EXCEPCIONES), rownames(cuenta)), "zona"] >= 1L))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
