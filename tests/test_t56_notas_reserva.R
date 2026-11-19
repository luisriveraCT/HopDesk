# =============================================================================
# tests/test_t56_notas_reserva.R
# T56: the app's floating "Notas del mes" (fixed at the bottom-left of every
# tab, R/ui_components.R) covered the last row of the Calendar and the end of
# the Agenda's panels. Every scrolling box that reaches the window's bottom
# now ends with room for the button (padding-bottom: var(--hd-notas-reserva)),
# so scrolled to the end nothing stays under it. Static; the browser check is
# tests/test_t56_notas_chromote.R.
#
# Run from project root:  Rscript tests/test_t56_notas_reserva.R
# =============================================================================
cat("=== test_t56_notas_reserva ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
leer <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

ui  <- leer(Sys.getenv("T56_UI", "R/ui_components.R"))
css <- gsub("(?s)/\\*.*?\\*/", "", ui, perl = TRUE)          # CSS comments out
reglas <- regmatches(css, gregexpr("[^{}]+\\{[^{}]*\\}", css))[[1]]
sel  <- trimws(sub("\\{.*$", "", reglas))
body <- sub("^[^{]*\\{", "", sub("\\}$", "", reglas))
sel_lista <- function(s) trimws(strsplit(s, ",", fixed = TRUE)[[1]])

ok("the room is one variable, 56px (the engine's HDT_MARGEN_ABAJO)",
   grepl(":root\\s*\\{\\s*--hd-notas-reserva:\\s*56px;\\s*\\}", css))
k <- which(grepl("padding-bottom:\\s*var\\(--hd-notas-reserva\\)", body))
ok("exactly one rule gives that room, and it only sets padding-bottom",
   length(k) == 1L && grepl("^\\s*padding-bottom:\\s*var\\(--hd-notas-reserva\\);\\s*$", body[k]))
cajas <- if (length(k) == 1L) sel_lista(gsub("\\s+", " ", sel[k])) else character(0)
for (s in c(".calendar-container", ".ph-company-panel", ".hd-scroll-pestana",
            ".tab-pane > .overflow-auto:not(:has(.hd-panorama))"))
  ok(sprintf("the rule covers %s", s), s %in% cajas)
ok("Panorama stays out (its Cinta lifts the button, T49)",
   any(grepl(":not\\(:has\\(\\.hd-panorama\\)\\)", cajas)))
ok("the button itself did not move (fixed, bottom 16px, left 16px)",
   grepl("\\.notes-toggle-bar \\{\\s*position: fixed;\\s*bottom: 16px; left: 16px;", ui))
ok("the engine still reserves the same 56 px below a table",
   grepl("var HDT_MARGEN_ABAJO = 56;", leer("www/hdtable.js"), fixed = TRUE))

# The two Grupo tabs carry the class; their inline scroll (T51) is untouched,
# and the class sets no height or overflow that could compete with it.
for (f in c("R/tiers_module.R", "R/empresas_module.R")) {
  x <- leer(f)
  ok(sprintf("%s: its scrolling box has class hd-scroll-pestana, inline scroll kept", f),
     grepl('class = "p-3 hd-scroll-pestana",[^\n]*\n\\s*style = "overflow-y: auto; max-height: calc\\(100vh - var\\(--hd-barra-alto\\) - 17px\\);"', x))
}
ok("no other rule styles .hd-scroll-pestana (nothing competes with the inline scroll)",
   sum(grepl("hd-scroll-pestana", sel, fixed = TRUE)) == 1L)

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
