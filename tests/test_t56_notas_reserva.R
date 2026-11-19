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
`%||%` <- function(a, b) if (is.null(a)) b else a
leer <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

ui  <- leer(Sys.getenv("T56_UI", "R/ui_components.R"))
css <- gsub("(?s)/\\*.*?\\*/", "", ui, perl = TRUE)          # CSS comments out
reglas <- regmatches(css, gregexpr("[^{}]+\\{[^{}]*\\}", css))[[1]]
sel  <- trimws(sub("\\{.*$", "", reglas))
body <- sub("^[^{]*\\{", "", sub("\\}$", "", reglas))
sel_lista <- function(s) trimws(strsplit(s, ",", fixed = TRUE)[[1]])

ok("the room is one variable, 56px (the engine's HDT_MARGEN_ABAJO)",
   grepl(":root\\s*\\{\\s*--hd-notas-reserva:\\s*56px;\\s*\\}", css))
k <- which(grepl("var\\(--hd-notas-reserva\\)", body))
# An empty block after the content, never padding: Bancos' and Intercompany's
# boxes carry Bootstrap's p-0 (padding 0 !important), which erased it.
ok("exactly one rule gives that room: an empty block of that height after the content (::after), no padding",
   length(k) == 1L && grepl('^\\s*content:\\s*\\\\"\\\\";\\s*display:\\s*block;\\s*height:\\s*var\\(--hd-notas-reserva\\);\\s*$', body[k]) &&
     !any(grepl("padding-bottom:\\s*var\\(--hd-notas-reserva\\)", body)))
cajas <- if (length(k) == 1L) sel_lista(gsub("\\s+", " ", sel[k])) else character(0)
ok("…every selector of that rule is an ::after", length(cajas) > 0L && all(grepl("::after$", cajas)))
cajas <- sub("::after$", "", cajas)
for (s in c(".ph-company-panel", ".hd-scroll-pestana",
            ".tab-pane > .overflow-auto:not(:has(.pan-cinta))"))
  ok(sprintf("the rule covers %s", s), s %in% cajas)
ok("the Calendar keeps its height and gets no room (its last week already ends clear)",
   !".calendar-container" %in% cajas &&
     grepl("\\.calendar-container \\{[^}]*height: calc\\(100vh - var\\(--hd-barra-alto\\) - 72px\\);\\s*min-height: 500px;", css))

# On the Calendar, "Notas del mes" lives in its header (calendar_html(),
# R/global.R) and the floating one hides -- but not with the Flujo slid in.
OCULTA <- "body:has(.tab-pane.active .cf-slide-wrapper:not(.cf-preview-active)) .notes-toggle-bar"
ok("one rule hides the floating button only on the Calendar without the Flujo preview, and only hides it",
   sum(sel == OCULTA) == 1L && grepl("^\\s*display:\\s*none;\\s*$", body[sel == OCULTA][1]))
ok("no other rule hides the floating button",
   sum(grepl("notes-toggle-bar", sel, fixed = TRUE) & grepl("display:\\s*none", body)) == 1L)
g <- leer("R/global.R")
cal <- regmatches(g, regexpr("(?s)calendar_html <- function.*?\n}\n", g, perl = TRUE))
ok("calendar_html() builds the header button: the floating one's look, a click on the app's one #toggle_notes (guarded)",
   grepl("class=\"notes-bar-btn cal-notes-btn\"", cal, fixed = TRUE) &&
     grepl("onclick=\"var b=document.getElementById(\\'toggle_notes\\'); if(b) b.click();\"", cal, fixed = TRUE))
ok("…its text from the dictionary (comun.calendario.notas), escaped",
   grepl("htmltools::htmlEscape(tr(\"comun.calendario.notas\"))", cal, fixed = TRUE))
ok("…and it sits in the header's left group, after the Flujo arrow",
   grepl("preview_arrow_html,\\s*\n\\s*notes_btn_html,\\s*\n\\s*'</div>',", cal))
for (idioma in c("es", "en")) {
  d <- jsonlite::fromJSON(file.path("www/i18n", paste0(idioma, ".json")))
  ok(sprintf("the key exists in %s.json", idioma), nzchar(d[["comun.calendario.notas"]] %||% ""))
}
ok("the Spanish text is the floating button's: \"Notas del mes\"",
   identical(jsonlite::fromJSON("www/i18n/es.json")[["comun.calendario.notas"]], "Notas del mes"))
ok("only a Panorama showing its Cinta stays out (the Cinta lifts the button, T49); without it Panorama gets the room",
   any(grepl(":not\\(:has\\(\\.pan-cinta\\)\\)", cajas)) && !any(grepl("hd-panorama", cajas, fixed = TRUE)))
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
ok("no other rule styles .hd-scroll-pestana, and that one only its ::after (nothing competes with the inline scroll)",
   sum(grepl("hd-scroll-pestana", sel, fixed = TRUE)) == 1L && ".hd-scroll-pestana::after" %in% sel_lista(gsub("\\s+", " ", sel[k])))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
