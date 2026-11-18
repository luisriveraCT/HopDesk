# =============================================================================
# tests/test_t49_notas_cinta.R
# T49: the app's floating "Notas del mes" (fixed 16 px from the bottom on
# every tab, R/ui_components.R) fell on La Cinta's name in Panorama. Only
# while the active tab shows a Cinta it sits above it (www/panorama.css); on
# any other tab it stays where it was. Static: the browser check is in
# reportes/T49_LISTO.md.
#
# Run from project root:  Rscript tests/test_t49_notas_cinta.R
# =============================================================================
cat("=== test_t49_notas_cinta ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

css <- paste(readLines(Sys.getenv("T49_CSS", "www/panorama.css"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
css <- gsub("(?s)/\\*.*?\\*/", "", css, perl = TRUE)      # comments out, also multi-line
# Every "selector { body }" pair, flattened (one level of @media is enough here).
reglas <- regmatches(css, gregexpr("[^{}]+\\{[^{}]*\\}", css))[[1]]
sel  <- trimws(sub("\\{.*$", "", reglas))
body <- sub("^[^{]*\\{", "", sub("\\}$", "", reglas))
SEL <- "body:has(.tab-pane.active .hd-panorama .pan-cinta) .notes-toggle-bar"

ok("the Cinta's height is one variable, 44px", grepl(":root\\s*\\{\\s*--pan-cinta-alto:\\s*44px;", css))
ok(".pan-cinta takes its height from that variable",
   any(sel == ".hd-panorama .pan-cinta" & grepl("height:\\s*var\\(--pan-cinta-alto\\)", body)))
lift <- body[sel == SEL]
ok("one rule lifts the button, tied to the ACTIVE tab showing a Cinta, by the same variable",
   length(lift) >= 1L && grepl("bottom:\\s*calc\\(var\\(--pan-cinta-alto\\)\\s*\\+\\s*28px\\)", lift[1]))
ok("with reduced motion (the Cinta wraps and grows) a fixed, higher place",
   length(lift) == 2L && grepl("bottom:\\s*90px", lift[2]) &&
     grepl("@media \\(prefers-reduced-motion: reduce\\)[^@]*body:has\\(\\.tab-pane\\.active", css))
ok("the lift only moves the button: no transition, nothing but bottom",
   all(grepl("^\\s*bottom:[^;]+;\\s*$", lift)))
CARA <- "body:has(.tab-pane.active .hd-panorama .pan-cinta) .notes-bar-btn:not(.notes-open):not(:hover)"
cara <- body[sel == CARA]
ok("…and, only there, an opaque face (over the navy Clima card); open and hover keep theirs",
   length(cara) == 1L && grepl("^\\s*background:\\s*var\\(--bs-body-bg\\);\\s*$", cara))
ok("no other rule of panorama.css reaches outside .hd-panorama",
   all(grepl("^(\\.hd-panorama|:root|from|to|@media)", sel) | sel %in% c(SEL, CARA)))
ui <- paste(readLines("R/ui_components.R", warn = FALSE), collapse = "\n")
ok("everywhere else the button is where it was (ui_components.R: fixed, bottom 16px, left 16px)",
   grepl("\\.notes-toggle-bar \\{\\s*position: fixed;\\s*bottom: 16px; left: 16px;", ui))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
