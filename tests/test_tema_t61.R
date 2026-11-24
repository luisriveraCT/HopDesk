# =============================================================================
# tests/test_tema_t61.R
# TEMA 6 (T6-1): R/global.R and R/tiers_module.R to theme roles.
#   1. CALENDAR_THEME (R/global.R, 14 hand-written colours) was dead code:
#      nothing read it. It is gone and must not come back, neither defined
#      nor read, in R/, app.R, www/ or scripts/.
# Run from project root:  Rscript tests/test_tema_t61.R
# =============================================================================
cat("=== test_tema_t61 ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
leer <- function(f) readLines(f, warn = FALSE, encoding = "UTF-8")
codigo <- c("app.R", list.files(c("R", "scripts"), pattern = "\\.R$", recursive = TRUE, full.names = TRUE),
            list.files("www", pattern = "\\.(js|css|html)$", recursive = TRUE, full.names = TRUE))

# ── 1. CALENDAR_THEME stays gone ─────────────────────────────────────────────
donde <- codigo[vapply(codigo, function(f) any(grepl("CALENDAR_THEME", leer(f), fixed = TRUE)), logical(1))]
ok(sprintf("1: CALENDAR_THEME is neither defined nor read anywhere (%d files looked at%s)",
           length(codigo), if (length(donde)) paste0("; found in ", paste(donde, collapse = ", ")) else ""),
   length(codigo) > 50L && !length(donde))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
