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

# ── 2. The calendar's currency pill: one helper, its tint by color-mix ──────
# .currency_color() and its palette are taken from R/bancos_module.R itself
# (only those two definitions, evaluated alone).
b <- new.env(); b$`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
for (ex in parse("R/bancos_module.R", keep.source = FALSE))
  if (is.call(ex) && identical(ex[[1]], as.name("<-")) && deparse(ex[[2]]) %in% c(".CURRENCY_COLORS", ".currency_color"))
    eval(ex, b)
ok("2: the currency helper is in R/bancos_module.R", is.function(b$.currency_color))
g <- paste(leer("R/global.R"), collapse = "\n")
cuerpo <- regmatches(g, regexpr("calendar_html <- function\\([\\s\\S]*?\\n\\}\\n", g, perl = TRUE))
ok("2: calendar_html() takes the colour from .currency_color() alone (no second fallback of its own)",
   length(cuerpo) == 1L && grepl("cur_col  <- .currency_color(cur)", cuerpo, fixed = TRUE) &&
     !grepl("#[0-9A-Fa-f]{6}", cuerpo) && !grepl("rgba(", cuerpo, fixed = TRUE))
tinte <- function(cur) sprintf("color-mix(in srgb, %s 13%%, transparent)", b$.currency_color(cur))
ok("2: the tint is color-mix at 13 % over transparent (the same colour at alpha 0.13 as before)",
   grepl('cur_tint <- sprintf("color-mix(in srgb, %s 13%%, transparent)", cur_col)', cuerpo, fixed = TRUE))
ok("2: MXN, USD and a currency with no colour (the grey fallback) keep today's colours",
   identical(b$.currency_color("MXN"), "#C0674A") && identical(b$.currency_color(" usd "), "#185FA5") &&
     identical(b$.currency_color("XYZ"), "#5F5E5A") && identical(b$.currency_color(NULL), "#5F5E5A") &&
     identical(tinte("MXN"), "color-mix(in srgb, #C0674A 13%, transparent)") &&
     identical(tinte("XYZ"), "color-mix(in srgb, #5F5E5A 13%, transparent)"))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
