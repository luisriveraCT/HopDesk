# =============================================================================
# tests/test_stage9_touched_files_parse.R
# Stage 9 (2026-08-03): a real incident during this stage -- Issue B's fix to
# R/ui_components.R added an explanatory JS comment (inside a
# tags$script(HTML("...")) block, itself an R double-quoted string) that
# contained literal double quotes ('".modal fade in"'). Those quotes
# prematurely closed the enclosing R string, corrupting the rest of the
# file's parse -- syntactically valid-looking R that nonetheless broke the
# whole app the moment anyone ran shiny::runApp(). None of this stage's own
# chromote tests caught it: they extract just the specific JS/R block under
# test into an isolated fixture, which happens to work in isolation even
# when the SURROUNDING file-level string context is broken, since the
# fixture never re-creates that surrounding context. Caught only because
# Mouse happened to run the app directly mid-session.
#
# This is now a permanent, cheap regression guard: every file this stage
# touched must still be valid, parseable R. This would have caught that
# incident instantly (parse() throws on the corrupted file) without needing
# to run the app at all.
# =============================================================================
cat("=== test_stage9_touched_files_parse ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) {
    cat(" PASS:", label, "\n"); .pass <<- .pass + 1L
  } else {
    cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L
  }
}

STAGE9_TOUCHED_FILES <- c(
  "app.R",
  "R/ui_components.R",
  "R/ledger_module.R",
  "R/bancos_module.R",
  "R/bancos_persistence.R",
  "R/sync_bus.R"
)

for (f in STAGE9_TOUCHED_FILES) {
  ok(sprintf("%s exists", f), file.exists(f))
  ok(sprintf("%s is valid, parseable R (guards against the Issue B quote-escaping incident recurring silently)", f),
     inherits(tryCatch(parse(f, keep.source = FALSE), error = function(e) e), "expression"))
}

# The specific incident, guarded directly: no R double-quoted string inside
# app_scripts()'s embedded <script> block should ever contain a literal,
# unescaped double quote in a JS-style /* ... */ comment again.
{
  txt <- readLines("R/ui_components.R", warn = FALSE)
  script_start <- grep("^app_scripts <- function\\(\\) \\{$", txt)
  ok("found app_scripts() to scan", length(script_start) > 0)
  if (length(script_start)) {
    fn_end <- grep("^\\}$", txt); fn_end <- fn_end[fn_end > script_start[1]][1]
    block <- txt[script_start[1]:fn_end]
    # The whole function body is one big tags$script(HTML("...")) -- an R
    # double-quoted string. A stray, un-escaped " anywhere inside a /* */
    # JS comment line would reproduce this exact incident.
    comment_lines <- grep("^\\s*(/\\*|\\*)", block, value = TRUE)
    # A backslash-escaped quote (\") is fine -- it's a literal " INSIDE the
    # enclosing R string, not a terminator. Only an UNESCAPED " is the real
    # incident shape (confirmed against the pre-existing, correctly-escaped
    # \"tooltip\"/\"dropdown\" comment lines already in this file).
    unescaped_quote_lines <- comment_lines[grepl('(?<!\\\\)"', comment_lines, perl = TRUE)]
    ok("no JS comment line inside app_scripts()'s HTML(\"...\") block contains an UNESCAPED literal double quote",
       length(unescaped_quote_lines) == 0)
  }
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0) stop("Tests FAILED.")
invisible(NULL)
