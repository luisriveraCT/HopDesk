# =============================================================================
# tests/test_company_map_global_guard.R
# Guard for CM01 (2026-10-30). The process-wide COMPANY_MAP was rewritten by
# every session, and production is one process shared by every client, so
# any code reading it could see another client's companies. Every read was
# moved to the session's map (shared$company_map() / company_map_rv()), and
# R/global.R keeps only an always-empty declaration.
#
# This test parses R/ and app.R and fails if code uses COMPANY_MAP anywhere
# else -- as a symbol or as a string (get("COMPANY_MAP"), assign(...)).
# Comments do not count.
#
# Run from project root:  Rscript tests/test_company_map_global_guard.R
# =============================================================================
cat("=== test_company_map_global_guard ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# Allowed uses, by file and exact line text.
allowed <- list(
  # The declaration itself.
  list(file = "R/global.R",
       text = "COMPANY_MAP <- setNames(character(0), character(0))"),
  # Owned by Requisiciones (A2); pending their change to
  # `error = function(e) c()`. Remove this entry once it lands.
  list(file = "R/pagar_hoy_module.R",
       text = "cmap_load <- tryCatch(shared$company_map(), error = function(e) COMPANY_MAP)")
)

files <- c("app.R", list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE))
hits  <- list()
for (f in files) {
  pd <- tryCatch(utils::getParseData(parse(f, keep.source = TRUE)), error = function(e) NULL)
  ok(sprintf("%s parses", f), !is.null(pd))
  if (is.null(pd)) next
  uses <- pd[pd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "STR_CONST") &
             gsub("^[\"']|[\"']$", "", pd$text) == "COMPANY_MAP", , drop = FALSE]
  if (!nrow(uses)) next
  src <- readLines(f, warn = FALSE)
  for (l in unique(uses$line1)) hits[[length(hits) + 1L]] <- list(file = f, line = l, text = trimws(src[l]))
}

is_allowed <- function(h) any(vapply(allowed, function(a)
  identical(a$file, h$file) && identical(a$text, h$text), logical(1)))
bad <- Filter(Negate(is_allowed), hits)
for (h in bad) message(sprintf("  COMPANY_MAP used at %s:%d: %s", h$file, h$line, h$text))
ok("no code reads or writes COMPANY_MAP outside the allowed lines", length(bad) == 0L)
ok("every allowed line is still there (drop stale entries)",
   all(vapply(allowed, function(a) any(vapply(hits, function(h)
     identical(a$file, h$file) && identical(a$text, h$text), logical(1))), logical(1))))

# Same pattern, same observer: the names seen last time are per session now.
app <- readLines("app.R", warn = FALSE)
ok("app.R no longer keeps .prev_company_names in .GlobalEnv",
   !any(grepl(".prev_company_names", app, fixed = TRUE)))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
