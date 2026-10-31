# =============================================================================
# tests/test_cashflow_export_group_brand.R
# The cash-flow export (Word, Excel, PDF) fell back to the fixed name
# "Networks Group" -- as the generators' default group_name, as the error
# fallback of shared$group_name(), and as the file-name prefix -- so a client
# without a group name saw another client's group on its cover and file name.
# Same decision as the financial report (2026-10-30): the session's group
# name, else the session's first company, else the neutral "HopDesk".
#
# Run from project root:  Rscript tests/test_cashflow_export_group_brand.R
# =============================================================================
cat("=== test_cashflow_export_group_brand ===\n\n")
`%||%` <- function(a, b) if (is.null(a)) b else a

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# Extract the real functions (no S3, no app).
env <- new.env()
fns <- c(".cf_group_display", ".add_cover_page", "generate_cashflow_word_doc",
         "generate_cashflow_html_doc", "generate_cashflow_excel_wb")
for (e in parse("R/cashflow_export_module.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% fns)
    eval(e, env)
env$`%||%` <- `%||%`
session_map <- c(NRS = "Networks Realtors", NTS = "Networks Trucking Services")

ok("the session's group name is shown",
   identical(env$.cf_group_display("Grupo Acme", session_map), "Grupo Acme"))
ok("without a group name, the session's first company is shown",
   identical(env$.cf_group_display("", session_map), "Networks Realtors"))
ok("a blank group name counts as none",
   identical(env$.cf_group_display("   ", session_map), "Networks Realtors"))
ok("with neither, the neutral HopDesk brand is shown",
   identical(env$.cf_group_display(NULL, c()), "HopDesk") &&
     identical(env$.cf_group_display("", c()), "HopDesk"))
ok("the neutral brand makes a file-name prefix",
   identical(gsub("[^A-Za-z0-9]", "", env$.cf_group_display("", c())), "HopDesk"))

for (fn in fns[-1])
  ok(sprintf("%s: group_name defaults to the neutral brand", fn),
     identical(formals(get(fn, env))$group_name, "HopDesk"))

# Every shared$group_name() read in the server goes through .cf_group_display()
# with the session's map.
reads <- 0L; wrapped <- 0L
walk <- function(x, inside = FALSE) {
  if (is.call(x)) {
    if (identical(x, quote(shared$group_name()))) {
      reads <<- reads + 1L
      if (inside) wrapped <<- wrapped + 1L
      return(invisible())
    }
    now_inside <- inside ||
      (identical(x[[1]], as.name(".cf_group_display")) && length(x) == 3L &&
         identical(x[[3]], quote(shared$company_map())))
    args <- as.list(x)[-1]
    for (i in seq_along(args)) if (!identical(args[[i]], quote(expr = ))) walk(args[[i]], now_inside)
  }
}
for (e in parse("R/cashflow_export_module.R")) walk(e)
ok("the export reads shared$group_name() in its 5 download paths", reads == 5L)
ok("every read goes through .cf_group_display(..., shared$company_map())", wrapped == reads)

src <- readLines("R/cashflow_export_module.R", warn = FALSE)
src <- src[!grepl("^\\s*#", src)]
ok("the export carries no fixed Networks Group brand",
   !any(grepl("Networks Group", src, fixed = TRUE)))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
