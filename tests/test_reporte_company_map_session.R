# =============================================================================
# tests/test_reporte_company_map_session.R
# The financial report branded itself ("XX Finances", the logo letter and
# "<group> — Reporte Financiero") from the GLOBAL COMPANY_MAP's first entry,
# and its company checkboxes fell back to that global when there were no
# accounts. Every session rewrites that global and production is one shared
# process, so a report could carry another client's group name.
#
# The brand now comes from the session: shared$group_name() (the source the
# cash-flow export already uses), else the first company of the session's
# map, else the template is left untouched.
#
# Run from project root:  Rscript tests/test_reporte_company_map_session.R
# =============================================================================
cat("=== test_reporte_company_map_session ===\n\n")
`%||%` <- function(a, b) if (is.null(a)) b else a

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# Extract the real function (no S3, no app).
env <- new.env()
for (e in parse("R/reporte_module.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), ".brand_inject"))
    eval(e, env)
env$`%||%` <- `%||%`

template <- paste(
  '<div class="rpt-brand-name">NG Finances</div>',
  '<div class="rpt-logo">N</div>',
  '<span>Networks Group &mdash; Reporte Financiero</span>',
  '<title>Reporte Financiero &mdash; Networks Group</title>')
session_map <- c(NRS = "Networks Realtors", NTS = "Networks Trucking Services")
brand <- function(...) tryCatch(env$.brand_inject(template, ...), error = function(e) {
  message("  ERROR: ", conditionMessage(e)); "" })

for (global in list(c(), c(HDX = "Otra Empresa"))) {
  assign("COMPANY_MAP", global, envir = .GlobalEnv)   # the old code read it from here
  etiqueta <- if (length(global)) "another client's map" else "an empty map"

  h <- brand(session_map, "Grupo Acme Uno")
  ok(sprintf("the session's group name brands the report (global holds %s)", etiqueta),
     grepl("Grupo Acme Uno &mdash; Reporte Financiero", h, fixed = TRUE) &&
       grepl("Reporte Financiero &mdash; Grupo Acme Uno", h, fixed = TRUE) &&
       grepl("GAU Finances", h, fixed = TRUE) && grepl('class="rpt-logo">G<', h, fixed = TRUE))
  ok(sprintf("the report never carries %s", etiqueta), nzchar(h) && !grepl("Otra|HDX", h))

  h <- brand(session_map, "")
  ok(sprintf("without a group name, the session's first company stands in (global holds %s)", etiqueta),
     grepl("Networks Realtors &mdash; Reporte Financiero", h, fixed = TRUE) &&
       grepl("NRS Finances", h, fixed = TRUE) && !grepl("Otra|HDX", h))

  h <- brand(c(), "")
  ok(sprintf("with no group and no companies the template is untouched (global holds %s)", etiqueta),
     identical(h, template))
}
rm("COMPANY_MAP", envir = .GlobalEnv)

ok("Networks Group still brands as NG Finances / N",
   { h <- brand(session_map, "Networks Group")
     grepl("NG Finances", h, fixed = TRUE) && grepl('class="rpt-logo">N<', h, fixed = TRUE) })

# Every .build_report_html() call passes the session's map and group name.
calls <- list()
walk <- function(x) {
  if (is.call(x)) {
    if (identical(x[[1]], as.name(".build_report_html"))) calls[[length(calls) + 1L]] <<- x
    args <- as.list(x)[-1]
    for (i in seq_along(args)) if (!identical(args[[i]], quote(expr = ))) walk(args[[i]])
  }
}
for (e in parse("R/reporte_module.R")) walk(e)
ok("found the 3 .build_report_html() calls", length(calls) == 3L)
ok("every .build_report_html() call passes cmap and group_name from shared",
   all(vapply(calls, function(cl)
     all(c("cmap", "group_name") %in% names(cl)) &&
       grepl("shared$company_map()", deparse(cl$cmap), fixed = TRUE) &&
       grepl("shared$group_name()",  deparse(cl$group_name), fixed = TRUE), logical(1))))

src <- readLines("R/reporte_module.R", warn = FALSE)
src <- src[!grepl("^\\s*#", src)]
ok("reporte_module.R does not read COMPANY_MAP", !any(grepl("COMPANY_MAP", src, fixed = TRUE)))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
