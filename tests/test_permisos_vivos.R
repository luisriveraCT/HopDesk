# "Permisos vivos": every permission in the registry is read somewhere in the
# server code. A key nobody reads is a decorative checkbox; this test is what
# keeps the 11 dead ones from coming back. A key whose call site belongs to a
# front still under construction carries `pendiente = "<front>"` in
# R/permisos.R and is skipped until then.
# Run from the repo root: Rscript tests/test_permisos_vivos.R
library(testthat)
`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/i18n.R")
source("R/tier_registry.R")
source("R/permisos.R")
source("R/pasivos_capabilities.R")

# Where a key is defined or only displayed does not count as a reader.
NO_CUENTAN <- c("R/permisos.R", "R/tier_registry.R")

codigo <- local({
  fs <- setdiff(c("app.R", list.files("R", "[.]R$", recursive = TRUE, full.names = TRUE)), NO_CUENTAN)
  setNames(lapply(fs, function(f) {
    l <- readLines(f, warn = FALSE, encoding = "UTF-8")
    l[!grepl("^[[:space:]]*#", l)]
  }), fs)
})

lectores <- function(k) {
  q <- "[\"']"
  pat <- paste0(
    "(puede(_o_avisa)?[(].*", q, k, q, ")",              # puede(u, "k")
    "|([$]", k, "([^a-z_]|$))",                           # perms$k
    "|([[][[]", q, k, q, "[]][]])",                       # perms[["k"]]
    "|(hd_staff_emails_with_perm[(]", q, k, q, ")")
  names(Filter(function(l) any(grepl(pat, l, perl = TRUE)), codigo))
}

test_that("cada permiso se lee en el servidor", {
  # Pasivos capabilities map onto keys, so has_capability() is a reader too.
  via_pasivos <- unique(stats::na.omit(unname(PASIVOS_CAPABILITIES)))
  muertos <- character()
  for (k in permisos_claves()) {
    if (!is.null(PERMISOS[[k]]$pendiente)) next
    if (k %in% via_pasivos) next
    if (!length(lectores(k))) muertos <- c(muertos, k)
  }
  if (length(muertos)) cat("Permisos sin lector:", paste(muertos, collapse = ", "), fill = TRUE)
  expect_length(muertos, 0)
})

test_that("las llaves muertas de la auditoría no regresan", {
  muertas <- c("can_view_cobros", "can_view_pagos", "can_view_agenda", "can_view_bancos",
               "can_view_reportes", "can_edit_invoices", "can_move_invoices", "can_manage_tags",
               "can_export_pdf", "can_manage_providers", "can_manage_users")
  expect_length(intersect(muertas, permisos_claves()), 0)
})

test_that("las capacidades de Pasivos apuntan a permisos que existen", {
  m <- stats::na.omit(unname(PASIVOS_CAPABILITIES))
  expect_length(setdiff(m, permisos_claves()), 0)
})
