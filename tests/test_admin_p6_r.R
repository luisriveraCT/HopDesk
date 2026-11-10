# =============================================================================
# tests/test_admin_p6_r.R
# T28's notes (coordinator's reviews of 2b and 4), guarded:
#   1. P6: no table of the engine sits inside a renderUI of tiersServer that
#      depends on a refresh counter (*_refresh()). Each ↻ or save would
#      rebuild it: selection and scroll lost, back to the top.
#   2. The counts the old titles carried ("Solicitudes pendientes (N)",
#      "Cola de emails (N)", "Contactos activos (N)") live on in each
#      table's own counter ("N registros"): the eight keep the piece.
#
# Run from project root:  Rscript tests/test_admin_p6_r.R
# =============================================================================
cat("=== test_admin_p6_r ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# ── 1. P6 ────────────────────────────────────────────────────────────────────
srv <- NULL
for (e in parse("R/tiers_module.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("tiersServer"))) srv <- e[[3]]
nombres_llamados <- function(x) {
  out <- character(0)
  rec <- function(y) {
    if (!is.call(y)) return(invisible())
    if (is.name(y[[1]])) out <<- c(out, as.character(y[[1]]))
    ys <- as.list(y); for (k in seq_along(ys)) if (is.call(ys[[k]])) rec(ys[[k]])
  }
  rec(x); out
}
renders <- list()
buscar <- function(x, salida = NULL) {
  if (!is.call(x)) return(invisible())
  # output$<name> <- renderUI({...})
  if (identical(x[[1]], as.name("<-")) && is.call(x[[3]]) && identical(x[[3]][[1]], as.name("renderUI")))
    renders[[length(renders) + 1L]] <<- list(salida = paste(deparse(x[[2]]), collapse = ""), cuerpo = x[[3]])
  xs <- as.list(x); for (k in seq_along(xs)) if (is.call(xs[[k]])) buscar(xs[[k]])
}
buscar(srv)
con_tabla <- Filter(function(r) any(grepl("^(hd_table_ui|tiers_[a-z]+_ui)$", nombres_llamados(r$cuerpo))), renders)
malos <- Filter(function(r) any(grepl("_refresh$", nombres_llamados(r$cuerpo))), con_tabla)
ok(sprintf("tiersServer tiene renderUI con tablas del motor (%d)", length(con_tabla)), length(con_tabla) >= 4L)
ok("P6: ningún renderUI con una tabla del motor depende de un contador de refresco",
   { for (r in malos) message("  ", r$salida, " lee: ",
                              paste(unique(grep("_refresh$", nombres_llamados(r$cuerpo), value = TRUE)), collapse = ", "))
     !length(malos) })

# ── 2. The counts ────────────────────────────────────────────────────────────
source("R/hd_registro.R"); source("R/hd_piezas.R")
tablas <- c("usuarios", "cuentas_bloqueadas", "solicitudes", "clientes", "contactos", "invitaciones",
            "cola_correos", "accesos_salto")
sin <- tablas[!vapply(tablas, function(t) "contador" %in% hd_piezas_de(t), logical(1))]
ok("las 8 tablas muestran su total ('N registros'), el número que antes iba en el título",
   { if (length(sin)) message("  sin contador: ", paste(sin, collapse = ", ")); !length(sin) })

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
