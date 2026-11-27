# =============================================================================
# tests/test_t87_m125.R
# T87 M1, M2, M5 (the Coordinador's conditions, 2026-11-24):
#   M1  the page's dictionary (i18n_head) leaves out the internal permissions'
#       and the staff tiers' names, from the registries; a guardian: no hdT()
#       of www/ asks for one, and no R code sends one as a key to the browser
#       (hd_table titles and texts are translated by hdT in the page).
#   M2  Invitaciones: a client is only told to share the link by hand; staff
#       still see the email service's whole state.
#   M5  one refusal text (permiso_denegado_texto): the permission is named
#       only to who can see Usuarios (staff or can_view_tiers), from the
#       registry; puede_o_avisa and pasivos_autoriza use it, no call site
#       changes. The Usuarios tab bar is not built for who cannot open it, and
#       a Finanzas or Compras session runs the whole tab with no error.
# Uses tests/helpers/tiers_fixture_app.R (all of R/, S3 in memory).
# Run from project root:  Rscript --vanilla tests/test_t87_m125.R
# =============================================================================
cat("=== test_t87_m125 ===\n\n")
if (interactive()) stop("Run this test with Rscript, never inside an R session.")
suppressPackageStartupMessages({ library(shiny) })
Sys.setenv(TIERS_ROL = "dev")
invisible(capture.output(source("tests/helpers/tiers_fixture_app.R", local = globalenv())))

.pass <- 0L; .fail <- 0L
ok <- function(label, cond, detalle = NULL) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else {
    cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L
    if (length(detalle)) cat("       ", paste(utils::head(detalle, 5), collapse = "\n        "), "\n")
  }
}

cat("== M1. El diccionario de la página ==\n")
internas <- i18n_claves_internas()
ok("1.1 las claves internas salen de los registros (permisos internos y niveles staff)",
   all(paste0("permisos.", permisos_internos()) %in% internas) &&
     all(paste0("niveles.nombre.", niveles_staff()) %in% internas) && length(internas) > 0L)
cab <- as.character(i18n_head())
ok("1.2 i18n_head() no las manda", !any(vapply(paste0('"', internas, '"'), grepl, logical(1), x = cab, fixed = TRUE)))
# T118: the page carries only what its JavaScript asks for; the names of the
# permissions and tiers (client ones too) reach the browser drawn by tr().
ok("1.3 ...y manda lo que el JS de la página pide (T118: solo eso; los nombres de permisos y niveles los dibuja R)",
   grepl('"tablas.buscar"', cab, fixed = TRUE) && !grepl('"permisos.can_manage_bancos"', cab, fixed = TRUE) &&
     nzchar(tr("permisos.can_manage_bancos")) && tr("permisos.can_manage_bancos") != "permisos.can_manage_bancos")
ok("1.4 R las sigue teniendo (tr en el servidor)", nzchar(tr(internas[1])) && tr(internas[1]) != internas[1])
js <- unlist(lapply(list.files("www", "[.]js$", recursive = TRUE, full.names = TRUE), readLines, warn = FALSE, encoding = "UTF-8"))
pedidas <- unique(sub("^hdT\\(['\"]", "", sub("['\"]$", "", regmatches(js, gregexpr("hdT\\(['\"][^'\"]+['\"]", js))[[1]])))
pedidas <- unique(unlist(lapply(js, function(l) { m <- regmatches(l, gregexpr("hdT\\(['\"][^'\"]+['\"]", l))[[1]]; gsub("^hdT\\(['\"]|['\"]$", "", m) })))
ok("1.5 ningún hdT(\"…\") de www/ pide una clave que la página ya no trae", !length(intersect(pedidas, internas)),
   intersect(pedidas, internas))
# hd_table's titles and texts travel as keys (hdtable.js translates them): no
# R code may build an internal key outside its registries.
src_r <- setdiff(c("app.R", list.files("R", "[.]R$", full.names = TRUE)), c("R/permisos.R", "R/tier_registry.R", "R/i18n.R"))
codigo <- unlist(lapply(src_r, function(f) { l <- readLines(f, warn = FALSE, encoding = "UTF-8"); l[!grepl("^\\s*#", l)] }))
literal <- internas[vapply(internas, function(k) any(grepl(paste0("[\"']", k, "[\"']"), codigo)), logical(1))]
armadas <- grep("paste0?\\([\"'](permisos|niveles\\.nombre)\\.[\"']", codigo, value = TRUE)
ok("1.6 ningún archivo de R/ arma ni escribe una clave interna para mandarla al navegador",
   !length(literal) && !length(armadas), c(literal, armadas))

cat("== M5. El rechazo ==\n")
u <- function(tier, ov = "{}", staff = FALSE) list(user = "x", tier = tier, is_staff = staff, permisos = permisos_resueltos(tier, ov))
nombre <- permiso_nombre("can_publish_panorama")
ok("5.1 Finanzas (no ve Usuarios): rechazo genérico, sin el nombre del permiso",
   identical(permiso_denegado_texto(u("finance"), "can_publish_panorama"), tr("permisos.denegado_generico")))
ok("5.2 Dev (ve Usuarios): con el nombre", grepl(nombre, permiso_denegado_texto(u(nivel_ti()), "can_publish_panorama"), fixed = TRUE))
ok("5.3 un Admin con can_view_tiers dado a mano: con el nombre",
   grepl(nombre, permiso_denegado_texto(u("admin", '{"can_view_tiers": true}'), "can_publish_panorama"), fixed = TRUE))
ok("5.4 staff: con el nombre", grepl(nombre, permiso_denegado_texto(u("hopdesk", staff = TRUE), "can_publish_panorama"), fixed = TRUE))
ok("5.5 sin usuario o con un permiso desconocido: genérico",
   identical(permiso_denegado_texto(NULL, "can_publish_panorama"), tr("permisos.denegado_generico")) &&
     identical(permiso_denegado_texto(u(nivel_ti()), "no_existe"), tr("permisos.denegado_generico")))
avisos <- list()
s <- MockShinySession$new()
s$sendNotification <- function(type, message) avisos[[length(avisos) + 1L]] <<- message
r <- withReactiveDomain(s, puede_o_avisa(list(usuario = function() u("finance")), "can_publish_panorama", session = s))
ok("5.6 puede_o_avisa (sus 84 llamadas, sin cambiar) rechaza y avisa en genérico",
   identical(r, FALSE) && length(avisos) == 1L && grepl(tr("permisos.denegado_generico"), paste(unlist(avisos), collapse = " "), fixed = TRUE) &&
     !grepl(nombre, paste(unlist(avisos), collapse = " "), fixed = TRUE))
fuentes <- unlist(lapply(c("app.R", list.files("R", "[.]R$", full.names = TRUE)), function(f) {
  l <- readLines(f, warn = FALSE, encoding = "UTF-8"); paste0(f, ":", grep('tr("permisos.denegado"', l, fixed = TRUE)) }))
fuentes <- fuentes[!grepl(":$", fuentes)]
ok("5.7 un solo lugar escribe el rechazo con nombre (el ayudante, en R/permisos.R)",
   length(fuentes) == 1L && startsWith(fuentes, "R/permisos.R:"), fuentes)

cat("== M5/M2. Usuarios en sesiones reales ==\n")
.mem_put <- function(k, df) assign(k, df, envir = .mem)
.mem_put("acme/usuarios.rds", rbind(get("acme/usuarios.rds", envir = .mem),
  usuario("fede", "finance", "acme", "Fede Finanzas"), usuario("cata", "compras", "acme", "Cata Compras")))
for (e in parse("R/email_service.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("email_service_status")))
    eval(e, envir = globalenv())
Sys.unsetenv("RESEND_API_KEY")
leer <- function(session, nombre) {
  o <- session$.__enclos_env__$private$outs[[nombre]]
  if (is.null(o)) return(NULL)
  if (is.null(o$promise)) { session$flushReact(); o <- session$.__enclos_env__$private$outs[[nombre]] }
  if (is.null(o$promise)) return(NULL)
  val <- NULL; listo <- FALSE
  promises::then(o$promise, function(v) { val <<- v; listo <<- TRUE }, function(e) { listo <<- TRUE })
  fin <- Sys.time() + 5
  while (!listo && Sys.time() < fin) later::run_now(0.05)
  if (is.list(val) && !is.null(val$val)) val$val else val
}
sesion <- function(user, tier, staff = FALSE, home = "acme") {
  quien <<- list(user = user, tier = tier, is_staff = staff, home = home)
  res <- list(error = NULL, tabs = NULL, correo = NULL)
  res$error <- tryCatch({
    testServer(server, {
      session$flushReact()
      res$tabs   <<- leer(session, "tiers-main_tabs_ui")
      res$correo <<- leer(session, "tiers-inv_estado_correo")
      session$flushReact()
    })
    NULL
  }, error = function(e) conditionMessage(e))
  res
}
texto <- function(x) paste(as.character(unlist(x)), collapse = " ")
for (q in list(c("fede", "finance"), c("cata", "compras"))) {
  r <- sesion(q[1], q[2])
  ok(sprintf("3.1 %s: la sesión corre Usuarios sin error", q[2]), is.null(r$error), r$error)
  ok(sprintf("3.2 %s: no se construye la barra de Usuarios", q[2]), is.null(r$tabs) || !nzchar(texto(r$tabs)))
}
r <- sesion("devacme", nivel_ti())
ok("3.3 Dev: la barra sí se construye", !is.null(r$tabs) && nzchar(texto(r$tabs)))
ok("2.1 Dev (cliente): Invitaciones solo dice que se comparta el enlace a mano",
   grepl(tr("niveles.correo.estado.cliente_manual"), texto(r$correo), fixed = TRUE) &&
     !grepl("RESEND|Renviron|RUNBOOK", texto(r$correo)))
r <- sesion("bunny", "hopdesk", staff = TRUE, home = "hd-admin")
ok("2.2 staff: sigue viendo el estado completo del servicio", grepl("RESEND_API_KEY", texto(r$correo), fixed = TRUE), r$error)

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
