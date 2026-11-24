# =============================================================================
# tests/test_t81_notas_aviso.R
# T81: a public note is news for the whole client (Panorama).
#   1. A new public note leaves one aviso for every client tier, linked to
#      its month (NOTA:<year>-<month>); a personal one leaves none.
#   2. Editing a public note tells nobody again; personal -> public does.
#   3. Deleting the note retires its aviso.
#   4. The author's own aviso is not new to them; it is to the others.
#   5. An aviso that fails never fails the note: it is saved all the same.
#   6. The link: only NOTA:<4 digits>-<01..12> or a tab; it opens the panel.
# S3 is an in-memory store local to this test (avisos.R's two helpers);
# aws.s3 is never touched. save_notes is a local store too.
# Contraprueba: T81_RAIZ=<checkout of 9769220> sources that notes_handlers.R:
# section 1 fails (no aviso).
# Run: Rscript --vanilla tests/test_t81_notas_aviso.R (repo root)
# =============================================================================
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
source("R/i18n.R")
source("R/tier_registry.R")
source("R/permisos.R")
source("R/avisos.R")
source("R/panorama_datos.R")
raiz <- Sys.getenv("T81_RAIZ", ".")
source(file.path(raiz, "R/notes_handlers.R"))

pass <- 0L; fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("  PASS ", label, "\n") }
  else { fail <<- fail + 1L; cat("  FAIL ", label, "\n") }
}

# ── In-memory S3 for avisos.R ────────────────────────────────────────────────
.mem <- new.env()
.s3_preload_cache <- new.env(); .s3_missing_cache <- new.env()
.s3_read_with <- function(key, client_id = NULL) .mem[[paste0(client_id, "/", key)]]
.s3_leer_para_escribir <- function(key, client_id) .mem[[paste0(client_id, "/", key)]]
.s3_existe <- function(key, client_id) !is.null(.mem[[paste0(client_id, "/", key)]])
.fallar_escrituras <- 0L
.s3_write <- function(obj, key, client_id = NULL) {
  if (.fallar_escrituras > 0L) { .fallar_escrituras <<- .fallar_escrituras - 1L; stop("simulada") }
  assign(paste0(client_id, "/", key), obj, envir = .mem)
}

# ── What notes_handlers calls around the save ────────────────────────────────
.notas_s3 <- NULL
save_notes <- function(df, client_id, base = NULL) { .notas_s3 <<- df; invisible(TRUE) }
load_notes <- function(client_id) .notas_s3 %||% .schema_notes()
.schema_notes <- function() tibble(ledger = character(), year = integer(), month = integer(),
  id = character(), title = character(), body = character(), author = character(),
  author_code = character(), visibility = character(),
  created = as.POSIXct(character()), updated = as.POSIXct(character()))
bump_sync_version <- function(...) invisible(NULL)
log_action <- function(...) invisible(NULL)
hoy_usuario <- function() as.Date("2026-11-23")
mes_es <- function(x) format(as.Date(x), "%Y-%m")
notes_panel_ui <- function(...) div(class = "panel-de-prueba")

CID <- "cliente_prueba"
usr <- function(user, tier) list(user = user, tier = tier, permisos = permisos_resueltos(tier))
niveles_de_cliente <- niveles_cliente()

srv <- function(input, output, session) {
  shared <- list(effective_client_id = function() CID, home_client_id = function() CID,
                 current_user_info = function() list(name = "Ana", account_code = "U0001", user = "ana"))
  notes_handlers(input, output, session, shared, notes_df = reactiveVal(.schema_notes()),
                 current_user = function() "ana")
}
avisos_nota <- function() { a <- avisos_leer(CID); a[grepl("^nota\\|", a$clave %||% character()), , drop = FALSE] }
pon <- function(session, nombre, valor) do.call(session$setInputs, stats::setNames(list(valor), nombre))

testServer(srv, {
  session$setInputs(month_sel = as.Date("2026-11-01"))

  cat("\n== 1. Una pública avisa; una personal no ==\n")
  session$setInputs(note_title_input = "Cierre", note_body_input = strrep("x", 300))
  session$setInputs(note_save_personal = 1)
  ok("1.1 una personal no deja aviso", nrow(avisos_nota()) == 0L)
  session$setInputs(add_note = 1, note_title_input = "Cierre de noviembre", note_body_input = strrep("y", 300))
  session$setInputs(note_save_public = 1)
  a <- avisos_nota()
  pub <- .notas_s3[.notas_s3$visibility == "public", ]
  ok("1.2 una pública deja un aviso", nrow(a) == 1L)
  ok("1.3 su clave es la de la nota", identical(a$clave[1], paste0("nota|", pub$id[1])))
  ok("1.4 llega a todos los niveles del cliente",
     setequal(jsonlite::fromJSON(a$niveles[1]), niveles_de_cliente))
  ok("1.5 lo ve cualquier nivel de cliente (sin permiso)",
     all(vapply(niveles_de_cliente, function(t) nrow(avisos_visibles(CID, usr("zoe", t))) == 1L, logical(1))))
  ok("1.6 su enlace es el mes de la nota", identical(a$enlace[1], "NOTA:2026-11"))
  ok("1.7 el título nombra autor y nota", grepl("Ana", a$titulo[1]) && grepl("Cierre de noviembre", a$titulo[1]))
  ok("1.8 el cuerpo se corta a 200", nchar(a$cuerpo[1]) == 200L)
  ok("1.9 lo escribió la autora", identical(a$creado_por[1], "ana"))

  cat("\n== 2. Editar no repite; personal -> pública sí avisa ==\n")
  pon(session, paste0("edit_note_", pub$id[1]), 1)
  session$setInputs(note_title_input = "Cierre de noviembre (v2)")
  session$setInputs(note_save_public = 2)
  ok("2.1 editar una pública no deja otro aviso", nrow(avisos_nota()) == 1L)
  ok("2.2 ni lo vuelve nuevo (contador 1)", identical(avisos_nota()$contador[1], 1L))
  per <- .notas_s3[.notas_s3$visibility == "personal", ]
  pon(session, paste0("edit_note_", per$id[1]), 1)
  session$setInputs(note_save_public = 3)
  ok("2.3 una personal que pasa a pública avisa", nrow(avisos_nota()) == 2L &&
       any(avisos_nota()$clave == paste0("nota|", per$id[1])))

  cat("\n== 3. Borrar la nota retira su aviso ==\n")
  pon(session, paste0("delete_note_", pub$id[1]), 1)
  session$setInputs(note_delete_confirm = 1)
  a <- avisos_nota()
  ok("3.1 la nota ya no está", !pub$id[1] %in% .notas_s3$id)
  ok("3.2 su aviso queda retirado (nada se borra)", isTRUE(a$retirado[a$clave == paste0("nota|", pub$id[1])]))
  ok("3.3 y ya nadie lo ve", !any(avisos_visibles(CID, usr("zoe", niveles_de_cliente[1]))$clave == paste0("nota|", pub$id[1])))
  ok("3.4 el de la otra nota sigue", !isTRUE(a$retirado[a$clave == paste0("nota|", per$id[1])]))

  cat("\n== 5. Un aviso que truena no tumba la nota ==\n")
  .fallar_escrituras <<- 2L
  session$setInputs(add_note = 2, note_title_input = "Sin aviso", note_body_input = "z")
  r <- tryCatch({ session$setInputs(note_save_public = 4); TRUE }, error = function(e) FALSE)
  ok("5.1 guardar no da error", r)
  ok("5.2 la nota se guardó", any(.notas_s3$title == "Sin aviso"))
  .fallar_escrituras <<- 0L

  cat("\n== 6. El enlace abre el panel en su mes ==\n")
  session$setInputs(abrir_notas = "NOTA:2026-10")
  ok("6.1 el panel queda abierto", !is.null(output$notes_panel))
  session$setInputs(abrir_notas = "NOTA:2026-10")
  ok("6.2 abrir otra vez no lo cierra", !is.null(output$notes_panel))
})

cat("\n== 4. El aviso de la autora no es nuevo para ella ==\n")
v <- avisos_visibles(CID, usr("zoe", niveles_de_cliente[1]))
antes <- as.POSIXct("2000-01-01", tz = "UTC")
f_ana <- panorama_feed(v, antes, usuario = "ana")
f_zoe <- panorama_feed(v, antes, usuario = "zoe")
nota <- grepl("^nota\\|", f_ana$clave)
ok("4.1 para la autora no es nuevo", any(nota) && !any(f_ana$nuevo[nota]))
ok("4.2 para los demás sí", all(f_zoe$nuevo[grepl("^nota\\|", f_zoe$clave)]))
ok("4.3 sin usuario, como antes", all(panorama_feed(v, antes)$nuevo))

cat("\n== 6b. Solo enlaces válidos ==\n")
ok("6.3 NOTA:2026-11 vale", panorama_enlace_ok("NOTA:2026-11") && identical(notas_enlace_mes("NOTA:2026-11"), as.Date("2026-11-01")))
ok("6.4 una pestaña vale", panorama_enlace_ok("CAL") && is.null(notas_enlace_mes("CAL")))
ok("6.5 mes 13 no", !panorama_enlace_ok("NOTA:2026-13") && is.null(notas_enlace_mes("NOTA:2026-13")))
ok("6.6 nada inyectado", !panorama_enlace_ok("NOTA:2026-11');alert(1)//") && is.null(notas_enlace_mes("NOTA:2026-11'x")))
ok("6.7 NA y vacío no", !panorama_enlace_ok(NA_character_) && !panorama_enlace_ok(""))

cat(sprintf("\nT81 notas -> avisos: %d PASS / %d FAIL\n", pass, fail))
if (fail > 0L) quit(status = 1L)
