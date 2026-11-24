# =============================================================================
# tests/test_t87_lectura.R
# T87 paso 3: what a client READS of its own log -- actividad_para_cliente()
# (R/audit_log_viewer_module.R; the table, the detail and the PDF read the
# same rows) -- and what staff read of a signed row (actividad_para_staff()).
#   1. Whitelist: a staff row of class S never shows; C, N and A do.
#   2. Signed by first name: "<primer> · HD Staff" (the dictionary's label);
#      the subscription "HopDesk"; never a user name, full name, code or
#      hd-admin -- in any column.
#   3. Rows written before T87 (staff user name, home_client in metadata,
#      the hidden external_access) are shown the same way, at read.
#   4. Entry + exit = one line "Tuvo acceso del <desde> al <hasta>" in the
#      browser's zone with the zone written; an entry with no exit yet says
#      so; two visits are two lines.
#   5. A client user's own row is untouched; the module's own metadata stays,
#      the log's internal keys go.
#   6. Staff see "<code> · <full name>" for a signed row (Principal's evidence).
# Run from project root:  Rscript --vanilla tests/test_t87_lectura.R
# =============================================================================
cat("=== test_t87_lectura ===\n\n")
suppressPackageStartupMessages({ library(shiny) })
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
for (f in c("R/i18n.R", "R/zona.R", "R/hd_registro.R", "R/hd_piezas.R", "R/monedas.R", "R/hd_columnas.R",
            "R/app_audit.R", "R/audit_log_viewer_module.R")) suppressMessages(source(f))

.pass <- 0L; .fail <- 0L
ok <- function(label, cond, detalle = NULL) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else {
    cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L
    if (length(detalle)) cat("       ", paste(utils::head(detalle, 5), collapse = "\n        "), "\n")
  }
}

auth_load_usuarios <- function(client_id = NULL)
  data.frame(username = c("zz_canario_staff", "mouse"), display_name = c("Zelda Kanaryx Qwertzil", "Mouse Rivera"),
             account_code = c("S9999", "S0001"), tier = c("hopdesk", "principal"), stringsAsFactors = FALSE)
t87_directorio_invalidar()

t0 <- as.POSIXct("2026-11-24 16:00:00", tz = "UTC")
fila <- function(min, user, action, description = "", metadata = "{}", module = "m", target_id = NA_character_)
  data.frame(id = paste0("r", min), ts = t0 + min * 60, user = user, module = module, action = action,
             description = description, target_id = target_id, s3_key = NA_character_, client_id = "acme",
             metadata = metadata, stringsAsFactors = FALSE)
df <- rbind(
  # T87 rows (as paso 2 writes them)
  fila(0,  "hd-staff:S9999", "acceso_staff_entrada", "Entró a los datos de la empresa", '{"actor_primer":"Zelda"}', "clientes"),
  fila(5,  "hd-staff:S9999", "eliminar_nota", "Nota eliminada: Cierre", '{"ledger":"AR","actor_primer":"Zelda"}', "notes", "nota-1"),
  fila(10, "hopdesk", "cambio_limite_usuarios", "El límite de usuarios cambió de 5 a 4",
       '{"old_max_users":5,"new_max_users":4,"texto_clave":"tablas.actividad.neutral.cambio_limite"}', "tiers"),
  fila(20, "hd-staff:S9999", "acceso_staff_salida", "Salió de los datos de la empresa", '{"motivo":"volver","actor_primer":"Zelda"}', "clientes"),
  # A second visit, still open
  fila(60, "hd-staff:S9999", "acceso_staff_entrada", "Entró a los datos de la empresa", '{"actor_primer":"Zelda"}', "clientes"),
  # Rows written before T87 (what is in prod's client folders today)
  fila(30, "zz_canario_staff", "external_access", "HopDesk staff accessed this client's data (session context switch)",
       '{"home_client":"hd-admin"}', "clientes", "acme"),
  fila(31, "zz_canario_staff", "confirmar_pago", "Pago confirmado por zz_canario_staff (Zelda Kanaryx Qwertzil)",
       '{"empresa":"NG","home_client":"hd-admin"}', "pagar_hoy", "zz_canario_staff"),
  fila(32, "zz_canario_staff", "ver_como", "zz_canario_staff (como Admin)", '{"nivel":"admin","home_client":"hd-admin"}', "niveles"),
  fila(33, "mouse", "hop_access_approved", "Mouse aprobó acceso de zz_canario_staff a acme (24h)",
       '{"grantee":"zz_canario_staff","home_client":"hd-admin"}', "tiers"),
  # A client user's own row
  fila(40, "ana", "eliminar_nota", "Nota eliminada: Mía", '{"ledger":"AP"}', "notes", "nota-2"))
TZ <- "America/Mexico_City"
v <- actividad_para_cliente(df, tz = TZ)
txt <- function(d) paste(unlist(lapply(d, as.character)), collapse = " | ")
FIRMA <- paste0("Zelda · ", tr("comun.staff.etiqueta"))

cat("== 1. Lista blanca ==\n")
ok("1.1 las acciones S (aunque sean viejas) no se muestran", !any(v$action %in% c("ver_como", "hop_access_approved")))
ok("1.2 C, N y A sí", all(c("eliminar_nota", "cambio_limite_usuarios", "confirmar_pago", "acceso_staff") %in% v$action))

cat("== 2. Firmado por primer nombre ==\n")
ok("2.1 lo del staff sale '<primer> · HD Staff'", all(v$user[v$action %in% c("confirmar_pago", "acceso_staff")] == FIRMA) &&
     any(v$user == FIRMA & v$action == "eliminar_nota"))
ok("2.2 la suscripción sale 'HopDesk'", identical(v$user[v$action == "cambio_limite_usuarios"], tr("comun.staff.hopdesk")))
prohibido <- c("zz_canario_staff", "Kanaryx", "Qwertzil", "S9999", "hd-staff", "hd-admin", "home_client", "actor_")
ok("2.3 ningún usuario, apellido, código ni hd-admin en ninguna columna",
   !any(vapply(prohibido, function(p) grepl(p, txt(v), fixed = TRUE), logical(1))),
   prohibido[vapply(prohibido, function(p) grepl(p, txt(v), fixed = TRUE), logical(1))])

cat("== 3. Lo escrito antes de T87 ==\n")
cp <- v[v$action == "confirmar_pago", ]
ok("3.1 una fila vieja del staff sale firmada, sin su usuario ni su nombre en la descripción",
   nrow(cp) == 1L && identical(cp$user, FIRMA) && !grepl("zz_canario|Kanaryx", cp$description) &&
     !grepl("zz_canario", cp$target_id))
ok("3.2 ...y su metadata pierde home_client y conserva la del módulo",
   grepl('"empresa":"NG"', cp$metadata, fixed = TRUE) && !grepl("home_client", cp$metadata, fixed = TRUE))

cat("== 4. Entrada y salida ==\n")
acc <- v[v$action == "acceso_staff", , drop = FALSE]
acc <- acc[order(acc$ts), , drop = FALSE]
desde <- .t87_cuando(t0, TZ); hasta <- .t87_cuando(t0 + 20 * 60, TZ)
ok("4.1 entrada + salida = una sola línea 'Tuvo acceso del … al …'",
   any(acc$description == tr("tablas.actividad.acceso.periodo", desde = desde, hasta = hasta)))
ok("4.2 la salida no queda como línea aparte", !any(v$action == "acceso_staff_salida"))
ok("4.3 en la zona del navegador, con la zona escrita", grepl("CST", desde, fixed = TRUE) && grepl("10:00", desde, fixed = TRUE))
ok("4.4 una visita abierta dice 'desde'", any(acc$description == tr("tablas.actividad.acceso.abierto", desde = .t87_cuando(t0 + 3600, TZ))))
ok("4.5 la entrada vieja (external_access) también es una línea de acceso",
   any(acc$ts == t0 + 30 * 60))
ok("4.6 tres visitas, tres líneas", nrow(acc) == 3L)

cat("== 5. Lo del cliente, intacto ==\n")
ana_antes <- df[df$user == "ana", ]; ana <- v[v$user == "ana", ]
ok("5.1 la fila de un usuario del cliente sale idéntica", identical(ana[, names(ana_antes)], ana_antes, ignore.environment = TRUE) ||
     isTRUE(all.equal(ana[, names(ana_antes)], ana_antes, check.attributes = FALSE)))
ok("5.2 la metadata del módulo se queda en lo del staff (ledger)", grepl('"ledger":"AR"', v$metadata[v$action == "eliminar_nota" & v$user == FIRMA], fixed = TRUE))
ok("5.3 una tabla vacía pasa tal cual", identical(actividad_para_cliente(df[0, ]), df[0, ]))

cat("== 6. Staff ve la cuenta detrás de la firma ==\n")
s <- actividad_para_staff(df)
ok("6.1 'hd-staff:S9999' -> 'S9999 · Zelda Kanaryx Qwertzil'",
   all(s$user[df$user == "hd-staff:S9999"] == "S9999 · Zelda Kanaryx Qwertzil"))
ok("6.2 lo demás sin tocar", identical(s$user[df$user != "hd-staff:S9999"], df$user[df$user != "hd-staff:S9999"]))
ok("6.3 un código desconocido se queda como está", identical(actividad_para_staff(fila(1, "hd-staff:S0404", "x"))$user, "hd-staff:S0404"))

cat("== 7. El visor la usa ==\n")
src <- readLines("R/audit_log_viewer_module.R", warn = FALSE, encoding = "UTF-8")
ok("7.1 el cliente recibe actividad_para_cliente() en la zona de su navegador",
   any(grepl("df <- actividad_para_cliente(df, tz = zona_navegador(session))", src, fixed = TRUE)))
ok("7.2 el staff recibe actividad_para_staff()", any(grepl("df <- actividad_para_staff(df)", src, fixed = TRUE)))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
