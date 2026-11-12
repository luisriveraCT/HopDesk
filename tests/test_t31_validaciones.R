# =============================================================================
# tests/test_t31_validaciones.R
# T31: R/usuarios_cuentas.R -- the server-side checks of what a person types
# on the activation, "olvidé" and reset forms, and reservar_usuario().
#   1. each field: the allowed values pass, and every injection attempt (HTML,
#      JS, quotes, regex metacharacters, control characters, a huge value,
#      a vector, a list, NA, invalid UTF-8) is refused without an error;
#   2. one password rule: 8+ characters, a digit and a symbol, at most 128;
#   3. usuario_ocultar(): the mail never shows the whole user name;
#   4. reservar_usuario(): unique in ALL of HopDesk, case-insensitive; the
#      index is the authority and a failed read of it refuses (never "free");
#   5. every message key exists in es and en.
# S3 is the in-memory store of tests/helpers/s3_memoria.R (no network).
# Run from project root:  Rscript tests/test_t31_validaciones.R
# CUENTAS_R=<file> checks another copy of R/usuarios_cuentas.R (contraprueba).
# =============================================================================
cat("=== test_t31_validaciones ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("tests/helpers/s3_memoria.R")
suppressMessages(source("R/i18n.R"))
source(Sys.getenv("CUENTAS_R", "R/usuarios_cuentas.R"))

enorme <- strrep("a", 5e6)
malo_utf8 <- rawToChar(as.raw(c(0x61, 0xff, 0x62)))
ataques <- list(
  "<script>alert(1)</script>", "\"><img src=x onerror=alert(1)>", "javascript:alert(1)",
  "' OR '1'='1", "\"; DROP TABLE usuarios; --", ".*", "(a+)+$", "[", "\\", "^$|.*",
  "{cliente}", "a\nb", "a\tb", "a\u0001b", enorme, malo_utf8,
  c("a", "b"), list("luis"), NA_character_, NULL, 42)

# ── 1. Fields ────────────────────────────────────────────────────────────────
ok("usuarios válidos pasan", all(vapply(c("luis", "mj_nunez", "abc", "u2026", "  Luis  "),
                                        function(x) is.null(usuario_error(x)), logical(1))))
ok("usuarios inválidos se rechazan", all(vapply(list("lu", strrep("a", 31), "luis rivera", "luís", "a.b",
                                                     "luis@x.mx", ""), function(x) !is.null(usuario_error(x)),
                                                logical(1))))
ok("un usuario con @ dice que no puede ser correo",
   identical(usuario_error("ana@empresa.mx"), "cuentas.error.usuario_correo"))
ok("ningún ataque pasa como usuario, y ninguno lanza error",
   all(vapply(ataques, function(x) !is.null(usuario_error(x)), logical(1))))

ok("nombres y puestos válidos pasan",
   is.null(nombre_error("María José Núñez")) && is.null(puesto_error("Analista de Tesorería (CxP) & Bancos")) &&
   is.null(nombre_error(strrep("ñ", 80))))
ok("nombre vacío o de espacios es obligatorio",
   identical(nombre_error("   "), "cuentas.error.nombre_obligatorio") &&
   identical(puesto_error(NULL), "cuentas.error.puesto_obligatorio"))
ok("81 caracteres es largo; 5 MB también, sin procesarlos",
   identical(nombre_error(strrep("a", 81)), "cuentas.error.nombre_largo") &&
   identical(puesto_error(enorme), "cuentas.error.puesto_largo"))
ok("< > { } y caracteres de control se rechazan en nombre y puesto",
   all(vapply(c("<b>x</b>", "a>b", "{cliente}", "a\nb", "a\u0007b"),
              function(x) identical(nombre_error(x), "cuentas.error.caracteres") &&
                identical(puesto_error(x), "cuentas.error.caracteres"), logical(1))))
ok("ningún ataque pasa como nombre salvo los que son texto inocuo",
   all(vapply(ataques, function(x) {
     e <- nombre_error(x)
     # Quotes, regex characters and "javascript:" are plain text in a name:
     # they are escaped when printed, never interpreted.
     inocuo <- is.character(x) && length(x) == 1L && !is.na(x) && nchar(x, "bytes") <= 80 &&
       validUTF8(x) && !grepl("[<>{}[:cntrl:]]", x)
     if (inocuo) is.null(e) else !is.null(e)
   }, logical(1))))

ok("correos válidos pasan", all(vapply(c("ana@empresa.mx", "A.Perez+caja@sub.empresa.com.mx", " x@y.io "),
                                       function(x) is.null(correo_error(x)), logical(1))))
ok("correos inválidos y ataques se rechazan",
   all(vapply(c(list("ana", "ana@", "@x.mx", "a@b", "a b@x.mx", "a@x.mx<script>", "a@x.mx\nBcc: z@z.mx",
                     paste0(strrep("a", 65), "@x.mx"), paste0("a@", strrep("b", 260), ".mx")), ataques),
              function(x) !is.null(correo_error(x)), logical(1))))
ok("correo_normalizar: minúsculas y sin espacios; NA ante lo que no es un texto",
   identical(correo_normalizar(" Ana@X.MX "), "ana@x.mx") && is.na(correo_normalizar(list("a@x.mx"))) &&
   is.na(correo_normalizar(enorme)))

# ── 2. Password ──────────────────────────────────────────────────────────────
ok("contraseñas con 8+, número y símbolo pasan",
   all(vapply(c("abcdef1!", "Contraseña 2026.", "\"'<>{}1a#"), function(x) is.null(contrasena_error(x)),
              logical(1))))
ok("cortas, sin número o sin símbolo: la regla",
   all(vapply(c("abc1!", "abcdefgh!", "abcdefgh1", "", "1234567!"[0]),
              function(x) identical(contrasena_error(x), "cuentas.error.contrasena_regla"), logical(1))))
ok("129 caracteres o 5 MB: demasiado larga, sin llegar a scrypt",
   identical(contrasena_error(paste0(strrep("a", 127), "1!")), "cuentas.error.contrasena_larga") &&
   identical(contrasena_error(enorme), "cuentas.error.contrasena_larga"))
ok("una contraseña que no es un texto no pasa y no truena",
   all(vapply(list(NULL, NA_character_, c("abcdef1!", "x"), list("abcdef1!"), 12345678),
              function(x) !is.null(contrasena_error(x)), logical(1))))

# ── 3. Hidden user name ──────────────────────────────────────────────────────
ok("usuario_ocultar: primera y última letra, o solo la primera",
   identical(usuario_ocultar("luis"), "l…s") && identical(usuario_ocultar("mjnunez"), "m…z") &&
   identical(usuario_ocultar("ana"), "a…") && identical(usuario_ocultar(NULL), "…"))
ok("usuario_ocultar nunca devuelve el usuario completo",
   all(vapply(c("luis", "mjnunez", "ana", "ab", "tesoreria_2026"),
              function(u) !grepl(u, usuario_ocultar(u), fixed = TRUE), logical(1))))

# ── 4. reservar_usuario ──────────────────────────────────────────────────────
idx_key <- "hd-admin/username_index.rds"
assign(idx_key, data.frame(username = c("luis", "viejo"), home_folder = c("networks", "otro"),
                           account_code = c("U0001", "U0002"), created_at = "2026-01-01",
                           active = c(TRUE, FALSE), stringsAsFactors = FALSE), envir = .mem)
ocupado <- function(expr) inherits(tryCatch(expr, error = function(e) e), "usuario_ocupado")
ok("un nombre libre se reserva en el índice global",
   { reservar_usuario("mjnunez", "networks", "U0003")
     idx <- get(idx_key, envir = .mem); any(idx$username == "mjnunez" & idx$active) })
ok("el mismo nombre en OTRO cliente está ocupado", ocupado(reservar_usuario("mjnunez", "otro", "U0001")))
ok("mayúsculas y espacios cuentan como el mismo nombre", ocupado(reservar_usuario("  LUIS ", "otro", "U0009")))
ok("un nombre en las credenciales (sin índice) también está ocupado",
   ocupado(reservar_usuario("claudia", "networks", "U0004", ocupados = c("Claudia", "x"))))
ok("un nombre dado de baja en el índice se puede volver a reservar",
   { reservar_usuario("viejo", "networks", "U0005")
     idx <- get(idx_key, envir = .mem); isTRUE(idx$active[idx$username == "viejo"]) &&
       identical(idx$home_folder[idx$username == "viejo"], "networks") })
ok("si el índice no se puede leer, se rechaza (nunca 'libre') y no se escribe",
   { .mem_falla <<- idx_key; antes <- .mem_escrituras
     r <- tryCatch({ reservar_usuario("nuevo1", "networks", "U0006"); "reservado" }, error = function(e) e)
     .mem_falla <<- character()
     inherits(r, "error") && !inherits(r, "usuario_ocupado") && .mem_escrituras == antes })
ok("un usuario con formato inválido no llega al índice",
   { antes <- .mem_escrituras
     inherits(tryCatch(reservar_usuario("<script>", "networks", "U0007"), error = function(e) e), "error") &&
       .mem_escrituras == antes })

# ── 5. Keys ──────────────────────────────────────────────────────────────────
codigo <- paste(readLines(Sys.getenv("CUENTAS_R", "R/usuarios_cuentas.R"), warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
claves <- unique(regmatches(codigo, gregexpr("cuentas\\.error\\.[a-z_]+", codigo))[[1]])
es <- jsonlite::fromJSON("www/i18n/es.json"); en <- jsonlite::fromJSON("www/i18n/en.json")
ok(sprintf("las %d claves cuentas.error.* existen en es y en", length(claves)),
   length(claves) >= 11L && all(claves %in% names(es)) && all(claves %in% names(en)))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
