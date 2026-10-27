# =============================================================================
# scripts/porkbun_dns_resend.R
#
# Crea en Porkbun, via su API, los registros DNS que Resend pide para
# verificar un dominio de envio.
#
# POR QUE NO LOS TRAE CODIFICADOS: Resend cambio de esquema. Los dominios
# dados de alta ANTES de agosto de 2026 reciben "MX + TXT de SPF"; los
# posteriores reciben DOS CNAME (send.* y rsend.*). Un script que asuma uno de
# los dos escribe registros que se ven bien y nunca verifican. Por eso lee lo
# que el panel de Resend muestra de verdad, desde:
#
#     scripts/resend_dns_records.tsv
#
# Ese archivo es tab-separated, con encabezado:  type  host  content  prio
# Se copia tal cual de la pestania "Fill in your DNS Records" de Resend.
# El campo `prio` va vacio salvo en MX.
#
# SIN SECRETOS EN ESTE ARCHIVO. Las llaves salen de .Renviron (git-ignored):
#
#   PORKBUN_API_KEY      pk1_...   porkbun.com/account/api
#   PORKBUN_SECRET_KEY   sk1_...   se muestra UNA sola vez
#
# Porkbun ademas exige API ACCESS = ON en el dominio
# (Domain Management -> Details -> API ACCESS).
#
# USO, desde la raiz del repo:
#   Rscript --vanilla scripts/porkbun_dns_resend.R            # prueba, no escribe
#   Rscript --vanilla scripts/porkbun_dns_resend.R --apply    # escribe
#
# Idempotente: si el registro ya existe con el valor correcto lo omite. Si
# existe con OTRO valor lo reporta y NO lo pisa -- reemplazar algo es decision
# tuya, no del script.
# =============================================================================

readRenviron(".Renviron")

`%||%` <- function(a, b) if (!is.null(a) && length(a)) a else b

APPLY       <- "--apply" %in% commandArgs(trailingOnly = TRUE)
ZONE        <- "hopdesk.app"                       # el dominio registrado en Porkbun
RECORDS_TSV <- "scripts/resend_dns_records.tsv"

.fail <- function(...) { message("\n[ERROR] ", ...); quit(status = 1L) }

API_KEY    <- trimws(Sys.getenv("PORKBUN_API_KEY"))
SECRET_KEY <- trimws(Sys.getenv("PORKBUN_SECRET_KEY"))

if (!nzchar(API_KEY) || !nzchar(SECRET_KEY))
  .fail("Faltan PORKBUN_API_KEY / PORKBUN_SECRET_KEY en .Renviron.\n",
        "        Crealas en https://porkbun.com/account/api")
if (!grepl("^pk1_", API_KEY))
  .fail("PORKBUN_API_KEY no empieza con 'pk1_'. Puede que hayas intercambiado\n",
        "        la llave con el secreto.")
if (!grepl("^sk1_", SECRET_KEY))
  .fail("PORKBUN_SECRET_KEY no empieza con 'sk1_'.")
if (!file.exists(RECORDS_TSV))
  .fail("No existe ", RECORDS_TSV, ".")

# ── Leer y validar los registros ─────────────────────────────────────────────
recs <- tryCatch(
  utils::read.delim(RECORDS_TSV, stringsAsFactors = FALSE, comment.char = "#",
                    colClasses = "character", na.strings = character(0)),
  error = function(e) .fail("No se pudo leer ", RECORDS_TSV, ": ", conditionMessage(e))
)
for (col in c("type", "host", "content"))
  if (!col %in% names(recs)) .fail(RECORDS_TSV, " no tiene la columna '", col, "'.")
if (!"prio" %in% names(recs)) recs$prio <- ""
if (!nrow(recs)) .fail(RECORDS_TSV, " no tiene ninguna fila.")

recs[] <- lapply(recs, trimws)
recs$type <- toupper(recs$type)

# Porkbun quiere el host RELATIVO a la zona. El panel de Resend a veces lo
# muestra relativo ("send.mail") y a veces completo ("send.mail.hopdesk.app").
# Normalizar aqui evita el error clasico: "send.mail.hopdesk.app.hopdesk.app".
.rel <- function(host) {
  host <- sub("\\.$", "", host)
  if (identical(host, ZONE) || !nzchar(host)) return("")
  suffix <- paste0(".", ZONE)
  if (endsWith(host, suffix)) substr(host, 1L, nchar(host) - nchar(suffix)) else host
}

VALID <- c("TXT", "CNAME", "MX", "A", "AAAA")
for (i in seq_len(nrow(recs))) {
  if (!recs$type[i] %in% VALID)
    .fail("Fila ", i, ": tipo '", recs$type[i], "' no soportado.")
  if (!nzchar(recs$content[i]))
    .fail("Fila ", i, ": 'content' vacio.")
  if (identical(recs$type[i], "MX") && !nzchar(recs$prio[i]))
    .fail("Fila ", i, ": un registro MX necesita 'prio'.")
  # Resend corta los valores con "..." en pantalla; pegarlos asi es el error
  # silencioso mas caro de este tramite.
  # "..." o el caracter de elipsis, suelto o entre corchetes como lo pinta Resend.
  if (grepl("\\.\\.\\.|…|PEGAR_AQUI", recs$content[i]))
    .fail("Fila ", i, " (", recs$type[i], " ", recs$host[i], "): el valor trae '...'.\n",
          "        Esta truncado como lo muestra la pantalla de Resend. Usa el\n",
          "        boton de copiar de Resend para obtener el valor completo.")
}
recs$name <- vapply(recs$host, .rel, character(1), USE.NAMES = FALSE)

# ── API de Porkbun ───────────────────────────────────────────────────────────
.post <- function(path, body = list()) {
  resp <- httr::POST(
    url = paste0("https://api.porkbun.com/api/json/v3", path),
    httr::add_headers(`X-API-Key` = API_KEY, `X-Secret-API-Key` = SECRET_KEY,
                      `Content-Type` = "application/json"),
    body = jsonlite::toJSON(body, auto_unbox = TRUE), encode = "raw"
  )
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE),
                     error = function(e) list(status = "ERROR", message = txt))
  parsed$.http <- httr::status_code(resp)
  parsed
}

cat("== Porkbun ==\n")
ping <- .post("/ping")
if (!identical(ping$status, "SUCCESS"))
  .fail("No se pudo autenticar (HTTP ", ping$.http, "): ",
        ping$message %||% "sin detalle", "\n",
        "        Revisa las llaves y que API ACCESS este ON para ", ZONE, ".")
cat("   autenticado OK\n")

# Escribir en una zona cuyos nameservers no son de Porkbun "funciona" pero no
# resuelve nada: fallar aqui en vez de dejar registros muertos.
pre <- .post(paste0("/dns/preflight/", ZONE))
if (identical(pre$status, "SUCCESS") && isFALSE(pre$`nameservers-ours`))
  .fail("Los nameservers de ", ZONE, " no son de Porkbun.")

cat("\n== ", nrow(recs), " registro(s) desde ", RECORDS_TSV, " ==\n", sep = "")
if (!APPLY) cat("   MODO PRUEBA -- no se escribe nada. Usa --apply para aplicar.\n")

created <- 0L; skipped <- 0L; problems <- 0L

for (i in seq_len(nrow(recs))) {
  r <- recs[i, ]
  cat("\n-- ", r$type, "  ", r$host, "\n", sep = "")
  cat("   host en Porkbun : '", r$name, "'\n", sep = "")
  cat("   valor           : ", substr(r$content, 1, 66),
      if (nchar(r$content) > 66) "..." else "", "\n", sep = "")

  existing <- .post(sprintf("/dns/retrieveByNameType/%s/%s/%s", ZONE, r$type, r$name))
  if (identical(existing$status, "SUCCESS") && length(existing$records)) {
    same <- FALSE
    for (e in existing$records)
      if (identical(tolower(trimws(e$content)), tolower(trimws(r$content)))) same <- TRUE
    if (same) {
      cat("   >> ya existe con el valor correcto, se omite\n")
      skipped <- skipped + 1L
      next
    }
    cat("   >> YA EXISTE un ", r$type, " en ese host con OTRO valor:\n", sep = "")
    for (e in existing$records) cat("      - ", substr(e$content, 1, 66), "\n", sep = "")
    cat("      No se toca. Borralo a mano si quieres reemplazarlo.\n")
    problems <- problems + 1L
    next
  }

  if (!APPLY) { cat("   >> se crearia\n"); next }

  body <- list(name = r$name, type = r$type, content = r$content, ttl = 600)
  if (nzchar(r$prio)) body$prio <- as.integer(r$prio)
  res <- .post(paste0("/dns/create/", ZONE), body)
  if (identical(res$status, "SUCCESS")) {
    cat("   >> creado (id ", res$id %||% "?", ")\n", sep = "")
    if (length(res$warnings)) for (w in res$warnings) cat("      aviso: ", w, "\n", sep = "")
    created <- created + 1L
  } else {
    cat("   >> FALLO (HTTP ", res$.http, "): ", res$message %||% "sin detalle", "\n", sep = "")
    problems <- problems + 1L
  }
}

cat("\n== Resumen ==\n")
cat("   creados   : ", created,  "\n", sep = "")
cat("   omitidos  : ", skipped,  "  (ya estaban correctos)\n", sep = "")
cat("   problemas : ", problems, "\n", sep = "")
if (!APPLY) {
  cat("\n   Nada se escribio. Vuelve a correrlo con --apply.\n")
} else if (problems == 0L) {
  cat("\n   Listo. Dale 'Verify DNS Records' en https://resend.com/domains\n")
}
