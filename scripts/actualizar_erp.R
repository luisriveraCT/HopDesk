# =============================================================================
# scripts/actualizar_erp.R -- T103: refresh a client's ERP data without the app
#
#   Rscript --vanilla scripts/actualizar_erp.R --cliente networks --usuario <user> [options]
#   (from the root of the checkout; scripts/actualizar_erp.bat is the one click)
#
# Does exactly what the app's "Refrescar SAP" does to S3 -- the same
# fetch_all_companies() (R/sap_api.R) and, inside it, the same
# save_sap_snapshot() (R/persistence.R) with its prev1/prev2 rotation and its
# T72 entry -- and nothing else: no Shiny, no Phase 1/2, no observers.
# Meant to run on a machine inside the client's VPN (SAP is not reachable
# from the hosted app).
#
# Options:
#   --cliente <id>     the client (its S3 folder). Required.
#   --usuario <user>   who runs it, as the app's login knows it (usuarios.rds):
#                      the log signs "Name · HD Staff" for staff (T87).
#                      Required except with --prueba.
#   --produccion       write to production. Also needs HOPDESK_PRODUCTION=1
#                      in the environment (the .bat sets both). Without it
#                      the run goes to the dev bucket (.Renviron.local) and
#                      stops if the bucket is not dev's.
#   --renviron <path>  .Renviron to read first (--vanilla reads none).
#   --libros AR,AP     ledgers (default both).
#   --prueba           read the ERP and compare with the current snapshot,
#                      page times included; writes NOTHING (no lock, no log).
#   --forzar           skip the floor against the current snapshot (C4).
#
# Safeguards (T103 paso 0, approved): a login per company first (no VPN =
# clear message, exit 2, nothing written); all companies or nothing (C3);
# the floor (C4); a snapshot that could not be saved is an error (C6) and the
# save is read back; a lock in S3 so two runs never overlap; the offline
# queue is off (hopdesk.sin_cola); credentials are never printed.
#
# Exit codes: 0 ok · 1 usage/setup · 2 no connection to the ERP · 3 lock held
#             4 nothing saved (incomplete, floor, failed save) · 5 saved, but
#             the log entry could not be written.
# Console text is Spanish on purpose (an operator's tool, not app UI).
# =============================================================================

ACTUALIZAR_ERP_ARCHIVOS <- c(
  "R/i18n.R", "R/monedas.R", "R/tier_registry.R", "R/persistence.R", "R/offline_queue.R",
  "R/secrets_encryption.R", "R/data_pipeline.R", "R/app_audit.R", "R/auth.R", "R/sync_bus.R", "R/sap_api.R")

.LOCK_KEY <- "erp_actualizacion_lock.rds"
.LOCK_TTL_HORAS <- 2

# ── Arguments ────────────────────────────────────────────────────────────────
erp_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  valor <- function(nombre) {
    i <- match(nombre, args)
    if (is.na(i) || i == length(args) || startsWith(args[i + 1L], "--")) return(NULL)
    args[i + 1L]
  }
  conocidas <- c("--cliente", "--usuario", "--produccion", "--renviron", "--libros", "--prueba", "--forzar")
  raras <- setdiff(args[startsWith(args, "--")], conocidas)
  if (length(raras)) stop("opción desconocida: ", paste(raras, collapse = ", "), call. = FALSE)
  libros <- toupper(strsplit(valor("--libros") %||% "AR,AP", ",")[[1]])
  if (!length(libros) || !all(libros %in% c("AR", "AP"))) stop("--libros acepta AR, AP o AR,AP", call. = FALSE)
  out <- list(cliente    = tolower(trimws(valor("--cliente") %||% "")),
              usuario    = tolower(trimws(valor("--usuario") %||% "")),
              produccion = "--produccion" %in% args,
              renviron   = valor("--renviron"),
              libros     = unique(libros),
              prueba     = "--prueba" %in% args,
              forzar     = "--forzar" %in% args)
  if (!nzchar(out$cliente)) stop("falta --cliente", call. = FALSE)
  if (!grepl("^[a-z0-9][a-z0-9_-]*$", out$cliente) || identical(out$cliente, "hd-admin"))
    stop("--cliente no es un cliente válido", call. = FALSE)
  if (!out$prueba && !nzchar(out$usuario)) stop("falta --usuario (o usa --prueba)", call. = FALSE)
  out
}

# ── Environment: prod only on purpose ───────────────────────────────────────
# Mirrors R/global.R: production only with HOPDESK_PRODUCTION=1; otherwise
# .Renviron.local (dev) is read. Here both directions are checked: the flag
# and --produccion must agree, and a dev run must really land on another bucket.
erp_entorno <- function(a) {
  if (!is.null(a$renviron)) {
    if (!file.exists(a$renviron)) stop("no existe el archivo de --renviron", call. = FALSE)
    readRenviron(a$renviron)
  }
  prod_flag <- identical(Sys.getenv("HOPDESK_PRODUCTION"), "1")
  if (a$produccion && !prod_flag)
    stop("--produccion pide además HOPDESK_PRODUCTION=1 (el .bat lo pone)", call. = FALSE)
  if (!a$produccion) {
    if (prod_flag) stop("HOPDESK_PRODUCTION=1 sin --produccion: ¿a qué bucket va? Pasa --produccion o quita la variable", call. = FALSE)
    bucket_prod <- Sys.getenv("S3_BUCKET")
    if (!file.exists(".Renviron.local")) stop("sin --produccion hace falta .Renviron.local (el bucket de dev)", call. = FALSE)
    readRenviron(".Renviron.local")
    if (!nzchar(Sys.getenv("S3_BUCKET")) || identical(Sys.getenv("S3_BUCKET"), bucket_prod))
      stop(".Renviron.local no cambia el bucket: no corro contra lo que podría ser prod", call. = FALSE)
  }
  faltan <- c("S3_BUCKET", "AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_DEFAULT_REGION")
  faltan <- faltan[!nzchar(Sys.getenv(faltan))]
  if (length(faltan)) stop("faltan variables: ", paste(faltan, collapse = ", "), call. = FALSE)
  # The offline queue is the app's; a script's failed write fails here (R4).
  options(hopdesk.sin_cola = TRUE,
          hopdesk.sap_timeout_login  = 20,   # VPN: 2 s read as "no connection"
          hopdesk.sap_timeout_pagina = 90,
          hopdesk.sap_reintentos     = 3L)
  invisible(Sys.getenv("S3_BUCKET"))
}

# ── Loading the app's code without the app (as scripts/avisos_job.R) ────────
erp_cargar <- function() {
  suppressPackageStartupMessages({
    for (p in c("dplyr", "tibble", "jsonlite", "aws.s3", "httr", "uuid", "later"))
      library(p, character.only = TRUE)
  })
  assign("%||%", function(a, b) if (is.null(a) || length(a) == 0L) b else a, envir = globalenv())
  for (e in parse("R/global.R", keep.source = FALSE))
    if (is.call(e) && identical(e[[1]], as.name("<-")) &&
        as.character(e[[2]])[1] %in% c("S3_KEYS", "S3_KEYS_CRITICAL"))
      eval(e, globalenv())
  for (f in ACTUALIZAR_ERP_ARCHIVOS) sys.source(f, envir = globalenv())
  invisible(TRUE)
}

# ── Small pieces ─────────────────────────────────────────────────────────────
.sin_url <- function(x) gsub("https?://[^[:space:]\"']+", "<url>", x)
erp_decir <- function(...) { cat(sprintf(...), "\n", sep = ""); utils::flush.console() }
.miles <- function(n) format(n, big.mark = ",", scientific = FALSE, trim = TRUE)

# The companies of the client, initials -> name: the same map the app builds.
erp_empresas <- function(cid) {
  emp <- .s3_leer_para_escribir(S3_KEYS_CRITICAL$empresas, cid)   # strict
  cmap <- .company_map_from_empresas(emp)
  if (!length(cmap)) stop("el cliente no tiene empresas activas", call. = FALSE)
  cmap
}

# The user's home folder (hd-admin for staff), from the SAME source and rule
# as the app's login (T103c): .load_or_init_credentials() on the shared
# hd-admin deployment reads hd-admin/usuarios.rds and each client's
# usuarios.rds without staff tiers, matches tolower(trimws(username)) and
# drops deleted rows; the home is the row's client_id, or its folder when
# empty (.normalize_credentials(), and home_client_id() at sign-in). Also
# refused: activo = FALSE, and a name on the emergency lock list (as
# auth_check_credentials()). hd-admin/username_index.rds is NOT read: accounts
# older than Stage 2 (e.g. "dev") sign in but are missing there.
# Every read is strict: a failed read stops, never "no such user".
erp_casa_usuario <- function(usuario, cid) {
  usuario <- tolower(trimws(usuario))
  fila <- NULL
  for (carpeta in unique(c(.HD_ADMIN_CID, tolower(cid)))) {
    u <- .usuarios_leer_estricto(paste0(carpeta, "/usuarios.rds"))   # NULL = no file
    if (!is.data.frame(u) || !nrow(u) || !"username" %in% names(u)) next
    vivo <- !(if ("deleted" %in% names(u)) u$deleted %in% TRUE else rep(FALSE, nrow(u)))
    if (carpeta != .HD_ADMIN_CID) vivo <- vivo & !(u$tier %in% niveles_staff())
    i <- which(vivo & tolower(trimws(as.character(u$username))) %in% usuario)
    if (length(i)) { fila <- u[i[1], , drop = FALSE]; fila$.carpeta <- carpeta; break }
  }
  if (is.null(fila)) stop("el usuario no existe", call. = FALSE)
  if ("activo" %in% names(fila) && fila$activo %in% FALSE) stop("el usuario está desactivado", call. = FALSE)
  lock <- read_emergency_lock()                                      # strict on a first read
  if (is.data.frame(lock) && nrow(lock) && usuario %in% tolower(trimws(as.character(lock$username))))
    stop("el usuario está bloqueado", call. = FALSE)
  casa <- if ("client_id" %in% names(fila)) as.character(fila$client_id) else NA_character_
  if (is.na(casa) || !nzchar(trimws(casa))) casa <- fila$.carpeta
  tolower(trimws(casa))
}

# ── Lock (S3, outside the tables: written directly, not through .s3_write,
#    so it never logs a T72 row nor counts as client data) ──────────────────
erp_lock_tomar <- function(cid, usuario) {
  key <- .s3_key(.LOCK_KEY, client_id = cid); b <- .s3_bucket()
  existe <- s3_objeto_existe(key, b)
  if (is.na(existe)) stop("no pude revisar el candado en S3", call. = FALSE)
  if (isTRUE(existe)) {
    l <- tryCatch(aws.s3::s3readRDS(object = key, bucket = b), error = function(e) NULL)
    edad <- if (is.null(l$inicio)) Inf else as.numeric(difftime(Sys.time(), l$inicio, units = "hours"))
    if (!isTRUE(l$liberado) && edad < .LOCK_TTL_HORAS)
      return(structure(FALSE, quien = l$usuario %||% "?", desde = l$inicio))
    if (!isTRUE(l$liberado)) erp_decir("  (había un candado de hace %.1f h, vencido: lo reemplazo)", edad)
  }
  token <- uuid::UUIDgenerate()
  aws.s3::s3saveRDS(list(token = token, usuario = usuario, inicio = Sys.time()), object = key, bucket = b)
  structure(TRUE, token = token)
}
# Released by marking it, not deleting it: only this run's own token, and
# no delete permission needed. A run cut hard (power, a killed window) leaves
# it held; it expires after .LOCK_TTL_HORAS.
erp_lock_soltar <- function(cid, token) {
  key <- .s3_key(.LOCK_KEY, client_id = cid); b <- .s3_bucket()
  l <- tryCatch(aws.s3::s3readRDS(object = key, bucket = b), error = function(e) NULL)
  if (identical(l$token, token))
    tryCatch(aws.s3::s3saveRDS(c(l[setdiff(names(l), "liberado")], list(liberado = TRUE, fin = Sys.time())),
                               object = key, bucket = b),
             error = function(e) erp_decir("  AVISO: no pude soltar el candado (vence solo en %d h)", .LOCK_TTL_HORAS))
  invisible(TRUE)
}

# ── Preflight: one login per company with credentials ───────────────────────
# Returns the initials that answered; stops with class erp_sin_conexion on
# the first transport failure (no VPN).
erp_preflight <- function(cmap, cid) {
  con <- character(0)
  for (i in names(cmap)) {
    url <- .sap_url_configurada(i, client_id = cid)
    if (!nzchar(url)) { erp_decir("  %-5s sin credenciales de ERP: se omite (como en la app)", i); next }
    t0 <- proc.time()
    r <- tryCatch({ sap_login(i, client_id = cid); TRUE },
                  sap_connection_error = function(e) e, error = function(e) e)
    if (inherits(r, "sap_connection_error"))
      stop(structure(class = c("erp_sin_conexion", "error", "condition"),
                     list(message = sprintf("No hay conexión con el ERP de %s: ¿está la VPN arriba? (%s)",
                                            i, .sin_url(conditionMessage(r))), call = NULL)))
    if (inherits(r, "error"))
      stop(sprintf("El ERP de %s rechazó el inicio de sesión: %s", i, .sin_url(conditionMessage(r))), call. = FALSE)
    erp_decir("  %-5s conectado (%.1f s)", i, (proc.time() - t0)[["elapsed"]])
    con <- c(con, i)
  }
  if (!length(con)) stop("ninguna empresa del cliente tiene credenciales de ERP", call. = FALSE)
  con
}

# ── One ledger ───────────────────────────────────────────────────────────────
erp_libro <- function(libro, cmap, cid, a) {
  options(hopdesk.sap_progreso = function(p)
    erp_decir("  %s · %-5s · p%-4d · %8s filas · %5.1f s  (%.0f s)", libro, p$empresa, p$pagina,
              .miles(p$acumulado), p$segundos, p$segundos_total))
  on.exit(options(hopdesk.sap_progreso = NULL), add = TRUE)
  res <- fetch_all_companies(libro, cmap, client_id = cid,
                             exigir_todas = TRUE, piso = !a$forzar,
                             guardar = !a$prueba, exigir_guardado = TRUE)
  r <- sap_ultimo_resumen(libro)
  for (o in r$empresas)
    erp_decir("  %s · %-5s · %-10s %8s filas%s", libro, o$initials, o$status, .miles(o$rows),
              if (!is.null(o$seconds)) sprintf(" · %.0f s", o$seconds) else "")
  if (a$prueba) {
    key <- if (libro == "AR") S3_KEYS_CRITICAL$sap_snap_ar else S3_KEYS_CRITICAL$sap_snap_ap
    actual <- tryCatch(.s3_leer_para_escribir(key, cid), error = function(e) NULL)
    antes <- if (is.null(actual$data)) table(character(0)) else table(actual$data$Empresa)
    despues <- if (is.null(res)) table(character(0)) else table(res$Empresa)
    for (emp in union(names(antes), names(despues)))
      erp_decir("  %s · %-28s foto actual %8s → ERP hoy %8s", libro, emp,
                .miles(if (emp %in% names(antes)) as.integer(antes[[emp]]) else 0L),
                .miles(if (emp %in% names(despues)) as.integer(despues[[emp]]) else 0L))
    if (!is.null(actual$saved_at))
      erp_decir("  %s · foto actual guardada el %s", libro, format(actual$saved_at, "%d/%m/%Y %H:%M"))
    v <- .sap_piso_violaciones(res, actual$data)
    if (length(v)) erp_decir("  %s · AVISO: el piso detendría el guardado: %s", libro, paste(v, collapse = "; "))
    return(r)
  }
  if (is.null(res)) stop(structure(class = c("erp_vacio", "error", "condition"),
                                   list(message = sprintf("%s: el ERP no devolvió facturas abiertas; no se guardó nada", libro), call = NULL)))
  # C6: read it back -- the saved_at and the rows must be this run's.
  key <- if (libro == "AR") S3_KEYS_CRITICAL$sap_snap_ar else S3_KEYS_CRITICAL$sap_snap_ap
  leida <- .s3_leer_para_escribir(key, cid)
  if (is.null(leida) || !identical(nrow(leida$data), nrow(res)) ||
      as.numeric(difftime(Sys.time(), leida$saved_at, units = "mins")) > 30)
    stop(structure(class = c("foto_no_guardada", "error", "condition"),
                   list(message = sprintf("%s: la foto leída de vuelta no es la de esta corrida", libro), call = NULL)))
  erp_decir("  %s guardado: %s filas, comprobado en S3", libro, .miles(nrow(res)))
  # F2: the open sessions re-read it on their next poll (8 s), unless the signal failed.
  erp_decir(if (isTRUE(r$avisado)) "  %s: las sesiones abiertas lo verán en segundos"
            else "  %s: AVISO, no se pudo avisar a las sesiones abiertas: que recarguen la página", libro)
  r
}

# ── The log (T72/T87): one erp_actualizado per ledger saved ─────────────────
erp_registrar <- function(r, cid, usuario, casa) {
  emp <- vapply(r$empresas, function(o) sprintf("%s:%d", o$initials, as.integer(o$rows)), "")
  log_action(user = usuario, module = "erp", action = "erp_actualizado",
             description = sprintf("Datos del ERP actualizados (%s): %s facturas abiertas", r$ledger, .miles(r$filas)),
             target_id = r$ledger,
             metadata = list(libro = r$ledger, filas = r$filas, empresas = unname(emp),
                             segundos = r$segundos, origen = "actualizar_erp"),
             client_id = cid, viewer_home_client_id = casa)
}
# Flushes the log now and waits for later's retries; TRUE when nothing is pending.
erp_vaciar_registro <- function(max_seg = 90) {
  .audit_flush_ahora()
  t0 <- proc.time()
  while (!later::loop_empty() && (proc.time() - t0)[["elapsed"]] < max_seg) later::run_now(1)
  pend <- vapply(ls(.audit_pending), function(k) NROW(.audit_pending[[k]]), 0L)
  !length(pend) || all(pend == 0L)
}

# ── Main ─────────────────────────────────────────────────────────────────────
erp_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  salir <- function(codigo, ...) { if (length(list(...))) erp_decir(...); return(invisible(codigo)) }
  if (!file.exists("R/sap_api.R")) return(salir(1L, "Córrelo desde la raíz del checkout (donde está R/)."))
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
  assign("%||%", `%||%`, envir = globalenv())
  a <- tryCatch(erp_args(args), error = function(e) e)
  if (inherits(a, "error")) return(salir(1L, "Uso: %s", conditionMessage(a)))
  bucket <- tryCatch(erp_entorno(a), error = function(e) e)
  if (inherits(bucket, "error")) return(salir(1L, "Entorno: %s", conditionMessage(bucket)))

  dir_logs <- getOption("hopdesk.erp_logs_dir", "logs")   # a test points it to a temp dir
  bitacora <- file.path(dir_logs, sprintf("actualizar_erp_%s_%s.log", a$cliente, format(Sys.time(), "%Y%m%d_%H%M%S")))
  dir.create(dir_logs, showWarnings = FALSE, recursive = TRUE)
  con <- file(bitacora, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  # The app's own messages and warnings ([SAP], [S3_WRITE]...) go to the
  # local log file, with any URL masked; the console gets only this script's lines.
  al_log <- function(m) { writeLines(.sin_url(sub("\n$", "", conditionMessage(m))), con); flush(con); invokeRestart("muffleMessage") }

  withCallingHandlers({
    erp_cargar()
    erp_decir("Actualizador del ERP · cliente %s · bucket %s%s", a$cliente, bucket,
              if (a$prueba) " · PRUEBA (no escribe nada)" else "")
    t0 <- proc.time()
    casa <- NULL
    if (!a$prueba) {
      casa <- tryCatch(erp_casa_usuario(a$usuario, a$cliente), error = function(e) e)
      if (inherits(casa, "error")) return(salir(1L, "Usuario: %s", conditionMessage(casa)))
    }
    cmap <- tryCatch(erp_empresas(a$cliente), error = function(e) e)
    if (inherits(cmap, "error")) return(salir(1L, "Empresas: %s", conditionMessage(cmap)))
    erp_decir("Empresas: %s", paste(names(cmap), collapse = ", "))

    erp_decir("Conexión con el ERP:")
    pf <- tryCatch(erp_preflight(cmap, a$cliente), erp_sin_conexion = function(e) e, error = function(e) e)
    if (inherits(pf, "erp_sin_conexion")) return(salir(2L, "%s\nNo se escribió nada.", conditionMessage(pf)))
    if (inherits(pf, "error")) return(salir(2L, "%s\nNo se escribió nada.", .sin_url(conditionMessage(pf))))

    token <- NULL
    if (!a$prueba) {
      lk <- tryCatch(erp_lock_tomar(a$cliente, a$usuario), error = function(e) e)
      if (inherits(lk, "error")) return(salir(1L, "Candado: %s", conditionMessage(lk)))
      if (!isTRUE(lk)) return(salir(3L, "Ya hay una actualización corriendo (%s, desde %s). No se escribió nada.",
                                     attr(lk, "quien"), format(attr(lk, "desde"), "%H:%M")))
      token <- attr(lk, "token")
    }

    hechos <- list(); codigo <- 0L
    # The lock is released whatever happens below (an error, Ctrl+C):
    # finally, not on.exit -- this block is a promise of withCallingHandlers.
    tryCatch({
    for (libro in a$libros) {
      erp_decir("%s:", if (libro == "AR") "Cuentas por cobrar (AR)" else "Cuentas por pagar (AP)")
      r <- tryCatch(erp_libro(libro, cmap, a$cliente, a),
                    sap_no_connection = function(e) e, erp_incompleto = function(e) e,
                    erp_piso = function(e) e, foto_no_guardada = function(e) e,
                    erp_vacio = function(e) e, error = function(e) e)
      if (inherits(r, "error")) {
        codigo <- if (inherits(r, "sap_no_connection")) 2L else 4L
        erp_decir("  %s NO se guardó: %s", libro, .sin_url(conditionMessage(r)))
        if (inherits(r, "erp_piso")) erp_decir("  (si la baja es real, vuelve a correr con --forzar)")
        next
      }
      if (!a$prueba) hechos[[libro]] <- r
    }
    }, finally = if (!is.null(token)) erp_lock_soltar(a$cliente, token))

    if (length(hechos)) {
      for (r in hechos) tryCatch(erp_registrar(r, a$cliente, a$usuario, casa),
                                 error = function(e) erp_decir("  registro: %s", conditionMessage(e)))
      if (!erp_vaciar_registro()) {
        erp_decir("AVISO: los datos se guardaron, pero el registro de la actualización no se pudo escribir.")
        if (codigo == 0L) codigo <- 5L
      }
    }
    erp_decir("%s en %.0f s · bitácora: %s",
              if (codigo == 0L) (if (a$prueba) "Prueba terminada" else "Listo") else sprintf("Terminó con código %d", codigo),
              (proc.time() - t0)[["elapsed"]], bitacora)
    invisible(codigo)
  }, message = al_log, warning = function(w) {
    writeLines(.sin_url(paste("[warning]", conditionMessage(w))), con); flush(con); invokeRestart("muffleWarning")
  })
}

if (sys.nframe() == 0L) quit(status = as.integer(erp_main()), save = "no")
