# =============================================================================
# R/sap_api.R
# SAP Business One Service Layer client.
# Handles: session auth, querying AR/AP invoices, response parsing.
# One session per company — credentials stored in environment variables.
#
# ENV VAR convention (set these per deployment, never commit to git):
#   SAP_{INITIALS}_URL       e.g. SAP_NG_URL=https://myserver:50000/b1s/v1
#   SAP_{INITIALS}_COMPANY   e.g. SAP_NG_COMPANY=MyDB
#   SAP_{INITIALS}_USER      e.g. SAP_NG_USER=manager
#   SAP_{INITIALS}_PASSWORD  e.g. SAP_NG_PASSWORD=secret
# =============================================================================

# ── Session cache ──────────────────────────────────────────────────────────────
# Stored as a named list: initials → list(cookie, expires_at)
# Sessions expire after 30 min (SAP default); we refresh 5 min early.

.sap_sessions <- new.env(parent = emptyenv())

.session_valid <- function(initials) {
  s <- .sap_sessions[[initials]]
  !is.null(s) && Sys.time() < (s$expires_at - 300)
}

# ── Credential loader (Stage 3b: fallback-aware) ────────────────────────────────
#
# Resolution order:
#   1. An active, non-deleted sap_b1_service_layer connection in this client's
#      erp_connections.rds store (Stage 3), matching `initials` in its
#      company_initials array — decrypted via decrypt_secret().
#   2. If a store row exists but fails to decrypt (wrong key, corrupted blob,
#      anything): log a LOUD warning and fall through to (3) exactly as if no
#      row had been found. A decrypt failure must never crash the fetch —
#      only be visible in the logs.
#   3. Legacy SAP_{INITIALS}_* env vars — unchanged, still the fallback safety
#      net. Do not remove this path; that is a distinct future decision, not
#      this stage's call to make.
#   4. Neither has anything: same "Missing SAP credentials" error as before —
#      no change to that failure mode.
#
# `client_id`: the caller (ultimately app.R's load_sap_data(), which has
# access to the session's effective_client_id()) passes this down explicitly.
# NULL falls back to Sys.getenv("CLIENT_ID") so every existing caller that
# doesn't pass it (tests, other call sites) keeps today's exact behavior.
.sap_creds <- function(initials, client_id = NULL) {
  i   <- toupper(initials)
  cid <- client_id %||% Sys.getenv("CLIENT_ID")

  store_creds <- if (nzchar(cid)) .sap_creds_from_store(i, cid) else NULL
  if (!is.null(store_creds)) return(store_creds)

  .sap_creds_from_env(i)
}

# In-memory record of which path last served each company's credentials —
# not a new persisted tracking mechanism, just what .sap_creds() itself just
# decided, kept around so the UI can show it (Stage 3b §3, source indicator).
# Per-process only: on a multi-replica deployment each replica has its own.
.sap_creds_source_log <- new.env(parent = emptyenv())

#' Last-known credential source for a company, or NULL if never resolved in
#' this process. `source` is "store" or "legacy"; `at` is a POSIXct.
sap_creds_source <- function(initials) {
  .sap_creds_source_log[[toupper(initials)]]
}

.sap_creds_record_source <- function(i, source) {
  assign(i, list(source = source, at = Sys.time()), envir = .sap_creds_source_log)
}

# Returns a creds list resolved from erp_connections.rds, or NULL to signal
# "fall through to the legacy env var path" — covers both "no matching row"
# and "row found but failed to decrypt" (the latter also logs a loud warning).
# The active SAP B1 connection row of company `i` in client `cid`'s
# erp_connections store, or NULL (no store, no row).
.sap_fila_conexion <- function(i, cid) {
  rows <- tryCatch(load_erp_connections(cid), error = function(e) NULL)
  if (is.null(rows) || !nrow(rows)) return(NULL)

  matches <- vapply(rows$company_initials, function(x) {
    inits <- tryCatch(jsonlite::fromJSON(x), error = function(e) character(0))
    isTRUE(i %in% inits)
  }, logical(1))
  active_ok  <- !is.na(rows$active) & rows$active == TRUE
  candidates <- rows[matches & rows$erp_type == "sap_b1_service_layer" & active_ok, , drop = FALSE]
  if (!nrow(candidates)) return(NULL)
  candidates[1, ]
}

.sap_creds_from_store <- function(i, cid) {
  row <- .sap_fila_conexion(i, cid)
  if (is.null(row)) return(NULL)

  config  <- tryCatch(jsonlite::fromJSON(row$config[1]), error = function(e) NULL)
  secrets <- tryCatch(decrypt_secret(row$secrets_encrypted[1]), error = function(e) {
    message(sprintf(
      "[SAP] WARNING: erp_connections credential for %s failed to decrypt (%s) — falling back to legacy .Renviron credentials. Investigate.",
      i, conditionMessage(e)
    ))
    NULL
  })
  if (is.null(config) || is.null(secrets)) return(NULL)

  ssl_verify <- if (is.null(config$ssl_verify)) TRUE else isTRUE(config$ssl_verify) ||
    identical(tolower(as.character(config$ssl_verify)), "true")

  message(sprintf("[SAP] credentials for %s resolved from erp_connections store", i))
  .sap_creds_record_source(i, "store")
  list(url = config$url, company = config$company, user = secrets$user,
       password = secrets$password, ssl_verify = ssl_verify)
}

.sap_creds_from_env <- function(i) {
  url      <- Sys.getenv(paste0("SAP_", i, "_URL"))
  company  <- Sys.getenv(paste0("SAP_", i, "_COMPANY"))
  user     <- Sys.getenv(paste0("SAP_", i, "_USER"))
  password <- Sys.getenv(paste0("SAP_", i, "_PASSWORD"))
  # SAP_{INITIALS}_SSL_VERIFY=false  → disable peer verification (on-prem / self-signed certs)
  # Defaults to TRUE (verify) — recommended for cloud / production deployments with valid certs
  ssl_raw  <- tolower(trimws(Sys.getenv(paste0("SAP_", i, "_SSL_VERIFY"), unset = "true")))
  ssl_verify <- !(ssl_raw %in% c("false", "0", "no"))

  missing <- c(
    if (!nzchar(url))      paste0("SAP_", i, "_URL"),
    if (!nzchar(company))  paste0("SAP_", i, "_COMPANY"),
    if (!nzchar(user))     paste0("SAP_", i, "_USER"),
    if (!nzchar(password)) paste0("SAP_", i, "_PASSWORD")
  )
  if (length(missing))
    stop("Missing SAP credentials for ", i, ": ", paste(missing, collapse = ", "), call. = FALSE)

  message(sprintf("[SAP] credentials for %s resolved from legacy .Renviron (no erp_connections row / decrypt failed)", i))
  .sap_creds_record_source(i, "legacy")
  list(url = url, company = company, user = user, password = password,
       ssl_verify = ssl_verify)
}

# ── Authentication ─────────────────────────────────────────────────────────────
#
# .sap_login_raw()/.sap_logout_raw() take credentials as explicit arguments
# instead of resolving them from SAP_{INITIALS}_* env vars. sap_login()/
# sap_logout() below are the legacy env-var-driven entry points, rewritten
# to call these low-level functions so there is exactly one HTTP
# login/logout implementation (Stage 3 spec §2: "reuse, don't reimplement").
# The legacy env-var path stays in place (not yet removed — see
# docs/saas_rebuild/STAGE_3_ERP_CREDENTIALS.md §6) until the new
# credential-store path has been proven end to end against live SAP data
# and Mouse has signed off.

.sap_login_raw <- function(url, company, user, password, ssl_verify = TRUE, label = NULL) {
  who <- if (!is.null(label)) paste0(" for ", label) else ""

  resp <- tryCatch(
    httr::POST(
      url    = paste0(url, "/Login"),
      httr::content_type_json(),
      # Total timeout 2s in the app (kept for when SAP is reachable). T103:
      # scripts/actualizar_erp.R raises it -- over a slow VPN 2s reads as "no
      # connection" when SAP is there.
      httr::timeout(getOption("hopdesk.sap_timeout_login", 2)),
      httr::config(
        ssl_verifypeer   = ssl_verify,
        connecttimeout   = 0.8   # TCP connect: fail fast when host is unreachable
      ),
      body   = jsonlite::toJSON(list(
        CompanyDB = company,
        UserName  = user,
        Password  = password
      ), auto_unbox = TRUE)
    ),
    error = function(e) {
      stop(structure(
        list(message = e$message, call = NULL),
        class = c("sap_connection_error", "error", "condition")
      ))
    }
  )

  if (httr::http_error(resp)) {
    stop("SAP login failed", who, ": ", httr::http_status(resp)$message, call. = FALSE)
  }

  cookie <- httr::cookies(resp)
  session_id <- cookie$value[cookie$name == "B1SESSION"]
  if (!length(session_id))
    stop("SAP login", who, " returned no session cookie.", call. = FALSE)

  list(
    cookie     = paste0("B1SESSION=", session_id),
    base_url   = url,
    ssl_verify = ssl_verify,
    expires_at = Sys.time() + 1800   # 30 min
  )
}

.sap_logout_raw <- function(session) {
  tryCatch(
    httr::POST(paste0(session$base_url, "/Logout"),
               httr::add_headers(Cookie = session$cookie),
               httr::config(ssl_verifypeer = session$ssl_verify)),
    error = function(e) NULL
  )
  invisible(TRUE)
}

sap_login <- function(initials, client_id = NULL) {
  creds <- .sap_creds(initials, client_id = client_id)
  .sap_sessions[[initials]] <- .sap_login_raw(
    url = creds$url, company = creds$company, user = creds$user,
    password = creds$password, ssl_verify = creds$ssl_verify, label = initials
  )
  invisible(TRUE)
}

sap_logout <- function(initials) {
  s <- .sap_sessions[[initials]]
  if (is.null(s)) return(invisible(NULL))
  .sap_logout_raw(s)
  rm(list = initials, envir = .sap_sessions)
  invisible(TRUE)
}

#' test_fn for erp_type "sap_b1_service_layer" (R/erp_connector_registry.R).
#' One-shot login against the given non-secret config + decrypted secrets,
#' logs out immediately, never throws — always returns list(ok=, message=).
test_sap_b1_service_layer_connection <- function(config, secrets) {
  ssl_verify <- if (is.null(config$ssl_verify)) TRUE else isTRUE(config$ssl_verify) ||
    identical(tolower(as.character(config$ssl_verify)), "true")

  tryCatch({
    session <- .sap_login_raw(
      url        = config$url,
      company    = config$company,
      user       = secrets$user,
      password   = secrets$password,
      ssl_verify = ssl_verify,
      label      = config$company
    )
    .sap_logout_raw(session)
    list(ok = TRUE, message = "Conexión exitosa.")
  }, error = function(e) {
    list(ok = FALSE, message = paste0("Error de conexión: ", conditionMessage(e)))
  })
}

.ensure_session <- function(initials, client_id = NULL) {
  if (!.session_valid(initials)) sap_login(initials, client_id = client_id)
}

# ── Low-level GET with pagination ──────────────────────────────────────────────

.sap_get_all <- function(initials, endpoint, filter = NULL, select = NULL,
                          top = 500, client_id = NULL) {
  .ensure_session(initials, client_id = client_id)
  s <- .sap_sessions[[initials]]

  # Single GET attempt — returns parsed body or stops.
  # Transport failures (timeout, DNS, refused) are re-thrown as sap_connection_error.
  # T103 C1: without `Prefer: odata.maxpagesize` the Service Layer answers
  # with its own PageSize (20 by default) whatever $top says -- N/20 round
  # trips per company and ledger, each with a deeper $skip. The header asks
  # for pages of `top`; a server that caps lower still works (page_size is
  # learned from the first answer, below).
  # T103 C2: the page timeout (8s in the app) and the retries after a
  # timeout (none in the app) come from options; scripts/actualizar_erp.R
  # raises both, so one slow late page no longer throws the whole ledger away.
  timeout_pagina <- getOption("hopdesk.sap_timeout_pagina", 8)
  reintentos     <- as.integer(getOption("hopdesk.sap_reintentos", 0L))
  progreso       <- getOption("hopdesk.sap_progreso")   # function(info) or NULL
  .do_get <- function(session, query) {
    r <- tryCatch(
      httr::GET(
        url   = paste0(session$base_url, "/", endpoint),
        query = query,
        httr::timeout(timeout_pagina),
        httr::add_headers(Cookie = session$cookie, `B1S-CaseInsensitive` = "true",
                          Prefer = paste0("odata.maxpagesize=", top)),
        httr::config(ssl_verifypeer = session$ssl_verify,
                     connecttimeout = 3L)   # TCP connect within 3s
      ),
      error = function(e) {
        stop(structure(
          list(message = e$message, call = NULL),
          class = c("sap_connection_error", "error", "condition")
        ))
      }
    )
    if (httr::http_error(r))
      stop("HTTP ", httr::status_code(r), ": ", httr::http_status(r)$message)
    jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"),
                       simplifyDataFrame = TRUE)
  }

  rows      <- list()
  skip      <- 0L
  has_more  <- TRUE
  page_num  <- 0L
  page_size <- NULL   # detected from first response — SAP may cap lower than top
  t0_fetch  <- proc.time()

  while (has_more) {
    page_num <- page_num + 1L
    query <- list(`$top` = top, `$skip` = skip)
    if (!is.null(filter)) query[["$filter"]] <- filter
    if (!is.null(select)) query[["$select"]] <- paste(select, collapse = ",")

    t0_page <- proc.time()
    # T103 C2: a transport failure (timeout) is retried `reintentos` times,
    # waiting 2, 4, 8... s; with 0 (the app) it propagates at once, as before.
    .get_con_reintentos <- function() {
      intento <- 0L
      repeat {
        res <- tryCatch(.do_get(s, query), sap_connection_error = function(e) e)
        if (!inherits(res, "sap_connection_error")) return(res)
        if (intento >= reintentos) stop(res)
        intento <- intento + 1L
        message(sprintf("[SAP] %s  p%d  transport failure, retry %d/%d: %s",
                        initials, page_num, intento, reintentos, conditionMessage(res)))
        Sys.sleep(2^intento)
      }
    }
    body <- tryCatch(
      .get_con_reintentos(),
      sap_connection_error = function(e) stop(e),   # propagate, no re-auth retry
      error = function(e) {
        # One re-auth retry — no sleep, dead hosts should already be timing out fast
        sap_login(initials, client_id = client_id)
        s <<- .sap_sessions[[initials]]
        .do_get(s, query)
      }
    )

    batch <- body$value
    if (is.null(batch) || !is.data.frame(batch) || !nrow(batch)) break

    rows <- c(rows, list(batch))
    n    <- nrow(batch)
    skip <- skip + n

    # Learn the effective page size from the first response.
    # SAP B1 Service Layer may enforce MaxPageSize (often 20) regardless of $top.
    if (is.null(page_size)) page_size <- n

    seg_pagina <- (proc.time() - t0_page)[["elapsed"]]
    message(sprintf("[SAP] %s  p%d  +%d rows  cumulative=%d  (%.1fs)",
                    initials, page_num, n, skip, seg_pagina))
    if (is.function(progreso))
      tryCatch(progreso(list(empresa = initials, endpoint = endpoint, pagina = page_num,
                             filas = n, acumulado = skip, segundos = seg_pagina,
                             segundos_total = (proc.time() - t0_fetch)[["elapsed"]])),
               error = function(e) NULL)

    # Continue if: OData nextLink present, OR batch filled a full page
    has_more <- !is.null(body[["@odata.nextLink"]]) || (n >= page_size && n > 0)
  }

  if (!length(rows)) return(tibble())
  result <- bind_rows(rows)
  message(sprintf("[SAP] %s — %d rows in %d pages (%.1fs total)",
                  initials, nrow(result), page_num,
                  (proc.time() - t0_fetch)[["elapsed"]]))
  result
}

# ── Invoice fetchers ───────────────────────────────────────────────────────────
# Both return a data frame with standardized column names
# matching what data_pipeline.R expects.

.SAP_AR_SELECT <- c(
  "DocNum","NumAtCard","CardCode","CardName","FederalTaxID",
  "DocDate","DocDueDate",
  "DocCurrency","DocTotal","DocTotalFc","PaidToDate"
)

.SAP_AP_SELECT <- c(
  "DocNum","NumAtCard","CardCode","CardName","FederalTaxID",
  "DocDate","DocDueDate",
  "DocCurrency","DocTotal","DocTotalFc","PaidToDate"
)

# Legacy codes SAP sends (MXP, MXV) are mapped by moneda_normalizar()
# (R/monedas.R, MONEDAS_ALIAS_LEGADO): the catalog's file, not this one.

# The local (system) currency of a company's SAP database: the one its
# DocTotal and PaidToDate are kept in. It is a fact of that database, not
# the client's presentation base -- a client that presents in USD still
# keeps its SAP in pesos. Read from the company's ERP connection
# (config$moneda_local, a catalog code); until one is written there,
# MONEDA_BASE_OMISION. Never fails: an unreadable store is the default.
.sap_moneda_local <- function(initials, client_id = NULL) {
  cid <- client_id %||% Sys.getenv("CLIENT_ID")
  m <- if (nzchar(cid)) tryCatch({
    row <- .sap_fila_conexion(toupper(initials), cid)
    if (is.null(row)) NULL else {
      config <- jsonlite::fromJSON(row$config[1])
      moneda_normalizar(config$moneda_local %||% NA_character_)[1]
    }
  }, error = function(e) NULL)
  if (!is.null(m) && isTRUE(moneda_valida(m))) m else MONEDA_BASE_OMISION
}

# Case-insensitive column finder — SAP B1 returns mixed-case column names
# depending on language (ES/EN) and service layer version.
# e.g. DocTotalFc / DocTotalFC / DocTotalfc — all mean the same field.
.get_col <- function(df, name) {
  hit <- names(df)[tolower(names(df)) == tolower(name)]
  if (length(hit)) df[[hit[1]]] else NULL
}

.parse_invoices <- function(raw, ledger, moneda_local = MONEDA_BASE_OMISION) {
  if (!nrow(raw)) {
    message("[PARSE] ", ledger, " — raw has 0 rows, returning empty")
    return(tibble())
  }

  # Log raw currency values before normalization
  raw_cur <- sort(unique(toupper(trimws(as.character(raw$DocCurrency)))))
  message("[PARSE] ", ledger, " — raw_rows=", nrow(raw),
          " currencies=", paste(raw_cur, collapse=","))

  # ── Amount field logic (confirmed 2026-03-08 against live SAP data) ──────────
  # SAP B1 Service Layer field semantics:
  #   DocTotal   = system currency (always MXN for this DB)
  #   DocTotalFc = document/transaction currency (USD, EUR, etc.)
  #   PaidToDate = system currency (MXN) — no PaidToDateFc available in $select
  #
  # For foreign currency invoices:
  #   - Use DocTotalFc for the invoice total (exact, in document currency)
  #   - Derive paid amount in document currency = PaidToDate / DocRate
  #     where DocRate = DocTotal / DocTotalFc (the exchange rate baked into the invoice)
  #   - If DocTotalFc is 0 or missing, fall back to DocTotal - PaidToDate (MXN)
  #
  # Verified: invoice 27819 — DocTotalFc=36,222.45 USD, DocTotal=622,019 MXN
  #           DocRate = 622019/36222.45 = 17.1722
  #
  fc_total   <- parse_currency_num(.get_col(raw, "DocTotalFc") %||% rep(NA_real_, nrow(raw)))
  lc_total   <- parse_currency_num(raw$DocTotal)
  lc_paid    <- parse_currency_num(raw$PaidToDate %||% 0)
  # Domestic = the database's local currency (moneda_local), after the
  # legacy codes are mapped (MXP and MXV are pesos: moneda_normalizar).
  # Anything else (USD, EUR, …, or no code) is a foreign-currency invoice.
  # MXV used to count as foreign: its amount came from DocTotalFc (in UDIS)
  # while Moneda said MXN; now its local amount in pesos is read.
  moneda_doc <- moneda_normalizar(raw$DocCurrency)
  is_foreign <- !moneda_doc %in% moneda_local
  fc_ok      <- !is.na(fc_total) & fc_total > 0 & lc_total > 0

  # Derive the implied exchange rate per invoice, then convert paid amount
  doc_rate   <- ifelse(fc_ok, lc_total / fc_total, 1)
  fc_paid    <- ifelse(fc_ok & doc_rate > 0, lc_paid / doc_rate, lc_paid)

  saldo <- ifelse(
    is_foreign & fc_ok,
    fc_total - fc_paid,
    lc_total - lc_paid
  )

  result <- raw |>
    transmute(
      Documento                  = as.character(DocNum),
      Factura                    = as.character(NumAtCard %||% DocNum),
      `Código de cliente`        = if (ledger == "AR") as.character(CardCode) else NA_character_,
      `Código de proveedor`      = if (ledger == "AP") as.character(CardCode) else NA_character_,
      Parte                      = as.character(CardName),
      RFC                        = toupper(trimws(as.character(
                                     .get_col(raw, "FederalTaxID") %||% NA_character_))),
      `Fecha de contabilización` = parse_sap_date(as.character(DocDate)),
      `Fecha de vencimiento`     = parse_sap_date(as.character(DocDueDate)),
      Moneda                     = moneda_doc,
      `Saldo vencido`            = saldo
    )

  # Log what the filters will remove
  n_no_date  <- sum(is.na(result$`Fecha de vencimiento`))
  n_zero_bal <- sum(result$`Saldo vencido` <= 0, na.rm = TRUE)
  result_f   <- result |> filter(!is.na(`Fecha de vencimiento`), `Saldo vencido` > 0)

  message("[PARSE] ", ledger, " — after_filter=", nrow(result_f),
          " dropped_no_date=", n_no_date,
          " dropped_zero_balance=", n_zero_bal,
          " currencies=", paste(sort(unique(result_f$Moneda)), collapse=","))

  result_f
}

fetch_ar <- function(initials, client_id = NULL) {
  message("[SAP] Fetching AR invoices for ", initials)
  raw <- .sap_get_all(
    initials = initials,
    endpoint = "Invoices",
    filter   = "DocumentStatus eq 'bost_Open'",
    select   = .SAP_AR_SELECT,
    client_id = client_id
  )
  .parse_invoices(raw, "AR", moneda_local = .sap_moneda_local(initials, client_id))
}

fetch_ap <- function(initials, client_id = NULL) {
  message("[SAP] Fetching AP invoices for ", initials)
  raw <- .sap_get_all(
    initials = initials,
    endpoint = "PurchaseInvoices",
    filter   = "DocumentStatus eq 'bost_Open'",
    select   = .SAP_AP_SELECT,
    client_id = client_id
  )
  .parse_invoices(raw, "AP", moneda_local = .sap_moneda_local(initials, client_id))
}

# ── Multi-company fetcher ──────────────────────────────────────────────────────
# Queries all configured companies and returns a single combined data frame.
# Runs sequentially to avoid hammering SAP.
# Returns NULL if ALL companies fail (caller shows an error); partial failures
# are logged and skipped -- unless `exigir_todas` (below).
#
# T103 -- the same path for the app's Refrescar and for scripts/actualizar_erp.R.
# The app calls it with the defaults (its behaviour is unchanged but for C5
# and C6); the script turns the safeguards on:
#   exigir_todas  C3: one company that FAILED (HTTP or parse error) means
#                 nothing is saved and an `erp_incompleto` error is raised.
#                 Today's default keeps the others and saves without it.
#   piso          C4: before saving, the new rows are checked against the
#                 current snapshot, company by company (Empresa): one that had
#                 rows and now has none, or that falls below half of 4+ rows,
#                 stops the save with an `erp_piso` error. The snapshot sits
#                 outside .s3_write's net (.S3_SIN_RED: it is replaced whole),
#                 so this is its own net. The current snapshot is read strictly:
#                 if S3 cannot answer, nothing is saved either.
#   guardar       FALSE = read and check only (the script's --prueba).
#   exigir_guardado  C6: a snapshot that could not be saved raises
#                 `foto_no_guardada`. Without it (the app) the live rows are
#                 still returned and the failure is in sap_ultimo_resumen()
#                 for the caller to show.
# C5: a company is fetched when its credentials resolve (erp_connections
# store or SAP_<INI>_* in .Renviron, .sap_creds()) -- it used to need
# SAP_<INI>_URL in the environment even when the store had everything.
# sap_ultimo_resumen(ledger): what the last call for that ledger did (one R
# process runs one call at a time, so the caller reads its own).

.sap_resumen <- new.env(parent = emptyenv())

# ── T103 F2: the snapshot's signal on the sync bus ──────────────────────────
# The snapshot is two keys feeding two session reactives (sap_data and
# sap_snapshot_info), so it travels as a signal, the T83 way (usuarios_db):
# the app registers "erp_foto_db" with SENAL_FOTO_ERP and re-reads both
# snapshots on a bump (app.R). Sent after every SAVED snapshot, from the app
# or from scripts/actualizar_erp.R -- outside the app the name is registered
# here first, so the bump is not refused. bump_sync_version() merges S3's
# stamp before writing (T103 F2), so a fresh process neither erases other
# tables' versions nor restarts this one. Never makes the save fail.
SENAL_FOTO_ERP <- "senal:foto_erp"
avisar_foto_erp <- function(client_id) {
  if (!exists("bump_sync_version", mode = "function") || !exists("register_synced", mode = "function"))
    return(invisible(FALSE))
  if (!"erp_foto_db" %in% names(get0(".SYNC_REGISTRY", ifnotfound = list())))
    register_synced("erp_foto_db", SENAL_FOTO_ERP, function(client_id = NULL) as.numeric(Sys.time()))
  ok <- tryCatch(isTRUE(bump_sync_version("erp_foto_db", client_id = client_id)), error = function(e) {
    message("[SAP] snapshot signal not sent: ", conditionMessage(e)); FALSE })
  invisible(ok)
}

# What a session does with the signal (app.R's observer, after its gates):
# re-read both snapshots strictly and say whether its data changed.
# `actual`: the session's list(AR, AP). Returns list(leida, cambio, datos,
# info): leida FALSE = a read failed, keep everything (never an empty copy);
# a ledger whose file does not exist keeps the session's; cambio FALSE = the
# same data (the echo of its own save), write nothing; info = the saved_at
# of each ledger read, for sap_snapshot_info.
foto_erp_releer <- function(client_id, actual) {
  leer <- function(l) .s3_leer_para_escribir(
    if (l == "AR") S3_KEYS_CRITICAL$sap_snap_ar else S3_KEYS_CRITICAL$sap_snap_ap, client_id)
  fotos <- tryCatch(list(AR = leer("AR"), AP = leer("AP")), error = function(e) e)
  if (inherits(fotos, "error"))
    return(list(leida = FALSE, cambio = FALSE, error = conditionMessage(fotos)))
  datos <- list(AR = fotos$AR$data %||% actual$AR, AP = fotos$AP$data %||% actual$AP)
  info  <- Filter(Negate(is.null), lapply(list(AR = fotos$AR, AP = fotos$AP), function(f)
    if (is.null(f$saved_at)) NULL else list(ts = f$saved_at, is_live = FALSE)))
  list(leida = TRUE, cambio = !(identical(actual$AR, datos$AR) && identical(actual$AP, datos$AP)),
       datos = datos, info = info)
}

sap_ultimo_resumen <- function(ledger) .sap_resumen[[toupper(ledger)]]

# The URL a company would use, "" when it has no credentials at all.
# Partial credentials (some SAP_<INI>_* missing) are an error, not "".
.sap_url_configurada <- function(initials, client_id = NULL) {
  creds <- tryCatch(.sap_creds(initials, client_id = client_id), error = function(e) e)
  if (inherits(creds, "error")) {
    faltan <- regmatches(conditionMessage(creds),
                         regexpr("SAP_[A-Z0-9]+_[A-Z]+(, SAP_[A-Z0-9]+_[A-Z]+)*$", conditionMessage(creds)))
    if (length(faltan) && length(strsplit(faltan, ", ")[[1]]) == 4L) return("")
    stop(creds)
  }
  creds$url %||% ""
}

# C4: companies whose rows would shrink too much, as text, or character(0).
.sap_piso_violaciones <- function(nuevo, anterior) {
  if (is.null(anterior) || !is.data.frame(anterior) || !nrow(anterior) ||
      !"Empresa" %in% names(anterior)) return(character(0))
  antes   <- table(anterior$Empresa)
  despues <- if (is.data.frame(nuevo) && nrow(nuevo)) table(nuevo$Empresa) else table(character(0))
  out <- character(0)
  for (emp in names(antes)) {
    a <- as.integer(antes[[emp]])
    d <- if (emp %in% names(despues)) as.integer(despues[[emp]]) else 0L
    if (a > 0L && d == 0L) out <- c(out, sprintf("%s: %d -> 0", emp, a))
    else if (a >= 4L && d < a / 2) out <- c(out, sprintf("%s: %d -> %d", emp, a, d))
  }
  out
}

.sap_condicion <- function(clase, mensaje, ...) {
  structure(class = c(clase, "error", "condition"),
            list(message = mensaje, call = NULL, ...))
}

fetch_all_companies <- function(ledger = c("AR","AP"), cmap, client_id = NULL,
                                exigir_todas = FALSE, piso = FALSE, guardar = TRUE,
                                exigir_guardado = FALSE) {
  ledger <- match.arg(ledger)
  fetcher <- if (ledger == "AR") fetch_ar else fetch_ap
  t0_ledger <- proc.time()

  # Track per-company outcome for the snapshot-content audit log below.
  # Each entry is: list(initials, rows, status, seconds)  where status ∈ "ok"|"zero"|"failed"|"skipped"|"conn_failed"|"skipped_no_conn"
  outcomes          <- vector("list", length(cmap))
  names(outcomes)   <- names(cmap)
  results           <- vector("list", length(cmap))
  names(results)    <- names(cmap)
  connection_failed <- FALSE   # circuit breaker: first sap_connection_error opens it
  resumen <- function(...) {
    r <- c(list(ledger = ledger, client_id = client_id,
                empresas = Filter(Negate(is.null), outcomes),
                segundos = round((proc.time() - t0_ledger)[["elapsed"]], 1)), list(...))
    assign(ledger, r, envir = .sap_resumen)
    r
  }
  assign(ledger, NULL, envir = .sap_resumen)

  for (initials in names(cmap)) {
    # Circuit open — skip remaining companies without attempting any network I/O
    if (connection_failed) {
      outcomes[[initials]] <- list(initials = initials, rows = 0L, status = "skipped_no_conn")
      next
    }
    t0_co <- proc.time()
    tryCatch({
      url <- .sap_url_configurada(initials, client_id = client_id)
      if (grepl("\\[your-", url, ignore.case = TRUE) || !nzchar(url)) {
        message("[SAP] Skipping ", initials, " (", ledger, ") — no ERP credentials configured")
        outcomes[[initials]] <- list(initials = initials, rows = 0L, status = "skipped")
      } else {
        message("[SAP] Fetching ", initials, " (", ledger, ") from SAP...")
        df <- fetcher(initials, client_id = client_id)
        elapsed_co <- round((proc.time() - t0_co)[["elapsed"]], 1)
        if (nrow(df)) {
          message("[SAP] ", initials, " (", ledger, ") — ", nrow(df), " rows in ",
                  elapsed_co, "s, Empresa=", cmap[[initials]])
          outcomes[[initials]] <- list(initials = initials, rows = nrow(df), status = "ok", seconds = elapsed_co)
          results[[initials]]  <- df |> mutate(Empresa = cmap[[initials]])
        } else {
          # SAP responded but returned 0 open invoices — all invoices for this
          # company are either paid or there are genuinely none outstanding.
          message("[SAP] ", initials, " (", ledger, ") — 0 open invoices in ",
                  elapsed_co, "s (all paid or none outstanding) — excluded from snapshot")
          outcomes[[initials]] <- list(initials = initials, rows = 0L, status = "zero", seconds = elapsed_co)
        }
      }
    }, sap_connection_error = function(e) {
      # Transport-level failure (TCP timeout, DNS, refused) — open the circuit.
      # Remaining companies are skipped; caller receives sap_no_connection.
      message("[SAP] ", initials, " (", ledger, ") connection failed — circuit open: ", e$message)
      outcomes[[initials]] <<- list(initials = initials, rows = 0L, status = "conn_failed",
                                    error = conditionMessage(e))
      connection_failed    <<- TRUE
    }, error = function(e) {
      # HTTP or parsing error for this specific company — log and continue
      message("[SAP] ", initials, " (", ledger, ") FAILED: ", e$message)
      warning("[SAP] ", initials, " (", ledger, ") failed: ", e$message)
      outcomes[[initials]] <<- list(initials = initials, rows = 0L, status = "failed",
                                    error = conditionMessage(e))
    })
  }

  # Circuit was opened — throw sap_no_connection so load_ledger() can fall back
  # to the snapshot without retrying any remaining companies or showing generic errors.
  if (connection_failed) {
    resumen(guardado = FALSE, motivo = "sin_conexion")
    stop(.sap_condicion("sap_no_connection", paste("SAP connection unavailable for", ledger)))
  }

  combined <- bind_rows(Filter(Negate(is.null), results))

  # ── Snapshot-content audit log ───────────────────────────────────────────────
  # Printed before every save so there is always a clear record of exactly which
  # companies and how many invoices made it into the snapshot that is about to
  # become the calendar's data source.
  ok_cos     <- Filter(function(o) o$status == "ok",      outcomes)
  zero_cos   <- Filter(function(o) o$status == "zero",    outcomes)
  failed_cos <- Filter(function(o) o$status == "failed",  outcomes)
  ok_summary <- if (length(ok_cos))
    paste(vapply(ok_cos, function(o) paste0(o$initials, "(", o$rows, ")"), character(1)), collapse = " ")
  else "none"
  message(sprintf(
    "[SAP] %s fetch summary — snapshot will contain: %s | zero-invoice: %s | failed: %s | total_rows=%d",
    ledger,
    ok_summary,
    if (length(zero_cos))   paste(vapply(zero_cos,   function(o) o$initials, character(1)), collapse = ",") else "—",
    if (length(failed_cos)) paste(vapply(failed_cos, function(o) o$initials, character(1)), collapse = ",") else "—",
    nrow(combined)
  ))

  # C3: all or nothing.
  if (isTRUE(exigir_todas) && length(failed_cos)) {
    fallidas <- vapply(failed_cos, function(o) o$initials, character(1))
    resumen(guardado = FALSE, motivo = "incompleto", fallidas = unname(fallidas))
    stop(.sap_condicion("erp_incompleto",
                        sprintf("%s: %s failed; nothing was saved", ledger, paste(fallidas, collapse = ", ")),
                        fallidas = unname(fallidas)))
  }

  if (!nrow(combined)) {
    resumen(guardado = FALSE, motivo = "vacio")
    return(NULL)
  }

  # C4: the floor against the snapshot S3 holds now (strict read).
  if (isTRUE(piso)) {
    key <- if (ledger == "AR") S3_KEYS_CRITICAL$sap_snap_ar else S3_KEYS_CRITICAL$sap_snap_ap
    actual <- .s3_leer_para_escribir(key, client_id)   # stop()s if S3 cannot answer
    malas  <- .sap_piso_violaciones(combined, actual$data)
    if (length(malas)) {
      resumen(guardado = FALSE, motivo = "piso", piso = malas)
      stop(.sap_condicion("erp_piso",
                          sprintf("%s: too few rows against the current snapshot (%s); nothing was saved",
                                  ledger, paste(malas, collapse = "; ")),
                          violaciones = malas))
    }
  }

  if (!isTRUE(guardar)) {
    resumen(guardado = FALSE, motivo = "prueba", filas = nrow(combined))
    return(combined)
  }

  # Save snapshot for outage resilience — rotates prev1/prev2 before writing.
  # The new snapshot contains ONLY the invoices returned by THIS fetch —
  # no data from any prior snapshot is carried forward.
  #
  # BUG (found 2026-09-01): this hardcoded client_id = NULL instead of using
  # this function's OWN client_id parameter -- every live fetch persisted the
  # snapshot under the shared deployment's prefix ("hd-admin/...") instead of
  # the client's own. Fixed: client_id is threaded through.
  #
  # C6: a save that failed is never "saved": the snapshot writer stops with
  # `foto_no_guardada`; here the live rows are still returned (the app shows
  # them and warns) unless exigir_guardado (the script: exit != 0).
  guardado <- tryCatch({ save_sap_snapshot(combined, ledger, client_id = client_id); TRUE },
                       foto_no_guardada = function(e) e)
  if (!isTRUE(guardado)) {
    resumen(guardado = FALSE, motivo = "no_guardado", error = conditionMessage(guardado),
            filas = nrow(combined))
    if (isTRUE(exigir_guardado)) stop(guardado)
    return(combined)
  }
  # F2: open sessions re-read the snapshot (only after a save that worked).
  resumen(guardado = TRUE, filas = nrow(combined), avisado = avisar_foto_erp(client_id))
  combined
}

# ── Availability check ─────────────────────────────────────────────────────────
# Returns a named logical vector: which companies have credentials configured

sap_companies_available <- function(cmap) {
  sapply(names(cmap), function(i) {
    all(nzchar(c(
      Sys.getenv(paste0("SAP_", i, "_URL")),
      Sys.getenv(paste0("SAP_", i, "_COMPANY")),
      Sys.getenv(paste0("SAP_", i, "_USER")),
      Sys.getenv(paste0("SAP_", i, "_PASSWORD"))
    )))
  })
}