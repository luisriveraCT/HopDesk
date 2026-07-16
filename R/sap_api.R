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
.sap_creds_from_store <- function(i, cid) {
  rows <- tryCatch(load_erp_connections(cid), error = function(e) NULL)
  if (is.null(rows) || !nrow(rows)) return(NULL)

  matches <- vapply(rows$company_initials, function(x) {
    inits <- tryCatch(jsonlite::fromJSON(x), error = function(e) character(0))
    isTRUE(i %in% inits)
  }, logical(1))
  active_ok  <- !is.na(rows$active) & rows$active == TRUE
  candidates <- rows[matches & rows$erp_type == "sap_b1_service_layer" & active_ok, , drop = FALSE]
  if (!nrow(candidates)) return(NULL)

  row     <- candidates[1, ]
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
      httr::timeout(2),   # total timeout 2s (kept for when SAP is reachable)
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
  .do_get <- function(session, query) {
    r <- tryCatch(
      httr::GET(
        url   = paste0(session$base_url, "/", endpoint),
        query = query,
        httr::timeout(8),   # 8s total per page
        httr::add_headers(Cookie = session$cookie, `B1S-CaseInsensitive` = "true"),
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
    body <- tryCatch(
      .do_get(s, query),
      sap_connection_error = function(e) stop(e),   # propagate immediately, no retry
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

    message(sprintf("[SAP] %s  p%d  +%d rows  cumulative=%d  (%.1fs)",
                    initials, page_num, n, skip,
                    (proc.time() - t0_page)[["elapsed"]]))

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

# Currency codes SAP sends that should be treated as MXN
.SAP_CURRENCY_MAP <- c(
  MXP = "MXN",   # Legacy ISO code SAP B1 sometimes sends for Mexican Peso
  MXV = "MXN"    # MXV (Mexican Unidad de Inversion) — map defensively
)

.normalize_currency <- function(x) {
  x <- toupper(trimws(as.character(x)))
  ifelse(x %in% names(.SAP_CURRENCY_MAP), .SAP_CURRENCY_MAP[x], x)
}

# Case-insensitive column finder — SAP B1 returns mixed-case column names
# depending on language (ES/EN) and service layer version.
# e.g. DocTotalFc / DocTotalFC / DocTotalfc — all mean the same field.
.get_col <- function(df, name) {
  hit <- names(df)[tolower(names(df)) == tolower(name)]
  if (length(hit)) df[[hit[1]]] else NULL
}

.parse_invoices <- function(raw, ledger) {
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
  # Domestic = system currency.  Accept both the current ISO code "MXN" and
  # SAP B1's legacy code "MXP" (still sent by some on-prem versions).
  # Anything else (USD, EUR, …) is treated as a foreign-currency invoice.
  is_foreign <- !toupper(trimws(as.character(raw$DocCurrency))) %in% c("MXN", "MXP")
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
      Moneda                     = .normalize_currency(DocCurrency),
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
  .parse_invoices(raw, "AR")
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
  .parse_invoices(raw, "AP")
}

# ── Multi-company fetcher ──────────────────────────────────────────────────────
# Queries all configured companies and returns a single combined data frame.
# Runs sequentially to avoid hammering SAP; each company takes ~1-3 seconds.
# Returns NULL if ALL companies fail (caller shows an error); partial failures
# are logged and skipped.

fetch_all_companies <- function(ledger = c("AR","AP"), cmap = COMPANY_MAP, client_id = NULL) {
  ledger <- match.arg(ledger)
  fetcher <- if (ledger == "AR") fetch_ar else fetch_ap

  # Track per-company outcome for the snapshot-content audit log below.
  # Each entry is: list(initials, rows, status)  where status ∈ "ok"|"zero"|"failed"|"skipped"|"conn_failed"|"skipped_no_conn"
  outcomes          <- vector("list", length(cmap))
  names(outcomes)   <- names(cmap)
  results           <- vector("list", length(cmap))
  names(results)    <- names(cmap)
  connection_failed <- FALSE   # circuit breaker: first sap_connection_error opens it

  for (initials in names(cmap)) {
    # Circuit open — skip remaining companies without attempting any network I/O
    if (connection_failed) {
      outcomes[[initials]] <- list(initials = initials, rows = 0L, status = "skipped_no_conn")
      next
    }
    tryCatch({
      url <- Sys.getenv(paste0("SAP_", toupper(initials), "_URL"))
      if (grepl("\\[your-", url, ignore.case = TRUE) || !nzchar(url)) {
        message("[SAP] Skipping ", initials, " (", ledger, ") — URL not configured")
        outcomes[[initials]] <- list(initials = initials, rows = 0L, status = "skipped")
      } else {
        message("[SAP] Fetching ", initials, " (", ledger, ") from SAP...")
        t0_co <- proc.time()
        df <- fetcher(initials, client_id = client_id)
        elapsed_co <- round((proc.time() - t0_co)[["elapsed"]], 1)
        if (nrow(df)) {
          message("[SAP] ", initials, " (", ledger, ") — ", nrow(df), " rows in ",
                  elapsed_co, "s, Empresa=", cmap[[initials]])
          outcomes[[initials]] <- list(initials = initials, rows = nrow(df), status = "ok")
          results[[initials]]  <- df |> mutate(Empresa = cmap[[initials]])
        } else {
          # SAP responded but returned 0 open invoices — all invoices for this
          # company are either paid or there are genuinely none outstanding.
          message("[SAP] ", initials, " (", ledger, ") — 0 open invoices in ",
                  elapsed_co, "s (all paid or none outstanding) — excluded from snapshot")
          outcomes[[initials]] <- list(initials = initials, rows = 0L, status = "zero")
        }
      }
    }, sap_connection_error = function(e) {
      # Transport-level failure (TCP timeout, DNS, refused) — open the circuit.
      # Remaining companies are skipped; caller receives sap_no_connection.
      message("[SAP] ", initials, " (", ledger, ") connection failed — circuit open: ", e$message)
      outcomes[[initials]] <<- list(initials = initials, rows = 0L, status = "conn_failed")
      connection_failed    <<- TRUE
    }, error = function(e) {
      # HTTP or parsing error for this specific company — log and continue
      message("[SAP] ", initials, " (", ledger, ") FAILED: ", e$message)
      warning("[SAP] ", initials, " (", ledger, ") failed: ", e$message)
      outcomes[[initials]] <<- list(initials = initials, rows = 0L, status = "failed")
    })
  }

  # Circuit was opened — throw sap_no_connection so load_ledger() can fall back
  # to the snapshot without retrying any remaining companies or showing generic errors.
  if (connection_failed) {
    stop(structure(
      list(message = paste("SAP connection unavailable for", ledger), call = NULL),
      class = c("sap_no_connection", "error", "condition")
    ))
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

  if (!nrow(combined)) return(NULL)

  # Save snapshot for outage resilience — rotates prev1/prev2 before writing.
  # The new snapshot contains ONLY the invoices returned by THIS fetch —
  # no data from any prior snapshot is carried forward.
  save_sap_snapshot(combined, ledger, client_id = NULL)
  combined
}

# ── Availability check ─────────────────────────────────────────────────────────
# Returns a named logical vector: which companies have credentials configured

sap_companies_available <- function(cmap = COMPANY_MAP) {
  sapply(names(cmap), function(i) {
    all(nzchar(c(
      Sys.getenv(paste0("SAP_", i, "_URL")),
      Sys.getenv(paste0("SAP_", i, "_COMPANY")),
      Sys.getenv(paste0("SAP_", i, "_USER")),
      Sys.getenv(paste0("SAP_", i, "_PASSWORD"))
    )))
  })
}