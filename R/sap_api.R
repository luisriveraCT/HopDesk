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

# ── Credential loader ──────────────────────────────────────────────────────────

.sap_creds <- function(initials) {
  i <- toupper(initials)
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

  list(url = url, company = company, user = user, password = password,
       ssl_verify = ssl_verify)
}

# ── Authentication ─────────────────────────────────────────────────────────────

sap_login <- function(initials) {
  creds <- .sap_creds(initials)

  resp <- tryCatch(
    httr::POST(
      url    = paste0(creds$url, "/Login"),
      httr::content_type_json(),
      httr::timeout(2),   # total timeout 2s (kept for when SAP is reachable)
      httr::config(
        ssl_verifypeer   = creds$ssl_verify,
        connecttimeout   = 0.8   # TCP connect: fail fast when host is unreachable
      ),
      body   = jsonlite::toJSON(list(
        CompanyDB = creds$company,
        UserName  = creds$user,
        Password  = creds$password
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
    stop("SAP login failed for ", initials, ": ", httr::http_status(resp)$message, call. = FALSE)
  }

  cookie <- httr::cookies(resp)
  session_id <- cookie$value[cookie$name == "B1SESSION"]
  if (!length(session_id))
    stop("SAP login for ", initials, " returned no session cookie.", call. = FALSE)

  .sap_sessions[[initials]] <- list(
    cookie     = paste0("B1SESSION=", session_id),
    base_url   = creds$url,
    ssl_verify = creds$ssl_verify,
    expires_at = Sys.time() + 1800   # 30 min
  )
  invisible(TRUE)
}

sap_logout <- function(initials) {
  s <- .sap_sessions[[initials]]
  if (is.null(s)) return(invisible(NULL))
  tryCatch(
    httr::POST(paste0(s$base_url, "/Logout"),
               httr::add_headers(Cookie = s$cookie),
               httr::config(ssl_verifypeer = s$ssl_verify)),
    error = function(e) NULL
  )
  rm(list = initials, envir = .sap_sessions)
  invisible(TRUE)
}

.ensure_session <- function(initials) {
  if (!.session_valid(initials)) sap_login(initials)
}

# ── Low-level GET with pagination ──────────────────────────────────────────────

.sap_get_all <- function(initials, endpoint, filter = NULL, select = NULL,
                          top = 500) {
  .ensure_session(initials)
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
        sap_login(initials)
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

fetch_ar <- function(initials) {
  message("[SAP] Fetching AR invoices for ", initials)
  raw <- .sap_get_all(
    initials = initials,
    endpoint = "Invoices",
    filter   = "DocumentStatus eq 'bost_Open'",
    select   = .SAP_AR_SELECT
  )
  .parse_invoices(raw, "AR")
}

fetch_ap <- function(initials) {
  message("[SAP] Fetching AP invoices for ", initials)
  raw <- .sap_get_all(
    initials = initials,
    endpoint = "PurchaseInvoices",
    filter   = "DocumentStatus eq 'bost_Open'",
    select   = .SAP_AP_SELECT
  )
  .parse_invoices(raw, "AP")
}

# ── Multi-company fetcher ──────────────────────────────────────────────────────
# Queries all configured companies and returns a single combined data frame.
# Runs sequentially to avoid hammering SAP; each company takes ~1-3 seconds.
# Returns NULL if ALL companies fail (caller shows an error); partial failures
# are logged and skipped.

fetch_all_companies <- function(ledger = c("AR","AP"), cmap = COMPANY_MAP) {
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
        df <- fetcher(initials)
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