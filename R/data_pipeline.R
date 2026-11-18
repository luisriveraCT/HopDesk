# =============================================================================
# R/data_pipeline.R
# Pure data transformation functions.
# Input: raw data frames (from SAP API or RDS cache)
# Output: clean, standardized, move-applied data frames ready for the calendar
# No reactives, no S3, no UI concerns.
# =============================================================================

# ── Column detection helpers ───────────────────────────────────────────────────

# Find the first column matching preferred names or a regex fallback
guess_col <- function(df, preferred = character(), regex = NULL) {
  nms <- names(df)
  hit <- preferred[preferred %in% nms][1]
  if (!is.na(hit)) return(hit)
  if (!is.null(regex)) {
    i <- which(grepl(regex, nms, ignore.case = TRUE))[1]
    if (length(i) && !is.na(i)) return(nms[i])
  }
  NULL
}

# Find the invoice/document number column
guess_doc_col <- function(df) {
  guess_col(df,
    preferred = c("Nº de documento","Número de documento","No. de documento",
                  "Documento","DocNum","DocEntry","Nº Factura","Número de factura"),
    regex     = "(?i)doc(ument|\\.)|factur"
  )
}

# ── Type coercion ──────────────────────────────────────────────────────────────

parse_sap_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  xc  <- as.character(x)
  num <- suppressWarnings(as.numeric(xc))
  out <- as.Date(rep(NA_real_, length(xc)), origin = "1970-01-01")

  is_num <- !is.na(num)
  if (any(is_num)) {
    d <- as.Date(num[is_num], origin = "1899-12-30")
    bad <- d < as.Date("1900-01-01") | d > as.Date("2100-01-01")
    d[bad] <- as.Date(num[is_num][bad], origin = "1904-01-01")
    out[is_num] <- d
  }
  if (any(!is_num)) {
    xs <- gsub("\\.", "/", xc[!is_num])
    d1 <- suppressWarnings(lubridate::dmy(xs))
    d2 <- suppressWarnings(lubridate::ymd(xs))
    out[!is_num] <- as.Date(ifelse(!is.na(d1), d1, d2), origin = "1970-01-01")
  }
  out
}

parse_currency_num <- function(x) {
  if (is.numeric(x)) return(x)
  x2 <- str_replace_all(as.character(x), "[^0-9,.-]", "")
  if (any(str_detect(x2, ",\\d{1,2}$"), na.rm = TRUE)) {
    x2 <- str_replace_all(x2, "\\.", "")
    x2 <- str_replace(x2, ",", ".")
  } else {
    x2 <- str_replace_all(x2, ",", "")
  }
  suppressWarnings(as.numeric(x2))
}

clean_str <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "-", "—", "–", ".", "NA", "N/A", "****")] <- NA_character_
  x
}

# ── Standardization ────────────────────────────────────────────────────────────

# Ensure a Moneda column exists and is normalized
# `base`: the currency of a file with no currency column (the client's base;
# MONEDA_BASE_OMISION until the caller knows it).
ensure_moneda <- function(df, base = MONEDA_BASE_OMISION) {
  if (!"Moneda" %in% names(df)) {
    col <- guess_col(df,
      preferred = c("Currency","Divisa","Saldo vencido (moneda)"),
      regex     = "moneda|currency|divisa"
    )
    df$Moneda <- if (!is.null(col)) df[[col]] else base
  }
  df |>
    mutate(
      # Upper case, and the legacy codes SAP B1 sends (MXP, MXV) to their
      # catalog code: moneda_normalizar() in R/monedas.R.
      Moneda = moneda_normalizar(Moneda)
    ) |>
    fill(Moneda, .direction = "down")
}

# Ensure a Documento column exists
ensure_documento <- function(df) {
  if ("Documento" %in% names(df)) return(df)
  col <- guess_doc_col(df)
  df$Documento <- if (!is.null(col)) as.character(df[[col]]) else NA_character_
  df
}

# Ensure date columns are Date type
ensure_dates <- function(df) {
  for (col in c("Fecha de vencimiento","Fecha de contabilización")) {
    if (col %in% names(df))
      df[[col]] <- parse_sap_date(df[[col]])
  }
  df
}

# Ensure amount columns are numeric
ensure_amounts <- function(df) {
  if ("Saldo vencido" %in% names(df))
    df[["Saldo vencido"]] <- parse_currency_num(df[["Saldo vencido"]])
  df
}

# Standardize party (client/vendor) name → always stored in `Parte`
standardize_party <- function(df, ledger) {
  pref <- if (ledger == "AR")
    c("Parte","Nombre del cliente","Cliente","Customer","CardName","Socio de negocios")
  else
    c("Parte","Nombre de acreedor","Nombre del proveedor","Proveedor","Vendor","CardName")

  regex <- if (ledger == "AR") "(cliente|cardname|customer|socio.*neg)"
           else                "(acreedor|proveedor|vendor|cardname)"

  col <- guess_col(df, preferred = pref, regex = regex)
  if (!"Parte" %in% names(df)) df$Parte <- NA_character_
  if (!is.null(col)) df$Parte <- clean_str(df[[col]])
  df |> fill(Parte, .direction = "down")
}

# ── Deduplication ──────────────────────────────────────────────────────────────
# One row per invoice: the same invoice twice (e.g. a page SAP sends again
# while it paginates) is one. The invoice is Empresa + Moneda + Documento +
# its posting date ("Fecha de contabilización") + due date + Parte (T52c):
# the ERP reuses a DocNum in another year, and two numbering series can give
# the same DocNum to two partners; the old key (without posting date and
# Parte) merged those and lost one balance. Installments (another due date)
# stay apart, as before. A snapshot without the posting date (NA) is keyed as
# before. Rows without a Documento are kept as-is (cannot safely collapse
# them). The first row of each invoice stays; when its copies carry other
# balances, a [DEDUPE] line says how many (counts only, no client data).
# The new key parts go after the due date in group_by(), so without a
# collision the rows come out in the same order as before (T52's identical()
# check against master).
dedupe_invoices <- function(df) {
  has_doc <- !is.na(df$Documento) & nzchar(df$Documento)
  # A snapshot may lack them (NA: keyed as before); taken out again below.
  faltan <- setdiff(c("Fecha de contabilización", "Parte"), names(df))
  for (n in faltan) df[[n]] <- NA

  with_doc <- df[has_doc, ] |>
    arrange(desc(!is.na(FechaVenc_Original)), desc(FechaVenc_Original)) |>
    group_by(Empresa, Moneda, Documento, FechaVenc_Original, `Fecha de contabilización`, Parte)

  if ("Saldo vencido" %in% names(df)) {
    copias <- with_doc |>
      summarise(n = n(), saldos = dplyr::n_distinct(.data[["Saldo vencido"]], na.rm = TRUE), .groups = "drop")
    distintas <- sum(copias$n > 1L & copias$saldos > 1L)
    if (distintas > 0L) {
      tipo <- unique(stats::na.omit(as.character(df$Tipo)))
      message(sprintf("[DEDUPE] %s: %d invoice(s) arrived more than once with different balances; kept the first row",
                      if (length(tipo)) paste(tipo, collapse = "/") else "?", distintas))
    }
  }

  out <- bind_rows(with_doc |> slice_head(n = 1) |> ungroup(), df[!has_doc, ])
  out[, setdiff(names(out), faltan), drop = FALSE]
}

# ── Policy date of each invoice (T52) ─────────────────────────────────────────
# An ERP DocNum is reused in another year: the invoice is Empresa + Moneda +
# Documento + its posting date ("Fecha de contabilización"), the identity
# agenda_filas_factura() already uses. policy_moves rows written since T52
# carry that date (fecha_factura) and only reach their own invoice. Older rows
# (fecha_factura NA) reach the document on the old key while it is a single
# invoice (one posting date in df; see .politica_doc_unica()). A row with any part
# of the key NA matches nothing (the old join's na_matches = "never").
# When a key has several rows, the latest FechaVenc_Politica wins, as before.
# One function for every reader (build_ledger_df(), the ledger module's
# df_with_moves()), so the two can never disagree again.
# Returns a Date per row of df.
.politica_doc <- function(empresa, moneda, documento) {
  k <- paste(empresa, moneda, documento, sep = "\r")
  k[is.na(empresa) | is.na(moneda) | is.na(documento)] <- NA_character_
  k
}

politica_por_factura <- function(df, policy_moves_df, ledger) {
  i <- fila_por_factura(df, policy_moves_df, ledger, "FechaVenc_Politica")
  v <- if (is.data.frame(policy_moves_df)) as.Date(policy_moves_df$FechaVenc_Politica) else as.Date(character())
  v[i]
}

# The row of `tabla` (policy_moves, moves: ledger, Empresa, Moneda,
# Documento, fecha_factura and the date column `valor`) that belongs to each
# row of df, as an index into `tabla` (NA = none). The rule of
# politica_por_factura() above, for every table keyed on the invoice (T52b
# extends it to the dates moved by hand): a dated row reaches only its own
# invoice; an undated one (written before the table had the date) reaches
# its document -- with solo_unica, only while the document is one invoice.
# When several rows reach a row, the latest `valor` wins, as the old joins'
# arrange(desc()) |> distinct().
fila_por_factura <- function(df, tabla, ledger, valor, solo_unica = TRUE) {
  n   <- if (is.data.frame(df)) nrow(df) else 0L
  out <- rep(NA_integer_, n)
  if (!n || !is.data.frame(tabla) || !nrow(tabla)) return(out)
  idx <- which(tabla$ledger %in% ledger)
  if (!length(idx)) return(out)

  tv <- as.Date(tabla[[valor]][idx])
  tf <- if ("fecha_factura" %in% names(tabla)) as.Date(tabla$fecha_factura[idx]) else as.Date(rep(NA_character_, length(idx)))
  tk <- .politica_doc(tabla$Empresa[idx], tabla$Moneda[idx], tabla$Documento[idx])
  o  <- order(tv, decreasing = TRUE, na.last = TRUE)       # match() takes the first: the latest
  idx <- idx[o]; tf <- tf[o]; tk <- tk[o]

  fc <- df[["Fecha de contabilización"]]
  fc <- if (is.null(fc)) NULL else tryCatch(as.Date(fc), error = function(e) NULL)
  if (is.null(fc)) fc <- as.Date(rep(NA_character_, n))
  dk <- .politica_doc(df$Empresa, df$Moneda, df$Documento)

  # Its own row: same document and posting date.
  propia  <- !is.na(tk) & !is.na(tf)
  k_prop  <- paste(tk[propia], format(tf[propia]), sep = "\r")
  dk_prop <- ifelse(is.na(dk) | is.na(fc), NA_character_, paste(dk, format(fc), sep = "\r"))
  out     <- idx[propia][match(dk_prop, k_prop)]

  # Undated rows, on the document alone -- with solo_unica only while the
  # document is one invoice: a single posting date among df's rows. With two
  # (a reused number) nobody can tell whose the old row is, so it reaches
  # neither.
  vieja <- !is.na(tk) & is.na(tf)
  usa   <- is.na(out) & !is.na(dk)
  if (solo_unica) usa <- usa & .politica_doc_unica(dk, fc)
  out[usa] <- idx[vieja][match(dk[usa], tk[vieja])]
  out
}

# The date moved by hand (moves.rds) of each row of df, and its note: a list
# of FechaVenc_Proyectada (Date) and notas (character, or NULL when the
# moves table has no notas column), one per row (T52b; see fila_por_factura()).
mudanza_por_factura <- function(df, moves_df, ledger) {
  i <- fila_por_factura(df, moves_df, ledger, "FechaVenc_Proyectada", solo_unica = FALSE)
  hay <- is.data.frame(moves_df)
  list(
    FechaVenc_Proyectada = (if (hay) as.Date(moves_df$FechaVenc_Proyectada) else as.Date(character()))[i],
    notas = if (!hay || !nrow(moves_df)) rep(NA_character_, length(i))
            else if ("notas" %in% names(moves_df)) as.character(moves_df$notas)[i]
            else NULL)
}

# For each key, whether it has at most one distinct posting date among the
# given rows (rows without a date do not count). The rule that lets a policy
# row written before T52 reach a document (politica_por_factura() and the
# "new" merge in settings_companies.R).
.politica_doc_unica <- function(k, fecha) {
  con <- !is.na(k) & !is.na(fecha)
  if (!any(con)) return(rep(TRUE, length(k)))
  pares <- unique(data.frame(k = k[con], f = as.numeric(fecha[con])))
  n_f   <- table(pares$k)
  n     <- as.integer(n_f[k])
  is.na(n) | n <= 1L
}

# ── Tags of each invoice (T52d) ───────────────────────────────────────────────
# The urgent / important tags (invoice_tags.rds) of each row of df, as
# data.frame(urgent, important). One function for every reader (the
# Calendar's label and day dots, Vencidos and the day modal, Exportar/Flujo,
# the Reporte), as politica_por_factura() is for the policy date.
#   - A row written since T52d carries fecha_factura: it reaches its own
#     invoice only (same document and posting date).
#   - An older row (fecha_factura NA) reaches its document as before T52d
#     while the document is one invoice (.politica_doc_unica() over
#     `facturas`), except when that invoice is later than the tag (posting
#     date > tagged_at): a tag cannot be about an invoice that did not exist
#     yet -- the number reused after the tagged one was paid. On a document
#     with two or more invoices it reaches none (nobody can tell whose it
#     is), as the policy row in T52. Without tagged_at, as before.
#     (Integrator and Coordinador, 2026-11-18.)
# `ledger`: one value for every row, or one per row. `facturas`: the
# ledger's open invoices (Empresa, Moneda, Documento, posting date, and
# Ledger/Tipo or the same `ledger`), to tell one invoice from a reused
# number when df is only part of them (a day, the overdue ones); by default
# df itself. A row of df without Documento, Empresa or Moneda gets no tag.
etiquetas_por_factura <- function(df, tags_df, ledger, facturas = NULL) {
  n   <- if (is.data.frame(df)) nrow(df) else 0L
  out <- data.frame(urgent = rep(FALSE, n), important = rep(FALSE, n))
  if (!n || !is.data.frame(tags_df) || !nrow(tags_df)) return(out)
  tg <- tags_df[tags_df$tag %in% c("urgent", "important"), , drop = FALSE]
  if (!nrow(tg)) return(out)

  led <- rep_len(as.character(ledger), n)
  dk  <- .politica_doc(paste(led, df$Empresa, sep = "\r"), df$Moneda, as.character(df$Documento))
  dk[is.na(led) | is.na(df$Empresa)] <- NA_character_
  fc  <- df[["Fecha de contabilización"]]
  fc  <- if (is.null(fc)) as.Date(rep(NA_character_, n)) else suppressWarnings(as.Date(fc))
  tk  <- .politica_doc(paste(tg$ledger, tg$Empresa, sep = "\r"), tg$Moneda, as.character(tg$Documento))
  tk[is.na(tg$ledger) | is.na(tg$Empresa)] <- NA_character_
  tf  <- if ("fecha_factura" %in% names(tg)) suppressWarnings(as.Date(tg$fecha_factura)) else as.Date(rep(NA_character_, nrow(tg)))
  # The tag's day in the client's zone (hoy_en(), R/zona.R), never
  # as.Date(POSIXct), which is the UTC day.
  ta <- as.Date(rep(NA_character_, nrow(tg)))
  if ("tagged_at" %in% names(tg)) {
    t_at <- suppressWarnings(as.POSIXct(tg$tagged_at, tz = "UTC"))
    hay  <- !is.na(t_at)
    if (any(hay)) ta[hay] <- hoy_en(ZONA_OMISION, ahora = t_at[hay])
  }

  pon <- function(filas, tag) {
    if (tag == "urgent") out$urgent[filas] <<- TRUE else out$important[filas] <<- TRUE
  }
  # Its own invoice.
  propia <- !is.na(tk) & !is.na(tf)
  if (any(propia)) {
    k_df <- ifelse(is.na(dk) | is.na(fc), NA_character_, paste(dk, format(fc), sep = "\r"))
    k_tg <- paste(tk[propia], format(tf[propia]), sep = "\r")
    for (i in which(propia)) pon(which(k_df %in% paste(tk[i], format(tf[i]), sep = "\r")), tg$tag[i])
  }
  # Older rows, without the posting date.
  vieja <- which(!is.na(tk) & is.na(tf))
  if (length(vieja)) {
    unica <- .etiquetas_doc_unica(dk, fc, facturas, led)
    for (i in vieja) {
      filas <- which(dk %in% tk[i] & unica)
      if (!length(filas)) next
      if (!is.na(ta[i]) && any(!is.na(fc[filas]) & fc[filas] > ta[i])) next   # later than the tag
      pon(filas, tg$tag[i])
    }
  }
  out
}

# For each row (its document key dk and posting date fc), whether its
# document is one invoice among `facturas` (or among the rows themselves):
# .politica_doc_unica(), the T52 rule, not a copy of it.
.etiquetas_doc_unica <- function(dk, fc, facturas, led) {
  if (!is.data.frame(facturas) || !nrow(facturas)) return(.politica_doc_unica(dk, fc))
  fl <- facturas[["Ledger"]] %||% facturas[["Tipo"]] %||% facturas[["ledger"]]
  fl <- if (is.null(fl)) rep(led[1], nrow(facturas)) else as.character(fl)
  fk <- .politica_doc(paste(fl, facturas$Empresa, sep = "\r"), facturas$Moneda, as.character(facturas$Documento))
  ff <- facturas[["Fecha de contabilización"]]
  ff <- if (is.null(ff)) as.Date(rep(NA_character_, nrow(facturas))) else suppressWarnings(as.Date(ff))
  u  <- .politica_doc_unica(c(fk, dk), c(ff, fc))
  u[nrow(facturas) + seq_along(dk)]
}

# ── Main pipeline ──────────────────────────────────────────────────────────────
# Accepts raw data from any source (SAP API response or legacy RDS).
# Returns a clean data frame ready for the calendar reactive.

build_ledger_df <- function(raw_df, ledger, empresa, moves_df, manual_df = NULL,
                            abonos_df = NULL, policy_moves_df = NULL, sap_ov = NULL,
                            provs_df = NULL, company_map = NULL, liabs_df = NULL) {
  ledger <- toupper(ledger)

  # Build SAP portion (may be empty)
  if (!is.null(raw_df) && nrow(raw_df)) {
    df <- raw_df |>
      ensure_dates() |>
      ensure_amounts() |>
      ensure_moneda() |>
      standardize_party(ledger) |>
      ensure_documento() |>
      mutate(
        Tipo               = ledger,
        source             = SAP_B1_ERP_TYPE,
        FechaVenc_Original = as.Date(`Fecha de vencimiento`)
      ) |>
      dedupe_invoices()
  } else {
    df <- tibble()
  }

  # Merge manual entries — works even when SAP data is empty
  if (!is.null(manual_df) && nrow(manual_df)) {
    manual_sub <- manual_df |>
      filter(.data$ledger == !!ledger) |>
      mutate(
        source             = "manual",
        FechaVenc_Original = as.Date(`Fecha de vencimiento`),
        Tipo               = ledger,
        Parte              = Parte %||% "",
        `Saldo vencido`    = Importe   # manual schema stores this as Importe
      )
    # Translate empresa initials to full names (manual items from provision
    # conversion may store initials; the empresa filter in df_combined() uses
    # full names — same translation already done below for provision rows).
    if (nrow(manual_sub) && !is.null(company_map) && length(company_map)) {
      manual_sub[["Empresa"]] <- vapply(
        manual_sub[["Empresa"]],
        function(e) company_map[[e %||% ""]] %||% e %||% NA_character_,
        character(1)
      )
    }
    df <- bind_rows(df, manual_sub)
  }

  # Merge provisions — AP only; provisional provisions appear in the ledger
  # as placeholder rows until converted to real manual_inv items.
  # provs_df is passed from a reactive so df_combined() re-fires on provision saves.
  # Only the provisions the caller passes: without them, none. This used to
  # fall back to load_pasivos_provisions() with no client_id and no
  # liabilities. On the shared deployment that read is refused (no tenant),
  # but with one tenant's CLIENT_ID it loaded them all into the Calendar's
  # df_base -- which takes its provisions from df_prov_rows() -- so each
  # showed twice, and the ones of a deleted liability showed at all (T18).
  if (identical(ledger, "AP")) {
    provs <- provs_df
    liabs <- liabs_df
    if (!is.null(provs) && nrow(provs)) {
      prov_rows <- pasivos_provisions_as_ledger_rows(provs, liabs, ledger = "AP")
      if (nrow(prov_rows)) {
        # Provisions store empresa as a company initial (e.g. "NG"); the empresa
        # filter in df_combined() uses full names from the Empresas module.
        # Translate here so provisions survive the filter.
        if (!is.null(company_map) && length(company_map)) {
          prov_rows[["Empresa"]] <- vapply(
            prov_rows[["Empresa"]],
            function(e) company_map[[e %||% ""]] %||% e %||% NA_character_,
            character(1)
          )
        }
        df <- bind_rows(df, prov_rows)
      }
    }
  }

  # Harmonize Codigo across SAP and manual rows. SAP carries CardCode under
  # `Código de proveedor` (AP) or `Código de cliente` (AR); manual carries it
  # as `Codigo`. Coalesce into a single canonical `Codigo` column so every
  # downstream slice (keep lists, .fresh_lu, vencidos disp_df, search keep)
  # picks it up uniformly.
  if (!"Codigo" %in% names(df)) df[["Codigo"]] <- NA_character_
  if ("Código de proveedor" %in% names(df)) {
    df[["Codigo"]] <- dplyr::coalesce(
      as.character(df[["Codigo"]]),
      as.character(df[["Código de proveedor"]])
    )
  }
  if ("Código de cliente" %in% names(df)) {
    df[["Codigo"]] <- dplyr::coalesce(
      as.character(df[["Codigo"]]),
      as.character(df[["Código de cliente"]])
    )
  }
  df[["Codigo"]] <- trimws(df[["Codigo"]] %||% "")
  df[["Codigo"]][df[["Codigo"]] == ""] <- NA_character_

  # Nothing at all — return NULL
  if (!nrow(df)) return(NULL)

  # Guarantee FechaEff exists before the left_join+mutate pipeline.
  # Provision rows already carry it (set by pasivos_calendar_glue); SAP/manual
  # rows do not — add an NA column so .data$FechaEff is always resolvable.
  if (!"FechaEff" %in% names(df)) df[["FechaEff"]] <- as.Date(NA_character_)

  # Apply date moves (moves_df may be NULL if .data_loaded hasn't completed
  # yet), each to its own invoice (T52b). A row with Documento NA (provisions
  # often have no template) matches nothing; with several rows the latest
  # projected date wins.
  result <- df
  mudanza <- mudanza_por_factura(result, moves_df, ledger)
  result$FechaVenc_Proyectada <- mudanza$FechaVenc_Proyectada
  if (!is.null(mudanza$notas)) result$notas <- mudanza$notas
  # The policy date of each invoice, on its own identity (T52).
  result$FechaVenc_Politica <- politica_por_factura(result, policy_moves_df, ledger)
  result <- result |>
    mutate(
      FechaVenc_Proyectada = as.Date(FechaVenc_Proyectada),
      FechaVenc_Politica   = as.Date(FechaVenc_Politica),
      # .data$FechaEff carries the policy-adjusted date already set by
      # pasivos_calendar_glue for provision rows; fall back to it before
      # FechaVenc_Original so that policy adjustments are preserved.
      FechaEff             = dplyr::coalesce(FechaVenc_Proyectada, FechaVenc_Politica,
                                             .data$FechaEff, FechaVenc_Original),
      Movida               = dplyr::case_when(
        !is.na(FechaVenc_Proyectada) ~ "Manual",
        !is.na(FechaVenc_Politica)   ~ "Políticas",
        TRUE                         ~ "Falso"
      ),
      .row_id              = row_number()
    )
  if (!"notas" %in% names(result)) result[["notas"]] <- NA_character_

  # ── Apply abonos parciales ───────────────────────────────────────────────────
  # Always add the three abono columns so downstream code (to_calendar_data,
  # calendar_html) can depend on them unconditionally.
  ab_summary <- active_abonos_summary(abonos_df) |>
    dplyr::filter(.data$ledger == !!toupper(ledger))

  if (nrow(ab_summary) > 0) {
    result <- result |>
      left_join(
        ab_summary |> dplyr::select(Empresa, Moneda, Documento, abono_total),
        by = c("Empresa", "Moneda", "Documento")
      ) |>
      mutate(
        abono_total    = replace_na(abono_total, 0),
        Saldo_original = `Saldo vencido`,
        `Saldo vencido` = pmax(0, `Saldo vencido` - abono_total),
        has_abono      = abono_total > 0
      )
  } else {
    result <- result |>
      mutate(
        abono_total    = 0,
        Saldo_original = `Saldo vencido`,
        has_abono      = FALSE
      )
  }

  # ── Apply SAP field overrides (Parte, Codigo, Factura, Notas) ───────────────
  # Overrides paint on top of SAP data for UI display only.
  # The underlying sap_data() snapshot and dedup keys are never touched.
  # Deliberately literal source == SAP_B1_ERP_TYPE below, not
  # is_erp_sourced(): this whole feature (table, columns, settings UI) is
  # SAP-specific by name, not a generic ERP-override mechanism yet —
  # generalizing it is a bigger, separate decision than this stage's "don't
  # hardcode the sap/manual split at every confirm/delete/stage site" scope.
  # Also a no-op either way today — `source` is always explicitly
  # SAP_B1_ERP_TYPE by the time rows reach here, never NA (see
  # build_ledger_df() above), so the two checks are equivalent now.
  # (Updated 2026-09-14: the literal itself changed from the old bare "sap"
  # to the connector registry's own erp_type key, SAP_B1_ERP_TYPE, so this
  # keeps matching real SAP-sourced rows now that `source` carries that value
  # instead — see is_erp_sourced()'s own comment in R/global.R.)
  if (!is.null(sap_ov) && is.data.frame(sap_ov) && nrow(sap_ov)) {
    if (!"Factura" %in% names(result)) result[["Factura"]] <- NA_character_
    ov_filt <- sap_ov |>
      dplyr::filter(.data$ledger == !!ledger) |>
      dplyr::select(Empresa, Moneda, Documento,
                    Parte_override, Codigo_override, Factura_override, Notas_override,
                    Moneda_override, Importe_override)
    if (nrow(ov_filt)) {
      result <- dplyr::left_join(result, ov_filt,
                                 by = c("Empresa", "Moneda", "Documento")) |>
        dplyr::mutate(
          has_sap_override = source == SAP_B1_ERP_TYPE & (
            !is.na(Parte_override) | !is.na(Codigo_override) |
            !is.na(Factura_override) | !is.na(Notas_override) |
            !is.na(Moneda_override) | !is.na(Importe_override)),
          Parte   = dplyr::if_else(source == SAP_B1_ERP_TYPE & !is.na(Parte_override),
                                   Parte_override,   Parte),
          Codigo  = dplyr::if_else(source == SAP_B1_ERP_TYPE & !is.na(Codigo_override),
                                   Codigo_override,  Codigo),
          Factura = dplyr::if_else(source == SAP_B1_ERP_TYPE & !is.na(Factura_override),
                                   Factura_override, Factura),
          notas   = dplyr::if_else(source == SAP_B1_ERP_TYPE & !is.na(Notas_override),
                                   Notas_override,   notas),
          Moneda  = dplyr::if_else(source == SAP_B1_ERP_TYPE & !is.na(Moneda_override),
                                   Moneda_override,  Moneda),
          Importe = dplyr::if_else(source == SAP_B1_ERP_TYPE & !is.na(Importe_override),
                                   dplyr::if_else(Importe < 0,
                                                  -abs(Importe_override),
                                                  abs(Importe_override)),
                                   Importe)
        ) |>
        dplyr::select(-Parte_override, -Codigo_override,
                      -Factura_override, -Notas_override,
                      -Moneda_override, -Importe_override)
    } else {
      result[["has_sap_override"]] <- FALSE
    }
  } else {
    result[["has_sap_override"]] <- FALSE
  }

  result
}

# ── Intercompany filter ────────────────────────────────────────────────────────
# mode: "exclude" | "include" (no-op) | "only"
# cmap: the SESSION's company map (shared$company_map()), used by the name
#   fallback below. Required: the global COMPANY_MAP can hold another client's
#   companies in the shared production process.
# empresas: the session's empresas table (shared$empresas_db()), optional
#   (T47). Used ONLY when the client has no intercompany registry (no
#   CardCodes, no RFCs): then a row is IC when its RFC is one of the
#   companies', or, without an RFC, when its counterpart is a company's
#   razón social / short name / alias after .norm_razon() -- exactly, never
#   approximately (R/empresas_identidad.R). With a registry, or without
#   `empresas`, the result is identical() to before T47.

# The session's empresas table for apply_ic_filter(), or NULL where the
# session has none (most test fixtures): then the filter is the pre-T47 one.
ic_empresas_de <- function(shared) tryCatch(
  if (is.function(shared$empresas_db)) shared$empresas_db() else NULL, error = function(e) NULL)

apply_ic_filter <- function(df, mode, code_col, ic_codes, ic_rfcs = character(), cmap, empresas = NULL) {
  force(cmap)
  if (mode == "include") return(df)

  has_codes <- length(ic_codes) > 0
  has_rfcs  <- length(ic_rfcs) > 0 && "RFC" %in% names(df)

  # No registry, and the companies' identity at hand: legal names and RFCs
  # ("NETWORKS LOGISTICS SA DE CV" is NL), not only the exact short name.
  identidad <- if (!has_codes && !has_rfcs && !is.null(empresas) && "Parte" %in% names(df))
    empresas_identidad(empresas) else NULL
  if (!is.null(identidad)) {
    rfc_col <- if ("RFC" %in% names(df)) df[["RFC"]] else NULL
    es_ic <- ic_por_identidad(df[["Parte"]], rfc_col, identidad)
    message("[IC_FILTER] mode=", mode, " identity-fallback rows_in=", nrow(df), " rows_ic=", sum(es_ic))
    if (mode == "exclude") return(df[!es_ic, , drop = FALSE])
    else                   return(df[ es_ic, , drop = FALSE])
  }

  if (!has_codes && !has_rfcs) {
    # Fallback: when no CardCodes or RFCs are registered, detect IC by matching
    # the Parte column against the session's company display names.
    # This is the same Layer-2 mechanism used by the Vencidos module.
    if ("Parte" %in% names(df)) {
      if (length(cmap)) {
        company_names_up <- toupper(unname(cmap))
        is_ic_by_name    <- toupper(trimws(df[["Parte"]])) %in% company_names_up
        n_ic <- sum(is_ic_by_name, na.rm = TRUE)
        message("[IC_FILTER] mode=", mode, " name-fallback rows_in=", nrow(df), " rows_ic=", n_ic)
        if (mode == "exclude") return(df[!is_ic_by_name | is.na(is_ic_by_name), , drop = FALSE])
        else                   return(df[ is_ic_by_name & !is.na(is_ic_by_name), , drop = FALSE])
      }
    }
    message("[IC_FILTER] mode=", mode, " — ic_codes and ic_rfcs both empty, no filter applied")
    return(df)
  }

  # Seed is_ic from CardCode matching (when codes are registered for this company)
  if (has_codes && !is.null(code_col) && code_col %in% names(df)) {
    codes <- normalize_code(df[[code_col]])
    is_ic <- codes %in% normalize_code(ic_codes)
  } else {
    is_ic <- rep(FALSE, nrow(df))
  }

  # RFC overrides — two-way:
  #   positive: RFC in IC list   → confirmed IC  (works even when no CardCodes registered)
  #   negative: RFC not in list  → confirmed NOT IC (eliminates false positives like GEODIS)
  if (has_rfcs) {
    inv_rfc      <- toupper(trimws(df[["RFC"]]))
    has_rfc      <- nzchar(inv_rfc) & !is.na(inv_rfc)
    ic_rfcs_norm <- toupper(trimws(ic_rfcs[nzchar(ic_rfcs)]))
    is_ic[has_rfc &  (inv_rfc %in% ic_rfcs_norm)] <- TRUE
    is_ic[has_rfc & !(inv_rfc %in% ic_rfcs_norm)] <- FALSE
  }

  n_ic <- sum(is_ic, na.rm = TRUE)
  message("[IC_FILTER] mode=", mode,
          " codes=", length(ic_codes), " rfcs=", length(ic_rfcs),
          " rows_in=", nrow(df), " rows_ic=", n_ic)
  if (mode == "exclude") df[!is_ic | is.na(is_ic), , drop = FALSE]
  else                   df[ is_ic & !is.na(is_ic), , drop = FALSE]
}

# Build the full CardCode set for one ledger from the v2 registry.
# Handles both the new per-empresa format and the legacy flat-list format
# so the app degrades gracefully if the S3 key is temporarily unavailable.
#
# registry: result of load_interco_v2() or shared$interco_v2()
# ledger:   "AR" | "AP"
# returns:  character vector of normalized full CardCodes, e.g. c("C1027","C1047",...)
build_ic_fullcodes <- function(registry, ledger) {
  ledger <- toupper(ledger)

  # Backward compat: old format had ar_clients / ap_suppliers flat vectors
  if (!is.null(registry$ar_clients) || !is.null(registry$ap_suppliers)) {
    key <- if (ledger == "AR") "ar_clients" else "ap_suppliers"
    return(normalize_code(registry[[key]] %||% character()))
  }

  if (is.null(registry$companies) || !length(registry$companies))
    return(character())

  prefix <- if (ledger == "AR") registry$ar_prefix %||% "C"
            else                registry$ap_prefix %||% "P"

  numeric_codes <- unlist(lapply(registry$companies, function(co) {
    if (ledger == "AR") co$ar %||% character()
    else                co$ap %||% character()
  }), use.names = FALSE)

  if (!length(numeric_codes)) return(character())
  unique(toupper(paste0(prefix, numeric_codes)))
}

# ── IC Scanner ────────────────────────────────────────────────────────────────
# Aggregates unique CardCodes + RFCs from loaded invoice snapshots into a
# candidate table that the dev can review in Settings › Intercompany.
#
# Returns a tibble: initials, ledger, code (full, e.g. "C1027"), nombre, rfc,
#                   n_facturas, is_ic (already in registry)
scan_ic_candidates <- function(sap_ar, sap_ap, registry, cmap) {
  inv_map <- setNames(names(cmap), unname(cmap))

  .extract <- function(df, ledger) {
    if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(tibble())
    code_col <- if (ledger == "AR") "C\u00f3digo de cliente" else "C\u00f3digo de proveedor"
    if (!code_col %in% names(df)) return(tibble())
    has_rfc  <- "RFC" %in% names(df)

    df |>
      dplyr::filter(!is.na(.data[[code_col]]), nzchar(trimws(.data[[code_col]]))) |>
      dplyr::transmute(
        initials = inv_map[Empresa],
        ledger   = ledger,
        code     = toupper(trimws(.data[[code_col]])),
        nombre   = Parte,
        rfc      = if (has_rfc) toupper(trimws(RFC)) else NA_character_
      ) |>
      dplyr::filter(!is.na(initials), nzchar(code)) |>
      dplyr::group_by(initials, ledger, code) |>
      dplyr::summarise(
        nombre     = dplyr::first(nombre),
        rfc        = dplyr::first(rfc[!is.na(rfc) & nzchar(rfc)]),
        n_facturas = dplyr::n(),
        .groups    = "drop"
      )
  }

  candidates <- dplyr::bind_rows(
    .extract(sap_ar, "AR"),
    .extract(sap_ap, "AP")
  )

  if (!nrow(candidates)) return(candidates)

  ar_prefix <- toupper(registry$ar_prefix %||% "C")
  ap_prefix <- toupper(registry$ap_prefix %||% "P")

  candidates |>
    dplyr::rowwise() |>
    dplyr::mutate(
      .prefix  = if (ledger == "AR") ar_prefix else ap_prefix,
      .numeric = sub(paste0("^", .prefix), "", code),
      is_ic    = !is.na(initials) &&
                   !is.na(nombre) && nzchar(nombre %||% "") &&
                   .numeric %in%
                   (registry$companies[[initials]][[if (ledger == "AR") "ar" else "ap"]] %||% character())
    ) |>
    dplyr::select(-.prefix, -.numeric) |>
    dplyr::ungroup() |>
    dplyr::arrange(initials, ledger, dplyr::desc(n_facturas))
}

# ── Calendar-ready aggregation ─────────────────────────────────────────────────
# Reduces the full invoice df to (Fecha, Moneda, Parte, Importe)
# which is exactly what calendar_plot() needs.

to_calendar_data <- function(df, amount_col = "Saldo vencido") {
  # Exclude confirmed payments AND soft-deleted ghosts from the calendar heat-map
  if ("confirmed" %in% names(df))
    df <- df[is.na(df[["confirmed"]]) | !df[["confirmed"]], , drop = FALSE]
  if ("is_ghost" %in% names(df))
    df <- df[is.na(df[["is_ghost"]]) | !df[["is_ghost"]], , drop = FALSE]

  # ── Column audit ────────────────────────────────────────────────────────────
  required_cols <- c("Empresa", "FechaEff", "Moneda", "Parte", amount_col)
  missing_cols  <- setdiff(required_cols, names(df))
  if (length(missing_cols)) {
    warning("[to_calendar_data] Missing columns: ", paste(missing_cols, collapse=", "),
            " — available: ", paste(names(df), collapse=", "))
    return(tibble(Empresa=character(), Fecha=as.Date(NA_character_), Moneda=NA_character_,
                  Parte=NA_character_, Importe=NA_real_)[0, ])
  }

  # Ensure abono columns exist with safe defaults before summarising
  if (!"abono_total"    %in% names(df)) df$abono_total    <- 0
  if (!"has_abono"      %in% names(df)) df$has_abono      <- FALSE
  if (!"Saldo_original" %in% names(df)) df$Saldo_original <- df[[amount_col]]

  # fix-aprox-pantallas: a provision quoted in USD and paid in MXN counts with
  # its amount converted at today's FIX (the provision bridge's importe_aprox)
  # instead of 0, and its party says so (aprox). This frame only paints the
  # Calendar's bubbles: the Agenda's keys never come from it.
  v <- if (exists("fx_columna_ver", mode = "function"))
    fx_columna_ver(df[[amount_col]], df$aprox, df$importe_aprox)
  else list(ver = df[[amount_col]], aprox = rep("", nrow(df)))
  df$.aprox <- nzchar(v$aprox)
  df[[amount_col]][df$.aprox] <- v$ver[df$.aprox]
  df$Saldo_original[df$.aprox] <- v$ver[df$.aprox]

  result <- df |>
    mutate(
      Importe        = abs(replace_na(.data[[amount_col]], 0)),
      Fecha          = as.Date(FechaEff),
      Moneda         = toupper(trimws(Moneda)),
      abono_total    = replace_na(abono_total,    0),
      Saldo_original = dplyr::coalesce(Saldo_original, Importe),
      has_abono      = replace_na(has_abono,      FALSE)
    ) |>
    filter(!is.na(Fecha)) |>
    group_by(Empresa, Fecha, Moneda, Parte) |>
    summarise(
      Importe        = sum(Importe,        na.rm = TRUE),
      abono_total    = sum(abono_total,    na.rm = TRUE),
      Saldo_original = sum(Saldo_original, na.rm = TRUE),
      has_abono      = any(has_abono,      na.rm = TRUE),
      aprox          = any(.aprox,         na.rm = TRUE),
      .groups = "drop"
    )

  message("[to_calendar_data] amount_col='", amount_col,
          "' rows_in=", nrow(df), " rows_out=", nrow(result),
          " monedas=", paste(sort(unique(result$Moneda)), collapse=","))
  result
}

# ── Canonical "is this invoice confirmed?" computation ─────────────────────────
# Ledger-integrity master plan, Stage 9: the single reference implementation
# every consumer (calendar, and eventually Cash Flow Preview/Export, Reporte's
# Cash Flow Pulse, Intercompany) reads from, instead of each one growing its
# own reimplementation. Two sources only -- a third (pagar_hoy_db.status==
# "confirmed") existed pre-Stage-4 but is now structurally dead: every confirm
# handler unconditionally unstages the Agenda row regardless of source, so no
# code path can ever leave one behind with status=="confirmed" again.
#
#   1. bancos_confirmados matching (Empresa+Documento+Moneda, case/whitespace
#      normalized) -- confirmations made via Agenda de Hoy. Never applied to
#      manual or provision rows (see below).
#   2. papelera SAP ghosts -- SAP items soft-deleted via the calendar/search
#      trash mechanic remain visible, struck through, excluded from sums.
#
# Manual rows: matched only by their own UUID (papelera anti-join, done by the
# caller before this runs) or explicit deletion -- never by document-key
# matching here, which would wrongly hide a brand-new manual entry that
# happens to reuse a past, already-confirmed Documento. A manual row that DOES
# end up confirmed (via the archive-on-confirm mechanism) is removed from df
# entirely below, rather than kept as a ghost like a SAP row.
#
# Provision rows: forcibly cleared on all three flags regardless of any match
# -- they can only change state through the explicit conversion modal, never
# through payment/confirmation matching (which would wrongly mark a provision
# "paid" whenever a real invoice happens to share its Documento key).
#
# df: combined ledger rows (SAP + manual + provisions), already empresa-
# filtered and past the manual-papelera anti-join -- the caller's concern,
# not this function's; papelera_df is still needed here for Source 2 (SAP
# ghosts), a distinct concern from the manual anti-join.
# bancos_confirmados_df/papelera_df: plain data frames, not reactives --
# callers resolve their own shared$xxx() and pass the result in, keeping this
# file's "no reactives, no S3" contract (see file header) intact.
compute_confirmed_flags <- function(df, ledger, bancos_confirmados_df, papelera_df) {
  tipo_val <- if (ledger == "AR") "cobro" else "pago"
  if (!"confirmed" %in% names(df)) df[["confirmed"]] <- FALSE
  na_conf <- is.na(df[["confirmed"]])
  if (any(na_conf)) df[["confirmed"]][na_conf] <- FALSE

  # A caller with no "source" column at all (e.g. Reporte's Cash Flow Pulse,
  # which is deliberately SAP-only) must get an all-FALSE mask, not
  # logical(0) -- "source" %in% names(df) is a length-1 scalar, so `&`-ing
  # it against !is.na(df[["source"]]) on a NULL/absent column silently
  # collapses the whole mask to zero length, which then breaks every
  # downstream `df[["confirmed"]] | bc_mask` recycling (found 2026-07-24 via
  # Reporte's own tests -- every other caller always has a real "source"
  # column, so this was never exercised until now).
  if ("source" %in% names(df)) {
    is_manual    <- !is.na(df[["source"]]) & df[["source"]] == "manual"
    is_provision <- !is.na(df[["source"]]) & df[["source"]] == "provision"
  } else {
    is_manual    <- rep(FALSE, nrow(df))
    is_provision <- rep(FALSE, nrow(df))
  }

  # Source 1: bancos_confirmados
  # Amount-match guard (Stage 10): standard everywhere bancos_confirmados
  # is matched, deliberately WITHOUT a date-window check -- Mouse's explicit
  # reasoning is that a date guard would treat a normal month-end SAP delay
  # as staleness and reopen a still-valid confirmation. Protects against
  # SAP reusing a DocNum years later for an unrelated future invoice, using
  # the exact key shape already proven in production at
  # R/interco_module.R's .ckey(): 2-decimal-rounded amount appended to the
  # existing (Empresa, Documento, Moneda) key.
  # Matched against df$Saldo_original -- the balance BEFORE this render's
  # abono-netting is applied (build_ledger_df() always sets it, for SAP and
  # manual rows alike, before subtracting active abonos into "Saldo
  # vencido"). df has no "Importe" column at all for SAP rows (confirmed
  # directly against a real SAP snapshot -- only Saldo vencido/Saldo_original
  # exist there); Saldo_original is also stable across time in the one way
  # that matters here: matching the live, ever-shrinking "Saldo vencido"
  # instead would cause exactly the false-negative Mouse is worried about --
  # an abono applied after confirmation would change Saldo vencido and make
  # a genuinely still-valid confirmation silently fail to match, reopening it.
  conf_db <- bancos_confirmados_df
  if (!is.null(conf_db) && nrow(conf_db)) {
    conf_db     <- confirmaciones_vigentes(conf_db)
    conf_active <- conf_db[conf_db[["tipo"]] == tipo_val &
                           !is.na(conf_db[["importe"]]), , drop = FALSE]
    # A confirmation that carries the invoice's posting date (written since
    # 2026-11-04, agenda_filas_factura()) closes it on Empresa + Documento +
    # Moneda + that date: the invoice's identity, which survives the ERP
    # moving its balance and still tells apart a DocNum reused years later.
    # Luis: confirming ALWAYS closes the invoice. Older confirmations keep the
    # amount key below.
    fecha_df <- df[["Fecha de contabilización"]]
    if (nrow(conf_active) && "fecha_factura" %in% names(conf_active) && !is.null(fecha_df)) {
      con_f <- conf_active[!is.na(conf_active[["fecha_factura"]]), , drop = FALSE]
      if (nrow(con_f)) {
        f_key   <- paste(toupper(trimws(df[["Empresa"]])), toupper(trimws(df[["Documento"]])),
                         toupper(trimws(df[["Moneda"]])), format(as.Date(fecha_df)))
        f_conf  <- paste(toupper(trimws(con_f[["empresa"]])), toupper(trimws(con_f[["documento"]])),
                         toupper(trimws(con_f[["moneda"]])), format(as.Date(con_f[["fecha_factura"]])))
        f_mask  <- (f_key %in% f_conf) & !is_manual & !is_provision
        df[["confirmed"]] <- df[["confirmed"]] | f_mask
        if (!"is_paid_ghost" %in% names(df)) df[["is_paid_ghost"]] <- FALSE
        df[["is_paid_ghost"]] <- df[["is_paid_ghost"]] | f_mask
      }
      conf_active <- conf_active[is.na(conf_active[["fecha_factura"]]), , drop = FALSE]
    }
    if (nrow(conf_active)) {
      bc_keys   <- unique(conf_active[, c("empresa","documento","moneda","importe"),
                                      drop = FALSE])
      amt_col   <- if ("Saldo_original" %in% names(df)) "Saldo_original" else "Saldo vencido"
      match_key <- paste(toupper(trimws(df[["Empresa"]])),
                         toupper(trimws(df[["Documento"]])),
                         toupper(trimws(df[["Moneda"]])),
                         sprintf("%.2f", round(as.numeric(df[[amt_col]]), 2)))
      conf_key  <- paste(toupper(trimws(bc_keys[["empresa"]])),
                         toupper(trimws(bc_keys[["documento"]])),
                         toupper(trimws(bc_keys[["moneda"]])),
                         sprintf("%.2f", round(as.numeric(bc_keys[["importe"]]), 2)))
      bc_mask   <- (match_key %in% conf_key) & !is_manual & !is_provision
      df[["confirmed"]]   <- df[["confirmed"]] | bc_mask
      if (!"is_paid_ghost" %in% names(df)) df[["is_paid_ghost"]] <- FALSE
      df[["is_paid_ghost"]] <- df[["is_paid_ghost"]] | bc_mask
    }
  }

  # Source 2: papelera SAP ghosts
  if (!is.null(papelera_df) && nrow(papelera_df)) {
    pap_this <- papelera_df[papelera_df[["ledger"]] == ledger |
                           papelera_df[["ledger"]] == "MIXED", , drop = FALSE]
    if (nrow(pap_this)) {
      # Deliberately NOT is_erp_sourced() here: an ambiguous/legacy NA source
      # in papelera should NOT be treated as a SAP ghost (which stays
      # visible, excluded from sums) -- safer to fall through to the manual
      # anti-join path (fully hidden) for unknown provenance. Still literally
      # SAP_B1_ERP_TYPE rather than generic (matching the override feature's
      # own reasoning above) -- revisit once a second ERP's ghost-visibility
      # behavior is actually decided, don't assume it should match SAP's.
      sap_pap <- pap_this[!is.na(pap_this[["source"]]) &
                            pap_this[["source"]] == SAP_B1_ERP_TYPE,
                          c("Empresa","Moneda","Documento"), drop = FALSE]
      if (nrow(sap_pap)) {
        match_key <- paste(df[["Empresa"]], df[["Moneda"]], df[["Documento"]])
        pap_key   <- paste(sap_pap[["Empresa"]], sap_pap[["Moneda"]], sap_pap[["Documento"]])
        # !is_manual guard (found 2026-07-24): this source only ever means
        # "an ERP row was trashed" -- without the guard, a brand-new manual
        # invoice that happens to reuse the same Empresa+Moneda+Documento key
        # as some unrelated, previously-archived SAP invoice (e.g. a generic
        # placeholder like "test") gets wrongly treated as that SAP ghost,
        # marked confirmed, and then deleted outright by the manual-removal
        # block below -- silently vanishing from the calendar even though it
        # was never staged, confirmed, or deleted by any real user action.
        # Source 1 (bancos_confirmados, above) already has this guard; this
        # one was missing it.
        ghost_mask <- (match_key %in% pap_key) & !is_manual & !is_provision
        df[["confirmed"]] <- df[["confirmed"]] | ghost_mask
        if (!"is_ghost" %in% names(df)) df[["is_ghost"]] <- FALSE
        df[["is_ghost"]]  <- df[["is_ghost"]] | ghost_mask
      }
    }
  }

  # Manual rows: keep in df with confirmed=TRUE → calendar excludes from
  # totals, day modal shows with strikethrough (SAP rows). Manual rows:
  # remove entirely from df → disappear from calendar and modal (their real
  # data survives in the archive, restorable via undo).
  if ("source" %in% names(df) && any(df[["confirmed"]] & df[["source"]] == "manual")) {
    df <- df[!(df[["confirmed"]] & df[["source"]] == "manual"), , drop = FALSE]
  }

  # Provisions cannot receive ANY payment/confirmation flag.
  # Belt: masks above already exclude is_provision.
  # Suspenders: forcibly clear all three flags here so the '✓ Pagado' badge
  # can never render on a provision row regardless of future mask changes.
  if ("source" %in% names(df) && "confirmed" %in% names(df)) {
    prov_mask <- !is.na(df[["source"]]) & df[["source"]] == "provision"
    if (any(prov_mask)) {
      df[["confirmed"]][prov_mask] <- FALSE
      if ("is_paid_ghost" %in% names(df)) df[["is_paid_ghost"]][prov_mask] <- FALSE
      if ("is_ghost"      %in% names(df)) df[["is_ghost"]][prov_mask]      <- FALSE
    }
  }

  df
}

# ── "Send straight to Agenda" — derive, never fabricate ────────────────────────
# Builds the pagar_hoy staging row for the one-click "create/convert and
# immediately stage to Agenda" convenience (direct manual-entry creation,
# Pasivos conversion modal). Takes the manual_inv row exactly as it now
# exists in the root table (post-write) — every field comes from that row,
# never from the raw form inputs that produced it. This is what keeps
# Agenda a mirror of Calendario's root data instead of a second,
# independently-computed copy that can silently drift from it.
# manual_row: single-row data frame, already bound into manual_inv/manual_df.
stage_manual_row_to_agenda <- function(manual_row, user) {
  stopifnot(is.data.frame(manual_row), nrow(manual_row) == 1)
  prov_id  <- manual_row[["provision_id"]][1]
  has_prov <- !is.na(prov_id) && nzchar(prov_id %||% "")
  now      <- Sys.time()
  tibble::tibble(
    id           = manual_row[["id"]][1],
    ledger       = manual_row[["ledger"]][1],
    Empresa      = manual_row[["Empresa"]][1],
    Moneda       = manual_row[["Moneda"]][1],
    Documento    = manual_row[["Documento"]][1],
    Parte        = manual_row[["Parte"]][1]  %||% "",
    Codigo       = manual_row[["Codigo"]][1] %||% "",
    tipo_item    = "factura",
    Importe      = manual_row[["Importe"]][1],
    FechaVenc    = as.Date(manual_row[["Fecha de vencimiento"]][1]),
    staged_by    = user,
    staged_at    = now,
    status       = "pending",
    provision_id = if (has_prov) prov_id else NA_character_,
    liability_id = if (has_prov) manual_row[["liability_id"]][1] else NA_character_,
    source       = if (has_prov) "provision" else "manual"
  )
}

# Writes one change to the Agenda (Agregar Item → "Enviar a Agenda").
# `leer()` reads the Agenda as S3 holds it now and stops when it can't;
# `cambio(ph)` returns it with the change (the caller's upsert, so the
# Agenda guardians see the caller -- its permission check and its rows);
# `guardar(df, base)` writes it rebased on `base`. Never falls back to the
# session's copy (a stale copy used to be saved over other sessions' rows),
# and never reports success for a failed write.
# Returns list(ok, ph, error): ph is the Agenda written (NULL when not ok).
agregar_a_agenda <- function(leer, cambio, guardar) {
  ph_base <- tryCatch(leer(), error = function(e) e)
  if (inherits(ph_base, "error") || !is.data.frame(ph_base))
    return(list(ok = FALSE, ph = NULL,
                error = if (inherits(ph_base, "error")) conditionMessage(ph_base) else "no agenda"))
  ph <- tryCatch(cambio(ph_base), error = function(e) e)
  if (inherits(ph, "error")) return(list(ok = FALSE, ph = NULL, error = conditionMessage(ph)))
  r <- tryCatch({ guardar(ph, ph_base); NULL }, error = function(e) e)
  if (!is.null(r)) return(list(ok = FALSE, ph = NULL, error = conditionMessage(r)))
  list(ok = TRUE, ph = ph, error = NULL)
}