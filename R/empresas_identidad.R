# =============================================================================
# R/empresas_identidad.R
# Who the client's own companies are, by name and RFC (T47). Shared by:
#   - R/bancos_detect.R: which company a BanBajío TXT header names;
#   - apply_ic_filter() (R/data_pipeline.R): for a client WITHOUT an
#     intercompany registry, which counterparts are the group's own
#     companies. With a registry (CardCodes or RFCs), the registry decides
#     and nothing here runs.
#
#   .norm_razon(x)              upper case, ASCII, punctuation -> space,
#                               legal-form tokens ("S.A. DE C.V.") dropped
#   .empresa_nombres(ini, df)   razón social, short name and aliases of one
#   empresas_identidad(df)      every active company's RFCs and normalized
#                               names (generic ones left out)
#   ic_por_identidad(parte, rfc, identidad)  TRUE where a row is one of them
#
# Never approximate: "NETWORKS LOGISTICS INTERNATIONAL" is not "NETWORKS
# LOGISTICS". Moved from R/bancos_detect.R without changes (no copies).
# =============================================================================

# Legal-form and filler tokens that carry no identity ("S.A. DE C.V.", etc.).
.BNC_LEGAL_TOKENS <- c(
  "SA", "S", "A", "DE", "CV", "C", "V", "RL", "SC", "SAPI", "SAB", "SOFOM",
  "ENR", "ER", "SOCIEDAD", "ANONIMA", "CAPITAL", "VARIABLE",
  "RESPONSABILIDAD", "LIMITADA", "CIVIL", "Y", "EL", "LA", "LOS", "LAS", "DEL"
)

# Uppercase, ASCII, punctuation -> space, legal-form tokens dropped.
.norm_razon <- function(x) {
  x <- as.character(x %||% "")
  x[is.na(x)] <- ""
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x[is.na(x)] <- ""
  x <- toupper(gsub("[^A-Za-z0-9]+", " ", x))
  vapply(strsplit(trimws(x), "\\s+"), function(tk) {
    tk <- tk[nzchar(tk) & !tk %in% .BNC_LEGAL_TOKENS]
    paste(tk, collapse = " ")
  }, character(1))
}

# Names an empresa is known by in the client's registry.
.empresa_nombres <- function(ini, empresas_df) {
  if (is.null(empresas_df) || !nrow(empresas_df) || !"initials" %in% names(empresas_df))
    return(character(0))
  r <- empresas_df[!is.na(empresas_df$initials) & empresas_df$initials == ini, , drop = FALSE]
  if (!nrow(r)) return(character(0))
  nm <- c(r$razon_social %||% character(0), r$nombre_corto %||% character(0))
  if ("nombres_alt" %in% names(r)) {
    for (j in r$nombres_alt) {
      alt <- tryCatch(as.character(jsonlite::fromJSON(j %||% "[]")),
                      error = function(e) character(0))
      nm <- c(nm, alt)
    }
  }
  nm <- trimws(as.character(nm))
  unique(nm[!is.na(nm) & nzchar(nm)])
}

# ── Identidad de las empresas del grupo (T47) ────────────────────────────────

# A normalized name of a single word under 5 letters ("NL", "GRUPO" is 5)
# is too generic to mark a row on its own: only its RFC can (Coordinador, P4).
.IDENT_GENERICO_MAX <- 4L

# The client's active companies as identity: data.frame(nombre, rfc), one row
# per normalized name (razón social, short name, aliases) with the RFC of its
# company (NA when the company has none); generic names left out. NULL with
# no companies: the caller keeps today's behavior.
empresas_identidad <- function(empresas) {
  if (!is.data.frame(empresas) || !nrow(empresas) || !"initials" %in% names(empresas)) return(NULL)
  e <- empresas[!(empresas$deleted %in% TRUE) & !(empresas$activa %in% FALSE) & !is.na(empresas$initials), , drop = FALSE]
  if (!nrow(e)) return(NULL)
  rfc_de <- function(ini) {
    r <- if ("rfc" %in% names(e)) toupper(trimws(as.character(e$rfc[e$initials == ini][1]))) else NA_character_
    if (is.na(r) || !nzchar(r)) NA_character_ else r
  }
  filas <- lapply(unique(e$initials), function(ini) {
    n <- unique(.norm_razon(.empresa_nombres(ini, e)))
    n <- n[nzchar(n)]
    if (!length(n)) return(NULL)
    data.frame(nombre = n, rfc = rfc_de(ini), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, filas)
  if (is.null(out) || !nrow(out)) return(NULL)
  tk <- strsplit(out$nombre, " ", fixed = TRUE)
  generico <- vapply(tk, function(t) length(t) == 1L && nchar(t[1]) <= .IDENT_GENERICO_MAX, logical(1))
  rfcs <- unique(stats::na.omit(vapply(unique(e$initials), rfc_de, character(1))))
  list(nombres = out[!generico, , drop = FALSE], rfcs = rfcs)
}

# TRUE where a row is one of the group's companies:
#   - a row with an RFC: that RFC is a company's (exact);
#   - a row without one: its counterpart, normalized, is a company's name
#     exactly (razón social, short name or alias; never approximate).
ic_por_identidad <- function(parte, rfc, identidad) {
  n <- max(length(parte), length(rfc))
  if (is.null(identidad) || !n) return(rep(FALSE, n))
  r <- toupper(trimws(as.character(rep_len(if (is.null(rfc)) NA_character_ else rfc, n))))
  con_rfc <- !is.na(r) & nzchar(r)
  por_nombre <- .norm_razon(rep_len(parte, n)) %in% identidad$nombres$nombre
  ifelse(con_rfc, r %in% identidad$rfcs, por_nombre)
}

# ── Qué empresa del catálogo nombra un texto (T61) ───────────────────────────

# The id (empresas$id) of the company each text names, NA when none or more
# than one does. Requisiciones compares companies by id, never by text: a
# requisition typed in the form holds the initials, one made "desde
# provisión" used to copy the provision's text, which is sometimes the
# short name. Exact only, never approximate, in this order:
#   1. current initials (case and spaces aside);
#   2. initials it had before a complete rename (iniciales_anteriores, CM29);
#   3. its razón social, short name or an alias, normalized with
#      .norm_razon() (legal-form tokens and punctuation aside).
# Deleted companies are left out; inactive ones count (a folder may name one).
empresa_id_de <- function(texto, empresas) {
  n <- length(texto)
  out <- rep(NA_character_, n)
  if (!n || !is.data.frame(empresas) || !nrow(empresas) || !all(c("id", "initials") %in% names(empresas)))
    return(out)
  e <- empresas[!(empresas$deleted %in% TRUE) & !is.na(empresas$id) & !is.na(empresas$initials), , drop = FALSE]
  if (!nrow(e)) return(out)
  t <- toupper(trimws(as.character(texto)))
  ini <- toupper(trimws(e$initials))
  unico <- function(ids) { ids <- unique(ids); if (length(ids) == 1L) ids else NA_character_ }
  ant <- if ("iniciales_anteriores" %in% names(e))
    lapply(e$iniciales_anteriores, function(j) toupper(trimws(tryCatch(as.character(jsonlite::fromJSON(j %||% "[]")),
                                                                       error = function(x) character()))))
  else vector("list", nrow(e))
  nombres <- lapply(e$initials, function(i) { x <- .norm_razon(.empresa_nombres(i, e)); x[nzchar(x)] })
  tn <- .norm_razon(texto)
  for (k in seq_len(n)) {
    if (is.na(t[k]) || !nzchar(t[k])) next
    id <- unico(e$id[ini == t[k]])
    if (is.na(id)) id <- unico(e$id[vapply(ant, function(a) t[k] %in% a, logical(1))])
    if (is.na(id) && nzchar(tn[k])) id <- unico(e$id[vapply(nombres, function(a) tn[k] %in% a, logical(1))])
    out[k] <- id
  }
  out
}
