# =============================================================================
# R/settings/settings_helpers.R
# Public helpers, schemas, and the complete MX bank catalog.
# =============================================================================
# (Originally part of R/settings_module.R; split in Stage 7.1 for session efficiency.)

# =============================================================================
# R/settings_module.R  —  Settings Hub
#
# Opened via the ⚙ gear icon in the calendar control bar (btn_settings).
#
# Sidebar panels:
#   1. Intercompany          — IC client / supplier code lists
#   2. Catálogo Proveedores  — vendor alias + CLABE for PPL export
#   3. Cuentas de Empresa    — bank accounts per company (feeds cuenta_origen)
#
# "Cuentas de Empresa" is a two-level hierarchy:
#   Banco (institution)  →  Cuenta (account at that bank)
#
#   Banco fields:  nombre, clave (3-digit CECOBAN)
#   Cuenta fields: Empresa, razon_social, rfc, Moneda, alias (≤15, informational),
#                  cuenta (12-digit number used as PPL origen field),
#                  clabe_interbancaria (18-digit CLABE, for reference),
#                  is_ppl_default (TRUE = use this account as PPL cuenta origen),
#                  banco_id (FK), activa
#
# ── Integration changes needed in app.R ──────────────────────────────────────
# global.R — add to S3_KEYS:
#   ctas_bancos  = "ctas_bancos.rds"
#   ctas_cuentas = "ctas_cuentas.rds"
#
# app.R server — add reactiveVals:
#   ctas_cuentas <- reactiveVal(load_ctas_cuentas())
#   # add to shared list: ctas_cuentas = ctas_cuentas
#
# app.R server — add alongside other settings_* calls:
#   settings_cuentas_observer(input, output, session, shared)
#
# pagar_hoy_module.R — replace .get_cuenta_origen() calls with:
#   get_cuenta_ppl(empresa, moneda, shared)
# =============================================================================

# ── Public helper: fetch 12-digit origin account for PPL ──────────────────────
get_cuenta_ppl <- function(empresa, moneda, shared) {
  ctas <- tryCatch({
    if (!is.null(shared$ctas_cuentas)) shared$ctas_cuentas()
    else load_ctas_cuentas()
  }, error = function(e) NULL)

  if (is.null(ctas) || !nrow(ctas)) {
    return(.get_cuenta_origen_legacy(empresa, moneda, shared))
  }

  # ctas$Empresa stores initials ("NTS"); callers may pass either the full name
  # ("Networks Trucking Services") or initials. Accept both.
  ini          <- names(COMPANY_MAP)[unname(COMPANY_MAP) == empresa]
  empresa_keys <- unique(c(empresa, ini))

  # Priority 1: empresa + moneda + PPL default flag
  m <- ctas |> dplyr::filter(
    Empresa %in% empresa_keys, toupper(Moneda) == toupper(moneda),
    activa == TRUE, isTRUE(is_ppl_default)) |> dplyr::slice(1)
  if (nrow(m) && nzchar(trimws(m$cuenta[1]))) return(trimws(m$cuenta[1]))

  # Priority 2: empresa + moneda, any active
  m <- ctas |> dplyr::filter(
    Empresa %in% empresa_keys, toupper(Moneda) == toupper(moneda),
    activa == TRUE) |> dplyr::slice(1)
  if (nrow(m) && nzchar(trimws(m$cuenta[1]))) return(trimws(m$cuenta[1]))

  # Priority 3: empresa only (currency-agnostic fallback)
  m <- ctas |> dplyr::filter(Empresa %in% empresa_keys, activa == TRUE) |> dplyr::slice(1)
  if (nrow(m) && nzchar(trimws(m$cuenta[1]))) return(trimws(m$cuenta[1]))

  ""
}

# ── Public helper: return data.frame for account selector in PPL modal ────────
# Columns: cuenta, banco, alias, is_ppl_default. NULL when no matches.
get_cuentas_ppl_data <- function(empresa, moneda, shared) {
  ctas <- tryCatch({
    if (!is.null(shared$ctas_cuentas)) shared$ctas_cuentas()
    else load_ctas_cuentas()
  }, error = function(e) NULL)
  if (is.null(ctas) || !nrow(ctas)) return(NULL)

  ini          <- names(COMPANY_MAP)[unname(COMPANY_MAP) == empresa]
  empresa_keys <- unique(c(empresa, ini))

  matches <- ctas |> dplyr::filter(
    Empresa %in% empresa_keys, toupper(Moneda) == toupper(moneda), activa == TRUE)
  if (!nrow(matches)) return(NULL)

  cid        <- tryCatch(shared$effective_client_id(), error = function(e) NULL)
  bancos_cat <- tryCatch(load_ctas_bancos(client_id = cid), error = function(e) NULL)
  banco_map  <- if (!is.null(bancos_cat) && nrow(bancos_cat))
    setNames(bancos_cat$nombre, bancos_cat$id) else character(0)

  data.frame(
    cuenta         = trimws(matches$cuenta),
    banco          = ifelse(matches$banco_id %in% names(banco_map),
                            banco_map[matches$banco_id], ""),
    alias          = trimws(dplyr::coalesce(matches$alias, "")),
    is_ppl_default = !is.na(matches$is_ppl_default) & matches$is_ppl_default,
    stringsAsFactors = FALSE
  )
}

.get_cuenta_origen_legacy <- function(empresa, moneda, shared) {
  if (!is.null(shared$bancos_cuentas)) {
    ctas <- tryCatch(shared$bancos_cuentas(), error = function(e) NULL)
    if (!is.null(ctas) && nrow(ctas)) {
      m <- ctas |> dplyr::filter(Empresa == empresa, activa == TRUE) |> dplyr::slice(1)
      if (nrow(m) && nzchar(trimws(m$cuenta[1]))) return(trimws(m$cuenta[1]))
    }
  }
  legacy <- tryCatch(load_bancos(), error = function(e) NULL)
  if (!is.null(legacy) && nrow(legacy)) {
    m <- legacy |> dplyr::filter(Empresa == empresa, activa == TRUE) |> dplyr::slice(1)
    if (nrow(m) && nzchar(trimws(m$cuenta[1]))) return(trimws(m$cuenta[1]))
  }
  ""
}

# ── Schemas ────────────────────────────────────────────────────────────────────
.schema_ctas_bancos <- function() tibble::tibble(
  id     = character(),
  nombre = character(),
  clave  = character()   # 3-digit CECOBAN code, e.g. "030"
)

.schema_ctas_cuentas <- function() tibble::tibble(
  id                  = character(),
  banco_id            = character(),   # FK → ctas_bancos$id
  Empresa             = character(),   # initials: "NTS", "NCS", …
  razon_social        = character(),
  rfc                 = character(),
  Moneda              = character(),   # "MXN" | "USD"
  alias               = character(),   # informational (≤15 chars)
  cuenta              = character(),   # 11-digit account used as PPL origen
  clabe_interbancaria = character(),   # 18-digit CLABE for reference / SPEI receipts
  is_ppl_default      = logical(),
  saldo_inicial       = numeric(),
  activa              = logical()
)

load_ctas_bancos <- function(client_id = NULL) {
  obj <- tryCatch(.s3_read_with(S3_KEYS$ctas_bancos, client_id = client_id), error = function(e) NULL)
  if (is.null(obj) || !is.data.frame(obj)) return(.schema_ctas_bancos())
  schema <- .schema_ctas_bancos()
  for (col in names(schema))
    if (!col %in% names(obj)) obj[[col]] <- schema[[col]][NA_integer_]
  obj
}

load_ctas_cuentas <- function(client_id = NULL) {
  obj <- tryCatch(.s3_read_with(S3_KEYS$ctas_cuentas, client_id = client_id), error = function(e) NULL)
  if (is.null(obj) || !is.data.frame(obj)) return(.schema_ctas_cuentas())
  schema <- .schema_ctas_cuentas()
  for (col in names(schema))
    if (!col %in% names(obj)) obj[[col]] <- schema[[col]][NA_integer_]
  obj
}

save_ctas_bancos  <- function(df, client_id = NULL) .s3_write(df, S3_KEYS$ctas_bancos, client_id = client_id)
save_ctas_cuentas <- function(df, client_id = NULL) .s3_write(df, S3_KEYS$ctas_cuentas, client_id = client_id)

# ── Complete MX bank catalog (BanBajío CECOBAN codes) ─────────────────────────
# Format: "Nombre|CLAVE" — JS splits on "|" to fill the two text inputs.
# Source: BanBajío Catálogos BB (89 institutions).
.BANCOS_MX_CHOICES <- c(
  "— escribir manualmente —"       = "",
  "BanBajío (030)"                 = "BanBajío|030",
  "BBVA Bancomer (012)"            = "BBVA Bancomer|012",
  "Banamex / Citibanamex (002)"    = "Banamex|002",
  "Santander (014)"                = "Santander|014",
  "Banorte / IXE (072)"            = "Banorte|072",
  "HSBC (021)"                     = "HSBC|021",
  "Scotiabank (044)"               = "Scotiabank|044",
  "Inbursa (036)"                  = "Inbursa|036",
  "Banregio (058)"                 = "Banregio|058",
  "Afirme (062)"                   = "Afirme|062",
  "ABC Capital (138)"              = "ABC Capital|138",
  "Actinver (133)"                 = "Actinver|133",
  "Arcus (706)"                    = "Arcus|706",
  "ASP Integra OPC (659)"          = "ASP Integra OPC|659",
  "Autofin (128)"                  = "Autofin|128",
  "Azteca (127)"                   = "Azteca|127",
  "Babien (166)"                   = "Babien|166",
  "Banco Covalto (154)"            = "Banco Covalto|154",
  "Banco S3 (160)"                 = "Banco S3|160",
  "Bancomext (006)"                = "Bancomext|006",
  "Bancoppel (137)"                = "Bancoppel|137",
  "Bancrea (152)"                  = "Bancrea|152",
  "Banjercito (019)"               = "Banjercito|019",
  "Bank of America (106)"          = "Bank of America|106",
  "Bank of China (159)"            = "Bank of China|159",
  "Bankaool (147)"                 = "Bankaool|147",
  "Banobras (009)"                 = "Banobras|009",
  "Bansi (060)"                    = "Bansi|060",
  "Barclays (129)"                 = "Barclays|129",
  "Bbase (145)"                    = "Bbase|145",
  "Bmonex (112)"                   = "Bmonex|112",
  "Caja Pop Mexicana (677)"        = "Caja Pop Mexicana|677",
  "Caja Telefonista (683)"         = "Caja Telefonista|683",
  "CB Intercam (630)"              = "CB Intercam|630",
  "CBM Banco (124)"                = "CBM Banco|124",
  "CI Bolsa (631)"                 = "CI Bolsa|631",
  "CIBanco (143)"                  = "CIBanco|143",
  "Compartamos (130)"              = "Compartamos|130",
  "Consubanco (140)"               = "Consubanco|140",
  "Credicapital (652)"             = "Credicapital|652",
  "Crediclub (688)"                = "Crediclub|688",
  "Credit Suisse (126)"            = "Credit Suisse|126",
  "Cristobal Colon (680)"          = "Cristobal Colon|680",
  "Cuenca (723)"                   = "Cuenca|723",
  "Donde (151)"                    = "Donde|151",
  "Finamex (616)"                  = "Finamex|616",
  "Fincomun (634)"                 = "Fincomun|634",
  "Fomped (689)"                   = "Fomped|689",
  "Fondeadora (699)"               = "Fondeadora|699",
  "Fondo FIRA (685)"               = "Fondo FIRA|685",
  "GBM (601)"                      = "GBM|601",
  "HDI Seguros (636)"              = "HDI Seguros|636",
  "Hipotecaria Federal (168)"      = "Hipotecaria Federal|168",
  "ICBC (155)"                     = "ICBC|155",
  "Indeval (902)"                  = "Indeval|902",
  "Inmobiliario (150)"             = "Inmobiliario|150",
  "Intercam Banco (136)"           = "Intercam Banco|136",
  "Invercap (686)"                 = "Invercap|686",
  "Invex (059)"                    = "Invex|059",
  "JP Morgan (110)"                = "JP Morgan|110",
  "Klar (661)"                     = "Klar|661",
  "Kuspit (653)"                   = "Kuspit|653",
  "Libertad (670)"                 = "Libertad|670",
  "Masari (602)"                   = "Masari|602",
  "Mercado Pago W (722)"           = "Mercado Pago W|722",
  "Mifel (042)"                    = "Mifel|042",
  "Mizuho Bank (158)"              = "Mizuho Bank|158",
  "Monexcb (600)"                  = "Monexcb|600",
  "MUFG (108)"                     = "MUFG|108",
  "Multiva Banco (132)"            = "Multiva Banco|132",
  "Multiva Cbolsa (613)"           = "Multiva Cbolsa|613",
  "NAFIN (135)"                    = "NAFIN|135",
  "Nu Mexico (638)"                = "Nu Mexico|638",
  "NVIO (710)"                     = "NVIO|710",
  "Oskndia (649)"                  = "Oskndia|649",
  "Pagatodo (148)"                 = "Pagatodo|148",
  "Profuturo (620)"                = "Profuturo|620",
  "Sabadell (156)"                 = "Sabadell|156",
  "Shinhan (157)"                  = "Shinhan|157",
  "Skandia (623)"                  = "Skandia|623",
  "STP (646)"                      = "STP|646",
  "Tactiv CB (648)"                = "Tactiv CB|648",
  "Tesored (703)"                  = "Tesored|703",
  "Transfer (684)"                 = "Transfer|684",
  "Unagra (656)"                   = "Unagra|656",
  "Valmex (617)"                   = "Valmex|617",
  "Value (605)"                    = "Value|605",
  "Ve Por Mas (113)"               = "Ve Por Mas|113",
  "Vector (608)"                   = "Vector|608",
  "Volkswagen (141)"               = "Volkswagen|141",
  "Otro"                           = "Otro|"
)

