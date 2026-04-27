# =============================================================================
# bootstrap_proveedores.R
#
# Run ONCE from your R console (with .Renviron loaded) to seed the proveedores
# catalog in S3 from the 23 suppliers extracted from pagos_06032026_002.txt.
#
# After this, new suppliers can be added via the Bancos → Proveedores UI (Phase 2)
# or manually by re-running this script with additional rows.
#
# Usage:
#   source("bootstrap_proveedores.R")
#   bootstrap_proveedores(empresa = "NTS")   # or whichever company this batch belongs to
# =============================================================================

source("global.R")   # loads persistence.R, S3 init, etc.

bootstrap_proveedores <- function(empresa = "NTS", dry_run = FALSE) {

  # ── Seed data from pagos_06032026_002.txt ────────────────────────────────
  # Empresa column: these are NTS (Networks Trucking Services) suppliers
  # based on origin account 133861560201.
  # Change empresa parameter if seeding for a different company.

  seed <- tibble::tribble(
    ~alias,            ~clabe,               ~medio_pago, ~nombre,
    "FONACOT",         "012914002012607667",  "SPI",  "FONACOT",
    "ADANRUBIO",       "002680045400148042",  "SPI",  "Adan Rubio",
    "AUTOCONSUMO",     "127180001399402852",  "SPI",  "Autoconsumo",
    "AUTOPARTESYMAS",  "044320010096188138",  "SPI",  "Autopartes y Mas",
    "CLEMENTEMISAEL",  "012680004888669459",  "SPI",  "Clemente Misael",
    "COMBUSTIBLESDIE", "002680476300004473",  "SPI",  "Combustibles Diesel",
    "DARIOMEJIA",      "012680015946664482",  "SPI",  "Dario Mejia",
    "VIMAR",           "014078920018617211",  "SPI",  "Vimar",
    "EJESYCOMPONENTE", "002680091700455887",  "SPI",  "Ejes y Componentes",
    "ERNESTOGONZA",    "002905701871197997",  "SPI",  "Ernesto Gonzalez",
    "IMPORTADORADEFI", "072580012200654064",  "SPI",  "Importadora de Filtros",
    "GALO",            "012700001151831622",  "SPI",  "Galo",
    "JUANCARLOSRANGE", "021680040567070372",  "SPI",  "Juan Carlos Rangel",
    "LUISDEJESUS",     "002905700649469926",  "SPI",  "Luis de Jesus",
    "MULTISERVICE",    "106180000144660159",  "SPI",  "Multiservice",
    "PROVEEDORMAYOR",  "014580655065108455",  "SPI",  "Proveedor Mayoreo",
    "REFACCIONESMEDI", "000000030036960201",  "BCO",  "Refacciones Medina",
    "SANITARIOSSANLU", "000000153097270201",  "BCO",  "Sanitarios San Luis",
    "SAULBEDOLLA",     "002680469900141248",  "SPI",  "Saul Bedolla",
    "TEAMMF",          "072700013353900208",  "SPI",  "Team MF",
    "TRANPORTESFORA",  "014497655109081754",  "SPI",  "Transportes Foraneos",
    "VESEGU",          "058680000000814273",  "SPI",  "Vesegu",
    "NREBB",           "000000133865030201",  "BCO",  "NRE BB"
  ) |>
    dplyr::mutate(
      id            = purrr::map_chr(seq_len(dplyr::n()), ~uuid::UUIDgenerate()),
      Empresa       = empresa,
      codigo        = NA_character_,   # SAP CardCode — fill in later
      rfc           = NA_character_,   # RFC — fill in later
      tipo          = dplyr::if_else(medio_pago == "BCO", "021", "012"),
      banco_destino = NA_character_,
      activo        = TRUE
    )

  cat("== Proveedores a importar ==\n")
  print(seed |> dplyr::select(alias, nombre, clabe, medio_pago, Empresa))
  cat(sprintf("\nTotal: %d proveedores para empresa '%s'\n", nrow(seed), empresa))

  if (dry_run) {
    cat("\n[DRY RUN] No se guardó nada. Llama con dry_run=FALSE para guardar.\n")
    return(invisible(seed))
  }

  # Merge with any existing records (don't overwrite different-company rows)
  existing <- tryCatch(
    load_proveedores(),
    error = function(e) { cat("No existing proveedores found.\n"); tibble::tibble() }
  )

  # Remove existing rows for this empresa to avoid dupes, then add seed
  keep <- if (nrow(existing))
    existing |> dplyr::filter(Empresa != empresa)
  else
    tibble::tibble()

  combined <- dplyr::bind_rows(keep, seed)
  save_proveedores(combined)

  cat(sprintf("\n✓ Guardado en S3: %d proveedores totales (%d para %s)\n",
              nrow(combined), nrow(seed), empresa))
  invisible(combined)
}
