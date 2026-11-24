# =============================================================================
# R/tema_registro.R
# HopDesk's design engine (TEMA): the one place that says what the app looks
# like. Approved design: hopdesk_coord/tablas/reportes/TEMA_paso0.md.
#
# Two layers of CSS custom properties, all named --hd-*:
#   paleta   the brand's constants (navy, ink, paper, gold...). The same in
#            every theme; www/hd_marca.css defines the same values under
#            .hd-marca and tests/test_temas_guardian.R keeps them equal.
#   roles    what a colour is FOR (fondo, primario, peligro...). Each theme
#            gives every role its value; screens use roles, never hex.
#   comunes  roles that are the same in every theme (tags A/B/C are a product
#            rule; type and spacing scales).
#
# www/hd_temas.css is GENERATED from this file by scripts/generar_temas_css.R
# and committed; nothing writes www/ at run time. The CSS puts the default
# theme on :root, so a page with no data-hd-tema attribute (or a script that
# never ran) still gets it, and the other themes on [data-hd-tema="..."].
# Emails, Word, Excel, PDF and SVG take resolved hex from tema_tokens(),
# because Gmail and Word do not understand var().
#
# Themes:
#   clasico  the bright-blue look the app has today (bslib flatly, #0A58CA).
#            Bootstrap keeps painting its own components with flatly (the
#            bridge in www/hd_tema_bootstrap.css applies under marca only).
#            Each role holds the most used hand-written value of its group
#            (TEMA_inventario.md §3), so moving a screen from hex to roles
#            keeps its look under Clásico.
#   marca    Panorama's look (navy + Fraunces), the official one. Default
#            only once Luis switches it on (TEMA stage 5).
#
# Loaded by shinyapps.io in alphabetical order before app.R (rule 29): only
# literal data and function definitions up here, no calls into other files.
# =============================================================================

HD_TEMA_OMISION <- "clasico"

# Bumped with every change to the generated file (browser cache on shinyapps.io).
HD_TEMAS_CSS_VERSION <- 21L

# ── Paleta (same in every theme) ─────────────────────────────────────────────
.HD_PALETA <- list(
  marino        = "#13213A",
  tinta         = "#1A2233",
  pizarra       = "#3B4558",
  gris          = "#5A6474",
  niebla        = "#B7C2D4",
  linea         = "#E3E8F0",
  papel         = "#F7F6F2",
  crema         = "#F3F1EA",
  blanco        = "#FFFFFF",
  oro           = "#C9AE6E",
  `oro-osc`     = "#B0924F",
  `oro-claro`   = "#E4CF9B",
  `oro-velo`    = "rgba(201, 174, 110, 0.14)",
  `ambar-bg`    = "#F7E9E1", `ambar-bd`  = "#E8CBBB", `ambar-tag`  = "#F0D6C8", `ambar-fg`  = "#9A4524",
  `salvia-bg`   = "#E2EEEF", `salvia-bd` = "#C6DDDF", `salvia-tag` = "#D0E5E7", `salvia-fg` = "#1E5B63",
  `lila-bg`     = "#F3E8EE", `lila-bd`   = "#E6CFDC", `lila-tag`   = "#EBD8E3", `lila-fg`   = "#6B3358",
  `olivo-bg`    = "#EEF0E2", `olivo-bd`  = "#DADFC6", `olivo-tag`  = "#E2E6CF", `olivo-fg`  = "#56602F",
  sube          = "#E6A383",
  baja          = "#A9C3D6",
  # The weekly mails' own tones (R/avisos_correos.R, CORREO_COLORES): Marca's,
  # used only there (the week's chart, the sand and review boxes, the lines).
  lino          = "#EDEBE4", `lino-bd`   = "#E4E1D8", `lino-riel`  = "#ECE9E1",
  arena         = "#EFE6D2", `arena-fg`  = "#6B5320", `arena-osc`  = "#E6D6AC",
  `niebla-osc`  = "#9AA6BA", `ambar-osc` = "#7C3519", bruma        = "#D7DCE5",
  `pizarra-clara` = "#5B6A86", `marino-claro` = "#2A3A58",
  serif         = "'Fraunces', Georgia, 'Times New Roman', serif",
  sans          = "'Lato', system-ui, -apple-system, 'Segoe UI', sans-serif"
)

# ── Comunes (roles with one value in every theme) ────────────────────────────
.HD_COMUNES <- list(
  # Tags A (red), B (orange), C (yellow): pill text / pill / row (Luis'
  # rule; the same values as www/hdtable.css and HD_ETIQUETAS today).
  `etq-a-texto` = "#B42318", `etq-a-pastilla` = "#FFE1DE", `etq-a-fila` = "#FFF0F0",
  `etq-b-texto` = "#B54708", `etq-b-pastilla` = "#FFE8D1", `etq-b-fila` = "#FFF3E6",
  `etq-c-texto` = "#8A6100", `etq-c-pastilla` = "#FFF1B8", `etq-c-fila` = "#FFFBE6",
  # The access pages (sign-in, activation, forgot/reset password, app.R):
  # HopDesk's own brand, shown before anyone is known, so ONE value in every
  # theme -- today's (TEMA stage 4, app.R). The guardian freezes them.
  `acceso-fondo` = "#07090D", `acceso-tarjeta` = "#0C1520", `acceso-tarjeta-2` = "#0B1420",
  `acceso-campo` = "#0F1B29", `acceso-campo-foco` = "#122030", `acceso-campo-texto` = "#2A3D52",
  `acceso-oro` = "#B49444", `acceso-oro-hover` = "#CBA84E", `acceso-texto` = "#D8E4F0",
  `acceso-texto-foco` = "#E8F0FA", `acceso-texto-2` = "#7A99B8", `acceso-lead` = "#9FB6CC",
  `acceso-etiqueta` = "#3D5878", `acceso-etiqueta-2` = "#6B8299", `acceso-nota` = "#51708D",
  `acceso-lema` = "#283F58", `acceso-pie` = "#1B2E40", `acceso-error` = "#E8837A",
  `acceso-sombra` = "0 40px 100px rgba(0,0,0,0.72), 0 0 0 1px rgba(180,148,68,0.05)",
  # The loading overlay (www/overlay_init.js), between sign-in and the app:
  # HopDesk's cream and gold, the same in every theme (it shows before the
  # client's theme has arrived), frozen at today's hex like the rest of acceso-*.
  `acceso-carga-fondo` = "#F8F6F1", `acceso-carga-oro` = "#B8963E", `acceso-carga-oro-claro` = "#D4AF55",
  `acceso-carga-lema` = "#A09070", `acceso-carga-texto` = "#3A3630", `acceso-carga-texto-2` = "#9A8E7E",
  `acceso-carga-riel` = "#E4DDD0", `acceso-carga-pct` = "#B0A090",
  # Type scale: absorbs today's 46 font sizes (TEMA_inventario.md §5).
  `txt-xs` = "11px", `txt-sm` = "12px", `txt-md` = "13px",
  `txt-base` = "14px", `txt-lg` = "16px", `txt-xl` = "20px",
  `esp-1` = "4px", `esp-2` = "8px", `esp-3` = "12px",
  `esp-4` = "16px", `esp-5` = "24px", `esp-6` = "32px",
  pildora = "999px"
)

# ── Temas (every theme gives every role a value) ─────────────────────────────
.HD_LATO <- "Lato, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif"

HD_TEMAS <- list(
  clasico = list(
    nombre = "comun.tema.clasico",
    ofrecer = TRUE,   # a client may choose it (TEMA stage 5; see temas_ofrecidos())
    puente = FALSE,   # Bootstrap keeps flatly's colours: Clásico is today's look
    tokens = list(
      # Values: the most used of each group, TEMA_inventario.md §3 (n).
      fondo              = "#FFFFFF",
      superficie         = "#FFFFFF",   # 137
      `superficie-2`     = "#F5F8FC",
      borde              = "#E5E7EB",   # 34
      `borde-fuerte`     = "#CCCCCC",   # 30
      texto              = "#374151",   # 16
      `texto-2`          = "#6B7280",   # 28
      `texto-tenue`      = "#9CA3AF",   # 13 (disabled, notes; not body text)
      `texto-inverso`    = "#FFFFFF",
      primario           = "#0A58CA",   # 54, bslib's primary
      `primario-hover`   = "#094BAC",   # Bootstrap's 15% shade of it
      `primario-texto`   = "#FFFFFF",
      `primario-suave`   = "#E8F0FE",
      acento             = "#B49444",   # 50, login and account emails
      `acento-texto`     = "#8B6914",
      enlace             = "#0A58CA",
      `barra-fondo`      = "#0A58CA",   # the navbar flatly paints today
      `barra-texto`      = "#FFFFFF",
      encabezado         = "#1E3A5F",   # 46, document and table headers (COBROS)
      `encabezado-texto` = "#FFFFFF",
      `encabezado-hover` = "#2A5080",
      `encabezado-borde` = "#163050",
      `pago-encabezado`  = "#3A1E5F",   # the Flujo's PAGOS header
      # A small dark surface: Flujo neto / Posición acumulada rows, Día 0, login.
      `oscuro-fondo`     = "#162D4A",
      `oscuro-fondo-2`   = "#112338",
      `oscuro-borde`     = "#0E1E30",
      `oscuro-texto`     = "#E8EEF5",
      `oscuro-texto-2`   = "#B0C4DE",
      `oscuro-exito`     = "#3FB950",   # positive / negative on the dark band (Bancos)
      `oscuro-peligro`   = "#F85149",
      # Fill (stripes, icons) and text are separate: a fill colour need not
      # read as text (TEMA_e4_mapas.md P8).
      exito              = "#198754", `exito-suave`   = "#D1E7DD", `exito-texto`   = "#065F46",
      aviso              = "#F59E0B", `aviso-suave`   = "#FFF3CD", `aviso-texto`   = "#856404",
      `aviso-fuerte`     = "#D97706",   # due today to the day after (Pasivos)
      peligro            = "#DC2626", `peligro-suave` = "#FEE2E2", `peligro-texto` = "#B91C1C",
      info               = "#0A58CA", `info-suave`    = "#E8F0FE",
      cobro              = "#1F5FD1",   # the table engine's (--hdt-cobro)
      pago               = "#15804A",   # the table engine's (--hdt-pago)
      # The Calendario's Pagos mode (Cobros is primario): today's brown.
      `modo-pago`        = "#7B4F2E",
      `modo-pago-suave`  = "#F5EDE6",
      `modo-pago-borde`  = "#EBD9CE",
      `modo-pago-dato`   = "#D4A98C",
      provision          = "#7C3AED",   # 8
      seleccion          = "#D8E5FF",   # the table engine's (--hdt-sel)
      hover              = "#F0F4FF",   # the table engine's (--hdt-hover)
      `fuente-titulos` = .HD_LATO,
      `fuente-texto`   = .HD_LATO,
      `radio-sm`       = "0.25rem",
      `radio-md`       = "0.375rem",
      radio            = "0.5rem",
      sombra           = "0 0.5rem 1rem rgba(0, 0, 0, 0.15)",
      foco             = "0 0 0 0.25rem rgba(10, 88, 202, 0.25)",
      `foco-color`     = "#0A58CA"   # a focus outline or border (foco is a whole shadow)
    )
  ),
  marca = list(
    nombre = "comun.tema.marca",
    # FALSE until every client screen paints it (stage 5b): only HopDesk staff
    # see it in the selector, to try it on a test client. The guardian keeps it
    # FALSE while client files still have hand-written colours.
    ofrecer = FALSE,
    puente = TRUE,    # www/hd_tema_bootstrap.css maps Bootstrap onto these roles
    tokens = list(
      fondo              = "#F7F6F2",   # papel
      superficie         = "#FFFFFF",
      `superficie-2`     = "#F3F1EA",   # crema
      borde              = "#D9D4C6",
      `borde-fuerte`     = "#B7C2D4",   # niebla
      texto              = "#1A2233",   # tinta
      `texto-2`          = "#5A6474",   # gris
      `texto-tenue`      = "#8B97AD",
      `texto-inverso`    = "#FFFFFF",
      primario           = "#13213A",   # marino
      `primario-hover`   = "#24365A",
      `primario-texto`   = "#FFFFFF",
      `primario-suave`   = "#E3E8F0",   # linea
      acento             = "#C9AE6E",   # oro: decoration and focus, never text
      `acento-texto`     = "#8A6D2C",   # gold that reads as text
      enlace             = "#1E5B63",
      `barra-fondo`      = "#13213A",
      `barra-texto`      = "#FFFFFF",
      encabezado         = "#13213A",
      `encabezado-texto` = "#FFFFFF",
      `encabezado-hover` = "#24365A",
      `encabezado-borde` = "#0B1527",
      `pago-encabezado`  = "#6B3358",   # lila-fg
      `oscuro-fondo`     = "#13213A",
      `oscuro-fondo-2`   = "#0E192C",
      `oscuro-borde`     = "#24365A",
      `oscuro-texto`     = "#F3F1EA",
      `oscuro-texto-2`   = "#B7C2D4",
      `oscuro-exito`     = "#9CCBA8",
      `oscuro-peligro`   = "#E6A383",   # Panorama's warm signal on the navy bar
      exito              = "#2F6B4F", `exito-suave`   = "#E3EFE8", `exito-texto`   = "#2F6B4F",
      aviso              = "#C08A3E", `aviso-suave`   = "#F7E9E1", `aviso-texto`   = "#9A4524",
      `aviso-fuerte`     = "#9A4524",
      peligro            = "#B42318", `peligro-suave` = "#FDECEA", `peligro-texto` = "#B42318",
      info               = "#1E5B63", `info-suave`    = "#E2EEEF",
      cobro              = "#24508F",
      pago               = "#2F6B4F",
      # The Calendario's Pagos mode: Panorama's ambar (pago is green here, = exito).
      `modo-pago`        = "#9A4524",   # ambar-fg
      `modo-pago-suave`  = "#F7E9E1",   # ambar-bg
      `modo-pago-borde`  = "#E8CBBB",   # ambar-bd
      `modo-pago-dato`   = "#D9B19C",
      provision          = "#6B3358",   # lila-fg
      seleccion          = "#E3E8F0",
      hover              = "#F3F1EA",
      `fuente-titulos` = "'Fraunces', Georgia, 'Times New Roman', serif",
      `fuente-texto`   = "'Lato', system-ui, -apple-system, 'Segoe UI', sans-serif",
      `radio-sm`       = "12px",
      `radio-md`       = "18px",
      radio            = "24px",
      sombra           = "0 1px 2px rgba(19, 33, 58, 0.06), 0 8px 24px rgba(19, 33, 58, 0.06)",
      foco             = "0 0 0 3px rgba(201, 174, 110, 0.55)",
      `foco-color`     = "#8A6D2C"   # gold a 1-2 px outline can carry; oro is 2.15:1
    )
  )
)

# ── Legado de Clásico ────────────────────────────────────────────────────────
# A screen moved from hex to roles (TEMA stage 4) unifies a colour with its
# role when they are close (ΔE <= 2). When today's colour is farther, the
# screen still uses the role, through a legacy token that keeps the exact
# value under Clásico only:
#     --hd-leg-<rol>-<hex>   e.g. --hd-leg-texto-0d1b2a
#     clasico -> #0D1B2A     every other theme -> var(--hd-texto)
# Usable in a .css, in an inline style = and from R (tema_color("leg-texto-
# 0d1b2a", tema) for Word, PDF and emails). Generated to
# www/hd_clasico_legado.css. When Clásico retires, scripts/
# retirar_legado_clasico.R turns every var(--hd-leg-ROL-hex) into
# var(--hd-ROL). The guardian ratchets the count. Entries: "<rol>-<hex>" =
# "#HEX" (the hex in the name, lower case, without #, is the value). For a
# shadow role (HD_ROLES_SOMBRA: a whole box-shadow, not a colour) the value
# is the whole shadow of today, the hex names its colour and a hash of the
# shadow (tema_hash_sombra()) tells apart two shadows of one colour:
#     `sombra-000000-<hash>` = "0 4px 20px rgba(0, 0, 0, 0.45)"
# A colour that today is a light tint of a role (no ΔE <= 2 mix exists)
# names the percentage: elsewhere it is the role at N % over the surface,
# not the solid role (TEMA_e4_mapas2 §v7):
#     --hd-leg-<rol>-p<N>-<hex>   e.g. --hd-leg-provision-p15-e0cffc
#     clasico -> #E0CFFC
#     every other theme -> color-mix(in srgb, var(--hd-provision) 15%, var(--hd-superficie))
# N is 1..99, colour roles only. From R, tema_color() gives it flattened
# (#RRGGBB), and also reads "<rol>-p<N>" without the legacy prefix, which
# is what the retiring script leaves in R code.
.HD_LEGADO <- list(
  `acento-00c4a7` = "#00C4A7",
  `acento-7c3aed` = "#7C3AED",
  `acento-c2185b` = "#C2185B",
  `acento-f97316` = "#F97316",
  `acento-p35-fed7aa` = "#FED7AA",
  `acento-p8-fff7ed` = "#FFF7ED",
  `acento-texto-6d28d9` = "#6D28D9",
  `aviso-e0a100` = "#E0A100",
  `aviso-e8b86a` = "#E8B86A",
  `aviso-f0a05a` = "#F0A05A",
  `aviso-fde047` = "#FDE047",
  `aviso-ffc107` = "#FFC107",
  `aviso-fuerte-b45309` = "#B45309",
  `aviso-fuerte-c05e0e` = "#C05E0E",
  `aviso-fuerte-e65100` = "#E65100",
  `aviso-fuerte-e67e22` = "#E67E22",
  `aviso-fuerte-fd7e14` = "#FD7E14",
  `aviso-fuerte-p14-fff3e0` = "#FFF3E0",
  `aviso-fuerte-p6-fff8f0` = "#FFF8F0",
  `aviso-p51-ffe58f` = "#FFE58F",
  `aviso-p7-fffdf0` = "#FFFDF0",
  `aviso-suave-fde68a` = "#FDE68A",
  `aviso-suave-fef9c3` = "#FEF9C3",
  `aviso-suave-fff4dc` = "#FFF4DC",
  `aviso-suave-fffaf0` = "#FFFAF0",
  `aviso-suave-fffbeb` = "#FFFBEB",
  `aviso-texto-664d03` = "#664D03",
  `aviso-texto-78350f` = "#78350F",
  `aviso-texto-7a3d00` = "#7A3D00",
  `aviso-texto-7a5c00` = "#7A5C00",
  `aviso-texto-854d0e` = "#854D0E",
  `aviso-texto-92400e` = "#92400E",
  `aviso-texto-997404` = "#997404",
  `aviso-texto-9a5600` = "#9A5600",
  `borde-d0dff7` = "#D0DFF7",
  `borde-d6e0f0` = "#D6E0F0",
  `borde-dce1ec` = "#DCE1EC",
  `borde-dce3ef` = "#DCE3EF",
  `borde-dde4ed` = "#DDE4ED",
  `borde-dde4f0` = "#DDE4F0",
  `borde-dde7f7` = "#DDE7F7",
  `borde-dee2e6` = "#DEE2E6",
  `borde-e0e0e0` = "#E0E0E0",
  `borde-e4eaf4` = "#E4EAF4",
  `borde-e8ecf2` = "#E8ECF2",
  `borde-e8ecf4` = "#E8ECF4",
  `borde-eeeeee` = "#EEEEEE",
  `borde-f0f0f0` = "#F0F0F0",
  `borde-f3f4f6` = "#F3F4F6",
  `borde-fuerte-adb5bd` = "#ADB5BD",
  `borde-fuerte-b0c6de` = "#B0C6DE",
  `borde-fuerte-b8ccde` = "#B8CCDE",
  `borde-fuerte-c5d0e6` = "#C5D0E6",
  `borde-fuerte-cbd5e1` = "#CBD5E1",
  `borde-fuerte-cdd3e0` = "#CDD3E0",
  `borde-fuerte-d0d9ec` = "#D0D9EC",
  `borde-fuerte-d1d5db` = "#D1D5DB",
  `encabezado-07090d` = "#07090D",
  `encabezado-0d1b3e` = "#0D1B3E",
  `encabezado-163368` = "#163368",
  `encabezado-1a2e5a` = "#1A2E5A",
  `encabezado-1a3a6b` = "#1A3A6B",
  `encabezado-1b3d7a` = "#1B3D7A",
  `encabezado-borde-0f1e3d` = "#0F1E3D",
  `encabezado-borde-2a5298` = "#2A5298",
  `encabezado-hover-1e4080` = "#1E4080",
  `encabezado-hover-2563a8` = "#2563A8",
  `encabezado-hover-4a6fa5` = "#4A6FA5",
  `etq-a-fila-fff5f5` = "#FFF5F5",
  `etq-a-texto-c62828` = "#C62828",
  `etq-a-texto-p68-e57373` = "#E57373",
  `etq-b-fila-fff3ec` = "#FFF3EC",
  `etq-b-texto-bf360c` = "#BF360C",
  `etq-b-texto-e65100` = "#E65100",
  `etq-b-texto-p79-e8874a` = "#E8874A",
  `etq-c-fila-fffbf0` = "#FFFBF0",
  `etq-c-texto-f57f17` = "#F57F17",
  `etq-c-texto-p78-f0c040` = "#F0C040",
  `exito-047857` = "#047857",
  `exito-059669` = "#059669",
  `exito-0d9f8a` = "#0D9F8A",
  `exito-10b981` = "#10B981",
  `exito-16a34a` = "#16A34A",
  `exito-p46-a3d9b3` = "#A3D9B3",
  `exito-suave-6ee7b7` = "#6EE7B7",
  `exito-suave-d1fae5` = "#D1FAE5",
  `exito-suave-d9f0e8` = "#D9F0E8",
  `exito-suave-e7f6ee` = "#E7F6EE",
  `exito-suave-e8f7f2` = "#E8F7F2",
  `exito-suave-e9f7ef` = "#E9F7EF",
  `exito-suave-f4fdf9` = "#F4FDF9",
  `exito-texto-064e3b` = "#064E3B",
  `exito-texto-0d6b5e` = "#0D6B5E",
  `exito-texto-0f5132` = "#0F5132",
  `exito-texto-155724` = "#155724",
  `exito-texto-15804a` = "#15804A",
  `exito-texto-1a5c2e` = "#1A5C2E",
  `exito-texto-1b3a2a` = "#1B3A2A",
  `exito-texto-2d6a4f` = "#2D6A4F",
  `exito-texto-2e7d32` = "#2E7D32",
  `foco-4a88c7-21e45b` = "0 0 0 2px rgba(74,136,199,0.3)",
  `foco-color-1e3a5f` = "#1E3A5F",
  `foco-color-4a88c7` = "#4A88C7",
  `hover-e8eef8` = "#E8EEF8",
  `hover-f0f4fa` = "#F0F4FA",
  `info-0dcaf0` = "#0DCAF0",
  `info-0ea5e9` = "#0EA5E9",
  `info-1565c0` = "#1565C0",
  `info-2456d6` = "#2456D6",
  `info-suave-e6edff` = "#E6EDFF",
  `info-suave-e8f4fd` = "#E8F4FD",
  `modo-pago-6a1b9a` = "#6A1B9A",
  `modo-pago-p19-ede0d6` = "#EDE0D6",
  `modo-pago-p20-f0e0d5` = "#F0E0D5",
  `modo-pago-p4-fdf8f5` = "#FDF8F5",
  `modo-pago-p7-fdf5ee` = "#FDF5EE",
  `oscuro-borde-1b3d7a` = "#1B3D7A",
  `oscuro-borde-1e2733` = "#1E2733",
  `oscuro-borde-2e4a6e` = "#2E4A6E",
  `oscuro-borde-3a5a80` = "#3A5A80",
  `oscuro-exito-6ee7a0` = "#6EE7A0",
  `oscuro-exito-6ee7c4` = "#6EE7C4",
  `oscuro-fondo-0d1117` = "#0D1117",
  `oscuro-fondo-0d1b3e` = "#0D1B3E",
  `oscuro-fondo-16213e` = "#16213E",
  `oscuro-fondo-1c2e42` = "#1C2E42",
  `oscuro-fondo-2-0d1e30` = "#0D1E30",
  `oscuro-fondo-2-1a1a2e` = "#1A1A2E",
  `oscuro-peligro-ff8080` = "#FF8080",
  `oscuro-peligro-ff8a8a` = "#FF8A8A",
  `oscuro-texto-2-484f58` = "#484F58",
  `oscuro-texto-2-6b8299` = "#6B8299",
  `oscuro-texto-2-7b9ab8` = "#7B9AB8",
  `oscuro-texto-2-7eb3f5` = "#7EB3F5",
  `oscuro-texto-2-8b949e` = "#8B949E",
  `oscuro-texto-2-a9bdd3` = "#A9BDD3",
  `oscuro-texto-c8d8ea` = "#C8D8EA",
  `oscuro-texto-ffffff` = "#FFFFFF",
  `pago-encabezado-8b3a0a` = "#8B3A0A",
  `peligro-9f1239` = "#9F1239",
  `peligro-be123c` = "#BE123C",
  `peligro-dc3545` = "#DC3545",
  `peligro-suave-fca5a5` = "#FCA5A5",
  `peligro-suave-fda4af` = "#FDA4AF",
  `peligro-suave-ffecec` = "#FFECEC",
  `peligro-texto-4c0519` = "#4C0519",
  `peligro-texto-7b1111` = "#7B1111",
  `peligro-texto-7f1d1d` = "#7F1D1D",
  `peligro-texto-881337` = "#881337",
  `peligro-texto-991b1b` = "#991B1B",
  `peligro-texto-c22b2b` = "#C22B2B",
  `peligro-texto-dc3545` = "#DC3545",
  `primario-0d6efd` = "#0D6EFD",
  `primario-1a6cc4` = "#1A6CC4",
  `primario-1d4ed8` = "#1D4ED8",
  `primario-1e40af` = "#1E40AF",
  `primario-2456d6` = "#2456D6",
  `primario-2563eb` = "#2563EB",
  `primario-6366f1` = "#6366F1",
  `primario-hover-0344a8` = "#0344A8",
  `primario-p42-9db8e8` = "#9DB8E8",
  `primario-suave-d6e4f0` = "#D6E4F0",
  `primario-suave-d8e6f5` = "#D8E6F5",
  `primario-suave-dbeafe` = "#DBEAFE",
  `primario-suave-e2eafc` = "#E2EAFC",
  `primario-suave-e6edff` = "#E6EDFF",
  `provision-6d28d9` = "#6D28D9",
  `provision-p12-f1e9ff` = "#F1E9FF",
  `provision-p8-f6f0ff` = "#F6F0FF",
  `sombra-000000-0351f6` = "0 -4px 10px rgba(0,0,0,.12)",
  `sombra-000000-0504d0` = "0 1px 4px rgba(0,0,0,.05)",
  `sombra-000000-0d67d2` = "0 2px 8px rgba(0,0,0,0.16)",
  `sombra-000000-12d688` = "0 2px 10px rgba(0,0,0,0.20)",
  `sombra-000000-17659d` = "0 2px 6px rgba(0,0,0,0.06)",
  `sombra-000000-44cce5` = "0 4px 20px rgba(0,0,0,0.45)",
  `sombra-000000-453bbb` = "0 4px 16px rgba(0,0,0,0.26)",
  `sombra-000000-8810b0` = "-4px 0 24px rgba(0,0,0,0.13)",
  `sombra-000000-b72920` = "0 6px 20px rgba(0,0,0,.18)",
  `sombra-000000-e70fe4` = "0 1px 4px rgba(0,0,0,.12)",
  `sombra-000000-e7d0fb` = "0 3px 10px rgba(0,0,0,0.10)",
  `sombra-0a2864-404c10` = "0 6px 24px rgba(10,40,100,0.13)",
  `sombra-2563eb-091a02` = "0 0 6px 3px rgba(37,99,235,0.75)",
  `sombra-7c3aed-bcc0b2` = "0 0 6px 3px rgba(124,58,237,0.75)",
  `superficie-2-e2e8f0` = "#E2E8F0",
  `superficie-2-e9ecef` = "#E9ECEF",
  `superficie-2-eef1f5` = "#EEF1F5",
  `superficie-2-eef1f8` = "#EEF1F8",
  `superficie-2-f0f7ff` = "#F0F7FF",
  `superficie-2-f1f3f5` = "#F1F3F5",
  `superficie-2-f2f2f2` = "#F2F2F2",
  `superficie-2-f8f9ff` = "#F8F9FF",
  `superficie-f5f0e8` = "#F5F0E8",
  `texto-000000` = "#000000",
  `texto-07090d` = "#07090D",
  `texto-0d1b2a` = "#0D1B2A",
  `texto-0d2240` = "#0D2240",
  `texto-111827` = "#111827",
  `texto-141b2d` = "#141B2D",
  `texto-1a1a1a` = "#1A1A1A",
  `texto-1a1a2e` = "#1A1A2E",
  `texto-1e293b` = "#1E293B",
  `texto-1f2937` = "#1F2937",
  `texto-2-374b5f` = "#374B5F",
  `texto-2-475569` = "#475569",
  `texto-2-495057` = "#495057",
  `texto-2-4a5568` = "#4A5568",
  `texto-2-4a5b6b` = "#4A5B6B",
  `texto-2-555555` = "#555555",
  `texto-2-5a6377` = "#5A6377",
  `texto-2-5a7596` = "#5A7596",
  `texto-2-606060` = "#606060",
  `texto-2-64748b` = "#64748B",
  `texto-2-666666` = "#666666",
  `texto-2-6c757d` = "#6C757D",
  `texto-2-6c8ebf` = "#6C8EBF",
  `texto-2-7b8794` = "#7B8794",
  `texto-2-888888` = "#888888",
  `texto-212529` = "#212529",
  `texto-222222` = "#222222",
  `texto-23303d` = "#23303D",
  `texto-2c3e50` = "#2C3E50",
  `texto-333333` = "#333333",
  `texto-444444` = "#444444",
  `texto-4b5563` = "#4B5563",
  `texto-555555` = "#555555",
  `texto-tenue-888888` = "#888888",
  `texto-tenue-94a3b8` = "#94A3B8",
  `texto-tenue-999999` = "#999999",
  `texto-tenue-9ba8b5` = "#9BA8B5",
  `texto-tenue-aaaaaa` = "#AAAAAA",
  `texto-tenue-bbbbbb` = "#BBBBBB"
)

# Roles whose value is a whole box-shadow, not a colour.
HD_ROLES_SOMBRA <- c("sombra", "foco")

# Does a shadow use this colour (as #hex or as rgb()/rgba() of it)?
.tema_sombra_con_color <- function(sombra, hex) {
  rgb <- strtoi(substring(sub("^#", "", hex), c(1, 3, 5), c(2, 4, 6)), 16L)
  grepl(hex, sombra, ignore.case = TRUE, fixed = FALSE) ||
    grepl(sprintf("rgba?\\(\\s*%d\\s*,\\s*%d\\s*,\\s*%d\\s*[,)]", rgb[1], rgb[2], rgb[3]), sombra)
}

# "texto-2-6b7280" -> list(rol = "texto-2", hex = "#6B7280"); NULL if malformed.
# A shadow role's name also carries a short hash of the whole shadow, since
# one colour can have several shadows: "sombra-000000-3fa2c1"
# -> list(rol = "sombra", hex = "#000000", hash = "3fa2c1").
# A tint names its percentage: "provision-p15-e0cffc" -> list(rol =
# "provision", hex = "#E0CFFC", hash = NULL, pct = 15L). pct is NULL otherwise.
tema_legado_partes <- function(nombre) {
  m <- regmatches(nombre, regexec("^(.+?)-(?:p([1-9][0-9]?)-)?([0-9a-f]{6})(?:-([0-9a-f]{6}))?$", nombre, perl = TRUE))[[1]]
  if (length(m) != 5L) return(NULL)
  pct <- if (nzchar(m[3])) as.integer(m[3]) else NULL
  if (!is.null(pct) && nzchar(m[5])) return(NULL)   # a tint is a colour, never a shadow
  list(rol = m[2], hex = paste0("#", toupper(m[4])), hash = if (nzchar(m[5])) m[5] else NULL, pct = pct)
}

# A #rrggbb at pct % over a solid #rrggbb, flattened (what color-mix(in srgb,
# A pct%, B) paints). Pure: no theme lookup, so the legacy values can use it.
.tema_mezclar_hex <- function(hex, pct, sobre) {
  a <- max(0, min(100, pct)) / 100
  rgb <- strtoi(substring(hex, c(2, 4, 6), c(3, 5, 7)), 16L)
  f <- strtoi(substring(sobre, c(2, 4, 6), c(3, 5, 7)), 16L)
  m <- as.integer(round(rgb * a + f * (1 - a)))
  sprintf("#%02X%02X%02X", m[1], m[2], m[3])
}

# The hash in a legacy shadow's name: the first 6 hex of the MD5 of the
# shadow, lower case and without spaces ("0 4px 20px rgba(0,0,0,0.45)").
tema_hash_sombra <- function(valor) {
  f <- tempfile(); on.exit(unlink(f))
  writeBin(charToRaw(tolower(gsub("\\s+", "", valor))), f)
  substr(unname(tools::md5sum(f)), 1L, 6L)
}

# The value of every legacy token under a theme.
.tema_legado_valores <- function(tema) {
  if (!length(.HD_LEGADO)) return(list())
  if (identical(tema, "clasico")) return(.HD_LEGADO)
  roles <- c(.HD_COMUNES, HD_TEMAS[[tema]]$tokens)   # a legacy token may point to a common (etq-*)
  stats::setNames(lapply(names(.HD_LEGADO), function(n) {
    p <- tema_legado_partes(n)
    if (is.null(p$pct)) roles[[p$rol]] else .tema_mezclar_hex(roles[[p$rol]], p$pct, roles[["superficie"]])
  }), names(.HD_LEGADO))
}

# One line of code with the legacy tokens turned into their roles: what
# scripts/retirar_legado_clasico.R applies to every file.
tema_legado_retirar <- function(lineas) {
  # A tint first (the general pattern would leave var(--hd-<rol>-p<N>)).
  lineas <- gsub("var\\(--hd-leg-([a-z0-9-]+?)-p([1-9][0-9]?)-[0-9a-f]{6}\\)",
                 "color-mix(in srgb, var(--hd-\\1) \\2%, var(--hd-superficie))", lineas, perl = TRUE)
  lineas <- gsub("var\\(--hd-leg-([a-z0-9-]+?)-[0-9a-f]{6}(?:-[0-9a-f]{6})?\\)", "var(--hd-\\1)", lineas, perl = TRUE)
  gsub("([\"'])leg-([a-z0-9-]+?)-[0-9a-f]{6}(?:-[0-9a-f]{6})?([\"'])", "\\1\\2\\3", lineas, perl = TRUE)
}

# A role (or legacy token) at N % over transparent, for what cannot take
# var() or color-mix(): SVG in a data: URI, vis.js canvas, Word, Excel. The
# same colour as color-mix(in srgb, var(--hd-rol) N%, transparent):
#   tema_mezcla("provision", 25)                 "rgba(124, 58, 237, 0.25)"
# `sobre`: a solid background to flatten onto, for places with no alpha
# (Word, Excel, emails): tema_mezcla("provision", 25, sobre = "#FFFFFF").
tema_mezcla <- function(rol, pct, tema = HD_TEMA_OMISION, sobre = NULL) {
  hex <- tema_color(rol, tema)
  if (!grepl("^#[0-9A-Fa-f]{6}$", hex)) stop("[tema] tema_mezcla needs a #rrggbb role: ", rol, call. = FALSE)
  a <- max(0, min(100, pct)) / 100
  rgb <- strtoi(substring(hex, c(2, 4, 6), c(3, 5, 7)), 16L)
  if (is.null(sobre)) return(sprintf("rgba(%d, %d, %d, %s)", rgb[1], rgb[2], rgb[3], format(a)))
  .tema_mezclar_hex(hex, pct, sobre)
}

# ── Lectura ──────────────────────────────────────────────────────────────────

temas_disponibles <- function() names(HD_TEMAS)

tema_valido <- function(tema) {
  t <- tolower(trimws(as.character(tema %||% "")))[1]
  !is.na(t) && t %in% names(HD_TEMAS)
}

# The theme a person sees: theirs, else their client's, else the system's.
# Anything unknown (a theme removed later, a failed read passed as NULL) falls
# through; it never errors.
tema_resolver <- function(usuario = NULL, cliente = NULL) {
  for (t in list(usuario, cliente)) if (tema_valido(t)) return(tolower(trimws(t)))
  HD_TEMA_OMISION
}

# The themes a client may choose (ofrecer = TRUE); HopDesk staff see every
# theme, to try one on a test client before it is offered (TEMA stage 5).
temas_ofrecidos <- function(es_staff = FALSE) {
  if (isTRUE(es_staff)) return(names(HD_TEMAS))
  names(HD_TEMAS)[vapply(HD_TEMAS, function(t) isTRUE(t$ofrecer), logical(1))]
}

# The theme of a Shiny session, for colours drawn in R (SVG in data: URIs,
# vis.js, Word...). The server puts a reactive in session$userData$hd_tema:
# the theme of the client being viewed (app.R, TEMA stage 5); read inside a
# render, a change of theme redraws. Outside a reactive context (a download's
# content) it is read isolated. No session, or no reactive: the default.
tema_de_sesion <- function(session = shiny::getDefaultReactiveDomain()) {
  rv <- if (is.null(session)) NULL else session$userData$hd_tema
  if (!is.function(rv)) return(tema_resolver(NULL))
  v <- tryCatch(rv(), error = function(e)
    tryCatch(shiny::isolate(rv()), error = function(e2) NULL))
  tema_resolver(v)
}

# A client's theme, read from its group_config.rds when called (never when
# this file loads: rule 29). For what runs outside the client's session (the
# emails). Any failure -- no file, a failed read, no load_group_config() in a
# test by pieces -- is the default theme: a theme never stops anything.
tema_de_cliente <- function(client_id) tema_resolver(cliente = tema_elegido_cliente(client_id))

# The theme a client CHOSE: its group_config's `tema` when it is one of the
# registry's, else NULL (never chose, unknown, or the read failed). For what
# must tell "chose Clásico" from "chose nothing": the weekly mails keep Marca
# for a client that never chose (D2, TEMA 5a).
tema_elegido_cliente <- function(client_id) {
  if (is.null(client_id) || !nzchar(client_id %||% "")) return(NULL)
  leer <- get0("load_group_config", mode = "function")
  if (is.null(leer)) return(NULL)
  cfg <- tryCatch(leer(client_id = client_id), error = function(e) NULL)
  t <- if (is.list(cfg)) cfg$tema else NULL
  if (tema_valido(t)) tolower(trimws(t)) else NULL
}

# Every --hd-* value of a theme, resolved: paleta + comunes + its roles +
# its legacy tokens (as "leg-<rol>-<hex>").
tema_tokens <- function(tema = HD_TEMA_OMISION) {
  if (!tema_valido(tema)) tema <- HD_TEMA_OMISION
  tema <- tolower(trimws(tema))
  leg <- .tema_legado_valores(tema)
  if (length(leg)) names(leg) <- paste0("leg-", names(leg))
  c(.HD_PALETA, .HD_COMUNES, HD_TEMAS[[tema]]$tokens, leg)
}

# "<rol>-p<N>" (no legacy prefix): the role at N % over the surface,
# flattened; what a retired tint leaves in R code.
# A standalone document (a mail, the report's iframe, a receipt, a PDF) cannot
# use var(--hd-*): its colours are {{rol}} or {{rol@pct}} markers that this
# turns into the theme's values (tema_color() / tema_mezcla()). `tema` NULL is
# the default theme. One copy for every document that paints in R.
tema_pintar <- function(html, tema = NULL) {
  tema <- tema %||% HD_TEMA_OMISION
  marcas <- unique(regmatches(html, gregexpr("\\{\\{[a-z0-9-]+(@[0-9]+)?\\}\\}", html))[[1]])
  for (mk in marcas) {
    p <- strsplit(substr(mk, 3L, nchar(mk) - 2L), "@", fixed = TRUE)[[1]]
    v <- if (length(p) == 2L) tema_mezcla(p[1], as.integer(p[2]), tema) else tema_color(p[1], tema)
    html <- gsub(mk, v, html, fixed = TRUE)
  }
  html
}

tema_color <- function(rol, tema = HD_TEMA_OMISION) {
  tk <- tema_tokens(tema)
  v <- tk[[rol]]
  if (is.null(v)) {
    m <- regmatches(rol, regexec("^(.+)-p([1-9][0-9]?)$", rol))[[1]]
    if (length(m) == 3L && grepl("^#[0-9A-Fa-f]{6}$", tk[[m[2]]] %||% ""))
      v <- .tema_mezclar_hex(tk[[m[2]]], as.integer(m[3]), tk[["superficie"]])
  }
  if (is.null(v)) stop("[tema] rol desconocido: ", rol, call. = FALSE)
  v
}

# ── CSS ──────────────────────────────────────────────────────────────────────

.tema_bloque <- function(selector, valores, titulo = NULL) {
  c(if (!is.null(titulo)) paste0("/* ", titulo, " */"),
    paste0(selector, " {"),
    paste0("  --hd-", names(valores), ": ", unlist(valores, use.names = FALSE), ";"),
    "}")
}

# The text of www/hd_temas.css (scripts/generar_temas_css.R writes it).
tema_css <- function() {
  otros <- setdiff(names(HD_TEMAS), HD_TEMA_OMISION)
  lineas <- c(
    "/* ============================================================================",
    "   www/hd_temas.css: GENERATED from R/tema_registro.R by",
    "   scripts/generar_temas_css.R. Do not edit by hand.",
    paste0("   Default theme on :root: ", HD_TEMA_OMISION, ". The others on <html data-hd-tema=\"...\">."),
    "   ========================================================================== */",
    "",
    .tema_bloque(":root", .HD_PALETA, "Paleta: the same in every theme"),
    "",
    .tema_bloque(":root", .HD_COMUNES, "Comunes: the same in every theme"),
    "",
    .tema_bloque(paste0(":root, [data-hd-tema=\"", HD_TEMA_OMISION, "\"]"),
                 HD_TEMAS[[HD_TEMA_OMISION]]$tokens,
                 paste0("Roles: ", HD_TEMA_OMISION, " (default)"))
  )
  for (t in otros)
    lineas <- c(lineas, "", .tema_bloque(paste0("[data-hd-tema=\"", t, "\"]"),
                                         HD_TEMAS[[t]]$tokens, paste0("Roles: ", t)))
  c(lineas, "")
}

# The text of www/hd_clasico_legado.css. Every theme but Clásico gets the
# role; Clásico (also on :root while it is the default) gets today's hex.
tema_legado_css <- function() {
  cab <- c(
    "/* ============================================================================",
    "   www/hd_clasico_legado.css: GENERATED from .HD_LEGADO in R/tema_registro.R",
    "   by scripts/generar_temas_css.R. Do not edit by hand.",
    "   --hd-leg-<rol>-<hex>: today's exact colour under Clasico, the role elsewhere.",
    "   Can only shrink (tests/test_temas_guardian.R).",
    "   ========================================================================== */")
  if (!length(.HD_LEGADO)) return(c(cab, "", "/* (empty) */", ""))
  a_rol <- stats::setNames(lapply(names(.HD_LEGADO), function(n) {
    p <- tema_legado_partes(n)
    if (is.null(p$pct)) paste0("var(--hd-", p$rol, ")")
    else sprintf("color-mix(in srgb, var(--hd-%s) %d%%, var(--hd-superficie))", p$rol, p$pct)
  }), paste0("leg-", names(.HD_LEGADO)))
  clasico <- stats::setNames(.HD_LEGADO, paste0("leg-", names(.HD_LEGADO)))
  sel <- "[data-hd-tema=\"clasico\"]"
  if (identical(HD_TEMA_OMISION, "clasico")) sel <- paste0(sel, ", :root:not([data-hd-tema])")
  c(cab, "",
    .tema_bloque(":root", a_rol, "Every theme: the role"), "",
    .tema_bloque(sel, clasico, "Clasico: today's exact value"), "")
}

# ── Puente con Bootstrap ─────────────────────────────────────────────────────
# bslib compiles flatly once (hd_bs_theme()); some of its colours are --bs-*
# variables, others are written into a class (.btn-primary { --bs-btn-bg:
# #0A58CA }, .form-control:focus { border-color: #85ace5 }). The bridge,
# www/hd_tema_bootstrap.css, re-points them at the roles, so the ~600
# Bootstrap classes in the modules follow the theme without touching them.
# Only themes with puente = TRUE get a block, every rule under
# html[data-hd-tema="<tema>"] (0,1,1: beats Bootstrap's :root,[data-bs-theme]
# whatever the load order). Clásico has none: Bootstrap stays as it is.
#
# Values are templates, expanded per theme by tema_puente_css():
#   {rol}           var(--hd-rol)
#   {rgb:rol}       the role's hex as "r, g, b" (Bootstrap's *-rgb triplets,
#                   which var() cannot produce)
#   {rgba:rol:a}    rgba() of the role at alpha a
# Anything else is literal and may not hold a colour (no #hex, no rgb()):
# the guardian checks it.
# bslib 0.9's selector for a plain .btn-default (as it compiles it, quotes
# included). The guardian fails if bslib ever writes another one.
.HD_BTN_DEFAULT_SEL <- ".btn-default:not(.btn-primary):not(.btn-info):not(.btn-success):not(.btn-warning):not(.btn-danger):not(.btn-dark):not(.btn-light):not([class*='btn-outline-'])"
.HD_PUENTE_BS <- list(
  list(sel = "", decl = c(
    `--bs-primary` = "{primario}", `--bs-primary-rgb` = "{rgb:primario}",
    `--bs-primary-text-emphasis` = "{primario}", `--bs-primary-bg-subtle` = "{primario-suave}",
    `--bs-primary-border-subtle` = "{borde-fuerte}",
    `--bs-body-color` = "{texto}", `--bs-body-color-rgb` = "{rgb:texto}",
    `--bs-body-bg` = "{fondo}", `--bs-body-bg-rgb` = "{rgb:fondo}",
    `--bs-emphasis-color` = "{texto}", `--bs-emphasis-color-rgb` = "{rgb:texto}",
    `--bs-secondary-color` = "{texto-2}", `--bs-secondary-color-rgb` = "{rgb:texto-2}",
    `--bs-heading-color` = "{texto}",
    `--bs-border-color` = "{borde}",
    `--bs-link-color` = "{enlace}", `--bs-link-color-rgb` = "{rgb:enlace}",
    `--bs-link-hover-color` = "{primario}", `--bs-link-hover-color-rgb` = "{rgb:primario}",
    `--bs-focus-ring-color` = "{rgba:foco-color:0.35}",
    `--bs-font-sans-serif` = "{fuente-texto}", `--bs-body-font-family` = "{fuente-texto}",
    `--bs-success` = "{exito}", `--bs-success-rgb` = "{rgb:exito}",
    `--bs-success-text-emphasis` = "{exito-texto}", `--bs-success-bg-subtle` = "{exito-suave}",
    `--bs-success-border-subtle` = "{borde-fuerte}",
    `--bs-danger` = "{peligro}", `--bs-danger-rgb` = "{rgb:peligro}",
    `--bs-danger-text-emphasis` = "{peligro-texto}", `--bs-danger-bg-subtle` = "{peligro-suave}",
    `--bs-danger-border-subtle` = "{borde-fuerte}",
    `--bs-warning` = "{aviso}", `--bs-warning-rgb` = "{rgb:aviso}",
    `--bs-warning-text-emphasis` = "{aviso-texto}", `--bs-warning-bg-subtle` = "{aviso-suave}",
    `--bs-warning-border-subtle` = "{borde-fuerte}",
    `--bs-info` = "{info}", `--bs-info-rgb` = "{rgb:info}",
    `--bs-info-text-emphasis` = "{info}", `--bs-info-bg-subtle` = "{info-suave}",
    `--bs-info-border-subtle` = "{borde-fuerte}")),
  list(sel = c("h1", "h2", "h3", "h4", "h5", "h6", ".h1", ".h2", ".h3", ".h4", ".h5", ".h6"),
       decl = c(`font-family` = "{fuente-titulos}")),
  list(sel = c(".bg-primary"), decl = c(`--bslib-color-bg` = "{primario}", `--bslib-color-fg` = "{primario-texto}")),
  list(sel = c(".text-primary"), decl = c(`--bslib-color-fg` = "{primario}")),
  # Forms: focus, checked boxes (Shiny's own inputs too), range.
  list(sel = c(".form-control:focus", ".form-select:focus", ".form-check-input:focus",
               ".shiny-input-container .checkbox input:focus", ".shiny-input-container .checkbox-inline input:focus",
               ".shiny-input-container .radio input:focus", ".shiny-input-container .radio-inline input:focus"),
       decl = c(`border-color` = "{foco-color}", `box-shadow` = "{foco}")),
  list(sel = c(".nav-link:focus-visible", ".nav-tabs>li>a:focus-visible", ".nav-pills>li>a:focus-visible"),
       decl = c(`box-shadow` = "{foco}")),
  list(sel = c(".form-check-input:checked", ".form-check-input[type=\"checkbox\"]:indeterminate",
               ".shiny-input-container .checkbox input:checked", ".shiny-input-container .checkbox-inline input:checked",
               ".shiny-input-container .radio input:checked", ".shiny-input-container .radio-inline input:checked"),
       decl = c(`background-color` = "{primario}", `border-color` = "{primario}")),
  list(sel = c(".form-range::-webkit-slider-thumb"), decl = c(`background-color` = "{primario}")),
  list(sel = c(".form-range::-moz-range-thumb"), decl = c(`background-color` = "{primario}")),
  # Navigation.
  list(sel = c(".nav-tabs"), decl = c(`--bs-nav-tabs-border-color` = "{borde}",
                                      `--bs-nav-tabs-link-active-color` = "{primario}",
                                      `--bs-nav-tabs-link-active-bg` = "{superficie}",
                                      `--bs-nav-tabs-link-active-border-color` = "{borde} {borde} {superficie}")),
  list(sel = c(".nav-tabs .nav-link.active", ".nav-tabs>li>a.active"), decl = c(color = "{primario}")),
  list(sel = c(".nav-pills"), decl = c(`--bs-nav-pills-link-active-bg` = "{primario}",
                                       `--bs-nav-pills-link-active-color` = "{primario-texto}")),
  list(sel = c(".nav-pills>li.active>a", ".dropdown-menu>li.active>a"),
       decl = c(color = "{primario-texto}", `background-color` = "{primario}")),
  list(sel = c(".dropdown-menu"), decl = c(
    `--bs-dropdown-bg` = "{superficie}", `--bs-dropdown-border-color` = "{borde}",
    `--bs-dropdown-link-color` = "{texto}", `--bs-dropdown-link-hover-color` = "{texto}",
    `--bs-dropdown-link-hover-bg` = "{hover}", `--bs-dropdown-link-active-color` = "{primario-texto}",
    `--bs-dropdown-link-active-bg` = "{primario}")),
  list(sel = c(".list-group"), decl = c(
    `--bs-list-group-active-color` = "{primario-texto}", `--bs-list-group-active-bg` = "{primario}",
    `--bs-list-group-active-border-color` = "{primario}", `--bs-list-group-action-hover-bg` = "{hover}",
    `--bs-list-group-disabled-bg` = "{superficie-2}")),
  list(sel = c(".pagination"), decl = c(
    `--bs-pagination-color` = "{enlace}", `--bs-pagination-bg` = "{superficie}",
    `--bs-pagination-border-color` = "{borde}", `--bs-pagination-hover-color` = "{primario}",
    `--bs-pagination-hover-bg` = "{hover}", `--bs-pagination-focus-box-shadow` = "{foco}",
    `--bs-pagination-active-color` = "{primario-texto}", `--bs-pagination-active-bg` = "{primario}",
    `--bs-pagination-active-border-color` = "{primario}", `--bs-pagination-disabled-color` = "{texto-tenue}",
    `--bs-pagination-disabled-bg` = "{superficie}")),
  list(sel = c(".progress", ".progress-stacked"), decl = c(`--bs-progress-bar-bg` = "{primario}")),
  list(sel = c(".progress-bar-primary"), decl = c(`background-color` = "{primario}", color = "{primario-texto}")),
  # Alerts: flatly paints them solid with white text; a theme gets its soft
  # fill and the text role that reads on it.
  list(sel = c(".alert"), decl = c(color = "var(--bs-alert-color)")),
  list(sel = c(".alert a", ".alert .alert-link"), decl = c(color = "inherit")),
  list(sel = c(".alert-primary"), decl = c(`background-color` = "{primario-suave}", `--bs-alert-color` = "{primario}")),
  list(sel = c(".alert-success"), decl = c(`background-color` = "{exito-suave}", `--bs-alert-color` = "{exito-texto}")),
  list(sel = c(".alert-danger"), decl = c(`background-color` = "{peligro-suave}", `--bs-alert-color` = "{peligro-texto}")),
  list(sel = c(".alert-warning"), decl = c(`background-color` = "{aviso-suave}", `--bs-alert-color` = "{aviso-texto}")),
  list(sel = c(".alert-info"), decl = c(`background-color` = "{info-suave}", `--bs-alert-color` = "{info}")),
  # The top bar (TEMA 2b). bslib paints it with --bslib-navbar-light-bg (flatly
  # compiles .navbar { --bslib-navbar-light-bg: #0A58CA }); nothing in the
  # repo paints it. Hover and active in barra-texto: the tab links live in the
  # ☰ panel, which has its own colours (R/ui_components.R, T51).
  list(sel = c(".navbar"), decl = c(
    `--bslib-navbar-light-bg` = "{barra-fondo}", `--bslib-navbar-dark-bg` = "{barra-fondo}",
    `--bs-navbar-color` = "{barra-texto}", `--bs-navbar-brand-color` = "{barra-texto}",
    `--bs-navbar-brand-hover-color` = "{barra-texto}", `--bs-navbar-hover-color` = "{barra-texto}",
    `--bs-navbar-active-color` = "{barra-texto}")),
  # The quiet button (TEMA 2b): .btn-secondary and .btn-default, which Shiny
  # gives every actionButton() without a class. flatly paints both grey
  # (#95a5a6, white text: 2.56:1); a theme gets its surface, ink and border.
  # The .btn-default selector repeats bslib's :not() chain (0,10,0) to beat it;
  # the guardian checks it against the CSS bslib compiles.
  list(sel = c(".btn-secondary", .HD_BTN_DEFAULT_SEL), decl = c(
    `--bs-btn-color` = "{texto}", `--bs-btn-bg` = "{superficie}", `--bs-btn-border-color` = "{borde-fuerte}",
    `--bs-btn-hover-color` = "{texto}", `--bs-btn-hover-bg` = "{hover}", `--bs-btn-hover-border-color` = "{texto-2}",
    `--bs-btn-focus-shadow-rgb` = "{rgb:foco-color}",
    `--bs-btn-active-color` = "{texto}", `--bs-btn-active-bg` = "{seleccion}", `--bs-btn-active-border-color` = "{texto-2}",
    `--bs-btn-disabled-color` = "{texto}", `--bs-btn-disabled-bg` = "{superficie}",
    `--bs-btn-disabled-border-color` = "{borde-fuerte}")),
  list(sel = c(".btn-outline-secondary", ".btn-outline-default"), decl = c(
    `--bs-btn-color` = "{texto-2}", `--bs-btn-bg` = "transparent", `--bs-btn-border-color` = "{borde-fuerte}",
    `--bs-btn-hover-color` = "{texto}", `--bs-btn-hover-bg` = "{hover}", `--bs-btn-hover-border-color` = "{texto-2}",
    `--bs-btn-focus-shadow-rgb` = "{rgb:foco-color}",
    `--bs-btn-active-color` = "{texto}", `--bs-btn-active-bg` = "{seleccion}", `--bs-btn-active-border-color` = "{texto-2}",
    `--bs-btn-disabled-color` = "{texto-2}", `--bs-btn-disabled-bg` = "transparent",
    `--bs-btn-disabled-border-color` = "{borde-fuerte}"))
)

# Bootstrap's coloured buttons: fill, the text on it, hover fill (NA: the fill
# mixed 15 % towards `mezcla`, 20 % when pressed: Bootstrap's own rule;
# black under white text, white under dark text, so the text keeps its
# contrast) and the colour of the outline variant (it reads as text on white,
# so a -texto role where the fill is light).
.HD_PUENTE_BOTONES <- list(
  clase    = c("primary",        "success",       "danger",        "warning",     "info"),
  fondo    = c("primario",       "exito",         "peligro",       "aviso",       "info"),
  texto    = c("primario-texto", "texto-inverso", "texto-inverso", "texto",       "texto-inverso"),
  hover    = c("primario-hover", NA,              NA,              NA,            NA),
  mezcla   = c("black",          "black",         "black",         "white",       "black"),
  contorno = c("primario",       "exito-texto",   "peligro-texto", "aviso-texto", "info")
)

# Expand one template for a theme (see .HD_PUENTE_BS).
.tema_puente_valor <- function(tpl, tema) {
  tok <- c(.HD_COMUNES, HD_TEMAS[[tema]]$tokens)
  hex_rgb <- function(rol) {
    h <- tok[[rol]]
    if (is.null(h) || !grepl("^#[0-9A-Fa-f]{6}$", h)) stop("[tema] puente: ", rol, " is not a #rrggbb role", call. = FALSE)
    paste(strtoi(substring(h, c(2, 4, 6), c(3, 5, 7)), 16L), collapse = ", ")
  }
  m <- gregexpr("\\{[^}]+\\}", tpl)
  regmatches(tpl, m) <- list(vapply(regmatches(tpl, m)[[1]], function(x) {
    p <- strsplit(substr(x, 2L, nchar(x) - 1L), ":", fixed = TRUE)[[1]]
    if (length(p) == 1L) {
      if (is.null(tok[[p]])) stop("[tema] puente: unknown role ", p, call. = FALSE)
      paste0("var(--hd-", p, ")")
    } else if (p[1] == "rgb" && length(p) == 2L) hex_rgb(p[2])
    else if (p[1] == "rgba" && length(p) == 3L) paste0("rgba(", hex_rgb(p[2]), ", ", p[3], ")")
    else stop("[tema] puente: bad template ", x, call. = FALSE)
  }, character(1)))
  tpl
}

# Every block of the bridge, buttons included, as list(sel, decl) with the
# templates still unexpanded.
.tema_puente_bloques <- function() {
  b <- .HD_PUENTE_BOTONES
  botones <- lapply(seq_along(b$clase), function(i) {
    f <- paste0("{", b$fondo[i], "}"); t <- paste0("{", b$texto[i], "}")
    h <- if (is.na(b$hover[i])) paste0("color-mix(in srgb, ", f, " 85%, ", b$mezcla[i], ")") else paste0("{", b$hover[i], "}")
    a <- if (is.na(b$hover[i])) paste0("color-mix(in srgb, ", f, " 80%, ", b$mezcla[i], ")") else h
    list(sel = paste0(".btn-", b$clase[i]), decl = c(
      `--bs-btn-color` = t, `--bs-btn-bg` = f, `--bs-btn-border-color` = f,
      `--bs-btn-hover-color` = t, `--bs-btn-hover-bg` = h, `--bs-btn-hover-border-color` = h,
      `--bs-btn-focus-shadow-rgb` = paste0("{rgb:", b$fondo[i], "}"),
      `--bs-btn-active-color` = t, `--bs-btn-active-bg` = a, `--bs-btn-active-border-color` = a,
      `--bs-btn-disabled-color` = t, `--bs-btn-disabled-bg` = f, `--bs-btn-disabled-border-color` = f))
  })
  contornos <- lapply(seq_along(b$clase), function(i) {
    c0 <- paste0("{", b$contorno[i], "}"); f <- paste0("{", b$fondo[i], "}"); t <- paste0("{", b$texto[i], "}")
    list(sel = paste0(".btn-outline-", b$clase[i]), decl = c(
      `--bs-btn-color` = c0, `--bs-btn-border-color` = c0,
      `--bs-btn-hover-color` = t, `--bs-btn-hover-bg` = f, `--bs-btn-hover-border-color` = f,
      `--bs-btn-focus-shadow-rgb` = paste0("{rgb:", b$fondo[i], "}"),
      `--bs-btn-active-color` = t, `--bs-btn-active-bg` = f, `--bs-btn-active-border-color` = f,
      `--bs-btn-disabled-color` = c0, `--bs-btn-disabled-border-color` = c0))
  })
  c(.HD_PUENTE_BS, botones, contornos)
}

# The text of www/hd_tema_bootstrap.css (scripts/generar_temas_css.R).
tema_puente_css <- function() {
  lineas <- c(
    "/* ============================================================================",
    "   www/hd_tema_bootstrap.css: GENERATED from .HD_PUENTE_BS in R/tema_registro.R",
    "   by scripts/generar_temas_css.R. Do not edit by hand.",
    "   Bootstrap (flatly) re-pointed at the theme's roles, only under the themes",
    "   with puente = TRUE. Clasico has no block: Bootstrap stays as it is.",
    "   ========================================================================== */")
  for (t in names(HD_TEMAS)[vapply(HD_TEMAS, function(x) isTRUE(x$puente), logical(1))]) {
    raiz <- paste0("html[data-hd-tema=\"", t, "\"]")
    lineas <- c(lineas, "", paste0("/* ", t, " */"))
    for (b in .tema_puente_bloques()) {
      sel <- if (identical(b$sel, "")) raiz else paste(raiz, b$sel)   # one selector per line
      sel <- c(if (length(sel) > 1L) paste0(sel[-length(sel)], ","), paste0(sel[length(sel)], " {"))
      val <- vapply(unname(b$decl), .tema_puente_valor, character(1), tema = t)
      lineas <- c(lineas, sel, paste0("  ", names(b$decl), ": ", val, ";"), "}")
    }
  }
  c(lineas, "")
}

# ── Para app.R ───────────────────────────────────────────────────────────────

# The one Bootstrap theme of every page (it was written three times in app.R).
# No per-session Sass: themes switch through the CSS custom properties.
hd_bs_theme <- function() {
  bslib::bs_theme(version = 5, bootswatch = "flatly",
                  primary = HD_TEMAS$clasico$tokens$primario)
}

# The browser side of the theme (TEMA stage 5), one line of JavaScript:
#  - first paint: the default theme, or the one this browser last saw for
#    the last client it showed (localStorage "hd-tema:<client>"), if it is
#    in the closed list. Only a convenience: no storage (private window,
#    blocked) paints the default and the server corrects it;
#  - the server's "hdTema" message ({tema, cliente}) sets the attribute and
#    remembers it per client. Anything not in the list is the default.
.tema_js <- function() {
  lista <- paste0("['", paste(names(HD_TEMAS), collapse = "','"), "']")
  paste0(
    "(function(){var L=", lista, ",D='", HD_TEMA_OMISION, "',h=document.documentElement;",
    "function ok(t){return L.indexOf(t)>=0?t:D;}",
    "var t=D;try{var c=localStorage.getItem('hd-tema-cliente');",
    "if(c)t=ok(localStorage.getItem('hd-tema:'+c));}catch(e){}",
    "h.setAttribute('data-hd-tema',t);",
    "function reg(){if(!(window.Shiny&&Shiny.addCustomMessageHandler))return false;",
    "Shiny.addCustomMessageHandler('hdTema',function(m){var v=ok(m&&m.tema);",
    "h.setAttribute('data-hd-tema',v);",
    "try{if(m&&m.cliente){localStorage.setItem('hd-tema:'+m.cliente,v);",
    "localStorage.setItem('hd-tema-cliente',m.cliente);}}catch(e){}});return true;}",
    "if(!reg())document.addEventListener('DOMContentLoaded',reg);})();")
}

# Goes in a page's header: the generated tokens and the theme attribute.
# tags$head() is hoisted into <head>, so the attribute is set while the page
# parses, before anything paints, and no server message is needed for it:
# nothing here can hold up the start. The CSS does not depend on it either
# (:root carries the default theme). The server's message changes it later.
tema_head <- function() {
  htmltools::tags$head(
    htmltools::tags$link(rel = "stylesheet", type = "text/css",
                         href = paste0("hd_temas.css?v=", HD_TEMAS_CSS_VERSION)),
    htmltools::tags$link(rel = "stylesheet", type = "text/css",
                         href = paste0("hd_clasico_legado.css?v=", HD_TEMAS_CSS_VERSION)),
    # The Bootstrap bridge: rules only under themes with puente = TRUE.
    htmltools::tags$link(rel = "stylesheet", type = "text/css",
                         href = paste0("hd_tema_bootstrap.css?v=", HD_TEMAS_CSS_VERSION)),
    htmltools::tags$script(htmltools::HTML(.tema_js()))
  )
}
