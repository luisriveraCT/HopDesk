# =============================================================================
# R/bancos_formatos.R
# The Libro de Banco's export formats: how its movements are written for a
# bank's or an accounting system's own layout.
#
# Adding another bank's or system's format = one more entry in
# BANCOS_FORMATOS_EXPORT: its button's dictionary key (nombre), its file
# name (archivo) and its writer (escribir). Later each client or group will
# choose its format from its own configuration (the multi-client settings
# layer); today everyone uses "banbajio" (Luis, 2026-11-03: it is used to
# reconcile and for accounting).
#
# escribir(movs, archivo, todas, company_map): movs are the Libro's rows as
# the module filters them (bancos_movimientos joined with the account's
# empresa, alias and moneda); todas = every account shows (then Empresa and
# Moneda go in their own columns). It writes the file and returns what the
# export log needs: list(n, empresa, moneda).
# =============================================================================

BANCOS_FORMATOS_EXPORT <- list(
  banbajio = list(
    nombre  = "tablas.bancos.exportar_banbajio",
    archivo = function(alias) paste0("movimientos_", alias, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    # BanBajío's own statement layout: Fecha Movimiento, Hora, Recibo,
    # Descripcion, Cargos, Abonos, Saldo (Empresa first and Moneda last
    # with every account); debits in red, credits in green.
    escribir = function(movs, file, todas, company_map) {
      base_exp <- dplyr::transmute(movs,
        `Fecha Movimiento` = format(as.Date(fecha), "%d-%b-%Y"),
        Hora               = as.character(hora),
        Recibo             = as.character(recibo),
        `Descripcion`      = descripcion_raw,
        Cargos             = cargo,
        Abonos             = abono,
        Saldo              = saldo_banco,
        .empresa           = {
          ini <- dplyr::na_if(trimws(dplyr::coalesce(empresa, "")), "")
          nm  <- .cmap_lookup(company_map, dplyr::coalesce(ini, ""))
          dplyr::coalesce(
            ifelse(!is.na(nm), unname(nm), NA_character_),  # full name from map
            ini,                                              # raw initials if not in map
            alias                                             # account alias when empresa is unset
          )
        },
        .moneda            = toupper(trimws(moneda %||% ""))
      )

      if (todas) {
        export <- dplyr::transmute(base_exp,
          Empresa            = .empresa,
          `Fecha Movimiento` = `Fecha Movimiento`,
          Hora               = Hora,
          Recibo             = Recibo,
          `Descripcion`      = `Descripcion`,
          Cargos             = Cargos,
          Abonos             = Abonos,
          Saldo              = Saldo,
          Moneda             = .moneda
        )
        # col layout: Empresa(1),Fecha(2),Hora(3),Recibo(4),Desc(5),Cargos(6),Abonos(7),Saldo(8),Moneda(9)
        txt_cols   <- 1:5
        col_cargo  <- 6L; col_abono <- 7L; col_saldo <- 8L; col_moneda <- 9L
        col_widths <- c(30, 16, 10, 16, 55, 14, 14, 14, 10)
      } else {
        export <- dplyr::select(base_exp, -`.empresa`, -`.moneda`)
        # col layout: Fecha(1),Hora(2),Recibo(3),Desc(4),Cargos(5),Abonos(6),Saldo(7)
        txt_cols   <- 1:4
        col_cargo  <- 5L; col_abono <- 6L; col_saldo <- 7L; col_moneda <- NULL
        col_widths <- c(16, 10, 16, 55, 14, 14, 14)
      }

      wb <- openxlsx::createWorkbook()
      ws <- "Movimientos"
      openxlsx::addWorksheet(wb, ws)
      openxlsx::modifyBaseFont(wb, fontName = "Calibri", fontSize = 10)
      openxlsx::freezePane(wb, ws, firstRow = TRUE)

      mxn <- '$#,##0.00'

      st_hdr <- openxlsx::createStyle(
        fgFill = "#1E3A5F", fontColour = "#FFFFFF",
        fontName = "Calibri", fontSize = 10, textDecoration = "bold",
        halign = "center", valign = "center",
        border = "TopBottomLeftRight", borderColour = "#4A7DB5", borderStyle = "thin"
      )
      st_txt <- openxlsx::createStyle(
        fontName = "Calibri", fontSize = 10, halign = "left", valign = "center",
        border = "TopBottomLeftRight", borderColour = "#CCCCCC", borderStyle = "thin"
      )
      st_num <- openxlsx::createStyle(
        fontName = "Calibri", fontSize = 10, halign = "right", valign = "center",
        numFmt = mxn,
        border = "TopBottomLeftRight", borderColour = "#CCCCCC", borderStyle = "thin"
      )
      st_cargo <- openxlsx::createStyle(
        fontName = "Calibri", fontSize = 10, fontColour = "#C0392B",
        halign = "right", valign = "center", numFmt = mxn,
        border = "TopBottomLeftRight", borderColour = "#CCCCCC", borderStyle = "thin"
      )
      st_abono <- openxlsx::createStyle(
        fontName = "Calibri", fontSize = 10, fontColour = "#1A7A3A",
        halign = "right", valign = "center", numFmt = mxn,
        border = "TopBottomLeftRight", borderColour = "#CCCCCC", borderStyle = "thin"
      )

      openxlsx::writeData(wb, ws, export, startRow = 1, startCol = 1,
                          headerStyle = st_hdr)

      nr <- nrow(export)
      if (nr > 0) {
        data_rows <- seq(2L, nr + 1L)

        openxlsx::addStyle(wb, ws, st_txt, rows = data_rows, cols = txt_cols,  gridExpand = TRUE)
        openxlsx::addStyle(wb, ws, st_num, rows = data_rows, cols = col_saldo, gridExpand = TRUE)
        if (!is.null(col_moneda))
          openxlsx::addStyle(wb, ws, st_txt, rows = data_rows, cols = col_moneda, gridExpand = TRUE)

        cargo_vals <- export$Cargos
        rows_cargo_red   <- which(!is.na(cargo_vals) & cargo_vals > 0) + 1L
        rows_cargo_plain <- setdiff(data_rows, rows_cargo_red)
        if (length(rows_cargo_red)   > 0)
          openxlsx::addStyle(wb, ws, st_cargo, rows = rows_cargo_red,   cols = col_cargo, gridExpand = TRUE)
        if (length(rows_cargo_plain) > 0)
          openxlsx::addStyle(wb, ws, st_num,   rows = rows_cargo_plain, cols = col_cargo, gridExpand = TRUE)

        abono_vals <- export$Abonos
        rows_abono_grn   <- which(!is.na(abono_vals) & abono_vals > 0) + 1L
        rows_abono_plain <- setdiff(data_rows, rows_abono_grn)
        if (length(rows_abono_grn)   > 0)
          openxlsx::addStyle(wb, ws, st_abono, rows = rows_abono_grn,   cols = col_abono, gridExpand = TRUE)
        if (length(rows_abono_plain) > 0)
          openxlsx::addStyle(wb, ws, st_num,   rows = rows_abono_plain, cols = col_abono, gridExpand = TRUE)
      }

      openxlsx::setColWidths(wb, ws, cols = seq_along(col_widths), widths = col_widths)
      openxlsx::setRowHeights(wb, ws, rows = 1, heights = 20)

      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

      list(n = nrow(export),
           empresa = if (todas) "Todas" else (base_exp$.empresa[1] %||% NA_character_),
           moneda  = if (todas) NA_character_ else (base_exp$.moneda[1] %||% NA_character_))
    }
  )
)
