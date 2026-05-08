# Session notes — 2026-05-06 / 05-08

## Pasivos module fixes

- **`+ Provisión manual` modal — Guardar broken**: All input IDs in `.ppm_modal_ui()` were bare strings; module server expected namespaced IDs. Fixed by threading `ns` through the modal builder.
- **Vendor suggestions in `+ Provisión manual`**: Added same card-based suggestion system (with auto-fill for Parte + Código) that the wizard already has. Same data source, matching logic, and payload format.
- **Tarjeta provisions generating $0**: `recurrence_type` was never set for tarjeta in the wizard collect step. Fixed by auto-setting `recurrence_type = "monthly_day"` and `recurrence_params = list(day = tarjeta_due_day)`.
- **Table crash "missing value where TRUE/FALSE needed"**: `if (eff_date < today)` threw on NA dates. Added `if (is.na(eff_date)) return(NULL)` guard in `pasivos_table_pivot.R`.
- **Row overdue/due-soon highlighting**: Replaced the non-functional `box-shadow` approach with proper `background-color` on the `tr` + a colored `border-left` on the sticky label `td`. Added `has_due_soon` (7-day window) through the full pivot → metadata → render pipeline.
- **"Guardar provisión" button**: Added to the convert modal so provisions can be edited and saved in-place without converting to an item. Updates overrideable fields (`amount_pago_override`, `fecha_efectiva_override`, text fields) without touching `estado` or engine fields.
- **"Eliminar" button with soft-delete**: Added to the convert modal with a two-step confirmation. Sets `estado = "deleted"` (row stays in S3 forever), archives a copy to the unified `papelera.rds`, and logs to the audit trail. Both display layers already filtered out non-`provisional` rows so no further changes were needed there.
- **Today-column header styling**: Added bold blue style specifically to `thead th.pasivos-table-today-col` so the current date stands out in the header while cell backgrounds below stay as-is.
- **Políticas de Pago missing parties**: `.all_partes()` now also pulls `parte` from active/paused liabilities in `pasivos_liabilities_db()`.

## Calendar day modal (ledger_module)

- **Provision rows in day panel**: All-provision groups now show a purple `⚡ Provisión` badge next to the vendor name and replace the `+ Agregar` button with a ⚡ bolt that fires `pasivos_convert_request`. On hover the bolt transitions to solid provision-purple (`#6d28d9`).

## One-time cleanup script

- **`wipe_scripts/remove_autodesk_pasivos.R`**: Dry-run-first script to soft-delete a stuck AUTODESK liability + its provisions. Prints all matching rows before writing; actual delete only runs when `.do_cleanup()` is called manually.

## Files changed

`R/pasivos_table_module.R` · `R/pasivos_table_pivot.R` · `R/pasivos_table_styles.R` · `R/pasivos_module.R` · `R/pasivos_calendar_glue.R` · `R/settings_module.R` · `R/ledger_module.R` · `R/ui_components.R` · `wipe_scripts/remove_autodesk_pasivos.R` *(new)*

---

## Session 2026-05-07/08 — Pasivos: payment frequency + variable rate widget

- **Frecuencia de pago** (`mensual / anual / semanal / diaria`): new schema fields `frecuencia_pago` + `mes_pago`; engine replaced hard-coded `/12` with `.periods_per_year()` and `.add_payment_period()`; wizard compound plazo+freq row; annual payment day picker (leap-year-aware, dynamic day choices); backward-compat coalesce in persistence loader.
- **EAR rate conversion**: switched all rate math from linear to compound — `(1+r)^(1/n)−1` and `((1+r)^n−1)×100`; 3 dp display, up to 13 dp input preserved.
- **Recursive rate input bug fixed**: observers now fire on `focusout` (blur) shadow inputs `pwiz_tasa_anual_blur` / `pwiz_tasa_periodo_blur` via JS event delegation — server-side `updateNumericInput` no longer triggers the cross-update observer, ending the bounce loop.
- **Rate type UI restructured**: `radioButtons` moved to top; dual anual↔período inputs shown only for `fija` via `conditionalPanel`; variable rates get their own panel.
- **Variable rate widget**: Hoy / Día anterior toggle (Ayer pre-selected); auto-fetches TIIE28 (Banxico SIE API, serie SF43783) or SOFR (FRED API) on tipo change or step entry; preview shows both days with spread (bps) math and EAR per period; source attribution per compliance.
- **Engine spread formula fixed**: was `(est + spread_bps) / ppy / 100`; corrected to `(1 + (est + spread_bps/100)/100)^(1/ppy) − 1`.
- **New file**: `R/pasivos_rates.R` — `.fetch_tiie28()` and `.fetch_sofr()`.

`R/pasivos_schemas.R` · `R/pasivos_engine.R` · `R/pasivos_wizard_steps.R` · `R/pasivos_wizard_module.R` · `R/pasivos_wizard_validate.R` · `R/pasivos_persistence.R` · `R/pasivos_rates.R` *(new)*

---

## Session 2026-05-08 — Abono Parcial: UI overhaul + confirm-gated balance deduction

- **Abono modal UI** (`staging_browse_module.R`): replaced flat flex-row list with a Vencidos-style Bootstrap table. Invoices grouped by Parte + Empresa + Moneda with collapsible group rows (per-group ▼ button + global ▼▼ Expandir / ▲▲ Colapsar). Group-level checkbox checks/unchecks all sub-rows. Referencia (No. Factura / NumAtCard) shown bold as primary label; Documento shown below in grey when it differs.
- **Search bar**: client-side filter above the existing dropdowns; matches simultaneously on Parte, Referencia, and Documento. Works across expanded and collapsed groups.
- **Confirm-gated deduction**: "Enviar a Agenda" now only stages rows to `pagar_hoy` as `tipo_item="abono"` / `status="pending"` — **no longer writes to `abonos_db` immediately**, so the calendar balance is not affected until the user explicitly clicks "Confirmar pagos" in Agenda de Hoy.
- **Confirmation wiring** (`pagar_hoy_module.R`): both `do_confirm_ap_` and `do_confirm_ar_` handlers now split confirmed rows by `tipo_item`. Abono rows are written to `abonos_db` with `status="active"` (triggering the calendar deduction); invoice rows continue through the existing `bancos_confirmados` / `conciliacion` path unchanged.

`R/staging_browse_module.R` · `R/pagar_hoy_module.R`

---

## Session 2026-05-08 — Pasivos: cargos iniciales, currency groups, fee sub-rows, unified editor

- **Cargo wiping fixed** (`pasivos_wizard_module`): add/remove handlers snapshot current input values before mutating `cargos_list`, so re-render no longer overwrites user-typed data.
- **Unique occurrence_index per fee** (`pasivos_engine`): fees now get `-(fi+1000)` indices instead of all `0`, preventing reconcile from collapsing multiple fee provisions into one.
- **Per-fee currency**: fee provisions use their own `moneda` field, bypassing the liability's FX conversion path.
- **Fee description in provisions**: `f$desc` stored in `fee_desc` on the schedule row and propagated to `referencia` in the generated provision for display purposes.
- **Residual balloon fix**: `capital` in the `separate` residual row was `0`; corrected to `monto`.
- **Calendar BAJIO USD fix** (`data_pipeline`): two `left_join` calls got `na_matches="never"` and a `FechaEff` column pre-init guard to prevent NA-Documento phantom matches that were suppressing USD provisions.
- **Client timezone**: browser TZ sent on `shiny:connected`; all timestamp formatting now uses the user's local zone.
- **Currency segmentation in Pasivos table**: rows sorted MXN → USD → EUR; one separator per currency block; 7-day summary badges per currency in the header line; `has_very_soon` highlight (0–2 days, deeper amber) added alongside `has_due_soon`.
- **Fee sub-rows**: each fee provision rendered as its own indented `↳ description` row (lavender bg, dashed border) sorted immediately after its parent liability; no currency pill or pencil on sub-rows.
- **Calendar fee dedup** (`pasivos_calendar_glue`): provisions without a real SAP `Documento` get a synthetic `PROV_<uuid>` key so fee provisions are never aggregated with each other in day panels.
- **Auditoría mode → provision editor** (`ledger_module`): `.open_edit_for_row()` now intercepts `source == "provision"` rows and fires `pasivos_cell_click` via `shinyjs::runjs` instead of opening the SAP editor.
- **Unified provision editor**: `pasivos_cell_click` (table cell click) now opens the full convert modal (Guardar provisión / Guardar como comprobante / Guardar y agregar / Eliminar). `pasivos_cell_editor.R` deleted and removed from `app.R`.
- **Table density**: font reduced `13px → 11.5px`, cell padding `6px 8px → 4px 7px`.

`R/pasivos_engine.R` · `R/pasivos_wizard_module.R` · `R/pasivos_module.R` · `R/pasivos_table_pivot.R` · `R/pasivos_table_module.R` · `R/pasivos_table_styles.R` · `R/pasivos_calendar_glue.R` · `R/ledger_module.R` · `R/data_pipeline.R` · `R/ui_components.R` · `app.R` · ~~`R/pasivos_cell_editor.R`~~ *(deleted)*
