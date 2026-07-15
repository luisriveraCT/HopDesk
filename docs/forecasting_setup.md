# Forecasting module — environment setup

## Required environment variables

- `BANXICO_TOKEN` — Banxico SIE API token (free registration at https://www.banxico.org.mx/SieAPIRest/service/v1/token)
- `FRED_API_KEY`  — FRED API key (free registration at https://fred.stlouisfed.org/docs/api/api_key.html)

On shinyapps.io: set via the Dashboard → app settings → environment variables.

Without these, fetches for legal metrics (USD/MXN FIX, TIIE, INPC, UDIS) and SOFR return errors.
The module still functions — historical observations and manual_curve methods work — but no new data is pulled.

Yahoo Finance (EUR/USD, EUR/MXN, GBP, JPY, CAD pairs) requires no API key.

## First-run seeding

On the first deployment, open the Forecasting tab and click **"Actualizar todas las métricas"** in the Registro de consultas sub-tab to populate historical observations. This is a one-time manual step; subsequent fetches can be triggered per-metric from the Series sub-tab.

## Architecture notes

- **Legal metrics** (Banxico-sourced) have `is_legal_metric = TRUE`. The dispatcher enforces no fallback — if Banxico's API is unavailable, the module returns NA rather than using an alternative source.
- **Subscription resolution** (three-tier): per-consumer → global_default → metric default. No Pasivos code changes are required in Stage 6.1.
- **Sync bus**: `forecasting_series_observations`, `forecasting_metrics`, `forecasting_subscriptions`, and `forecasting_manual_curves` are all registered with the sync bus. Changes propagate to other sessions within 3 seconds.
