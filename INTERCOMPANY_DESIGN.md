# Intercompany Feature — Design & Implementation

## Overview

The Intercompany (IC) feature allows the app to classify invoices as intercompany
transactions and filter/highlight them independently of regular third-party invoices.
This is critical for diagnosing the group's consolidated financial position, since IC
balances cancel each other out and must be isolated.

---

## Research Findings

### SAP API Limitations

| Path | Outcome |
|------|---------|
| `GET /BusinessPartners` | **403 Forbidden** — user lacks list permission |
| Single BP key lookup | **403 Forbidden** |
| `$expand=BusinessPartner` on Invoices | **403 Forbidden** |
| `POST /SQLQueries` | **403 Forbidden** — no query creation rights |
| `GET /SQLQueries` | 200 (list only, empty) |
| RFC (FederalTaxID) via any path | **Not accessible** with current credentials |

**Conclusion:** RFC enrichment via SAP API is not possible. CardCode exact-match is the
only reliable IC identification mechanism available.

### IC Code Structure

SAP B1 (Networks Group configuration) uses:
- `C{n}` prefix for AR (customer) CardCodes — e.g., `C1027`
- `P{n}` prefix for AP (supplier) CardCodes — e.g., `P1426`

**Critical finding:** the numeric base `n` is **different** for the same entity as
customer vs. supplier. `C1027` and `P1426` both represent Networks Crossdocking Services
in NTS's SAP, but share no common number. AR and AP codes must be stored separately.

This prefix convention is **not universal** — other SAP installations may use purely
numeric codes or different letters. Prefixes are configurable per deployment.

### Discovered IC Codes (from live SAP data, April 2026)

| Source | Ledger | CardCode | Entity |
|--------|--------|----------|--------|
| NG | AR | C1001 | Networks & Logistics |
| NG | AR | C1003 | Networks Trucking Services |
| NG | AP | P1026 | Networks Realtors |
| NG | AP | P1039 | Networks & Logistics |
| NG | AP | P1042 | Networks Trucking Services |
| NTS | AR | C1027 | Networks Crossdocking Services |
| NTS | AR | C1047 | Networks & Logistics |
| NTS | AR | C1139 | Networks Realtors |
| NTS | AR | C1170 | Networks Group LCT |
| NTS | AP | P1100 | Networks Logistics SA de CV |
| NTS | AP | P1185 | Networks Realtors |
| NTS | AP | P1426 | Networks Crossdocking Services |
| NTS | AP | P1559 | Networks Group LCT |
| NCS | AR | C1015 | Networks & Logistics |
| NCS | AR | C1016 | Networks Realtors |
| NCS | AR | C1085 | Networks Trucking Services |
| NCS | AP | P1053 | Networks Trucking Services |
| NCS | AP | P1054 | Networks & Logistics |
| NCS | AP | P1061 | Networks Realtors |
| NCS | AP | P1413 | Networks Group LCT |
| NRS | AR | C1003 | Networks Crossdocking Services |
| NRS | AR | C1012 | Networks Trucking Services |
| NRS | AR | C1015 | Networks & Logistics |
| NRS | AR | C1020 | Networks Group LCT |
| NRS | AP | P1025 | Networks & Logistics |
| NRS | AP | P1026 | Networks Crossdocking Services |
| NRS | AP | P1209 | Networks Trucking Services |
| NL | AR | C1003 | Networks Crossdocking Services |
| NL | AR | C1004 | Networks Realtors |
| NL | AR | C1006 | Networks Trucking Services |
| NL | AR | C1034 | Networks Group LCT |
| NL | AP | P1039 | Networks Trucking Services |
| NL | AP | P1067 | Networks Crossdocking Services |
| NL | AP | P1080 | Networks Realtors |
| NL | AP | P1110 | Networks Group LCT |

Note: Some company pairs may be absent because no open invoices existed in that
direction at scan time. Admins can add missing codes manually via Settings.

---

## Architecture

### Data Flow

```
SAP Invoice Data (CardCode field)
        │
        ▼
  build_ic_fullcodes(registry, ledger)
  → union of all IC full codes for that ledger
  → ["C1027", "C1047", "C1085", ...]
        │
        ▼
  apply_ic_filter(df, mode, code_col, ic_codes)
  mode = "exclude" → remove IC rows from calendar
  mode = "include" → show all (no-op)
  mode = "only"    → keep only IC rows
        │
        ▼
  Calendar / Detail view
```

For the **Agregar (manual entry) IC badge**:

```
input$me_empresa (selected empresa)
        │
        ▼
  Lookup empresa initials via COMPANY_MAP
  Get registry$companies[[initials]]$ap codes
  paste0(ap_prefix, ap_codes) → empresa-specific AP IC full codes
        │
        ▼
  For each vendor suggestion:
    is_ic = toupper(codigo) %in% toupper(ap_ic_codes)
    if is_ic → render "Intercompany" blue badge
```

---

## Data Schema

### S3 Key: `interco_v2.rds`

Replaces the old `interco_settings.rds` (`ar_clients` / `ap_suppliers` flat lists).

```r
list(
  ar_prefix  = "C",   # SAP prefix for AR CardCodes. Default "C". Configurable per group.
  ap_prefix  = "P",   # SAP prefix for AP CardCodes. Default "P". Configurable per group.
  companies  = list(  # Named by empresa initials from COMPANY_MAP
    NG  = list(ar = c("1003","1001"),              ap = c("1042","1039","1026")),
    NTS = list(ar = c("1027","1047","1139","1170"), ap = c("1426","1100","1185","1559")),
    NCS = list(ar = c("1085","1015","1016"),        ap = c("1053","1054","1061","1413")),
    NRS = list(ar = c("1003","1012","1015","1020"), ap = c("1026","1025","1209")),
    NL  = list(ar = c("1003","1006","1004","1034"), ap = c("1039","1067","1080","1110"))
  )
)
```

- Each entry under `companies` is keyed by initials matching `COMPANY_MAP`.
- `ar` / `ap` contain **numeric parts only** (no prefix). Prefix is added at match time.
- When a new empresa is added to `COMPANY_MAP`, its entry is auto-created empty.
- When a 6th company (LCT) is added to `COMPANY_MAP`, it gains its own accordion panel.

---

## Settings UI (Settings › Intercompany)

```
┌─ Prefijos de códigos SAP ──────────────────────────────────────┐
│  Prefijo clientes (CxC): [C]   Prefijo proveedores (CxP): [P]  │
│  (Modificar solo si tu SAP usa una convención diferente)        │
└────────────────────────────────────────────────────────────────┘

▼ Networks Group (NG)
  Clientes IC — CxC              Proveedores IC — CxP
  ┌────────────────────┐         ┌────────────────────┐
  │ 1003               │         │ 1042               │
  │ 1001               │         │ 1039               │
  │                    │         │ 1026               │
  └────────────────────┘         └────────────────────┘

▼ Networks Trucking Services (NTS)
  ...

[💾 Guardar configuración]
```

**Input rules:**
- One numeric code per line (no prefix). Example: `1027`
- If admin accidentally types `C1027` → app strips the prefix, stores `1027`, shows notification
- Codes are deduplicated on save

---

## Agregar (Manual Entry) IC Badge

When adding a **CxP (AP) manual entry**:
1. User selects `Empresa` (e.g., "Networks Trucking Services")
2. User types a supplier name → suggestion dropdown appears
3. For each suggestion, if its SAP `codigo` is in NTS's AP IC code list → show blue "Intercompany" badge
4. CardName in suggestion already identifies the entity (e.g., "NETWORKS & LOGISTICS SA DE CV")
5. No code changes needed to the AR side (no suggestion dropdown there)

---

## Filter Logic

```r
# Build full CardCode set for one ledger (used by calendar toggle)
build_ic_fullcodes(registry, "AR")
# → paste0("C", unique(unlist(all companies' ar lists)))
# → c("C1027","C1047","C1085","C1015","C1016","C1003","C1012","C1020","C1006","C1004","C1034","C1003","C1001")

# Context-specific check for Agregar (empresa = NTS, ledger = AP)
paste0("P", registry$companies[["NTS"]]$ap)
# → c("P1426","P1100","P1185","P1559")
```

`apply_ic_filter()` signature is unchanged — it still receives a flat `ic_codes` vector.
`build_ic_fullcodes()` is a new helper that prepares that vector from the registry.

---

## Files Modified

| File | Change |
|------|--------|
| `global.R` | Add `interco_v2 = "interco_v2.rds"` to `S3_KEYS` |
| `R/persistence.R` | Add `load_interco_v2()`, `save_interco_v2()` |
| `R/data_pipeline.R` | Add `build_ic_fullcodes(registry, ledger)` helper |
| `R/ledger_module.R` | Use `shared$interco_v2()` + `build_ic_fullcodes()` at both call sites |
| `R/settings_module.R` | Replace IC textarea UI with accordion; new save handler |
| `app.R` | Rename `interco_cfg` → `interco_v2`; update load; add IC badge to `me_prov_suggestions` |
| `seed_interco_v2.R` | One-time migration: write discovered codes to S3 |

---

## Implementation Steps

### Step 1 — Schema & Persistence (`global.R`, `persistence.R`)
- Add S3 key
- Add `load_interco_v2()` (returns empty registry if key absent)
- Add `save_interco_v2(registry)`

### Step 2 — Filter Helper (`data_pipeline.R`)
- Add `build_ic_fullcodes(registry, ledger)` with backward compat for old format

### Step 3 — Ledger Module (`ledger_module.R`)
- Replace 2 occurrences of old interco code-list lookup with new helper calls

### Step 4 — Settings UI (`settings_module.R`)
- Replace `observeEvent(input$stg_btn_interco, ...)` body with accordion UI
- Replace `observeEvent(input$save_interco, ...)` with new `save_interco_v2` handler

### Step 5 — App Wiring (`app.R`)
- Rename `interco_cfg` reactiveVal to `interco_v2` with new empty default
- Update `shared` list reference
- Update load observer
- Update `.cache_set` call
- Update `me_prov_suggestions` renderUI with IC badge logic

### Step 6 — Migration Seed (`seed_interco_v2.R`)
- Build pre-populated registry from discovered codes
- Write to S3 so admin starts with all known codes pre-filled

---

## SaaS Scalability

| Concern | Handled by |
|---------|-----------|
| Non-standard SAP prefix (not C/P) | `ar_prefix`/`ap_prefix` fields, configurable per group |
| New empresa added to group | Auto-appears in Settings accordion via `COMPANY_MAP` iteration |
| Groups with > 5 companies | Registry is a named list — no size limit |
| External IC entities (holding, treasury) | Add `tipo = "externo"` field per company entry in future — filter logic unchanged |
| Per-client isolation | All S3 writes use `CLIENT_ID`-scoped paths — no cross-client data |
| Future SAP BP access grant | If RFC becomes available, it can replace/augment code matching without schema changes |

---

## Migration Plan

1. **Run `seed_interco_v2.R`** — writes pre-populated `interco_v2.rds` to S3 with all
   discovered codes. Admin can verify and supplement missing codes in Settings.

2. **Old `interco_settings.rds`** remains in S3 untouched. The old `S3_KEYS$interco`
   key and `load_interco()` / `save_interco()` functions are kept in `persistence.R`
   for reference but are no longer called by the app.

3. **No data loss** — the old flat code lists had no real data configured (the feature
   was unused). The new registry is fully pre-seeded from live SAP invoice data.
