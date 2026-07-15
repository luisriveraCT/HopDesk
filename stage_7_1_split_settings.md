# Antiguedad_App — Stage 7.1: Split `settings_module.R`

**Audience:** Claude Code (fresh session)
**Project:** Hopdesk (Antiguedad_App, R/Shiny)
**Prerequisite:** Stages 0–6.1 complete and stable; app working as intended.
**Scope:** Split `R/settings_module.R` (4,442 lines, 12+ concerns) into a directory of focused sub-modules. **Zero behavior changes.** This is purely structural — the same app, organized so future sessions load only what's relevant.

---

## 0. Operating instructions

1. **No behavior changes. None.** Every screen, button, observer, and reactive must work identically after the split. If you see a bug while doing this work, write it down for a follow-up; do not fix it in the same pass.
2. **No "while we're here" refactors.** No renaming, no logic improvements, no comment cleanup beyond what's strictly necessary to move code between files. Variables that were `<<-` global stay `<<-` global; helper functions that were `.private` stay `.private`. The goal is identical-app-different-layout.
3. **Preserve the public API exactly.** Six function names are called from `app.R`. They keep their names, signatures, and observable behavior. Internal helpers can move freely.
4. **Source order matters.** Update `R/global.R` to source the new files in dependency order. Helpers before consumers. The original `source("R/settings_module.R")` line gets replaced by a block of `source()` calls for the new files.
5. **Test gate is regression + smoke.** Run the existing test suite (must remain 100% green). Then open every Settings panel manually and click around for ~10 seconds per panel to verify nothing visibly broke.
6. **One session, one file split.** Do not interleave with any other work in this session.

---

## 1. Public API to preserve

The following functions are called from `app.R` and must retain their names and signatures:

| Function                          | Lives in (after split)                  |
|-----------------------------------|------------------------------------------|
| `show_settings_modal(...)`        | `R/settings/settings_hub.R`              |
| `settings_observers(...)`         | `R/settings/settings_hub.R`              |
| `settings_sincro_observer(...)`   | `R/settings/settings_sincro.R`           |
| `settings_policies_observer(...)` | `R/settings/settings_policies.R`         |
| `settings_companies_observer(...)`| `R/settings/settings_companies.R`        |
| `settings_cuentas_observer(...)`  | `R/settings/settings_cuentas.R`          |

Any other function that turns out to be called from outside `settings_module.R` during the split (find via `grep` before moving) follows the same rule: it keeps its name, and its new file is registered in `global.R` accordingly.

---

## 2. Target file structure

Create directory `R/settings/`. Move content from `R/settings_module.R` into the following files. Line ranges are based on the original file's section dividers.

### 2.1 `R/settings/settings_helpers.R`  (~250 lines)
**Source:** L1–258 of the original.
**Contains:** File header comment; `get_default_origin_account_12()`, `get_default_origin_account()`, and any other small public helpers that don't belong to a specific panel.

### 2.2 `R/settings/settings_hub.R`  (~880 lines)
**Source:** L259–1133 of the original.
**Contains:** `show_settings_modal()` (L259–325) and the main `settings_observers()` (L326–1133).
**Note:** `settings_observers` is the orchestration function called from `app.R`. It may *call* helpers and observers in the other sub-modules — that's fine, they all get sourced before this file.

### 2.3 `R/settings/settings_bp.R`  (~95 lines)
**Source:** L1134–1228 of the original (Business Partners + BP datatable builder).
**Contains:** `get_bp_table()` and related BP-specific helpers.

### 2.4 `R/settings/settings_proveedores.R`  (~278 lines)
**Source:** L1229–1506 of the original (Catálogo de Proveedores).
**Note:** This is the proveedores catalog UI helpers used by the Settings panel — NOT the full proveedores module (which lives in `R/proveedores_module.R`).

### 2.5 `R/settings/settings_cuentas.R`  (~513 lines)
**Source:** L1507–2019 of the original (Cuentas de Empresa UI + observer).
**Contains:** Both the UI helpers (L1507–1632) and `settings_cuentas_observer` (L1633–2019).

### 2.6 `R/settings/settings_sincro.R`  (~297 lines)
**Source:** L2020–2316 of the original.
**Contains:** Sincronización panel UI (L2020–2130) + `settings_sincro_observer` (L2131–2316).

### 2.7 `R/settings/settings_policies.R`  (~1,302 lines)
**Source:** L2317–3157 + L3255–3618 of the original (Políticas de Pago).
**Contains:**
- Policies UI helpers (L2317–2784)
- Interactive dual-month calendar preview (L2785–3008)
- Revert policies modal (L3158–3254)
- `settings_policies_observer` (L3255–3618)

**Note:** L3009–3157 (SOCIOS COMERCIALES UI helpers) is the start of the companies section and belongs in `settings_companies.R`, NOT here. The line range above carefully excludes it.

If `settings_policies.R` ends up too large (>1,000 lines), it's acceptable to split it further into `settings_policies_ui.R` (helpers + preview + modal) and `settings_policies_observer.R` (just the observer). Use judgment based on how cleanly the file reads after the move.

### 2.8 `R/settings/settings_companies.R`  (~973 lines)
**Source:** L3009–3157 + L3619–4442 of the original.
**Contains:**
- Socios Comerciales UI helpers (L3009–3157)
- `settings_companies_observer` (L3619–4442)

Same flexibility: if it ends up too large, split into `_ui.R` and `_observer.R`.

---

## 3. Mechanical process

Follow this order to minimize risk:

### Step 1 — Create the directory
`mkdir R/settings/`

### Step 2 — Move sections one at a time, smallest to largest
Suggested order: `settings_helpers.R` (small, leaf) → `settings_bp.R` → `settings_sincro.R` → `settings_proveedores.R` → `settings_cuentas.R` → `settings_companies.R` → `settings_policies.R` → `settings_hub.R` (largest, depends on everything else).

For each move:
1. Create the new file with a header comment matching the existing house style (see example below).
2. Copy the exact line range from the original. **No edits to the moved code.**
3. Remove those lines from the original.
4. Verify the original file's section dividers above and below the moved section are still consistent (no orphaned `# ===` blocks).

### Step 3 — Update `R/global.R`
Replace the line:
```r
source("R/settings_module.R")
```
With:
```r
# Settings — split across R/settings/ for session efficiency.
# Sourced in dependency order: helpers → panel UI/observers → hub orchestrator.
source("R/settings/settings_helpers.R")
source("R/settings/settings_bp.R")
source("R/settings/settings_proveedores.R")
source("R/settings/settings_cuentas.R")
source("R/settings/settings_sincro.R")
source("R/settings/settings_policies.R")
source("R/settings/settings_companies.R")
source("R/settings/settings_hub.R")
```

### Step 4 — Delete the original
Once all content has been moved and the app loads, `rm R/settings_module.R`.

### Step 5 — Verify by grep
```bash
# Confirm no references remain to the old file path:
grep -rn "settings_module\.R" R/ app.R global.R Rprofile 2>/dev/null
# Should return zero results (other than possibly historical comments in spec files).
```

### Step 6 — File header convention
Each new file starts with:
```r
# =============================================================================
# R/settings/<filename>.R
# <one-line description of the file's concern>
# =============================================================================
# (Originally part of R/settings_module.R; split in Stage 7.1 for session efficiency.)
```

---

## 4. Risk areas to watch

### 4.1 Cross-section references
Some functions in one section may call helpers from another section. Use `grep` before declaring a move complete:

```bash
# After moving section X to its new file, check whether anything in the OLD
# file still references functions defined in X:
grep -n "<function_name_from_X>" R/settings_module.R
```

If hits remain in the old file but the function definition has moved, the source order in `global.R` must place X's new file BEFORE whatever still needs it. The proposed order above already handles this — helpers first, hub last — but verify per move.

### 4.2 `<<-` global assignments
The original file may use `<<-` to assign into the calling environment. These work the same way regardless of which file the assignment lives in, but only if the caller is in the same session. Skim each moved section for `<<-` and verify the assignment target is still accessible.

### 4.3 Observers registered against `input$...`
Observers tied to `input$...` events keep working regardless of file location, but the *observer must be created during a session* (i.e., called inside `server()` or a sub-module called from server). If any observer was incorrectly created at file-load time in the original (rare but possible), the split will expose it. If you find one, write it down — don't fix it.

### 4.4 Function call chains spanning sections
Example: `settings_observers` (in `settings_hub.R` post-split) likely calls helpers defined in `settings_helpers.R`. As long as `settings_helpers.R` is sourced first (which it is, per Step 3), this works.

If you find a cycle (file A calls into file B which calls back into A), report it before resolving — it may be a real architectural issue worth a follow-up.

---

## 5. Tests

### 5.1 Automated regression
After the split is complete and `R/settings_module.R` is deleted:

```r
# From the project root:
source("R/global.R")  # must complete without errors
```

Then run the existing test suites that touch settings or general app behavior:
- Any `tests/test_*.R` file that exists.
- Specifically verify Pasivos tests (`tests/test_pasivos_*.R`) still pass, since they may indirectly depend on settings observers being available.

**All previously-passing tests must remain green.**

### 5.2 Manual smoke checklist

Open the app and verify each Settings sub-panel:

1. **Open Settings modal** — opens, renders without console errors.
2. **Business Partners panel** — table loads, can search, can match.
3. **Catálogo de Proveedores panel** — table loads, can add/edit a proveedor.
4. **Cuentas de Empresa panel** — table loads, can add/edit a bank account.
5. **Sincronización panel** — settings render, toggle works.
6. **Políticas de Pago panel** — table loads, can add a policy, calendar preview renders.
7. **Socios Comerciales (Companies) panel** — table loads, can add/edit a company.
8. **Close Settings, reopen** — clean reopen, no stale state.

For each panel, allow ~10 seconds of clicking. The goal is not exhaustive testing — it's catching anything that visibly broke. If a panel works pre-split, it must work identically post-split.

### 5.3 Pasivos sanity
The Pasivos module depends indirectly on `parte_alias_map_db` and `proveedores_db` populated by Settings handlers. Quick check:
- Open Pasivos > Tabla — provisions visible.
- Open the Pasivos wizard — Parte autocomplete works.

If either breaks, a Settings observer wasn't sourced or wasn't called. Diagnose by examining the `global.R` source order.

---

## 6. Stage 7.1 completion criteria

1. `R/settings_module.R` no longer exists.
2. `R/settings/` directory contains 8 files (or up to 10 if larger files were further split).
3. `R/global.R` sources the new files in dependency order.
4. All existing tests pass.
5. Manual smoke checklist runs cleanly through all 8 panel checks.
6. `grep -rn "settings_module\.R" R/ app.R 2>/dev/null` returns zero functional references (spec doc references are fine).
7. The user has personally clicked through Settings and confirmed it looks and behaves identically to before.

---

## 7. What's NOT in 7.1

- `bancos_module.R` split → Stage 7.2.
- `ledger_module.R` split → Stage 7.3.
- `pagar_hoy_module.R` split → Stage 7.4.
- `proveedores_module.R` split → Stage 7.5 (if soak confirms it's needed).
- `search_module.R` split → Stage 7.6 (same caveat).
- Any behavior fix, bug fix, or improvement. Those go in their own stages.
- Renaming functions or restructuring APIs.

---

## 8. Hand-off

When 7.1 is green:
- The user reports back on session-load cost reduction observed when next editing settings code (anecdotal — no formal measurement needed).
- If the split feels good, proceed to 7.2 (`bancos_module.R`).
- If anything feels off about the split, pause and discuss before continuing.

The discipline of one-file-at-a-time means each stage is small, low-risk, and independently revertable if needed.

---

**End of Stage 7.1 specification.**
