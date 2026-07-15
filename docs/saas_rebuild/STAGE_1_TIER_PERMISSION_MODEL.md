# Stage 1 — Tier & Permission Model

Read `ARCHITECTURE.md` in this folder first — this stage implements its §4.

## Why this stage is first

Fixes the tesoreria routing bug at its root (one source of truth for
staff-vs-client, instead of four independent guesses), and closes a live
**cross-tenant security hole** found during the audit — see below. Both are
symptoms of the same underlying defect: the `dev` tier used to mean "Luis
Rivera, full access to everything" back when this app had one client. It now
means "a client's own IT department" (confirmed: `networks/usuarios.rds` has
a real `dev`-tier account, `hd-admin/usuarios.rds` has none). The code was
never fully updated to reflect that change in meaning — in 6 places, `dev` is
still treated as staff-equivalent for actions that must never cross a
client boundary.

## 🔴 Found during audit: `dev`-tier client users can jump into other clients today

In `R/tiers_module.R`, the "context switcher" (jump mechanism) and the
"Accesos de Salto" panel treat `is_dev()` as equivalent to `is_hopdesk()` /
`is_principal()` in exactly the places that let someone actually change
`active_client_id` to another client's folder or see the jump-request UI:

| Line | What it gates | Bug |
|---|---|---|
| `tiers_module.R:480` | `req()` to open the "Cambiar cliente" (switch context) modal | Any `dev`-tier user can open it |
| `tiers_module.R:487` | Which clients populate that modal's dropdown | For `dev`, shows **every active client in the registry**, unrestricted — same as principal |
| `tiers_module.R:544` | `req()` to actually confirm the switch | Any `dev`-tier user can execute it |
| `tiers_module.R:549` | Whether a hop-grant is required before switching | `dev` is exempted from needing a grant at all — jumps freely, like principal |
| `tiers_module.R:2675` | Visibility of the "Accesos de Salto" panel | `dev` can see the staff jump-grant management UI |
| `tiers_module.R:3016` | Submitting a request for a jump grant | `dev` can request grants meant for Hopdesk staff |

**Concretely: today, Networks Group's own `dev` account (or any future
client's `dev` account) can click "Cambiar cliente" and switch straight into
the `hopdesk` test client's folder — or any other client registered in
`hd-admin/client_registry.rds` — with no grant, no staff involvement, no
record that it's unusual.** This is exactly the vacuum boundary Mouse
described as non-negotiable, and it does not currently hold.

This is not a hypothetical — it's live in the deployed code. **Confirmed
and approved for fix by Mouse (2026-07-15):** "the dev tier is exactly as
you say, a tier designed for CLIENTS, not our Hopdesk staff. dev tier is
used for Clients to have their own IT people involved with
development-and-support-related tools that the Finance people do not need
nor know how to use." This is the top priority within this stage — fix it
first, before the other Stage 1 items.

**Before editing, re-verify the line numbers below with a fresh
`grep -n "is_dev()" R/tiers_module.R`** — this table was built against a
specific point-in-time read of the file; if the file has changed since,
match by the surrounding context described in each row (button/modal name,
panel name), not blindly by line number.

**Six other `is_dev()` sites are correct and must stay as-is** — they gate a
client's own tier-configuration UI (`tiers_module.R:578,603,3553,3702`),
same-client user-table visibility rules (`:646`), and tier-assignment
validation when creating/editing a user within one's own client (`:711,753,
1006`). None of these cross a client boundary — `dev` legitimately has
elevated access *within their own client*, which is correct per Mouse's
description of the tier.

**"Must stay" means the *behavior*, not the literal `is_dev()` token.**
Steps 5 and 6 below deliberately ask you to rewrite the dropdown and
tier-assignment logic at those same three lines (711, 753, 1006) using
`tier_rank()` / `is_staff_tier()` instead of hardcoded tier-name chains —
that rewrite is intentional and correct even though the literal `is_dev()`
call disappears from those lines as a result. The constraint is: after the
rewrite, a `dev`-tier session must still be able to assign
`dev`/`admin`/`finance`/`analysis` to a user in its own client, and must
still be unable to assign `hopdesk`/`principal` — verify this with the Test
Plan's automated tests, not by grepping whether the token `is_dev()` still
appears at that line. The six jump-related sites are a different, hard
constraint: there, `dev` must lose the *behavior* itself, under any
rewrite, not just have its check reworded.

## Design

### 1. New file: `R/tier_registry.R`

A single list, one entry per tier, sourced early in `global.R` (before
`R/auth.R`, since `auth.R` will reference it) :

```
TIER_REGISTRY structure per tier:
  label          — display string
  rank           — integer, higher = more powerful; used for hierarchy
                   comparisons instead of hardcoded tier-name lists
  internal_only  — TRUE for Hopdesk staff tiers, FALSE for client tiers.
                   This is the ONLY place that decides this, ever.
```

Populate with the six current tiers per `ARCHITECTURE.md` §4.1's table.
Add two helper functions here, used everywhere downstream:
- `is_staff_tier(tier)` → `TIER_REGISTRY[[tier]]$internal_only`
- `tier_rank(tier)` → `TIER_REGISTRY[[tier]]$rank`

Do **not** move `auth_resolve_perms()`'s existing per-tier permission
defaults into this file in this stage — keep that function as-is to limit
this stage's surface area. `tier_registry.R` is metadata only (label, rank,
internal_only); permission *defaults* stay where they are for now.

### 2. `current_user_info()` gets a new field: `is_staff`

In `app.R`, wherever `current_user_info()` resolves `tier_val` (currently
around the reactive that builds the list returned to the rest of the app),
add: `is_staff = is_staff_tier(tier_val %||% "finance")`. Every downstream
consumer reads `current_user_info()$is_staff` — nothing re-derives it from a
tier-name list again.

### 3. Fix the landing-tab race (replaces the `once=TRUE` pattern)

Current defect: the static UI (`bslib::page_navbar(..., selected = if
(IS_ADMIN_DEPLOYMENT) "TIERS" else "CAL", ...)`) is built once for the whole
process, so **every** session on the shared deployment starts on the same
tab — today that means every session, staff and client alike, starts on
"TIERS". A `once = TRUE` observer is supposed to redirect non-staff sessions
away, but `once` combined with `req()` has ambiguous timing (if `req()`
aborts the handler early because tier isn't resolved yet, it is not
guaranteed the observer gets another chance to fire) — this is the likely
mechanism behind the tesoreria incident.

Fix:
- Change the static default to always `"CAL"` — the correct landing tab for
  the majority of sessions (every client tier). No client session needs a
  redirect at all anymore; they simply land correctly by default.
- For staff sessions only, add a **deterministic manual-guard redirect** —
  not `observeEvent(..., once = TRUE)`. Use an explicit local
  `reactiveVal(FALSE)` flag (e.g. `redirected_to_staff_home`) that is
  checked and set manually inside a plain `observe()`:
  ```
  observe({
    info <- current_user_info()
    if (isTRUE(redirected_to_staff_home())) return()   # already handled
    if (is.null(info) || info$user == "unknown") return()  # not resolved yet
    redirected_to_staff_home(TRUE)                      # mark BEFORE acting
    if (isTRUE(info$is_staff)) updateNavbarPage(session, "nav", selected = "GRUPO")
  })
  ```
  This fires exactly once, deterministically, only after `info$user` is
  genuinely resolved (not the `"unknown"` placeholder), regardless of how
  many times the reactive re-evaluates before that point. Client sessions
  do nothing (they're already on `CAL`); staff sessions get moved to
  `GRUPO`.

### 4. Tab-visibility observer (`app.R` ~1277-1306)

Keep the CSS+`runjs` override mechanism (`shinyjs::show/hide` can't beat the
`!important` stylesheet rule, as the existing comment correctly notes). Just
replace `is_staff <- nzchar(tier) && tier %in% c("hopdesk","principal")` with
`is_staff <- isTRUE(current_user_info()$is_staff)`. Leave the
`in_client_ctx` comparison against `env_cid` alone for now — Stage 2 replaces
that comparison entirely when the home/jump split lands; don't do that work
twice.

### 5. Usuarios/Grupo "add user" tier dropdown (`tiers_module.R` ~704-714)

Currently hardcodes which tiers each viewer tier may offer. Replace the
hardcoded per-viewer-tier `choices` lists with: filter the full tier list to
`internal_only == FALSE` unless the viewer session itself `is_staff`, in
which case also offer the `internal_only == TRUE` tiers appropriate to the
viewer's own rank (principal can offer hopdesk; hopdesk cannot offer
principal). This is cosmetic (§below covers the server-side enforcement) but
keeps the dropdown correct as new tiers get added later without another code
change.

### 6. Server-side tier-assignment enforcement (`tiers_module.R` ~744-754, ~1004-1007)

Keep the existing "assigning tier X requires being tier X-or-higher"
guard — it's the right idea — but rewrite it generically using
`tier_rank()` and `is_staff_tier()` from the registry instead of the current
hardcoded `identical(tier, "dev")` / `gsub("^(dev|hopdesk|principal)$", ...)`
chains, so a tier added in the future is covered automatically. Explicitly
verify: a non-staff session can never save a user with `internal_only ==
TRUE`, no matter what the client sent in the request — this must be checked
here, not only in the dropdown.

### 7. Fix the six jump-related `is_dev()` sites

Remove `is_dev()` from exactly the six lines listed in the red flag above.
Replace each with `isTRUE(current_user_info()$is_staff)` (which today means
`is_hopdesk() || is_principal()`, but stays correct automatically if a
future internal-only tier is added). Leave every other `is_dev()` site
untouched (see the "correct and must stay" list above) — verify against
that list before touching any given line, don't do a mechanical find/replace
across the whole file.

### 8. Hop-grant watchdog exemption (`tiers_module.R:2579`)

Once step 7 lands, `dev` can never have `active_client_id() != home` in the
first place, so its exemption here becomes unreachable dead logic. Leave it
in place for this stage (removing it is Stage 2's job, when the watchdog
itself is replaced by the home/jump split) — just don't let its presence
block step 7's fix.

## Explicitly out of scope for this stage (handled in Stage 2)

- The hop-grant watchdog's false-positive on client users at home.
- Making every module (not just calendar) reload uniformly on a context
  change.
- Any change to `active_client_id` itself, or the `env_cid` comparison in
  the tab-visibility observer.

Do not be tempted to fix these here — Stage 2 needs the home/jump split in
place first, and mixing the two stages' changes will make review harder.

## Test plan

**Automated** — extend `tests/test_saas_perms.R`:
- `TIER_REGISTRY` has exactly `{principal, hopdesk}` with `internal_only ==
  TRUE`, and `{dev, admin, finance, analysis}` with `internal_only == FALSE`.
- A simulated `dev`-tier session cannot open the context-switcher modal, and
  a direct call to the confirm-switch handler with a `dev` session and a
  foreign `client_id` is rejected.
- A simulated `finance`-tier session's `current_user_info()$is_staff` is
  `FALSE`; a `hopdesk`-tier session's is `TRUE`.
- Attempting to save a new user with `tier = "hopdesk"` from a `finance` or
  `dev` session is rejected server-side (not just hidden from the dropdown).

**Manual** (against a non-prod copy of the data if at all possible — ask
Mouse before touching real client folders):
1. Log in as a `dev`-tier Networks account. Confirm: no "Cambiar cliente"
   button, no "Accesos de Salto" section, cannot submit a hop-grant request.
   Confirm the tier config lock/edit UI for Networks' own settings still
   works (this must NOT regress).
2. Log in as tesoreria (`finance`, Networks). Confirm: lands on `CAL`
   immediately, no `GRUPO`/`TIERS` tab visible at all, every financial tab
   is visible.
3. Log in as Bunny (`hopdesk`). Confirm: lands on `GRUPO`, financial tabs
   hidden until a jump, context-switcher still works, still requires a
   valid grant for any client other than `hd-admin`.
4. Log in as Mouse (`principal`). Confirm: everything still works exactly as
   before — this stage must not regress the top tier.

## Prompt for the implementing Claude Code session

```
Read C:\Users\luisr\Antiguedad_App\docs\saas_rebuild\ARCHITECTURE.md and
docs\saas_rebuild\STAGE_1_TIER_PERMISSION_MODEL.md in full before writing
any code.

Work on a new branch off saas-checkpoint-2026-07-15, named
stage-1-tier-permission-model.

Implement Stage 1 exactly as specified in that document: the new
R/tier_registry.R file, the current_user_info() is_staff field, the
landing-tab race fix (manual-guard pattern, not once=TRUE), the
tab-visibility observer update, the tier dropdown fix, the server-side
tier-assignment enforcement, and — most importantly — remove is_dev() from
exactly the six jump-related call sites listed in the "🔴 Found during
audit" section, and only those six. Do not touch any of the is_dev() sites
listed as "correct and must stay."

Do not touch anything listed under "Explicitly out of scope for this
stage" — that belongs to Stage 2, which does not exist yet.

Work in small, reviewable increments. After each logical change, run the
existing test suite (tests/_run_saas.R and related test_saas_*.R files) and
report results before moving to the next change. Write the new automated
tests described in this document's Test Plan section into
tests/test_saas_perms.R.

When the automated tests pass, stop and report back what changed, file by
file, plus the manual test plan from this document for Mouse to run
himself before this branch is merged. Do not merge to master yourself.
```
