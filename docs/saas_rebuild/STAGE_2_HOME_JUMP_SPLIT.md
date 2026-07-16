# Stage 2 — Home/Jump Split, Cross-Session Data Leak, and Internal-Tool Visibility

Read `ARCHITECTURE.md` (§5) and `STAGE_1_TIER_PERMISSION_MODEL.md` first —
this stage assumes Stage 1's `is_staff` / tier registry already landed and
passed its tests (confirmed done, 2026 — Mouse verified Stage 1 in the live
app).

This stage grew to three parts based on what testing Stage 1 surfaced:

1. The originally-planned home/jump split (fixes the remaining half of the
   tesoreria-class bug and the non-uniform module loading).
2. **A newly-found, higher-severity issue**: a process-wide cache can leak
   one client's real SAP invoice data into another session — including a
   Hopdesk staff session that never jumped anywhere. This is what Mouse
   observed as "Hopdesk staff waiting on Networks to load."
3. Hiding Hopdesk-internal tools from client sessions entirely (not just
   locking their content) — found while Mouse tested Stage 1 as a `dev`
   user and saw tab labels for staff-only features he should never see the
   existence of.

Do all three in this stage — they're independent of each other but all
touch the same "what does a client vs. staff session actually get" surface,
so reviewing them together is more efficient than three separate stages.

---

## Part A — Home vs. Jump split

### The problem

Today, one variable — `active_client_id` — is used both for "which client's
folder does this session's data belong to" (true for every user, staff and
client alike) and "which client has a staff member temporarily jumped into"
(true only for a `hopdesk` session mid-jump). Because these are conflated:

- The hop-grant watchdog (`tiers_module.R` ~2577-2640) polls every 30s and
  force-resets `active_client_id` to `"hd-admin"` for any non-principal
  session whose `active_client_id != "hd-admin"` and who lacks a matching
  `hop_grants` row — which describes every ordinary client user sitting in
  their own home folder, not just an expired staff jump.
- Only the calendar/SAP-snapshot loading path was ever wired to react to
  `active_client_id` changing; every other module (Vencidos, Agenda de Hoy,
  Bancos, Intercompany, Pasivos, Forecasting, Reporte) loads once at session
  start and never reconsiders which client it should be reading from. This
  is why non-calendar tabs render empty regardless of which client-routing
  bug is or isn't currently active.

### The fix

Introduce two genuinely separate reactiveVals in `app.R`, replacing the
single overloaded `active_client_id`:

- `home_client_id` — set exactly once at login from the user's own record
  (`current_user_info()$client_id`), never changed for the session's
  lifetime. This is `"hd-admin"` for staff, `"networks"` etc. for a client
  user.
- `jump_client_id` — `NULL` by default; only ever set by the staff
  context-switcher (`tiers_module.R`'s `btn_switch_ctx` flow), only ever
  settable by a session where `current_user_info()$is_staff` is `TRUE`
  (Stage 1 already made this the single source of truth), and cleared back
  to `NULL` on logout, on revocation, or when staff explicitly switches back
  to their home context.

Add one derived reactive, `effective_client_id <- reactive({ jump_client_id() %||% home_client_id() })`. **Every** module's data-loading reactive
(`sap_data`, `moves_db`, `bancos_movimientos_db`, `proveedores_db`,
`pasivos_*_db`, `forecasting_*_db`, everything currently keyed off
`active_client_id()` or the env var) must be rewritten to depend on
`effective_client_id()` and reload when it changes — replacing today's
"only the calendar path reloads" pattern with one mechanism every module
subscribes to identically. Concretely: extract the Phase-2 aux-data-loading
block (`app.R` ~1560-1800, currently fires once per session) into a
reloadable function keyed by `effective_client_id()`, invoked once at
startup and again every time `effective_client_id()` changes.

Rewrite the hop-grant watchdog to key off `jump_client_id()` instead of
`active_client_id()` — it should never fire at all when `jump_client_id()`
is `NULL` (a session sitting in its own home context, staff or client, is
never subject to a "grant expired" check, because there's no grant to
expire). This is what makes the tesoreria-class bug and this watchdog's
false-positive structurally impossible rather than merely guarded against.

Update every consumer that reads `active_client_id()` today (search the
whole codebase, not just `app.R` — this variable is referenced across
several modules) to read `effective_client_id()` instead, except the staff
context-switcher itself, which writes to `jump_client_id()` specifically.

---

## Part B — 🔴 Cross-session SAP data cache can leak one client's data into another session

### Confirmed mechanism (verified by reading the code, not guessed)

`app.R` has a **process-wide, client-blind** cache:

```r
.GlobalEnv$.sap_global_cache <- list(AR = ar, AP = ap, fetched_at = Sys.time())
```

written at `app.R:1495-1499` after any session's live SAP fetch completes
(`load_sap_data()`, called from `.dispatch_sap()`), and read back at
`app.R:2024-2043` (`.dispatch_sap()`): if this cache is less than 5 minutes
old and has any AR/AP rows at all, **any session** — regardless of which
client it belongs to — seeds its own `sap_data()` from it and skips doing
its own properly client-scoped fetch entirely:

```r
if (age < 300 && (!is.null(cache$AR) || !is.null(cache$AP))) {
  message("[SAP] Cross-session cache hit...")
  sap_data(list(AR = cache$AR, AP = cache$AP))   # <-- not checked against this session's own client!
  ...
  return()
}
```

Concretely: a Networks client user's session correctly fetches Networks'
real invoice data (properly scoped via `company_map_rv()`), and caches it
process-wide. If a Hopdesk staff member logs into their own home context
(no jump) within the next 5 minutes, `.dispatch_sap()` will seed their
`sap_data()` with **Networks' real invoice data** instead of the empty
result their own (correctly-scoped-to-nothing) fetch would have produced.
The reverse also happens: if staff logs in first and caches an empty
result, a Networks user logging in shortly after can get **served an empty
calendar** from that stale staff-session cache instead of their own real
data. This is very likely also an unrecorded contributor to earlier "why is
my calendar empty" reports, not just the routing bug already fixed in
Stage 1.

This is a real, live tenant-isolation leak — not merely the "Hopdesk staff
had to wait" perf annoyance Mouse described (that's the user-visible
symptom of the *same* bug: the fix that stops staff "waiting on Networks"
is the same fix that stops staff *receiving Networks' data outright*).

### The fix

Replace the single `.GlobalEnv$.sap_global_cache` blob with a cache **keyed
by `effective_client_id()`** — e.g. `.GlobalEnv$.sap_global_cache[[cid]]`,
a named list of per-client cache entries, each with its own `AR`, `AP`, and
`fetched_at`. `.dispatch_sap()` must look up (and write to) only the entry
for `isolate(effective_client_id())` of the session calling it — never any
other key. A staff-at-home session (`effective_client_id() == "hd-admin"`)
must have — and be limited to — its own cache entry, which will correctly
and permanently stay empty (since `hd-admin`'s folder has no invoice data
and no companies to fetch), so it never again borrows or waits on any other
client's fetch.

Audit `R/sap_api.R` and anywhere else `Sys.getenv("CLIENT_ID")` is read
directly for a data decision (rather than `effective_client_id()`) — the env
var is only ever correct for "which deployment am I," never for "which
client's data should this session load." Flag every such site found; don't
assume the list above is exhaustive.

---

## Part C — Hide Hopdesk-internal tools from client sessions entirely

### The problem

Testing Stage 1 as a `dev`-tier Networks user surfaced this: the
Usuarios/Grupo module's sub-tab bar (`tiers_module.R` — tabs Usuarios,
Actividad, Seguridad, Clientes, Invitaciones, Accesos de Salto,
Notificaciones, Bitácora Global, Config. de Tiers) renders **every tab's
label** to every viewer, and only locks the *content* behind a permission
message for tabs the viewer isn't allowed to use ("Requiere permiso
can_view_global_audit", "Solo disponible para cuentas Hopdesk y
Principal"). A client should never see that these tools exist at all —
their names alone expose the existence of Hopdesk's internal operations
(other clients' registry, cross-client jump grants, staff notifications,
the cross-client audit log) to a business user who has no reason to know
Hopdesk manages other clients this way.

### The fix

Classify each sub-tab and hide (not lock-message) the internal-only ones
entirely from any session where `current_user_info()$is_staff` is `FALSE`
— don't render the `nav_panel` at all, the same principle Stage 1 applied
to tiers/permissions, now applied to this module's own navigation:

| Tab | Client-visible? | Why |
|---|---|---|
| Usuarios | Yes | Managing their own client's users is core self-service |
| Actividad | Yes | Their own client's activity log (Stage 4 scope) |
| Seguridad | **No — hide** | Code comment marks it "(principal only)" — emergency account lock, writes to `hd-admin/emergency_lock.rds` |
| Clientes | **No — hide** | The cross-client registry — Hopdesk's own client-management tool |
| Invitaciones | Yes, tentatively — **confirm with Mouse first** | A client inviting their own team fits the self-service goal, but `dev` tier's permission default (`can_manage_invites = FALSE` in `auth.R`) currently blocks it anyway — decide whether to flip that default as part of this stage, or leave clients unable to invite via this tab for now |
| Accesos de Salto | **No — hide** | Hopdesk staff's jump-grant management — ties directly to the Stage 1 jump-security fix |
| Notificaciones | **No — hide** | Already self-labeled "Solo disponible para cuentas Hopdesk y Principal" |
| Bitácora Global | **No — hide** | Cross-client audit log — a client's own log is the "Actividad" tab instead (Stage 4) |
| Config. de Tiers | Yes | A client's own tier/permission defaults — legitimately `dev`'s job per Stage 1 |

Implement by building the tab list conditionally (filtering out the "hide"
rows when `!current_user_info()$is_staff`) rather than rendering all tabs
and relying on the per-tab lock message — the lock messages can stay as a
defense-in-depth fallback, but the primary mechanism should be "the tab
isn't there."

**Stop and ask Mouse** about the Invitaciones row above before implementing
it either way — it's a real product decision (should a client's own
dev/admin be able to invite their own teammates without asking Hopdesk),
not something to infer silently.

---

## Explicitly out of scope for this stage

- Anything from Stage 3 (ERP credentials) or Stage 4 (audit log filter UI) —
  Part A's `Actividad` tab staying client-visible does not mean building
  its filter UI now; that's still Stage 4.
- Renaming or restructuring the Usuarios/Grupo module beyond the tab-hiding
  described in Part C.

## Test plan

**Automated:**
- A simulated Hopdesk-staff-at-home session (`is_staff = TRUE`,
  `jump_client_id() == NULL`) never triggers the hop-grant watchdog logic
  at all (assert the watchdog's `observe()` body doesn't reach its
  grant-lookup code for this case).
- A simulated client session's `effective_client_id()` never changes for
  the life of the session regardless of what `jump_client_id()` would be if
  it were staff (it should always be `NULL` for a client session, and
  attempts to set it are rejected — this is really re-testing Stage 1's
  jump fix from a different angle).
- Two simulated concurrent sessions, one `networks`/`finance`, one
  `hd-admin`/`hopdesk` (home, no jump): after the `networks` session
  performs a live SAP fetch, assert the `hd-admin` session's `sap_data()`
  contains zero rows, not the `networks` session's data — this is the
  regression test for Part B and must fail against the current
  (unfixed) code before your change, and pass after.
- Every module previously keyed off `active_client_id()` reloads when
  `effective_client_id()` changes in a test harness — not just the
  calendar/SAP path.
- Tab visibility: a simulated `finance`/`dev`/`admin`/`analysis` session
  never receives the `Seguridad`, `Clientes`, `Accesos de Salto`,
  `Notificaciones`, or `Bitácora Global` nav panels in the rendered UI tree
  at all (assert their absence, not just that clicking them is blocked). A
  `hopdesk`/`principal` session still receives all of them.

**Manual** (same account set as Stage 1 — dev/Networks, tesoreria/Networks,
Bunny/hopdesk, Mouse/principal):
1. Log in as Bunny (hopdesk), confirm home context loads instantly with
   zero data (no Networks or any other client's numbers appear anywhere),
   confirm jumping into `networks` via a valid grant still works exactly as
   before, confirm the 30-minute grace/revocation behavior for an actual
   jump still works.
2. With a Networks session active in one browser and a fresh Bunny login in
   another within the same few minutes, confirm Bunny's home calendar stays
   empty — this is the direct manual reproduction of the Part B leak.
3. Log in as the `dev` Networks account, confirm the Grupo tab bar shows
   only Usuarios / Actividad / Config. de Tiers (and Invitaciones, pending
   Mouse's answer) — no Seguridad/Clientes/Accesos de Salto/
   Notificaciones/Bitácora Global tabs visible at all.
4. Confirm every other financial tab (Vencidos, Agenda de Hoy, Bancos,
   Intercompany, Pasivos, Forecasting, Reporte) loads real data for a client
   session immediately, not just the calendar.

## Prompt for the implementing Claude Code session

```
Read C:\Users\luisr\Antiguedad_App\docs\saas_rebuild\ARCHITECTURE.md,
docs\saas_rebuild\STAGE_1_TIER_PERMISSION_MODEL.md, and
docs\saas_rebuild\STAGE_2_HOME_JUMP_SPLIT.md in full before writing any
code. Confirm Stage 1 is already merged into the base you're branching
from — if it isn't, stop and ask before proceeding.

Work on a new branch off the branch Stage 1 was merged into, named
stage-2-home-jump-split.

Implement all three parts of Stage 2:
- Part A: split active_client_id into home_client_id / jump_client_id /
  effective_client_id, rewire every module's data loading to key off
  effective_client_id() with genuine reload-on-change (not just the
  calendar path), and rewrite the hop-grant watchdog to key off
  jump_client_id() so it never fires for a home-context session.
- Part B: fix the cross-session .sap_global_cache leak described in this
  document — key the cache by client id, verify with the two-concurrent-
  session test described in the Test Plan that a Networks fetch can never
  seed a Hopdesk-staff-at-home session's data, and audit for any other
  place Sys.getenv("CLIENT_ID") is used for a data decision instead of
  effective_client_id().
- Part C: hide (don't just lock-message) the Seguridad, Clientes, Accesos
  de Salto, Notificaciones, and Bitácora Global tabs entirely from any
  non-staff session, per the table in this document. Before touching the
  Invitaciones tab's visibility or its can_manage_invites default, stop and
  ask Mouse the specific question this document raises about it — do not
  decide it yourself either way.

Work in small, reviewable increments — Part B's cache-leak fix is the
highest priority within this stage given its severity; do it first and get
it independently tested before moving to Parts A and C. After each logical
change, run the existing test suite plus the new tests this document's
Test Plan describes, and report results before moving on.

When done, stop and report back what changed file by file, the answer you
need from Mouse on Invitaciones, and the manual test plan from this
document for him to run himself before merging. Do not merge to master
yourself.
```
