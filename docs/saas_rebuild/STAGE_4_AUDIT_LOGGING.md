# Stage 4 — Audit Log Visibility, Filters, and Railguards

Read `ARCHITECTURE.md` §6 first. Stages 1, 2, 3, and 3b are merged into
`saas-checkpoint-2026-07-15` and confirmed passing (267/267). Branch off
that branch.

## What already exists — build on it, don't rebuild it

`R/app_audit.R`'s `log_action()`/`read_audit_log()` are correct and stay
as-is: every action writes to the client folder it affects, and staff
acting inside a jumped client dual-writes to that client's log AND
Hopdesk's own home log, tagged with `home_client` metadata. This stage is
entirely about who gets to *see* what through the UI, and the filters
available once they do — not the write path.

## Two real gaps found while reading the current code (not hypothetical)

1. **The "Actividad" tab can leak Hopdesk's own staff log to any
   `hopdesk`-tier user.** `tiers_module.R`'s `activity_tbl` reads
   `read_audit_log(client_id = current_client_id())`, gated only by
   `req_authorized()` (`dev`/`hopdesk`/`principal`). `current_client_id()`
   correctly resolves to `effective_client_id()` — but for a `hopdesk`
   session sitting at home (not jumped), that resolves to `"hd-admin"`,
   which means a plain `hopdesk` account can view Hopdesk's own staff
   activity log through this tab today, with no additional check. This
   directly violates "hopdesk can see client logs, never the staff log" —
   fix this as part of Part A below.
2. **"Bitácora Global" can only ever show `hd-admin`'s own log — there is
   no way to browse a specific client's full log.** `global_audit_section`
   hardcodes `read_audit_log(client_id = "hd-admin")` — it shows Hopdesk's
   own log (which, via the dual-write, already contains every staff jump
   action across every client), but there is no client selector, so
   staff/principal cannot pull up, say, Networks' complete log including
   Networks' own native users' actions. Its permission gate
   (`can_view_global_audit`, default `FALSE` for `hopdesk`) also means a
   plain `hopdesk` account can't open this tab at all today — contradicts
   "hopdesk sees client logs with filters."

## Design

### Part A — Split the permission in two, fix both gaps

Two distinct capabilities, currently conflated into one flag:

| Flag | hopdesk | principal | meaning |
|---|---|---|---|
| `can_view_client_audit_logs` (**new**) | `TRUE` | `TRUE` | Browse any *client's* full audit log via a selector |
| `can_view_staff_audit_log` (**rename of `can_view_global_audit`**, keep the same key name if easier — your call, just fix the semantics) | `FALSE` | `TRUE` | View `hd-admin`'s own log specifically |

Fixes:
- **Actividad tab**: when `current_user_info()$is_staff` is `TRUE` and
  `effective_client_id()` resolves to the viewer's own home (`hd-admin`),
  additionally require `can_view_staff_audit_log` — a plain `hopdesk`
  session at home should see "requires higher permission," not the log.
  When the viewer is a native client user, or staff mid-jump, behavior is
  unchanged (shows that client's own log, as today — this is correct).
- **Bitácora Global tab**: change its gate to `can_view_client_audit_logs`
  (so `hopdesk` can open it by default, matching Mouse's intent). Add a
  client selector (reuse the same active-clients-from-registry pattern
  already used in the context-switcher modal, `tiers_module.R` ~483-499).
  Only for a viewer with `can_view_staff_audit_log` (principal), add one
  extra selector option, "Hopdesk (interno)," mapped to `client_id =
  "hd-admin"` — this is the only way to see the staff log itself, and it
  must not appear as an option for a plain `hopdesk` viewer at all (not
  just gated on click — absent from the choices).

### Part B — One shared filter/viewer component, used by both tabs

Mouse's explicit instruction: *"I want the filters to be the same for
everyone who gets access to the logs — make it all the same UI."*

New module: `R/audit_log_viewer_module.R` (`auditLogViewerUI(id)` /
`auditLogViewerServer(id, shared, mode, allowed_client_ids = NULL,
include_staff_log = FALSE)`):

- `mode = "own_client"` — Actividad's use: no client selector, no scope
  decision to make, just render effective_client_id()'s own log with
  filters.
- `mode = "multi_client"` — Bitácora Global's use: renders a client
  selector built from `allowed_client_ids` (already resolved by the
  caller — see Part A), plus the "Hopdesk (interno)" option only if
  `include_staff_log` is `TRUE`.

Filter bar (compact — a single row if it fits, small controls, per
Mouse's screen-real-estate preference): a **datetime range** (from/to,
defaulting to the last 7 days — this single control covers "date range"
and "time range" both, since narrowing to a specific hour window is just a
narrower range on the same picker; confirm this reading with Mouse if the
implementing session thinks he meant something more granular), a **Módulo**
dropdown (populated from the distinct `module` values actually present in
the loaded rows, "Todos" default), an **Acción** dropdown (same pattern),
and a free-text **Usuario** filter. Results table: reuse the exact column
set/style Actividad already has today (Hora, Usuario, Módulo, Acción,
Descripción, ID destino), adding a **Cliente** column only in
`multi_client` mode (Bitácora Global already has one — keep it).

Rewire both `activity_tbl` (Actividad) and `global_audit_section`
(Bitácora Global) in `tiers_module.R` to call this one module instead of
each building/maintaining its own table — this is the actual mechanism
that keeps them "the same UI" instead of two implementations that will
drift the next time either one is touched.

### Part C — Railguards (same philosophy as Stage 3's isolation guarantee, don't invent a new one)

Add `read_audit_log_scoped()` in `R/app_audit.R` — the *only* function the
new viewer module is allowed to call for data, never `read_audit_log()`
directly from the module:

```
read_audit_log_scoped(requested_client_id, viewer_is_staff,
                       viewer_can_view_client_logs, viewer_can_view_staff_log,
                       viewer_home_client_id, since = NULL, until = NULL)
```

- `requested_client_id == "hd-admin"` and `!viewer_can_view_staff_log` →
  `stop()` loudly. A UI letting someone reach this without the right
  permission is itself a bug worth surfacing immediately, not a case to
  silently return empty for.
- `requested_client_id` is any other client, viewer is **not** staff, and
  `requested_client_id != viewer_home_client_id` → `stop()` loudly. A
  native client session must never be able to request a different
  client's log, full stop, regardless of what the UI sent.
- `requested_client_id` is any other client, viewer **is** staff, and
  `!viewer_can_view_client_logs` → `stop()` loudly.
- Otherwise, call `read_audit_log(requested_client_id, since, until)`
  (extend `read_audit_log()` with an `until` parameter alongside the
  existing `since` — simple addition, same chunk-reading logic).

## Explicitly out of scope for this stage

- Changing who *within* a client can see their own Actividad tab
  (currently `dev`/`hopdesk`/`principal` via `req_authorized()` — this
  stage doesn't touch that boundary, only fixes the staff-log leak and
  builds the shared filter UI).
- Any change to `log_action()`'s write/dual-write behavior.
- A dedicated "audit log for hopdesk's own actions, browsable by client,"
  beyond what `read_audit_log(client_id = "hd-admin")` already returns
  (the dual-write mechanism already tags these correctly via the
  `home_client` metadata field — that's sufficient for what's asked here).

## Test plan

**Automated:**
- A simulated plain `hopdesk` session (no `can_view_staff_audit_log`
  override) requesting `read_audit_log_scoped("hd-admin", ...)` gets a
  loud error, not an empty table.
- A simulated `finance`-tier session at `networks` requesting
  `read_audit_log_scoped("hopdesk", ...)` (a different client) gets a loud
  error.
- A simulated `hopdesk` session requesting `read_audit_log_scoped(
  "networks", ...)` succeeds (this is exactly what "hopdesk sees client
  logs" means).
- A simulated `principal` session can request `"hd-admin"` and any client
  successfully.
- The Bitácora Global client selector never renders a "Hopdesk (interno)"
  option for a plain `hopdesk` session (assert its absence in the rendered
  choices, not just that selecting it would fail).
- `read_audit_log()`'s new `until` param correctly excludes rows after the
  boundary (mirror the existing `since` test pattern already in the repo).

**Manual:**
1. As tesoreria or another `finance`-tier Networks user: confirm Actividad
   still shows Networks' own log exactly as before (regression check).
2. As Bunny (`hopdesk`, no special override): open Bitácora Global, confirm
   a client selector appears with Networks (and any other active client)
   as options, confirm selecting Networks shows Networks' real log with
   working filters, confirm there is no "Hopdesk (interno)" option
   anywhere in her view, confirm opening Actividad while at home (not
   jumped) shows "requires higher permission," not hd-admin's log.
3. As Mouse (`principal`): confirm Bitácora Global shows every client plus
   "Hopdesk (interno)," and selecting the latter shows the real staff
   activity log including dual-written jump actions from other clients.
4. Confirm the filter bar looks and behaves identically in both tabs (same
   controls, same layout) — this is the actual proof Part B's shared
   component worked, not just that both tabs happen to show tables.

## Prompt for the implementing Claude Code session

```
Read C:\Users\luisr\Antiguedad_App\docs\saas_rebuild\ARCHITECTURE.md and
docs\saas_rebuild\STAGE_4_AUDIT_LOGGING.md in full before writing any
code. Branch off saas-checkpoint-2026-07-15 (Stages 1-3b already merged),
name the branch stage-4-audit-logging.

Implement all three parts: the permission split (Part A, including the
two real gaps this document identifies — verify they still exist in the
current code before fixing them, don't assume), the shared
R/audit_log_viewer_module.R used by both the Actividad and Bitácora
Global tabs (Part B), and the read_audit_log_scoped() railguard function
that is the only path either tab's UI is allowed to use to fetch log data
(Part C).

Do not touch log_action()'s write path or the dual-write mechanism - it's
correct and out of scope. Do not change who within a client can see their
own Actividad tab.

Work in small increments, starting with read_audit_log_scoped() and its
tests before building UI on top of it. Run the full existing test suite
after each logical change.

When done, stop and report back file by file, plus the manual test plan
from this document for Mouse to run before merging. Do not merge to
master or the checkpoint branch yourself.
```
