# Stage 9 Attempt Log

Created per `STAGE_9_LIVE_UI_BUGS.md`'s "Documentation-of-failure convention."
Append an entry every time an approach tried during this stage turns out
wrong or needs reverting/reworking -- what was tried, why it seemed
reasonable, and specifically why it didn't work. Mirrors the master plan's
"Stage N correction" sections, as its own file so it doesn't get lost.

---

## Issue B (modal reopening): `session$rootScope()` alone did not fix it

**What was tried:** the Stage 9 doc's hypothesis was that the ONLY problem
was a Shiny module-namespace mismatch -- `R/ui_components.R`'s JS emits a
bare, root-level `Shiny.setInputValue('cal_day_modal_closed', ...)`, but
`R/ledger_module.R`'s consumer (`observeEvent(input$cal_day_modal_closed,
...)`) lives inside `moduleServer(id, ...)`, so it actually listens for the
namespaced `ar-cal_day_modal_closed`/`ap-cal_day_modal_closed`, which never
matches. First fix attempt: change only the R side to
`observeEvent(session$rootScope()$input$cal_day_modal_closed, ...)` --
`session$rootScope()` is the standard, documented Shiny mechanism for a
module to read a root-level, non-namespaced input, so this looked like the
complete fix per the doc's own diagnosis.

**Why it didn't work:** built a live chromote reproduction (minimal 2-module
fixture app, mirroring `ledgerModuleServer("ar", ...)`/`("ap", ...)`) with a
`fire_count` reactiveVal instrumented into the observer, closed the modal
via `$('#shiny-modal').modal('hide')` (the exact call Shiny's bundled JS
makes internally for a backdrop click, ESC, or the close button), and the
observer's fire count stayed at **0** -- even with the rootScope fix
applied. Added a SECOND, ROOT-LEVEL (non-module) diagnostic observer on the
literal `input$cal_day_modal_closed` to isolate where the break really was:
it ALSO stayed at 0. The JS signal was never reaching R at all, at ANY
scope -- the module-namespace fix was correct but irrelevant if the
underlying event never fires in the first place.

Root cause of THIS layer: the original JS used
`document.addEventListener('hidden.bs.modal', ...)`, a **native DOM**
listener. But Shiny's own bundled `showModal()`/`modalDialog()` uses the
**older jQuery/Bootstrap-3-style modal plugin** (confirmed live:
`window.bootstrap` is `undefined` in a real Shiny page; the modal element's
class is `"modal fade in"`, the Bootstrap-3/4 jQuery-plugin shape, not
Bootstrap 5's `"modal fade show"`). That plugin fires `hidden.bs.modal` as a
**jQuery-namespaced event** via `$el.trigger('hidden.bs.modal')` -- this is
jQuery's own internal event system, invisible to `document.addEventListener`
regardless of namespace, because jQuery does not re-dispatch custom event
names as real native `CustomEvent`s unless explicitly told to. Confirmed by
switching the JS binding to `$(document).on('hidden.bs.modal', ...)` (jQuery
listening for jQuery's own event) -- root fire count immediately went to 1,
and with the rootScope fix ALSO in place, both AR's and AP's fire counts
went to 1 and `modal_open` correctly flipped to `FALSE`.

**Lesson:** this bug was two independent breaks stacked, not one. The doc's
namespace-mismatch diagnosis was correct as far as it went, but its own
"what to investigate before writing a fix" step 1 ("read shinymanager's...")
was about a different issue (Issue A); for Issue B, the equivalent
gut-check -- "is the JS signal itself actually reaching R at ALL, root or
module, before assuming only the module scope is broken" -- was the missing
verification step. A root-scope-only fix would have shipped and looked
correct in code review (the module-namespace theory is real and does need
fixing) while silently doing nothing at runtime, because the prerequisite
(the JS event ever firing) was never true to begin with. Confirmed via a
live chromote reproduction before committing anything -- exactly the
"verify before building on it" instruction this stage's doc gave, applied
one layer deeper than the doc itself anticipated.

**Final fix (both halves, both required):**
- `R/ui_components.R`: `document.addEventListener('hidden.bs.modal', ...)`
  → `$(document).on('hidden.bs.modal', ...)`.
- `R/ledger_module.R`: `observeEvent(input$cal_day_modal_closed, ...)` →
  `observeEvent(session$rootScope()$input$cal_day_modal_closed, ...)`.

See `tests/test_ledger_modal_reopen_chromote.R` for the committed live proof
against the real, current code (both halves fixed). The rootScope-only and
JS-only intermediate states described above were reproduced in scratch
fixtures during investigation (not committed) specifically to isolate which
half of the fix was doing the work before committing either change.

---

## Issue E1 (sync latency measurement): two fixture-building mistakes before
getting a clean two-session timing harness

**What was tried first:** a minimal fixture app sourcing the real
`R/sync_bus.R` against a local, in-memory fake of the S3 layer, with a
`shared <- reactiveValues(shared_val = 0)` object (the ordinary,
most-common Shiny pattern for a bag of reactive fields), to measure real
propagation delay between two chromote sessions under the real 8000ms
`poll_ms`.

**Why it didn't work (first bug):** every trial timed out -- neither
session ever saw the bumped value, even after 15s. Added `message()` trace
lines into a throwaway copy of `sync_bus.R` and found the real cause:
`setup_sync_bus()`'s reload step does `rv <- shared[[name]]; if
(is.function(rv)) rv(new_value)` -- this requires `shared[[name]]` to BE a
`reactiveVal()` getter/setter function, not a `shiny::reactiveValues()`
field (which is read/set via `$`/`[[` assignment, never called like a
function). The trace showed `rv is.function=FALSE` on every tick -- the
reload was silently no-opping. Checked app.R:1131 and confirmed the REAL
app's `shared` object is `shared <- list(...)`, a plain list where each
element is individually a `reactiveVal(...)` -- not a `reactiveValues()`
object at all. Fixed the fixture to `shared <- list(shared_val =
reactiveVal(0))` and the mechanism worked immediately.

**Second bug, same debugging session:** after fixing the above, a
DIFFERENT error appeared first: `[sync_bus] version stamp write failed:
could not find function ".s3_write"`, even though `.s3_write` was clearly
defined earlier in the same fixture file. Root cause: the fixture sourced
`R/sync_bus.R` with `local = FALSE`, which (per `source()`'s own semantics)
evaluates the sourced file in `.GlobalEnv` specifically -- but
`shiny::runApp()` on a single-file app evaluates the app file's OWN top
level in a private, app-specific environment, not `.GlobalEnv`. So the
fixture's own `.s3_write`/`.s3_key`/etc (defined via ordinary top-level
`<-` in that app-private environment) ended up in a DIFFERENT environment
than `sync_bus.R`'s functions (forced into `.GlobalEnv` by `local = FALSE`),
breaking lexical lookup between them. Fixed by sourcing with `local = TRUE`
instead, keeping everything in the one shared app-private environment.

**Lesson:** both mistakes were about environment/object-shape mismatches
between a hand-built test fixture and the real app's actual conventions,
not bugs in `sync_bus.R` itself -- exactly the kind of thing a live,
executable reproduction catches immediately (both surfaced as an obvious,
loud failure on the very first run) that a purely-read-the-code review
would not have caught before writing the "real" timing test. Confirmed
correct in both cases by checking the real app.R's own shape rather than
guessing. See `tests/test_sync_bus_timing_chromote.R` for the corrected,
committed fixture.

---

## Issue E2 (cross-client isolation): a "fix" for the UX concern that turned
out to be unnecessary -- reverted after checking Shiny's own source

**What was tried:** the Stage 9 doc's sub-question (b) asked whether an
unrelated client's write forces every OTHER client's session to
recompute/re-render, even when the reloaded data stays correctly scoped. A
first, less careful two-session chromote reproduction (client A writes,
client B -- a different, uninvolved client -- watches) appeared to confirm
this: B's own re-render counter went from 0 to 1 right after A's write. This
looked like exactly the bug Mouse described ("it should NOT interrupt
anything on anyone's window... ESPECIALLY CRITICAL CROSS CLIENTS"), so a fix
was written and applied to `R/sync_bus.R`: before calling `rv(new_value)` in
the poll/reload loop, compare `new_value` against `isolate(rv())` and skip
the call entirely if `identical()`.

**Why it didn't hold up:** re-examining the FIRST reproduction's setup
before trusting the result (per this stage's own "verify before building on
it" discipline, applied one layer deeper): client B's `shared$shared_val`
reactiveVal is only ever initialized ONCE, at session start, under WHATEVER
identity was active at that moment (in the test, B started as "networks"
before switching its own dropdown to "acme" mid-test). Switching identity
alone does not itself trigger a sync_bus reload -- only an actual
`bump_sync_version()` call does. So B's very first observed "re-render"
was really B finally catching up to its OWN correct client's data for the
first time (a genuine, deserved reload), not a false trigger from A's
unrelated write. Rebuilt the reproduction to have B "prime" its own
baseline (one real write+reload under its true identity) BEFORE testing
whether an UNRELATED client's write causes a SECOND, spurious reload -- and
with that confound removed, B's re-render count never increased in
repeated trials, even against the ORIGINAL, unfixed `sync_bus.R`.

Traced why directly in the installed shiny package rather than guessing:
`shiny:::ReactiveVal$public_methods$set` already does
`if (identical(private$value, value)) return(invisible(FALSE))` before
touching `private$dependents$invalidate()`. Combined with app.R:1131's
`shared` being a plain list of individual `reactiveVal()`s (not
`shiny::reactiveValues()`, confirmed during Issue E1's investigation), this
means Shiny itself already skips invalidation whenever a poll-triggered
reload returns byte-for-byte identical content to what a session already
has -- which is exactly what happens for every OTHER client whenever ONE
client writes to a shared dataset name.

**Reverted the `R/sync_bus.R` change** (`git stash` + `git stash drop`,
confirmed via `git diff` that the file matched its last commit again)
rather than keep it: it was 100% behaviorally redundant with what
`reactiveVal()` already does internally, so keeping it would have been dead
code duplicating existing behavior for no benefit -- exactly the kind of
unnecessary addition this project's own conventions say not to ship.

**Lesson:** the exact same discipline this stage's doc asked for
("verify... they were researched carefully but are not guaranteed still
accurate") applies just as much to a hypothesis formed mid-stage, from a
live test result, as to the original doc's own hypotheses. A live
reproduction that shows the shape of a bug is strong evidence, but the
FIRST version of a hand-built harness can itself have a confound -- worth
re-checking the harness's own setup, and worth checking upstream (Shiny's
own source) for whether a lower layer already solves the problem, before
writing a fix. The final, corrected reproduction with the confound removed
is `tests/test_sync_bus_isolation_chromote.R`, which also proves it has
teeth (Part 2: a deliberately-broken sync_bus.R variant IS caught by the
same harness) for the correctness/security half of E2 that IS real and DOES
need the existing client_id-threading logic to stay intact.
