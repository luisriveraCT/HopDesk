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
