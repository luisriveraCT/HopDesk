# Stage 7 — Password Hashing

Branch off `master`.

## Why, and the standard Mouse set: invisible, simple, complete

`R/auth.R`'s own header comment has said this since the very first version
of the file: *"shinymanager's data.frame credential path uses plain-text
comparison... passwords in usuarios.rds are plain text."* Given the care
just put into ERP credential encryption (Stage 3), leaving login passwords
themselves unprotected is the biggest remaining inconsistency in the app's
security posture.

Mouse's instruction: *"full simple hashing that considers all possibilities
on credentials usage so that it runs so smoothly you never even know it's
happening."* Two halves to that: **simple** (bcrypt, not a custom scheme —
well-understood, one well-known R package, nothing clever), and
**complete** (audit every single place a password is set or checked, not
just the main login path — a partial fix that hashes new passwords but
misses one admin flow would be worse than doing nothing, since it'd create
a false sense of "this is handled now").

Password strength rules (currently: 6+ chars, one number, one symbol) are
explicitly out of scope — leave them exactly as they are.

## Design

### 1. `hash_password()` / `verify_password()` in `R/auth.R`

Add the `bcrypt` package (add to `global.R`'s `library()` block and to
Stage 6's CI package list once both exist). Two small functions:
`hash_password(plain)` → bcrypt hash string; `verify_password(plain, hash)`
→ `TRUE`/`FALSE`. Nothing clever — this is the entire cryptographic
surface of this stage.

### 2. Transparent migration of existing plaintext passwords

Bcrypt hashes have a recognizable, fixed shape (start with `$2a$`, `$2b$`,
or `$2y$` followed by a cost factor). Add a check — wherever `usuarios.rds`
is loaded for real use (`auth_load_usuarios()` is the natural place) —
that treats any `password_hash` value **not** matching that shape as
legacy plaintext: hash it in place with `hash_password()`, and persist the
upgrade back to S3 immediately, once, silently. No user-facing change, no
forced reset, no locked-out accounts — exactly "you never even know it's
happening." Write a code comment here explaining why this auto-detection
exists (so nobody "cleans up" what looks like redundant logic later).

### 3. The login check itself

`R/auth.R`'s `auth_check_credentials()` currently calls
`shinymanager::check_credentials(creds)`, which does a raw `==` comparison
against the `password` column — this only works for plaintext. Replace
this with your own comparison: after the migration step in §2 has run, the
column always holds a bcrypt hash, so the check becomes
`verify_password(entered_password, stored_hash)` instead of relying on
shinymanager's built-in comparator. Keep everything else about
`auth_check_credentials()` (the emergency-lock check, the case-insensitive
username handling) exactly as it is — this stage only changes *how the
password itself is compared*.

### 4. Every place a password gets *written* — audit all of these, not just login

Confirmed during this audit — hash before writing in every one:
- `app.R`'s `force_pw_submit` handler (first-login forced password change).
- `app.R`'s `invite_pw_submit` handler (invite-acceptance account creation).
- `R/tiers_module.R`'s admin-set-password flow (`new_password`/
  `new_password2` inputs, around line 744) — an admin/dev/staff setting or
  resetting someone else's password.
- `scripts/recover_mouse.R` and `scripts/add_bunny.R` — both currently
  write `readline()` input directly into `password_hash` with zero
  hashing. These are emergency/setup scripts run directly by Mouse, not
  through the app, but for the "considers all possibilities" standard
  they need the same treatment — hash before writing here too.

Grep the whole repo for `password_hash` and `readline(` combined with
password-sounding prompts to make sure this list is exhaustive — don't
trust this document's list alone; verify it against the current code.

### 5. Defense in depth: what if a write site is missed anyway?

Since §2's migration step runs on every load, any password that slips
through unhashed (a future write site nobody remembered to update) gets
auto-corrected the very next time that user's record is loaded, not left
broken forever. This doesn't excuse missing a write site — do the audit in
§4 properly — but it's a safety net worth keeping and documenting as such.

## Explicitly out of scope

- Password strength rules — untouched.
- Any change to `usuarios.rds`'s schema (column name stays `password_hash`
  even though it's finally accurate now).
- Multi-factor auth, password expiry policies, or anything beyond "the
  stored value is now a proper hash, always."

## Test plan — this touches every login, be thorough

**Automated:**
- `hash_password()`/`verify_password()` round-trip: a hashed password
  verifies correctly, a wrong password does not.
- A simulated legacy plaintext row is transparently upgraded to a bcrypt
  hash on load, AND the original plaintext password still successfully
  authenticates immediately afterward through the new check — this is the
  single most important test in this stage; it directly proves nobody gets
  locked out.
- A password set through each of the four write paths in §4 is stored as
  a bcrypt hash, never plaintext — one test per path.
- `scripts/recover_mouse.R` / `scripts/add_bunny.R`, run in a test/dry-run
  mode, write a hash, not plaintext.

**Manual (the real proof, given the blast radius of this change):**
1. Before merging, back up `usuarios.rds` for every active client folder
   (S3 versioning already covers this, per Stage 3b's verification, but
   take an explicit note of the current version IDs so a rollback target
   is obvious if needed).
2. Deploy, then have Mouse log in as **himself** (principal) first —
   lowest-risk account to verify, and if anything's wrong here it's caught
   immediately by the person doing the testing.
3. Have Mouse log in as an existing real Networks user (e.g. tesoreria) to
   confirm an ordinary client account isn't locked out.
4. Test one full password-set flow end to end: an admin resetting someone
   else's password, then that person logging in with the new password.
5. Confirm `usuarios.rds` for at least one client, inspected directly,
   shows bcrypt-shaped hashes for every user after the migration — not
   just for the ones who happened to log in during testing (the migration
   should touch every row on the first load, not just active logins).

## Prompt for the implementing Claude Code session

```
Read C:\Users\luisr\Antiguedad_App\docs\saas_rebuild\STAGE_7_PASSWORD_HASHING.md
in full before writing any code. Branch off master, name the branch
stage-7-password-hashing.

This stage touches every user's login credential across every client -
highest blast-radius stage so far if done wrong. Implement exactly what's
specified: hash_password()/verify_password() (bcrypt), the transparent
migration of legacy plaintext passwords on load, the rewritten login
comparison, and hashing at every one of the four write sites in §4 (grep
the repo yourself to confirm that list is exhaustive before trusting it).

Do not touch password strength rules or the usuarios.rds schema beyond
what's specified.

Work in small increments. Do not skip the "legacy plaintext still
authenticates after migration" test - it is the single most important
proof this stage doesn't lock anyone out. Run the full existing test suite
after each logical change.

Before Mouse tests this live, remind him to note the current S3 version
IDs for each client's usuarios.rds (S3 versioning is already enabled) as
an explicit rollback point, given how high-stakes this specific stage is.

When done, report back file by file, and give Mouse the exact manual test
sequence from this document (himself first, then an ordinary client
account, then a full admin-reset-password flow) before he merges. Do not
merge to master yourself.
```
