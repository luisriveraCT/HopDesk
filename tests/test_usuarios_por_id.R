# =============================================================================
# tests/test_usuarios_por_id.R
# Usuarios (R/tiers_module.R) acts on an account by its username, never by its
# row position. Found in T19 (2026-11-04): Editar and 🗑 sent the row number,
# and the editor / the delete confirmation re-read usuarios_rv()[idx]. With the
# editor open on row 3, deleting row 2 made row 3 another account, and Guardar
# wrote tier, permissions, activo and password onto it.
# Run: Rscript --vanilla tests/test_usuarios_por_id.R (from the repo root)
# =============================================================================

.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

codigo <- readLines("R/tiers_module.R", encoding = "UTF-8", warn = FALSE)

# Only the two helpers, from the real file.
ex <- parse("R/tiers_module.R", keep.source = FALSE)
for (e in ex) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c(".fila_usuario", ".usuario_js"))
    eval(e, envir = globalenv())
}
ok("the helpers exist in tiers_module.R", exists(".fila_usuario") && exists(".usuario_js"))

# ── 1. The T19 scenario: the editor follows the account, not the row ─────────
u <- data.frame(username = c("ana", "beto", "carla"),
                tier = c("admin", "finance", "analysis"), stringsAsFactors = FALSE)
editando <- "carla"                                  # Editar on row 3 sends "carla"
ok("before: carla is row 3", identical(.fila_usuario(u, editando), 3L))
u2 <- u[u$username != "beto", , drop = FALSE]        # beto (row 2) deleted meanwhile
ok("after deleting row 2, the editor still finds carla (now row 2)",
   identical(u2$username[.fila_usuario(u2, editando)], "carla"))
u3 <- rbind(data.frame(username = "zeta", tier = "admin"), u2)  # a new account on top
ok("after a new row on top, still carla",
   identical(u3$username[.fila_usuario(u3, editando)], "carla"))
ok("an account that left the list gives NULL (nothing is written)",
   is.null(.fila_usuario(u2, "beto")))

# ── 2. Only what the viewer's list holds; bad input never matches ────────────
ok("an account hidden from the viewer (not in its filtered list) gives NULL",
   is.null(.fila_usuario(u[u$tier != "admin", ], "ana")))
ok("a row number sent by hand matches nothing", is.null(.fila_usuario(u, 2L)))
ok("NULL, NA, empty and two names match nothing",
   is.null(.fila_usuario(u, NULL)) && is.null(.fila_usuario(u, NA)) &&
   is.null(.fila_usuario(u, "")) && is.null(.fila_usuario(u, c("ana", "beto"))))
ok("an empty or NULL list matches nothing",
   is.null(.fila_usuario(u[0, ], "ana")) && is.null(.fila_usuario(NULL, "ana")))
dup <- rbind(u, data.frame(username = "ana", tier = "dev"))
ok("a username twice in the list matches nothing (never guess)", is.null(.fila_usuario(dup, "ana")))

# ── 3. The button carries a safe JS string ──────────────────────────────────
ok("plain username → quoted JS string, escaped for the attribute",
   identical(.usuario_js("carla"), "&quot;carla&quot;"))
ok("quotes and markup in a username cannot break out of the attribute",
   !grepl("[<>\"']", .usuario_js("a\"b'<x>")))

# ── 4. Guardian on the source: no position comes back ───────────────────────
ok("no Editar/🗑 button sends a row number (%d)",
   !any(grepl("trs_(edit|del)_click", codigo) & grepl("%d", codigo)) &&
   !any(grepl("setInputValue\\(.*%d", codigo[grep("edit_input_id|del_input_id", codigo)])))
ok("no handler re-reads a row by position (idx <= nrow(usuarios))",
   !any(grepl("idx\\s*<=\\s*nrow\\(usuarios\\)", codigo)))
ok("the old position reactives are gone",
   !any(grepl("pending_del_idx|selected_user_idx", codigo)))
n_busca <- sum(grepl(".fila_usuario(usuarios", codigo, fixed = TRUE))
ok(sprintf("delete click, delete confirm, editor and Guardar look the account up by name (%d)", n_busca),
   n_busca >= 4)

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail) quit(status = 1)
