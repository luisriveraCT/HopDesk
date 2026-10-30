# Propiedad de archivos

Varias sesiones de Claude trabajan este repo a la vez: los frentes del plan
"Panorama y Requisiciones", el equipo de Tablas y las sesiones sueltas de
Luis. Este documento dice quién puede editar qué. El mapa exacto, que es el
que se aplica, vive en `coordinacion/propiedad.tsv`.

## Dueños

| Dueño | Quién es | Rama |
|---|---|---|
| integrador | El coordinador de Panorama y Requisiciones (frentes A1, A2 y F) | `master`, `feat/a1*`, `feat/a2*`, `feat/f*` |
| B | Pantalla Panorama y modo Publicar | `feat/b*` |
| C | Alertas del equipo | `feat/c*` |
| D | Correos, proceso programado, archivo y compresión | `feat/d*` |
| E | Requisiciones | `feat/e*`, `requisiciones/*` |
| T | Sistema de Tablas (motor `hd_table`) | `tablas/*` |
| compartido | Cualquiera, con su prefijo de claves | — |

Un archivo que no está en el mapa es libre.

**Compartido = diccionario de idiomas** (`www/i18n/*.json`). Cada quien agrega
solo claves con su prefijo: `tablas.*` (T), `requisiciones.*` (E),
`panorama.*` (B), `avisos.*` (C, D), `niveles.*` y `permisos.*` (integrador),
`comun.*` (avisar en el puente antes de agregar). `tests/test_i18n.R` debe
seguir pasando.

## Reglas

1. **Una rama y un worktree por frente.** Nadie trabaja en `master` salvo el
   integrador. Nunca en el checkout principal de otra sesión.
2. **Solicitudes, no ediciones.** Si necesitas algo en un archivo ajeno,
   escríbelo en `coordinacion/<tu frente>.md` (o en el puente con Tablas,
   `C:\Users\luisr\hopdesk_coord\requisiciones\bitacora.md`) con el texto
   exacto. El dueño lo aplica.
3. **Contratos congelados.** Las funciones de `CAPACIDADES.md` marcadas como
   contrato no cambian de firma sin pasar por el integrador.
4. **Cola de merge, uno a la vez:** rebase sobre `master`, suite completa,
   revisión independiente, merge, aviso a los demás frentes para que hagan
   rebase. Orden: A1, A2, luego C y E, luego B, luego F. D cuando estén las
   credenciales.
5. **Despliegue** solo desde `master` limpio, con commit y push.
6. **Nunca `git stash` a secas**: la pila es compartida entre worktrees.

## Candado

- **Al hacer commit:** `.githooks/pre-commit` revisa que tu rama no toque
  archivos de otro frente. Se activa por clon con
  `git config core.hooksPath .githooks` (afecta a todos los worktrees de ese
  clon; avisar antes de activarlo).
- **En GitHub:** el job `propiedad` de `.github/workflows/test.yml` hace la
  misma revisión en cada pull request, así que también atrapa una sesión que
  se saltó el gancho.

Cambiar el mapa es trabajo del integrador.
