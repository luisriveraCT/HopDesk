# Capacidades de HopDesk

Funciones que un frente puede usar sin leer ni escribir los archivos de otro.
Las marcadas **contrato** no cambian de firma sin pasar por el integrador
(`coordinacion/PROPIEDAD.md`). Cada frente agrega aquí lo que entrega.

## Niveles — `R/tier_registry.R` (integrador)

| Función | Para qué |
|---|---|
| `niveles_cliente()` · `niveles_staff()` · `niveles_todos()` | Niveles en orden, del rango más alto al más bajo. **contrato** |
| `nivel_rango(t)` · `nivel_al_menos(t, ref)` | Comparar rangos. **contrato** |
| `nivel_nombre(t)` · `nivel_color(t)` | Nombre traducido e insignia. |
| `nivel_ti()` · `nivel_tope()` · `nivel_alta()` | El nivel de TI del cliente, el nivel más alto de staff, el preseleccionado al crear usuarios. |
| `niveles_invitables()` · `niveles_asignables(viewer, is_staff)` | Qué niveles puede crear una invitación o asignar un usuario. |
| `nivel_pestanas(t)` · `nivel_calendario(t)` | Pestañas que ve un nivel (`NULL` = todas) y si su Calendario está fijo en CxC o CxP. |
| `is_staff_tier(t)` · `tier_assignment_allowed(...)` | Igual que antes. |

Agregar un nivel = una entrada en `TIER_REGISTRY` y `niveles.nombre.<clave>` en el diccionario.

## Permisos — `R/permisos.R` (integrador)

| Función | Para qué |
|---|---|
| `puede(usuario, permiso)` | La única forma de revisar un permiso. `usuario` = `shared$usuario()`. Falla cerrado. **contrato** |
| `puede_o_avisa(shared, permiso)` | En un observer: avisa y devuelve `FALSE` si no tiene permiso. `if (!puede_o_avisa(shared, "k")) return()` |
| `permisos_resueltos(tier, overrides_json, config_cliente)` | Resolución completa: nivel → configuración del cliente → usuario → bloqueados. |
| `perm_visible_keys(is_staff)` · `permisos_internos()` · `permiso_nombre(k)` | Para pantallas de permisos. |

Permisos de Requisiciones (frente E): `can_view_requisiciones`,
`can_create_requisiciones`, `can_edit_requisiciones`,
`can_trash_own_requisiciones`, `can_trash_others_requisiciones`,
`can_edit_provisiones` (Crear provisión). Al conectar el primer lector, quita
`pendiente = "E"` de su entrada en `R/permisos.R` (pídelo al integrador).

## Sesión — `app.R` (integrador)

| En `shared` | Qué es |
|---|---|
| `shared$usuario()` | `list(user, name, tier, tier_real, is_staff, viendo_como, permisos)`. `tier` es el nivel efectivo ("Ver como" incluido). **contrato** |
| `shared$tier_efectivo()` | Nivel con el que se ve la app. **contrato** |
| `shared$permisos_version` | Súbelo (+1) después de guardar permisos para que la sesión los recalcule. |

## Avisos y preferencias — `R/avisos.R` (integrador)

Todas reciben `client_id` explícito y fallan cerradas sin él. Cada escritura relee S3 antes de escribir.

| Función | Para qué | La usan |
|---|---|---|
| `avisos_leer(client_id)` | Todos los avisos del cliente. **contrato** | B, D |
| `avisos_visibles(client_id, usuario)` | Lo que ese usuario puede ver; filtra en el servidor. **contrato** | B |
| `avisos_agregar(client_id, aviso, por)` | Agrega; con la misma `clave` viva actualiza en vez de duplicar. Devuelve el id. **contrato** | B, C, D, E |
| `avisos_retirar(client_id, id, por)` · `avisos_marcar_hecho(client_id, id, usuario)` | Nada se borra. **contrato** | B |
| `avisos_personales(client_id, usuarios, aviso)` | Comentarios y menciones; nunca al autor; se agrupan por `clave`. **contrato** | E |
| `avisos_actividad(client_id, accion, usuario, params)` | Gancho de las 4 acciones; nunca lanza error. **contrato** | C |
| `audiencia_niveles(client_id)` | Niveles de cliente con su número de personas, para la barra. **contrato** | B |
| `prefs_leer(client_id, usuario)` · `prefs_guardar(client_id, usuario, cambios)` | Inicio, idioma, última audiencia, última visita. **contrato** | B, E, F |

## Pasivos — `R/pasivos_capabilities.R`

`has_capability(user, "pasivos.<cap>")` responde con los permisos de la sesión
(`PASIVOS_CAPABILITIES` dice qué permiso pide cada capacidad).
`pasivos_autoriza(cap)` es la versión para observers.

## Idiomas — `R/i18n.R` (T)

`tr("clave", ...)`, `tr_n("clave", n)`; en JS `hdT()`, `hdTn()`. Ver el encabezado de `R/i18n.R`.

## Tablas — `R/hd_table.R` (T)

`hd_table_server(...)` / `hd_table_ui(...)`. Contrato y piezas en el puente con Tablas y en `R/hd_registro.R`.
