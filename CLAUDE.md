# HopDesk — reglas para cualquier sesión de Claude

Varias sesiones trabajan este repo al mismo tiempo. Antes de editar nada:

1. `git fetch` y revisa `git log` de tu rama y de `master`. No confíes en lo
   que "recuerdas" del estado del repo.
2. Lee `coordinacion/PROPIEDAD.md`. No edites archivos de un frente abierto;
   pide el cambio en `coordinacion/<tu frente>.md` con el texto exacto.
3. Trabaja en tu propio worktree y tu propia rama
   (`git worktree add ../Antiguedad_App_<nombre> -b <rama> master`). Nunca en
   el checkout de otra sesión, nunca `git stash` a secas.
4. Nada de push ni de despliegue sin aprobación de Luis. Despliegue solo desde
   `master` limpio, con commit y push.

## Convenciones que no se negocian

- **Todo en español**, y todo texto visible sale del diccionario:
  `tr("clave")` en R, `hdT("clave")` en JS (`R/i18n.R`, `www/i18n/*.json`).
- **Cero niveles escritos a mano.** Los niveles viven en `R/tier_registry.R`;
  pregunta con `niveles_cliente()`, `nivel_ti()`, `nivel_al_menos()`, etc.
  `tests/test_niveles_guardian.R` falla si escribes `"admin"` en otro lado.
- **Permisos solo con `puede(usuario, "clave")`** (o `puede_o_avisa(shared, ...)`
  en un observer), siempre en el servidor. Un permiso nuevo va en
  `R/permisos.R`, con nombre en los dos idiomas y al menos un lector;
  `tests/test_permisos_vivos.R` lo exige.
- **Tablas nuevas con `hd_table()`**, nunca DT ni HTML propio.
- **S3 siempre con `client_id` explícito**; las escrituras releen antes de
  escribir (ver `R/avisos.R`, `.s3_modificar()`).
- **Nombre ↔ iniciales de empresa** solo con `shared$company_map()`, nunca con
  `COMPANY_MAP`.
- **Correr en local va al bucket de desarrollo** salvo `HOPDESK_PRODUCTION=1`.
- **Las pruebas nunca parchan `aws.s3`** sin restaurarlo; mejor un almacén en
  memoria local a la prueba (ver `tests/test_avisos.R`).
- Rscript: `"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" --vanilla`.

Las funciones que otros frentes pueden usar están en `CAPACIDADES.md`.
