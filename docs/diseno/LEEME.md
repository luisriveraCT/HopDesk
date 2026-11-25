# Pantallas de HopDesk: el registro de diseño

`pantallas.tsv` es el registro de **cada pantalla de HopDesk**, como `R/hd_registro.R` lo es de las tablas: pestañas, subpestañas, secciones de Configuración, paneles, modales, correos, PDF, exportaciones y páginas fuera de la app. Cada pantalla es una fila.

Sirve para que quien diseñe sepa **exactamente qué hay que diseñar**: dónde vive cada pantalla, de dónde se abre, qué tablas tiene y en qué estado de diseño está.

Es un TSV: se abre en Excel o en Google Sheets y se puede filtrar. No es código y la app no lo carga.

## Las columnas

| columna | qué lleva |
|---|---|
| `id` | Único y estable, en minúsculas, con puntos por nivel: `req.detalle.evidencia`. No se cambia una vez puesto; otras filas lo usan como `padre`. |
| `nombre` | Cómo la llama la gente. |
| `tipo` | `pestana` · `menu` · `boton` (de la barra de arriba) · `pieza` (compartida, como el motor de tablas) · `subpestana` · `seccion` (de Configuración) · `modal` · `panel` · `correo` · `pdf` · `excel` · `descarga` (otro archivo) · `pagina` (fuera de la app). |
| `padre` | El `id` de la pantalla desde donde se abre. Va vacío solo en pestañas, menús, botones, piezas, páginas y correos. |
| `archivo` | Dónde está su código. |
| `ancla` | Lo que se busca en ese archivo para encontrarla (ver abajo). **Nunca un número de línea.** |
| `niveles` | Quién la ve, separado con `;`. Vacío quiere decir lo que diga `R/tier_registry.R` para su pestaña. |
| `permiso` | La clave de `R/permisos.R`, si hace falta una. |
| `tablas` | Los ids de `hd_registro` que muestra, separados con `;`. |
| `diseno` | `pendiente` · `en_diseno` · `listo`. |
| `notas` | Lo que quien diseñe debe saber. Corto. |

## Las anclas

La guardiana encuentra sola estas, y exige que estén en el TSV:

| ancla | qué es |
|---|---|
| `nav:<value>` | Un `nav_panel`, `nav_menu` o `tabPanel`. Si no tiene `value`, se usa su título (el texto o la clave de `tr()`). |
| `modal:<función>` | Un `modalDialog` o `hd_modal` dentro de esa función. |
| `modal:<función>@<evento>` | Lo mismo, cuando se abre dentro de un `observeEvent(<evento>, …)`. |
| `…#2`, `…#3` | El segundo o tercer modal con la misma ancla en el mismo archivo, en el orden del código. |
| `correo:<función>` | Una función `email_*_html` o `correo_*_html`. |
| `descarga:<id>` | `output$<id> <- downloadHandler(…)`. |
| `pagina:<parámetro>` | Una rama de `query[["…"]]` en `app.R`: una página fuera de la app, como `?invite=`. |

Estas las escribe una persona, para lo que el barrido no puede reconocer por su forma (paneles, botones, páginas). La guardiana solo revisa que existan:

| ancla | qué es |
|---|---|
| `fn:<función>` | Una función definida en ese archivo. |
| `texto:<texto>` | Un texto literal que aparece en ese archivo. |

## Si la prueba falla

`tests/test_pantallas_registro.R` compara el código contra el TSV, en las dos direcciones.

- **Agregaste una pantalla.** La prueba dice cuál falta y trae la fila lista para pegar:
  ```
  Pantalla sin registrar: R/requisiciones_module.R  modal:.req_pedir_algo
      agrega a docs/diseno/pantallas.tsv la fila:
      <id>	Título del modal	modal	<padre>	R/requisiciones_module.R	modal:.req_pedir_algo			pendiente
  ```
  Pega la línea en el TSV. Cambia `<id>` y `<padre>`, y revisa el nombre.
- **Sacaste o renombraste una pantalla.** Sale como «Fila fantasma». Borra la fila o corrige su `ancla`. Si la pantalla sigue existiendo, conserva el `id`.
- **Agregaste una tabla a `hd_registro`.** Ponla en la columna `tablas` de la pantalla que la muestra.

Para ver de una vez todas las filas que faltan:

```
Rscript --vanilla scripts/pantallas_barrido.R             # las muestra
Rscript --vanilla scripts/pantallas_barrido.R --escribir  # las agrega al TSV
```

Una fila que agrega el script es un borrador: el `id`, el `padre` y el `nombre` los revisa una persona.

## Lo que viene

- **T97b:** una ficha por pantalla en `docs/diseno/pantallas/`, generada con su estado de tema (colores escritos a mano) e idioma (textos fuera del diccionario), sus tablas y sus notas.
- **T97c:** las capturas en Clásico y en Marca, como staff y como cliente.
  - Se guardan **fuera del repo**, en `hopdesk_coord/diseno/capturas/`, porque muestran datos.
  - La ficha dice dónde está cada captura; no la incrusta.
