# Fuentes guardadas dentro de la app

| Archivo | Fuente | Cubre |
|---|---|---|
| `fraunces-latin.woff2` | Fraunces (variable, 400–600) | Latín básico: español e inglés, acentos y ñ |
| `fraunces-latin-ext.woff2` | Fraunces (variable, 400–600) | Latín extendido |

- **Origen:** los mismos archivos que usa el lienzo de Design de Panorama (`hopdesk_coord/requisiciones/referencia/`), que son los cortes de Google Fonts.
- **Licencia:** Fraunces es de The Fraunces Project Authors (https://github.com/undercasetype/Fraunces) y se distribuye con la **SIL Open Font License 1.1** (https://openfontlicense.org). La licencia permite guardarla y servirla dentro de la app. No se vende sola, y conserva su nombre y su aviso de copyright.
- **Dónde se usa:** `www/hd_marca.css` declara la fuente. La usa cualquier elemento dentro de la clase `.hd-marca`.
- **Por qué va dentro de la app:** así no depende de un servicio externo al cargar la página.
- Lato no se guarda aquí porque ya llega con el tema de la app (bslib, flatly).
