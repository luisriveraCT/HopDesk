# Runbook — Activar y pagar el servicio de correo de HopDesk

**Objetivo:** que las invitaciones de HopDesk lleguen al buzón del invitado, no al log.
**Proveedor:** [Resend](https://resend.com) (ya integrado en el código, `R/email_service.R`).
**Tiempo real de ejecución:** 30–45 min de trabajo + 15 min a 24 h de espera por DNS.

---

## 0. Situación actual (verificada el 2026-09-30)

| Pieza | Estado |
|---|---|
| Integración con Resend (`R/email_service.R`) | ✅ Lista |
| Creación de invitación + token + enlace de activación | ✅ Funciona |
| Pantalla de activación (`?invite=<token>` en `app.R`) | ✅ Funciona |
| Cola de reintentos en `hd-admin/email_queue.rds` | ✅ Funciona |
| **Envío real de correo (producción)** | ⚠️ Clave real puesta, falta **redesplegar** |
| **Envío real de correo (local)** | ✅ `sandbox` a propósito, vía `.Renviron.local` |
| **Cuenta de Resend** | ✅ Creada, plan Free |
| **API key en `.Renviron`** | ✅ Instalada y probada contra la API real |
| **Entrega real verificada** | ✅ 3 correos a Primary de Gmail, no a Spam |
| **Dominio propio** | ✅ **`hopdesk.app`** (Porkbun, hasta 2028-10-27) |
| **Registros DNS en Porkbun** | ✅ Los 4 creados vía API el 2026-10-27, resuelven públicamente |
| **Dominio verificado en Resend** | ❌ **Pendiente** — falta pulsar `Verify DNS Records` |

**Lo único que falta son los 3 registros DNS de la sección 3.** Verificado
contra la API real el 2026-10-27: la clave funciona y está correctamente
restringida a solo-envío, las dos plantillas (invitación y bienvenida) se
entregan bien, y el error que devuelve hoy el remitente definitivo es
exactamente el esperado:

```
HTTP 403: The hopdesk.app domain is not verified.
```

Sin dominio verificado, Resend sólo entrega desde `onboarding@resend.dev` y
sólo al correo del dueño de la cuenta.

> `hopdesk.com` **no es tuyo** (está estacionado en Afternic, en venta) y no
> hace falta: el dominio del producto es **`hopdesk.app`**. El código ya usa
> `noreply@mail.hopdesk.app` como remitente por defecto.

---

## 1. Dominio remitente — **RESUELTO (2026-10-27)**

> **Dominio:** `hopdesk.app`, registrado en **Porkbun** (vence 2028-10-27).
> **Nameservers:** `curitiba / fortaleza / maceio / salvador .ns.porkbun.com`.
> **Se verifica en Resend el SUBDOMINIO `mail.hopdesk.app`.**
> **Remitente final:** `noreply@mail.hopdesk.app`.

### Por qué un subdominio

Es lo que recomienda Resend y es la decisión del 2026-10-27: **aísla por
completo la reputación de envío transaccional del correo humano** que algún día
viva en `hopdesk.app` (`soporte@`, `ventas@`). Si un buzón humano se compromete
o alguien marca un correo como spam, las invitaciones no se ven arrastradas —
y al revés.

Un matiz que conviene tener claro, porque invierte la intuición: **Resend no
pone su SPF en el dominio que verificas**, sino en `send.<dominio>` — el
*return-path* de Amazon SES. Verificar `mail.hopdesk.app` crea entonces los
registros bajo `send.mail.hopdesk.app`. Ese anidamiento se ve raro en el panel
de DNS pero es correcto y esperado.

Consecuencia directa: **el DMARC necesita alineación relajada (`aspf=r`)**,
porque el remitente es `@mail.hopdesk.app` mientras el SPF se valida contra
`send.mail.hopdesk.app`. Con `aspf=s` esa diferencia contaría como desalineada
y DMARC fallaría en todos los correos.

Si algún día HopDesk manda marketing o boletines, ésos van en **otro**
subdominio (`news.hopdesk.app`) — nunca mezclados con las invitaciones.

### ⚠️ Lo que NO hay que hacer

**No uses `networkslogistics.com.mx` a secas.** Ese dominio hoy tiene:

```
MX  -> networkslogistics-com-mx.mail.protection.outlook.com   (Microsoft 365)
TXT -> v=spf1 include:spf.protection.outlook.com -all
```

Ese `-all` es un *hard fail*: cualquier correo enviado desde ese dominio por
algo que no sea Exchange Online se rechaza. Para meter a Resend ahí habría que
tocar el SPF del dominio corporativo — es decir, arriesgar el correo de toda la
empresa para mandar invitaciones. No vale la pena.

`hopdesk.app` resuelve esto de raíz: es un dominio propio del producto, sin
correo corporativo que arriesgar. Una invitación que llegara desde
`@networkslogistics.com.mx` además le diría al cliente que está entrando a un
sistema interno de Networks, no a una plataforma.

### Nota sobre la URL de la app

El correo sale de `@mail.hopdesk.app`, pero el **enlace de activación apunta a
`https://1weh8r-luis-rivera.shinyapps.io/Hopdesk/`** — la app construye el
enlace desde `session$clientData`, así que refleja dónde está corriendo de
verdad. Es una inconsistencia **cosmética**, no funcional: el enlace sirve.

Decisión del 2026-10-27: **se queda así por ahora.** Un dominio personalizado
en shinyapps.io exige el plan Professional (~$299 USD/mes), desproporcionado
para esto. Si algún día se migra (Posit Connect Cloud, o el `docker-compose.yml`
+ `nginx.conf.template` que ya están en el repo), el enlace se corrige solo —
no hay nada que cambiar en el código del correo.

> `.app` está en la lista HSTS precargada: **cualquier** cosa que se sirva en
> ese dominio tiene que ser HTTPS obligatoriamente. Irrelevante para el correo,
> importante el día que se monte la app ahí.

---

## 2. Crear la cuenta y elegir el plan

1. Entra a <https://resend.com/signup> y crea la cuenta con un correo que
   **no sea personal** — usa un buzón que sobreviva a cualquier persona, por
   ejemplo `suscripciones@networkslogistics.com.mx` (el mismo que ya usa la
   cuenta `mouse` de hd-admin).
2. Nombra el *team* `HopDesk`.
3. Activa **2FA** en la cuenta. Esa cuenta puede mandar correo en nombre de tu
   dominio; trátala como una credencial de producción.

### Planes y cuál te toca pagar

| Plan | Precio | Correos/mes | Límite diario | Dominios | Retención de logs |
|---|---|---|---|---|---|
| **Free** | $0 | 3,000 | **100/día** | 3 | 30 días |
| **Pro** | **$20 USD/mes** | 50,000 | sin límite diario | 10 | — |
| Pro (superior) | $35 USD/mes | 100,000 | sin límite diario | 10 | — |
| Scale | desde $90 USD/mes | 100,000+ | sin límite diario | 1,000 | — |

**Qué consume HopDesk hoy.** Los correos son transaccionales y de bajo volumen:

- 1 invitación por usuario nuevo (+1 de bienvenida al activar la cuenta)
- avisos de límite de usuarios (solo al llegar a ≤2 lugares libres)
- solicitudes de "más usuarios" de un cliente
- avisos de cambio de límite a los `principal`

Aun onboarding 50 usuarios en un mes, eso son ~150 correos. **Muy por debajo de
los 3,000 del plan gratuito.**

**Mi recomendación:** empieza en **Free**. No pagues los $20 todavía.

Sube a **Pro ($20/mes)** cuando se cumpla cualquiera de estas:

- necesitas mandar **más de 100 correos en un mismo día** (onboarding masivo de
  un cliente grande — 100 invitaciones de golpe topan el plan gratuito);
- necesitas los logs de entrega de **más de 30 días atrás** para soporte o
  auditoría (hoy el audit log de HopDesk registra que *se pidió* el envío, pero
  el *entregado/rebotado* vive en Resend);
- quieres verificar más de 3 dominios (uno por cliente white-label).

**Cómo pagar:** `Settings → Billing → Upgrade`, tarjeta de crédito
corporativa. Se factura en USD, mensual, cancelable en cualquier momento.
Resend emite factura descargable desde `Billing → Invoices` — para deducibilidad
en México necesitarás además el comprobante de la tarjeta, porque **no emiten
CFDI**.

---

## 3. Verificar el dominio en Resend

1. `Domains → Add Domain`.
2. Escribe **`mail.hopdesk.app`** (el subdominio completo, tal cual).
3. Región: deja **`us-east-1`** (es el valor por omisión, bajo *Advanced
   options*; mismo continente que tus usuarios y que el bucket S3). Elegir otra
   cambia los valores de los registros, así que decídelo antes de capturarlos,
   no después.
4. Resend te muestra los registros. **Ojo: hay dos esquemas distintos.**

   Resend cambió de esquema a mediados de 2026. Los dominios dados de alta
   **antes de agosto de 2026** reciben `MX` + `TXT` de SPF (Amazon SES). Los
   **posteriores** reciben **dos `CNAME`** apuntando a `...mta.net`.
   `mail.hopdesk.app` se dio de alta el 2026-10-27, así que le tocó el
   esquema nuevo:

| Sección en Resend | Tipo | Host | Valor |
|---|---|---|---|
| Domain Verification · DKIM | `TXT` | `resend._domainkey.mail` | `p=MIGfMA0GCSq...` |
| Enable Sending · SPF | `CNAME` | `rsend.mail` | `rsend.<...>.mta.net` |
| Enable Sending · SPF | `CNAME` | `send.mail` | `send.<...>.mta.net` |
| DMARC (opcional) | `TXT` | `_dmarc` | ver sección 4 |

> **Copia los valores con el botón de copiar del panel, nunca de esta tabla ni
> seleccionando a mano.** Son únicos por dominio, y la pantalla los **trunca
> con `...`** — pegar un valor truncado produce un registro que se ve bien y
> nunca verifica.

**Los dos CNAME se verifican por separado.** Si sólo pasa uno, el dominio queda
`partially_verified`: envía, pero sin servidor de respaldo. Pon los dos.

#### Lo que NO hay que capturar

- **Tracking (`CNAME links.mail`)**: reescribe los enlaces del correo para
  medir aperturas y clics. En una invitación eso convierte una URL de
  `hopdesk.app` en una de un tercero — justo lo que entrena a la gente a
  desconfiar de un correo. No lo necesitamos.
- **Enable Receiving**: déjalo apagado. Las respuestas se resuelven con la
  cabecera `Reply-To`, no recibiendo correo (ver sección 5).

### Cómo capturarlos en Porkbun

Tres caminos, de menos a más trabajo.

**a) El botón `Go to Porkbun` de Resend.** Resend detecta el proveedor y lo
ofrece en la misma pantalla. Si captura los registros solo, ya terminaste.

**b) Automatizado.** `scripts/porkbun_dns_resend.R` los crea vía la API de
Porkbun, leyendo `scripts/resend_dns_records.tsv`:

```powershell
cd C:\Users\luisr\Antiguedad_App
Rscript --vanilla scripts/porkbun_dns_resend.R            # prueba, no escribe
Rscript --vanilla scripts/porkbun_dns_resend.R --apply    # escribe
```

Necesita `PORKBUN_API_KEY` / `PORKBUN_SECRET_KEY` en `.Renviron` (crear en
<https://porkbun.com/account/api>) y `API ACCESS = ON` en el dominio. Borra la
llave cuando termines: sólo se usa una vez.

El script normaliza el host, es idempotente, no pisa un registro que ya exista
con otro valor, y **rechaza cualquier valor que traiga `...`**.

**c) A mano.** <https://porkbun.com/account/dns/hopdesk.app>.

**Porkbun pide el host RELATIVO** y le agrega `.hopdesk.app` solo. En este
esquema el panel de Resend ya los muestra relativos (`send.mail`,
`resend._domainkey.mail`), así que se pegan tal cual. Si alguna vez los
mostrara completos, quítales `.hopdesk.app`: pegarlos completos produce
`send.mail.hopdesk.app.hopdesk.app`, el error número uno de este trámite.

Detalles de Porkbun que importan:

- **No pongas comillas** alrededor de los valores TXT; Porkbun las agrega sola.
- TTL mínimo **600**. El panel de Resend dice "Auto"; 600 está bien.
- La clave DKIM es larga y **no debe llevar saltos de línea**.
- El dominio trae registros de *parking* (`ALIAS` raíz y `CNAME www`).
  **Déjalos**: no estorban.
- Si activas el **Email Forwarding gratuito de Porkbun** (para tener
  `soporte@hopdesk.app`), agrega sus `MX` y `SPF` **en la raíz**. No hay
  conflicto: lo de Resend vive bajo `.mail`.

5. De vuelta en Resend, `Verify DNS Records`. Con Porkbun suele tardar
   **menos de 15 minutos**; dale hasta unas horas antes de preocuparte.
6. Confirma desde tu máquina antes de seguir:

```powershell
nslookup -type=MX  send.mail.hopdesk.app
nslookup -type=TXT send.mail.hopdesk.app
nslookup -type=TXT resend._domainkey.mail.hopdesk.app
```

El dominio debe quedar en **`Verified`** (verde) en Resend. No sigas hasta eso.

---

## 4. Endurecer la entregabilidad (10 min, hazlo)

Una vez verificado el dominio (SPF y DKIM ya pasan), agrega **DMARC** en Porkbun.
Sin esto, Gmail y Outlook mandan invitaciones a Spam con más frecuencia.

Empieza en modo observación — un cuarto registro, mismo panel de DNS:

| Type | Host | Answer | TTL |
|---|---|---|---|
| `TXT` | `_dmarc` | `v=DMARC1; p=none; rua=mailto:suscripciones@networkslogistics.com.mx; pct=100; aspf=r; adkim=r` | `600` |

El `aspf=r` (alineación relajada) **no es opcional aquí**: tu remitente es
`@mail.hopdesk.app` pero el SPF se valida contra `send.mail.hopdesk.app`. Con
`aspf=s` (estricta) esa diferencia se consideraría desalineada y DMARC fallaría
en todos los correos.

El registro va en el host `_dmarc` (la raíz), **no** en `_dmarc.mail`: cuando un
receptor no encuentra DMARC en `mail.hopdesk.app` lo busca en el dominio
organizacional `hopdesk.app` y aplica ése. Un solo registro cubre el subdominio
de envío y cualquier correo futuro en la raíz.

Resend sugiere en su panel un `v=DMARC1; p=none;` pelón. El de arriba es el
mismo con `rua=` añadido, que es lo que hace la diferencia: sin esa dirección
**nadie te manda los reportes**, y entonces `p=none` no sirve de nada — su
único propósito es observar durante dos semanas antes de endurecer.

Déjalo en `p=none` **dos semanas**, revisa que no lleguen reportes de fallo, y
sólo entonces súbelo a `p=reject`.

---

## 5. Generar la API key y cargarla en la app

1. En Resend: `API Keys → Create API Key`.
   - Nombre: `hopdesk-prod`
   - Permiso: **`Sending access`** (no `Full access` — la app sólo envía)
   - Dominio: restringe al dominio que acabas de verificar
2. Copia la clave. **Se muestra una sola vez** y empieza con `re_`.
3. Edita `C:\Users\luisr\Antiguedad_App\.Renviron` (no está en git, es correcto):

```
RESEND_API_KEY=re_TuClaveRealAqui
RESEND_FROM_EMAIL=noreply@mail.hopdesk.app
```

> **Ya hecho (2026-10-27).** La clave está instalada y verificada contra la API
> real de Resend. Lo único que falta es que el dominio quede `Verified`.

### Producción envía, local nunca

`.Renviron` lleva la clave real porque **viaja dentro del bundle** de
shinyapps.io. Pero ese mismo archivo se lee en cada corrida local, así que
cualquier prueba en tu máquina que tocara una invitación habría mandado correo
de verdad a un cliente real.

Por eso `.Renviron.local` ahora trae `RESEND_API_KEY=sandbox`. Se lee **después**
de `.Renviron` y lo sobreescribe (`R/global.R`), y `.rscignore` lo excluye del
deploy — exactamente la misma mecánica que ya protege el bucket de desarrollo.
Resultado: **producción manda correo real, local se queda en sandbox**, sin que
tengas que acordarte de nada.

Si alguna vez necesitas probar entrega real desde tu máquina, comenta esa línea
en `.Renviron.local` y vuelve a ponerla al terminar.

`RESEND_FROM_EMAIL` **tiene que ser del dominio verificado**. Si dejas
`noreply@hopdesk.com` con una clave real, Resend devuelve `403` en cada envío.

4. Prueba en local antes de tocar producción:

```powershell
cd C:\Users\luisr\Antiguedad_App
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" --vanilla -e "readRenviron('.Renviron'); '%||%' <- function(a,b) if (is.null(a)) b else a; source('R/email_service.R'); str(email_service_status()); print(send_email('larivera@networkslogistics.com.mx','Prueba HopDesk','<p>Prueba de entrega.</p>', queue_on_fail=FALSE))"
```

Esperado: `mode` = `live` y `TRUE`. Revisa tu bandeja **y la carpeta de spam**.
(`queue_on_fail=FALSE` evita que una prueba fallida ensucie la cola real de S3.)

---

## 6. Desplegar a producción

`.Renviron` **viaja dentro del bundle** de shinyapps.io — así es como producción
ya recibe las credenciales de AWS. Por lo tanto **cambiar la clave exige
redesplegar**; reiniciar la app no basta.

La app de producción es **`Hopdesk`** (`rsconnect/shinyapps.io/1weh8r-luis-rivera/Hopdesk.dcf`),
no `antiguedad-app`:

```powershell
cd C:\Users\luisr\Antiguedad_App
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" --vanilla -e "rsconnect::deployApp(appDir='C:/Users/luisr/Antiguedad_App', appName='Hopdesk', account='1weh8r-luis-rivera', forceUpdate=TRUE)"
```

Antes de desplegar verifica que `.rscignore` **no** excluya `.Renviron`
(hoy sólo excluye `.Renviron.local`, que es lo correcto: ese trae el bucket de
desarrollo y nunca debe llegar a producción).

---

## 7. Verificar que quedó encendido

1. Entra a producción: <https://1weh8r-luis-rivera.shinyapps.io/Hopdesk/>
2. `Grupo → Invitaciones`. Arriba debe verse un banner **verde**:
   *"Envío de correo activo — Remitente: noreply@mail.hopdesk.app"*.
   Si sale amarillo, la clave no llegó al bundle → repite el paso 6.
3. Mándate una invitación a ti mismo a un correo **externo** (Gmail personal,
   no el corporativo: quieres probar entregabilidad real).
4. Debe salir el modal **"Invitación enviada"** con el enlace de activación.
5. Abre el correo → *Activar mi cuenta* → crea contraseña.
6. Debe aparecer el modal **"Cuenta activada"** con el **nombre de usuario**, y
   llegarte un segundo correo de bienvenida con ese mismo usuario.
7. Inicia sesión con ese usuario.
8. Borra la cuenta de prueba desde `Grupo → Usuarios`.

### Si hay correos atorados de antes

Todo lo que se intentó enviar mientras el correo estaba apagado quedó en
`hd-admin/email_queue.rds`. En `Grupo → Invitaciones` aparece la tarjeta
**"Cola de emails (N)"** con el botón **"Reintentar todo"**. Púlsalo una vez
después del despliegue.

---

## 8. Operación diaria

| Síntoma | Qué significa | Qué hacer |
|---|---|---|
| Banner **verde** | Envío activo | Nada |
| Banner **amarillo "Modo sandbox"** | `RESEND_API_KEY=sandbox` | Es el modo de desarrollo. En producción, revisa el despliegue |
| Banner **amarillo "Correo sin configurar"** | Falta la variable | La clave no llegó al bundle; redespliega |
| Banner **amarillo "Clave de API inválida"** | La clave no empieza con `re_` | Pegaste otro valor; regenérala |
| Modal rojo *"Invitación creada — correo NO enviado"* | La invitación existe, el correo no salió | Copia el enlace del modal y mándaselo al invitado por otro medio. Queda en la cola |
| `Cola de emails` crece | Fallos repetidos | Revisa `resend.com → Logs`. Causas típicas: dominio desverificado, cuota mensual agotada, buzón inexistente |

**Datos operativos que conviene tener presentes:**

- El enlace de invitación **expira en 48 horas** (`create_invite(expires_hours = 48)`).
  Pasado eso hay que reenviar desde la misma pestaña.
- El token es de **un solo uso**.
- El **nombre de usuario no es el correo**: se genera del nombre para mostrar.
  Por eso se muestra en un modal al activar y se manda por correo; si el
  invitado lo pierde, lo ves en `Grupo → Usuarios`.
- Los avisos de límite de usuarios se disparan solos cuando quedan **≤2 lugares**.

---

## 9. Resumen de costos

| Concepto | Costo | Estado |
|---|---|---|
| `hopdesk.app` (Porkbun, 2 años) | ya pagado | ✅ Hasta 2028-10-27 |
| Resend **Free** | **$0** | ⬅ **Lo que necesitas ahora.** 3,000 correos/mes contra ~150 de consumo real |
| DNS en Porkbun | $0 | Incluido con el dominio |
| Resend **Pro** | $20 USD/mes | Sólo al pasar de 100 correos/día o si necesitas logs de entrega de más de 30 días |
| Dominio propio en shinyapps.io | $299 USD/mes | ❌ Descartado 2026-10-27, desproporcionado |

**Costo adicional para encender el correo hoy: $0.** Lo único que falta es
tiempo: ~20 min de captura + la espera de propagación DNS.

---

## 10. Qué cambió en el código para esto (2026-09-30)

- `R/email_service.R`
  - `email_service_status()` — clasifica la configuración (`live` / `sandbox` /
    `unconfigured` / `invalid`) y alimenta el badge de la UI.
  - `send_email()` — **una `RESEND_API_KEY` vacía ya no se trata como sandbox.**
    Antes devolvía `TRUE` y la app decía "Invitación enviada" para un correo que
    nunca se mandó ni se encoló. Ahora falla, encola y explica por qué.
  - Valida el destinatario antes de gastar una llamada a la API.
  - La cola guarda `last_error` (se calculaba y se tiraba).
  - `retry_all_queued_emails()` — drena la cola completa de una vez.
  - `email_welcome_html()` — correo de bienvenida con el nombre de usuario.
- `R/tiers_module.R`
  - Badge de estado del servicio en la pestaña Invitaciones.
  - Al invitar, modal que dice la verdad y **siempre muestra el enlace**, para
    poder entregarlo a mano si el correo falla.
  - El audit log registra si el correo salió o no.
  - Botón "Reintentar todo" en la cola.
- `R/persistence.R`
  - `hd_staff_emails_with_perm()` — nuevo. `get_limit_alert_recipients()` y la
    solicitud de "más usuarios" filtraban por `hd_staff$can_manage_invites`,
    que **no es una columna** de `usuarios.rds` sino un permiso derivado del
    tier. `isTRUE(NULL)` es `FALSE`, así que esas listas de destinatarios
    siempre salían vacías: **ningún aviso de límite ha llegado nunca a nadie.**
  - El filtro de contactos usaba `isTRUE()` sobre un vector, que colapsa a un
    solo `FALSE` — descartaba a todos los contactos de alerta.
- `app.R` — al activar la cuenta, modal persistente con el nombre de usuario
  (antes sólo un aviso de 6 segundos) + correo de bienvenida.
### 2026-10-27 — al conectar la API real

- **`R/email_service.R` — bug que habría tirado el 100 % de los correos.**
  El cuerpo JSON se construía con `to = list(to)` y `auto_unbox = FALSE`, lo
  que producía `{"to":[["x@y.com"]]}` — un array anidado. La API responde
  `422 The 'to' field must be a 'string'`. **Ningún envío habría salido en
  producción.** La suite no lo veía porque el modo sandbox retorna antes de
  serializar nada. Se extrajo `.resend_body()` para poder afirmarlo sin red, y
  se corrigió con `I(to)` → `{"to":["x@y.com"]}`.
- Remitente por defecto: `noreply@hopdesk.com` (dominio ajeno) →
  `noreply@mail.hopdesk.app`.
- `.Renviron.local` fija `RESEND_API_KEY=sandbox` para que **local nunca mande
  correo real** mientras producción sí (ver sección 5).
- **`From` con nombre visible.** La primera prueba llegó a Gmail firmada como
  un escueto "onboarding": sin *display name*, el cliente de correo muestra
  sólo la parte local de la dirección. Ahora va
  `HopDesk <noreply@mail.hopdesk.app>`, configurable con `RESEND_FROM_NAME`.
- **`Reply-To` por mensaje, no global.** `hopdesk.app` no tiene MX
  (verificado 2026-10-27): ninguna dirección del dominio puede recibir, así
  que la respuesta de un invitado confundido rebota al vacío. Apuntarla a un
  buzón corporativo ajeno tampoco sirve: expone ese buzón a cada invitado y
  no es de quien mandó la invitación. La invitación ahora pone como
  `Reply-To` **el correo de quien la envió** (`send_email(reply_to=)`), que es
  un humano al que el destinatario puede preguntarle "¿esto qué es?".
  `RESEND_REPLY_TO` sigue existiendo como respaldo global y está vacío a
  propósito. Para apagarlo desde `.Renviron.local` usa `none`, **no** un
  valor vacío: `readRenviron()` ignora las líneas `NOMBRE=` sin valor.
- **Plantillas a prueba de clientes de correo.** Sin `<style>` (varios
  clientes borran el `<head>`), sin `<svg>` (Gmail lo elimina) y sin logo en
  imagen: las imágenes vienen bloqueadas por defecto, y un logotipo que sale
  como recuadro gris es la peor primera impresión para un correo que pide
  hacer clic y escribir una contraseña. El sello es texto con la tipografía
  y los colores de la app (`#07090d` / `#b49444`), los mismos de la página de
  activación a la que se llega un segundo después.
- **Escape de HTML.** El nombre del invitado sale de un formulario y viaja en
  un correo firmado por un dominio en el que el proveedor del destinatario ya
  aprendió a confiar. `.email_esc()` lo escapa en las tres plantillas.
- **Parte de texto plano.** Los correos sólo-HTML puntúan peor en los filtros
  de spam y algunos clientes corporativos sólo renderizan el texto.
  `.html_to_text()` la deriva del propio HTML — no puede desfasarse — y
  **conserva las URLs**: una invitación cuyo texto plano perdiera el enlace no
  serviría de nada.

### Dónde se verificó

Contra la API real, no contra mocks. Las tres plantillas se entregaron a
**Primary de Gmail, no a Spam**, antes siquiera de tener el dominio verificado.

### 2026-09-30 — la implementación original

- `tests/test_email_service_live.R` — 63 aserciones sobre todo lo anterior,
  registradas en `tests/_run_saas.R`. Suite completa: **1,751 passed, 0 failed**.
