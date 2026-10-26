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
| **Envío real de correo** | ❌ **Apagado** — `RESEND_API_KEY=sandbox` |
| **Cuenta de Resend** | ❌ No existe |
| **Dominio verificado para enviar** | ❌ No existe |

El único bloqueo real es **el dominio**. Sin un dominio verificado en Resend,
la API responde `403` a cualquier destinatario que no sea el correo del dueño
de la cuenta. Este runbook resuelve eso.

> `hopdesk.com` **no es tuyo**: está estacionado en Afternic (está en venta).
> El código lo trae de fábrica como remitente por defecto, pero es un
> *placeholder*, no un dominio que controles.

---

## 1. Decidir el dominio remitente (la única decisión real)

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

### Opción A — Dominio propio de HopDesk *(recomendada)*

Registra un dominio para el producto y verifica en Resend el **subdominio**
`send.<tu-dominio>`.

- Sondeo DNS del 2026-09-30 (indicativo, confírmalo en el registrador):
  `hopdesk.mx`, `hopdesk.com.mx`, `hopdesk.app` y `gethopdesk.com` **no tienen
  NS** → probablemente libres. `hopdesk.io` y `hopdesk.com` están tomados.
- Costo: ~**$15–40 USD/año** según extensión (`.mx` suele ir arriba de `.com`).
- Registrador sugerido: el mismo **GoDaddy** donde ya tienes
  `networkslogistics.com.mx` (los NS son `ns23/ns24.domaincontrol.com`), para
  administrar todo el DNS en un solo panel.

**Por qué es la recomendada:** HopDesk se está vendiendo como producto a
terceros. Una invitación que llega desde `@networkslogistics.com.mx` le dice al
cliente que está entrando a un sistema interno de Networks, no a una
plataforma. Además aísla la reputación de envío: si mañana un cliente marca una
invitación como spam, eso no toca el correo corporativo.

### Opción B — Subdominio de Networks *(salida rápida)*

Si necesitas mandar invitaciones **esta semana** sin esperar la compra:
verifica `hopdesk.networkslogistics.com.mx` en Resend.

- Costo: **$0**. El dominio ya es tuyo y el DNS ya está en GoDaddy.
- El SPF/DKIM del subdominio es **independiente** del raíz: Exchange Online no
  se entera y no se toca.
- Remitente: `noreply@hopdesk.networkslogistics.com.mx`.

Es perfectamente válido y migrable después (Resend permite 3 dominios en el
plan gratuito, así que puedes tener los dos y cambiar `RESEND_FROM_EMAIL`
cuando el dominio propio esté listo).

**Recomendación concreta:** arranca con **B** hoy para no frenar las
invitaciones, y registra el dominio de **A** en paralelo. La migración después
es cambiar una línea en `.Renviron` y redesplegar.

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
2. Escribe el subdominio de envío, **no el raíz**:
   - Opción A → `send.hopdesk.mx`
   - Opción B → `hopdesk.networkslogistics.com.mx`
3. Región: elige **`us-east-1`** (mismo continente que tus usuarios y que el
   bucket S3; menor latencia y nada que reconfigurar después).
4. Resend te muestra **tres registros DNS**. Son, en forma:

| Tipo | Nombre / Host | Valor | Prioridad |
|---|---|---|---|
| `MX` | `send` | `feedback-smtp.us-east-1.amazonses.com` | `10` |
| `TXT` | `send` | `v=spf1 include:amazonses.com ~all` | — |
| `TXT` | `resend._domainkey` | `p=MIGfMA0GCSq...` (clave larga) | — |

> **Copia los valores del panel, no de esta tabla.** La clave DKIM es única por
> dominio y cambia. La tabla es para que reconozcas la forma de lo que vas a pegar.

### Cómo capturarlos en GoDaddy

`Mis productos → DNS → Administrar zonas → <tu dominio> → Agregar registro`.

GoDaddy pide el **host relativo**, no el nombre completo. La regla:

- Si verificaste `send.hopdesk.mx` en el dominio `hopdesk.mx`:
  host `send` y `resend._domainkey.send`
- Si verificaste `hopdesk.networkslogistics.com.mx` dentro de
  `networkslogistics.com.mx`:
  host `send.hopdesk` y `resend._domainkey.hopdesk`

Es decir: **quítale el dominio raíz al nombre que muestra Resend y pega el
resto.** Este es el paso donde falla el 90 % de las verificaciones.

- TTL: deja el predeterminado (1 hora).
- **No pongas comillas** alrededor del valor TXT en GoDaddy; las agrega solo.
- Si el registrador ofrece "proxy"/CDN, déjalo **desactivado** (es DNS puro).

5. De vuelta en Resend, `Verify DNS Records`. Normalmente tarda **15 minutos**;
   puede tardar hasta 24–48 h si GoDaddy es lento en propagar.
6. Confirma desde tu máquina antes de seguir:

```powershell
nslookup -type=TXT resend._domainkey.send.hopdesk.mx
nslookup -type=MX  send.hopdesk.mx
nslookup -type=TXT send.hopdesk.mx
```

El dominio debe quedar en **`Verified`** (verde) en Resend. No sigas hasta eso.

---

## 4. Endurecer la entregabilidad (10 min, hazlo)

Una vez verificado el dominio (SPF y DKIM ya pasan), agrega **DMARC** en GoDaddy.
Sin esto, Gmail y Outlook mandan invitaciones a Spam con más frecuencia.

Empieza en modo observación:

| Tipo | Host | Valor |
|---|---|---|
| `TXT` | `_dmarc.send` *(o `_dmarc.send.hopdesk`)* | `v=DMARC1; p=none; rua=mailto:suscripciones@networkslogistics.com.mx; pct=100; aspf=r; adkim=r` |

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
RESEND_FROM_EMAIL=noreply@send.hopdesk.mx
```

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
   *"Envío de correo activo — Remitente: noreply@send.hopdesk.mx"*.
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

| Concepto | Costo | Cuándo |
|---|---|---|
| Resend Free | **$0** | Ahora. Suficiente para el volumen actual |
| Dominio `.mx` / `.com` (opción A) | **~$15–40 USD/año** | Opcional, recomendado para el producto |
| Subdominio de Networks (opción B) | **$0** | Si arrancas hoy |
| Resend Pro | **$20 USD/mes** | Sólo al pasar de 100 correos/día o necesitar logs >30 días |

**Arranque mínimo viable: $0.**

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
- `tests/test_email_service_live.R` — 43 aserciones nuevas sobre todo lo anterior,
  registradas en `tests/_run_saas.R`. Suite completa: **1,731 passed, 0 failed**.
