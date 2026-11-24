"""
hopdesk-avisos-disparo (T80): starts the weekly Panorama email on time.

GitHub's cron started the email 4.7 to 11.5 hours late (2026-11-20..23). AWS
EventBridge Scheduler, in Mexico time, invokes this function; it starts
.github/workflows/avisos.yml by workflow_dispatch, which GitHub runs in
seconds.

Event: {"modo": "lunes" | "viernes" | "archivo", "alertar": false}
  - It looks for this week's runs of the workflow whose name is
    "avisos <modo> real" (the workflow's run-name).
  - One finished with success: the email already went out; nothing to do.
  - One is queued or running: wait for it; nothing to do.
  - Otherwise: dispatch with modo and prueba=false.
  - alertar=true (the 17:00/17:50 check): when there is no success, it also
    publishes an alert to the SNS topic, then dispatches (a last try).
Starting it more than once is safe: the job never resends (its
correos_enviados.rds) and skips a client whose email already reached
everyone (job_ya_salio).

Environment:
  REPO          owner/name, e.g. luisriveraCT/HopDesk
  WORKFLOW      avisos.yml
  REF           the branch to run, master
  TOKEN_PARAM   name of the SSM SecureString holding a fine-grained GitHub
                token (this repo only, Actions read and write). This code
                reads it and never logs or returns it.
  SNS_TOPIC_ARN the alert topic (only needed for alertar)

An error raises, so the Scheduler's retry policy tries again.
The log carries the mode, counts and the decision only.
"""
import json
import os
import urllib.error
import urllib.request
from datetime import datetime, timedelta, timezone

MODOS = ("lunes", "viernes", "archivo")
# Mexico has had no daylight saving time since 2022: always UTC-6, as the
# workflow's crons and the job (UMBRAL_TZ) assume.
MX = timezone(timedelta(hours=-6))
API = "https://api.github.com"


def inicio_semana_utc(ahora):
    """Monday 00:00 Mexico of the ISO week of `ahora`, in UTC: the job's
    week (umbral_semana is %G-W%V of the Mexico day)."""
    mx = ahora.astimezone(MX)
    lunes = (mx - timedelta(days=mx.weekday())).replace(hour=0, minute=0, second=0, microsecond=0)
    return lunes.astimezone(timezone.utc)


def decidir(corridas, modo):
    """'listo' | 'en_curso' | 'disparar', from the workflow's runs of this
    week (dicts with name, status, conclusion)."""
    nombre = "avisos %s real" % modo
    mias = [c for c in corridas if c.get("name") == nombre]
    if any(c.get("conclusion") == "success" for c in mias):
        return "listo"
    if any(c.get("status") in ("queued", "in_progress", "waiting", "requested", "pending") for c in mias):
        return "en_curso"
    return "disparar"


def _pedir(metodo, url, token, cuerpo=None):
    datos = json.dumps(cuerpo).encode("utf-8") if cuerpo is not None else None
    req = urllib.request.Request(url, data=datos, method=metodo, headers={
        "Accept": "application/vnd.github+json",
        "Authorization": "Bearer " + token,
        "X-GitHub-Api-Version": "2022-11-28",
        "User-Agent": "hopdesk-avisos-disparo",
    })
    try:
        with urllib.request.urlopen(req, timeout=15) as r:
            texto = r.read().decode("utf-8")
            return r.status, (json.loads(texto) if texto else None)
    except urllib.error.HTTPError as e:
        # The status only: GitHub's body could echo request details.
        raise RuntimeError("GitHub respondió HTTP %d en %s" % (e.code, metodo)) from None


def corridas_de_la_semana(token, repo, workflow, desde):
    url = "%s/repos/%s/actions/workflows/%s/runs?per_page=100&created=%%3E%%3D%s" % (
        API, repo, workflow, desde.strftime("%Y-%m-%dT%H:%M:%SZ"))
    _, cuerpo = _pedir("GET", url, token)
    return (cuerpo or {}).get("workflow_runs", [])


def disparar(token, repo, workflow, ref, modo):
    url = "%s/repos/%s/actions/workflows/%s/dispatches" % (API, repo, workflow)
    estado, _ = _pedir("POST", url, token, {"ref": ref, "inputs": {"modo": modo, "prueba": "false"}})
    if estado != 204:
        raise RuntimeError("workflow_dispatch devolvió HTTP %d" % estado)


def leer_token(nombre):
    import boto3
    r = boto3.client("ssm").get_parameter(Name=nombre, WithDecryption=True)
    return r["Parameter"]["Value"]


def alertar(topic_arn, modo, ahora):
    import boto3
    dia = ahora.astimezone(MX).strftime("%Y-%m-%d %H:%M")
    boto3.client("sns").publish(
        TopicArn=topic_arn,
        Subject="HopDesk: el correo de %s no ha salido" % modo,
        Message=("A las %s (hora de México) no hay una corrida real exitosa del correo de %s esta semana.\n"
                 "Ya se pidió un último intento. Revisa la pestaña Actions del repositorio, "
                 "workflow 'Avisos semanales (D)'.") % (dia, modo))


def lambda_handler(event, context, ahora=None, deps=None):
    """deps (tests only): dict with leer_token, corridas, disparar, alertar."""
    modo = (event or {}).get("modo")
    if modo not in MODOS:
        raise ValueError("modo debe ser lunes, viernes o archivo")
    con_alerta = bool((event or {}).get("alertar", False))
    ahora = ahora or datetime.now(timezone.utc)
    d = deps or {}
    repo, workflow = os.environ["REPO"], os.environ.get("WORKFLOW", "avisos.yml")
    ref = os.environ.get("REF", "master")

    token = d.get("leer_token", leer_token)(os.environ["TOKEN_PARAM"])
    corridas = d.get("corridas", corridas_de_la_semana)(token, repo, workflow, inicio_semana_utc(ahora))
    decision = decidir(corridas, modo)
    alerta = False
    if decision != "listo" and con_alerta:
        d.get("alertar", alertar)(os.environ["SNS_TOPIC_ARN"], modo, ahora)
        alerta = True
    if decision == "disparar":
        d.get("disparar", disparar)(token, repo, workflow, ref, modo)
    resultado = {"modo": modo, "decision": decision, "alerta": alerta, "corridas_semana": len(corridas)}
    print(json.dumps(resultado))
    return resultado
