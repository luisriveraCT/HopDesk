"""
hopdesk-avisos-disparo (T80): starts the weekly Panorama email on time.

GitHub's cron started the email 4.7 to 11.5 hours late (2026-11-20..23). AWS
EventBridge Scheduler, in Mexico time, invokes this function; it starts
.github/workflows/avisos.yml by workflow_dispatch, which GitHub runs in
seconds.

Event: {"modo": "lunes" | "viernes" | "archivo"}
  - It looks for this week's runs of the workflow whose name is
    "avisos <modo> real" (the workflow's run-name).
  - One finished with success: the email already went out; nothing to do.
  - One is queued or running: wait for it; nothing to do.
  - Otherwise: dispatch with modo and prueba=false.
  The 17:00 (Monday) and 17:50 (Friday) schedules are the same call: a last
  try. There is no email alert (T80b, Luis): when the email has not gone
  out, HopDesk itself says so in Panorama (R/avisos_correo_vigia.R).
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


def lambda_handler(event, context, ahora=None, deps=None):
    """deps (tests only): dict with leer_token, corridas, disparar."""
    modo = (event or {}).get("modo")
    if modo not in MODOS:
        raise ValueError("modo debe ser lunes, viernes o archivo")
    ahora = ahora or datetime.now(timezone.utc)
    d = deps or {}
    repo, workflow = os.environ["REPO"], os.environ.get("WORKFLOW", "avisos.yml")
    ref = os.environ.get("REF", "master")

    token = d.get("leer_token", leer_token)(os.environ["TOKEN_PARAM"])
    corridas = d.get("corridas", corridas_de_la_semana)(token, repo, workflow, inicio_semana_utc(ahora))
    decision = decidir(corridas, modo)
    if decision == "disparar":
        d.get("disparar", disparar)(token, repo, workflow, ref, modo)
    resultado = {"modo": modo, "decision": decision, "corridas_semana": len(corridas)}
    print(json.dumps(resultado))
    return resultado
