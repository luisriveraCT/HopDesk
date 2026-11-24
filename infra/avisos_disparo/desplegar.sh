#!/usr/bin/env bash
# =============================================================================
# infra/avisos_disparo/desplegar.sh (T80, T80b): the AWS trigger of the
# weekly email. EventBridge Scheduler (Mexico time) -> Lambda
# hopdesk-avisos-disparo -> workflow_dispatch of .github/workflows/avisos.yml.
#
#   bash infra/avisos_disparo/desplegar.sh --param <ssm name>             # preview
#   bash infra/avisos_disparo/desplegar.sh --param <ssm name> --aplicar   # writes
#
# Without --aplicar it only reads (describe/get) and prints what it would
# create or update. With --aplicar it does it. Running it again changes only
# what differs: it is idempotent.
#
# The GitHub token NEVER passes through this script. It takes the NAME of an
# SSM SecureString that already exists; whoever holds the token creates it
# beforehand, typing it by hand (fine-grained token: this repo only,
# "Actions: read and write", expiring in at most a year), for example:
#     aws ssm put-parameter --type SecureString --name /hopdesk/avisos/github_token --value "$(read -rs t; echo "$t")"
# The script checks that the parameter exists without decrypting it.
#
# No email alert (T80b, Luis): when the email has not gone out, HopDesk
# says so in Panorama (R/avisos_correo_vigia.R). No SNS here.
#
# Creates (region us-east-1, where the token lives; --region <r> to change it.
# AWS_REGION is NOT read: CloudShell sets it to the console's region):
#   - IAM role hopdesk-avisos-disparo-lambda: logs, read THAT parameter
#   - Lambda hopdesk-avisos-disparo (python3.12, lambda_function.py)
#   - IAM role hopdesk-avisos-disparo-scheduler: invoke THAT Lambda
#   - Schedule group hopdesk-avisos and its 11 schedules (below)
# Touches nothing else: not the job's OIDC role, not the bucket.
# =============================================================================
set -euo pipefail

PARAM=""; APLICAR=0; REGION="us-east-1"
while [ $# -gt 0 ]; do
  case "$1" in
    --param)   PARAM="${2:-}"; shift 2 ;;
    --aplicar) APLICAR=1; shift ;;
    --region)  REGION="${2:-}"; shift 2 ;;
    *) echo "argumento desconocido: $1" >&2; exit 2 ;;
  esac
done
[ -n "$PARAM" ] || { echo "uso: $0 --param <nombre ssm> [--aplicar]" >&2; exit 2; }
case "$PARAM" in /*) ;; *) echo "--param es el NOMBRE del parámetro (empieza con /), nunca el token" >&2; exit 2 ;; esac

REPO="luisriveraCT/HopDesk"; WORKFLOW="avisos.yml"; REF="master"
FUNCION="hopdesk-avisos-disparo"
ROL_LAMBDA="hopdesk-avisos-disparo-lambda"
ROL_SCHED="hopdesk-avisos-disparo-scheduler"
GRUPO="hopdesk-avisos"
ZONA="America/Mexico_City"
DIR="$(cd "$(dirname "$0")" && pwd)"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
# python3 where only python3 exists (AWS CloudShell), python elsewhere.
PY="$(command -v python3 || command -v python || true)"
[ -n "$PY" ] || { echo "falta python3 (o python)" >&2; exit 2; }

# name|cron (Mexico time)|event. The 17:00 Monday and 17:50 Friday ones are
# the last try (same call: it does nothing if the email already went out).
HORARIOS=(
  "lunes-0800|cron(0 8 ? * MON *)|{\"modo\":\"lunes\"}"
  "lunes-0900|cron(0 9 ? * MON *)|{\"modo\":\"lunes\"}"
  "lunes-1100|cron(0 11 ? * MON *)|{\"modo\":\"lunes\"}"
  "lunes-ultimo-1700|cron(0 17 ? * MON *)|{\"modo\":\"lunes\"}"
  "viernes-1700|cron(0 17 ? * FRI *)|{\"modo\":\"viernes\"}"
  "viernes-1720|cron(20 17 ? * FRI *)|{\"modo\":\"viernes\"}"
  "viernes-1740|cron(40 17 ? * FRI *)|{\"modo\":\"viernes\"}"
  "viernes-ultimo-1750|cron(50 17 ? * FRI *)|{\"modo\":\"viernes\"}"
  "archivo-0030|cron(30 0 ? * SAT *)|{\"modo\":\"archivo\"}"
  "archivo-0300|cron(0 3 ? * SAT *)|{\"modo\":\"archivo\"}"
  "archivo-0600|cron(0 6 ? * SAT *)|{\"modo\":\"archivo\"}"
)

aws_() { aws --region "$REGION" --output json "$@"; }
existe() { "$@" >/dev/null 2>&1; }
hacer() {   # hacer "<what>" cmd...: runs only with --aplicar
  local que="$1"; shift
  if [ "$APLICAR" = 1 ]; then echo "  HACIENDO: $que"; "$@" >/dev/null; else echo "  HARÍA:    $que"; fi
}

echo "== Modo: $([ "$APLICAR" = 1 ] && echo APLICAR || echo 'VISTA PREVIA (no escribe nada)') · región $REGION"
CUENTA="$(aws_ sts get-caller-identity --query Account --output text)"
echo "== Cuenta $CUENTA"

# ── 0. The token's parameter must exist (never decrypted here) ──────────────
TIPO="$(aws_ ssm describe-parameters --parameter-filters "Key=Name,Values=$PARAM" --query 'Parameters[0].Type' --output text 2>/dev/null || true)"
if [ "$TIPO" != "SecureString" ]; then
  echo "FALTA: el parámetro $PARAM no existe o no es SecureString. Créalo primero (ver el encabezado)." >&2
  [ "$APLICAR" = 1 ] && exit 1
fi
PARAM_ARN="arn:aws:ssm:$REGION:$CUENTA:parameter${PARAM}"

# ── 1. The Lambda's role ────────────────────────────────────────────────────
cat > "$TMP/confia_lambda.json" <<'JSON'
{"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":"lambda.amazonaws.com"},"Action":"sts:AssumeRole"}]}
JSON
cat > "$TMP/permisos_lambda.json" <<JSON
{"Version":"2012-10-17","Statement":[
 {"Effect":"Allow","Action":"ssm:GetParameter","Resource":"$PARAM_ARN"}]}
JSON
if existe aws_ iam get-role --role-name "$ROL_LAMBDA"; then echo "  YA EXISTE: rol $ROL_LAMBDA"
else hacer "crear rol $ROL_LAMBDA" aws_ iam create-role --role-name "$ROL_LAMBDA" --assume-role-policy-document "file://$TMP/confia_lambda.json"; fi
hacer "poner la política del rol $ROL_LAMBDA (leer $PARAM)" \
  aws_ iam put-role-policy --role-name "$ROL_LAMBDA" --policy-name disparo --policy-document "file://$TMP/permisos_lambda.json"
hacer "adjuntar AWSLambdaBasicExecutionRole (logs)" \
  aws_ iam attach-role-policy --role-name "$ROL_LAMBDA" --policy-arn arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole

# ── 2. The Lambda ───────────────────────────────────────────────────────────
"$PY" -c "import zipfile,sys; z=zipfile.ZipFile(sys.argv[1],'w'); z.write(sys.argv[2],'lambda_function.py'); z.close()" \
  "$TMP/funcion.zip" "$DIR/lambda_function.py"
ENTORNO="Variables={REPO=$REPO,WORKFLOW=$WORKFLOW,REF=$REF,TOKEN_PARAM=$PARAM}"
FUNCION_ARN="arn:aws:lambda:$REGION:$CUENTA:function:$FUNCION"
if existe aws_ lambda get-function --function-name "$FUNCION"; then
  echo "  YA EXISTE: Lambda $FUNCION"
  hacer "actualizar el código de $FUNCION" aws_ lambda update-function-code --function-name "$FUNCION" --zip-file "fileb://$TMP/funcion.zip"
  [ "$APLICAR" = 1 ] && aws_ lambda wait function-updated --function-name "$FUNCION"
  hacer "actualizar la configuración de $FUNCION" aws_ lambda update-function-configuration --function-name "$FUNCION" \
    --runtime python3.12 --handler lambda_function.lambda_handler --timeout 30 --environment "$ENTORNO"
else
  [ "$APLICAR" = 1 ] && sleep 10   # a new role takes a few seconds to be assumable
  hacer "crear Lambda $FUNCION (python3.12, 30 s)" aws_ lambda create-function --function-name "$FUNCION" \
    --runtime python3.12 --handler lambda_function.lambda_handler --timeout 30 \
    --role "arn:aws:iam::$CUENTA:role/$ROL_LAMBDA" --zip-file "fileb://$TMP/funcion.zip" --environment "$ENTORNO"
fi

# ── 3. The Scheduler's role ─────────────────────────────────────────────────
cat > "$TMP/confia_sched.json" <<JSON
{"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":"scheduler.amazonaws.com"},"Action":"sts:AssumeRole",
 "Condition":{"StringEquals":{"aws:SourceAccount":"$CUENTA"}}}]}
JSON
cat > "$TMP/permisos_sched.json" <<JSON
{"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":"lambda:InvokeFunction","Resource":"$FUNCION_ARN"}]}
JSON
if existe aws_ iam get-role --role-name "$ROL_SCHED"; then echo "  YA EXISTE: rol $ROL_SCHED"
else hacer "crear rol $ROL_SCHED" aws_ iam create-role --role-name "$ROL_SCHED" --assume-role-policy-document "file://$TMP/confia_sched.json"; fi
hacer "poner la política del rol $ROL_SCHED (invocar $FUNCION)" \
  aws_ iam put-role-policy --role-name "$ROL_SCHED" --policy-name invocar --policy-document "file://$TMP/permisos_sched.json"

# ── 4. Schedule group and schedules ─────────────────────────────────────────
if existe aws_ scheduler get-schedule-group --name "$GRUPO"; then echo "  YA EXISTE: grupo $GRUPO"
else hacer "crear grupo de horarios $GRUPO" aws_ scheduler create-schedule-group --name "$GRUPO"; fi
for h in "${HORARIOS[@]}"; do
  IFS='|' read -r nombre expr evento <<< "$h"
  "$PY" -c "import json,sys; print(json.dumps({'Arn':sys.argv[1],'RoleArn':sys.argv[2],'Input':sys.argv[3],
'RetryPolicy':{'MaximumRetryAttempts':3,'MaximumEventAgeInSeconds':1800}}))" \
    "$FUNCION_ARN" "arn:aws:iam::$CUENTA:role/$ROL_SCHED" "$evento" > "$TMP/objetivo_$nombre.json"
  args=(--name "$nombre" --group-name "$GRUPO" --schedule-expression "$expr" --schedule-expression-timezone "$ZONA"
        --flexible-time-window Mode=OFF --target "file://$TMP/objetivo_$nombre.json" --state ENABLED)
  if existe aws_ scheduler get-schedule --name "$nombre" --group-name "$GRUPO"; then
    hacer "actualizar horario $nombre · $expr $ZONA · $evento" aws_ scheduler update-schedule "${args[@]}"
  else
    hacer "crear horario $nombre · $expr $ZONA · $evento" aws_ scheduler create-schedule "${args[@]}"
  fi
done

echo "== Listo ($([ "$APLICAR" = 1 ] && echo aplicado || echo 'vista previa: nada se escribió'))."
echo "   Prueba sin esperar al lunes (dispara de verdad solo si esta semana no salió):"
echo "   aws lambda invoke --function-name $FUNCION --payload '{\"modo\":\"archivo\"}' --cli-binary-format raw-in-base64-out salida.json"
