#!/usr/bin/env bash
# T80/T80b: desplegar.sh against a fake `aws` that logs every call (no AWS).
#   bash infra/avisos_disparo/test_desplegar.sh
# Checks: preview only reads; --aplicar creates everything once; a second
# --aplicar over existing resources creates nothing; the token never is an
# argument; a value that is not a parameter name is refused; no SNS or email
# alert anywhere (T80b).
set -u
DIR="$(cd "$(dirname "$0")" && pwd)"
T="$(mktemp -d)"; trap 'rm -rf "$T"' EXIT
mkdir -p "$T/bin"
cat > "$T/bin/aws" <<'FAKE'
#!/usr/bin/env bash
echo "$*" >> "$FAKE_LOG"
args="$*"
case "$args" in
  *"sts get-caller-identity"*) echo 111122223333 ;;
  *"ssm describe-parameters"*) echo SecureString ;;
  *"scheduler create-schedule --name"*)
    # FAKE_FALLA_ASUMIR=n: the first n say what IAM says while a new role
    # propagates; FAKE_OTRO_ERROR=1: any other error.
    n=$(cat "$FAKE_LOG.asumir" 2>/dev/null || echo 0)
    if [ "$n" -lt "${FAKE_FALLA_ASUMIR:-0}" ]; then
      echo $((n + 1)) > "$FAKE_LOG.asumir"
      echo "An error occurred (ValidationException) when calling the CreateSchedule operation: The execution role you provide must allow AWS EventBridge Scheduler to assume the role." >&2
      exit 254
    fi
    if [ "${FAKE_OTRO_ERROR:-0}" = 1 ]; then echo "An error occurred (AccessDeniedException): no" >&2; exit 254; fi
    exit 0 ;;
  *" get-"*) [ "${FAKE_EXISTE:-0}" = 1 ] && exit 0 || exit 254 ;;
  *) exit 0 ;;
esac
FAKE
chmod +x "$T/bin/aws"
export PATH="$T/bin:$PATH" FAKE_LOG="$T/log"

pass=0; fail=0
ok() { if eval "$2"; then echo " PASS: $1"; pass=$((pass+1)); else echo " FAIL: $1"; fail=$((fail+1)); fi; }
escrituras() { grep -cE ' (create-|put-|attach-|update-|subscribe|delete-)' "$FAKE_LOG" || true; }

: > "$FAKE_LOG"
bash "$DIR/desplegar.sh" --param /hopdesk/avisos/github_token > "$T/out1" 2>&1; st=$?
ok "vista previa sale 0" "[ $st = 0 ]"
ok "vista previa: ninguna llamada que escriba" "[ \"\$(escrituras)\" = 0 ]"
ok "vista previa: dice HARÍA y lista los 11 horarios" "[ \"\$(grep -c 'HARÍA:    crear horario' $T/out1)\" = 11 ]"

: > "$FAKE_LOG"
AWS_REGION=us-east-2 bash "$DIR/desplegar.sh" --param /hopdesk/avisos/github_token --aplicar > "$T/out2" 2>&1; st=$?
ok "aplicar sale 0" "[ $st = 0 ]"
ok "siempre us-east-1 (donde vive el token), aunque AWS_REGION diga otra (CloudShell)" \
  "! grep -q 'us-east-2' $FAKE_LOG && [ \"\$(grep -c -- '--region us-east-1' $FAKE_LOG)\" = \"\$(wc -l < $FAKE_LOG)\" ]"
ok "aplicar crea 2 roles, Lambda, grupo y 11 horarios" \
  "[ \"\$(grep -c 'iam create-role' $FAKE_LOG)\" = 2 ] && grep -q 'lambda create-function' $FAKE_LOG && grep -q 'create-schedule-group' $FAKE_LOG && [ \"\$(grep -c 'scheduler create-schedule --name' $FAKE_LOG)\" = 11 ]"
ok "los horarios van en hora de México, sin ventana flexible" \
  "[ \"\$(grep 'scheduler create-schedule --name' $FAKE_LOG | grep -c 'America/Mexico_City.*Mode=OFF')\" = 11 ]"
ok "la Lambda recibe el NOMBRE del parámetro, nunca un token" \
  "grep 'lambda create-function' $FAKE_LOG | grep -q 'TOKEN_PARAM=/hopdesk/avisos/github_token' && ! grep -qiE 'ghp_|github_pat_' $FAKE_LOG"
ok "el rol de la Lambda solo lee ESE parámetro" \
  "grep -q 'put-role-policy --role-name hopdesk-avisos-disparo-lambda' $FAKE_LOG"
ok "T80b: nada de SNS ni de correo (ni en las llamadas ni en el script)" \
  "! grep -qi 'sns' $FAKE_LOG && ! grep -qiE 'aws_ sns|sns:|subscribe|--correo' $DIR/desplegar.sh"
ok "T80b: los de 17:00 lunes y 17:50 viernes son el último intento, sin 'alertar'" \
  "grep 'lunes-ultimo-1700' $FAKE_LOG | grep -q 'cron(0 17 ? \* MON \*)' && grep -q 'viernes-ultimo-1750' $FAKE_LOG && ! grep -q 'alertar' $DIR/desplegar.sh"

: > "$FAKE_LOG"
FAKE_EXISTE=1 bash "$DIR/desplegar.sh" --param /hopdesk/avisos/github_token --aplicar > "$T/out3" 2>&1; st=$?
ok "segunda vez sobre lo que ya existe: sale 0 y no crea nada" \
  "[ $st = 0 ] && ! grep -qE ' create-(role|function|schedule-group|schedule )' $FAKE_LOG"
ok "segunda vez: actualiza código y los 11 horarios" \
  "grep -q 'update-function-code' $FAKE_LOG && [ \"\$(grep -c 'scheduler update-schedule' $FAKE_LOG)\" = 11 ]"

# T80c: a just-created Scheduler role is not assumable for a few seconds.
: > "$FAKE_LOG"; rm -f "$FAKE_LOG.asumir"
FAKE_FALLA_ASUMIR=2 DESPLEGAR_ESPERA_S=0 bash "$DIR/desplegar.sh" --param /hopdesk/avisos/github_token --aplicar > "$T/out4" 2>&1; st=$?
ok "T80c: el rol todavía no se puede asumir 2 veces: espera, reintenta y crea los 11 horarios" \
  "[ $st = 0 ] && [ \"\$(grep -c 'ESPERANDO: el rol del Scheduler' $T/out4)\" = 2 ] && [ \"\$(grep -c 'scheduler create-schedule --name' $FAKE_LOG)\" = 13 ]"
: > "$FAKE_LOG"; rm -f "$FAKE_LOG.asumir"
FAKE_FALLA_ASUMIR=99 DESPLEGAR_ESPERA_S=0 bash "$DIR/desplegar.sh" --param /hopdesk/avisos/github_token --aplicar > "$T/out5" 2>&1; st=$?
ok "T80c: si nunca se puede asumir, se rinde tras 6 reintentos, muestra el error y sale distinto de 0" \
  "[ $st != 0 ] && [ \"\$(grep -c 'ESPERANDO: el rol del Scheduler' $T/out5)\" = 6 ] && grep -q 'assume the role' $T/out5"
: > "$FAKE_LOG"; rm -f "$FAKE_LOG.asumir"
FAKE_OTRO_ERROR=1 DESPLEGAR_ESPERA_S=0 bash "$DIR/desplegar.sh" --param /hopdesk/avisos/github_token --aplicar > "$T/out6" 2>&1; st=$?
ok "T80c: otro error no se reintenta: para al primero y lo muestra" \
  "[ $st != 0 ] && ! grep -q 'ESPERANDO' $T/out6 && grep -q 'AccessDeniedException' $T/out6 && [ \"\$(grep -c 'scheduler create-schedule --name' $FAKE_LOG)\" = 1 ]"
ok "T80c: la vista previa nunca espera ni reintenta" "! grep -q 'ESPERANDO' $T/out1"

bash "$DIR/desplegar.sh" --param ghp_algo > /dev/null 2>&1; st=$?
ok "un --param que no es nombre (/...) se rechaza" "[ $st = 2 ]"
bash "$DIR/desplegar.sh" > /dev/null 2>&1; st=$?
ok "sin --param se rechaza" "[ $st = 2 ]"

echo "TOTAL: $pass passed, $fail failed"
[ "$fail" = 0 ]
