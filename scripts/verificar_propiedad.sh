#!/usr/bin/env bash
# Rechaza cambios a archivos que son de otro frente.
#
#   scripts/verificar_propiedad.sh <rama> <archivo>...
#
# La rama dice quién eres (ver frente_de_rama). El dueño de cada archivo
# sale de coordinacion/propiedad.tsv. Sale con 1 y lista los archivos
# ajenos si hay alguno. Lo usan .githooks/pre-commit y el CI.
set -u

MAPA="$(git rev-parse --show-toplevel)/coordinacion/propiedad.tsv"

frente_de_rama() {
  case "$1" in
    master|main)                  echo "integrador" ;;
    feat/a1*|feat/a2*|feat/f*)    echo "integrador" ;;
    feat/b*)                      echo "B" ;;
    feat/c*)                      echo "C" ;;
    feat/d*)                      echo "D" ;;
    feat/e*|requisiciones/*)      echo "E" ;;
    tablas/*)                     echo "T" ;;
    *)                            echo "libre" ;;
  esac
}

dueno_de() {
  local f="$1" pat dueno
  while IFS=$'\t' read -r pat dueno; do
    [[ -z "$pat" || "$pat" == \#* ]] && continue
    # shellcheck disable=SC2053
    if [[ "$f" == $pat ]]; then echo "$dueno"; return; fi
  done < "$MAPA"
  echo ""
}

rama="${1:-}"; shift || true
yo="$(frente_de_rama "$rama")"
ajenos=()
for f in "$@"; do
  d="$(dueno_de "$f")"
  [[ -z "$d" || "$d" == "compartido" || "$d" == "$yo" ]] && continue
  ajenos+=("$f (es de $d)")
done

if (( ${#ajenos[@]} )); then
  echo "La rama '$rama' (frente: $yo) toca archivos de otro frente:" >&2
  printf '  - %s\n' "${ajenos[@]}" >&2
  echo "Pídelo en coordinacion/<tu frente>.md con el texto exacto; el dueño lo aplica." >&2
  exit 1
fi
exit 0
